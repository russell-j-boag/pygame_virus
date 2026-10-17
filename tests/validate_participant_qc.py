#!/usr/bin/env python3
"""Independent audit recalculation. Does not import participant_qc or task code."""
import argparse
import csv
import hashlib
import itertools
import json
import math
import re
import statistics
import subprocess
from collections import Counter, defaultdict
from datetime import datetime, timezone
from html.parser import HTMLParser
from pathlib import Path

ROOT=Path(__file__).resolve().parents[1]
CATEGORIES=['calibration_target','manual_target','near_chance_or_worse','anticipatory']
EXCLUSION=['calibration_target','near_chance_or_worse','anticipatory']
CONDITIONS=['CALIBRATION','MANUAL','AUTOMATION1','AUTOMATION2']
LABELS=['Calibration','Manual','Aided 95%','Aided 65%','Aided overall','Experimental overall','Calibration final trials']


def sha(path):return hashlib.sha256(Path(path).read_bytes()).hexdigest()
def csv_rows(path):
    with Path(path).open(newline='') as handle:return list(csv.DictReader(handle))
def num(x):
    try:
        x=float(x)
        return x if math.isfinite(x) else None
    except (ValueError,TypeError):return None
def empty(x):return x is None or str(x).strip().lower() in ('','na','nan')
def bit(x):return {'true':1,'false':0,'1':1,'0':0}.get(str(x).lower())


def equal(actual,expected,label):
    if expected is None:
        assert empty(actual),(label,actual,expected)
    elif isinstance(expected,bool):
        assert bit(actual)==int(expected),(label,actual,expected)
    elif isinstance(expected,(float,int)):
        assert num(actual) is not None and math.isclose(float(actual),expected,rel_tol=1e-12,abs_tol=1e-12),(label,actual,expected)
    else:
        assert actual==expected,(label,actual,expected)


def interval(k,n,level):
    if not n:return None,None
    z=statistics.NormalDist().inv_cdf(.5+level/2)
    denominator=n+z*z
    center=(k+z*z/2)/denominator
    half=z*math.sqrt(k*(n-k)/n+z*z/4)/denominator
    return max(0,center-half),min(1,center+half)


def quantile(values,q):
    values=sorted(values)
    if not values:return None
    pos=(len(values)-1)*q;lo=math.floor(pos);hi=math.ceil(pos)
    return values[lo]+(values[hi]-values[lo])*(pos-lo)


def recalculate(rows,cfg):
    pid=int(rows[0]['participant_id'])
    order=list(itertools.permutations(CONDITIONS[1:]))[(pid-1)%6]
    mapping=('J','D') if ((pid-1)//6)%2 else ('D','J')
    segments=[];start=1
    for idx,code in enumerate(['CALIBRATION',*order],1):
        n=300 if code=='CALIBRATION' else 500
        segments.append((code,start,start+n-1,idx));start+=n
    invalid=set();global_invalid=False
    parsed={}
    for raw in rows:
        t=int(raw['global_trial']);assert t not in parsed
        answered=raw['response'] in ['BLACK','WHITE'];timeout=raw['response']=='TIMEOUT'
        rt=num(raw['rt_s']);ok=answered and raw['response']==raw['stimulus']
        parsed[t]=dict(raw,answered=answered,timeout=timeout,rt=rt,ok=int(ok))
        seg=next((s for s in segments if s[1]<=t<=s[2]),None)
        if seg is None:global_invalid=True;continue
        code,lo,hi,idx=seg;aided=code.startswith('AUTOMATION')
        valid=(int(raw['block_idx'])==idx and raw['block']==code and int(raw['trial'])==t-lo+1
            and (raw['key_black'],raw['key_white'])==mapping and bit(raw['keymap_flip'])==int(mapping[0]=='J')
            and bit(raw['auto_on'])==int(aided) and raw['stimulus'] in ['BLACK','WHITE'] and (answered or timeout))
        if 'trial_deadline_s' in raw:valid &= num(raw['trial_deadline_s'])==10
        if aided:
            valid &= num(raw['aid_accuracy_setting'])==(.95 if code=='AUTOMATION1' else .65)
            valid &= raw['aid_label'] in ['BLACK','WHITE'] and bit(raw['aid_correct'])==int(raw['aid_label']==raw['stimulus'])
        else:valid &= all(empty(raw[k]) for k in ['aid_accuracy_setting','aid_label','aid_correct'])
        if answered:valid &= bit(raw['correct'])==int(ok) and raw['feedback']==('CORRECT' if ok else 'INCORRECT') and rt is not None
        if timeout:valid &= (empty(raw['correct']) or bit(raw['correct'])==0) and raw['feedback']=='TOO SLOW' and (empty(raw['rt_s']) or rt is not None)
        if not valid:invalid.add(t)
    meta=dict(participant_id=pid,run_timestamp=rows[0]['run_timestamp'],key_black=rows[0]['key_black'],key_white=rows[0]['key_white'],main_block_order='|'.join(order))

    def measure(code,label,expected):
        data=[parsed[t] for t in sorted(expected & parsed.keys())]
        complete=not global_invalid and not (expected & invalid) and expected<=parsed.keys()
        n=len(data);answered=[r for r in data if r['answered']];fast=[r for r in answered if r['rt'] is not None and r['rt']<cfg['fast_rt_seconds']]
        k=sum(r['ok'] for r in data);fk=sum(r['ok'] for r in fast);nf=len(fast)
        positives=[r['rt'] for r in answered if r['rt'] is not None and r['rt']>0]
        low,high=interval(k,n,cfg['interval_level']) if complete else (None,None)
        flow,fhigh=interval(fk,nf,cfg['interval_level']) if complete else (None,None)
        accuracy=k/n if complete else None
        out=dict(meta,scope=code,scope_label=label,trial_start=min(expected),trial_end=max(expected),
            assessable=complete,n_expected=len(expected),n_trials=n,n_answered=len(answered),
            n_timeouts=sum(r['timeout'] for r in data),n_correct=k if complete else None,accuracy=accuracy,
            accuracy_lower=low,accuracy_upper=high,n_fast=nf,fast_fraction=nf/n if complete else None,
            n_fast_correct=fk if complete else None,fast_accuracy=fk/nf if complete and nf else None,
            fast_accuracy_lower=flow,fast_accuracy_upper=fhigh,
            n_nonpositive_rt=sum(r['rt'] is not None and r['rt']<=0 for r in answered),
            n_positive_fast=sum(r['rt']>0 for r in fast),median_positive_rt_ms=statistics.median(positives)*1000 if positives else None,
            answered_accuracy=k/len(answered) if complete and answered else None,
            timeout_fraction=sum(r['timeout'] for r in data)/n if complete else None,
            n_timeouts_with_recorded_rt=sum(r['timeout'] and r['rt'] is not None for r in data),
            n_beyond_deadline=sum(r['rt'] is not None and r['rt']>10 for r in answered),
            calibration_target_accuracy=.8,target_difference_pp=100*(accuracy-.8) if complete else None)
        for ms in cfg['sensitivity_thresholds_ms']:
            out[f'fraction_rt_lt_{ms}ms']=sum(r['rt'] is not None and r['rt']<ms/1000 for r in answered)/n if complete else None
        return out
    ranges={code:set(range(lo,hi+1)) for code,lo,hi,_ in segments}
    ranges['aided_overall']=ranges['AUTOMATION1']|ranges['AUTOMATION2']
    ranges['experimental_overall']=set(range(301,1801))
    ranges['calibration_tail']=set(range(301-cfg['calibration_tail_n'],301))
    metrics=[measure(code,label,ranges[code]) for code,label in zip(CONDITIONS+['aided_overall','experimental_overall','calibration_tail'],LABELS)]
    checks=[]
    for m in metrics:
        categories=['calibration_target'] if m['scope']=='calibration_tail' else ['near_chance_or_worse','anticipatory']
        if m['scope']=='MANUAL':categories+=['manual_target']
        for category in categories:
            flag=False
            if m['assessable']:
                if category in ['calibration_target','manual_target']:flag=abs(m['target_difference_pp'])>cfg[category+'_band_pp']+1e-12
                elif category=='near_chance_or_worse':flag=m['accuracy']<=cfg['chance_accuracy_max']+1e-12
                else:flag=(m['n_fast']>=cfg['fast_count_min'] and m['fast_fraction']>=cfg['fast_fraction_min']-1e-12 and m['fast_accuracy'] is not None and m['fast_accuracy']<=cfg['fast_accuracy_max']+1e-12)
            checks.append(dict({k:m[k] for k in ['participant_id','run_timestamp','key_black','key_white','main_block_order','scope','scope_label','n_trials','accuracy','target_difference_pp','n_fast','fast_fraction','fast_accuracy']},category=category,use_for_exclusion=category in EXCLUSION,
                status='not_assessable' if not m['assessable'] else 'flag' if flag else 'clear'))
    summary=dict(meta)
    for category in CATEGORIES:
        statuses={c['status'] for c in checks if c['category']==category}
        summary[category]='flag' if 'flag' in statuses else 'not_assessable' if 'not_assessable' in statuses else 'clear'
    flagged=[c for c in checks if c['status']=='flag'];candidate=[c for c in flagged if c['use_for_exclusion']]
    summary.update(n_flags=len(flagged),n_not_assessable=sum(c['status']=='not_assessable' for c in checks),
        n_flagged_rules=sum(summary[c]=='flag' for c in CATEGORIES),flagged_any_rule=bool(flagged),
        n_exclusion_rules=sum(summary[c]=='flag' for c in EXCLUSION),exclude_by_rules=bool(candidate),
        exclusion_reasons='; '.join(c['category']+':'+c['scope'] for c in candidate),
        review_required=bool(flagged) or any(not m['assessable'] for m in metrics),
        reasons='; '.join(c['category']+':'+c['scope'] for c in flagged))
    bins=[]
    for code,lo,hi,_ in segments:
        for start in range(lo,hi+1,cfg['bin_size']):
            end=start+cfg['bin_size']-1
            b=measure(code,LABELS[CONDITIONS.index(code)],set(range(start,end+1)))
            b.update(trial_midpoint=(start+end)/2,condition_trial_start=start-lo+1,condition_trial_midpoint=(start+end)/2-lo+1)
            bins.append(b)
    return metrics,checks,summary,bins


def compare_rows(actual,expected,keys):
    def identity(r):return tuple(str(r[k]) for k in keys)
    indexed={identity(r):r for r in actual}
    assert len(indexed)==len(actual)==len(expected),(keys,len(actual),len(expected))
    for expected_row in expected:
        row=indexed[identity(expected_row)]
        for key,value in expected_row.items():equal(row[key],value,(identity(row),key))
        if 'assessable' in expected_row:
            assert bool(row['integrity_issues']) != expected_row['assessable']


class Links(HTMLParser):
    def __init__(self):super().__init__();self.links=[];self.ids=set()
    def handle_starttag(self,tag,attrs):
        attrs=dict(attrs)
        if 'id' in attrs:self.ids.add(attrs['id'])
        self.links += [attrs[k] for k in ('href','src') if k in attrs]


def validate(out,expect_current=False):
    manifest=json.loads((out/'manifest.json').read_text());cfg=json.loads((out/'config_snapshot.json').read_text())
    assert manifest['generator']=='participant_qc' and manifest['exclusions_applied'] is False
    assert (out/'COMPLETE.txt').is_file()
    assert sha(manifest['input_csv'])==manifest['input_sha256']
    assert sha(manifest['config_path'])==manifest['config_sha256']
    assert json.loads(Path(manifest['config_path']).read_text())==cfg
    for record in manifest['sources']+manifest['raw_sources']:assert sha(record['path'])==record['sha256'],record['path']
    assert {str(p.resolve()) for p in Path(manifest['raw_directory']).glob('results_*_b00_ALL.csv')}=={r['path'] for r in manifest['raw_sources']}
    inventory={str(p.relative_to(out)) for p in out.rglob('*') if p.is_file()}-{'manifest.json','COMPLETE.txt'}
    assert inventory=={r['path'] for r in manifest['outputs']}
    for record in manifest['outputs']:
        path=out/record['path'];assert sha(path)==record['sha256'] and path.stat().st_size==record['bytes'],path
    people=defaultdict(list)
    for r in csv_rows(manifest['input_csv']):people[int(r['participant_id'])].append(r)
    people=dict(sorted(people.items()))
    metrics=[];checks=[];summaries=[];bins=[]
    for pid,rows in people.items():
        assert len({r['run_timestamp'] for r in rows})==1
        mm,cc,ss,bb=recalculate(rows,cfg);metrics+=mm;checks+=cc;summaries.append(ss);bins+=bb
    compare_rows(csv_rows(out/'scope_metrics.csv'),metrics,('participant_id','scope'))
    compare_rows(csv_rows(out/'trial_bin_summary.csv'),bins,('participant_id','scope','trial_start'))
    compare_rows(csv_rows(out/'screening_results.csv'),checks,('participant_id','scope','category'))
    compare_rows(csv_rows(out/'flagged_checks.csv'),[c for c in checks if c['status']=='flag'],('participant_id','scope','category'))
    compare_rows(csv_rows(out/'participant_summary.csv'),summaries,('participant_id',))
    candidates=[s for s in summaries if s['exclude_by_rules']]
    compare_rows(csv_rows(out/'exclusion_candidates.csv'),candidates,('participant_id',))
    invalid=[m for m in metrics if not m['assessable']]
    issues=csv_rows(out/'integrity_issues.csv')
    assert {(int(r['participant_id']),r['scope']) for r in issues}=={(r['participant_id'],r['scope']) for r in invalid}
    totals=json.loads((out/'exclusion_summary.json').read_text())
    assert totals['participant_ids']==[s['participant_id'] for s in candidates]
    assert totals['unique_flagged_n']==len(candidates) and totals['remaining_if_applied_n']==len(people)-len(candidates)
    assert not totals['exclusions_applied'] and not totals['qc_data_filtered']
    assert totals['participant_n']==len(people) and totals['diagnostic_only_n']==len(totals['diagnostic_only_participant_ids'])
    equal(totals['unique_flagged_percent'],100*len(candidates)/len(people),'candidate percent')
    assert totals['rule_counts']=={c:sum(s[c]=='flag' for s in summaries) for c in EXCLUSION}
    assert totals['diagnostic_rule_counts']=={'manual_target':sum(s['manual_target']=='flag' for s in summaries)}
    table=csv_rows(out/'exclusion_summary_table.csv')
    assert len(table)==6
    for r in table:
        criterion=r['criterion']
        ids=([s['participant_id'] for s in summaries if s[criterion]=='flag'] if criterion in CATEGORIES else
             [s['participant_id'] for s in candidates] if criterion=='any_exclusion_rule' else [])
        n=len(people)-len(candidates) if criterion=='remaining_if_applied' else len(ids)
        equal(r['participant_n'],n,criterion);equal(r['percent_of_cohort'],100*n/len(people),criterion)
        assert r['participant_ids']=='; '.join(f'{p:03d}' for p in ids)
    assert totals['diagnostic_only_participant_ids']==[s['participant_id'] for s in summaries if s['flagged_any_rule'] and not s['exclude_by_rules']]
    assert totals['incomplete_assessment_ids']==[s['participant_id'] for s in summaries if s['n_not_assessable']]
    for c in CATEGORIES:assert totals['rule_participant_ids'][c]==[s['participant_id'] for s in summaries if s[c]=='flag']
    for row in totals['overlap']:assert row['participant_ids']==[s['participant_id'] for s in candidates if s['n_exclusion_rules']==row['n_rules']] and row['participant_n']==len(row['participant_ids'])
    overall={m['participant_id']:m for m in metrics if m['scope']=='experimental_overall'}
    groups=defaultdict(list)
    for s in summaries:
        if s['anticipatory']=='flag':groups[(s['main_block_order'],s['key_black'],s['key_white'])].append(s['participant_id'])
    comparisons=json.loads((out/'comparator_selection.json').read_text());assert len(comparisons)==len(groups)
    for comp,(group,targets) in zip(comparisons,sorted(groups.items())):
        assert (comp['group'],comp['key_black'],comp['key_white'])==group and comp['targets']==targets
        pool=[s['participant_id'] for s in summaries if (s['main_block_order'],s['key_black'],s['key_white'])==group and s['anticipatory']=='clear' and not s['n_not_assessable'] and overall[s['participant_id']]['median_positive_rt_ms'] is not None]
        centers={k:statistics.median(overall[p][k] for p in pool) for k in ['accuracy','median_positive_rt_ms']} if pool else {}
        scales={k:statistics.median(abs(overall[p][k]-center) for p in pool) for k,center in centers.items()}
        assert comp['centers']==centers and comp['median_absolute_deviations']==scales
        ranking=sorted([(sum(abs(overall[p][k]-centers[k])/scales[k] for k in centers if scales[k]>1e-12),p) for p in pool])
        assert comp['comparator']==(ranking[0][1] if ranking else None)
        assert len(comp['ranked_candidates'])==len(ranking)
        for r,(score,pid) in zip(comp['ranked_candidates'],ranking):assert r['participant_id']==pid;equal(r['score'],score,'comparator score')
    pools=json.loads((out/'reference_pools.json').read_text());bands=csv_rows(out/'reference_bands.csv')
    assert set(pools)==set(map(str,people))
    aligned={(b['participant_id'],b['scope'],b['condition_trial_midpoint']):b for b in bins}
    expected_bands=[]
    for s in summaries:
        pid=s['participant_id'];peers=[x['participant_id'] for x in summaries if x['participant_id']!=pid and x['anticipatory']=='clear' and not x['n_not_assessable']]
        assert pools[str(pid)]==peers
        for b in [b for b in bins if b['participant_id']==pid]:
            row={k:b[k] for k in ['participant_id','scope','condition_trial_midpoint','trial_midpoint']};row['peer_n']=len(peers)
            for key in ['accuracy','fast_fraction']:
                values=[100*aligned[p,b['scope'],b['condition_trial_midpoint']][key] for p in peers]
                row.update({key+'_'+name:quantile(values,q) for name,q in [('q25',.25),('median',.5),('q75',.75)]})
            expected_bands.append(row)
    compare_rows(bands,expected_bands,('participant_id','scope','condition_trial_midpoint'))
    ledger=json.loads((out/'source_runs.json').read_text());assert ledger==manifest['raw_sources']
    selected=[r for r in ledger if r['selected']]
    assert {(r['participant_id'],r['run_timestamp']) for r in selected}=={(pid,rows[0]['run_timestamp']) for pid,rows in people.items()}
    assert len(selected)==len(people)
    for source in selected:
        raw=csv_rows(source['path']);canonical={int(r['global_trial']):r for r in people[source['participant_id']]}
        assert len(raw)==len(canonical)==source['trial_n']
        assert {int(r['global_trial']) for r in raw}==set(canonical)
        assert list(raw[0])==source['compared_columns']
        for row in raw:
            for field,value in row.items():
                other=canonical[int(row['global_trial'])][field]
                if empty(value):assert empty(other)
                elif num(value) is not None:equal(other,float(value),('raw',source['path'],field))
                elif bit(value) is not None:assert bit(other)==bit(value)
                else:assert other==value,(source['path'],field)
    parser=Links();parser.feed((out/'index.html').read_text())
    for link in parser.links:
        if link.startswith('#'):assert link[1:] in parser.ids
        elif '://' not in link:assert (out/link).is_file(),link
    plot_sources=json.loads((out/'plot_sources.json').read_text())
    assert [x['participant_id'] for x in plot_sources['individuals']]==list(people)
    pages=plot_sources['comparison_pages']
    assert sorted(p for page in pages for p in page['targets'])==sorted(p for targets in groups.values() for p in targets)
    for page in pages:
        assert len(page['targets']) in (1,2)
        match=next(c for c in comparisons if page['targets'][0] in c['targets'])
        assert page['comparator']==match['comparator'] and set(page['targets'])<=set(match['targets'])
    assert (out/'anticipatory_comparisons.pdf').exists()==bool(pages)
    pdfs=list(out.rglob('*.pdf'));pngs=list(out.rglob('*.png'))
    assert len(pngs)==4+len(people)+len(pages)
    assert len(pdfs)==5+len(people)+len(pages)+bool(pages)
    from PIL import Image
    for path in pngs:
        with Image.open(path) as im:assert im.width>1000 and im.height>500;im.verify()
    pdf_pages={}
    for path in pdfs:
        info=subprocess.check_output(['/opt/homebrew/bin/pdfinfo',str(path)],text=True)
        count=int(re.search(r'^Pages:\s+(\d+)',info,re.M)[1])
        expected=len(people) if path.name=='all_individual_diagnostics.pdf' else len(pages) if path.name=='anticipatory_comparisons.pdf' else 1
        assert count==expected,(path,count,expected);pdf_pages[str(path.relative_to(out))]=count
    assert manifest['participant_n']==len(people) and manifest['trial_n']==sum(map(len,people.values()))
    assert manifest['flag_counts']=={c:sum(s[c]=='flag' for s in summaries) for c in CATEGORIES}
    assert manifest['block_orders']==dict(Counter(s['main_block_order'] for s in summaries))
    assert manifest['integrity_scope_n']==len(invalid)
    if expect_current:
        assert list(people)==list(range(1,49)) and sum(map(len,people.values()))==86400
        assert len(metrics)==336 and len(checks)==672 and len(bins)==1728 and not invalid
        assert not candidates and totals['diagnostic_only_participant_ids']==[28,29,40]
        assert sum(m['n_nonpositive_rt'] for m in metrics if m['scope'] in CONDITIONS)==10
        assert sum(m['n_timeouts'] for m in metrics if m['scope'] in CONDITIONS)==36
        assert all(m['n_beyond_deadline']==m['n_timeouts_with_recorded_rt']==0 for m in metrics)
        assert all(v==8 for v in Counter(s['main_block_order'] for s in summaries).values())
        assert all(v==4 for v in Counter((s['main_block_order'],s['key_black']) for s in summaries).values())
        assert len(ledger)==49 and len(selected)==48
        assert [(r['participant_id'],r['run_timestamp']) for r in ledger if not r['selected']]==[(35,'20260422_112735')]
        assert people[35][0]['run_timestamp']=='20260422_133042'
        assert manifest['input_sha256']=='453c21f80444b6f8380c81e25b6d65ca7eb19472b8c9ed654be8526ad07a640f'
    return dict(status='passed',report=str(out),validated_utc=datetime.now(timezone.utc).isoformat(),
        independent_validator_sha256=sha(__file__),report_manifest_sha256=sha(out/'manifest.json'),
        participant_n=len(people),trial_n=sum(map(len,people.values())),metric_n=len(metrics),check_n=len(checks),bin_n=len(bins),
        candidate_ids=totals['participant_ids'],diagnostic_only_ids=totals['diagnostic_only_participant_ids'],
        raw_sources_n=len(ledger),selected_raw_sources_n=len(selected),png_n=len(pngs),pdf_n=len(pdfs),pdf_pages=pdf_pages,
        snapshot_assertions=expect_current,exclusions_applied=False)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output-dir',type=Path,default=ROOT/'analysis_outputs/semester1_2026_qc')
    parser.add_argument('--expect-current-cohort',action='store_true')
    parser.add_argument('--receipt',type=Path)
    parser.add_argument('--preservation-snapshot',type=Path)
    args=parser.parse_args();receipt=validate(args.output_dir.resolve(),args.expect_current_cohort)
    if args.preservation_snapshot:
        hashes=json.loads(args.preservation_snapshot.read_text())
        assert all(sha(path)==digest for path,digest in hashes.items())
        receipt['preserved_file_n']=len(hashes);receipt['preservation_snapshot_sha256']=sha(args.preservation_snapshot)
    if args.receipt:
        assert not args.receipt.resolve().is_relative_to(args.output_dir.resolve()),'Store validation outside the generated report'
        args.receipt.parent.mkdir(parents=True,exist_ok=True);args.receipt.write_text(json.dumps(receipt,indent=2)+'\n')
    print(json.dumps({k:v for k,v in receipt.items() if k!='pdf_pages'},indent=2))


if __name__=='__main__':main()
