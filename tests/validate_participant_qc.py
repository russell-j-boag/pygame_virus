#!/usr/bin/env python3
"""Independent raw-data recomputation and artifact QA; no QC-generator imports.

The optional snapshot assertions apply only to the initial 60-person audit.
Writes a receipt only when --receipt is supplied, outside the generated bundle.
"""
import argparse
import csv
import hashlib
import json
import math
import statistics
import subprocess
from collections import Counter, defaultdict
from datetime import datetime, timezone
from html.parser import HTMLParser
from pathlib import Path

import numpy as np
from PIL import Image

ROOT=Path(__file__).resolve().parents[1]
ORDERS=(('MANUAL','AIDFIRST','STIMFIRST'),('STIMFIRST','AIDFIRST','MANUAL'),
        ('MANUAL','STIMFIRST','AIDFIRST'),('AIDFIRST','STIMFIRST','MANUAL'),
        ('AIDFIRST','MANUAL','STIMFIRST'),('STIMFIRST','MANUAL','AIDFIRST'))
MAIN=('MANUAL','AIDFIRST','STIMFIRST')


def read(path):
    with Path(path).open(newline='') as f:return list(csv.DictReader(f))


def digest(path):return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def num(value):
    try:
        v=float(value)
        return v if math.isfinite(v) else None
    except (ValueError,TypeError):return None


def blank(v):return v is None or str(v).strip().lower() in ('','na','nan','none')


def boolean(v):return {'true':True,'false':False,'1':True,'0':False}.get(str(v).lower())


def near(actual,expected,label):
    if expected is None:
        assert blank(actual), (label,actual,expected)
    elif isinstance(expected,bool):
        assert boolean(actual)==expected, (label,actual,expected)
    elif isinstance(expected,(int,float)):
        assert num(actual) is not None and math.isclose(float(actual),expected,rel_tol=1e-10,abs_tol=1e-10),(label,actual,expected)
    else:assert str(actual)==str(expected),(label,actual,expected)


def equivalent(a,b):
    if blank(a) or blank(b):return blank(a) and blank(b)
    if str(a).lower() in ('true','false') or str(b).lower() in ('true','false'):return boolean(a)==boolean(b)
    if num(a) is not None and num(b) is not None:return math.isclose(num(a),num(b),rel_tol=1e-12,abs_tol=1e-12)
    return a==b


def interval(k,n,level):
    if n==0:return None,None
    z=statistics.NormalDist().inv_cdf(.5+level/2)
    # Wilson score roots, algebraically distinct from generator implementation.
    a=n+z*z;b=-(2*k+z*z);c=k*k/n
    discriminant=max(0,b*b-4*a*c)
    return max(0,(-b-math.sqrt(discriminant))/(2*a)),min(1,(-b+math.sqrt(discriminant))/(2*a))


def trial_valid(r,d,pid):
    order=ORDERS[((pid-1)//2)%6];code=r['condition_code'];practice=code=='PRACTICE'
    block=0 if practice else order.index(code)
    if not practice:block+=1
    t=int(r['trial']);n=60 if practice else 260
    if int(r['block_idx'])!=block or not 1<=t<=n or int(r['global_trial'])!=(t if practice else (block-1)*260+t):return False
    flip=pid%2==0
    if r['key_black']!=('J' if flip else 'D') or r['key_white']!=('D' if flip else 'J') or boolean(r['keymap_flip'])!=flip:return False
    if not blank(r['trial_deadline_s']) or r['difficulty_mode']!=('staircase' if practice else 'fixed_delta'):return False
    aided=code in ('AIDFIRST','STIMFIRST')
    if boolean(r['auto_on'])!=aided or r['aid_condition']!={'PRACTICE':'manual','MANUAL':'manual','AIDFIRST':'aid_first','STIMFIRST':'stimulus_first'}[code]:return False
    if r['stimulus'] not in ('BLACK','WHITE'):return False
    if aided:
        if num(r['aid_accuracy_setting'])!=.85 or r['aid_label'] not in ('BLACK','WHITE') or boolean(r['aid_correct'])!=(r['aid_label']==r['stimulus']):return False
    elif any(not blank(r[k]) for k in ('aid_label','aid_correct','aid_accuracy_setting')):return False
    for phase,prefix in ((1,'preview'),(2,'decision2_preview')):
        real=code==('AIDFIRST' if phase==1 else 'STIMFIRST')
        if r[prefix+'_display']!=('aid_only' if real else 'masked_placeholder') or r[prefix+'_label']!=(r['aid_label'] if real else '#####'):return False
    if boolean(r['changed_response'])!=(r['decision1_response']!=r['decision2_response']):return False
    response=r[f'decision{d}_response'];timeout=response=='TIMEOUT';answered=response in ('BLACK','WHITE')
    if not answered and not timeout:return False
    ok=answered and response==r['stimulus']
    if boolean(r[f'decision{d}_correct'])!=ok and not (timeout and blank(r[f'decision{d}_correct'])):return False
    if answered and num(r[f'decision{d}_rt_s']) is None:return False
    if timeout and not blank(r[f'decision{d}_rt_s']) and num(r[f'decision{d}_rt_s']) is None:return False
    if r[f'decision{d}_display']!=('stimulus_only' if d==1 else 'blank_response'):return False
    if aided and answered and boolean(r[f'decision{d}_matches_aid'])!=(response==r['aid_label']):return False
    if not aided and not blank(r[f'decision{d}_matches_aid']):return False
    if d==2 and r['feedback']!=('TOO SLOW' if timeout else 'CORRECT' if ok else 'INCORRECT'):return False
    return True


def score(rows,d,pid,expected,cfg,invalid_practice=False):
    actual={(int(r['block_idx']),int(r['trial'])) for r in rows}
    valid=actual==expected and len(actual)==len(rows) and not invalid_practice and all(trial_valid(r,d,pid) for r in rows)
    responses=[r for r in rows if r[f'decision{d}_response'] in ('BLACK','WHITE')]
    timeouts=[r for r in rows if r[f'decision{d}_response']=='TIMEOUT']
    fast=[r for r in responses if num(r[f'decision{d}_rt_s']) is not None and num(r[f'decision{d}_rt_s'])<cfg['fast_rt_seconds']]
    k=sum(r[f'decision{d}_response']==r['stimulus'] for r in responses)
    fk=sum(r[f'decision{d}_response']==r['stimulus'] for r in fast)
    n,nf=len(rows),len(fast)
    low,high=interval(k,n,cfg['interval_level']) if valid else (None,None)
    fl,fu=interval(fk,nf,cfg['interval_level']) if valid else (None,None)
    positive=[1000*num(r[f'decision{d}_rt_s']) for r in responses if num(r[f'decision{d}_rt_s']) is not None and num(r[f'decision{d}_rt_s'])>0]
    metrics=dict(assessable=valid,n_trials=n,n_expected=len(expected),n_answered=len(responses),n_timeouts=len(timeouts),n_correct=k if valid else None,
        accuracy=k/n if valid and n else None,accuracy_lower=low,accuracy_upper=high,answered_accuracy=k/len(responses) if valid and responses else None,
        timeout_fraction=len(timeouts)/n if valid and n else None,n_fast=nf,n_fast_correct=fk if valid else None,
        fast_fraction=nf/n if valid and n else None,fast_accuracy=fk/nf if valid and nf else None,fast_accuracy_lower=fl,fast_accuracy_upper=fu,
        n_nonpositive_rt=sum(num(r[f'decision{d}_rt_s']) is not None and num(r[f'decision{d}_rt_s'])<=0 for r in responses),
        n_timeouts_with_recorded_rt=sum(num(r[f'decision{d}_rt_s']) is not None for r in timeouts),
        median_positive_rt_ms=statistics.median(positive) if positive and valid else None)
    for ms in cfg['sensitivity_thresholds_ms']:
        metrics[f'fraction_rt_lt_{ms}ms']=sum(num(r[f'decision{d}_rt_s']) is not None and num(r[f'decision{d}_rt_s'])<ms/1000 for r in responses)/n if valid and n else None
    return metrics


class Links(HTMLParser):
    def __init__(self):super().__init__();self.targets=[]
    def handle_starttag(self,tag,attrs):
        for key,value in attrs:
            if key in ('src','href') and value and not value.startswith(('#','https:','http:','data:')):self.targets.append(value)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report',type=Path,default=ROOT/'analysis_outputs/semester2_2026_qc')
    parser.add_argument('--expect-current-cohort',action='store_true')
    parser.add_argument('--receipt',type=Path)
    args=parser.parse_args();report=args.report.resolve()
    if args.receipt and args.receipt.resolve().is_relative_to(report):raise ValueError('Validation receipts must be outside the generated bundle')
    manifest=json.loads((report/'manifest.json').read_text());cfg=json.loads((report/'config_snapshot.json').read_text())
    assert manifest['study']=='aid_onset' and manifest['exclusions_applied'] is False
    for item in manifest['sources']:assert digest(item['path'])==item['sha256'],item['path']
    assert cfg==json.loads(Path(manifest['config_path']).read_text())
    outputs=manifest['outputs'];assert len({o['path'] for o in outputs})==len(outputs)
    for item in outputs:
        p=(report/item['path']).resolve();assert p.is_relative_to(report)
        assert p.stat().st_size==item['bytes'] and digest(p)==item['sha256'],item['path']
    assert {str(p.relative_to(report)) for p in report.rglob('*') if p.is_file()}=={o['path'] for o in outputs}|{'manifest.json'}
    raw=read(manifest['input_csv']);people=defaultdict(list)
    for r in raw:people[int(r['participant_id'])].append(r)
    assert len(people)==manifest['participant_n'] and len(raw)==manifest['main_trial_n']
    source_table=read(manifest['collation_manifest']);sources=read(report/'source_run_manifest.csv')
    for source in source_table:
        path=Path(source['source_file'])
        if not path.is_absolute():source['source_file']=str(Path(manifest['collation_manifest']).resolve().parent/path)
    for source in sources:
        if source['status']=='present':assert digest(source['source_file'])==source['sha256']
        else:assert not Path(source['source_file']).exists()
    for source in source_table:assert hashlib.md5(Path(source['source_file']).read_bytes()).hexdigest()==source['source_md5']
    target={};practice_invalid={};meta={}
    for pid,rr in people.items():
        ts={r['run_timestamp'] for r in rr};assert len(ts)==1;ts=ts.pop()
        assert {(int(r['block_idx']),int(r['trial'])) for r in rr}.__len__()==len(rr)
        selected=[s for s in source_table if int(s['participant_id'])==pid];assert len(selected)==3 and {s['export_type'] for s in selected}=={'trials','questionnaire','sliders'}
        assert all(s['run_timestamp']==ts for s in selected)
        source=Path(next(s['source_file'] for s in selected if s['export_type']=='trials'))
        assert source.name==f'results_p{pid:03d}_{ts}_b00_ALL.csv'
        original=read(source);assert len(original)==len(rr)
        sort=lambda data:sorted(data,key=lambda r:(int(r['block_idx']),int(r['trial'])))
        for a,b in zip(sort(rr),sort(original)):
            assert all(k in b and equivalent(v,b[k]) for k,v in a.items()),(pid,a['trial'])
        practice=source.with_name(source.name.replace('_ALL.csv','_PRACTICE.csv'))
        delta=practice.with_name(practice.name.replace('results_','delta_',1))
        cc=read(practice) if practice.exists() else []
        assert all(int(r['participant_id'])==pid and r['run_timestamp']==ts and r['condition_code']=='PRACTICE' for r in cc)
        targets={num(r['staircase_target_accuracy']) for r in cc}
        target[pid]=next(iter(targets)) if len(targets)==1 and None not in targets else None
        if target[pid] is not None and not 0<target[pid]<1:target[pid]=None
        dd=read(delta) if delta.exists() else []
        invalid=not cc or len(dd)!=1
        if dd:
            s=dd[0];assert int(s['participant_id'])==pid and s['run_timestamp']==ts
            invalid=invalid or num(s['burnin_trials_excluded'])!=20 or num(s['n_trials_summarised'])!=40 or num(s['n_trials_total'])!=60 or targets!={num(s['staircase_target_accuracy'])} or num(s['staircase_target_accuracy']) is None
        practice_invalid[pid]=invalid;rr.extend(cc)
        meta[pid]=dict(participant_id=pid,run_timestamp=ts,calibration_target_accuracy=target[pid],key_black=rr[0]['key_black'],key_white=rr[0]['key_white'],main_block_order='|'.join(ORDERS[((pid-1)//2)%6]))
    assert sum(len(r) for r in people.values())-len(raw)==manifest['practice_trial_n']
    metrics={};bins={};checks=[]
    for pid,rr in sorted(people.items()):
        order=ORDERS[((pid-1)//2)%6];indices={c:i for i,c in enumerate(('PRACTICE',*order))}
        for scope in ('PRACTICE',*MAIN,'aided_overall','experimental_overall','calibration_tail'):
            codes=('AIDFIRST','STIMFIRST') if scope=='aided_overall' else MAIN if scope=='experimental_overall' else ('PRACTICE',) if scope=='calibration_tail' else (scope,)
            expected={(indices[c],t) for c in codes for t in range(21 if scope=='calibration_tail' else 1,(60 if c=='PRACTICE' else 260)+1)}
            selected=[r for r in rr if r['condition_code'] in codes and (scope!='calibration_tail' or int(r['trial'])>=21)]
            for d in ((2,) if scope=='calibration_tail' else (1,2)):
                m=score(selected,d,pid,expected,cfg,practice_invalid[pid] if scope in ('PRACTICE','calibration_tail') else False)
                m.update(meta[pid]);metrics[pid,scope,d]=m
                categories=['calibration_target'] if scope=='calibration_tail' else ['near_chance_or_worse','anticipatory']
                if scope=='MANUAL' and d==2:categories.append('manual_target')
                for cat in categories:
                    candidate=cat in ('near_chance_or_worse','anticipatory') and scope not in ('PRACTICE','calibration_tail')
                    valid=m['assessable'];difference=None
                    if cat in ('calibration_target','manual_target'):
                        valid=valid and target[pid] is not None
                        difference=100*(m['accuracy']-target[pid]) if valid else None
                        flag=valid and abs(difference)>cfg[cat+'_band_pp']+1e-10
                    elif cat=='near_chance_or_worse':flag=valid and m['accuracy']<=cfg['chance_accuracy_max']+1e-12
                    else:flag=valid and m['n_fast']>=cfg['fast_count_min'] and m['fast_fraction']>=cfg['fast_fraction_min']-1e-12 and m['fast_accuracy'] is not None and m['fast_accuracy']<=cfg['fast_accuracy_max']+1e-12
                    checks.append(dict(participant_id=pid,scope=scope,decision=d,category=cat,use_for_exclusion=candidate,status='not_assessable' if not valid else 'flag' if flag else 'clear',target_difference_pp=difference))
        for code,idx in indices.items():
            n=60 if code=='PRACTICE' else 260;size=cfg['practice_bin_size' if code=='PRACTICE' else 'main_bin_size']
            for start in range(1,n+1,size):
                end=min(start+size-1,n)
                selected=[r for r in rr if r['condition_code']==code and start<=int(r['trial'])<=end]
                for d in (1,2):
                    b=score(selected,d,pid,{(idx,t) for t in range(start,end+1)},cfg,practice_invalid[pid] if code=='PRACTICE' else False)
                    b.update(meta[pid]);b.update(trial_start=start,trial_end=end,condition_trial_midpoint=(start+end)/2,trial_midpoint=(0 if idx==0 else 60+(idx-1)*260)+(start+end)/2)
                    bins[pid,code,d,start]=b
    exported=read(report/'scope_metrics.csv');assert len(exported)==len(metrics)
    for row in exported:
        key=(int(row['participant_id']),row['scope'],int(row['decision']))
        for k,v in metrics[key].items():near(row[k],v,(key,k))
    exported_bins=read(report/'trial_bin_summary.csv');assert len(exported_bins)==len(bins)
    for row in exported_bins:
        key=(int(row['participant_id']),row['scope'],int(row['decision']),int(row['trial_start']))
        for k,v in bins[key].items():near(row[k],v,(key,k))
    exported_checks=read(report/'screening_results.csv');assert len(exported_checks)==len(checks)
    keyed={(int(c['participant_id']),c['scope'],int(c['decision']),c['category']):c for c in exported_checks}
    assert len(keyed)==len(checks)
    for c in checks:
        row=keyed[c['participant_id'],c['scope'],c['decision'],c['category']]
        for k,v in c.items():near(row[k],v,(c,k))
    flags=[c for c in checks if c['status']=='flag']
    assert read(report/'flagged_checks.csv')==[c for c in exported_checks if c['status']=='flag']
    assert read(report/'integrity_issues.csv')==[m for m in exported if not boolean(m['assessable'])]
    candidates=sorted({c['participant_id'] for c in flags if c['use_for_exclusion']})
    anticipatory={c['participant_id'] for c in flags if c['use_for_exclusion'] and c['category']=='anticipatory'}
    incomplete={c['participant_id'] for c in checks if c['status']=='not_assessable'}
    diagnostic=sorted({c['participant_id'] for c in flags if not c['use_for_exclusion']}-set(candidates))
    summary=json.loads((report/'exclusion_summary.json').read_text())
    assert summary['participant_ids']==candidates and summary['unique_flagged_n']==len(candidates)
    assert summary['remaining_if_applied_n']==len(people)-len(candidates) and summary['exclusions_applied'] is False
    near(summary['percentage'],100*len(candidates)/len(people),'percentage')
    assert summary['diagnostic_only_participant_ids']==diagnostic and summary['incomplete_assessment_ids']==sorted(incomplete)
    rule_ids={cat:sorted({c['participant_id'] for c in flags if c['use_for_exclusion'] and c['category']==cat}) for cat in ('near_chance_or_worse','anticipatory')}
    assert summary['rule_participant_ids']==rule_ids and summary['rule_counts']=={k:len(v) for k,v in rule_ids.items()}
    overlaps=Counter(sum(p in ids for ids in rule_ids.values()) for p in candidates)
    assert summary['overlap_counts']=={str(k):v for k,v in overlaps.items()}
    expected_table=[dict(criterion=k,count=str(len(v)),participant_ids=';'.join(f'{p:03d}' for p in v)) for k,v in rule_ids.items()]
    expected_table += [dict(criterion=label,count=str(len(ids)),participant_ids=';'.join(f'{p:03d}' for p in ids)) for label,ids in (
        ('Unique candidate union',candidates),('Diagnostic only',diagnostic),('Incomplete assessment',sorted(incomplete)))]
    assert read(report/'exclusion_summary_table.csv')==expected_table
    summaries=read(report/'participant_summary.csv');assert len(summaries)==len(people)
    for s in summaries:
        pid=int(s['participant_id']);cc=[c for c in checks if c['participant_id']==pid]
        for k,v in meta[pid].items():near(s[k],v,(pid,k))
        for k,v in dict(exclude_by_rules=pid in candidates,diagnostic_only=pid in diagnostic,n_exclusion_rules=sum(pid in ids for ids in rule_ids.values()),n_not_assessable=sum(c['status']=='not_assessable' for c in cc),n_trials=len(people[pid]),n_main_trials=sum(r['condition_code'] in MAIN for r in people[pid])).items():near(s[k],v,(pid,k))
        for key,use in (('exclusion_reasons',True),('diagnostic_reasons',False)):
            expected={f"{c['category']}:{c['scope']}:D{c['decision']}" for c in cc if c['status']=='flag' and c['use_for_exclusion']==use}
            assert set(filter(None,s[key].split('; ')))==expected
    assert read(report/'exclusion_candidates.csv')==[s for s in summaries if boolean(s['exclude_by_rules'])]
    pools=json.loads((report/'reference_pools.json').read_text())
    expected_pools={str(pid):sorted(p for p in people if p!=pid and target[pid] is not None and target[p]==target[pid] and p not in anticipatory|incomplete) for pid in people}
    assert pools==expected_pools
    bands=read(report/'reference_bands.csv');assert len(bands)==len(bins)
    for row in bands:
        pid=int(row['participant_id']);code=row['scope'];d=int(row['decision']);start=int(row['trial_start'])
        near(row['peer_n'],len(pools[str(pid)]),'peer count')
        for key in ('accuracy','fast_fraction','fast_accuracy'):
            values=[bins[p,code,d,start][key] for p in pools[str(pid)] if bins[p,code,d,start][key] is not None]
            near(row[f'{key}_peer_n'],len(values),'nonempty peer count')
            quantiles=np.quantile(values,[.25,.5,.75],method='linear') if values else [None]*3
            for name,v in zip(('q25','median','q75'),quantiles):near(row[f'{key}_{name}'],v,'peer quantile')
    comparisons=json.loads((report/'comparator_selection.json').read_text())
    assert sorted(p for match in comparisons for p in match['targets'])==sorted(anticipatory)
    for match in comparisons:
        group=(match['calibration_target_accuracy'],match['key_black'],match['key_white'])
        pool=[pid for pid in sorted(people) if group[0] is not None and (target[pid],meta[pid]['key_black'],meta[pid]['key_white'])==group and pid not in anticipatory|incomplete and all(metrics[pid,'experimental_overall',d]['median_positive_rt_ms'] is not None for d in (1,2))]
        values={p:{f'D{d}_{k}':metrics[p,'experimental_overall',d][k] for d in (1,2) for k in ('accuracy','median_positive_rt_ms')} for p in pool}
        centers={k:statistics.median(values[p][k] for p in pool) for k in next(iter(values.values()),{})}
        scales={k:statistics.median(abs(values[p][k]-centers[k]) for p in pool) for k in centers}
        ranked=sorted(((sum(abs(values[p][k]-centers[k])/scales[k] for k in centers if scales[k]>1e-12),p) for p in pool))
        assert match['comparator']==(ranked[0][1] if ranked else None)
        assert [r['participant_id'] for r in match['ranked_candidates']]==[p for _,p in ranked]
        for row,(value,pid) in zip(match['ranked_candidates'],ranked):near(row['score'],value,'comparator score')
        for k in centers:near(match['centers'][k],centers[k],'center');near(match['median_absolute_deviations'][k],scales[k],'MAD')
    plot_sources=json.loads((report/'plot_sources.json').read_text())
    assert sorted(p['participant_id'] for p in plot_sources['individuals'])==sorted(people)
    assert sorted(p['participant_id'] for p in plot_sources['comparison_pages'])==sorted(anticipatory)
    assert len(plot_sources['cohort'])==4
    pdfs=list(report.rglob('*.pdf'));pngs=list(report.rglob('*.png'))
    assert len(pdfs)==len(pngs)==4+len(people)+len(anticipatory)
    for p in pdfs:
        info=subprocess.check_output(['pdfinfo',str(p)],text=True)
        assert next(int(line.split(':',1)[1]) for line in info.splitlines() if line.startswith('Pages:'))==1,p
        assert p.with_suffix('.png').exists()
    for p in pngs:
        with Image.open(p) as im:assert im.width>=2000 and im.height>=900;im.verify()
    html=Links();html.feed((report/'index.html').read_text())
    for link in html.targets:assert (report/link).is_file(),link
    if args.expect_current_cohort:
        assert len(people)==60 and len(raw)==46800 and manifest['practice_trial_n']==3600
        assert candidates==[10,13,20,22,27] and rule_ids['near_chance_or_worse']==[22,27]
        assert not incomplete and len(diagnostic)==18
        assert meta[59]['run_timestamp']=='20260924_110831' and 59 not in candidates
    receipt=dict(validated_utc=datetime.now(timezone.utc).isoformat(),report=str(report),report_manifest_sha256=digest(report/'manifest.json'),validator_sha256=digest(__file__),
        independent_recomputation=True,participant_n=len(people),scope_metric_n=len(metrics),bin_n=len(bins),screening_check_n=len(checks),candidate_ids=candidates,
        pdf_n=len(pdfs),png_n=len(pngs),html_link_n=len(html.targets),current_snapshot_checked=args.expect_current_cohort,visual_review='Separate rendered inspection required')
    if args.receipt:
        args.receipt.parent.mkdir(parents=True,exist_ok=True);args.receipt.write_text(json.dumps(receipt,indent=2)+'\n')
    print(json.dumps(receipt,indent=2))


if __name__=='__main__':main()
