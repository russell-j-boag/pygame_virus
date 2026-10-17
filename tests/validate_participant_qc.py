#!/usr/bin/env python3
"""Independent raw-source reconciliation and artifact checks for a QC report.

This validator does not import the production QC implementation. It writes no
study artifacts; redirect stdout to save the validation receipt.
"""
import argparse
import csv
import hashlib
import json
import math
import os
import sys
import re
import statistics
import subprocess
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote

from PIL import Image

ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT))
os.environ.setdefault("PYGAME_HIDE_SUPPORT_PROMPT","1")
from python import virus_task as task

CATEGORIES=["calibration_target","manual_target","near_chance_or_worse","anticipatory"]
EXCLUSION_CATEGORIES=["calibration_target","near_chance_or_worse","anticipatory"]


def read(path):
    with Path(path).open(newline="") as handle:return list(csv.DictReader(handle))


def number(value):
    try:
        x=float(value)
        return x if math.isfinite(x) else None
    except (ValueError,TypeError):return None


def near(actual,expected):
    a=number(actual)
    if expected is None:assert a is None,(actual,expected)
    else:assert a is not None and math.isclose(a,expected,abs_tol=1e-10,rel_tol=1e-10),(a,expected)


class Links(HTMLParser):
    def __init__(self):super().__init__();self.paths=[];self.ids=set()
    def handle_starttag(self,tag,attrs):
        attrs=dict(attrs)
        self.paths.extend(attrs[k] for k in ("href","src") if k in attrs)
        if "id" in attrs:self.ids.add(attrs["id"])


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir",type=Path,default=ROOT/"analysis_outputs/semester2_2026_qc")
    parser.add_argument("--expect-current-cohort",action="store_true",help="Assert the first 87-participant snapshot, not a rule for future cohorts")
    args=parser.parse_args();out=args.output_dir.resolve()
    manifest=json.loads((out/"manifest.json").read_text());cfg=json.loads((out/"config_snapshot.json").read_text())
    assert manifest["generator"]=="participant_qc" and manifest["qc_data_filtered"] is False
    assert (out/"COMPLETE.txt").exists()
    source=Path(manifest["input_csv"])
    digest=lambda p:hashlib.sha256(Path(p).read_bytes()).hexdigest()
    assert digest(source)==manifest["input_sha256"]
    assert digest(manifest["config_path"])==manifest["config_sha256"]
    for item in manifest["sources"]:assert digest(item["path"])==item["sha256"],item["path"]
    for item in manifest["outputs"]:
        path=out/item["path"]
        assert path.is_relative_to(out) and path.stat().st_size==item["bytes"] and digest(path)==item["sha256"],path
    assert {str(p.relative_to(out)) for p in out.rglob("*") if p.is_file() and p.name not in ("manifest.json","COMPLETE.txt")}=={r["path"] for r in manifest["outputs"]}
    raw=read(source);by_id=defaultdict(dict)
    for r in raw:
        pid,t=int(r["participant_id"]),int(r["global_trial"])
        assert t not in by_id[pid]
        by_id[pid][t]=r
    summaries=read(out/"participant_summary.csv");metrics=read(out/"scope_metrics.csv");checks=read(out/"screening_results.csv")
    assert {int(s["participant_id"]) for s in summaries}==set(by_id)
    assert len(summaries)==len(by_id)==manifest["participant_n"]
    assert len(raw)==manifest["trial_n"]
    assert manifest["study"]=="time_pressure"
    corrections={(int(r["source_participant_id"]),r["run_timestamp"]):int(r["participant_id"])
                 for r in read(manifest["identity_corrections"])}
    bounds={}; row_issues={}
    for pid, trials in by_id.items():
        first=next(iter(trials.values()));source_id=int(first["source_participant_id"])
        assert pid==corrections.get((source_id,first["run_timestamp"]),source_id)
        allocation=task.counterbalance_allocation_for_participant(source_id)
        mapping=task.key_mapping_for_participant(source_id)
        lo=1;bs={}; metadata={}
        for idx,block in enumerate(task.build_blocks_for_participant(source_id,task.BLOCKS),1):
            hi=lo+block["N_TRIALS"]-1;code=block["CONDITION_DEADLINE_CODE"]
            bs[code]=set(range(lo,hi+1))
            for t in range(lo,hi+1):metadata[t]=(idx,lo,block)
            lo=hi+1
        bs["aided_overall"]=bs["A_HP"]|bs["A_LP"]
        bs["experimental_overall"]=set(range(301,1901))
        bs["calibration_tail"]=set(range(301-cfg["calibration_tail_n"],301))
        bounds[pid]=bs
        bad=set()
        for t,r in trials.items():
            assert int(r["source_participant_id"])==source_id and r["run_timestamp"]==first["run_timestamp"]
            if t not in metadata:bad.add(t);continue
            idx,start,block=metadata[t]
            def boolean(v):return {"true":1,"false":0,"1":1,"0":0}.get(str(v).lower())
            def empty(v):return v in ("","NA","NaN")
            ok=(int(r["block_idx"])==idx and int(r["trial"])==t-start+1
                and r["block"]==block["name"] and r["condition_deadline_code"]==block["CONDITION_DEADLINE_CODE"]
                and r["time_pressure_condition"]==block["TIME_PRESSURE_CONDITION"]
                and number(r["trial_deadline_s"])==block["TRIAL_DEADLINE_MS"]/1000
                and boolean(r["auto_on"])==int(block["AUTOMATION_ON"])
                and r["automation_reliability_pattern"]==allocation["reliability_pattern"]
                and r["automation_reliability_group"]==block["AUTOMATION_RELIABILITY_GROUP"]
                and boolean(r["keymap_flip"])==int(allocation["key_flip"])
                and r["key_black"]==mapping["key_black_name"] and r["key_white"]==mapping["key_white_name"]
                and r["stimulus"] in ("BLACK","WHITE"))
            if block["AUTOMATION_ON"]:
                ok=ok and number(r["aid_accuracy_setting"])==block["AID_ACCURACY"] and r["aid_label"] in ("BLACK","WHITE") and boolean(r["aid_correct"])==int(r["aid_label"]==r["stimulus"])
            else:ok=ok and all(empty(r[k]) for k in ("aid_accuracy_setting","aid_label","aid_correct"))
            if r["response"] in ("BLACK","WHITE"):
                ok=ok and number(r["rt_s"]) is not None and boolean(r["correct"])==int(r["response"]==r["stimulus"]) and r["feedback"]==("CORRECT" if r["response"]==r["stimulus"] else "INCORRECT")
            elif r["response"]=="TIMEOUT":
                ok=ok and (empty(r["correct"]) or boolean(r["correct"])==0) and r["feedback"]=="TOO SLOW" and (empty(r["rt_s"]) or number(r["rt_s"]) is not None)
            else:ok=False
            if not ok:bad.add(t)
        row_issues[pid]=bad
    expected={};values_checked=0

    def reconcile(m,selected):
        nonlocal values_checked
        pid=int(m["participant_id"]);rr=[by_id[pid][t] for t in sorted(selected) if t in by_id[pid]]
        first=next(iter(by_id[pid].values()))
        for key in ("source_participant_id","run_timestamp","automation_reliability_pattern","key_black","key_white"):
            assert m[key]==first[key]
        allocation=task.counterbalance_allocation_for_participant(int(first["source_participant_id"]))
        assert m["main_block_order"]=="|".join(f"{name[0]}_{pressure}" for pressure,name in allocation["post_calibration_block_order"])
        assert int(m["n_trials"])==len(rr) and int(m["n_expected"])==len(selected)
        assessable=(set(by_id[pid]) >= selected and not row_issues[pid].intersection(selected)
                    and not (set(by_id[pid])-set(range(1,1901))))
        assert m["assessable"]==str(assessable)
        if m["assessable"]!="True":
            assert m["integrity_issues"] and m["accuracy"]==m["fast_accuracy"]==""
            return None
        assert len(rr)==len(selected)
        answered=[r for r in rr if r["response"] in ("BLACK","WHITE")]
        assert all(number(r["rt_s"]) is not None for r in answered)
        assert all(r["response"] in ("BLACK","WHITE","TIMEOUT") for r in rr)
        fast=[r for r in answered if number(r["rt_s"])<cfg["fast_rt_seconds"]]
        correct=sum(r["response"]==r["stimulus"] for r in rr)
        fast_correct=sum(r["response"]==r["stimulus"] for r in fast)
        n,nf=len(rr),len(fast)
        values=dict(n_correct=correct,accuracy=correct/n,n_answered=len(answered),n_timeouts=n-len(answered),
            n_fast=nf,n_fast_correct=fast_correct,fast_fraction=nf/n,fast_accuracy=fast_correct/nf if nf else None,
            n_nonpositive_rt=sum(number(r["rt_s"])<=0 for r in answered),n_positive_fast=sum(number(r["rt_s"])>0 for r in fast))
        target=task.CALIBRATION_TARGET_ACCURACY
        values.update(answered_accuracy=correct/len(answered) if answered else None,timeout_fraction=(n-len(answered))/n,
            n_timeouts_with_recorded_rt=sum(r["response"]=="TIMEOUT" and number(r["rt_s"]) is not None for r in rr),
            n_beyond_deadline=sum(number(r["rt_s"])>float(r["trial_deadline_s"]) for r in answered))
        values.update(target_difference_pp=100*(correct/n-target))
        positive=[number(r["rt_s"]) for r in answered if number(r["rt_s"])>0]
        values["median_positive_rt_ms"]=1000*statistics.median(positive) if positive else None
        for ms in cfg["sensitivity_thresholds_ms"]:values[f"fraction_rt_lt_{ms}ms"]=sum(number(r["rt_s"])<ms/1000 for r in answered)/n
        z=statistics.NormalDist().inv_cdf((1+cfg["interval_level"])/2)
        for prefix,k,total in [("accuracy",correct,n),("fast_accuracy",fast_correct,nf)]:
            if total:
                center=(k+z*z/2)/(total+z*z)
                radius=z*math.sqrt(k*(total-k)/total+z*z/4)/(total+z*z)
                values[prefix+"_lower"]=max(0,center-radius);values[prefix+"_upper"]=min(1,center+radius)
            else:values[prefix+"_lower"]=values[prefix+"_upper"]=None
        for key,value in values.items():near(m[key],value);values_checked+=1
        return values

    assert len(metrics)==8*len(by_id)
    for m in metrics:
        selected=bounds[int(m["participant_id"])][m["scope"]]
        assert int(m["trial_start"])==min(selected) and int(m["trial_end"])==max(selected)
        key=(int(m["participant_id"]),m["scope"])
        assert key not in expected
        expected[key]=reconcile(m,selected)
    bins=read(out/"trial_bin_summary.csv");bin_lookup={}
    assert len(bins)==1900//cfg["bin_size"]*len(by_id)
    for b in bins:
        lo,hi=int(b["trial_start"]),int(b["trial_end"])
        assert (lo-1)%cfg["bin_size"]==0 and hi-lo+1==cfg["bin_size"]
        pid=int(b["participant_id"]);start=min(bounds[pid][b["scope"]])
        assert int(b["condition_trial_start"])==lo-start+1
        near(b["condition_trial_midpoint"],(lo+hi)/2-start+1)
        near(b["trial_midpoint"],(lo+hi)/2)
        key=(pid,b["scope"],float(b["condition_trial_midpoint"]))
        assert key not in bin_lookup
        bin_lookup[key]=reconcile(b,set(range(lo,hi+1)))
    flags=defaultdict(list);statuses=defaultdict(list)
    assert len(checks)==16*len(by_id)
    wanted={(pid,scope,category) for pid in by_id for scope in bounds[pid]
            for category in (["calibration_target"] if scope=="calibration_tail" else
                             ["near_chance_or_worse","anticipatory"]+(["manual_target"] if scope=="M_LP" else []))}
    assert {(int(c["participant_id"]),c["scope"],c["category"]) for c in checks}==wanted
    for c in checks:
        pid,scope,category=int(c["participant_id"]),c["scope"],c["category"]
        assert category in CATEGORIES
        assert (category=="calibration_target")== (scope=="calibration_tail")
        if category=="manual_target":assert scope=="M_LP"
        assert c["use_for_exclusion"]==str(category in EXCLUSION_CATEGORIES)
        e=expected[pid,scope]
        if e is None:want="not_assessable"
        else:
            a=e["accuracy"]
            if category in ("calibration_target","manual_target"):
                flag=abs(e["target_difference_pp"])>cfg[category+"_band_pp"]+1e-10
            elif category=="near_chance_or_worse":flag=a<=cfg["chance_accuracy_max"]+1e-10
            else:flag=e["fast_fraction"]>=cfg["fast_fraction_min"]-1e-10 and e["n_fast"]>=cfg["fast_count_min"] and e["fast_accuracy"] is not None and e["fast_accuracy"]<=cfg["fast_accuracy_max"]+1e-10
            want="flag" if flag else "clear"
        assert c["status"]==want,(pid,scope,category,c["status"],want)
        statuses[pid,category].append(want)
        if want=="flag":flags[category].append(pid)
    for s in summaries:
        person_checks=[c for c in checks if c["participant_id"]==s["participant_id"]]
        assert int(s["n_flags"])==sum(c["status"]=="flag" for c in person_checks)
        assert int(s["n_not_assessable"])==sum(c["status"]=="not_assessable" for c in person_checks)
        assert s["review_required"]==str(any(c["status"]!="clear" for c in person_checks))
        assert "manual_aid_ordering" not in s
        for category in CATEGORIES:
            st=statuses[int(s["participant_id"]),category]
            want="flag" if "flag" in st else "not_assessable" if "not_assessable" in st else "clear"
            assert s[category]==want
        n_rules=sum(s[c]=="flag" for c in CATEGORIES)
        assert int(s["n_flagged_rules"])==n_rules
        assert s["flagged_any_rule"]==str(n_rules>0)
        n_exclusions=sum(s[c]=="flag" for c in EXCLUSION_CATEGORIES)
        assert int(s["n_exclusion_rules"])==n_exclusions
        assert s["exclude_by_rules"]==str(n_exclusions>0)
        expected_reasons="; ".join(f"{c['category']}:{c['scope']}" for c in checks if c["participant_id"]==s["participant_id"] and c["status"]=="flag" and c["category"] in EXCLUSION_CATEGORIES)
        assert s["exclusion_reasons"]==expected_reasons
    assert read(out/"flagged_checks.csv")==[c for c in checks if c["status"]=="flag"]
    issues=read(out/"integrity_issues.csv")
    assert len(issues)==manifest["integrity_scope_n"]
    assert {(r["participant_id"],r["scope"]) for r in issues}=={(m["participant_id"],m["scope"]) for m in metrics if m["assessable"]!="True"}
    union=sorted(set(pid for c in EXCLUSION_CATEGORIES for pid in flags[c]))
    candidates=read(out/"exclusion_candidates.csv")
    assert candidates==[s for s in summaries if int(s["participant_id"]) in union]
    exclusions=json.loads((out/"exclusion_summary.json").read_text())
    assert exclusions["participant_ids"]==union
    assert exclusions["participant_n"]==len(by_id)
    assert exclusions["unique_flagged_n"]==manifest["unique_flagged_n"]==len(union)
    assert exclusions["remaining_if_applied_n"]==manifest["remaining_if_applied_n"]==len(by_id)-len(union)
    near(exclusions["unique_flagged_percent"],100*len(union)/len(by_id))
    assert exclusions["exclusions_applied"] == manifest["exclusions_applied"]
    assert exclusions["qc_data_filtered"] is False
    application=exclusions["primary_analysis"]
    assert application==manifest["primary_analysis"]
    if application:
        selection=read(out/"analysis_cohort_selection.csv")
        assert {int(r["participant_id"]) for r in selection}==set(by_id)
        excluded_ids=sorted(int(r["participant_id"]) for r in selection if r["included"].lower()=="false")
        included_ids={int(r["participant_id"]) for r in selection if r["included"].lower()=="true"}
        assert application["excluded_participant_ids"]==excluded_ids
        assert application["included_participant_n"]==len(included_ids)
        assert application["source_participant_n"]==len(by_id)
        assert {int(r["participant_id"]) for r in read(Path(application["analysis_directory"])/"cohort.csv")}==included_ids
        assert exclusions["exclusions_applied"]==bool(excluded_ids)
    cases=exclusions["identity_reuse_cases"]
    assert cases==manifest["identity_reuse_cases"]
    expected_reused=defaultdict(list)
    for s in summaries:expected_reused[int(s["source_participant_id"])].append(s)
    expected_reused={k:v for k,v in expected_reused.items() if len(v)>1}
    assert {c["source_participant_id"] for c in cases}==set(expected_reused)
    for c in cases:
        ss=expected_reused[c["source_participant_id"]]
        assert c["analysis_participant_ids"]==sorted(int(s["participant_id"]) for s in ss)
        assert {(r["participant_id"],r["run_timestamp"]) for r in c["runs"]}=={(int(s["participant_id"]),s["run_timestamp"]) for s in ss}
        assert all(r["allocation_source_participant_id"]==c["source_participant_id"] for r in c["runs"])
    identity_rows=read(out/"identity_corrections_summary.csv")
    assert len(identity_rows)==sum(len(c["runs"]) for c in cases)
    summary_text=(out/"exclusion_summary.txt").read_text()
    for c in cases:
        for r in c["runs"]:assert r["run_timestamp"] in summary_text and f"analysis ID {r['participant_id']:03d}" in summary_text
    assert exclusions["exclusion_categories"]==manifest["exclusion_categories"]==EXCLUSION_CATEGORIES
    assert exclusions["diagnostic_categories"]==manifest["diagnostic_categories"]==["manual_target"]
    assert exclusions["rule_counts"]==manifest["exclusion_rule_counts"]=={c:len(set(flags[c])) for c in EXCLUSION_CATEGORIES}
    assert manifest["flag_counts"]=={c:len(set(flags[c])) for c in CATEGORIES}
    assert exclusions["diagnostic_rule_counts"]=={"manual_target":len(set(flags["manual_target"]))}
    assert exclusions["rule_participant_ids"]=={c:sorted(set(flags[c])) for c in CATEGORIES}
    diagnostic_only=sorted(set(flags["manual_target"])-set(union))
    assert exclusions["diagnostic_only_participant_ids"]==diagnostic_only
    assert exclusions["diagnostic_only_n"]==len(diagnostic_only)
    assert exclusions["incomplete_assessment_ids"]==[int(s["participant_id"]) for s in summaries if int(s["n_not_assessable"])]
    assert len(exclusions["overlap"])==len(EXCLUSION_CATEGORIES)
    for k,r in enumerate(exclusions["overlap"],1):
        ids=[int(s["participant_id"]) for s in summaries if sum(s[c]=="flag" for c in EXCLUSION_CATEGORIES)==k]
        assert r==dict(n_rules=k,participant_n=len(ids),participant_ids=ids)
    table=read(out/"exclusion_summary_table.csv")
    assert [r["criterion"] for r in table]==EXCLUSION_CATEGORIES+["manual_target","any_exclusion_rule","remaining_if_applied"]
    for row in table:
        category=row["criterion"]
        ids=sorted(set(flags[category])) if category in CATEGORIES else union if category=="any_exclusion_rule" else []
        n=len(by_id)-len(union) if category=="remaining_if_applied" else len(ids)
        role="Exclusion criterion" if category in EXCLUSION_CATEGORIES else "Diagnostic only" if category=="manual_target" else "Total"
        assert row["role"]==role and int(row["participant_n"])==n
        assert row["participant_ids"]=="; ".join(f"{p:03d}" for p in ids)
        near(row["percent_of_cohort"],100*n/len(by_id))
    pools=json.loads((out/"reference_pools.json").read_text())
    for s in summaries:
        pid=int(s["participant_id"])
        want=[int(q["participant_id"]) for q in summaries if q["participant_id"]!=s["participant_id"] and q["automation_reliability_pattern"]==s["automation_reliability_pattern"] and q["anticipatory"]=="clear" and int(q["n_not_assessable"])==0]
        assert pools[str(pid)]==want
    # NumPy's default linear quantile, independently implemented for peer bands.
    def quantile(values,p):
        values=sorted(values);position=(len(values)-1)*p;lo=math.floor(position);hi=math.ceil(position)
        return values[lo]+(position-lo)*(values[hi]-values[lo])
    bands=read(out/"reference_bands.csv")
    assert len(bands)==len(bins)
    assert {(int(b["participant_id"]),b["scope"],float(b["condition_trial_midpoint"])) for b in bands}==set(bin_lookup)
    for row in bands:
        pid=int(row["participant_id"]);mid=float(row["condition_trial_midpoint"]);scope=row["scope"]
        assert int(row["peer_n"])==len(pools[str(pid)])
        for key in ("accuracy","fast_fraction"):
            values=[100*bin_lookup[p,scope,mid][key] for p in pools[str(pid)] if bin_lookup[p,scope,mid] is not None]
            for name,q in [("q25",.25),("median",.5),("q75",.75)]:near(row[key+"_"+name],quantile(values,q) if values else None)
    matches=json.loads((out/"comparator_selection.json").read_text())
    assert sorted(p for m in matches for p in m["targets"])==sorted(set(flags["anticipatory"]))
    for m in matches:
        assert m["targets"]==[int(s["participant_id"]) for s in summaries if s["anticipatory"]=="flag" and s["automation_reliability_pattern"]==m["group"] and s["key_black"]==m["key_black"] and s["key_white"]==m["key_white"]]
        candidates=[int(s["participant_id"]) for s in summaries if s["automation_reliability_pattern"]==m["group"] and s["key_black"]==m["key_black"] and s["key_white"]==m["key_white"] and s["anticipatory"]!="flag" and int(s["n_not_assessable"])==0 and expected[int(s["participant_id"]),"experimental_overall"]["median_positive_rt_ms"] is not None]
        distances={p:0 for p in candidates}
        for metric in ("accuracy","median_positive_rt_ms"):
            if not candidates:continue
            values={p:expected[p,"experimental_overall"][metric] for p in candidates}
            center=statistics.median(values.values());mad=statistics.median(abs(v-center) for v in values.values())
            if mad>1e-12:
                for p,v in values.items():distances[p]+=abs(v-center)/mad
        ranked=sorted(candidates,key=lambda p:(distances[p],p))
        assert m["comparator"]==(ranked[0] if ranked else None)
        assert [r["participant_id"] for r in m["ranked_candidates"]]==ranked
        for r in m["ranked_candidates"]:near(r["score"],distances[r["participant_id"]])
    links=Links();links.feed((out/"index.html").read_text())
    for link in links.paths:
        if link.startswith("#"):assert link[1:] in links.ids
        else:assert (out/unquote(link)).is_file(),link
    plots=json.loads((out/"plot_sources.json").read_text())
    assert [p["participant_id"] for p in plots["individuals"]]==sorted(by_id)
    assert sorted(p for page in plots["comparison_pages"] for p in page["targets"])==sorted(set(flags["anticipatory"]))
    expected_pairs=len(by_id)+4+len(plots["comparison_pages"])
    pdfs=sorted(out.rglob("*.pdf"));pngs=sorted(out.rglob("*.png"))
    assert len(pngs)==expected_pairs
    assert len(pdfs)==expected_pairs+1+bool(plots["comparison_pages"])
    def pdfcheck(path):
        info=subprocess.run(["pdfinfo",str(path)],check=True,capture_output=True,text=True).stdout
        pages=int(re.search(r"^Pages:\s+(\d+)",info,re.M).group(1))
        want=len(by_id) if path.name=="all_individual_diagnostics.pdf" else len(plots["comparison_pages"]) if path.name=="anticipatory_comparisons.pdf" else 1
        assert pages==want,(path,pages,want)
    with ThreadPoolExecutor(max_workers=6) as pool:list(pool.map(pdfcheck,pdfs))
    for path in pngs:
        with Image.open(path) as im:
            assert im.width>=1000 and im.height>=1000
            im.verify()
    flags={c:sorted(set(flags[c])) for c in CATEGORIES}
    if args.expect_current_cohort:
        assert len(by_id)==87 and len(raw)==165300 and not read(out/"integrity_issues.csv")
        assert set(by_id)==set(range(1,88))
        assert next(s for s in summaries if s["participant_id"]=="84")["source_participant_id"]=="83"
    print(json.dumps(dict(status="passed",participants=len(by_id),numeric_values_checked=values_checked,
        checks_verified=len(checks),pdfs_verified=len(pdfs),pngs_verified=len(pngs),flagged_participants=flags,
        unique_flagged_n=len(union),unique_participant_ids=union,diagnostic_only_participant_ids=diagnostic_only,
        validation="Raw-source metrics, Wilson intervals, all screens/bins, unique exclusions and overlaps, peer bands, comparator selection, provenance, HTML links, PDF/PNG integrity"),indent=2))


if __name__=="__main__":main()
