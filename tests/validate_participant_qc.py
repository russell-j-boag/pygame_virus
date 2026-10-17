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
    parser.add_argument("--expect-current-cohort",action="store_true",help="Assert the approved 89-participant snapshot, not a rule for future cohorts")
    args=parser.parse_args();out=args.output_dir.resolve()
    manifest=json.loads((out/"manifest.json").read_text());cfg=json.loads((out/"config_snapshot.json").read_text())
    assert manifest["generator"]=="participant_qc" and manifest["exclusions_applied"] is False
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
    bounds={"calibration_full":(1,300),"manual_pre":(301,500),"P1_95":(501,900),"P2_70":(901,1300),
            "P3_95":(1301,1700),"manual_post":(1701,1900),"aided_overall":(501,1700),
            "experimental_overall":(301,1900),"calibration_tail":(301-cfg["calibration_tail_n"],300)}
    expected={};values_checked=0

    def reconcile(m,lo,hi):
        nonlocal values_checked
        pid=int(m["participant_id"]);rr=[by_id[pid][t] for t in range(lo,hi+1) if t in by_id[pid]]
        assert int(m["n_trials"])==len(rr) and int(m["n_expected"])==hi-lo+1
        if m["assessable"]!="True":
            assert m["integrity_issues"] and m["accuracy"]==m["fast_accuracy"]==""
            return None
        assert len(rr)==hi-lo+1
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
        target=float(rr[0]["calibration_target_accuracy"])
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

    assert len(metrics)==9*len(by_id)
    for m in metrics:
        lo,hi=bounds[m["scope"]]
        assert int(m["trial_start"])==lo and int(m["trial_end"])==hi
        key=(int(m["participant_id"]),m["scope"])
        assert key not in expected
        expected[key]=reconcile(m,lo,hi)
    bins=read(out/"trial_bin_summary.csv");bin_lookup={}
    assert len(bins)==1900//cfg["bin_size"]*len(by_id)
    for b in bins:
        lo,hi=int(b["trial_start"]),int(b["trial_end"])
        assert (lo-1)%cfg["bin_size"]==0 and hi-lo+1==cfg["bin_size"]
        bin_lookup[int(b["participant_id"]),float(b["trial_midpoint"])]=reconcile(b,lo,hi)
    flags=defaultdict(list);statuses=defaultdict(list)
    assert len(checks)==19*len(by_id)
    for c in checks:
        pid,scope,category=int(c["participant_id"]),c["scope"],c["category"]
        assert category in CATEGORIES
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
    union=sorted(set(pid for c in EXCLUSION_CATEGORIES for pid in flags[c]))
    candidates=read(out/"exclusion_candidates.csv")
    assert candidates==[s for s in summaries if int(s["participant_id"]) in union]
    exclusions=json.loads((out/"exclusion_summary.json").read_text())
    assert exclusions["participant_ids"]==union
    assert exclusions["participant_n"]==len(by_id)
    assert exclusions["unique_flagged_n"]==manifest["unique_flagged_n"]==len(union)
    assert exclusions["remaining_if_applied_n"]==manifest["remaining_if_applied_n"]==len(by_id)-len(union)
    near(exclusions["unique_flagged_percent"],100*len(union)/len(by_id))
    assert exclusions["exclusions_applied"] is False
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
        want=[int(q["participant_id"]) for q in summaries if q["participant_id"]!=s["participant_id"] and q["calibration_target_group"]==s["calibration_target_group"] and q["anticipatory"]=="clear" and int(q["n_not_assessable"])==0]
        assert pools[str(pid)]==want
    # NumPy's default linear quantile, independently implemented for peer bands.
    def quantile(values,p):
        values=sorted(values);position=(len(values)-1)*p;lo=math.floor(position);hi=math.ceil(position)
        return values[lo]+(position-lo)*(values[hi]-values[lo])
    for row in read(out/"reference_bands.csv"):
        pid=int(row["participant_id"]);mid=float(row["trial_midpoint"])
        for key in ("accuracy","fast_fraction"):
            values=[100*bin_lookup[p,mid][key] for p in pools[str(pid)] if bin_lookup[p,mid] is not None]
            for name,q in [("q25",.25),("median",.5),("q75",.75)]:near(row[key+"_"+name],quantile(values,q) if values else None)
    matches=json.loads((out/"comparator_selection.json").read_text())
    for m in matches:
        candidates=[int(s["participant_id"]) for s in summaries if s["calibration_target_group"]==m["group"] and s["key_black"]==m["key_black"] and s["key_white"]==m["key_white"] and s["anticipatory"]!="flag" and int(s["n_not_assessable"])==0 and expected[int(s["participant_id"]),"experimental_overall"]["median_positive_rt_ms"] is not None]
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
    links=Links();links.feed((out/"index.html").read_text())
    for link in links.paths:
        if link.startswith("#"):assert link[1:] in links.ids
        else:assert (out/unquote(link)).is_file(),link
    plots=json.loads((out/"plot_sources.json").read_text())
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
        assert len(by_id)==89 and len(raw)==169100 and not read(out/"integrity_issues.csv")
        assert flags["calibration_target"]==[]
        assert flags["anticipatory"]==[23,49,51,65,79]
        assert flags["near_chance_or_worse"]==[23,49,51,75,79,81]
        assert len(flags["manual_target"])==11
        assert union==[23,49,51,65,75,79,81]
        assert diagnostic_only==[1,21,25,37,63,85]
    print(json.dumps(dict(status="passed",participants=len(by_id),numeric_values_checked=values_checked,
        checks_verified=len(checks),pdfs_verified=len(pdfs),pngs_verified=len(pngs),flagged_participants=flags,
        unique_flagged_n=len(union),unique_participant_ids=union,diagnostic_only_participant_ids=diagnostic_only,
        validation="Raw-source metrics, Wilson intervals, all screens/bins, unique exclusions and overlaps, peer bands, comparator selection, provenance, HTML links, PDF/PNG integrity"),indent=2))


if __name__=="__main__":main()
