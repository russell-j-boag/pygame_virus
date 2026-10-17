#!/usr/bin/env python3
"""Run the isolated participant QC report. No data filtering is performed."""
import argparse
import hashlib
import json
import os
import platform
import sys
import tempfile
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0,str(ROOT))
os.environ.setdefault("MPLCONFIGDIR",str(Path(tempfile.gettempdir())/"dynamic_reliability_qc_matplotlib"))

from participant_qc.core import CATEGORIES, EXCLUSION_CATEGORIES, DIAGNOSTIC_CATEGORIES, analyse, choose_comparators, exclusion_summary, load_config, load_trials
from participant_qc.reporting import exclusion_table_rows, write_csv, write_report


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def check_destination(source, destination):
    source, destination = source.resolve(), destination.resolve()
    if source.is_relative_to(destination):
        raise ValueError("QC output must not contain the input CSV")
    protected=[ROOT/"data",ROOT/"output",ROOT/"plots",ROOT/"participant_qc",ROOT/"tests",ROOT/"analysis_outputs/semester2_2026_behavioural"]
    if any(p.is_relative_to(destination) or destination.is_relative_to(p) for p in protected):
        raise ValueError("QC output must be separate from canonical data, analyses, plots, and source directories")
    if destination.exists() and any(destination.iterdir()):
        receipt=destination/"manifest.json"
        if not receipt.exists() or json.loads(receipt.read_text()).get("generator")!="participant_qc":
            raise ValueError("Refusing to replace an output directory not owned by participant_qc")


def main(argv=None):
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input",type=Path,default=ROOT/"data/data_virus_all.csv")
    parser.add_argument("--config",type=Path,default=Path(__file__).with_name("config.json"))
    parser.add_argument("--output-dir",type=Path,default=ROOT/"analysis_outputs/semester2_2026_qc")
    args=parser.parse_args(argv)
    source=args.input.resolve(); destination=args.output_dir.resolve(); config_path=args.config.resolve()
    check_destination(source,destination)
    sources=sorted(Path(__file__).parent.glob("*.py"))+[config_path]
    source_hashes=[dict(path=str(p),sha256=sha256(p)) for p in sources]
    cfg=load_config(config_path); before=sha256(source); config_before=sha256(config_path)
    people=load_trials(source); result=analyse(people,cfg); comparisons=choose_comparators(result)
    exclusions=exclusion_summary(result)
    print(f"Screened {len(people)} participants; {len(result['issues'])} scope-level integrity findings",flush=True)
    destination.parent.mkdir(parents=True,exist_ok=True)
    stage=Path(tempfile.mkdtemp(prefix="."+destination.name+".build-",dir=destination.parent))
    print(f"Building report in {stage}",flush=True)
    write_csv(stage/"participant_summary.csv",result["summaries"])
    write_csv(stage/"exclusion_candidates.csv",[s for s in result["summaries"] if s["exclude_by_rules"]],result["summaries"][0].keys())
    write_csv(stage/"exclusion_summary_table.csv",exclusion_table_rows(exclusions))
    (stage/"exclusion_summary.json").write_text(json.dumps(exclusions,indent=2)+"\n")
    write_csv(stage/"scope_metrics.csv",result["metrics"])
    write_csv(stage/"screening_results.csv",result["checks"])
    write_csv(stage/"flagged_checks.csv",[r for r in result["checks"] if r["status"]=="flag"],result["checks"][0].keys())
    write_csv(stage/"trial_bin_summary.csv",result["bins"])
    write_csv(stage/"integrity_issues.csv",result["issues"],("participant_id","run_timestamp","scope","n_trials","n_expected","integrity_issues"))
    (stage/"config_snapshot.json").write_text(json.dumps(cfg,indent=2)+"\n")
    (stage/"comparator_selection.json").write_text(json.dumps(comparisons,indent=2)+"\n")
    from participant_qc.plotting import generate_plots
    individuals,pages,pools,bands=generate_plots(people,result,cfg,comparisons,stage)
    (stage/"reference_pools.json").write_text(json.dumps(pools,indent=2)+"\n")
    write_csv(stage/"reference_bands.csv",bands)
    plot_sources={
        "screening_overview":["participant_summary.csv","exclusion_summary.json"],
        "calibration_manual_accuracy":["scope_metrics.csv","screening_results.csv"],
        "whole_scope_accuracy":["scope_metrics.csv"],
        "anticipatory_cohort":["scope_metrics.csv","comparator_selection.json"],
        "individual_and_comparison_pages":["input CSV (trial RT scatter)","trial_bin_summary.csv","scope_metrics.csv","reference_bands.csv","reference_pools.json","comparator_selection.json"],
        "individuals":individuals,"comparison_pages":pages,
    }
    (stage/"plot_sources.json").write_text(json.dumps(plot_sources,indent=2)+"\n")
    write_report(stage,result,cfg,individuals,pages)
    if sha256(source)!=before or sha256(config_path)!=config_before or any(sha256(r["path"])!=r["sha256"] for r in source_hashes):
        raise RuntimeError(f"Input/config changed during generation; unpublished report retained in {stage}")
    import numpy,matplotlib
    manifest=dict(schema_version=1,generator="participant_qc",created_utc=datetime.now(timezone.utc).isoformat(),
        input_csv=str(source),input_sha256=before,config_path=str(config_path),config_sha256=config_before,
        participant_n=len(people),trial_n=sum(map(len,people.values())),
        groups=dict(Counter(rows[0]["calibration_target_group"] for rows in people.values())),
        flag_counts={c:sum(s[c]=="flag" for s in result["summaries"]) for c in CATEGORIES},
        exclusion_categories=EXCLUSION_CATEGORIES,diagnostic_categories=DIAGNOSTIC_CATEGORIES,
        exclusion_rule_counts=exclusions["rule_counts"],
        unique_flagged_n=exclusions["unique_flagged_n"],
        remaining_if_applied_n=exclusions["remaining_if_applied_n"],
        integrity_scope_n=len(result["issues"]),exclusions_applied=False,
        python_version=platform.python_version(),numpy_version=numpy.__version__,matplotlib_version=matplotlib.__version__,
        sources=source_hashes,
        outputs=[dict(path=str(p.relative_to(stage)),bytes=p.stat().st_size,sha256=sha256(p)) for p in sorted(stage.rglob("*")) if p.is_file()])
    (stage/"manifest.json").write_text(json.dumps(manifest,indent=2)+"\n")
    (stage/"COMPLETE.txt").write_text("Generation complete. Review flags only; no exclusions applied.\n")
    if destination.exists():
        if any(destination.iterdir()):
            archive=destination.with_name(destination.name+".previous_"+datetime.now().strftime("%Y%m%d_%H%M%S_%f"))
            destination.rename(archive);print(f"Previous QC report preserved in {archive}",flush=True)
        else:destination.rmdir()
    stage.rename(destination)
    print(json.dumps({"report":str(destination/"index.html"),"flag_counts":manifest["flag_counts"],
        "unique_flagged_n":exclusions["unique_flagged_n"],"remaining_if_applied_n":exclusions["remaining_if_applied_n"],
        "exclusions_applied":False},indent=2))


if __name__=="__main__":
    main()
