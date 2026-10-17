#!/usr/bin/env python3
"""Build the isolated aid-onset participant audit; never applies exclusions."""
import argparse
import json
import os
import platform
import sys
import tempfile
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0,str(ROOT))
os.environ.setdefault('MPLCONFIGDIR',str(Path(tempfile.gettempdir())/'aid_onset_qc_matplotlib'))
os.environ.setdefault('XDG_CACHE_HOME',str(Path(tempfile.gettempdir())/'aid_onset_qc_cache'))

from participant_qc.core import (analyse, choose_comparators, exclusion_summary, load_config,
                               load_dataset, sha256, EXCLUSION_CATEGORIES)
from participant_qc.reporting import write_csv, write_report, exclusion_table_rows


def check_destination(destination, inputs):
    destination=Path(destination).resolve()
    protected=[ROOT/p for p in ('data','output','plots','participant_qc','tests','python','config','R','.git','.agents','.codex')]
    protected += [p for p in (ROOT/'analysis_outputs').iterdir() if p.is_dir() and not p.name.startswith('semester2_2026_qc')] if (ROOT/'analysis_outputs').exists() else []
    if any(p.resolve().is_relative_to(destination) or destination.is_relative_to(p.resolve()) for p in protected):
        raise ValueError('QC output must be separate from canonical data, analyses and source directories')
    if any(Path(p).resolve().is_relative_to(destination) for p in inputs):
        raise ValueError('QC output must not contain an input or source')
    if destination.exists():
        if not destination.is_dir() or destination.is_symlink():
            raise ValueError('QC output must be a directory owned by this generator')
        if any(destination.iterdir()):
            receipt=destination/'manifest.json'
            if not receipt.exists() or json.loads(receipt.read_text()).get('study')!='aid_onset' or json.loads(receipt.read_text()).get('generator')!='participant_qc':
                raise ValueError('Refusing to replace a directory not owned by this QC generator')


def main(argv=None):
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--input',type=Path,default=ROOT/'data/data_virus_all.csv')
    parser.add_argument('--manifest',type=Path,default=ROOT/'data/collation_manifest.csv')
    parser.add_argument('--config',type=Path,default=Path(__file__).with_name('config.json'))
    parser.add_argument('--output-dir',type=Path,default=ROOT/'analysis_outputs/semester2_2026_qc')
    args=parser.parse_args(argv)
    if args.output_dir.is_symlink():
        raise ValueError('QC output must not be a symlink')
    source, manifest, config, destination = (p.resolve() for p in (args.input,args.manifest,args.config,args.output_dir))
    implementation=sorted(Path(__file__).parent.glob('*.py'))+[ROOT/'python/virus_task.py',ROOT/'python/instructions.py']
    input_paths=[source,manifest,config,*implementation]
    check_destination(destination,input_paths)
    before={str(p):sha256(p) for p in input_paths}
    cfg=load_config(config)
    people,sources,practice_issues=load_dataset(source,manifest)
    check_destination(destination,[*input_paths,*(r['source_file'] for r in sources)])
    result=analyse(people,cfg,practice_issues)
    comparisons=choose_comparators(result); exclusions=exclusion_summary(result)
    print(f"Screened {len(people)} participants; {exclusions['unique_flagged_n']} candidates; {len(result['issues'])} invalid scopes",flush=True)
    destination.parent.mkdir(parents=True,exist_ok=True)
    stage=Path(tempfile.mkdtemp(prefix='.'+destination.name+'.build-',dir=destination.parent))
    print(f'Building in {stage}',flush=True)
    write_csv(stage/'participant_summary.csv',result['summaries'])
    write_csv(stage/'exclusion_candidates.csv',[s for s in result['summaries'] if s['exclude_by_rules']],result['summaries'][0].keys())
    write_csv(stage/'exclusion_summary_table.csv',exclusion_table_rows(exclusions))
    for name,key in (('scope_metrics','metrics'),('screening_results','checks'),('trial_bin_summary','bins')):
        write_csv(stage/f'{name}.csv',result[key])
    write_csv(stage/'flagged_checks.csv',[c for c in result['checks'] if c['status']=='flag'],result['checks'][0].keys())
    write_csv(stage/'integrity_issues.csv',result['issues'],result['metrics'][0].keys())
    write_csv(stage/'source_run_manifest.csv',sources,sources[0].keys())
    for name,obj in (('exclusion_summary',exclusions),('config_snapshot',cfg),('comparator_selection',comparisons)):
        (stage/f'{name}.json').write_text(json.dumps(obj,indent=2,allow_nan=False)+'\n')
    from participant_qc.plotting import generate_plots
    cohort,individuals,pages,pools,bands=generate_plots(people,result,cfg,comparisons,stage)
    write_csv(stage/'reference_bands.csv',bands)
    (stage/'reference_pools.json').write_text(json.dumps(pools,indent=2)+'\n')
    plot_sources=dict(cohort=cohort,individuals=individuals,comparison_pages=pages,
        cohort_tables=['scope_metrics.csv','screening_results.csv','participant_summary.csv'],
        traces=['source_run_manifest.csv','trial_bin_summary.csv','reference_bands.csv','reference_pools.json','comparator_selection.json'],
        note='Trial scatter uses the canonical main CSV and exact-run practice exports. Bin lines break at condition boundaries.')
    (stage/'plot_sources.json').write_text(json.dumps(plot_sources,indent=2)+'\n')
    write_report(stage,result,cfg,cohort,individuals,pages)
    if any(sha256(p)!=digest for p,digest in before.items()):
        raise RuntimeError(f'Input or implementation changed during generation; unpublished build retained at {stage}')
    for r in sources:
        p=Path(r['source_file'])
        if (r['status']=='present' and (not p.exists() or sha256(p)!=r['sha256'])) or (r['status']=='missing' and p.exists()):
            raise RuntimeError(f'Source changed during generation: {p}; unpublished build retained at {stage}')
    import numpy,matplotlib
    receipt=dict(schema_version=1,generator='participant_qc',study='aid_onset',created_utc=datetime.now(timezone.utc).isoformat(),
        input_csv=str(source),collation_manifest=str(manifest),config_path=str(config),
        participant_n=len(people),main_trial_n=sum(s['n_main_trials'] for s in result['summaries']),
        practice_trial_n=sum(s['n_trials']-s['n_main_trials'] for s in result['summaries']),
        unique_flagged_n=exclusions['unique_flagged_n'],exclusion_categories=EXCLUSION_CATEGORIES,exclusions_applied=False,
        python_version=platform.python_version(),numpy_version=numpy.__version__,matplotlib_version=matplotlib.__version__,
        sources=[dict(path=p,sha256=digest) for p,digest in before.items()],
        raw_sources=sources)
    (stage/'COMPLETE.txt').write_text('Generation complete. Review evidence only; no exclusions applied. Independent validation is a separate step.\n')
    receipt['outputs']=[dict(path=str(p.relative_to(stage)),bytes=p.stat().st_size,sha256=sha256(p)) for p in sorted(stage.rglob('*')) if p.is_file()]
    (stage/'manifest.json').write_text(json.dumps(receipt,indent=2,allow_nan=False)+'\n')
    archive=None
    if destination.exists():
        archive=destination.with_name(destination.name+'.previous_'+datetime.now().strftime('%Y%m%d_%H%M%S_%f'))
        destination.rename(archive)
    try:
        stage.rename(destination)
    except OSError:
        if archive is not None:
            archive.rename(destination)
        raise
    print(json.dumps(dict(report=str(destination/'index.html'),**exclusions),indent=2))


if __name__=='__main__':
    main()
