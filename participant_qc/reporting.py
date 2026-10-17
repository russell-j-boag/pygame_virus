"""Portable, searchable HTML and stable CSV exports."""
import csv
from html import escape
from pathlib import Path

from participant_qc.core import exclusion_summary


def write_csv(path, rows, fields=None):
    fields = list(fields or (rows[0].keys() if rows else []))
    with Path(path).open('w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def exclusion_table_rows(summary):
    rows = [dict(criterion=c, count=n, participant_ids=';'.join(f'{p:03d}' for p in summary['rule_participant_ids'][c])) for c,n in summary['rule_counts'].items()]
    rows.append(dict(criterion='Unique candidate union',count=summary['unique_flagged_n'],participant_ids=';'.join(f'{p:03d}' for p in summary['participant_ids'])))
    rows.append(dict(criterion='Diagnostic only',count=len(summary['diagnostic_only_participant_ids']),participant_ids=';'.join(f'{p:03d}' for p in summary['diagnostic_only_participant_ids'])))
    rows.append(dict(criterion='Incomplete assessment',count=len(summary['incomplete_assessment_ids']),participant_ids=';'.join(f'{p:03d}' for p in summary['incomplete_assessment_ids'])))
    return rows


def table(rows, fields, searchable=False):
    header=''.join(f'<th>{escape(k.replace("_"," "))}</th>' for k in fields)
    body=''.join('<tr>'+''.join(f'<td>{escape(str(r.get(k,""))) if r.get(k) is not None else "NA"}</td>' for k in fields)+'</tr>' for r in rows)
    return '<div class="scroll"><table'+(' id="review"' if searchable else '')+'><thead><tr>'+header+'</tr></thead><tbody>'+body+'</tbody></table></div>'


def write_report(out,result,cfg,cohort,individuals,pages):
    out=Path(out); summary=exclusion_summary(result)
    candidates=', '.join(f'{p:03d}' for p in summary['participant_ids']) or 'None'
    text=f'''<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Aid-onset participant QC</title><style>
body{{font:16px/1.55 system-ui,sans-serif;color:#243746;background:#f5f7f9;margin:0}}main{{max-width:1400px;margin:auto;padding:30px}}
h1,h2,h3{{line-height:1.2;color:#17364a}}h1{{font-size:36px}}section,.card{{background:white;padding:22px;margin:20px 0;border-radius:8px}}
a{{color:#17648b}}table{{border-collapse:collapse;width:100%;font-size:13px}}th,td{{text-align:left;padding:9px;border-bottom:1px solid #dce3e8;vertical-align:top}}
th{{background:#eaf0f3}}.scroll{{overflow:auto;max-height:650px}}input{{padding:12px;width:min(600px,90%);font:inherit}}img{{width:100%;height:auto}}
.note{{border-left:4px solid #c56b1a;padding-left:14px}}.links{{display:flex;flex-wrap:wrap;gap:14px}}summary{{cursor:pointer;font-weight:bold}}
</style></head><body><main><h1>Aid-onset participant exclusion audit</h1>
<p>Retrospective review evidence. <strong>No participants or trials have been excluded.</strong></p>
<div class="card"><strong>{summary['unique_flagged_n']} / {summary['participant_n']} candidates ({summary['percentage']:.1f}%)</strong>
<p>Candidate IDs: {candidates}. If these rules were applied, {summary['remaining_if_applied_n']} participants would remain.</p>
<p>{len(summary['diagnostic_only_participant_ids'])} diagnostic-only participants; {len(summary['incomplete_assessment_ids'])} participants with incomplete assessments.</p></div>
<section><h2>Rules and interpretation</h2><p>Decision 1 and Decision 2 are scored separately in Manual, Aid-first, Stimulus-first, pooled aided, and all main trials. Repeated flags count once per exclusion criterion and once per participant.</p>
<ul><li>Low accuracy: all-trial accuracy &le;{100*cfg['chance_accuracy_max']:g}%.</li>
<li>Anticipatory: RT &lt;{cfg['fast_rt_seconds']*1000:g} ms on &ge;{cfg['fast_fraction_min']*100:g}% of all scope trials, &ge;{cfg['fast_count_min']} fast responses, and fast-response accuracy &le;{cfg['fast_accuracy_max']*100:g}%, jointly in the same decision and scope.</li>
<li>Diagnostic only: final-40 practice Decision 2 accuracy differs from the recorded target by &gt;{cfg['calibration_target_band_pp']:g} percentage points; Manual Decision 2 differs by &gt;{cfg['manual_target_band_pp']:g} points; and low-accuracy/anticipatory checks over all 60 practice trials for both decisions.</li></ul>
<p class="note">Aid-first Decision 1 can be prepared from the preceding advice; Decision 2 can be prepared before its response screen. Their fast-response flags are included by agreement and require interpretation. Low-accuracy screens do not establish statistical equivalence to chance. These rules are not preregistered.</p>
<p>Timeouts are incorrect and never fast. Exactly the RT threshold is not fast. Finite nonpositive answered RTs count as fast and remain visible. Fast proportions use all trials; fast accuracy uses fast answered trials. Empty subsets have missing accuracy. No trimming occurs.</p>
<p>Wilson intervals are descriptive and do not model adaptive calibration or serial dependence. Invalid/incomplete checks remain unassessable, never clear. The candidate union remains review evidence even when some other checks are incomplete.</p></section>
<section><h2>Candidate counts</h2>{table(exclusion_table_rows(summary),['criterion','count','participant_ids'])}
<p>Distinct-rule overlaps: {escape(str(summary['overlap_counts']))}. Calibration and Manual target deviations never add an exclusion criterion.</p></section>
<section><h2>Participant review</h2><input id="search" aria-label="Search participants or reasons" placeholder="Search participant, rule, scope or run timestamp">
{table(result['summaries'],['participant_id','run_timestamp','exclude_by_rules','n_exclusion_rules','n_not_assessable','exclusion_reasons','diagnostic_reasons'],True)}</section>
<section><h2>Cohort figures</h2>'''
    for stem in cohort:
        text+=f'<h3>{escape(Path(stem).name.replace("_"," ").title())}</h3><a href="{stem}.pdf">PDF</a><img src="{stem}.png" alt="{escape(Path(stem).name)}">'
    text+='</section><section><h2>Individual diagnostics</h2><div class="links">'
    for item in individuals:
        pid,stem=item['participant_id'],item['stem']
        text+=f'<span>{pid:03d}: <a href="{stem}.pdf">PDF</a> / <a href="{stem}.png">PNG</a></span>'
    text+='</div></section><section><h2>Anticipatory candidate comparisons</h2><p>Complete comparators match calibration target and key mapping, with no main-task anticipatory flag. Rank by summed absolute MAD-standardised distance to subgroup medians of overall accuracy and positive median RT for both decisions. Zero-MAD terms contribute zero; participant ID breaks ties. Other flags do not silently remove comparators.</p><p>Peer bands exclude the participant, match calibration target and align by condition, within-block trial and decision. They are descriptive across block orders, not adjustments for order effects.</p>'
    for item in pages:
        stem=item['stem']; label=f"Participant {item['participant_id']:03d}; comparator {item['comparator']:03d}" if item['comparator'] is not None else f"Participant {item['participant_id']:03d}; no matching comparator available"
        text+=f'<details><summary>{label}</summary><a href="{stem}.pdf">PDF</a><img loading="lazy" src="{stem}.png" alt="{label}"></details>'
    text+='</section><section><h2>Data, provenance and reproducibility</h2><p>Practice and calibration summaries use the exact canonical run timestamp, including the selected replacement p59. Missing sources never fall back to older runs. The source manifest records selected input files and hashes; canonical data and historical raw runs are preserved.</p><div class="links">'
    for name in ('participant_summary.csv','scope_metrics.csv','screening_results.csv','flagged_checks.csv','exclusion_candidates.csv','exclusion_summary_table.csv','exclusion_summary.json','integrity_issues.csv','trial_bin_summary.csv','source_run_manifest.csv','comparator_selection.json','reference_pools.json','reference_bands.csv','plot_sources.json','config_snapshot.json','manifest.json','COMPLETE.txt'):
        text+=f'<a href="{name}">{name}</a>'
    text+='''</div></section></main><script>document.getElementById('search').addEventListener('input',e=>{const q=e.target.value.toLowerCase();document.querySelectorAll('#review tbody tr').forEach(r=>r.hidden=!r.textContent.toLowerCase().includes(q));});</script></body></html>'''
    (out/'index.html').write_text(text)
