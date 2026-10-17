"""Portable local HTML index and machine-readable audit tables."""
import csv
import html
from pathlib import Path

from participant_qc.core import CATEGORIES, EXCLUSION_CATEGORIES, DIAGNOSTIC_CATEGORIES, exclusion_summary

LABELS = {
    "calibration_target":"Calibration target", "manual_target":"Manual target (diagnostic only)",
    "near_chance_or_worse":"Near-chance or worse",
    "anticipatory":"Anticipatory responding",
}


def exclusion_table_rows(exclusions):
    rows = []
    total = exclusions["participant_n"]
    for category in EXCLUSION_CATEGORIES + DIAGNOSTIC_CATEGORIES:
        ids = exclusions["rule_participant_ids"][category]
        rows.append(dict(criterion=category, label=LABELS[category],
            role="Exclusion criterion" if category in EXCLUSION_CATEGORIES else "Diagnostic only",
            participant_n=len(ids), percent_of_cohort=100*len(ids)/total if total else 0,
            participant_ids="; ".join(f"{p:03d}" for p in ids)))
    for key,label,n,ids in [
        ("any_exclusion_rule","Total unique exclusion candidates",exclusions["unique_flagged_n"],exclusions["participant_ids"]),
        ("remaining_if_applied","Included in primary analyses" if exclusions["exclusions_applied"] and
          exclusions["primary_analysis"].get("excluded_participant_ids")==exclusions["participant_ids"] else "Remaining if criteria applied",
         exclusions["remaining_if_applied_n"],[])]:
        rows.append(dict(criterion=key,label=label,role="Total",participant_n=n,
            percent_of_cohort=100*n/total if total else 0,
            participant_ids="; ".join(f"{p:03d}" for p in ids)))
    return rows


def write_exclusion_summary(out, exclusions):
    """Human-readable selection record plus a separate identity-correction table."""
    application = exclusions["primary_analysis"]
    lines = ["Participant exclusion and identity summary", "",
             f"Source participants: {exclusions['participant_n']}",
             "QC audit population: all source participants; source data are not deleted."]
    if exclusions["exclusions_applied"]:
        lines.extend([f"Excluded from primary analyses: {', '.join(f'{p:03d}' for p in application['excluded_participant_ids'])}",
                      f"Included in primary analyses and all current hypothesis figures: {application['included_participant_n']}"])
    else:
        lines.append("No applied primary-analysis exclusion decision is recorded in this audit.")
    lines.extend(["", "Exclusion criteria: calibration target deviation, near-chance performance, anticipatory responding.",
                  "Manual-target deviation is diagnostic only.", "", "Identity corrections (not exclusions):"])
    rows = []
    for case in exclusions["identity_reuse_cases"]:
        lines.append(f"Recorded participant ID {case['source_participant_id']:03d} was reused for {len(case['runs'])} separate participant runs.")
        for run in case["runs"]:
            lines.append(f"  Run {run['run_timestamp']}: analysis ID {run['participant_id']:03d}; allocation remains recorded ID {case['source_participant_id']:03d}; "
                         + ("primary inclusion not recorded." if run["included_in_primary"] is None else
                            "included in primary analyses." if run["included_in_primary"] else "not included in primary analyses."))
            rows.append(dict(source_participant_id=case["source_participant_id"], **run, note=case["resolution"]))
        lines.append(case["resolution"])
    if not rows:
        lines.append("None.")
    (Path(out)/"exclusion_summary.txt").write_text("\n".join(lines)+"\n")
    write_csv(Path(out)/"identity_corrections_summary.csv",rows,
              ("source_participant_id","participant_id","run_timestamp","allocation_source_participant_id","included_in_primary","note"))


def write_csv(path, rows, empty_fields=()):
    fields = list(rows[0]) if rows else list(empty_fields)
    with Path(path).open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader(); writer.writerows(rows)


def pct(value):
    return "NA" if value is None else f"{100*value:.1f}%"


def interval(row, prefix):
    value, low, high = row[prefix], row[prefix+"_lower"], row[prefix+"_upper"]
    return "NA" if value is None else f"{pct(value)} [{pct(low)}, {pct(high)}]"


def write_report(out,result,cfg,individuals,comparison_pages):
    out=Path(out); esc=html.escape
    interval_label=f"{100*cfg['interval_level']:g}%"
    exclusions=exclusion_summary(result)
    application=exclusions["primary_analysis"]
    status_note=(f"{len(result['summaries'])} source participants remain visible in this QC audit. "
        f"<strong>{len(application['excluded_participant_ids'])} are excluded from the primary analyses, leaving {application['included_participant_n']} participants.</strong> "
        "Raw data and audit diagnostics remain intact." if exclusions["exclusions_applied"] else
        f"{len(result['summaries'])} participants. Flags are review evidence; no primary-analysis exclusions are recorded here.")
    applied_note=(f'<p><strong>Applied to the current hypothesis analyses and figures:</strong> '
                  f'{", ".join(f"{p:03d}" for p in application["excluded_participant_ids"])}. '
                  f'<a href="analysis_cohort_selection.csv">Complete inclusion/exclusion record</a>.</p>' if application else '')
    identity_rows="".join(f'<tr><td>{case["source_participant_id"]:03d}</td><td>{esc(run["run_timestamp"])}</td>'
        f'<td>{run["participant_id"]:03d}</td><td>{run["allocation_source_participant_id"]:03d}</td>'
        f'<td>{"Not recorded" if run["included_in_primary"] is None else "Included" if run["included_in_primary"] else "Excluded"}</td></tr>'
        for case in exclusions["identity_reuse_cases"] for run in case["runs"])
    identity_section=(f'<h2>Reused participant ID: identity correction, not exclusion</h2>'
        '<p>Two distinct runs were recorded under participant ID 083 and are treated as separate participants under the documented correction. '
        'The earlier run retains analysis ID 083; the later run is analysis ID 084. Both retain the allocation actually administered under recorded ID 083. '
        'The reused ID does not add an exclusion or reduce the source participant count.</p>'
        f'<table><thead><tr><th>Recorded ID</th><th>Run timestamp</th><th>Analysis ID</th><th>Administered allocation ID</th><th>Primary analysis</th></tr></thead><tbody>{identity_rows}</tbody></table>'
        '<p><a href="identity_corrections_summary.csv">Identity correction record (CSV)</a></p>' if identity_rows else '')
    remaining_note=(f"leaving <strong>{application['included_participant_n']}</strong> in the current primary analyses" if exclusions["exclusions_applied"] else
                    f"leaving <strong>{exclusions['remaining_if_applied_n']}</strong> if the three criteria are applied")
    final_table="".join(f'<tr><td>{esc(r["label"])}</td><td>{esc(r["role"])}</td><td>{r["participant_n"]}</td><td>{r["percent_of_cohort"]:.1f}%</td><td>{esc(r["participant_ids"]) or "-"}</td></tr>' for r in exclusion_table_rows(exclusions))
    candidate_links=", ".join(f'<a href="#p{p:03d}">{p:03d}</a>' for p in exclusions["participant_ids"]) or "None"
    overlap_rows="".join(f'<tr><td>{r["n_rules"]}</td><td>{r["participant_n"]}</td><td>{", ".join(f"{p:03d}" for p in r["participant_ids"]) or "None"}</td></tr>' for r in exclusions["overlap"])
    incomplete_note=(f'<p>{len(exclusions["incomplete_assessment_ids"])} participants have unassessable checks. These checks are not counted as exclusions or evidence of eligibility.</p>' if exclusions["incomplete_assessment_ids"] else '')
    table=[]; details=[]
    for s in result["summaries"]:
        pid=s["participant_id"]
        cells="".join(f'<td class="{s[c]}">{esc(s[c].replace("_"," "))}</td>' for c in CATEGORIES)
        table.append(f'<tr data-review="{str(s["review_required"]).lower()}"><td><a href="#p{pid:03d}">{pid:03d}</a></td><td>{esc(s["automation_reliability_pattern"])}</td>{cells}<td>{s["n_exclusion_rules"]}</td><td>{s["n_flags"]}</td></tr>')
        scopes=[m for m in result["metrics"] if m["participant_id"]==pid]
        checks=[c for c in result["checks"] if c["participant_id"]==pid and c["status"]!="clear"]
        reasons="".join(f'<li><strong>{esc(LABELS[c["category"]])}</strong> / {esc(c["scope_label"])}: {esc(c["rule"])} ({esc(c["status"])})</li>' for c in checks) or "<li>No review flags on the configured screens.</li>"
        rows="".join(f'<tr><td>{esc(m["scope_label"])}</td><td>{m["n_trials"]}/{m["n_expected"]}</td><td>{interval(m,"accuracy")}</td><td>{pct(m["answered_accuracy"])}</td><td>{m["n_timeouts"]} ({pct(m["timeout_fraction"])})</td><td>{m["n_fast"]}</td><td>{pct(m["fast_fraction"])}</td><td>{interval(m,"fast_accuracy")}</td><td>{m["n_nonpositive_rt"]}</td><td>{esc(m["integrity_issues"]) or "Complete"}</td></tr>' for m in scopes)
        details.append(f'''<details id="p{pid:03d}"><summary>Participant {pid:03d} - {esc(s['automation_reliability_pattern'])} - {s['n_exclusion_rules']} exclusion criteria / {s['n_flags']} flagged checks including diagnostics</summary>
<p>Recorded ID: {s['source_participant_id']:03d}. Run: {esc(s['run_timestamp'])}. Order: {esc(s['main_block_order'])}. Keys: black={esc(s['key_black'])}, white={esc(s['key_white'])}.
<a href="individual/p{pid:03d}_diagnostics.pdf">Diagnostic PDF</a> | <a href="individual/p{pid:03d}_diagnostics.png">PNG</a></p>
<ul>{reasons}</ul><div class="scroll"><table><thead><tr><th>Scope</th><th>Trials</th><th>Accuracy [{interval_label} interval]</th><th>Answered accuracy</th><th>Timeouts</th><th>Fast n</th><th>Fast rate</th><th>Fast accuracy [{interval_label} interval]</th><th>Nonpositive n</th><th>Integrity</th></tr></thead><tbody>{rows}</tbody></table></div></details>''')
    counts={c:sum(s[c]=="flag" for s in result["summaries"]) for c in CATEGORIES}
    counts_html="".join(f'<li>{esc(LABELS[c])}: <strong>{counts[c]}</strong> participants</li>' for c in CATEGORIES)
    comparisons="".join(f'<li><a href="{p["stem"]}.pdf">Participants {", ".join(f"{x:03d}" for x in p["targets"])}</a>; comparator {p["comparator"] if p["comparator"] is not None else "unavailable"} (<a href="{p["stem"]}.png">PNG</a>)</li>' for p in comparison_pages)
    if not comparisons: comparisons="<li>No anticipatory flags; no comparison pages required.</li>"
    bundle_link='<p><a href="anticipatory_comparisons.pdf">All anticipatory comparisons (PDF)</a></p>' if comparison_pages else ''
    figure_titles={"screening_overview":"Screening overview", "calibration_manual_accuracy":"Calibration and manual accuracy", "whole_scope_accuracy":"Whole-scope accuracy", "anticipatory_cohort":"Anticipatory cohort context"}
    figures="".join(f'<article><h3>{title}</h3><a href="{stem}.png"><img src="{stem}.png" alt="{title}" loading="lazy"></a><p><a href="{stem}.pdf">PDF</a> | <a href="{stem}.png">PNG</a></p></article>' for stem,title in figure_titles.items())
    output=f'''<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Participant QC review</title><style>
body{{font:16px system-ui,sans-serif;max-width:1450px;margin:32px auto;padding:0 24px;color:#243746;background:#fafbfd}}h1,h2,h3{{color:#172b3a}}a{{color:#236b8e}}table{{border-collapse:collapse;width:100%;font-size:13px}}th,td{{padding:9px;border-bottom:1px solid #dfe5e9;text-align:left}}th{{background:#edf1f4}}.flag{{background:#fae6eb;color:#8c1735;font-weight:bold}}.not_assessable{{background:#fff0c4}}.clear{{color:#546773}}.scroll{{overflow:auto}}.figures{{display:grid;grid-template-columns:repeat(auto-fit,minmax(420px,1fr));gap:24px}}article{{background:white;padding:18px;border:1px solid #dfe5e9}}img{{max-width:100%;height:auto;max-height:700px;object-fit:contain}}details{{margin:14px 0;padding:12px;border:1px solid #dfe5e9;background:white}}summary{{cursor:pointer;font-weight:bold}}input{{font:inherit;padding:7px}}.notice{{background:#edf3f7;padding:16px;border-left:4px solid #236b8e}}
</style></head><body><h1>Participant quality-control review</h1>
<p>{status_note}</p>
<div class="notice">M_LP target deviation is retained as a diagnostic summary only and does not cause exclusion. The three exclusion criteria are calibration target deviation, near-chance performance, and anticipatory responding. Near-chance flags are descriptive screens, not proof of statistical equivalence to chance.</div>
<h2>Exclusion summary</h2>{applied_note}
<p><strong>{exclusions['unique_flagged_n']} of {exclusions['participant_n']} participants ({exclusions['unique_flagged_percent']:.1f}%)</strong> meet at least one exclusion criterion, {remaining_note}. Each participant is counted once across exclusion criteria and scopes.</p>
<div class="scroll"><table><thead><tr><th>Criterion or total</th><th>Use</th><th>Participants</th><th>Percentage</th><th>Participant IDs</th></tr></thead><tbody>{final_table}</tbody></table></div>
<p>Participants: {candidate_links}.</p>
<p>{exclusions['diagnostic_only_n']} participants have diagnostic flags only and are not selected for exclusion.</p>
<div class="scroll"><table><thead><tr><th>Number of exclusion criteria met</th><th>Participants</th><th>Participant IDs</th></tr></thead><tbody>{overlap_rows}</tbody></table></div>{incomplete_note}
<p><a href="exclusion_summary.txt">Exclusion and identity summary (text)</a> | <a href="exclusion_summary_table.csv">Summary table (CSV)</a> | <a href="exclusion_candidates.csv">Unique candidate list (CSV)</a> | <a href="exclusion_summary.json">Totals, application and identity notes (JSON)</a></p>
{identity_section}
<h2>Exclusion criteria</h2><ul>
<li>Calibration: final {cfg['calibration_tail_n']} trials; absolute deviation from the 77% LP target &gt;{cfg['calibration_target_band_pp']:g} percentage points.</li>
<li>Near-chance or worse: whole-scope accuracy &lt;={100*cfg['chance_accuracy_max']:g}%.</li>
<li>Anticipatory: RT &lt;{1000*cfg['fast_rt_seconds']:g} ms, at least {cfg['fast_count_min']} fast responses, fast fraction &gt;={100*cfg['fast_fraction_min']:g}%, and fast-response accuracy &lt;={100*cfg['fast_accuracy_max']:g}% within the same whole scope.</li></ul>
<p><strong>Diagnostic only:</strong> M_LP target deviation &gt;{cfg['manual_target_band_pp']:g} percentage points in the whole M_LP block. This flag stays in summaries and plots but is not an exclusion criterion.</p>
<p>Whole scopes: full CAL_LP, M_HP, A_HP, M_LP, A_LP, all aided trials, and all 1,600 experimental trials. Timeouts count as incorrect; fast-rate denominators include all trials. Fast counts include nonpositive RTs. Exactly {1000*cfg['fast_rt_seconds']:g} ms is not fast. Empty fast subsets have no accuracy estimate. A timeout can retain a keypress RT when the deadline takes precedence within the same frame; it remains a timeout and never contributes to fast responses. Recorded timeout RTs and answered RTs beyond nominal deadlines are counted separately in the scope metrics.</p>
<p>Intervals are descriptive {interval_label} Wilson binomial intervals, not decision rules; they do not model temporal dependence or the adaptive calibration process. Missing/invalid scopes are not assessable. These are retrospective screens. HP deliberately changes response speed and can increase timeouts; low all-trial accuracy is not by itself evidence of disengagement. There is no target-deviation screen for M_HP.</p>
<h2>Participants flagged by screen</h2><ul>{counts_html}</ul><p>Counts overlap. There are {len(result['issues'])} scope-level integrity findings.</p>
<p><a href="participant_summary.csv">Participant summary CSV</a> | <a href="scope_metrics.csv">Scope metrics CSV</a> | <a href="screening_results.csv">All screening results CSV</a> | <a href="flagged_checks.csv">Flagged checks CSV</a> | <a href="integrity_issues.csv">Integrity findings</a> | <a href="config_snapshot.json">Configuration</a> | <a href="manifest.json">Provenance</a></p>
<h2>Review table</h2><p><input id="search" aria-label="Search participants" placeholder="Search participant or group"> <label><input id="review" type="checkbox"> Review cases only</label></p>
<div class="scroll"><table id="participants"><thead><tr><th>Participant</th><th>Group</th>{''.join(f'<th>{esc(LABELS[c])}</th>' for c in CATEGORIES)}<th>Exclusion criteria met</th><th>Flagged checks (all screens)</th></tr></thead><tbody>{''.join(table)}</tbody></table></div>
<h2>Cohort figures</h2><div class="figures">{figures}</div>
<h2>Anticipatory comparisons</h2><ul>{comparisons}</ul>{bundle_link}<p>Comparators match reliability pattern and keys; peer bands align conditions and within-block trials and pass the anticipatory screen; they may have other review flags. Selection and peer pools are recorded in <a href="comparator_selection.json">comparator selection</a> and <a href="reference_pools.json">peer references</a>.</p>
<h2>Identity and allocation</h2><p>Analysis IDs, recorded IDs, and run timestamps are retained. The documented later recorded-p083 run is analysis participant 084; its administered allocation remains that of recorded ID 083. No raw exports are rewritten.</p>
<h2>Individual diagnostics</h2><p><a href="all_individual_diagnostics.pdf">All individual pages (PDF)</a>. Each plot includes calibration; header metrics summarise the experimental trials.</p>{''.join(details)}
<script>
const search=document.getElementById('search'), review=document.getElementById('review');
function filterRows(){{document.querySelectorAll('#participants tbody tr').forEach(row=>{{row.hidden=!(row.textContent.toLowerCase().includes(search.value.toLowerCase())&&(!review.checked||row.dataset.review==='true'));}});}}
search.addEventListener('input',filterRows);review.addEventListener('change',filterRows);
function openTarget(){{const target=document.getElementById(location.hash.slice(1));if(target&&target.tagName==='DETAILS')target.open=true;}}
window.addEventListener('hashchange',openTarget);openTarget();
</script></body></html>'''
    (out/"index.html").write_text(output)
