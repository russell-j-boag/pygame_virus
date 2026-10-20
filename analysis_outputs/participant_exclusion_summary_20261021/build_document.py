from pathlib import Path
import csv
import sys
import hashlib
from zipfile import ZipFile
from docx import Document
from docx.shared import Inches, Pt, RGBColor
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.section import WD_SECTION_START, WD_ORIENT
from docx.enum.table import WD_CELL_VERTICAL_ALIGNMENT
from docx.enum.style import WD_STYLE_TYPE
from docx.oxml import OxmlElement
from docx.oxml.ns import qn

ROOT = Path('/Users/rjb779/Documents/pygame_virus_dynamic_reliability')
OUT = Path(__file__).parent
QC = ROOT / 'analysis_outputs/semester2_2026_qc'
sys.path.insert(0, '/Users/rjb779/.codex/plugins/cache/openai-primary-runtime/documents/26.623.12021/skills/documents/scripts')
from table_geometry import apply_table_geometry

# Preset: standard_business_brief; header: memo_masthead without decorative rule.
# Named overrides: TableText 10 pt; FigureCaption 10 pt; landscape figure pages
# use 0.5-inch vertical and 0.75-inch horizontal margins for readable source plots.
doc = Document()
def style(name, size, before=0, after=6, color='222222', bold=False, line=1.1):
    s = doc.styles[name] if name in doc.styles else doc.styles.add_style(name, WD_STYLE_TYPE.PARAGRAPH)
    s.font.name = 'Calibri'; s.font.size = Pt(size); s.font.bold = bold
    s.font.color.rgb = RGBColor.from_string(color)
    f=s.paragraph_format; f.space_before=Pt(before); f.space_after=Pt(after); f.line_spacing=line
    return s
style('Normal',11)
style('Title',23,after=4,color='172B3A',bold=True)
style('Subtitle',12,after=12,color='555555')
for n,size,before,after,color in [(1,16,16,8,'2E74B5'),(2,13,12,6,'2E74B5'),(3,12,8,4,'1F4D78')]:
    style(f'Heading {n}',size,before,after,color,True).paragraph_format.keep_with_next=True
style('TableText',10,after=3)
style('SourceNote',9,before=4,after=4,color='555555')
style('FigureCaption',10,after=5)
style('PlotImage',10,after=0,line=1)
style('Header',9,after=0,color='666666')
style('Footer',9,after=0,color='666666')
s=doc.sections[0]
s.page_width=Inches(8.5); s.page_height=Inches(11)
s.top_margin=s.bottom_margin=s.left_margin=s.right_margin=Inches(1)
s.header_distance=s.footer_distance=Inches(.492)
s.header.paragraphs[0].text='Dynamic-reliability study | Reviewed participant exclusions'
p=s.footer.paragraphs[0]; p.alignment=WD_ALIGN_PARAGRAPH.RIGHT
p.add_run('Page ')
fld=OxmlElement('w:fldSimple'); fld.set(qn('w:instr'),'PAGE'); p._p.append(fld)
doc.core_properties.title='Dynamic-reliability study: reviewed participant exclusions'
doc.core_properties.subject='Reviewed primary-analysis exclusions and saved participant diagnostics'

import json
from decimal import Decimal, ROUND_HALF_UP

def read_csv(path):
    with path.open(newline='') as f:
        return list(csv.DictReader(f))

summary = json.loads((QC/'exclusion_summary.json').read_text())
manifest = json.loads((QC/'manifest.json').read_text())
membership_path = ROOT/'analysis_outputs/semester2_2026_behavioural/primary/cohort_membership.csv'
members = read_csv(membership_path)
excluded = [r for r in members if r['in_primary']=='FALSE']
ids = sorted(int(r['participant_id']) for r in excluded)
assert ids == summary['participant_ids'] == [23,49,51,65,75,79,81]
assert len(members)==89 and len(excluded)==7
assert hashlib.sha256((ROOT/'data/data_virus_all.csv').read_bytes()).hexdigest()==manifest['input_sha256']
assert all(r['calibration_target_group']=='CAL65' for r in excluded)
metrics = {(int(r['participant_id']),r['scope']):r for r in read_csv(QC/'scope_metrics.csv')}
flags = read_csv(QC/'flagged_checks.csv')
def pct(value):
    return str((Decimal(value)*100).quantize(Decimal('0.1'),rounding=ROUND_HALF_UP))+'%'
def fast(pid, scope, label):
    r=metrics[pid,scope]
    assert float(r['fast_fraction'])>=.2 and int(r['n_fast'])>=20 and float(r['fast_accuracy'])<=.55
    return f"{label}: {r['n_fast']}/{r['n_trials']} fast ({pct(r['fast_fraction'])}); fast accuracy {pct(r['fast_accuracy'])}."
def acc(pid, scope):
    r=metrics[pid,scope]
    return f"{r['n_correct']}/{r['n_trials']} ({pct(r['accuracy'])})"

doc.add_paragraph('Reviewed participant exclusions','Title')
doc.add_paragraph('Dynamic-reliability study | Prepared 21 October 2026','Subtitle')
doc.add_paragraph('Seven of 89 participants (7.9%) were excluded from the primary analyses, leaving 82 (38 CAL65; 44 CAL90). All seven exclusions were in CAL65. The all-participant sensitivity analyses retain all 89 participants.')

both='Anticipatory responding and near-chance performance'
evidence={
23:fast(23,'manual_post','Manual post')+' Overall accuracy '+acc(23,'manual_post')+'.',
49:fast(49,'P2_70','Aid 70%')+' Overall accuracy 49.5%.\n'+fast(49,'P3_95','Aid 95% again')+' Overall accuracy 51.3%.\n'+fast(49,'manual_post','Manual post')+' Overall accuracy 55.0%.\nPooled aided and experimental scopes also met the anticipatory criterion.',
51:fast(51,'P2_70','Aid 70%')+'\n'+fast(51,'P3_95','Aid 95% again')+'\nManual-post accuracy '+acc(51,'manual_post')+'.',
65:'Both manual segments, all three aided phases, and pooled aided/experimental scopes met the anticipatory criterion.\n'+fast(65,'experimental_overall','All experimental trials')+' No whole scope met the near-chance criterion.',
75:'Manual-post accuracy '+acc(75,'manual_post')+'. Only 6/200 trials (3.0%) were fast; no anticipatory flag.',
79:fast(79,'manual_post','Manual post')+' Overall accuracy '+acc(79,'manual_post')+'.',
81:'Manual-post accuracy '+acc(81,'manual_post')+'. No fast responses in that segment (0/200); no anticipatory flag.'}
reasons={p:both if p in (23,49,51,79) else 'Anticipatory responding' if p==65 else 'Near-chance performance' for p in ids}
rows=[['Participant','Reviewed exclusion reason','Supporting diagnostic evidence']]+[[f'{p:03d}',reasons[p],evidence[p]] for p in ids]
t=doc.add_table(rows=len(rows),cols=3); t.style='Table Grid'
for i,values in enumerate(rows):
    trpr=t.rows[i]._tr.get_or_add_trPr(); trpr.append(OxmlElement('w:cantSplit'))
    for j,value in enumerate(values):
        cell=t.cell(i,j); cell.vertical_alignment=WD_CELL_VERTICAL_ALIGNMENT.CENTER
        p=cell.paragraphs[0]; p.style='TableText'; p.add_run(value)
        if j==0: p.alignment=WD_ALIGN_PARAGRAPH.CENTER
        if i==0:
            p.runs[0].bold=True
            sh=OxmlElement('w:shd'); sh.set(qn('w:fill'),'F2F4F7'); cell._tc.get_or_add_tcPr().append(sh)
    if i==0: trpr.append(OxmlElement('w:tblHeader'))
apply_table_geometry(t,[1152,2376,5832])
doc.add_paragraph('Fast = recorded RT <200 ms. Manual pre/post = the 200-trial segments before/after the aided block. Aided phases: 95%, 70%, then 95% again (400 trials each). Percentages rounded to one decimal place.','SourceNote')

doc.add_paragraph('Review criteria and scope','Heading 2')
doc.add_paragraph('Anticipatory responding required ≥20% of all trials with RT <200 ms, ≥20 fast responses, and ≤55% accuracy on those fast responses, jointly within the same scope. Near-chance performance meant ≤55% all-trial accuracy. Timeouts count as incorrect and never as fast; finite nonpositive answered RTs count as fast. Exactly 200 ms is not fast.')
doc.add_paragraph('Assessed whole scopes were full calibration, each manual segment, each aided phase, all 1,200 aided trials, and all 1,600 experimental trials. Calibration-target exclusion required accuracy in calibration trials 151–300 to differ from the assigned target by more than 10 percentage points; no participant met this criterion.')
doc.add_paragraph('Six participants met the near-chance criterion and five met the anticipatory criterion; four met both, giving seven unique exclusions. Eleven participants had manual-target deviations, which were diagnostic only. The six with diagnostic flags only (001, 021, 025, 037, 063 and 085) remain in the primary sample.')
doc.add_paragraph('These are retrospective descriptive criteria, not preregistered exclusions or evidence of equivalence to chance. They include post-exposure performance; the all-participant analyses provide a sensitivity check for this selection.')

doc.add_paragraph('Remaining participant IDs to collect','Heading 2')
doc.add_paragraph('The target is 96 retained participants. With 82 currently retained, 14 further participants are required: seven replacements for exclusions and seven new participants, assuming all pass QC.')
doc.add_paragraph('Re-collect: 023, 049, 051, 065, 075, 079, 081.\nNew participants: 090, 091, 092, 093, 094, 095, 096.')
doc.add_paragraph('Collection target and replacement plan confirmed by the researcher on 21 October 2026. These IDs are outstanding collection slots, not completed replacement runs.','SourceNote')

doc.add_paragraph('Reading the diagnostic plots','Heading 2')
doc.add_paragraph('The following pages reproduce the saved individual plots for all seven excluded participants, including the two excluded only for near-chance performance. Each shows the full sequence including calibration, trial-level RTs, and fast-response rates and accuracy in 50-trial bins. Headline statistics pool the 1,600 experimental trials; the table identifies the relevant decision scopes. Binned patterns do not independently trigger exclusion.')
doc.add_paragraph('The plots retain their original audit wording, “no participants are excluded”. This describes the standalone QC report, which leaves source data intact. The current primary analyses apply the seven exclusions documented here; the all-participant sensitivity retains everyone.')
doc.add_paragraph('Provenance','Heading 2')
doc.add_paragraph('Decision membership: analysis_outputs/semester2_2026_behavioural/primary/cohort_membership.csv. Evidence: analysis_outputs/semester2_2026_qc/ (exclusion_summary.json, flagged_checks.csv, scope_metrics.csv and individual/pNNN_diagnostics.png). Rules: participant_qc/config.json; README.md, “Participant QC and exclusion review”.','SourceNote')
doc.add_paragraph('Saved QC generated 17 October 2026; its input hash matches the current data/data_virus_all.csv. Prepared from saved outputs on 21 October 2026. Source data, analysis membership and diagnostic plots were not changed.','SourceNote')

caps={23:'Manual post triggered both exclusion criteria.',49:'Aid 70%, Aid 95% again and Manual post triggered both criteria; pooled scopes also triggered anticipatory flags.',51:'Aid 70% and Aid 95% again triggered anticipatory flags; Manual post triggered the near-chance criterion.',65:'Anticipatory flags occurred in both manual segments, all aided phases and pooled scopes; no near-chance flag.',75:'Manual post triggered the near-chance criterion only.',79:'Manual post triggered both exclusion criteria.',81:'Manual post triggered the near-chance criterion only.'}
for idx,pid in enumerate(ids):
    if idx==0:
        s=doc.add_section(WD_SECTION_START.NEW_PAGE)
        s.orientation=WD_ORIENT.LANDSCAPE; s.page_width=Inches(11); s.page_height=Inches(8.5)
        s.top_margin=s.bottom_margin=Inches(.5); s.left_margin=s.right_margin=Inches(.75)
        s.header_distance=s.footer_distance=Inches(.25)
    else: doc.add_page_break()
    p=doc.add_paragraph(style='FigureCaption'); p.paragraph_format.keep_with_next=True
    p.add_run(f'Participant {pid:03d}. ').bold=True; p.add_run(caps[pid])
    p=doc.add_paragraph(style='PlotImage')
    shape=p.add_run().add_picture(str(QC/f'individual/p{pid:03d}_diagnostics.png'),width=Inches(9.5))
    shape._inline.docPr.set('descr',f'Participant {pid:03d}: recorded RTs, fraction below 200 ms and accuracy across calibration, manual pre, three aided phases and manual post. '+caps[pid])

target=OUT/'dynamic_reliability_reviewed_exclusions.docx'
doc.save(target)
with ZipFile(target) as z:
    media=[z.read(n) for n in z.namelist() if n.startswith('word/media/')]
    assert len(media)==7
    for pid in ids: assert (QC/f'individual/p{pid:03d}_diagnostics.png').read_bytes() in media
assert len(Document(target).tables)==1
sources=[membership_path,QC/'manifest.json',QC/'scope_metrics.csv',QC/'flagged_checks.csv',QC/'exclusion_summary.json',ROOT/'participant_qc/config.json']+[QC/f'individual/p{pid:03d}_diagnostics.png' for pid in ids]
qa={'participant_ids':ids,'source_n':89,'primary_n':82,'input_matches_qc_manifest':True,'embedded_source_images_verified':7,'source_sha256':{str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sources}}
qa['collection_plan'] = {'target_retained_n':96,'currently_retained_n':82,'remaining_n':14,'replacement_ids':ids,'new_ids':list(range(90,97)),'assumption':'All 14 further participants pass QC','confirmed_on':'2026-10-21'}
(OUT/'source_verification.json').write_text(json.dumps(qa,indent=2)+'\n')
print(target)
print('Verified 7 primary exclusions, source input hash, 1 table and 7 byte-identical source diagnostic plots.')
