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

ROOT = Path('/Users/rjb779/Documents/pygame_virus_time_pressure')
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
s.header.paragraphs[0].text='Time-pressure study | Reviewed participant exclusions'
p=s.footer.paragraphs[0]; p.alignment=WD_ALIGN_PARAGRAPH.RIGHT
p.add_run('Page ')
fld=OxmlElement('w:fldSimple'); fld.set(qn('w:instr'),'PAGE'); p._p.append(fld)
doc.core_properties.title='Time-pressure study: reviewed participant exclusions'
doc.core_properties.subject='Reviewed decisions dated 17 October 2026 and individual anticipatory-response plots'

doc.add_paragraph('Reviewed participant exclusions','Title')
doc.add_paragraph('Time-pressure study | Decisions recorded 17 October 2026','Subtitle')
doc.add_paragraph('Three of 87 participants (3.4%) were excluded from the primary analyses, leaving 84 participants. The table below summarises the reviewed decisions; the following pages reproduce the individual anticipatory-response diagnostic plots.')
rows=[
['Participant','Reviewed exclusion reason','Supporting diagnostic evidence'],
['004','Anticipatory responding and near-chance performance','Aided HP: 210/400 trials (52.5%) had RT <200 ms; fast-response accuracy was 51.9%. Overall block accuracy was 52.5%.\nPooled aided trials also met the anticipatory criterion: 217/800 (27.1%) fast, with 51.2% fast-response accuracy.'],
['010','Anticipatory responding','Manual LP: 129/400 trials (32.3%) had RT <200 ms; fast-response accuracy was 45.0%.'],
['048','Anticipatory responding','Manual HP: 147/400 trials (36.8%) had RT <200 ms; fast-response accuracy was 52.4%.']]
t=doc.add_table(rows=4,cols=3); t.style='Table Grid'
for i,vals in enumerate(rows):
    trpr=t.rows[i]._tr.get_or_add_trPr(); trpr.append(OxmlElement('w:cantSplit'))
    for j,value in enumerate(vals):
        cell=t.cell(i,j); cell.vertical_alignment=WD_CELL_VERTICAL_ALIGNMENT.CENTER
        p=cell.paragraphs[0]; p.style='TableText'; p.add_run(value)
        if j==0: p.alignment=WD_ALIGN_PARAGRAPH.CENTER
        if i==0:
            p.runs[0].bold=True
            sh=OxmlElement('w:shd'); sh.set(qn('w:fill'),'F2F4F7'); cell._tc.get_or_add_tcPr().append(sh)
    if i==0: trpr.append(OxmlElement('w:tblHeader'))
apply_table_geometry(t,[1152,2376,5832])
doc.add_paragraph('Source: reviewed exclusion decisions and saved participant QC scope metrics. HP = high pressure (1 s); LP = low pressure (3 s).','SourceNote')
doc.add_paragraph('Review criteria', 'Heading 2')
doc.add_paragraph('Anticipatory responding required ≥20% of all trials with RT <200 ms, ≥20 fast responses, and ≤55% accuracy on those fast responses, jointly within the same assessed scope. Near-chance performance meant ≤55% overall accuracy, counting timeouts as incorrect. These were retrospective descriptive criteria, not preregistered exclusions or evidence of equivalence to chance.')
doc.add_paragraph('No participant met the calibration-target exclusion criterion. Fourteen participants had manual-LP target-deviation flags; these were diagnostic only. Participant 010 was excluded for the separate anticipatory criterion.')
doc.add_page_break()
doc.add_paragraph('Recorded ID duplication and correction', 'Heading 1')
doc.add_paragraph('Recorded participant ID 083 was used for two separate runs on 15 October 2026. No run was originally recorded as participant 084. Following the reviewed decision to treat the two runs as different participants, the later 083 run was assigned analysis ID 084.')
p=doc.add_paragraph()
p.add_run('Run 20261015_125827 (12:58:27): ').bold=True
p.add_run('recorded ID 083; retained as analysis ID 083.')
p=doc.add_paragraph()
p.add_run('Run 20261015_130522 (13:05:22): ').bold=True
p.add_run('recorded ID 083; reassigned to analysis ID 084.')
doc.add_paragraph('Both participants are retained in the primary analyses. This is an identity correction, not an exclusion: it does not add to the three reviewed exclusions or reduce the 87-participant source cohort. Both runs retain the task allocation administered under recorded ID 083.')
doc.add_paragraph('The current combined dataset contains 87 distinct runs, 86 original recorded IDs and 87 unique corrected analysis IDs. Recorded ID 083 is the only reused ID; 084 is absent only from the original recorded IDs and is present in the corrected analysis dataset.')
doc.add_paragraph('Identity sources: config/participant_identity_corrections.csv; analysis_outputs/semester2_2026_qc/identity_corrections_summary.csv; data/semester2_2026_data/data_virus_all.csv.','SourceNote')
doc.add_paragraph('Remaining participant IDs to collect', 'Heading 2')
doc.add_paragraph('Uncollected allocations: 084, 088, 089, 090, 091, 092, 093, 094, 095 and 096 (10 participants).', 'Normal')
doc.add_paragraph('A true task participant 084 is still needed: O21 × HP65_LP95 × flipped keys. The existing analysis ID 084 received task ID 083’s allocation.', 'Normal')
doc.add_paragraph('If replacing the three excluded participants as well, add allocations 004, 010 and 048, bringing the total to 13 collections.', 'Normal')
doc.add_paragraph('Collection status checked against the raw exports on 21 October 2026.', 'SourceNote')
doc.add_paragraph('Reading the plots', 'Heading 2')
doc.add_paragraph('Each plot shows the full recorded sequence, including calibration, with trial-level RTs, fast-response rates and accuracy in 50-trial bins. Plot headline statistics pool all 1,600 experimental trials; the table reports the specific scopes that triggered exclusion. Binned patterns are descriptive and do not independently trigger exclusion.')
doc.add_paragraph('Provenance: config/participant_exclusions.csv; analysis_outputs/semester2_2026_qc/scope_metrics.csv; individual/p004_diagnostics.png, p010_diagnostics.png and p048_diagnostics.png within that QC folder. Prepared 21 October 2026 from saved outputs; source data and decisions were not changed.','SourceNote')

caps={4:'Aided HP triggered both criteria; pooled aided trials also triggered the anticipatory criterion.',10:'Manual LP triggered the anticipatory criterion.',48:'Manual HP triggered the anticipatory criterion.'}
for idx,pid in enumerate((4,10,48)):
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
    shape._inline.docPr.set('descr',f'Participant {pid:03d}: recorded RTs, fraction of responses below 200 ms and accuracy across calibration and four experimental blocks, with condition-aligned peer references.')

target=OUT/'time_pressure_reviewed_exclusions.docx'
doc.save(target)
# Verify exact source images are embedded and the reviewed IDs agree.
with open(ROOT/'config/participant_exclusions.csv',newline='') as f:
    assert {int(r['participant_id']) for r in csv.DictReader(f)}=={4,10,48}
with ZipFile(target) as z:
    media=[z.read(n) for n in z.namelist() if n.startswith('word/media/')]
    assert len(media)==3
    for pid in (4,10,48): assert (QC/f'individual/p{pid:03d}_diagnostics.png').read_bytes() in media
assert len(Document(target).tables)==1
print(target)
print('Verified 3 reviewed IDs, 1 table and 3 byte-identical embedded diagnostic plots.')
