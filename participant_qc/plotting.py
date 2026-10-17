"""Matplotlib reports adapted from hmm_pmdc's anticipatory-response audit.

Participant selection, peer pools, thresholds, and sample sizes are data-driven.
"""
import math
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.colors import ListedColormap
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
import numpy as np

from participant_qc.core import CATEGORIES, SCOPES, SEGMENTS, exclusion_summary, segments_for_person, number

BLUE, FAST, NEG, GREY = "#236B8E", "#E18422", "#B42042", "#7E8790"
GROUP_COLORS = {"HP95_LP65": BLUE, "HP65_LP95": "#CE6A16"}
CATEGORY_LABELS = ["Calibration\ntarget", "Manual target\n(diagnostic only)", "Near-chance\nor worse", "Anticipatory\nresponding"]


def setup_style():
    plt.rcParams.update({"font.family":"DejaVu Sans", "font.size":10,
        "axes.labelcolor":"#34434F", "text.color":"#243746", "axes.titlecolor":"#243746",
        "pdf.fonttype":42, "ps.fonttype":42, "savefig.facecolor":"white"})


def save_pair(fig, out, stem):
    base = Path(out)/stem
    base.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(base.with_suffix(".png"), dpi=180)
    fig.savefig(base.with_suffix(".pdf"), metadata={"Title":stem, "Subject":"Full source-cohort QC audit; primary analysis selection is documented in the report"})


def decorate_phase(ax, segments, top=False):
    shades = {"CAL_LP": "#F1EDF6", "M_HP": "#FAF0E2", "A_HP": "#F8ECD9",
              "M_LP": "#ECEFF2", "A_LP": "#EFF4F8"}
    for segment in segments:
        code, label, lo, hi, _, block = segment
        shade = shades[code]
        if block["AUTOMATION_ON"]: label += f" {100*block['AID_ACCURACY']:g}%"
        ax.axvspan(lo-.5, hi+.5, color=shade, zorder=-20)
        if lo > 1:
            ax.axvline(lo-.5, color="#B8C0C6", lw=.65, zorder=-10)
        if top:
            ax.text((lo+hi)/2, 1.045, label.replace(" ","\n",1), transform=ax.get_xaxis_transform(),
                    ha="center", va="bottom", fontsize=8, color="#45515C")
    ax.set_xlim(.5,1900.5)
    ax.set_xticks([1,300,700,1100,1500,1900])
    ax.tick_params(axis="both", labelsize=8, length=3)
    ax.spines[["top","right"]].set_visible(False)
    ax.spines[["left","bottom"]].set_color("#A4ADB5")
    ax.grid(axis="y", color="white", lw=.8, zorder=-5)


def peer_references(result):
    by_id = {}
    for b in result["bins"]:
        by_id.setdefault(b["participant_id"], []).append(b)
    references, pools, band_rows = {}, {}, []
    aligned = {(b["participant_id"], b["scope"], b["condition_trial_midpoint"]): b for b in result["bins"]}
    for person in result["summaries"]:
        pid = person["participant_id"]
        peers = [s["participant_id"] for s in result["summaries"] if s["participant_id"] != pid
                 and s["automation_reliability_pattern"] == person["automation_reliability_pattern"]
                 and s["anticipatory"] == "clear" and s["n_not_assessable"] == 0]
        pools[str(pid)] = peers
        band = {key:[] for key in ("accuracy", "fast_fraction")}
        for i, b in enumerate(by_id[pid]):
            row = dict(participant_id=pid, scope=b["scope"], condition_trial_midpoint=b["condition_trial_midpoint"], trial_midpoint=b["trial_midpoint"], peer_n=len(peers))
            for key in band:
                vals = [100*aligned[p,b["scope"],b["condition_trial_midpoint"]][key] for p in peers
                        if aligned[p,b["scope"],b["condition_trial_midpoint"]][key] is not None]
                quantiles = np.percentile(vals,[25,50,75]).tolist() if vals else [None]*3
                band[key].append([float("nan") if x is None else x for x in quantiles])
                row.update({f"{key}_{name}":v for name,v in zip(("q25","median","q75"),quantiles)})
            band_rows.append(row)
        references[pid] = {key:np.array(values) for key,values in band.items()}
    return by_id, references, pools, band_rows


def percent(value):
    return "not assessable" if value is None else f"{100*value:.1f}%"


def trace_figure(selected, people, result, cfg, by_id, references, pools, comparator=None):
    ncols = len(selected)
    fig, axs = plt.subplots(3,ncols,figsize=(17,11.7),sharex=True,squeeze=False,
                            gridspec_kw={"height_ratios":[1.65,1,1]})
    fig.subplots_adjust(left=.07,right=.985,top=.785,bottom=.16,hspace=.35,wspace=.15)
    fig.suptitle("Anticipatory-response review",x=.07,y=.975,ha="left",fontsize=24,fontweight="bold",color="#172B3A")
    fig.text(.07,.938,"Full recorded sequence, including calibration | primary-analysis selection is documented in the QC report",fontsize=12,color="#475664")
    overall = {m["participant_id"]:m for m in result["metrics"] if m["scope"] == "experimental_overall"}
    summaries = {s["participant_id"]:s for s in result["summaries"]}
    threshold_ms = 1000*cfg["fast_rt_seconds"]
    for col,pid in enumerate(selected):
        rows, s = people[pid], overall[pid]
        segments = segments_for_person(rows)
        center = (axs[0,col].get_position().x0+axs[0,col].get_position().x1)/2
        flagged = summaries[pid]["anticipatory"] == "flag"
        label = f"Participant {pid:03d} | {s['automation_reliability_pattern']}" + (" | comparator" if pid == comparator else "")
        fig.text(center,.894,label,ha="center",fontsize=11,fontweight="bold",color=NEG if flagged else BLUE)
        fig.text(center,.865,f"Experimental <{threshold_ms:g} ms: {s['n_fast']}/{s['n_trials']} ({percent(s['fast_fraction'])}) | nonpositive: {s['n_nonpositive_rt']}",ha="center",fontsize=9.8)
        note = " | INTEGRITY REVIEW" if summaries[pid]["n_not_assessable"] else ""
        fig.text(center,.844,f"Experimental accuracy: {percent(s['accuracy'])} | fast accuracy: {percent(s['fast_accuracy'])}{note}",ha="center",fontsize=9.6)
        x = np.array([r["global_trial"] for r in rows])
        rt = np.array([r["rt"] if r["rt"] is not None else np.nan for r in rows])
        answered = np.array([r["answered"] for r in rows])
        timeout = np.array([r["timeout"] for r in rows])
        nonpositive = answered & np.isfinite(rt) & (rt <= 0)
        fast = answered & np.isfinite(rt) & (rt < cfg["fast_rt_seconds"])
        normal = answered & np.isfinite(rt) & ~fast
        ax = axs[0,col]
        ax.scatter(x[normal],1000*rt[normal],s=5,c=BLUE,alpha=.3,linewidths=0,rasterized=True)
        ax.scatter(x[fast & ~nonpositive],1000*rt[fast & ~nonpositive],s=8,c=FAST,alpha=.7,linewidths=0,rasterized=True)
        ax.scatter(x[nonpositive],1000*rt[nonpositive],s=22,c=NEG,marker="x",lw=.85,zorder=5)
        deadlines = np.array([1000*number(r["trial_deadline_s"]) if number(r["trial_deadline_s"]) is not None else np.nan for r in rows])
        ax.scatter(x[timeout],deadlines[timeout],s=25,c="#6B4B89",marker="v",zorder=6)
        for _, _, lo, hi, _, block in segments:
            ax.hlines(block["TRIAL_DEADLINE_MS"],lo,hi,color="#6B4B89",ls=":",lw=.8)
        ax.axhline(threshold_ms,color=FAST,ls="--",lw=1)
        ax.axhline(0,color="#AEB6BC",lw=.6)
        ax.set_yscale("symlog",linthresh=threshold_ms,linscale=1)
        finite = 1000*rt[np.isfinite(rt)]
        ax.set_ylim(min(-32,float(finite.min())*1.2) if len(finite) else -32,
                    max(3500,float(finite.max())*1.1) if len(finite) else 3500)
        ticks = sorted(set([0,100,threshold_ms,500,1000,2000,3000]))
        ax.set_yticks(ticks); ax.set_yticklabels([f"{t:g}" for t in ticks]); ax.minorticks_off()
        decorate_phase(ax,segments,top=True)
        xx = [b["trial_midpoint"] for b in by_id[pid]]
        for row,key,color in [(1,"fast_fraction",FAST),(2,"accuracy",BLUE)]:
            ax = axs[row,col]
            lower,median,upper = references[pid][key].T
            ax.fill_between(xx,lower,upper,color=GREY,alpha=.22,linewidth=0,zorder=1)
            ax.plot(xx,median,color=GREY,lw=1,ls=":",zorder=2)
            yy = [100*b[key] if b[key] is not None else np.nan for b in by_id[pid]]
            ax.plot(xx,yy,color=color,lw=1.6,marker="o",ms=2.7,zorder=3)
            ax.set_ylim(-4,104); ax.set_yticks([0,25,50,75,100])
            if row == 2:
                ax.axhline(50,color="#56636D",ls="--",lw=.9)
                fast_y = [100*b["fast_accuracy"] if b["fast_accuracy"] is not None else np.nan for b in by_id[pid]]
                ax.scatter(xx,fast_y,s=[7+min(30,b["n_fast"]) for b in by_id[pid]],
                           facecolors="none",edgecolors=FAST,linewidths=.9,zorder=4)
                ax.set_xlabel(f"Recorded trial | reference peers: {len(pools[str(pid)])}",fontsize=9)
            else:
                ax.axhline(100*cfg["fast_fraction_min"],color=NEG,lw=.8,ls="--")
            decorate_phase(ax,segments)
    axs[0,0].set_ylabel(f"Recorded RT (ms)\nlinear to {threshold_ms:g} ms; log above",fontsize=10)
    axs[1,0].set_ylabel(f"Responses <{threshold_ms:g} ms (%)\nper {cfg['bin_size']}-trial bin",fontsize=10)
    axs[2,0].set_ylabel(f"Accuracy (%)\nper {cfg['bin_size']}-trial bin",fontsize=10)
    handles = [Line2D([],[],marker="o",ls="",color=BLUE,label="Other answered RT"),
        Line2D([],[],marker="o",ls="",color=FAST,label=f"Positive RT <{threshold_ms:g} ms"),
        Line2D([],[],marker="x",ls="",color=NEG,label="Nonpositive RT"),
        Line2D([],[],marker="v",ls="",color="#6B4B89",label="Timeout at deadline"),
        Line2D([],[],marker="o",ls="",color=FAST,markerfacecolor="none",label="Fast-only accuracy"),
        Patch(facecolor=GREY,alpha=.22,label="Condition-aligned peers: median + IQR")]
    fig.legend(handles=handles,loc="lower center",bbox_to_anchor=(.52,.095),ncol=6,frameon=False,fontsize=8.8)
    fig.text(.07,.073,"Fast counts include nonpositive RTs; proportions use all trials in the stated scope. Accuracy counts timeouts as incorrect.",fontsize=9.2)
    fig.text(.07,.050,f"Lines: disjoint {cfg['bin_size']}-trial bins. Open orange points: fast-only accuracy; point size reflects fast-response count. Empty/invalid bins are missing.",fontsize=9.2)
    fig.text(.07,.027,"Peer bands use other complete participants in the same reliability pattern without anticipatory flags; matched by condition and within-block trial. Bins do not trigger flags.",fontsize=9.2)
    return fig


def screening_figure(result):
    people = result["summaries"]; chunks = np.array_split(np.arange(len(people)), 2 if len(people)>40 else 1)
    height = max(7, max(map(len,chunks))*.23+2.8)
    fig,axs = plt.subplots(1,len(chunks),figsize=(16,height),squeeze=False)
    fig.subplots_adjust(left=.07,right=.97,top=.9,bottom=.12,wspace=.25)
    fig.suptitle(f"Participant review flags | n = {len(people)}",x=.07,y=.97,ha="left",fontsize=22,fontweight="bold")
    exclusions=exclusion_summary(result)
    remaining=(f"{exclusions['primary_analysis']['included_participant_n']} included in primary analyses" if exclusions["exclusions_applied"] else
               f"{exclusions['remaining_if_applied_n']} remaining if applied")
    fig.text(.07,.935,f"Three exclusion criteria: {exclusions['unique_flagged_n']} unique participants ({exclusions['unique_flagged_percent']:.1f}%) | {remaining}",fontsize=12)
    cmap=ListedColormap(["#EDF1F4",NEG,"#E7B954"])
    for ax,indices in zip(axs[0],chunks):
        matrix=np.array([[{"clear":0,"flag":1,"not_assessable":2}[people[i][c]] for c in CATEGORIES] for i in indices])
        ax.imshow(matrix,cmap=cmap,vmin=0,vmax=2,aspect="auto")
        ax.set_yticks(range(len(indices))); ax.set_yticklabels([f"{people[i]['participant_id']:03d}" for i in indices],fontsize=8)
        ax.set_xticks(range(len(CATEGORIES))); ax.set_xticklabels(CATEGORY_LABELS,fontsize=9)
        ax.tick_params(length=0)
        for y,row in enumerate(matrix):
            for x,value in enumerate(row):
                if value: ax.text(x,y,"!" if value==1 else "?",ha="center",va="center",color="white" if value==1 else "#243746",fontsize=8)
        for spine in ax.spines.values(): spine.set_visible(False)
    fig.legend(handles=[Patch(color=cmap(i),label=label) for i,label in enumerate(["Clear for this screen","Review flag","Not assessable"])],loc="lower center",bbox_to_anchor=(.5,.055),ncol=3,frameon=False)
    fig.text(.07,.027,"Manual target flags are diagnostic only. This audit retains all source participants; analysis exclusions are documented in the report.",fontsize=10)
    return fig


def calibration_figure(result,cfg):
    fig,axs=plt.subplots(2,1,figsize=(17,10),sharey=True)
    fig.subplots_adjust(left=.07,right=.98,top=.87,bottom=.16,hspace=.5)
    fig.suptitle("Calibration and manual performance",x=.07,y=.97,ha="left",fontsize=23,fontweight="bold")
    fig.text(.07,.925,f"Calibration: final {cfg['calibration_tail_n']} trials | manual LP: diagnostic only | manual HP: descriptive, no target flag",fontsize=12)
    lookup={(m["participant_id"],m["scope"]):m for m in result["metrics"]}
    flagged={(c["participant_id"],c["scope"]) for c in result["checks"] if c["status"]=="flag" and c["category"] in ("calibration_target","manual_target")}
    entries=[("calibration_tail","Calibration",BLUE,"o"),("M_LP","Manual LP",FAST,"s"),("M_HP","Manual HP (descriptive)","#75627E","^")]
    for ax,group,target in zip(axs,["HP95_LP65","HP65_LP95"],[.77,.77]):
        ids=[s["participant_id"] for s in result["summaries"] if s["automation_reliability_pattern"]==group]
        ax.axhspan(100*target-cfg["manual_target_band_pp"],100*target+cfg["manual_target_band_pp"],color="#E8ECEF",alpha=.7)
        ax.axhline(100*target,color=GROUP_COLORS[group],lw=1,ls="--")
        for j,(scope,label,color,marker) in enumerate(entries):
            xx=np.arange(len(ids))+(j-1)*.21
            yy=[100*lookup[p,scope]["accuracy"] if lookup[p,scope]["accuracy"] is not None else np.nan for p in ids]
            ax.scatter(xx,yy,color=color,marker=marker,s=25,label=label,zorder=3)
            pick=[i for i,p in enumerate(ids) if (p,scope) in flagged]
            ax.scatter(xx[pick],np.array(yy)[pick],facecolors="none",edgecolors=NEG,s=72,linewidths=.9,zorder=4)
        ax.set_title(f"{group} | target {100*target:g}% | n = {len(ids)}",loc="left",fontweight="bold")
        ax.set_xticks(range(len(ids)));ax.set_xticklabels([f"{p:03d}" for p in ids],rotation=90,fontsize=8)
        ax.set_ylim(0,103);ax.set_ylabel("Accuracy (%)"); ax.set_xlabel("Participant")
        ax.spines[["top","right"]].set_visible(False);ax.grid(axis="y",color="#EFF2F4")
    handles,labels=axs[0].get_legend_handles_labels()
    handles += [Line2D([],[],marker="o",ls="",color=NEG,markerfacecolor="none",label="Target deviation flag")]
    fig.legend(handles=handles,loc="lower center",bbox_to_anchor=(.5,.065),ncol=4,frameon=False)
    fig.text(.07,.027,f"Grey band: manual target +/-{cfg['manual_target_band_pp']:g} pp. Calibration flag band: +/-{cfg['calibration_target_band_pp']:g} pp. Dashed lines: assigned targets.",fontsize=10)
    return fig


def chance_figure(result,cfg):
    people=result["summaries"]; chunks=np.array_split(np.arange(len(people)),2 if len(people)>40 else 1)
    lookup={(m["participant_id"],m["scope"]):m for m in result["metrics"]}
    fig,axs=plt.subplots(1,len(chunks),figsize=(17,max(8,max(map(len,chunks))*.24+3)),squeeze=False)
    fig.subplots_adjust(left=.06,right=.96,top=.91,bottom=.13,wspace=.22)
    fig.suptitle(f"Whole-scope accuracy | near-chance-or-worse screen <= {100*cfg['chance_accuracy_max']:g}%",x=.06,y=.975,ha="left",fontsize=20,fontweight="bold")
    cmap=plt.get_cmap("YlGnBu").copy();cmap.set_bad("#E7B954")
    labels=["Calibration LP","Manual HP","Aided HP","Manual LP","Aided LP","Aided overall","Experimental"]
    for ax,indices in zip(axs[0],chunks):
        matrix=np.array([[lookup[people[i]["participant_id"],s[0]]["accuracy"] if lookup[people[i]["participant_id"],s[0]]["accuracy"] is not None else np.nan for s in SCOPES] for i in indices])
        ax.imshow(matrix,cmap=cmap,vmin=0,vmax=1,aspect="auto")
        ax.set_xticks(range(len(SCOPES)));ax.set_xticklabels(labels,rotation=45,ha="right",fontsize=8)
        ax.set_yticks(range(len(indices)));ax.set_yticklabels([f"{people[i]['participant_id']:03d}" for i in indices],fontsize=8);ax.tick_params(length=0)
        for y,row in enumerate(matrix):
            for x,value in enumerate(row):
                ax.text(x,y,"?" if np.isnan(value) else f"{100*value:.0f}",ha="center",va="center",fontsize=6.8,color="white" if value>.7 else "#243746")
                if np.isfinite(value) and value<=cfg["chance_accuracy_max"]+1e-12:
                    ax.add_patch(plt.Rectangle((x-.49,y-.49),.98,.98,fill=False,edgecolor=NEG,lw=1.4))
        for spine in ax.spines.values():spine.set_visible(False)
    fig.text(.06,.035,"Cells show rounded percentages; flags use unrounded values. Red outline: screen triggered. Yellow/?: not assessable. Timeouts count as incorrect.",fontsize=10)
    return fig


def cohort_figure(result,cfg,comparisons):
    overall={m["participant_id"]:m for m in result["metrics"] if m["scope"]=="experimental_overall"}
    rank=sorted(result["summaries"],key=lambda s:(-(overall[s["participant_id"]]["fast_fraction"] or 0),s["participant_id"]))
    comparators={m["comparator"] for m in comparisons if m["comparator"] is not None}
    selected=list(dict.fromkeys([s["participant_id"] for s in rank[:6]]+sorted(comparators)))
    fig=plt.figure(figsize=(17,11.7)); grid=fig.add_gridspec(2,2,left=.07,right=.97,top=.86,bottom=.17,hspace=.43,wspace=.28,height_ratios=[1,1.1])
    ax=fig.add_subplot(grid[0,:]); ms=1000*cfg["fast_rt_seconds"]
    fig.suptitle("Anticipatory responding in cohort context",x=.07,y=.975,ha="left",fontsize=23,fontweight="bold")
    fig.text(.07,.933,f"{len(rank)} participants | primary threshold: {ms:g} ms | whole-scope burden and accuracy jointly determine flags",fontsize=12)
    colors=[NEG if s["anticipatory"]=="flag" else BLUE if s["participant_id"] in comparators else "#A9B6C0" for s in rank]
    values=[100*overall[s["participant_id"]]["fast_fraction"] if overall[s["participant_id"]]["fast_fraction"] is not None else np.nan for s in rank]
    ax.bar(range(len(rank)),values,color=colors,width=.82)
    ax.axhline(100*cfg["fast_fraction_min"],ls="--",color=NEG,lw=1)
    ax.set_xticks(range(len(rank)));ax.set_xticklabels([f"{s['participant_id']:03d}" for s in rank],rotation=90,fontsize=7)
    ax.set_ylabel(f"Experimental responses <{ms:g} ms (%)");ax.set_xlabel("Participant, ranked by fast-response proportion")
    ax.set_title("A. Whole experimental session",loc="left",fontweight="bold")
    ax.spines[["top","right"]].set_visible(False);ax.grid(axis="y",color="#E8ECEF");ax.set_axisbelow(True)
    ax.legend(handles=[Patch(color=NEG,label="Anticipatory flag in any scope"),Patch(color=BLUE,label="Matched comparator"),Patch(color="#A9B6C0",label="Other participant")],frameon=False,ncol=3,loc="upper right",fontsize=9)
    ax=fig.add_subplot(grid[1,0]);lookup={(m["participant_id"],m["scope"]):m for m in result["metrics"]}
    matrix=np.array([[100*lookup[p,s[0]]["fast_fraction"] if lookup[p,s[0]]["fast_fraction"] is not None else np.nan for s in SEGMENTS] for p in selected])
    ax.imshow(matrix,cmap="YlOrRd",vmin=0,vmax=100,aspect="auto")
    ax.set_xticks(range(len(SEGMENTS)));ax.set_xticklabels([s[1].replace(" ","\n",1) for s in SEGMENTS],fontsize=9)
    ax.set_yticks(range(len(selected)));ax.set_yticklabels([f"{p:03d}" for p in selected]);ax.set_title("B. Condition-specific fast-response proportions",loc="left",fontweight="bold",pad=12)
    for i,row in enumerate(matrix):
        for j,v in enumerate(row):ax.text(j,i,"?" if np.isnan(v) else f"{v:.1f}%",ha="center",va="center",fontsize=9,color="white" if v>65 else "#243746")
    ax.tick_params(length=0)
    ax=fig.add_subplot(grid[1,1])
    for i,p in enumerate(selected):
        yy=[100*overall[p][f"fraction_rt_lt_{t}ms"] if overall[p][f"fraction_rt_lt_{t}ms"] is not None else np.nan for t in cfg["sensitivity_thresholds_ms"]]
        ax.plot(cfg["sensitivity_thresholds_ms"],yy,marker="o",lw=1.5,label=f"{p:03d}",color=plt.get_cmap("tab10")(i%10))
    ax.set_title("C. Sensitivity to the descriptive RT threshold",loc="left",fontweight="bold",pad=12)
    ax.set_xlabel("RT threshold (ms), including nonpositive RTs");ax.set_ylabel("Experimental responses below threshold (%)")
    ax.set_xticks(cfg["sensitivity_thresholds_ms"]);ax.set_ylim(bottom=-1);ax.grid(color="#E8ECEF")
    ax.spines[["top","right"]].set_visible(False);ax.legend(ncol=3,frameon=False,fontsize=9)
    fig.text(.07,.085,"Heatmap and sensitivity curves: six highest experimental fast-response proportions plus available matched comparators.",fontsize=10)
    fig.text(.07,.060,f"Flag requires >= {100*cfg['fast_fraction_min']:g}% fast trials, at least {cfg['fast_count_min']} fast responses, and fast accuracy <= {100*cfg['fast_accuracy_max']:g}% within the same whole scope.",fontsize=10)
    fig.text(.07,.035,"A participant can be flagged within one block despite a low overall fast-response rate. All individual pages and flagged comparisons are linked in the report.",fontsize=10)
    return fig


def generate_plots(people,result,cfg,comparisons,out):
    setup_style(); out=Path(out)
    by_id,references,pools,band_rows=peer_references(result)
    for stem,figure in [("screening_overview",screening_figure(result)),
        ("calibration_manual_accuracy",calibration_figure(result,cfg)),
        ("whole_scope_accuracy",chance_figure(result,cfg)),
        ("anticipatory_cohort",cohort_figure(result,cfg,comparisons))]:
        save_pair(figure,out,stem);plt.close(figure)
    individuals=[];comparison_pages=[]
    with PdfPages(out/"all_individual_diagnostics.pdf") as pdf:
        for pid in people:
            stem=f"individual/p{pid:03d}_diagnostics"
            fig=trace_figure([pid],people,result,cfg,by_id,references,pools)
            save_pair(fig,out,stem);pdf.savefig(fig);plt.close(fig)
            individuals.append(dict(participant_id=pid,stem=stem))
            print(f"Rendered participant {pid:03d}",flush=True)
    if comparisons:
        with PdfPages(out/"anticipatory_comparisons.pdf") as pdf:
            page=0
            for match in comparisons:
                for start in range(0,len(match["targets"]),2):
                    page+=1;targets=match["targets"][start:start+2]
                    selected=([match["comparator"]] if match["comparator"] is not None else [])+targets
                    stem=f"comparisons/comparison_{page:02d}"
                    fig=trace_figure(selected,people,result,cfg,by_id,references,pools,match["comparator"])
                    if match["comparator"] is None:
                        fig.text(.5,.812,"No matching comparator available",ha="center",color=NEG,fontsize=10)
                    save_pair(fig,out,stem);pdf.savefig(fig);plt.close(fig)
                    comparison_pages.append(dict(stem=stem,targets=targets,comparator=match["comparator"]))
    return individuals,comparison_pages,pools,band_rows
