"""Decision-specific cohort, individual and matched-comparison figures."""
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
import numpy as np

from participant_qc.core import MAIN, number, segments

BLUE, ORANGE, RED, GREY = "#236B8E", "#C56B1A", "#B42042", "#7E8790"
LABELS = {"PRACTICE":"Practice (diagnostic)", "MANUAL":"Manual", "AIDFIRST":"Aid first",
          "STIMFIRST":"Stimulus first", "aided_overall":"Pooled aided", "experimental_overall":"All main trials"}
SHADES = {"PRACTICE":"#F1EDF6", "MANUAL":"#ECEFF2", "AIDFIRST":"#EDF5FA", "STIMFIRST":"#FBF0E3"}


def save_pair(fig, out, stem):
    base = Path(out)/stem
    base.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(base.with_suffix(".png"), dpi=160, facecolor="white")
    fig.savefig(base.with_suffix(".pdf"), facecolor="white", metadata={"Title":stem, "Subject":"Review evidence; no exclusions applied"})
    plt.close(fig)
    return stem


def peer_references(result):
    aligned = {(b["participant_id"],b["scope"],b["decision"],b["trial_start"]):b for b in result["bins"]}
    pools, bands = {}, []
    for s in result["summaries"]:
        pid = s["participant_id"]
        peers = [p["participant_id"] for p in result["summaries"] if p["participant_id"] != pid and s["calibration_target_accuracy"] is not None and p["calibration_target_accuracy"] == s["calibration_target_accuracy"] and p["anticipatory"] == "clear" and not p["n_not_assessable"]]
        pools[str(pid)] = peers
        for b in (b for b in result["bins"] if b["participant_id"] == pid):
            row = {k:b[k] for k in ("participant_id", "scope", "decision", "trial_start", "trial_end", "condition_trial_midpoint", "trial_midpoint")}
            row["peer_n"] = len(peers)
            for key in ("accuracy", "fast_fraction", "fast_accuracy"):
                vals = [aligned[p,b["scope"],b["decision"],b["trial_start"]][key] for p in peers]
                vals = [v for v in vals if v is not None]
                row[f"{key}_peer_n"] = len(vals)
                qq = np.percentile(vals,[25,50,75]).tolist() if vals else [None]*3
                row.update({f"{key}_{name}":v for name,v in zip(("q25","median","q75"),qq)})
            bands.append(row)
    return pools, bands


def percent(v):
    return "NA" if v is None else f"{v*100:.1f}%"


def floats(values, multiplier=1):
    return np.array([float("nan") if v is None else multiplier*v for v in values])


def decorate(ax, meta, top=False):
    for code, _, lo, hi in segments(meta):
        ax.axvspan(lo-.5,hi+.5,color=SHADES[code],zorder=-10)
        if lo > 1:
            ax.axvline(lo-.5,color="#ADB6BC",lw=.65)
        if top:
            label = "Practice" if code == "PRACTICE" else LABELS[code]
            ax.text((lo+hi)/2,1.025,label,transform=ax.get_xaxis_transform(),ha="center",fontsize=7)
    ax.set_xlim(.5,840.5)
    ax.set_xticks([1,60,320,580,840])
    ax.spines[["top","right"]].set_visible(False)
    ax.grid(axis="y",color="white",lw=.7)
    ax.tick_params(labelsize=8)


def trace_figure(selected, people, result, cfg, pools, bands, comparator=None):
    cols = [(p,d) for p in selected for d in (1,2)]
    fig, axes = plt.subplots(4,len(cols),figsize=(16 if len(cols)==2 else 23,12),squeeze=False,
                             gridspec_kw={"height_ratios":[1.5,1,1,1]})
    fig.subplots_adjust(left=.065,right=.985,top=.79,bottom=.13,hspace=.31,wspace=.21)
    fig.suptitle("Aid-onset participant review" if comparator is None else "Aid-onset matched comparison",x=.065,y=.97,ha="left",fontsize=23,fontweight="bold")
    fig.text(.065,.937,"Decision phases scored separately | retrospective screens | no exclusions applied",fontsize=12)
    summaries = {s["participant_id"]:s for s in result["summaries"]}
    overall = {(m["participant_id"],m["decision"]):m for m in result["metrics"] if m["scope"] == "experimental_overall"}
    for col,(pid,d) in enumerate(cols):
        s, m = summaries[pid], overall[pid,d]
        xcenter = sum((axes[0,col].get_position().x0,axes[0,col].get_position().x1))/2
        role = "comparator" if pid == comparator else "candidate" if s["exclude_by_rules"] else "review"
        fig.text(xcenter,.895,f"Participant {pid:03d} | D{d} | {role}",ha="center",fontsize=11,fontweight="bold",color=RED if s["exclude_by_rules"] else BLUE)
        fig.text(xcenter,.872,f"Main accuracy {percent(m['accuracy'])}; fast {m['n_fast']}/{m['n_trials']} ({percent(m['fast_fraction'])})",ha="center",fontsize=9)
        fig.text(xcenter,.851,f"Fast accuracy {percent(m['fast_accuracy'])}; nonpositive RTs {m['n_nonpositive_rt']}",ha="center",fontsize=9)
        fig.text(xcenter,.831,f"Run {s['run_timestamp']} | peers {len(pools[str(pid)])}"+(" | INCOMPLETE" if s['n_not_assessable'] else ""),ha="center",fontsize=8)
        rr = people[pid]
        x = np.array([r["session_trial"] for r in rr])
        rt = floats([number(r[f"decision{d}_rt_s"]) for r in rr],1000)
        answered = np.array([r[f"decision{d}_response"] in ("BLACK","WHITE") for r in rr])
        fast = answered & np.isfinite(rt) & (rt < 1000*cfg["fast_rt_seconds"])
        nonpositive = answered & np.isfinite(rt) & (rt <= 0)
        ax = axes[0,col]
        for mask,color,size in ((answered & ~fast,BLUE,5),(fast & ~nonpositive,ORANGE,8)):
            ax.scatter(x[mask],rt[mask],s=size,c=color,alpha=.45,linewidths=0,rasterized=True)
        ax.scatter(x[nonpositive],rt[nonpositive],s=24,c=RED,marker="x",lw=.8)
        timeout = np.array([r[f"decision{d}_response"] == "TIMEOUT" for r in rr])
        missing = answered & ~np.isfinite(rt)
        ax.scatter(x[timeout],np.zeros(sum(timeout)),s=20,marker="v",c="#76549B")
        ax.scatter(x[missing],np.zeros(sum(missing)),s=22,marker="x",c=GREY)
        threshold = cfg["fast_rt_seconds"]*1000
        ax.axhline(threshold,color=ORANGE,ls="--",lw=.9)
        ax.set_yscale("symlog",linthresh=threshold)
        finite = rt[np.isfinite(rt)]
        ax.set_ylim(min(-30,float(min(finite))*1.15) if len(finite) else -30,max(2000,float(max(finite))*1.15) if len(finite) else 2000)
        ticks = [0,100,200,500,1000,2000,10000,60000]
        ticks = [v for v in ticks if v <= ax.get_ylim()[1]]
        ax.set_yticks(ticks,labels=[str(v) for v in ticks]);ax.minorticks_off()
        decorate(ax,s,True)
        bb = [b for b in result["bins"] if b["participant_id"] == pid and b["decision"] == d]
        refs = {(b["scope"],b["trial_start"]):b for b in bands if b["participant_id"] == pid and b["decision"] == d}
        for row,key,color in ((1,"fast_fraction",ORANGE),(2,"accuracy",BLUE),(3,"fast_accuracy",ORANGE)):
            ax = axes[row,col]
            # Separate lines at block boundaries rather than connecting unlike scopes.
            for code,_,_,_ in segments(s):
                block = [b for b in bb if b["scope"] == code]
                xx = [b["trial_midpoint"] for b in block]
                ref = [refs[b["scope"],b["trial_start"]] for b in block]
                ax.fill_between(xx,floats([r[f"{key}_q25"] for r in ref],100),floats([r[f"{key}_q75"] for r in ref],100),color=GREY,alpha=.22,lw=0)
                ax.plot(xx,floats([r[f"{key}_median"] for r in ref],100),color=GREY,ls=":",lw=1)
                yy = floats([b[key] for b in block],100)
                ax.plot(xx,yy,color=color,lw=1,alpha=.8)
                ax.scatter(xx,yy,s=[9+min(35,b["n_fast"]) if key=="fast_accuracy" else 10 for b in block],facecolors="none" if key=="fast_accuracy" else color,edgecolors=color,lw=.8)
            boundary = cfg["fast_fraction_min"] if key=="fast_fraction" else cfg["chance_accuracy_max"] if key=="accuracy" else cfg["fast_accuracy_max"]
            ax.axhline(100*boundary,color=RED,ls="--",lw=.8)
            ax.set_ylim(-4,104);ax.set_yticks([0,25,50,75,100]);decorate(ax,s)
        axes[3,col].set_xlabel("Chronological trial (practice + main)",fontsize=9)
    for row,label in enumerate(("RT (ms)\nlinear to threshold; log above","Fast responses (%)","All-trial accuracy (%)","Fast-only accuracy (%)")):
        axes[row,0].set_ylabel(label,fontsize=10)
    fig.text(.065,.071,"Grey median/IQR: complete peers with the same calibration target, without anticipatory candidate flags; aligned by condition and trial.",fontsize=9)
    fig.text(.065,.050,"Practice bins: 10 trials. Main bins: 50 trials (last bin: 10). Open fast-accuracy points scale with fast count; empty subsets are missing.",fontsize=9)
    fig.text(.065,.029,"Aid-first D1 and all D2 responses can be prepared before the response screen. Fast-RT flags require interpretation. Practice checks are diagnostic only.",fontsize=9)
    return fig


def cohort_figures(result,cfg,out):
    ids = [s["participant_id"] for s in result["summaries"]]
    columns = [("calibration_target",2,"calibration_tail"),("manual_target",2,"MANUAL"),("near_chance_or_worse",1,None),("near_chance_or_worse",2,None),("anticipatory",1,None),("anticipatory",2,None)]
    matrix = []
    for cat,d,scope in columns:
        row=[]
        for pid in ids:
            cc=[c for c in result["checks"] if c["participant_id"]==pid and c["category"]==cat and c["decision"]==d and (c["scope"]==scope if scope else c["use_for_exclusion"])]
            row.append(2 if any(c["status"]=="flag" for c in cc) else 1 if any(c["status"]=="not_assessable" for c in cc) else 0)
        matrix.append(row)
    fig,ax=plt.subplots(figsize=(17,6));fig.subplots_adjust(left=.14,right=.985,top=.82,bottom=.18)
    ax.imshow(matrix,aspect="auto",cmap=ListedColormap(["#E5F0F1","#B6BCC2",RED]),vmin=0,vmax=2)
    ax.set_yticks(range(6),["Practice target (diagnostic)","Manual target (diagnostic)","Low accuracy D1","Low accuracy D2","Anticipatory D1","Anticipatory D2"])
    ax.set_xticks(range(len(ids)),[f"{p:03d}" for p in ids],rotation=90,fontsize=7)
    fig.suptitle("Screening overview",x=.14,y=.94,ha="left",fontsize=22,fontweight="bold")
    fig.text(.14,.88,"Red = flag; grey = unassessable; pale = clear. Candidate totals count each participant once across main scopes and decisions.",fontsize=10)
    ax.set_xlabel("Participant");fig.text(.14,.05,"Target-deviation and all practice flags are diagnostic only. No exclusions applied.",fontsize=10)
    stems=[save_pair(fig,out,"figures/screening_overview")]
    fig,axes=plt.subplots(2,1,figsize=(16,9),sharex=True);fig.subplots_adjust(left=.07,right=.98,top=.85,bottom=.12,hspace=.35)
    for ax,scope,label,color in zip(axes,("calibration_tail","MANUAL"),("Practice final 40 trials - Decision 2","Manual full block - Decision 2"),(BLUE,ORANGE)):
        mm=[next(m for m in result["metrics"] if m["participant_id"]==p and m["scope"]==scope and m["decision"]==2) for p in ids]
        x=np.arange(len(ids)); y=floats([m['accuracy'] for m in mm],100)
        ax.errorbar(x,y,yerr=[y-floats([m['accuracy_lower'] for m in mm],100),floats([m['accuracy_upper'] for m in mm],100)-y],fmt="o",color=color,ms=4,lw=.7,capsize=2)
        targets=floats([m['calibration_target_accuracy'] for m in mm],100)
        band=cfg['calibration_target_band_pp' if scope=='calibration_tail' else 'manual_target_band_pp']
        ax.plot(x,targets,color=GREY,lw=1);ax.fill_between(x,targets-band,targets+band,color=GREY,alpha=.13)
        ax.set_ylim(30,102);ax.set_title(label,loc='left');ax.set_ylabel('Accuracy (%)');ax.grid(axis='y',alpha=.15)
    axes[-1].set_xticks(range(len(ids)),[f'{p:03d}' for p in ids],rotation=90,fontsize=8)
    axes[-1].set_xlabel('Participant')
    fig.suptitle('Calibration and Manual target context - diagnostic only',x=.07,y=.96,ha='left',fontsize=21,fontweight='bold')
    fig.text(.07,.91,'Points and descriptive Wilson intervals; grey band is the recorded target +/- configured tolerance. No target-deviation exclusions.',fontsize=10)
    fig.text(.07,.03,'Wilson intervals do not account for adaptive calibration or serial dependence.',fontsize=10)
    stems.append(save_pair(fig,out,'figures/calibration_manual_accuracy'))
    scopes=('PRACTICE',*MAIN,'aided_overall','experimental_overall')
    for kind in ('whole_scope_accuracy','anticipatory_cohort'):
        fig,axes=plt.subplots(2,3,figsize=(17,10));fig.subplots_adjust(left=.065,right=.98,top=.85,bottom=.12,hspace=.36,wspace=.25)
        for ax,scope in zip(axes.flat,scopes):
            flag_labels=[]
            for d,color in ((1,BLUE),(2,ORANGE)):
                mm=[m for m in result['metrics'] if m['scope']==scope and m['decision']==d and m['assessable']]
                if kind=='whole_scope_accuracy':
                    ax.scatter([m['participant_id']+(d-1.5)*.18 for m in mm],[100*m['accuracy'] for m in mm],s=16,color=color,alpha=.8,label=f'Decision {d}')
                    ax.axhline(cfg['chance_accuracy_max']*100,color=RED,ls='--',lw=.7)
                    ax.set(xlabel='Participant',ylabel='Accuracy (%)',ylim=(0,103))
                else:
                    mm=[m for m in mm if m['fast_accuracy'] is not None]
                    ax.scatter([100*m['fast_fraction'] for m in mm],[100*m['fast_accuracy'] for m in mm],s=[12+min(65,m['n_fast']/2) for m in mm],color=color,alpha=.6,label=f'Decision {d}')
                    ax.set(xlabel='Fast responses / all trials (%)',ylabel='Fast-only accuracy (%)',xlim=(-2,102),ylim=(-3,103))
                    ax.axvline(cfg['fast_fraction_min']*100,color=RED,ls='--',lw=.7)
                    ax.axhline(cfg['fast_accuracy_max']*100,color=RED,ls='--',lw=.7)
                    for m in mm:
                        if m['n_fast']>=cfg['fast_count_min'] and m['fast_fraction']>=cfg['fast_fraction_min'] and m['fast_accuracy']<=cfg['fast_accuracy_max']:
                            flag_labels.append(f"{m['participant_id']:03d}/D{d}")
                            ax.scatter([100*m['fast_fraction']],[100*m['fast_accuracy']],s=90,facecolors='none',edgecolors=RED,lw=1)
            title=LABELS[scope]
            if kind=='anticipatory_cohort':title+='\nFlags: '+(', '.join(flag_labels) or 'none')
            ax.set_title(title,loc='left',fontweight='bold',fontsize=10);ax.grid(alpha=.12);ax.spines[['top','right']].set_visible(False)
        axes[0,0].legend(loc='lower left',fontsize=9)
        title='Whole-scope accuracy by decision' if kind=='whole_scope_accuracy' else 'Anticipatory-response context by decision'
        fig.suptitle(title,x=.065,y=.96,ha='left',fontsize=22,fontweight='bold')
        fig.text(.065,.91,'Both decisions are screened separately in main scopes. Practice remains diagnostic; no exclusions applied.',fontsize=11)
        note='Low-accuracy screen: all-trial accuracy <= configured threshold. Timeouts count as incorrect.' if kind=='whole_scope_accuracy' else f"All three conditions must hold: RT < {cfg['fast_rt_seconds']*1000:g} ms on >= {cfg['fast_fraction_min']*100:g}% of trials, >= {cfg['fast_count_min']} fast responses, fast accuracy <= {cfg['fast_accuracy_max']*100:g}%."
        fig.text(.065,.045,note,fontsize=10)
        stems.append(save_pair(fig,out,f'figures/{kind}'))
    return stems


def generate_plots(people,result,cfg,comparisons,out):
    plt.rcParams.update({'font.family':'DejaVu Sans','font.size':10,'pdf.fonttype':42,'axes.labelcolor':'#34434F','text.color':'#243746'})
    pools,bands=peer_references(result)
    cohort=cohort_figures(result,cfg,out)
    individuals=[];pages=[]
    for pid in people:
        stem=f'individuals/participant_{pid:03d}'
        save_pair(trace_figure([pid],people,result,cfg,pools,bands),out,stem)
        individuals.append(dict(participant_id=pid,stem=stem))
    for match in comparisons:
        for pid in match['targets']:
            comparator=match['comparator']
            selected=[pid]+([comparator] if comparator is not None else [])
            stem=f'comparisons/participant_{pid:03d}'
            save_pair(trace_figure(selected,people,result,cfg,pools,bands,comparator),out,stem)
            pages.append(dict(participant_id=pid,comparator=comparator,stem=stem))
    return cohort,individuals,pages,pools,bands
