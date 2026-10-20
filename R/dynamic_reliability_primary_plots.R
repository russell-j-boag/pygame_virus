# Plot-only consumers of the revised analysis bundle.
# Vertical caps occupy 3% of panel width, corrected for horizontal panel count.
# Panel-relative units preserve these proportions across layouts and axis ranges.
dpr_errorbar_cap_fraction <- .03
dpr_errorbar_linewidth <- .70
dpr_errorbar_geom <- ggplot2::ggproto("GeomDprErrorbar",ggplot2::GeomErrorbar,
  draw_panel=function(data,panel_params,coord,lineend="butt",width=NULL,flipped_aes=FALSE,horizontal_panels=1) {
    if(!nrow(data))return(grid::zeroGrob())
    data <- coord$transform(data,panel_params)
    horizontal <- xor(flipped_aes,inherits(coord,"CoordFlip"))
    half <- grid::unit(dpr_errorbar_cap_fraction*horizontal_panels/2,"npc")
    gp <- grid::gpar(col=scales::alpha(data$colour,data$alpha),
      lwd=data$linewidth*ggplot2::.pt,lty=data$linetype,lineend=lineend)
    if(horizontal) {
      low <- grid::unit(data$xmin,"native");high <- grid::unit(data$xmax,"native")
      centre <- grid::unit(data$y,"native")
      stem <- grid::segmentsGrob(low,centre,high,centre,gp=gp,name="dpr.errorbar.stem")
      # Horizontal change/difference intervals use an uncapped line segment.
      return(grid::grobTree(stem,name="dpr.errorbar"))
    } else {
      low <- grid::unit(data$ymin,"native");high <- grid::unit(data$ymax,"native")
      centre <- grid::unit(data$x,"native")
      stem <- grid::segmentsGrob(centre,low,centre,high,gp=gp,name="dpr.errorbar.stem")
      a <- grid::segmentsGrob(centre-half,low,centre+half,low,gp=gp,name="dpr.errorbar.cap.lower")
      b <- grid::segmentsGrob(centre-half,high,centre+half,high,gp=gp,name="dpr.errorbar.cap.upper")
    }
    grid::grobTree(stem,a,b,name="dpr.errorbar")
  })
# Scale diamond size and outline together, 20% larger than the original H1b marker
# (size 3.4, stroke .9) in its 783.78125-point-wide plotting panel.
# Resolve physical dimensions at draw time, inside the actual panel viewport.
dpr_diamond_scale <- 1.20
dpr_diamond_reference_mm <- 783.78125 * 25.4 / 72
dpr_make_diamonds <- function(x) {
  horizontal_panels <- if(is.null(x$horizontal_panels))1 else x$horizontal_panels
  ratio <- horizontal_panels * dpr_diamond_scale * grid::convertWidth(grid::unit(1,"npc"),"mm",valueOnly=TRUE)/dpr_diamond_reference_mm
  d <- x$data
  marks <- grid::pointsGrob(d$x,d$y,pch=23,
    gp=grid::gpar(col=scales::alpha(d$colour,d$alpha),fill=scales::alpha(d$fill,d$alpha),
      fontsize=(3.4*ggplot2::.pt+.9*ggplot2::.stroke/2)*ratio,
      lwd=.9*ggplot2::.stroke/2*ratio),name="dpr.diamond.points")
  grid::setChildren(x,grid::gList(marks))
}
registerS3method("makeContent","dpr_scaled_diamonds",dpr_make_diamonds,envir=asNamespace("grid"))
dpr_diamond_geom <- ggplot2::ggproto("GeomDprDiamond",ggplot2::GeomPoint,
  draw_panel=function(data,panel_params,coord,na.rm=FALSE,horizontal_panels=1) {
    if(!nrow(data))return(grid::zeroGrob())
    grid::gTree(data=coord$transform(data,panel_params),horizontal_panels=horizontal_panels,cl="dpr_scaled_diamonds",name="dpr.diamonds")
  })
# A full-width panel has factor one; each side-by-side subdivision contributes
# its column count. Explicit spanning panels use only their occupied width share.
dpr_patch_columns <- function(p) {
  layout <- p$patches$layout
  n <- length(p$patches$plots)+as.integer(!inherits(p,"plot_filler"))
  if(!is.null(layout$design)) {
    design <- as.data.frame(unclass(layout$design))
    return(max(design$r)/(design$r-design$l+1))
  }
  nc <- layout$ncol;nr <- layout$nrow
  if(is.null(nc) && length(layout$widths)>1)nc <- length(layout$widths)
  if(is.null(nr) && length(layout$heights)>1)nr <- length(layout$heights)
  rep(ggplot2::wrap_dims(n,nrow=nr,ncol=nc)[2],n)
}
# Clone only interval and diamond layers; preserve other marks and data.
# Patchwork columns and facet columns multiply; vertical stacking adds no factor.
dpr_style_errorbars <- function(p,horizontal_panels=1) {
  if(!inherits(p,"ggplot"))return(p)
  own_panels <- horizontal_panels
  if(inherits(p,"patchwork")) {
    factors <- dpr_patch_columns(p)
    p$patches$plots <- lapply(seq_along(p$patches$plots),function(i)
      dpr_style_errorbars(p$patches$plots[[i]],horizontal_panels*factors[i]))
    if(!inherits(p,"plot_filler"))own_panels <- horizontal_panels*tail(factors,1)
  }
  if(!inherits(p$facet,"FacetNull") && length(p$layers)) {
    layout <- ggplot2::ggplot_build(p)$layout$layout
    own_panels <- own_panels*max(layout$COL)
  }
  p$layers <- lapply(p$layers,function(layer) {
    bar <- inherits(layer$geom,"GeomErrorbar")
    diamond <- inherits(layer$geom,"GeomPoint") && identical(as.numeric(layer$aes_params$shape),23)
    if(!bar && !diamond)return(layer)
    styled <- ggplot2::ggproto(NULL,layer)
    styled$geom_params <- utils::modifyList(layer$geom_params,list(horizontal_panels=own_panels))
    if(bar) {
      styled$geom <- dpr_errorbar_geom
      styled$aes_params <- utils::modifyList(layer$aes_params,list(linewidth=dpr_errorbar_linewidth))
    } else {
      styled$geom <- dpr_diamond_geom
      styled$aes_params <- utils::modifyList(layer$aes_params,list(size=3.4,stroke=.9))
    }
    styled
  })
  p
}
dpr_theme <- function()theme_minimal(base_size=12,base_family="Helvetica")+theme(
  text=element_text(colour="#25323D"),panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
  plot.title=element_text(size=14,face="bold"),plot.subtitle=element_text(size=10),
  axis.title=element_text(size=11),axis.text=element_text(size=10),legend.position="bottom",
  plot.margin=margin(8,12,8,8),plot.caption=element_text(size=9,hjust=0),strip.text=element_text(face="bold"))
# Recolour subtitle text after layout so its spacing and the collected legend
# stay unchanged. Blank lines preserve each original multiline text position.
dpr_colour_group_subtitles <- function(p,colours) {
  recolour <- function(g) {
    if(inherits(g,"text") && length(g$label)==1 && is.character(g$label)) {
      lines <- strsplit(g$label,"\n",fixed=TRUE)[[1]]
      if(length(lines)==3 && startsWith(lines[1],"CAL65:") && startsWith(lines[2],"CAL90:")) {
        parts <- lapply(seq_along(lines),function(i) {
          z <- g;z$label <- paste(ifelse(seq_along(lines)==i,lines,""),collapse="\n")
          z$gp$col <- if(i<3)unname(colours[i]) else "#25323D"
          z$name <- paste0(g$name,"_line",i);z
        })
        return(do.call(grid::grobTree,parts))
      }
      if(length(lines)==1 && grepl("CAL65 n = .*CAL90 n =",g$label)) {
        chunks <- regmatches(g$label,regexpr("CAL65 n = [0-9]+, ",g$label))
        prefix <- sub("CAL65 n = .*","",g$label)
        labels <- c(prefix,chunks,sub(".*CAL90 n = ","CAL90 n = ",g$label))
        parts <- list();offset <- grid::unit(0,"pt")
        for(i in seq_along(labels))if(nzchar(labels[i])) {
          z <- g;z$label <- labels[i];z$x <- g$x+offset;z$hjust <- 0
          z$gp$col <- if(i==1)"#25323D" else unname(colours[i-1])
          z$name <- paste0(g$name,"_group",i);parts[[length(parts)+1L]] <- z
          offset <- offset+grid::grobWidth(z)
        }
        return(do.call(grid::grobTree,parts))
      }
    }
    if(length(g$grobs))g$grobs <- lapply(g$grobs,recolour)
    if(length(g$children))g$children <- do.call(grid::gList,lapply(g$children,recolour))
    g
  }
  recolour(patchwork::patchworkGrob(p))
}
dpr_p_label <- function(p)ifelse(is.na(p),"unavailable",ifelse(p<.001,"***",ifelse(p<.01,"**",ifelse(p<.05,"*","ns"))))
dpr_p_text <- function(p)ifelse(is.na(p),"unavailable",ifelse(p<.001,"< .001",sprintf("%.3f",p)))
dpr_labels <- c(accuracy="All-trial accuracy",correct_rt="Correct-response RT",task_dprime="Task discrimination (d-prime)",
  advice_bias="Advice acceptance (-c)",task_bias="BLACK preference (-c)",advice_dprime="Advice selectivity (d-prime)",
  accuracy_benchmark="Accuracy beyond the aid",task_dprime_benchmark="Task discrimination beyond the aid",
  trust="Final trust",perceived="Perceived aid reliability",signed_error="Perceived minus realised reliability")
dpr_phase_labels <- c("Manual pre"="Manual\npre",P1="Phase 1\n95% aid",P2="Phase 2\n70% aid",P3="Phase 3\n95% aid","Manual post"="Manual\npost")
dpr_comparison <- function(results,kind,sample_name) {
  x <- filter(results,analysis=="primary",grepl("^H",hypothesis),sample==sample_name,correction %in% c("none","loglinear"))
  keys <- c("hypothesis","outcome","contrast_id")
  if(kind=="correction") {
    x <- filter(x,correction=="loglinear"); y <- filter(results,analysis=="primary",sample==sample_name,correction=="extreme_only")
  } else if(kind=="participant")y <- filter(results,analysis=="participant_sensitivity",sample==sample_name,correction %in% c("none","loglinear"))
  else {
    x <- filter(results,analysis=="primary",grepl("^H",hypothesis),sample=="primary",correction %in% c("none","loglinear"))
    y <- filter(results,analysis=="primary",sample=="all_participants",correction %in% c("none","loglinear"))
  }
  inner_join(select(x,all_of(keys),estimate_reference=estimate,p_reference=p_holm,available_reference=inference_available),
    select(y,all_of(keys),estimate_comparison=estimate,p_comparison=p_holm,available_comparison=inference_available),by=keys) %>%
    mutate(direction_changed=sign(estimate_reference)!=sign(estimate_comparison),
      decision=case_when(!available_reference | !available_comparison | is.na(p_reference) | is.na(p_comparison)~"Unavailable",
        p_reference<.05 & p_comparison>=.05~"Primary only",p_reference>=.05 & p_comparison<.05~"Comparison only",TRUE~"Same decision"))
}
dpr_plot_build <- function(bundle,registry,out) {
  figures <- list(); annotations <- list()
  colours <- c(CAL65="#0072B2",CAL90="#D55E00","CAL90 - CAL65"="#334858")
  caption <- "Faint points: participants; open diamonds: fitted estimates and pointwise 95% CIs. Holm within hypothesis/outcome: *** p<.001, ** p<.01, * p<.05; ns p>=.05."
  for(sample_name in c("primary","all_participants")) {
    d <- bundle$data[[sample_name]]; means <- bundle$estimates[[sample_name]]
    tests <- filter(bundle$results,sample==sample_name,analysis=="primary",correction %in% c("none","loglinear"))
    members <- bundle$membership %>% filter(sample_name=="all_participants" | in_primary)
    sample_label <- paste0(if(sample_name=="primary")"" else "All-participant sensitivity; ",
      "CAL65 n = ",sum(members$calibration_target_group=="CAL65"),", CAL90 n = ",sum(members$calibration_target_group=="CAL90"))
    prefix <- if(sample_name=="primary")"" else "sensitivity_"
    current_data <- current_raw <- current_tests <- list()
    register <- function(o,m,raw,t=tibble()) {
      if("outcome" %in% names(m))m <- mutate(m,source_outcome=outcome)
      if("outcome" %in% names(raw))raw <- mutate(raw,source_outcome=outcome)
      current_data[[o]] <<- mutate(as_tibble(m),outcome=o)
      current_raw[[o]] <<- mutate(as_tibble(raw),outcome=o)
      if(nrow(t))current_tests[[o]] <<- t
    }
    finish <- function(order,p,note,subtitle=sample_label) {
      row <- registry[match(order,registry$order),];stem <- paste0(prefix,row$filename)
      p <- p+plot_annotation(title=paste(strwrap(row$title,width=85),collapse="\n"),subtitle=subtitle,
        caption=paste(strwrap(note,width=155),collapse="\n"),
        theme=dpr_theme()+theme(plot.title=element_text(size=19,face="bold"),plot.subtitle=element_text(size=11)))
      figures[[stem]] <<- if(order==11)dpr_colour_group_subtitles(p,colours[GROUPS]) else p
      write_csv(bind_rows(current_data),file.path(out,"data",paste0(stem,"_data.csv")))
      write_csv(bind_rows(current_raw),file.path(out,"data",paste0(stem,"_participants.csv")))
      aa <- bind_rows(current_tests)
      if(nrow(aa)) {
        aa <- mutate(aa,filename=stem,symbol=dpr_p_label(p_holm));annotations[[stem]] <<- aa
        write_csv(aa,file.path(out,"data",paste0(stem,"_annotations.csv")))
      }
      current_data <<- current_raw <<- current_tests <<- list()
    }
    choose <- function(h,o)filter(tests,hypothesis==h,outcome==o)
    human <- filter(d$task_scores,agent=="participant",correction=="loglinear")
    advice <- filter(d$advice_scores,correction=="loglinear")
    perfraw <- function(o)if(o=="task_dprime")mutate(human,value=dprime) else mutate(d$performance,value=.data[[o]])
    changes <- function(o,baseline="Manual pre") {
      raw <- perfraw(o)
      inner_join(filter(raw,stage!=baseline) %>% select(participant_id,group,phase=stage,left=value),
        filter(raw,stage==baseline) %>% select(participant_id,right=value),by="participant_id") %>%
        mutate(value=(left-right)*if(o=="accuracy")100 else 1)
    }
    gain_panel <- function(raw,t,o,levels=c("P1","P2","P3"),ylab=NULL) {
      cells <- filter(t,group %in% GROUPS) %>% mutate(x=match(phase,levels)+ifelse(group=="CAL65",-.15,.15))
      raw <- filter(raw,phase %in% levels) %>% mutate(x=match(phase,levels)+ifelse(group=="CAL65",-.15,.15))
      ratio <- o=="correct_rt"
      rg <- range(c(raw$value,cells$lower,cells$upper,0),finite=TRUE);span <- max(diff(rg),.05)
      # Match Virus/ATC: a common reference-test row above all data, then group comparisons.
      reference_y <- rg[2]+.12*span
      top <- rg[2]+.29*span; group_tests <- filter(t,group=="CAL90 - CAL65") %>% mutate(x=match(phase,levels),y=top)
      p <- ggplot()+geom_hline(yintercept=0,colour="#9BA5AD",linetype="dotted")+
        geom_point(data=raw,aes(x,value,colour=group),position=position_jitter(width=.045,height=0,seed=26),alpha=.20,size=1.1)+
        geom_errorbar(data=cells,aes(x,ymin=lower,ymax=upper,colour=group),width=.07,linewidth=.85)+
        geom_point(data=cells,aes(x,estimate,colour=group),shape=23,fill="white",stroke=.9,size=3.4)+
        geom_text(data=cells,aes(x,y=reference_y,label=dpr_p_label(p_holm)),
          inherit.aes=FALSE,size=3.7,vjust=-.1)+
        geom_segment(data=group_tests,aes(x=x-.15,xend=x+.15,y=y,yend=y),colour="#334858",linewidth=.45)+
        geom_text(data=group_tests,aes(x,y=y+.04*span,label=paste("Group:",dpr_p_label(p_holm))),size=3.2,colour="#334858")+
        scale_colour_manual(values=colours,name=NULL)+
        scale_x_continuous(breaks=seq_along(levels),labels=dpr_phase_labels[levels],limits=c(.6,length(levels)+.4))+
        labs(title=dpr_labels[[o]],x=NULL,y=ylab)+dpr_theme()+coord_cartesian(ylim=c(rg[1]-.05*span,top+.13*span))
      if(length(levels)>1)p <- p+geom_line(data=raw,aes(x,value,group=participant_id,colour=group),alpha=.08,linewidth=.25)
      if(ratio)p <- p+scale_y_continuous(breaks=pretty(rg,5),labels=function(x)format(round(exp(x),2),trim=TRUE))
      register(o,cells,raw,t);p
    }
    for(order in 1:2) {
      panels <- list()
      for(o in if(order==1)c("accuracy","task_dprime") else "correct_rt")
        panels[[o]] <- gain_panel(changes(o),choose("H1",o),o,ylab=if(o=="accuracy")"Aided minus manual pre (pp)" else
          if(o=="task_dprime")"Aided minus manual pre (d-prime)" else "Aided / manual pre RT ratio (log-spaced axis)")
      finish(order,wrap_plots(panels,nrow=1,guides="collect"),
        paste(caption,"Aligned labels above the data test no benefit; higher group brackets compare benefits. Manual-pre is the reference.",
          if(order==2)"Ratios below 1 mean faster correct responses; group contrasts are ratios of ratios." else "Accuracy includes timeouts; SDT uses answered trials."))
    }
    panels <- list()
    for(o in c("accuracy_benchmark","task_dprime_benchmark")) {
      raw <- if(o=="accuracy_benchmark")mutate(d$benchmark,value=value*100) else filter(d$task_benchmark,correction=="loglinear")
      panels[[o]] <- gain_panel(raw,choose("H2",o),o,ylab=if(o=="accuracy_benchmark")"Human minus aid accuracy (pp)" else "Human minus matched aid (d-prime)")
    }
    finish(3,wrap_plots(panels,nrow=1,guides="collect"),paste(caption,
      "Accuracy assumes timely aid on every trial; task d-prime matches precisely human-answered trials."))
    trajectory_panel <- function(raw,m,o,levels,dimension="phase",descriptive=FALSE) {
      raw <- raw %>% filter(.data[[dimension]] %in% levels) %>% mutate(x=match(.data[[dimension]],levels)+ifelse(group=="CAL65",-.055,.055))
      m <- m %>% filter(.data[[dimension]] %in% levels) %>% mutate(x=match(.data[[dimension]],levels)+ifelse(group=="CAL65",-.055,.055))
      if(o=="correct_rt") {raw$value <- exp(raw$value);for(col in c("estimate","lower","upper"))m[[col]] <- exp(m[[col]])}
      p <- ggplot(raw,aes(x,value,colour=group))+
        geom_line(aes(group=participant_id),alpha=.07,linewidth=.22)+
        geom_point(position=position_jitter(width=.025,height=0,seed=26),alpha=.16,size=.9)+
        geom_line(data=m,aes(x,estimate,group=group),linewidth=.65)+
        geom_errorbar(data=m,aes(x,y=NULL,ymin=lower,ymax=upper),width=.065,linewidth=.75)+
        geom_point(data=m,aes(x,estimate),shape=if(descriptive)16 else 23,fill="white",size=3,stroke=if(descriptive)1 else .8)+
        scale_colour_manual(values=colours,name=NULL)+scale_x_continuous(breaks=seq_along(levels),labels=dpr_phase_labels[levels],limits=c(.7,length(levels)+.3))+
        labs(title=if(o %in% names(dpr_labels))dpr_labels[[o]] else o,x=NULL,y=if(o=="accuracy")"Accuracy (%)" else if(o=="correct_rt")"Geometric mean RT (s)" else "SDT z units")+dpr_theme()
      list(plot=p,data=m,raw=raw)
    }
    forest_panel <- function(t,title,limits=NULL) {
      t <- t %>% mutate(label=paste(gsub(" minus "," - ",comparison,fixed=TRUE),group,sep=": "),label=factor(label,rev(unique(label))))
      rg <- if(is.null(limits))range(c(0,t$lower,t$upper),finite=TRUE) else limits;span <- max(diff(rg),.05)
      ggplot(t,aes(estimate,label,colour=group))+geom_vline(xintercept=0,linetype="dashed",colour="#9BA5AD")+
        geom_errorbar(aes(xmin=lower,xmax=upper),orientation="y",width=.12,linewidth=.75)+
        geom_point(shape=23,fill="white",size=3,stroke=.8)+
        geom_text(aes(x=rg[2]+.18*span,label=dpr_p_label(p_holm)),size=3.4,show.legend=FALSE)+
        scale_colour_manual(values=colours,guide="none")+coord_cartesian(xlim=c(rg[1]-.08*span,rg[2]+.35*span))+
        labs(title=title,x="Difference (SDT z units)",y=NULL)+dpr_theme()+
        theme(axis.text.y=element_text(size=9),panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="#E5EAEE",linewidth=.3))
    }
    for(h in c("H3","H4"))for(measure in c("bias","dprime")) {
      panels <- list();order <- if(h=="H3")4 else 6;order <- order+as.integer(measure=="dprime")
      shared <- filter(bundle$results,hypothesis==h,outcome %in% paste(c("advice","task"),measure,sep="_"),analysis=="primary",correction=="loglinear")
      contrast_limits <- range(c(0,shared$lower,shared$upper),finite=TRUE)
      all_advice <- filter(bundle$data$all_participants$advice_scores,phase!="FULL",correction=="loglinear")
      all_task <- filter(bundle$data$all_participants$task_scores,agent=="participant",!is.na(phase),correction=="loglinear")
      mean_limits <- range(c(0,if(measure=="bias")all_advice$advice_bias else all_advice$dprime,
        if(measure=="bias")all_task$task_bias else all_task$dprime),finite=TRUE)
      for(domain in c("advice","task")) {
        o <- paste(domain,measure,sep="_")
        raw <- if(domain=="advice")filter(advice,phase!="FULL") else filter(human,!is.na(phase))
        raw <- mutate(raw,value=if(measure=="dprime")dprime else if(domain=="advice")advice_bias else task_bias)
        m <- means[[paste("aided",o,"loglinear",sep="_")]];t <- choose(h,o)
        tr <- trajectory_panel(raw,m,o,if(h=="H3")c("P1","P2") else c("P1","P2","P3"))
        panels[[paste0(domain,"_mean")]] <- tr$plot+coord_cartesian(ylim=mean_limits)
        panels[[paste0(domain,"_contrast")]] <- forest_panel(t,if(h=="H3")"Change at the drop" else "Recovery and residual differences",contrast_limits)
        register(o,tr$data,tr$raw,t)
      }
      finish(order,wrap_plots(panels,ncol=2,guides="collect"),paste(caption,
        if(measure=="bias")"Advice -c: acceptance; task -c: BLACK preference." else "Advice selectivity and final task discrimination reuse the same decisions."))
    }
    for(o in c("accuracy","agreement","correct_rt")) {
      rt <- o=="correct_rt"
      raw <- filter(d$trajectories,outcome==o) %>%
        mutate(value=if(rt)exp(value) else 100*value,advice=if(o!="agreement")"All" else as.character(advice))
      m <- means[[paste0(o,"_quadratic_trend")]] %>% mutate(advice=if(o!="agreement")"All" else as.character(advice))
      if(rt)for(v in c("estimate","lower","upper"))m[[v]] <- exp(m[[v]])
      t <- choose("H5",o)
      if(rt) {
        allraw <- filter(bundle$data$all_participants$trajectories,outcome==o)
        allfit <- bundle$estimates$all_participants[[paste0(o,"_quadratic_trend")]]
        rg <- range(c(exp(allraw$value),exp(allfit$lower),exp(allfit$upper),m$lower,m$upper),finite=TRUE)
        span <- max(diff(rg),.1)
        bounds <- tibble(advice="All",bottom=max(0,rg[1]-.04*span),top=rg[2]+.35*span,
          text_top=rg[2]+.28*span,text_step=.10*span)
      } else bounds <- tibble(advice=unique(raw$advice)) %>%
        mutate(bottom=ifelse(advice=="Incorrect",0,40),top=if(o=="agreement")140 else 118,
          text_top=if(o=="agreement")132 else 114,text_step=if(o=="agreement")9 else 4)
      aa <- t %>% select(group,phase,advice,estimand,p_holm) %>%
        mutate(symbol=dpr_p_label(p_holm)) %>% select(-p_holm) %>%
        pivot_wider(names_from=estimand,values_from=symbol) %>% left_join(bounds,by="advice") %>%
        mutate(line=match(group,c(GROUPS,"CAL90 - CAL65")),y=text_top-(line-1)*text_step,
          label=paste0(ifelse(group=="CAL90 - CAL65","Difference",group),": S-M ",middle_start,"; M-E ",end_middle,"; Curve ",curvature))
      p <- ggplot(raw,aes(progress,value,colour=group))+
        geom_blank(data=bounds,aes(x=0,y=bottom),inherit.aes=FALSE)+
        geom_blank(data=bounds,aes(x=0,y=top),inherit.aes=FALSE)+
        geom_line(aes(group=participant_id),alpha=.07,linewidth=.22)+geom_point(alpha=.13,size=.8)+
        geom_ribbon(data=m,aes(x=progress,y=NULL,ymin=lower,ymax=upper,fill=group,group=group),alpha=.12,colour=NA)+
        geom_line(data=m,aes(progress,estimate,group=group),linewidth=.8)+
        geom_point(data=filter(m,progress %in% c(0,.5,1)),aes(progress,estimate),shape=23,fill="white",size=2.3,stroke=.4)+
        geom_text(data=aa,aes(x=.02,y=y,label=label,colour=group),inherit.aes=FALSE,hjust=0,size=2.8,show.legend=FALSE)+
        facet_grid(advice~phase,scales="free_y",labeller=labeller(phase=c(P1="Phase 1: 95%",P2="Phase 2: 70%",P3="Phase 3: 95%")))+
        scale_colour_manual(values=colours,breaks=GROUPS,name=NULL)+scale_fill_manual(values=colours,guide="none")+
        scale_x_continuous(breaks=c(0,.5,1),labels=c("Start","Middle","End"))+
        labs(x=NULL,y=if(rt)"Correct-response RT (s; geometric mean)" else if(o=="accuracy")"Accuracy (%)" else "Agreement (%)")+dpr_theme()
      if(!rt)p <- p+scale_y_continuous(breaks=function(limits)if(limits[1]>0)seq(40,100,10) else seq(0,100,25))
      register(o,m,raw,t)
      # Stable catalog keys: insert H5c between H5b and H6 without renumbering other keys.
      finish(switch(o,accuracy=8,agreement=9,correct_rt=9.5),p,paste(
        "Quadratic curves and pointwise 95% bands; participant 100-trial bins. S-M: middle minus start; M-E: end minus middle; Curve: quadratic coefficient on",
        if(rt)"log-RT scale." else "log-odds scale.",
        if(o=="agreement")"Holm across 54 tests (both advice types)." else "Holm across 27 tests.",
        "*** p<.001; ** p<.01; * p<.05; ns p>=.05. Difference: CAL90 minus CAL65. Curvature alone does not establish a plateau."))
    }
    panels <- list()
    for(o in c("trust","perceived","signed_error")) {
      raw <- mutate(d$ratings,value=.data[[o]],x=match(group,GROUPS));m <- means[[paste0("ratings_",o)]] %>% mutate(x=match(group,GROUPS))
      t <- choose("H6",o);rg <- range(c(raw$value,m$lower,m$upper,if(o=="signed_error")0),finite=TRUE);span <- max(diff(rg),.1)
      group_y <- rg[2]+if(o=="signed_error") .29*span else .12*span
      p <- ggplot(raw,aes(x,value,colour=group))+geom_point(position=position_jitter(width=.09,height=0,seed=26),alpha=.25,size=1.4)+
        geom_errorbar(data=m,aes(x,y=NULL,ymin=lower,ymax=upper),width=.10,linewidth=.85)+
        geom_point(data=m,aes(x,estimate),shape=23,fill="white",size=3.5,stroke=.8)+
        annotate("segment",x=1,xend=2,y=group_y,yend=group_y)+
        annotate("text",x=1.5,y=group_y+.07*span,label=paste("Group:",dpr_p_label(t$p_holm[t$contrast_id=="group_difference"])),size=3.6)+
        scale_colour_manual(values=colours,guide="none")+scale_x_continuous(breaks=1:2,labels=GROUPS,limits=c(.6,2.4))+
        labs(title=dpr_labels[[o]],x=NULL,y=if(o=="trust")"Trust (1-5)" else if(o=="perceived")"Perceived reliability (%)" else "Error (pp)")+
        dpr_theme()+coord_cartesian(ylim=c(rg[1]-.07*span,group_y+.17*span))
      if(o=="signed_error") {
        z <- filter(t,startsWith(contrast_id,"zero_")) %>% mutate(x=match(group,GROUPS))
        p <- p+geom_hline(yintercept=0,linetype="dashed",colour="#9BA5AD")+
          geom_text(data=z,aes(x,label=dpr_p_label(p_holm)),y=rg[2]+.12*span,
            inherit.aes=FALSE,size=3.7,vjust=-.1)
      }
      panels[[o]] <- p;register(o,m,raw,t)
    }
    finish(10,wrap_plots(panels,nrow=1),paste(caption,"Participant-level models use HC3 covariance. Signed-error family includes the group difference and two tests versus zero. Ratings concern the full aided block."))
    association_plot <- function(h,order) {
      panels <- list()
      for(o in c("trust","signed_error"))for(predictor in c("advice_bias","advice_dprime")) {
        raw <- filter(d$associations,hypothesis==h,outcome==o,.data$predictor==.env$predictor,correction=="loglinear",analysis=="primary")
        pred <- filter(d$association_predictions,hypothesis==h,outcome==o,.data$predictor==.env$predictor,correction=="loglinear",analysis=="primary")
        t <- filter(tests,hypothesis==h,outcome==o,startsWith(contrast_id,paste0(predictor,"__")))
        t <- t[match(c(GROUPS,"CAL90 - CAL65"),t$group),]
        subtitle <- paste(sprintf("%s: %.2f [%.2f, %.2f]; Holm p %s (%s)",
          ifelse(t$group=="CAL90 - CAL65","Slope difference",t$group),t$estimate,t$lower,t$upper,dpr_p_text(t$p_holm),dpr_p_label(t$p_holm)),collapse="\n")
        p <- ggplot(raw,aes(score,value,colour=group))+
          geom_ribbon(data=pred,aes(x=score,y=NULL,ymin=lower,ymax=upper,fill=group,group=group),alpha=.12,colour=NA)+
          geom_line(data=pred,aes(score,estimate,group=group),linewidth=.8)+geom_point(alpha=.55,size=1.6)+
          scale_colour_manual(values=colours,name=NULL)+scale_fill_manual(values=colours,guide="none")+
          labs(title=paste(if(o=="trust")"Final trust" else "Perceived reliability error","and",if(predictor=="advice_bias")"acceptance" else "selectivity"),
            subtitle=subtitle,
            x=if(predictor=="advice_bias")"Advice acceptance (-c)" else "Advice selectivity (d-prime)",y=if(o=="trust")"Trust (1-5)" else "Error (pp)")+
          dpr_theme()+theme(plot.title=element_text(size=12),plot.subtitle=element_text(size=8.5,lineheight=1.1))
        key <- paste(o,predictor,sep="__");panels[[key]] <- p;register(key,pred,raw,t)
      }
      finish(order,wrap_plots(panels,ncol=2,guides="collect"),
        paste("Group-specific HC3 fits and pointwise 95% CIs; points: participants. Six-test Holm family per rating: two predictors x (two slopes + CAL90-minus-CAL65 difference).",
          if(h=="H7")"SDT uses pooled whole-block counts, matching the final-rating scope." else "Supporting analysis: Phase 2 SDT predicts whole-block final ratings."))
    }
    association_plot("H7",11)
    panels <- list()
    for(o in c("accuracy","task_dprime","correct_rt")) {
      raw <- perfraw(o);if(o=="accuracy")raw$value <- 100*raw$value
      m <- means[[paste("performance",o,if(o=="task_dprime")"loglinear" else "none",sep="_")]]
      tr <- trajectory_panel(raw,m,o,STAGES,"stage");panels[[o]] <- tr$plot;register(o,tr$data,tr$raw)
    }
    finish(12,wrap_plots(panels,nrow=1,guides="collect"),paste(caption,"Descriptive synthesis of fitted stage means; hypothesis tests appear in H1-H4."))
    panels <- list()
    for(o in c("accuracy","task_dprime","correct_rt"))panels[[o]] <- gain_panel(changes(o),choose("S2",o),o,levels="Manual post",
      ylab=if(o=="accuracy")"Manual post minus pre (pp)" else if(o=="task_dprime")"Manual post minus pre (d-prime)" else "Manual post / pre RT ratio")
    finish(13,wrap_plots(panels,nrow=1,guides="collect"),paste(caption,"Supporting S2: three tests per outcome."))
    panels <- list()
    for(o in c("completion","answered_accuracy")) {
      raw <- mutate(d$performance,value=100*.data[[o]])
      m <- raw %>% group_by(group,stage) %>% summarise(estimate=mean(value),se=sd(value)/sqrt(n()),n=n(),.groups="drop") %>%
        mutate(lower=estimate-qt(.975,n-1)*se,upper=estimate+qt(.975,n-1)*se)
      tr <- trajectory_panel(raw,m,o,STAGES,"stage",TRUE)
      panels[[o]] <- tr$plot+labs(title=if(o=="completion")"Response completion" else "Accuracy on answered trials",y="Percent")
      register(o,tr$data,tr$raw)
    }
    finish(14,wrap_plots(panels,nrow=1,guides="collect"),
      "Descriptive only: faint points are participants; filled circles and intervals are ordinary participant means and pointwise 95% t CIs. SDT excludes timeouts; all-trial accuracy retains their cost.")
    for(kind in c("correction","participant","sample")) {
      comp <- dpr_comparison(bundle$results,kind,sample_name)
      comparison_label <- switch(kind,correction="Extreme-only correction",participant="Participant sensitivity",sample=paste("All",nrow(bundle$membership),"participants"))
      counts <- count(comp,hypothesis,decision)
      p1 <- ggplot(counts,aes(n,hypothesis,fill=decision))+geom_col(width=.65)+
        scale_fill_manual(values=c("Same decision"="#89AABA","Primary only"="#D78B4B","Comparison only"="#947EB0","Unavailable"="#BAC1C7"),drop=FALSE)+
        labs(title="Agreement on Holm decisions",x="Number of comparisons",y=NULL,fill=NULL)+dpr_theme()
      scatter <- comp %>% mutate(x=pmin(-log10(pmax(p_reference,1e-300)),12),y=pmin(-log10(pmax(p_comparison,1e-300)),12))
      p2 <- ggplot(scatter,aes(x,y,colour=hypothesis))+geom_abline(slope=1,intercept=0,colour="#AAB4BC")+
        geom_hline(yintercept=-log10(.05),linetype="dotted")+geom_vline(xintercept=-log10(.05),linetype="dotted")+
        geom_point(alpha=.7,size=2)+coord_equal(xlim=c(0,12),ylim=c(0,12))+
        labs(title="Adjusted p-values",x="Primary: -log10(Holm p)",y=paste(comparison_label,": -log10(Holm p)"),colour=NULL)+dpr_theme()
      register(kind,counts,comp)
      finish(match(kind,c("correction","participant","sample"))+14,p1+p2+plot_layout(widths=c(1,1.15)),
        paste0(nrow(comp)," primary comparisons; ",sum(comp$direction_changed,na.rm=TRUE)," estimate-direction changes. Dotted lines: p=.05; display capped at p=1e-12. Exact estimates and p-values are exported. ",
          if(kind=="participant")"Participant contrasts can target different estimands; neither method substitutes for the other." else "Sensitivity results do not replace primary inference."),
        if(kind=="sample")paste0("Matched QC-retained (",sum(bundle$membership$in_primary),") and all-participant (",nrow(bundle$membership),") analyses") else sample_label)
    }
    association_plot("S7",18)
  }
  list(figures=figures,annotations=bind_rows(annotations))
}
