suppressPackageStartupMessages(library(patchwork))
ao_palette <- c(Manual="#727B88",`Aid-first`="#1976A3",`Stimulus-first`="#C56924",
  `Incorrect advice`="#BB503B",`Correct advice`="#087F72")
ao_theme <- function() theme_minimal(base_size=12,base_family="Helvetica")+
  theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
    plot.title=element_text(face="bold",size=14,margin=margin(b=12)),
    axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="bottom",
    plot.margin=margin(8,14,8,14))
ao_p <- function(p) ifelse(is.na(p),"unavailable",ifelse(p<.001,"< .001",paste0("= ",formatC(p,digits=3,format="f"))))
ao_significance <- function(p,available=TRUE) {
  ifelse(!available | is.na(p),"unavailable",
    ifelse(p<.001,"***",ifelse(p<.01,"**",ifelse(p<.05,"*","ns"))))
}
ao_comparison <- c(AF_minus_Manual="AF - Manual",SF_minus_Manual="SF - Manual",SF_minus_AF="SF - AF",
  human_minus_aid_AF="Human - aid, AF",human_minus_aid_SF="Human - aid, SF",correct_minus_incorrect="Correct - incorrect",
  mean_aid_minus_self="Mean aid - self")
ao_outcome <- c(accuracy="Accuracy",task_dprime="Task d-prime",accuracy_advantage="Accuracy advantage",
  task_dprime_advantage="Task d-prime advantage",advice_bias="Advice bias",task_bias="Task bias",advice_dprime="Advice d-prime",
  switching="Switching",beneficial="Beneficial revisions",harmful="Harmful revisions",selective_uptake="Selective uptake",perceived="Perceived reliability",trust="Trust")
ao_annotation <- function(t) {
  paste0(unname(ao_comparison[t$comparison]),": ",ifelse(t$inference_available,
    paste0(sprintf("%+.2f",t$estimate)," [",sprintf("%.2f",t$lower),", ",sprintf("%.2f",t$upper),
      "]; Holm p ",ao_p(t$p_holm),ifelse(t$significant,paste0(" ",t$symbol),""),
      ifelse(t$sensitivity_disagreement," [a]","")),"primary inference unavailable"))
}
ao_bracket_layout <- function(t,r,m,lev,full_range=NULL) {
  rg <- range(c(r$value,m$lower,m$upper,full_range),finite=TRUE)
  span <- max(diff(rg),.15)
  x <- function(condition) match(condition,lev)
  t$annotation_type <- ifelse(grepl("^human_minus_aid_",t$comparison),"zero_reference",
    ifelse(t$comparison=="mean_aid_minus_self","pooled_bracket","bracket"))
  t$x1 <- vapply(t$comparison,function(id) switch(id,AF_minus_Manual=x("Manual"),SF_minus_Manual=x("Manual"),
    SF_minus_AF=x("Aid-first"),correct_minus_incorrect=x("Incorrect advice"),
    mean_aid_minus_self=x("Manual"),human_minus_aid_AF=x("Aid-first"),human_minus_aid_SF=x("Stimulus-first")),numeric(1))
  t$x2 <- vapply(t$comparison,function(id) switch(id,AF_minus_Manual=x("Aid-first"),SF_minus_Manual=x("Stimulus-first"),
    SF_minus_AF=x("Stimulus-first"),correct_minus_incorrect=x("Correct advice"),
    mean_aid_minus_self=mean(c(x("Aid-first"),x("Stimulus-first"))),
    human_minus_aid_AF=x("Aid-first"),human_minus_aid_SF=x("Stimulus-first")),numeric(1))
  ao_check(!anyNA(t$x1) && !anyNA(t$x2),"Unmapped significance annotation")
  t$symbol <- ao_significance(t$p_holm,t$inference_available)
  t$label <- paste0(ifelse(t$annotation_type=="zero_reference","vs zero: ",
    ifelse(t$annotation_type=="pooled_bracket","Mean aid - self: ","")),t$symbol,
    ifelse(t$sensitivity_disagreement & t$inference_available," [a]",""))
  t$tick <- .025*span;t$y <- rg[2]+.08*span
  zero <- which(t$annotation_type=="zero_reference")
  links <- which(t$annotation_type!="zero_reference")
  # Short pairwise brackets sit below the enclosing/pooled comparison.
  bracket_order <- links[order(t$annotation_type[links]=="pooled_bracket",t$x2[links]-t$x1[links])]
  zero_clearance <- if(length(zero)) .13 else 0
  t$y[bracket_order] <- rg[2]+(.09+zero_clearance)*span+seq_along(bracket_order)*.12*span
  t$text_y <- t$y+.018*span
  t$group_x1 <- ifelse(t$annotation_type=="pooled_bracket",x("Aid-first"),NA_real_)
  t$group_x2 <- ifelse(t$annotation_type=="pooled_bracket",x("Stimulus-first"),NA_real_)
  t$axis_lower <- rg[1]-.04*span
  t$axis_upper <- max(t$text_y)+.09*span
  t
}
ao_panel <- function(row,raw,means,tests) {
  r <- filter(raw,model==row$model);m <- filter(means,model==row$model)
  ids <- strsplit(row$contrast_ids,";",fixed=TRUE)[[1]]
  t <- tests[match(ids,tests$contrast_id),]
  lev <- if(row$outcome=="selective_uptake") c("Incorrect advice","Correct advice") else ao_levels[ao_levels %in% r$condition]
  # H4 displays exactly the two timing cells; its task test remains the H1 test.
  if(row$order==4) lev <- ao_levels[-1]
  r <- filter(r,condition %in% lev) %>% mutate(condition=factor(condition,levels=lev))
  m <- filter(m,condition %in% lev) %>% mutate(condition=factor(condition,levels=lev))
  r$x <- match(r$condition,lev);m$x <- match(m$condition,lev)
  zero <- row$outcome %in% c("accuracy_advantage","task_dprime_advantage","advice_bias","task_bias")
  limits <- if(row$order==11)c(0,100) else if(zero)0 else NULL
  t <- ao_bracket_layout(t,r,m,lev,limits)
  links <- filter(t,annotation_type!="zero_reference")
  zeros <- filter(t,annotation_type=="zero_reference")
  pooled <- filter(t,annotation_type=="pooled_bracket")
  labels <- if(row$outcome=="perceived") c("Manual\n(self)","Aid-first\n(aid)","Stimulus-first\n(aid)") else lev
  data_range <- range(c(r$value,m$lower,m$upper,limits),finite=TRUE)
  breaks <- pretty(data_range,n=5);breaks <- breaks[breaks>=data_range[1] & breaks<=data_range[2]]
  p <- ggplot(r,aes(x,value,color=condition))+
    geom_point(position=position_jitter(width=.12,height=0,seed=20261017),alpha=.22,size=1.65)+
    geom_errorbar(data=m,inherit.aes=FALSE,
      mapping=aes(x=x,ymin=lower,ymax=upper),width=.10,linewidth=.85,color="#172B3A",na.rm=TRUE)+
    geom_point(data=m,aes(x,estimate),inherit.aes=FALSE,shape=23,fill="white",color="#172B3A",size=3.8,stroke=1)+
    scale_color_manual(values=ao_palette,guide="none")+
    scale_x_continuous(breaks=seq_along(lev),labels=labels,limits=c(.6,length(lev)+.4))+
    scale_y_continuous(breaks=breaks)+ao_theme()+labs(title=row$panel_title)
  if(zero) p <- p+geom_hline(yintercept=0,color="#9AA4AE",linetype="dashed")
  if(nrow(links)) p <- p+
    geom_segment(data=links,aes(x=x1,xend=x2,y=y,yend=y),inherit.aes=FALSE,linewidth=.4,color="#334858")+
    geom_segment(data=links,aes(x=x1,xend=x1,y=y,yend=y-tick),inherit.aes=FALSE,linewidth=.4,color="#334858")+
    geom_segment(data=links,aes(x=x2,xend=x2,y=y,yend=y-tick),inherit.aes=FALSE,linewidth=.4,color="#334858")+
    geom_text(data=links,aes(x=(x1+x2)/2,y=text_y,label=label),inherit.aes=FALSE,vjust=0,size=3.6,color="#334858")
  if(nrow(zeros)) p <- p+geom_text(data=zeros,aes(x=x1,y=text_y,label=label),inherit.aes=FALSE,vjust=0,size=3.3,color="#334858")
  # The endpoint is the average of both aided ratings, not a single aided cell.
  if(nrow(pooled)) p <- p+
    geom_segment(data=pooled,aes(x=group_x1,xend=group_x2,y=y-tick,yend=y-tick),inherit.aes=FALSE,linewidth=.4,color="#334858")+
    geom_segment(data=pooled,aes(x=group_x1,xend=group_x1,y=y-tick,yend=y-2*tick),inherit.aes=FALSE,linewidth=.4,color="#334858")+
    geom_segment(data=pooled,aes(x=group_x2,xend=group_x2,y=y-tick,yend=y-2*tick),inherit.aes=FALSE,linewidth=.4,color="#334858")
  p <- p+coord_cartesian(ylim=c(t$axis_lower[1],t$axis_upper[1]),clip="on")
  notes <- tibble(x=0,y=seq(3,3-nrow(t)+1),label=ao_annotation(t))
  foot <- ggplot(notes,aes(x,y,label=label))+geom_text(hjust=0,size=3.35,family="Helvetica",color="#263746")+
    coord_cartesian(xlim=c(0,1),ylim=c(.5,3.5),clip="off")+theme_void()+theme(plot.margin=margin(0,14,0,14))
  list(plot=p/foot+plot_layout(heights=c(4,1.15)),raw=r,means=m,tests=t)
}
ao_context <- function(d,outcome,title) {
  r <- filter(d,.data$outcome==!!outcome) %>% mutate(condition=factor(condition,levels=ao_levels),decision=factor(decision,levels=c("D1","D2")))
  m <- r %>% group_by(condition,decision) %>% summarise(estimate=mean(value),se=sd(value)/sqrt(n()),df=n()-1,.groups="drop") %>%
    mutate(lower=estimate-qt(.975,df)*se,upper=estimate+qt(.975,df)*se)
  p <- ggplot(r,aes(condition,value,color=condition,shape=decision,group=interaction(condition,decision)))+
    geom_point(position=position_jitterdodge(jitter.width=.15,dodge.width=.4,seed=11),alpha=.16,size=1.5)+
    geom_errorbar(data=m,aes(y=estimate,ymin=lower,ymax=upper),position=position_dodge(.4),width=.10,linewidth=.8)+
    geom_point(data=m,aes(y=estimate),position=position_dodge(.4),size=3.2)+
    scale_color_manual(values=ao_palette,guide="none")+scale_shape_manual(values=c(D1=1,D2=16),name=NULL,
      labels=c(D1="Initial decision",D2="Final decision"))+ao_theme()+labs(title=title)
  if(outcome=="task_bias") p <- p+geom_hline(yintercept=0,linetype="dashed",color="#9AA4AE")
  list(plot=p,raw=r,means=m)
}
ao_html <- function(x) {x <- gsub("&","&amp;",x,fixed=TRUE);x <- gsub("<","&lt;",x,fixed=TRUE);gsub(">","&gt;",x,fixed=TRUE)}
ao_build_plots <- function(root,analysis,out) {
  read <- function(name) read_csv(file.path(analysis,paste0(name,".csv")),show_col_types=FALSE)
  ao_check(file.exists(file.path(analysis,"VALIDATED.txt")),"Run tests/validate_aid_onset_primary.R before plotting")
  ao_check(identical(readLines(file.path(analysis,"VALIDATED.txt")),paste("artifact_manifest_md5",unname(tools::md5sum(file.path(analysis,"artifact_manifest.csv"))))),"Analysis validation receipt is stale")
  inputs <- read("input_checksums");ao_verify(inputs)
  artifacts <- read("artifact_manifest");ao_verify(mutate(artifacts,path=file.path(analysis,path)))
  registry <- read("hypothesis_figures");tests <- read("contrasts");paired <- read("participant_sensitivity")
  tests$sensitivity_disagreement <- tests$significant!=paired$significant[match(paste(tests$contrast_id,tests$correction),paste(paired$contrast_id,paired$correction))]
  raw <- read("raw");means <- read("means");context <- read("decision_context")
  primary <- filter(tests,primary,correction=="loglinear")
  dir.create(dirname(out),showWarnings=FALSE,recursive=TRUE)
  stage <- tempfile(".presentation_build_",tmpdir=dirname(out));dir.create(file.path(stage,"data"),recursive=TRUE)
  on.exit(if(dir.exists(stage)) unlink(stage,recursive=TRUE))
  catalog <- registry %>% distinct(order,figure,section,title) %>% arrange(order) %>%
    mutate(width_in=12,height_in=6.75,png_width=3600,png_height=2025,dpi=300,n_participants=60,exclusions_applied=FALSE)
  for(i in seq_len(nrow(catalog))) {
    f <- catalog[i,];rs <- filter(registry,figure==f$figure)
    message("Rendering ",f$figure)
    rp <- mp <- tp <- tibble()
    subtitle <- "60 participants"
    caption <- "Faint points: participants. Diamonds: model estimates with pointwise 95% CIs. AF = Aid-first; SF = Stimulus-first."
    if(f$section=="core" || f$order==11) {
      pieces <- lapply(seq_len(nrow(rs)),function(j) ao_panel(rs[j,],filter(raw,correction=="loglinear"),
        filter(means,correction=="loglinear"),filter(tests,correction=="loglinear")))
      p <- wrap_plots(lapply(pieces,`[[`,"plot"),nrow=1)
      rp <- bind_rows(lapply(pieces,`[[`,"raw"));mp <- bind_rows(lapply(pieces,`[[`,"means"));tp <- bind_rows(lapply(pieces,`[[`,"tests"))
      caveat <- unique(na.omit(rs$caveat));if(length(caveat)) caption <- paste(caption,paste(caveat,collapse=" "),sep="\n")
      caption <- paste(caption,"Holm within outcome: *** p < .001; ** p < .01; * p < .05; ns p >= .05. Binary estimates condition on zero participant random effect.",sep="\n")
      if(f$order==2) caption <- paste(caption,"Cell labels test human-minus-aid against zero; brackets compare the two timing advantages.",sep="\n")
      if(any(tp$sensitivity_disagreement)) caption <- paste(caption,"[a] Model and paired-participant significance differ; see S5 and the sensitivity tables.",sep="\n")
      if(f$order==11) subtitle <- "Supporting tests | Manual uses the same all-trial data in both panels; it has no advice-correctness category"
    } else if(f$order %in% c(9,10)) {
      pieces <- lapply(seq_len(nrow(rs)),function(j) ao_context(context,rs$outcome[j],rs$panel_title[j]))
      p <- wrap_plots(lapply(pieces,`[[`,"plot"),nrow=1)
      rp <- bind_rows(lapply(pieces,`[[`,"raw"));mp <- bind_rows(lapply(pieces,`[[`,"means"))
      subtitle <- "Supporting descriptive context | 60 participants | Matched valid initial/final response pairs"
      caption <- "Points: participants. Large symbols: equally weighted participant means with pointwise 95% t CIs; no additional hypothesis tests.\nAid-first initial decisions already follow advice exposure. These within-trial changes do not share an independent baseline."
    } else if(f$order==12) {
      tp <- tests %>% filter(comparison=="SF_minus_AF",outcome %in% c("advice_bias","task_bias","advice_dprime","task_dprime","task_dprime_advantage")) %>%
        mutate(label=unname(ao_outcome[outcome]),correction=factor(correction,levels=c("loglinear","extreme_only")))
      p <- ggplot(tp,aes(estimate,forcats::fct_rev(factor(label)),color=correction))+
        geom_vline(xintercept=0,linetype="dashed",color="#9AA4AE")+
        geom_errorbar(aes(xmin=lower,xmax=upper),orientation="y",position=position_dodge(.45),width=.15,linewidth=.8)+
        geom_point(position=position_dodge(.45),size=3)+scale_color_manual(values=c(loglinear="#1976A3",extreme_only="#C56924"),
          labels=c(loglinear="Loglinear (primary)",extreme_only="Extreme-only (sensitivity)"),name=NULL)+ao_theme()+
        labs(x="Stimulus-first - Aid-first",y=NULL)+theme(axis.title.x=element_text(),panel.grid.major.y=element_blank())
      subtitle <- "Supporting sensitivity | Identical model specifications and contrast families under each correction"
      caption <- "Points and intervals: model contrasts with pointwise 95% CIs. Holm adjustment is repeated separately under each correction.\nThese are sensitivity checks on the same responses, not independent confirmatory evidence. Numerical tests accompany this figure."
    } else {
      a <- primary %>% select(contrast_id,hypothesis,outcome,comparison,p_holm,estimate,inference_available) %>% mutate(method="Mixed model")
      b <- paired %>% filter(primary,correction=="loglinear") %>% select(all_of(names(a)[names(a)!="method"])) %>% mutate(method="Participant paired")
      tp <- bind_rows(a,b) %>% mutate(label=paste0(hypothesis," ",unname(ao_outcome[outcome]),": ",unname(ao_comparison[comparison])),
        p_display=pmax(p_holm,1e-8),group=ifelse(hypothesis %in% c("H1","H2","H3","H4"),1L,2L))
      pieces <- lapply(1:2,function(g) {
        d <- filter(tp,group==g);d$label <- factor(d$label,levels=rev(unique(d$label)))
        ggplot(d,aes(p_display,label,color=method,shape=method))+
          geom_vline(xintercept=.05,linetype="dashed",color="#687D8B")+
          geom_line(aes(group=label),color="#CBD1D7",linewidth=.5)+geom_point(size=2.4)+
          scale_x_log10(limits=c(1e-8,1),breaks=c(1e-8,1e-4,.05,1),labels=c("<= 1e-8",".0001",".05","1"))+
          scale_color_manual(values=c(`Mixed model`="#1976A3",`Participant paired`="#C56924"),name=NULL)+
          scale_shape_manual(values=c(`Mixed model`=16,`Participant paired`=17),name=NULL)+ao_theme()+
          theme(axis.text.y=element_text(size=9),panel.grid.major.x=element_line(color="#E5E9ED"),axis.title.x=element_text())+
          labs(x="Holm-adjusted p (log scale)",y=NULL)
      })
      p <- wrap_plots(pieces,nrow=1,guides="collect") & theme(legend.position="bottom")
      subtitle <- "Supporting sensitivity | All 28 unique primary comparisons | Same within-outcome Holm families"
      caption <- "Dots left of .05 meet the adjusted threshold; values below 1e-8 are shown at the left edge. Paired tests weight each participant equally.\nBinary model and paired estimates target different summaries; disagreements are retained. Paired inference never substitutes for unavailable primary inference."
    }
    p <- p+plot_annotation(title=f$title,subtitle=subtitle,caption=caption,
      theme=theme(plot.title=element_text(size=20,face="bold",color="#172B3A"),plot.subtitle=element_text(size=11,color="#526572",margin=margin(b=12)),
        plot.caption=element_text(size=8.8,hjust=0,color="#526572",lineheight=1.25),plot.margin=margin(18,22,14,22)))
    ggsave(file.path(stage,paste0(f$figure,".pdf")),p,width=12,height=6.75,device=cairo_pdf,bg="white")
    ggsave(file.path(stage,paste0(f$figure,".png")),p,width=12,height=6.75,dpi=300,bg="white")
    write_csv(rp,file.path(stage,"data",paste0(f$figure,"_participants.csv")))
    write_csv(mp,file.path(stage,"data",paste0(f$figure,"_estimates.csv")))
    write_csv(tp,file.path(stage,"data",paste0(f$figure,"_annotations.csv")))
    if(nrow(tp) && "contrast_id" %in% names(tp) && f$order<=8) ao_check(all(tp$contrast_id %in% primary$contrast_id),"Unexpected primary annotation")
  }
  combine <- function(stems,path) {
    status <- system2("pdfunite",shQuote(c(file.path(stage,paste0(stems,".pdf")),file.path(stage,path))))
    ao_check(status==0,"PDF combination failed")
  }
  combine(catalog$figure[catalog$section=="core"],"aid_onset_core_hypotheses.pdf")
  combine(catalog$figure,"aid_onset_all_hypotheses.pdf")
  # Historical filename remains a convenient alias for the eight-page core deck.
  file.copy(file.path(stage,"aid_onset_core_hypotheses.pdf"),file.path(stage,"aid_onset_hypotheses_slides.pdf"))
  file.copy(file.path(stage,"aid_onset_core_hypotheses.pdf"),file.path(stage,"primary_hypotheses.pdf"))
  write_csv(catalog,file.path(stage,"figure_catalog.csv"));write_csv(registry,file.path(stage,"data/hypothesis_crosswalk.csv"))
  for(name in c("contrasts","participant_sensitivity","diagnostics","historical_family_sensitivity","selection")) file.copy(file.path(analysis,paste0(name,".csv")),file.path(stage,"data"))
  file.copy(file.path(analysis,"analysis_contract.txt"),file.path(stage,"data"))
  cards <- vapply(seq_len(nrow(catalog)),function(i) {
    f <- catalog[i,]
    links <- paste0('<a href="',f$figure,'.pdf">PDF</a> · <a href="',f$figure,'.png">PNG</a>')
    kinds <- c(participants="Participant values",estimates="Estimates",annotations="Tests")
    for(key in names(kinds)) {
      path <- file.path(stage,"data",paste0(f$figure,"_",key,".csv"))
      if(file.info(path)$size>2) links <- paste0(links,' · <a href="data/',f$figure,'_',key,'.csv">',kinds[[key]],'</a>')
    }
    paste0('<section id="',f$figure,'"><p class="section">',f$section,'</p><h2>',ao_html(f$title),'</h2><img src="',f$figure,
      '.png" alt="',ao_html(f$title),'" loading="lazy"><p>',links,'</p></section>')
  },character(1))
  table <- paste0('<tr><td>',ao_html(paste(primary$hypothesis,unname(ao_outcome[primary$outcome]))),'</td><td>',
    ao_html(unname(ao_comparison[primary$comparison])),'</td><td>',sprintf('%+.3f',primary$estimate),'</td><td>',
    ao_html(ao_p(primary$p_holm)),'</td></tr>')
  writeLines(c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">',
    '<title>Aid onset: revised hypotheses</title><style>body{font:17px/1.55 -apple-system,BlinkMacSystemFont,Arial;color:#213544;background:#f5f7fa;margin:0}main{max-width:1240px;margin:auto;padding:36px 24px}h1{font-size:38px}h2{font-size:25px}a{color:#116b98}section,.intro{background:white;border:1px solid #dce4ea;border-radius:12px;padding:24px;margin:28px 0}img{width:100%;height:auto}.section{font-size:12px;text-transform:uppercase;letter-spacing:2px;color:#677c8b}table{width:100%;border-collapse:collapse;font-size:14px}td,th{text-align:left;border-bottom:1px solid #e1e7ec;padding:8px}nav a{display:inline-block;margin:0 16px 8px 0}</style></head><body><main>',
    '<h1>Aid onset: revised hypothesis report</h1><p>Performance → bias and discrimination → revisions and uptake → subjective evaluations</p>',
    '<div class="intro"><strong>All 60 canonical participants are included. Participant exclusions have not been applied.</strong>',
    '<p>This is a retrospective reorganisation after inspection of earlier results, not a preregistered framework. Eight core figures cover H1–H7; five supporting figures provide context and sensitivity checks. There are 28 unique primary comparisons. H4 reuses H1’s task-discrimination timing contrast, model and Holm-adjusted p-value.</p>',
    '<nav><a href="aid_onset_core_hypotheses.pdf">Core figures (8-page PDF)</a><a href="aid_onset_all_hypotheses.pdf">All figures (13-page PDF)</a><a href="data/hypothesis_crosswalk.csv">Historical crosswalk</a><a href="data/analysis_contract.txt">Analysis definitions</a><a href="data/diagnostics.csv">Model diagnostics</a></nav>',
    '<p>Primary models include participant random intercepts. Binary results use response-scale percentage-point contrasts and predictions conditional on zero participant random effect; continuous outcomes use linear mixed models with Satterthwaite degrees of freedom. Tests are two-sided, with Holm adjustment within hypothesis/outcome. Brackets use *** p &lt; .001, ** p &lt; .01, * p &lt; .05, and ns p &ge; .05. H2 cell labels test human-minus-aid against zero; H7 groups both aided ratings for the mean-aid versus self comparison. Intervals are pointwise 95%; interval overlap is not a test.</p>',
    '<p>Task SDT: H = BLACK response given BLACK stimulus; F = BLACK response given WHITE stimulus. Advice SDT: H = agreement given correct advice; F = agreement given incorrect advice. Both use loglinear correction. Positive task −c indicates BLACK preference; positive advice −c indicates acceptance tendency, which does not establish inappropriate reliance. Final agreement does not establish active uptake.</p>',
    '<p>Shared responses across outcomes do not provide independent evidence. Nonsignificance does not establish equivalence. Aid-first initial decisions already include advice exposure. Selective uptake is conditional on initial disagreement in Stimulus-first, where advice correctness also determines initial correctness.</p>',
    paste0('<p>',sum(read("diagnostics")$inference_available),' of ',nrow(read("diagnostics")),
      ' fits passed the convergence, singularity and rank checks. Conditional cell dispersion is elevated for several binary models, especially switching and beneficial revisions; inspect the diagnostics and participant sensitivities before interpreting their uncertainty.</p>'),
    '<p><a href="data/contrasts.csv">All model contrasts</a> · <a href="data/participant_sensitivity.csv">Participant sensitivities</a> · <a href="data/historical_family_sensitivity.csv">Historical Holm-family sensitivities</a></p></div>',
    cards,'<section><h2>Unique primary comparisons</h2><table><thead><tr><th>Outcome</th><th>Comparison</th><th>Estimate</th><th>Holm p</th></tr></thead><tbody>',
    table,'</tbody></table></section></main></body></html>'),file.path(stage,"index.html"))
  provenance <- ao_hashes(c(inputs$path,file.path(analysis,artifacts$path),file.path(analysis,c("artifact_manifest.csv","VALIDATED.txt")),
    file.path(root,c("plot_aid_onset_hypotheses.R","R/aid_onset_primary_plots.R","tests/validate_aid_onset_primary.R"))))
  write_csv(provenance,file.path(stage,"data/input_checksums.csv"));ao_verify(provenance)
  writeLines(paste("Completed",format(Sys.time(),tz="UTC"),"UTC"),file.path(stage,"COMPLETE.txt"))
  write_csv(ao_artifacts(stage),file.path(stage,"artifact_manifest.csv"))
  ao_check(system2(file.path(R.home("bin"),"Rscript"),shQuote(c(file.path(root,"tests/validate_aid_onset_primary.R"),analysis,stage)))==0,
    "Staged report validation failed; preceding public gallery preserved")
  # All generation and source validation precede replacement of the public gallery.
  archive <- ao_install(stage,out)
  message("Saved unified gallery: ",out)
  if(!is.null(archive)) message("Previous gallery preserved: ",archive)
  invisible(catalog)
}
