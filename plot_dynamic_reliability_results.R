# Figures from a completed, provenance-checked analysis. PDF and 300-dpi PNG.
# Usage: Rscript plot_dynamic_reliability_results.R [analysis_dir] [plot_dir]
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr); library(ggplot2); library(patchwork)})
Sys.setenv(XDG_CACHE_HOME = file.path(tempdir(), "fontconfig-cache"))
dir.create(Sys.getenv("XDG_CACHE_HOME"), recursive = TRUE, showWarnings = FALSE)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 2)
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
root <- dirname(script)
input <- if (length(args)) args[1] else file.path(root, "analysis_outputs/semester2_2026_behavioural")
out <- if (length(args) >= 2) args[2] else file.path(root, "plots/semester2_2026_behavioural")
stopifnot(file.exists(file.path(input, "COMPLETE.txt")))
read <- function(name) read_csv(file.path(input, paste0(name, ".csv")), show_col_types = FALSE)
for (name in c("analysis_artifacts", "input_manifest", "source_checksums")) {
  m <- read(name)
  if (!all(file.exists(m$path)) || !identical(unname(tools::md5sum(m$path)), m$md5))
    stop("Stale or changed analysis inputs/artifacts: ", name, call. = FALSE)
}
dir.create(file.path(out, "data"), recursive = TRUE, showWarnings = FALSE)
unlink(file.path(out, "COMPLETE.txt"))
GROUPS <- c("CAL65", "CAL90")
STAGES <- c("Manual pre", "P1", "P2", "P3", "Manual post")
stage_labels <- c("Manual\npre", "P1\n95% aid", "P2\n70% aid", "P3\n95% aid", "Manual\npost")
colours <- c(CAL65 = "#0072B2", CAL90 = "#D55E00", "CAL90 - CAL65" = "#334858")
methods <- c("Mixed model", "Participant sensitivity")
shapes <- c("Mixed model" = 5, "Participant sensitivity" = 16)
counts <- read("cohort_counts")
N <- sum(counts$n_participants)
group_labels <- setNames(paste0(GROUPS, " (n = ", counts$n_participants[match(GROUPS, counts$group)], ")"), GROUPS)
theme_findings <- function() theme_minimal(base_size = 12.5, base_family = "Helvetica") + theme(
  text = element_text(colour = "#1C2D3E"), plot.title = element_text(size = 16, face = "bold", margin = margin(b=7)),
  plot.subtitle = element_text(size=11, margin=margin(b=10), lineheight=1.15),
  plot.title.position="plot", plot.caption.position="plot",
  plot.caption=element_text(size=9, colour="#566372", hjust=0, lineheight=1.2, margin=margin(t=10)),
  panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(),
  panel.grid.major.y=element_line(colour="#E4E9ED", linewidth=.35),
  axis.title.x=element_blank(), axis.text=element_text(colour="#1C2D3E", size=10.5),
  strip.text=element_text(size=11, face="bold"), panel.spacing=grid::unit(1.3,"lines"),
  legend.position="bottom", legend.title=element_blank(), plot.margin=margin(12,16,10,12),
  plot.background=element_rect(fill="white", colour=NA))
desc_caption <- paste0(N, " participants. Faint points/lines: individuals. Large points: equal-weight participant means.\nWhiskers: pointwise 95% t intervals. Fixed sequence: phase differences also include time and experience effects.")
effect_caption <- "Open diamonds: random-intercept mixed model. Filled circles: equal-weight participant sensitivity.\nWhiskers: pointwise 95% CIs; two-sided tests use Holm adjustment within outcome/family/method."
export <- function(x, name) write_csv(x, file.path(out, "data", paste0(name, ".csv")))
save <- function(plot, name, width=12, height=7) {
  ggsave(file.path(out, paste0(name, ".pdf")), plot, width=width, height=height, device=cairo_pdf, bg="white")
  ggsave(file.path(out, paste0(name, ".png")), plot, width=width, height=height, dpi=300, bg="white")
}
offset <- function(x) (((match(x, sort(unique(x)))*37L) %% 71L)/70-.5)*.17
perf <- read("participant_performance") %>% mutate(stage=factor(stage, STAGES), group=factor(group, GROUPS))
means <- read("descriptive_performance") %>% mutate(stage=factor(stage, STAGES), group=factor(group, GROUPS))
contrasts <- read("planned_contrasts") %>% mutate(method=factor(method, methods))
diagnostics <- read("model_diagnostics")
perf_plot <- function(value, manual=FALSE) {
  percent <- value == "accuracy"; mult <- if (percent) 100 else 1
  d <- perf %>% filter(!manual | stage %in% c("Manual pre", "Manual post")) %>%
    mutate(value=.data[[value]]*mult, x=if (manual) as.integer(stage == "Manual post")+1 else as.integer(stage),
           point_x=x+offset(participant_id))
  s <- means %>% filter(outcome==value, !manual | stage %in% c("Manual pre", "Manual post")) %>%
    mutate(across(c(mean,lower,upper), ~.x*mult), x=if (manual) as.integer(stage == "Manual post")+1 else as.integer(stage))
  export(d, paste0(if (manual) "manual_", value, "_participants")); export(s, paste0(if (manual) "manual_", value, "_means"))
  ggplot(d, aes(point_x,value)) +
    geom_line(aes(group=participant_id), colour="#87929D", alpha=.19, linewidth=.3) +
    geom_point(aes(colour=group), size=1, alpha=.3) +
    geom_line(data=s, aes(x,mean,group=group,colour=group), linewidth=.65) +
    geom_errorbar(data=s, aes(x=x,ymin=lower,ymax=upper,colour=group), inherit.aes=FALSE, width=.1, linewidth=.8) +
    geom_point(data=s,aes(x,mean,colour=group),size=3.5) +
    geom_text(data=s,aes(x,mean,label=if (percent) sprintf("%.1f%%",mean) else sprintf("%.2f s",mean)),
              vjust=-1.4,size=3.2,fontface="bold",check_overlap=TRUE) +
    facet_wrap(~group,labeller=as_labeller(group_labels)) + scale_colour_manual(values=colours,guide="none") +
    scale_x_continuous(breaks=if(manual) 1:2 else 1:5,labels=if(manual) c("Manual pre","Manual post") else stage_labels,expand=expansion(add=.3)) +
    scale_y_continuous(expand=expansion(mult=c(.1,.15))) + theme_findings() +
    labs(title=if(percent) "Accuracy across the session" else "Correct-response times across the session",
      subtitle=if(percent) "All experimental trials; timeouts count as incorrect." else "Arithmetic participant means; inferential comparisons use log RT.",
      y=if(percent) "Accuracy (%)" else "Mean correct RT (s)", caption=desc_caption)
}
effect_data <- function(d) d %>% mutate(
  group_label=ifelse(grepl("CAL90 - CAL65",contrast),"CAL90 - CAL65",ifelse(grepl("CAL65",contrast),"CAL65","CAL90")),
  group_label=factor(group_label,c(GROUPS,"CAL90 - CAL65")), comparison=sub(".*: ","",contrast),
  outcome_label=case_when(outcome=="accuracy" ~ "Accuracy", outcome=="correct_rt" ~ "Correct RT",
    grepl("^Incorrect",contrast) ~ "Incorrect-advice agreement", TRUE ~ "Correct-advice agreement"))
effect_plot <- function(d, title, subtitle, rt=FALSE) {
  d <- effect_data(d) %>% mutate(value=if(rt) 100*(1-ratio) else estimate,
    lo=if(rt) 100*(1-ratio_upper) else lower, hi=if(rt) 100*(1-ratio_lower) else upper,
    comparison=factor(comparison,rev(unique(comparison))))
  # Failed models remain in tables; they do not receive inferential marks.
  unavailable <- sum(!d$inference_available)
  d <- filter(d,inference_available)
  ggplot(d,aes(value,comparison,shape=method,colour=group_label,group=method)) +
    geom_vline(xintercept=0,linetype="dashed",colour="#87929D") +
    geom_errorbar(aes(xmin=lo,xmax=hi),orientation="y",position=position_dodge(width=.4),width=.15,linewidth=.7) +
    geom_point(position=position_dodge(width=.4),size=3.2,stroke=.9) +
    facet_grid(outcome_label~group_label,labeller=labeller(outcome_label=function(x)
      gsub("-advice agreement","-advice\nagreement",x,fixed=TRUE))) + scale_colour_manual(values=colours,guide="none") +
    scale_shape_manual(values=shapes,drop=FALSE) + theme_findings() +
    theme(axis.title.x=element_text(),strip.text.y=element_text(angle=0,size=10),
      panel.grid.major.y=element_blank(),panel.grid.major.x=element_line(colour="#E4E9ED",linewidth=.35)) +
    labs(title=title,subtitle=subtitle,x=if(rt) "Reduction in correct RT (%)" else "Difference (percentage points)",y=NULL,
      caption=paste0(effect_caption,if(unavailable) paste0("\n",unavailable," unavailable model contrasts omitted; see diagnostics.") else ""))
}
save(perf_plot("accuracy"),"01_performance_sequence")
gains <- contrasts %>% filter(outcome=="accuracy",family=="core",grepl("Manual pre$",contrast))
export(gains,"accuracy_benefit_contrasts")
save(effect_plot(gains,"When does automation improve or impair accuracy?",
  "Aided phase minus manual-pre accuracy. The right panel compares those gains between calibration groups."),"02_automation_benefit",12,6)

a <- read("participant_agreement") %>% mutate(phase=factor(phase,c("P1","P2","P3")))
am <- read("descriptive_agreement") %>% mutate(phase=factor(phase,c("P1","P2","P3")))
export(a,"agreement_participants"); export(am,"agreement_means")
agreement_plot <- ggplot(a,aes(phase,100*agreement,colour=group)) +
  geom_line(aes(group=participant_id),alpha=.1,linewidth=.3) + geom_point(alpha=.2,size=.7) +
  geom_line(data=am,aes(y=100*mean,group=group),linewidth=.7,position=position_dodge(width=.15)) +
  geom_errorbar(data=am,aes(y=100*mean,ymin=100*lower,ymax=100*upper),width=.08,linewidth=.8,position=position_dodge(width=.15)) +
  geom_point(data=am,aes(y=100*mean),size=3.5,position=position_dodge(width=.15)) +
  facet_wrap(~advice,labeller=as_labeller(c(Correct="Agreement with correct advice",Incorrect="Agreement with incorrect advice"))) +
  scale_colour_manual(values=colours) + scale_x_discrete(labels=c(P1="P1: 95%",P2="P2: 70%",P3="P3: 95%")) +
  theme_findings() + labs(title="Does aid agreement adjust to relative competence?",
    subtitle="Answered trials only. Agreement is a behavioural proxy, not an advice-caused change of mind.",y="Agreement (%)",
    caption="Faint points/lines: individuals. Large points: equal-weight participant means; whiskers: pointwise 95% t intervals.")
drop <- contrasts %>% filter(outcome=="agreement",family=="core",grepl("P2 - P1$",contrast))
export(drop,"agreement_drop_contrasts")
save(agreement_plot / effect_plot(drop,"Change when reliability drops","P2 minus P1, stratified by advice correctness.") +
  plot_layout(heights=c(1.1,1)),"03_relative_competence",13,11)

recovery <- contrasts %>% filter(family=="core",grepl("P3 - P[12]$",contrast))
export(recovery,"recovery_contrasts")
save(effect_plot(recovery,"Does behaviour recover when aid reliability returns?",
  "P3 - P2 measures change after recovery; P3 - P1 measures the remaining difference. Nonsignificance is not equivalence."),"04_recovery",14,9)

trajectories <- read("descriptive_trajectories") %>% mutate(x=(match(phase,c("P1","P2","P3"))-1)*400+(bin-.5)*100)
curves <- bind_rows(lapply(c("accuracy","agreement"),function(v) {
  path <- file.path(input,paste0(v,"_trend_curves.csv"))
  ok <- diagnostics$inference_available[diagnostics$model==paste0(v,"_trend")]
  if(!file.exists(path) || !isTRUE(ok)) return(tibble())
  d <- read_csv(path,show_col_types=FALSE)
  d %>% mutate(outcome=if(v=="accuracy") "Accuracy" else paste(advice,"advice agreement"),
    x=(match(phase,c("P1","P2","P3"))-1)*400+1+399*progress)
}))
export(trajectories,"trajectory_bins"); export(curves,"model_trajectories")
export(read("participant_trajectories"),"trajectory_participants")
export(filter(contrasts,family=="adaptation"),"adaptation_contrasts")
dynamics <- ggplot(trajectories,aes(x,100*mean,colour=group)) +
  geom_vline(xintercept=c(400.5,800.5),linetype="dashed",colour="#A6AFB7") +
  geom_errorbar(aes(ymin=100*lower,ymax=100*upper),width=12,linewidth=.5) +
  geom_point(size=2) + geom_text(aes(label=n),vjust=1.7,size=2.5,show.legend=FALSE) +
  facet_grid(outcome~group,labeller=labeller(group=as_labeller(group_labels))) +
  scale_colour_manual(values=colours,guide="none") + scale_x_continuous(breaks=c(200,600,1000),labels=c("P1: 95%","P2: 70%","P3: 95%")) +
  theme_findings() + labs(title="How does behaviour change within each reliability phase?",
    subtitle="Points: nonoverlapping 100-trial bins. Lines: phase-specific logistic model trends.",
    y="Accuracy / agreement (%)",caption="Bins give equal weight to participants with observations; small labels show their number. Whiskers: pointwise 95% t intervals.\nEmpty advice cells are missing, not zero. Curves condition on a zero participant random effect and do not estimate learning times.")
if(nrow(curves)) dynamics <- dynamics + geom_line(data=curves,aes(x,100*prob,group=interaction(group,phase)),linewidth=.65)
save(dynamics,"05_adaptation_trajectories",13,10)

rt <- contrasts %>% filter(outcome=="correct_rt",family=="speed",grepl("Manual pre$",contrast),!grepl("CAL90 - CAL65",contrast))
export(rt,"rt_benefit_contrasts")
save(perf_plot("mean_correct_rt") / effect_plot(rt,"How much faster are correct responses with the aid?",
  "Positive values indicate faster responses: 100 x (1 - aided/manual geometric-mean RT ratio).",TRUE) +
  plot_layout(heights=c(1,1)),"06_response_speed",13,11)
manual <- contrasts %>% filter(family=="manual_change")
export(manual,"manual_change_contrasts")
save(perf_plot("accuracy",TRUE) / perf_plot("mean_correct_rt",TRUE) +
  plot_annotation(title="Does manual performance change across the session?",
    subtitle="These paired changes combine practice, fatigue and possible automation exposure effects.",theme=theme_findings()),"07_manual_pre_post",12,11)

ratings <- read("participant_ratings")
export(ratings,"subjective_participants"); export(read("subjective_contrasts"),"subjective_contrasts")
rating_panel <- function(value,title,ylabel) {
  d <- ratings %>% mutate(value=.data[[value]])
  s <- d %>% group_by(group) %>% summarise(n=n(),mean=mean(value),se=sd(value)/sqrt(n()),.groups="drop") %>%
    mutate(lower=mean-qt(.975,n-1)*se,upper=mean+qt(.975,n-1)*se)
  export(s,paste0(value,"_means"))
  p <- ggplot(d,aes(group,value,colour=group)) + geom_point(position=position_jitter(width=.07,seed=20261005),alpha=.3,size=1.4) +
    geom_errorbar(data=s,aes(y=mean,ymin=lower,ymax=upper),width=.08,linewidth=.8) +
    geom_point(data=s,aes(y=mean),size=3.5) + scale_colour_manual(values=colours,guide="none") +
    theme_findings() + labs(title=title,y=ylabel)
  if(value=="perceived_aid_error") p <- p + geom_hline(yintercept=0,linetype="dashed",colour="#87929D")
  p
}
# Common group-adjusted slopes from the stored participant-level models; no unadjusted smoother.
association_panel <- function(value,title,ylabel) {
  fit <- readRDS(file.path(input,"models",paste0(value,"_association.rds")))
  line <- bind_rows(lapply(GROUPS,function(g) {
    r <- range(ratings$agreement[ratings$group==g]); data.frame(group=g,agreement=seq(r[1],r[2],length.out=40))
  })) %>% mutate(agreement_10pp=(agreement-mean(ratings$agreement))/.1)
  line$estimate <- as.numeric(predict(fit,newdata=line)); export(line,paste0(value,"_association_lines"))
  ggplot(ratings,aes(100*agreement,.data[[value]],colour=group)) + geom_point(size=1.8,alpha=.65) +
    geom_line(data=line,aes(x=100*agreement,y=estimate,colour=group),inherit.aes=FALSE,linewidth=.7) +
    scale_colour_manual(values=colours) + theme_findings() + theme(axis.title.x=element_text()) +
    labs(title=title,x="P2 incorrect-advice agreement (%)",y=ylabel)
}
subjective <- (rating_panel("trust","Final trust in the aid","Six-item mean trust (1-5)") |
  rating_panel("perceived_aid_error","Perceived minus realised aid accuracy","Rating error (percentage points)")) /
  (association_panel("trust","Trust and degraded-phase behaviour","Mean trust (1-5)") |
  association_panel("perceived_aid_error","Perceived accuracy and degraded-phase behaviour","Rating error (percentage points)")) +
  plot_annotation(title="How do participants evaluate the complete aided sequence?",
    caption="Ratings were collected once after all 1,200 aided trials. Top: participant observations and means with pointwise 95% t intervals.\nBottom: group-adjusted linear associations, with a common slope. HC3 inference is in the report. Associations do not establish causation.",
    theme=theme_findings())
save(subjective,"08_subjective_evaluation",13,10)
# Available self-ratings describe manual performance only, never individual aided phases.
self <- ratings %>% select(participant_id,group,self_pre,self_post) %>% pivot_longer(starts_with("self_"),names_to="segment",values_to="rated") %>%
  mutate(stage=factor(ifelse(segment=="self_pre","Manual pre","Manual post"),c("Manual pre","Manual post"))) %>%
  left_join(perf %>% select(participant_id,stage,accuracy),by=c("participant_id","stage"))
export(self,"manual_self_ratings")
self_plot <- ggplot(self,aes(100*accuracy,rated,colour=group)) + geom_abline(slope=1,intercept=0,linetype="dashed",colour="#87929D") +
  geom_point(alpha=.65,size=2) + facet_wrap(~stage) + scale_colour_manual(values=colours) +
  coord_equal(xlim=c(0,100),ylim=c(0,100)) + theme_findings() + theme(axis.title.x=element_text()) +
  labs(title="How accurately do participants judge their manual performance?",x="Observed accuracy (%)",y="Self-rated accuracy (%)",
       caption="Descriptive comparison only. Diagonal: accurate self-rating. Automation self-accuracy ratings are absent from these exports.")
save(self_plot,"09_manual_self_ratings",11,6)
figures <- tools::file_path_sans_ext(basename(sort(list.files(out,"\\.pdf$"))))
titles <- c("Performance across the sequence", "Automation benefits and costs", "Relative competence and aid agreement",
  "Recovery", "Within-phase adaptation", "Correct-response speed", "Manual pre/post change", "Subjective evaluation", "Manual self-ratings")
cards <- vapply(seq_along(figures), function(i) sprintf(
  '<article><h2>%s</h2><a href="%s.png"><img src="%s.png" alt="%s"></a><p><a href="%s.pdf">PDF</a> | <a href="%s.png">PNG</a></p></article>',
  titles[i],figures[i],figures[i],titles[i],figures[i],figures[i]),character(1))
writeLines(c('<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">',
  '<title>Dynamic reliability: behavioural figures</title><style>body{font:16px Helvetica,Arial,sans-serif;color:#1C2D3E;background:#f7f9fb;margin:32px}main{display:grid;grid-template-columns:repeat(auto-fit,minmax(420px,1fr));gap:24px}article{background:white;padding:18px;border:1px solid #e4e9ed;border-radius:8px}h2{font-size:18px}img{width:100%;height:auto}a{color:#0072B2}p{line-height:1.5}header{max-width:1000px;margin-bottom:28px}</style>',
  '<header><h1>Dynamic reliability: behavioural figures</h1>',
  sprintf('<p>%d participants. CAL65 is blue; CAL90 is orange. Open diamonds show mixed-model estimates; filled circles show participant sensitivity estimates.</p>',N),
  '<p>All hypotheses were specified after data collection. Convergence alone does not establish model adequacy; read participant sensitivities alongside mixed-model estimates. Phase order is fixed; agreement does not establish advice-caused switching. Confidence intervals are pointwise.</p>',
  '<p><a href="presentation/index.html">Presentation figures: one 16:9 figure per H1-H4 / E1-E2</a>. Click any supporting figure below for its full-resolution PNG or PDF. Numerical panel data are in the <a href="data/">data folder</a>.</p></header><main>',
  cards,'</main></html>'),file.path(out,"index.html"))
source(file.path(root,"plot_dynamic_reliability_slides.R"))
plot_dynamic_reliability_slides(input,out,root)
plot_sources <- c(script,file.path(root,"plot_dynamic_reliability_slides.R"))
write_csv(tibble(path=plot_sources,md5=unname(tools::md5sum(plot_sources))),file.path(out,"plot_sources.csv"))
manifest <- list.files(out,pattern="\\.(csv|pdf|png|html)$",recursive=TRUE,full.names=TRUE)
# The presentation bundle has its own receipt and can be rebuilt independently.
manifest <- manifest[!startsWith(normalizePath(manifest),paste0(normalizePath(file.path(out,"presentation")),"/"))]
manifest <- manifest[basename(manifest)!="plot_manifest.csv"]
write_csv(tibble(path=normalizePath(manifest),md5=unname(tools::md5sum(manifest))),file.path(out,"plot_manifest.csv"))
writeLines(format(Sys.time(),tz="UTC"),file.path(out,"COMPLETE.txt"))
message("Figures completed: ",out)
