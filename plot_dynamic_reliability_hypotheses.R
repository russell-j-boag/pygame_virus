# Saved revised analyses -> validated staged report -> atomic publication.
# Rscript plot_dynamic_reliability_hypotheses.R [analysis_dir] [plot_dir] [combined_csv] [qc_dir] [--stage-only]
dpr_verify_analysis <- function(base) {
  primary <- file.path(base,"primary")
  if(!file.exists(file.path(primary,"COMPLETE.txt")))stop("Run Rscript analyse_dynamic_reliability_primary.R before plotting.",call.=FALSE)
  input <- readr::read_csv(file.path(primary,"input_checksums.csv"),show_col_types=FALSE)
  output <- readr::read_csv(file.path(primary,"artifact_manifest.csv"),show_col_types=FALSE)
  sha <- function(path)digest::digest(file=path,algo="sha256")
  for(m in list(input,transform(output,path=file.path(primary,path)))) {
    if(!all(file.exists(m$path)) || !identical(vapply(m$path,sha,character(1),USE.NAMES=FALSE),m$sha256))
      stop("Revised analysis inputs, specification or artifacts changed. Run analyse_dynamic_reliability_primary.R before plotting.",call.=FALSE)
  }
  invisible(input)
}
dpr_publish_report <- function(stage,plot_base) {
  stopifnot(file.exists(file.path(stage,"VALIDATED.txt")))
  verify <- function(manifest,relative=FALSE) {
    m <- readr::read_csv(manifest,show_col_types=FALSE)
    paths <- if(relative)file.path(stage,m$path) else m$path
    stopifnot(all(file.exists(paths)),identical(vapply(paths,function(p)digest::digest(file=p,algo="sha256"),character(1),USE.NAMES=FALSE),m$sha256))
  }
  verify(file.path(stage,"data/input_checksums.csv"))
  verify(file.path(stage,"data/artifact_manifest.csv"),TRUE)
  out <- file.path(plot_base,"presentation");previous <- NULL
  if(dir.exists(out)) {
    archive <- file.path(plot_base,"archive");dir.create(archive,showWarnings=FALSE)
    previous <- tempfile(paste0("presentation_",format(Sys.time(),"%Y%m%d_%H%M%S"),"_"),tmpdir=archive)
    stopifnot(file.rename(out,previous))
  }
  if(!file.rename(stage,out)) {
    if(!is.null(previous))stopifnot(file.rename(previous,out))
    stop("Publication failed; preceding report restored.",call.=FALSE)
  }
  message("Published H1-H7 report: ",file.path(out,"index.html"))
  if(!is.null(previous))message("Previous report archived intact: ",previous)
  invisible(out)
}
plot_dynamic_reliability_hypotheses <- function(base,plot_base,root,input=NULL,qc=NULL,publish=TRUE) {
  suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr);library(ggplot2);library(patchwork)})
  root <- normalizePath(root);base <- normalizePath(base)
  source(file.path(root,"analyse_dynamic_reliability_primary.R"),local=TRUE)
  source(file.path(root,"R/dynamic_reliability_primary_plots.R"),local=TRUE)
  source(file.path(root,"plot_dynamic_reliability_slides.R"),local=TRUE)
  input <- normalizePath(if(is.null(input))file.path(root,"data/data_virus_all.csv") else input)
  qc <- normalizePath(if(is.null(qc))file.path(root,"analysis_outputs/semester2_2026_qc") else qc)
  provenance <- dpr_verify_analysis(base)
  stopifnot(input %in% provenance$path,file.path(qc,"manifest.json") %in% provenance$path)
  primary <- file.path(base,"primary");bundle <- readRDS(file.path(primary,"bundle.rds"))
  registry <- read_csv(file.path(root,"config/hypothesis_figures.csv"),show_col_types=FALSE,na="")
  registry$contrast_ids[is.na(registry$contrast_ids)] <- ""
  catalog <- registry %>% distinct(order,filename,title,role,hypothesis,legacy_labels,notes) %>% arrange(order)
  stopifnot(nrow(catalog)==22,sum(catalog$role=="Core")==12,!anyDuplicated(catalog$filename))
  dir.create(plot_base,recursive=TRUE,showWarnings=FALSE);plot_base <- normalizePath(plot_base)
  stage <- tempfile(".presentation_build_",tmpdir=plot_base);dir.create(file.path(stage,"data"),recursive=TRUE)
  message("Staged H1-H7 report: ",stage)
  source_paths <- c(provenance$path,file.path(primary,c("input_checksums.csv","artifact_manifest.csv","bundle.rds","contrast_audit.rds")),
    file.path(root,c("plot_dynamic_reliability_hypotheses.R","plot_dynamic_reliability_slides.R","R/dynamic_reliability_primary_plots.R","tests/validate_dynamic_reliability_primary.R","tests/test_dynamic_reliability_errorbars.R")))
  start_hashes <- task_hashes(unique(source_paths))
  built <- dpr_plot_build(bundle,registry,stage)
  built$figures <- lapply(built$figures,dpr_style_errorbars)
  for(stem in names(built$figures))for(ext in c("pdf","png"))
    ggsave(file.path(stage,paste0(stem,".",ext)),built$figures[[stem]],width=12,height=6.75,dpi=300,bg="white",device=if(ext=="pdf")cairo_pdf else "png")
  # Redraw historical pages from saved results with the shared display style.
  old_stems <- c("19_historical_incorrect_agreement_drop","20_historical_accuracy_and_agreement_recovery","21_historical_trust_and_incorrect_agreement")
  histories <- filter(catalog,role=="Historical")
  historical_manifest <- list()
  legacy_stems <- c("01_h1_relative_competence","03_h3_recovery","04_h4_subjective_evaluation")
  legacy <- list()
  scratch <- tempfile("historical_figures_");dir.create(scratch)
  on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
  for(sample in c("primary","all_participants")) {
    legacy_input <- if(sample=="primary")file.path(base,"hypothesis_components/primary/analysis") else base
    legacy[[sample]] <- build_dynamic_reliability_legacy_slides(legacy_input,file.path(scratch,sample),root,return_bundle=TRUE)
    start_hashes <- distinct(bind_rows(start_hashes,task_hashes(legacy[[sample]]$inputs)),path,.keep_all=TRUE)
  }
  for(sample in c("primary","all_participants"))for(i in seq_len(nrow(histories))) {
    prefix <- if(sample=="primary")"" else "sensitivity_";stem <- paste0(prefix,histories$filename[i])
    candidates <- file.path(plot_base,"presentation",paste0(prefix,c(histories$filename[i],old_stems[i])))
    found <- candidates[file.exists(paste0(candidates,".pdf")) & file.exists(paste0(candidates,".png"))][1]
    if(is.na(found))stop("Missing historical figure: ",stem,call.=FALSE)
    historical_caption <- paste(dpr_sample_caption(bundle$membership,sample),
      legacy[[sample]]$figures[[legacy_stems[i]]]$patches$annotation$caption)
    p <- legacy[[sample]]$figures[[legacy_stems[i]]]+plot_annotation(
      title=c("20. Incorrect-advice agreement after the drop","21. Accuracy and agreement recovery",
        "22. Final evaluations and incorrect-advice agreement")[i],
      subtitle=unname(dpr_measure_subtitles[[histories$filename[i]]]),
      caption=paste(strwrap(historical_caption,width=155),collapse="\n"))
    p <- dpr_style_errorbars(p)
    for(ext in c("pdf","png")) {
      dst <- file.path(stage,paste0(stem,".",ext))
      ggsave(dst,p,width=12,height=6.75,dpi=300,bg="white",device=if(ext=="pdf")cairo_pdf else "png")
      historical_manifest[[length(historical_manifest)+1L]] <- tibble(path=paste0(stem,".",ext),sha256=task_sha(dst))
    }
    for(path in list.files(file.path(dirname(found),"data"),pattern=paste0("^",basename(found),"_.*\\.csv$"),full.names=TRUE)) {
      target <- file.path("data",sub(basename(found),stem,basename(path),fixed=TRUE))
      regenerated <- file.path(scratch,sample,"presentation/data",sub(basename(found),legacy_stems[i],basename(path),fixed=TRUE))
      stopifnot(file.exists(regenerated),isTRUE(all.equal(read_csv(path,show_col_types=FALSE),
        read_csv(regenerated,show_col_types=FALSE),check.attributes=FALSE)))
      stopifnot(file.copy(path,file.path(stage,target)))
      historical_manifest[[length(historical_manifest)+1L]] <- tibble(sha256=task_sha(path),path=target)
    }
  }
  write_csv(bind_rows(historical_manifest),file.path(stage,"data/historical_manifest.csv"))
  full_catalog <- bind_rows(lapply(c("primary","all_participants"),function(sample) {
    members <- filter(bundle$membership,sample=="all_participants" | in_primary)
    prefix <- if(sample=="primary")"" else "sensitivity_"
    mutate(catalog,filename=paste0(prefix,filename),sample=sample,
      n_participants=nrow(members),n_cal65=sum(members$calibration_target_group=="CAL65"),n_cal90=sum(members$calibration_target_group=="CAL90"))
  }))
  write_csv(full_catalog,file.path(stage,"figure_catalog.csv"));write_csv(registry,file.path(stage,"hypothesis_mapping.csv"))
  write_csv(registry,file.path(stage,"data/figure_contract.csv"))
  write_csv(built$annotations,file.path(stage,"data/revised_annotations.csv"))
  for(kind in c("correction","participant","sample"))for(sample in if(kind=="sample")"primary" else c("primary","all_participants"))
    write_csv(dpr_comparison(bundle$results,kind,sample),file.path(stage,"data",paste0(sample,"_",kind,"_comparison.csv")))
  # All public table links are local copies, independent of future component builds.
  dir.create(file.path(stage,"data/analysis"))
  files <- list.files(primary,pattern="\\.(csv|txt)$",recursive=TRUE,full.names=TRUE)
  for(path in files) {
    dest <- file.path(stage,"data/analysis",substring(path,nchar(primary)+2));dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
    stopifnot(file.copy(path,dest))
  }
  unite <- Sys.which("pdfunite");stopifnot(nzchar(unite))
  for(name in c("core_hypotheses","all_hypotheses","sensitivity_hypotheses")) {
    chosen <- filter(full_catalog,sample==if(name=="sensitivity_hypotheses")"all_participants" else "primary")
    if(name=="core_hypotheses")chosen <- filter(chosen,role=="Core")
    stopifnot(system2(unite,shQuote(c(file.path(stage,paste0(chosen$filename,".pdf")),file.path(stage,paste0(name,".pdf")))))==0)
  }
  dpr_report_html(stage,full_catalog,bundle,built$annotations)
  stopifnot(identical(vapply(start_hashes$path,task_sha,character(1),USE.NAMES=FALSE),start_hashes$sha256))
  write_csv(start_hashes,file.path(stage,"data/input_checksums.csv"))
  log <- file.path(stage,"validation.log")
  status <- system2(file.path(R.home("bin"),"Rscript"),shQuote(c(file.path(root,"tests/validate_dynamic_reliability_primary.R"),base,stage)),stdout=log,stderr=log)
  if(status!=0)stop("Validation failed; staged report retained: ",stage,"; see validation.log",call.=FALSE)
  message(paste(readLines(log),collapse="\n"))
  writeLines(paste("Validated",format(Sys.time(),tz="UTC")),file.path(stage,"VALIDATED.txt"))
  paths <- list.files(stage,recursive=TRUE,full.names=TRUE)
  write_csv(tibble(path=substring(paths,nchar(stage)+2),bytes=file.info(paths)$size,
    sha256=vapply(paths,task_sha,character(1),USE.NAMES=FALSE)),file.path(stage,"data/artifact_manifest.csv"))
  if(publish)dpr_publish_report(stage,plot_base) else message("Validated staged report ready for visual review: ",stage)
  invisible(if(publish)file.path(plot_base,"presentation") else stage)
}
dpr_report_html <- function(out,catalog,bundle,annotations) {
  esc <- function(x)gsub('"',"&quot;",gsub(">","&gt;",gsub("<","&lt;",gsub("&","&amp;",as.character(x),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE)
  p_text <- function(p)ifelse(is.na(p),"unavailable",ifelse(p<.001,"< .001",sprintf("%.3f",p)))
  primary <- filter(catalog,sample=="primary");secondary <- filter(catalog,sample=="all_participants")
  prose <- c(H1="How do accuracy, discrimination and correct-response speed benefit from assistance at each reliability phase, and how do benefits differ by calibration group?",
    H2="Does the final human-plus-aid decision outperform following the aid alone? Accuracy includes timeouts; task SDT uses the same human-answered trials for both agents.",
    H3="How does the reliability drop alter advice acceptance, BLACK response preference, selective advice following and final stimulus discrimination?",
    H4="Which behaviours recover when reliability returns, and which still differ from the initial high-reliability phase?",
    H5="How do accuracy, advice following and correct-response speed change from start to middle and from middle to end of each phase, and is there added quadratic curvature? Phase-centred quadratic models were adopted on 20 October 2026 after inspecting the linear trajectories. The two change contrasts use predictions at trial positions 1, 200.5 and 400; the midpoint lies between trials 200 and 201. Models use all eligible individual trials and figures retain four 100-trial bins. This split was selected retrospectively after inspecting the trajectories. Both changes and curvature share a Holm family of 27 tests for accuracy/RT or 54 for agreement. Curvature is tested on log-odds or log-RT scales; a curved trajectory or nonsignificant late slope does not establish a plateau.",
    H6="How do calibration groups evaluate the aid after the complete sequence, and how accurate are their perceived reliability ratings?",
    H7="How are final trust and perceived-reliability error associated with whole-block advice acceptance and selectivity within each calibration group, and do those slopes differ between groups?")
  html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Dynamic reliability: revised H1-H7 hypotheses</title>',
    '<style>body{font:17px/1.55 system-ui,sans-serif;color:#25323d;max-width:1250px;margin:32px auto;padding:0 24px}h1{font-size:34px}h2{margin-top:45px}h3{font-size:23px}img{width:100%;border:1px solid #dce4e9}article{margin:30px 0 48px}a{color:#1976a3}.meta{font-size:14px;color:#566372}nav,.method{padding:18px;background:#f0f5f7}details{margin:15px 0}summary{cursor:pointer;font-weight:600}table{border-collapse:collapse;font-size:13px;width:100%}td,th{padding:7px;border-bottom:1px solid #dde5e9;text-align:left}.warn{background:#fff1db;padding:15px}</style></head><body>',
    '<h1>Adapting to changing aid reliability</h1>',
    '<p>How does relative human-aid competence shape the benefits of assistance, adaptation to a reliability drop, and recovery when reliability returns?</p>',
    sprintf('<p><strong>%d QC-retained participants throughout the primary analyses; all %d as matched sensitivity.</strong> Seven revised hypotheses, 12 core figures, and 206 primary comparisons per cohort.</p>',sum(bundle$membership$in_primary),nrow(bundle$membership)),
    '<p><a href="core_hypotheses.pdf">12-page core PDF</a> | <a href="all_hypotheses.pdf">All 22 primary-sample figures</a> | <a href="sensitivity_hypotheses.pdf">22-page all-participant sensitivity</a> | <a href="hypothesis_mapping.csv">Hypothesis crosswalk</a></p>',
    '<div class="method"><p>The revised H1-H7 framework was agreed on 17 October 2026 after inspecting earlier results. It is retrospective, not preregistered. The hypotheses and correction families below replace the earlier presentation numbering; historical figures retain their original tests.</p>',
    '<p>Faint points show participants; open diamonds show fitted estimates with pointwise 95% intervals. Tests are two-sided, with Holm correction within hypothesis and outcome. *** p&lt;.001; ** p&lt;.01; * p&lt;.05; ns p&ge;.05. Unavailable inference is labelled unavailable. Sensitivity analyses do not replace primary inference.</p>',
    '<p>Advice -c means acceptance; task -c means BLACK preference. Advice d-prime measures selective following; task d-prime measures final stimulus discrimination. These use the same responses and are not independent replications.</p></div>',
    '<nav><a href="#H1">H1 Benefits</a> | <a href="#H2">H2 Human value</a> | <a href="#H3">H3 Drop</a> | <a href="#H4">H4 Recovery</a> | <a href="#H5">H5 Adaptation</a> | <a href="#H6">H6 Evaluations</a> | <a href="#H7">H7 Associations</a> | <a href="#supporting">Supporting</a> | <a href="#historical">Historical</a></nav>',
    '<details><summary>Methods, cohort decisions and diagnostics</summary>',
    paste0('<p>Excluded IDs: ',paste(bundle$membership$participant_id[!bundle$membership$in_primary],collapse=", "),'. Calibration, near-chance and anticipatory rules determine inclusion; manual target deviation remains diagnostic. QC selection is retrospective and includes post-exposure performance; compare the all-participant sensitivity.</p>'),
    '<p>Repeated outcomes use participant-random-intercept mixed models. Single final ratings use participant-level HC3 regressions. H7 and the supporting Phase-2 associations include group-by-SDT interactions: two group slopes and their difference for each predictor, with six-test Holm families per rating. H7 uses whole-block pooled SDT counts. This interaction update followed review of the common-slope plots and is retrospective. Extra participant-cell variation can remain despite convergence; participant sensitivities and diagnostic limitations remain visible.</p>',
    '<p><a href="data/analysis/methods.txt">Full methods</a> | <a href="data/analysis/cohort_membership.csv">Membership</a> | <a href="data/analysis/contrasts.csv">All tests</a> | <a href="data/analysis/model_diagnostics.csv">Model diagnostics</a> | <a href="data/primary_sample_comparison.csv">Cohort sensitivity</a> | <a href="data/primary_participant_comparison.csv">Participant sensitivity</a> | <a href="data/primary_correction_comparison.csv">Correction sensitivity</a></p></details>')
  bad <- filter(bundle$diagnostics,!inference_available)
  if(nrow(bad))html <- c(html,paste0('<p class="warn">Primary inference unavailable for: ',esc(paste(paste(bad$sample,bad$model,sep="/"),collapse=", ")),'. See model diagnostics.</p>'))
  links <- function(stem) {
    s <- paste0('<a href="',stem,'.pdf">PDF</a> | <a href="',stem,'.png">PNG</a>')
    for(kind in c("data","participants","annotations"))if(file.exists(file.path(out,"data",paste0(stem,"_",kind,".csv"))))
      s <- paste0(s,' | <a href="data/',stem,'_',kind,'.csv">',kind,'</a>')
    s
  }
  last <- ""
  for(i in seq_len(nrow(primary))) {
    r <- primary[i,];s <- secondary[i,]
    if(r$role=="Core" && r$hypothesis!=last) {
      html <- c(html,paste0('<h2 id="',r$hypothesis,'">',r$hypothesis,'</h2><p>',prose[[r$hypothesis]],'</p>'));last <- r$hypothesis
    }
    if(i==which(primary$role=="Supporting")[1])html <- c(html,'<h2 id="supporting">Supporting analyses and sensitivities</h2>')
    if(i==which(primary$role=="Historical")[1])html <- c(html,'<h2 id="historical">Historical hypothesis formulations</h2><p>Original plotted data and tests are retained, with updated display text and the shared error-bar style. Their hypothesis labels belong to the earlier framework.</p>')
    r$notes <- gsub("; nonsignificance is not equivalence|; the post block is not an untreated control","",r$notes)
    r$notes <- sub("Original figure and test families retained unchanged", "Original plotted data and test families retained; display text and error-bar style updated",r$notes,fixed=TRUE)
    html <- c(html,paste0('<article><h3>',esc(r$title),'</h3><p class="meta">',esc(r$legacy_labels),' | ',esc(r$notes),'</p>',
      '<img loading="lazy" src="',r$filename,'.png" alt="',esc(r$title),'"><p>',links(r$filename),'</p>'))
    a <- filter(annotations,filename==r$filename)
    if(nrow(a)) {
      html <- c(html,'<details><summary>Tests shown in this figure</summary><table><tr><th>Outcome</th><th>Comparison</th><th>Group</th><th>Estimate [95% CI]</th><th>Holm p</th><th>Family size</th></tr>')
      for(j in seq_len(nrow(a))) {
        q <- a[j,];v <- if(q$outcome=="correct_rt" && q$estimand!="curvature")sprintf("%.3f [%.3f, %.3f] ratio",q$ratio,q$ratio_lower,q$ratio_upper) else sprintf("%.3f [%.3f, %.3f] %s",q$estimate,q$lower,q$upper,if(q$estimand=="curvature")q$inference_scale else "")
        if(!q$inference_available)v <- paste(v,"(inference unavailable)")
        html <- c(html,paste0('<tr><td>',esc(q$outcome),'</td><td>',esc(q$comparison),'</td><td>',esc(q$group),'</td><td>',v,'</td><td>',p_text(q$p_holm),'</td><td>',q$family_size,'</td></tr>'))
      }
      html <- c(html,'</table></details>')
    }
    if(identical(r$hypothesis,"H5")) {
      outcome <- unique(a$outcome)
      html <- c(html,'<details><summary>Linear-only sensitivity and trajectory shape</summary>',
        '<p>Linear-only endpoint tests use separate Holm families (9 accuracy, 18 agreement, 9 RT). Participant linear slopes accompany these fits only and do not validate quadratic curvature. Shape intervals are pointwise and descriptive; fitted reversals are point estimates, not additional tests.</p>',
        '<p><a href="data/analysis/h5_linear_sensitivity.csv">Linear model and participant sensitivity tests</a> | <a href="data/analysis/primary/shape_summaries.csv">Primary slope summaries</a> | <a href="data/analysis/all_participants/shape_summaries.csv">All-participant slope summaries</a></p>',
        '<table><tr><th>Sample</th><th>Method</th><th>Phase / advice</th><th>Group</th><th>Linear net change [95% CI]</th><th>Holm p</th></tr>')
      linear <- filter(bundle$results,hypothesis=="H5",outcome==!!outcome,analysis %in% c("linear_sensitivity","linear_participant_sensitivity"))
      for(j in seq_len(nrow(linear))) {
        q <- linear[j,];v <- if(outcome=="correct_rt")sprintf("%.3f [%.3f, %.3f] ratio",q$ratio,q$ratio_lower,q$ratio_upper) else sprintf("%.2f [%.2f, %.2f] pp",q$estimate,q$lower,q$upper)
        html <- c(html,paste0('<tr><td>',esc(q$sample),'</td><td>',esc(q$analysis),'</td><td>',q$phase,' / ',q$advice,
          '</td><td>',esc(q$group),'</td><td>',v,'</td><td>',p_text(q$p_holm),'</td></tr>'))
      }
      html <- c(html,'</table><p>Slopes below are per full-phase unit of progress. Model slopes are log odds (accuracy/agreement) or log RT. Response slopes are percentage points (accuracy/agreement) or seconds (geometric RT). A reversal is flagged only when the fitted turning point lies strictly inside the phase.</p>',
        '<table><tr><th>Sample</th><th>Phase / advice</th><th>Group</th><th>Position</th><th>Model slope [95% CI]</th><th>Response slope [95% CI]</th><th>Fitted reversal</th></tr>')
      for(sample_name in c("primary","all_participants")) {
        shape <- filter(bundle$data[[sample_name]]$shape_summaries,outcome==!!outcome)
        for(j in seq_len(nrow(shape))) {
          q <- shape[j,]
          html <- c(html,paste0('<tr><td>',sample_name,'</td><td>',q$phase,' / ',if(is.na(q$advice))"All" else q$advice,
            '</td><td>',q$group,'</td><td>',q$position,'</td><td>',sprintf('%.3f [%.3f, %.3f]',q$model_estimate,q$model_lower,q$model_upper),
            '</td><td>',sprintf('%.3f [%.3f, %.3f]',q$response_estimate,q$response_lower,q$response_upper),'</td><td>',
            if(q$reversal_within_phase)sprintf('At %.1f%% of phase',100*q$turning_progress) else 'None within phase','</td></tr>'))
        }
      }
      html <- c(html,'</table></details>')
    }
    if(r$hypothesis %in% c("H7","S7")) {
      common <- filter(bundle$results,hypothesis==r$hypothesis,analysis=="common_slope_sensitivity",correction=="loglinear")
      html <- c(html,'<details><summary>Supporting comparison: previous common-slope models</summary>',
        '<p>These additive models allow different intercepts but impose equal group slopes. Their two-test Holm families per rating are separate from the six-test interaction-model families. The group-specific models above remain primary for these association questions.</p>',
        '<p><a href="data/analysis/common_slope_contrasts.csv">All common-slope sensitivity tests</a></p>',
        '<table><tr><th>Sample</th><th>Rating</th><th>SDT predictor</th><th>Common slope [95% CI]</th><th>Holm p</th></tr>')
      for(j in seq_len(nrow(common))) {
        q <- common[j,]
        html <- c(html,paste0('<tr><td>',if(q$sample=="primary")"QC-retained" else "All participants",'</td><td>',
          esc(q$outcome),'</td><td>',esc(q$contrast_id),'</td><td>',sprintf('%.3f [%.3f, %.3f]',q$estimate,q$lower,q$upper),
          '</td><td>',esc(p_text(q$p_holm)),'</td></tr>'))
      }
      html <- c(html,'</table></details>')
    }
    html <- c(html,paste0('<details><summary>All ',s$n_participants,' participants: corresponding sensitivity figure</summary>',
      '<img loading="lazy" src="',s$filename,'.png" alt="Sensitivity: ',esc(r$title),'"><p>',links(s$filename),'</p></details></article>'))
  }
  writeLines(c(html,'</body></html>'),file.path(out,"index.html"))
}
if(sys.nframe()==0L) {
  root <- dirname(normalizePath(sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])))
  args <- commandArgs(trailingOnly=TRUE);publish <- !"--stage-only" %in% args;args <- args[args!="--stage-only"]
  stopifnot(length(args)<=4)
  defaults <- file.path(root,c("analysis_outputs/semester2_2026_behavioural","plots/semester2_2026_behavioural","data/data_virus_all.csv","analysis_outputs/semester2_2026_qc"))
  defaults[seq_along(args)] <- args
  plot_dynamic_reliability_hypotheses(defaults[1],defaults[2],root,defaults[3],defaults[4],publish)
}
