# Revised primary analysis -> staged, validated figures -> canonical report.
# Rscript plot_time_pressure_hypotheses.R [analysis_dir] [plot_dir] [combined_csv] [--stage-only]
primary_publish_report <- function(stage,plot_base) {
  stopifnot(file.exists(file.path(stage,"VALIDATED.txt")))
  out <- file.path(plot_base,"presentation")
  previous <- NULL
  if(dir.exists(out)) {
    archive <- file.path(plot_base,"archive");dir.create(archive,showWarnings=FALSE)
    previous <- tempfile(paste0("presentation_",format(Sys.time(),"%Y%m%d_%H%M%S"),"_"),tmpdir=archive)
    stopifnot(file.rename(out,previous))
  }
  if(!file.rename(stage,out)) {
    if(!is.null(previous))file.rename(previous,out)
    stop("Unable to publish report; preceding report restored")
  }
  message("Primary hypothesis report: ",file.path(out,"index.html"));invisible(out)
}
plot_time_pressure_hypotheses <- function(base,plot_base,root,input=NULL,publish=TRUE) {
  suppressPackageStartupMessages({library(dplyr);library(tidyr);library(readr);library(ggplot2);library(patchwork)})
  root <- normalizePath(root);base <- normalizePath(base)
  dir.create(plot_base,recursive=TRUE,showWarnings=FALSE);plot_base <- normalizePath(plot_base)
  if(is.null(input))input <- file.path(root,"data/semester2_2026_data/data_virus_all.csv")
  input <- normalizePath(input)
  source(file.path(root,"analyse_time_pressure_primary.R"),local=TRUE)
  source(file.path(root,"R/time_pressure_primary_plots.R"),local=TRUE)
  primary_dir <- file.path(base,"primary")
  stopifnot(file.exists(file.path(primary_dir,"COMPLETE.txt")))
  hashes <- read_csv(file.path(primary_dir,"input_checksums.csv"),show_col_types=FALSE)
  stopifnot(input %in% hashes$path)
  if(!all(file.exists(hashes$path)) || !identical(unname(tools::md5sum(hashes$path)),hashes$md5))
    stop("Primary analysis inputs or specification changed. Run analyse_time_pressure_primary.R before plotting.")
  bundle <- readRDS(file.path(primary_dir,"bundle.rds"))
  association_names <- c("H7_advice_bias_loglinear","H7_advice_dprime_loglinear")
  association_paths <- file.path(primary_dir,"models",paste0(association_names,".rds"))
  association_models <- setNames(lapply(association_paths,readRDS),association_names)
  registry <- read_csv(file.path(root,"config/hypothesis_figures.csv"),show_col_types=FALSE,na="")
  registry$contrast_ids[is.na(registry$contrast_ids)] <- ""
  catalog <- registry %>% distinct(order,filename,title,role,hypothesis,legacy_labels,notes) %>% arrange(order)
  stopifnot(!anyDuplicated(catalog$filename),identical(catalog$order,as.numeric(seq_len(nrow(catalog)))))
  stage <- tempfile(".presentation_build_",tmpdir=plot_base);dir.create(file.path(stage,"data"),recursive=TRUE)
  message("Building staged report: ",stage)
  built <- primary_plot_build(bundle,registry,stage,association_models);figures <- lapply(built$figures,report_style_errorbars)
  for(stem in names(figures))for(ext in c("pdf","png"))ggsave(file.path(stage,paste0(stem,".",ext)),figures[[stem]],
    width=12,height=6.75,dpi=300,bg="white",device=if(ext=="pdf")cairo_pdf else "png")
  # Historical measures are also regenerated from the included cohort; no old
  # full-cohort panel is copied into the current collection.
  dir.create(file.path(stage,"data/primary"))
  files <- list.files(primary_dir,pattern="\\.(csv|txt)$",full.names=TRUE)
  stopifnot(all(file.copy(files,file.path(stage,"data/primary"))))
  write_csv(catalog,file.path(stage,"figure_catalog.csv"))
  write_csv(registry,file.path(stage,"hypothesis_mapping.csv"))
  write_csv(registry,file.path(stage,"data/figure_contract.csv"))
  # Merge the exact standalone PDFs for the same included cohort.
  unite <- Sys.which("pdfunite");stopifnot(nzchar(unite))
  for(name in c("core_hypotheses","all_hypotheses")) {
    selected <- if(name=="core_hypotheses")filter(catalog,role=="Core")$filename else catalog$filename
    status <- system2(unite,shQuote(c(file.path(stage,paste0(selected,".pdf")),file.path(stage,paste0(name,".pdf")))))
    stopifnot(status==0)
  }
  esc <- function(x)gsub('"',"&quot;",gsub("<","&lt;",gsub("&","&amp;",x,fixed=TRUE),fixed=TRUE),fixed=TRUE)
  diag <- bundle$diagnostics;bad <- filter(diag,!inference_available)
  primary <- filter(bundle$results,analysis=="mixed_model",correction %in% c("none","loglinear"))
  html <- c('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Time-pressure primary hypotheses</title>',
    '<style>body{font:17px/1.55 system-ui,sans-serif;color:#25323d;max-width:1250px;margin:32px auto;padding:0 24px}h1{font-size:34px}h2{margin-top:44px}h3{font-size:23px}img{width:100%;border:1px solid #dce4e9}article{margin:32px 0 48px}a{color:#1976a3}.meta{font-size:14px;color:#566372}nav,.method{padding:18px;background:#f0f5f7}details{margin:15px 0}summary{cursor:pointer;font-weight:600}table{border-collapse:collapse;font-size:13px;width:100%}td,th{padding:7px;border-bottom:1px solid #dde5e9;text-align:left}.warn{background:#fff1db;padding:15px}code{font-size:13px}</style></head><body>',
    '<h1>Time-pressure primary hypotheses</h1>',
    sprintf('<p>%d participants; eight primary questions; ten main figures. Accuracy and task discrimination are presented together, followed by response bias, advice discrimination, trust and adjustment.</p>',nrow(bundle$data$cohort)),
    '<p><a href="core_hypotheses.pdf">Main figures (10-page PDF)</a> | <a href="all_hypotheses.pdf">All figures (18-page PDF)</a> | <a href="hypothesis_mapping.csv">Hypothesis crosswalk</a> | <a href="data/primary/contrasts.csv">All tests and sensitivities</a> | <a href="data/primary/methods.txt">Methods</a></p>',
    sprintf('<div class="method"><p>This primary analysis framework was agreed on 17 October 2026 after inspecting earlier results. It is not preregistered. Of %d source participants, %s are excluded under the reviewed QC criteria, leaving <strong>%d participants in every current figure and analysis</strong>. Earlier full-cohort reports remain archived. <a href="data/primary/cohort_selection.csv">Participant selection and reasons</a>.</p>',
      nrow(bundle$data$cohort_selection),paste(sprintf('%03d',bundle$data$exclusions$participant_id),collapse=', '),nrow(bundle$data$cohort)),
    '<p>Recorded ID 083 was reused for two runs. Run 20261015_125827 retains analysis ID 083; run 20261015_130522 is analysis ID 084. Both remain included as separate participants and retain the allocation actually administered under recorded ID 083. This identity correction is not an exclusion.</p>',
    '<p>Open diamonds show mixed-model estimates and pointwise 95% confidence intervals; faint points show raw participants. All tests are two-sided. Holm correction is applied separately within each hypothesis and outcome, including all H2 subfigures together. Stars: *** p&lt;.001, ** p&lt;.01, * p&lt;.05; ns: p&ge;.05. An unavailable test is not an ns result. The tables include exact raw and adjusted p-values, contrast weights, diagnostics and independently labelled sensitivity results.</p>',
    '<p>H7 allows both within-person and between-person trust slopes to vary by allocation group. For each SDT outcome, four group slopes and two direct slope differences share a six-test Holm family. Differences are HP95/LP65 minus HP65/LP95. A significant slope in one group alone does not establish a difference between groups. This 18 October 2026 update followed inspection of the common-slope plots and is retrospective. The preceding additive models remain as common-slope sensitivities with separate two-test Holm families per outcome and correction.</p>',
    '<p>H7 scatter plots show fitted lines and shaded pointwise 95% confidence bands from the same adjusted mixed models as the reported group slopes. Each allocation pattern is shown over its observed trust range. Within-person panels predict HP minus LP; between-person panels average HP and LP. Block position is held at its observed mean within each pattern and pressure. Bands describe uncertainty in the population mean or mean change, rather than individual prediction intervals.</p>',
    '<p>All-trial accuracy includes timeouts. SDT describes answered trials: advice -c measures acceptance and advice d-prime measures selectivity; task -c measures BLACK preference and task d-prime measures final stimulus discrimination. These summarise the same decisions, not independent replications. Confidence-interval overlap is not a formal comparison; a nonsignificant gap is not evidence of equivalence.</p></div>',
    paste0('<nav>',paste0('<a href="#',unique(na.omit(catalog$hypothesis)),'">',unique(na.omit(catalog$hypothesis)),'</a>',collapse=' &nbsp; | &nbsp; '),' &nbsp; | &nbsp; <a href="#supporting">Supporting and historical figures</a></nav>'))
  if(nrow(bad))html <- c(html,paste0('<p class="warn">Primary inference is unavailable for ',esc(paste(bad$model,collapse=", ")),'. See <a href="data/primary/model_diagnostics.csv">model diagnostics</a>; sensitivities are separate analyses.</p>'))
  binary_diag <- filter(diag,!is.na(conditional_cell_dispersion))
  html <- c(html,paste0('<details><summary>Model adequacy and sensitivity checks</summary><p>All fits use participant random intercepts. Numerical convergence does not establish adequate fit. Conditional cell dispersion: ',
    paste(sprintf('%s %.2f',binary_diag$model,binary_diag$conditional_cell_dispersion),collapse='; '),
    '. Participant contrasts or cluster-robust sensitivities retain a separate inferential status. Residual plots and complete model summaries are retained in the analysis output directory.</p><p><a href="data/primary/model_diagnostics.csv">Fit diagnostics</a> | <a href="data/model_participant_comparison.csv">Model versus participant sensitivity</a> | <a href="data/correction_sensitivity.csv">SDT correction sensitivity</a></p></details>'))
  last <- ""
  for(i in seq_len(nrow(catalog))) {
    r <- catalog[i,];stem <- r$filename
    if(r$role=="Core" && r$hypothesis!=last){html <- c(html,paste0('<h2 id="',r$hypothesis,'">',r$hypothesis,'</h2>'));last <- r$hypothesis}
    if(i==11)html <- c(html,'<h2 id="supporting">Supporting and historical figures</h2><p>All views use the included cohort. Historical measures retain their original test definitions and separate families, with models refitted after exclusions.</p>')
    links <- paste0('<a href="',stem,'.pdf">PDF</a> | <a href="',stem,'.png">PNG</a>')
    for(kind in c("data","participants","annotations","fits"))if(file.exists(file.path(stage,"data",paste0(stem,"_",kind,".csv"))))
      links <- paste0(links,' | <a href="data/',stem,'_',kind,'.csv">',kind,'</a>')
    html <- c(html,paste0('<article><h3>',esc(r$title),'</h3><p class="meta">',esc(r$legacy_labels),' &rarr; ',esc(r$role),'</p><p>',esc(r$notes),
      '</p><img loading="lazy" src="',stem,'.png" alt="',esc(r$title),'"><p>',links,'</p>'))
    if(r$role=="Core") {
      tests <- filter(built$annotations,filename==stem)
      html <- c(html,'<details><summary>Primary tests shown in this figure</summary><table><tr><th>Outcome</th><th>Comparison</th><th>Estimate [95% CI]</th><th>Holm p</th><th>Family size</th></tr>')
      for(j in seq_len(nrow(tests))) {
        q <- tests[j,];value <- if(q$outcome=="correct_rt")sprintf('%.3f [%.3f, %.3f] ratio',q$ratio,q$ratio_lower,q$ratio_upper) else sprintf('%.3f [%.3f, %.3f]',q$estimate,q$lower,q$upper)
        html <- c(html,paste0('<tr><td>',esc(q$outcome),'</td><td>',esc(q$contrast_id),'</td><td>',value,'</td><td>',primary_p_text(q$p_holm),'</td><td>',q$family_size,'</td></tr>'))
      }
      html <- c(html,'</table></details>')
      if(r$hypothesis=="H7") {
        common <- filter(bundle$results,hypothesis=="H7",analysis=="common_slope_sensitivity",correction=="loglinear")
        html <- c(html,'<details><summary>Common-slope sensitivity comparison</summary><p>These preceding additive-model slopes use separate two-test Holm families per SDT outcome. They do not replace the group-specific primary tests. <a href="data/primary/common_slope_contrasts.csv">All common-slope results and SDT corrections</a>.</p><table><tr><th>Outcome</th><th>Component</th><th>Common slope [95% CI]</th><th>Holm p</th></tr>')
        for(j in seq_len(nrow(common))) {
          q <- common[j,]
          html <- c(html,paste0('<tr><td>',esc(q$outcome),'</td><td>',esc(q$contrast_id),'</td><td>',
            sprintf('%.3f [%.3f, %.3f]',q$estimate,q$lower,q$upper),'</td><td>',primary_p_text(q$p_holm),'</td></tr>'))
        }
        html <- c(html,'</table></details>')
      }
    }
    html <- c(html,'</article>')
  }
  html <- c(html,'</body></html>');writeLines(html,file.path(stage,"index.html"))
  consumed <- c(hashes$path,file.path(root,c("plot_time_pressure_hypotheses.R","R/time_pressure_primary_plots.R","tests/validate_time_pressure_primary.R")),
    association_paths,
    file.path(primary_dir,c("bundle.rds","contrasts.csv","estimates.rds","contrast_audit.rds","model_diagnostics.csv")))
  write_csv(tibble(path=normalizePath(unique(consumed)),md5=unname(tools::md5sum(unique(consumed)))),file.path(stage,"data/input_checksums.csv"))
  # Validate scientific contract and output integrity before publication.
  log <- file.path(stage,"validation.log")
  status <- system2(file.path(R.home("bin"),"Rscript"),shQuote(c(file.path(root,"tests/validate_time_pressure_primary.R"),base,stage)),stdout=log,stderr=log)
  if(status!=0)stop("Validation failed; staged report preserved at ",stage,"; see validation.log")
  message(paste(readLines(log),collapse="\n"))
  artifacts <- list.files(stage,recursive=TRUE,full.names=TRUE)
  write_csv(tibble(path=substring(artifacts,nchar(stage)+2),bytes=file.info(artifacts)$size,md5=unname(tools::md5sum(artifacts))),file.path(stage,"data/artifact_manifest.csv"))
  writeLines(paste("Validated",format(Sys.time(),tz="UTC")),file.path(stage,"VALIDATED.txt"))
  if(publish)primary_publish_report(stage,plot_base) else message("Staged report ready for visual review: ",stage)
  invisible(if(publish)file.path(plot_base,"presentation") else stage)
}
if(sys.nframe()==0) {
  script <- sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]);root <- dirname(normalizePath(script))
  args <- commandArgs(trailingOnly=TRUE);stopifnot(length(args)<=4)
  if(length(args)==4)stopifnot(args[4]=="--stage-only")
  plot_time_pressure_hypotheses(if(length(args)>=1)args[1] else file.path(root,"analysis_outputs/semester2_2026_behavioural"),
    if(length(args)>=2)args[2] else file.path(root,"plots/semester2_2026_behavioural"),root,if(length(args)>=3)args[3] else NULL,publish=length(args)<4)
}
