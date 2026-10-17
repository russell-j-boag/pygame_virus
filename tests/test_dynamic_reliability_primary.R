# Focused regressions for response-scale intervals, missing pairs and unavailable inference.
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(emmeans)})
script <- normalizePath(sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]))
root <- dirname(dirname(script));source(file.path(root,"analyse_dynamic_reliability_primary.R"))
grid <- expand.grid(group=GROUPS,stage=c("Manual pre","P1"))
spec <- dpr_specs(grid,list(c("P1","Manual pre")))
raw <- expand_grid(participant_id=1:6,stage=c("Manual pre","P1")) %>%
  mutate(group=ifelse(participant_id<=3,"CAL65","CAL90"),value=ifelse(stage=="P1",participant_id,0)) %>%
  filter(!(participant_id==1 & stage=="Manual pre"))
a <- dpr_participant(raw,"value",grid,spec,1);b <- dpr_participant(raw,"value",grid,spec,100)
for(name in c("estimate","se","lower","upper"))stopifnot(isTRUE(all.equal(a[[name]]*100,b[[name]])))
stopifnot(isTRUE(all.equal(a$p_raw,b$p_raw)),identical(a$n_participants,c(2L,3L,5L)),
  isTRUE(all.equal(a$estimate,c(2.5,5,2.5))))
for(i in 1:3) {
  t <- if(i==1)t.test(c(2,3)) else if(i==2)t.test(c(4,5,6)) else t.test(c(4,5,6),c(2,3))
  stopifnot(abs(a$lower[i]-t$conf.int[1])<1e-10,abs(a$upper[i]-t$conf.int[2])<1e-10)
}
fit <- lm(value~group*stage,raw);em <- emmeans(fit,~group*stage)
sp <- dpr_specs(as.data.frame(em),list(c("P1","Manual pre")))
unavailable <- dpr_table(em,sp,100,FALSE)
stopifnot(all(!unavailable$inference_available),all(is.na(unavailable$p_raw)),
  all(is.na(unavailable$lower)),all(is.na(unavailable$upper)),all(is.finite(unavailable$estimate)))
bad <- tibble(participant_id=1,group="CAL65",phase="FULL",advice=c("Correct","Incorrect"),
  n_answered=c(10,0),n_agree=c(8,0))
stopifnot(inherits(tryCatch({sdt_scores(bad);NULL},error=identity),"error"))
cat("PASS: percentage-point CI scaling; paired eligibility without zero imputation; independent t intervals; unavailable inference masks; empty SDT denominators rejected.\n")
# Known unequal slopes must remain unequal; the interaction is their signed difference.
x <- expand_grid(group=factor(GROUPS,GROUPS),centred_score=seq(-2,2,length.out=15)) %>%
  mutate(value=ifelse(group=="CAL65",1+centred_score,2+3*centred_score)+.15*cos(centred_score*4))
m <- lm(value~group*centred_score,x);V <- sandwich::vcovHC(m,type="HC3")
tr <- emtrends(m,~group,var="centred_score",vcov.=V,df=df.residual(m))
sp <- dpr_association_specs(as.data.frame(tr),"advice_bias","FULL")
z <- dpr_table(tr,sp)
stopifnot(max(abs(z$estimate-c(1,3,2)))<1e-8,identical(z$group,c(GROUPS,"CAL90 - CAL65")))
X <- model.matrix(m);bread <- solve(crossprod(X));omega <- residuals(m)^2/(1-hatvalues(m))^2
independent_V <- bread%*%crossprod(X,X*omega)%*%bread
stopifnot(abs(z$se[3]-sqrt(independent_V["groupCAL90:centred_score","groupCAL90:centred_score"]))<1e-10)
common <- lm(value~group+centred_score,x)
stopifnot(abs(coef(common)["centred_score"]-2)<1e-8)
cat("PASS: unequal group slopes, signed interaction contrast and independent HC3 variance; additive model remains a separate common-slope sensitivity.\n")
