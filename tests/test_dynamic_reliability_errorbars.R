# Vertical caps retain H1b's panel-relative proportions; horizontal intervals have no caps.
suppressPackageStartupMessages({library(ggplot2);library(patchwork);library(grid)})
script <- normalizePath(sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]))
source(file.path(dirname(dirname(script)),'R/dynamic_reliability_primary_plots.R'))
d <- data.frame(x=c(1,2),y=c(3,5),lo=c(2,4),hi=c(4,6))
vertical <- ggplot(d,aes(x,y))+geom_errorbar(aes(ymin=lo,ymax=hi),width=.07,linewidth=.85)
horizontal <- ggplot(d,aes(y,x))+geom_errorbar(aes(xmin=lo,xmax=hi),orientation='y',width=.12,linewidth=.75)
collect <- function(g,name) {
  here <- if(identical(g$name,name))list(g) else list()
  children <- c(as.list(g$grobs),as.list(g$children))
  c(here,unlist(lapply(children,collect,name=name),recursive=FALSE))
}
cases <- list(vertical,vertical+scale_x_continuous(limits=c(-100,100)),horizontal,vertical+coord_flip())
for(i in seq_along(cases)) {
  p <- cases[[i]]
  styled <- dpr_style_errorbars(p)
  before <- ggplot_build(p)$data[[1]];after <- ggplot_build(styled)$data[[1]]
  stopifnot(identical(before[setdiff(names(before),'linewidth')],after[setdiff(names(after),'linewidth')]),
    all(after$linewidth==.70),!inherits(p$layers[[1]]$geom,'GeomDprErrorbar'))
  for(size in list(c(12,6.75),c(4,3))) {
    file <- tempfile(fileext='.pdf');pdf(file,width=size[1],height=size[2])
    g <- ggplotGrob(styled);grid.draw(g)
    stems <- collect(g,'dpr.errorbar.stem')
    lower <- collect(g,'dpr.errorbar.cap.lower');upper <- collect(g,'dpr.errorbar.cap.upper')
    expected_caps <- if(i<=2)1L else 0L
    stopifnot(length(stems)==1,length(lower)==expected_caps,length(upper)==expected_caps,
      all(abs(stems[[1]]$gp$lwd-.70*72.27/25.4)<1e-10))
    for(cap in c(lower,upper)) {
      dx <- convertWidth(cap$x1-cap$x0,'npc',valueOnly=TRUE)
      dy <- convertHeight(cap$y1-cap$y0,'npc',valueOnly=TRUE)
      stopifnot(all(abs(sqrt(dx^2+dy^2)-.03)<1e-10),
        all(abs(cap$gp$lwd-.70*72.27/25.4)<1e-10))
    }
    dev.off();unlink(file)
  }
}
# Nested patchwork panels are all styled without changing the original layers.
p <- (vertical | horizontal)/(vertical | horizontal)
styled <- dpr_style_errorbars(p)
file <- tempfile(fileext="pdf");pdf(file,width=12,height=6.75)
g <- patchworkGrob(styled)
stopifnot(length(collect(g,'dpr.errorbar.stem'))==4,
  length(collect(g,'dpr.errorbar.cap.lower'))==2,length(collect(g,'dpr.errorbar.cap.upper'))==2,
  !inherits(vertical$layers[[1]]$geom,'GeomDprErrorbar'))
dev.off();unlink(file)
cat('PASS: panel-relative vertical H1b caps, uncapped horizontal intervals and lighter strokes across device sizes, ranges and nested layouts; interval coordinates and original layers unchanged.\n')

# Diamonds scale with panel width in both dimensions, preserving their shape.
p <- ggplot(d,aes(x,y))+geom_point(shape=23,fill="white",size=3.4,stroke=.9)
styled <- dpr_style_errorbars(p)
stopifnot(inherits(styled$layers[[1]]$geom,"GeomDprDiamond"),
  !inherits(p$layers[[1]]$geom,"GeomDprDiamond"),
  identical(ggplot_build(p)$data[[1]],ggplot_build(styled)$data[[1]]))
file <- tempfile(fileext="pdf");pdf(file,width=12,height=6.75)
g <- grid::gTree(data=data.frame(x=.5,y=.5,colour="black",fill="white",alpha=1),cl="dpr_scaled_diamonds")
values <- lapply(c(200,100),function(w) {
  pushViewport(viewport(width=unit(w,"mm"),height=unit(60,"mm")))
  z <- grid::makeContent(g)$children[[1]]$gp
  popViewport();c(z$fontsize,z$lwd)
})
stopifnot(all(abs(values[[1]]/values[[2]]-2)<1e-10))
dev.off();unlink(file)
cat("PASS: diamond size and outline scale together with panel width; centres and original layers unchanged.\n")

# Count horizontal subdivisions, not vertically stacked rows.
p <- vertical+geom_point(shape=23,fill="white",size=3.4,stroke=.9)
faceted <- p+facet_wrap(~x,nrow=1)
layouts <- list(single=p,columns=p|p,rows=p/p,grid=(p|p)/(p|p),
  mixed=p/(p|p),facets=faceted,nested=faceted|p,
  spanning=wrap_plots(p,p,p,design="AA\nBC"))
expected <- list(1,c(2,2),c(1,1),rep(2,4),c(1,2,2),c(2,2),c(4,4,2),c(1,2,2))
file <- tempfile(fileext="pdf");pdf(file,width=12,height=6.75)
for(i in seq_along(layouts)) {
  styled <- dpr_style_errorbars(layouts[[i]])
  g <- if(inherits(styled,"patchwork"))patchworkGrob(styled) else ggplotGrob(styled)
  diamonds <- collect(g,"dpr.diamonds")
  factors <- vapply(diamonds,function(z)z$horizontal_panels,numeric(1))
  stopifnot(identical(sort(unname(factors)),sort(as.numeric(expected[[i]]))))
  caps <- collect(g,"dpr.errorbar.cap.lower")
  widths <- vapply(caps,function(z)convertWidth(z$x1-z$x0,"npc",valueOnly=TRUE)[1],numeric(1))
  stopifnot(all(abs(sort(widths)-sort(.03*expected[[i]]))<1e-10))
}
# Halving a panel's width and applying a two-column correction preserves diamonds.
g <- grid::gTree(data=data.frame(x=.5,y=.5,colour="black",fill="white",alpha=1),cl="dpr_scaled_diamonds")
values <- lapply(1:2,function(n) {
  pushViewport(viewport(width=unit(200/n,"mm"),height=unit(60,"mm")))
  g$horizontal_panels <- n;z <- grid::makeContent(g)$children[[1]]$gp
  popViewport();c(z$fontsize,z$lwd)
})
stopifnot(all(abs(values[[1]]-values[[2]])<1e-10))
dev.off();unlink(file)
cat("PASS: horizontal-column compensation in facets, nested and spanning layouts; vertical rows do not increase size.\n")
