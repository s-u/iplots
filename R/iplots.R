.packageName <- "iplots"

# iplots alpha
# Version: $Id$
# (C)Copyright 2003 Simon Urbanek
#
# -- global variables:
# .iplots.fw     - framework object
# .iplots        - list of iplot objects
# .iplot.curid   - current plot ID
# .iplot.current - .iplots[[.iplot.curid]]

# we need SJava for all the .Java commands (should we use "require"?)
library(SJava);

# library initialization: Add "<iplots>/cont/iplots.jar" to classpath,
# initialize Java and create an instance on the Framework "glue" class
.First.lib <- function(lib, pkg) {
  dlp<-Sys.getenv("DYLD_LIBRARY_PATH")
  if (dlp!="") # for Mac OS X we need to remove X11 from lib-path
    Sys.putenv("DYLD_LIBRARY_PATH"=sub("/usr/X11R6/lib","",dlp))
  cp<-paste(lib,pkg,"cont","iplots.jar",sep=.Platform$file.sep)
  .JavaInit(list(classPath=c(cp)))
  .iplots.fw<<-.JavaConstructor("Framework")
  .iset.selection<<-vector()
  .isets<<-list()
  .isets[[1]]<<-list()
}

# "Debug Initialize" - like .First.lib but for debugging purposes only
# main use is to "source" this file and use .di() for initialization.
.di <- function() {
  dlp<-Sys.getenv("DYLD_LIBRARY_PATH")
  if (dlp!="") # for Mac OS X we need to remove X11 from lib-path
    Sys.putenv("DYLD_LIBRARY_PATH"=sub("/usr/X11R6/lib","",dlp))
  .JavaInit(list(classPath=".")) # debug version uses "." as classpath
  .iplots.fw<<-.JavaConstructor("Framework")
  .iset.selection<<-vector()
  .isets<<-list()
  .isets[[1]]<<-list()
}

# iSet API

# select a current dataset
iset.set <- function(which = iset.next()) {
  if (length(which)!=1)
    stop("You must specify exactly one dataset.")

  ci<-.Java(.iplots.fw,"curSetId")+1
  .iset.save(ci)
  
  if (is.character(which)) {
    nso<-.Java(.iplots.fw,"selectSet",which)
    if (is.null(nso))
      stop("Therre is no such iSet.")
    ci<-.Java(.iplots.fw,"curSetId")+1
    .iplots<<-.isets[[ci]]$iplots
    .iplot.curid<<-.isets[[ci]]$iplot.cid
    .iplot.current<<-.isets[[ci]]$iplot.cur
    ci
  } else {
    if (!is.numeric(which))
      stop("The 'which' parameter mut be a name or an ID.")

    nso<-.Java(.iplots.fw,"selectSet",as.integer(which-1))
    if (is.null(nso))
      stop("Invalid iSet ID.")
    ci<-.Java(.iplots.fw,"curSetId")+1
    .iplots<<-.isets[[ci]]$iplots
    .iplot.curid<<-.isets[[ci]]$iplot.cid
    .iplot.current<<-.isets[[ci]]$iplot.cur
    .Java(.iplots.fw,"getSetName")
  }
}

iset.cur <- function() {
  .Java(.iplots.fw,"curSetId")+1
}

iset.next <- function(which=iset.cur()) {
  if (!is.numeric(which))
    stop("'which' must be a number.")
  w<-as.integer(which)-1
  t<-.Java(.iplots.fw,"countSets")
  ((w+1) %% t)+1
}

iset.next <- function(which=iset.cur()) {
  if (!is.numeric(which))
    stop("'which' must be a number.")
  w<-as.integer(which)-1
  t<-.Java(.iplots.fw,"countSets")
  ((w-1) %% t)+1
}

iset.list <- function() {
  tot<-.Java(.iplots.fw,"countSets")
  l<-vector()
  for (i in 1:tot)
    l[.Java(.iplots.fw,"getSetName",as.integer(i-1))]<-i
  l
}

.iset.save <-function (ci=iset.cur()) {
  .isets[[ci]]<<-list(iplots=.iplots,iplot.cid<-.iplot.curid,iplot.cur<-.iplot.current)
}

iset.new <- function(name=NULL) {
  .iset.save()
  ci<-.Java(.iplots.fw,"newSet",name)+1
  .iplots<<-list()
  .iplot.current<<-NULL
  .iplot.curid<<-0
  iset.set(ci)
  ci
}

# create a new variable (undocunmented!)
ivar.new <- function (name,cont) {
  if(is.factor(cont)) {
    v<-.Java(.iplots.fw,"newVar",name,as.character(cont))
    if (v>=0) {
      .Java(.Java(.iplots.fw,"getVar",v),"categorize",TRUE)
    }
    return(v)
  }
  .Java(.iplots.fw,"newVar",name,cont)
}

# update contents of an existing variable (undocumented!)
ivar.update <- function (vid,cont) { .Java(.iplots.fw,"replaceVar",vid,cont); .Java(.iplots.fw,"updateVars"); }

# return contents of a variable (alias for iset.var)
ivar <- function (vid) { iset.var(vid) }

#============================
# iplot management functions
#============================

iplot.set <- function (which=iplot.next()) {
  a<-try(.iplots[[which<-as.integer(which)]])
  if (!is.list(a)) {
    warning("There is no such plot")
  } else {
    .iplot.current<<-a
    .iplot.curid<<-which
  }
}

iplot.list <- function () {
  if (exists(".iplots")) .iplots else list()
}

.iplot.close <- function(plot) {
  .Java(.Java(plot$obj,"getFrame"),"dispose")
}

iplot.off <- function(plot=iplot.cur()) {
  if (length(.iplots)==0) return()

  j<-list()
  k <- 1
  for (i in 1:length(.iplots))
    if (i!=plot) {
      j[[k]]<-.iplots[[i]]
      k<-k+1
    } else
      .iplot.close(.iplots[[i]])
  .iplots <<- j
  if (length(j)==0) {
    .iplot.current<<-NULL
    .iplot.curid<<-0
  } else
    iplot.set(iplot.prev())
  invisible()
}

# this function should not be called directly by the user
# is creates a new R iplot-object for an existing Java plot object
iplot.new <- function (plotObj) {
  if (!exists(".iplots")) {
    .iplots <<- list()
  }
  a<-list(id=(length(.iplots)+1),obj=plotObj)
  class(a)<-"iplot"
  attr(a,"iname")<-.Java(plotObj,"getTitle")
  .iplot.current <<- .iplots[[.iplot.curid <<- (length(.iplots)+1)]] <<- a
}

iplot.cur <- function() { .iplot.curid }
iplot.next <- function(which = iplot.cur()) {
 (which%%length(.iplots))+1
}
iplot.prev <- function(which = iplot.cur()) {
 ((which-2)%%length(.iplots))+1
}

# generic print for i[lots
print.iplot <- function(p) { cat("ID:",p$id," Name: \"",attr(p,"iname"),"\"\n",sep="") }

# low-level plot calls
.iplot.setXaxis <- function(ipl,x1,x2) { .Java(.Java(ipl,"getXAxis"),"setValueRange",x1,x2-x1); }
.iplot.setYaxis <- function(ipl,x1,x2) { .Java(.Java(ipl,"getYAxis"),"setValueRange",x1,x2-x1); }

.iplot.iPlot <- function (x,y,...) {
  a<-iplot.new(.Java(.iplots.fw,"newScatterplot",x,y))
  iplot.opt(...,plot=a)
  a
}

.iplot.iHist <- function (v1) { iplot.new(lastPlot<-.Java(.iplots.fw,"newHistogram",v1)) }

.iplot.iBar  <- function (v1) { iplot.new(lastPlot<-.Java(.iplots.fw,"newBarchart",v1)) }

# user-level plot calls

iplot <- function(x,y,...) {
   if ((is.vector(x) || is.factor(x)) && length(x)>1) x<-ivar.new(.Java(.iplots.fw,"getNewTmpVar",as.character(deparse(substitute(x)))),x)
   if ((is.vector(y) || is.factor(y)) && length(y)>1) y<-ivar.new(.Java(.iplots.fw,"getNewTmpVar",as.character(deparse(substitute(y)))),y)
   .iplot.iPlot(x,y,...)
}

ibar <- function(v1) {
   if ((is.vector(v1) || is.factor(v1)) && length(v1)>1) v1<-ivar.new(.Java(.iplots.fw,"getNewTmpVar",as.character(deparse(substitute(v1)))),v1);
   .iplot.iBar(v1)
}

ihist <- function(v1) {
   if ((is.vector(v1) || is.factor(v1)) && length(v1)>1) v1<-ivar.new(.Java(.iplots.fw,"getNewTmpVar",as.character(deparse(substitute(v1)))),v1);
   .iplot.iHist(v1)
}

iplot.opt <- function(xlim=NULL, ylim=NULL, col=NULL, plot=.iplot.current) {
  if (!is.null(xlim)) .iplot.setXaxis(plot$obj,xlim[1],xlim[2])
  if (!is.null(ylim)) .iplot.setYaxis(plot$obj,ylim[1],ylim[2])
  if (!is.null(col)) iset.brush(col)
  if (!(is.null(xlim) && is.null(ylim))) {
    if (plot$obj[[2]]=="ScatterCanvas") .Java(plot$obj,"updatePoints")
    .Java(plot$obj,"setUpdateRoot",as.integer(0))
    .Java(plot$obj,"repaint")
  }
  # dirty temporary fix
  .Java(plot$obj,"forcedFlush");
}

.iplot.getPar <- function(plot=.iplot.current) {
  p <- list()
  if (plot$obj[[2]]=="ScatterCanvas") {
    p$xlim<-.Java(.Java(plot$obj,"getXAxis"),"getValueRange")
    p$ylim<-.Java(.Java(plot$obj,"getYAxis"),"getValueRange")
  }
  p
}

iplot.data <- function(id=NULL) {
  if (!is.null(id)) {
    v<-.Java(.iplot.current$obj,"getData",as.integer(id-1))
    if (!is.null(v)) {
      if(.Java(.iplots.fw,"varIsNum",v)!=0)
        .Java(.iplots.fw,"getDoubleContent",v)
      else
        as.factor(.Java(.iplots.fw,"getStringContent",v))
    } else {
      NULL
    }
  } else {
    i<-1
    l<-list()
    while (!is.null(a<-iplot.data(i))) {
      l[[i]]<-a
      if (i==1) names(l)<-"x"
      if (i==2) names(l)<-c("x","y")
      i<-i+1
    }
    l
  }
}

# old API functions

iplot.showVars <- function() { .Java(.iplots.fw,"ivar.newFrame"); }
iplot.resetXaxis <- function(ipl=lastPlot) { .Java(.Java(ipl,"getXAxis"),"setDefaultRange"); }
iplot.resetYaxis <- function(ipl=lastPlot) { .Java(.Java(ipl,"getYAxis"),"setDefaultRange"); }
iplot.resetAxes <- function(ipl=lastPlot) { resetXaxis(ipl); resetYaxis(ipl); }
iset.df <- function(df) { ndf<-list(); for(i in names(df)) { ndf[[i]]<-ivar.new(i,df[[i]]) }; as.data.frame(ndf) }

# selection/highlighting API

iset.select <- function(what, mode="replace", mark=TRUE) {
  m<-.Java(.Java(.iplots.fw,"getCurrentSet"),"getMarker")
  if (is.logical(what)) {
    for(i in 1:length(what)) { if (what[i]) .Java(m,"set",as.integer(i-1),mark) }
  } else {
    for(i in what) { .Java(m,"set",as.integer(i-1),mark) }
  }
  .Java(.iplots.fw,"updateMarker")
}

iset.selected <- function() {
  v<-vector()
  m<-.Java(.Java(.iplots.fw,"getCurrentSet"),"getMarker")
  .Java(m,"getSelectedIDs")+1
#  numc<-.Java(m,"size")
#  for(i in 1:numc) { if(.Java(m,"get",as.integer(i-1))<0) v<-c(v,i) }
#  v;
}

iset.selectAll <- function() { .Java(.Java(.Java(.iplots.fw,"getCurrentSet"),"getMarker"),"selectAll",as.integer(1)); .Java(.iplots.fw,"updateMarker"); }
iset.selectNone <- function() { .Java(.Java(.Java(.iplots.fw,"getCurrentSet"),"getMarker"),"selectNone"); .Java(.iplots.fw,"updateMarker"); }

# brushing API

# in paper: iset.color(color, what=iset.selected())
iset.col <- function(...) { iset.brush(...) }
iset.brush <- function(col) {
  if (is.null(col) || is.na(col)) col<-as.integer(c(0,0))
  if (is.numeric(col) && !is.integer(col)) col<-as.integer(col)
  if (is.factor(col)) col<-as.integer(as.integer(col)+1)
  .Java(.iplots.fw,"setSecMark",col);
  # dirty temporary fix
  .Java(.iplot.current$obj,"forcedFlush");
  invisible()
}

#** iset.cols(): return colors

iset.updateVars <- function() { .Java(.iplots.fw,"updateVars"); }

iset.var <- function(vid) {
  vid<-as.integer(vid)
  if(.Java(.iplots.fw,"varIsNum",vid)!=0) .Java(.iplots.fw,"getDoubleContent",vid) else as.factor(.Java(.iplots.fw,"getStringContent",vid))
}

# iobj API

# internal function - creates a new object of the Java-class <type>
iobj.new <- function(plot, type) {
  pm<-.Java(plot$obj,"getPlotManager")
  a<-list(obj=.JavaConstructor(type,pm),pm=pm,plot=plot)
  class(a)<-"iobj"
  plot$curobj<-a
  # dirty temporary fix
  .Java(plot$obj,"forcedFlush");
  a
}

iobj.list <- function(plot = .iplot.current) {
  pm<-.Java(plot$obj,"getPlotManager")
  i<-.Java(pm,"count")
  l<-list()
  if (i>0) {
    for(j in 1:i) {
      a<-list(obj=.Java(pm,"get",as.integer(j-1)),pm=pm,plot=plot)
      class(a)<-"iobj"
      l[[j]]<-a
    }
  } 
  l
}

iobj.get <- function(pos, plot = .iplot.current) {
  pm<-.Java(plot$obj,"getPlotManager")
  a<-list(obj=.Java(pm,"get",as.integer(pos-1)),pm=pm,plot=plot)
  class(a)<-"iobj"
  a
}

iobj.cur <- function(plot = .iplot.current) {
  pm<-.Java(plot$obj,"getPlotManager")
  oo<-.Java(pm,"getCurrentObject");
  if (is.null(oo)) {
    NULL
  } else {
    a<-list(obj=oo,pm=pm,plot=plot)
    class(a)<-"iobj"
    a
  }
}

.iplot.get.by.pm <-function (which) { # get plot from the list by pm entry
  for (i in .iplots)
    if (i$pm == which) return(i)
  NULL;
}

iobj.rm <- function(obj=iobj.cur()) {
  if (is.numeric(obj)) obj<-iobj.get(obj)
  .Java(obj$pm,"rm",obj$obj)
  obj$plot$curobj<-.Java(obj$pm,"count")
  .Java(obj$pm,"update")
  rm(obj)
}

.iobj.opt.PlotText <- function(o,x=NULL,y=NULL,txt=NULL,ax=NULL,ay=NULL) {
  if (!is.null(x) || !is.null(y) || !is.null(ax) || !is.null(ay)) {
    if (is.null(x)) x<-.Java(o$obj,"getX")
    if (is.null(y)) y<-.Java(o$obj,"getY")
    if (length(x)==1) x<-c(x,x)
    if (length(y)==1) y<-c(y,y)
    if (is.null(ax) && is.null(ay))
      .Java(o$obj,"set",as.real(x),as.real(y))
    else {
      if (is.null(ax)) ax<-.Java(o$obj,"getAX")
      if (is.null(ay)) ay<-.Java(o$obj,"getAY")
      if (length(ax)==1) ax<-c(ax,ax)
      if (length(ay)==1) ay<-c(ay,ay)
      .Java(o$obj,"set",as.real(x),as.real(y),as.real(ax),as.real(ay))
    }
  } else {
    if (!is.null(txt)) {
      if (length(txt)==1) txt<-c(txt,txt)
      .Java(o$obj,"set",as.character(txt))
    }
  }
}

itext <- function(x, y=NULL, labels=seq(along=x), ax=NULL, ay=NULL, ...) {
  if (!inherits(.iplot.current,"iplot")) {
    stop("There is no current plot")
  } else {
    pt<-iobj.new(.iplot.current,"PlotText")
    c<-xy.coords(x,y,recycle=TRUE)
    iobj.opt(pt,x=c$x,y=c$y,ax=ax,ay=ay,txt=labels)
    if (length(list(...))>0)
      iobj.opt(pt,...)
    pt
  }
}

.iobj.opt.get.PlotText <- function(o) {
  return(list(x=.Java(o$obj,"getX"),y=.Java(o$obj,"getY"),
              ax=.Java(o$obj,"getAX"),ay=.Java(o$obj,"getAY"),
              txt=.Java(o$obj,"getText")))
}

iobj.opt <- function(o=iobj.cur(),...) {
  if (length(list(...))==0)
    .iobj.opt.get(o)
  else
    .iobj.opt(o,...)
}

.iobj.opt.get <- function(o) {
  if (!is.null(o)) {
    cl<-o$obj[[2]]
    if (cl=="PlotText")
      .iobj.opt.get.PlotText(o)
  }
}

.iobj.opt.PlotPolygon <- function(o,x,y=NULL) {
  .co<-xy.coords(x,y)
  x<-.co$x
  y<-.co$y
  .Java(o$obj,"set",x,y)
}

.iobj.opt <- function(o=iobj.cur(),...,col=NULL, fill=NULL, layer=NULL, reg=NULL, visible=NULL, coord=NULL, update=TRUE, a=NULL, b=NULL) {
  if (is.numeric(o)) o<-iobj.get(o)
  if (!is.null(layer)) .Java(o$obj,"setLayer",as.integer(layer))
  if (length(list(...))>0) {
    if (o$obj[[2]]=="PlotText")
      .iobj.opt.PlotText(o,...)
    else if (o$obj[[2]]=="PlotPolygon")
      .iobj.opt.PlotPolygon(o,...)
    else
      .Java(o$obj,"set",...)
  }
  if (!is.null(col)|| !is.null(fill)) .iobj.color(o,col,fill)
  if (!is.null(reg)||!is.null(a)||!is.null(b)) .iabline.set(a=a,b=b,reg=reg,obj=o)
  if (!is.null(visible)) .Java(o$obj,"setVisible",visible);
  if (!is.null(coord) && length(coord)>0) {
    if (length(coord)==1)
      .Java(o$obj,"setCoordinates",as.integer(coord))
    else
      .Java(o$obj,"setCoordinates",as.integer(coord[1]),as.integer(coord[2]))
  }
  if (update) {
    .Java(o$obj,"update")
    # dirty temporary fix
    .Java(.iplot.current$obj,"forcedFlush")
  }
}

iobj.set <- function(obj) {
  if (is.numeric(obj)) obj<-iobj.get(obj)
  if (is.null(obj)) stop("opject doesn't exist")
  .Java(obj$pm,"setCurrentObject",obj$obj);
}

.iobj.color <- function(obj=iobj.cur(), col=NULL, fill=NULL) {
  if (is.numeric(obj)) obj<-iobj.get(as.integer(obj))
  if (!is.null(col))
    if (is.na(col))
      .Java(obj$obj,"setDrawColor",NULL)
    else
      .Java(obj$obj,"setDrawColor",.JavaConstructor("PlotColor",col))

  if (!is.null(fill))
    if (is.na(fill))
      .Java(obj$obj,"setFillColor",NULL)
    else
      .Java(obj$obj,"setFillColor",.JavaConstructor("PlotColor",fill))      
  
  .Java(obj$obj,"update")
  # dirty temporary fix
  .Java(.iplot.current$obj,"forcedFlush");
}

print.iobj <- function(o) {
  cat(.Java(o$obj,"toString"),"\n")
}

ilines <- function(x,y=NULL,col=NULL,fill=NULL,visible=NULL) {
  if (!inherits(.iplot.current,"iplot")) {
    stop("There is no current plot")
  } else {
    .co<-xy.coords(x,y)
    x<-.co$x
    y<-.co$y
    pp<-iobj.new(.iplot.current,"PlotPolygon")
    .Java(pp$obj,"set",as.numeric(x),as.numeric(y))
    if (!is.null(col) || !is.null(fill))
      .iobj.color(pp,col,fill) # includes "update"
    else
      .Java(pp$obj,"update")
    if (!is.null(visible))
      .Java(pp$obj,"setVisible",visible)
    pp
  }
}

.iabline.AB <- function(a=NULL, b=NULL, reg=NULL, coef=NULL, ...) {
  if (!is.null(reg)) a<-reg
  if (!is.null(a) && is.list(a)) {
    t <- as.vector(coefficients(a))
    if (length(t) == 1) {
      a<-0
      b<-t
    } else {
      a<-t[1]
      b<-t[2]
    }
  }
  if (!is.null(coef)) {
    a <- coef[1]
    b <- coef[2]
  }
  list(a=a,b=b)
}

iabline <- function(a=NULL, b=NULL, reg=NULL, coef=NULL, ...) {
  l<-.iabline.AB(a,b,reg,coef)
  a<-l$a
  b<-l$b
  if (is.null(.iplot.current) || !inherits(.iplot.current,"iplot")) {
    stop("There is no current plot")
  } else {
    ax<-.Java(.iplot.current$obj,"getXAxis")
    if (is.null(ax)) {
      stop("The plot has no X axis")
    } else {
      r<-.Java(ax,"getValueRange")
      mi<-min(r)
      mx<-max(r)
      ilines(c(mi,mx),c(a+b*mi,a+b*mx),...)
    }
  }
}

.iabline.set <- function(...,obj=iobj.cur()) {
  l<-.iabline.AB(...)
  a<-l$a
  b<-l$b
  plot<-obj$plot;
  ax<-.Java(.iplot.current$obj,"getXAxis")
  if (is.null(ax)) {
    stop("The plot has no X axis")
  } else {
    r<-.Java(ax,"getValueRange")
    mi<-min(r)
    mx<-max(r)
    iobj.opt(obj,c(mi,mx),c(a+b*mi,a+b*mx))
  }
}

ievent.wait <- function() {
  msg<-.Java(.iplots.fw,"eventWait")
  if (!is.null(msg)) {
    o<-list(obj=msg,msg=.Java(msg,"getMessageID"),cmd=.Java(msg,"getCommand"),pars=.Java(msg,"parCount"))
    class(o)<-"ievent"
    return(o)
  }
  return(NULL)
}

iset.sel.changed <- function (iset=iset.cur(),...) {
  a <- iset.selected()
  if (length(a)!=length(.iset.selection))
    b <- TRUE
  else
    b <- !(sum(a==.iset.selection)==length(a)) # this is stupid, but works :P
  if (b) .iset.selection <<- a
  b
}

#------ DEBUG/TEST code

.db <- function() {
  iplot(rnorm(200),rnorm(200))
  ilines(1:200/200*6-3,dnorm(1:200/200*6-3)*4)
}

# event loop demo from DSC-2003 iPlots paper
.a <- function() {
  x<-rnorm(100)
  y<-rnorm(100)
  iplot(x, y)
  iabline(lm(y ~ x), col="black")
  iabline(0, 0, col="marked", visible=FALSE)
  while (!is.null(ievent.wait())) {
    if (iset.sel.changed()) {
      s <- iset.selected()
      if (length(s)>0)
        iobj.opt(reg=lm(y[s] ~ x[s]), visible=TRUE)
      else
        iobj.opt(visible=FALSE)
    }
  }
}
