.packageName <- "iplots"

library(SJava);
.First.lib <- function(lib, pkg) {
  cp<-paste(lib,pkg,"cont",sep=.Platform$file.sep)
  .JavaInit(list(classPath=c(cp)))
  fw<<-.JavaConstructor("Framework")
}

.di <- function() { .JavaInit(list(classPath=".")); fw<<-.JavaConstructor("Framework") }

ivar.new <- function (name,cont) {
  if(is.factor(cont)) {
    v<-.Java(fw,"newVar",name,as.character(cont))
    if (v>=0) {
      .Java(.Java(fw,"getVar",v),"categorize",TRUE)
    }
    return(v)
  }
  .Java(fw,"newVar",name,cont)
}

ivar.update <- function (vid,cont) { .Java(fw,"replaceVar",vid,cont); .Java(fw,"updateVars"); }

iplot.set <- function (which=iplot.next()) {
  a<-try(.iplots[[which<-as.integer(which)]])
  if (!is.list(a)) {
    warning("There is no such plot")
  } else {
    .iplot.current<<-a
    .iplot.curid<<-which
  }
}

ivar <- function (vid) { iset.var(vid) }

iplot.list <- function () {
  .iplots
}

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

print.iplot <- function(p) { cat("ID:",p$id," Name: \"",attr(p,"iname"),"\"\n",sep="") }

iplot.iPlot <- function (v1,v2) {
  lastPlot<<-.Java(fw,"newScatterplot",v1,v2)
  iplot.new(lastPlot)
}
iplot.iHist <- function (v1) { iplot.new(lastPlot<-.Java(fw,"newHistogram",v1)) }
iplot.iBar  <- function (v1) { iplot.new(lastPlot<-.Java(fw,"newBarchart",v1)) }

iplot <- function(v1,v2) {
   if ((is.vector(v1) || is.factor(v1)) && length(v1)>1) v1<-ivar.new(.Java(fw,"getNewTmpVar",as.character(deparse(substitute(v1)))),v1);
   if ((is.vector(v2) || is.factor(v2)) && length(v2)>1) v2<-ivar.new(.Java(fw,"getNewTmpVar",as.character(deparse(substitute(v2)))),v2);
   iplot.iPlot(v1,v2)
}

ibar <- function(v1) {
   if ((is.vector(v1) || is.factor(v1)) && length(v1)>1) v1<-ivar.new(.Java(fw,"getNewTmpVar",as.character(deparse(substitute(v1)))),v1);
   iplot.iBar(v1)
}

ihist <- function(v1) {
   if ((is.vector(v1) || is.factor(v1)) && length(v1)>1) v1<-ivar.new(.Java(fw,"getNewTmpVar",as.character(deparse(substitute(v1)))),v1);
   iplot.iHist(v1)
}

iplot.showVars <- function() { .Java(fw,"ivar.newFrame"); }

iplot.setXaxis <- function(ipl=lastPlot,x1,x2) { .Java(.Java(ipl,"getXAxis"),"setValueRange",x1,x2-x1); }
iplot.setYaxis <- function(ipl=lastPlot,x1,x2) { .Java(.Java(ipl,"getYAxis"),"setValueRange",x1,x2-x1); }
iplot.resetXaxis <- function(ipl=lastPlot) { .Java(.Java(ipl,"getXAxis"),"setDefaultRange"); }
iplot.resetYaxis <- function(ipl=lastPlot) { .Java(.Java(ipl,"getYAxis"),"setDefaultRange"); }
iplot.resetAxes <- function(ipl=lastPlot) { resetXaxis(ipl); resetYaxis(ipl); }
iplot.set <- function(ipl) { lastPlot<<-ipl; }

iset.select <- function(what,mark=TRUE) {
  m<-.Java(.Java(fw,"getCurrentSet"),"getMarker")
  if (is.logical(what)) {
    for(i in 1:length(what)) { if (what[i]) .Java(m,"set",as.integer(i-1),mark) }
  } else {
    for(i in what) { .Java(m,"set",as.integer(i-1),mark) }
  }
  .Java(fw,"updateMarker")
}

iset.df <- function(df) { ndf<-list(); for(i in names(df)) { ndf[[i]]<-ivar.new(i,df[[i]]) }; as.data.frame(ndf) }

iset.selected <- function() {
  v<-vector()
  m<-.Java(.Java(fw,"getCurrentSet"),"getMarker")
  numc<-.Java(m,"size")
  for(i in 1:numc) { if(.Java(m,"get",as.integer(i-1))<0) v<-c(v,i) }
  v;
}

iset.brush <- function(col) {
  if (is.numeric(col) && !is.integer(col)) col<-as.integer(col)
  if (is.factor(col)) col<-as.integer(as.integer(col)+1)
  .Java(fw,"setSecMark",col);
}

iset.selectAll <- function() { .Java(.Java(.Java(fw,"getCurrentSet"),"getMarker"),"selectAll",as.integer(1)); .Java(fw,"updateMarker"); }
iset.selectNone <- function() { .Java(.Java(.Java(fw,"getCurrentSet"),"getMarker"),"selectNone"); .Java(fw,"updateMarker"); }

iset.updateVars <- function() { .Java(fw,"updateVars"); }

iset.var <- function(vid) {
  vid<-as.integer(vid)
  if(.Java(fw,"varIsNum",vid)!=0) .Java(fw,"getDoubleContent",vid) else as.factor(.Java(fw,"getStringContent",vid))
}

iobj.new <- function(plot, type) {
  pm<-.Java(plot$obj,"getPlotManager")
  a<-list(obj=.JavaConstructor(type,pm),pm=pm,plot=plot)
  class(a)<-"iobj"
  plot$curobj<-a
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

iobj.rm <- function(obj) {
  if (is.numeric(obj)) obj<-iobj.get(obj)
  .Java(obj$pm,"rm",obj$obj)
  obj$plot$curobj<-.Java(obj$pm,"count")
  .Java(obj$pm,"update")
  rm(obj)
}

iobj.set <- function(o,...) {
  if (is.numeric(o)) o<-iobj.get(o)
  .Java(o$obj,"set",...)
  .Java(o$obj,"update")
}

iobj.color <- function(obj, fg=NULL, bg=NULL) {
  if (is.numeric(obj)) obj<-iobj.get(as.integer(obj))
  if (!is.null(fg)) .Java(obj$obj,"setDrawColor",.JavaConstructor("PlotColor",fg))
  if (!is.null(bg)) .Java(obj$obj,"setFillColor",.JavaConstructor("PlotColor",bg))
  .Java(obj$obj,"update")
}

iobj.cur <- function(plot=.iplot.current) {
  plot$curobj
}

print.iobj <- function(o) {
  cat(.Java(o$obj,"toString"),"\n")
}

ilines <- function(x,y) {
  if (!inherits(.iplot.current,"iplot")) {
    warning("There is no current plot")
  } else {
    pp<-iobj.new(.iplot.current,"PlotPolygon")
    .Java(pp$obj,"set",as.numeric(x),as.numeric(y))
    .Java(pp$obj,"update")
  }
}

