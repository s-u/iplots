.packageName <- "iplots"

library(SJava);
.First.lib <- function(lib, pkg) {
  cp<-paste(lib,pkg,"cont",sep=.Platform$file.sep)
  .JavaInit(list(classPath=c(cp)))
  fw<<-.JavaConstructor("Framework")
}

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

iplot.iPlot <- function (v1,v2) { lastPlot<<-.Java(fw,"newScatterplot",v1,v2) }
iplot.iHist <- function (v1) { lastPlot<-.Java(fw,"newHistogram",v1) }
iplot.iBar  <- function (v1) { lastPlot<-.Java(fw,"newBarchart",v1) }

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

iset.var <- function(vid) { if(.Java(fw,"varIsNum",vid)!=0) .Java(fw,"getDoubleContent",vid) else as.factor(.Java(fw,"getStringContent",vid)) }
