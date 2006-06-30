#==========================================================================
# iplots - interactive plots for R
# Package version: 1.0-2
#
# $Id$
# (C)Copyright 2003 Simon Urbanek
# Authors: Simnon Urbanek, Alex Gouberman, Tobias Wichtrey
#
#==========================================================================
# -- global variables (in the package environment)
# .iplots.fw     - framework object
# .iplots        - list of iplot objects
# .iplot.curid   - current plot ID
# .iplot.current - .iplots[[.iplot.curid]]
# .iplots.env is the environment of this package
#==========================================================================

#==========================================================================
# used objects:
#==========================================================================
#
# ivar
#   vid (int), name (char), obj (jobjRef [SVar])
#
# iplot
#   id (int), iname (char), obj (jobjRef)
#
# from rJava:
#
# jobjRef
#   jobj (int), jclass (char)
#

#==========================================================================
# initialization
#==========================================================================

# library initialization: Add "<iplots>/cont/iplots.jar" to classpath,
# initialize Java and create an instance on the Framework "glue" class
.First.lib <- function(lib, pkg) {
  require(rJava)
  dlp<-Sys.getenv("DYLD_LIBRARY_PATH")
  if (dlp!="") # for Mac OS X we need to remove X11 from lib-path
    Sys.putenv("DYLD_LIBRARY_PATH"=gsub("/usr/X11R6/lib","",dlp))
  cp<-paste(lib,pkg,"cont","iplots.jar",sep=.Platform$file.sep)

  .jinit(cp, silent=TRUE)

  ## variables from iplots 0.x-x
  ipv <- c(".iplots",".iplots.fw",".iset.selection",".isets",".iplot.curid",".iplot.current")
  mipv <- match(ipv, ls(envir=.GlobalEnv, all.names=TRUE))
  if (any(!is.na(mipv))) {
    rm(list=ipv[!is.na(mipv)], envir=.GlobalEnv) 
    warning("iPlots currently don't support saving of sessions. Data belonging to iPlots from your previous session will be discarded.")
  }

  ipe <- as.environment(match("package:iplots", search()))

  assign(".iplots.fw", if (nchar(Sys.getenv("NOAWT"))>0) NULL else .jnew("org/rosuda/iplots/Framework"), ipe)

   # we need to reset everything for sanity reasons
  assign(".iset.selection", vector(), ipe)
  assign(".isets", list(), ipe)
  .isets[[1]]<<-list()
  ipe$.iplots<-list()
  ipe$.iplot.curid<-1
  ipe$.iplot.current<-NULL
}

# helper function to identify a class in a strstr manner (not nice)
.class.strstr <- function(o, class) {
  if (!inherits(o, "jobjRef")) return(FALSE);
  if (length(grep(class, .jclass(o)))>0) TRUE else FALSE
}

#==========================================================================
# iSet API
#==========================================================================

# select a current dataset
iset.set <- function(which = iset.next()) {
  if (length(which)!=1)
    stop("You must specify exactly one dataset.")

  ci<-.jcall(.iplots.fw,"I","curSetId")+1
  .iset.save(ci)

  if (is.character(which)) {
    nso<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/SVarSet;","selectSet",which)
    if (is.null(nso))
      stop("There is no such iSet.")
    ci<-.jcall(.iplots.fw,"I","curSetId")+1
    .iplots<<-.isets[[ci]]$iplots
    .iplot.curid<<-.isets[[ci]]$iplot.cid
    .iplot.current<<-.isets[[ci]]$iplot.cur
    ci
  } else {
    if (!is.numeric(which))
      stop("The 'which' parameter mut be a name or an ID.")

    nso<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/SVarSet;","selectSet",as.integer(which-1))
    if (is.null(nso))
      stop("Invalid iSet ID.")
    ci<-.jcall(.iplots.fw,"I","curSetId")+1
    .iplots<<-.isets[[ci]]$iplots
    .iplot.curid<<-.isets[[ci]]$iplot.cid
    .iplot.current<<-.isets[[ci]]$iplot.cur
    .jstrVal(.jcall(.iplots.fw,"S","getSetName"))
  }
}

iset.cur <- function() {
  .jcall(.iplots.fw,"I","curSetId")+1
}

iset.next <- function(which=iset.cur()) {
  if (!is.numeric(which))
    stop("'which' must be a number.")
  w<-as.integer(which)-1
  t<-.jcall(.iplots.fw,"I","countSets")
  ((w+1) %% t)+1
}

iset.prev <- function(which=iset.cur()) {
  if (!is.numeric(which))
    stop("'which' must be a number.")
  w<-as.integer(which)-1
  t<-.jcall(.iplots.fw,"I","countSets")
  ((w-1) %% t)+1
}

iset.list <- function() {
  tot<-.jcall(.iplots.fw,"I","countSets")
  l<-vector()
  for (i in 1:tot)
    l[.jstrVal(.jcall(.iplots.fw,"S","getSetName",as.integer(i-1)))]<-i
  l
}

.iset.save <-function (ci=iset.cur()) {
  .isets[[ci]]<<-list(iplots=.iplots,iplot.cid<-.iplot.curid,iplot.cur<-.iplot.current)
}

iset.new <- function(name=NULL) {
  .iset.save()
  if (is.null(name)) name<-.jnull("java/lang/String")
  ci<-.jcall(.iplots.fw,"I","newSet",name)+1
  .iplots<<-list()
  .iplot.current<<-NULL
  .iplot.curid<<-0
  iset.set(ci)
  ci
}

#==========================================================================
# variables handling (iVar)
#==========================================================================

.ivar.check.length <- function(cont) {
  dsl <- .jcall(.iplots.fw,"I","getCurVarSetLength")
  if (dsl>0 && length(cont)!=dsl)
    cat("iSet and data length differ. Please observe the dialog box (it may be hidden by the R window).\n");
}

# create a new variable (undocumented!)
ivar.new <- function (name=deparse(substitute(cont)), cont) {
  if (!is.character(name) || length(name)>1)
    stop("variable name must be a single string")
  if(is.factor(cont) || is.character(cont)) {
    cont<-as.factor(cont)
    .ivar.check.length(cont)
    id<-.jcall(.iplots.fw,"I","newVar",name,as.integer(cont),as.character(levels(cont)))
    if (id==-2) stop("Operation canceled by user.")
    if (id==-3) {
      iset.new()
      id<-.jcall(.iplots.fw,"I","newVar",name,as.integer(cont),as.character(levels(cont)))
      if (id<0)
        stop("Unable to create an iVariable");
    }
    if (id>=0) {
      vobj<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/SVar;","getVar",id)
      .jcall(.jcast(vobj,"org/rosuda/ibase/SVarFixFact"),"V","createMissingsCat")
      .jcall(vobj,"V","categorize",TRUE)
      v<-list(vid=id,name=name,obj=vobj)
      class(v)<-"ivar"
      return(v)
    }
    return (NULL)
  }
  .ivar.check.length(cont)
  id<-.jcall(.iplots.fw,"I","newVar",name,cont)
  if (id==-2) stop("Operation canceled by user.")
  if (id==-3) {
    iset.new()
    id<-.jcall(.iplots.fw,"I","newVar",name,cont)
    if (id<0)
      stop("Unable to create an iVariable");
  }
  if (id>=0) {
    v<-list(vid=id,name=name,obj=.jcall(.iplots.fw,"Lorg/rosuda/ibase/SVar;","getVar",id))
    class(v)<-"ivar"
    return(v)
  }
  return(NULL);
}

ivar.data <- function(var) {
  if (!inherits(var,"ivar"))
    stop("parameter is not an iVariable.")
  vid<-as.integer(var$id)
  if(.jcall(.iplots.fw,"I","varIsNum",vid)!=0) .jcall(.iplots.fw,"[D","getDoubleContent",vid) else as.factor(.jcall(.iplots.fw,"[S","getStringContent",vid))
}

# re-format the variable name to prevent collision with existing variables
.ivar.valid.name <- function(name) {
  as.character(.jcall(.iplots.fw,"S","getNewTmpVar",as.character(name)))
}

# update contents of an existing variable (undocumented!)
ivar.update <- function (var, cont, batch = FALSE) {
  if (!inherits(var, "ivar"))
    stop("invalid variable")
  if (is.factor(cont))
    .jcall(.iplots.fw, "I", "replaceVar",
           var$vid, as.integer(unclass(cont)-1), levels(cont))
  else if (is.numeric(cont) || is.character(cont))
    .jcall(.iplots.fw, "I", "replaceVar", var$vid, cont)
  else {
      vf <- factor(as.character(cont))
      .jcall(.iplots.fw, "I", "replaceVar",
             var$vid, as.integer(unclass(vf)-1), levels(vf))
    }
  if (!batch)
    iset.updateVars()
}

#`<-.ivar` <- ivar.update

#==========================================================================
# iplot management functions
#==========================================================================

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
  .jcall(.jcall(plot$obj,"Ljava/awt/Frame;","getFrame"),"V","dispose")
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
.iplot.new <- function (plotObj) {
  if (!exists(".iplots")) {
    .iplots <<- list()
  }
  a<-list(id=(length(.iplots)+1),obj=plotObj)
  class(a)<-"iplot"
  attr(a,"iname")<-.jstrVal(.jcall(plotObj,"S","getTitle"))
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
print.iplot <- function(x, ...) { cat("ID:",x$id," Name: \"",attr(x,"iname"),"\"\n",sep="") }

# low-level plot calls
.iplot.setXaxis <- function(ipl,x1,x2) { .jcall(.jcall(ipl,"Lorg/rosuda/ibase/toolkit/Axis;","getXAxis"),"Z","setValueRange",x1,x2-x1); }
.iplot.setYaxis <- function(ipl,x1,x2) { .jcall(.jcall(ipl,"Lorg/rosuda/ibase/toolkit/Axis;","getYAxis"),"Z","setValueRange",x1,x2-x1); }

.iplot.iPlot <- function (x,y,...) {
  a<-.iplot.new(.jcall(.iplots.fw,"Lorg/rosuda/ibase/plots/ScatterCanvas;","newScatterplot",x$vid,y$vid))
  if (length(list(...))>0) iplot.opt(...,plot=a)
  a
}

.iplot.iHist <- function (var, ...) {
  a<-.iplot.new(lastPlot<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/plots/HistCanvas;","newHistogram",var$vid))
  if (length(list(...))>0) iplot.opt(...,plot=a)
  a
}

.iplot.iBar  <- function (var, ...) {
  a<-.iplot.new(lastPlot<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/plots/BarCanvas;","newBarchart",var$vid))
  if (length(list(...))>0) iplot.opt(...,plot=a)
  a
}

.iplot.iBox <- function (x, y, ...) {
 if(!inherits(x,"ivar")){
   vv<-vector()
    for (v in x) {
      if (inherits(v, "ivar"))
        vv<-c(vv,v$vid)
      else
        vv<-c(vv,v)
    }
    a<-.iplot.new(lastPlot<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/plots/ParallelAxesCanvas;","newBoxplot",as.integer(vv)))    
  }
  else {
    if (is.null(y)) a<-.iplot.new(lastPlot<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/plots/ParallelAxesCanvas;","newBoxplot",x$vid))
    else a<-.iplot.new(lastPlot<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/plots/ParallelAxesCanvas;","newBoxplot",x$vid,y$vid))
  }
  if (length(list(...))>0) iplot.opt(...,plot=a)
  a
}

.iplot.iHammock <- function (vars, ...) {
  vv<-vector()
  for (v in vars) {
    if (inherits(v, "ivar"))
      vv<-c(vv,v$vid)
    else
      vv<-c(vv,v)
  }
  if (length(vv)<2)
    stop("At least 2 valid variables are necessary for a hammock plot")
  a<-.iplot.new(lastPlot<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/plots/HamCanvas;","newHammock",as.integer(vv)))
  if (length(list(...))>0) iplot.opt(...,plot=a)
  a
}

.iplot.iPCP  <- function (vars, ...) {
  vv<-vector()
  for (v in vars) {
    if (inherits(v, "ivar"))
      vv<-c(vv,v$vid)
    else
      vv<-c(vv,v)
  }
  if (length(vv)<2)
    stop("At least 2 valid variables are necessary for a pcp plot")
  a<-.iplot.new(lastPlot<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/plots/ParallelAxesCanvas;","newPCP",as.integer(vv)))
  if (length(list(...))>0) iplot.opt(...,plot=a)
  a
}

.iplot.iMosaic <- function (vars, ...) {
  vv<-vector()
  for (v in vars) {
    if (inherits(v, "ivar"))
      vv<-c(vv,v$vid)
    else
      vv<-c(vv,v)
  }
  if (length(vv)<2)
    stop("At least 2 valid variables are necessary for a mosaic plot")
  a<-.iplot.new(lastPlot<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/plots/MosaicCanvas;","newMosaic",as.integer(vv)))
  if (length(list(...))>0) iplot.opt(...,plot=a)
  a
}

#==========================================================================
# user-level plot calls
#==========================================================================

imosaic <- function (vars, ...){
  len<-length(vars)
  vv <- vector()
  if (inherits(vars,"ivar"))
    vv <- vars$vid
  else {
    if (is.list(vars) && length(vars)>1) {
      for (v in 1:length(vars)) vv <- c(vv, ivar.new(.ivar.valid.name(names(vars)[v]), vars[,v])$vid)
    } else {
      vv <- ivar.new(.ivar.valid.name(deparse(substitute(vars)), vars[,v])$vid)
    }
  }
  if (length(vv)<1) stop("at least one valid variable is required")
  .iplot.iMosaic(vv,...)
}

iplot <- function(x, y=NULL, xlab=NULL, ylab=NULL, ...) {
  if (inherits(x,"ivar") || inherits(y,"ivar")) {
    lx <- if (inherits(x,"ivar")) .jcall(x$obj,"I","size") else length(x)
    ly <- if (inherits(y,"ivar")) .jcall(y$obj,"I","size") else length(y)
  } else {
    xlabel <- if (!missing(x))
        deparse(substitute(x))
    ylabel <- if (!missing(y))
        deparse(substitute(y))
    xy <- xy.coords(x, y, xlabel, ylabel, log="")
    xlab <- if (is.null(xlab))
        xy$xlab
    else xlab
    ylab <- if (is.null(ylab))
        xy$ylab
    else ylab
    nx<-xlab
    ny<-ylab
    if (!is.factor(x)) x<-xy$x
    if (!is.factor(y)) y<-xy$y
    lx<-length(x)
    ly<-length(y)
  }
  if (lx<2)
    stop("iplot coordinates must specify at least two points")
  if (!inherits(y,"ivar") && length(y)==1) ry<-rep(y,lx)
  if (lx!=ly)
    stop(paste("Incompatible vector lengths (",lx," and ",ly,").\nBoth vectors x and y must be of the same length.",sep=''))

  if (!inherits(x,"ivar"))
    x<-ivar.new(.ivar.valid.name(nx), x)
  if (!inherits(y,"ivar"))
    y<-ivar.new(.ivar.valid.name(ny), y)
  if (is.null(x)) stop("Invalid X variable")
  if (is.null(y)) stop("Invalid Y variable")
  .iplot.iPlot(x,y,...)
}

ibar <- function(var, ...) {
  len<-length(var)
  if (inherits(var,"ivar")) len<-.jcall(var$obj,"I","size")
  if (len<2)
    stop("ibar requires at least two data points")
   if ((is.vector(var) || is.factor(var)) && length(var)>1)
     var<-ivar.new(.ivar.valid.name(deparse(substitute(var))[1]), as.factor(var));
  .iplot.iBar(var, ...)
}

ihist <- function(var, ...) {
  len<-length(var)
  if (inherits(var,"ivar")) len<-.jcall(var$obj,"I","size")
  if (len<2)
    stop("ihist requires at least two data points")
#  if (!is.numeric(var))
#    stop("Variable must be numeric.")
  if (!inherits(var, "ivar"))
    var<-ivar.new(.ivar.valid.name(deparse(substitute(var))[1]), var);
  .iplot.iHist(var, ...)
}

ibox <- function(x, y=NULL, ...) {
  if(is.list(x)) {
    vv<-vector()
    i <- 1
    for (var in x) {
      if (length(var) > 1) {
        if (is.factor(var))
          var <- as.integer(var)
        varname <- names(x)[[i]]
        if (!is.null(varname))
  	  var <- ivar.new(.ivar.valid.name(varname), var)
  	else
 	  var <- ivar.new(.ivar.valid.name("boxplot"), var)
        if (inherits(var,"ivar"))  vv <- c(vv,var$vid)
      }
      i <- i+1
    }
    .iplot.iBox(vv,y, ...)
  }
  else {
    len<-length(x)
    if (inherits(x,"ivar")) len<-.jcall(x$obj,"I","size")
    if (len<2)
      stop("ibox requires at least two data points")
    x<-ivar.new(.ivar.valid.name(deparse(substitute(x))[1]), x);
    if (!is.null(y))
      y <- ivar.new(.ivar.valid.name(deparse(substitute(y))[1]), as.factor(y));
    .iplot.iBox(x, y, ...)
  }
}

ihammock <- function(vars, ...) {
  vv<-vector()
  for (var in vars) {
    var<-as.factor(var)
    if (length(var) > 1) {
      var <- ivar.new(.ivar.valid.name("hammock."), var)
      if (inherits(var,"ivar"))  vv <- c(vv,var$vid)
    }
  }
  if (length(vv)<2) stop("at least two valid variables are required")
  .iplot.iHammock(vv, ...)
}

ipcp <- function(vars, ...) {
  vv<-vector()
  i <- 1
  for (var in vars) {
    if (length(var) > 1) {
      varname <- names(vars)[[i]]
      if (!is.null(varname))
	      var <- ivar.new(.ivar.valid.name(varname), var)
	  else
	      var <- ivar.new(.ivar.valid.name("pcp"), var)
      if (inherits(var,"ivar"))  vv <- c(vv,var$vid)
    }
    i <- i+1
  }
  if (length(vv)<2) stop("at least two valid variables are required")
  .iplot.iPCP(vv, ...)
}

iplot.opt <- function(..., plot=iplot.cur()) {
  if (is.numeric(plot)) plot<-.iplots[[as.integer(plot)]]
  if (length(list(...))==0)
    .iplot.getPar(plot)
  else
    .iplot.opt(...,plot=plot)
}

.iplot.opt <- function(xlim=NULL, ylim=NULL, col=NULL, main=NULL, ..., plot=iplot.cur()) {
  repaint = FALSE

  optlist <- list(...)
  if(length(optlist)>0) {
    for(i in 1:length(optlist)) .jcall(plot$obj,"V","setOption",names(optlist)[i],optlist[[i]])
    repaint = TRUE
  }

  if (is.numeric(plot)) plot<-.iplots[[as.integer(plot)]]
  if (!is.null(xlim)) .iplot.setXaxis(plot$obj,xlim[1],xlim[2])
  if (!is.null(ylim)) .iplot.setYaxis(plot$obj,ylim[1],ylim[2])
  if (!is.null(main))
    .jcall(.jcall(plot$obj,"Ljava/awt/Frame;","getFrame"),"V","setTitle",as.character(main))
  if (!is.null(col)) iset.brush(col)
  if (!(is.null(xlim) && is.null(ylim))) {
    repaint = TRUE
  }

  if(repaint) {
    .jcall(plot$obj,"V","updateObjects")
    .jcall(plot$obj,"V","setUpdateRoot",as.integer(0))
    .jcall(plot$obj,"V","repaint")
  }

  # dirty temporary fix
  .jcall(plot$obj,"V","forcedFlush")
  invisible()

}

.iplot.getPar <- function(plot=.iplot.current) {
  p <- list()
#  if (.class.strstr(plot$obj,"ScatterCanvas")) {
#    p$xlim<-.Java(.Java(plot$obj,"getXAxis"),"getValueRange")
#    p$ylim<-.Java(.Java(plot$obj,"getYAxis"),"getValueRange")
#  }
  p
}

iplot.data <- function(id=NULL) {
  if (!is.null(id)) {
    v<-.jcall(.iplot.current$obj,"Lorg/rosuda/ibase/SVar;","getData",as.integer(id-1))
    if (!is.null(v)) {
      if(.jcall(.iplots.fw,"I","varIsNum",v)!=0)
        .jcall(.iplots.fw,"[D","getDoubleContent",v,evalArray=TRUE)
      else
        as.factor(.jcall(.iplots.fw,"[S","getStringContent",v))
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

iplot.showVars <- function() { .jcall(.iplots.fw,"V","ivar.newFrame"); }
iplot.resetXaxis <- function(ipl=lastPlot) { .jcall(.jcall(ipl,"Lorg/rosuda/ibase/toolkit/Axis;","getXAxis"),"V","setDefaultRange"); }
iplot.resetYaxis <- function(ipl=lastPlot) { .jcall(.jcall(ipl,"Lorg/rosuda/ibase/toolkit/Axis;","getYAxis"),"V","setDefaultRange"); }
iplot.resetAxes <- function(ipl=lastPlot) { resetXaxis(ipl); resetYaxis(ipl); }
iset.df <- function(df) { ndf<-list(); for(i in names(df)) { ndf[[i]]<-ivar.new(i,df[[i]]) }; }

# BaseCanvas methods

iplot.zoomIn<-function(x1,y1,x2,y2) {.jcall(.iplot.current$obj,,"performZoomIn",as.integer(x1),as.integer(y1),as.integer(x2),as.integer(y2));invisible();}
iplot.zoomOut<-function(x,y) {.jcall(.iplot.current$obj,,"performZoomOut",as.integer(x),as.integer(y));invisible();}
iplot.resetZoom<-function() {.jcall(.iplot.current$obj,"V","resetZoom");invisible();}
iplot.rotate<-function(i){.jcall(.iplot.current$obj,"V","rotate",as.integer(i));invisible();}
#//iplot.setMargins<-function(left,right,top,bottom) {
#//	v<-c(left,right,top,bottom);
#//	.jcall(.iplot.current$obj,"V","setDefaultMargins",.jarray(v));invisible();}

iplot.backend <- function(type = NULL) {
  getypes <- c("awt","swing","opengl")
  if (!is.null(type)) {
    ge <- pmatch(type[1],getypes)
    if (any(is.na(ge))) stop("invalid backend type")
    .jcall(.iplots.fw,"V","setGraphicsEngine",as.integer(ge-1))
  }
  getypes[.jcall(.iplots.fw,"I","getGraphicsEngine")+1]
}

iplot.setExtendedQuery <- function(plotID,str) {
	if(str==F)  .jcall(.iplots.fw,,"useExtQueryString",as.integer(plotID),F)
	else .jcall(.iplots.fw,,"setExtQueryString",as.integer(plotID),.jnew("java/lang/String",str))
	invisible();
}
iplot.setExtendedQuery<-function(str) {iplot.setExtendedQuery(.iplot.curid,str); invisible();}


#==========================================================================
# selection/highlighting API
#==========================================================================

iset.select <- function(what, mode="replace", mark=TRUE) {
  m<-.jcall(.jcall(.iplots.fw,"Lorg/rosuda/ibase/SVarSet;","getCurrentSet"),"Lorg/rosuda/ibase/SMarker;","getMarker")
  if (mode=="replace") iset.selectNone()
  if (mode=="intersect")
    error("I'm sorry, mode='intersect' is not yet supported in this version.")
  if (is.logical(what)) {
    for(i in 1:length(what)) { if (what[i]) .jcall(m,"V","set",as.integer(i-1),mark) }
  } else {
    for(i in what) { .jcall(m,"V","set",as.integer(i-1),mark) }
  }
  .jcall(.iplots.fw,"V","updateMarker")
  invisible()
}

iset.selected <- function() {
  m<-.jcall(.jcall(.iplots.fw,"Lorg/rosuda/ibase/SVarSet;","getCurrentSet"),"Lorg/rosuda/ibase/SMarker;","getMarker")
  .jcall(m,"[I","getSelectedIDs",evalArray=TRUE)+1
}

iset.selectAll <- function() { .jcall(.jcall(.jcall(.iplots.fw,"Lorg/rosuda/ibase/SVarSet;","getCurrentSet"),"Lorg/rosuda/ibase/SMarker;","getMarker"),"V","selectAll",as.integer(1)); .jcall(.iplots.fw,"V","updateMarker"); }
iset.selectNone <- function() { .jcall(.jcall(.jcall(.iplots.fw,"Lorg/rosuda/ibase/SVarSet;","getCurrentSet"),"Lorg/rosuda/ibase/SMarker;","getMarker"),"V","selectNone"); .jcall(.iplots.fw,"V","updateMarker"); }


#==========================================================================
# brushing API
#==========================================================================

# in paper: iset.color(color, what=iset.selected())
iset.col <- function(col=NULL) { iset.brush(col) }
iset.brush <- function(col=NULL) {
  if (is.null(col) || is.na(col)) col<-as.integer(c(0,0))
  if (is.numeric(col) && !is.integer(col)) col<-as.integer(col)
  if (is.factor(col)) col<-as.integer(as.integer(col)+1)
  .jcall(.iplots.fw,"V","setSecMark",col);
  # dirty temporary fix
  .jcall(.iplot.current$obj,"V","forcedFlush");
  invisible()
}

#** iset.cols(): return colors

iset.updateVars <- function() { .jcall(.iplots.fw,"V","updateVars"); }

#==========================================================================
# iobj API
#==========================================================================

# internal function - creates a new object of the Java-class <type>
.iobj.new <- function(plot, type) {
  pm<-.jcall(plot$obj,"Lorg/rosuda/ibase/toolkit/PlotManager;","getPlotManager")
  a<-list(obj=.jnew(paste("org/rosuda/ibase/toolkit",type,sep="/"),pm),pm=pm,plot=plot)
  class(a)<-"iobj"
  plot$curobj<-a
  # dirty temporary fix
  .jcall(plot$obj,"V","forcedFlush");
  a
}

iobj.list <- function(plot = .iplot.current) {
  pm<-.jcall(plot$obj,"Lorg/rosuda/ibase/toolkit/PlotManager;","getPlotManager")
  i<-.jcall(pm,"I","count")
  l<-list()
  if (i>0) {
    for(j in 1:i) {
      a<-list(obj=.jcall(pm,"Lorg/rosuda/ibase/toolkit/PlotObject;","get",as.integer(j-1)),pm=pm,plot=plot)
      class(a)<-"iobj"
      l[[j]]<-a
    }
  }
  l
}

iobj.get <- function(pos, plot = iplot.cur()) {
  if (is.numeric(plot)) plot<-.iplots[[plot]]
  if (!inherits(plot,"iplot"))
    stop("The specified plot is no iplot")
  pm<-.jcall(plot$obj,"Lorg/rosuda/ibase/toolkit/PlotManager;","getPlotManager")
  a<-list(obj=.jcall(pm,"Lorg/rosuda/ibase/toolkit/PlotObject;","get",as.integer(pos-1)),pm=pm,plot=plot)
  class(a)<-"iobj"
  a
}

iobj.cur <- function(plot = .iplot.current) {
  pm<-.jcall(plot$obj,"Lorg/rosuda/ibase/toolkit/PlotManager;","getPlotManager")
  oo<-.jcall(pm,"Lorg/rosuda/ibase/toolkit/PlotObject;","getCurrentObject");
  if (is.null(oo)) {
    NULL
  } else {
    a<-list(obj=oo,pm=pm,plot=plot)
    class(a)<-"iobj"
    a
  }
}

.iobj.equal <- function(a,b) {
  if (!inherits(a,"iobj") || !inherits(b,"iobj")) error("wrong types to compare")
  (!is.null(a) && !is.null(b)) && .jcall(a$obj,"Z","equals",.jcast(b$obj,"java/lang/Object")))
}

`==.iobj` <- .iobj.equal
`!=.iobj` <- function(a,b) !.iobj.equal(a,b)

iobj.next <- function(which=iobj.cur()) {
  l <- iobj.list()
  if (length(l) == 0) return(numeric())
  if (length(l) == 1) return(1)
  if (inherits(which, "iobj")) which <- base::which(unlist(lapply(l,.iobj.equal,which)))
  which <- if (length(which) != 1) 1 else which + 1
  which <- ((which-1) %% length(l)) + 1
  which
}

iobj.prev <- function(which=iobj.cur()) {
  l <- iobj.list()
  if (length(l) == 0) return(numeric())
  if (length(l) == 1) return(1)
  if (inherits(which, "iobj")) which <- base::which(unlist(lapply(l,.iobj.equal,which)))
  which <- if (length(which) != 1) 1 else which - 1
  which <- ((which-1) %% length(l)) + 1
  which
}

.iplot.get.by.pm <-function (which) { # get plot from the list by pm entry
  for (i in .iplots)
    if (i$pm == which) return(i)
  NULL;
}

iobj.rm <- function(which=iobj.cur()) {
  if (is.vector(which) && (inherits(which[1],"iobj") || is.numeric(which))) {
    for (i in which) iobj.rm(i)
    return()
  }
  if (is.numeric(which)) which<-iobj.get(which)
  .jcall(which$pm,"V","rm",.jcast(which$obj,"org/rosuda/ibase/toolkit/PlotObject"))
  which$plot$curobj<-.jcall(which$pm,"I","count")
  .jcall(which$pm,"V","update")
  rm(which)
}

.iobj.opt.PlotText <- function(o,x=NULL,y=NULL,txt=NULL,ax=NULL,ay=NULL) {
  if (!is.null(x) || !is.null(y)) {
    if (is.null(x)) x<-.jcall(o$obj,"[D","getX",evalArray=TRUE)
    if (is.null(y)) y<-.jcall(o$obj,"[D","getY",evalArray=TRUE)
    .jcall(o$obj,"V","set",as.real(x),as.real(y))
  }
  if (!is.null(ax))
    .jcall(o$obj,"V","setAX",as.real(ax))
  if (!is.null(ay))
    .jcall(o$obj,"V","setAY",as.real(ay))
  if (!is.null(txt)) {
    .jcall(o$obj,"V","set",as.character(txt))
  }
}

itext <- function(x, y=NULL, labels=seq(along=x), ax=NULL, ay=NULL, ..., plot=iplot.cur()) {
  if (is.numeric(plot)) plot<-.iplots[[plot]]
  if (!inherits(plot,"iplot")) {
    stop("There is no current plot")
  } else {
    pt<-.iobj.new(plot,"PlotText")
    c<-xy.coords(x,y,recycle=TRUE)
    iobj.opt(pt,x=c$x,y=c$y,ax=ax,ay=ay,txt=labels)
    if (length(list(...))>0)
      iobj.opt(pt,...)
    pt
  }
}

.iobj.opt.get.PlotText <- function(o) {
  return(list(x=.jcall(o$obj,"[D","getX",evalArray=TRUE),
              y=.jcall(o$obj,"[D","getY",evalArray=TRUE),
              ax=.jcall(o$obj,"[D","getAX",evalArray=TRUE),
              ay=.jcall(o$obj,"[D","getAY",evalArray=TRUE),
              txt=.jstrVal(.jcall(o$obj,"S","getText"))))
}

iobj.opt <- function(o=iobj.cur(),...) {
  if (is.list(o) && inherits(o[[1]],"iobj")) {
    r<-list()
    for (i in o) r<-c(r, iobj.opt(o=i, ...))
    r
  } else {
    if (length(list(...))==0)
      .iobj.opt.get(o)
    else
      .iobj.opt(o,...)
  }
}

.iobj.opt.get <- function(o) {
  if (!is.null(o)) {
    if (.class.strstr(o$obj, "PlotText"))
      .iobj.opt.get.PlotText(o)
  }
}

.iobj.opt.PlotPolygon <- function(o,x,y=NULL) {
  .co<-xy.coords(x,y)
  x<-.co$x
  y<-.co$y
  .jcall(o$obj,"V","set",as.numeric(x),as.numeric(y))
}

.iobj.opt <- function(o=iobj.cur(),...,col=NULL, fill=NULL, layer=NULL, reg=NULL, visible=NULL, coord=NULL, update=TRUE, a=NULL, b=NULL) {
  if (is.numeric(o)) o<-iobj.get(o)
  if (!is.null(layer)) .jcall(o$obj,"V","setLayer",as.integer(layer))
  if (length(list(...))>0) {
    if (.class.strstr(o$obj,"PlotText"))
      .iobj.opt.PlotText(o,...)
    else if (.class.strstr(o$obj,"PlotPolygon"))
      .iobj.opt.PlotPolygon(o,...)
    else
      .jcall(o$obj,"V","set",...)
  }
  if (!is.null(col)|| !is.null(fill)) .iobj.color(o,col,fill)
  if (!is.null(reg)||!is.null(a)||!is.null(b)) .iabline.set(a=a,b=b,reg=reg,obj=o)
  if (!is.null(visible))
    .jcall(o$obj,"V","setVisible",as.logical(visible))
  if (!is.null(coord) && length(coord)>0) {
    if (length(coord)==1)
      .jcall(o$obj,"V","setCoordinates",as.integer(coord))
    else
      .jcall(o$obj,"V","setCoordinates",as.integer(coord[1]),as.integer(coord[2]))
  }
  if (update) {
    .jcall(o$obj,"V","update")
    # dirty temporary fix
    .jcall(.iplot.current$obj,"V","forcedFlush")
  }
}

iobj.set <- function(which=iobj.next()) {
  if (is.numeric(which)) which<-iobj.get(which)
  if (is.null(which)) stop("opject doesn't exist")
  .jcall(which$pm,"V","setCurrentObject",which$obj);
}

.iobj.color <- function(obj=iobj.cur(), col=NULL, fill=NULL) {
  if (is.numeric(obj)) obj<-iobj.get(as.integer(obj))
  if (!is.null(col))
    if (is.na(col))
      .jcall(obj$obj,"V","setDrawColor",NULL)
    else
      .jcall(obj$obj,"V","setDrawColor",.jnew("org/rosuda/ibase/toolkit/PlotColor",col))

  if (!is.null(fill))
    if (is.na(fill))
      .jcall(obj$obj,"V","setFillColor",NULL)
    else
      .jcall(obj$obj,"V","setFillColor",.jnew("org/rosuda/ibase/toolkit/PlotColor",fill))

  .jcall(obj$obj,"V","update")
  # dirty temporary fix
  .jcall(.iplot.current$obj,"V","forcedFlush");
}

print.iobj <- function(x, ...) {
  cat(.jstrVal(x$obj),"\n")
}

ilines <- function(x,y=NULL,col=NULL,fill=NULL,visible=NULL,plot=iplot.cur()) {
  if (is.numeric(plot)) plot<-.iplots[[plot]]
  if (!inherits(plot,"iplot")) {
    stop("There is no current plot")
  } else {
    .co<-xy.coords(x,y)
    x<-.co$x
    y<-.co$y
    if (length(x)==1 && length(y)==1) {
      x<-c(x,x)
      y<-c(y,y)
    }
    pp<-.iobj.new(plot,"PlotPolygon")
    .jcall(pp$obj,"V","set",as.numeric(x),as.numeric(y))
    if (!is.null(col) || !is.null(fill))
      .iobj.color(pp,col,fill) # includes "update"
    else
      .jcall(pp$obj,"V","update")
    if (!is.null(visible))
      .jcall(pp$obj,"V","setVisible",visible)
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
  if (is.null(b)) b<-0
  list(a=a,b=b)
}

iabline <- function(a=NULL, b=NULL, reg=NULL, coef=NULL, ..., plot=iplot.cur()) {
  if (is.numeric(plot)) plot<-.iplots[[plot]]
  l<-.iabline.AB(a,b,reg,coef)
  a<-l$a
  b<-l$b
  if (is.null(plot) || !inherits(plot,"iplot")) {
    stop("There is no current plot")
  } else {
    ax<-.jcall(plot$obj,"Lorg/rosuda/ibase/toolkit/Axis;","getXAxis")
    if (is.null(ax)) {
      stop("The plot has no X axis")
    } else {
      r<-.jcall(ax,"[D","getValueRange",evalArray=TRUE)
      mi<-min(r)
      mx<-max(r)
      ilines(c(mi,mx),c(a+b*mi,a+b*mx),...,plot=plot)
    }
  }
}

.iabline.set <- function(...,obj=iobj.cur()) {
  l<-.iabline.AB(...)
  a<-l$a
  b<-l$b
  plot<-obj$plot;
  ax<-.jcall(plot$obj,"Lorg/rosuda/ibase/toolkit/Axis;","getXAxis")
  if (is.null(ax)) {
    stop("The plot has no X axis")
  } else {
    r<-.jcall(ax,"[D","getValueRange",evalArray=TRUE)
    mi<-min(r)
    mx<-max(r)
    iobj.opt(o=obj,c(mi,mx),c(a+b*mi,a+b*mx))
  }
  invisible()
}

ievent.wait <- function() {
  msg<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/NotifyMsg;","eventWait")
  if (!is.null(msg)) {
    o<-list(obj=msg,msg=.jcall(msg,"I","getMessageID"),cmd=.jcall(msg,"S","getCommand"),pars=.jcall(msg,"I","parCount"))
    class(o)<-"ievent"
    return(o)
  }
  return(NULL)
}

iset.sel.changed <- function (iset=iset.cur()) {
  a <- iset.selected()
  if (length(a)!=length(.iset.selection))
    b <- TRUE
  else
    b <- !(sum(a==.iset.selection)==length(a)) # this is stupid, but works :P
  if (b) .iset.selection <<- a
  b
}

.iDebug <- function(level=1) {
  .jcall(.iplots.fw,"V","setDebugLevel",as.integer(level))
}
