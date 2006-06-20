## this environemnt is the root of all custom plots
## * each type of a custom plot has one entry in this environemnt, keyed
##   by the plot type name
## * each such entry is a list consisting of
##    plots     - an environment holding all instances
##    construct - custom plot call-back function
## * each instance is an environemnt containing at least following vars
##    plot      - plot object
##    construct - custom plot call-back function
##    data      - copy of the data passed to the instance
##    ...       - all initial parameters are assigned here
##    ...       - all call parameters are assigned here
.ic.plots<-new.env(baseenv())

## (x,y) top left corner of rectangle
## (w,h) width, height of rectangle
sobj.rect<-function(ids, x, y, w, h, orientation="up") {
  or <- pmatch(orientation[1],c("up","right","down","left")) - 1
  if (is.na(or)) stop("Invalid orientation.")
  pp=.jnew("org/rosuda/ibase/toolkit/PPrimRectangle",as.integer(or))
  if (w<0) { x<-x+w; w<--w }
  if (h<0) { y<-y+h; h<--h }
  .jcall(pp,,"setBounds",as.double(x),as.double(y),as.double(abs(w)),as.double(abs(h)))
  ## Java IDs are 0-based, R's are 1-based
  .jcall(pp,,"setCaseIDs",.jarray(as.integer(ids - 1)))
  .iplots.cp.addPP(plot,pp)
  pp
}

.iplots.cp.addPP<-function(pplot,pp) {
  .jcall(pplot$obj,,"addPP",pp)
}

sobj.reset <- function(plot=plot) {
  .jcall(plot$obj,,"resetPP")
}

iaxis.reset.all <- function(plot=plot) {
  .jcall(pplot$obj,,"resetAxes")
}

.iplots.cp.setBorderColorSel<-function(pplot,pp) {
  .jcall(pplot$obj,,"setColors",pp)
}

## trl==translation in orthogonal direction
iaxis <- function(type="cont", or="x", valuerange, pxc,
                  variable=NULL, trl=0, drawAxis=FALSE) {
  if (length(valuerange)!=2) stop("invalid value range specification")
  if (length(pxc)!=2)  stop("invalid graphics range specification")

  if (or=="x") or<-0 else
  if (or=="y") or<-1 else
  stop("invalid orientation")

  axtype <- as.integer(pmatch(type[1],c("continuous","discrete")) - 1)
  if (is.na(axtype)) stop("invalid axis type")
  if (!exists("plot")) stop("iaxis can be called from within a custom plot constructor only")
  svar <- .jnull("org/rosuda/ibase/SVar");
  if (!is.null(variable)) {
    svar <-
      if (inherits(variable, "ivar"))
        variable
      else
        ivar.new("Var Name",l[[1]])$obj
  }
  a <- .jnew("org/rosuda/ibase/toolkit/Axis",svar,as.integer(or),as.integer(axtype));
  .jcall(a,,"setGeometry",as.integer(or),as.integer(pxc[1]),as.integer(pxc[2]-pxc[1]));
  .jcall(a,"Z","setValueRange",as.double(valuerange[1]),as.double(valuerange[2]-valuerange[1]));
  .jcall(plot$obj,,"setAxis",a,as.integer(trl),drawAxis);
  .jcall(a,,"addDepend",.jcast(plot$obj,"org/rosuda/ibase/Dependent"));
  if (axtype == 0)
    function(pos) .jcall(a,"I","getValuePos",as.double(pos))
  else
    function(pos) .jcall(a,"I","getCasePos",as.double(pos))
}

icustom.plot<-function(name, construct, min.data.dim=1, param=list()) {
  .ic.plots[[name]] <- list(plots=new.env(baseenv()),construct=construct)
  ## real params: vars, ..., name
  f<-function() {
    vv<-vector()
    for (i in 1:length(vars)) { 
      varname <- names(vars)[i]
      var <- vars[i]
      if (!is.null(varname))
        var <- ivar.new(.ivar.valid.name(varname), var)
      else
        var <- ivar.new(.ivar.valid.name("customVar"), var)
      if (inherits(var,"ivar"))  vv <- c(vv,var$vid)
    }
    ## get ID for the next plot or "1" if the environemnt is empty
    id<-1
    if (length(ls(.ic.plots[[name]]$plots))>0)
      id<-max(as.integer(ls(.ic.plots[[name]]$plots)))+1
    
    lastPlot<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/plots/CustomCanvas;","newCustomplot", as.integer(vv),
                     paste('.ic.plots[["',name,'"]]',sep=''), as.character(id))
    
    a<-iplot.new(lastPlot);
    e<-new.env(parent=.GlobalEnv)
    environment(construct) <- e
    e$construct<-construct
    e$.construct <- function(...) { iaxis.reset.all(); sobj.reset(); construct(...) }
    environment(e$.construct) <- e
    e$plot<-a
    e$data<-vars 
    for (x in names(param)) e[[x]]<-param[[x]]
    if (length(list(...))>0)
      { l<-list(...); for (i in names(l)) e[[i]] <- l[[i]] }
    .ic.plots[[name]]$plots[[as.character(id)]] < -e
  }
  formals(f)<-c(alist(vars=,...=),list(name=name))
  f
}

iagepyr <- icustom.plot(name="Alterspyramide",
                        min.data.dim=2,
                        param=list(bin.width=10),
    construct=function(width,height) {
      side <- unclass(data[[2]])
      left <- which(side==1)
      right <- which(side==2)
      anchor <- min(data[[1]])
      bins <- ceiling(max(data[[1]])/bin.width)
      bin <- ceiling((data[[1]]-anchor)/bin.width)
      l.largest.bin <- max(table(bin));
      r.largest.bin <- l.largest.bin;
      
      iaxis.reset.all()
      a.r <- iaxis("cont", "x", c(0, r.largest.bin), c(width/2, width-10), height-10)
      a.l <- iaxis("cont", "x", c(0, l.largest.bin), c(width/2, 10), height-10)
      a.y <- iaxis("cont", "y", c(0, bins), c(height-10,20), width/2)
      
      sobj.reset()
      for (i in 1:bins) {
        ids.l <- which(bin==i & side==1)
        ids.r <- which(bin==i & side==2)
        
        if (length(ids.l)>0)
          sobj.rect(ids.l, a.l(0), a.y(i-0.5),
                    a.l(length(ids.l))-a.l(0),
                    a.y(i+0.5)-a.y(i-0.5),
                    "left")
        if(length(ids.r)>0)
          sobj.rect(ids.r, a.r(0), a.y(i-0.5),
                    a.r(length(ids.r))-a.r(0),
                    a.y(i+0.5)-a.y(i-0.5),
                    "right")
      }
    })
