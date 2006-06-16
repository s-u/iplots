##################################
# added extended functions
# have to be ordered in list above
##################################


iplot.getGrDevID<-function() {
		if(iplot.grdevice=="AWT") return(0);
		if(iplot.grdevice=="SWING") return(1);
		if(iplot.grdevice=="OPENGL") return(2);
	}

## (x,y) top left corner of rectangle
## (w,h) width, height of rectangle
new.PPrimRect<-function(pplot,x,y,w,h,or,ids) {
	pp=.jnew("org/rosuda/ibase/toolkit/PPrimRectangle",as.integer(or));
	if (w<0) { x<-x+w; w<--w }
	if (h<0) { y<-y+h; h<--h }
	.jcall(pp,,"setBounds",as.double(x),as.double(y),as.double(abs(w)),as.double(abs(h)));
	.jcall(pp,,"setCaseIDs",.jarray(ids));
	.iplots.cp.addPP(pplot,pp);
	#print(pp);
	return(pp);
}

.iplots.cp.addPP<-function(pplot,pp) {
	.jcall(pplot$obj,,"addPP",pp);
}

.iplots.cp.resetPP<-function(pplot) {
	.jcall(pplot$obj,,"resetPP");
}

.iplots.cp.resetAxes<-function(pplot) {
	.jcall(pplot$obj,,"resetAxes");
}

.iplots.cp.setBorderColorSel<-function(pplot,pp) {
	.jcall(pplot$obj,,"setColors",pp);
}

## trl==translattion in orthogonal direction
iaxiscont<-function(pplot,n,or,valuerange,pxc,trl=0,drawAxis=T,...) {
	if(length(valuerange)!=2&&length(pxc)!=2) return(NULL);
	if(or=="x") or=0;
	if(or=="y") or=1;
	axtype=0;
	svar=.jnull("org/rosuda/ibase/SVar");
	if (length(list(...))>0) {
		l<-list(...);
		svar=ivar.new("Var Name",l[[1]])$obj;
	}
	a=.jnew("org/rosuda/ibase/toolkit/Axis",svar,as.integer(or),as.integer(axtype));
	.jcall(a,,"setGeometry",as.integer(or),as.integer(pxc[1]),as.integer(pxc[2]));
	.jcall(a,"Z","setValueRange",as.double(valuerange[1]),as.double(valuerange[2]));
	.jcall(pplot$obj,,"setAxis",a,as.integer(trl),drawAxis);
	.jcall(a,,"addDepend",.jcast(pplot$obj,"org/rosuda/ibase/Dependent"));
	print(a);
	gvp=function(pos) {
		ppos=.jcall(a,"I","getValuePos",as.double(pos));
		return(ppos);
	}
	return(gvp);
}

iaxiscat<-function(pplot,n,or,valuerange,pixelcoord) {
	if(length(valuerange)!=2&&length(pixelcoord)!=2) return(NULL);
	axtype=1;
	if(or=="x") or=0;
	if(or=="y") or=1;
	a=.jnew("org/rosuda/ibase/toolkit/Axis",.jnull("org/rosuda/ibase/SVar"),as.integer(or),as.integer(axtype));
	.jcall(a,,"setGeometry",as.integer(or),as.integer(pixelcoord[1]),as.integer(pixelcoord[2]));
	.jcall(a,"Z","setValueRange",as.double(valuerange[1]),as.double(valuerange[2]));
	.jcall(pplot$obj,,"setAxis",a);
	.jcall(a,,"addDepend",.jcast(pplot$obj,"org/rosuda/ibase/Dependent"));
	gcp=function(pos) {
		ppos=.jcall(a,"I","getCasePos",as.double(pos));
		return(ppos);
	}
	return(gcp);
}

iaxis<-function(pplot,n,or,valuerange,pixelcoord) {
	if(length(valuerange)!=2&&length(pixelcoord)!=2) return(NULL);
	if(or=="x") or=0;
	if(or=="y") or=1;
	a=.jnew("org/rosuda/ibase/toolkit/Axis",.jnull("org/rosuda/ibase/SVar"));
	.jcall(a,,"setGeometry",as.integer(or),as.integer(pixelcoord[1]),as.integer(pixelcoord[2]));
	.jcall(a,"Z","setValueRange",as.double(valuerange[1]),as.double(valuerange[2]));
	.jcall(pplot$obj,,"setAxis",a);
	.jcall(a,,"addDepend",.jcast(pplot$obj,"org/rosuda/ibase/Dependent"));
	gcp=function(pos) {
		if(.jcall(v,"Z","isCat")==T) {ppos=.jcall(a,"I","getCasePos",as.double(pos)); return(ppos);}
		else if(.jcall(v,"Z","isNum")==T) {ppos=.jcall(a,"I","getValuePos",as.double(pos)); return(ppos);}
	}
	return(gcp);
}

.ic.plots<-new.env(baseenv())

icustom.plot<-function(name,min.data.dim,param,construct) {
	.ic.plots[[name]]=list(plots=new.env(baseenv()),construct=construct)
	# real params: vars, ..., name
	f<-function() {
	  vv<-vector()
	  i <- 1
	  for (var in vars) {
    	if (length(var) > 1) {
	      varname <- names(vars)[[i]]
	      if (!is.null(varname))
		      var <- ivar.new(.ivar.valid.name(varname), var)
		  else
		      var <- ivar.new(.ivar.valid.name("customVar"), var)
	      if (inherits(var,"ivar"))  vv <- c(vv,var$vid)
	    }
	    i <- i+1
 	 }
 	 id<-1
 	 if (length(ls(.ic.plots[[name]]$plots))>0) id<-max(as.integer(ls(.ic.plots[[name]]$plots)))+1
		lastPlot<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/plots/CustomCanvas;","newCustomplot",as.integer(vv),
				paste('.ic.plots[["',name,'"]]',sep=''),as.character(id))
		
	 a<-iplot.new(lastPlot);
	 e<-new.env(parent=.GlobalEnv)
	 e$construct<-construct
	 environment(e$construct)<-e
	 e$plot<-a
	 e$data<-vars 
	 for (x in names(param)) e[[x]]<-param[[x]]
	 if (length(list(...))>0) #iplot.opt(...,plot=a)
	   { l<-list(...); for (i in names(l)) e[[i]]<-l[[i]] }
 	 .ic.plots[[name]]$plots[[as.character(id)]]<-e
	}
	formals(f)<-c(alist(vars=,...=),list(name=name))
	f
}

iagepyr<-icustom.plot(name="Alterspyramide",min.data.dim=2,param=list(bin.width=10),
		construct=function(width,height)	 {
		    side=unclass(data[[2]])
		    left=which(side==1)
		    right=which(side==2)
   			anchor=min(data[[1]])
    		bins=ceiling(max(data[[1]])/bin.width)
		    bin=ceiling((data[[1]]-anchor)/bin.width)
		   	l.largest.bin=max(table(bin));
		   	r.largest.bin=l.largest.bin;

			.iplots.cp.resetAxes(plot);
   	        a.r=iaxiscont(plot, 1, "x", c(0, r.largest.bin), c(width/2, width-10-width/2),height-10)
   	   	    a.l=iaxiscont(plot, 2, "x", c(0, l.largest.bin), c(width/2, 10-width/2),height-10)
            a.y=iaxiscont(plot, 3, "y", c(0, bins), c(height-10,-height+30),width/2)

			.iplots.cp.resetPP(plot);
            for (i in 1:bins) {
	        	ids.l=which(bin==i & side==1)-1
	        	ids.r=which(bin==i & side==2)-1
	        	
     	    	if (length(ids.l)>0) {
     	    		x=a.l(0);
     	    		y=a.y(i-0.5);
     	    		w=a.l(length(ids.l))-a.l(0);
     	    		h=a.y(i+0.5)-a.y(i-0.5);
					pp<-new.PPrimRect(plot,x,y,w,h, as.integer(3), id=ids.l);
				}
				if(length(ids.r)>0) {
					x=a.r(0);
					y=a.y(i-0.5);
					w=a.r(length(ids.r))-a.r(0);
					h=a.y(i+0.5)-a.y(i-0.5);
					pp<-new.PPrimRect(plot,x,y,w,h,as.integer(1),id=ids.r);
				}

		    }
		});
