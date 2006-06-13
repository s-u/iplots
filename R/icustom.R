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
new.PPrimRect<-function(pplot,x1,y1,x2,y2,ids) {
	or=1;
	if(w<0 && h>=0) {or=4;}
	else if(w>=0 && h<0) {or=3;}
	else if(w>=0 && h>=0) {or=1;}
	else {or=2;}
	pp=.jnew("org/rosuda/ibase/toolkit/PPrimRectangle",as.integer(or));
	.jcall(pp,,"setBounds",as.double(x),as.double(y),as.double(w),as.double(h));
	.jcall(pp,,"setCaseIDs",.jarray(ids));
	.iplots.cp.addPP(pplot,pp);
	print(pp);
	return(pp);
}

.iplots.cp.addPP<-function(pplot,pp) {
	.jcall(pplot$obj,,"addPP",pp);
}

.iplots.cp.resetPP<-function(pplot) {
	.jcall(pplot$obj,,"resetPP");
}

iaxiscont<-function(pplot,n,or,valuerange,pixelcoord,...) {
	if(length(valuerange)!=2&&length(pixelcoord)!=2) return(NULL);
	if(or=="x") or=0;
	if(or=="y") or=1;
	axtype=0;
	svar=.jnull("org/rosuda/ibase/SVar");
	if (length(list(...))>0) {
		l<-list(...);
		svar=ivar.new("Var Name",l[[1]])$obj;
	}
	a=.jnew("org/rosuda/ibase/toolkit/Axis",svar,as.integer(or),as.integer(axtype));
	.jcall(a,,"setGeometry",as.integer(or),as.integer(pixelcoord[1]),as.integer(pixelcoord[2]));
	.jcall(a,"Z","setValueRange",as.double(valuerange[1]),as.double(valuerange[2]));
	.jcall(pplot$obj,,"setAxis",a);
	.jcall(a,,"addDepend",.jcast(pplot$obj,"org/rosuda/ibase/Dependent"));
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

# data(iris);
# dat=iris[1:2];
# dat[[2]]=factor(as.integer(runif(150,0,2)));levels(dat[[2]])<-c("M","W")
# dat[[1]]=dat[[1]]*13-30

# pplot=.iplots[[iplot.cur()]];

icustom.plot<-function(name,min.data.dim,param,construct) {
	
	f<-function(vars,...) {
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
	  if (length(list(...))>0) iplot.opt(...,plot=a)
		lastPlot<-.jcall(.iplots.fw,"Lorg/rosuda/ibase/plots/CustomCanvas;","newCustomplot",as.integer(vv),"");
		
		a<-iplot.new(lastPlot);
		.jcall(lastPlot,,"updateObjects");
		return(a);
	}
	
}

iagepyr.definition=list(name="Alterspyramide",min.data.dim=2,param=list(bin.width=5),
		construct=function(plot,width,height,data)	 {
			print("Bin im construct");
			bin.width=iagepyr.definition$param$bin.width;
		    side=unclass(data[[2]])
		    left=which(side==1)
		    right=which(side==2)
   			anchor=min(data[[1]])
    		bins=ceiling(max(data[[1]])/bin.width)
		    bin=ceiling((data[[1]]-anchor)/bin.width)
		   	largest.bin=max(table(bin))

##          v1<-c()
##			for(i in seq(1,length(left))) v1<-c(v1,data[[left[i],1]])
##			v2<-c()
##			for(i in seq(1,length(right))) v2<-c(v2,data[[right[i],1]])
##	   	    a.l=iaxiscont(plot, 1, "x", c(0, largest.bin), c(width/2, 10), v1)
## 	        a.r=iaxiscont(plot, 2, "x", c(0, largest.bin), c(width/2, width-10), v2)

   	   	    a.l=iaxiscont(plot, 1, "x", c(0, largest.bin), c(width/2, 10))
   	        a.r=iaxiscont(plot, 2, "x", c(0, largest.bin), c(width/2, width-10))
            a.y=iaxiscont(plot, 3, "y", c(0, bins), c(0,100))
            
			.iplots.cp.resetPP(plot);
            for (i in 1:bins) {
	        	ids.l=which(bin==i & side==1)
	        	ids.r=which(bin==i & side==2)
     	    	if (length(ids.l)>0)
	        		new.PPrimRect(plot, a.l(length(ids.l))-a.l(0), a.y(i+0.5)-a.y(i-0.5), a.l(0), a.y(i-0.5),id=ids.l)
	        	if (length(ids.r)>0)
	        		new.PPrimRect(plot, a.r(0), a.y(i-0.5), a.r(length(ids.r))-a.r(0), a.y(i+0.5)-a.y(i-0.5), id=ids.r);
	        		
		    }
		});

#iagepyr<-icustom.plot(iagepyr.definition);
