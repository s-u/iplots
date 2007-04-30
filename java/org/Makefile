# Makefile for iPlots
# $Id: Makefile 2681 2006-11-30 18:38:08Z urbanek $
#
# Note that some projects may be better compiled using xcodebuild

JAPIURL:=http://java.sun.com/j2se/1.4.2/docs/api
ifeq ($(JDKVER),)
JDKVER:=1.4
endif
JFLAGS+=-encoding UTF-8 -target $(JDKVER) -source $(JDKVER)

#----------- source definitions --------------

IGLOBAL_SRC:=$(wildcard rosuda/util/*.java)
# PoGraSS must be generated manually, because SVG is optional
POGRASS_SRC:=rosuda/pograss/PoGraSS.java rosuda/pograss/PoGraSSPS.java rosuda/pograss/PoGraSSPDF.java rosuda/pograss/PoGraSSmeta.java rosuda/pograss/PoGraSSgraphics.java
# variables with XTREME suffix use JOGL for OpenGL
POGRASS_SRC_XTREME:=$(POGRASS_SRC) rosuda/pograss/PoGraSSjogl.java
IBASE_SRC_RAW:= $(IGLOBAL_SRC) $(wildcard rosuda/ibase/*.java) $(wildcard rosuda/ibase/plots/*.java) $(wildcard rosuda/ibase/toolkit/*.java) $(POGRASS_SRC) rosuda/plugins/Plugin.java rosuda/plugins/PluginManager.java
IBASE_SRC_XTREME:=rosuda/ibase/toolkit/PGSJoglCanvas.java
IBASE_SRC:=$(filter-out $(IBASE_SRC_XTREME),$(IBASE_SRC_RAW))
PLUGINS_SRC:=$(wildcard rosuda/plugins/*.java)
IPLOTS_SRC:=$(wildcard rosuda/iplots/*.java)
MRJSTUBS_SRC:=$(wildcard rosuda/util/MRJstubs/*.java)

#ifneq ($(JOGL),yes)
#IBASE_SRC:=$(filter-out %Jogl% %JOGL%,$(IBASE_SRC))
IBASE_SRC:=$(filter-out %JOGLGraphicsDevice.java,$(IBASE_SRC))
#endif

#--------- targets ---------

TARGETS=iplots.jar

JAVAC=javac $(JFLAGS)

all: $(TARGETS)

iplots.jar: MRJstubs.jar $(IBASE_SRC) $(IPLOTS_SRC)
	rm -rf org
	$(JAVAC) -d . -classpath $^
	jar fc $@ org
	rm -rf org	

MRJstubs.jar: $(MRJSTUBS_SRC)
	rm -rf com
	$(JAVAC) -d . $^
	jar fc $@ com
	rm -rf com

docs: doc

doc: $(IBASE_SRC) $(IPLOTS_SRC)
	rm -rf JavaDoc
	mkdir JavaDoc
	javadoc -d JavaDoc -author -version -breakiterator -link $(JAPIURL) $^

clean:
	find . -name .DS_* | xargs rm -rf
	rm -rf $(TARGETS) com net org JavaDoc *~ rtest.class TextConsole.class *.java rosuda/JRI*.tar.gz rosuda/JGRlinux*.tar.gz rosuda/JGRsrc*.tar.gz rosuda/projects/klimt/build rosuda/projects/jgr/build rosuda/projects/iplots/build

.PHONY: clean all doc docs

