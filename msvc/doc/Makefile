top_srcdir= ..\..\src
srcdir	= ..\..\src\doc

prefix=@prefix@
exec_prefix=$(prefix)\bin
infodir = $(prefix)\doc\info
mandir=$(prefix)\doc\man
docdir=$(prefix)\doc
manext=1

INFOEXT = info
SHELL = cmd.exe
INSTALL	= ..\install.bat
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL)
INSTALL_INFO = $(INSTALL)
mkinstalldirs = $(SHELL) $(top_srcdir)/gc/mkinstalldirs.bat
INFO_FILES = ecl.$(INFOEXT) ecldev.$(INFOEXT)
HTML_FILES = index.html license.html lgpl.html news.html benchmark.html \
	install.html download.html

RM     = erase
RMDIR  = rmdir /S /Q
MKDIR  = mkdir
CP     = copy /Y

ECL_VERSION=0.9f

FILTER = ..\c\cut$(EXE) "@PACKAGE_VERSION@" "$(ECL_VERSION)"

ECL = ../ecl

all: $(INFO_FILES) $(HTML_FILES) developers_manual user_manual clx_manual

ecl.dvi: $(srcdir)/user.txi $(srcdir)/macros.txi clisp.sty ecl.sty
	tex $(srcdir)/user.txi
ecldev.dvi: $(srcdir)/devel.txi $(srcdir)/macros.txi clisp.sty ecl.sty
	tex $(srcdir)/devel.txi
clx.dvi: clx.texinfo
	tex clx.texinfo

ecl.ps: ecl.dvi $(srcdir)/macros.txi
	dvips -o $@ ecl.dvi
ecldev.ps: ecldev.dvi $(srcdir)/macros.txi
	dvips -o $@ ecldev.dvi
clx.ps: clx.dvi
	dvips -o $@ clx.dvi

install: all
	IF NOT EXIST $(docdir) $(MKDIR) $(docdir)
	for %i in (Copyright LGPL) do $(CP) $(top_srcdir)\..\%i $(docdir)
	for %i in ($(HTML_FILES)) do $(CP) %i $(docdir)
	IF NOT EXIST $(docdir)\ecldev $(MKDIR) $(docdir)\ecldev
	for %i in (ecldev\*) do $(CP) %i $(docdir)\ecldev
	IF NOT EXIST $(docdir)\ecl $(MKDIR) $(docdir)\ecl
	for %i in (ecl\*) do $(CP) %i $(docdir)\ecl
	IF NOT EXIST $(docdir)\clx $(MKDIR) $(docdir)\clx
	for %i in (clx\*) do $(CP) %i $(docdir)\clx
flatinstall: all
	IF NOT EXIST $(docdir) $(MKDIR) $(docdir)
	for %i in (Copyright LGPL) do $(CP) $(top_srcdir)\..\%i $(docdir)
	for %i in ($(HTML_FILES)) do $(CP) %i $(docdir)
	IF NOT EXIST $(docdir)\ecldev $(MKDIR) $(docdir)\ecldev
	for %i in (ecldev\*) do $(CP) %i $(docdir)\ecldev
	IF NOT EXIST $(docdir)\ecl $(MKDIR) $(docdir)\ecl
	for %i in (ecl\*) do $(CP) %i $(docdir)\ecl
	IF NOT EXIST $(docdir)\clx $(MKDIR) $(docdir)\clx
	for %i in (clx\*) do $(CP) %i $(docdir)\clx

uninstall:
	for k in $(INFO_FILES); do \
	  rm $$k; \
	  if [ -x $(INSTALL_INFO) ]; then \
	    $(INSTALL_INFO) --delete $$k; \
	  fi; \
	done
	rm -r $(infodir)/ecl.$(INFOEXT) $(infodir)/ecldev.$(INFOEXT); \
	rm $(mandir)/man$(manext)/ecl.$(manext)

head2: developers_manual user_manual clx_manual $(srcdir)/head Makefile
	IF EXIST ecl\index.html ( \
	  ..\c\cut.exe "ecl/user.html" "ecl/index.html" \
	      "ecldev/devel.html" "ecldev/index.html" \
	      < $(srcdir)/head > head2 \
	) ELSE ( \
	  copy /y $(srcdir)/head head2 \
	)
ecl.info.gz: ecl.info
	gzip < ecl.info > ecl.info.gz
ecldev.info.gz: ecldev.info
	gzip < ecldev.info > ecldev.info.gz
clx.info.gz: clx.info
	gzip < clx.info > clx.info.gz
ecl.info: $(srcdir)/user.txi $(srcdir)/macros.txi
	makeinfo -I $(srcdir) --no-split $(srcdir)/user.txi
ecldev.info: $(srcdir)/devel.txi $(srcdir)/macros.txi
	makeinfo -I $(srcdir) --no-split $(srcdir)/devel.txi
clx.info: clx.texinfo
	makeinfo --no-split clx.texinfo
clx.texinfo: $(top_srcdir)/clx/manual/clx.texinfo
	cp $(top_srcdir)/clx/manual/clx.texinfo .

download.html: $(srcdir)/download.in.html head2
	( type head2 $(srcdir)\download.in.html $(srcdir)\end ) | $(FILTER) > $@
index.html: $(srcdir)/index.in.html head2
	( type head2 $(srcdir)\index.in.html $(srcdir)\end ) | $(FILTER) > $@
install.html: $(srcdir)/install.in.html head2
	( type head2 $(srcdir)\install.in.html $(srcdir)\end ) | $(FILTER) > $@
news.html: $(srcdir)\..\..\ANNOUNCEMENT head2
	type head2 > html_tmp
	echo "<pre>" >> html_tmp
	type $(srcdir)\..\..\ANNOUNCEMENT >> html_tmp
	echo "</pre>" >> html_tmp
	type $(srcdir)\end >> html_tmp
	$(FILTER) < html_tmp > $@
	$(RM) html_tmp
benchmark.html: $(srcdir)/benchmark.in.html ../gabriel/BENCHMARK head2
	type head2 > html_tmp
	type $(srcdir)\benchmark.in.html >> html_tmp
	echo "<pre>" >> html_tmp
	type ..\gabriel\BENCHMARK >> html_tmp
	echo "</pre>" >> html_tmp
	type $(srcdir)\end >> html_tmp
	$(FILTER) < html_tmp > $@
	$(RM) html_tmp
../gabriel/BENCHMARK:
	if not exist ..\gabriel $(MKDIR) ..\gabriel
	echo No benchmarks available > ..\gabriel\BENCHMARK
license.html: $(top_srcdir)/../Copyright head2
	type head2 > html_tmp
	echo "<pre>" >> html_tmp
	type $(top_srcdir)\..\Copyright >> html_tmp
	echo "</pre>" >> html_tmp
	type $(srcdir)\end >> html_tmp
	$(FILTER) < html_tmp > $@
	$(RM) html_tmp
lgpl.html: $(top_srcdir)/../LGPL head2
	type head2 > html_tmp
	echo "<pre>" >> html_tmp
	type $(top_srcdir)\..\LGPL >> html_tmp
	echo "</pre>" >> html_tmp
	type $(srcdir)\end >> html_tmp
	$(FILTER) < html_tmp > $@
	$(RM) html_tmp
user_manual: $(srcdir)/user.txi $(srcdir)/macros.txi
	echo "Producing ecl.html; ignore error messages."
	IF NOT EXIST ecl MKDIR ecl
	makeinfo -v -I $(srcdir) --html $(srcdir)/user.txi
	echo > user_manual
developers_manual: $(srcdir)/devel.txi $(srcdir)/macros.txi
	echo "Producing ecldev.html; ignore error messages."
	IF NOT EXIST ecldev MKDIR ecldev
	makeinfo -v -I $(srcdir) --html $(srcdir)/devel.txi
	echo > developers_manual
clx_manual: $(srcdir)/../clx/manual/clx.texinfo
	echo "Producing clx.html; ignore error messages."
	IF NOT EXIST clx MKDIR clx
	makeinfo -v --html $(srcdir)\..\clx\manual\clx.texinfo
	echo > clx_manual
clean:
	-for %i in (ecl ecldev clx ..\gabriel) do $(RMDIR) %i
	-for %i in (ecl.info* ecldev.info* $(HTML_FILES) head2 user_manual developers_manual clx_manual ..\gabriel\BENCHMARK) do $(RM) %i
