AUTOMAKE_OPTIONS = -Wno-gnu

if AMBUILDBINARIES

bin_PROGRAMS = @CROSS_PREFIX@vtwrapper

if AMHAVECSFS
bin_LINK_vtCC = @CROSS_PREFIX@vtCC$(EXEEXT)
else
bin_LINK_vtCC =
endif

if AMBUILDVTNVCC
bin_LINK_vtnvcc = @CROSS_PREFIX@vtnvcc$(EXEEXT)
else
bin_LINK_vtnvcc =
endif

bin_LINKS = \
	@CROSS_PREFIX@vtcc$(EXEEXT) \
	@CROSS_PREFIX@vtc++$(EXEEXT) \
	@CROSS_PREFIX@vtcxx$(EXEEXT) \
	$(bin_LINK_vtCC) \
	@CROSS_PREFIX@vtfort$(EXEEXT) \
	@CROSS_PREFIX@vtf77$(EXEEXT) \
	@CROSS_PREFIX@vtf90$(EXEEXT) \
	$(bin_LINK_vtnvcc)

install-exec-hook-always:
	(cd $(DESTDIR)$(bindir); \
	for l in $(bin_LINKS); do \
		rm -f $$l; $(LN_S) @CROSS_PREFIX@vtwrapper$(EXEEXT) $$l; \
	done)
uninstall-local-exec-always:
	(cd $(DESTDIR)$(bindir); \
	for l in $(bin_LINKS); do \
		rm -f $$l; \
	done)

else # AMBUILDBINARIES
install-exec-hook-always:
uninstall-local-exec-always:
endif # AMBUILDBINARIES

if AMBUILDVTNVCC
vtdata_DATA_vtnvcc = @CROSS_PREFIX@vtnvcc-wrapper-data.txt
else
vtdata_DATA_vtnvcc =
endif

if AMHAVETAUINST
vtdata_DATA_tauinst_spec = TAUINST.SPEC
else
vtdata_DATA_tauinst_spec =
endif

vtdata_DATA = \
	@CROSS_PREFIX@vtcc-wrapper-data.txt \
	@CROSS_PREFIX@vtc++-wrapper-data.txt \
	@CROSS_PREFIX@vtfort-wrapper-data.txt \
	$(vtdata_DATA_vtnvcc) \
	$(vtdata_DATA_tauinst_spec) \
	fake_omp.h
vtdatadir = $(datadir)

install-data-hook-always:
	(cd $(DESTDIR)$(datadir); \
	rm -f @CROSS_PREFIX@vtcxx-wrapper-data.txt; \
	$(LN_S) @CROSS_PREFIX@vtc++-wrapper-data.txt @CROSS_PREFIX@vtcxx-wrapper-data.txt)
	(cd $(DESTDIR)$(datadir); \
	rm -f @CROSS_PREFIX@vtf77-wrapper-data.txt; \
	$(LN_S) @CROSS_PREFIX@vtfort-wrapper-data.txt @CROSS_PREFIX@vtf77-wrapper-data.txt)
	(cd $(DESTDIR)$(datadir); \
	rm -f @CROSS_PREFIX@vtf90-wrapper-data.txt; \
	$(LN_S) @CROSS_PREFIX@vtfort-wrapper-data.txt @CROSS_PREFIX@vtf90-wrapper-data.txt)
	$(am__mv) $(DESTDIR)$(datadir)/fake_omp.h $(DESTDIR)$(datadir)/omp.h
uninstall-local-data-always:
	rm -f $(DESTDIR)$(datadir)/@CROSS_PREFIX@vtcxx-wrapper-data.txt
	rm -f $(DESTDIR)$(datadir)/@CROSS_PREFIX@vtf77-wrapper-data.txt
	rm -f $(DESTDIR)$(datadir)/@CROSS_PREFIX@vtf90-wrapper-data.txt
	rm -f $(DESTDIR)$(datadir)/omp.h

if AMHAVECSFS
install-data-hook-1:
	(cd $(DESTDIR)$(datadir); \
	rm -f @CROSS_PREFIX@vtCC-wrapper-data.txt; \
	$(LN_S) @CROSS_PREFIX@vtc++-wrapper-data.txt @CROSS_PREFIX@vtCC-wrapper-data.txt)
uninstall-local-data-1:
	rm -f $(DESTDIR)$(datadir)/@CROSS_PREFIX@vtCC-wrapper-data.txt
else # AMHAVECSFS
install-data-hook-1:
uninstall-local-data-1:
endif # AMHAVECSFS

if AMMACOS
install-data-hook-2:
	cp $(srcdir)/TAUINST_MACOS.SPEC $(DESTDIR)$(datadir)/TAUINST.SPEC
uninstall-local-data-2:
	rm -f $(DESTDIR)$(datadir)/TAUINST.SPEC
else # AMMACOS
install-data-hook-2:
uninstall-local-data-2:
endif # AMMACOS

install-exec-hook: install-exec-hook-always
install-data-hook: install-data-hook-always install-data-hook-1 install-data-hook-2
uninstall-local: uninstall-local-exec-always uninstall-local-data-always uninstall-local-data-1 uninstall-local-data-2

if AMHAVEFC
HAVEFCCXXFLAGS = -DHAVE_FC
else # AMHAVEFC
HAVEFCCXXFLAGS =
endif # AMHAVEFC

if AMHAVEMPI
HAVEMPICXXFLAGS = -DHAVE_MPI
else # AMHAVEMPI
HAVEMPICXXFLAGS =
endif # AMHAVEMPI

if AMHAVEFMPI
HAVEFMPICXXFLAGS = -DHAVE_FMPI
else # HAVEFMPICXXFLAGS
HAVEFMPICXXFLAGS =
endif # HAVEFMPICXXFLAGS

if AMHAVETHREADS
HAVETHREADSCXXFLAGS = -DHAVE_THREADS
if AMHAVEOMP
HAVEOMPCXXFLAGS = -DHAVE_OMP
endif # AMHAVEOMP
else # AMHAVETHREADS
HAVETHREADSCXXFLAGS =
HAVEOMPCXXFLAGS =
endif # AMHAVETHREADS

if AMHAVECROSSPREFIX
@CROSS_PREFIX@vtcc-wrapper-data.txt: vtcc-wrapper-data.txt
	@cp vtcc-wrapper-data.txt @CROSS_PREFIX@vtcc-wrapper-data.txt
@CROSS_PREFIX@vtc++-wrapper-data.txt: vtc++-wrapper-data.txt
	@cp vtc++-wrapper-data.txt @CROSS_PREFIX@vtc++-wrapper-data.txt
@CROSS_PREFIX@vtfort-wrapper-data.txt: vtfort-wrapper-data.txt
	@cp vtfort-wrapper-data.txt @CROSS_PREFIX@vtfort-wrapper-data.txt
@CROSS_PREFIX@vtnvcc-wrapper-data.txt: vtnvcc-wrapper-data.txt
	@cp vtnvcc-wrapper-data.txt @CROSS_PREFIX@vtnvcc-wrapper-data.txt

CROSSPREFIXCLEANFILES = \
	@CROSS_PREFIX@vtcc-wrapper-data.txt \
	@CROSS_PREFIX@vtc++-wrapper-data.txt \
	@CROSS_PREFIX@vtfort-wrapper-data.txt \
	@CROSS_PREFIX@vtnvcc-wrapper-data.txt
else # AMHAVECROSSPREFIX
CROSSPREFIXCLEANFILES =
endif # AMHAVECROSSPREFIX

AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_builddir)/include -I$(top_srcdir)/util -I$(top_builddir)/util

CC = $(CC_FOR_BUILD)
CFLAGS = $(CFLAGS_FOR_BUILD)
CPP = $(CPP_FOR_BUILD)
CXX = $(CXX_FOR_BUILD)
CXXFLAGS = $(CXXFLAGS_FOR_BUILD)
CXXCPP = $(CXXCPP_FOR_BUILD)
CPPFLAGS = $(CPPFLAGS_FOR_BUILD)
LDFLAGS = $(LDFLAGS_FOR_BUILD)
LIBS = $(LIBS_FOR_BUILD)

@CROSS_PREFIX@vtwrapper_SOURCES = \
	vt_wrapper.cc \
	$(top_srcdir)/util/installdirs.c \
	$(top_srcdir)/util/util.c

@CROSS_PREFIX@vtwrapper_CXXFLAGS = \
	$(HAVEFCCXXFLAGS) \
	$(HAVEMPICXXFLAGS) \
	$(HAVEFMPICXXFLAGS) \
	$(HAVETHREADSCXXFLAGS) \
	$(HAVEOMPCXXFLAGS)

CLEANFILES = $(CROSSPREFIXCLEANFILES)

EXTRA_DIST = \
	fake_omp.h \
	TAUINST.SPEC \
	TAUINST_MACOS.SPEC

