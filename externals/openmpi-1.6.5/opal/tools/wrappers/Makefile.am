#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2006 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

DEFS="-DEXEEXT=\"$(EXEEXT)\""

real_man_pages = generic_wrapper.1 opal_wrapper.1
EXTRA_DIST = $(real_man_pages:.1=.1in) CMakeLists.txt

include $(top_srcdir)/Makefile.man-page-rules

if !OPAL_WANT_SCRIPT_WRAPPER_COMPILERS
if OMPI_INSTALL_BINARIES

bin_PROGRAMS = opal_wrapper

nodist_man_MANS = opal_wrapper.1

dist_pkgdata_DATA = help-opal-wrapper.txt

# Only install the following for developer-level installs
if WANT_INSTALL_HEADERS

nodist_pkgdata_DATA = \
	opalcc-wrapper-data.txt \
	opalc++-wrapper-data.txt

nodist_man_MANS += opalcc.1 opalc++.1

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = opal.pc

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f opalcc$(EXEEXT); $(LN_S) opal_wrapper opalcc)
	(cd $(DESTDIR)$(bindir); rm -f opalc++$(EXEEXT); $(LN_S) opal_wrapper opalc++)

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/opalcc$(EXEEXT) \
	$(DESTDIR)$(bindir)/opalc++$(EXEEXT)

endif # WANT_INSTALL_HEADERS

endif # OMPI_INSTALL_BINARIES
endif # OPAL_WANT_SCRIPT_WRAPPER_COMPILERS

opal_wrapper_SOURCES = opal_wrapper.c
opal_wrapper_LDADD = $(top_builddir)/opal/libopen-pal.la

# Ensure that the man pages are rebuilt if the opal_config.h file
# changes; a "good enough" way to know if configure was run again (and
# therefore the release date or version may have changed)
$(real_man_pages): $(top_builddir)/opal/include/opal_config.h

opalcc.1: generic_wrapper.1
	rm -f opalcc.1
	sed -e 's/#COMMAND#/opalcc/g' -e 's/#PROJECT#/Open PAL/g' -e 's/#PROJECT_SHORT#/OPAL/g' -e 's/#LANGUAGE#/C/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > opalcc.1

opalc++.1: generic_wrapper.1
	rm -f opalc++.1
	sed -e 's/#COMMAND#/opalc++/g' -e 's/#PROJECT#/Open PAL/g' -e 's/#PROJECT_SHORT#/OPAL/g' -e 's/#LANGUAGE#/C++/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > opalc++.1

distclean-local:
	rm -f $(real_man_pages) opalcc.1 opalc++.1
