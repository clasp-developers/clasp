#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

#man_pages = ortecc.1 ortec++.1

if !ORTE_DISABLE_FULL_SUPPORT

# always include the following in tarballs as the selection of
# script vs binary wrapper compilers is a configure option
bin_SCRIPTS = orte_wrapper_script
CLEANFILES += $(bin_SCRIPTS)

dist_pkgdata_DATA = \
	ortecc-wrapper-data.txt \
	ortec++-wrapper-data.txt


if OPAL_WANT_SCRIPT_WRAPPER_COMPILERS
install-exec-hook-always:
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f ortecc; $(LN_S) orte_wrapper_script ortecc)
	(cd $(DESTDIR)$(bindir); rm -f ortec++; $(LN_S) orte_wrapper_script ortec++)

uninstall-local-always:
	rm -f $(DESTDIR)$(bindir)/ortecc \
	$(DESTDIR)$(bindir)/ortec++
if CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
	(cd $(DESTDIR)$(bindir); rm -f orteCC; $(LN_S) orte_wrapper_script orteCC)
uninstall-local: uninstall-local-always
	rm -f $(DESTDIR)$(bindir)/orteCC
else # CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
uninstall-local: uninstall-local-always
endif # CASE_SENSITIVE_FS

else # OPAL_WANT_SCRIPT_WRAPPER_COMPILERS

if OMPI_INSTALL_BINARIES

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = orte.pc

#man_MANS = $(man_pages)

install-exec-hook-always:
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f ortecc$(EXEEXT); $(LN_S) opal_wrapper ortecc)
	(cd $(DESTDIR)$(bindir); rm -f ortec++$(EXEEXT); $(LN_S) opal_wrapper ortec++)

uninstall-local-always:
	rm -f $(DESTDIR)$(bindir)/ortecc$(EXEEXT) \
	$(DESTDIR)$(bindir)/ortec++$(EXEEXT)

if CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
	(cd $(DESTDIR)$(bindir); rm -f orteCC$(EXEEXT); $(LN_S) opal_wrapper orteCC)

install-data-hook:
	(cd $(DESTDIR)$(pkgdatadir); rm -f orteCC-wrapper-data.txt; $(LN_S) ortec++-wrapper-data.txt orteCC-wrapper-data.txt)
	(cd $(DESTDIR)$(mandir)/man1; rm -f orteCC.1; $(LN_S) ortec++.1 orteCC.1)

uninstall-local: uninstall-local-always
	rm -f $(DESTDIR)$(bindir)/mpiCC$(EXEEXT) \
	$(DESTDIR)$(mandir)/man1/mpiCC.1 \
	$(DESTDIR)$(pkgdatadir)/mpiCC-wrapper-data.txt

else # CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always

install-data-hook:

uninstall-local: uninstall-local-always

endif # CASE_SENSITIVE_FS

endif # OMPI_INSTALL_BINARIES

endif # OPAL_WANT_SCRIPT_WRAPPER_COMPILERS

distclean-local:
#	rm -f $(man_pages)

#ortecc.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
#	rm -f ortecc.1
#	sed -e 's/#COMMAND#/ortecc/g' -e 's/#PROJECT#/OpenRTE/g' -e 's/#PROJECT_SHORT#/ORTE/g' -e 's/#LANGUAGE#/C/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > ortecc.1

#ortec++.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
#	rm -f ortec++.1
#	sed -e 's/#COMMAND#/ortec++/g' -e 's/#PROJECT#/OpenRTE/g' -e 's/#PROJECT_SHORT#/ORTE/g' -e 's/#LANGUAGE#/C++/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > ortec++.1

endif # !ORTE_DISABLE_FULL_SUPPORT
