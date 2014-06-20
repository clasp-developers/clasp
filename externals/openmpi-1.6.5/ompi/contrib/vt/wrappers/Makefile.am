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
# Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

if !OPAL_WANT_SCRIPT_WRAPPER_COMPILERS
if OMPI_INSTALL_BINARIES

dist_pkgdata_DATA = \
        mpicc-vt-wrapper-data.txt \
	mpic++-vt-wrapper-data.txt \
	mpif77-vt-wrapper-data.txt \
	mpif90-vt-wrapper-data.txt

install-exec-hook-always:
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f mpicc-vt$(EXEEXT); $(LN_S) opal_wrapper mpicc-vt)
	(cd $(DESTDIR)$(bindir); rm -f mpic++-vt$(EXEEXT); $(LN_S) opal_wrapper mpic++-vt)
	(cd $(DESTDIR)$(bindir); rm -f mpicxx-vt$(EXEEXT); $(LN_S) opal_wrapper mpicxx-vt)
	(cd $(DESTDIR)$(bindir); rm -f mpif77-vt$(EXEEXT); $(LN_S) opal_wrapper mpif77-vt)
	(cd $(DESTDIR)$(bindir); rm -f mpif90-vt$(EXEEXT); $(LN_S) opal_wrapper mpif90-vt)

install-data-hook-always:
	(cd $(DESTDIR)$(pkgdatadir); rm -f mpicxx-vt-wrapper-data.txt; $(LN_S) mpic++-vt-wrapper-data.txt mpicxx-vt-wrapper-data.txt)

uninstall-local-always:
	rm -f $(DESTDIR)$(bindir)/mpicc-vt$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpic++-vt$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpicxx-vt$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpif77-vt$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpif90-vt$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/mpicxx-vt-wrapper-data.txt

if CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
	(cd $(DESTDIR)$(bindir); rm -f mpiCC-vt$(EXEEXT); $(LN_S) opal_wrapper mpiCC-vt)

install-data-hook: install-data-hook-always
	(cd $(DESTDIR)$(pkgdatadir); rm -f mpiCC-vt-wrapper-data.txt; $(LN_S) mpic++-vt-wrapper-data.txt mpiCC-vt-wrapper-data.txt)

uninstall-local: uninstall-local-always
	rm -f $(DESTDIR)$(bindir)/mpiCC-vt$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/mpiCC-vt-wrapper-data.txt

else # CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
install-data-hook: install-data-hook-always
uninstall-local: uninstall-local-always

endif # CASE_SENSITIVE_FS

endif # OMPI_INSTALL_BINARIES
endif # OPAL_WANT_SCRIPT_WRAPPER_COMPILERS
