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

man_pages = mpicc.1 mpic++.1 mpicxx.1 mpif77.1 mpif90.1

if OPAL_WANT_SCRIPT_WRAPPER_COMPILERS

bin_SCRIPTS = ompi_wrapper_script
CLEANFILES += $(bin_SCRIPTS)

install-exec-hook-always:
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f mpicc; $(LN_S) ompi_wrapper_script mpicc)
	(cd $(DESTDIR)$(bindir); rm -f mpic++; $(LN_S) ompi_wrapper_script mpic++)
	(cd $(DESTDIR)$(bindir); rm -f mpicxx; $(LN_S) ompi_wrapper_script mpicxx)
	(cd $(DESTDIR)$(bindir); rm -f mpif77; $(LN_S) ompi_wrapper_script mpif77)
	(cd $(DESTDIR)$(bindir); rm -f mpif90; $(LN_S) ompi_wrapper_script mpif90)
uninstall-local-always:
	rm -f $(DESTDIR)$(bindir)/mpicc \
	$(DESTDIR)$(bindir)/mpic++ \
	$(DESTDIR)$(bindir)/mpicxx \
	$(DESTDIR)$(bindir)/mpif77 \
	$(DESTDIR)$(bindir)/mpif90
if CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
	(cd $(DESTDIR)$(bindir); rm -f mpiCC; $(LN_S) ompi_wrapper_script mpiCC)
uninstall-local: uninstall-local-always
	rm -f $(DESTDIR)$(bindir)/mpiCC
else # CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
uninstall-local: uninstall-local-always
endif # CASE_SENSITIVE_FS

else # OPAL_WANT_SCRIPT_WRAPPER_COMPILERS

if OMPI_INSTALL_BINARIES

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = ompi.pc ompi-c.pc ompi-cxx.pc ompi-f77.pc ompi-f90.pc

man_MANS = $(man_pages)

dist_pkgdata_DATA = \
        mpicc-wrapper-data.txt \
	mpic++-wrapper-data.txt \
	mpif77-wrapper-data.txt \
	mpif90-wrapper-data.txt

install-exec-hook-always:
	test -z "$(bindir)" || $(MKDIR_P) "$(DESTDIR)$(bindir)"
	(cd $(DESTDIR)$(bindir); rm -f mpicc$(EXEEXT); $(LN_S) opal_wrapper mpicc)
	(cd $(DESTDIR)$(bindir); rm -f mpic++$(EXEEXT); $(LN_S) opal_wrapper mpic++)
	(cd $(DESTDIR)$(bindir); rm -f mpicxx$(EXEEXT); $(LN_S) opal_wrapper mpicxx)
	(cd $(DESTDIR)$(bindir); rm -f mpif77$(EXEEXT); $(LN_S) opal_wrapper mpif77)
	(cd $(DESTDIR)$(bindir); rm -f mpif90$(EXEEXT); $(LN_S) opal_wrapper mpif90)

install-data-hook-always:
	(cd $(DESTDIR)$(pkgdatadir); rm -f mpicxx-wrapper-data.txt; $(LN_S) mpic++-wrapper-data.txt mpicxx-wrapper-data.txt)

uninstall-local-always:
	rm -f $(DESTDIR)$(bindir)/mpicc$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpic++$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpicxx$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpif77$(EXEEXT) \
	$(DESTDIR)$(bindir)/mpif90$(EXEEXT) \
	$(DESTDIR)$(pkgdatadir)/mpicxx-wrapper-data.txt

if CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
	(cd $(DESTDIR)$(bindir); rm -f mpiCC$(EXEEXT); $(LN_S) opal_wrapper mpiCC)

install-data-hook: install-data-hook-always
	(cd $(DESTDIR)$(pkgdatadir); rm -f mpiCC-wrapper-data.txt; $(LN_S) mpic++-wrapper-data.txt mpiCC-wrapper-data.txt)
	(cd $(DESTDIR)$(mandir)/man1; rm -f mpiCC.1; $(LN_S) mpic++.1 mpiCC.1)

uninstall-local: uninstall-local-always
	rm -f $(DESTDIR)$(bindir)/mpiCC$(EXEEXT) \
	$(DESTDIR)$(mandir)/man1/mpiCC.1 \
	$(DESTDIR)$(pkgdatadir)/mpiCC-wrapper-data.txt

else # CASE_SENSITIVE_FS
install-exec-hook: install-exec-hook-always
install-data-hook: install-data-hook-always
uninstall-local: uninstall-local-always

endif # CASE_SENSITIVE_FS

endif # OMPI_INSTALL_BINARIES

endif # OPAL_WANT_SCRIPT_WRAPPER_COMPILERS


########################################################
#
# Man page generation / handling
#
########################################################
distclean-local:
	rm -f $(man_pages)

$(top_builddir)/opal/tools/wrappers/generic_wrapper.1:
	(cd $(top_builddir)/opal/tools/wrappers && $(MAKE) $(AM_MAKEFLAGS) generic_wrapper.1)

mpicc.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f mpicc.1
	sed -e 's/#COMMAND#/mpicc/g' -e 's/#PROJECT#/Open MPI/g' -e 's/#PROJECT_SHORT#/OMPI/g' -e 's/#LANGUAGE#/C/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > mpicc.1

mpic++.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f mpic++.1
	sed -e 's/#COMMAND#/mpic++/g' -e 's/#PROJECT#/Open MPI/g' -e 's/#PROJECT_SHORT#/OMPI/g' -e 's/#LANGUAGE#/C++/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > mpic++.1

mpicxx.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f mpicxx.1
	sed -e 's/#COMMAND#/mpicxx/g' -e 's/#PROJECT#/Open MPI/g' -e 's/#PROJECT_SHORT#/OMPI/g' -e 's/#LANGUAGE#/C++/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > mpicxx.1

mpif77.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f mpif77.1
	sed -e 's/#COMMAND#/mpif77/g' -e 's/#PROJECT#/Open MPI/g' -e 's/#PROJECT_SHORT#/OMPI/g' -e 's/#LANGUAGE#/Fortran 77/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > mpif77.1

mpif90.1: $(top_builddir)/opal/tools/wrappers/generic_wrapper.1
	rm -f mpif90.1
	sed -e 's/#COMMAND#/mpif90/g' -e 's/#PROJECT#/Open MPI/g' -e 's/#PROJECT_SHORT#/OMPI/g' -e 's/#LANGUAGE#/Fortran 90/g' < $(top_builddir)/opal/tools/wrappers/generic_wrapper.1 > mpif90.1
