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
# Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009-2010 Sun Microsystems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

man_pages = mpirun.1 mpiexec.1 ompi-ps.1 ompi-iof.1 ompi-clean.1 ompi-top.1

if WANT_FT
man_pages += ompi-checkpoint.1 ompi-restart.1
endif

if OMPI_INSTALL_BINARIES
if !ORTE_DISABLE_FULL_SUPPORT

nodist_man_MANS = $(man_pages)

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f mpirun$(EXEEXT); $(LN_S) orterun$(EXEEXT) mpirun$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f mpiexec$(EXEEXT); $(LN_S) orterun$(EXEEXT) mpiexec$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-ps$(EXEEXT); $(LN_S) orte-ps$(EXEEXT) ompi-ps$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-clean$(EXEEXT); $(LN_S) orte-clean$(EXEEXT) ompi-clean$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-iof$(EXEEXT); $(LN_S) orte-iof$(EXEEXT) ompi-iof$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-top$(EXEEXT); $(LN_S) orte-top$(EXEEXT) ompi-top$(EXEEXT))
if WANT_FT
	(cd $(DESTDIR)$(bindir); rm -f ompi-checkpoint$(EXEEXT); $(LN_S) orte-checkpoint$(EXEEXT) ompi-checkpoint$(EXEEXT))
	(cd $(DESTDIR)$(bindir); rm -f ompi-restart$(EXEEXT); $(LN_S) orte-restart$(EXEEXT) ompi-restart$(EXEEXT))
endif

uninstall-local:
	rm -f $(DESTDIR)$(bindir)/mpirun$(EXEEXT) \
		$(DESTDIR)$(bindir)/mpiexec$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-ps$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-clean$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-iof$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-top$(EXEEXT)
if WANT_FT
	rm -f $(DESTDIR)$(bindir)/ompi-checkpoint$(EXEEXT) \
		$(DESTDIR)$(bindir)/ompi-restart$(EXEEXT)
endif

endif # !ORTE_DISABLE_FULL_SUPPORT
endif # OMPI_INSTALL_BINARIES

$(top_builddir)/orte/tools/orterun/orterun.1:
	(cd $(top_builddir)/orte/tools/orterun && $(MAKE) $(AM_MAKEFLAGS) orterun.1)

mpirun.1: $(top_builddir)/orte/tools/orterun/orterun.1
	cp -f $(top_builddir)/orte/tools/orterun/orterun.1 mpirun.1

mpiexec.1: $(top_builddir)/orte/tools/orterun/orterun.1
	cp -f $(top_builddir)/orte/tools/orterun/orterun.1 mpiexec.1

$(top_builddir)/orte/tools/orte-ps/orte-ps.1:
	(cd $(top_builddir)/orte/tools/orte-ps && $(MAKE) $(AM_MAKEFLAGS) orte-ps.1)

ompi-ps.1: $(top_builddir)/orte/tools/orte-ps/orte-ps.1
	cp -f $(top_builddir)/orte/tools/orte-ps/orte-ps.1 ompi-ps.1

$(top_builddir)/orte/tools/orte-clean/orte-clean.1:
	(cd $(top_builddir)/orte/tools/orte-clean && $(MAKE) $(AM_MAKEFLAGS) orte-clean.1)

ompi-clean.1: $(top_builddir)/orte/tools/orte-clean/orte-clean.1
	cp -f $(top_builddir)/orte/tools/orte-clean/orte-clean.1 ompi-clean.1

$(top_builddir)/orte/tools/orte-checkpoint/orte-checkpoint.1:
	(cd $(top_builddir)/orte/tools/orte-checkpoint && $(MAKE) $(AM_MAKEFLAGS) orte-checkpoint.1)

ompi-checkpoint.1: $(top_builddir)/orte/tools/orte-checkpoint/orte-checkpoint.1
	cp -f $(top_builddir)/orte/tools/orte-checkpoint/orte-checkpoint.1 ompi-checkpoint.1

$(top_builddir)/orte/tools/orte-iof/orte-iof.1:
	(cd $(top_builddir)/orte/tools/orte-iof && $(MAKE) $(AM_MAKEFLAGS) orte-iof.1)

ompi-iof.1: $(top_builddir)/orte/tools/orte-iof/orte-iof.1
	cp -f $(top_builddir)/orte/tools/orte-iof/orte-iof.1 ompi-iof.1

$(top_builddir)/orte/tools/orte-restart/orte-restart.1:
	(cd $(top_builddir)/orte/tools/orte-restart && $(MAKE) $(AM_MAKEFLAGS) orte-restart.1)

ompi-restart.1: $(top_builddir)/orte/tools/orte-restart/orte-restart.1
	cp -f $(top_builddir)/orte/tools/orte-restart/orte-restart.1 ompi-restart.1

$(top_builddir)/orte/tools/orte-top/orte-top.1:
	(cd $(top_builddir)/orte/tools/orte-top && $(MAKE) $(AM_MAKEFLAGS) orte-top.1)

ompi-top.1: $(top_builddir)/orte/tools/orte-top/orte-top.1
	cp -f $(top_builddir)/orte/tools/orte-top/orte-top.1 ompi-top.1

distclean-local:
	rm -f $(man_pages)
