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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = -I$(top_srcdir)/test/support

#check_PROGRAMS = \
#        ompi_numtostr \
#        opal_error \
#        opal_if \
#        opal_os_path \
#        opal_timer \
#        opal_os_create_dirpath \
#        opal_argv \
#        opal_basename \
#        opal_path_nfs


check_PROGRAMS = \
	opal_path_nfs
 
TESTS = \
	$(check_PROGRAMS)

#ompi_numtostr_SOURCES = ompi_numtostr.c
#ompi_numtostr_LDADD = \
#        $(top_builddir)/opal/libopen-pal.la \
#        $(top_builddir)/test/support/libsupport.a 
#ompi_numtostr_DEPENDENCIES = $(ompi_numtostr_LDADD)

#opal_error_SOURCES = opal_error.c
#opal_error_LDADD = \
#        $(top_builddir)/orte/libopen-rte.la \
#opal_error_DEPENDENCIES = $(opal_error_LDADD)

#opal_if_SOURCES = opal_if.c
#opal_if_LDADD = \
#        $(top_builddir)/opal/libopen-pal.la \
#        $(top_builddir)/test/support/libsupport.a 
#opal_if_DEPENDENCIES = $(opal_if_LDADD)

#opal_argv_SOURCES = opal_argv.c
#opal_argv_LDADD = \
#        $(top_builddir)/opal/libopen-pal.la \
#        $(top_builddir)/test/support/libsupport.a 
#opal_argv_DEPENDENCIES = $(opal_argv_LDADD)

#opal_basename_SOURCES = opal_basename.c
#opal_basename_LDADD = \
#        $(top_builddir)/opal/libopen-pal.la \
#        $(top_builddir)/test/support/libsupport.a 
#opal_basename_DEPENDENCIES = $(opal_basename_LDADD)

opal_path_nfs_SOURCES = opal_path_nfs.c
opal_path_nfs_LDADD = \
        $(top_builddir)/opal/libopen-pal.la \
        $(top_builddir)/test/support/libsupport.a
opal_path_nfs_DEPENDENCIES = $(opal_path_nfs_LDADD)

#opal_os_path_SOURCES = opal_os_path.c
#opal_os_path_LDADD = \
#        $(top_builddir)/opal/libopen-pal.la \
#        $(top_builddir)/test/support/libsupport.a 
#opal_os_path_DEPENDENCIES = $(opal_os_path_LDADD)

#opal_timer_SOURCES = opal_timer.c
#opal_timer_LDADD = \
#        $(top_builddir)/opal/libopen-pal.la \
#        $(top_builddir)/test/support/libsupport.a 
#opal_timer_DEPENDENCIES = $(opal_timer_LDADD)


#orte_sys_info_SOURCES = orte_sys_info.c
#orte_sys_info_LDADD = \
#        $(top_builddir)/orte/libopen-rte.la \
#        $(top_builddir)/test/support/libsupport.a 
#orte_sys_info_DEPENDENCIES = $(orte_sys_info_LDADD)

#opal_os_create_dirpath_SOURCES = opal_os_create_dirpath.c
#opal_os_create_dirpath_LDADD = \
#        $(top_builddir)/opal/libopen-pal.la \
#        $(top_builddir)/test/support/libsupport.a 
#opal_os_create_dirpath_DEPENDENCIES = $(opal_os_create_dirpath_LDADD)

#orte_session_dir_SOURCES = orte_session_dir.c
#orte_session_dir_LDADD = \
#        $(top_builddir)/orte/libopen-rte.la \
#        $(top_builddir)/test/support/libsupport.a 
#orte_session_dir_DEPENDENCIES = $(orte_session_dir_LDADD)

#orte_universe_setup_file_io_SOURCES = orte_universe_setup_file_io.c
#orte_universe_setup_file_io_LDADD = \
#        $(top_builddir)/orte/libopen-rte.la \
#        $(top_builddir)/test/support/libsupport.a 
#orte_universe_setup_file_io_DEPENDENCIES = $(orte_universe_setup_file_io_LDADD)

clean-local:
	rm -f test_session_dir_out test-file opal_path_nfs.out
