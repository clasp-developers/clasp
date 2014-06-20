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
# Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This makefile.am does not stand on its own - it is included from orte/Makefile.am

include $(top_srcdir)/Makefile.man-page-rules

dist_pkgdata_DATA += util/hostfile/help-hostfile.txt \
                     util/dash_host/help-dash-host.txt \
                     util/help-regex.txt

nodist_man_MANS = util/hostfile/orte_hosts.7

# We are using $(am_dirstamp) instead of creating our own dirstamp since there
# is src code in util/hostfile directory is created.  The automake process
# creates the $(am_dirstamp), we found the use of this in the generated Makefile
# in the util/Makefile
$(nodist_man_MANS): util/hostfile/$(am__dirstamp) $(top_builddir)/opal/include/opal_config.h

EXTRA_DIST += $(nodist_man_MANS:.7=.7in)

AM_LFLAGS = -Porte_util_hostfile_
LEX_OUTPUT_ROOT = lex.orte_util_hostfile_

headers += \
	util/name_fns.h \
        util/proc_info.h \
        util/session_dir.h \
        util/show_help.h

libopen_rte_la_SOURCES += \
        util/error_strings.c \
        util/name_fns.c \
        util/proc_info.c \
        util/session_dir.c \
        util/show_help.c

if !ORTE_DISABLE_FULL_SUPPORT

headers += \
	util/context_fns.h \
	util/parse_options.h \
	util/pre_condition_transports.h \
        util/hnp_contact.h \
        util/hostfile/hostfile.h \
        util/hostfile/hostfile_lex.h \
        util/dash_host/dash_host.h \
        util/comm/comm.h \
        util/nidmap.h \
        util/regex.h

libopen_rte_la_SOURCES += \
	util/context_fns.c \
	util/parse_options.c \
	util/pre_condition_transports.c \
        util/hnp_contact.c \
        util/hostfile/hostfile_lex.l \
        util/hostfile/hostfile.c \
        util/dash_host/dash_host.c \
        util/comm/comm.c \
        util/nidmap.c \
        util/regex.c

endif

# Remove the generated man pages
distclean-local:
	rm -f $(nodist_man_MANS)
