#
# Copyright (c) 2007      Los Alamos National Security, LLC.
#                         All rights reserved. 
# Copyright (c) 2009      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

EXTRA_DIST = .windows

sources = \
	routed_binomial.h \
	routed_binomial.c \
	routed_binomial_component.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_routed_binomial_DSO
component_noinst =
component_install = mca_routed_binomial.la
else
component_noinst = libmca_routed_binomial.la
component_install =
endif

mcacomponentdir = $(pkglibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_routed_binomial_la_SOURCES = $(sources)
mca_routed_binomial_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_routed_binomial_la_SOURCES = $(sources)
libmca_routed_binomial_la_LDFLAGS = -module -avoid-version

