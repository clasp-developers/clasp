# -*- makefile -*-
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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
# Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
#                         reserved. 
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# This Makefile.am is quite complex and confusing.  Part of the
# problem is that Libtool (v1.5.18) does not understand F90, so we
# have to do a few things manually (and no shared libraries). Here's a
# summary of what is happening:
#
# Depedencies / who generates whom:
#
# configure generates fortran_kinds.sh
#       |
#      \|/
# fortran_kinds.sh ----> sourced by scripts/* ----> generates mpi_*.f90
#                                 |                         |
#                                \|/                        |
#                    generates mpi-f90-interfaces.h         |
#                    /                                      |
#                   \/                                      |
#  mpi-f90-interfaces.h                                     |
#                 |                                         |
#                \|/                                       \|/
#            used by mpi.f90                 used by mpi_*.f90
#           /        |                               |
#          /        \|/                             \|/
#         /  generates mpi.o                 generates mpi_*.o
#        \/                 \               /
# generates mpi module      \/             \/
# (filename unknown)      generates libmpi_f90.a
#
# Once you understand this diagram, this Makefile is much more clear.
# 

SUBDIRS = scripts

AM_FCFLAGS = -I$(top_builddir)/ompi/include -I$(top_srcdir)/ompi/include \
             $(OMPI_FC_MODULE_FLAG). -I$(srcdir) \
             -I$(top_builddir)/ompi/mpi/f90 $(FCFLAGS_f90)

# Do different things if the top-level configure decided that we're
# going to build F90 bindings or not.

lib_LTLIBRARIES =
if OMPI_WANT_F90_BINDINGS

# Add the f90 library to the list of libraries to build

lib_LTLIBRARIES += libmpi_f90.la

# Ensure that the F90 interfaces are re-generated based on the values
# that come in from configure

mpi-f90-interfaces.h: fortran_kinds.sh

CLEANFILES += mpi-f90-interfaces.h

# Run scripts to generate the f90 source files.  We only need to
# generate the .f90 files once, so the "test" checks to see if the
# file exists before running the script.  However, the
# mpi-f90-interfaces.h file may need to be generated multiple times
# (even if it already exists), such as if someone re-runs configure
# and changes the value fortran_kinds.sh (which is taken care of by
# the mpi-f90-interfaces.h dependency on fortran_kinds.sh, above).

mpi-f90-interfaces.h: $(srcdir)/scripts/mpi-f90-interfaces.h.sh
	@ p="`pwd`"; \
	echo $(srcdir)/scripts/mpi-f90-interfaces.h.sh $$p \> $@; \
	$(srcdir)/scripts/mpi-f90-interfaces.h.sh $$p > $@

# Unbelievably, some versions of sh (cough cough Solaris 9 cough
# cough) actually seem to internally perform a "cd" into a
# subdirectory when you run "./foo/bar", such that if you try to
# source a script in the top-level directory in the bar script (e.g.,
# ". some_script" in the bar script), it will try to run it in the
# "foo" subdirectory, rather than the top-level directory!  #$@#$%#$%
# So we have to pass in the pwd to the scripts so that they know where
# some_script is.

$(nodist_libmpi_f90_la_SOURCES): fortran_kinds.sh
	@ p="`pwd`"; \
	echo $(srcdir)/scripts/$@.sh $$p \> $@; \
	$(srcdir)/scripts/$@.sh $$p > $@ ;

# Automake doesn't know how to do F90 dependency analysis, so manually
# list this here (i.e., "mpi-f90-interfaces.h" is included in
# mpi.f90).

# Print a warning indicating that compiling mpi.f90 can take a while.
# The only way to do this portably is to have our own rules for mpi.o
# and mpi.obj (having a "fake" target that just has echo statement in
# it may trigger mpi.o|obj to be rebuilt because the fake name doesn't
# exist; there aren't good portable ways to indicate that the target
# name is fake and should never exist).

# Note the "-I." in the commands below.  This is because mpi.f90
# includes mpi-f90-interfaces.h, which will be in the build tree
# (i.e., the current directory) because it is generated.

mpi.o: mpi.f90 mpi-f90-interfaces.h
	@echo "***************************************************************"
	@echo "* Compiling the mpi.f90 file may take a few minutes."
	@echo "* This is quite normal -- do not be alarmed if the compile"
	@echo "* process seems to 'hang' at this point for several minutes."
	@echo "***************************************************************"
	$(FCCOMPILE) -c -I. -o $@ $(FCFLAGS_f90) $<

mpi.obj: mpi.f90 mpi-f90-interfaces.h
	@echo "***************************************************************"
	@echo "* Compiling the mpi.f90 file may take a few minutes."
	@echo "* This is quite normal -- do not be alarmed if the compile"
	@echo "* process seems to 'hang' at this point for several minutes."
	@echo "***************************************************************"
	$(FCCOMPILE) -c -I. -o $@ $(FCFLAGS_f90) `$(CYGPATH_W) '$<'`

BUILT_SOURCES = mpi-f90-interfaces.h
endif

# mpi.f90 is the only f90 source file directly in libmpif90 that gets
# distributed.  This file is always in the library, regardless of the
# size that we're building.

libmpi_f90_la_SOURCES = \
     attr_fn-f90-interfaces.h \
     conversion_fn_null-f90-interface.h \
     mpi.f90
libmpi_f90_la_LIBADD = $(top_builddir)/ompi/mpi/f77/libmpi_f77.la

# These files are all generated by scripts in the scripts/ directory.

trivial_sources = \
     mpi_sizeof.f90 

small_sources = \
     mpi_comm_spawn_multiple_f90.f90 \
     mpi_testall_f90.f90 \
     mpi_testsome_f90.f90 \
     mpi_waitall_f90.f90 \
     mpi_waitsome_f90.f90 \
     mpi_wtick_f90.f90 \
     mpi_wtime_f90.f90

medium_sources = \
     mpi_address_f90.f90 \
     mpi_accumulate_f90.f90 \
     mpi_bcast_f90.f90 \
     mpi_bsend_f90.f90 \
     mpi_bsend_init_f90.f90 \
     mpi_buffer_attach_f90.f90 \
     mpi_buffer_detach_f90.f90 \
     mpi_file_iread_f90.f90 \
     mpi_file_iread_at_f90.f90 \
     mpi_file_iread_shared_f90.f90 \
     mpi_file_iwrite_f90.f90 \
     mpi_file_iwrite_at_f90.f90 \
     mpi_file_iwrite_shared_f90.f90 \
     mpi_file_read_f90.f90 \
     mpi_file_read_all_f90.f90 \
     mpi_file_read_all_begin_f90.f90 \
     mpi_file_read_all_end_f90.f90 \
     mpi_file_read_at_f90.f90 \
     mpi_file_read_at_all_f90.f90 \
     mpi_file_read_at_all_begin_f90.f90 \
     mpi_file_read_at_all_end_f90.f90 \
     mpi_file_read_ordered_f90.f90 \
     mpi_file_read_ordered_begin_f90.f90 \
     mpi_file_read_ordered_end_f90.f90 \
     mpi_file_read_shared_f90.f90 \
     mpi_file_write_f90.f90 \
     mpi_file_write_all_f90.f90 \
     mpi_file_write_all_begin_f90.f90 \
     mpi_file_write_all_end_f90.f90 \
     mpi_file_write_at_f90.f90 \
     mpi_file_write_at_all_f90.f90 \
     mpi_file_write_at_all_begin_f90.f90 \
     mpi_file_write_at_all_end_f90.f90 \
     mpi_file_write_ordered_f90.f90 \
     mpi_file_write_ordered_begin_f90.f90 \
     mpi_file_write_ordered_end_f90.f90 \
     mpi_file_write_shared_f90.f90 \
     mpi_get_f90.f90 \
     mpi_ibsend_f90.f90 \
     mpi_irecv_f90.f90 \
     mpi_irsend_f90.f90 \
     mpi_isend_f90.f90 \
     mpi_issend_f90.f90 \
     mpi_put_f90.f90 \
     mpi_recv_f90.f90 \
     mpi_recv_init_f90.f90 \
     mpi_rsend_f90.f90 \
     mpi_rsend_init_f90.f90 \
     mpi_send_f90.f90 \
     mpi_send_init_f90.f90 \
     mpi_sendrecv_replace_f90.f90 \
     mpi_ssend_f90.f90 \
     mpi_ssend_init_f90.f90 \
     mpi_win_create_f90.f90

large_sources = \
     mpi_allgather_f90.f90 \
     mpi_allgatherv_f90.f90 \
     mpi_allreduce_f90.f90 \
     mpi_alltoall_f90.f90 \
     mpi_alltoallv_f90.f90 \
     mpi_alltoallw_f90.f90 \
     mpi_exscan_f90.f90 \
     mpi_gather_f90.f90 \
     mpi_gatherv_f90.f90 \
     mpi_pack_f90.f90 \
     mpi_pack_external_f90.f90 \
     mpi_reduce_f90.f90 \
     mpi_reduce_scatter_f90.f90 \
     mpi_scan_f90.f90 \
     mpi_scatter_f90.f90 \
     mpi_scatterv_f90.f90 \
     mpi_sendrecv_f90.f90 \
     mpi_unpack_f90.f90 \
     mpi_unpack_external_f90.f90

# The rest of the files that are in the library depend on which size
# we're building.

nodist_libmpi_f90_la_SOURCES = $(trivial_sources)
if OMPI_WANT_BUILD_F90_SMALL
nodist_libmpi_f90_la_SOURCES += $(small_sources)
endif
if OMPI_WANT_BUILD_F90_MEDIUM
nodist_libmpi_f90_la_SOURCES += $(small_sources) $(medium_sources)
endif
if OMPI_WANT_BUILD_F90_LARGE
nodist_libmpi_f90_la_SOURCES += $(small_sources) $(medium_sources) $(large_sources)
endif

# see note in config/ompi_setup_f90.m4
libmpi_f90_la_LDFLAGS = \
        -version-info $(libmpi_f90_so_version) \
        $(OMPI_F90_EXTRA_SHARED_LIBRARY_FLAGS)

#
# Clean up all F90 module files and all generated files
#

MOSTLYCLEANFILES = *.mod *.ompi_module
DISTCLEANFILES = $(nodist_libmpi_f90_la_SOURCES)

#
# Install the generated .mod files.  Unfortunately, each F90 compiler
# may generate different filenames, so we have to use a glob.  :-(
#

if OMPI_WANT_F90_BINDINGS
install-exec-hook:
	@ for file in `ls *.mod`; do \
	  echo $(INSTALL) $$file $(DESTDIR)$(libdir); \
	  $(INSTALL) $$file $(DESTDIR)$(libdir); \
	done

uninstall-local:
	@ for file in `ls *.mod`; do \
	  echo rm -f $(DESTDIR)$(libdir)/$$file; \
	  rm -f $(DESTDIR)$(libdir)/$$file; \
	done
else

# Need to have empty targets because AM can't handle having an
# AM_CONDITIONAL was targets in the "if" statement but not in the
# "else".  :-(

install-exec-hook:
uninstall-local:
endif
