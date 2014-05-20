if AMBUILDBINARIES
bin_PROGRAMS = otfmerge-mpi
endif

OTFMERGESRCDIR = $(srcdir)/..
include $(srcdir)/../Makefile.common

CC = $(MPICC)

AM_CPPFLAGS = $(COMMONINCLUDES) $(MPI_INCLUDE_LINE)

otfmerge_mpi_CFLAGS = -DOTFMERGE_MPI $(COMMONCFLAGS) $(MPICFLAGS)
otfmerge_mpi_LDADD = $(COMMONLDADD) $(MPI_LIB_LINE)
otfmerge_mpi_DEPENDENCIES = $(COMMONDEPENDENCIES)
otfmerge_mpi_SOURCES = $(COMMONSOURCES)

