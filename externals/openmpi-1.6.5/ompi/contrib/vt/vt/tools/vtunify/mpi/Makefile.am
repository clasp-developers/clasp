if AMBUILDBINARIES
bin_PROGRAMS = vtunify-mpi
endif

if AMBUILDMPIUNIFYLIB
lib_LTLIBRARIES = libvt-mpi-unify.la
endif

VTUNIFYSRCDIR = $(srcdir)/..
include $(srcdir)/../Makefile.common

CC = $(MPICC)
CXX = $(MPICXX)

vtunify_mpi_CFLAGS = -DVT_MPI $(MPICFLAGS) $(MPIINCDIR)
vtunify_mpi_CXXFLAGS = $(COMMONCXXFLAGS) -DVT_MPI $(MPICXXFLAGS) $(MPIINCDIR)
vtunify_mpi_LDFLAGS = $(COMMONLDFLAGS) $(MPICXXFLAGS)

vtunify_mpi_SOURCES = \
	$(COMMONSOURCES) \
	vt_unify_lib.h \
	vt_unify_mpi.h

vtunify_mpi_LDADD = \
	$(COMMONLDADD) \
	$(MPILIBDIR) $(MPILIB)

vtunify_mpi_DEPENDENCIES = \
	$(COMMONDEPENDENCIES)

libvt_mpi_unify_la_CFLAGS = -DVT_LIB $(vtunify_mpi_CFLAGS)
libvt_mpi_unify_la_CXXFLAGS = -DVT_LIB -DVT_MPI $(HOOKSCXXFLAGS) $(ETIMESYNCCXXFLAGS) $(MPICXXFLAGS) $(MPIINCDIR)
libvt_mpi_unify_la_LDFLAGS = $(MPICXXFLAGS)
libvt_mpi_unify_la_SOURCES = $(vtunify_mpi_SOURCES)
libvt_mpi_unify_la_LIBADD = $(COMMONLDADD) $(MPILIBDIR) $(PMPILIB)
libvt_mpi_unify_la_DEPENDENCIES = $(vtunify_mpi_DEPENDENCIES)

