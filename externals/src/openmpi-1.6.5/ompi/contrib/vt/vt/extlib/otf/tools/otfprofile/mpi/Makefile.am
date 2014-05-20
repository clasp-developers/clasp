if AMBUILDBINARIES
bin_PROGRAMS = otfprofile-mpi
endif

OTFPROFILESRCDIR = $(srcdir)/..
include $(srcdir)/../Makefile.common

CXX = $(MPICXX)

AM_CPPFLAGS = $(COMMONINCLUDES) $(MPI_INCLUDE_LINE)

otfprofile_mpi_CXXFLAGS = -DOTFPROFILE_MPI $(COMMONCXXFLAGS) $(MPICXXFLAGS)
otfprofile_mpi_LDADD = $(COMMONLDADD) $(MPI_LIB_LINE)
otfprofile_mpi_DEPENDENCIES = $(COMMONDEPENDENCIES)
otfprofile_mpi_SOURCES = \
	$(COMMONSOURCES) \
	$(OTFPROFILESRCDIR)/reduce_data.h \
	$(OTFPROFILESRCDIR)/reduce_data.cpp

