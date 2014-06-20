CC = $(MPICC)

if AMHAVECLOCKGETTIME
if AMHAVEMPI

check_PROGRAMS = otf_generic_streams_mpi

otf_generic_streams_mpi_CFLAGS = $(MPICFLAGS)
otf_generic_streams_mpi_SOURCES = generic_streams_mpi.c
otf_generic_streams_mpi_DEPENDENCIES = $(top_builddir)/otflib/libopen-trace-format.la
otf_generic_streams_mpi_LDADD = $(top_builddir)/otflib/libopen-trace-format.la \
	$(CLOCK_GETTIME_LIB) \
	$(MPI_LIB_LINE)
otf_generic_streams_mpi_CPPFLAGS = $(AM_CPPFLAGS) \
	-I$(top_builddir)/otflib \
	-I$(top_srcdir)/otflib \
	$(MPI_INCLUDE_LINE)

clean-local:
	rm -f *.otf *.def *.events *.marker *.otf.z *.def.z *.events.z *.marker.z

endif
endif
