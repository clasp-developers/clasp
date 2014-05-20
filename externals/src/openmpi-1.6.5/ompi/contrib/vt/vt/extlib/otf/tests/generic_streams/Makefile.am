if AMHAVECLOCKGETTIME

check_PROGRAMS = otf_generic_streams

otf_generic_streams_SOURCES = generic_streams.c
otf_generic_streams_DEPENDENCIES = \
	$(top_builddir)/otflib/libopen-trace-format.la
otf_generic_streams_LDADD = $(otf_generic_streams_DEPENDENCIES) \
	$(CLOCK_GETTIME_LIB)
otf_generic_streams_CPPFLAGS = $(AM_CPPFLAGS) \
	-I$(top_builddir)/otflib \
	-I$(top_srcdir)/otflib

clean-local:
	rm -f *.otf *.def *.events *.marker *.otf.z *.def.z *.events.z *.marker.z

endif
