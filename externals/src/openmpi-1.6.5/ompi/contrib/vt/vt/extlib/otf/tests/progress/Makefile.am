check_PROGRAMS = otf_progress

otf_progress_SOURCES = progress.c
otf_progress_DEPENDENCIES = $(top_builddir)/otflib/libopen-trace-format.la
otf_progress_LDADD = $(top_builddir)/otflib/libopen-trace-format.la
otf_progress_CPPFLAGS = $(AM_CPPFLAGS) \
	-I$(top_builddir)/otflib \
	-I$(top_srcdir)/otflib

clean-local:
	rm -f *.otf *.def *.events *.marker *.otf.z *.def.z *.events.z *.marker.z
