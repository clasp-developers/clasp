check_PROGRAMS = otf_hello

otf_hello_SOURCES = hello.c
otf_hello_LDADD = \
	$(top_builddir)/otflib/libopen-trace-format.la
otf_hello_DEPENDENCIES = $(otf_hello_LDADD)
otf_hello_CPPFLAGS = $(AM_CPPFLAGS) \
	-I$(top_builddir)/otflib \
	-I$(top_srcdir)/otflib

clean-local:
	rm -f *.otf *.def *.events *.marker *.otf.z *.def.z *.events.z *.marker.z
