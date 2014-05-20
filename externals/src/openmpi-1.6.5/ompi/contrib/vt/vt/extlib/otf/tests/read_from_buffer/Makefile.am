check_PROGRAMS = otf_read_from_buffer

otf_read_from_buffer_SOURCES = read_from_buffer.c
otf_read_from_buffer_DEPENDENCIES = $(top_builddir)/otflib/libopen-trace-format.la
otf_read_from_buffer_LDADD = $(otf_read_from_buffer_DEPENDENCIES)
otf_read_from_buffer_CPPFLAGS = $(AM_CPPFLAGS) \
	-I$(top_builddir)/otflib \
	-I$(top_srcdir)/otflib
