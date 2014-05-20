AM_CPPFLAGS = -I$(top_builddir)/otflib -I$(top_srcdir)/otflib

if AMBUILDBINARIES
bin_PROGRAMS = \
	otfinfo
endif

otfinfo_LDADD = $(top_builddir)/otflib/libopen-trace-format.la
otfinfo_DEPENDENCIES = $(otfinfo_LDADD)
otfinfo_SOURCES = \
	handler.h \
	hash.h \
	otfinfo_error.h \
	handler.c \
	hash.c \
	otfinfo.c \
	otfinfo_error.c

