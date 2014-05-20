AM_CPPFLAGS = -I$(top_builddir)/otflib -I$(top_srcdir)/otflib

if AMBUILDBINARIES
#bin_PROGRAMS = otfdump # until v1.12.2 (conflicts with otfdump from OpenType Fonts)
bin_PROGRAMS = otfprint
endif

otfprint_LDADD = $(top_builddir)/otflib/libopen-trace-format.la
otfprint_DEPENDENCIES = $(otfprint_LDADD)
otfprint_SOURCES = \
	Makefile.am \
	Handler.h \
	Handler.cpp \
	otfdump.cpp

EXTRA_DIST = \
	otfdump_vs08.vcproj

