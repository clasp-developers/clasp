AM_CPPFLAGS = -I$(top_builddir)/otflib -I$(top_srcdir)/otflib

if AMBUILDBINARIES
bin_PROGRAMS = \
	otfshrink
endif

otfshrink_LDADD = $(top_builddir)/otflib/libopen-trace-format.la
otfshrink_DEPENDENCIES = $(otfaux_LDADD)
otfshrink_SOURCES = \
	otfshrink.h \
	Handler.h \
	procList.h \
	otfshrink.cpp \
	Handler.cpp

