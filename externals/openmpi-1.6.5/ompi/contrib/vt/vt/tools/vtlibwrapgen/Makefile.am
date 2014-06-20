if AMBUILDBINARIES
bin_PROGRAMS = vtlibwrapgen
endif

AM_CPPFLAGS = $(CTOOLINCDIR) -I$(top_srcdir)/include -I$(top_builddir)/include -I$(top_srcdir)/util -I$(top_builddir)/util

CC = $(CC_FOR_BUILD)
CFLAGS = $(CFLAGS_FOR_BUILD)
CPP = $(CPP_FOR_BUILD)
CXX = $(CXX_FOR_BUILD)
CXXFLAGS = $(CXXFLAGS_FOR_BUILD)
CXXCPP = $(CXXCPP_FOR_BUILD)
CPPFLAGS = $(CPPFLAGS_FOR_BUILD)
LDFLAGS = $(LDFLAGS_FOR_BUILD)
LIBS = $(LIBS_FOR_BUILD)

vtlibwrapgen_SOURCES = \
	vt_libwrapgen.h \
	vt_libwrapgen_filter.h \
	vt_libwrapgen_parser.h \
	vt_libwrapgen.cc \
	vt_libwrapgen_filter.cc \
	vt_libwrapgen_parser.cc \
	$(top_srcdir)/util/installdirs.c \
	$(top_srcdir)/util/util.c

nodist_vtlibwrapgen_SOURCES = \
	vt_libwrapgen_defs.h

vtlibwrapgen_LDADD = \
	$(CTOOLLIBDIR) $(CTOOLLIB)

