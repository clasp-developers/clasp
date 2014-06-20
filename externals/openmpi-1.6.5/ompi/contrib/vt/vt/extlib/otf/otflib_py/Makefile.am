EXTRA_DIST = otf.i

AM_CPPFLAGS = \
	$(ZLIB_INCLUDE_LINE) \
	-I$(top_builddir)/otflib -I$(top_srcdir)/otflib


noinst_HEADERS = \
	pyOTF_AuxiliaryFunctions.h \
	pyOTF_Wrapper.h
	

pkgpython_PYTHON = __init__.py
nodist_pkgpython_PYTHON = otf.py
pkgpyexec_LTLIBRARIES = _otf.la
nodist__otf_la_SOURCES = otf_wrap.c
_otf_la_CPPFLAGS = $(AM_CPPFLAGS) $(SWIG_PYTHON_CFLAGS)
_otf_la_LDFLAGS = -module
_otf_la_LIBADD = $(top_builddir)/otflib/libopen-trace-format.la
_otf_la_DEPENDENCIES = $(_otf_la_LDADD)

otf_wrap.c : otf.i
	$(SWIG) $(SWIG_PYTHON_OPTS) $(INCLUDES) -o $@ $<

CLEANFILES= \
            otf_wrap.c \
            otf.py
