AM_CPPFLAGS = \
	-I$(top_builddir)/otflib -I$(top_srcdir)/otflib \
	$(ZLIB_INCLUDE_LINE)

if AMBUILDBINARIES
bin_PROGRAMS = otfcompress

install-exec-hook:
	(cd $(DESTDIR)$(bindir); rm -f otfdecompress$(EXEEXT); \
	$(LN_S) otfcompress$(EXEEXT) otfdecompress$(EXEEXT))
uninstall-local:
	rm -f $(DESTDIR)$(bindir)/otfdecompress$(EXEEXT)
endif

otfcompress_LDADD = $(top_builddir)/otflib/libopen-trace-format.la $(ZLIB_LIB_LINE)
otfcompress_DEPENDENCIES = $(top_builddir)/otflib/libopen-trace-format.la
otfcompress_SOURCES = \
	sys_time.h \
	otfcompress.c

EXTRA_DIST = \
	otfcompress_vs08.vcproj

