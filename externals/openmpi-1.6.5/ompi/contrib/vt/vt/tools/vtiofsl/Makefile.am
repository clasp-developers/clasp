if AMBUILDBINARIES
bin_SCRIPTS = \
	vtiofsl-common \
	vtiofsl-start \
	vtiofsl-stop
nodist_bin_SCRIPTS = \
	vtiofsl-platform
endif

vtsysconf_DATA = \
	vtiofsl-platform-crayxk6-iofwd.cf \
	vtiofsl-platform-crayxk6-iofwd-msaa.cf
vtsysconfdir = $(sysconfdir)

vtiofsl-platform-crayxk6-iofwd.cf: $(top_srcdir)/tools/vtiofsl/platform/crayxk6-iofwd.cf
	@cp $^ $@

vtiofsl-platform-crayxk6-iofwd-msaa.cf: $(top_srcdir)/tools/vtiofsl/platform/crayxk6-iofwd-msaa.cf
	@cp $^ $@

vtiofsl-platform: $(top_srcdir)/tools/vtiofsl/platform/$(IOFSLSCRIPTS)
	@cp $^ $@

EXTRA_DIST = \
	platform/crayxk6 \
	platform/crayxk6-iofwd.cf \
	platform/crayxk6-iofwd-msaa.cf

CLEANFILES = \
	vtiofsl-platform

