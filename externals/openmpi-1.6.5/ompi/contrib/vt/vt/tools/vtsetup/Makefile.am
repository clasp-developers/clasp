SUBDIRS = . doc

if AMBUILDBINARIES
bin_SCRIPTS = vtsetup
vtbindata_DATA = vtsetup.jar
vtbindatadir = $(bindir)
endif

vtsysconf_DATA = \
	vtsetup-config.dtd \
	vtsetup-config.xml
vtsysconfdir = $(sysconfdir)

vtdata_DATA = \
	vtsetup-data.xml \
	vtsetup-data.dtd
vtdatadir = $(datadir)

EXTRA_DIST = \
	vtsetup.jar \
	vtsetup-config.dtd \
	vtsetup-data.dtd \
	ChangeLog \
	README \
	src/vtsetup-1.0.1.src.tar.gz

