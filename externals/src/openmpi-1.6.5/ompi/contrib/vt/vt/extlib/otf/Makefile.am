ACLOCAL_AMFLAGS = -I config/m4

if AMHAVESWIGPYTHON
OTFLIB_PY = otflib_py
else
OTFLIB_PY =
endif

SUBDIRS = \
	vendor/jenkins_hash \
	otflib \
	otfauxlib \
	$(OTFLIB_PY) \
	tools \
	tests \
	docu

EXTRA_DIST = \
	otf_vc08.sln \
	config/otf_get_version.sh \
	VERSION
