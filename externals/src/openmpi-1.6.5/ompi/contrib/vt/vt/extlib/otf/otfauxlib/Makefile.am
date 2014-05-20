include_HEADERS = otfaux.h \
	OTFAUX_State.h \
	OTFAUX_MsgMatching.h \
	OTFAUX_Thumbnail.h

noinst_HEADERS = OTFAUX_SharedState.h \
	OTFAUX_Process.h \
	OTFAUX_Stack.h

lib_LTLIBRARIES = libotfaux.la

libotfaux_la_CPPFLAGS = $(AM_CPPFLAGS) \
	-I$(top_builddir)/otflib \
	-I$(top_srcdir)/otflib \
	-I$(top_srcdir)/vendor/jenkins_hash

libotfaux_la_CFLAGS = -prefer-pic
#libotfaux_la_LDFLAGS = -version-info @OTF_VERSION_LIBRARY@
libotfaux_la_LIBADD = $(top_builddir)/vendor/jenkins_hash/libjenkins_hash.la
libotfaux_la_SOURCES = \
	OTFAUX_MsgMatching.c \
	OTFAUX_Thumbnail.c \
	OTFAUX_Process.c \
	OTFAUX_State.c

EXTRA_DIST = \
	libotfaux_vs08.vcproj
