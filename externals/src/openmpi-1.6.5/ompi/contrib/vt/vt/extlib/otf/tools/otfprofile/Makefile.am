if AMHAVEMPI
SUBDIRS = . mpi
endif

if AMBUILDBINARIES
bin_PROGRAMS = otfprofile
endif

OTFPROFILESRCDIR = $(srcdir)
include $(srcdir)/Makefile.common

AM_CPPFLAGS = $(COMMONINCLUDES)

otfprofile_CXXFLAGS = $(COMMONCXXFLAGS)
otfprofile_LDADD = $(COMMONLDADD)
otfprofile_DEPENDENCIES = $(COMMONDEPENDENCIES)
otfprofile_SOURCES = $(COMMONSOURCES)

