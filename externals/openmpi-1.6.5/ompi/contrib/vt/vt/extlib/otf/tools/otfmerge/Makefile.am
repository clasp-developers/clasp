if AMHAVEMPI
SUBDIRS = . mpi
endif

if AMBUILDBINARIES
bin_PROGRAMS = otfmerge
endif

OTFMERGESRCDIR = $(srcdir)
include $(srcdir)/Makefile.common

AM_CPPFLAGS = $(COMMONINCLUDES)

otfmerge_CFLAGS = $(COMMONCFLAGS)
otfmerge_LDADD = $(COMMONLDADD)
otfmerge_DEPENDENCIES = $(COMMONDEPENDENCIES)
otfmerge_SOURCES = $(COMMONSOURCES)

