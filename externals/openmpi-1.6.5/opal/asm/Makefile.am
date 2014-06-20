#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#



######################################################################
#
# This is a bit complicated.  If there is anything in the library,
# it will always be atomic-asm.S.  We just symlink atomic-asm.S to
# the best atomic operations available (as determined at configure
# time)
#
######################################################################
generated/@OMPI_ASM_FILE@: base/@OPAL_ASSEMBLY_ARCH@.asm
	@ if test ! -f "$(top_srcdir)/opal/asm/$@" ; then \
	    cmd="$(PERL) '$(top_srcdir)/opal/asm/generate-asm.pl' '@OPAL_ASSEMBLY_ARCH@' '@OPAL_ASSEMBLY_FORMAT@' '$(top_srcdir)/opal/asm/base' '$(top_builddir)/opal/asm/generated/@OMPI_ASM_FILE@'" ; \
	    echo "$$cmd" ; \
	    eval $$cmd ; \
	fi

atomic-asm.S: generated/@OMPI_ASM_FILE@
	rm -f atomic-asm.S
	@ if test -f "$(top_builddir)/opal/asm/generated/@OMPI_ASM_FILE@" ; then \
	    cmd="ln -s \"$(top_builddir)/opal/asm/generated/@OMPI_ASM_FILE@\" atomic-asm.S" ; \
	    echo "$$cmd" ; \
	    eval $$cmd ; \
	else \
	    cmd="ln -s \"$(top_srcdir)/opal/asm/generated/@OMPI_ASM_FILE@\" atomic-asm.S" ; \
	    echo "$$cmd" ; \
	    eval $$cmd ; \
	fi

if OPAL_HAVE_ASM_FILE
nodist_libasm_la_SOURCES = atomic-asm.S
libasm_la_DEPENDENCIES = generated/@OMPI_ASM_FILE@
else
nodist_libasm_la_SOURCES =
libasm_la_DEPENDENCIES =
endif

noinst_LTLIBRARIES = libasm.la
dist_libasm_la_SOURCES = asm.c

EXTRA_DIST	= \
	asm-data.txt \
	generate-asm.pl \
	generate-all-asm.pl \
	base/aix.conf \
	base/default.conf \
	base/ALPHA.asm \
	base/AMD64.asm \
	base/ARM.asm \
	base/IA32.asm \
	base/IA64.asm \
	base/MIPS.asm \
	base/POWERPC32.asm \
	base/POWERPC64.asm \
	base/SPARC.asm \
	base/SPARCV9_32.asm \
	base/SPARCV9_64.asm

######################################################################

clean-local:
	rm -f atomic-asm.S

distclean-local:
	rm -f generated/atomic-local.s

######################################################################

#
# Copy over all the generated files
#
dist-hook:
	mkdir "${distdir}/generated"
	$(PERL) "$(top_srcdir)/opal/asm/generate-all-asm.pl" "$(PERL)" "$(srcdir)" "$(distdir)"
