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
# Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# Portions taken from original ROMIO Makefile*
#

EXTRA_DIST = users-guide.pdf users-guide.tex source-guide.tex README romio.bib

all:
	latex users-guide.tex
	latex users-guide.tex
	latex users-guide.tex
	dvips -o users-guide.ps users-guide.dvi
	gzip -f users-guide.ps

bib:
	latex users-guide.tex
	bibtex users-guide
	latex users-guide.tex
	latex users-guide.tex
	dvips -o users-guide.ps users-guide.dvi

clean-local:
	-rm -f users-guide.toc users-guide.log users-guide.dvi \
	       users-guide.aux
