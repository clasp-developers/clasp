/****************************************************************************
**  SCALASCA    http://www.scalasca.org/                                   **
**  KOJAK       http://www.fz-juelich.de/jsc/kojak/                        **
*****************************************************************************
**  Copyright (c) 1998-2008                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYING in the package base directory for details         **
****************************************************************************/

#ifndef OPARI_H
#define OPARI_H

#include <iosfwd>
  using std::istream;
  using std::ostream;

#include "ompragma.h"

enum Language { L_NA  = 0x00,
	        L_F77 = 0x01, L_F90 = 0x02, L_FORTRAN  = 0x03,
		L_C   = 0x04, L_CXX = 0x08, L_C_OR_CXX = 0x0C };

void process_fortran(istream& is, const char* infile, ostream& os,
                     bool addSharedDecl);
void process_c_or_cxx(istream& is, const char* infile, ostream& os,
                      bool addSharedDecl);
void process_pragma(OMPragma* p, ostream& os, bool* hasEnd=0, bool* isFor=0);

void cleanup_and_exit();

#endif
