/****************************************************************************
**  SCALASCA    http://www.scalasca.org/                                   **
**  KOJAK       http://www.fz-juelich.de/jsc/kojak/                        **
*****************************************************************************
**  Copyright (c) 1998-2008                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYING in the package base directory for details         **
****************************************************************************/

#include "ompragma.h"
#include <iostream>

void OMPragma::find_name() {
  string w;

  if ( lines.empty() ) {
    // automatically generated END pragma for C/C++
    name = "$END$";
    return;
  }
  name = find_next_word();
  if ( name == "parallel" ) {
    w = find_next_word();
    if ( w == "do"  || w == "sections" ||
         w == "for" || w == "workshare" /*2.0*/) name += w;
  } else if ( name == "end" ) {
    w = find_next_word();
    name += w;
    if ( w == "parallel" ) {
      w = find_next_word();
      if ( w == "do"  || w == "sections" ||
           w == "for" || w == "workshare" /*2.0*/) name += w;
    }
  } else if ( name == "no" || name == "inst" ) {   /*INST*/
    name += find_next_word();      /*INST*/
  }
}

bool OMPragma::is_nowait() {
  unsigned dummy = 0;
  string::size_type dummy2 = 0;
  return find_word("nowait", dummy, dummy2);
}

bool OMPragma::has_copypriv() {
  unsigned dummy = 0;
  string::size_type dummy2 = 0;
  return find_word("copyprivate", dummy, dummy2);
}

string OMPragma::find_sub_name() {
  string cname = find_next_word();
  if ( cname == "(" ) return find_next_word();
  return "";
}

