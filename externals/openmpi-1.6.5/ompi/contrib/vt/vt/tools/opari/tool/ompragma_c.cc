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
#include <iomanip>
#include <sstream>

string OMPragmaC::find_next_word() {
  while ( pline < lines.size() ) {
    string::size_type wbeg = lines[pline].find_first_not_of(" \t", ppos);
    if ( wbeg == string::npos || lines[pline][wbeg] == '\\' ) {
      ++pline;
      if ( pline < lines.size() ) { ppos = 0; } else { return ""; }
    } else if ( lines[pline][wbeg] == '(' || lines[pline][wbeg] == ')' ) {
      ppos = wbeg+1;
      return string(1, lines[pline][wbeg]);
    } else {
      ppos = lines[pline].find_first_of(" \t()", wbeg);
      return lines[pline].substr(wbeg, ppos==string::npos ? ppos : ppos-wbeg);
    }
  }
  return "";
}

bool OMPragmaC::find_word(const char* word,
                         unsigned& line, string::size_type& pos) {
  for (unsigned i=line; i<lines.size(); ++i) {
    string::size_type w = lines[i].find(word);
    if ( w != string::npos ) {
      line = i;
      pos  = w;
      return true;
    }
    pos = 0;
  }
  return false;
}

void OMPragmaC::add_nowait() {
  int lastline = lines.size() - 1;
  lines[lastline].append(" nowait");
}

void OMPragmaC::add_descr(int n) {
  std::ostringstream os;
  if (asd) {
    os << " POMP_DLIST_" << std::setw(5) << std::setfill('0') << n;
  } else {
    // not 100% right but best we can do if compiler doesn't allow macro
    // replacement on pragma statements
    os << " shared(omp_rd_" << n << ")";
#ifdef OPARI_VT
    os << " copyin(omp_pt_" << n << ")";
#endif //OPARI_VT
  }
  int lastline = lines.size() - 1;
  lines[lastline].append(os.str());
}

namespace {
  //inline void sreplace(string& lhs, const string& rhs, int from, int to) {
  //  for (int i=from; i<to; ++i) lhs[i] = rhs[i];
  //}

  inline void sreplace(string& lhs, const char* rhs, int from) {
    do { lhs[from] = *rhs; ++from; ++rhs; } while ( *rhs );
  }

  void fix_clause_arg(OMPragma* outer, OMPragma* inner,
                      unsigned& line, string::size_type& pos) {
    char* optr = &(outer->lines[line][pos]);
    char* iptr = &(inner->lines[line][pos]);
    while ( *optr != ')' ) {
      while ( *optr == '\\' ) {
        // skip to next line
        ++line;
        if ( line >= outer->lines.size() ) return;
        pos = 0;
        optr = &(outer->lines[line][pos]);
        iptr = &(inner->lines[line][pos]);
      }
      *iptr = *optr;
      *optr = ' ';
      ++iptr; ++optr; ++pos;
    }
    *iptr = ')';
    *optr = ' ';
  }
}

void OMPragmaC::remove_empties() {
  // remove lines without content
  vector<string>::iterator it=lines.begin();
  while ( it!=lines.end() ) {
    string::size_type l = it->find_first_not_of(" \t&");
    if ( l == string::npos || (*it)[l] == '\\' )
      it = lines.erase(it);
    else
      ++it;
  }

  // make sure last line is not a continuated line
  int lastline = lines.size() - 1;
  string::size_type lastpos = lines[lastline].size() -1;
  if ( lines[lastline][lastpos] == '\\' ) lines[lastline][lastpos] = ' ';
}


OMPragma* OMPragmaC::split_combined() {
  // make empty copy with continuation characters
  vector<string> innerLines;
  for (unsigned i=0; i<lines.size(); ++i) {
    innerLines.push_back(string(lines[i].size(), ' '));
    if ( i != lines.size() ) innerLines[i][innerLines[i].size()-1] = '\\';
  }

  // copy sentinel
  unsigned line = 0; string::size_type pos = 0;
  pos = lines[0].find("#");
  innerLines[0][pos++] = '#';

  find_word("pragma", line, pos);
  sreplace(innerLines[line], "pragma", pos);
  pos += 6;
  
  find_word("omp", line, pos);
  sreplace(innerLines[line], "omp", pos);
  pos += 3;
  
  OMPragmaC* inner = new OMPragmaC(filename, lineno, 0, 0, innerLines, asd);

  // fix pragma name
  line = pos = 0;
  if ( find_word("for", line, pos) ) {
    sreplace(lines[line], "   ", pos);
    sreplace(inner->lines[line], "for", pos);
  }
  line = pos = 0;
  if ( find_word("sections", line, pos) ) {
    sreplace(lines[line], "        ", pos);
    sreplace(inner->lines[line], "sections", pos);
  }

  // fix pragma clauses
  line = pos = 0;
  while ( find_word("ordered", line, pos) ) {
    sreplace(lines[line], "       ", pos);
    sreplace(inner->lines[line], "ordered", pos);
    pos += 7;
  }
  line = pos = 0;
  while ( find_word("lastprivate", line, pos) ) {
    sreplace(lines[line], "           ", pos);
    sreplace(inner->lines[line], "lastprivate", pos);
    pos += 11;
    fix_clause_arg(this, inner, line, pos);
  }
  line = pos = 0;
  while ( find_word("schedule", line, pos) ) {
    sreplace(lines[line], "        ", pos);
    sreplace(inner->lines[line], "schedule", pos);
    pos += 8;
    fix_clause_arg(this, inner, line, pos);
  }

  // final cleanup
  remove_empties();
  inner->remove_empties();

  return inner;
}

