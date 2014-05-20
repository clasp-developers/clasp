/****************************************************************************
**  SCALASCA    http://www.scalasca.org/                                   **
**  KOJAK       http://www.fz-juelich.de/jsc/kojak/                        **
*****************************************************************************
**  Copyright (c) 1998-2008                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYING in the package base directory for details         **
****************************************************************************/

#include <iostream>
  using std::cerr;
#include <stack>
  using std::stack;
#include <vector>
  using std::vector;
#include <map>
  using std::map;
#include <string>
  using std::getline;
  using std::string;
#include <cctype>
  using std::isalnum;
  using std::isalpha;

#include "opari.h"
#include "handler.h"

namespace {
  string find_next_word(vector<string>& preStmt, unsigned size,
                        unsigned& pline, string::size_type& ppos) {
    while ( pline < size ) {
      string::size_type wbeg = preStmt[pline].find_first_not_of(" \t", ppos);
      if ( wbeg == string::npos || preStmt[pline][wbeg] == '\\' ) {
        ++pline;
        if ( pline < size ) { ppos = 0; } else { return ""; }
      } else {
        ppos = preStmt[pline].find_first_of(" \t()", wbeg);
        return preStmt[pline].substr(wbeg,
                                     ppos==string::npos ? ppos : ppos-wbeg);
      }
    }
    return "";
  }

  bool process_preStmt(vector<string>& preStmt, ostream& os,
                       const char* infile, int lineno,
                       string::size_type ppos, bool* e, bool* f, bool asd) {
    unsigned s = preStmt.size();
    bool inComment = false;
    bool inString = false;

    for (unsigned i=0; i<s; ++i) {
      string::size_type pos = 0;
      string& line = preStmt[i];

      // "remove" comments
      while ( pos < line.size() ) {
        if ( inComment ) {
          // look for comment end
          if ( line[pos] == '*' && line[pos+1] == '/' ) {
            line[pos++] = ' ';
            inComment = false;
          }
          line[pos++] = ' ';
        } else if ( inString || line[pos] == '\"' ) {
          // character string constant
          if ( line[pos] == '\"' ) {
            pos++;
          } else { // inString
            inString = false;
          }
          while ( pos < line.size() ) {
            if ( line[pos] == '\\' ) {
              pos++;
              if ( line[pos] == '\0' ) {
                inString = true;
                break;
              }
            } else if ( line[pos] == '\"' ) {
              pos++;
              break;
            }
            pos++;
          }
        } else if ( line[pos] == '/' ) {
          pos++;
          if ( line[pos] == '/' ) {
            // c++ comments
            line[pos-1] = ' ';
            line[pos++] = ' ';
            while ( pos < line.size() ) { line[pos++] = ' '; }
          } else if ( line[pos] == '*' ) {
            // c comment start
            line[pos-1] = ' ';
            line[pos++] = ' ';
            inComment = true;
          }
        } else {
          pos++;
        }
      }

      // shift bonded line-continuation '\' one position to right
      if ( !inString &&
           line[line.size()-1] == '\\'
           && line.size() >= 2 && line[line.size()-2] != ' '
           && line[line.size()-2] != '\t' ) {
        line.insert(line.size()-1, " ");
      }
    }

    unsigned pline = 0;
    string first = find_next_word(preStmt, s, pline, ppos);
    if ( first == "pragma" ) {
      // OpenMP pragma?
      string word = find_next_word(preStmt, s, pline, ppos);
      if ( (word == "omp") || (word == "pomp") ) {
        OMPragmaC* p = new OMPragmaC(infile, lineno, pline, ppos, preStmt, asd);
        process_pragma(p, os, e, f);
        return true;
      }
    } else if ( first == "include" ) {
      // include <omp.h> -> remove it
      string word = find_next_word(preStmt, s, pline, ppos);
      if ( (word == "\"omp.h\"") || (word == "<omp.h>") ) {
        s = 0;
      }
    }

    for (unsigned i=0; i<s; ++i) os << preStmt[i] << "\n";
    preStmt.clear();
    return false;
  }
}

void process_c_or_cxx(istream& is, const char* infile, ostream& os,
                      bool addSharedDecl) {
  string line;
  bool inComment = false;
  bool inString = false;
  bool preContLine = false;
  bool requiresEnd = true;
  bool isFor = false;
  int lineno = 1;
  string::size_type pos = 0;
  int level = 0;
  int numSemi = 0;
  string::size_type lstart = string::npos;
  vector<string> preStmt;
  vector<string> endStmt;
  stack<int> nextEnd;
  map<string,string> wrapper;

  wrapper["omp_init_lock"]         = "POMP_Init_lock";
  wrapper["omp_destroy_lock"]      = "POMP_Destroy_lock";
  wrapper["omp_set_lock"]          = "POMP_Set_lock";
  wrapper["omp_unset_lock"]        = "POMP_Unset_lock";
  wrapper["omp_test_lock"]         = "POMP_Test_lock";
  wrapper["omp_init_nest_lock"]    = "POMP_Init_nest_lock";
  wrapper["omp_destroy_nest_lock"] = "POMP_Destroy_nest_lock";
  wrapper["omp_set_nest_lock"]     = "POMP_Set_nest_lock";
  wrapper["omp_unset_nest_lock"]   = "POMP_Unset_nest_lock";
  wrapper["omp_test_nest_lock"]    = "POMP_Test_nest_lock";

  nextEnd.push(-1);

  while ( getline(is, line) ) {
    /* workaround for bogus getline implementations */
    if ( line.size() == 1 && line[0] == '\0' ) break;

    /* remove possible trailing Carriage Return from line */
    if ( line.size() > 0 && line[line.length()-1] == '\r' )
      line.erase(line.length()-1);

    if ( preContLine ) {
      /*
       * preprocessor directive continuation
       */
      preStmt.push_back(line);
      if ( line[line.size()-1] != '\\' ) {
        preContLine = false;
        if ( process_preStmt(preStmt, os, infile, lineno-preStmt.size()+1,
                             lstart+1, &requiresEnd, &isFor, addSharedDecl) ) {
          if ( requiresEnd ) {
            nextEnd.push(level);
            numSemi = isFor ? 3 : 1;
          } else {
            numSemi = 0;
          }
        }
      }

    } else if ( !inComment && 
         ((lstart = line.find_first_not_of(" \t")) != string::npos) &&
         line[lstart] == '#' ) {
      /*
       * preprocessor directive
       */
      preStmt.push_back(line);
      if ( line[line.size()-1] == '\\' ) {
        preContLine = true;
      } else {
        if ( process_preStmt(preStmt, os, infile, lineno, lstart+1,
                             &requiresEnd, &isFor, addSharedDecl) ) {
          if ( requiresEnd ) {
            nextEnd.push(level);
            numSemi = isFor ? 3 : 1;
          } else {
            numSemi = 0;
          }
        }
      }

    } else {
      /*
       * regular line
       */
      bool newlinePrinted = false;

      while ( pos < line.size() ) {
        newlinePrinted = false;
        if ( inComment ) {
          // look for comment end
          if ( line[pos] == '*' && line[pos+1] == '/' ) {
            os << "*/";
            inComment = false;
            pos += 2;
          } else {
            os << line[pos++];
          }

        } else if ( !inString && line[pos] == '/' ) {
          pos++;
          if ( line[pos] == '/' ) {
            // c++ comments
            pos++;
            os << "//";
            while ( pos < line.size() ) { os << line[pos++]; }
          } else if ( line[pos] == '*' ) {
            // c comment start
            pos++;
            os << "/*";
            inComment = true;
          } else {
            os << '/';
          }

        } else if ( inString || line[pos] == '\"' ) {
          // character string constant
          if ( line[pos] == '\"' ) {
            os << '\"';
            pos++;
          } else { // inString
            inString = false;
          }

          while( pos < line.size() ) {
            if ( line[pos] == '\\' ) {
              os << '\\';
              pos++;
              if ( line[pos] == '\0' ) {
                inString = true;
                break;
              }
            } else if ( line[pos] == '\"' ) {
              os << '\"';
              pos++;
              break;
            }
            os << line[pos];
            pos++;
          }

        } else if ( line[pos] == '\'' ) {
          // character constant
          os << "\'";
          do {
            pos++;
            if ( line[pos] == '\\' ) {
              os << '\\';
              pos++;
              if ( line[pos] == '\'' ) {
                os << '\'';
                pos++;
              }
            }
            os << line[pos];
          }
          while ( line[pos] != '\'' );
          pos++;

        } else if ( isalpha(line[pos]) || line[pos] == '_' ) {
          // identifier
          string::size_type startpos = pos;
          while ( pos < line.size() &&
                  (isalnum(line[pos]) || line[pos]=='_') ) {
            pos++;
          }
          string ident(line, startpos, pos-startpos);
          map<string,string>::iterator w = wrapper.find(ident);
          if ( w != wrapper.end() && instrument_locks() )
            os << w->second;
          else
            os << ident;

	  if ( ident == "for" && numSemi == 1 ) numSemi = 3;

        } else if ( line[pos] == '{' ) {
          // block open
          os << line[pos++];
          level++;
          numSemi = 0;

        } else if ( line[pos] == '}' ) {
          // block close
          os << line[pos++];
          level--;
          if ( nextEnd.top() == level ) {
            int moreChars = (pos < line.size());
            os << '\n';
            newlinePrinted = true;

            // while because block can actually close more than one pragma
            while ( nextEnd.top() == level ) {
              // hack: use pline (arg3) for correction value for line info
              process_pragma(new OMPragmaC(infile, lineno+1-moreChars,
                                           1-moreChars, 0, endStmt,
                                           addSharedDecl), os);
              nextEnd.pop();
            }
            if ( moreChars ) for(unsigned i=0; i<pos; ++i) os << ' ';
          }

        } else if ( line[pos] == ';' ) {
          // statement end
          os << line[pos++];
          numSemi--;
          if ( numSemi == 0 ) {
            int moreChars = (pos < line.size());
            os << '\n';
            newlinePrinted = true;
            // hack: use pline (arg3) for correction value for line info
            process_pragma(new OMPragmaC(infile, lineno+1-moreChars,
                                         1-moreChars, 0, endStmt,
                                         addSharedDecl), os);
            nextEnd.pop();

            // check whether statement actually closes more pragma
            while ( nextEnd.top() == level ) {
              // hack: use pline (arg3) for correction value for line info
              process_pragma(new OMPragmaC(infile, lineno+1-moreChars,
                                           1-moreChars, 0, endStmt,
                                           addSharedDecl), os);
              nextEnd.pop();
            }
            if ( moreChars ) for(unsigned i=0; i<pos; ++i) os << ' ';
          }

        } else {
          os << line[pos++];
        }
      }
      if ( !newlinePrinted ) os << '\n';
    }
    ++lineno;
    pos = 0;
  }

  // check end position stack
  if ( nextEnd.top() != -1 ) {
    cerr << "ERROR: could not determine end of OpenMP construct (braces mismatch?)\n";
    print_regstack_top();
    cleanup_and_exit();
  }
}
