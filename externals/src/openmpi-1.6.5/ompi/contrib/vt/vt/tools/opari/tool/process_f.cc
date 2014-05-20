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
#include <vector>
  using std::vector;
#include <stack>
  using std::stack;
#include <cctype>
  using std::tolower;
  using std::toupper;
#include <string>
  using std::getline;
#include <algorithm>
  using std::transform;
  using std::sort;
#include <functional>
  using std::greater;
#include <cstring>
  using std::strlen;

#ifdef EBUG
#  include <iomanip>
   using std::setw;
#endif

#include "opari.h"
#include "handler.h"

enum Loop_type { TYPE_NO_OMP      = 0x00,
                 TYPE_OMP_LOOP    = 0x01,
                 TYPE_OMP_PARLOOP = 0x02};

typedef struct {
  Loop_type is_omp;
  string    label;
} LoopDescriptionT;

enum Line_type { NORMAL_LINE  = 0x00, 
		 PRAGMA_PARSTART     = 0x01, PRAGMA_PAREND     = 0x02,
		 PRAGMA_LOOPSTART    = 0x03, PRAGMA_LOOPEND    = 0x04, 
		 PRAGMA_PARLOOPSTART = 0x05, PRAGMA_PARLOOPEND = 0x06,
		 UNKNOWN_LINE        = 0x07, PRAGMA_UNKNOWN    = 0x08};


namespace {
  void look_for(const string& lowline, const char* word,
                vector<string::size_type>& positions) {
    string::size_type s = 0;
    while ( (s = lowline.find(word, s)) != string::npos ) {
      positions.push_back(s);
      s += strlen(word);
    }
  }

  bool is_comment_line(string& lowline, string& line) {
    if ( lowline[0] == '!' || lowline[0] == '*' || lowline[0] == 'c' ) {
      // fixed form comment

      if ( lowline[1] == '$' &&
           lowline.find_first_not_of(" \t0123456789", 2) > 5 ) {
        // OpenMP Conditional Compilation
        lowline[0] = ' ';
        lowline[1] = ' ';
        return false;
      } else if ( lowline[1] == 'p' && lowline[2] == '$' &&
           lowline.find_first_not_of(" \t0123456789", 3) > 5 ) {
        // POMP Conditional Compilation
        lowline[0] = line[0] = ' ';
        lowline[1] = line[1] = ' ';
        lowline[2] = line[2] = ' ';
        return false;
      } else {
        return true;
      }
    }

    string::size_type s = lowline.find_first_not_of(" \n");
    if ( s != string::npos && lowline[s] == '!' ) {
      // free form full line comment

      string::size_type c = lowline.find("!$ ");
      if ( c == s ) {
        // OpenMP Conditional Compilation
        lowline[s]   = ' ';
        lowline[s+1] = ' ';
        return false;
      }
      c = lowline.find("!p$ ");
      if ( c == s ) {
        // POMP Conditional Compilation
        lowline[s]   = line[s] = ' ';
        lowline[s+1] = line[s+1] = ' ';
        lowline[s+2] = line[s+2] = ' ';
        return false;
      }
      return true;
    }
    return false;
  }

  bool is_loop_start(string& lowline, string& line, string& label ) {
    string::size_type pstart = string::npos;
    string::size_type pos    = string::npos;
    string::size_type poslab = string::npos;

    label="";
    if (! (line.size()) ) return false;

    // is there a 'do '
    pstart = lowline.find("do");
    if(pstart==string::npos ||
       (lowline[pstart+2] != '\0' &&
        lowline[pstart+2] != ' '  &&
        lowline[pstart+2] != '\t')) {
      return false;
    }


    pos = lowline.find_first_not_of(" \t");
    if(pos!=pstart) {
      // there is a DO_construct_name
      poslab = lowline.find_first_of(":",pos);
      if(poslab==string::npos) return false;
      label=line.substr(pos,poslab-pos);
      // skip white space
      pos = lowline.find_first_not_of(" \t",poslab+1);
    }

    //check again, if pos now start of do, otherwise not a correct do statement
    pstart = lowline.find("do",pos);
    if(pstart!=pos ||
       (lowline[pstart+2] != '\0' &&
        lowline[pstart+2] != ' '  &&
        lowline[pstart+2] != '\t')) {
      return false;
    }

    pos = lowline.find_first_not_of(" \t",pos+2);
    if(isdigit(lowline[pos])) {
      // there is a stmtlabel
      poslab=pos;
      pos = lowline.find_first_not_of("0123456789",pos);
      label=line.substr(poslab,pos-poslab);
    }
   
    return true;
  }

  bool is_loop_end(string& lowline, string& line, string toplabel) {
    string::size_type pos,poslab;
    string label;

     
    if (! (line.size()) ) return false;

    pos = lowline.find_first_not_of(" \t");

    // is it a nonblock DO loop?
    poslab=toplabel.find_first_not_of("0123456789");
    if((toplabel.size()>0) && (poslab==string::npos)) {
      // search for nonblock Do loop
      poslab = pos;
      pos    = lowline.find_first_not_of("0123456789",pos);

      // is there a label in this line?
      if(poslab==pos) return false; 
      label=line.substr(poslab,pos-poslab);
      
      // is it the label of the top loop
      if (toplabel==label) return true;
      else                 return false;

    } else {
      // search for block Do loop
      if(lowline.compare(pos, 3, "end") != 0)
        return false;

      pos = lowline.find_first_not_of(" \t", pos+3);
      if(pos==string::npos) return false;

      if(lowline.compare(pos, 2, "do") != 0)
        return false;

      if(!(lowline[pos+2] == ' ' || lowline[pos+2] == '\t' || lowline[pos+2] == '\0'))
        return false;

      // search for label
      if(toplabel.size()) {
	
	// skip white space
	poslab = lowline.find_first_not_of(" \t",pos+2);
	pos = lowline.find_first_of(" \t",poslab);
	if(poslab==pos) return false; 
	label=line.substr(poslab,pos-poslab);
	
	// is it the label of the top loop
	if (toplabel==label) return true;
	else                 return false;
	
      } else {
	return true; // end do without label 
      }
    }
  }

  
  // check type of OMP statement 
  Line_type check_pragma(OMPragma* pragma) {
    int save_pline,save_ppos; 
    Line_type linetype=PRAGMA_UNKNOWN;
    save_pline=pragma->pline;
    save_ppos=pragma->ppos;
    pragma->find_name();
    pragma->pline=save_pline;               // reset parse position
    pragma->ppos=save_ppos;
    if(pragma->name.find("do")!=string::npos) {
      linetype=PRAGMA_LOOPSTART;
      if(pragma->name == "enddo")              linetype=PRAGMA_LOOPEND;
      else if(pragma->name == "paralleldo")    linetype=PRAGMA_PARLOOPSTART;
      else if(pragma->name == "endparalleldo") linetype=PRAGMA_PARLOOPEND;
    } else {
      if(pragma->name == "parallel")           linetype=PRAGMA_PARSTART;
      else if(pragma->name == "endparallel")   linetype=PRAGMA_PAREND;
    }
    return(linetype);
  }
  
  void test_and_insert_ompenddo(ostream& os, Line_type typeOfLastLine, Loop_type& waitforOMPEndDo,
				const string& infile, int& lineno, int ppos, int pomp, bool a) {
    OMPragma* newPragma;
    int c;
    string  pragma;

    // OMP end do found? 
    if(waitforOMPEndDo==TYPE_OMP_LOOP) {
      waitforOMPEndDo=TYPE_NO_OMP;
      if(typeOfLastLine!=PRAGMA_LOOPEND) {
	// generate end do
	pragma="";
	for(c=0;c<ppos;c++) pragma+=" ";
	pragma+="!$omp end do ";
	newPragma = new OMPragmaF(infile, lineno, ppos+6, pragma, pomp, a);
	process_pragma(newPragma, os);
	//	lineno++;
      }
    } else if(waitforOMPEndDo==TYPE_OMP_PARLOOP) {
      waitforOMPEndDo=TYPE_NO_OMP;
      if(typeOfLastLine!=PRAGMA_PARLOOPEND) {
	// generate end parallel do or complete pragma
	
	pragma="";
	for(c=0;c<ppos;c++) pragma+=" ";
	pragma+="!$omp end parallel do ";
	newPragma = new OMPragmaF(infile, lineno, ppos+6, pragma, pomp, a);
	process_pragma(newPragma, os);
	
      } 
    }
  }
  
  void del_strings_and_comments(string& lowline, char& inString) {
    // zero out string constants and free form comments
    for (unsigned i=0; i<lowline.size(); ++i) {
      if ( inString ) {
        // inside string
        if ( lowline[i] == inString ) {
          lowline[i] = '@';
          ++i;
          if ( i >= lowline.size() ) {
            // eol: no double string delimiter -> string ends
            inString = 0;
            break;
          }
          if ( lowline[i] != inString ) {
            // no double string delimiter -> string ends
            inString = 0;
            continue;
          }
        }
        lowline[i] = '@';
      }

      else if ( lowline[i] == '!' ) {
        /* -- zero out partial line F90 comments -- */
        for (; i<lowline.size(); ++i) lowline[i] = 'C';
        break;
      }

      else if ( lowline[i] == '\'' || lowline[i] == '\"' ) {
        inString = lowline[i];
        lowline[i] = '@';
      }
    }
  }

  void replace_openmp_api_calls(string& lowline, string& line) {
    // replace call to omp_*_lock routines
    vector<string::size_type> positions;
    look_for(lowline, "omp_init_lock", positions);
    look_for(lowline, "omp_destroy_lock", positions);
    look_for(lowline, "omp_set_lock", positions);
    look_for(lowline, "omp_unset_lock", positions);
    look_for(lowline, "omp_test_lock", positions);
    look_for(lowline, "omp_init_nest_lock", positions);        /*2.0*/
    look_for(lowline, "omp_destroy_nest_lock", positions);     /*2.0*/
    look_for(lowline, "omp_set_nest_lock", positions);         /*2.0*/
    look_for(lowline, "omp_unset_nest_lock", positions);       /*2.0*/
    look_for(lowline, "omp_test_nest_lock", positions);        /*2.0*/
    sort(positions.begin(), positions.end(), greater<string::size_type>());
    for (unsigned i=0; i<positions.size(); ++i) {
      line.replace(positions[i], 3, "POMP");
      line[positions[i]+5] = toupper(line[positions[i]+5]);
    }
  }

  struct fo_tolower : public std::unary_function<int,int> {
    int operator()(int x) const {
      return std::tolower(x);
    }
  };
}

void process_fortran(istream& is, const char* infile, ostream& os,
                     bool addSharedDecl) {
  string line;
  int lineno = 0;
  OMPragma* currPragma = 0;
  bool needPragma = false;
  char inString = 0;
  string::size_type pstart = string::npos;
  string::size_type lstart = string::npos;

  // for endloop instrumentation
  stack<LoopDescriptionT> loopstack; 
  LoopDescriptionT toploop;
  int pragma_indent=0;
  Line_type typeOfLastLine=UNKNOWN_LINE;
  Loop_type waitforOMPEndDo = TYPE_NO_OMP;
  string label = ""; 
  toploop.is_omp=TYPE_NO_OMP; toploop.label ="<none>";

  while ( getline(is, line) ) {
    /* workaround for bogus getline implementations */
    if ( line.size() == 1 && line[0] == '\0' ) break;

    /* remove possible trailing Carriage Return from line */
    if ( line.size() > 0 && line[line.length()-1] == '\r' )
      line.erase(line.length()-1);

    ++lineno;
    string lowline(line);
    transform(line.begin(), line.end(), lowline.begin(), fo_tolower());

    if ( inString ) {
      if ( ! is_comment_line(lowline, line) ) {
        del_strings_and_comments(lowline, inString);
        if (  instrument_locks() ) replace_openmp_api_calls(lowline, line);
      }
      os << line << '\n';
#     ifdef EBUG
      cerr << setw(3) << lineno << ":S+: " << line << '\n';
#     endif

    } else if ( line.size() &&
      (lowline[0] == '!' || lowline[0] == 'c' || lowline[0] == '*') && ( 
         (lowline[1] == '$' &&
                 ( 
                   (lowline[2] == 'p' && lowline[3] == 'o' &&
                    lowline[4] == 'm' && lowline[5] == 'p')
                 ||
                   (lowline[2] == 'o' &&
                    lowline[3] == 'm' && lowline[4] == 'p')) )
	 ||
	 (lowline[1] == 'p' && lowline[2] == 'o' &&
	  lowline[3] == 'm' && lowline[4] == 'p' && lowline[5] == '$') ) && (
      ! (lowline[0] == '!' && lowline[lowline.find_last_not_of(" \t")] == '&')) && (
      ! (lowline[0] == '!' && needPragma)

      )) {

      int pomp = ((lowline[1] == 'p') || (lowline[2] == 'p'));

      /*
       * fix form omp directive
       */
      if ( lowline[5+pomp]==' ' || lowline[5+pomp]=='\t'
                                || lowline[5+pomp]=='0' ) {
	pragma_indent = 0;
        // new directive
        if ( currPragma ) {
          // if necessary process last complete directive
	  typeOfLastLine = check_pragma(currPragma);
	  test_and_insert_ompenddo(os, typeOfLastLine, waitforOMPEndDo, 
				   infile, currPragma->lineno, pragma_indent,
				   pomp, addSharedDecl);
          process_pragma(currPragma, os);
          currPragma = 0;
        }
        currPragma = new OMPragmaF(infile, lineno, 6+pomp, lowline, pomp,
                                   addSharedDecl);

      } else {
        // continuation directive line
        if ( currPragma ) {
          currPragma->lines.push_back(lowline);
        } else {
          cerr << infile << ":" << lineno
               << ": ERROR: invalid continuation line\n";
          cleanup_and_exit();
        }
      }

    } else if ( line.size() &&
                (lstart = lowline.find_first_not_of(" \t")) != string::npos &&
                ((lstart == (pstart = lowline.find("!$omp"))) ||
                 (lstart == (pstart = lowline.find("!$pomp"))) ||
                 (lstart == (pstart = lowline.find("!pomp$"))))
              ) {

      int pomp = ((lowline[pstart+1] == 'p') || (lowline[pstart+2] == 'p'));
      pragma_indent = pstart;

      /*
       * free form omp directive
       */
      if ( needPragma ) {
        // continuation directive line
        currPragma->lines.push_back(lowline);
      } else {
        // new directive
        if ( currPragma ) {
          // if necessary process last complete directive
          typeOfLastLine = check_pragma(currPragma);
          test_and_insert_ompenddo(os, typeOfLastLine, waitforOMPEndDo,
                                   infile, currPragma->lineno, pragma_indent,
                                   pomp, addSharedDecl);
          process_pragma(currPragma, os);
          currPragma = 0;
        }
        currPragma
                = new OMPragmaF(infile, lineno, pstart+5+pomp, lowline, pomp,
                                addSharedDecl);
      }
      string::size_type com = lowline.find("!", pstart+4+pomp);
      if ( com != string::npos ) --com;
      string::size_type amp = lowline.find_last_not_of(" \t", com);
      if ( amp != string::npos && lowline[amp] == '&' ) {
        // last non-white non-comment character == '&' --> continuation
        needPragma = true;
      } else {
        // complete
        needPragma = false;
	typeOfLastLine = check_pragma(currPragma);
	test_and_insert_ompenddo(os, typeOfLastLine, waitforOMPEndDo,
				 infile, currPragma->lineno, pragma_indent,
                                 0, addSharedDecl);
        process_pragma(currPragma, os);
        currPragma = 0;
      }
      
    } else {
      /*
       * normal line
       */
      if ( needPragma ) {
        cerr << infile << ":" << lineno-1
             << ": ERROR: missing continuation line\n";
        cleanup_and_exit();
      } else if ( currPragma ) {
        // if necessary process last complete directive
	typeOfLastLine = check_pragma(currPragma);
	test_and_insert_ompenddo(os, typeOfLastLine, waitforOMPEndDo, infile,
                                 currPragma->lineno, pragma_indent, 0,
                                 addSharedDecl);
        process_pragma(currPragma, os);
        currPragma = 0;
      }

      if ( is_comment_line(lowline, line) ) {
	// normal line: comment but no OMP pragma
        os << line << '\n';
#       ifdef EBUG
        cerr << setw(3) << lineno << ":C : " << line << '\n';
#       endif
      } else if ( line.size() == 0 || 
                  lowline.find_first_not_of(" \t") == string::npos ) {
	// empty line
        os << line << '\n';
#       ifdef EBUG
        cerr << setw(3) << lineno << ":E : " << line << '\n';
#       endif
      } else {
	// really normal line
        del_strings_and_comments(lowline, inString);
        if ( instrument_locks() ) {
          replace_openmp_api_calls(lowline, line);
        }
	test_and_insert_ompenddo(os, typeOfLastLine, waitforOMPEndDo,
				 infile, lineno, pragma_indent, 0,
                                 addSharedDecl);
        os << line << '\n';
        extra_handler(lineno, os);

	// search for loop start statement
	if (  is_loop_start(lowline, line, label ) ) {
	  if(typeOfLastLine==PRAGMA_LOOPSTART)
            toploop.is_omp=TYPE_OMP_LOOP;
	  else if(typeOfLastLine==PRAGMA_PARLOOPSTART)
	    toploop.is_omp=TYPE_OMP_PARLOOP;
	  else
	    toploop.is_omp=TYPE_NO_OMP;	  
	  toploop.label = label;
	  loopstack.push(toploop);

#         ifdef EBUG
	  cerr << setw(3) << lineno << ":L" << (toploop.is_omp?"O":" ")
               << ": " << line << " (" << label << ")\n";
#         endif

	// search for loop end statement
	} else if ( (!loopstack.empty()) &&
                    is_loop_end(lowline, line, toploop.label) ) {

#         ifdef EBUG
	  cerr << setw(3) << lineno << ":l" << (waitforOMPEndDo?"O":" ")
	       << ": " << line << "\n";
#         endif
	  waitforOMPEndDo = toploop.is_omp;
	  loopstack.pop();
	  if (!loopstack.empty()) { toploop = loopstack.top(); }
	  else { toploop.is_omp = TYPE_NO_OMP; toploop.label = "<none>"; }

	  // more than one loop ending on same statement (only numerical labels)
	  while ( (toploop.label.find_first_of("0123456789") != string::npos) 
		  && (is_loop_end(lowline, line, toploop.label) )
	    ) {
	    waitforOMPEndDo = toploop.is_omp;
	    loopstack.pop();
	    if (!loopstack.empty()) { toploop = loopstack.top(); }
	    else { toploop.is_omp = TYPE_NO_OMP; toploop.label = "<none>"; }
	  }

	} else {
	  // normal line
#         ifdef EBUG
	  cerr << setw(3) << lineno << ":  : " << line << '\n';
#         endif
	  
	}

	typeOfLastLine=NORMAL_LINE;
      }
    }
  }

  // currPragma should be deleted at this point; ensure that to prevent
  // memory leak
  if ( currPragma )
    delete currPragma;
}
