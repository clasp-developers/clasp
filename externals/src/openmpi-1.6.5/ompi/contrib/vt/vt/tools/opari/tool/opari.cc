/****************************************************************************
**  SCALASCA    http://www.scalasca.org/                                   **
**  KOJAK       http://www.fz-juelich.de/jsc/kojak/                        **
*****************************************************************************
**  Copyright (c) 1998-2008                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYING in the package base directory for details         **
****************************************************************************/

#include <fstream>
  using std::ifstream;
  using std::ofstream;
#include <iostream>
  using std::cout;
  using std::cerr;
#include <cstdio>
  using std::remove;
#include <cstring>
  using std::strcmp;
  using std::strrchr;
  using std::strncpy;
  using std::strcat;
  using std::strlen;
#include <cstdlib>
  using std::exit;

#include "util/util.h"

#include "opari.h"
#include "handler.h"

namespace {
  void define_POMP(ostream& os) {
    os << "#ifdef _POMP\n"
       << "#  undef _POMP\n"
       << "#endif\n"
       << "#define _POMP 200110\n\n";
  }

  char* out_filename = 0;
}

void cleanup_and_exit() {
  if ( out_filename ) remove(out_filename);
  exit(1);
}

int main (int argc, char *argv[]) {
  // -- parse options
  int a = 1;
  Language lang = L_NA;
  bool keepSrcInfo = true;
  bool addSharedDecl = true;
  bool errFlag = false;
  const char* rcfile = 0;
  char* rcdir = 0;
  const char* infile = 0;
  const char* tabfile = 0;
  const char* disabled = 0;

  while ( a < argc && argv[a][0] == '-' ) {
    if ( strcmp(argv[a], "-f77") == 0 ) {
      lang = L_F77;
    } else if ( strcmp(argv[a], "-f90") == 0 ) {
      lang = L_F90;
    } else if ( strcmp(argv[a], "-c++") == 0 ) {
      lang = L_CXX;
    } else if ( strcmp(argv[a], "-c") == 0 ) {
      lang = L_C;
    } else if ( strcmp(argv[a], "-nosrc") == 0 ) {
      keepSrcInfo = false;
    } else if ( strcmp(argv[a], "-nodecl") == 0 ) {
      addSharedDecl = false;
    } else if ( strcmp(argv[a], "-rcfile") == 0 ) {
      if ( (a+1) < argc ) {
        rcfile = argv[++a];
      } else {
        cerr << "ERROR: missing value for option -rcfile\n";
        errFlag = true;
      }
    } else if ( strcmp(argv[a], "-table") == 0 ) {
      if ( (a+1) < argc ) {
        tabfile = argv[++a];
      } else {
        cerr << "ERROR: missing value for option -table\n";
        errFlag = true;
      }
    } else if ( strcmp(argv[a], "-disable") == 0 ) {
      if ( (a+1) < argc ) {
        disabled = argv[++a];
        if ( set_disabled(disabled) ) errFlag = true;
      } else {
        cerr << "ERROR: missing value for option -disable\n";
        errFlag = true;
      }
    } else {
      cerr << "ERROR: unknown option " << argv[a] << "\n";
      errFlag = true;
    }
    ++a;
  }

  // -- parse file arguments
  ifstream is;
  ofstream os;

  switch ( argc - a ) {
  case 2:
    if ( strcmp(argv[a+1], "-") == 0 ) {
      os.std::ostream::rdbuf(cout.rdbuf());
    } else {
      os.open(argv[a+1]);
      if ( !os ) {
        cerr << "ERROR: cannot open output file " << argv[a+1] << "\n";
        errFlag = true;
      }
      out_filename = argv[a+1];
    }
    /*NOBREAK*/
  case 1:
    infile = argv[a];
    is.open(infile);
    if ( !is ) {
      cerr << "ERROR: cannot open input file " << infile << "\n";
      errFlag = true;
    }
    break;
  case 0:
    if ( tabfile ) break;
    cerr << "ERROR: missing input file\n";
    /*NOBREAK*/
  default:
    errFlag = true;
    break;
  }

  // initialize language if necessary
  //   name *.[fF]    => Fortran77
  //   name *.[fF]9*  => Fortran90
  //   name *.c       => C
  //   name *.[cC]*   => C++
  if ( !errFlag && infile && lang == L_NA ) {
    const char* dot = strrchr(infile, '.');
    if ( dot != 0  && dot[1] ) {
      switch ( dot[1] ) {
      case 'f': case 'F':
        lang = dot[2]=='9' ? L_F90 : L_F77;
        break;
      case 'c': case 'C':
        lang = dot[2] ? L_CXX : L_C;
        break;
      }
    }

    if ( lang == L_NA ) {
      cerr << "ERROR: cannot determine input file language\n";
      errFlag = true;
    }
  }

  // generate output file name if necessary
  if ( !errFlag && infile && (a+1) == argc ) {
    out_filename = new char[strlen(infile)+5];
    char* dot = (char *) strrchr(infile, '.');
    if ( dot != 0 ) {
      char* infile_prefix = new char[strlen(infile)];
      strncpy(infile_prefix, infile, (int)(dot - infile));
      infile_prefix[(int)(dot-infile)] = '\0';

      vt_snprintf(out_filename, strlen(infile)+5, "%s.mod%s",
	          infile_prefix, dot);
    
      if ( keepSrcInfo && (lang & L_FORTRAN) ) {
        dot = strrchr(out_filename, '.');
        *(++dot) = 'F';
      }

      os.open(out_filename);
      if ( !os ) {
        cerr << "ERROR: cannot open output file " << out_filename << "\n";
        errFlag = true;
      }

      delete [] infile_prefix;
    } else {
      cerr << "ERROR: cannot generate output file name\n";
      errFlag =true;
    }
  }

  // print usage and die on error
  if ( errFlag ) {
    cerr << "usage: " << argv[0] << " [-f77|-f90|-c|-c++] [-nosrc]\n"
         << "       [-disable <construct>[,<construct>]...]\n"
         << "       [-rcfile <file>] [-table <tabfile>] <infile> [<outfile>]\n";
    cerr << "   or: " << argv[0] << " [-rcfile <file>] -table <tabfile>\n";
    exit(1);
  }

  // generate opari resource file if necessary
  if ( !rcfile ) {
    rcfile = "opari.rc";
    rcdir = (char*)".";
  } else {
    const char* sep = strrchr(rcfile, '/');
    if ( sep ) {
      rcdir = new char[strlen(rcfile)];
      int dirlen = sep - rcfile;
      strncpy(rcdir, rcfile, dirlen);
      rcdir[dirlen] = '\0';
    } else {
      rcdir  = (char*)".";
    }
  }

  // do actual work
  if ( tabfile && (a == argc) ) {
    // just generate table file
    generateTableFile(rcdir, rcfile, tabfile);
  } else if( infile ) {
    // generate opari include file name
    // C: in working directory
    // F: in rcfile directory
    char* incfile = 0;
    int len = 0;
    if ( lang & L_FORTRAN ) {
      // only need base filename without path
      const char* dirsep = strrchr(infile, '/');
      if ( dirsep ) {
	len = strlen(rcdir)+strlen(dirsep)+11+1;
        incfile = new char[len];
        vt_snprintf(incfile, len - 1, "%s/%s.opari.inc", rcdir, dirsep+1);
      } else {
	len = strlen(rcdir)+strlen(infile)+12+1;
        incfile = new char[len];
        vt_snprintf(incfile, len - 1, "%s/%s.opari.inc", rcdir, infile);
      }
    } else {
      len = strlen(infile)+11+1;
      incfile = new char[len];
      vt_snprintf(incfile, len - 1, "%s.opari.inc", infile);
    }

    // transform
    do_transform = true;
    init_handler(infile, rcfile, lang, keepSrcInfo);
    if ( lang & L_FORTRAN ) {
      if ( keepSrcInfo ) {
        define_POMP(os);
        os << "#line 1 \"" << infile << "\"" << "\n";
      }
      process_fortran(is, infile, os, addSharedDecl);
    } else {
      define_POMP(os);
      os << "#include \"" << incfile << "\"" << "\n";
      if ( keepSrcInfo ) os << "#line 1 \"" << infile << "\"" << "\n";
      process_c_or_cxx(is, infile, os, addSharedDecl);
    }
    finalize_handler(rcdir, incfile, tabfile);
    delete [] incfile;
  }

  return 0;
}
