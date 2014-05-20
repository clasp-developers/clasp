/****************************************************************************
**  SCALASCA    http://www.scalasca.org/                                   **
**  KOJAK       http://www.fz-juelich.de/jsc/kojak/                        **
*****************************************************************************
**  Copyright (c) 1998-2008                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYING in the package base directory for details         **
****************************************************************************/

#include <vector>
  using std::vector;
#include <map>
  using std::map;
#include <stack>
  using std::stack;
#include <iostream>
  using std::cerr;
#include <fstream>
  using std::ifstream;
  using std::ofstream;
#include <string>
  using std::string;
  using std::getline;
#include <cstdlib>
  using std::exit;
#include <cstring>
  using std::strcmp;
#include <cctype>
  using std::toupper;

#include "handler.h"
#include "ompregion.h"

/*
 * global data
 */

namespace {
  class File {
  public:
    File(const string& n, int f, int l, Language la)
        : name(n), first(f), last(l), lang(la) {}
    string name;
    unsigned first, last;
    Language lang;
  };

  typedef map<string, phandler_t> htab;
  htab table;
  vector<OMPRegion*> regions;
  stack<OMPRegion*> regStack;
  OMPRegion* atomicRegion = 0;
  vector<File*> fileList;
  Language lang = L_NA;
  bool keepSrcInfo = false;
  const char* infile = "";
  const char* rcfile = "";
}

bool do_transform = true;

/*
 * local utility functions and data
 */

namespace {
  enum constructs {
    C_NONE     = 0x0000,
    C_ATOMIC   = 0x0001,
    C_CRITICAL = 0x0002,
    C_MASTER   = 0x0004,
    C_SINGLE   = 0x0008,
    C_LOCKS    = 0x0010,
    C_FLUSH    = 0x0020,
    C_SYNC     = 0x00FF,
    C_OMP      = 0x0FFF,
    C_ALL      = 0xFFFF
  };

  unsigned enabled = C_ALL;

  unsigned string2construct(const string& str) {
    switch ( str[0] ) {
    case 'a': if ( str == "atomic" )   return C_ATOMIC;
              break;
    case 'c': if ( str == "critical" ) return C_CRITICAL;
              break;
    case 'f': if ( str == "flush" )    return C_FLUSH;
              break;
    case 'l': if ( str == "locks" )    return C_LOCKS;
              break;
    case 'm': if ( str == "master" )   return C_MASTER;
              break;
    case 'o': if ( str == "omp" )      return C_OMP;
              break;
    case 's': if ( str == "single" )   return C_SINGLE;
              if ( str == "sync" )     return C_SYNC;
              break;
    }
    return C_NONE;
  }

  unsigned IDused = 0;

  int nextID() { return ++IDused; }

  void generate_call(const char* event, const char* type, int id, ostream& os) {
    char c1 = toupper(type[0]);
    if ( lang & L_FORTRAN )
      os << "      call POMP_" << c1 << (type+1)
         << "_" << event << "(" << id << ")\n";
    else {
      if ( strcmp(event, "begin") == 0 ) os << "{ ";
#ifdef OPARI_VT
      if ( strcmp(event, "fork") == 0 ||
           ( strcmp(event, "begin") == 0 && strcmp(type, "parallel") == 0 ) )
        os << "POMP_" << c1 << (type+1)
           << "_" << event << "2(&omp_rd_" << id << ", &omp_pt_" << id << ");";
      else
#endif // OPARI_VT
      os << "POMP_" << c1 << (type+1)
         << "_" << event << "(&omp_rd_" << id << ");";
      if ( strcmp(event, "end") == 0 ) os << " }";
      os << "\n";
    }
  }

  void generate_pragma(const char* p, ostream& os) {
    if ( lang & L_FORTRAN )
      os << "!$omp " << p << "\n";
    else
      os << "#pragma omp " << p << "\n";
  }

  void generate_barrier(int n, ostream& os) {
    generate_call("enter", "barrier", n, os);
    generate_pragma("barrier", os);
    generate_call("exit", "barrier", n, os);
  }

  void print_pragma(OMPragma* p, ostream& os) {
    if ( p->lines.size() && keepSrcInfo ) {
      // print original source location information reset pragma
      os << "#line " << p->lineno << " \"" << infile << "\"" << "\n";
    }
    // print pragma text
    for (unsigned i=0; i<p->lines.size(); ++i) os << p->lines[i] << "\n";
  }

  void print_pragma_plain(OMPragma* p, ostream& os) {
    for (unsigned i=0; i<p->lines.size(); ++i) os << p->lines[i] << "\n";
  }

  void reset_src_info(OMPragma* p, ostream& os) {
    os << "#line " << p->lineno+p->lines.size()
       << " \"" << infile << "\"" << "\n";
  }

  OMPRegion* REnter(OMPragma* p, int n, bool new_outer = false) {
    OMPRegion* r = new OMPRegion(p->name, p->filename, n,
                                 p->lineno, p->lineno+p->lines.size()-1,
                                 new_outer);
    regions.push_back(r);
    regStack.push(r);
    return r;
  }

  OMPRegion* RTop(OMPragma* p) {
    if ( regStack.empty() ) {
      cerr << infile << ":" << p->lineno
           << ": ERROR: unbalanced pragma/directive nesting\n";
      cleanup_and_exit();
    } else {
      return regStack.top();
    }
    return 0;
  }

  int RExit(OMPragma* p, bool end_outer = false) {
    OMPRegion* r = RTop(p);

#if defined(__GNUC__) && (__GNUC__ < 3)
    if ( p->name[0] != '$' && p->name.substr(3) != r->name ) {
#else
    if ( p->name[0] != '$' && p->name.compare(3, string::npos, r->name) != 0 ) {
#endif
      cerr << infile << ":" << r->begin_first_line
             << ": ERROR: missing end" << r->name << " directive for "
             << r->name << " directive\n";
      cerr << infile << ":" << p->lineno
             << ": ERROR: non-matching " << p->name
             << " directive\n";
      cleanup_and_exit();
    }
    if ( p->lines.size() ) {
      r->end_first_line = p->lineno;
      r->end_last_line  = p->lineno + p->lines.size() - 1;
    } else {
      // C/C++ $END$ pragma
      r->end_first_line = r->end_last_line = p->lineno - p->pline;
    }
    if (end_outer) r->finish();
    regStack.pop();
    return r->id;
  }

  void readResourceFile(const char* inf, const char* rcf) {
    ifstream rcs(rcf);
    if ( !rcs ) {
      IDused = 0;
      if ( inf ) fileList.push_back(new File(inf, 1, -1, lang));
    } else {
      string magic;
      getline(rcs, magic);
      if ( magic != "<OPARIRC 1.0>" ) {
        cerr << "ERROR: wrong magic number for " << rcf << "\n";
        cleanup_and_exit();
      }
      rcs >> IDused;
      rcs.ignore(9999, '\n');   // ignore rest of line

      string line;
      while ( getline(rcs, line) ) {
        /* workaround for bogus getline implementations */
        if ( line.size() == 1 && line[0] == '\0' ) break;

        if ( inf && line == inf ) {
          cerr << "WARNING: " << inf << " was already processed\n";
        } else {
          int f, l, la;
          rcs >> f >> l >> la;
          fileList.push_back(new File(line, f, l, static_cast<Language>(la)));
        }
        rcs.ignore(9999, '\n');   // ignore rest of line
      }
      if ( inf ) fileList.push_back(new File(inf, IDused+1, -1, lang));
    }
  }

  void writeTableFile(const char* rcdir, const char* tabfile) {
    ofstream tabs(tabfile);
    if ( !tabs ) {
      cerr << "ERROR: cannot open opari table file " << tabfile << "\n";
      cleanup_and_exit();
    }
    OMPRegion::generate_header(tabs);
    tabs << "\n";

    // For Fortran: include generated .opari.inc files containing the
    //              descriptors
    // For C/C++  : generate extern declaration for descriptor (as they
    //              are part of the C/C++ file
    for (unsigned i=0; i<fileList.size(); ++i) {
      if ( fileList[i]->lang & L_FORTRAN ) {
        tabs << "#include \"" << rcdir << "/";
        // only need base filename without path
        string::size_type sep = fileList[i]->name.rfind('/');
        if ( sep != string::npos )
          tabs << fileList[i]->name.substr(sep+1) << ".opari.inc\"\n";
        else
          tabs << fileList[i]->name << ".opari.inc\"\n";
      } else if ( unsigned d = fileList[i]->first ) {
        for (; d<=fileList[i]->last; ++d)
          tabs << "extern struct ompregdescr omp_rd_" << d << ";\n";
      }
    }
    tabs << "\n";

    // Generate descriptor table
    // Note: might contain holes!
    tabs << "int POMP_MAX_ID = " << (IDused+1) << ";\n\n";
    tabs << "struct ompregdescr* pomp_rd_table[" << (IDused+1) << "] = {\n";
    if ( IDused == 0 ) tabs << "  0,\n"; // hole
    int id = -1;
    for (unsigned i=0; i<fileList.size(); ++i) {
      int d = fileList[i]->first;
      if ( d ) {
        for (int n=id+1; n<d; ++n) tabs << "  0,\n";  // hole
        id = fileList[i]->last;
        for (; d<=id; ++d) tabs << "  &omp_rd_" << d << ",\n";
      }
    }
    tabs << "};\n";
  }
}

/*
 * OpenMP pragma transformation functions
 */

namespace {
  void h_parallel(OMPragma* p, ostream& os) {
    int n = nextID();
    /*OMPRegion* r =*/ REnter(p, n, true);
    p->add_descr(n);
    generate_call("fork", "parallel", n, os);
    print_pragma(p, os);
    generate_call("begin", "parallel", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_endparallel(OMPragma* p, ostream& os) {
    int n = RExit(p, true);
    generate_barrier(n, os);
    generate_call("end", "parallel", n, os);
    print_pragma(p, os);
    generate_call("join", "parallel", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_for(OMPragma* p, ostream& os) {
    int n = nextID();
    OMPRegion* r = REnter(p, n);
    if ( ! p->is_nowait() ) {
      p->add_nowait();
      r->noWaitAdded = true;
    }
    generate_call("enter", "for", n, os);
    print_pragma(p, os);
  }

  void h_endfor(OMPragma* p, ostream& os) {
    OMPRegion* r = RTop(p);
    int n = RExit(p);
    if ( r->noWaitAdded ) generate_barrier(n, os);
    generate_call("exit", "for", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_do(OMPragma* p, ostream& os) {
    int n = nextID();
    /*OMPRegion* r =*/ REnter(p, n);
    generate_call("enter", "do", n, os);
    print_pragma(p, os);
  }

  void h_enddo(OMPragma* p, ostream& os) {
    int n = RExit(p);
    if ( p->is_nowait() ) {
      print_pragma(p, os);
    } else {
      p->add_nowait();
      print_pragma(p, os);
      generate_barrier(n, os);
    }
    generate_call("exit", "do", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_sections_c(OMPragma* p, ostream& os) {
    int n = nextID();
    OMPRegion* r = REnter(p, n);
    if ( ! p->is_nowait() ) {
      p->add_nowait();
      r->noWaitAdded = true;
    }
    generate_call("enter", "sections", n, os);
    print_pragma(p, os);
  }

  void h_section_c(OMPragma* p, ostream& os) {
    OMPRegion* r = RTop(p);
    OMPRegion* s = new OMPRegion(p->name, p->filename, r->id,
                                 p->lineno, p->lineno+p->lines.size()-1);
    regStack.push(s);
    if ( r->num_sections ) print_pragma_plain(p, os); else print_pragma(p, os);
    generate_call("begin", "section", r->id, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
    r->num_sections++;
  }

  void h_endsection_c(OMPragma* p, ostream& os) {
    OMPRegion* r = RTop(p);
    generate_call("end", "section", r->id, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
    regStack.pop();
  }

  void h_endsections_c(OMPragma* p, ostream& os) {
    OMPRegion* r = RTop(p);
    int n = RExit(p);
    if ( r->noWaitAdded ) generate_barrier(n, os);
    generate_call("exit", "sections", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_sections(OMPragma* p, ostream& os) {
    int n = nextID();
    /*OMPRegion* r =*/ REnter(p, n);
    generate_call("enter", "sections", n, os);
    print_pragma(p, os);
  }

  void h_section(OMPragma* p, ostream& os) {
    OMPRegion* r = RTop(p);
    if ( r->num_sections ) {
      // close last section if necessary
      generate_call("end", "section", r->id, os);
    }
    print_pragma(p, os);
    generate_call("begin", "section", r->id, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
    ++(r->num_sections);
  }

  void h_endsections(OMPragma* p, ostream& os) {
    int n = RExit(p);
    generate_call("end", "section", n, os);
    if ( p->is_nowait() ) {
      print_pragma(p, os);
    } else {
      p->add_nowait();
      print_pragma(p, os);
      generate_barrier(n, os);
    }
    generate_call("exit", "sections", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_single_c(OMPragma* p, ostream& os) {
    int n = nextID();
    OMPRegion* r = REnter(p, n);
    if ( ! p->is_nowait() ) {
      if ( ! p->has_copypriv() ) p->add_nowait();
      r->noWaitAdded = true;
    }
    generate_call("enter", "single", n, os);
    print_pragma(p, os);
    generate_call("begin", "single", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_endsingle_c(OMPragma* p, ostream& os) {
    OMPRegion* r = RTop(p);
    int n = RExit(p);
    generate_call("end", "single", n, os);
    if ( r->noWaitAdded ) generate_barrier(n, os);
    generate_call("exit", "single", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_single(OMPragma* p, ostream& os) {
    int n = nextID();
    /*OMPRegion* r =*/ REnter(p, n);
    generate_call("enter", "single", n, os);
    print_pragma(p, os);
    generate_call("begin", "single", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_endsingle(OMPragma* p, ostream& os) {
    int n = RExit(p);
    generate_call("end", "single", n, os);
    if ( p->is_nowait() ) {
      print_pragma(p, os);
    } else {
      if ( ! p->has_copypriv() ) p->add_nowait();
      print_pragma(p, os);
      generate_barrier(n, os);
    }
    generate_call("exit", "single", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_master(OMPragma* p, ostream& os) {
    int n = nextID();
    /*OMPRegion* r =*/ REnter(p, n);
    print_pragma(p, os);
    generate_call("begin", "master", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_endmaster_c(OMPragma* p, ostream& os) {
    int n = RExit(p);
    generate_call("end", "master", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_endmaster(OMPragma* p, ostream& os) {
    int n = RExit(p);
    generate_call("end", "master", n, os);
    print_pragma(p, os);
  }

  void h_critical(OMPragma* p, ostream& os) {
    int n = nextID();
    OMPRegion* r = REnter(p, n);
    r->sub_name = p->find_sub_name();
    generate_call("enter", "critical", n, os);
    print_pragma(p, os);
    generate_call("begin", "critical", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_endcritical(OMPragma* p, ostream& os) {
    OMPRegion* r = RTop(p);
    int n = RExit(p);
    if ( p->name[0] != '$' ) {
      string cname = p->find_sub_name();
      if ( cname != r->sub_name  ) {
        cerr << infile << ":" << r->begin_first_line
             << ": ERROR: missing end critical(" << r->sub_name
             << ") directive\n";
        cerr << infile << ":" << p->lineno
             << ": ERROR: non-matching end critical(" << cname
             << ") directive\n";
        cleanup_and_exit();
      }
    }
    generate_call("end", "critical", n, os);
    print_pragma(p, os);
    generate_call("exit", "critical", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_parallelfor(OMPragma* p, ostream& os) {
    int n = nextID();
    /*OMPRegion* r =*/ REnter(p, n, true);
    OMPragma* forPragma = p->split_combined();
    forPragma->add_nowait();

    generate_call("fork", "parallel", n, os);
    print_pragma(p, os);   // #omp parallel
    generate_call("begin", "parallel", n, os);
    generate_call("enter", "for", n, os);
    print_pragma(forPragma, os);  // #omp for nowait
    delete forPragma;
  }

  void h_endparallelfor(OMPragma* p, ostream& os) {
    int n = RExit(p, true);
    generate_barrier(n, os);
    generate_call("exit", "for", n, os);
    generate_call("end", "parallel", n, os);
    generate_call("join", "parallel", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_paralleldo(OMPragma* p, ostream& os) {
    int n = nextID();
    /*OMPRegion* r =*/ REnter(p, n, true);
    generate_call("fork", "parallel", n, os);
    OMPragma* doPragma = p->split_combined();
    print_pragma(p, os);   // #omp parallel
    generate_call("begin", "parallel", n, os);
    generate_call("enter", "do", n, os);
    print_pragma(doPragma, os);  // #omp do
    delete doPragma;
  }

  void h_endparalleldo(OMPragma* p, ostream& os) {
    int n = RExit(p, true);
    generate_pragma("end do nowait", os);
    generate_barrier(n, os);
    generate_call("exit", "do", n, os);
    generate_call("end", "parallel", n, os);
    generate_pragma("end parallel", os);
    generate_call("join", "parallel", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_parallelsections_c(OMPragma* p, ostream& os) {
    int n = nextID();
    /*OMPRegion* r =*/ REnter(p, n, true);
    OMPragma* secPragma = p->split_combined();
    secPragma->add_nowait();

    generate_call("fork", "parallel", n, os);
    print_pragma(p, os);          // #omp parallel
    generate_call("begin", "parallel", n, os);
    generate_call("enter", "sections", n, os);
    print_pragma(secPragma, os);  // #omp sections
    delete secPragma;
  }

  void h_endparallelsections_c(OMPragma* p, ostream& os) {
    int n = RExit(p, true);
    generate_barrier(n, os);
    generate_call("exit", "sections", n, os);
    generate_call("end", "parallel", n, os);
    generate_call("join", "parallel", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_parallelsections(OMPragma* p, ostream& os) {
    int n = nextID();
    /*OMPRegion* r =*/ REnter(p, n, true);
    generate_call("fork", "parallel", n, os);
    OMPragma* secPragma = p->split_combined();
    print_pragma(p, os);          // #omp parallel
    generate_call("begin", "parallel", n, os);
    generate_call("enter", "sections", n, os);
    print_pragma(secPragma, os);  // #omp sections
    delete secPragma;
  }

  void h_endparallelsections(OMPragma* p, ostream& os) {
    int n = RExit(p, true);
    generate_call("end", "section", n, os);
    generate_pragma("end sections nowait", os);
    generate_barrier(n, os);
    generate_call("exit", "sections", n, os);
    generate_call("end", "parallel", n, os);
    generate_pragma("end parallel", os);
    generate_call("join", "parallel", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_barrier(OMPragma* p, ostream& os) {
    int n = nextID();
    OMPRegion* r = new OMPRegion(p->name, p->filename, n,
                                 p->lineno, p->lineno+p->lines.size()-1);
    regions.push_back(r);
    generate_call("enter", "barrier", n, os);
    print_pragma(p, os);
    generate_call("exit", "barrier", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_flush(OMPragma* p, ostream& os) {
    int n = nextID();
    OMPRegion* r = new OMPRegion(p->name, p->filename, n,
                                 p->lineno, p->lineno+p->lines.size()-1);
    regions.push_back(r);
    generate_call("enter", "flush", n, os);
    print_pragma(p, os);
    generate_call("exit", "flush", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_atomic(OMPragma* p, ostream& os) {
    int n = nextID();
    OMPRegion* r = new OMPRegion(p->name, p->filename, n,
                                 p->lineno, p->lineno+p->lines.size()-1);
    regions.push_back(r);
    generate_call("enter", "atomic", n, os);
    print_pragma(p, os);
    atomicRegion = r;
  }

  /*2.0*/
  void h_workshare(OMPragma* p, ostream& os) {
    int n = nextID();
    /*OMPRegion* r =*/ REnter(p, n);
    generate_call("enter", "workshare", n, os);
    print_pragma(p, os);
  }

  /*2.0*/
  void h_endworkshare(OMPragma* p, ostream& os) {
    int n = RExit(p);
    if ( p->is_nowait() ) {
      print_pragma(p, os);
      generate_call("exit", "workshare", n, os);
    } else {
      p->add_nowait();
      print_pragma(p, os);
      generate_barrier(n, os);
      generate_call("exit", "workshare", n, os);
    }
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  /*2.0*/
  void h_parallelworkshare(OMPragma* p, ostream& os) {
    int n = nextID();
    /*OMPRegion* r =*/ REnter(p, n, true);
    generate_call("fork", "parallel", n, os);
    OMPragma* wsPragma = p->split_combined();
    print_pragma(p, os);   // #omp parallel
    generate_call("begin", "parallel", n, os);
    generate_call("enter", "workshare", n, os);
    print_pragma(wsPragma, os);  // #omp workshare
    delete wsPragma;
  }

  /*2.0*/
  void h_endparallelworkshare(OMPragma* p, ostream& os) {
    int n = RExit(p, true);
    generate_pragma("end workshare nowait", os);
    generate_barrier(n, os);
    generate_call("exit", "workshare", n, os);
    generate_call("end", "parallel", n, os);
    generate_pragma("end parallel", os);
    generate_call("join", "parallel", n, os);
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  /*INST*/
  void h_inst(OMPragma* p, ostream& os) {
    char c1 = toupper(p->name.substr(4)[0]);
    if ( lang & L_FORTRAN )
      os << "      call POMP_" << c1 << p->name.substr(5) << "()\n";
    else
      os << "POMP_" << c1 << p->name.substr(5) << "();\n";
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  /*INST*/
  void h_instbegin(OMPragma* p, ostream& os) {
    int n = nextID();
    OMPRegion* r = REnter(p, n);
    r->name     = "region";
    r->sub_name = p->find_sub_name();
    if ( lang & L_FORTRAN )
      os << "      call POMP_Begin(" << n << ")\n";
    else
      os << "POMP_Begin(&omp_rd_" << n << ");\n";
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  /*INST*/
  void h_instaltend(OMPragma* p, ostream& os) {
    OMPRegion* r = RTop(p);
    string cname = p->find_sub_name();
    if ( cname != r->sub_name  ) {
      cerr << infile << ":" << r->begin_first_line
             << ": ERROR: missing inst end(" << r->sub_name
             << ") pragma/directive\n";
      cerr << infile << ":" << p->lineno
             << ": ERROR: non-matching inst end(" << cname
             << ") pragma/directive\n";
      cleanup_and_exit();
    }
    if ( lang & L_FORTRAN )
      os << "      call POMP_End(" << r->id << ")\n";
    else
      os << "POMP_End(&omp_rd_" << r->id << ");\n";
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  /*INST*/
  void h_instend(OMPragma* p, ostream& os) {
    p->name = "endregion";
    OMPRegion* r = RTop(p);
    string cname = p->find_sub_name();
    int n = RExit(p);
    if ( cname != r->sub_name  ) {
      cerr << infile << ":" << r->begin_first_line
             << ": ERROR: missing inst end(" << r->sub_name
             << ") pragma/directive\n";
      cerr << infile << ":" << p->lineno
             << ": ERROR: non-matching inst end(" << cname
             << ") pragma/directive\n";
      cleanup_and_exit();
    }
    if ( lang & L_FORTRAN )
      os << "      call POMP_End(" << n << ")\n";
    else
      os << "POMP_End(&omp_rd_" << n << ");\n";
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  /*INST*/
  void h_instrument(OMPragma* p, ostream& os) {
    do_transform = true;
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  /*INST*/
  void h_noinstrument(OMPragma* p, ostream& os) {
    do_transform = false;
    if ( keepSrcInfo ) reset_src_info(p, os);
  }

  void h_cxx_end(OMPragma* p, ostream& os) {
    if ( atomicRegion ) {
      extra_handler(p->lineno-p->pline, os);
    } else {
      OMPRegion* r = RTop(p);
      phandler_t handler = find_handler("end"+r->name);
      handler(p, os);
    }
  }
}

/*
 * External interface functions
 */

void init_handler(const char* inf, const char* rcf, Language l, bool g) {
  // remember environment
  lang = l;
  keepSrcInfo = g;
  infile = inf;
  rcfile = rcf;

  readResourceFile(inf, rcf);

  // init handler table
  if ( enabled & C_OMP ) {
    if ( lang & L_FORTRAN ) {
      table["do"]           = h_do;
      table["enddo"]        = h_enddo;
      table["workshare"]    = h_workshare;    /*2.0*/
      table["endworkshare"] = h_endworkshare; /*2.0*/
      table["sections"]     = h_sections;
      table["section"]      = h_section;
      table["endsections"]  = h_endsections;
      if ( enabled & C_SINGLE ) {
        table["single"]     = h_single;
        table["endsingle"]  = h_endsingle;
      }
      if ( enabled & C_MASTER ) {
        table["master"]     = h_master;
        table["endmaster"]  = h_endmaster;
      }

      table["paralleldo"]           = h_paralleldo;
      table["endparalleldo"]        = h_endparalleldo;
      table["parallelsections"]     = h_parallelsections;
      table["endparallelsections"]  = h_endparallelsections;
      table["parallelworkshare"]    = h_parallelworkshare;    /*2.0*/
      table["endparallelworkshare"] = h_endparallelworkshare; /*2.0*/
    } else {
      table["for"]          = h_for;
      table["endfor"]       = h_endfor;
      table["sections"]     = h_sections_c;
      table["section"]      = h_section_c;
      table["endsection"]   = h_endsection_c;
      table["endsections"]  = h_endsections_c;
      if ( enabled & C_SINGLE ) {
        table["single"]     = h_single_c;
        table["endsingle"]  = h_endsingle_c;
      }
      if ( enabled & C_MASTER ) {
        table["master"]     = h_master;        // F version OK here
        table["endmaster"]  = h_endmaster_c;   // but not here
      }
  
      table["parallelfor"]         = h_parallelfor;
      table["endparallelfor"]      = h_endparallelfor;
      table["parallelsections"]    = h_parallelsections_c;
      table["endparallelsections"] = h_endparallelsections_c;

      table["$END$"]        = h_cxx_end;
    }
    table["parallel"]      = h_parallel;
    table["endparallel"]   = h_endparallel;
    if ( enabled & C_CRITICAL ) {
      table["critical"]    = h_critical;
      table["endcritical"] = h_endcritical;
    }

    table["barrier"]       = h_barrier;
    if ( enabled & C_FLUSH ) {
      table["flush"]       = h_flush;
    }
    if ( enabled & C_ATOMIC ) {
      table["atomic"]      = h_atomic;
    }
  }

  table["instinit"]      = h_inst;         /*INST*/
  table["instfinalize"]  = h_inst;         /*INST*/
  table["inston"]        = h_inst;         /*INST*/
  table["instoff"]       = h_inst;         /*INST*/
  table["instbegin"]     = h_instbegin;    /*INST*/
  table["instaltend"]    = h_instaltend;   /*INST*/
  table["instend"]       = h_instend;      /*INST*/

  table["instrument"]    = h_instrument;   /*INST*/
  table["noinstrument"]  = h_noinstrument; /*INST*/
}

void finalize_handler(const char* rcdir,
                      const char* incfile, const char* tabfile) {
  // check region stack
  if ( ! regStack.empty() ) {
    cerr << "ERROR: unbalanced pragma/directive nesting\n";
    print_regstack_top();
    cleanup_and_exit();
  }

  // update information about current file in file list
  File* myFile = fileList.back();
  if ( myFile->first > IDused ) {
    myFile->first = 0;
    myFile->last  = 0;
  } else {
    myFile->last  = IDused;
  }

  // update resource file
  ofstream rcs(rcfile);
  if ( !rcs ) {
    cerr << "ERROR: cannot open opari resource file " << rcfile << "\n";
    exit(1);
  }
  rcs << "<OPARIRC 1.0>\n";
  rcs << IDused << "\n";
  for (unsigned i=0; i<fileList.size(); ++i) {
    rcs << fileList[i]->name << "\n";
    rcs << fileList[i]->first << " "
        << fileList[i]->last  << " "
        << static_cast<int>(fileList[i]->lang) << "\n";
  }

  // generate opari include file
  ofstream incs(incfile);
  if ( !incs ) {
    cerr << "ERROR: cannot open opari include file " << incfile << "\n";
    exit(1);
  }

  OMPRegion::generate_header(incs);
  if ( regions.size() ) {
    for (unsigned i=0; i<regions.size(); ++i)
#ifdef OPARI_VT
      regions[i]->generate_descr(incs, lang);
#else // OPARI_VT
      regions[i]->generate_descr(incs);
#endif // OPARI_VT
  }

  // generate opari table file
  if ( tabfile ) writeTableFile(rcdir, tabfile);

  // free file list
  for (unsigned i=0; i<fileList.size(); ++i) {
    delete fileList[i];
  }
}

void generateTableFile(const char* rcdir,
                       const char* rcf, const char* tabfile) {
  rcfile = rcf;
  readResourceFile(0, rcf);

  // generate opari table file
  writeTableFile(rcdir, tabfile);

  // free file list
  for (unsigned i=0; i<fileList.size(); ++i) {
    delete fileList[i];
  }
}

phandler_t find_handler(const string& pragma) {
  htab::iterator it = table.find(pragma);
  if ( it != table.end() )
    return it->second;
  else
    return print_pragma;
}

void extra_handler(int lineno, ostream& os) {
  if ( atomicRegion ) {
    atomicRegion->end_first_line = lineno;
    atomicRegion->end_last_line  = lineno;
    generate_call("exit", "atomic", atomicRegion->id, os);
    if ( keepSrcInfo ) {
      os << "#line " << (lineno+1) << " \"" << infile << "\"" << "\n";
    }
    atomicRegion = 0;
  }
}

bool set_disabled(const string& constructs) {
  string::size_type pos = 0 ;
  string::size_type last = 0 ;
  unsigned c;
  string s;

  do {
    pos = constructs.find_first_of(",", last);
    if ( pos < string::npos ) {
      s = constructs.substr(last, pos-last);
      if ( (c = string2construct(s)) == C_NONE ) {
        cerr << "ERROR: unknown value \'" << s << "\' for option -disable\n";
        return true;
      } else {
        enabled &= ~c;
      }
      last = pos + 1;
    }
  } while ( pos < string::npos );

  s = constructs.substr(last);
  if ( (c = string2construct(s)) == C_NONE ) {
    cerr << "ERROR: unknown value \'" << s << "\' for option -disable\n";
    return true;
  } else {
    enabled &= ~c;
  }
 
  return false;
}

bool instrument_locks() {
  return enabled & C_LOCKS;
}

bool genLineStmts() {
  return keepSrcInfo;
}

void print_regstack_top() {
  OMPRegion* rt = regStack.top();
  cerr << "       near OpenMP " << rt->name << " construct at "
       << rt->file_name << ":" << rt->begin_first_line;
  if ( rt->begin_first_line != rt->begin_last_line )
    cerr << "-" << rt->begin_last_line << "\n";
  else
    cerr << "\n";
}
