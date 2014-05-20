/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_LIBWRAPGEN_H_
#define _VT_LIBWRAPGEN_H_

#include "vt_libwrapgen_defs.h"
#include "vt_libwrapgen_filter.h"
#include "vt_libwrapgen_parser.h"

#include "vt_inttypes.h"

#include <fstream>
#include <map>
#include <string>
#include <vector>

#include <limits.h>
#ifndef PATH_MAX
# ifdef _POSIX_PATH_MAX
#   define PATH_MAX _POSIX_PATH_MAX
# else // _POSIX_PATH_MAX
#   define PATH_MAX 256
# endif // _POSIX_PATH_MAX
#endif // PATH_MAX

// typedef for generator modes
//
typedef enum
{
  MODE_GENSRC,  // generates wrapper source file
  MODE_BUILDLIB // builds wrapper library from generated source file
} GenModeT;

// data structure for program parameters
//
struct ParamsS
{
  ParamsS()
  : output_dir("."),
    verbose_level(1), mode(MODE_GENSRC),
    g_output_srcfile(VT_LIBWRAPGEN_DEFAULT_OUTPUT_SRC_FILE),
    g_cpp_cmd(VT_LIBWRAPGEN_DEFAULT_CPP),
    g_use_cpp(true), g_keep_cpp_file(false),
    b_output_libprefix(VT_LIBWRAPGEN_DEFAULT_OUTPUT_LIB_PREFIX),
    b_libtool_cmd(""),
    b_cc_cmd(VT_LIBWRAPGEN_DEFAULT_CC),
    b_cc_flags(VT_LIBWRAPGEN_DEFAULT_CFLAGS),
    b_shared(false), b_static(false) {}

  // whole command line
  std::string              command_line;

  // output directory
  std::string              output_dir;

  // command line parameters
  //

  // general
  uint32_t                 verbose_level;
  GenModeT                 mode;

  // generate
  bool                     g_progress;
  std::string              g_output_srcfile;
  std::string              g_input_filtfile;
  std::string              g_group;
  std::vector<std::string> g_input_headers;
  std::vector<std::string> g_allowed_sysheaders;
  std::vector<std::string> g_shlibs;
  // CTool
  std::string              g_cpp_cmd;
  std::string              g_cpp_flags;
  std::string              g_cpp_dir;
  bool                     g_use_cpp;
  bool                     g_keep_cpp_file;

  // build
  std::string              b_output_libprefix;
  std::string              b_input_srcfile;
  std::string              b_libtool_cmd;
  std::string              b_libtool_flags;
  std::string              b_cc_cmd;
  std::string              b_cc_flags;
  std::string              b_ld_cmd;
  std::string              b_ld_flags;
  std::string              b_libs;
  bool                     b_shared;
  bool                     b_static;

};

// Generator class
//
class GeneratorC
{
  friend class ParserC;

public:

  // contructor
  GeneratorC();

  // destructor
  ~GeneratorC();

  // generate library wrapper source file
  bool genSource( void );

  // build wrapper library from generated source file
  bool buildLib( void );

private:

  // data structure for a function to be generated
  //
  struct FuncS
  {
    std::string name;
    std::string rettype;

    struct LocS
    {
      std::string file;
      uint32_t    line;
    } loc;

    struct ArgS
    {
      std::string name;
      std::string type;
      std::string type_base;
      std::string type_before;
      std::string type_after;
    };
    std::vector<ArgS> args;

    bool noret;
  };

  // write head of wrapper source file
  void writeHead( void );

  // write wrapper function to source file
  void writeFunction( const FuncS& func );

  // remove a non-empty directory (like 'rm -rf')
  bool removeDir( const std::string& path );

  // pointer to instance of class Filter
  FilterC* m_filter;

  // pointer to instance of class Parser
  ParserC* m_parser;

  // output file stream of wrapper source file
  std::ofstream m_outputStream;

  // map of already generated functions
  std::map<std::string, bool> genFuncs;

};

extern void VPrint( uint8_t level, const char * fmt, ... );

// global variables
//

// name of program's executable 
extern const std::string ExeName;

// program parameters
extern ParamsS           Params;

// instance of class Generator
extern GeneratorC*       theGenerator;

#endif // _VT_LIBWRAPGEN_H_
