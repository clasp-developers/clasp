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

#include "vt_inttypes.h"

#include "util/installdirs.h"
#include "util/util.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <map>
#include <queue>
#include <string>
#include <vector>

#include <fnmatch.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

// types, enumerations, and structures
//

//
// languages types
//
typedef enum
{
  LANG_CC,  // C
  LANG_CXX, // C++
  LANG_FC   // Fortran
} LangTypeT;

//
// instrumentation types
//
typedef enum
{
  INST_TYPE_COMPINST = 0x1, // auto. instr. by compiler
  INST_TYPE_MANUAL   = 0x2, // manual instr. by VT API
  INST_TYPE_DYNINST  = 0x4, // binary instrumentation by Dyninst
  INST_TYPE_TAUINST  = 0x8  // auto. source code instr. by TAU
} InstTypeT;

//
// flags for the "showme" mode
//
enum
{
  SHOWME_FLAG_ALL     = 0x1, // show all commands that would be executed
  SHOWME_FLAG_COMPILE = 0x2, // show the compiler flags
  SHOWME_FLAG_LINK    = 0x4  // show the linker flags
};

//
// data structure for source files to be modified by OPARI and/or TAU
//
struct ModFileS
{
  ModFileS( const std::string& _src_file, const std::string& _obj_file,
            const int _action_flags )
    : src_file( _src_file ), obj_file( _obj_file ),
      action_flags( _action_flags ) {}

  //
  // flags for modification actions to perform on source file
  //
  enum
  {
    MOD_ACTION_FLAG_OPARI   = 0x1, // OPARI instrumentation
    MOD_ACTION_FLAG_TAUINST = 0x2  // TAU instrumentation
  };

  std::string src_file; // source file name
  std::string obj_file; // object file name

  int action_flags;     // modification action flags

};

//
// data structure for wrapper configuration
//
struct ConfigS
{
  ConfigS()
    : lang_type( LANG_CC ), inst_type( INST_TYPE_MANUAL ), inst_avail( 0 ),
      showme_flags( 0 ), be_verbose( false ), keep_files( false ),
      reuse_files( false ), comp_only( false ), outfile_given( false ),
      uses_mpi( false ), uses_threads( false ), uses_openmp( false ),
      preprocess( false ), opari_rcfile( DEFAULT_OPARI_RCFILE() ),
      opari_tabfile( DEFAULT_OPARI_TABFILE() ), opari_keep_rcfile( false ) {}

  // set language type
  inline bool setLanguage( const LangTypeT lang );

  // is language type set to Fortran?
  inline bool fortran() const;

  // set compiler command
  inline void setCompilerCmd( const std::string& cmd );

  // add compiler argument(s)
  inline void addCompilerArg( const std::string& arg );

  // add library to link
  inline void addCompilerLib( const std::string& lib );

  // add source file to be modified by OPARI and/or TAU
  inline void addModSrcFile( const std::string& file );

  // set OPARI table file
  inline void setOpariTabFile( const std::string& file );

  // set OPARI rc file
  inline void setOpariRcFile( const std::string& file );

  // add/set OPARI argument(s)
  inline void addOpariArg( const std::string& arg );

  // add/set TAU instumentor argument(s)
  inline void addTauinstArg( const std::string& arg );

  // add/set TAU parser argument(s)
  inline void addTauinstParseArg( const std::string& arg );

  // add/set C preprocessor flag(s)
  inline void addPrepFlag( const std::string& flag );

  // set flag for MPI usage
  inline void setUsesMpi( bool set, bool ovwrt = false );

  // set flag for Thread usage
  inline void setUsesThreads( bool set, bool ovwrt = false );

  // set flag for OpenMP usage
  inline void setUsesOpenMP( bool set, bool ovwrt = false );

  // set available instrumentation type
  inline bool setInstAvail( const std::string& type );

  // set instrumentation type
  //
  inline bool setInstType( const InstTypeT type );
  inline bool setInstType( const std::string& type );

  // get instrumentation type
  inline InstTypeT getInstType() const;

  // get name of instrumentation type
  inline std::string getInstTypeName() const;

  // is instrumentation type available?
  inline bool isInstAvail( const InstTypeT type ) const;

  // is source file excluded from instrumentation?
  inline bool isFileExcluded( const std::vector<std::string>& excls,
                const std::string& file ) const;

  // read exclusion file and store its content in 'excls'
  bool readExclFile( const std::string& file,
         std::vector<std::string>& excls ) const;

  static const std::string DEFAULT_OPARI_RCFILE() { return "opari.rc"; }
  static const std::pair<std::string, std::string> DEFAULT_OPARI_TABFILE()
    { return std::make_pair( "opari.tab.c", "opari.tab.o" ); }

  LangTypeT   lang_type;             // language type
  InstTypeT   inst_type;             // instrumentation type
                                     // (e.g. compinst, manual, ...)
  std::vector<std::string>
    inst_excl_files;                 // source files to be excluded from
                                     // automatic instrumentation by the
                                     // compiler or PDT/TAU

  int         inst_avail;            // bitmask for available instr.-types
  int         showme_flags;          // bitmask for showme flags
  bool        be_verbose;            // Flag: be verbose?
  bool        keep_files;            // Flag: remove intermediate files?
  bool        reuse_files;           // Flag: reuse intermediate files?
  bool        comp_only;             // Flag: compile only?
  bool        outfile_given;         // Flag: output file given?
  bool        uses_mpi;              // Flag: uses MPI?
  bool        uses_threads;          // Flag: uses Threads?
  bool        uses_openmp;           // Flag: uses OpenMP? (use OPARI)

  std::string vt_version;            // VT version

  std::string vt_incdir;             // VT's include directory
  std::string vt_libdir;             // VT's library directory

  std::string vt_seqlib;             // VT-library for sequential programs
  std::string vt_mpilib;             // VT-library for MPI programs
  std::string vt_mtlib;              // VT-library for multithreading programs
  std::string vt_hyblib;             // VT-library for hybrid (MPI/Threads)
                                     // programs
  std::string vt_pomplib;            // VT's POMP library
  std::string vt_dynattlib;          // VT's Dyninst attach library

  std::string prep_cmd;              // C preprocessor command
  std::string prep_flags;            // C preprocessor flags
  bool        preprocess;            // preprocess source files before parsing
                                     // by OPARI and/or PDT

  std::string comp_cmdenv;           // compiler command env. name
  std::string comp_flagsenv;         // compiler flags env. name
  std::string comp_cmd;              // compiler command
  std::string comp_args;             // compiler arguments
  std::string comp_flags;            // compiler flags
  std::string comp_ldflags;          // linker flags
  std::string comp_fdflag;           // flag to define preprocessor macro
  std::string comp_instflags;        // compiler instrumentation flags
  std::string comp_libs;             // libraries to link

  std::string opari_cmd;             // OPARI command
  std::string opari_args;            // OPARI arguments
  std::string opari_rcfile;          // OPARI's rc file
  std::pair<std::string, std::string>
    opari_tabfile;                   // OPARI's table source/object file
  std::string opari_tab_compcmd;     // compiler command for OPARI's table file
  std::string opari_tab_compflags;   // compiler flags for OPARI's table file
  bool        opari_keep_rcfile;     // Flag: don't delete OPARI's rc file?
  std::vector<std::string>
    opari_excl_files;                // source files to be excluded from OPARI
                                     // instrumentation

  std::string compinst_flags;        // compiler flags to enable instrumentation
  std::string dyninst_flags;         // compiler flags to produce debugging
                                     // information (needed for binary inst.)

  std::string tauinst_cmd;           // TAU instrumentor command
  std::string tauinst_args;          // TAU instrumentor arguments
  std::string tauinst_parsecmd;      // PDT source code parser command
  std::string tauinst_parseargs;     // PDT parser arguments

  std::vector<ModFileS>
    mod_files;                       // source files to be modified by OPARI
                                     // and/or TAU

};

// function declarations
//

// read wrapper's data file
bool readDataFile( void );

// read environment variables
bool readEnvironmentVars( void );

// parse command line parameters
bool parseCommandLine( int argc, char** argv );

// wrap compiler command
int doWrap( void );

// show compiler/linker flags
void showFlags( void );

// show usage text
void showUsage( void );

// show or execute command
int showOrExecuteCommand( std::string& cmd );

// get OPARI generated include files from table file
// (only necessary for Fortran)
void getIncFilesFromTabFile( std::vector<std::string>& incfiles );

// is file name a CUDA (*.cu) file?
inline bool isCuFile( const std::string& file );

// add string to string-list or reset string-list to the given string
inline void addOrSetStringList( std::string& list, const std::string& str,
  bool reset = false );

// remove leading, trailing, and double spaces from a string
inline void trimString( std::string& str );

// global variables
//

// name of program's executable
char* ExeName;

// wrapper configuration
ConfigS Config;

int
main( int argc, char** argv )
{
  int rc = 0;

  // get name of program's executable
  //
  if( ( ExeName = strrchr( argv[0], '/' ) ) )
    ExeName++;
  else
    ExeName = argv[0];

  // read wrapper's data file
  //
  if( !readDataFile() )
    return 1;

  // read environment variables
  //
  if( !readEnvironmentVars() )
    return 1;

  // parse command line parameters
  //
  if( !parseCommandLine( argc, argv ) )
    return 1;

  // either wrap compiler command or show compiler/linker flags
  //
  if( Config.showme_flags == 0 || Config.showme_flags == SHOWME_FLAG_ALL )
  {
    rc = doWrap();
  }
  else
  {
    showFlags();
  }

  return rc;
}

bool
readDataFile()
{
  bool error = false;

  const std::string data_file =
    std::string( vt_installdirs_get( VT_INSTALLDIR_DATADIR ) ) + "/" +
    std::string( ExeName ) + "-wrapper-data.txt";

  const uint32_t keys_num = 31;
  const std::string keys[] = {
    "version", "language", "compiler_env", "compiler_flags_env",
    "compiler", "compiler_flags", "linker_flags", "libs", "preprocessor",
    "preprocessor_flags", "includedir", "libdir", "vtlib", "vtmpilib",
    "vtmtlib", "vthyblib", "vtpomplib", "vtdynattlib", "opari_bin",
    "opari_opts", "opari_tab_compiler", "opari_tab_compiler_flags",
    "compinst_compiler_flags", "dyninst_compiler_flags", "tauinst_bin",
    "tauinst_opts", "tauinst_parse_bin", "tauinst_parse_opts", "inst_avail",
    "inst_default", "partype_default"
  };

  std::ifstream in( data_file.c_str() );
  if( !in )
  {
    std::cerr << ExeName << ": Error: Could not open configuration file "
              << data_file << ". Aborting." << std::endl;
    return false;
  }

  char buffer[1024];
  std::string line;
  uint32_t line_no = 0;
  uint32_t key_idx = 0;

  while( !error && key_idx < keys_num &&
         in.getline( buffer, sizeof( buffer ) ) )
  {
    line_no++;

    if( buffer[0] == '#' || buffer[0] == '\n' || buffer[0] == '\0' )
      continue;
    if( buffer[strlen(buffer)-1] == '\n' )
      buffer[strlen(buffer)-1] = '\0';

    line = buffer;

    std::string::size_type valpos = line.find( '=' );
    std::string key;
    std::string value;

    if( valpos == std::string::npos || valpos < 1 )
    {
      std::cerr << ExeName << ": "
                << data_file << ":" << line_no << ": "
                << "could not be parsed" << std::endl;
      error = true;
      break;
    }

    key = line.substr( 0, valpos );
    value = line.substr( valpos + 1 );

    if( key.compare( keys[key_idx++] ) != 0 )
    {
      std::cerr << ExeName << ": "
                << data_file << ":" << line_no << ": "
                << "unexpected key '"
                << key << "'" << std::endl;
      error = true;
      break;
    }

    // expand install directories from value except keys 1-4
    //
    if( key_idx > 4 )
    {
      char* tmp = vt_installdirs_expand( value.c_str() );
      if( !tmp )
      {
        std::cerr << ExeName << ": "
                  << data_file << ":" << line_no << ": "
                  << "could not be parsed" << std::endl;
        error = true;
        break;
      }

      value = tmp;
      free( tmp );
    }

    switch( key_idx )
    {
      case 1: // version
      {
        Config.vt_version = value;
        break;
      }
      case 2: // language
      {
        if( value.compare( "C" ) == 0 )
          error = !Config.setLanguage( LANG_CC );
        else if( value.compare( "C++" ) == 0 )
          error = !Config.setLanguage( LANG_CXX );
        else if( value.compare( "Fortran" ) == 0 )
          error = !Config.setLanguage( LANG_FC );
        else
        {
          std::cerr << ExeName << ": "
                     << data_file << ":" << line_no << ": "
                     << "unknown language '" << value << "'" << std::endl;
          error = true;
        }
        break;
      }
      case 3: // compiler_env
      {
        Config.comp_cmdenv = value;
        break;
      }
      case 4: // compiler_flags_env
      {
        Config.comp_flagsenv = value;
        break;
      }
      case 5: // compiler
      {
        Config.setCompilerCmd( value );
        break;
      }
      case 6: // compiler_flags
      {
        Config.comp_flags = value;
        break;
      }
      case 7: // linker flags
      {
        Config.comp_ldflags = value;
        break;
      }
      case 8: // libs
      {
        Config.comp_libs = value;
        break;
      }
      case 9: // preprocessor
      {
        Config.prep_cmd = value;
        break;
      }
      case 10: // preprocessor flags
      {
        Config.prep_flags = value;
        break;
      }
      case 11: // includedir
      {
        Config.vt_incdir = "-I" + value;
        break;
      }
      case 12: // libdir
      {
        Config.vt_libdir = "-L" + value;
        break;
      }
      case 13: // vtlib
      {
        Config.vt_seqlib = value;
        break;
      }
      case 14: // vtmpilib
      {
        Config.vt_mpilib = value;
        break;
      }
      case 15: // vtmtlib
      {
        Config.vt_mtlib = value;
        break;
      }
      case 16: // vthyblib
      {
        Config.vt_hyblib = value;
        break;
      }
      case 17: // vtpomplib
      {
        Config.vt_pomplib = value;
        break;
      }
      case 18: // vtdynattlib
      {
        Config.vt_dynattlib = value;
        break;
      }
      case 19: // opari_bin
      {
        Config.opari_cmd = value;
        break;
      }
      case 20: // opari_opts
      {
        Config.opari_args = value;
        break;
      }
      case 21: // opari_tab_compiler
      {
        Config.opari_tab_compcmd = value;
        break;
      }
      case 22: // opari_tab_compiler_flags
      {
        Config.opari_tab_compflags = value;
        break;
      }
      case 23: // compinst_compiler_flags
      {
        Config.compinst_flags = value;
        break;
      }
      case 24: // dyninst_compiler_flags
      {
        Config.dyninst_flags = value;
        break;
      }
      case 25: // tauinst_bin
      {
        Config.tauinst_cmd = value;
        break;
      }
      case 26: // tauinst_opts
      {
        Config.tauinst_args = value;
        break;
      }
      case 27: // tauinst_parse_bin
      {
        Config.tauinst_parsecmd = value;
        break;
      }
      case 28: // tauinst_parse_opts
      {
        Config.tauinst_parseargs = value;
        break;
      }
      case 29: // inst_avail
      {
        char cvalue[128];
        strncpy( cvalue, value.c_str(), sizeof( cvalue ) - 1 );
        cvalue[sizeof( cvalue ) - 1]  = '\0';

        char* token = strtok( cvalue, " " );
        if( !token )
        {
          std::cerr << ExeName << ": "
                    << data_file << ":" << line_no << ": "
                    << "could not be parsed" << std::endl;
          error = true;
          break;
        }

        do
        {
          if( !Config.setInstAvail( token ) )
          {
            std::cerr << ExeName << ": "
                      << data_file << ":" << line_no << ": "
                      << "unknown instrumentation type '"
                      << token << "'" << std::endl;
            error = true;
            break;
          }
        } while( ( token = strtok( 0, " " ) ) );

        break;
      }
      case 30: // inst_default
      {
        if( !Config.setInstType( value ) )
        {
          std::cerr << ExeName << ": "
                    << data_file << ":" << line_no << ": "
                    << "unknown or not supported instrumentation type '"
                    << value << "'" << std::endl;
          error = true;
        }
        break;
      }
      case 31: // partype_default
      default:
      {
        if( value.compare( "seq" ) == 0 )
        {
        }
        else if( value.compare( "mpi" ) == 0 )
        {
          Config.setUsesMpi( true );
        }
        else if( value.compare( "mt" ) == 0 )
        {
          Config.setUsesThreads( true );
        }
        else if( value.compare( "hyb" ) == 0 )
        {
          Config.setUsesMpi( true );
          Config.setUsesThreads( true );
        }
        else
        {
          std::cerr << ExeName << ": "
                    << data_file << ":" << line_no << ": "
                    << "unknown parallelization type '"
                    << value << "'" << std::endl;
          error = true;
        }
        break;
      }
    }
  }

  in.close();

  if( !error && key_idx < keys_num )
  {
    std::cerr << ExeName << ": "
              << data_file << ": "
              << "unexpected end of file" << std::endl;
    return false;
  }

  return !error;
}

bool
readEnvironmentVars()
{
  char* env;

  // read environment var. for compiler command
  // (VT_<CC|CXX|FC>)
  //
  env = getenv( Config.comp_cmdenv.c_str() );
  if( env ) Config.setCompilerCmd( env );

  // read environment var. for extra compiler flags
  // (VT_<C|CXX|F|FC>FLAGS)
  env = getenv( Config.comp_flagsenv.c_str() );
  if( env )
  {
    if( Config.comp_flags.length() > 0 )
      Config.comp_flags += " ";
    Config.comp_flags += env;
  }

  // read environment var. for extra linker flags
  // (VT_LDFLAGS)
  env = getenv( "VT_LDFLAGS" );
  if( env ) Config.comp_ldflags = env;

  // read environment var. for extra libs
  // (VT_LIBS)
  env = getenv( "VT_LIBS" );
  if( env )
  {
    if( Config.comp_libs.length() > 0 )
      Config.comp_libs += " ";
    Config.comp_libs += env;
  }

  // read environment var. for instrumentation type
  // (VT_INST)
  //
  env = getenv( "VT_INST" );
  if( env )
  {
    if( !Config.setInstType( env ) )
    {
      std::cerr << ExeName << ": Error: VT_INST: "
                << "Unknown or not supported instrumentation type '"
                << env << "'. Aborting." << std::endl;
      return false;
    }
  }

  return true;
}

bool
parseCommandLine( int argc, char** argv )
{
  std::vector<std::string> args;
  std::string arg;
  uint32_t i;

  // pre-process arguments
  //
  for( i = 1; i < (uint32_t)argc; i++ )
  {
    arg = argv[i];

    // we also accept "--vt:" - modify "--vt:" to "-vt:"
    //
    if( arg.compare( 0, 5, "--vt:" ) == 0 )
    {
      arg.erase( 0, 1 );
    }

    // merge separated compiler arguments
    // -<I|D|L|l> <dir|lib>
    //             ^
    if( arg.compare( "-I" ) == 0 ||
        arg.compare( "-D" ) == 0 ||
        arg.compare( "-L" ) == 0 ||
        arg.compare( "-l" ) == 0 )
    {
      if( i < (uint32_t)argc - 1 )
        arg += argv[++i];
    }

    args.push_back( arg );
  }

  for( i = 0; i < args.size(); i++ )
  {
    arg = args[i];

    // -vt:help
    //
    if( arg.compare( "-vt:help" ) == 0 )
    {
      showUsage();
      exit(0);
    }

    // -vt:version
    //
    if( arg.compare( "-vt:version" ) == 0 )
    {
      std::cout << Config.vt_version << std::endl;
      exit(0);
    }
    // -vt:show, -vt:showme
    //
    else if( arg.compare( "-vt:show" ) == 0 ||
             arg.compare( "-vt:showme" ) == 0 )
    {
      Config.showme_flags = SHOWME_FLAG_ALL;
    }
    // -vt:showme-compile
    //
    else if( arg.compare( "-vt:showme-compile" ) == 0 )
    {
      if( ( Config.showme_flags & SHOWME_FLAG_ALL ) != 0 )
        Config.showme_flags ^= SHOWME_FLAG_ALL;
      Config.showme_flags |= SHOWME_FLAG_COMPILE;
    }
    // -vt:showme-link
    //
    else if( arg.compare( "-vt:showme-link" ) == 0 )
    {
      if( ( Config.showme_flags & SHOWME_FLAG_ALL ) != 0 )
        Config.showme_flags ^= SHOWME_FLAG_ALL;
      Config.showme_flags |= SHOWME_FLAG_LINK;
    }
    // -vt:verbose
    //
    else if( arg.compare( "-vt:verbose" ) == 0 )
    {
      Config.be_verbose = true;
    }
    // -vt:keepfiles
    //
    else if( arg.compare( "-vt:keepfiles" ) == 0 )
    {
      Config.keep_files = true;
    }
    // -vt:nocleanup
    //
    else if( arg.compare( "-vt:nocleanup" ) == 0 )
    {
      std::cerr << ExeName << ": Warning: The option -vt:nocleanup is "
                << "deprecated, please use -vt:keepfiles instead."
                << std::endl;
      Config.keep_files = true;
    }
    // -vt:reusefiles
    //
    else if( arg.compare( "-vt:reusefiles" ) == 0 )
    {
       Config.reuse_files = true;
    }
    // -vt:inst <type>
    //
    else if( arg.compare( "-vt:inst" ) == 0 )
    {
      if( i == args.size() - 1 )
      {
        std::cerr << ExeName << ": <type> expected -- -vt:inst"
                  << std::endl;
        return false;
      }

      arg = args[++i];

      if( !Config.setInstType( arg ) )
      {
        std::cerr << ExeName << ": "
                  << "unknown or not supported instrumentation type '"
                  << arg << "'" << std::endl;
        return false;
      }
    }
    // -vt:inst-exclude-file-list
    //
    else if( arg.compare( "-vt:inst-exclude-file-list" ) == 0 )
    {
      if( i == args.size() - 1 )
      {
        std::cerr << ExeName << ": <file> expected -- "
                  << "-vt:inst-exclude-file-list" << std::endl;
        return false;
      }

      size_t excl_file_list_len = args[++i].length()+1;
      char* excl_file_list = new char[excl_file_list_len];
      strncpy( excl_file_list, args[i].c_str(), excl_file_list_len - 1 );
      excl_file_list[excl_file_list_len - 1] = '\0';

      char* token = strtok( excl_file_list, "," );
      do
      {
        std::string file = token;
        trimString( file );
        if( file.length() > 0 )
          Config.inst_excl_files.push_back( file );
      } while( ( token = strtok( 0, "," ) ) );

      delete [] excl_file_list;
    }
    // -vt:inst-exclude-file
    //
    else if( arg.compare( "-vt:inst-exclude-file" ) == 0 )
    {
      if( i == args.size() - 1 )
      {
        std::cerr << ExeName << ": <file> expected -- "
                  << "-vt:inst-exclude-file" << std::endl;
        return false;
      }

      if( !Config.readExclFile( args[++i], Config.inst_excl_files ) )
        return false;
    }
    // -vt:opari <args>
    //
    else if( arg.compare( "-vt:opari" ) == 0 )
    {
      if( i == args.size() - 1 )
      {
        std::cerr << ExeName << ": <args> expected -- -vt:opari"
                  << std::endl;
        return false;
      }

      i++;
      if( args[i][0] == '!' )
      {
        Config.opari_args = "";
        Config.opari_rcfile = ConfigS::DEFAULT_OPARI_RCFILE();
        Config.opari_tabfile = ConfigS::DEFAULT_OPARI_TABFILE();
      }

      size_t opari_args_len = args[i].length()+1;
      char* opari_args = new char[opari_args_len];
      strncpy( opari_args, args[i].c_str(), opari_args_len - 1 );
      opari_args[opari_args_len - 1] = '\0';

      char* token = strtok( opari_args, " " );
      do
      {
        if( strcmp( token, "-rcfile" ) == 0 )
        {
          token = strtok( 0, " " );
          if( !token )
          {
            std::cerr << ExeName << ": <rcfile> expected -- -rcfile"
                      << std::endl;
            delete [] opari_args;
            return false;
          }

          Config.setOpariRcFile( token );
        }
        else if( strcmp( token, "-table" ) == 0 )
        {
          token = strtok( 0, " " );
          if( !token )
          {
            std::cerr << ExeName << ": <tabfile> expected -- -table"
                      << std::endl;
            delete [] opari_args;
            return false;
          }

          Config.setOpariTabFile( token );
        }
        else
        {
          Config.addOpariArg( token );
        }
      } while( ( token = strtok( 0, " " ) ) );

      delete [] opari_args;
    }
    // -vt:opari-rcfile
    //
    else if( arg.compare( "-vt:opari-rcfile" ) == 0 )
    {
      if( i == args.size() - 1 )
      {
        std::cerr << ExeName << ": <file> expected -- -vt:opari-rcfile"
                  << std::endl;
        return false;
      }

      Config.setOpariRcFile( args[++i] );
    }
    // -vt:opari-table
    //
    else if( arg.compare( "-vt:opari-table" ) == 0 )
    {
      if( i == args.size() - 1 )
      {
        std::cerr << ExeName << ": <file> expected -- -vt:opari-table"
                  << std::endl;
        return false;
      }

      Config.setOpariTabFile( args[++i] );
    }
    // -vt:opari-exclude-file-list
    //
    else if( arg.compare( "-vt:opari-exclude-file-list" ) == 0 )
    {
      if( i == args.size() - 1 )
      {
        std::cerr << ExeName << ": <file> expected -- "
                  << "-vt:opari-exclude-file-list" << std::endl;
        return false;
      }

      size_t excl_file_list_len = args[++i].length()+1;
      char* excl_file_list = new char[excl_file_list_len];
      strncpy( excl_file_list, args[i].c_str(), excl_file_list_len - 1 );
      excl_file_list[excl_file_list_len - 1] = '\0';

      char* token = strtok( excl_file_list, "," );
      do
      {
        std::string file = token;
        trimString( file );
        if( file.length() > 0 )
          Config.opari_excl_files.push_back( file );
      } while( ( token = strtok( 0, "," ) ) );

      delete [] excl_file_list;
    }
    // -vt:opari-exclude-file
    //
    else if( arg.compare( "-vt:opari-exclude-file" ) == 0 )
    {
      if( i == args.size() - 1 )
      {
        std::cerr << ExeName << ": <file> expected -- "
                  << "-vt:opari-exclude-file" << std::endl;
        return false;
      }

      if( !Config.readExclFile( args[++i], Config.opari_excl_files ) )
        return false;
    }
    // -vt:noopari
    //
    else if( arg.compare( "-vt:noopari" ) == 0 )
    {
      Config.setUsesOpenMP( false, true );
    }
    // -vt:tau <args>
    //
    else if( arg.compare( "-vt:tau" ) == 0 )
    {
      if( i == args.size() - 1 )
      {
        std::cerr << ExeName << ": <args> expected -- -vt:tau"
                  << std::endl;
        return false;
      }

      Config.addTauinstArg( args[++i] );
    }
    // -vt:pdt <args>
    //
    else if( arg.compare( "-vt:pdt" ) == 0 )
    {
      if( i == args.size() - 1 )
      {
        std::cerr << ExeName << ": <args> expected -- -vt:pdt"
                  << std::endl;
        return false;
      }

      Config.addTauinstParseArg( args[++i] );
    }
    // -vt:preprocess
    //
    else if( arg.compare( "-vt:preprocess" ) == 0 )
    {
      Config.preprocess = true;
    }
    // -vt:cpp
    //
    else if( arg.compare( "-vt:cpp" ) == 0 )
    {
      if( i == args.size() - 1 )
      {
        std::cerr << ExeName << ": <cmd> expected -- -vt:cpp"
                  << std::endl;
        return false;
      }

      Config.prep_cmd = args[++i];
    }
    // -vt:cppflags
    //
    else if( arg.compare( "-vt:cppflags" ) == 0 )
    {
      if( i == args.size() - 1 )
      {
        std::cerr << ExeName << ": <flags> expected -- -vt:cppflags"
                  << std::endl;
        return false;
      }

      Config.addPrepFlag( args[++i] );
    }
    // -vt:seq
    //
    else if( arg.compare( "-vt:seq" ) == 0 )
    {
      Config.setUsesMpi( false, true );
      Config.setUsesThreads( false, true );
      Config.setUsesOpenMP( false, true );
    }
    // -vt:mpi
    //
    else if( arg.compare( "-vt:mpi" ) == 0 )
    {
      Config.setUsesMpi( true, true );
      Config.setUsesThreads( false, true );
      Config.setUsesOpenMP( false, true );
    }
    // -vt:mt
    //
    else if( arg.compare( "-vt:mt" ) == 0 )
    {
      Config.setUsesMpi( false, true );
      Config.setUsesThreads( true, true );
    }
    // -vt:hyb
    //
    else if( arg.compare( "-vt:hyb" ) == 0 )
    {
      Config.setUsesMpi( true, true );
      Config.setUsesThreads( true, true );
    }
    // pthread flags/libs
    //
    else if( arg.compare( "-Kthread" ) == 0 ||
             arg.compare( "-kthread" ) == 0 ||
             arg.compare( "-pthread" ) == 0 ||
             arg.compare( "-pthreads" ) == 0 ||
             arg.compare( "-mthreads" ) == 0 ||
             arg.compare( "--thread-safe" ) == 0 ||
             arg.compare( "-lpthreads" ) == 0 ||
             arg.compare( "-llthread" ) == 0 ||
             arg.compare( "-lpthread" ) == 0 )
    {
      Config.setUsesThreads( true );
    }
    // openmp flag
    //
    else if( arg.compare( "-openmp" ) == 0 ||
             arg.compare( "-fopenmp" ) == 0 ||
             arg.compare( "-Popenmp" ) == 0 ||
             arg.compare( "-xopenmp" ) == 0 ||
             arg.compare( "-mp" ) == 0 ||
             arg.compare( "-homp" ) == 0 )
    {
      Config.setUsesThreads( true );
      Config.setUsesOpenMP( true );
    }
    else if( arg.length() > 6 && arg.compare( 0, 6, "-qsmp=" ) == 0 )
    {
      char carg[128];
      strncpy( carg, arg.substr(6).c_str(), sizeof( carg ) - 1 );
      carg[sizeof( carg ) - 1]  = '\0';

      char* token = strtok( carg, ":" );
      do
      {
        if( strcmp( token, "omp" ) == 0 )
        {
          Config.setUsesThreads( true );
          Config.setUsesOpenMP( true );
          break;
        }
      } while( ( token = strtok( 0, ":" ) ) );
    }
    else if( arg.compare( "-h" ) == 0 && i < args.size() - 1 &&
             args[i+1].compare( "omp" ) == 0 )
    {
      Config.setUsesThreads( true );
      Config.setUsesOpenMP( true );
      i++;
    }
    // nvcc's pthread/openmp flag
    //
    else if( arg.compare( 0, 10, "-Xcompiler" ) == 0 ||
             arg.compare( 0, 18, "--compiler-options" ) == 0 ||
             arg.compare( 0, 8, "-Xlinker" ) == 0 ||
             arg.compare( 0, 16, "--linker-options" ) == 0 )
    {
      if( arg.find( "-pthread" ) != std::string::npos )
        Config.setUsesThreads( true );

      if( arg.find( "-fopenmp" ) != std::string::npos )
      {
        Config.setUsesThreads( true );
        Config.setUsesOpenMP( true );
      }
    }
  }

  bool addlibs = false;

  for( i = 0; i < args.size(); i++ )
  {
    arg = args[i];

    // escape '\', '"', ''', ' ', '(', and ')'
    //
    const char special_char[] = "\\\"' ()";
    size_t found = arg.find_first_of( special_char );
    while( found != std::string::npos )
    {
      arg.insert( found, "\\" );
      found = arg.find_first_of( special_char, found + 2 );
    }

    // ignore arguments already processed above
    //
    if( arg.compare( "-vt:help" ) == 0 ||
        arg.compare( "-vt:version" ) == 0 ||
        arg.compare( "-vt:show" ) == 0 ||
        arg.compare( "-vt:showme" ) == 0 ||
        arg.compare( "-vt:showme-compile" ) == 0 ||
        arg.compare( "-vt:showme-link" ) == 0 ||
        arg.compare( "-vt:verbose" ) == 0 ||
        arg.compare( "-vt:keepfiles" ) == 0 ||
        arg.compare( "-vt:nocleanup" ) == 0 ||
        arg.compare( "-vt:reusefiles" ) == 0 ||
        arg.compare( "-vt:seq" ) == 0 ||
        arg.compare( "-vt:mpi" ) == 0 ||
        arg.compare( "-vt:mt" ) == 0 ||
        arg.compare( "-vt:hyb" ) == 0 ||
        arg.compare( "-vt:noopari" ) == 0 ||
        arg.compare( "-vt:preprocess" ) == 0 )
    {
      // do nothing
    }
    else if( arg.compare( "-vt:inst" ) == 0 ||
             arg.compare( "-vt:inst-exclude-file-list" ) == 0 ||
             arg.compare( "-vt:inst-exclude-file" ) == 0 ||
             arg.compare( "-vt:opari" ) == 0 ||
             arg.compare( "-vt:opari-rcfile" ) == 0 ||
             arg.compare( "-vt:opari-table" ) == 0 ||
             arg.compare( "-vt:opari-exclude-file-list" ) == 0 ||
             arg.compare( "-vt:opari-exclude-file" ) == 0 ||
             arg.compare( "-vt:tau" ) == 0 ||
             arg.compare( "-vt:pdt" ) == 0 ||
             arg.compare( "-vt:cpp" ) == 0 ||
             arg.compare( "-vt:cppflags" ) == 0 )
    {
      // skip next argument, if necessary
      i++;
    }
    // -vt:<cc|CC|c++|cxx|fc> <cmd>
    //
    else if( ( Config.lang_type == LANG_CC &&
               arg.compare( "-vt:cc" ) == 0 ) ||
             ( Config.lang_type == LANG_CXX &&
               ( arg.compare( "-vt:CC" ) == 0 ||
                 arg.compare( "-vt:c++" ) == 0 ||
                 arg.compare( "-vt:cxx" ) == 0 ) ) ||
             ( Config.lang_type == LANG_FC &&
               ( arg.compare( "-vt:fc" ) == 0 ||
                 arg.compare( "-vt:f77" ) == 0 ||
                 arg.compare( "-vt:f90" ) == 0 ) ) )
    {
      if( i == args.size() - 1 )
      {
        std::cerr << ExeName << ": <cmd> expected -- "
                  << arg << std::endl;
        return false;
      }

      Config.setCompilerCmd( args[++i] );
    }
    // -vt:*  -> unknown wrapper argument
    //
    else if( arg.compare( 0, 4, "-vt:" ) == 0 )
    {
      std::cerr << ExeName << ": unknown option -- "
                << arg << std::endl;
      return false;
    }
    // source file
    //
    else if( ( arg.length() >= 2 &&
               arg.compare( arg.length() - 2, 2, ".c" ) == 0 ) ||
             ( arg.length() >= 2 &&
               arg.compare( arg.length() - 2, 2, ".C" ) == 0 ) ||
             ( arg.length() >= 3 &&
               arg.compare( arg.length() - 3, 3, ".cc" ) == 0 ) ||
             ( arg.length() >= 3 &&
               arg.compare( arg.length() - 3, 3, ".CC" ) == 0 ) ||
             ( arg.length() >= 4 &&
               arg.compare( arg.length() - 4, 4, ".cpp" ) == 0 ) ||
             ( arg.length() >= 4 &&
               arg.compare( arg.length() - 4, 4, ".CPP" ) == 0 ) ||
             ( arg.length() >= 4 &&
               arg.compare( arg.length() - 4, 4, ".cxx" ) == 0 ) ||
             ( arg.length() >= 4 &&
               arg.compare( arg.length() - 4, 4, ".CXX" ) == 0 ) ||
             ( arg.length() >= 2 &&
               arg.compare( arg.length() - 2, 2, ".f" ) == 0 ) ||
             ( arg.length() >= 2 &&
               arg.compare( arg.length() - 2, 2, ".F" ) == 0 ) ||
             ( arg.length() >= 4 &&
               arg.compare( arg.length() - 4, 4, ".f77" ) == 0 ) ||
             ( arg.length() >= 4 &&
               arg.compare( arg.length() - 4, 4, ".F77" ) == 0 ) ||
             ( arg.length() >= 4 &&
               arg.compare( arg.length() - 4, 4, ".f90" ) == 0 ) ||
             ( arg.length() >= 4 &&
               arg.compare( arg.length() - 4, 4, ".F90" ) == 0 ) ||
             ( arg.length() >= 4 &&
               arg.compare( arg.length() - 4, 4, ".f95" ) == 0 ) ||
             ( arg.length() >= 4 &&
               arg.compare( arg.length() - 4, 4, ".F95" ) == 0 ) ||
             ( arg.length() >= 3 &&
               arg.compare( arg.length() - 3, 3, ".cu" ) == 0 ) )
    {
      static bool implicit_exclusion_warn = false;
      static std::queue<std::string> implicit_excluded_files;

      const std::string& file = arg;

      // source file excluded from instrumentation ?
      if( Config.isFileExcluded( Config.inst_excl_files, file ) )
      {
        // disable compiler instrumentation by switching to manual
        if( Config.inst_type == INST_TYPE_COMPINST )
        {
          Config.setInstType( INST_TYPE_MANUAL );

          // source files involved in the compile step but not are in the
          // exclusion list will be implicitly excluded from the compiler
          // instrumentation; set indicator for printing a warning message
          // when this happens
          implicit_exclusion_warn = true;
        }

        // add source file to the compiler arguments
        Config.addCompilerArg( file );
      }
      else
      {
        if( Config.showme_flags == 0 ||
            Config.showme_flags == SHOWME_FLAG_ALL )
        {
          // add source file to be modified by OPARI and/or TAU ...
          if( Config.inst_type == INST_TYPE_TAUINST || Config.uses_openmp )
            Config.addModSrcFile( file );
          // ... or add it as it is to the compiler arguments
          else
            Config.addCompilerArg( file );

          // the source file might be implicitly excluded from the compiler
          // instrumentation; store its name for a warning message
          if( !Config.inst_excl_files.empty() )
            implicit_excluded_files.push( file );
        }
        else
        {
          Config.addCompilerArg( file );
        }
      }

      // warn about implicitly excluded source files from compiler
      // instrumentation
      //
      if( implicit_exclusion_warn && !implicit_excluded_files.empty() )
      {
        while( !implicit_excluded_files.empty() )
        {
          std::cerr << "Warning: Implicitly excluded "
                    << implicit_excluded_files.front()
                    << " from compiler instrumentation." << std::endl;
          implicit_excluded_files.pop();
        }
      }
    }
    // -<I|D>*
    //
    else if( arg.compare( 0, 2, "-I" ) == 0 ||
             arg.compare( 0, 2, "-D" ) == 0 )
    {
      if( Config.preprocess )
        Config.addPrepFlag( arg );
      if( Config.inst_type == INST_TYPE_TAUINST )
        Config.addTauinstParseArg( arg );
      Config.addCompilerArg( arg );
    }
    // -WF,-D*
    //
    else if( Config.fortran() && arg.compare( 0, 6, "-WF,-D" ) == 0 )
    {
      if( Config.preprocess )
        Config.addPrepFlag( arg.substr( 4 ) );
      if( Config.inst_type == INST_TYPE_TAUINST )
        Config.addTauinstParseArg( arg.substr( 4 ) );
      Config.addCompilerArg( arg );
    }
    // Fortran line length flag
    //
    else if( Config.fortran() &&
             ( arg.compare( "-ffixed-line-length-132" ) == 0 ||
               arg.compare( "-extend_source" ) == 0 ||
               arg.compare( "-Mextend" ) == 0 ||
               arg.compare( "-e" ) == 0 ||
               arg.compare( "-qfixed=132" ) == 0 ) )
    {
      if( Config.inst_type == INST_TYPE_TAUINST )
        Config.addTauinstParseArg( "-ffixed-line-length-132" );
      Config.addCompilerArg( arg );
    }
    // Fortran free format flag
    //
    else if( Config.fortran() &&
             ( arg.compare( "-ffree-form" ) == 0 ||
               arg.compare( "-free" ) == 0 ||
               arg.compare( "-Mfree" ) == 0 ||
               arg.compare( 0, 6, "-qfree" ) == 0 ) )
    {
      if( Config.inst_type == INST_TYPE_TAUINST ||
          Config.tauinst_parsecmd.compare( "f95parse" ) == 0 )
        Config.addTauinstParseArg( "-R free" );
      Config.addCompilerArg( arg );
    }
    // Fortran fixed format flag
    //
    else if( Config.fortran() &&
             ( arg.compare( "-ffixed-form" ) == 0 ||
               arg.compare( "-fixed" ) == 0 ||
               arg.compare( "-Mfixed" ) == 0 ||
               arg.compare( 0, 7, "-qfixed" ) == 0 ) )
    {
      if( Config.inst_type == INST_TYPE_TAUINST &&
          Config.tauinst_parsecmd.compare( "f95parse" ) == 0 )
        Config.addTauinstParseArg( "-R fixed" );
      Config.addCompilerArg( arg );
    }
    // -c
    //
    else if( arg.compare( "-c" ) == 0 )
    {
      Config.comp_only = true;
      Config.addCompilerArg( arg );
    }
    // -o
    //
    else if( arg.compare( "-o" ) == 0 )
    {
      Config.outfile_given = true;
      Config.addCompilerArg( arg );
    }
    // CUDA (runtime) library
    //
    else if( arg.compare( "-lcuda" ) == 0 ||
             arg.compare( "-lcudart" ) == 0 ||
             arg.find( "libcuda" ) != std::string::npos ||
             arg.find( "libcudart" ) != std::string::npos )
    {
      Config.addCompilerLib( arg );
    }
    // MPI library
    //
    else if( arg.compare( 0, 5, "-lmpi" ) == 0 ||
             arg.compare( 0, 7, "-lmtmpi" ) == 0 ||
             arg.compare( 0, 7, "-lhpmpi" ) == 0 ||
             arg.compare( 0, 7, "-lpcmpi" ) == 0 ||
             arg.compare( 0, 7, "-lscmpi" ) == 0 ||
             arg.find( "libmpi" ) != std::string::npos ||
             arg.find( "libmtmpi" ) != std::string::npos ||
             arg.find( "libhpmpi" ) != std::string::npos ||
             arg.find( "libpcmpi" ) != std::string::npos ||
             arg.find( "libscmpi" ) != std::string::npos )
    {
      Config.setUsesMpi( true );
      Config.addCompilerLib( arg );
      addlibs = true;
    }
    // -<L|l>*
    //
    else if( arg.compare( 0, 2, "-L" ) == 0 || arg.compare( 0, 2, "-l" ) == 0 )
    {
      if( addlibs )
        Config.addCompilerLib( arg );
      else
        Config.addCompilerArg( arg );
    }
    // unknown argument
    //
    else
    {
      Config.addCompilerArg( arg );
    }
  }

  return true;
}

int
doWrap()
{
  int rc = 0;

  // call compiler without any parameters, if
  // insufficient arguments given
  //
  if( Config.showme_flags == 0 && Config.comp_args.length() == 0 )
  {
    rc = system( Config.comp_cmd.c_str() );
    return WEXITSTATUS( rc );
  }

  std::string cmd;
  uint32_t i;

  // vector which holds all intermediately created files
  std::vector<std::string> files_to_remove;

  // map which holds all object files created by the compiler
  // which must be renamed to original file names
  std::map<std::string, std::string> obj_files_to_rename;

  // process source files by OPARI and/or TAU
  //
  for( i = 0; i < Config.mod_files.size(); i++ )
  {
    const ModFileS& mod_file = Config.mod_files[i];

    std::string src_file = mod_file.src_file;

    std::string::size_type si;

    // preprocess source file
    //
    if( Config.preprocess )
    {
      // create output file name of C preprocessor
      //
      std::string cpp_file = src_file;
      si = cpp_file.rfind( '.' );
      vt_assert( si != std::string::npos );
      cpp_file.insert( si, ".cpp" );

      files_to_remove.push_back( cpp_file );

      // add path to empty omp.h and macro definition '_OPENMP' to preprocessor
      // flags, if OpenMP is enabled
      if( Config.uses_openmp &&
          i == 0 /* only once! */ )
      {
        Config.addPrepFlag( std::string( "-I" ) +
          vt_installdirs_get( VT_INSTALLDIR_DATADIR ) +
          " -D_OPENMP" );
      }

      // run preprocessor or reuse existing output file
      //
      if( !Config.reuse_files || access( cpp_file.c_str(), R_OK ) != 0 )
      {
        // compose C preprocessor command
        //
        cmd =
          Config.prep_cmd + " " +
          Config.prep_flags + " " +
          src_file + " " +
          " -o " + cpp_file;

        // show/execute C preprocessor command
        if( ( rc = showOrExecuteCommand( cmd ) ) != 0 )
          return rc;
      }
      else
      {
        if( Config.be_verbose )
          std::cout << "+++ reuse existing " << cpp_file << std::endl;
      }
      
      src_file = cpp_file;
    }

    // run OPARI command on source file
    //
    if( ( mod_file.action_flags & ModFileS::MOD_ACTION_FLAG_OPARI ) != 0 )
    {
      // create output file name of OPARI
      //

      std::string pomp_file = src_file;
      si = pomp_file.rfind( '.' );
      vt_assert( si != std::string::npos );
      pomp_file.insert( si, ".pomp" );

      // convert Fortran source file suffix to upper case, in order to
      // invoke the C preprocessor before compiling
      //
      if( Config.fortran() )
      {
        si = pomp_file.rfind( ".f" );
        if( si != std::string::npos )
          pomp_file.replace( si, 2, ".F" );
      }

      files_to_remove.push_back( pomp_file );

      // create OPARI include file name (only necessary for C/C++)
      //
      if( !Config.fortran() )
      {
        std::string inc_file = src_file + ".opari.inc";
        files_to_remove.push_back( inc_file );
      }

      // run OPARI or reuse existing output file
      //
      if( !Config.reuse_files || access( pomp_file.c_str(), R_OK ) != 0 )
      {
        // compose OPARI command
        //
        cmd =
          Config.opari_cmd + " " +
          Config.opari_args + " " +
          "-rcfile " + Config.opari_rcfile + " " +
          "-table " + Config.opari_tabfile.first + " " +
          src_file + " " +
          pomp_file;

        // show/execute OPARI command
        if( ( rc = showOrExecuteCommand( cmd ) ) != 0 )
          return rc;
      }
      else
      {
        if( Config.be_verbose )
          std::cout << "+++ reuse existing " << pomp_file << std::endl;
      }

      src_file = pomp_file;
    }

    // run PDT parser and TAU instrumentor command on source file
    //
    if( ( mod_file.action_flags & ModFileS::MOD_ACTION_FLAG_TAUINST ) != 0 )
    {
      // create output file name of the PDT parser
      //
      std::string pdb_file = src_file;
      si = src_file.rfind( '/' );
      if( si != std::string::npos )
        pdb_file = src_file.substr( si+1 );
      si = pdb_file.rfind( '.' );
      vt_assert( si != std::string::npos );
      pdb_file.replace( si, 4, ".pdb" );

      files_to_remove.push_back( pdb_file );

      // create output file name of the TAU instrumentor
      //

      std::string tau_file = src_file;
      si = tau_file.rfind( '.' );
      vt_assert( si != std::string::npos );
      tau_file.insert( si, ".tau" );

      // convert Fortran source file suffix to upper case, in order to
      // invoke the C preprocessor before compiling
      // (already done if OPARI was invoked)
      //
      if( Config.fortran() && !Config.uses_openmp )
      {
        si = tau_file.rfind( ".f" );
        if( si != std::string::npos )
          tau_file.replace( si, 2, ".F" );
      }

      files_to_remove.push_back( tau_file );

      // adjust PDT parser options, if source file is instrumented by OPARI
      //
      if( Config.uses_openmp )
      {
        // add current working directory to include search path to find OPARI
        // generated header files (only necessary for C/C++)
        if( !Config.fortran() )
          Config.addTauinstParseArg( "-I." );

        // add macro definition '_OPENMP', if OpenMP is enabled
        Config.addTauinstParseArg( "-D_OPENMP" );
      }

      // run PDT parser or reuse existing output file(s)
      //
      if( !Config.reuse_files ||
          ( access( pdb_file.c_str(), R_OK ) != 0 &&
            access( tau_file.c_str(), R_OK ) != 0 ) )
      {
        // compose PDT parse command
        //
        cmd =
          Config.tauinst_parsecmd + " " +
          src_file + " " +
          Config.vt_incdir + " " +
          Config.tauinst_parseargs;

        // show/execute PDT parse command
        if( ( rc = showOrExecuteCommand( cmd ) ) != 0 )
          return rc;
      }
      else
      {
        if( Config.be_verbose )
          std::cout << "+++ reuse existing " << pdb_file << std::endl;
      }

      // run TAU instrumentor or reuse existing output file
      //
      if( !Config.reuse_files || access( tau_file.c_str(), R_OK ) != 0 )
      {
        // compose TAU instrumentor command
        //
        cmd =
          Config.tauinst_cmd + " " +
          pdb_file + " " +
          src_file + " " +
          "-o " +
          tau_file + " " +
          Config.tauinst_args;

        // show/execute TAU instrumentor command
        if( ( rc = showOrExecuteCommand( cmd ) ) != 0 )
          return rc;
      }
      else
      {
        if( Config.be_verbose )
          std::cout << "+++ reuse existing " << tau_file << std::endl;
      }

      src_file = tau_file;
    }

    // create and store compiler output file name for renaming, if it's not
    // specified by the command line
    //
    if( Config.comp_only && !Config.outfile_given )
    {
      std::string obj_file = src_file;

      si = src_file.rfind( '/' );
      if( si != std::string::npos )
        obj_file = src_file.substr( si+1 );

      si = obj_file.rfind( '.' );
      vt_assert( si != std::string::npos );
      obj_file = obj_file.substr( 0, si ) + ".o";

      obj_files_to_rename[obj_file] = mod_file.obj_file;
    }
  }

  // adjust compiler flags, if source file is instrumented by OPARI
  //
  if( Config.uses_openmp )
  {
    // add current working directory to include search path to find OPARI
    // generated header files (only necessary for C/C++)
    //
    if( !Config.fortran() )
      Config.addCompilerArg( "-I." );

    // If the source file(s) are instrumented by OPARI *and* TAU, the source
    // code locations determined by TAU are nonsense. Add compiler flag to
    // disable recording of source code locations.
    //
    if( Config.inst_type == INST_TYPE_TAUINST )
      Config.addCompilerArg( Config.comp_fdflag + "TAUINST_NOSRC" );
  }

  // compile only ?
  if( Config.comp_only )
  {
    // compose compiler command
    //
    cmd =
      Config.comp_cmd + " " +
      Config.vt_incdir + " " +
      Config.comp_instflags + " " +
      Config.comp_flags + " " +
      Config.comp_args;

    // show/execute compiler command
    if( ( rc = showOrExecuteCommand( cmd ) ) != 0 )
      return rc;

    // rename compiler output files to original file names
    //
    if( Config.showme_flags == 0 )
    {
      for( std::map<std::string, std::string>::iterator it =
           obj_files_to_rename.begin(); it != obj_files_to_rename.end(); it++ )
      {
        if( Config.be_verbose )
        {
          std::cout << "+++ rename " << it->first
                    << " to " << it->second << std::endl;
        }

        if( rename( it->first.c_str(), it->second.c_str() ) == -1 )
        {
          std::cerr << ExeName << ": could not rename " << it->first
                    << " to " << it->second << std::endl;
          return 1;
        }
      }
    }
  }
  // [compile and] link
  else
  {
    std::string vtlib;

    if( Config.uses_openmp &&
        access( Config.opari_tabfile.first.c_str(), R_OK ) == 0 )
    {
      // compose command for compiling OPARI table file
      //
      cmd =
        Config.opari_tab_compcmd + " " +
        Config.opari_tab_compflags + " " +
        Config.vt_incdir + " " +
        ( Config.fortran() ? "-I. " : "" ) +
        "-c " +
        Config.opari_tabfile.first + " " +
        "-o " +
        Config.opari_tabfile.second;

      // show/execute compiler command
      if( ( rc = showOrExecuteCommand( cmd ) ) != 0 )
        return rc;

      // add OPARI table object file to to linker arguments
      Config.addCompilerArg( Config.opari_tabfile.second );

      // collect intermediate OPARI output files for removing
      //

      // OPARI include files
      //
      if( Config.fortran() )
      {
        std::vector<std::string> incfiles;
        getIncFilesFromTabFile( incfiles );
        for( i = 0; i < incfiles.size(); i++ )
          files_to_remove.push_back( incfiles[i].c_str() );
      }

      // OPARI table source/object file
      //
      files_to_remove.push_back( Config.opari_tabfile.first );
      files_to_remove.push_back( Config.opari_tabfile.second );

      // OPARI rc file
      //
      if( !Config.opari_keep_rcfile )
        files_to_remove.push_back( Config.opari_rcfile );
    }

    // compose compiler command
    //

    if( Config.inst_type == INST_TYPE_DYNINST )
      vtlib = Config.vt_dynattlib;

    if( Config.uses_threads )
    {
      if( Config.uses_openmp )
        vtlib += " " + Config.vt_pomplib;

      if( Config.uses_mpi )
        vtlib += " " + Config.vt_hyblib;
      else
        vtlib += " " + Config.vt_mtlib;
    }
    else
    {
      if( Config.uses_mpi )
        vtlib += " " + Config.vt_mpilib;
      else
        vtlib += " " + Config.vt_seqlib;
    }

    cmd =
      Config.comp_cmd + " " +
      Config.vt_incdir + " " +
      Config.comp_instflags + " " +
      Config.comp_flags + " " +
      Config.comp_args + " " +
      Config.comp_ldflags + " " +
      Config.vt_libdir + " " +
      vtlib + " " +
      Config.comp_libs;

    // show/execute compiler command
    if( ( rc = showOrExecuteCommand( cmd ) ) != 0 )
      return rc;
  }

  // remove intermediate files
  //
  if( Config.showme_flags == 0 && !Config.keep_files )
  {
    for( i = 0; i < files_to_remove.size(); i++ )
    {
      if( Config.be_verbose )
        std::cout << "+++ remove " << files_to_remove[i] << std::endl;

      remove( files_to_remove[i].c_str() );
    }
  }

  return 0;
}

void
showFlags()
{
  std::string flags;

  // add compiler flags
  //
  if( ( Config.showme_flags & SHOWME_FLAG_COMPILE ) != 0 )
  {
    flags +=
      Config.vt_incdir + " " +
      Config.comp_flags + " ";
  }

  // add linker flags
  //
  if( ( Config.showme_flags & SHOWME_FLAG_LINK ) != 0 )
  {
    std::string vtlib;

    if( Config.inst_type == INST_TYPE_DYNINST )
      vtlib = Config.vt_dynattlib;

    if( Config.uses_threads )
    {
      if( Config.uses_openmp )
        vtlib += " " + Config.vt_pomplib;

      if( Config.uses_mpi )
        vtlib += " " + Config.vt_hyblib;
      else
        vtlib += " " + Config.vt_mtlib;
    }
    else
    {
      if( Config.uses_mpi )
        vtlib += " " + Config.vt_mpilib;
      else
        vtlib += " " + Config.vt_seqlib;
    }

    flags +=
      Config.comp_ldflags + " " +
      Config.vt_libdir + " " +
      vtlib + " " +
      Config.comp_libs;
  }

  // add common flags
  //
  flags +=
    Config.comp_instflags + " " +
    Config.comp_args;

  // remove leading, trailing, and double spaces
  trimString( flags );

  // show flags
  std::cout << flags << std::endl;
}

void
showUsage()
{
  std::string str_lang;
  std::string str_lang_suffix;

  switch( Config.lang_type )
  {
    case LANG_CC:
      str_lang = "C";
      str_lang_suffix = "cc ";
      break;
    case LANG_CXX:
      str_lang = "C++";
      str_lang_suffix = "cxx";
      break;
    case LANG_FC:
      str_lang = "Fortran";
      str_lang_suffix = "fc";
      break;
  }

  std::cout << std::endl
            << " " << ExeName << " - " << str_lang << " compiler wrapper for VampirTrace." << std::endl
            << std::endl
            << " Syntax: " << ExeName << " [options] ..." << std::endl
            << std::endl
            << "   options:" << std::endl
            << "     -vt:help            Show this help message." << std::endl
            << std::endl
            << "     -vt:version         Show VampirTrace version." << std::endl
            << std::endl
            << "     -vt:" << str_lang_suffix << " <cmd>" << std::endl
            << "                         Set the underlying compiler command." <<  std::endl
            << "                         (default: " << Config.comp_cmd << ")" << std::endl
            << std::endl
            << "     -vt:inst <insttype> Set the instrumentation type." << std::endl
            << std::endl
            << "      possible values:" << std::endl
            << std::endl
            << "       compinst          fully-automatic by compiler" << std::endl
            << "       manual            manual by using VampirTrace's API" << std::endl
            << "       dyninst           binary by using Dyninst" << std::endl
            << "       tauinst           automatic source code instrumentation by using PDT/TAU" << std::endl
            << std::endl
            << "      default: " << Config.getInstTypeName() << std::endl
            << std::endl
            << "     -vt:inst-exclude-file-list <file>[,file,...]" << std::endl
            << "                         Set list of source files to be excluded from the" << std::endl
            << "                         automatic instrumentation by the compiler or PDT/TAU." << std::endl
            << "                         (file names can contain wildcards)" << std::endl
            << std::endl
            << "     -vt:inst-exclude-file <file>" << std::endl
            << "                         Set pathname of file containing a list of source files" << std::endl
            << "                         to be excluded from the automatic instrumentation by" << std::endl
            << "                         the compiler or PDT/TAU." << std::endl
            << "                         (file names can contain wildcards, one file name per" << std::endl
            << "                          line)" << std::endl
            << std::endl
            << "      Note when using an exclusion list for automatic compiler instrumentation:" << std::endl
            << "      If a source file from the exclusion list is involved in a compile step," << std::endl
            << "      the instrumentation is disabled for this step." << std::endl
            << std::endl
            << "     -vt:opari <[!]args> Set/add options for the OPARI command." << std::endl
            << "                         (see " << vt_installdirs_get(VT_INSTALLDIR_DATADIR) << "/doc/opari/Readme.html for more information, default: " << Config.opari_args << ")" << std::endl
            << std::endl
            << "     -vt:opari-rcfile <file>" << std::endl
            << "                         Set pathname of the OPARI resource file." << std::endl
            << "                         (default: " << ConfigS::DEFAULT_OPARI_RCFILE() << ")" << std::endl
            << std::endl
            << "     -vt:opari-table <file>" << std::endl
            << "                         Set pathname of the OPARI runtime table file." << std::endl
            << "                         (default: " << ConfigS::DEFAULT_OPARI_TABFILE().first << ")" << std::endl
            << std::endl
            << "     -vt:opari-exclude-file-list <file>[,file,...]" << std::endl
            << "                         Set list of source files to be excluded from the" << std::endl
            << "                         instrumentation of OpenMP constructs by OPARI." << std::endl
            << "                         (file names can contain wildcards)" << std::endl
            << std::endl
            << "     -vt:opari-exclude-file <file>" << std::endl
            << "                         Set pathname of file containing a list of source files" << std::endl
            << "                         to be excluded from the instrumentation of OpenMP" << std::endl
            << "                         constructs by OPARI." << std::endl
            << "                         (file names can contain wildcards, one file name per" << std::endl
            << "                          line)" << std::endl
            << std::endl
            << "     -vt:noopari         Disable instrumentation of OpenMP contructs by OPARI." << std::endl
            << std::endl
            << "     -vt:tau <[!]args>   Set/add options for the TAU instrumentor command." << std::endl
            << "                         (default: " << Config.tauinst_args << ")" << std::endl
            << std::endl
            << "     -vt:pdt <[!]args>   Set/add options for the PDT parse command." << std::endl
            << "                         (default: " << Config.tauinst_parseargs << ")" << std::endl
            << std::endl
            << "     -vt:preprocess      Preprocess the source files before parsing" << std::endl
            << "                         by OPARI and/or PDT." << std::endl
            << std::endl
            << "     -vt:cpp <cmd>       Set C preprocessor command." << std::endl
            << "                         (default: " << Config.prep_cmd << ")" << std::endl
            << std::endl
            << "     -vt:cppflags <[!]flags>" << std::endl
            << "                         Set/add flags for the C preprocessor." << std::endl
            << "                         (default: " << Config.prep_flags << ")" << std::endl
            << std::endl
            << "     -vt:<seq|mpi|mt|hyb>" << std::endl
            << "                         Enforce application's parallelization type." << std::endl
            << "                         It's only necessary if it could not be determined" << std::endl
            << "                         automatically based on underlying compiler and flags." << std::endl
            << "                         seq = sequential" << std::endl
            << "                         mpi = parallel (uses MPI)" << std::endl
            << "                         mt = parallel (uses OpenMP/POSIX threads)" << std::endl
            << "                         hyb = hybrid parallel (MPI + Threads)" << std::endl
            << "                         (default: automatically)" << std::endl
            << std::endl
            << "     -vt:verbose         Enable verbose mode." << std::endl
            << std::endl
            << "     -vt:keepfiles       Keep intermediate files." << std::endl
            << std::endl
            << "     -vt:reusefiles      Reuse intermediate files, if exist." << std::endl
            << std::endl
            << "     -vt:show[me]        Do not invoke the underlying compiler." << std::endl
            << "                         Instead, show the command line(s) that would be" << std::endl
            << "                         executed to compile and link the program." << std::endl
            << std::endl
            << "     -vt:showme-compile  Do not invoke the underlying compiler." << std::endl
            << "                         Instead, show the compiler flags that would be" << std::endl
            << "                         supplied to the compiler." << std::endl
            << std::endl
            << "     -vt:showme-link     Do not invoke the underlying compiler." << std::endl
            << "                         Instead, show the linker flags that would be" << std::endl
            << "                         supplied to the compiler." << std::endl
            << std::endl
            << "     See the man page for your underlying compiler for other options that can" << std::endl
            << "     be passed through '" << ExeName << "'." << std::endl
            << std::endl
            << "   environment variables:" << std::endl
            << "     VT_INST             Instrumentation type (equivalent to '-vt:inst'*)" << std::endl;
  switch( Config.lang_type )
  {
    case LANG_CC:
      std::cout << "     VT_CC               C compiler command (equivalent to '-vt:" << str_lang_suffix << "'*)" << std::endl
                << "     VT_CFLAGS           C compiler flags" << std::endl;
      break;
    case LANG_CXX:
      std::cout << "     VT_CXX              C++ compiler command (equivalent to '-vt:" << str_lang_suffix << "'*)" << std::endl
                << "     VT_CXXFLAGS         C++ compiler flags" << std::endl;
      break;
    case LANG_FC:
      std::cout << "     VT_FC               Fortran compiler command (equivalent to '-vt:" << str_lang_suffix <<"'*)" << std::endl
                << "     VT_FCFLAGS          Fortran compiler flags" << std::endl;
      break;
  }
  std::cout << "     VT_LDFLAGS          Linker flags" << std::endl
            << "     VT_LIBS             Libraries to pass to the linker" << std::endl
            << std::endl
            << "     * The corresponding command line options overwrites the environment" << std::endl
            << "       variables setting." << std::endl
            << std::endl
            << "   examples:" << std::endl
            << "     automatically instrumentation by compiler:" << std::endl
            << std::endl;
  switch( Config.lang_type )
  {
    case LANG_CC:
      std::cout << "        vtcc -vt:cc gcc -vt:inst compinst -c foo.c -o foo.o" << std::endl
                << "        vtcc -vt:cc gcc -vt:inst compinst -c bar.c -o bar.o" << std::endl
                << "        vtcc -vt:cc gcc -vt:inst compinst foo.o bar.o -o foo" << std::endl
                << std::endl
                << "     manually instrumentation by using VT's API:"  << std::endl
                << std::endl
                << "        vtcc -vt:inst manual foobar.c -o foobar -DVTRACE";
      break;
    case LANG_CXX:
      std::cout << "        vtcxx -vt:cxx g++ -vt:inst compinst -c foo.cpp -o foo.o" << std::endl
                << "        vtcxx -vt:cxx g++ -vt:inst compinst bar.cpp -o bar.o" << std::endl
                << "        vtcxx -vt:cxx g++ -vt:inst compinst foo.o bar.o -o foo" << std::endl
                << std::endl
                << "     manually instrumentation by using VT's API:" << std::endl
                << std::endl
                << "        vtcxx -vt:inst manual foobar.cpp -o foobar -DVTRACE";
      break;
    case LANG_FC:
      std::cout << "        vtfort -vt:fc gfortran -vt:inst compinst -c foo.F90 -o foo.o" << std::endl
                << "        vtfort -vt:fc gfortran -vt:inst compinst bar.F90 -o bar.o" << std::endl
                << "        vtfort -vt:fc gfortran -vt:inst compinst foo.o bar.o -o foo" << std::endl
                << std::endl
                << "     manually instrumentation by using VT's API:" << std::endl
                << std::endl
                << "        vtfort -vt:inst manual foobar.F90 -o foobar -DVTRACE";
      break;
  }

  if( Config.fortran() )
  {
    std::cout << std::endl << std::endl
              << "     IMPORTANT: Fortran source files instrumented by using VT's API" << std::endl
              << "                have to be (CPP) preprocessed.";
  }
  std::cout << std::endl << std::endl;
}

int
showOrExecuteCommand( std::string& cmd )
{
  int rc = 0;

  // remove leading, trailing, and double spaces
  trimString( cmd );

  // either show
  if( Config.showme_flags )
  {
    std::cout << cmd << std::endl;
  }
  // or execute command
  else
  {
    if( Config.be_verbose )
      std::cout << "+++ " << cmd << std::endl;
    if( ( rc = system( cmd.c_str() ) ) != -1 )
      rc = WEXITSTATUS( rc );
  }

  return rc;
}

void
getIncFilesFromTabFile( std::vector<std::string>& incfiles )
{
  std::ifstream in( Config.opari_tabfile.first.c_str() );
  if( in )
  {
    char buffer[1024];
    std::string line;

    while( in.getline( buffer, sizeof( buffer ) ) )
    {
      line = buffer;

      if( line.find( "#include" ) != std::string::npos &&
          line.find( ".opari.inc" ) != std::string::npos )
      {
        std::string incfile = line.substr( line.find( "#include" ) + 10 );
        incfile.erase( incfile.length() - 1 );

        incfiles.push_back( incfile );
      }
    }

    in.close();
  }
}

bool
isCuFile( const std::string& file )
{
  return
    ( file.length() >= 3 && file.compare( file.length() - 3, 3, ".cu" ) == 0 );
}

void
addOrSetStringList( std::string& list, const std::string& str, bool reset )
{
  if( reset )
  {
    list = str;
  }
  else
  {
    if( list.length() > 0 ) list += " ";
    list += str;
  }
}

void
trimString( std::string& str )
{
  std::string::size_type i, j;

  // remove leading and trailing spaces
  //

  i = str.find_first_not_of( " \t" );
  j = str.find_last_not_of( " \t" );

  if( ( i == std::string::npos ) || ( j == std::string::npos ) )
    str = "";
  else
    str = str.substr( i, j - i + 1 );

  // remove double spaces
  //
  while( str.length() > 0 && ( i = str.find( "  " ) ) != std::string::npos )
    str.erase( i, 1 );
}

//////////////////// struct ConfigS ////////////////////

bool
ConfigS::setLanguage( const LangTypeT lang )
{
#if !(defined(HAVE_FC) && HAVE_FC)
  if( lang == LANG_FC )
  {
    std::cerr << "Unfortunately, this installation of VampirTrace "
              << "was not compiled with Fortran support." << std::endl
              << "As such, the " << ExeName << " compiler is non-functional."
              << std::endl;
    return false;
  }
#endif // HAVE_FC

  lang_type = lang;
  return true;
}

bool
ConfigS::fortran() const
{
  return ( lang_type == LANG_FC );
}

void
ConfigS::setCompilerCmd( const std::string& cmd )
{
  std::string bcomp = cmd;
  std::string::size_type ls = cmd.rfind('/');

  if( ls != std::string::npos ) bcomp = cmd.substr( ls+1 );

  if( !uses_mpi &&
      ( bcomp.compare( 0, 2, "mp" ) == 0 ||
        bcomp.compare( 0, 4, "sxmp" ) == 0 ||
        bcomp.compare( 0, 4, "scmp" ) == 0 ) )
  {
    setUsesMpi( true );
  }

  if( bcomp.compare( 0, 3, "xlf" ) == 0 ||
      bcomp.compare( 0, 9, "blrts_xlf" ) == 0 ||
      bcomp.compare( 0, 3, "bgf" ) == 0 )
  {
    comp_fdflag = "-WF,-D";
  }
  else
  {
    comp_fdflag = "-D";
  }

  comp_cmd = cmd;
}

void
ConfigS::addCompilerArg( const std::string& arg )
{
  vt_assert( arg.length() > 0 );
  addOrSetStringList( Config.comp_args, arg );
}

void
ConfigS::addCompilerLib( const std::string& lib )
{
  vt_assert( lib.length() > 0 );
  addOrSetStringList( Config.comp_libs, lib );
}

void
ConfigS::addModSrcFile( const std::string& file )
{
  int mod_action_flags = 0;

  // perform OPARI instrumentation?
  //
  if( uses_openmp && !isFileExcluded( opari_excl_files, file ) )
    mod_action_flags |= ModFileS::MOD_ACTION_FLAG_OPARI;

  // perform TAU instrumentation?
  //
  if( Config.inst_type == INST_TYPE_TAUINST )
  {
    // skip CUDA (*.cu) source files for now
    //
    if( isCuFile( file ) )
    {
      std::cerr << "Warning: Skip " << file << " for instrumenting with "
                << "PDT/TAU - not yet supported." << std::endl;
    }
    else
    {
      mod_action_flags |= ModFileS::MOD_ACTION_FLAG_TAUINST;
    }
  }

  // add unmodified source file name to compiler arguments, if there
  // is nothing to do for OPARI and TAU
  //
  if( !mod_action_flags )
  {
    addCompilerArg( file );
  }
  // otherwise, register source file for processing by OPARI and/or TAU
  //
  else
  {
    std::string file_base;
    std::string file_obj;
    std::string::size_type si;

    // get base name of source file
    //
    file_base = file;
    si = file.rfind( '/' );
    if( si != std::string::npos )
      file_base = file.substr( si+1 );

    // create object file name of source file
    //
    si = file_base.rfind( '.' );
    vt_assert( si != std::string::npos );
    file_obj = file_base.substr( 0, si ) + ".o";

    // store source/object file and modification action flags
    mod_files.push_back( ModFileS( file, file_obj, mod_action_flags ) );

    // add modified source file name to compiler arguments
    //

    si = file.rfind( '.' );
    vt_assert( si != std::string::npos );

    std::string base = file.substr( 0, si );
    std::string suffix = file.substr( si );
    std::string mod_file = base;

    if( preprocess )
      mod_file += ".cpp";
    if( ( mod_action_flags & ModFileS::MOD_ACTION_FLAG_OPARI ) != 0 )
      mod_file += ".pomp";
    if( ( mod_action_flags & ModFileS::MOD_ACTION_FLAG_TAUINST ) != 0 )
      mod_file += ".tau";

    // convert Fortran source file suffix to upper case, in order to
    // invoke the C preprocessor before compiling
    if( fortran() && suffix.compare( 0, 2, ".f" ) == 0 )
      suffix.replace( 0, 2, ".F" );

    mod_file += suffix;

    addCompilerArg( mod_file );
  }
}

void
ConfigS::setOpariTabFile( const std::string& file )
{
  std::string file_src;
  std::string file_obj;

  if( !( file.length() >= 2 &&
         file.compare( file.length()-2, 2, ".c" ) == 0 ) )
  {
    file_src = file + ".c";
    file_obj = file + ".o";
  }
  else
  {
    file_src = file_obj = file;
    file_obj.replace( file.length()-2, 2, ".o" );
  }

  opari_tabfile = std::make_pair( file_src, file_obj );
}

void
ConfigS::setOpariRcFile( const std::string& file )
{
  opari_rcfile = file;
  opari_keep_rcfile = true;
}

void
ConfigS::addOpariArg( const std::string& arg )
{
  vt_assert( arg.length() > 0 );
  addOrSetStringList( Config.opari_args, arg, arg[0] == '!' );
}

void
ConfigS::addTauinstArg( const std::string& arg )
{
  vt_assert( arg.length() > 0 );
  addOrSetStringList( Config.tauinst_args, arg, arg[0] == '!' );
}

void
ConfigS::addTauinstParseArg( const std::string& arg )
{
  vt_assert( arg.length() > 0 );
  addOrSetStringList( Config.tauinst_parseargs, arg, arg[0] == '!' );
}

void
ConfigS::addPrepFlag( const std::string& flag )
{
  vt_assert( flag.length() > 0 );
  addOrSetStringList( Config.prep_flags, flag, flag[0] == '!' );
}

void
ConfigS::setUsesMpi( bool set, bool ovwrt )
{
  static bool first = true;

  if( first || ovwrt )
  {
    if( set )
    {
#if !(defined(HAVE_MPI) && HAVE_MPI)
      std::cerr << "Unfortunately, this installation of VampirTrace was not "
                << "compiled with" << std::endl
                << "MPI support." << std::endl;
      exit(1);
#else // HAVE_MPI
      if( fortran() )
      {
#  if !(defined(HAVE_FMPI) && HAVE_FMPI)
        std::cerr << "Unfortunately, this installation of VampirTrace was "
                  << "not compiled with" << std::endl
                  << "MPI Fortran support." << std::endl;
        exit(1);
#  endif // WRAP_FMPI
      }
#endif // HAVE_MPI
    }

    first = false;
    uses_mpi = set;
  }
}

void
ConfigS::setUsesThreads( bool set, bool ovwrt )
{
  static bool first = true;

  if( first || ovwrt )
  {
#if !(defined(HAVE_THREADS) && HAVE_THREADS)
    if( set )
    {
      std::cerr << "Unfortunately, this installation of VampirTrace was not "
                << "compiled with" << std::endl
                << "Multithreading support." << std::endl;
      exit(1);
    }
#endif // HAVE_THREADS

    first = false;
    uses_threads = set;
  }
}

void
ConfigS::setUsesOpenMP( bool set, bool ovwrt )
{
  static bool first = true;

  if( first || ovwrt )
  {
#if !(defined(HAVE_OMP) && HAVE_OMP)
    if( set )
    {
      std::cerr << "Unfortunately, this installation of VampirTrace was not "
                << "compiled with" << std::endl
                << "OpenMP support." << std::endl;
      exit(1);
    }
#endif // HAVE_OMP

    first = false;
    uses_openmp = set;
  }
}

bool
ConfigS::setInstAvail( const std::string& type )
{
  if( type.compare( "compinst" ) == 0 )
    inst_avail |= INST_TYPE_COMPINST;
  else if( type.compare( "dyninst" ) == 0 )
    inst_avail |= INST_TYPE_DYNINST;
  else if( type.compare( "tauinst" ) == 0 )
    inst_avail |= INST_TYPE_TAUINST;
  else if( type.compare( "manual" ) == 0 )
    inst_avail |= INST_TYPE_MANUAL;
  else
    return false;

  return true;
}

bool
ConfigS::setInstType( const InstTypeT type )
{
  vt_assert( inst_avail != 0 );

  // instrumentation available ?
  if( !isInstAvail( type ) )
    return false;

  inst_type = type;

  if( type == INST_TYPE_COMPINST )
    comp_instflags = compinst_flags;
  else if( type == INST_TYPE_DYNINST )
    comp_instflags = dyninst_flags;
  else
    comp_instflags = "";

  return true;
}

bool
ConfigS::setInstType( const std::string& type )
{
  if( type.compare( "compinst" ) == 0 )
    return setInstType( INST_TYPE_COMPINST );
  else if( type.compare( "dyninst" ) == 0 )
    return setInstType( INST_TYPE_DYNINST );
  else if( type.compare( "tauinst" ) == 0 )
    return setInstType( INST_TYPE_TAUINST );
  else if( type.compare( "manual" ) == 0 )
    return setInstType( INST_TYPE_MANUAL );
  else
    return false;
}

InstTypeT
ConfigS::getInstType() const
{
  return inst_type;
}

std::string
ConfigS::getInstTypeName() const
{
  std::string name;

  switch( inst_type )
  {
    case INST_TYPE_COMPINST:
      name = "compinst";
      break;
    case INST_TYPE_MANUAL:
      name = "manual";
      break;
    case INST_TYPE_DYNINST:
      name = "dyninst";
      break;
    case INST_TYPE_TAUINST:
    default:
      name = "tauinst";
      break;
  }

  return name;
}

bool
ConfigS::isInstAvail( const InstTypeT type ) const
{
  return ( inst_avail & type );
}

bool
ConfigS::isFileExcluded( const std::vector<std::string>& excls,
  const std::string& file ) const
{
  for( std::vector<std::string>::const_iterator it = excls.begin();
       it != excls.end(); ++it )
  {
    if( fnmatch( it->c_str(), file.c_str(), 0 ) == 0 )
      return true;
  }

  return false;
}

bool
ConfigS::readExclFile( const std::string& file,
  std::vector<std::string>& excls ) const
{
  std::ifstream in( file.c_str() );
  if( !in )
  {
    std::cerr << ExeName << ": Error: Could not open exclusion file "
              << file << ". Aborting." << std::endl;
    return false;
  }

  char buffer[1024];

  while( in.getline( buffer, sizeof( buffer ) ) )
  {
    std::string line = buffer;
    trimString( line );
    if( line.length() > 0 )
      excls.push_back( line );
  }

  in.close();

  return true;
}
