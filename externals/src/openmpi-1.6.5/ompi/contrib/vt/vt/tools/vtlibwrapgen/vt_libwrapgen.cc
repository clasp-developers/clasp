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

#include "vt_libwrapgen.h"

#include "util/installdirs.h"

#include <iostream>
#include <sstream>

#include <dirent.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

static bool getParams( int argc, char** argv );
static void showUsage( void );

// global variables
//

// name of program's executable
const std::string ExeName = "vtlibwrapgen";

// program parameters
ParamsS           Params;

// instance of class Generator
GeneratorC*       theGenerator;

int
main( int argc, char** argv )
{
  // get program parameters
  if( !getParams( argc, argv ) )
    return 1;

  // create instance of class Generator
  theGenerator = new GeneratorC();

  // either generate library wrapper source file ...
  //
  if( Params.mode == MODE_GENSRC )
  {
    if( !theGenerator->genSource() )
      return 1;
  }
  // ... or build wrapper library from generated source file
  //
  else
  {
    if( !theGenerator->buildLib() )
      return 1;
  }

  // delete instance of class Generator
  delete theGenerator;

  VPrint( 1, "Done\n" );
  
  return 0;
}

void
VPrint( uint8_t level, const char * fmt, ... )
{
  va_list ap;

  if( Params.verbose_level >= level )
  {
    va_start( ap, fmt );
    vprintf( fmt, ap );
    fflush( stdout );
    va_end( ap );
  }
}

static bool
getParams( int argc, char** argv )
{
  int i;

  // get environment variables
  //

  char* env;

  // VT_CC
  if( (env = getenv( "VT_CC" )) )
    Params.b_cc_cmd = env;
  // VT_CFLAGS
  if( (env = getenv( "VT_CFLAGS" )) )
    Params.b_cc_flags = env;
  // VT_CPP
  if( (env = getenv( "VT_CPP" )) )
    Params.g_cpp_cmd = env;
  // VT_CPPFLAGS
  if( (env = getenv( "VT_CPPFLAGS" )) )
    Params.g_cpp_flags = env;
  // VT_LD
  if( (env = getenv( "VT_LD" )) )
    Params.b_ld_cmd = env;
  // VT_LDFLAGS
  if( (env = getenv( "VT_LDFLAGS" )) )
    Params.b_ld_flags = env;
  // VT_LIBS
  if( (env = getenv( "VT_LIBS" )) )
    Params.b_libs = env;


  // parse command line parameters
  //

  // enum for argument errors
  //
  enum
  {
    ARG_OK,
    ARG_MISSING,
    ARG_INVALID,
    ARG_UNRECOGNIZED
  } arg_error = ARG_OK;

  // general parameters
  //
  for( i = 0; i < argc; i++ )
  {
    // assemble whole command line
    //
    if( i == 0 )
    {
      Params.command_line = argv[i];
      continue;
    }
    else
    {
      Params.command_line += std::string(" ") + std::string(argv[i]);
    }

    // -h, --help
    //
    if( strcmp( argv[i], "-h" ) == 0 ||
        strcmp( argv[i], "--help" ) == 0 )
    {
      showUsage();
      exit(0);
    }
    // -V, --version
    //
    if( strcmp( argv[i], "-V" ) == 0 ||
        strcmp( argv[i], "--version" ) == 0 )
    {
      std::cout << VT_LIBWRAPGEN_VERSION << std::endl;
      exit(0);
    }
    // -q, --quiet
    //
    else if( strcmp( argv[i], "-q" ) == 0 ||
             strcmp( argv[i], "--quiet" ) == 0 )
    {
      Params.verbose_level = 0;
    }
    // -v, --verbose
    //
    else if( strcmp( argv[i], "-v" ) == 0 ||
             strcmp( argv[i], "--verbose" ) == 0 )
    {
      Params.verbose_level++;
    }
    // --gen
    //
    // else if( strcmp( argv[i], "--gen" ) == 0 ) {} // default action
    // --build
    //
    else if( strcmp( argv[i], "--build" ) == 0 )
    {
      Params.mode = MODE_BUILDLIB;
    }
  }

  // gen- or build-parameters
  //
  for( i = 1; i < argc; i++ )
  {
    // already handled parameters
    //
    if( strcmp( argv[i], "--gen" ) == 0 ||
        strcmp( argv[i], "--build" ) == 0 ||
        strcmp( argv[i], "-q" ) == 0 ||
        strcmp( argv[i], "--quiet" ) == 0 ||
        strcmp( argv[i], "-v" ) == 0 ||
        strcmp( argv[i], "--verbose" ) == 0 )
    {
      // ignore
    }
    else
    {
      size_t arg_len = strlen( argv[i] );

      // gen-parameters
      //
      if( Params.mode == MODE_GENSRC )
      {
        static bool add_header = false;

        if( add_header )
        {
          Params.g_input_headers.push_back( argv[i] );
        }
        // -o, --output
        //
        else if( strcmp( argv[i], "-o" ) == 0 )
        {
          if( i == argc - 1 ) arg_error = ARG_MISSING;
          else Params.g_output_srcfile = argv[++i];
        }
        else if( strncmp( argv[i], "--output", 8 ) == 0 &&
                 (arg_len == 8 || argv[i][8] == '=') )
        {
          if( arg_len == 8 ) arg_error = ARG_MISSING;
          else if( arg_len == 9 ) arg_error = ARG_INVALID;
          else Params.g_output_srcfile = argv[i]+9;
        }
        // -l, --shlib
        //
        else if( strcmp( argv[i], "-l" ) == 0 )
        {
          if( i == argc - 1 ) arg_error = ARG_MISSING;
          else Params.g_shlibs.push_back( argv[++i] );
        }
        else if( strncmp( argv[i], "--shlib", 7 ) == 0 &&
                 (arg_len == 7 || argv[i][7] == '=') )
        {
          if( arg_len == 7 ) arg_error = ARG_MISSING;
          else if( arg_len == 8 ) arg_error = ARG_INVALID;
          else Params.g_shlibs.push_back( argv[i]+8 );
        }
        // -f, --filter
        //
        else if( strcmp( argv[i], "-f" ) == 0 )
        {
          if( i == argc - 1 ) arg_error = ARG_MISSING;
          else Params.g_input_filtfile = argv[++i];
        }
        else if( strncmp( argv[i], "--filter", 8 ) == 0 &&
                 (arg_len == 8 || argv[i][8] == '=') )
        {
          if( arg_len == 8 ) arg_error = ARG_MISSING;
          else if( arg_len == 9 ) arg_error = ARG_INVALID;
          else Params.g_input_filtfile = argv[i]+9;
        }
        // -g, --group
        //
        else if( strcmp( argv[i], "-g" ) == 0 )
        {
          if( i == argc - 1 ) arg_error = ARG_MISSING;
          else Params.g_group = argv[++i];
        }
        else if( strncmp( argv[i], "--group", 7 ) == 0 &&
                 (arg_len == 7 || argv[i][7] == '=') )
        {
          if( arg_len == 7 ) arg_error = ARG_MISSING;
          else if( arg_len == 8 ) arg_error = ARG_INVALID;
          else Params.g_group = argv[i]+8;
        }
        // -s, --sysheader
        //
        else if( strcmp( argv[i], "-s" ) == 0 )
        {
          if( i == argc - 1 ) arg_error = ARG_MISSING;
          else Params.g_allowed_sysheaders.push_back( argv[++i] );
        }
        else if( strncmp( argv[i], "--sysheader", 11 ) == 0 &&
                 (arg_len == 11 || argv[i][11] == '=') )
        {
          if( arg_len == 11 ) arg_error = ARG_MISSING;
          else if( arg_len == 12 ) arg_error = ARG_INVALID;
          else Params.g_allowed_sysheaders.push_back( argv[i]+12 );
        }
        // --nocpp
        //
        else if( strcmp( argv[i], "--nocpp" ) == 0 )
        {
          Params.g_use_cpp = false;
        }
        // --keepcppfile
        //
        else if( strcmp( argv[i], "--keepcppfile" ) == 0 )
        {
          Params.g_keep_cpp_file = true;
        }
        // --cpp
        //
        else if( strncmp( argv[i], "--cpp", 5 ) == 0 &&
                 (arg_len == 5 || argv[i][5] == '=') )
        {
          if( arg_len == 5 ) arg_error = ARG_MISSING;
          else if( arg_len == 6 ) arg_error = ARG_INVALID;
          else Params.g_cpp_cmd = argv[i]+6;
        }
        // --cppflags
        //
        else if( strncmp( argv[i], "--cppflags", 10 ) == 0 &&
                 (arg_len == 10 || argv[i][10] == '=') )
        {
          if( arg_len == 10 ) arg_error = ARG_MISSING;
          else if( arg_len == 11 ) arg_error = ARG_INVALID;
          else Params.g_cpp_flags = argv[i]+11;
        }
        // --cppdir
        //
        else if( strncmp( argv[i], "--cppdir", 8 ) == 0 &&
                 (arg_len == 8 || argv[i][8] == '=') )
        {
          if( arg_len == 8 ) arg_error = ARG_MISSING;
          else if( arg_len == 9 ) arg_error = ARG_INVALID;
          else Params.g_cpp_dir = argv[i]+9;
        }
        // unrecognized gen-parameter
        //
        else if( argv[i][0] == '-' )
        {
          arg_error = ARG_UNRECOGNIZED;
        }
        // input header file
        //
        else
        {
          add_header = true;
          i--;
        }
      }
      // build-parameters
      //
      else
      {
        // -o, --output
        //
        if( strcmp( argv[i], "-o" ) == 0 )
        {
          if( i == argc - 1 ) arg_error = ARG_MISSING;
          else Params.b_output_libprefix = argv[++i];
        }
        else if( strncmp( argv[i], "--output", 8 ) == 0 &&
                 (arg_len == 8 || argv[i][8] == '=') )
        {
          if( arg_len == 8 ) arg_error = ARG_MISSING;
          else if( arg_len == 9 ) arg_error = ARG_INVALID;
          else Params.b_output_libprefix = argv[i]+9;
        }
        // --shared
        else if( strcmp( argv[i], "--shared" ) == 0 )
        {
          Params.b_static = false;
          Params.b_shared = true;
        }
        // --static
        //
        else if( strcmp( argv[i], "--static" ) == 0 )
        {
          Params.b_shared = false;
          Params.b_static = true;
        }
        // --libtool
        //
        else if( strncmp( argv[i], "--libtool", 9 ) == 0 &&
                 (arg_len == 9 || argv[i][9] == '=') )
        {
          if( arg_len == 9 ) arg_error = ARG_MISSING;
          else if( arg_len == 10 ) arg_error = ARG_INVALID;
          else Params.b_libtool_cmd = argv[i]+10;
        }
        // --cc
        //
        else if( strncmp( argv[i], "--cc", 4 ) == 0 &&
                 (arg_len == 4 || argv[i][4] == '=') )
        {
          if( arg_len == 4 ) arg_error = ARG_MISSING;
          else if( arg_len == 5 ) arg_error = ARG_INVALID;
          else Params.b_cc_cmd = argv[i]+5;
        }
        // --cflags
        //
        else if( strncmp( argv[i], "--cflags", 8 ) == 0 &&
                 (arg_len == 8 || argv[i][8] == '=') )
        {
          if( arg_len == 8 ) arg_error = ARG_MISSING;
          else if( arg_len == 9 ) arg_error = ARG_INVALID;
          else Params.b_cc_flags = argv[i]+9;
        }
        // --ld
        //
        else if( strncmp( argv[i], "--ld", 4 ) == 0 &&
                 (arg_len == 4 || argv[i][4] == '=') )
        {
          if( arg_len == 4 ) arg_error = ARG_MISSING;
          else if( arg_len == 5 ) arg_error = ARG_INVALID;
          else Params.b_ld_cmd = argv[i]+5;
        }
        // --ldflags
        //
        else if( strncmp( argv[i], "--ldflags", 9 ) == 0 &&
                 (arg_len == 9 || argv[i][9] == '=') )
        {
          if( arg_len == 9 ) arg_error = ARG_MISSING;
          else if( arg_len == 10 ) arg_error = ARG_INVALID;
          else Params.b_ld_flags = argv[i]+10;
        }
        // --libs
        //
        else if( strncmp( argv[i], "--libs", 6 ) == 0 &&
                 (arg_len == 6 || argv[i][6] == '=') )
        {
          if( arg_len == 6 ) arg_error = ARG_MISSING;
          else if( arg_len == 7 ) arg_error = ARG_INVALID;
          else Params.b_libs = argv[i]+7;
        }
        // input source file or unrecognized build-parameter
        //
        else
        {
          if( Params.b_input_srcfile.length() == 0 )
            Params.b_input_srcfile = argv[i];
          else
            arg_error = ARG_UNRECOGNIZED;
        }
      }
    }

    // abort loop, if an argument error occurred
    if( arg_error != ARG_OK )
      break;
  }

  // show error message, if necessary
  //
  if( arg_error != ARG_OK )
  {
    switch( arg_error )
    {
      case ARG_MISSING:
        std::cerr << ExeName << ": missing argument for "
                  << argv[i] << std::endl;
        break;
      case ARG_INVALID:
        std::cerr << ExeName << ": invalid argument for "
                  << argv[i] << std::endl;
        break;
      case ARG_UNRECOGNIZED:
        std::cerr << ExeName << ": unrecognized option `"
                  << argv[i] << "'" << std::endl
                  << ExeName << ": Try `" << ExeName 
                  << " --help' for more information." << std::endl;
        break;
      default:
        break;
    }

    return false;
  }

  // show usage text, if parameters are incomplete
  //
  if( (Params.mode == MODE_GENSRC && Params.g_input_headers.size() == 0) ||
      (Params.mode == MODE_BUILDLIB && Params.b_input_srcfile.length() == 0) )
  {
    showUsage();
    exit(0);
  }

  // post-process parameters
  //
  size_t idx;

  if( Params.mode == MODE_GENSRC )
  {
    // extract output directory from source file
    //
    idx = Params.g_output_srcfile.find_last_of('/');
    if( idx != std::string::npos )
      Params.output_dir = Params.g_output_srcfile.substr( 0, idx );

    // convert relative paths of allowed system header files to absolute paths
    //
    for( i = 0; i < (int)Params.g_allowed_sysheaders.size(); i++ )
    {
    // prepend path of system header files, if it's a relative path
      //
      if( Params.g_allowed_sysheaders[i][0] != '/' )
      {
        Params.g_allowed_sysheaders[i] =
            VT_LIBWRAPGEN_DEFAULT_SYSHEADER_PREFIX +
            Params.g_allowed_sysheaders[i];
      }
    }
  }
  else // Params.mode == MODE_BUILDLIB
  {
    // extract output directory from library prefix
    //
    idx = Params.b_output_libprefix.find_last_of('/');
    if( idx != std::string::npos )
      Params.output_dir = Params.b_output_libprefix.substr( 0, idx );

    // set linker command to compiler command, if necessary
    if( Params.b_ld_cmd.length() == 0 )
      Params.b_ld_cmd = Params.b_cc_cmd;

    // set linker flags to compiler flags, if necessary
    if( Params.b_ld_flags.length() == 0 )
      Params.b_ld_flags = Params.b_cc_flags;

    // expand install directories from libtool command, if necessary
    //
    if( Params.b_libtool_cmd.length() == 0 )
    {
      char* tmp = vt_installdirs_expand( VT_LIBWRAPGEN_DEFAULT_LIBTOOL );
      Params.b_libtool_cmd = tmp;
      free( tmp );
    }

    // disable libtool verbosity, if necessary
    if( Params.verbose_level == 0 )
      Params.b_libtool_flags = "--quiet";

    // add VT include directory to compiler flags
    Params.b_cc_flags += " -I" +
      std::string(vt_installdirs_get( VT_INSTALLDIR_INCLUDEDIR ));

    // add libtool arg. '-static' or '-shared' to compiler/linker flags,
    // if necessary
    if( Params.b_static )
    {
      Params.b_cc_flags += " -static";
      Params.b_ld_flags += " -static";
    }
    else if( Params.b_shared )
    {
      Params.b_cc_flags += " -shared";
      Params.b_ld_flags += " -shared";
    }
  }

  return true;
}

static void
showUsage()
{
  std::cout << std::endl
    << " " << ExeName << " - library wrapper generator for VampirTrace." << std::endl
    << std::endl
    << " Syntax: " << std::endl
    << "   Generate a library wrapper source file:" << std::endl
    << "     " << ExeName << " [gen-options] <input header file> [input header file...]" << std::endl
    << std::endl
    << "   Build a wrapper library from a generated source file:" << std::endl
    << "     " << ExeName << " --build [build-options] <input lib. wrapper source file>" << std::endl
    << std::endl
    << "   options:" << std::endl
    << "     --gen               Generate a library wrapper source file. (default)" << std::endl
    << "                         See 'gen-options' below for valid options." << std::endl
    << std::endl
    << "     --build             Build a wrapper library from a generated source file." << std::endl
    << "                         See 'build-options' below for valid options." << std::endl
    << std::endl
    << "     -h, --help          Show this help message." << std::endl
    << std::endl
    << "     -V, --version       Show VampirTrace version." << std::endl
    << std::endl
    << "     -q, --quiet         Enable quiet mode." << std::endl
    << "                         (only emergency output)" << std::endl
    << std::endl
    << "     -v, --verbose       Increase output verbosity." << std::endl
    << "                         (can be used more than once)" << std::endl
    << std::endl
    << "   gen-options:" << std::endl
    << "     -o, --output=FILE   Pathname of output wrapper source file." << std::endl
    << "                         (default: "VT_LIBWRAPGEN_DEFAULT_OUTPUT_SRC_FILE")" << std::endl
    << std::endl
    << "     -l, --shlib=SHLIB   Pathname of shared library that contains the actual" << std::endl
    << "                         library functions." << std::endl
    << "                         (can be used more then once)" << std::endl
    << std::endl
    << "     -f, --filter=FILE   Pathname of input filter file." << std::endl
    << std::endl
    << "     -g, --group=NAME    Separate function group name for wrapped functions." << std::endl
    << std::endl
    << "     -s, --sysheader=FILE" << std::endl
    << "                         Header file to be included additionally." << std::endl
    << std::endl
    << "     --nocpp             Don't use preprocessor." << std::endl
    << std::endl
    << "     --keepcppfile       Don't remove preprocessed header files." << std::endl
    << std::endl
    << "     --cpp=CPP           C preprocessor command" << std::endl
    << "                         (default: "VT_LIBWRAPGEN_DEFAULT_CPP")" << std::endl
    << std::endl
    << "     --cppflags=CPPFLAGS C preprocessor flags, e.g. -I<include dir>" << std::endl
    << std::endl
    << "     --cppdir=DIR        Change to this preprocessing directory." << std::endl
    << std::endl
    << "     environment variables:" << std::endl
    << "       VT_CPP            C preprocessor command (equivalent to '--cpp')" << std::endl
    << "       VT_CPPFLAGS       C preprocessor flags (equivalent to '--cppflags')" << std::endl
    << std::endl
    << "   build-options:" << std::endl
    << "     -o, --output=PREFIX Prefix of output wrapper library." << std::endl
    << "                         (default: "VT_LIBWRAPGEN_DEFAULT_OUTPUT_LIB_PREFIX")" << std::endl
    << std::endl
    << "     --shared            Do only build shared wrapper library." << std::endl
    << std::endl
    << "     --static            Do only build static wrapper library." << std::endl
    << std::endl
    << "     --libtool=LT        Libtool command" << std::endl;

  char* tmp = vt_installdirs_expand( VT_LIBWRAPGEN_DEFAULT_LIBTOOL );
  std::cout << "                         (default: " << tmp << ")" << std::endl;
  free( tmp );

  std::cout 
    << "     --cc=CC             C compiler command" << std::endl
    << "                         (default: "VT_LIBWRAPGEN_DEFAULT_CC")" << std::endl
    << std::endl
    << "     --cflags=CFLAGS     C compiler flags" << std::endl
    << "                         (default: "VT_LIBWRAPGEN_DEFAULT_CFLAGS")" << std::endl
    << std::endl
    << "     --ld=LD             linker command" << std::endl
    << "                         (default: CC)" << std::endl
    << std::endl
    << "     --ldflags=LDFLAGS   linker flags, e.g. -L<lib dir>" << std::endl
    << "                         (default: CFLAGS)" << std::endl
    << std::endl
    << "     --libs=LIBS         libraries to pass to the linker, e.g. -l<library>" << std::endl
    << std::endl
    << "     environment variables:" << std::endl
    << "       VT_CC             C compiler command (equivalent to '--cc')" << std::endl
    << "       VT_CFLAGS         C compiler flags (equivalent to '--cflags')" << std::endl
    << "       VT_LD             linker command (equivalent to '--ld')" << std::endl
    << "       VT_LDFLAGS        linker flags (equivalent to '--ldflags')" << std::endl
    << "       VT_LIBS           libraries to pass to the linker" << std::endl
    << "                         (equivalent to '--libs')" << std::endl
    << std::endl
    << "   examples:" << std::endl
    << "     Generating wrapper library 'libm_wrap' for the Math library 'libm.so':" << std::endl
    << std::endl
    << "       " << ExeName << " -l libm.so -g MATH -o mwrap.c /usr/include/math.h" << std::endl
    << "       " << ExeName << " --build -o libm_wrap mwrap.c" << std::endl
    << "       export LD_PRELOAD=$PWD/libm_wrap.so:libvt.so" << std::endl
    << std::endl;
}

//////////////////// class Generator ////////////////////

// public methods
//

GeneratorC::GeneratorC()
{
  // Empty
}

GeneratorC::~GeneratorC()
{
  // Empty
}

bool
GeneratorC::genSource()
{
  VPrint( 1, "Generating library wrapper source file\n" );

  // create instance of class Filter
  m_filter = new FilterC();

  // create instance of class Parser
  m_parser = new ParserC();

  // read filter file, if given
  //
  if( Params.g_input_filtfile.length() > 0 )
  {
    if( !m_filter->read() )
      return false;
  }

  // open output file stream
  //
  m_outputStream.open( Params.g_output_srcfile.c_str() );
  if( !m_outputStream )
  {
    std::cerr << ExeName << ": Could not open output file "
        << Params.g_output_srcfile << std::endl;
    return false;
  }

  // write head to output file stream
  writeHead();

  // parse header files and write wrapper functions to output file stream
  //
  bool error = !m_parser->parse();

  // close output file stream
  m_outputStream.close();

  // remove output file, if an error occurred
  if( error )
    remove( Params.g_output_srcfile.c_str() );
  else
    VPrint( 1, " Created %s\n", Params.g_output_srcfile.c_str() );

  // delete instance of class Filter
  delete m_filter;

  // delete instance of class Parser
  delete m_parser;

  return !error;
}

bool
GeneratorC::buildLib()
{
  VPrint( 1, "Building wrapper library\n" );

  // check whether input source file exists
  //
  if( access( Params.b_input_srcfile.c_str(), R_OK ) == -1 )
  {
    std::cerr << ExeName << ": Could not open source file "
              << Params.b_input_srcfile << ": "
              << strerror( errno ) << std::endl;
    return false;
  }

  // create directory for temporary files
  //
  std::stringstream tmpdir;
  tmpdir << Params.output_dir << "/" << ExeName << getpid() << ".tmp";
  VPrint( 2, " Creating temporary directory %s\n", tmpdir.str().c_str() );
  if( mkdir( tmpdir.str().c_str(), 448 ) == -1 )
  {
    std::cerr << ExeName << ": Could not create directory "
              << tmpdir.str() << ": "
              << strerror( errno ) << std::endl;
    return false;
  }

  bool error = false;

  do
  {
    char prevdir[PATH_MAX];
    if( !getcwd( prevdir, PATH_MAX ) )
    {
      std::cerr << ExeName 
                << ": Could not determine current working directory"
                << ": " << strerror( errno ) << std::endl;
      error = true;
      break;
    }

    // change to temp. directory
    //
    VPrint( 2, " Changing to temporary directory %s\n", tmpdir.str().c_str() );
    if( chdir( tmpdir.str().c_str() ) == -1 )
    {
      std::cerr << ExeName << ": Could not change to temporary directory "
                << tmpdir.str() << ": "
                << strerror( errno ) << std::endl;
      error = true;
      break;
    }

    std::stringstream cmd;
    size_t idx;
    int rc;

    // create object file name
    //
    std::string objname = Params.b_input_srcfile;

    idx = objname.find_last_of('/');
    if( idx != std::string::npos )
      objname = objname.substr( idx + 1 );
    idx = objname.find_last_of(".c");
    if( idx != std::string::npos )
      objname = objname.substr( 0, idx - 1 );
    objname += ".lo";

    // compile library wrapper source file
    //
    VPrint( 1, " Compiling library wrapper source file %s\n",
            Params.b_input_srcfile.c_str() );
    cmd << Params.b_libtool_cmd << " " << Params.b_libtool_flags;
    if( Params.verbose_level <= 1 )
      cmd << " --quiet";
    cmd << " --mode=compile " << Params.b_cc_cmd << " " << Params.b_cc_flags
        << " -c ";
    if( Params.b_input_srcfile[0] != '/' )
      cmd << prevdir << "/";
    cmd << Params.b_input_srcfile << " -o " << objname;

    rc = system( cmd.str().c_str() );
    if( rc == -1 || WEXITSTATUS(rc) != 0 || access( objname.c_str(), R_OK ) == -1 )
    {
      std::cerr << ExeName << ": Command '" 
                << cmd.str() << "' failed" << std::endl;
      if( chdir( prevdir ) == -1 )
      {
        std::cerr << ExeName << ": Could not change to initial directory "
                  << prevdir << ": "
                  << strerror( errno ) << std::endl;
      }
      error = true;
      break;
    }

    // create library file name
    //
    std::string libname_base = Params.b_output_libprefix;
    std::string libname_lt;

    idx = libname_base.find_last_of('/');
    if( idx != std::string::npos )
      libname_base = libname_base.substr( idx + 1 );
    libname_lt = libname_base + ".la";

    // link wrapper library
    //
    VPrint( 1, " Linking libtool object file %s\n", objname.c_str() );
    cmd.str("");
    cmd << Params.b_libtool_cmd << " " << Params.b_libtool_flags;
    if( Params.verbose_level <= 1 )
      cmd << " --quiet";
    cmd << " --mode=link " << Params.b_ld_cmd
        << " -rpath /usr/local/lib -avoid-version " << Params.b_ld_flags
        << " -o " << libname_lt << " " << objname;

    rc = system( cmd.str().c_str() );
    if( rc == -1 || WEXITSTATUS(rc) != 0 || access( libname_lt.c_str(), R_OK ) == -1 )
    {
      std::cerr << ExeName << ": Command '" 
                << cmd.str() << "' failed" << std::endl;
      if( chdir( prevdir ) == -1 )
      {
        std::cerr << ExeName << ": Could not change to initial directory "
                  << prevdir << ": "
                  << strerror( errno ) << std::endl;
      }
      error = true;
      break;
    }

    // move built libraries to destination directory
    //
    std::pair<std::string, std::string> libname_a;
    std::pair<std::string, std::string> libname_so;

    libname_a.first   = ".libs/" + libname_base + ".a";
    libname_a.second  = "../" + libname_base + ".a";
    libname_so.first  = ".libs/" + libname_base + ".so";
    libname_so.second = "../" + libname_base + ".so";

    if( access( libname_a.first.c_str(), R_OK ) != -1 )
    {
      rename( libname_a.first.c_str(), libname_a.second.c_str() );
      VPrint( 1, " Created %s.a\n", libname_base.c_str() );
    }
    if( access( libname_so.first.c_str(), R_OK ) != -1 )
    {
      rename( libname_so.first.c_str(), libname_so.second.c_str() );
      VPrint( 1, " Created %s.so\n", libname_base.c_str() );
    }

    // change to previous directory
    //
    VPrint( 2, " Changing to previous directory %s\n", prevdir );
    if( chdir( prevdir ) == -1 )
    {
      std::cerr << ExeName << ": Could not change to initial directory "
                << prevdir << ": "
                << strerror( errno ) << std::endl;
      error = true;
      break;
    }
  } while( 0 );

  // remove temporary directory
  //
  VPrint( 2, " Removing temporary directory %s\n", tmpdir.str().c_str() );
  if( !removeDir( tmpdir.str() ) )
  {
    std::cerr << ExeName << ": Could not remove temporary directory "
              << tmpdir.str() << std::endl;
    error = true;
  }

  return !error;
}


// private methods
//

void
GeneratorC::writeHead()
{
  std::ofstream& out = m_outputStream;

  time_t t;
  char* ts;
  uint32_t i;

  t = time(0);
  ts = ctime(&t);
  ts[strlen(ts)-1] = '\0';

  out << "/**" << std::endl
      << " * Generated by '" << ExeName << "' which is a part of VampirTrace" << std::endl
      << " * on " << ts << "." << std::endl
      << " *" << std::endl
      << " * command:" << std::endl
      << " *   " << Params.command_line << std::endl
      << " **/" << std::endl
      << std::endl
      << "#include \"vt_libwrap.h\"" << std::endl
      << std::endl;

  for( i = 0; i < Params.g_input_headers.size(); i++ )
    out << "#include \"" << Params.g_input_headers[i] << "\"" << std::endl;
  out << std::endl;

  out << "/* -- library wrapper object --" << std::endl
      << " * Holds the opaque 'handle' for the shared library that contains the actual" << std::endl
      << " * library functions. It will be initialized at the first wrapper event by" << std::endl
      << " * VT_LIBWRAP_FUNC_INIT." << std::endl
      << " */" << std::endl
      << "static VTLibwrap* lw = VT_LIBWRAP_NULL;" << std::endl
      << std::endl
      << std::endl
      << "/* -- library wrapper attributes --" << std::endl
      << " * Describes the attributes of the library wrapper object created. The" << std::endl
      << " * underlying data structure of the type 'VTLibwrapAttr' has the following" << std::endl
      << " * elements:" << std::endl
      << " *" << std::endl
      << " * int shlibs_num:" << std::endl
      << " *   Number of shared libraries which will be searched for the actual library" << std::endl
      << " *   functions. If 0 is specified, the addresses of the functions will be" << std::endl
      << " *   searched in the shared libraries linked to the application, in order," << std::endl
      << " *   starting after the VampirTrace library." << std::endl
      << " * char* shlibs[VT_LIBWRAP_MAX_SHLIBS]:" << std::endl
      << " *   Array of pathnames to the shared libraries which will be searched for the" << std::endl
      << " *   actual library functions." << std::endl
      << " * char* func_group:" << std::endl
      << " *   Separate function group which will be assigned to all of the wrapped" << std::endl
      << " *   functions. If no group specified (NULL), each wrapped function will be" << std::endl
      << " *   assigned to the default group 'Application'." << std::endl
      << " * char libc:" << std::endl
      << " *   Do additional search actual library functions in the LIBC." << std::endl
      << " *   (1=yes / 0=no)" << std::endl
      << " * char wait_for_init:" << std::endl
      << " *   Wait for initialization of VampirTrace before generating events by these" << std::endl
      << " *   wrapper functions. (1=yes / 0=no)" << std::endl
      << " * VTLibwrapAttrInitFunc init_func:" << std::endl
      << " *   Pointer to a function which may be used to initialize the library wrapper" << std::endl
      << " *   attributes above. It is called at the first wrapper event, if it is not set" << std::endl
      << " *   to NULL. The type 'VTLibwrapAttrInitFunc' is an alias for the function" << std::endl
      << " *   pointer type '(void)(*)(VTLibwrapAttr* attr)'." << std::endl
      << " *" << std::endl
      << " *" << std::endl
      << " * There are three ways to initialize the library wrapper attributes:" << std::endl
      << " *" << std::endl
      << " * 1. default attributes:" << std::endl
      << " *    The library wrapper attributes will initialized as follows:" << std::endl
      << " *      shlibs_num = 0" << std::endl
      << " *      shlibs = { NULL, ... } - no shared libraries" << std::endl
      << " *      func_group = NULL      - no separate function group" << std::endl
      << " *      libc = 0               - do not search actual library functions in LIBC" << std::endl
      << " *      wait_for_init = 0      - do not wait for initialization of VampirTrace" << std::endl
      << " *" << std::endl
      << " * uncomment the following line to use the default attributes:" << std::endl
      << " */" << std::endl
      << "/* static VTLibwrapAttr lw_attr = VT_LIBWRAP_ATTR_DEFAULT; */" << std::endl
      << std::endl
      << "/* 2. attribute initializer" << std::endl
      << " *    The library wrapper attributes will be initialized at the first wrapper" << std::endl
      << " *    event by the specified function." << std::endl
      << " *" << std::endl
      << " * uncomment the following lines to use the attribute initializer:" << std::endl
      << " */" << std::endl
      << "/*" << std::endl
      << "static void libwrap_attr_init(VTLibwrapAttr* attr) {" << std::endl
      << "  attr->shlibs_num = <num>;" << std::endl
      << "  attr->shlibs[0] = <shlib1>;" << std::endl
      << "  attr->shlibs[1] = <shlib2>;" << std::endl
      << "  ..." << std::endl
      << "  attr->func_group = <groupname>;" << std::endl
      << "  attr->libc = <0|1>;" << std::endl
      << "  attr->wait_for_init = <0|1>;" << std::endl
      << "}" << std::endl
      << "static VTLibwrapAttr lw_attr = VT_LIBWRAP_ATTR_INITIALIZER(libwrap_attr_init);" << std::endl
      << "*/" << std::endl
      << std::endl
      << "/* 3. constant attributes:" << std::endl
      << " *    The library wrapper attributes will be initialized at declaration." << std::endl
      << " *" << std::endl
      << " * uncomment the following lines to use the constant attributes:" << std::endl
      << " */" << std::endl
      << "static VTLibwrapAttr lw_attr = {" << std::endl
      << "  " << Params.g_shlibs.size() << ", /* shlibs_num */" << std::endl;

  out << "  { ";
  if( Params.g_shlibs.size() == 0 )
  {
    out << "NULL";
  }
  else
  {
    for( i = 0; i < Params.g_shlibs.size(); i++ )
    {
      if( i > 0 ) out << "," << std::endl << "    ";
      out << "\"" << Params.g_shlibs[i] << "\"";
    }
  }
  out << " }, /* shlibs */" << std::endl;

  if( Params.g_group.length() > 0 )
    out << "  \"" << Params.g_group << "\",";
  else
    out << "  NULL,";
  out << " /* func_group */" << std::endl;

  out << "  0, /* libc */" << std::endl
      << "  0 /* wait_for_init */" << std::endl
      << "};" << std::endl
      << std::endl
      << std::endl
      << "/* -- wrapper functions -- */" << std::endl
      << std::endl;
}

void
GeneratorC::writeFunction( const FuncS& func )
{
  // check whether function is already generated
  //
  std::map<std::string, bool>::iterator gf_it = genFuncs.find( func.name );
  if( gf_it != genFuncs.end() )
    return;
  else
    genFuncs[func.name] = true;
  
  std::ofstream& out = m_outputStream;
  uint32_t i;

  VPrint( 2, "  %s:%s",
          func.loc.file.length() > 0 ? func.loc.file.c_str() : "<unknown>",
          func.name.c_str() );
  
  out << "/* -- ";

  if( func.loc.file.length() > 0 )
    out << func.loc.file;
  else
    out << "<unknown>";

  out << ":" << func.name << " -- ";

  // register function and check whether it shall be wrapped
  //
  if( m_filter->constraint( func.loc.file, func.name ) )
  {
    VPrint( 2, " -- skipped\n" );
    
    out << "not generated */" << std::endl
        << std::endl;
    return;
  }
  else
  {
    VPrint( 2, " -- generated\n" );
    
    out << "*/" << std::endl
        << std::endl;
  }

  out << func.rettype << " "
      << func.name << "(";

  for( i = 0; i < func.args.size(); i++ )
  {
    if( i > 0 ) out << ", ";
    out << func.args[i].type_base
        << func.args[i].type_before
        << func.args[i].name
        << func.args[i].type_after;
  }

  out << ")" << std::endl
      << "{" << std::endl;

  if( !func.noret )
    out << "  " << func.rettype << " ret;" << std::endl << std::endl;

  out << "  VT_LIBWRAP_FUNC_INIT(lw, lw_attr, \"" << func.name << "\"," << std::endl
      << "    " << func.rettype << ", (";

  if( func.args.size() == 0 )
  {
    out << "void";
  }
  else
  {
    for( i = 0; i < func.args.size(); i++ )
    {
      if( i > 0 ) out << ", ";
      out << func.args[i].type;
    }
  }

  out << ")," << std::endl
      << "    ";

  if( func.loc.file.length() > 0 )
    out << "\"" << func.loc.file << "\", " << func.loc.line;
  else
    out << "NULL, 0";

  out << ");" << std::endl
      << std::endl
      << "  VT_LIBWRAP_FUNC_START(lw);" << std::endl
      << std::endl;

  if( !func.noret )
    out << "  ret = ";
  else
    out << "  ";

  out << "VT_LIBWRAP_FUNC_CALL(lw, (";

  for( i = 0; i < func.args.size(); i++ )
  {
    if( i > 0 ) out << ", ";
    out << func.args[i].name;
  }

  out << "));" << std::endl
      << std::endl
      << "  VT_LIBWRAP_FUNC_END(lw);" << std::endl;

  if( !func.noret )
    out << std::endl
        << "  return ret;" << std::endl;

  out << "}" << std::endl
      << std::endl;
}

bool
GeneratorC::removeDir( const std::string& path )
{
  struct stat path_stat;
  if( (stat( path.c_str(), &path_stat ) == -1) && (errno != ENOENT) )
    return false;
  if( !S_ISDIR( path_stat.st_mode ) )
    return ( (remove( path.c_str() ) == 0) || (errno == ENOENT) );

  DIR* dir = opendir( path.c_str() );
  if( !dir )
    return false;

  bool error = false;
  struct dirent* ent;
  while( !error && (ent = readdir( dir )) )
  {
    if( strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0 )
      continue;

    std::string subpath = path;
    subpath += '/';
    subpath += ent->d_name;
    error = !removeDir( subpath );
  }

  closedir(dir);

  if( !error )
    return ( (rmdir( path.c_str() ) == 0) || (errno == ENOENT) );
  else
    return false;
}
