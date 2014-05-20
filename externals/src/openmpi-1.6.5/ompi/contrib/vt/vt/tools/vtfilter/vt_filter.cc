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

#include "vt_filter.h"
#include "vt_filter_gen.h"
#include "vt_filter_trc.h"

#include "otf.h"

#include <iostream>

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef VT_MPI
# include "mpi.h"
#endif // VT_MPI

#if defined(HAVE_OMP) && HAVE_OMP
# include <omp.h>
#endif // HAVE_OMP

// local functions
//

// get program parameters
static bool getParams( int argc, char** argv );

// show usage text
static void showUsage( void );

// convert a string list to a vector
inline static bool stringList2Vector( const std::string& str,
                                      std::vector<std::string>& vec,
                                      const std::string& delim = ";" );

// local variables
//

// enforce gen. mode if called as vtfiltergen[-mpi]
static bool enforceGenMode = false;

// global variables
//

// name of program's executable
std::string ExeName;

// program parameters
ParamsS Params;

#ifdef VT_MPI
  // number of MPI-ranks
  VT_MPI_INT NumRanks;

  // MPI-rank of calling process
  VT_MPI_INT MyRank;
#endif // VT_MPI

int
main( int argc, char ** argv )
{
  bool error = false;

#ifdef VT_MPI
  // initialize MPI
  //
  MPI_Init( (VT_MPI_INT*)&argc, &argv );
  MPI_Comm_size( MPI_COMM_WORLD, &NumRanks );
  MPI_Comm_rank( MPI_COMM_WORLD, &MyRank );
#endif // VT_MPI

  // get program's executable name
  //
  ExeName = argv[0];
  std::string::size_type si = ExeName.rfind('/');
  if( si != std::string::npos )
    ExeName.erase( 0, si+1 );

  // enforce gen. mode if called as vtfiltergen[-mpi]
  //
  if( ExeName.substr( 0, 11 ) == "vtfiltergen" )
  {
    enforceGenMode = true;
    Params.mode = MODE_GEN;
  }

  do
  {
    // get program parameters
    if( ( error = !getParams( argc, argv ) ) )
      break;

    // show usage text, if necessary/desired
    //
    if( Params.show_usage )
    {
      MASTER showUsage();
      break;
    }

    // show VT version, if desired
    //
    if( Params.show_version )
    {
      MASTER std::cout << PACKAGE_VERSION << std::endl;
      break;
    }

    // check whether the input trace file is readable
    //
    MASTER
    {
      OTF_FileManager* manager = OTF_FileManager_open( 1 );
      vt_assert( manager );

      OTF_Reader* reader =
        OTF_Reader_open( Params.input_trcfile.c_str(), manager );

      if( !reader )
      {
        std::cerr << ExeName << ": Error: Could not open input trace file "
                  << Params.input_trcfile << ".otf. Aborting." << std::endl;

        OTF_FileManager_close( manager );
        error = true;
        break;
      }

      OTF_Reader_close( reader );
      OTF_FileManager_close( manager );
    }

    // either generate a filter file ...
    //
    if( Params.mode == MODE_GEN )
    {
      // create instance of class FilterGenerator
      FilterGeneratorC* gen = new FilterGeneratorC();

      // generate filter file
      error = !gen->run();

      // delete instance of class FilterGenerator
      delete gen;
    }
    // ... or filter a trace by an already existing filter file
    //
    else // Params.mode == MODE_FILTTRC
    {
      // create instance of class FilterTrace
      FilterTraceC* filt = new FilterTraceC();

      // filter input trace file
      error = !filt->run();

      // delete instance of class FilterTrace
      delete filt;
    }

  } while( false );

#ifdef VT_MPI
  // either abort on error or finalize
  //
  if( error )
  {
    MPI_Abort( MPI_COMM_WORLD, 1 );
  }
  else
  {
    if( NumRanks > 1 )
    {
      // block until all ranks have reached this point
      MPI_Barrier( MPI_COMM_WORLD );
    }

    MPI_Finalize();
  }
#endif // VT_MPI

  return (error) ? 1 : 0;
}

void
VPrint( uint8_t level, const char * fmt, ... )
{
  va_list ap;

  MASTER
  {
    if( Params.verbose_level >= level )
    {
      va_start( ap, fmt );
      vprintf( fmt, ap );
      fflush( stdout );
      va_end( ap );
    }
  }
}

void
PVPrint( uint8_t level, const char * fmt, ... )
{
  va_list ap;

  if( Params.verbose_level >= level )
  {
    va_start( ap, fmt );
#   if !(defined(VT_MPI) || (defined(HAVE_OMP) && HAVE_OMP))
      vprintf( fmt, ap );
#   else // !(VT_MPI || HAVE_OMP)
      char msg[1024] = "";
#     if defined(VT_MPI) && !(defined(HAVE_OMP) && HAVE_OMP)
        snprintf( msg, sizeof(msg)-1, "[%d] ", MyRank );
#     elif !defined(VT_MPI) && (defined(HAVE_OMP) && HAVE_OMP)
        if( omp_in_parallel() )
          snprintf( msg, sizeof(msg)-1, "[%d] ", omp_get_thread_num() );
#     else // !VT_MPI && HAVE_OMP
        if( omp_in_parallel() )
        {
          snprintf( msg, sizeof(msg)-1, "[%d:%d] ", MyRank,
                    omp_get_thread_num() );
        }
        else
        {
          snprintf( msg, sizeof(msg)-1, "[%d] ", MyRank );
        }
#     endif // !VT_MPI && HAVE_OMP
      vsnprintf(msg + strlen(msg), sizeof(msg)-1, fmt, ap);
#     if defined(HAVE_OMP) && HAVE_OMP
#       pragma omp critical
#     endif // HAVE_OMP
      printf( "%s", msg );
#   endif // !(VT_MPI || HAVE_OMP)
    va_end( ap );
  }
}

static bool
getParams( int argc, char** argv )
{
  // show usage text, if no options are given
  //
  if( argc == 1 )
  {
    Params.show_usage = true;
    return true;
  }

  // All environment variables and command line options which have a '*' in its
  // comment are obsolete. They are still available to keep backward-
  // compatibility to the old vtfilter tool.

  // get environment variables
  //

  char* env;

  // TRACEFILTER_EXCLUDEFILE*
  //
  if( ( env = getenv( "TRACEFILTER_EXCLUDEFILE" ) ) &&
      strlen( env ) > 0 ) Params.g_excl_file = env;
  // TRACEFILTER_INCLUDEFILE*
  //
  if( ( env = getenv( "TRACEFILTER_INCLUDEFILE" ) ) &&
      strlen( env ) > 0 ) Params.g_incl_file = env;

  // parse command line options
  //

  uint32_t i;

  // enum for option errors
  //
  enum
  {
    OPT_ERR_OK,
    OPT_ERR_ARG_MISSING,
    OPT_ERR_ARG_INVALID,
    OPT_ERR_UNRECOGNIZED,
    OPT_ERR_OTHER
  } opt_error = OPT_ERR_OK;

  // error string for OPT_ERR_OTHER
  std::string opt_error_other;

  // pre-process options: convert to std::string and put them into a vector
  //
  std::vector<std::string> args;
  for( i = 1; i < (uint32_t)argc; i++ )
  {
    // add option to vector
    args.push_back( argv[i] );

    // extract argument from --<opt>=<arg> option and add this to vector
    // as a separate option
    // this is for supporting both: --<opt>=<arg> and --<opt> <arg>
    //
    std::string& arg = args.back();
    if( arg.compare( 0, 2, "--" ) == 0 )
    {
      std::string::size_type eq = arg.find( '=' );
      if( eq != std::string::npos )
      {
        std::string tmp = arg.substr( eq + 1 );
        arg.erase( eq );
        args.push_back( tmp );
      }
    }
  }

  // general options
  //
  for( i = 0; i < args.size(); i++ )
  {
    // --gen, -gen*
    //
    if( !enforceGenMode &&
        ( args[i].compare( "--gen" ) == 0 ||
          args[i].compare( "-gen" ) == 0 ) )
    {
      Params.mode = MODE_GEN;
    }
    // --filt, -filt*
    //
    else if( !enforceGenMode &&
             ( args[i].compare( "--filt" ) == 0 ||
               args[i].compare( "-filt" ) == 0 ) )
    {
      Params.mode = MODE_FILT;
    }
    // -h, --help
    //
    else if( args[i].compare( "-h" ) == 0 ||
             args[i].compare( "--help" ) == 0 )
    {
      Params.show_usage = true;
      return true;
    }
    // -V, --version
    //
    else if( args[i].compare( "-V" ) == 0 ||
             args[i].compare( "--version" ) == 0 )
    {
      Params.show_version = true;
      return true;
    }
    // -v, --verbose
    //
    else if( args[i].compare( "-v" ) == 0 ||
             args[i].compare( "--verbose" ) == 0 )
    {
      Params.verbose_level++;
    }
    // -q, --quiet
    //
//    else if( args[i].compare( "-q" ) == 0 ||
//             args[i].compare( "--quiet" ) == 0 )
//    {
//      Params.verbose_level = 0;
//      Params.show_progress = false;
//    }
    // -p, --progress
    //
    else if( args[i].compare( "-p" ) == 0 ||
             args[i].compare( "--progress" ) == 0 )
    {
      Params.show_progress = true;
    }
  }

  // gen- or filt-options
  //
  for( i = 0; i < args.size(); i++ )
  {
    // already handled options
    //
    if( ( !enforceGenMode &&
          ( args[i].compare( "--gen" ) == 0 ||
            args[i].compare( "-gen" ) == 0 ||
            args[i].compare( "--filt" ) == 0 ||
            args[i].compare( "-filt" ) == 0 ) ) ||
        args[i].compare( "-v" ) == 0 ||
        args[i].compare( "--verbose" ) == 0 ||
//        args[i].compare( "-q" ) == 0 ||
//        args[i].compare( "--quiet" ) == 0 ||
        args[i].compare( "-p" ) == 0 ||
        args[i].compare( "--progress" ) == 0 )
    {
      // ignore
    }
    else
    {
      // gen-options
      //
      if( Params.mode == MODE_GEN )
      {
        // -o, --output, -fo*
        //
        if( args[i].compare( "-o" ) == 0 ||
            args[i].compare( "--output" ) == 0 ||
            args[i].compare( "-fo" ) == 0 )
        {
          if( i == args.size() - 1 )
            opt_error = OPT_ERR_ARG_MISSING;
          else
            Params.g_output_filtfile = args[++i];
        }
        // -r, --reduce
        //
        else if( args[i].compare( "-r" ) == 0 ||
                 args[i].compare( "--reduce" ) == 0 )
        {
          if( i == args.size() - 1 )
          {
            opt_error = OPT_ERR_ARG_MISSING;
          }
          else
          {
            int percent = atoi( args[i+1].c_str() );
            if( percent < 1 || percent > 99 )
            {
              opt_error = OPT_ERR_ARG_INVALID;
            }
            else
            {
              Params.g_reduce_ratio = (uint32_t)percent;
              i++;
            }
          }
        }
        // -l, --limit
        //
        else if( args[i].compare( "-l" ) == 0 ||
                 args[i].compare( "--limit" ) == 0 )
        {
          if( i == args.size() - 1 )
          {
            opt_error = OPT_ERR_ARG_MISSING;
          }
          else
          {
            int limit = atoi( args[i+1].c_str() );
            if( limit < 0 )
            {
              opt_error = OPT_ERR_ARG_INVALID;
            }
            else
            {
              Params.g_call_limit = (uint32_t)limit;
              i++;
            }
          }
        }
        // -s, --stats, -stats*
        //
        else if( args[i].compare( "-s" ) == 0 ||
                 args[i].compare( "--stats" ) == 0 ||
                 args[i].compare( "-stats" ) == 0 )
        {
          Params.g_print_stats = true;
        }
        // -e, --exclude, -ex*
        //
        else if( args[i].compare( "-e" ) == 0 ||
                 args[i].compare( "--exclude" ) == 0 ||
                 args[i].compare( "-ex" ) == 0 )
        {
          if( i == args.size() - 1 )
          {
            opt_error = OPT_ERR_ARG_MISSING;
          }
          else
          {
            if( !stringList2Vector( args[i+1], Params.g_excl_funcs ) )
              opt_error = OPT_ERR_ARG_INVALID;
            else
              i++;
          }
        }
        // --exclude-file
        //
        else if( args[i].compare( "--exclude-file" ) == 0 )
        {
          if( i == args.size() - 1 )
            opt_error = OPT_ERR_ARG_MISSING;
          else
            Params.g_excl_file = args[++i];
        }
        //
        // -i, --include, -in*
        //
        else if( args[i].compare( "-i" ) == 0 ||
                 args[i].compare( "--include" ) == 0 ||
                 args[i].compare( "-in" ) == 0 )
        {
          if( i == args.size() - 1 )
          {
            opt_error = OPT_ERR_ARG_MISSING;
          }
          else
          {
            if( !stringList2Vector( args[i+1], Params.g_incl_funcs ) )
              opt_error = OPT_ERR_ARG_INVALID;
            else
              i++;
          }
        }
        // --include-file
        //
        else if( args[i].compare( "--include-file" ) == 0 )
        {
          if( i == args.size() - 1 )
            opt_error = OPT_ERR_ARG_MISSING;
          else
            Params.g_incl_file = args[++i];
        }
        // --include-callees, -inc*
        //
        else if( args[i].compare( "--include-callees" ) == 0 ||
                 args[i].compare( "-inc" ) == 0 )
        {
          Params.g_incl_callees = true;
        }
        // unrecognized gen-parameter
        //
        else if( args[i][0] == '-' )
        {
          opt_error = OPT_ERR_UNRECOGNIZED;
        }
        else
        {
          if( Params.input_trcfile.length() == 0 )
            Params.input_trcfile = args[i];
          else
            opt_error = OPT_ERR_UNRECOGNIZED;
        }
      }
      // filt-options
      //
      else
      {
        // -o, --output, -to*
        //
        if( args[i].compare( "-o" ) == 0 ||
            args[i].compare( "--output" ) == 0 ||
            args[i].compare( "-to" ) == 0 )
        {
          if( i == args.size() - 1 )
          {
            opt_error = OPT_ERR_ARG_MISSING;
          }
          else
          {
            char* output_trcfile = OTF_stripFilename( args[i+1].c_str() );
            if( !output_trcfile )
            {
              opt_error = OPT_ERR_ARG_INVALID;
            }
            else
            {
              Params.f_output_trcfile = output_trcfile;
              delete [] output_trcfile;
              i++;
            }
          }
        }
        // -f, --filter, -fi*
        //
        else if( args[i].compare( "-f" ) == 0 ||
                 args[i].compare( "--filter" ) == 0 ||
                 args[i].compare( "-fi" ) == 0 )
        {
          if( i == args.size() - 1 )
            opt_error = OPT_ERR_ARG_MISSING;
          else
            Params.f_input_filtfile = args[++i];
        }
        // -s, --max-streams
        //
        else if( args[i].compare( "-s" ) == 0 ||
                 args[i].compare( "--max-streams" ) == 0 )
        {
          if( i == args.size() - 1 )
          {
            opt_error = OPT_ERR_ARG_MISSING;
          }
          else
          {
            int max_streams = atoi( args[i+1].c_str() );
            if( max_streams < 0 )
            {
              opt_error = OPT_ERR_ARG_INVALID;
            }
            else
            {
              Params.f_max_output_streams = (uint32_t)max_streams;
              i++;
            }
          }
        }
        // --max-file-handles
        //
        else if( args[i].compare( "--max-file-handles" ) == 0 )
        {
          if( i == args.size() - 1 )
          {
            opt_error = OPT_ERR_ARG_MISSING;
          }
          else
          {
            int max_file_handles = atoi( args[i+1].c_str() );
            if( max_file_handles < 1 )
            {
              opt_error = OPT_ERR_ARG_INVALID;
            }
            else
            {
              Params.f_max_file_handles = (uint32_t)max_file_handles;
              i++;
            }
          }
        }
        // --nocompress
        //
        else if( args[i].compare( "--nocompress" ) == 0 )
        {
          Params.f_compress_level = 0;
        }
        // -z*
        //
        else if( args[i].compare( "-z" ) == 0 )
        {
          if( i == args.size() - 1 )
          {
            opt_error = OPT_ERR_ARG_MISSING;
          }
          else
          {
            int level = atoi( args[i+1].c_str() );
            if( level < 0 || level > 9 )
            {
              opt_error = OPT_ERR_ARG_INVALID;
            }
            else
            {
              Params.f_compress_level = (uint32_t)level;
              i++;
            }
          }
        }
        // unrecognized gen-parameter
        //
        else if( args[i][0] == '-' )
        {
          opt_error = OPT_ERR_UNRECOGNIZED;
        }
        // input trace file
        //
        else
        {
          // input trace file already given?
          if( Params.input_trcfile.length() == 0 )
          {
            // no, strip the file name from the ".otf" suffix if present
            char* input_trcfile = OTF_stripFilename( args[i].c_str() );

            // either store the stripped file name or show an error message
            // if the given file name is invalid
            //
            if( input_trcfile )
            {
              Params.input_trcfile = input_trcfile;
              delete [] input_trcfile;
            }
            else
            {
              opt_error = OPT_ERR_OTHER;
              opt_error_other =
                ExeName + ": invalid input trace file name -- '" +
                args[i] + "'";
            }
          }
          else
          {
            opt_error = OPT_ERR_UNRECOGNIZED;
          }
        }
      }
    }

    // abort loop, if an option error occurred
    if( opt_error != OPT_ERR_OK )
      break;
  }

  // parameters are sufficient?
  //
  if( opt_error == OPT_ERR_OK )
  {
    if( Params.input_trcfile.length() == 0 )
    {
      opt_error = OPT_ERR_OTHER;
      opt_error_other = ExeName + ": no input trace file specified";
    }
    else if( Params.mode == MODE_GEN &&
             Params.g_output_filtfile.length() == 0 )
    {
      opt_error = OPT_ERR_OTHER;
      opt_error_other = ExeName + ": no output filter file specified";
    }
    else if( Params.mode == MODE_FILT &&
             Params.f_input_filtfile.length() == 0 )
    {
      opt_error = OPT_ERR_OTHER;
      opt_error_other = ExeName + ": no input filter file specified";
    }
    else if( Params.mode == MODE_FILT &&
             Params.f_output_trcfile.length() == 0 )
    {
      opt_error = OPT_ERR_OTHER;
      opt_error_other = ExeName + ": no output trace file specified";
    }
  }

  // show error message, if necessary
  //
  MASTER
  {
    switch( opt_error )
    {
      case OPT_ERR_OK:
      {
        break;
      }
      case OPT_ERR_ARG_MISSING:
      {
        std::cerr << ExeName << ": option '" << args[i]
                  << "' requires an argument" << std::endl;
        break;
      }
      case OPT_ERR_ARG_INVALID:
      {
        std::cerr << ExeName << ": invalid argument `"
                  << args[i+1] << "' for `" << args[i] << "'" << std::endl;
        break;
      }
      case OPT_ERR_UNRECOGNIZED:
      {
        std::cerr << ExeName << ": unrecognized option -- '"
                  << args[i] << "'" << std::endl;
        break;
      }
      case OPT_ERR_OTHER:
      {
        std::cerr << opt_error_other << std::endl;
        break;
      }
    }

    if( opt_error != OPT_ERR_OK )
    {
      std::cerr << "Try `" << ExeName << " --help' for more information."
                << std::endl;
    }
  }

  return ( opt_error == OPT_ERR_OK );
}

static void
showUsage()
{
  if( enforceGenMode )
  {
    std::cout
      << std::endl
      << " " << ExeName << " - filter generator for VampirTrace." << std::endl
      << std::endl
      << " Syntax: " << ExeName << " [options] <input trace file>" << std::endl;
  }
  else
  {
    std::cout
      << std::endl
      << " " << ExeName << " - filter tool for VampirTrace." << std::endl
      << std::endl
      << " Syntax: " << std::endl
      << "   Generate a filter file:" << std::endl
      << "     " << ExeName << " --gen [gen-options] <input trace file>" << std::endl
      << std::endl
      << "   Filter a trace using an already existing filter file:" << std::endl
      << "     " << ExeName << " [--filt] [filt-options]" << std::endl
      << "       --filter=<input filter file> <input trace file>" << std::endl;
  }

  std::cout
    << std::endl
    << "   options:" << std::endl;

  if( !enforceGenMode )
  {
    std::cout
      << "     --gen               Generate a filter file." << std::endl
      << "                         See 'gen-options' below for valid options." << std::endl
      << std::endl
      << "     --filt              Filter a trace using an already existing" << std::endl
      << "                         filter file. (default)" << std::endl
      << "                         See 'filt-options' below for valid options." << std::endl
      << std::endl;
  }

  std::cout
    << "     -h, --help          Show this help message." << std::endl
    << std::endl
    << "     -V, --version       Show VampirTrace version." << std::endl
    << std::endl
    << "     -p, --progress      Show progress." << std::endl
    << std::endl
    << "     -v, --verbose       Increase output verbosity." << std::endl
    << "                         (can be used more than once)" << std::endl
    << std::endl;
//    << "     -q, --quiet         Enable quiet mode." << std::endl
//    << "                         (only emergency output)" << std::endl
//    << std::endl;

  if( !enforceGenMode )
  {
    std::cout << "   gen-options:" << std::endl;
  }

  std::cout
    << "     -o, --output=FILE   Pathname of output filter file." << std::endl
    << std::endl
    << "     -r, --reduce=N      Reduce the trace size to N percent of the original size." << std::endl
    << "                         The program relies on the fact that the major part of" << std::endl
    << "                         the trace are function calls. The approximation of size" << std::endl
    << "                         will get worse with a rising percentage of" << std::endl
    << "                         communication and other non function calling or" << std::endl
    << "                         performance counter records." << std::endl
    << std::endl
    << "     -l, --limit=N       Limit the number of calls for filtered function to N." << std::endl
    << "                         (default: " << ParamsS::g_default_call_limit << ")" << std::endl
    << std::endl
    << "     -s, --stats         Prints out the desired and the expected percentage" << std::endl
    << "                         of file size." << std::endl
    << std::endl
    << "     -e, --exclude=FUNC[;FUNC;...]" << std::endl
    << "                         Exclude certain functions from filtering." << std::endl
    << "                         A function name may contain wildcards." << std::endl
    << std::endl
    << "     --exclude-file=FILE Pathname of file containing a list of functions to be" << std::endl
    << "                         excluded from filtering." << std::endl
    << std::endl
    << "     -i, --include=FUNC[;FUNC;...]" << std::endl
    << "                         Force to include certain functions into the filter." << std::endl
    << "                         A function name may contain wildcards." << std::endl
    << std::endl
    << "     --include-file=FILE Pathname of file containing a list of functions to be" << std::endl
    << "                         included into the filter." << std::endl
    << std::endl
    << "     --include-callees   Automatically include callees of included functions" << std::endl
    << "                         as well into the filter." << std::endl
    << std::endl;

  if( !enforceGenMode )
  {
    std::cout
      << "   filt-options:" << std::endl
      << "     -o, --output=FILE   Pathname of output trace file." << std::endl
      << std::endl
      << "     -f, --filter=FILE   Pathname of input filter file." << std::endl
      << std::endl
      << "     -s, --max-streams=N Maximum number of output streams." << std::endl
      << "                         Set this to 0 to get the same number of output streams" << std::endl
#ifndef VT_MPI
      << "                         as input streams." << std::endl
#else // VT_MPI
      << "                         as MPI processes used, but at least the number of" << std::endl
      << "                         input streams." << std::endl
#endif // VT_MPI
      << "                         (default: " << ParamsS::f_default_max_output_streams << ")" << std::endl
      << std::endl
      << "     --max-file-handles=N" << std::endl
      << "                         Maximum number of files that are allowed to be open" << std::endl
      << "                         simultaneously." << std::endl
      << "                         (default: " << ParamsS::f_default_max_file_handles << ")" << std::endl
      << std::endl
      << "     --nocompress        Don't compress output trace files." << std::endl
      << std::endl;
  }

  std::cout
    << "   obsolete options and environment variables:" << std::endl
    << "   (still available for backward-compatibility)" << std::endl;

  if( !enforceGenMode )
  {
    std::cout
      << "     -gen                equivalent to '--gen'" << std::endl
      << "     -filt               equivalent to '--filt'" << std::endl
      << std::endl
      << "   gen-options:" << std::endl;
  }

  std::cout
    << "     -fo                 equivalent to '-o' or '--output'" << std::endl
    << "     -stats              equivalent to '-s' or '--stats'" << std::endl
    << "     -ex                 equivalent to '-e' or '--exclude'" << std::endl
    << "     -in                 equivalent to '-i' or '--include'" << std::endl
    << "     -inc                equivalent to '--include-callees'" << std::endl
    << std::endl
    << "     environment variables:" << std::endl
    << "       TRACEFILTER_EXCLUDEFILE" << std::endl
    << "                         equivalent to '--exclude-file'" << std::endl
    << "       TRACEFILTER_INCLUDEFILE" << std::endl
    << "                         equivalent to '--include-file'" << std::endl
    << std::endl;

  if( !enforceGenMode )
  {
    std::cout
      << "   filt-options:" << std::endl
      << "     -to                 equivalent to '-o' or '--output'" << std::endl
      << "     -fi                 equivalent to '-f' or '--filter'" << std::endl
      << "     -z LEVEL            Set the compression level. Level reaches from 0 to 9" << std::endl
      << "                         where 0 is no compression (--nocompress) and 9 is the" << std::endl
      << "                         highest level." << std::endl
      << "                         (default: " << Params.f_default_compress_level << ")" << std::endl
      << std::endl;
  }
}

static bool
stringList2Vector( const std::string& str, std::vector<std::string>& vec,
                   const std::string& delim )
{
  bool error = false;

  if( str.length() > 0 )
  {
    // convert input string to char* for strtok
    char* cstr = strdup( str.c_str() );

    // extract list entries from string
    //

    char* token = strtok( cstr, delim.c_str() );
    std::string entry;
    do
    {
      entry = token;

      // trim list entry
      //
      std::string::size_type si = entry.find_first_not_of( " " );
      if( si != std::string::npos )
        entry.erase( 0, si );
      si = entry.find_last_not_of( " " );
      if( si != std::string::npos )
        entry.erase( si + 1 );

      // add list entry to output vector, if it's not empty
      //
      if( entry.length() > 0 )
        vec.push_back( entry );
      else
        error = true;

    } while( !error && ( token = strtok( 0, delim.c_str() ) ) );

    free( cstr );
  }
  else
  {
    error = true;
  }

  return !error;
}
