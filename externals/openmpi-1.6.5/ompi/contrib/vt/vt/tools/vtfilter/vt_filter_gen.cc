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

#include <iostream>
#include <sstream>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int vtfilter_main( int argc, char** argv );

//////////////////// class FilterGeneratorC ////////////////////

// public methods
//

FilterGeneratorC::FilterGeneratorC() : FilterCommonC()
{
  // empty
}

FilterGeneratorC::~FilterGeneratorC()
{
  // empty
}

bool
FilterGeneratorC::run()
{
  bool error = false;

  VPrint( 1, "Generating filter file\n" );

  int argc;
  char** argv = 0;
  char** envp = 0;

  do
  {
    envp = new char*[2];
    envp[0] = envp[1] = 0;

    // convert program parameters for the old vtfilter
    //
    if( ( error = !getOldParams( argc, argv, envp ) ) )
      break;

    // run old vtfilter in gen-mode
    //
    if( ( error = ( vtfilter_main( argc, argv ) != 0 ) ) )
      break;

    VPrint( 1, "Done\n" );

  } while( false );

  // free some memory
  //

  if( argv )
  {
    for( int i = 0; i < argc; i++ )
      delete [] argv[i];
    delete [] argv;
  }

  //if( envp )
  {
    if( envp[0] )
      delete [] envp[0];
    if( envp[1] )
      delete [] envp[1];
    delete [] envp;
  }

  return !error;
}

// private methods
//

bool
FilterGeneratorC::getOldParams( int& argc, char**& argv, char**& envp )
{
  // at this point we should have an input trace file and an output filter file
  //
  vt_assert( !Params.input_trcfile.empty() );
  vt_assert( !Params.g_output_filtfile.empty() );

  // vector of converted command line options
  std::vector<std::string> args;

  std::ostringstream os;

  // pathname of program's executable (argv[0])
  args.push_back( "vtfilter" );

  // -gen
  args.push_back( "-gen" );

  // -p
  //
  if( Params.show_progress )
    args.push_back( "-p" );

  // -fo
  //
  args.push_back( "-fo" );
  args.push_back( Params.g_output_filtfile );

  // -r
  //
  if( Params.g_reduce_ratio == 0 )
    Params.g_reduce_ratio = 100;
  args.push_back( "-r" );
  os << Params.g_reduce_ratio;
  args.push_back( os.str() );
  os.str(""); os.clear();

  // -l
  //
  args.push_back( "-l" );
  os << Params.g_call_limit;
  args.push_back( os.str() );
  os.str(""); os.clear();

  // -stats
  //
  if( Params.g_print_stats )
    args.push_back( "-stats" );

  // -in
  //
  if( Params.g_incl_funcs.size() > 0 )
  {
    args.push_back( "-in" );

    std::string incl_list = "";
    for( uint32_t i = 0; i < Params.g_incl_funcs.size(); i++ )
    {
      incl_list += Params.g_incl_funcs[i];
      if( i < Params.g_incl_funcs.size() - 1 )
        incl_list += ",";
    }
    args.push_back( incl_list );
  }

  // -ex
  //
  if( Params.g_excl_funcs.size() > 0 )
  {
    args.push_back( "-ex" );

    std::string excl_list = "";
    for( uint32_t i = 0; i < Params.g_excl_funcs.size(); i++ )
    {
      excl_list += Params.g_excl_funcs[i];
      if( i < Params.g_excl_funcs.size() - 1 )
        excl_list += ",";
    }
    args.push_back( excl_list );
  }

  // -inc
  //
  if( Params.g_incl_callees )
    args.push_back( "-inc" );

  // input trace file
  args.push_back( Params.input_trcfile );

  // env. TRACEFILTER_INCLUDEFILE
  //
  if( !Params.g_incl_file.empty() )
  {
    envp[0] = new char[24 + Params.g_incl_file.length() + 1];
    vt_assert( envp[0] );
    sprintf( envp[0], "TRACEFILTER_INCLUDEFILE=%s",
             Params.g_incl_file.c_str() );
    putenv( envp[0] );
  }

  // env. TRACEFILTER_EXCLUDEFILE
  //
  if( !Params.g_excl_file.empty() )
  {
    envp[1] = new char[24 + Params.g_excl_file.length() + 1];
    vt_assert( envp[1] );
    sprintf( envp[1], "TRACEFILTER_EXCLUDEFILE=%s",
             Params.g_excl_file.c_str() );
    putenv( envp[1] );
  }

  // convert C++ vector and strings to array of char*
  //
  argc = args.size();
  argv = new char *[argc];

  for( int i = 0; i < argc; i++ )
  {
    argv[i] = strdup( args[i].c_str() );
    vt_assert( argv[i] );
  }

  return true;
}
