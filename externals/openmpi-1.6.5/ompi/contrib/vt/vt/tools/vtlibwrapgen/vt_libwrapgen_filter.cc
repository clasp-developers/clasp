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
#include "vt_libwrapgen_filter.h"

#include <fstream>
#include <iostream>
#include <sstream>
#include <algorithm>


//////////////////// class Filter ////////////////////

// public methods
//

FilterC::FilterC()
{
  // Empty
}

FilterC::~FilterC()
{
  // Empty
}

bool
FilterC::read()
{
  // open filter file
  //
  std::ifstream in( Params.g_input_filtfile.c_str() );
  if( !in )
  {
    std::cerr << ExeName << ": Could not open filter file "
              << Params.g_input_filtfile << std::endl;
    return false;
  }

  std::string line;
  uint32_t line_no = 0;
  bool error = false;

  // read lines of filter file
  //
  while( std::getline( in, line ) )
  {
    // increment line number
    line_no++;

    // trim leading and trailing spaces from line
    trim( line );

    // ignore line, if it's empty of a comment
    if( line.length() == 0 || line[0] == '#' ) continue;

    // search double-dashes in line
    size_t dashdash = line.find("--");
    if( dashdash == std::string::npos || dashdash == 0 )
    {
      error = true;
      break;
    }

    // get (list of) pattern and whose call limit
    //
    std::string pattern_list = line.substr( 0, dashdash - 1 );
    trim( pattern_list );
    std::string climit = line.substr( dashdash + 2 );
    trim( climit );

    if( pattern_list.length() == 0 || climit.length() == 0 )
    {
      error = true;
      break;
    }

    // add filter directive to vector
    //
    std::istringstream pattern_in( pattern_list );
    std::string pattern;

    // deny file/function, if call limit is 0
    bool allowed = ( climit.compare( "0" ) == 0 ) ? false : true;

    while( std::getline( pattern_in, pattern, ';' ) )
    {
      filtPattern.push_back( PatternS( pattern, allowed ) );
    }
  }

  // close filter file
  in.close();

  // parse error occurred ?
  //
  if( error )
  {
    std::cerr << ExeName << ": " << Params.g_input_filtfile << ":" << line_no
              << ": Could not be parsed" << std::endl;
    return false;
  }

  return true;
}

bool
FilterC::constraint( const std::string& file, const std::string& funcName )
{
  static std::string sysheader_prefix = VT_LIBWRAPGEN_DEFAULT_SYSHEADER_PREFIX;

  // check whether function is allowed or denied
  //
  std::vector<PatternS>::iterator pt_it =
    std::find( filtPattern.begin(), filtPattern.end(), funcName );
  if( pt_it != filtPattern.end() )
    return !pt_it->allowed;

  // system header file ?
  //
  if( file.length() > sysheader_prefix.length() &&
      file.compare( 0, sysheader_prefix.length(), sysheader_prefix ) == 0 )
  {
    // check whether header file is specified
    //
    if( std::find( Params.g_input_headers.begin(),
                   Params.g_input_headers.end(),
                   file ) == Params.g_input_headers.end() )
    {
      // check whether header file is an allowed system header file
      //
      if( std::find( Params.g_allowed_sysheaders.begin(),
                     Params.g_allowed_sysheaders.end(),
                     file ) == Params.g_allowed_sysheaders.end() )
        return true;
    }
  }

  return false;
}
