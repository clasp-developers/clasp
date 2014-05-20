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

#ifndef _VT_LIBWRAPGEN_FILTER_H_
#define _VT_LIBWRAPGEN_FILTER_H_

#include "util/util.h"

#include <string>
#include <vector>

#include <fnmatch.h>  // :TODO: not portable!
#include <stdlib.h>

// Filter class
//
class FilterC
{
public:

  // contructor
  FilterC();

  // destructor
  ~FilterC();

  // read filter file
  bool read( void );

  // register function and check whether it shall be wrapped
  bool constraint( const std::string& file, const std::string& funcName );

private:

  // data structure for a filter entry
  //
  struct PatternS
  {
    PatternS(const std::string& _pattern, bool _allowed)
      : pattern(_pattern), allowed(_allowed) {}

    std::string pattern;
    bool        allowed;

    bool operator==( const std::string& a ) const
    {
      return ( fnmatch( pattern.c_str(), a.c_str(), 0 ) == 0 );
    }
  };

  // trim leading and trailing spaces from a string
  //
  void trim( std::string& str )
  {
    size_t len = str.length();
    if( len == 0 ) return;

    char* c_str = new char[len+1];

    str.copy( c_str, len );
    c_str[len] = '\0';

    vt_strtrim( c_str );

    str = c_str;

    delete [] c_str;
  }

  // vector of filter pattern
  std::vector<PatternS> filtPattern;

};

#endif // _VT_LIBWRAPGEN_FILTER_H_
