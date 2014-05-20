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

#ifndef _VT_LIBWRAPGEN_PARSER_H_
#define _VT_LIBWRAPGEN_PARSER_H_

#include "vt_inttypes.h"

// Parser class
//
class ParserC
{
public:

  // contructor
  ParserC();

  // destructor
  ~ParserC();

  // parse header files
  bool parse();

private:

  static void statementCallback( void* st );

  // number of statements to process
  uint32_t m_statementNum;

};

#endif // _VT_LIBWRAPGEN_PARSER_H_
