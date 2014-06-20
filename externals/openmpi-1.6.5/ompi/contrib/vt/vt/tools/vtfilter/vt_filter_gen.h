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

#ifndef _VT_FILTER_GEN_H_
#define _VT_FILTER_GEN_H_

#include "vt_filter_common.h"

#include <string>

//
// FilterGeneratorC class
//
class FilterGeneratorC : public FilterCommonC
{
public:

  // contructor
  FilterGeneratorC();

  // destructor
  ~FilterGeneratorC();

  // generate a filter file
  bool run();

private:

  // convert program parameters for the old vtfilter
  bool getOldParams( int& argc, char**& argv, char**& envp );

  // content of obsolete environment variable TRACEFILTER_INCLUDEFILE
  std::string m_inclFileEnv;

  // content of obsolete environment variable TRACEFILTER_EXCLUDEFILE
  std::string m_exclFileEnv;

};

#endif // _VT_FILTER_GEN_H_
