#ifndef _core_useBoostRegex_h
#define _core_useBoostRegex_h

// If you need boost::regex, include this file,  it has all the pragmas to suppress warnings set up


#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wchar-subscripts"
#pragma GCC diagnostic ignored "-Wunused-function"
// For some reason boost regex uses "tolower" which conflicts with macro
#undef tolower 
#undef toupper
#include <boost/regex.hpp>
#pragma clang diagnostic pop


#endif // _core_useBoostRegex_H
