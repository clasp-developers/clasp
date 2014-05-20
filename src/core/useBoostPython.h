#ifndef _core_useBoostPython_H
#define _core_useBoostPython_H

#ifdef USEBOOSTPYTHON
#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#include <boost/python.hpp>
#include <boost/python/raw_function.hpp>
#pragma clang diagnostic pop
#endif

#endif
