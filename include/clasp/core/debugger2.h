#ifndef debugger2_h
#define debugger2_h

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>

namespace core {

  T_mv early_debug(T_sp condition, bool can_continue);

}; // namespace core

#endif /* guard */
