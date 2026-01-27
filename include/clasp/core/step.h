#pragma once

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>

namespace core {

void breakstep(T_sp source, void* frame);
void breakstep_args(void* frame, Function_sp fun, List_sp args);

}; // namespace core
