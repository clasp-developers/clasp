#ifndef core_BridgeCommonLisp_H
#define core_BridgeCommonLisp_H

//
// Plays the role of Python.h
//


#include <assert.h>
#include "brclport.h"

#include "foundation.h"
#include "object.h"
#include "cons.h"
#include "lisp.h"
#include "singleDispatchMethod.h"

typedef core::T_O BrclObject;
typedef core::Class_O BrclTypeObject;
typedef core::SingleDispatchMethod_O BrclMethodObject;

core::T_O* Brcl_None = _Nil<core::T_O>().get();


#endif
