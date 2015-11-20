/*
    File: intrinsics.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#ifndef llvmo_intrinsics_H
#define llvmo_intrinsics_H

extern "C" {

void cc_call_with_variable_bound(core::T_mv *result, core::T_O *symbol, core::T_O *value, core::T_O *thunk);
void cc_funwind_protect(core::T_mv *result, core::T_O *protected_fn, core::T_O *cleanup_fn);
void cc_catch(core::T_mv *result, core::T_O *tag, core::T_O *func);
void cc_throw(core::T_O *tag, core::T_O *resultP);
};

namespace llvmo {

void redirect_llvm_interface_addSymbol();

void initialize_intrinsics();
void initialize_link_intrinsics();

typedef enum { noFunctionBoundToSymbol,
               badKeywordArgument,
               couldNotCoerceToClosure,
               destinationMustBeActivationFrame,
               invalidIndexForFunctionFrame,
               unboundSymbolValue,
               unboundSymbolFunction,
               unboundSymbolSetfFunction
} ErrorCode;

extern void intrinsic_error(ErrorCode err, core::T_sp arg0 = _Nil<core::T_O>(), core::T_sp arg1 = _Nil<core::T_O>(), core::T_sp arg2 = _Nil<core::T_O>());
}

#endif
