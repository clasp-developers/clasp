#pragma once

/*
    File: llvmoPackage.h
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

#include <clasp/core/common.h>

PACKAGE_USE("COMMON-LISP");
PACKAGE_SHADOW("FUNCTION");
NAMESPACE_PACKAGE_ASSOCIATION(llvmo, LlvmoPkg, "LLVM-SYS");

namespace llvmo {

FORWARD(ClaspJIT);

class LlvmoExposer_O : public core::Exposer_O {
private:
public:
  LlvmoExposer_O(core::LispPtr lisp) : Exposer_O(lisp, LlvmoPkg){};
  virtual void expose(core::LispPtr lisp, WhatToExpose what) const;
};

FORWARD(ExecutionEngine);
FORWARD(LLVMContext);
FORWARD(Module);
FORWARD(Type)
void af_addAllSymbolsToExecutionEngine(llvmo::ExecutionEngine_sp engine, llvmo::Module_sp module, llvmo::Type_sp shared_ptr_type);

/* Call this before ANY symbols are created and they will be saved
       and can be added to an llvm::ExecutionEngine as a GlobalValue
    */
void redirect_llvm_interface_addSymbol();

bool llvm_sys__load_ll(core::Pathname_sp filename, bool verbose, bool print, core::T_sp externalFormat, size_t startupID = 0);
bool llvm_sys__load_bc(core::Pathname_sp filename, bool verbose, bool print, core::T_sp externalFormat, size_t startupID = 0);

ClaspJIT_sp llvm_sys__clasp_jit();

void initialize_llvm();
void initialize_ClaspJIT();

core::Pointer_mv cmp__compile_trampoline(core::T_sp name);

}; // namespace llvmo
