/*
    File: llvmoPackage.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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

#ifndef	llvmoCando_H
#define llvmoCando_H

#include "core/common.h"


#define CLASP_LINK_PASS_NAME "ClaspLinkPass"
#define GLOBAL_BOOT_FUNCTIONS_NAME "globalClaspBootFunctions"
#define GLOBAL_BOOT_FUNCTIONS_SIZE_NAME "globalClaspBootFunctionsSize"

NAMESPACE_PACKAGE_ASSOCIATION(llvmo,LlvmoPkg,"LLVM-SYS");


namespace llvmo
{




    class LlvmoExposer : public core::Exposer
    {
    private:
    public:
        DISABLE_NEW();
    LlvmoExposer(core::Lisp_sp lisp) : Exposer(lisp,LlvmoPkg) {};
        virtual void expose(core::Lisp_sp lisp,WhatToExpose what) const;
    };


    FORWARD(ExecutionEngine);	
    FORWARD(Module);
    FORWARD(Type)
    void af_addAllSymbolsToExecutionEngine(llvmo::ExecutionEngine_sp engine, llvmo::Module_sp module, llvmo::Type_sp shared_ptr_type );


    /* Call this before ANY symbols are created and they will be saved
       and can be added to an llvm::ExecutionEngine as a GlobalValue
    */
    void redirect_llvm_interface_addSymbol();



};
#endif
