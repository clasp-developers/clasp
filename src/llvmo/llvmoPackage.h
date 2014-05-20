
#ifndef	llvmoCando_H
#define llvmoCando_H

#include "core/common.h"


NAMESPACE_PACKAGE_ASSOCIATION(llvmo,LlvmoPkg,"LLVM-SYS");


namespace llvmo
{




    class LlvmoExposer : public core::PackageExposer
    {
    private:
    public:
    LlvmoExposer(core::Lisp_sp lisp) : PackageExposer(lisp,LlvmoPkg) {};
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
