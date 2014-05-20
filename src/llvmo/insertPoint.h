#ifndef _llvmo_insertPoint_H_
#define _llvmo_insertPoint_H_


#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRBuilder.h"
#include "core/foundation.h"
#include "core/object.h"
#include "insertPoint.fwd.h"

#include "llvmo/llvmoPackage.h"

    
namespace llvmo
{
    class InsertPoint_O : public core::T_O
    {
	LISP_BASE1(core::T_O);
	LISP_CLASS(llvmo,LlvmoPkg,InsertPoint_O,"InsertPoint");
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(InsertPoint_O);
    public:
	static InsertPoint_sp create(llvm::IRBuilderBase::InsertPoint& ip);
	    
    private: // instance variables here
	llvm::IRBuilderBase::InsertPoint 	_InsertPoint;
    public: // Functions here
	llvm::IRBuilderBase::InsertPoint& insertPoint() { return this->_InsertPoint;};
    }; // InsertPoint class
	
}; // llvmo namespace
    TRANSLATE(llvmo::InsertPoint_O);
    
    

    
#endif /* _llvmo_insertPoint_H_ */
