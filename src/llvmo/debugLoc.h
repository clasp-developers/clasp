#ifndef _llvmo_debugLoc_H_
#define _llvmo_debugLoc_H_


#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRBuilder.h"
#include "core/foundation.h"
#include "core/object.h"
#include "debugLoc.fwd.h"
#include "llvmoExpose.fwd.h"
#include "llvmo/debugInfoExpose.fwd.h"
#include "llvmo/llvmoPackage.h"

    
namespace llvmo
{
    class DebugLoc_O : public core::T_O
    {
	LISP_BASE1(core::T_O);
	LISP_CLASS(llvmo,LlvmoPkg,DebugLoc_O,"DebugLoc");
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(DebugLoc_O);
    public:
	static DebugLoc_sp get(int lineno, int column, DebugInfo_sp debugInfo);
	    
    private: // instance variables here
	llvm::DebugLoc 	_DebugLoc;
    public: // Functions here
	llvm::DebugLoc& debugLoc() { return this->_DebugLoc;};
	uint getLine() const { return this->_DebugLoc.getLine();};
	uint getCol() const { return this->_DebugLoc.getCol();};
	MDNode_sp getScope(LLVMContext_sp context) const;
    }; // DebugLoc class
	
}; // llvmo namespace
    TRANSLATE(llvmo::DebugLoc_O);
    
    

    
#endif /* _llvmo_debugLoc_H_ */
