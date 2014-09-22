#include "llvm/Pass.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

#include "llvmoPackage.h"
#include "llvmoExpose.h"
#include "core/wrappers.h"

using namespace llvm;

namespace {
  struct ClaspLinkPass : public ModulePass {
    static char ID;
    ClaspLinkPass() : ModulePass(ID) {}

    virtual bool runOnModule(Module &M) {
        errs() << "ClaspLinkPass " << __FILE__ << ":" <<__LINE__ << '\n';
        GlobalVariable* funcs = M.getNamedGlobal(GLOBAL_BOOT_FUNCTIONS_NAME);
        if (funcs == NULL) {
            errs() << "Could not find global variable " << GLOBAL_BOOT_FUNCTIONS_NAME << '\n';
            return false;
        }
        llvm::PointerType* t = funcs->getType();
        errs() << "Dumping t\n";
        t->dump();
        errs() << '\n';
        llvm::Type* et = t->getElementType();
        errs() << "Dumping et\n";
        et->dump();
        errs() << '\n';
        int num = et->getArrayNumElements();
        errs() << "Number of elements: " << num << '\n';
        funcs->dump();
        llvm::ConstantInt* ci = llvm::ConstantInt::get(M.getContext(), llvm::APInt(/*nbits*/32, num, true));
#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
        llvm::GlobalVariable* gv = new llvm::GlobalVariable(M
                                                            , llvm::IntegerType::get(M.getContext(),32)
                                                            , true
                                                            , llvm::GlobalValue::InternalLinkage
                                                            , ci
                                                            , GLOBAL_BOOT_FUNCTIONS_SIZE_NAME );
#pragma clang diagnostic pop
        return true;  // Change this to true once we modify the module
    }
  };
}


namespace llvmo {

    
#define ARGS_af_addGlobalBootFunctionsSizePass "(pass-manager)"
#define DECL_af_addGlobalBootFunctionsSizePass ""
#define DOCS_af_addGlobalBootFunctionsSizePass "addGlobalBootFunctionsSizePass"
    void af_addGlobalBootFunctionsSizePass(llvmo::PassManager_sp passManager)
    {
        ModulePass* claspLinkPass = new ClaspLinkPass();
        passManager->wrappedPtr()->add(claspLinkPass);
    }


    void initialize_claspLinkPass()
    {
        core::af_def(LlvmoPkg,"addGlobalBootFunctionsSizePass",&af_addGlobalBootFunctionsSizePass);
    }

};



char ClaspLinkPass::ID = 0;
static RegisterPass<ClaspLinkPass> X(CLASP_LINK_PASS_NAME, "ClaspLinkPass", false, false);
