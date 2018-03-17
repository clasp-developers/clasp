/*
    File: claspLinkPass.cc
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
#include <llvm/Pass.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

#include <clasp/llvmo/llvmoPackage.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/core/wrappers.h>

using namespace llvm;

namespace {
struct ClaspLinkPass : public ModulePass {
  static char ID;
  ClaspLinkPass() : ModulePass(ID) {}

  virtual bool runOnModule(Module &M) {
    errs() << "ClaspLinkPass " << __FILE__ << ":" << __LINE__ << '\n';
    GlobalVariable *funcs = M.getNamedGlobal(GLOBAL_BOOT_FUNCTIONS_NAME);
    if (funcs == NULL) {
      errs() << "Could not find global variable " << GLOBAL_BOOT_FUNCTIONS_NAME << '\n';
      return false;
    }
    llvm::PointerType *t = funcs->getType();
#if 0
    errs() << "Dumping t\n";
    t->dump();
    errs() << '\n';
    llvm::Type *et = t->getElementType();
    errs() << "Dumping et\n";
    et->dump();
    errs() << '\n';
    int num = et->getArrayNumElements();
    errs() << "Number of elements: " << num << '\n';
    funcs->dump();
#endif
    llvm::ConstantInt *ci = llvm::ConstantInt::get(M.getContext(), llvm::APInt(/*nbits*/ 32, num, true));
#if 0
#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
    llvm::GlobalVariable *gv = new llvm::GlobalVariable(M, llvm::IntegerType::get(M.getContext(), 32), true, llvm::GlobalValue::InternalLinkage, ci, GLOBAL_BOOT_FUNCTIONS_SIZE_NAME);
#pragma clang diagnostic pop
#endif
    return true; // Change this to true once we modify the module
  }
};
}

namespace llvmo {

#define ARGS_af_addGlobalBootFunctionsSizePass "(pass-manager)"
#define DECL_af_addGlobalBootFunctionsSizePass ""
#define DOCS_af_addGlobalBootFunctionsSizePass "addGlobalBootFunctionsSizePass"
CL_DEFUN void llvm_sys__addGlobalBootFunctionsSizePass(llvmo::PassManager_sp passManager) {
  ModulePass *claspLinkPass = new ClaspLinkPass();
  passManager->wrappedPtr()->add(claspLinkPass);
}

void initialize_claspLinkPass() {
//  core::af_def(LlvmoPkg, "addGlobalBootFunctionsSizePass", &af_addGlobalBootFunctionsSizePass);
}
};

#if 0
char ClaspLinkPass::ID = 0;
static RegisterPass<ClaspLinkPass> X(CLASP_LINK_PASS_NAME, "ClaspLinkPass", false, false);
#endif
