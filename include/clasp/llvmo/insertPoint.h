/*
    File: insertPoint.h
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
#ifndef _llvmo_insertPoint_H_
#define _llvmo_insertPoint_H_

#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRBuilder.h>
#include <clasp/core/object.h>
#include <clasp/llvmo/insertPoint.fwd.h>

#include <clasp/llvmo/llvmoPackage.h>

namespace llvmo {
class InsertPoint_O : public core::General_O {
  LISP_CLASS(llvmo, LlvmoPkg, InsertPoint_O, "InsertPoint",core::General_O);

public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(InsertPoint_O);

public:
  static InsertPoint_sp create(llvm::IRBuilderBase::InsertPoint &ip);

private: // instance variables here
  llvm::IRBuilderBase::InsertPoint _InsertPoint;

public: // Functions here
  llvm::IRBuilderBase::InsertPoint &insertPoint() { return this->_InsertPoint; };
}; // InsertPoint class

}; // llvmo namespace

#endif /* _llvmo_insertPoint_H_ */
