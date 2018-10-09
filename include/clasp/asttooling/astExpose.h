/*
    File: astExpose.h
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
#ifndef asttooling_astExpose_H
#define asttooling_astExpose_H

#include <clasp/core/common.h>
#include <clang/AST/DeclBase.h>

PACKAGE_USE("COMMON-LISP");
PACKAGE_NICKNAME("CAST");
NAMESPACE_PACKAGE_ASSOCIATION(clang_ast,ClangAstPkg,"CLANG-AST");

namespace asttooling {

core::T_sp mostDerivedDecl(const clang::Decl *d);
core::T_sp mostDerivedStmt(const clang::Stmt *d);
core::T_sp mostDerivedType(const clang::Type *d);

void initialize_astExpose();
};
#endif
