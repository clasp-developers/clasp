#ifndef	asttooling_astExpose_H
#define asttooling_astExpose_H

#include "core/common.h"
#include "clang/AST/DeclBase.h"

namespace asttooling
{


    core::T_sp mostDerivedDecl(const clang::Decl* d);
    core::T_sp mostDerivedStmt(const clang::Stmt* d);
    core::T_sp mostDerivedType(const clang::Type* d);


    void initialize_astExpose();



};
#endif

