/*
    File: astExpose.cc
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

#include <clasp/core/foundation.h>

#include <clang/AST/Comment.h>
#include <clang/AST/ASTDumper.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/AST/DeclFriend.h>
#include <clang/AST/DeclOpenMP.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/StmtCXX.h>
#include <clang/AST/ExprObjC.h>
#include <clang/AST/StmtObjC.h>
#include <clang/AST/StmtOpenMP.h>
#include <clang/ASTMatchers/ASTMatchers.h>
#include <clang/ASTMatchers/Dynamic/VariantValue.h>
#include <clang/Tooling/Tooling.h>
#include <clang/Tooling/Refactoring.h>
#include <clang/ASTMatchers/ASTMatchFinder.h>
#include <clang/Frontend/FrontendActions.h>

#include <clasp/core/object.h>
#include <clasp/core/array.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/package.h>
#include <clasp/llvmo/llvmoExpose.h>

#include <clasp/asttooling/astExpose.h>
#include <clasp/llvmo/translators.h>
#include <clasp/asttooling/translators.h>
#include <clasp/core/symbolTable.h>
#include <clasp/asttooling/asttoolingPackage.h>

using namespace clang;
using namespace clbind;

//
// This needs to be before clbind is included
//
#ifdef USE_MPS
#define NAMESPACE_clbind_clang
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_clbind_clang
#endif

#include <clasp/clbind/clbind.h>

namespace asttooling {
core::T_sp mostDerivedDecl(const clang::Decl* cd);
core::T_sp mostDerivedStmt(const clang::Stmt* x);
core::T_sp mostDerivedType(const clang::Type* x);
}; // namespace asttooling

namespace translate {

#define DECL(_T_, _ignore_)                                                                                                        \
  template <> struct to_object<clang::_T_##Decl*> {                                                                                \
    static core::T_sp convert(clang::_T_##Decl* p) { return asttooling::mostDerivedDecl(p); }                                      \
  };
#include <clang/AST/DeclNodes.inc>
#undef DECL

#define STMT(_T_, _ignore_)                                                                                                        \
  template <> struct to_object<clang::_T_*> {                                                                                      \
    static core::T_sp convert(clang::_T_* p) { return asttooling::mostDerivedStmt(p); }                                            \
  };
#include <clang/AST/StmtNodes.inc>
#undef STMT

#define TYPE(_T_, _ignore_)                                                                                                        \
  template <> struct to_object<clang::_T_##Type*> {                                                                                \
    static core::T_sp convert(clang::_T_##Type* p) { return asttooling::mostDerivedType(p); }                                      \
  };
#define ABSTRACT_TYPE(_C_, _B_) TYPE(_C_, _B_)
#define NON_CANONICAL_TYPE(_C_, _B_) TYPE(_C_, _B_)
#define DEPENDENT_TYPE(_C_, _B_) TYPE(_C_, _B_)
#define NON_CANONICAL_UNLESS_DEPENDENT_TYPE(_C_, _B_) TYPE(_C_, _B_)
#include "clang/AST/TypeNodes.inc"
#undef ABSTRACT_TYPE
#undef NON_CANONICAL_TYPE
#undef DEPENDENT_TYPE
#undef NON_CANONICAL_UNLESS_DEPENDENT_TYPE
#undef TYPE
}; // namespace translate

using namespace clbind;
using namespace clang;

typedef clbind::Wrapper<clang::TypeLoc, std::unique_ptr<clang::TypeLoc>> TypeLoc_wrapper;

typedef clbind::Wrapper<clang::TemplateName, std::unique_ptr<clang::TemplateName>> TemplateName_wrapper;

namespace asttooling {

template <typename T, typename B> core::T_sp cast_decl(clang::Decl* d) {
  if (T* x = dyn_cast<T>(d)) {
    return clbind::Wrapper<T, T*>::make_wrapper(x, reg::registered_class<T>::id);
  }
  SIMPLE_ERROR("Could not cast Decl to known Decl");
}

template <typename T> core::T_sp cast_stmt(clang::Stmt* d) {
  T* x = llvm::cast<T>(d);
  return clbind::Wrapper<T, T*>::make_wrapper(x, reg::registered_class<T>::id);
}

core::T_sp mostDerivedDecl(const clang::Decl* cd) {
  if (!cd)
    return nil<core::T_O>();
  clang::Decl* d = const_cast<clang::Decl*>(cd);
  if (!d) {
    SIMPLE_ERROR("Could not downcast clang::Decl @{} to most derived object", (void*)(cd));
  }
  switch (d->getKind()) {
#undef NAMED
#define DECL(_C_, _B_)                                                                                                             \
  case clang::Decl::_C_: {                                                                                                         \
    core::T_sp res = cast_decl<clang::_C_##Decl, clang::_B_>(d);                                                                   \
    return res;                                                                                                                    \
  }
#define ABSTRACT_DECL(_X_)
#include <clang/AST/DeclNodes.inc>
#undef ABSTRACT_DECL
#undef DECL
  default:
    break;
  };
  SIMPLE_ERROR("Could not cast Decl");
};

core::T_sp mostDerivedStmt(const clang::Stmt* x) {
  if (!x)
    return nil<core::T_O>();
  clang::Stmt* s = const_cast<clang::Stmt*>(x);
  if (!s) {
    SIMPLE_ERROR("Could not downcast clang::Stmt @{} to most derived object", (void*)(x));
  }

#define STMT(_C_, _ignore_)                                                                                                        \
  case clang::Stmt::_C_##Class: {                                                                                                  \
    core::T_sp res = cast_stmt<clang::_C_>(s);                                                                                     \
    return res;                                                                                                                    \
  }
#define ABSTRACT_STMT(_x_)
  switch (s->getStmtClass()) {
  case clang::Stmt::NoStmtClass:
    break;
#include <clang/AST/StmtNodes.inc>
#undef ABSTRACT_STMT
#undef STMT
  default:
    SIMPLE_ERROR("Add a case for missing LLVM classes");
  }
  SIMPLE_ERROR("Could not cast Stmt");
};

template <typename T> core::T_sp cast_type(clang::Type* d) {
  T* x = llvm::cast<T>(d);
  return clbind::Wrapper<T, T*>::make_wrapper(x, reg::registered_class<T>::id);
}

core::T_sp mostDerivedType(const clang::Type* x) {
  if (!x)
    return nil<core::T_O>();
  clang::Type* s = const_cast<clang::Type*>(x);
  if (!s) {
    SIMPLE_ERROR("Could not downcast clang::Type @{} to most derived object", (void*)(x));
  }
  switch (s->getTypeClass()) {
    //
    // Copied from /llvm13/include/clang/AST/TypeNodes.inc
    //
#define TYPE(_C_, _B_)                                                                                                             \
  case clang::Type::_C_: {                                                                                                         \
    core::T_sp res = cast_type<clang::_C_##Type>(s);                                                                               \
    return res;                                                                                                                    \
  }
#define ABSTRACT_TYPE(_C_, _B_)
#define NON_CANONICAL_TYPE(_C_, _B_) TYPE(_C_, _B_)
#define DEPENDENT_TYPE(_C_, _B_) TYPE(_C_, _B_)
#define NON_CANONICAL_UNLESS_DEPENDENT_TYPE(_C_, _B_) TYPE(_C_, _B_)
#include "clang/AST/TypeNodes.inc"
#undef ABSTRACT_TYPE
#undef NON_CANONICAL_TYPE
#undef DEPENDENT_TYPE
#undef NON_CANONICAL_UNLESS_DEPENDENT_TYPE
#undef TYPE
  default:
    break;
  };
  std::string buf;
  llvm::raw_string_ostream out(buf);
  clang::ASTDumper dumper(out, false);
  dumper.Visit(s);
  SIMPLE_ERROR("astExpose.cc>mostDerivedType. Could not cast clang::Type s->getTypeClass()-> {}. typename: {} ",
               (int)s->getTypeClass(), out.str());
}

}; // namespace asttooling

namespace asttooling {
core::T_sp af_getAsCXXRecordDecl(clang::Type* tp) {
  const clang::CXXRecordDecl* declp = tp->getAsCXXRecordDecl();
  if (declp) {
    return mostDerivedDecl(declp);
  }
  return nil<core::T_O>();
};

CL_LAMBDA(astnode &optional (stream *standard-output*));
DOCGROUP(clasp);
__attribute__((optnone)) CL_DEFUN void cast__dump(core::T_sp obj, core::T_sp stream) {
  //    .def("dump", (void (clang::Stmt::*)() const) & clang::Stmt::dump)
  //    .def("dump", (void(clang::Type::*)() const)&clang::Type::dump)
  //    .def("dump", (void (clang::Decl::*)() const) & clang::Decl::dump)
  llvm::SmallString<1024> stringOutput;
  llvm::raw_svector_ostream ostream(stringOutput);
  core::WrappedPointer_sp wp_node = gc::As<core::WrappedPointer_sp>(obj);
  //  printf("%s:%d:%s About to try and cast wp_node id: %lu to %lu \n", __FILE__, __LINE__, __FUNCTION__, wp_node->classId(),
  //  reg::registered_class<clang::Decl>::id);
  if (clang::Decl* decl = wp_node->castOrNull<clang::Decl>()) {
    decl->dump(ostream);
  } else if (clang::Stmt* stmt = wp_node->castOrNull<clang::Stmt>()) {
    clang::ASTDumper P(ostream, /*ShowColors=*/false);
    P.Visit(stmt);
  } else if (clang::Type* type = wp_node->castOrNull<clang::Type>()) {
    clang::ASTDumper P(ostream, /*ShowColors=*/false);
    P.Visit(type);
  } else if (clang::QualType* qtype = wp_node->castOrNull<clang::QualType>()) {
    clang::ASTDumper P(ostream, /*ShowColors=*/false);
    P.Visit(*qtype);
  } else {
    SIMPLE_ERROR("{}:{} Handle unboxing of other node types in cast__dump", __FILE__, __LINE__);
  }
  core::clasp_write_string(ostream.str().str(), stream);
}

DOCGROUP(clasp);
CL_DEFUN clang::QualType cast__makeQualTypeDefault() {
  clang::QualType qt;
  return qt;
};

DOCGROUP(clasp);
CL_DEFUN clang::QualType cast__makeQualType(clang::Type* ty) {
  clang::QualType qt(ty, 0);
  return qt;
};

DOCGROUP(clasp);
CL_DEFUN std::string cast__getAsString(clang::QualType qt) { return qt.getAsString(); }

DOCGROUP(clasp);
CL_DEFUN bool cast__isCanonical(clang::QualType qt) { return qt.isCanonical(); }

DOCGROUP(clasp);
CL_DEFUN bool cast__isStaticLocal(clang::VarDecl* vd) { return vd->isStaticLocal(); }

DOCGROUP(clasp);
CL_DEFUN clang::QualType cast__getCanonicalType(clang::QualType qt) { return qt.getCanonicalType(); }

void af_getNameForDiagnostic(clang::ClassTemplateSpecializationDecl* decl, core::T_sp stream, clang::LangOptions& langOps,
                             bool qualified) {
  string str;
  llvm::raw_string_ostream ostr(str);
  decl->getNameForDiagnostic(ostr, langOps, qualified);
  printf("getNameForDiagnostic<%s>\n", str.c_str());
  clasp_write_characters(str.c_str(), str.size(), stream);
};

core::T_sp af_constant_array_get_size(clang::ConstantArrayType* cat) {
  llvm::APInt size = cat->getSize();
  llvm::SmallString<40> svi;
  size.toString(svi, 10, true, false);
  return core::Integer_O::create(svi.str().str());
}

DOCGROUP(clasp);
CL_DEFUN core::T_sp cast__getTypePtrOrNull(clang::QualType qt) {
  const clang::Type* tp = qt.getTypePtrOrNull();
  if (tp) {
    return asttooling::mostDerivedType(tp);
  }
  return nil<core::T_O>();
};

}; // namespace asttooling

SYMBOL_EXPORT_SC_(AstToolingPkg, STARclangTemplateSpecializationKindSTAR);
SYMBOL_EXPORT_SC_(AstToolingPkg, STARclangTemplateArgumentArgKindSTAR);
SYMBOL_EXPORT_SC_(AstToolingPkg, STARclangAccessSpecifierSTAR);
CLBIND_TRANSLATE_SYMBOL_TO_ENUM(clang::TemplateArgument::ArgKind, asttooling::_sym_STARclangTemplateArgumentArgKindSTAR);
CLBIND_TRANSLATE_SYMBOL_TO_ENUM(clang::TemplateSpecializationKind, asttooling::_sym_STARclangTemplateSpecializationKindSTAR);
CLBIND_TRANSLATE_SYMBOL_TO_ENUM(clang::AccessSpecifier, asttooling::_sym_STARclangAccessSpecifierSTAR);

namespace asttooling {

llvm::APSInt getAsIntegral(clang::TemplateArgument* arg) { return arg->getAsIntegral(); }

bool isIntegerType(clang::Type* ty) { return ty->isIntegerType(); }

QualType getElementType(clang::ArrayType* array_type) { return array_type->getElementType(); }

QualType getInnerType(clang::ParenType* paren_type) { return paren_type->getInnerType(); }

clang::QualType getPointeeType(clang::Type* type) {
  if (auto* ptr = llvm::dyn_cast<clang::PointerType>(type)) {
    return ptr->getPointeeType();
  } else if (auto* mptr = llvm::dyn_cast<clang::MemberPointerType>(type)) {
    return mptr->getPointeeType();
  }
  SIMPLE_ERROR("getPointeeType only accepts PointerType and MemberPointerType");
}

void initialize_astExpose() {
  core::Package_sp pkg =
      gc::As<core::Package_sp>(_lisp->findPackage(ClangAstPkg)); //, {"CAST"}, {}); //{"CAST"},{"CL","CORE","AST_TOOLING"});
  pkg->shadow(core::SimpleBaseString_O::make("TYPE"));
  clbind::package_ pkgg(ClangAstPkg);
  clbind::scope_& m = pkgg.scope();
  clbind::class_<clang::Decl> cl(m, "Decl");
  cl.def("getGlobalID", &clang::Decl::getGlobalID)
      .def("isImplicit", &clang::Decl::isImplicit)
      .def("setImplicit", &clang::Decl::setImplicit)
      .def("getBeginLoc", &clang::Decl::getBeginLoc)
      .def("getEndLoc", &clang::Decl::getEndLoc)
      .def("getAccess", &clang::Decl::getAccess);
  clbind::enum_<clang::AccessSpecifier>(m, asttooling::_sym_STARclangAccessSpecifierSTAR)
      .value("AS_public", clang::AS_public)
      .value("AS_protected", clang::AS_protected)
      .value("AS_private", clang::AS_private)
      .value("AS_none", clang::AS_none);

#define DECL(_Class_, _Base_) auto scope_Decl_##_Class_ = clbind::class_<_Class_##Decl, _Base_>(m, #_Class_ "Decl");
#include <clang/AST/DeclNodes.inc>
#undef DECL
  scope_Decl_Named.def("getIdentifier", &NamedDecl::getIdentifier);
  scope_Decl_Named.def("getName", &NamedDecl::getName);
  scope_Decl_Named.def("getNameAsString", &NamedDecl::getNameAsString);
  scope_Decl_Named.def("getQualifiedNameAsString", (std::string(NamedDecl::*)() const) & NamedDecl::getQualifiedNameAsString);
  scope_Decl_CXXRecord.iterator("bases-iterator",
                                (CXXRecordDecl::base_class_iterator(CXXRecordDecl::*)()) & CXXRecordDecl::bases_begin,
                                (CXXRecordDecl::base_class_iterator(CXXRecordDecl::*)()) & CXXRecordDecl::bases_end);
  scope_Decl_CXXRecord.iterator("vbases-iterator",
                                (CXXRecordDecl::base_class_iterator(CXXRecordDecl::*)()) & CXXRecordDecl::vbases_begin,
                                (CXXRecordDecl::base_class_iterator(CXXRecordDecl::*)()) & CXXRecordDecl::vbases_end);
  scope_Decl_CXXRecord.def("hasDefinition", &clang::CXXRecordDecl::hasDefinition);
  scope_Decl_CXXRecord.def("isPolymorphic", &clang::CXXRecordDecl::isPolymorphic);
  scope_Decl_ClassTemplateSpecialization.def("getTemplateArgs", &clang::ClassTemplateSpecializationDecl::getTemplateArgs);
  #if LLVM_VERSION_MAJOR < 19
  scope_Decl_ClassTemplateSpecialization.def("getTypeAsWritten", &clang::ClassTemplateSpecializationDecl::getTypeAsWritten);
  #endif
  scope_Decl_ClassTemplateSpecialization.def("getPointOfInstantiation",
                                             &clang::ClassTemplateSpecializationDecl::getPointOfInstantiation);
  scope_Decl_ClassTemplateSpecialization.def("getSpecializedTemplate",
                                             &clang::ClassTemplateSpecializationDecl::getSpecializedTemplate);
  scope_Decl_ClassTemplateSpecialization.def("getSpecializationKind",
                                             &clang::ClassTemplateSpecializationDecl::getSpecializationKind);
  enum_<clang::TemplateSpecializationKind>(m, asttooling::_sym_STARclangTemplateSpecializationKindSTAR)
      .value("TSK_Undeclared", clang::TemplateSpecializationKind::TSK_Undeclared)
      .value("TSK_ImplicitInstantiation", clang::TemplateSpecializationKind::TSK_ImplicitInstantiation)
      .value("TSK_ExplicitSpecialization", clang::TemplateSpecializationKind::TSK_ExplicitSpecialization)
      .value("TSK_ExplicitInstantiationDeclaration", clang::TemplateSpecializationKind::TSK_ExplicitInstantiationDeclaration)
      .value("TSK_ExplicitInstantiationDefinition", clang::TemplateSpecializationKind::TSK_ExplicitInstantiationDefinition);
  m.def("getNameForDiagnostic", &af_getNameForDiagnostic, "(decl stream lang-opts qualified)"_ll);
  scope_Decl_TypedefName.def("getUnderlyingType", &clang::TypedefNameDecl::getUnderlyingType);
  scope_Decl_TypedefName.def("getCanonicalDecl",
                             (TypedefNameDecl * (clang::TypedefNameDecl::*)()) & clang::TypedefNameDecl::getCanonicalDecl);
  scope_Decl_Value.def("getType", &clang::ValueDecl::getType);
  scope_Decl_Field.def("getFieldIndex", &clang::FieldDecl::getFieldIndex);
  scope_Decl_Function.def("hasBody", (bool(clang::FunctionDecl::*)() const) & clang::FunctionDecl::hasBody);
  scope_Decl_Function.def("isThisDeclarationADefinition", &clang::FunctionDecl::isThisDeclarationADefinition);
  scope_Decl_Function.def("doesThisDeclarationHaveABody", &clang::FunctionDecl::doesThisDeclarationHaveABody);
  scope_Decl_Var.def("getSourceRange", &clang::VarDecl::getSourceRange);
  scope_Decl_Var.def("isLocalVarDecl", &clang::VarDecl::isLocalVarDecl);
  scope_Decl_Var.def("hasGlobalStorage", &clang::VarDecl::hasGlobalStorage);
  m.def("isStaticLocal", &cast__isStaticLocal);

  class_<Stmt>(m, "Stmt").def("getBeginLoc", &clang::Stmt::getBeginLoc).def("getEndLoc", &clang::Stmt::getEndLoc);
#define STMT(_Class_, _Base_) auto scope_Stmt_##_Class_ = class_<_Class_, _Base_>(m, #_Class_);
#include <clang/AST/StmtNodes.inc>
#undef STMT
  scope_Stmt_Expr.def("getType", &Expr::getType);
  scope_Stmt_Expr.def("getBestDynamicClassType", &clang::Expr::getBestDynamicClassType);
  scope_Stmt_CXXNewExpr.def("getNumPlacementArgs", &clang::CXXNewExpr::getNumPlacementArgs);
  scope_Stmt_CXXMemberCallExpr.def("getMethodDecl", &clang::CXXMemberCallExpr::getMethodDecl);
  scope_Stmt_CXXMemberCallExpr.def("getImplicitObjectArgument", &clang::CXXMemberCallExpr::getImplicitObjectArgument);
  scope_Stmt_StringLiteral.def("getString", &clang::StringLiteral::getString);

  class_<Type>(m, "Type")
      //            .  def("getAsCXXRecordDecl",&clang::Type::getAsCXXRecordDecl)
      //            .  def("getAsStructureType",&clang::Type::getAsStructureType)
      .def("getAsTemplateSpecializationType", &clang::Type::getAs<clang::TemplateSpecializationType>)
      //    .def("isIntegerType", &clang::Type::isIntegerType)
      .def("getCanonicalTypeInternal", &clang::Type::getCanonicalTypeInternal);
  m.def("isIntegerType", &isIntegerType);
  m.def("getAsCXXRecordDecl", &af_getAsCXXRecordDecl, "(arg)"_ll);

#define TYPE(_Class_, _Base_) auto scope_Type_##_Class_ = class_<_Class_##Type, _Base_>(m, #_Class_ "Type");
#define ABSTRACT_TYPE(_C_, _B_) TYPE(_C_, _B_)
#define NON_CANONICAL_TYPE(_C_, _B_) TYPE(_C_, _B_)
#define DEPENDENT_TYPE(_C_, _B_) TYPE(_C_, _B_)
#define NON_CANONICAL_UNLESS_DEPENDENT_TYPE(_C_, _B_) TYPE(_C_, _B_)
#include "clang/AST/TypeNodes.inc"
#undef ABSTRACT_TYPE
#undef NON_CANONICAL_TYPE
#undef DEPENDENT_TYPE
#undef NON_CANONICAL_UNLESS_DEPENDENT_TYPE
#undef TYPE
  scope_Type_Builtin.def("desugar", &clang::BuiltinType::desugar);

  scope_Type_Builtin.def("desugar", &clang::BuiltinType::desugar);
  scope_Type_Pointer.def("desugar", &clang::PointerType::desugar);
  //    .def("getPointeeType", &clang::PointerType::getPointeeType);
  m.def("getPointeeType", &getPointeeType);
  scope_Type_LValueReference.def("desugar", &clang::LValueReferenceType::desugar);
  scope_Type_RValueReference.def("desugar", &clang::RValueReferenceType::desugar);
  scope_Type_MemberPointer.def("desugar", &clang::MemberPointerType::desugar);
  //    .def("getPointeeType", &clang::MemberPointerType::getPointeeType);
  m.def("getElementType", &getElementType);
  scope_Type_ConstantArray.def("desugar", &clang::ConstantArrayType::desugar);
  m.def("constant-array-get-size", &af_constant_array_get_size, "(constant-array)"_ll);
  scope_Type_IncompleteArray.def("desugar", &clang::IncompleteArrayType::desugar);
  scope_Type_DependentSizedArray.def("desugar", &clang::DependentSizedArrayType::desugar);
  scope_Type_Vector.def("desugar", &clang::VectorType::desugar);
  scope_Type_FunctionProto.def("desugar", &clang::FunctionProtoType::desugar);
  //    .def("getInnerType", &clang::ParenType::getInnerType);
  m.def("getInnerType", &getInnerType);
  scope_Type_Typedef.def("getDecl", &clang::TypedefType::getDecl);
  scope_Type_Typedef.def("isSugared", &clang::TypedefType::isSugared);
  scope_Type_Typedef.def("desugar", &clang::TypedefType::desugar);
  scope_Type_Record.def("getDecl", &clang::RecordType::getDecl);
  scope_Type_Record.def("desugar", &clang::RecordType::desugar);
  scope_Type_Enum.def("desugar", &clang::EnumType::desugar);
  scope_Type_Elaborated.def("desugar", &clang::ElaboratedType::desugar);
  scope_Type_Elaborated.def("getNamedType", &clang::ElaboratedType::getNamedType);
  scope_Type_TemplateTypeParm.def("desugar", &clang::TemplateTypeParmType::desugar);
  scope_Type_SubstTemplateTypeParm.def("desugar", &clang::SubstTemplateTypeParmType::desugar);
  scope_Type_TemplateSpecialization.def("getTemplateName", &clang::TemplateSpecializationType::getTemplateName);
#if LLVM_VERSION_MAJOR < 16
  scope_Type_TemplateSpecialization.def("getNumArgs", &clang::TemplateSpecializationType::getNumArgs);
  scope_Type_TemplateSpecialization.def("getArg", &clang::TemplateSpecializationType::getArg);
#endif
  scope_Type_TemplateSpecialization.def("desugar", &clang::TemplateSpecializationType::desugar);
  scope_Type_TemplateSpecialization.def("getAliasedType", &clang::TemplateSpecializationType::getAliasedType);
  scope_Type_TemplateSpecialization.def("isTypeAlias", &clang::TemplateSpecializationType::isTypeAlias);
  scope_Type_TemplateSpecialization.def("isCurrentInstantiation", &clang::TemplateSpecializationType::isCurrentInstantiation);
  scope_Type_InjectedClassName.def("getDecl", &clang::InjectedClassNameType::getDecl);
  scope_Type_DependentName.def("desugar", &clang::DependentNameType::desugar);
  scope_Type_DependentName.def("isSugared", &clang::DependentNameType::isSugared);
  scope_Type_DependentTemplateSpecialization.def("desugar", &clang::DependentTemplateSpecializationType::desugar);
  class_<clang::TypeLoc>(m, "TypeLoc")
      .def("getSourceRange", &clang::TypeLoc::getSourceRange)
      .def("getLocalSourceRange", &clang::TypeLoc::getLocalSourceRange)
      .def("getBeginLoc", &clang::TypeLoc::getBeginLoc)
      .def("getEndLoc", &clang::TypeLoc::getEndLoc);
  class_<clang::CXXBaseSpecifier>(m, "CXXBaseSpecifier").def("getType", &clang::CXXBaseSpecifier::getType);
  class_<clang::TemplateArgument>(m, "TemplateArgument")
      .def("getKind", &clang::TemplateArgument::getKind)
      .def("pack_size", &clang::TemplateArgument::pack_size)
      .def("getPackAsArray", &clang::TemplateArgument::getPackAsArray)
      .def("getAsType", &clang::TemplateArgument::getAsType)
      .def("getAsTemplate", &clang::TemplateArgument::getAsTemplate)
      .def("getNullPtrType", &clang::TemplateArgument::getNullPtrType)
      .def("getAsDecl", &clang::TemplateArgument::getAsDecl)
      .def("getAsExpr", &clang::TemplateArgument::getAsExpr);
  //            .  iterator("pack",&clang::TemplateArgument::pack_begin, &clang::TemplateArgument::pack_end)
  m.def("getAsIntegral", &getAsIntegral);
  enum_<clang::TemplateArgument::ArgKind>(m, asttooling::_sym_STARclangTemplateArgumentArgKindSTAR)
      .value("argkind-Type", clang::TemplateArgument::ArgKind::Type)
      .value("argkind-Null", clang::TemplateArgument::ArgKind::Null)
      .value("argkind-Declaration", clang::TemplateArgument::ArgKind::Declaration)
      .value("argkind-NullPtr", clang::TemplateArgument::ArgKind::NullPtr)
      .value("argkind-Integral", clang::TemplateArgument::ArgKind::Integral)
      .value("argkind-Template", clang::TemplateArgument::ArgKind::Template)
      .value("argkind-TemplateExpansion", clang::TemplateArgument::ArgKind::TemplateExpansion)
      .value("argkind-Expression", clang::TemplateArgument::ArgKind::Expression)
      .value("argkind-Pack", clang::TemplateArgument::ArgKind::Pack);
  class_<clang::TemplateName>(m, "TemplateName").def("getAsTemplateDecl", &clang::TemplateName::getAsTemplateDecl);
  class_<clang::TypeSourceInfo>(m, "TypeSourceInfo")
      .def("getType", &clang::TypeSourceInfo::getType)
      .def("getTypeLoc", &clang::TypeSourceInfo::getTypeLoc);
  class_<clang::TemplateArgumentList>(m, "TemplateArgumentList")
      .def("size", &clang::TemplateArgumentList::size)
      .def("TemplateArgumentList-get", &clang::TemplateArgumentList::get);
  class_<clang::IdentifierInfo>(m, "IdentifierInfo");
}
}; // namespace asttooling
