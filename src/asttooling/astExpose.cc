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
#include <clasp/core/lispStream.h>
#include <clasp/core/package.h>

#include <clasp/asttooling/astExpose.h>
#include <clasp/llvmo/translators.h>
#include <clasp/asttooling/translators.h>
#include <clasp/core/symbolTable.h>
#include <clasp/asttooling/asttoolingPackage.h>

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
core::T_sp mostDerivedDecl(const clang::Decl *cd);
core::T_sp mostDerivedStmt(const clang::Stmt *x);
core::T_sp mostDerivedType(const clang::Type *x);
};

namespace translate {

#define DECL(_T_, _ignore_)                          \
  template <> struct to_object<clang::_T_##Decl *> { \
    static core::T_sp convert(clang::_T_##Decl *p) { \
      if (!p)                                        \
        return _Nil<core::T_O>();                    \
      return asttooling::mostDerivedDecl(p);         \
    }                                                \
  };
DECL(AccessSpec, Decl);
DECL(Block, Decl);
DECL(Captured, Decl);
DECL(ClassScopeFunctionSpecialization, Decl);
DECL(Empty, Decl);
DECL(FileScopeAsm, Decl);
DECL(Friend, Decl);
DECL(FriendTemplate, Decl);
DECL(Import, Decl);
DECL(LinkageSpec, Decl);
DECL(Named, Decl);
DECL(Label, NamedDecl);
DECL(Namespace, NamedDecl);
DECL(NamespaceAlias, NamedDecl);
DECL(ObjCCompatibleAlias, NamedDecl);
DECL(ObjCContainer, NamedDecl);
DECL(ObjCCategory, ObjCContainerDecl);
DECL(ObjCImpl, ObjCContainerDecl);
DECL(ObjCCategoryImpl, ObjCImplDecl);
DECL(ObjCImplementation, ObjCImplDecl);
DECL(ObjCInterface, ObjCContainerDecl);
DECL(ObjCProtocol, ObjCContainerDecl);
DECL(ObjCMethod, NamedDecl);
DECL(ObjCProperty, NamedDecl);
DECL(Template, NamedDecl);
DECL(RedeclarableTemplate, TemplateDecl);
DECL(ClassTemplate, RedeclarableTemplateDecl);
DECL(FunctionTemplate, RedeclarableTemplateDecl);
DECL(TypeAliasTemplate, RedeclarableTemplateDecl);
DECL(VarTemplate, RedeclarableTemplateDecl);
DECL(TemplateTemplateParm, TemplateDecl);
DECL(Type, NamedDecl);
DECL(Tag, TypeDecl);
DECL(Enum, TagDecl);
DECL(Record, TagDecl);
DECL(CXXRecord, RecordDecl);
DECL(ClassTemplateSpecialization, CXXRecordDecl);
DECL(ClassTemplatePartialSpecialization, ClassTemplateSpecializationDecl);
DECL(TemplateTypeParm, TypeDecl);
DECL(TypedefName, TypeDecl);
DECL(TypeAlias, TypedefNameDecl);
DECL(Typedef, TypedefNameDecl);
DECL(UnresolvedUsingTypename, TypeDecl);
DECL(Using, NamedDecl);
DECL(UsingDirective, NamedDecl);
DECL(UsingShadow, NamedDecl);
DECL(Value, NamedDecl);
DECL(Declarator, ValueDecl);
DECL(Field, DeclaratorDecl);
DECL(ObjCAtDefsField, FieldDecl);
DECL(ObjCIvar, FieldDecl);
DECL(Function, DeclaratorDecl);
DECL(CXXMethod, FunctionDecl);
DECL(CXXConstructor, CXXMethodDecl);
DECL(CXXConversion, CXXMethodDecl);
DECL(CXXDestructor, CXXMethodDecl);
DECL(MSProperty, DeclaratorDecl);
DECL(NonTypeTemplateParm, DeclaratorDecl);
DECL(Var, DeclaratorDecl);
DECL(ImplicitParam, VarDecl);
DECL(ParmVar, VarDecl);
DECL(VarTemplateSpecialization, VarDecl);
DECL(VarTemplatePartialSpecialization, VarTemplateSpecializationDecl);
DECL(EnumConstant, ValueDecl);
DECL(IndirectField, ValueDecl);
DECL(UnresolvedUsingValue, ValueDecl);
DECL(OMPThreadPrivate, Decl);
DECL(ObjCPropertyImpl, Decl);
DECL(StaticAssert, Decl);
DECL(TranslationUnit, Decl);
#undef DECL

#define STMT(_T_)                              \
  template <> struct to_object<clang::_T_ *> { \
    static core::T_sp convert(clang::_T_ *p) { \
      if (!p)                                  \
        return _Nil<core::T_O>();              \
      return asttooling::mostDerivedStmt(p);   \
    }                                          \
  };
STMT(Expr);
STMT(GCCAsmStmt);
STMT(MSAsmStmt);
// firstAsmStmtConstant= GCCAsmStmtClass, lastAsmStmtConstant= MSAsmStmtClass,
STMT(AttributedStmt);
STMT(BreakStmt);
STMT(CXXCatchStmt);
STMT(CXXForRangeStmt);
STMT(CXXTryStmt);
STMT(CapturedStmt);
STMT(CompoundStmt);
STMT(ContinueStmt);
STMT(DeclStmt);
STMT(DoStmt);
STMT(BinaryConditionalOperator);
STMT(ConditionalOperator);
// firstAbstractConditionalOperatorConstant= BinaryConditionalOperatorClass, lastAbstractConditionalOperatorConstant= ConditionalOperatorClass,
STMT(AddrLabelExpr);
STMT(ArraySubscriptExpr);
STMT(ArrayTypeTraitExpr);
STMT(AsTypeExpr);
STMT(AtomicExpr);
STMT(BinaryOperator);
STMT(CompoundAssignOperator);
// firstBinaryOperatorConstant= BinaryOperatorClass, lastBinaryOperatorConstant= CompoundAssignOperatorClass,
STMT(BlockExpr);
STMT(CXXBindTemporaryExpr);
STMT(CXXBoolLiteralExpr);
STMT(CXXConstructExpr);
STMT(CXXTemporaryObjectExpr);
// firstCXXConstructExprConstant= CXXConstructExprClass, lastCXXConstructExprConstant= CXXTemporaryObjectExprClass,
STMT(CXXDefaultArgExpr);
STMT(CXXDefaultInitExpr);
STMT(CXXDeleteExpr);
STMT(CXXDependentScopeMemberExpr);
STMT(CXXNewExpr);
STMT(CXXNoexceptExpr);
STMT(CXXNullPtrLiteralExpr);
STMT(CXXPseudoDestructorExpr);
STMT(CXXScalarValueInitExpr);
STMT(CXXStdInitializerListExpr);
STMT(CXXThisExpr);
STMT(CXXThrowExpr);
STMT(CXXTypeidExpr);
STMT(CXXUnresolvedConstructExpr);
STMT(CXXUuidofExpr);
STMT(CallExpr);
STMT(CUDAKernelCallExpr);
STMT(CXXMemberCallExpr);
STMT(CXXOperatorCallExpr);
STMT(UserDefinedLiteral);
// firstCallExprConstant= CallExprClass, lastCallExprConstant= UserDefinedLiteralClass,
STMT(CStyleCastExpr);
STMT(CXXFunctionalCastExpr);
STMT(CXXConstCastExpr);
STMT(CXXDynamicCastExpr);
STMT(CXXReinterpretCastExpr);
STMT(CXXStaticCastExpr);
// firstCXXNamedCastExprConstant= CXXConstCastExprClass, lastCXXNamedCastExprConstant= CXXStaticCastExprClass,
STMT(ObjCBridgedCastExpr);
// firstExplicitCastExprConstant= CStyleCastExprClass, lastExplicitCastExprConstant= ObjCBridgedCastExprClass,
STMT(ImplicitCastExpr);
// firstCastExprConstant= CStyleCastExprClass, lastCastExprConstant= ImplicitCastExprClass,
STMT(CharacterLiteral);
STMT(ChooseExpr);
STMT(CompoundLiteralExpr);
STMT(ConvertVectorExpr);
STMT(DeclRefExpr);
STMT(DependentScopeDeclRefExpr);
STMT(DesignatedInitExpr);
STMT(ExprWithCleanups);
STMT(ExpressionTraitExpr);
STMT(ExtVectorElementExpr);
STMT(FloatingLiteral);
STMT(FunctionParmPackExpr);
STMT(GNUNullExpr);
STMT(GenericSelectionExpr);
STMT(ImaginaryLiteral);
STMT(ImplicitValueInitExpr);
STMT(InitListExpr);
STMT(IntegerLiteral);
STMT(LambdaExpr);
STMT(MSPropertyRefExpr);
STMT(MaterializeTemporaryExpr);
STMT(MemberExpr);
STMT(ObjCArrayLiteral);
STMT(ObjCBoolLiteralExpr);
STMT(ObjCBoxedExpr);
STMT(ObjCDictionaryLiteral);
STMT(ObjCEncodeExpr);
STMT(ObjCIndirectCopyRestoreExpr);
STMT(ObjCIsaExpr);
STMT(ObjCIvarRefExpr);
STMT(ObjCMessageExpr);
STMT(ObjCPropertyRefExpr);
STMT(ObjCProtocolExpr);
STMT(ObjCSelectorExpr);
STMT(ObjCStringLiteral);
STMT(ObjCSubscriptRefExpr);
STMT(OffsetOfExpr);
STMT(OpaqueValueExpr);
STMT(UnresolvedLookupExpr);
STMT(UnresolvedMemberExpr);
// firstOverloadExprConstant= UnresolvedLookupExprClass, lastOverloadExprConstant= UnresolvedMemberExprClass,
STMT(PackExpansionExpr);
STMT(ParenExpr);
STMT(ParenListExpr);
STMT(PredefinedExpr);
STMT(PseudoObjectExpr);
STMT(ShuffleVectorExpr);
STMT(SizeOfPackExpr);
STMT(StmtExpr);
STMT(StringLiteral);
STMT(SubstNonTypeTemplateParmExpr);
STMT(SubstNonTypeTemplateParmPackExpr);
STMT(TypeTraitExpr);
STMT(UnaryExprOrTypeTraitExpr);
STMT(UnaryOperator);
STMT(VAArgExpr);
// firstExprConstant= BinaryConditionalOperatorClass, lastExprConstant= VAArgExprClass,
STMT(ForStmt);
STMT(GotoStmt);
STMT(IfStmt);
STMT(IndirectGotoStmt);
STMT(LabelStmt);
STMT(MSDependentExistsStmt);
STMT(NullStmt);
STMT(OMPSimdDirective);
STMT(OMPForDirective);
STMT(OMPParallelDirective);
STMT(OMPSectionDirective);
STMT(OMPSectionsDirective);
STMT(OMPSingleDirective);
// firstOMPExecutableDirectiveConstant= OMPParallelDirectiveClass, lastOMPExecutableDirectiveConstant= OMPParallelDirectiveClass,
STMT(ObjCAtCatchStmt);
STMT(ObjCAtFinallyStmt);
STMT(ObjCAtSynchronizedStmt);
STMT(ObjCAtThrowStmt);
STMT(ObjCAtTryStmt);
STMT(ObjCAutoreleasePoolStmt);
STMT(ObjCForCollectionStmt);
STMT(ReturnStmt);
STMT(SEHExceptStmt);
STMT(SEHFinallyStmt);
STMT(SEHTryStmt);
STMT(CaseStmt);
STMT(DefaultStmt);
// firstSwitchCaseConstant= CaseStmtClass, lastSwitchCaseConstant= DefaultStmtClass,
STMT(SwitchStmt);
STMT(WhileStmt);
#undef STMT

#define TYPE(_T_, _ignore_)                          \
  template <> struct to_object<clang::_T_##Type *> { \
    static core::T_sp convert(clang::_T_##Type *p) { \
      if (!p)                                        \
        return _Nil<core::T_O>();                    \
      return asttooling::mostDerivedType(p);         \
    }                                                \
  };

TYPE(Builtin, Type);
TYPE(Complex, Type);
TYPE(Pointer, Type);
TYPE(BlockPointer, Type);
TYPE(Reference, Type);
TYPE(LValueReference, ReferenceType);
TYPE(RValueReference, ReferenceType);
TYPE(MemberPointer, Type);
TYPE(Array, Type);
TYPE(ConstantArray, ArrayType);
TYPE(IncompleteArray, ArrayType);
TYPE(VariableArray, ArrayType);
TYPE(DependentSizedArray, ArrayType);
TYPE(DependentSizedExtVector, Type);
TYPE(Vector, Type);
TYPE(ExtVector, VectorType);
TYPE(Function, Type);
TYPE(FunctionProto, FunctionType);
TYPE(FunctionNoProto, FunctionType);
TYPE(UnresolvedUsing, Type);
TYPE(Paren, Type);
TYPE(Typedef, Type);
TYPE(Adjusted, Type);
TYPE(Decayed, AdjustedType);
TYPE(TypeOfExpr, Type);
TYPE(TypeOf, Type);
TYPE(Decltype, Type);
TYPE(UnaryTransform, Type);
TYPE(Tag, Type);
TYPE(Record, TagType);
TYPE(Enum, TagType);
TYPE(Elaborated, Type);
TYPE(Attributed, Type);
TYPE(TemplateTypeParm, Type);
TYPE(SubstTemplateTypeParm, Type);
TYPE(SubstTemplateTypeParmPack, Type);
TYPE(TemplateSpecialization, Type);
TYPE(Auto, Type);
TYPE(InjectedClassName, Type);
TYPE(DependentName, Type);
TYPE(DependentTemplateSpecialization, Type);
TYPE(PackExpansion, Type);
TYPE(ObjCObject, Type);
TYPE(ObjCInterface, ObjCObjectType);
TYPE(ObjCObjectPointer, Type);
TYPE(Atomic, Type);
#undef TYPE
};

using namespace clbind;
using namespace clang;




typedef clbind::Wrapper<clang::QualType, std::unique_ptr<clang::QualType>> QualType_wrapper;

typedef clbind::Wrapper<clang::TypeLoc, std::unique_ptr<clang::TypeLoc>> TypeLoc_wrapper;

typedef clbind::Wrapper<clang::TemplateName, std::unique_ptr<clang::TemplateName>> TemplateName_wrapper;

namespace asttooling {

template <typename T>
core::T_sp cast_decl(clang::Decl *d) {
  if (T *x = dyn_cast<T>(d)) {
    return clbind::Wrapper<T, T *>::make_wrapper(x, reg::registered_class<T>::id);
  }
  SIMPLE_ERROR(BF("Could not cast Decl to known Decl"));
}

template <typename T>
core::T_sp cast_stmt(clang::Stmt *d) {
  T *x = llvm::cast<T>(d);
  return clbind::Wrapper<T, T *>::make_wrapper(x, reg::registered_class<T>::id);
}

core::T_sp mostDerivedDecl(const clang::Decl *cd) {
  clang::Decl *d = const_cast<clang::Decl *>(cd);
  if (!d) {
    SIMPLE_ERROR(BF("Could not downcast clang::Decl @%p to most derived object") % (void *)(cd));
  }
#define DECL(_C_, _B_)                               \
  case clang::Decl::_C_: {                           \
    core::T_sp res = cast_decl<clang::_C_##Decl>(d); \
    return res;                                      \
  }
  switch (d->getKind()) {
    DECL(AccessSpec, Decl);
    DECL(Block, Decl);
    DECL(Captured, Decl);
    DECL(ClassScopeFunctionSpecialization, Decl);
    DECL(Empty, Decl);
    DECL(FileScopeAsm, Decl);
    DECL(Friend, Decl);
    DECL(FriendTemplate, Decl);
    DECL(Import, Decl);
    DECL(LinkageSpec, Decl);
    //DECL(Named, Decl);
    DECL(Label, NamedDecl);
    DECL(Namespace, NamedDecl);
    DECL(NamespaceAlias, NamedDecl);
    DECL(ObjCCompatibleAlias, NamedDecl);
    //DECL(ObjCContainer, NamedDecl);
    DECL(ObjCCategory, ObjCContainerDecl);
    //DECL(ObjCImpl, ObjCContainerDecl);
    DECL(ObjCCategoryImpl, ObjCImplDecl);
    DECL(ObjCImplementation, ObjCImplDecl);
    DECL(ObjCInterface, ObjCContainerDecl);
    DECL(ObjCProtocol, ObjCContainerDecl);
    DECL(ObjCMethod, NamedDecl);
    DECL(ObjCProperty, NamedDecl);
    //DECL(Template, NamedDecl);
    //DECL(RedeclarableTemplate, TemplateDecl);
    DECL(ClassTemplate, RedeclarableTemplateDecl);
    DECL(FunctionTemplate, RedeclarableTemplateDecl);
    DECL(TypeAliasTemplate, RedeclarableTemplateDecl);
    DECL(VarTemplate, RedeclarableTemplateDecl);
    DECL(TemplateTemplateParm, TemplateDecl);
    //DECL(Type, NamedDecl);
    //DECL(Tag, TypeDecl);
    DECL(Enum, TagDecl);
    DECL(Record, TagDecl);
    DECL(CXXRecord, RecordDecl);
    DECL(ClassTemplateSpecialization, CXXRecordDecl);
    DECL(ClassTemplatePartialSpecialization, ClassTemplateSpecializationDecl);
    DECL(TemplateTypeParm, TypeDecl);
    //DECL(TypedefName, TypeDecl);
    DECL(TypeAlias, TypedefNameDecl);
    DECL(Typedef, TypedefNameDecl);
    DECL(UnresolvedUsingTypename, TypeDecl);
    DECL(Using, NamedDecl);
    DECL(UsingDirective, NamedDecl);
    DECL(UsingShadow, NamedDecl);
    //DECL(Value, NamedDecl);
    //DECL(Declarator, ValueDecl);
    DECL(Field, DeclaratorDecl);
    DECL(ObjCAtDefsField, FieldDecl);
    DECL(ObjCIvar, FieldDecl);
    DECL(Function, DeclaratorDecl);
    DECL(CXXMethod, FunctionDecl);
    DECL(CXXConstructor, CXXMethodDecl);
    DECL(CXXConversion, CXXMethodDecl);
    DECL(CXXDestructor, CXXMethodDecl);
    DECL(MSProperty, DeclaratorDecl);
    DECL(NonTypeTemplateParm, DeclaratorDecl);
    DECL(Var, DeclaratorDecl);
    DECL(ImplicitParam, VarDecl);
    DECL(ParmVar, VarDecl);
    DECL(VarTemplateSpecialization, VarDecl);
    DECL(VarTemplatePartialSpecialization, VarTemplateSpecializationDecl);
    DECL(EnumConstant, ValueDecl);
    DECL(IndirectField, ValueDecl);
    DECL(UnresolvedUsingValue, ValueDecl);
    DECL(OMPThreadPrivate, Decl);
    DECL(ObjCPropertyImpl, Decl);
    DECL(StaticAssert, Decl);
    DECL(TranslationUnit, Decl);
#undef DECL

  default:
    break;
  };
  SIMPLE_ERROR(BF("Could not cast Decl"));
};

core::T_sp mostDerivedStmt(const clang::Stmt *x) {
  clang::Stmt *s = const_cast<clang::Stmt *>(x);
  if (!s) {
    SIMPLE_ERROR(BF("Could not downcast clang::Stmt @%p to most derived object") % (void *)(x));
  }

#define CASE_STMT(_C_)                         \
  case clang::Stmt::_C_##Class: {              \
    core::T_sp res = cast_stmt<clang::_C_>(s); \
    return res;                                \
  }
  switch (s->getStmtClass()) {
  case clang::Stmt::NoStmtClass:
    break;
    CASE_STMT(GCCAsmStmt);
    CASE_STMT(MSAsmStmt);
    // firstAsmStmtConstant= GCCAsmStmtClass, lastAsmStmtConstant= MSAsmStmtClass,
    CASE_STMT(AttributedStmt);
    CASE_STMT(BreakStmt);
    CASE_STMT(CXXCatchStmt);
    CASE_STMT(CXXForRangeStmt);
    CASE_STMT(CXXTryStmt);
    CASE_STMT(CapturedStmt);
    CASE_STMT(CompoundStmt);
    CASE_STMT(ContinueStmt);
    CASE_STMT(DeclStmt);
    CASE_STMT(DoStmt);
    CASE_STMT(BinaryConditionalOperator);
    CASE_STMT(ConditionalOperator);
    // firstAbstractConditionalOperatorConstant= BinaryConditionalOperatorClass, lastAbstractConditionalOperatorConstant= ConditionalOperatorClass,
    CASE_STMT(AddrLabelExpr);
    CASE_STMT(ArraySubscriptExpr);
    CASE_STMT(ArrayTypeTraitExpr);
    CASE_STMT(AsTypeExpr);
    CASE_STMT(AtomicExpr);
    CASE_STMT(BinaryOperator);
    CASE_STMT(CompoundAssignOperator);
    // firstBinaryOperatorConstant= BinaryOperatorClass, lastBinaryOperatorConstant= CompoundAssignOperatorClass,
    CASE_STMT(BlockExpr);
    CASE_STMT(CXXBindTemporaryExpr);
    CASE_STMT(CXXBoolLiteralExpr);
    CASE_STMT(CXXConstructExpr);
    CASE_STMT(CXXTemporaryObjectExpr);
    // firstCXXConstructExprConstant= CXXConstructExprClass, lastCXXConstructExprConstant= CXXTemporaryObjectExprClass,
    CASE_STMT(CXXDefaultArgExpr);
    CASE_STMT(CXXDefaultInitExpr);
    CASE_STMT(CXXDeleteExpr);
    CASE_STMT(CXXDependentScopeMemberExpr);
    CASE_STMT(CXXNewExpr);
    CASE_STMT(CXXNoexceptExpr);
    CASE_STMT(CXXNullPtrLiteralExpr);
    CASE_STMT(CXXPseudoDestructorExpr);
    CASE_STMT(CXXScalarValueInitExpr);
    CASE_STMT(CXXStdInitializerListExpr);
    CASE_STMT(CXXThisExpr);
    CASE_STMT(CXXThrowExpr);
    CASE_STMT(CXXTypeidExpr);
    CASE_STMT(CXXUnresolvedConstructExpr);
    CASE_STMT(CXXUuidofExpr);
    CASE_STMT(CallExpr);
    CASE_STMT(CUDAKernelCallExpr);
    CASE_STMT(CXXMemberCallExpr);
    CASE_STMT(CXXOperatorCallExpr);
    CASE_STMT(UserDefinedLiteral);
    // firstCallExprConstant= CallExprClass, lastCallExprConstant= UserDefinedLiteralClass,
    CASE_STMT(CStyleCastExpr);
    CASE_STMT(CXXFunctionalCastExpr);
    CASE_STMT(CXXConstCastExpr);
    CASE_STMT(CXXDynamicCastExpr);
    CASE_STMT(CXXReinterpretCastExpr);
    CASE_STMT(CXXStaticCastExpr);
    // firstCXXNamedCastExprConstant= CXXConstCastExprClass, lastCXXNamedCastExprConstant= CXXStaticCastExprClass,
    CASE_STMT(ObjCBridgedCastExpr);
    // firstExplicitCastExprConstant= CStyleCastExprClass, lastExplicitCastExprConstant= ObjCBridgedCastExprClass,
    CASE_STMT(ImplicitCastExpr);
    // firstCastExprConstant= CStyleCastExprClass, lastCastExprConstant= ImplicitCastExprClass,
    CASE_STMT(CharacterLiteral);
    CASE_STMT(ChooseExpr);
    CASE_STMT(CompoundLiteralExpr);
    CASE_STMT(ConvertVectorExpr);
    CASE_STMT(DeclRefExpr);
    CASE_STMT(DependentScopeDeclRefExpr);
    CASE_STMT(DesignatedInitExpr);
    CASE_STMT(ExprWithCleanups);
    CASE_STMT(ExpressionTraitExpr);
    CASE_STMT(ExtVectorElementExpr);
    CASE_STMT(FloatingLiteral);
    CASE_STMT(FunctionParmPackExpr);
    CASE_STMT(GNUNullExpr);
    CASE_STMT(GenericSelectionExpr);
    CASE_STMT(ImaginaryLiteral);
    CASE_STMT(ImplicitValueInitExpr);
    CASE_STMT(InitListExpr);
    CASE_STMT(IntegerLiteral);
    CASE_STMT(LambdaExpr);
    CASE_STMT(MSPropertyRefExpr);
    CASE_STMT(MaterializeTemporaryExpr);
    CASE_STMT(MemberExpr);
    CASE_STMT(ObjCArrayLiteral);
    CASE_STMT(ObjCBoolLiteralExpr);
    CASE_STMT(ObjCBoxedExpr);
    CASE_STMT(ObjCDictionaryLiteral);
    CASE_STMT(ObjCEncodeExpr);
    CASE_STMT(ObjCIndirectCopyRestoreExpr);
    CASE_STMT(ObjCIsaExpr);
    CASE_STMT(ObjCIvarRefExpr);
    CASE_STMT(ObjCMessageExpr);
    CASE_STMT(ObjCPropertyRefExpr);
    CASE_STMT(ObjCProtocolExpr);
    CASE_STMT(ObjCSelectorExpr);
    CASE_STMT(ObjCStringLiteral);
    CASE_STMT(ObjCSubscriptRefExpr);
    CASE_STMT(OffsetOfExpr);
    CASE_STMT(OpaqueValueExpr);
    CASE_STMT(UnresolvedLookupExpr);
    CASE_STMT(UnresolvedMemberExpr);
    // firstOverloadExprConstant= UnresolvedLookupExprClass, lastOverloadExprConstant= UnresolvedMemberExprClass,
    CASE_STMT(PackExpansionExpr);
    CASE_STMT(ParenExpr);
    CASE_STMT(ParenListExpr);
    CASE_STMT(PredefinedExpr);
    CASE_STMT(PseudoObjectExpr);
    CASE_STMT(ShuffleVectorExpr);
    CASE_STMT(SizeOfPackExpr);
    CASE_STMT(StmtExpr);
    CASE_STMT(StringLiteral);
    CASE_STMT(SubstNonTypeTemplateParmExpr);
    CASE_STMT(SubstNonTypeTemplateParmPackExpr);
    CASE_STMT(TypeTraitExpr);
    CASE_STMT(UnaryExprOrTypeTraitExpr);
    CASE_STMT(UnaryOperator);
    CASE_STMT(VAArgExpr);
    // firstExprConstant= BinaryConditionalOperatorClass, lastExprConstant= VAArgExprClass,
    CASE_STMT(ForStmt);
    CASE_STMT(GotoStmt);
    CASE_STMT(IfStmt);
    CASE_STMT(IndirectGotoStmt);
    CASE_STMT(LabelStmt);
    CASE_STMT(MSDependentExistsStmt);
    CASE_STMT(NullStmt);
    CASE_STMT(OMPSimdDirective);
    CASE_STMT(OMPForDirective);
    CASE_STMT(OMPParallelDirective);
    CASE_STMT(OMPSectionDirective);
    CASE_STMT(OMPSectionsDirective);
    CASE_STMT(OMPSingleDirective);
    // firstOMPExecutableDirectiveConstant= OMPParallelDirectiveClass, lastOMPExecutableDirectiveConstant= OMPParallelDirectiveClass,
    CASE_STMT(ObjCAtCatchStmt);
    CASE_STMT(ObjCAtFinallyStmt);
    CASE_STMT(ObjCAtSynchronizedStmt);
    CASE_STMT(ObjCAtThrowStmt);
    CASE_STMT(ObjCAtTryStmt);
    CASE_STMT(ObjCAutoreleasePoolStmt);
    CASE_STMT(ObjCForCollectionStmt);
    CASE_STMT(ReturnStmt);
    CASE_STMT(SEHExceptStmt);
    CASE_STMT(SEHFinallyStmt);
    CASE_STMT(SEHTryStmt);
    CASE_STMT(CaseStmt);
    CASE_STMT(DefaultStmt);
    // firstSwitchCaseConstant= CaseStmtClass, lastSwitchCaseConstant= DefaultStmtClass,
    CASE_STMT(SwitchStmt);
    CASE_STMT(WhileStmt);
  // firstStmtConstant= GCCAsmStmtClass, lastStmtConstant= WhileStmtClass
  default:
    SIMPLE_ERROR(BF("Add a case for missing LLVM classes"));
  }
  SIMPLE_ERROR(BF("Could not cast Stmt"));
};

template <typename T>
core::T_sp cast_type(clang::Type *d) {
  T *x = llvm::cast<T>(d);
  return clbind::Wrapper<T, T *>::make_wrapper(x, reg::registered_class<T>::id);
}

core::T_sp mostDerivedType(const clang::Type *x) {
  clang::Type *s = const_cast<clang::Type *>(x);
  if (!s) {
    SIMPLE_ERROR(BF("Could not downcast clang::Type @%p to most derived object") % (void *)(x));
  }

#define TYPE(_C_, _B_)                               \
  case clang::Type::_C_: {                           \
    core::T_sp res = cast_type<clang::_C_##Type>(s); \
    return res;                                      \
  }
#define ABSTRACT_TYPE(_C_, _B_)
  switch (s->getTypeClass()) {
    TYPE(Builtin, Type);
    TYPE(Complex, Type);
    TYPE(Pointer, Type);
    TYPE(BlockPointer, Type);
    ABSTRACT_TYPE(Reference, Type);
    TYPE(LValueReference, ReferenceType);
    TYPE(RValueReference, ReferenceType);
    TYPE(MemberPointer, Type);
    ABSTRACT_TYPE(Array, Type);
    TYPE(ConstantArray, ArrayType);
    TYPE(IncompleteArray, ArrayType);
    TYPE(VariableArray, ArrayType);
    TYPE(DependentSizedArray, ArrayType);
    TYPE(DependentSizedExtVector, Type);
    TYPE(Vector, Type);
    TYPE(ExtVector, VectorType);
    ABSTRACT_TYPE(Function, Type);
    TYPE(FunctionProto, FunctionType);
    TYPE(FunctionNoProto, FunctionType);
    TYPE(UnresolvedUsing, Type);
    TYPE(Paren, Type);
    TYPE(Typedef, Type);
    TYPE(Adjusted, Type); // What is this?
    TYPE(Decayed, AdjustedType);
    TYPE(TypeOfExpr, Type);
    TYPE(TypeOf, Type);
    TYPE(Decltype, Type);
    TYPE(UnaryTransform, Type);
    ABSTRACT_TYPE(Tag, Type);
    TYPE(Record, TagType);
    TYPE(Enum, TagType);
    TYPE(Elaborated, Type);
    TYPE(Attributed, Type);
    TYPE(TemplateTypeParm, Type);
    TYPE(SubstTemplateTypeParm, Type);
    TYPE(SubstTemplateTypeParmPack, Type);
    TYPE(TemplateSpecialization, Type);
    TYPE(Auto, Type);
    TYPE(InjectedClassName, Type);
    TYPE(DependentName, Type);
    TYPE(DependentTemplateSpecialization, Type);
    TYPE(PackExpansion, Type);
    TYPE(ObjCObject, Type); // What is this
    TYPE(ObjCInterface, ObjCObjectType);
    TYPE(ObjCObjectPointer, Type);
    TYPE(Atomic, Type);
#undef TYPE
  default:
    break;
  };
  SIMPLE_ERROR(BF("astExpose.cc>mostDerivedType | Could not cast clang::Type s->getTypeClass()-> %d") % s->getTypeClass());
}


core::T_sp af_constant_array_get_size(clang::ConstantArrayType *cat) {
    llvm::APInt size = cat->getSize();
  string s = size.toString(10,true);
  return core::Integer_O::create(s);
}


#define ARGS_af_getTypePtrOrNull "(arg)"
#define DECL_af_getTypePtrOrNull ""
#define DOCS_af_getTypePtrOrNull "getTypePtrOrNull - returns the most derived Type* ptr or NIL"
core::T_sp af_getTypePtrOrNull(clang::QualType qt) {
  const clang::Type *tp = qt.getTypePtrOrNull();
  if (tp) {
    return mostDerivedType(tp);
  }
  return _Nil<core::T_O>();
};

core::T_sp af_getAsCXXRecordDecl(clang::Type *tp) {
  const clang::CXXRecordDecl *declp = tp->getAsCXXRecordDecl();
  if (declp) {
    return mostDerivedDecl(declp);
  }
  return _Nil<core::T_O>();
};

void af_getNameForDiagnostic(clang::ClassTemplateSpecializationDecl *decl, core::T_sp stream, LangOptions &langOps, bool qualified) {
  string str;
  llvm::raw_string_ostream ostr(str);
  decl->getNameForDiagnostic(ostr, langOps, qualified);
  printf("getNameForDiagnostic<%s>\n", str.c_str());
  clasp_write_characters(str.c_str(), str.size(), stream);
};

#define ARGS_af_makeQualType "(type)"
#define DECL_af_makeQualType ""
#define DOCS_af_makeQualType "makeQualType"
clang::QualType af_makeQualType(clang::Type *ty) {
  clang::QualType qt(ty, 0);
  return qt;
};
};

SYMBOL_EXPORT_SC_(AstToolingPkg, STARclangTemplateSpecializationKindSTAR);
SYMBOL_EXPORT_SC_(AstToolingPkg, STARclangTemplateArgumentArgKindSTAR);
SYMBOL_EXPORT_SC_(AstToolingPkg, STARclangAccessSpecifierSTAR);
CLBIND_TRANSLATE_SYMBOL_TO_ENUM(clang::TemplateArgument::ArgKind, asttooling::_sym_STARclangTemplateArgumentArgKindSTAR);
CLBIND_TRANSLATE_SYMBOL_TO_ENUM(clang::TemplateSpecializationKind, asttooling::_sym_STARclangTemplateSpecializationKindSTAR);
CLBIND_TRANSLATE_SYMBOL_TO_ENUM(clang::AccessSpecifier, asttooling::_sym_STARclangAccessSpecifierSTAR);

namespace asttooling {
void initialize_astExpose() {
  core::Package_sp pkg = gc::As<core::Package_sp>(_lisp->findPackage(ClangAstPkg)); //, {"CAST"}, {}); //{"CAST"},{"CL","CORE","AST_TOOLING"});
  pkg->shadow(core::SimpleBaseString_O::make("TYPE"));
  package_ pkgg(ClangAstPkg);
  scope_& m = pkgg.scope();
  class_<clang::Decl> cl(m,"Decl");
  cl.def("getGlobalID", &clang::Decl::getGlobalID)
    .def("isImplicit", &clang::Decl::isImplicit)
    .def("setImplicit", &clang::Decl::setImplicit)
    .def("dump", (void (clang::Decl::*)() const) & clang::Decl::dump)
    .def("getBeginLoc", &clang::Decl::getBeginLoc)
    .def("getEndLoc", &clang::Decl::getEndLoc)
    .def("getAccess",&clang::Decl::getAccess)
    .enum_<clang::AccessSpecifier>(asttooling::_sym_STARclangAccessSpecifierSTAR)[
                                                                                  value("AS_public", clang::AS_public),
                                                                                  value("AS_protected", clang::AS_protected),
                                                                                  value("AS_private", clang::AS_private),
                                                                                  value("AS_none", clang::AS_none) ];

  
#define CLASS_DECL(_m_,_Class_, _Base_) class_<_Class_##Decl, _Base_>(_m_,#_Class_ "Decl")
  CLASS_DECL(m,AccessSpec, Decl);
  CLASS_DECL(m,Block, Decl);
  CLASS_DECL(m,Captured, Decl);
  CLASS_DECL(m,ClassScopeFunctionSpecialization, Decl);
  CLASS_DECL(m,Empty, Decl);
  CLASS_DECL(m,FileScopeAsm, Decl);
  CLASS_DECL(m,Friend, Decl);
  CLASS_DECL(m,FriendTemplate, Decl);
  CLASS_DECL(m,Import, Decl);
  CLASS_DECL(m,LinkageSpec, Decl);
  CLASS_DECL(m,Named, Decl)
    .def("getIdentifier", &NamedDecl::getIdentifier)
    .def("getName", &NamedDecl::getName)
    .def("getNameAsString", &NamedDecl::getNameAsString)
    .def("getQualifiedNameAsString", (std::string (NamedDecl::*)() const) & NamedDecl::getQualifiedNameAsString);
  CLASS_DECL(m,Label, NamedDecl);
  CLASS_DECL(m,Namespace, NamedDecl);
  CLASS_DECL(m,NamespaceAlias, NamedDecl);
  CLASS_DECL(m,ObjCCompatibleAlias, NamedDecl);
  CLASS_DECL(m,ObjCContainer, NamedDecl);
  CLASS_DECL(m,ObjCCategory, ObjCContainerDecl);
  CLASS_DECL(m,ObjCImpl, ObjCContainerDecl);
  CLASS_DECL(m,ObjCCategoryImpl, ObjCImplDecl);
  CLASS_DECL(m,ObjCImplementation, ObjCImplDecl);
  CLASS_DECL(m,ObjCInterface, ObjCContainerDecl);
  CLASS_DECL(m,ObjCProtocol, ObjCContainerDecl);
  CLASS_DECL(m,ObjCMethod, NamedDecl);
  CLASS_DECL(m,ObjCProperty, NamedDecl);
  CLASS_DECL(m,Template, NamedDecl);
  CLASS_DECL(m,RedeclarableTemplate, TemplateDecl);
  CLASS_DECL(m,ClassTemplate, RedeclarableTemplateDecl);
  CLASS_DECL(m,FunctionTemplate, RedeclarableTemplateDecl);
  CLASS_DECL(m,TypeAliasTemplate, RedeclarableTemplateDecl);
  CLASS_DECL(m,VarTemplate, RedeclarableTemplateDecl);
  CLASS_DECL(m,TemplateTemplateParm, TemplateDecl);
  CLASS_DECL(m,Type, NamedDecl);
  CLASS_DECL(m,Tag, TypeDecl);
  CLASS_DECL(m,Enum, TagDecl);
  CLASS_DECL(m,Record, TagDecl);
  CLASS_DECL(m,CXXRecord, RecordDecl)
    .iterator("bases-iterator",
              (CXXRecordDecl::base_class_iterator (CXXRecordDecl::*)()) & CXXRecordDecl::bases_begin,
              (CXXRecordDecl::base_class_iterator (CXXRecordDecl::*)()) & CXXRecordDecl::bases_end)
    .iterator("vbases-iterator",
              (CXXRecordDecl::base_class_iterator (CXXRecordDecl::*)()) & CXXRecordDecl::vbases_begin,
              (CXXRecordDecl::base_class_iterator (CXXRecordDecl::*)()) & CXXRecordDecl::vbases_end)
    .def("hasDefinition", &clang::CXXRecordDecl::hasDefinition),
    CLASS_DECL(m,ClassTemplateSpecialization, CXXRecordDecl)
        //            .   def("desugar",&clang::ClassTemplateSpecializationDecl::desugar)
    .def("getTemplateArgs", &clang::ClassTemplateSpecializationDecl::getTemplateArgs)
    .def("getTypeAsWritten", &clang::ClassTemplateSpecializationDecl::getTypeAsWritten)
    .def("getPointOfInstantiation", &clang::ClassTemplateSpecializationDecl::getPointOfInstantiation)
    .def("getSpecializedTemplate", &clang::ClassTemplateSpecializationDecl::getSpecializedTemplate)
    .def("getSpecializationKind", &clang::ClassTemplateSpecializationDecl::getSpecializationKind)
    .enum_<clang::TemplateSpecializationKind>(asttooling::_sym_STARclangTemplateSpecializationKindSTAR)[
                                                                                                        value("TSK_Undeclared", clang::TemplateSpecializationKind::TSK_Undeclared),
                                                                                                        value("TSK_ImplicitInstantiation", clang::TemplateSpecializationKind::TSK_ImplicitInstantiation),
                                                                                                        value("TSK_ExplicitSpecialization", clang::TemplateSpecializationKind::TSK_ExplicitSpecialization),
                                                                                                        value("TSK_ExplicitInstantiationDeclaration", clang::TemplateSpecializationKind::TSK_ExplicitInstantiationDeclaration),
                                                                                                        value("TSK_ExplicitInstantiationDefinition", clang::TemplateSpecializationKind::TSK_ExplicitInstantiationDefinition)
                                                                                                        ],
    m.def("getNameForDiagnostic", &af_getNameForDiagnostic, "docstring", "(decl stream lang-opts qualified)");
  CLASS_DECL(m,ClassTemplatePartialSpecialization, ClassTemplateSpecializationDecl);
  CLASS_DECL(m,TemplateTypeParm, TypeDecl);
  CLASS_DECL(m,TypedefName, TypeDecl)
    .def("getUnderlyingType", &clang::TypedefNameDecl::getUnderlyingType)
    .def("getCanonicalDecl", (TypedefNameDecl * (clang::TypedefNameDecl::*)()) & clang::TypedefNameDecl::getCanonicalDecl);
  CLASS_DECL(m,TypeAlias, TypedefNameDecl);
  CLASS_DECL(m,Typedef, TypedefNameDecl);
  CLASS_DECL(m,UnresolvedUsingTypename, TypeDecl);
  CLASS_DECL(m,Using, NamedDecl);
  CLASS_DECL(m,UsingDirective, NamedDecl);
  CLASS_DECL(m,UsingShadow, NamedDecl);
  CLASS_DECL(m,Value, NamedDecl)
    .def("getType", &clang::ValueDecl::getType);
  CLASS_DECL(m,Declarator, ValueDecl);
  CLASS_DECL(m,Field, DeclaratorDecl)
    .def("getFieldIndex",&clang::FieldDecl::getFieldIndex);
  CLASS_DECL(m,ObjCAtDefsField, FieldDecl);
  CLASS_DECL(m,ObjCIvar, FieldDecl);
  CLASS_DECL(m,Function, DeclaratorDecl)
    .def("hasBody", (bool (clang::FunctionDecl::*)() const) & clang::FunctionDecl::hasBody)
    .def("isThisDeclarationADefinition", &clang::FunctionDecl::isThisDeclarationADefinition)
    .def("doesThisDeclarationHaveABody", &clang::FunctionDecl::doesThisDeclarationHaveABody);
  CLASS_DECL(m,CXXMethod, FunctionDecl);
  CLASS_DECL(m,CXXConstructor, CXXMethodDecl);
  CLASS_DECL(m,CXXConversion, CXXMethodDecl);
  CLASS_DECL(m,CXXDestructor, CXXMethodDecl);
  CLASS_DECL(m,MSProperty, DeclaratorDecl);
  CLASS_DECL(m,NonTypeTemplateParm, DeclaratorDecl);
  CLASS_DECL(m,Var, DeclaratorDecl)
    .def("getSourceRange", &clang::VarDecl::getSourceRange)
    .def("isStaticLocal", &clang::VarDecl::isStaticLocal)
    .def("isLocalVarDecl", &clang::VarDecl::isLocalVarDecl)
    .def("hasGlobalStorage", &clang::VarDecl::hasGlobalStorage);
  CLASS_DECL(m,ImplicitParam, VarDecl);
  CLASS_DECL(m,ParmVar, VarDecl);
  CLASS_DECL(m,VarTemplateSpecialization, VarDecl);
  CLASS_DECL(m,VarTemplatePartialSpecialization, VarTemplateSpecializationDecl);
  CLASS_DECL(m,EnumConstant, ValueDecl);
  CLASS_DECL(m,IndirectField, ValueDecl);
  CLASS_DECL(m,UnresolvedUsingValue, ValueDecl);
  CLASS_DECL(m,OMPThreadPrivate, Decl);
  CLASS_DECL(m,ObjCPropertyImpl, Decl);
  CLASS_DECL(m,StaticAssert, Decl);
  CLASS_DECL(m,TranslationUnit, Decl);
#undef DECL
  class_<Stmt>(m,"Stmt")
    .def("dump", (void (clang::Stmt::*)() const) & clang::Stmt::dump)
    .def("getBeginLoc", &clang::Stmt::getBeginLoc)
    .def("getEndLoc", &clang::Stmt::getEndLoc);
#define CLASS_STMT(_m_,_Class_, _Base_) class_<_Class_, _Base_>(_m_,#_Class_)
  CLASS_STMT(m,AsmStmt, Stmt);
  CLASS_STMT(m,GCCAsmStmt, AsmStmt);
  CLASS_STMT(m,MSAsmStmt, AsmStmt);
  CLASS_STMT(m,AttributedStmt, Stmt);
  CLASS_STMT(m,BreakStmt, Stmt);
  CLASS_STMT(m,CXXCatchStmt, Stmt);
  CLASS_STMT(m,CXXForRangeStmt, Stmt);
  CLASS_STMT(m,CXXTryStmt, Stmt);
  CLASS_STMT(m,CapturedStmt, Stmt);
  CLASS_STMT(m,CompoundStmt, Stmt);
  CLASS_STMT(m,ContinueStmt, Stmt);
  CLASS_STMT(m,DeclStmt, Stmt);
  CLASS_STMT(m,DoStmt, Stmt);
  CLASS_STMT(m,Expr, Stmt)
    .def("getType", &Expr::getType)
    .def("getBestDynamicClassType", &clang::Expr::getBestDynamicClassType);
  CLASS_STMT(m,AbstractConditionalOperator, Expr);
  CLASS_STMT(m,BinaryConditionalOperator, AbstractConditionalOperator);
  CLASS_STMT(m,ConditionalOperator, AbstractConditionalOperator);
  CLASS_STMT(m,AddrLabelExpr, Expr);
  CLASS_STMT(m,ArraySubscriptExpr, Expr);
  CLASS_STMT(m,ArrayTypeTraitExpr, Expr);
  CLASS_STMT(m,AsTypeExpr, Expr);
  CLASS_STMT(m,AtomicExpr, Expr);
  CLASS_STMT(m,BinaryOperator, Expr);
  CLASS_STMT(m,CompoundAssignOperator, BinaryOperator);
  CLASS_STMT(m,BlockExpr, Expr);
  CLASS_STMT(m,CXXBindTemporaryExpr, Expr);
  CLASS_STMT(m,CXXBoolLiteralExpr, Expr);
  CLASS_STMT(m,CXXConstructExpr, Expr);
  CLASS_STMT(m,CXXTemporaryObjectExpr, CXXConstructExpr);
  CLASS_STMT(m,CXXDefaultArgExpr, Expr);
  CLASS_STMT(m,CXXDefaultInitExpr, Expr);
  CLASS_STMT(m,CXXDeleteExpr, Expr);
  CLASS_STMT(m,CXXDependentScopeMemberExpr, Expr);
  CLASS_STMT(m,CXXNewExpr, Expr)
    .def("getNumPlacementArgs", &clang::CXXNewExpr::getNumPlacementArgs);
  CLASS_STMT(m,CXXNoexceptExpr, Expr);
  CLASS_STMT(m,CXXNullPtrLiteralExpr, Expr);
  CLASS_STMT(m,CXXPseudoDestructorExpr, Expr);
  CLASS_STMT(m,CXXScalarValueInitExpr, Expr);
  CLASS_STMT(m,CXXStdInitializerListExpr, Expr);
  CLASS_STMT(m,CXXThisExpr, Expr);
  CLASS_STMT(m,CXXThrowExpr, Expr);
  CLASS_STMT(m,CXXTypeidExpr, Expr);
  CLASS_STMT(m,CXXUnresolvedConstructExpr, Expr);
  CLASS_STMT(m,CXXUuidofExpr, Expr);
  CLASS_STMT(m,CallExpr, Expr);
  CLASS_STMT(m,CUDAKernelCallExpr, CallExpr);
  CLASS_STMT(m,CXXMemberCallExpr, CallExpr)
    .def("getMethodDecl", &clang::CXXMemberCallExpr::getMethodDecl)
    .def("getImplicitObjectArgument", &clang::CXXMemberCallExpr::getImplicitObjectArgument);
  CLASS_STMT(m,CXXOperatorCallExpr, CallExpr);
  CLASS_STMT(m,UserDefinedLiteral, CallExpr);
  CLASS_STMT(m,CastExpr, Expr);
  CLASS_STMT(m,ExplicitCastExpr, CastExpr);
  CLASS_STMT(m,CStyleCastExpr, ExplicitCastExpr);
  CLASS_STMT(m,CXXFunctionalCastExpr, ExplicitCastExpr);
  CLASS_STMT(m,CXXNamedCastExpr, ExplicitCastExpr);
  CLASS_STMT(m,CXXConstCastExpr, CXXNamedCastExpr);
  CLASS_STMT(m,CXXDynamicCastExpr, CXXNamedCastExpr);
  CLASS_STMT(m,CXXReinterpretCastExpr, CXXNamedCastExpr);
  CLASS_STMT(m,CXXStaticCastExpr, CXXNamedCastExpr);
  CLASS_STMT(m,ObjCBridgedCastExpr, ExplicitCastExpr);
  CLASS_STMT(m,ImplicitCastExpr, CastExpr);
  CLASS_STMT(m,CharacterLiteral, Expr);
  CLASS_STMT(m,ChooseExpr, Expr);
  CLASS_STMT(m,CompoundLiteralExpr, Expr);
  CLASS_STMT(m,ConvertVectorExpr, Expr);
  CLASS_STMT(m,DeclRefExpr, Expr);
  CLASS_STMT(m,DependentScopeDeclRefExpr, Expr);
  CLASS_STMT(m,DesignatedInitExpr, Expr);
  CLASS_STMT(m,ExprWithCleanups, Expr);
  CLASS_STMT(m,ExpressionTraitExpr, Expr);
  CLASS_STMT(m,ExtVectorElementExpr, Expr);
  CLASS_STMT(m,FloatingLiteral, Expr);
  CLASS_STMT(m,FunctionParmPackExpr, Expr);
  CLASS_STMT(m,GNUNullExpr, Expr);
  CLASS_STMT(m,GenericSelectionExpr, Expr);
  CLASS_STMT(m,ImaginaryLiteral, Expr);
  CLASS_STMT(m,ImplicitValueInitExpr, Expr);
  CLASS_STMT(m,InitListExpr, Expr);
  CLASS_STMT(m,IntegerLiteral, Expr);
  CLASS_STMT(m,LambdaExpr, Expr);
  CLASS_STMT(m,MSPropertyRefExpr, Expr);
  CLASS_STMT(m,MaterializeTemporaryExpr, Expr);
  CLASS_STMT(m,MemberExpr, Expr);
  CLASS_STMT(m,ObjCArrayLiteral, Expr);
  CLASS_STMT(m,ObjCBoolLiteralExpr, Expr);
  CLASS_STMT(m,ObjCBoxedExpr, Expr);
  CLASS_STMT(m,ObjCDictionaryLiteral, Expr);
  CLASS_STMT(m,ObjCEncodeExpr, Expr);
  CLASS_STMT(m,ObjCIndirectCopyRestoreExpr, Expr);
  CLASS_STMT(m,ObjCIsaExpr, Expr);
  CLASS_STMT(m,ObjCIvarRefExpr, Expr);
  CLASS_STMT(m,ObjCMessageExpr, Expr);
  CLASS_STMT(m,ObjCPropertyRefExpr, Expr);
  CLASS_STMT(m,ObjCProtocolExpr, Expr);
  CLASS_STMT(m,ObjCSelectorExpr, Expr);
  CLASS_STMT(m,ObjCStringLiteral, Expr);
  CLASS_STMT(m,ObjCSubscriptRefExpr, Expr);
  CLASS_STMT(m,OffsetOfExpr, Expr);
  CLASS_STMT(m,OpaqueValueExpr, Expr);
  CLASS_STMT(m,OverloadExpr, Expr);
  CLASS_STMT(m,UnresolvedLookupExpr, OverloadExpr);
  CLASS_STMT(m,UnresolvedMemberExpr, OverloadExpr);
  CLASS_STMT(m,PackExpansionExpr, Expr);
  CLASS_STMT(m,ParenExpr, Expr);
  CLASS_STMT(m,ParenListExpr, Expr);
  CLASS_STMT(m,PredefinedExpr, Expr);
  CLASS_STMT(m,PseudoObjectExpr, Expr);
  CLASS_STMT(m,ShuffleVectorExpr, Expr);
  CLASS_STMT(m,SizeOfPackExpr, Expr);
  CLASS_STMT(m,StmtExpr, Expr);
  CLASS_STMT(m,StringLiteral, Expr)
    .def("getString",&clang::StringLiteral::getString);
  CLASS_STMT(m,SubstNonTypeTemplateParmExpr, Expr);
  CLASS_STMT(m,SubstNonTypeTemplateParmPackExpr, Expr);
  CLASS_STMT(m,TypeTraitExpr, Expr);
  CLASS_STMT(m,UnaryExprOrTypeTraitExpr, Expr);
  CLASS_STMT(m,UnaryOperator, Expr);
  CLASS_STMT(m,VAArgExpr, Expr);
  CLASS_STMT(m,ForStmt, Stmt);
  CLASS_STMT(m,GotoStmt, Stmt);
  CLASS_STMT(m,IfStmt, Stmt);
  CLASS_STMT(m,IndirectGotoStmt, Stmt);
  CLASS_STMT(m,LabelStmt, Stmt);
  CLASS_STMT(m,MSDependentExistsStmt, Stmt);
  CLASS_STMT(m,NullStmt, Stmt);
  CLASS_STMT(m,OMPExecutableDirective, Stmt);
  CLASS_STMT(m,OMPSimdDirective, OMPExecutableDirective);
  CLASS_STMT(m,OMPForDirective, OMPExecutableDirective);
  CLASS_STMT(m,OMPParallelDirective, OMPExecutableDirective);
  CLASS_STMT(m,OMPSectionDirective, OMPExecutableDirective);
  CLASS_STMT(m,OMPSectionsDirective, OMPExecutableDirective);
  CLASS_STMT(m,OMPSingleDirective, OMPExecutableDirective);
  CLASS_STMT(m,ObjCAtCatchStmt, Stmt);
  CLASS_STMT(m,ObjCAtFinallyStmt, Stmt);
  CLASS_STMT(m,ObjCAtSynchronizedStmt, Stmt);
  CLASS_STMT(m,ObjCAtThrowStmt, Stmt);
  CLASS_STMT(m,ObjCAtTryStmt, Stmt);
  CLASS_STMT(m,ObjCAutoreleasePoolStmt, Stmt);
  CLASS_STMT(m,ObjCForCollectionStmt, Stmt);
  CLASS_STMT(m,ReturnStmt, Stmt);
  CLASS_STMT(m,SEHExceptStmt, Stmt);
  CLASS_STMT(m,SEHFinallyStmt, Stmt);
  CLASS_STMT(m,SEHTryStmt, Stmt);
  CLASS_STMT(m,SwitchCase, Stmt);
  CLASS_STMT(m,CaseStmt, SwitchCase);
  CLASS_STMT(m,DefaultStmt, SwitchCase);
  CLASS_STMT(m,SwitchStmt, Stmt);
  CLASS_STMT(m,WhileStmt, Stmt);
  class_<Type>(m,"Type")
    .def("dump", (void(clang::Type::*)() const)&clang::Type::dump)
        //            .  def("getAsCXXRecordDecl",&clang::Type::getAsCXXRecordDecl)
        //            .  def("getAsStructureType",&clang::Type::getAsStructureType)
    .def("getAsTemplateSpecializationType", &clang::Type::getAs<clang::TemplateSpecializationType>, policies<>(), "Specialization of getAs<TemplateSpecializationType>", "", "")
    .def("isIntegerType", &clang::Type::isIntegerType)
    .def("getCanonicalTypeInternal", &clang::Type::getCanonicalTypeInternal);
  m.def("getAsCXXRecordDecl", &af_getAsCXXRecordDecl, "getAsCXXRecordDecl - returns the most derived CXXRecordDecl* ptr or NIL", "(arg)");

#define CLASS_TYPE(_m_,_Class_, _Base_) class_<_Class_##Type, _Base_>(_m_,#_Class_ "Type")
  CLASS_TYPE(m,Builtin, Type)
    .def("desugar", &clang::BuiltinType::desugar);
  CLASS_TYPE(m,Complex, Type);
  CLASS_TYPE(m,Pointer, Type)
    .def("desugar", &clang::PointerType::desugar)
    .def("getPointeeType", &clang::PointerType::getPointeeType);
  CLASS_TYPE(m,BlockPointer, Type);
  CLASS_TYPE(m,Reference, Type);
  CLASS_TYPE(m,LValueReference, ReferenceType)
    .def("desugar", &clang::LValueReferenceType::desugar);
  CLASS_TYPE(m,RValueReference, ReferenceType)
    .def("desugar", &clang::RValueReferenceType::desugar);
  CLASS_TYPE(m,MemberPointer, Type)
    .def("desugar", &clang::MemberPointerType::desugar)
    .def("getPointeeType", &clang::MemberPointerType::getPointeeType);
  CLASS_TYPE(m,Array, Type)
    .def("getElementType", &clang::ArrayType::getElementType);
  CLASS_TYPE(m,ConstantArray, ArrayType)
    .def("desugar", &clang::ConstantArrayType::desugar);
  m.def("constant-array-get-size", &af_constant_array_get_size, "returns the size of the constant array", "(constant-array)");
  CLASS_TYPE(m,IncompleteArray, ArrayType)
    .def("desugar", &clang::IncompleteArrayType::desugar);
  CLASS_TYPE(m,VariableArray, ArrayType);
  CLASS_TYPE(m,DependentSizedArray, ArrayType)
    .def("desugar", &clang::DependentSizedArrayType::desugar);
  CLASS_TYPE(m,DependentSizedExtVector, Type);
  CLASS_TYPE(m,Vector, Type)
    .def("desugar", &clang::VectorType::desugar);
  CLASS_TYPE(m,ExtVector, VectorType);
  CLASS_TYPE(m,Function, Type);
  CLASS_TYPE(m,FunctionProto, FunctionType)
    .def("desugar", &clang::FunctionProtoType::desugar);
  CLASS_TYPE(m,FunctionNoProto, FunctionType);
  CLASS_TYPE(m,UnresolvedUsing, Type);
  CLASS_TYPE(m,Paren, Type)
    .def("getInnerType", &clang::ParenType::getInnerType);
  CLASS_TYPE(m,Typedef, Type)
    .def("getDecl", &clang::TypedefType::getDecl)
    .def("isSugared", &clang::TypedefType::isSugared)
    .def("desugar", &clang::TypedefType::desugar);
  CLASS_TYPE(m,Adjusted, Type);
  CLASS_TYPE(m,Decayed, AdjustedType);
  CLASS_TYPE(m,TypeOfExpr, Type);
  CLASS_TYPE(m,TypeOf, Type);
  CLASS_TYPE(m,Decltype, Type);
  CLASS_TYPE(m,UnaryTransform, Type);
  CLASS_TYPE(m,Tag, Type);
  CLASS_TYPE(m,Record, TagType)
    .def("getDecl", &clang::RecordType::getDecl)
    .def("desugar", &clang::RecordType::desugar);
  CLASS_TYPE(m,Enum, TagType)
    .def("desugar", &clang::EnumType::desugar);
  CLASS_TYPE(m,Elaborated, Type)
    .def("getNamedType", &clang::ElaboratedType::getNamedType);
  CLASS_TYPE(m,Attributed, Type);
  CLASS_TYPE(m,TemplateTypeParm, Type)
    .def("desugar", &clang::TemplateTypeParmType::desugar);
  CLASS_TYPE(m,SubstTemplateTypeParm, Type)
    .def("desugar", &clang::SubstTemplateTypeParmType::desugar);
  CLASS_TYPE(m,SubstTemplateTypeParmPack, Type);
  CLASS_TYPE(m,TemplateSpecialization, Type)
    .def("getTemplateName", &clang::TemplateSpecializationType::getTemplateName)
    .def("getNumArgs", &clang::TemplateSpecializationType::getNumArgs)
    .def("getArg", &clang::TemplateSpecializationType::getArg)
    .def("desugar", &clang::TemplateSpecializationType::desugar)
    .def("getAliasedType", &clang::TemplateSpecializationType::getAliasedType)
    .def("isTypeAlias", &clang::TemplateSpecializationType::isTypeAlias)
    .def("isCurrentInstantiation", &clang::TemplateSpecializationType::isCurrentInstantiation);
  CLASS_TYPE(m,Auto, Type)
    ,
//        .def("desugar", &clang::AutoType::desugar);
    CLASS_TYPE(m,InjectedClassName, Type)
    .def("getDecl", &clang::InjectedClassNameType::getDecl);
  CLASS_TYPE(m,DependentName, Type)
    .def("desugar", &clang::DependentNameType::desugar)
    .def("isSugared", &clang::DependentNameType::isSugared);
  CLASS_TYPE(m,DependentTemplateSpecialization, Type)
    .def("desugar", &clang::DependentTemplateSpecializationType::desugar);
  CLASS_TYPE(m,PackExpansion, Type);
  CLASS_TYPE(m,ObjCObject, Type);
  CLASS_TYPE(m,ObjCInterface, ObjCObjectType);
  CLASS_TYPE(m,ObjCObjectPointer, Type);
  CLASS_TYPE(m,Atomic, Type);
  class_<clang::QualType>(m,"QualType")
    .def("getAsString", (std::string (clang::QualType::*)() const) & clang::QualType::getAsString)
    .def("isCanonical", &clang::QualType::isCanonical)
    .def("getCanonicalType", &clang::QualType::getCanonicalType);
  m.def("getTypePtrOrNull", &af_getTypePtrOrNull, "returns the most derived Type* ptr or NIL");
  m.def("makeQualType", &af_makeQualType, "docstring","(type)");
  class_<clang::TypeLoc>(m,"TypeLoc")
    .def("getSourceRange", &clang::TypeLoc::getSourceRange)
    .def("getLocalSourceRange", &clang::TypeLoc::getLocalSourceRange)
    .def("getBeginLoc", &clang::TypeLoc::getBeginLoc)
    .def("getEndLoc", &clang::TypeLoc::getEndLoc);
  class_<clang::CXXBaseSpecifier>(m,"CXXBaseSpecifier")
    .def("getType", &clang::CXXBaseSpecifier::getType);
  class_<clang::TemplateArgument>(m,"TemplateArgument")
    .def("getKind", &clang::TemplateArgument::getKind)
    .def("pack_size", &clang::TemplateArgument::pack_size)
    .def("getPackAsArray", &clang::TemplateArgument::getPackAsArray)
    .def("getAsType", &clang::TemplateArgument::getAsType)
    .def("getAsIntegral", &clang::TemplateArgument::getAsIntegral)
    .def("getAsTemplate", &clang::TemplateArgument::getAsTemplate)
    .def("getNullPtrType", &clang::TemplateArgument::getNullPtrType)
    .def("getAsDecl", &clang::TemplateArgument::getAsDecl)
    .def("getAsExpr", &clang::TemplateArgument::getAsExpr)
        //            .  iterator("pack",&clang::TemplateArgument::pack_begin, &clang::TemplateArgument::pack_end)
    .enum_<clang::TemplateArgument::ArgKind>(asttooling::_sym_STARclangTemplateArgumentArgKindSTAR)
    [
     value("argkind-Type", clang::TemplateArgument::ArgKind::Type),
     value("argkind-Null", clang::TemplateArgument::ArgKind::Null),
     value("argkind-Declaration", clang::TemplateArgument::ArgKind::Declaration),
     value("argkind-NullPtr", clang::TemplateArgument::ArgKind::NullPtr),
     value("argkind-Integral", clang::TemplateArgument::ArgKind::Integral),
     value("argkind-Template", clang::TemplateArgument::ArgKind::Template),
     value("argkind-TemplateExpansion", clang::TemplateArgument::ArgKind::TemplateExpansion),
     value("argkind-Expression", clang::TemplateArgument::ArgKind::Expression),
     value("argkind-Pack", clang::TemplateArgument::ArgKind::Pack)
     ];
    class_<clang::TemplateName>(m,"TemplateName")
    .def("getAsTemplateDecl", &clang::TemplateName::getAsTemplateDecl);
  class_<clang::TypeSourceInfo>(m,"TypeSourceInfo")
    .def("getType", &clang::TypeSourceInfo::getType)
    .def("getTypeLoc", &clang::TypeSourceInfo::getTypeLoc);
  class_<clang::TemplateArgumentList>(m,"TemplateArgumentList")
    .def("size", &clang::TemplateArgumentList::size)
    .def("TemplateArgumentList-get", &clang::TemplateArgumentList::get);
  class_<clang::IdentifierInfo>(m,"IdentifierInfo");
}
};
