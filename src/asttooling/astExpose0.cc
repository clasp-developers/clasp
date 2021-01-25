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

};

