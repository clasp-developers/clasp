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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/str.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/package.h>

#include <clasp/llvmo/translators.h>
#include <clasp/asttooling/translators.h>
#include <clasp/asttooling/symbolTable.h>
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

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::Decl>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::Decl const>);

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::Stmt>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::Stmt const>);

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::Type>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::Type const>);

typedef clbind::Wrapper<clang::QualType, std::unique_ptr<clang::QualType>> QualType_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(QualType_wrapper);

typedef clbind::Wrapper<clang::TypeLoc, std::unique_ptr<clang::TypeLoc>> TypeLoc_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(TypeLoc_wrapper);

INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<const clang::ASTContext>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::ASTContext>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::TemplateArgument const>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::TemplateName const>);
typedef clbind::Wrapper<clang::TemplateName, std::unique_ptr<clang::TemplateName>> TemplateName_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(TemplateName_wrapper);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::TypeSourceInfo>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::TemplateArgumentList>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::IdentifierInfo>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::TemplateArgumentList const>);

#define DECL(_C_, _B_) INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::_C_##Decl>)
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
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Iterator<clang::CXXBaseSpecifier *>);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::CXXBaseSpecifier>);
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

#define STMT_REF_COUNT_ACCESSORS(_C_) INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::_C_>)
STMT_REF_COUNT_ACCESSORS(Expr);
STMT_REF_COUNT_ACCESSORS(GCCAsmStmt);
STMT_REF_COUNT_ACCESSORS(MSAsmStmt);
// firstAsmStmtConstant= GCCAsmStmtClass, lastAsmStmtConstant= MSAsmStmtClass,
STMT_REF_COUNT_ACCESSORS(AttributedStmt);
STMT_REF_COUNT_ACCESSORS(BreakStmt);
STMT_REF_COUNT_ACCESSORS(CXXCatchStmt);
STMT_REF_COUNT_ACCESSORS(CXXForRangeStmt);
STMT_REF_COUNT_ACCESSORS(CXXTryStmt);
STMT_REF_COUNT_ACCESSORS(CapturedStmt);
STMT_REF_COUNT_ACCESSORS(CompoundStmt);
STMT_REF_COUNT_ACCESSORS(ContinueStmt);
STMT_REF_COUNT_ACCESSORS(DeclStmt);
STMT_REF_COUNT_ACCESSORS(DoStmt);
STMT_REF_COUNT_ACCESSORS(BinaryConditionalOperator);
STMT_REF_COUNT_ACCESSORS(ConditionalOperator);
// firstAbstractConditionalOperatorConstant= BinaryConditionalOperatorClass, lastAbstractConditionalOperatorConstant= ConditionalOperatorClass,
STMT_REF_COUNT_ACCESSORS(AddrLabelExpr);
STMT_REF_COUNT_ACCESSORS(ArraySubscriptExpr);
STMT_REF_COUNT_ACCESSORS(ArrayTypeTraitExpr);
STMT_REF_COUNT_ACCESSORS(AsTypeExpr);
STMT_REF_COUNT_ACCESSORS(AtomicExpr);
STMT_REF_COUNT_ACCESSORS(BinaryOperator);
STMT_REF_COUNT_ACCESSORS(CompoundAssignOperator);
// firstBinaryOperatorConstant= BinaryOperatorClass, lastBinaryOperatorConstant= CompoundAssignOperatorClass,
STMT_REF_COUNT_ACCESSORS(BlockExpr);
STMT_REF_COUNT_ACCESSORS(CXXBindTemporaryExpr);
STMT_REF_COUNT_ACCESSORS(CXXBoolLiteralExpr);
STMT_REF_COUNT_ACCESSORS(CXXConstructExpr);
STMT_REF_COUNT_ACCESSORS(CXXTemporaryObjectExpr);
// firstCXXConstructExprConstant= CXXConstructExprClass, lastCXXConstructExprConstant= CXXTemporaryObjectExprClass,
STMT_REF_COUNT_ACCESSORS(CXXDefaultArgExpr);
STMT_REF_COUNT_ACCESSORS(CXXDefaultInitExpr);
STMT_REF_COUNT_ACCESSORS(CXXDeleteExpr);
STMT_REF_COUNT_ACCESSORS(CXXDependentScopeMemberExpr);
STMT_REF_COUNT_ACCESSORS(CXXNewExpr);
STMT_REF_COUNT_ACCESSORS(CXXNoexceptExpr);
STMT_REF_COUNT_ACCESSORS(CXXNullPtrLiteralExpr);
STMT_REF_COUNT_ACCESSORS(CXXPseudoDestructorExpr);
STMT_REF_COUNT_ACCESSORS(CXXScalarValueInitExpr);
STMT_REF_COUNT_ACCESSORS(CXXStdInitializerListExpr);
STMT_REF_COUNT_ACCESSORS(CXXThisExpr);
STMT_REF_COUNT_ACCESSORS(CXXThrowExpr);
STMT_REF_COUNT_ACCESSORS(CXXTypeidExpr);
STMT_REF_COUNT_ACCESSORS(CXXUnresolvedConstructExpr);
STMT_REF_COUNT_ACCESSORS(CXXUuidofExpr);
STMT_REF_COUNT_ACCESSORS(CallExpr);
STMT_REF_COUNT_ACCESSORS(CUDAKernelCallExpr);
STMT_REF_COUNT_ACCESSORS(CXXMemberCallExpr);
STMT_REF_COUNT_ACCESSORS(CXXOperatorCallExpr);
STMT_REF_COUNT_ACCESSORS(UserDefinedLiteral);
// firstCallExprConstant= CallExprClass, lastCallExprConstant= UserDefinedLiteralClass,
STMT_REF_COUNT_ACCESSORS(CStyleCastExpr);
STMT_REF_COUNT_ACCESSORS(CXXFunctionalCastExpr);
STMT_REF_COUNT_ACCESSORS(CXXConstCastExpr);
STMT_REF_COUNT_ACCESSORS(CXXDynamicCastExpr);
STMT_REF_COUNT_ACCESSORS(CXXReinterpretCastExpr);
STMT_REF_COUNT_ACCESSORS(CXXStaticCastExpr);
// firstCXXNamedCastExprConstant= CXXConstCastExprClass, lastCXXNamedCastExprConstant= CXXStaticCastExprClass,
STMT_REF_COUNT_ACCESSORS(ObjCBridgedCastExpr);
// firstExplicitCastExprConstant= CStyleCastExprClass, lastExplicitCastExprConstant= ObjCBridgedCastExprClass,
STMT_REF_COUNT_ACCESSORS(ImplicitCastExpr);
// firstCastExprConstant= CStyleCastExprClass, lastCastExprConstant= ImplicitCastExprClass,
STMT_REF_COUNT_ACCESSORS(CharacterLiteral);
STMT_REF_COUNT_ACCESSORS(ChooseExpr);
STMT_REF_COUNT_ACCESSORS(CompoundLiteralExpr);
STMT_REF_COUNT_ACCESSORS(ConvertVectorExpr);
STMT_REF_COUNT_ACCESSORS(DeclRefExpr);
STMT_REF_COUNT_ACCESSORS(DependentScopeDeclRefExpr);
STMT_REF_COUNT_ACCESSORS(DesignatedInitExpr);
STMT_REF_COUNT_ACCESSORS(ExprWithCleanups);
STMT_REF_COUNT_ACCESSORS(ExpressionTraitExpr);
STMT_REF_COUNT_ACCESSORS(ExtVectorElementExpr);
STMT_REF_COUNT_ACCESSORS(FloatingLiteral);
STMT_REF_COUNT_ACCESSORS(FunctionParmPackExpr);
STMT_REF_COUNT_ACCESSORS(GNUNullExpr);
STMT_REF_COUNT_ACCESSORS(GenericSelectionExpr);
STMT_REF_COUNT_ACCESSORS(ImaginaryLiteral);
STMT_REF_COUNT_ACCESSORS(ImplicitValueInitExpr);
STMT_REF_COUNT_ACCESSORS(InitListExpr);
STMT_REF_COUNT_ACCESSORS(IntegerLiteral);
STMT_REF_COUNT_ACCESSORS(LambdaExpr);
STMT_REF_COUNT_ACCESSORS(MSPropertyRefExpr);
STMT_REF_COUNT_ACCESSORS(MaterializeTemporaryExpr);
STMT_REF_COUNT_ACCESSORS(MemberExpr);
STMT_REF_COUNT_ACCESSORS(ObjCArrayLiteral);
STMT_REF_COUNT_ACCESSORS(ObjCBoolLiteralExpr);
STMT_REF_COUNT_ACCESSORS(ObjCBoxedExpr);
STMT_REF_COUNT_ACCESSORS(ObjCDictionaryLiteral);
STMT_REF_COUNT_ACCESSORS(ObjCEncodeExpr);
STMT_REF_COUNT_ACCESSORS(ObjCIndirectCopyRestoreExpr);
STMT_REF_COUNT_ACCESSORS(ObjCIsaExpr);
STMT_REF_COUNT_ACCESSORS(ObjCIvarRefExpr);
STMT_REF_COUNT_ACCESSORS(ObjCMessageExpr);
STMT_REF_COUNT_ACCESSORS(ObjCPropertyRefExpr);
STMT_REF_COUNT_ACCESSORS(ObjCProtocolExpr);
STMT_REF_COUNT_ACCESSORS(ObjCSelectorExpr);
STMT_REF_COUNT_ACCESSORS(ObjCStringLiteral);
STMT_REF_COUNT_ACCESSORS(ObjCSubscriptRefExpr);
STMT_REF_COUNT_ACCESSORS(OffsetOfExpr);
STMT_REF_COUNT_ACCESSORS(OpaqueValueExpr);
STMT_REF_COUNT_ACCESSORS(UnresolvedLookupExpr);
STMT_REF_COUNT_ACCESSORS(UnresolvedMemberExpr);
// firstOverloadExprConstant= UnresolvedLookupExprClass, lastOverloadExprConstant= UnresolvedMemberExprClass,
STMT_REF_COUNT_ACCESSORS(PackExpansionExpr);
STMT_REF_COUNT_ACCESSORS(ParenExpr);
STMT_REF_COUNT_ACCESSORS(ParenListExpr);
STMT_REF_COUNT_ACCESSORS(PredefinedExpr);
STMT_REF_COUNT_ACCESSORS(PseudoObjectExpr);
STMT_REF_COUNT_ACCESSORS(ShuffleVectorExpr);
STMT_REF_COUNT_ACCESSORS(SizeOfPackExpr);
STMT_REF_COUNT_ACCESSORS(StmtExpr);
STMT_REF_COUNT_ACCESSORS(StringLiteral);
STMT_REF_COUNT_ACCESSORS(SubstNonTypeTemplateParmExpr);
STMT_REF_COUNT_ACCESSORS(SubstNonTypeTemplateParmPackExpr);
STMT_REF_COUNT_ACCESSORS(TypeTraitExpr);
STMT_REF_COUNT_ACCESSORS(UnaryExprOrTypeTraitExpr);
STMT_REF_COUNT_ACCESSORS(UnaryOperator);
STMT_REF_COUNT_ACCESSORS(VAArgExpr);
// firstExprConstant= BinaryConditionalOperatorClass, lastExprConstant= VAArgExprClass,
STMT_REF_COUNT_ACCESSORS(ForStmt);
STMT_REF_COUNT_ACCESSORS(GotoStmt);
STMT_REF_COUNT_ACCESSORS(IfStmt);
STMT_REF_COUNT_ACCESSORS(IndirectGotoStmt);
STMT_REF_COUNT_ACCESSORS(LabelStmt);
STMT_REF_COUNT_ACCESSORS(MSDependentExistsStmt);
STMT_REF_COUNT_ACCESSORS(NullStmt);
STMT_REF_COUNT_ACCESSORS(OMPSimdDirective);
STMT_REF_COUNT_ACCESSORS(OMPForDirective);
STMT_REF_COUNT_ACCESSORS(OMPParallelDirective);
// firstOMPExecutableDirectiveConstant= OMPParallelDirectiveClass, lastOMPExecutableDirectiveConstant= OMPParallelDirectiveClass,
STMT_REF_COUNT_ACCESSORS(ObjCAtCatchStmt);
STMT_REF_COUNT_ACCESSORS(ObjCAtFinallyStmt);
STMT_REF_COUNT_ACCESSORS(ObjCAtSynchronizedStmt);
STMT_REF_COUNT_ACCESSORS(ObjCAtThrowStmt);
STMT_REF_COUNT_ACCESSORS(ObjCAtTryStmt);
STMT_REF_COUNT_ACCESSORS(ObjCAutoreleasePoolStmt);
STMT_REF_COUNT_ACCESSORS(ObjCForCollectionStmt);
STMT_REF_COUNT_ACCESSORS(ReturnStmt);
STMT_REF_COUNT_ACCESSORS(SEHExceptStmt);
STMT_REF_COUNT_ACCESSORS(SEHFinallyStmt);
STMT_REF_COUNT_ACCESSORS(SEHTryStmt);
STMT_REF_COUNT_ACCESSORS(CaseStmt);
STMT_REF_COUNT_ACCESSORS(DefaultStmt);
// firstSwitchCaseConstant= CaseStmtClass, lastSwitchCaseConstant= DefaultStmtClass,
STMT_REF_COUNT_ACCESSORS(SwitchStmt);
STMT_REF_COUNT_ACCESSORS(WhileStmt);

#define TYPE_REF_COUNT_ACCESSORS(_C_, _B_) INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::_C_##Type>)
TYPE_REF_COUNT_ACCESSORS(Builtin, Type);
TYPE_REF_COUNT_ACCESSORS(Complex, Type);
TYPE_REF_COUNT_ACCESSORS(Pointer, Type);
TYPE_REF_COUNT_ACCESSORS(BlockPointer, Type);
TYPE_REF_COUNT_ACCESSORS(Reference, Type);
TYPE_REF_COUNT_ACCESSORS(LValueReference, ReferenceType);
TYPE_REF_COUNT_ACCESSORS(RValueReference, ReferenceType);
TYPE_REF_COUNT_ACCESSORS(MemberPointer, Type);
TYPE_REF_COUNT_ACCESSORS(Array, Type);
TYPE_REF_COUNT_ACCESSORS(ConstantArray, ArrayType);
TYPE_REF_COUNT_ACCESSORS(IncompleteArray, ArrayType);
TYPE_REF_COUNT_ACCESSORS(VariableArray, ArrayType);
TYPE_REF_COUNT_ACCESSORS(DependentSizedArray, ArrayType);
TYPE_REF_COUNT_ACCESSORS(DependentSizedExtVector, Type);
TYPE_REF_COUNT_ACCESSORS(Vector, Type);
TYPE_REF_COUNT_ACCESSORS(ExtVector, VectorType);
TYPE_REF_COUNT_ACCESSORS(Function, Type);
TYPE_REF_COUNT_ACCESSORS(FunctionProto, FunctionType);
TYPE_REF_COUNT_ACCESSORS(FunctionNoProto, FunctionType);
TYPE_REF_COUNT_ACCESSORS(UnresolvedUsing, Type);
TYPE_REF_COUNT_ACCESSORS(Paren, Type);
TYPE_REF_COUNT_ACCESSORS(Typedef, Type);
TYPE_REF_COUNT_ACCESSORS(Adjusted, Type);
TYPE_REF_COUNT_ACCESSORS(Decayed, AdjustedType);
TYPE_REF_COUNT_ACCESSORS(TypeOfExpr, Type);
TYPE_REF_COUNT_ACCESSORS(TypeOf, Type);
TYPE_REF_COUNT_ACCESSORS(Decltype, Type);
TYPE_REF_COUNT_ACCESSORS(UnaryTransform, Type);
TYPE_REF_COUNT_ACCESSORS(Tag, Type);
TYPE_REF_COUNT_ACCESSORS(Record, TagType);
TYPE_REF_COUNT_ACCESSORS(Enum, TagType);
TYPE_REF_COUNT_ACCESSORS(Elaborated, Type);
TYPE_REF_COUNT_ACCESSORS(Attributed, Type);
TYPE_REF_COUNT_ACCESSORS(TemplateTypeParm, Type);
TYPE_REF_COUNT_ACCESSORS(SubstTemplateTypeParm, Type);
TYPE_REF_COUNT_ACCESSORS(SubstTemplateTypeParmPack, Type);
TYPE_REF_COUNT_ACCESSORS(TemplateSpecialization, Type);
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(Wrapper<clang::TemplateSpecializationType const>)

TYPE_REF_COUNT_ACCESSORS(Auto, Type);
TYPE_REF_COUNT_ACCESSORS(InjectedClassName, Type);
TYPE_REF_COUNT_ACCESSORS(DependentName, Type);
TYPE_REF_COUNT_ACCESSORS(DependentTemplateSpecialization, Type);
TYPE_REF_COUNT_ACCESSORS(PackExpansion, Type);
TYPE_REF_COUNT_ACCESSORS(ObjCObject, Type);
TYPE_REF_COUNT_ACCESSORS(ObjCInterface, ObjCObjectType);
TYPE_REF_COUNT_ACCESSORS(ObjCObjectPointer, Type);
TYPE_REF_COUNT_ACCESSORS(Atomic, Type);

namespace asttooling {

template <typename T>
core::T_sp cast_decl(clang::Decl *d) {
  if (T *x = dyn_cast<T>(d)) {
    return clbind::Wrapper<T, T *>::create(x, reg::registered_class<T>::id);
  }
  SIMPLE_ERROR(BF("Could not cast Decl to known Decl"));
}

template <typename T>
core::T_sp cast_stmt(clang::Stmt *d) {
  T *x = llvm::cast<T>(d);
  return clbind::Wrapper<T, T *>::create(x, reg::registered_class<T>::id);
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
  return clbind::Wrapper<T, T *>::create(x, reg::registered_class<T>::id);
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

#define ARGS_af_getTypePtrOrNull "(arg)"
#define DECL_af_getTypePtrOrNull ""
#define DOCS_af_getTypePtrOrNull "getTypePtrOrNull - returns the most derived Type* ptr or NIL"
core::T_sp af_getTypePtrOrNull(clang::QualType qt) {
  _G();
  const clang::Type *tp = qt.getTypePtrOrNull();
  if (tp) {
    return mostDerivedType(tp);
  }
  return _Nil<core::T_O>();
};

#define ARGS_af_getAsCXXRecordDecl "(arg)"
#define DECL_af_getAsCXXRecordDecl ""
#define DOCS_af_getAsCXXRecordDecl "getAsCXXRecordDecl - returns the most derived CXXRecordDecl* ptr or NIL"
core::T_sp af_getAsCXXRecordDecl(clang::Type *tp) {
  _G();
  const clang::CXXRecordDecl *declp = tp->getAsCXXRecordDecl();
  if (declp) {
    return mostDerivedDecl(declp);
  }
  return _Nil<core::T_O>();
};

#define ARGS_af_getNameForDiagnostic "(decl stream lang-opts qualified)"
#define DECL_af_getNameForDiagnostic ""
#define DOCS_af_getNameForDiagnostic "getNameForDiagnostic"
void af_getNameForDiagnostic(clang::ClassTemplateSpecializationDecl *decl, core::T_sp stream, LangOptions &langOps, bool qualified) {
  _G();
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
  _G();
  clang::QualType qt(ty, 0);
  return qt;
};
};

CLBIND_TRANSLATE_SYMBOL_TO_ENUM(clang::TemplateArgument::ArgKind, asttooling::_sym_STARclangTemplateArgumentArgKindSTAR);
CLBIND_TRANSLATE_SYMBOL_TO_ENUM(clang::TemplateSpecializationKind, asttooling::_sym_STARclangTemplateSpecializationKindSTAR);

namespace asttooling {

#define ClangAstPkg "CLANG-AST"

void initialize_astExpose() {
  SYMBOL_EXPORT_SC_(AstToolingPkg, STARclangTemplateSpecializationKindSTAR);
  SYMBOL_EXPORT_SC_(AstToolingPkg, STARclangTemplateArgumentArgKindSTAR);
  core::Package_sp pkg = _lisp->makePackage(ClangAstPkg, {"CAST"}, {}); //{"CAST"},{"CL","CORE","AST_TOOLING"});
  pkg->shadow(core::Str_O::create("TYPE"));
  package(ClangAstPkg)[ //,{"CAST"},{"CL","CORE","AST-TOOLING"}) [
    class_<clang::Decl>("Decl", no_default_constructor)
        .def("getGlobalID", &clang::Decl::getGlobalID)
        .def("isImplicit", &clang::Decl::isImplicit)
        .def("setImplicit", &clang::Decl::setImplicit)
        .def("dump", (void (clang::Decl::*)() const) & clang::Decl::dump)
        .def("getLocStart", &clang::Decl::getLocStart)
        .def("getLocEnd", &clang::Decl::getLocEnd)
#define CLASS_DECL(_Class_, _Base_) class_<_Class_##Decl, _Base_>(#_Class_ "Decl", no_default_constructor)
    ,
    CLASS_DECL(AccessSpec, Decl),
    CLASS_DECL(Block, Decl),
    CLASS_DECL(Captured, Decl),
    CLASS_DECL(ClassScopeFunctionSpecialization, Decl),
    CLASS_DECL(Empty, Decl),
    CLASS_DECL(FileScopeAsm, Decl),
    CLASS_DECL(Friend, Decl),
    CLASS_DECL(FriendTemplate, Decl),
    CLASS_DECL(Import, Decl),
    CLASS_DECL(LinkageSpec, Decl),
    CLASS_DECL(Named, Decl)
        .def("getIdentifier", &NamedDecl::getIdentifier)
        .def("getName", &NamedDecl::getName)
        .def("getNameAsString", &NamedDecl::getNameAsString)
        .def("getQualifiedNameAsString", (std::string (NamedDecl::*)() const) & NamedDecl::getQualifiedNameAsString),
    CLASS_DECL(Label, NamedDecl),
    CLASS_DECL(Namespace, NamedDecl),
    CLASS_DECL(NamespaceAlias, NamedDecl),
    CLASS_DECL(ObjCCompatibleAlias, NamedDecl),
    CLASS_DECL(ObjCContainer, NamedDecl),
    CLASS_DECL(ObjCCategory, ObjCContainerDecl),
    CLASS_DECL(ObjCImpl, ObjCContainerDecl),
    CLASS_DECL(ObjCCategoryImpl, ObjCImplDecl),
    CLASS_DECL(ObjCImplementation, ObjCImplDecl),
    CLASS_DECL(ObjCInterface, ObjCContainerDecl),
    CLASS_DECL(ObjCProtocol, ObjCContainerDecl),
    CLASS_DECL(ObjCMethod, NamedDecl),
    CLASS_DECL(ObjCProperty, NamedDecl),
    CLASS_DECL(Template, NamedDecl),
    CLASS_DECL(RedeclarableTemplate, TemplateDecl),
    CLASS_DECL(ClassTemplate, RedeclarableTemplateDecl),
    CLASS_DECL(FunctionTemplate, RedeclarableTemplateDecl),
    CLASS_DECL(TypeAliasTemplate, RedeclarableTemplateDecl),
    CLASS_DECL(VarTemplate, RedeclarableTemplateDecl),
    CLASS_DECL(TemplateTemplateParm, TemplateDecl),
    CLASS_DECL(Type, NamedDecl),
    CLASS_DECL(Tag, TypeDecl),
    CLASS_DECL(Enum, TagDecl),
    CLASS_DECL(Record, TagDecl),
    CLASS_DECL(CXXRecord, RecordDecl)
        .iterator("bases-iterator",
                  (CXXRecordDecl::base_class_iterator (CXXRecordDecl::*)()) & CXXRecordDecl::bases_begin,
                  (CXXRecordDecl::base_class_iterator (CXXRecordDecl::*)()) & CXXRecordDecl::bases_end)
        .iterator("vbases-iterator",
                  (CXXRecordDecl::base_class_iterator (CXXRecordDecl::*)()) & CXXRecordDecl::vbases_begin,
                  (CXXRecordDecl::base_class_iterator (CXXRecordDecl::*)()) & CXXRecordDecl::vbases_end)
        .def("hasDefinition", &clang::CXXRecordDecl::hasDefinition),
    CLASS_DECL(ClassTemplateSpecialization, CXXRecordDecl)
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
    /* af_fn */ def("getNameForDiagnostic", &af_getNameForDiagnostic, policies<>(),
                    ARGS_af_getNameForDiagnostic,
                    DECL_af_getNameForDiagnostic,
                    DOCS_af_getNameForDiagnostic),
    CLASS_DECL(ClassTemplatePartialSpecialization, ClassTemplateSpecializationDecl),
    CLASS_DECL(TemplateTypeParm, TypeDecl),
    CLASS_DECL(TypedefName, TypeDecl)
        .def("getUnderlyingType", &clang::TypedefNameDecl::getUnderlyingType)
        .def("getCanonicalDecl", (TypedefNameDecl * (clang::TypedefNameDecl::*)()) & clang::TypedefNameDecl::getCanonicalDecl),
    CLASS_DECL(TypeAlias, TypedefNameDecl),
    CLASS_DECL(Typedef, TypedefNameDecl),
    CLASS_DECL(UnresolvedUsingTypename, TypeDecl),
    CLASS_DECL(Using, NamedDecl),
    CLASS_DECL(UsingDirective, NamedDecl),
    CLASS_DECL(UsingShadow, NamedDecl),
    CLASS_DECL(Value, NamedDecl)
        .def("getType", &clang::ValueDecl::getType),
    CLASS_DECL(Declarator, ValueDecl),
    CLASS_DECL(Field, DeclaratorDecl),
    CLASS_DECL(ObjCAtDefsField, FieldDecl),
    CLASS_DECL(ObjCIvar, FieldDecl),
    CLASS_DECL(Function, DeclaratorDecl)
        .def("hasBody", (bool (clang::FunctionDecl::*)() const) & clang::FunctionDecl::hasBody)
        .def("isThisDeclarationADefinition", &clang::FunctionDecl::isThisDeclarationADefinition)
        .def("doesThisDeclarationHaveABody", &clang::FunctionDecl::doesThisDeclarationHaveABody),
    CLASS_DECL(CXXMethod, FunctionDecl),
    CLASS_DECL(CXXConstructor, CXXMethodDecl),
    CLASS_DECL(CXXConversion, CXXMethodDecl),
    CLASS_DECL(CXXDestructor, CXXMethodDecl),
    CLASS_DECL(MSProperty, DeclaratorDecl),
    CLASS_DECL(NonTypeTemplateParm, DeclaratorDecl),
    CLASS_DECL(Var, DeclaratorDecl)
        .def("getSourceRange", &clang::VarDecl::getSourceRange)
        .def("isStaticLocal", &clang::VarDecl::isStaticLocal)
        .def("isLocalVarDecl", &clang::VarDecl::isLocalVarDecl)
        .def("hasGlobalStorage", &clang::VarDecl::hasGlobalStorage),
    CLASS_DECL(ImplicitParam, VarDecl),
    CLASS_DECL(ParmVar, VarDecl),
    CLASS_DECL(VarTemplateSpecialization, VarDecl),
    CLASS_DECL(VarTemplatePartialSpecialization, VarTemplateSpecializationDecl),
    CLASS_DECL(EnumConstant, ValueDecl),
    CLASS_DECL(IndirectField, ValueDecl),
    CLASS_DECL(UnresolvedUsingValue, ValueDecl),
    CLASS_DECL(OMPThreadPrivate, Decl),
    CLASS_DECL(ObjCPropertyImpl, Decl),
    CLASS_DECL(StaticAssert, Decl),
    CLASS_DECL(TranslationUnit, Decl)
#undef DECL
    ,
    class_<Stmt>("Stmt", no_default_constructor)
        .def("dump", (void (clang::Stmt::*)() const) & clang::Stmt::dump)
        .def("getLocStart", &clang::Stmt::getLocStart)
        .def("getLocEnd", &clang::Stmt::getLocEnd)
#define CLASS_STMT(_Class_, _Base_) class_<_Class_, _Base_>(#_Class_, no_default_constructor)
    ,
    CLASS_STMT(AsmStmt, Stmt),
    CLASS_STMT(GCCAsmStmt, AsmStmt),
    CLASS_STMT(MSAsmStmt, AsmStmt),
    CLASS_STMT(AttributedStmt, Stmt),
    CLASS_STMT(BreakStmt, Stmt),
    CLASS_STMT(CXXCatchStmt, Stmt),
    CLASS_STMT(CXXForRangeStmt, Stmt),
    CLASS_STMT(CXXTryStmt, Stmt),
    CLASS_STMT(CapturedStmt, Stmt),
    CLASS_STMT(CompoundStmt, Stmt),
    CLASS_STMT(ContinueStmt, Stmt),
    CLASS_STMT(DeclStmt, Stmt),
    CLASS_STMT(DoStmt, Stmt),
    CLASS_STMT(Expr, Stmt)
        .def("getType", &Expr::getType)
        .def("getBestDynamicClassType", &clang::Expr::getBestDynamicClassType),
    CLASS_STMT(AbstractConditionalOperator, Expr),
    CLASS_STMT(BinaryConditionalOperator, AbstractConditionalOperator),
    CLASS_STMT(ConditionalOperator, AbstractConditionalOperator),
    CLASS_STMT(AddrLabelExpr, Expr),
    CLASS_STMT(ArraySubscriptExpr, Expr),
    CLASS_STMT(ArrayTypeTraitExpr, Expr),
    CLASS_STMT(AsTypeExpr, Expr),
    CLASS_STMT(AtomicExpr, Expr),
    CLASS_STMT(BinaryOperator, Expr),
    CLASS_STMT(CompoundAssignOperator, BinaryOperator),
    CLASS_STMT(BlockExpr, Expr),
    CLASS_STMT(CXXBindTemporaryExpr, Expr),
    CLASS_STMT(CXXBoolLiteralExpr, Expr),
    CLASS_STMT(CXXConstructExpr, Expr),
    CLASS_STMT(CXXTemporaryObjectExpr, CXXConstructExpr),
    CLASS_STMT(CXXDefaultArgExpr, Expr),
    CLASS_STMT(CXXDefaultInitExpr, Expr),
    CLASS_STMT(CXXDeleteExpr, Expr),
    CLASS_STMT(CXXDependentScopeMemberExpr, Expr),
    CLASS_STMT(CXXNewExpr, Expr)
        .def("getNumPlacementArgs", &clang::CXXNewExpr::getNumPlacementArgs),
    CLASS_STMT(CXXNoexceptExpr, Expr),
    CLASS_STMT(CXXNullPtrLiteralExpr, Expr),
    CLASS_STMT(CXXPseudoDestructorExpr, Expr),
    CLASS_STMT(CXXScalarValueInitExpr, Expr),
    CLASS_STMT(CXXStdInitializerListExpr, Expr),
    CLASS_STMT(CXXThisExpr, Expr),
    CLASS_STMT(CXXThrowExpr, Expr),
    CLASS_STMT(CXXTypeidExpr, Expr),
    CLASS_STMT(CXXUnresolvedConstructExpr, Expr),
    CLASS_STMT(CXXUuidofExpr, Expr),
    CLASS_STMT(CallExpr, Expr),
    CLASS_STMT(CUDAKernelCallExpr, CallExpr),
    CLASS_STMT(CXXMemberCallExpr, CallExpr)
        .def("getMethodDecl", &clang::CXXMemberCallExpr::getMethodDecl)
        .def("getImplicitObjectArgument", &clang::CXXMemberCallExpr::getImplicitObjectArgument),
    CLASS_STMT(CXXOperatorCallExpr, CallExpr),
    CLASS_STMT(UserDefinedLiteral, CallExpr),
    CLASS_STMT(CastExpr, Expr),
    CLASS_STMT(ExplicitCastExpr, CastExpr),
    CLASS_STMT(CStyleCastExpr, ExplicitCastExpr),
    CLASS_STMT(CXXFunctionalCastExpr, ExplicitCastExpr),
    CLASS_STMT(CXXNamedCastExpr, ExplicitCastExpr),
    CLASS_STMT(CXXConstCastExpr, CXXNamedCastExpr),
    CLASS_STMT(CXXDynamicCastExpr, CXXNamedCastExpr),
    CLASS_STMT(CXXReinterpretCastExpr, CXXNamedCastExpr),
    CLASS_STMT(CXXStaticCastExpr, CXXNamedCastExpr),
    CLASS_STMT(ObjCBridgedCastExpr, ExplicitCastExpr),
    CLASS_STMT(ImplicitCastExpr, CastExpr),
    CLASS_STMT(CharacterLiteral, Expr),
    CLASS_STMT(ChooseExpr, Expr),
    CLASS_STMT(CompoundLiteralExpr, Expr),
    CLASS_STMT(ConvertVectorExpr, Expr),
    CLASS_STMT(DeclRefExpr, Expr),
    CLASS_STMT(DependentScopeDeclRefExpr, Expr),
    CLASS_STMT(DesignatedInitExpr, Expr),
    CLASS_STMT(ExprWithCleanups, Expr),
    CLASS_STMT(ExpressionTraitExpr, Expr),
    CLASS_STMT(ExtVectorElementExpr, Expr),
    CLASS_STMT(FloatingLiteral, Expr),
    CLASS_STMT(FunctionParmPackExpr, Expr),
    CLASS_STMT(GNUNullExpr, Expr),
    CLASS_STMT(GenericSelectionExpr, Expr),
    CLASS_STMT(ImaginaryLiteral, Expr),
    CLASS_STMT(ImplicitValueInitExpr, Expr),
    CLASS_STMT(InitListExpr, Expr),
    CLASS_STMT(IntegerLiteral, Expr),
    CLASS_STMT(LambdaExpr, Expr),
    CLASS_STMT(MSPropertyRefExpr, Expr),
    CLASS_STMT(MaterializeTemporaryExpr, Expr),
    CLASS_STMT(MemberExpr, Expr),
    CLASS_STMT(ObjCArrayLiteral, Expr),
    CLASS_STMT(ObjCBoolLiteralExpr, Expr),
    CLASS_STMT(ObjCBoxedExpr, Expr),
    CLASS_STMT(ObjCDictionaryLiteral, Expr),
    CLASS_STMT(ObjCEncodeExpr, Expr),
    CLASS_STMT(ObjCIndirectCopyRestoreExpr, Expr),
    CLASS_STMT(ObjCIsaExpr, Expr),
    CLASS_STMT(ObjCIvarRefExpr, Expr),
    CLASS_STMT(ObjCMessageExpr, Expr),
    CLASS_STMT(ObjCPropertyRefExpr, Expr),
    CLASS_STMT(ObjCProtocolExpr, Expr),
    CLASS_STMT(ObjCSelectorExpr, Expr),
    CLASS_STMT(ObjCStringLiteral, Expr),
    CLASS_STMT(ObjCSubscriptRefExpr, Expr),
    CLASS_STMT(OffsetOfExpr, Expr),
    CLASS_STMT(OpaqueValueExpr, Expr),
    CLASS_STMT(OverloadExpr, Expr),
    CLASS_STMT(UnresolvedLookupExpr, OverloadExpr),
    CLASS_STMT(UnresolvedMemberExpr, OverloadExpr),
    CLASS_STMT(PackExpansionExpr, Expr),
    CLASS_STMT(ParenExpr, Expr),
    CLASS_STMT(ParenListExpr, Expr),
    CLASS_STMT(PredefinedExpr, Expr),
    CLASS_STMT(PseudoObjectExpr, Expr),
    CLASS_STMT(ShuffleVectorExpr, Expr),
    CLASS_STMT(SizeOfPackExpr, Expr),
    CLASS_STMT(StmtExpr, Expr),
    CLASS_STMT(StringLiteral, Expr),
    CLASS_STMT(SubstNonTypeTemplateParmExpr, Expr),
    CLASS_STMT(SubstNonTypeTemplateParmPackExpr, Expr),
    CLASS_STMT(TypeTraitExpr, Expr),
    CLASS_STMT(UnaryExprOrTypeTraitExpr, Expr),
    CLASS_STMT(UnaryOperator, Expr),
    CLASS_STMT(VAArgExpr, Expr),
    CLASS_STMT(ForStmt, Stmt),
    CLASS_STMT(GotoStmt, Stmt),
    CLASS_STMT(IfStmt, Stmt),
    CLASS_STMT(IndirectGotoStmt, Stmt),
    CLASS_STMT(LabelStmt, Stmt),
    CLASS_STMT(MSDependentExistsStmt, Stmt),
    CLASS_STMT(NullStmt, Stmt),
    CLASS_STMT(OMPExecutableDirective, Stmt),
    CLASS_STMT(OMPSimdDirective, OMPExecutableDirective),
    CLASS_STMT(OMPForDirective, OMPExecutableDirective),
    CLASS_STMT(OMPParallelDirective, OMPExecutableDirective),
    CLASS_STMT(OMPSectionDirective, OMPExecutableDirective),
    CLASS_STMT(OMPSectionsDirective, OMPExecutableDirective),
    CLASS_STMT(OMPSingleDirective, OMPExecutableDirective),
    CLASS_STMT(ObjCAtCatchStmt, Stmt),
    CLASS_STMT(ObjCAtFinallyStmt, Stmt),
    CLASS_STMT(ObjCAtSynchronizedStmt, Stmt),
    CLASS_STMT(ObjCAtThrowStmt, Stmt),
    CLASS_STMT(ObjCAtTryStmt, Stmt),
    CLASS_STMT(ObjCAutoreleasePoolStmt, Stmt),
    CLASS_STMT(ObjCForCollectionStmt, Stmt),
    CLASS_STMT(ReturnStmt, Stmt),
    CLASS_STMT(SEHExceptStmt, Stmt),
    CLASS_STMT(SEHFinallyStmt, Stmt),
    CLASS_STMT(SEHTryStmt, Stmt),
    CLASS_STMT(SwitchCase, Stmt),
    CLASS_STMT(CaseStmt, SwitchCase),
    CLASS_STMT(DefaultStmt, SwitchCase),
    CLASS_STMT(SwitchStmt, Stmt),
    CLASS_STMT(WhileStmt, Stmt)

    ,
    class_<Type>("Type", no_default_constructor)
        .def("dump", &clang::Type::dump)
        //            .  def("getAsCXXRecordDecl",&clang::Type::getAsCXXRecordDecl)
        //            .  def("getAsStructureType",&clang::Type::getAsStructureType)
        .def("getAsTemplateSpecializationType", &clang::Type::getAs<clang::TemplateSpecializationType>, policies<>(), "", "", "Specialization of getAs<TemplateSpecializationType>")
        .def("isIntegerType", &clang::Type::isIntegerType)
        .def("getCanonicalTypeInternal", &clang::Type::getCanonicalTypeInternal),
    /* regular function bug first arg is Type*/ def("getAsCXXRecordDecl", &af_getAsCXXRecordDecl, policies<>(), ARGS_af_getAsCXXRecordDecl, DECL_af_getAsCXXRecordDecl, DOCS_af_getAsCXXRecordDecl)

#define CLASS_TYPE(_Class_, _Base_) class_<_Class_##Type, _Base_>(#_Class_ "Type", no_default_constructor)
    ,
    CLASS_TYPE(Builtin, Type)
        .def("desugar", &clang::BuiltinType::desugar),
    CLASS_TYPE(Complex, Type),
    CLASS_TYPE(Pointer, Type)
        .def("desugar", &clang::PointerType::desugar)
        .def("getPointeeType", &clang::PointerType::getPointeeType),
    CLASS_TYPE(BlockPointer, Type),
    CLASS_TYPE(Reference, Type),
    CLASS_TYPE(LValueReference, ReferenceType)
        .def("desugar", &clang::LValueReferenceType::desugar),
    CLASS_TYPE(RValueReference, ReferenceType)
        .def("desugar", &clang::RValueReferenceType::desugar),
    CLASS_TYPE(MemberPointer, Type)
        .def("desugar", &clang::MemberPointerType::desugar)
        .def("getPointeeType", &clang::MemberPointerType::getPointeeType),
    CLASS_TYPE(Array, Type)
        .def("getElementType", &clang::ArrayType::getElementType),
    CLASS_TYPE(ConstantArray, ArrayType)
        .def("desugar", &clang::ConstantArrayType::desugar),
    CLASS_TYPE(IncompleteArray, ArrayType)
        .def("desugar", &clang::IncompleteArrayType::desugar),
    CLASS_TYPE(VariableArray, ArrayType),
    CLASS_TYPE(DependentSizedArray, ArrayType)
        .def("desugar", &clang::DependentSizedArrayType::desugar),
    CLASS_TYPE(DependentSizedExtVector, Type),
    CLASS_TYPE(Vector, Type)
        .def("desugar", &clang::VectorType::desugar),
    CLASS_TYPE(ExtVector, VectorType),
    CLASS_TYPE(Function, Type),
    CLASS_TYPE(FunctionProto, FunctionType)
        .def("desugar", &clang::FunctionProtoType::desugar),
    CLASS_TYPE(FunctionNoProto, FunctionType),
    CLASS_TYPE(UnresolvedUsing, Type),
    CLASS_TYPE(Paren, Type)
        .def("getInnerType", &clang::ParenType::getInnerType),
    CLASS_TYPE(Typedef, Type)
        .def("getDecl", &clang::TypedefType::getDecl)
        .def("isSugared", &clang::TypedefType::isSugared)
        .def("desugar", &clang::TypedefType::desugar),
    CLASS_TYPE(Adjusted, Type),
    CLASS_TYPE(Decayed, AdjustedType),
    CLASS_TYPE(TypeOfExpr, Type),
    CLASS_TYPE(TypeOf, Type),
    CLASS_TYPE(Decltype, Type),
    CLASS_TYPE(UnaryTransform, Type),
    CLASS_TYPE(Tag, Type),
    CLASS_TYPE(Record, TagType)
        .def("getDecl", &clang::RecordType::getDecl)
        .def("desugar", &clang::RecordType::desugar),
    CLASS_TYPE(Enum, TagType)
        .def("desugar", &clang::EnumType::desugar),
    CLASS_TYPE(Elaborated, Type)
        .def("getNamedType", &clang::ElaboratedType::getNamedType),
    CLASS_TYPE(Attributed, Type),
    CLASS_TYPE(TemplateTypeParm, Type)
        .def("desugar", &clang::TemplateTypeParmType::desugar),
    CLASS_TYPE(SubstTemplateTypeParm, Type)
        .def("desugar", &clang::SubstTemplateTypeParmType::desugar),
    CLASS_TYPE(SubstTemplateTypeParmPack, Type),
    CLASS_TYPE(TemplateSpecialization, Type)
        .def("getTemplateName", &clang::TemplateSpecializationType::getTemplateName)
        .def("getNumArgs", &clang::TemplateSpecializationType::getNumArgs)
        .def("getArg", &clang::TemplateSpecializationType::getArg)
        .def("desugar", &clang::TemplateSpecializationType::desugar)
        .def("getAliasedType", &clang::TemplateSpecializationType::getAliasedType)
        .def("isTypeAlias", &clang::TemplateSpecializationType::isTypeAlias)
        .def("isCurrentInstantiation", &clang::TemplateSpecializationType::isCurrentInstantiation),
    CLASS_TYPE(Auto, Type)
        .def("desugar", &clang::AutoType::desugar),
    CLASS_TYPE(InjectedClassName, Type)
        .def("getDecl", &clang::InjectedClassNameType::getDecl),
    CLASS_TYPE(DependentName, Type)
        .def("desugar", &clang::DependentNameType::desugar)
        .def("isSugared", &clang::DependentNameType::isSugared),
    CLASS_TYPE(DependentTemplateSpecialization, Type)
        .def("desugar", &clang::DependentTemplateSpecializationType::desugar),
    CLASS_TYPE(PackExpansion, Type),
    CLASS_TYPE(ObjCObject, Type),
    CLASS_TYPE(ObjCInterface, ObjCObjectType),
    CLASS_TYPE(ObjCObjectPointer, Type),
    CLASS_TYPE(Atomic, Type),
    class_<clang::QualType>("QualType", no_default_constructor)
        .def("getAsString", (std::string (clang::QualType::*)() const) & clang::QualType::getAsString)
        .def("isCanonical", &clang::QualType::isCanonical)
        .def("getCanonicalType", &clang::QualType::getCanonicalType),
    /*reg function but first arg is QualType*/ def("getTypePtrOrNull", &af_getTypePtrOrNull, policies<>(), ARGS_af_getTypePtrOrNull, DECL_af_getTypePtrOrNull, DOCS_af_getTypePtrOrNull),
    def("makeQualType", &af_makeQualType, policies<>(), ARGS_af_makeQualType, DECL_af_makeQualType, DOCS_af_makeQualType),
    class_<clang::TypeLoc>("TypeLoc", no_default_constructor)
        .def("getSourceRange", &clang::TypeLoc::getSourceRange)
        .def("getLocalSourceRange", &clang::TypeLoc::getLocalSourceRange)
        .def("getLocStart", &clang::TypeLoc::getLocStart)
        .def("getLocEnd", &clang::TypeLoc::getLocEnd),
    class_<clang::CXXBaseSpecifier>("CXXBaseSpecifier", no_default_constructor)
        .def("getType", &clang::CXXBaseSpecifier::getType),
    class_<clang::TemplateArgument>("TemplateArgument", no_default_constructor)
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
        .enum_<clang::TemplateArgument::ArgKind>(asttooling::_sym_STARclangTemplateArgumentArgKindSTAR)[
      value("Type", clang::TemplateArgument::ArgKind::Type),
      value("Null", clang::TemplateArgument::ArgKind::Null),
      value("Declaration", clang::TemplateArgument::ArgKind::Declaration),
      value("NullPtr", clang::TemplateArgument::ArgKind::NullPtr),
      value("Integral", clang::TemplateArgument::ArgKind::Integral),
      value("Template", clang::TemplateArgument::ArgKind::Template),
      value("TemplateExpansion", clang::TemplateArgument::ArgKind::TemplateExpansion),
      value("Expression", clang::TemplateArgument::ArgKind::Expression),
      value("Pack", clang::TemplateArgument::ArgKind::Pack)
    ],
    class_<clang::TemplateName>("TemplateName", no_default_constructor)
        .def("getAsTemplateDecl", &clang::TemplateName::getAsTemplateDecl),
    class_<clang::TypeSourceInfo>("TypeSourceInfo", no_default_constructor)
        .def("getType", &clang::TypeSourceInfo::getType)
        .def("getTypeLoc", &clang::TypeSourceInfo::getTypeLoc),
    class_<clang::TemplateArgumentList>("TemplateArgumentList", no_default_constructor)
        .def("size", &clang::TemplateArgumentList::size)
        .def("TemplateArgumentList-get", &clang::TemplateArgumentList::get),
    class_<clang::IdentifierInfo>("IdentifierInfo", no_default_constructor)
  ];
}
};
