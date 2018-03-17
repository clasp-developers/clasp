/*
    File: astVisitor.cc
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
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Tooling/Tooling.h>
#include <clang/Tooling/Refactoring.h>

#include <clasp/core/object.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/translators.h>
#include <clasp/core/wrappers.h>
#include <clasp/clbind/clbind_wrappers.h>
#include <clasp/llvmo/translators.h>
#include <clasp/core/symbolTable.h>
#include <clasp/asttooling/translators.h>
#include <clasp/asttooling/astVisitor.h>

namespace asttooling {

#define ARGS_ast_tooling__makeAstVisitor "(target)"
#define DECL_ast_tooling__makeAstVisitor ""
#define DOCS_ast_tooling__makeAstVisitor "makeAstVisitor"
CL_DEFUN AstVisitor_sp ast_tooling__makeAstVisitor(core::T_sp target) {
  return AstVisitor_O::create(target);
};





SYMBOL_EXPORT_SC_(AstToolingPkg, VisitStmt);

template <typename T>
gctools::smart_ptr<clbind::Wrapper<T, T *>> Wrap(T *p) { return clbind::Wrapper<T, T *>::make_wrapper(p, reg::registered_class<T>::id); };

bool AstVisitor_O::VisitStmt(clang::Stmt *node) {
  return core::T_sp(core::eval::funcall(_sym_VisitStmt, this->_Target, Wrap(node))).isTrue();
}

#if 0
    bool VisitAsmStmt(clang::AsmStmt *node) {
        return true;
    }

    bool VisitGCCAsmStmt(clang::GCCAsmStmt *node) {
        return true;
    }

    bool VisitMSAsmStmt(clang::MSAsmStmt *node) {
        return true;
    }

    bool VisitAttributedStmt(clang::AttributedStmt *node) {
        return true;
    }

    bool VisitBreakStmt(clang::BreakStmt *node) {
        return true;
    }

    bool VisitCXXCatchStmt(clang::CXXCatchStmt *node) {
        return true;
    }

    bool VisitCXXForRangeStmt(clang::CXXForRangeStmt *node) {
        return true;
    }

    bool VisitCXXTryStmt(clang::CXXTryStmt *node) {
        return true;
    }

    bool VisitCapturedStmt(clang::CapturedStmt *node) {
        return true;
    }

    bool VisitCompoundStmt(clang::CompoundStmt *node) {
        return true;
    }

    bool VisitContinueStmt(clang::ContinueStmt *node) {
        return true;
    }

    bool VisitDeclStmt(clang::DeclStmt *node) {
        return true;
    }

    bool VisitDoStmt(clang::DoStmt *node) {
        return true;
    }

    bool VisitExpr(clang::Expr *node) {
        return true;
    }

    bool VisitAbstractConditionalOperator(clang::AbstractConditionalOperator *node) {
        return true;
    }

    bool VisitBinaryConditionalOperator(clang::BinaryConditionalOperator *node) {
        return true;
    }

    bool VisitConditionalOperator(clang::ConditionalOperator *node) {
        return true;
    }

    bool VisitAddrLabelExpr(clang::AddrLabelExpr *node) {
        return true;
    }

    bool VisitArraySubscriptExpr(clang::ArraySubscriptExpr *node) {
        return true;
    }

    bool VisitArrayTypeTraitExpr(clang::ArrayTypeTraitExpr *node) {
        return true;
    }

    bool VisitAsTypeExpr(clang::AsTypeExpr *node) {
        return true;
    }

    bool VisitAtomicExpr(clang::AtomicExpr *node) {
        return true;
    }

    bool VisitBinaryOperator(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitCompoundAssignOperator(clang::CompoundAssignOperator *node) {
        return true;
    }

    bool VisitBinaryTypeTraitExpr(clang::BinaryTypeTraitExpr *node) {
        return true;
    }

    bool VisitBlockExpr(clang::BlockExpr *node) {
        return true;
    }

    bool VisitCXXBindTemporaryExpr(clang::CXXBindTemporaryExpr *node) {
        return true;
    }

    bool VisitCXXBoolLiteralExpr(clang::CXXBoolLiteralExpr *node) {
        return true;
    }

    bool VisitCXXConstructExpr(clang::CXXConstructExpr *node) {
        return true;
    }

    bool VisitCXXTemporaryObjectExpr(clang::CXXTemporaryObjectExpr *node) {
        return true;
    }

    bool VisitCXXDefaultArgExpr(clang::CXXDefaultArgExpr *node) {
        return true;
    }

    bool VisitCXXDefaultInitExpr(clang::CXXDefaultInitExpr *node) {
        return true;
    }

    bool VisitCXXDeleteExpr(clang::CXXDeleteExpr *node) {
        return true;
    }

    bool VisitCXXDependentScopeMemberExpr(clang::CXXDependentScopeMemberExpr *node) {
        return true;
    }

    bool VisitCXXNewExpr(clang::CXXNewExpr *node) {
        return true;
    }

    bool VisitCXXNoexceptExpr(clang::CXXNoexceptExpr *node) {
        return true;
    }

    bool VisitCXXNullPtrLiteralExpr(clang::CXXNullPtrLiteralExpr *node) {
        return true;
    }

    bool VisitCXXPseudoDestructorExpr(clang::CXXPseudoDestructorExpr *node) {
        return true;
    }

    bool VisitCXXScalarValueInitExpr(clang::CXXScalarValueInitExpr *node) {
        return true;
    }

    bool VisitCXXThisExpr(clang::CXXThisExpr *node) {
        return true;
    }

    bool VisitCXXThrowExpr(clang::CXXThrowExpr *node) {
        return true;
    }

    bool VisitCXXTypeidExpr(clang::CXXTypeidExpr *node) {
        return true;
    }

    bool VisitCXXUnresolvedConstructExpr(clang::CXXUnresolvedConstructExpr *node) {
        return true;
    }

    bool VisitCXXUuidofExpr(clang::CXXUuidofExpr *node) {
        return true;
    }

    bool VisitCallExpr(clang::CallExpr *node) {
        return true;
    }

    bool VisitCUDAKernelCallExpr(clang::CUDAKernelCallExpr *node) {
        return true;
    }

    bool VisitCXXMemberCallExpr(clang::CXXMemberCallExpr *node) {
        return true;
    }

    bool VisitCXXOperatorCallExpr(clang::CXXOperatorCallExpr *node) {
        return true;
    }

    bool VisitUserDefinedLiteral(clang::UserDefinedLiteral *node) {
        return true;
    }

    bool VisitCastExpr(clang::CastExpr *node) {
        return true;
    }

    bool VisitExplicitCastExpr(clang::ExplicitCastExpr *node) {
        return true;
    }

    bool VisitCStyleCastExpr(clang::CStyleCastExpr *node) {
        return true;
    }

    bool VisitCXXFunctionalCastExpr(clang::CXXFunctionalCastExpr *node) {
        return true;
    }

    bool VisitCXXNamedCastExpr(clang::CXXNamedCastExpr *node) {
        return true;
    }

    bool VisitCXXConstCastExpr(clang::CXXConstCastExpr *node) {
        return true;
    }

    bool VisitCXXDynamicCastExpr(clang::CXXDynamicCastExpr *node) {
        return true;
    }

    bool VisitCXXReinterpretCastExpr(clang::CXXReinterpretCastExpr *node) {
        return true;
    }

    bool VisitCXXStaticCastExpr(clang::CXXStaticCastExpr *node) {
        return true;
    }

    bool VisitObjCBridgedCastExpr(clang::ObjCBridgedCastExpr *node) {
        return true;
    }

    bool VisitImplicitCastExpr(clang::ImplicitCastExpr *node) {
        return true;
    }

    bool VisitCharacterLiteral(clang::CharacterLiteral *node) {
        return true;
    }

    bool VisitChooseExpr(clang::ChooseExpr *node) {
        return true;
    }

    bool VisitCompoundLiteralExpr(clang::CompoundLiteralExpr *node) {
        return true;
    }

    bool VisitDeclRefExpr(clang::DeclRefExpr *node) {
        return true;
    }

    bool VisitDependentScopeDeclRefExpr(clang::DependentScopeDeclRefExpr *node) {
        return true;
    }

    bool VisitDesignatedInitExpr(clang::DesignatedInitExpr *node) {
        return true;
    }

    bool VisitExprWithCleanups(clang::ExprWithCleanups *node) {
        return true;
    }

    bool VisitExpressionTraitExpr(clang::ExpressionTraitExpr *node) {
        return true;
    }

    bool VisitExtVectorElementExpr(clang::ExtVectorElementExpr *node) {
        return true;
    }

    bool VisitFloatingLiteral(clang::FloatingLiteral *node) {
        return true;
    }

    bool VisitFunctionParmPackExpr(clang::FunctionParmPackExpr *node) {
        return true;
    }

    bool VisitGNUNullExpr(clang::GNUNullExpr *node) {
        return true;
    }

    bool VisitGenericSelectionExpr(clang::GenericSelectionExpr *node) {
        return true;
    }

    bool VisitImaginaryLiteral(clang::ImaginaryLiteral *node) {
        return true;
    }

    bool VisitImplicitValueInitExpr(clang::ImplicitValueInitExpr *node) {
        return true;
    }

    bool VisitInitListExpr(clang::InitListExpr *node) {
        return true;
    }

    bool VisitIntegerLiteral(clang::IntegerLiteral *node) {
        return true;
    }

    bool VisitLambdaExpr(clang::LambdaExpr *node) {
        return true;
    }

    bool VisitMSPropertyRefExpr(clang::MSPropertyRefExpr *node) {
        return true;
    }

    bool VisitMaterializeTemporaryExpr(clang::MaterializeTemporaryExpr *node) {
        return true;
    }

    bool VisitMemberExpr(clang::MemberExpr *node) {
        return true;
    }

    bool VisitObjCArrayLiteral(clang::ObjCArrayLiteral *node) {
        return true;
    }

    bool VisitObjCBoolLiteralExpr(clang::ObjCBoolLiteralExpr *node) {
        return true;
    }

    bool VisitObjCBoxedExpr(clang::ObjCBoxedExpr *node) {
        return true;
    }

    bool VisitObjCDictionaryLiteral(clang::ObjCDictionaryLiteral *node) {
        return true;
    }

    bool VisitObjCEncodeExpr(clang::ObjCEncodeExpr *node) {
        return true;
    }

    bool VisitObjCIndirectCopyRestoreExpr(clang::ObjCIndirectCopyRestoreExpr *node) {
        return true;
    }

    bool VisitObjCIsaExpr(clang::ObjCIsaExpr *node) {
        return true;
    }

    bool VisitObjCIvarRefExpr(clang::ObjCIvarRefExpr *node) {
        return true;
    }

    bool VisitObjCMessageExpr(clang::ObjCMessageExpr *node) {
        return true;
    }

    bool VisitObjCPropertyRefExpr(clang::ObjCPropertyRefExpr *node) {
        return true;
    }

    bool VisitObjCProtocolExpr(clang::ObjCProtocolExpr *node) {
        return true;
    }

    bool VisitObjCSelectorExpr(clang::ObjCSelectorExpr *node) {
        return true;
    }

    bool VisitObjCStringLiteral(clang::ObjCStringLiteral *node) {
        return true;
    }

    bool VisitObjCSubscriptRefExpr(clang::ObjCSubscriptRefExpr *node) {
        return true;
    }

    bool VisitOffsetOfExpr(clang::OffsetOfExpr *node) {
        return true;
    }

    bool VisitOpaqueValueExpr(clang::OpaqueValueExpr *node) {
        return true;
    }

    bool VisitOverloadExpr(clang::OverloadExpr *node) {
        return true;
    }

    bool VisitUnresolvedLookupExpr(clang::UnresolvedLookupExpr *node) {
        return true;
    }

    bool VisitUnresolvedMemberExpr(clang::UnresolvedMemberExpr *node) {
        return true;
    }

    bool VisitPackExpansionExpr(clang::PackExpansionExpr *node) {
        return true;
    }

    bool VisitParenExpr(clang::ParenExpr *node) {
        return true;
    }

    bool VisitParenListExpr(clang::ParenListExpr *node) {
        return true;
    }

    bool VisitPredefinedExpr(clang::PredefinedExpr *node) {
        return true;
    }

    bool VisitPseudoObjectExpr(clang::PseudoObjectExpr *node) {
        return true;
    }

    bool VisitShuffleVectorExpr(clang::ShuffleVectorExpr *node) {
        return true;
    }

    bool VisitSizeOfPackExpr(clang::SizeOfPackExpr *node) {
        return true;
    }

    bool VisitStmtExpr(clang::StmtExpr *node) {
        return true;
    }

    bool VisitStringLiteral(clang::StringLiteral *node) {
        return true;
    }

    bool VisitSubstNonTypeTemplateParmExpr(clang::SubstNonTypeTemplateParmExpr *node) {
        return true;
    }

    bool VisitSubstNonTypeTemplateParmPackExpr(clang::SubstNonTypeTemplateParmPackExpr *node) {
        return true;
    }

    bool VisitTypeTraitExpr(clang::TypeTraitExpr *node) {
        return true;
    }

    bool VisitUnaryExprOrTypeTraitExpr(clang::UnaryExprOrTypeTraitExpr *node) {
        return true;
    }

    bool VisitUnaryOperator(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitUnaryTypeTraitExpr(clang::UnaryTypeTraitExpr *node) {
        return true;
    }

    bool VisitVAArgExpr(clang::VAArgExpr *node) {
        return true;
    }

    bool VisitForStmt(clang::ForStmt *node) {
        return true;
    }

    bool VisitGotoStmt(clang::GotoStmt *node) {
        return true;
    }

    bool VisitIfStmt(clang::IfStmt *node) {
        return true;
    }

    bool VisitIndirectGotoStmt(clang::IndirectGotoStmt *node) {
        return true;
    }

    bool VisitLabelStmt(clang::LabelStmt *node) {
        return true;
    }

    bool VisitMSDependentExistsStmt(clang::MSDependentExistsStmt *node) {
        return true;
    }

    bool VisitNullStmt(clang::NullStmt *node) {
        return true;
    }

    bool VisitObjCAtCatchStmt(clang::ObjCAtCatchStmt *node) {
        return true;
    }

    bool VisitObjCAtFinallyStmt(clang::ObjCAtFinallyStmt *node) {
        return true;
    }

    bool VisitObjCAtSynchronizedStmt(clang::ObjCAtSynchronizedStmt *node) {
        return true;
    }

    bool VisitObjCAtThrowStmt(clang::ObjCAtThrowStmt *node) {
        return true;
    }

    bool VisitObjCAtTryStmt(clang::ObjCAtTryStmt *node) {
        return true;
    }

    bool VisitObjCAutoreleasePoolStmt(clang::ObjCAutoreleasePoolStmt *node) {
        return true;
    }

    bool VisitObjCForCollectionStmt(clang::ObjCForCollectionStmt *node) {
        return true;
    }

    bool VisitReturnStmt(clang::ReturnStmt *node) {
        return true;
    }

    bool VisitSEHExceptStmt(clang::SEHExceptStmt *node) {
        return true;
    }

    bool VisitSEHFinallyStmt(clang::SEHFinallyStmt *node) {
        return true;
    }

    bool VisitSEHTryStmt(clang::SEHTryStmt *node) {
        return true;
    }

    bool VisitSwitchCase(clang::SwitchCase *node) {
        return true;
    }

    bool VisitCaseStmt(clang::CaseStmt *node) {
        return true;
    }

    bool VisitDefaultStmt(clang::DefaultStmt *node) {
        return true;
    }

    bool VisitSwitchStmt(clang::SwitchStmt *node) {
        return true;
    }

    bool VisitWhileStmt(clang::WhileStmt *node) {
        return true;
    }

    bool VisitUnaryPostInc(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitUnaryPostDec(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitUnaryPreInc(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitUnaryPreDec(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitUnaryAddrOf(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitUnaryDeref(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitUnaryPlus(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitUnaryMinus(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitUnaryNot(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitUnaryLNot(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitUnaryReal(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitUnaryImag(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitUnaryExtension(clang::UnaryOperator *node) {
        return true;
    }

    bool VisitBinPtrMemD(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinPtrMemI(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinMul(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinDiv(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinRem(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinAdd(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinSub(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinShl(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinShr(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinLT(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinGT(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinLE(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinGE(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinEQ(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinNE(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinAnd(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinXor(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinOr(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinLAnd(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinLOr(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinAssign(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinComma(clang::BinaryOperator *node) {
        return true;
    }

    bool VisitBinMulAssign(clang::CompoundAssignOperator *node) {
        return true;
    }

    bool VisitBinDivAssign(clang::CompoundAssignOperator *node) {
        return true;
    }

    bool VisitBinRemAssign(clang::CompoundAssignOperator *node) {
        return true;
    }

    bool VisitBinAddAssign(clang::CompoundAssignOperator *node) {
        return true;
    }

    bool VisitBinSubAssign(clang::CompoundAssignOperator *node) {
        return true;
    }

    bool VisitBinShlAssign(clang::CompoundAssignOperator *node) {
        return true;
    }

    bool VisitBinShrAssign(clang::CompoundAssignOperator *node) {
        return true;
    }

    bool VisitBinAndAssign(clang::CompoundAssignOperator *node) {
        return true;
    }

    bool VisitBinOrAssign(clang::CompoundAssignOperator *node) {
        return true;
    }

    bool VisitBinXorAssign(clang::CompoundAssignOperator *node) {
        return true;
    }

    bool VisitType(clang::Type *node) {
        return true;
    }

    bool VisitBuiltinType(clang::BuiltinType *node) {
        return true;
    }

    bool VisitComplexType(clang::ComplexType *node) {
        return true;
    }

    bool VisitPointerType(clang::PointerType *node) {
        return true;
    }

    bool VisitBlockPointerType(clang::BlockPointerType *node) {
        return true;
    }

    bool VisitReferenceType(clang::ReferenceType *node) {
        return true;
    }

    bool VisitLValueReferenceType(clang::LValueReferenceType *node) {
        return true;
    }

    bool VisitRValueReferenceType(clang::RValueReferenceType *node) {
        return true;
    }

    bool VisitMemberPointerType(clang::MemberPointerType *node) {
        return true;
    }

    bool VisitArrayType(clang::ArrayType *node) {
        return true;
    }

    bool VisitConstantArrayType(clang::ConstantArrayType *node) {
        return true;
    }

    bool VisitIncompleteArrayType(clang::IncompleteArrayType *node) {
        return true;
    }

    bool VisitVariableArrayType(clang::VariableArrayType *node) {
        return true;
    }

    bool VisitDependentSizedArrayType(clang::DependentSizedArrayType *node) {
        return true;
    }

    bool VisitDependentSizedExtVectorType(clang::DependentSizedExtVectorType *node) {
        return true;
    }

    bool VisitVectorType(clang::VectorType *node) {
        return true;
    }

    bool VisitExtVectorType(clang::ExtVectorType *node) {
        return true;
    }

    bool VisitFunctionType(clang::FunctionType *node) {
        return true;
    }

    bool VisitFunctionProtoType(clang::FunctionProtoType *node) {
        return true;
    }

    bool VisitFunctionNoProtoType(clang::FunctionNoProtoType *node) {
        return true;
    }

    bool VisitUnresolvedUsingType(clang::UnresolvedUsingType *node) {
        return true;
    }

    bool VisitParenType(clang::ParenType *node) {
        return true;
    }

    bool VisitTypedefType(clang::TypedefType *node) {
        return true;
    }

    bool VisitTypeOfExprType(clang::TypeOfExprType *node) {
        return true;
    }

    bool VisitTypeOfType(clang::TypeOfType *node) {
        return true;
    }

    bool VisitDecltypeType(clang::DecltypeType *node) {
        return true;
    }

    bool VisitUnaryTransformType(clang::UnaryTransformType *node) {
        return true;
    }

    bool VisitTagType(clang::TagType *node) {
        return true;
    }

    bool VisitRecordType(clang::RecordType *node) {
        return true;
    }

    bool VisitEnumType(clang::EnumType *node) {
        return true;
    }

    bool VisitElaboratedType(clang::ElaboratedType *node) {
        return true;
    }

    bool VisitAttributedType(clang::AttributedType *node) {
        return true;
    }

    bool VisitTemplateTypeParmType(clang::TemplateTypeParmType *node) {
        return true;
    }

    bool VisitSubstTemplateTypeParmType(clang::SubstTemplateTypeParmType *node) {
        return true;
    }

    bool VisitSubstTemplateTypeParmPackType(clang::SubstTemplateTypeParmPackType *node) {
        return true;
    }

    bool VisitTemplateSpecializationType(clang::TemplateSpecializationType *node) {
        return true;
    }

    bool VisitAutoType(clang::AutoType *node) {
        return true;
    }

    bool VisitInjectedClassNameType(clang::InjectedClassNameType *node) {
        return true;
    }

    bool VisitDependentNameType(clang::DependentNameType *node) {
        return true;
    }

    bool VisitDependentTemplateSpecializationType(clang::DependentTemplateSpecializationType *node) {
        return true;
    }

    bool VisitPackExpansionType(clang::PackExpansionType *node) {
        return true;
    }

    bool VisitObjCObjectType(clang::ObjCObjectType *node) {
        return true;
    }

    bool VisitObjCInterfaceType(clang::ObjCInterfaceType *node) {
        return true;
    }

    bool VisitObjCObjectPointerType(clang::ObjCObjectPointerType *node) {
        return true;
    }

    bool VisitAtomicType(clang::AtomicType *node) {
        return true;
    }

    bool VisitTypeLoc(clang::TypeLoc node) {
        return true;
    }

    bool VisitQualifiedTypeLoc(clang::QualifiedTypeLoc node) {
        return true;
    }

    bool VisitUnqualTypeLoc(clang::UnqualTypeLoc node) {
        return true;
    }

    bool VisitBuiltinTypeLoc(clang::BuiltinTypeLoc node) {
        return true;
    }

    bool VisitComplexTypeLoc(clang::ComplexTypeLoc node) {
        return true;
    }

    bool VisitPointerTypeLoc(clang::PointerTypeLoc node) {
        return true;
    }

    bool VisitBlockPointerTypeLoc(clang::BlockPointerTypeLoc node) {
        return true;
    }

    bool VisitReferenceTypeLoc(clang::ReferenceTypeLoc node) {
        return true;
    }

    bool VisitLValueReferenceTypeLoc(clang::LValueReferenceTypeLoc node) {
        return true;
    }

    bool VisitRValueReferenceTypeLoc(clang::RValueReferenceTypeLoc node) {
        return true;
    }

    bool VisitMemberPointerTypeLoc(clang::MemberPointerTypeLoc node) {
        return true;
    }

    bool VisitArrayTypeLoc(clang::ArrayTypeLoc node) {
        return true;
    }

    bool VisitConstantArrayTypeLoc(clang::ConstantArrayTypeLoc node) {
        return true;
    }

    bool VisitIncompleteArrayTypeLoc(clang::IncompleteArrayTypeLoc node) {
        return true;
    }

    bool VisitVariableArrayTypeLoc(clang::VariableArrayTypeLoc node) {
        return true;
    }

    bool VisitDependentSizedArrayTypeLoc(clang::DependentSizedArrayTypeLoc node) {
        return true;
    }

    bool VisitDependentSizedExtVectorTypeLoc(clang::DependentSizedExtVectorTypeLoc node) {
        return true;
    }

    bool VisitVectorTypeLoc(clang::VectorTypeLoc node) {
        return true;
    }

    bool VisitExtVectorTypeLoc(clang::ExtVectorTypeLoc node) {
        return true;
    }

    bool VisitFunctionTypeLoc(clang::FunctionTypeLoc node) {
        return true;
    }

    bool VisitFunctionProtoTypeLoc(clang::FunctionProtoTypeLoc node) {
        return true;
    }

    bool VisitFunctionNoProtoTypeLoc(clang::FunctionNoProtoTypeLoc node) {
        return true;
    }

    bool VisitUnresolvedUsingTypeLoc(clang::UnresolvedUsingTypeLoc node) {
        return true;
    }

    bool VisitParenTypeLoc(clang::ParenTypeLoc node) {
        return true;
    }

    bool VisitTypedefTypeLoc(clang::TypedefTypeLoc node) {
        return true;
    }

    bool VisitTypeOfExprTypeLoc(clang::TypeOfExprTypeLoc node) {
        return true;
    }

    bool VisitTypeOfTypeLoc(clang::TypeOfTypeLoc node) {
        return true;
    }

    bool VisitDecltypeTypeLoc(clang::DecltypeTypeLoc node) {
        return true;
    }

    bool VisitUnaryTransformTypeLoc(clang::UnaryTransformTypeLoc node) {
        return true;
    }

    bool VisitTagTypeLoc(clang::TagTypeLoc node) {
        return true;
    }

    bool VisitRecordTypeLoc(clang::RecordTypeLoc node) {
        return true;
    }

    bool VisitEnumTypeLoc(clang::EnumTypeLoc node) {
        return true;
    }

    bool VisitElaboratedTypeLoc(clang::ElaboratedTypeLoc node) {
        return true;
    }

    bool VisitAttributedTypeLoc(clang::AttributedTypeLoc node) {
        return true;
    }

    bool VisitTemplateTypeParmTypeLoc(clang::TemplateTypeParmTypeLoc node) {
        return true;
    }

    bool VisitSubstTemplateTypeParmTypeLoc(clang::SubstTemplateTypeParmTypeLoc node) {
        return true;
    }

    bool VisitSubstTemplateTypeParmPackTypeLoc(clang::SubstTemplateTypeParmPackTypeLoc node) {
        return true;
    }

    bool VisitTemplateSpecializationTypeLoc(clang::TemplateSpecializationTypeLoc node) {
        return true;
    }

    bool VisitAutoTypeLoc(clang::AutoTypeLoc node) {
        return true;
    }

    bool VisitInjectedClassNameTypeLoc(clang::InjectedClassNameTypeLoc node) {
        return true;
    }

    bool VisitDependentNameTypeLoc(clang::DependentNameTypeLoc node) {
        return true;
    }

    bool VisitDependentTemplateSpecializationTypeLoc(clang::DependentTemplateSpecializationTypeLoc node) {
        return true;
    }

    bool VisitPackExpansionTypeLoc(clang::PackExpansionTypeLoc node) {
        return true;
    }

    bool VisitObjCObjectTypeLoc(clang::ObjCObjectTypeLoc node) {
        return true;
    }

    bool VisitObjCInterfaceTypeLoc(clang::ObjCInterfaceTypeLoc node) {
        return true;
    }

    bool VisitObjCObjectPointerTypeLoc(clang::ObjCObjectPointerTypeLoc node) {
        return true;
    }

    bool VisitAtomicTypeLoc(clang::AtomicTypeLoc node) {
        return true;
    }

    bool VisitDecl(clang::Decl *node) {
        return true;
    }

    bool VisitAccessSpecDecl(clang::AccessSpecDecl *node) {
        return true;
    }

    bool VisitBlockDecl(clang::BlockDecl *node) {
        return true;
    }

    bool VisitCapturedDecl(clang::CapturedDecl *node) {
        return true;
    }

    bool VisitClassScopeFunctionSpecializationDecl(clang::ClassScopeFunctionSpecializationDecl *node) {
        return true;
    }

    bool VisitEmptyDecl(clang::EmptyDecl *node) {
        return true;
    }

    bool VisitFileScopeAsmDecl(clang::FileScopeAsmDecl *node) {
        return true;
    }

    bool VisitFriendDecl(clang::FriendDecl *node) {
        return true;
    }

    bool VisitFriendTemplateDecl(clang::FriendTemplateDecl *node) {
        return true;
    }

    bool VisitImportDecl(clang::ImportDecl *node) {
        return true;
    }

    bool VisitLinkageSpecDecl(clang::LinkageSpecDecl *node) {
        return true;
    }

    bool VisitNamedDecl(clang::NamedDecl *node) {
        return true;
    }

    bool VisitLabelDecl(clang::LabelDecl *node) {
        return true;
    }

    bool VisitNamespaceDecl(clang::NamespaceDecl *node) {
        return true;
    }

    bool VisitNamespaceAliasDecl(clang::NamespaceAliasDecl *node) {
        return true;
    }

    bool VisitObjCCompatibleAliasDecl(clang::ObjCCompatibleAliasDecl *node) {
        return true;
    }

    bool VisitObjCContainerDecl(clang::ObjCContainerDecl *node) {
        return true;
    }

    bool VisitObjCCategoryDecl(clang::ObjCCategoryDecl *node) {
        return true;
    }

    bool VisitObjCImplDecl(clang::ObjCImplDecl *node) {
        return true;
    }

    bool VisitObjCCategoryImplDecl(clang::ObjCCategoryImplDecl *node) {
        return true;
    }

    bool VisitObjCImplementationDecl(clang::ObjCImplementationDecl *node) {
        return true;
    }

    bool VisitObjCInterfaceDecl(clang::ObjCInterfaceDecl *node) {
        return true;
    }

    bool VisitObjCProtocolDecl(clang::ObjCProtocolDecl *node) {
        return true;
    }

    bool VisitObjCMethodDecl(clang::ObjCMethodDecl *node) {
        return true;
    }

    bool VisitObjCPropertyDecl(clang::ObjCPropertyDecl *node) {
        return true;
    }

    bool VisitTemplateDecl(clang::TemplateDecl *node) {
        return true;
    }

    bool VisitRedeclarableTemplateDecl(clang::RedeclarableTemplateDecl *node) {
        return true;
    }

    bool VisitClassTemplateDecl(clang::ClassTemplateDecl *node) {
        return true;
    }

    bool VisitFunctionTemplateDecl(clang::FunctionTemplateDecl *node) {
        return true;
    }

    bool VisitTypeAliasTemplateDecl(clang::TypeAliasTemplateDecl *node) {
        return true;
    }

    bool VisitTemplateTemplateParmDecl(clang::TemplateTemplateParmDecl *node) {
        return true;
    }

    bool VisitTypeDecl(clang::TypeDecl *node) {
        return true;
    }

    bool VisitTagDecl(clang::TagDecl *node) {
        return true;
    }

    bool VisitEnumDecl(clang::EnumDecl *node) {
        return true;
    }

    bool VisitRecordDecl(clang::RecordDecl *node) {
        return true;
    }

    bool VisitCXXRecordDecl(clang::CXXRecordDecl *node) {
        return true;
    }

    bool VisitClassTemplateSpecializationDecl(clang::ClassTemplateSpecializationDecl *node) {
        return true;
    }

    bool VisitClassTemplatePartialSpecializationDecl(clang::ClassTemplatePartialSpecializationDecl *node) {
        return true;
    }

    bool VisitTemplateTypeParmDecl(clang::TemplateTypeParmDecl *node) {
        return true;
    }

    bool VisitTypedefNameDecl(clang::TypedefNameDecl *node) {
        return true;
    }

    bool VisitTypeAliasDecl(clang::TypeAliasDecl *node) {
        return true;
    }

    bool VisitTypedefDecl(clang::TypedefDecl *node) {
        return true;
    }

    bool VisitUnresolvedUsingTypenameDecl(clang::UnresolvedUsingTypenameDecl *node) {
        return true;
    }

    bool VisitUsingDecl(clang::UsingDecl *node) {
        return true;
    }

    bool VisitUsingDirectiveDecl(clang::UsingDirectiveDecl *node) {
        return true;
    }

    bool VisitUsingShadowDecl(clang::UsingShadowDecl *node) {
        return true;
    }

    bool VisitValueDecl(clang::ValueDecl *node) {
        return true;
    }

    bool VisitDeclaratorDecl(clang::DeclaratorDecl *node) {
        return true;
    }

    bool VisitFieldDecl(clang::FieldDecl *node) {
        return true;
    }

    bool VisitObjCAtDefsFieldDecl(clang::ObjCAtDefsFieldDecl *node) {
        return true;
    }

    bool VisitObjCIvarDecl(clang::ObjCIvarDecl *node) {
        return true;
    }

    bool VisitFunctionDecl(clang::FunctionDecl *node) {
        return true;
    }

    bool VisitCXXMethodDecl(clang::CXXMethodDecl *node) {
        return true;
    }

    bool VisitCXXConstructorDecl(clang::CXXConstructorDecl *node) {
        return true;
    }

    bool VisitCXXConversionDecl(clang::CXXConversionDecl *node) {
        return true;
    }

    bool VisitCXXDestructorDecl(clang::CXXDestructorDecl *node) {
        return true;
    }

    bool VisitMSPropertyDecl(clang::MSPropertyDecl *node) {
        return true;
    }

    bool VisitNonTypeTemplateParmDecl(clang::NonTypeTemplateParmDecl *node) {
        return true;
    }

    bool VisitVarDecl(clang::VarDecl *node) {
        return true;
    }

    bool VisitImplicitParamDecl(clang::ImplicitParamDecl *node) {
        return true;
    }

    bool VisitParmVarDecl(clang::ParmVarDecl *node) {
        return true;
    }

    bool VisitEnumConstantDecl(clang::EnumConstantDecl *node) {
        return true;
    }

    bool VisitIndirectFieldDecl(clang::IndirectFieldDecl *node) {
        return true;
    }

    bool VisitUnresolvedUsingValueDecl(clang::UnresolvedUsingValueDecl *node) {
        return true;
    }

    bool VisitOMPThreadPrivateDecl(clang::OMPThreadPrivateDecl *node) {
        return true;
    }

    bool VisitObjCPropertyImplDecl(clang::ObjCPropertyImplDecl *node) {
        return true;
    }

    bool VisitStaticAssertDecl(clang::StaticAssertDecl *node) {
        return true;
    }

    bool VisitTranslationUnitDecl(clang::TranslationUnitDecl *node) {
        return true;
    }
#endif

void initialize_astVisitor() {
//  Defun(makeAstVisitor);

  {
    using namespace clbind;
#if 0
            package(AstToolingPkg) [
                class_<clang::tooling::ClangTool>("ClangTool",no_default_constructor)
                .def_constructor("make-ClangTool",constructor<const clang::tooling::CompilationDatabase&,llvm::ArrayRef<std::string> >())
                ];
#endif
  }
};
}
