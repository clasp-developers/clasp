/*
    File: clangTooling.cc
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
#include <clang/AST/DeclTemplate.h>
#include <clang/AST/DeclFriend.h>
#include <clang/AST/DeclOpenMP.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/StmtCXX.h>
#include <clang/AST/ExprObjC.h>
#include <clang/AST/StmtObjC.h>
#include <clang/AST/StmtOpenMP.h>
#include <clang/Basic/Version.h>
#include <clang/Tooling/JSONCompilationDatabase.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/AST/Comment.h>
#include <clang/AST/RecordLayout.h>
#include <clang/Tooling/Tooling.h>
#include <clang/Tooling/Refactoring.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendActions.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/Rewrite/Core/Rewriter.h>
#include <clang/Sema/Sema.h>
#include <clang/Lex/Lexer.h>
#include <clang/Lex/Preprocessor.h>
#include <clang/ASTMatchers/Dynamic/VariantValue.h>
#include <clang/ASTMatchers/ASTMatchFinder.h>
#include <clang/ASTMatchers/Dynamic/Diagnostics.h>
#include <clang/ASTMatchers/Dynamic/Parser.h>
#include <clasp/core/object.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/translators.h>
#include <clasp/core/array.h>
#include <clasp/core/arguments.h>
#include <clasp/clbind/clbind.h>
#include <clasp/llvmo/translators.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/asttooling/astExpose.h>
#include <clasp/asttooling/asttoolingPackage.h>
#include <clasp/asttooling/translators.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/wrappers.h>
//#include <clasp/asttooling/Diagnostics.h>
//#include <clasp/asttooling/Registry.h>
#include <clasp/asttooling/clangTooling.h>
#include <clasp/core/translators.h>

#ifdef USE_MPS
#define NAMESPACE_clbind_clang
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_clbind_clang
#endif

namespace translate {


template <>
struct from_object<clang::tooling::CommandLineArguments> {
  typedef clang::tooling::CommandLineArguments DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.notnilp()) {
      if (core::Cons_sp cargs = o.asOrNull<core::Cons_O>()) {
        core::List_sp args = cargs;
        _v.clear();
        for (auto cur : args) {
          core::String_sp s = gc::As<core::String_sp>(oCar(cur));
          _v.push_back(s->get_std_string());
        }
        return;
      } else if (core::Vector_sp vargs = o.asOrNull<core::Vector_O>()) {
        _v.clear();
        for (int i(0), iEnd(vargs->length()); i < iEnd; ++i) {
          core::String_sp s = gc::As<core::String_sp>(vargs->rowMajorAref(i));
          _v.push_back(s->get_std_string());
        }
        return;
      }
    }
    SIMPLE_ERROR(("Conversion of %s to clang::tooling::CommandLineArguments not supported yet") , _rep_(o));
  }
};


template <>
struct from_object<clang::tooling::ArgumentsAdjuster> {
  typedef clang::tooling::ArgumentsAdjuster DeclareType;
  //	clang::tooling::CommandLineArguments(const clang::tooling::CommandLineArguments&)> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    // printf("%s:%d Entered from_object<clang::tooling::ArgumentsAdjuster> with arg: %s@%p\n", __FILE__, __LINE__, _rep_(o).c_str(), (void*)o.tagged_());
    if (o.nilp()) {
      SIMPLE_ERROR(("You cannot pass nil as a function"));
    } else if (core::Function_sp func = o.asOrNull<core::Function_O>()) {
      this->_v = [func](const clang::tooling::CommandLineArguments &args, llvm::StringRef filename ) -> clang::tooling::CommandLineArguments {
			// Should resolve to vector<string>
          core::T_sp targs = translate::to_object<clang::tooling::CommandLineArguments>::convert(args);
          core::T_sp tfilename = translate::to_object<llvm::StringRef>::convert(filename);
          core::T_mv result = core::eval::funcall(func,targs,tfilename);;
          translate::from_object<const clang::tooling::CommandLineArguments&> cresult(result);
          return cresult._v;
      };
      return;
    } else if (clang::tooling::ArgumentsAdjuster *argAdj = gc::As<core::WrappedPointer_sp>(o)->cast<clang::tooling::ArgumentsAdjuster>()) {
      this->_v = *argAdj;
      return;
    }
    SIMPLE_ERROR(("Cannot convert %s into a clang::tooling::ArgumentsAdjuster") , _rep_(o));
  }
};
};


namespace asttooling {
/*! Many ASTMatchers like recordDecl() were renamed to cxxRecordDecl() with
messes with Clasp's name lispification.  lispify(cxxRecordDecl) --> CXX-RECORD-DECL
But the class that cxxRecordDecl() is supposed to match is CXXRECORD-DECL (lispify(CXXRecordDecl))
So I'll fix it here by converting names that start with "cxx" to start with "CXX"
Also fix up CUDA and RV.*/
DOCGROUP(clasp);
CL_DEFUN std::string ast_tooling__fix_matcher_name(const string& orig_name)
{
  if ( orig_name.substr(0,3) == "cxx") {
    stringstream fixed;
    fixed << "CXX" << orig_name.substr(3);
    return fixed.str();
  }
  if ( orig_name.substr(0,4) == "cuda") {
    stringstream fixed;
    fixed << "CUDA" << orig_name.substr(4);
    return fixed.str();
  }
  if ( orig_name.substr(0,2) == "rV") {
    stringstream fixed;
    fixed << "RV" << orig_name.substr(2);
    return fixed.str();
  }
  return orig_name;
}


DOCGROUP(clasp);
CL_DEFUN core::Symbol_sp ast_tooling__intern_matcher_keyword(const string& orig_name)
{
  core::Symbol_sp name = core::lispify_intern_keyword(ast_tooling__fix_matcher_name(orig_name));
  return name;
}

SYMBOL_EXPORT_SC_(AstToolingPkg,STARmatcher_namesSTAR);
void add_matcher_name(const string& name, core::Symbol_sp symbol)
{
  core::List_sp one = core::Cons_O::createList(symbol,core::SimpleBaseString_O::make(name));
  _sym_STARmatcher_namesSTAR->defparameter(core::Cons_O::create(one,_sym_STARmatcher_namesSTAR->symbolValue()));
}


void initialize_matchers() {

#define REGISTER_OVERLOADED_2(name) add_matcher_name(#name,ast_tooling__intern_matcher_keyword(#name))
#define REGISTER_MATCHER(name) add_matcher_name(#name,ast_tooling__intern_matcher_keyword(#name))

  _sym_STARmatcher_namesSTAR->defparameter(nil<core::T_O>());
  // These are copied from llvm38/tools/clang/lib/ASTMatchers/Dynamic/Registry.cpp
  // If there are changes to Registry.cpp - copy them here.
  REGISTER_OVERLOADED_2(callee);
  REGISTER_OVERLOADED_2(hasPrefix);
  REGISTER_OVERLOADED_2(hasType);
  REGISTER_OVERLOADED_2(isDerivedFrom);
  REGISTER_OVERLOADED_2(isSameOrDerivedFrom);
  REGISTER_OVERLOADED_2(loc);
  REGISTER_OVERLOADED_2(pointsTo);
  REGISTER_OVERLOADED_2(references);
  REGISTER_OVERLOADED_2(thisPointerType);

  REGISTER_MATCHER(accessSpecDecl);
  REGISTER_MATCHER(alignOfExpr);
  REGISTER_MATCHER(allOf);
  REGISTER_MATCHER(anyOf);
  REGISTER_MATCHER(anything);
  REGISTER_MATCHER(argumentCountIs);
  REGISTER_MATCHER(arraySubscriptExpr);
  REGISTER_MATCHER(arrayType);
  REGISTER_MATCHER(asmStmt);
  REGISTER_MATCHER(asString);
  REGISTER_MATCHER(atomicType);
  REGISTER_MATCHER(autoType);
  REGISTER_MATCHER(binaryOperator);
  REGISTER_MATCHER(blockPointerType);
  REGISTER_MATCHER(booleanType);
  REGISTER_MATCHER(breakStmt);
  REGISTER_MATCHER(builtinType);
  REGISTER_MATCHER(callExpr);
  REGISTER_MATCHER(caseStmt);
  REGISTER_MATCHER(castExpr);
  REGISTER_MATCHER(characterLiteral);
  REGISTER_MATCHER(classTemplateDecl);
  REGISTER_MATCHER(classTemplateSpecializationDecl);
  REGISTER_MATCHER(complexType);
  REGISTER_MATCHER(compoundLiteralExpr);
  REGISTER_MATCHER(compoundStmt);
  REGISTER_MATCHER(conditionalOperator);
  REGISTER_MATCHER(constantArrayType);
  REGISTER_MATCHER(containsDeclaration);
  REGISTER_MATCHER(continueStmt);
  REGISTER_MATCHER(cStyleCastExpr);
  REGISTER_MATCHER(cudaKernelCallExpr);
  REGISTER_MATCHER(cxxBindTemporaryExpr);
  REGISTER_MATCHER(cxxBoolLiteral);
  REGISTER_MATCHER(cxxCatchStmt);
  REGISTER_MATCHER(cxxConstCastExpr);
  REGISTER_MATCHER(cxxConstructExpr);
  REGISTER_MATCHER(cxxConstructorDecl);
  REGISTER_MATCHER(cxxConversionDecl);
  REGISTER_MATCHER(cxxCtorInitializer);
  REGISTER_MATCHER(cxxDefaultArgExpr);
  REGISTER_MATCHER(cxxDeleteExpr);
  REGISTER_MATCHER(cxxDestructorDecl);
  REGISTER_MATCHER(cxxDynamicCastExpr);
  REGISTER_MATCHER(cxxForRangeStmt);
  REGISTER_MATCHER(cxxFunctionalCastExpr);
  REGISTER_MATCHER(cxxMemberCallExpr);
  REGISTER_MATCHER(cxxMethodDecl);
  REGISTER_MATCHER(cxxNewExpr);
  REGISTER_MATCHER(cxxNullPtrLiteralExpr);
  REGISTER_MATCHER(cxxOperatorCallExpr);
  REGISTER_MATCHER(cxxRecordDecl);
  REGISTER_MATCHER(cxxReinterpretCastExpr);
  REGISTER_MATCHER(cxxStaticCastExpr);
  REGISTER_MATCHER(cxxTemporaryObjectExpr);
  REGISTER_MATCHER(cxxThisExpr);
  REGISTER_MATCHER(cxxThrowExpr);
  REGISTER_MATCHER(cxxTryStmt);
  REGISTER_MATCHER(cxxUnresolvedConstructExpr);
  REGISTER_MATCHER(decayedType);
  REGISTER_MATCHER(decl);
  REGISTER_MATCHER(declaratorDecl);
  REGISTER_MATCHER(declCountIs);
  REGISTER_MATCHER(declRefExpr);
  REGISTER_MATCHER(declStmt);
  REGISTER_MATCHER(defaultStmt);
  REGISTER_MATCHER(dependentSizedArrayType);
  REGISTER_MATCHER(doStmt);
  REGISTER_MATCHER(eachOf);
  REGISTER_MATCHER(elaboratedType);
  REGISTER_MATCHER(enumConstantDecl);
  REGISTER_MATCHER(enumDecl);
  REGISTER_MATCHER(equalsBoundNode);
  REGISTER_MATCHER(equalsIntegralValue);
  REGISTER_MATCHER(explicitCastExpr);
  REGISTER_MATCHER(expr);
  REGISTER_MATCHER(exprWithCleanups);
  REGISTER_MATCHER(fieldDecl);
  REGISTER_MATCHER(floatLiteral);
  REGISTER_MATCHER(forEach);
  REGISTER_MATCHER(forEachConstructorInitializer);
  REGISTER_MATCHER(forEachDescendant);
  REGISTER_MATCHER(forEachSwitchCase);
  REGISTER_MATCHER(forField);
  REGISTER_MATCHER(forStmt);
  REGISTER_MATCHER(friendDecl);
  REGISTER_MATCHER(functionDecl);
  REGISTER_MATCHER(functionTemplateDecl);
  REGISTER_MATCHER(functionType);
  REGISTER_MATCHER(gotoStmt);
  REGISTER_MATCHER(has);
  REGISTER_MATCHER(hasAncestor);
  REGISTER_MATCHER(hasAnyArgument);
  REGISTER_MATCHER(hasAnyConstructorInitializer);
  REGISTER_MATCHER(hasAnyParameter);
  REGISTER_MATCHER(hasAnySubstatement);
  REGISTER_MATCHER(hasAnyTemplateArgument);
  REGISTER_MATCHER(hasAnyUsingShadowDecl);
  REGISTER_MATCHER(hasArgument);
  REGISTER_MATCHER(hasArgumentOfType);
//  REGISTER_MATCHER(hasAttr);   // I can't support this matcher
  REGISTER_MATCHER(hasAutomaticStorageDuration);
  REGISTER_MATCHER(hasBase);
  REGISTER_MATCHER(hasBody);
  REGISTER_MATCHER(hasCanonicalType);
  REGISTER_MATCHER(hasCaseConstant);
  REGISTER_MATCHER(hasCondition);
  REGISTER_MATCHER(hasConditionVariableStatement);
  REGISTER_MATCHER(hasDecayedType);
  REGISTER_MATCHER(hasDeclaration);
  REGISTER_MATCHER(hasDeclContext);
  REGISTER_MATCHER(hasDeducedType);
  REGISTER_MATCHER(hasDescendant);
  REGISTER_MATCHER(hasDestinationType);
  REGISTER_MATCHER(hasEitherOperand);
  REGISTER_MATCHER(hasElementType);
  REGISTER_MATCHER(hasElse);
  REGISTER_MATCHER(hasFalseExpression);
  REGISTER_MATCHER(hasGlobalStorage);
  REGISTER_MATCHER(hasImplicitDestinationType);
  REGISTER_MATCHER(hasIncrement);
  REGISTER_MATCHER(hasIndex);
  REGISTER_MATCHER(hasInitializer);
  REGISTER_MATCHER(hasKeywordSelector);
  REGISTER_MATCHER(hasLHS);
  REGISTER_MATCHER(hasLocalQualifiers);
  REGISTER_MATCHER(hasLocalStorage);
  REGISTER_MATCHER(hasLoopInit);
  REGISTER_MATCHER(hasLoopVariable);
  REGISTER_MATCHER(hasMethod);
  REGISTER_MATCHER(hasName);
  REGISTER_MATCHER(hasNullSelector);
  REGISTER_MATCHER(hasObjectExpression);
  REGISTER_MATCHER(hasOperatorName);
  REGISTER_MATCHER(hasOverloadedOperatorName);
  REGISTER_MATCHER(hasParameter);
  REGISTER_MATCHER(hasParent);
  REGISTER_MATCHER(hasQualifier);
  REGISTER_MATCHER(hasRangeInit);
  REGISTER_MATCHER(hasReceiverType);
  REGISTER_MATCHER(hasRHS);
  REGISTER_MATCHER(hasSelector);
  REGISTER_MATCHER(hasSingleDecl);
  REGISTER_MATCHER(hasSize);
  REGISTER_MATCHER(hasSizeExpr);
  REGISTER_MATCHER(hasSourceExpression);
  REGISTER_MATCHER(hasStaticStorageDuration);
  REGISTER_MATCHER(hasTargetDecl);
  REGISTER_MATCHER(hasTemplateArgument);
  REGISTER_MATCHER(hasThen);
  REGISTER_MATCHER(hasThreadStorageDuration);
  REGISTER_MATCHER(hasTrueExpression);
  REGISTER_MATCHER(hasTypeLoc);
  REGISTER_MATCHER(hasUnaryOperand);
  REGISTER_MATCHER(hasUnarySelector);
  REGISTER_MATCHER(hasValueType);
  REGISTER_MATCHER(ifStmt);
  REGISTER_MATCHER(ignoringImpCasts);
  REGISTER_MATCHER(ignoringParenCasts);
  REGISTER_MATCHER(ignoringParenImpCasts);
  REGISTER_MATCHER(implicitCastExpr);
  REGISTER_MATCHER(incompleteArrayType);
  REGISTER_MATCHER(initListExpr);
  REGISTER_MATCHER(injectedClassNameType);
  REGISTER_MATCHER(innerType);
  REGISTER_MATCHER(integerLiteral);
  REGISTER_MATCHER(isAnonymous);
  REGISTER_MATCHER(isArrow);
  REGISTER_MATCHER(isBaseInitializer);
  REGISTER_MATCHER(isCatchAll);
  REGISTER_MATCHER(isClass);
  REGISTER_MATCHER(isConst);
  REGISTER_MATCHER(isConstQualified);
  REGISTER_MATCHER(isCopyConstructor);
  REGISTER_MATCHER(isDefaultConstructor);
  REGISTER_MATCHER(isDefinition);
  REGISTER_MATCHER(isDeleted);
  REGISTER_MATCHER(isExceptionVariable);
  REGISTER_MATCHER(isExplicit);
  REGISTER_MATCHER(isExplicitTemplateSpecialization);
  REGISTER_MATCHER(isExpr);
  REGISTER_MATCHER(isExternC);
  REGISTER_MATCHER(isFinal);
  REGISTER_MATCHER(isInline);
  REGISTER_MATCHER(isImplicit);
  REGISTER_MATCHER(isExpansionInFileMatching);
  REGISTER_MATCHER(isExpansionInMainFile);
  REGISTER_MATCHER(isInstantiated);
  REGISTER_MATCHER(isExpansionInSystemHeader);
  REGISTER_MATCHER(isInteger);
  REGISTER_MATCHER(isIntegral);
  REGISTER_MATCHER(isInTemplateInstantiation);
  REGISTER_MATCHER(isListInitialization);
  REGISTER_MATCHER(isMemberInitializer);
  REGISTER_MATCHER(isMoveConstructor);
  REGISTER_MATCHER(isNoThrow);
  REGISTER_MATCHER(isOverride);
  REGISTER_MATCHER(isPrivate);
  REGISTER_MATCHER(isProtected);
  REGISTER_MATCHER(isPublic);
  REGISTER_MATCHER(isPure);
  REGISTER_MATCHER(isStruct);
  REGISTER_MATCHER(isTemplateInstantiation);
  REGISTER_MATCHER(isUnion);
  REGISTER_MATCHER(isVariadic);
  REGISTER_MATCHER(isVirtual);
  REGISTER_MATCHER(isVolatileQualified);
  REGISTER_MATCHER(isWritten);
  REGISTER_MATCHER(labelStmt);
  REGISTER_MATCHER(lambdaExpr);
  REGISTER_MATCHER(lValueReferenceType);
  REGISTER_MATCHER(matchesName);
  REGISTER_MATCHER(matchesSelector);
  REGISTER_MATCHER(materializeTemporaryExpr);
  REGISTER_MATCHER(member);
  REGISTER_MATCHER(memberExpr);
  REGISTER_MATCHER(memberPointerType);
  REGISTER_MATCHER(namedDecl);
  REGISTER_MATCHER(namespaceAliasDecl);
  REGISTER_MATCHER(namespaceDecl);
  REGISTER_MATCHER(namesType);
  REGISTER_MATCHER(nestedNameSpecifier);
  REGISTER_MATCHER(nestedNameSpecifierLoc);
  REGISTER_MATCHER(nullStmt);
  REGISTER_MATCHER(numSelectorArgs);
  REGISTER_MATCHER(ofClass);
  REGISTER_MATCHER(objcInterfaceDecl);
  REGISTER_MATCHER(objcMessageExpr);
  REGISTER_MATCHER(objcObjectPointerType);
  REGISTER_MATCHER(on);
  REGISTER_MATCHER(onImplicitObjectArgument);
  REGISTER_MATCHER(parameterCountIs);
  REGISTER_MATCHER(parenType);
  REGISTER_MATCHER(parmVarDecl);
  REGISTER_MATCHER(pointee);
  REGISTER_MATCHER(pointerType);
  REGISTER_MATCHER(qualType);
  REGISTER_MATCHER(recordDecl);
  REGISTER_MATCHER(recordType);
  REGISTER_MATCHER(referenceType);
  REGISTER_MATCHER(refersToDeclaration);
  REGISTER_MATCHER(refersToIntegralType);
  REGISTER_MATCHER(refersToType);
  REGISTER_MATCHER(returns);
  REGISTER_MATCHER(returnStmt);
  REGISTER_MATCHER(rValueReferenceType);
  REGISTER_MATCHER(sizeOfExpr);
  REGISTER_MATCHER(specifiesNamespace);
  REGISTER_MATCHER(specifiesType);
  REGISTER_MATCHER(specifiesTypeLoc);
  REGISTER_MATCHER(statementCountIs);
  REGISTER_MATCHER(staticAssertDecl);
  REGISTER_MATCHER(stmt);
  REGISTER_MATCHER(stringLiteral);
  REGISTER_MATCHER(substNonTypeTemplateParmExpr);
  REGISTER_MATCHER(substTemplateTypeParmType);
  REGISTER_MATCHER(switchCase);
  REGISTER_MATCHER(switchStmt);
  REGISTER_MATCHER(templateArgument);
  REGISTER_MATCHER(templateArgumentCountIs);
  REGISTER_MATCHER(templateSpecializationType);
  REGISTER_MATCHER(templateTypeParmType);
  REGISTER_MATCHER(throughUsingDecl);
  REGISTER_MATCHER(to);
  REGISTER_MATCHER(translationUnitDecl);
  REGISTER_MATCHER(type);
  REGISTER_MATCHER(typedefDecl);
  REGISTER_MATCHER(typedefType);
  REGISTER_MATCHER(typeLoc);
  REGISTER_MATCHER(unaryExprOrTypeTraitExpr);
  REGISTER_MATCHER(unaryOperator);
  REGISTER_MATCHER(unaryTransformType);
  REGISTER_MATCHER(unless);
  REGISTER_MATCHER(unresolvedUsingTypenameDecl);
  REGISTER_MATCHER(unresolvedUsingValueDecl);
  REGISTER_MATCHER(userDefinedLiteral);
  REGISTER_MATCHER(usingDecl);
  REGISTER_MATCHER(usingDirectiveDecl);
  REGISTER_MATCHER(valueDecl);
  REGISTER_MATCHER(varDecl);
  REGISTER_MATCHER(variableArrayType);
  REGISTER_MATCHER(voidType);
  REGISTER_MATCHER(whileStmt);
  REGISTER_MATCHER(withInitializer);

}


};







namespace asttooling {

using namespace clbind;

SYMBOL_EXPORT_SC_(AstToolingPkg, ArgumentsAdjusterAdjust);
SYMBOL_EXPORT_SC_(AstToolingPkg, bind);
SYMBOL_EXPORT_SC_(AstToolingPkg, VisitStmt);
SYMBOL_EXPORT_SC_(AstToolingPkg, VisitDecl);
SYMBOL_EXPORT_SC_(AstToolingPkg, VisitType);
SYMBOL_EXPORT_SC_(AstToolingPkg, HandleTranslationUnit);
SYMBOL_EXPORT_SC_(AstToolingPkg, CreateASTConsumer);
SYMBOL_EXPORT_SC_(AstToolingPkg, run);
SYMBOL_EXPORT_SC_(AstToolingPkg, create);
SYMBOL_EXPORT_SC_(AstToolingPkg, onStartOfTranslationUnit);
SYMBOL_EXPORT_SC_(AstToolingPkg, onEndOfTranslationUnit);
};

namespace asttooling {

DOCGROUP(clasp);
CL_DEFUN core::T_sp ast_tooling__clangVersionString() {
  core::T_sp version = core::SimpleBaseString_O::make(CLANG_VERSION_STRING);
  return version;
};

core::T_sp ast_tooling__getSingleMatcher(core::T_sp variantMatcher) {
  clang::ast_matchers::dynamic::VariantMatcher *vp = gc::As<core::WrappedPointer_sp>(variantMatcher)->cast<clang::ast_matchers::dynamic::VariantMatcher>();
  llvm::Optional<clang::ast_matchers::internal::DynTypedMatcher> dtm = vp->getSingleMatcher();
  if (dtm.hasValue()) {
    return clbind::Wrapper<clang::ast_matchers::internal::DynTypedMatcher>::make_wrapper(*dtm, reg::registered_class<clang::ast_matchers::internal::DynTypedMatcher>::id);
  }
  return nil<core::T_O>();
};

core::HashTable_sp ast_tooling__IDToNodeMap(core::T_sp bn) {
  if (const clang::ast_matchers::BoundNodes *boundNodes = gc::As<core::WrappedPointer_sp>(bn)->cast<const clang::ast_matchers::BoundNodes>()) {
    core::HashTable_sp ht = core::HashTable_O::create(::cl::_sym_eq);
    const clang::ast_matchers::BoundNodes::IDToNodeMap &nodemap = boundNodes->getMap();
    clang::ast_matchers::BoundNodes::IDToNodeMap::const_iterator it;
    for (it = nodemap.begin(); it != nodemap.end(); it++) {
      const string &key = it->first;
      const clang::DynTypedNode &dtn = it->second;
      core::T_sp value;
      if (const clang::Decl *decl = dtn.get<clang::Decl>()) {
        value = mostDerivedDecl(decl);
      } else if (const clang::Stmt *stmt = dtn.get<clang::Stmt>()) {
        value = mostDerivedStmt(stmt);
      } else if (const clang::Type *type = dtn.get<clang::Type>()) {
        value = mostDerivedType(type);
      } else if (const clang::QualType *qtype = dtn.get<clang::QualType>()) {
        value = translate::to_object<clang::QualType>::convert(*qtype);
      } else if (const clang::TypeLoc *typeLocType = dtn.get<clang::TypeLoc>()) {
        value = translate::to_object<clang::TypeLoc>::convert(*typeLocType);
      } else {
        SIMPLE_ERROR(("%s:%d Handle boxing of other node types in ast_tooling__IDToNodeMap") , __FILE__ , __LINE__);
      }
      ht->hash_table_setf_gethash(_lisp->internKeyword(key), value);
    }
    return ht;
  }
  SIMPLE_ERROR(("Wrong argument type for IDToNodeMap"));
}

void ast_tooling__match(core::T_sp tmatchFinder, core::T_sp tnode, core::T_sp tastContext) {
  clang::ast_matchers::MatchFinder *matchFinder = gc::As<core::WrappedPointer_sp>(tmatchFinder)->cast<clang::ast_matchers::MatchFinder>();
  clang::ASTContext *astContext = gc::As<core::WrappedPointer_sp>(tastContext)->cast<clang::ASTContext>();
  core::WrappedPointer_sp wp_node = gc::As<core::WrappedPointer_sp>(tnode);
  if (clang::Decl *decl = wp_node->castOrNull<clang::Decl>()) {
    matchFinder->match(*decl, *astContext);
  } else if (clang::Stmt *stmt = wp_node->castOrNull<clang::Stmt>()) {
    matchFinder->match(*stmt, *astContext);
  } else if (clang::Type *type = wp_node->castOrNull<clang::Type>()) {
    matchFinder->match(*type, *astContext);
  } else if (clang::QualType *qtype = wp_node->castOrNull<clang::QualType>()) {
    matchFinder->match(*qtype, *astContext);
  } else if (clang::TypeLoc *typeloc = wp_node->castOrNull<clang::TypeLoc>()) {
    matchFinder->match(*typeloc, *astContext);
  } else {
    SIMPLE_ERROR(("%s:%d Handle unboxing of other node types in ast_tooling__match") , __FILE__ , __LINE__);
  }
};

#define ARGS_ast_tooling__newFrontendActionFactory "(consumer-factory)"
#define DECL_ast_tooling__newFrontendActionFactory ""
#define DOCS_ast_tooling__newFrontendActionFactory "newFrontendActionFactory"
core::T_sp ast_tooling__newFrontendActionFactory(core::T_sp consumerFactory) {
  if (clang::ast_matchers::MatchFinder *matchFinder = gc::As<core::WrappedPointer_sp>(consumerFactory)->cast<clang::ast_matchers::MatchFinder>()) {
    typedef clbind::Wrapper<clang::tooling::FrontendActionFactory, std::unique_ptr<clang::tooling::FrontendActionFactory>> wrapped_type;
    std::unique_ptr<clang::tooling::FrontendActionFactory> val = clang::tooling::newFrontendActionFactory(matchFinder);
#if 0
	    clang::tooling::FrontendActionFactory* fafptr = val.release();
	    gctools::smart_ptr<wrapped_type> sp(wrapped_type::make_wrapper(fafptr,reg::registered_class<clang::tooling::FrontendActionFactory>::id));
#else
    gctools::smart_ptr<wrapped_type> sp(wrapped_type::make_wrapper(std::move(val), reg::registered_class<clang::tooling::FrontendActionFactory>::id));
#endif
    return sp;
  }
  SIMPLE_ERROR(("Implement newFrontendActionFactory for %s") , _rep_(consumerFactory));
};

core::List_sp ast_tooling__ReplacementsAsList(clang::tooling::RefactoringTool& refactoringTool ) {
  map<std::string,clang::tooling::Replacements>& mapReplacements = refactoringTool.getReplacements();
  ql::list ll;
  for ( auto pair : mapReplacements ) {
    for ( auto rep : pair.second ) {
      ll << core::SimpleBaseString_O::make(rep.toString());
    }
  }
  return ll.result();
}

void ast_tooling__RefactoringToolReplacementsAdd(clang::tooling::RefactoringTool& refactoringTool, const clang::tooling::Replacement& replacement) {
  auto filePath = replacement.getFilePath();
//  printf("%s:%d:%s filePath = %s\n", __FILE__, __LINE__, __FUNCTION__, filePath.str().c_str() );
  map<std::string,clang::tooling::Replacements>& mapReplacements = refactoringTool.getReplacements();
//  printf("%s:%d:%s map size = %lu\n", __FILE__, __LINE__, __FUNCTION__, mapReplacements.size() );
  auto ii = mapReplacements.find(filePath.str());
  if ( ii == mapReplacements.end() ) {
    clang::tooling::Replacements repls;
    auto res = repls.add(replacement);
    if (res) {
      SIMPLE_ERROR(("Error adding replacement"));
    }
    mapReplacements[filePath.str()] =repls;
  } else {
    auto res = ii->second.add(replacement);
    if (res) {
      SIMPLE_ERROR(("Error adding replacement to existing replacements"));
    }
  }
#if 0  
  llvm::Error err = replacements.add(one);
  if (err) {
    return false;
  }
  return true;
#endif
}
#if 0

#define ARGS_ast_tooling__deduplicate "(replacements)"
#define DECL_ast_tooling__deduplicate ""
#define DOCS_ast_tooling__deduplicate "deduplicate wraps and lispifys clang::tooling::deduplicate - it takes a Cons of replacements and returns (values replacements overlapping-ranges)"
DOCGROUP(clasp);
CL_DEFUN core::T_mv ast_tooling__deduplicate(core::List_sp replacements) {
  core::List_sp creps = replacements;
  vector<clang::tooling::Replacement> vreps;
  for (; creps.notnilp(); creps = oCdr(creps)) {
    core::T_sp one = oCar(creps);
    clang::tooling::Replacement oneRep = *(gc::As<core::WrappedPointer_sp>(one)->cast<clang::tooling::Replacement>());
    vreps.push_back(oneRep);
  }
  vector<clang::tooling::Range> vranges;
  clang::tooling::deduplicate(vreps, vranges);
  core::Cons_sp firstRep = core::Cons_O::create(nil<core::T_O>(),nil<core::T_O>());
  core::Cons_sp curRep = firstRep;
  for (auto i : vreps) {
    clang::tooling::Replacement *rp = new clang::tooling::Replacement(i);
    core::T_sp wrapRep = clbind::Wrapper<clang::tooling::Replacement, std::unique_ptr<clang::tooling::Replacement>>::create(rp, reg::registered_class<clang::tooling::Replacement>::id);
    core::Cons_sp oneRepCons = core::Cons_O::create(wrapRep);
    curRep->setCdr(oneRepCons);
    curRep = oneRepCons;
  }
  core::Cons_sp firstRang = core::Cons_O::create(nil<core::T_O>(),nil<core::T_O>());
  core::Cons_sp curRang = firstRang;
  for (auto j : vranges) {
    // Why does Range not have a Copy constructor?????
    clang::tooling::Range *rp = new clang::tooling::Range(j); //i.getOffset(),i.getLength());
    core::T_sp wrapRang = clbind::Wrapper<clang::tooling::Range, std::unique_ptr<clang::tooling::Range>>::make_wrapper(rp, reg::registered_class<clang::tooling::Range>::id);
    core::Cons_sp oneRangCons = core::Cons_O::create(wrapRang);
    curRang->setCdr(oneRangCons);
    curRang = oneRangCons;
  }
  return Values(oCdr(firstRep), oCdr(firstRang));
}
#endif




#if 1
DOCGROUP(clasp);
CL_DEFUN void ast_tooling__testDerivable(clang::ast_matchers::MatchFinder::MatchCallback *ptr) {
  printf("%s:%d - got DerivableMatchCallback object --> %p\n", __FILE__, __LINE__, ptr);
  ptr->onEndOfTranslationUnit();
};
#endif
};


namespace asttooling {

/*Return the field offset in bits */
size_t getFieldOffset(clang::ASTContext* context, clang::RecordDecl* record, size_t fieldIndex)
{
  const clang::Type* type = record->getTypeForDecl();
  if ( type->isDependentType() ) return 0;
  const clang::ASTRecordLayout& layout = context->getASTRecordLayout(record);
//  printf("getFieldOffset context = %p record = %p(%s) fieldIndex = %" PRu "\n", context, record, record->getNameAsString().c_str(), fieldIndex );
//  printf("  layout = %p\n", &layout );
  size_t offset = layout.getFieldOffset(fieldIndex);
//  printf("Returning offset=%" PRu "\n", offset);
  return offset;
}

size_t getRecordSize(clang::ASTContext* context, clang::RecordDecl* record)
{
  const clang::Type* type = record->getTypeForDecl();
  if ( type->isDependentType() ) return 0;
//  printf("getRecordSize context = %p record = %p(%s)\n", context, record, record->getNameAsString().c_str() );
  const clang::ASTRecordLayout& layout = context->getASTRecordLayout(record);
//  printf("  layout = %p\n", &layout );
  size_t size= layout.getSize().getQuantity();
//  printf("Returning size=%" PRu "\n", size);
  return size;
}



};

SYMBOL_EXPORT_SC_(KeywordPkg,windows);
SYMBOL_EXPORT_SC_(KeywordPkg,gnu);
SYMBOL_EXPORT_SC_(KeywordPkg,auto_detect);

namespace translate {
template <>
struct from_object<clang::tooling::JSONCommandLineSyntax> {
  typedef clang::tooling::JSONCommandLineSyntax DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o==kw::_sym_windows) {
      this->_v = clang::tooling::JSONCommandLineSyntax::Windows;
      return;
    } else if (o==kw::_sym_gnu) {
      this->_v = clang::tooling::JSONCommandLineSyntax::Gnu;
      return;
    } else if (o==kw::_sym_auto_detect) {
      this->_v = clang::tooling::JSONCommandLineSyntax::AutoDetect;
      return;
    }
    SIMPLE_ERROR(("syntax parameter must be one of :auto-detect, :gnu, or :windows - was passed %s") , _rep_(o));
  };
};
};




namespace asttooling {


#if 0
    class_<clang::SourceLocation> cl_ag(m,"SourceLocation");
    cl_ag.def("isFileID", &clang::SourceLocation::isFileID)
        .def("printToString", &clang::SourceLocation::printToString);
    class_<clang::PresumedLoc> cl_ah(m,"PresumedLoc");
    cl_ah.def("isValid", &clang::PresumedLoc::isValid)
        .def("isInvalid", &clang::PresumedLoc::isInvalid)
        .def("PresumedLoc-getFilename", &clang::PresumedLoc::getFilename)
        .def("getLine", &clang::PresumedLoc::getLine)
        .def("getColumn", &clang::PresumedLoc::getColumn)
        .def("getIncludeLoc", &clang::PresumedLoc::getIncludeLoc);
#endif

DOCGROUP(clasp);
CL_DEFUN bool ast_tooling__isFileID(const clang::SourceLocation& ploc) {
  return ploc.isValid();
}

DOCGROUP(clasp);
CL_DEFUN std::string ast_tooling__printToString(const clang::SourceLocation& ploc,const clang::SourceManager &SM) {
  return ploc.printToString(SM);
}


DOCGROUP(clasp);
CL_DEFUN bool ast_tooling__isValid(const clang::PresumedLoc& ploc) {
  return ploc.isValid();
}

DOCGROUP(clasp);
CL_DEFUN bool ast_tooling__isInvalid(const clang::PresumedLoc& ploc) {
  return ploc.isInvalid();
}

DOCGROUP(clasp);
CL_DEFUN std::string ast_tooling__PresumedLoc_getFilename(const clang::PresumedLoc& ploc) {
  return ploc.getFilename();
}
DOCGROUP(clasp);
CL_DEFUN unsigned  ast_tooling__getLine(const clang::PresumedLoc& ploc) {
  return ploc.getLine();
}

DOCGROUP(clasp);
CL_DEFUN unsigned ast_tooling__getColumn(const clang::PresumedLoc& ploc) {
  return ploc.getColumn();
}

DOCGROUP(clasp);
CL_DEFUN clang::SourceLocation ast_tooling__getIncludeLoc(const clang::PresumedLoc& ploc) {
  return ploc.getIncludeLoc();
}



};
namespace asttooling {
DOCGROUP(clasp);
CL_DEFUN core::T_mv ast_tooling__wrapped_JSONCompilationDatabase_loadFromFile(core::T_sp FilePath, core::Symbol_sp ssyntax ) {
  clang::tooling::JSONCommandLineSyntax syntax = translate::from_object<clang::tooling::JSONCommandLineSyntax>(ssyntax)._v;
  std::string ErrorMessage;
  std::unique_ptr<clang::tooling::JSONCompilationDatabase> result = clang::tooling::JSONCompilationDatabase::loadFromFile(gc::As<core::String_sp>(FilePath)->get_std_string(),ErrorMessage,syntax);
  return Values(translate::to_object<clang::tooling::JSONCompilationDatabase*,translate::adopt_pointer>::convert(result.release()), core::SimpleBaseString_O::make(ErrorMessage));
}



void initialize_clangTooling() {

  initialize_matchers();
  // overloaded functions that had trouble resolving
  clang::ASTContext &(clang::ASTUnit::*clang_ASTUnit_getASTContext)() = &clang::ASTUnit::getASTContext;
  {
    // AST-TOOLING, pkg must be in its own scope
    package_ pkg(AstToolingPkg, {}, {});
    scope_& m = pkg.scope();
    class_<clang::tooling::CompilationDatabase>(m,"CompilationDatabase")
        .def("getAllFiles", &clang::tooling::CompilationDatabase::getAllFiles)
        .def("getCompileCommands", &clang::tooling::CompilationDatabase::getCompileCommands)
        .def("getAllCompileCommands", &clang::tooling::CompilationDatabase::getAllCompileCommands);
    class_<clang::tooling::JSONCompilationDatabase, bases<clang::tooling::CompilationDatabase>> cljcd(m,"JSONCompilationDatabase");
    /*This was used when exposing the original clang function, the wrapped one doesn't have a second string parameter */
    m.def("getFieldOffset",&getFieldOffset);
    m.def("getRecordSize",&getRecordSize);
// RESTOREME   m.def("JSONCompilationDatabase-loadFromFile", &clang::tooling::JSONCompilationDatabase::loadFromFile, adopt<result>() ,outValue<1>());
    class_<clang::ASTConsumer> cl_aa(m,"Clang-ASTConsumer");
    class_<clang::LangOptions> cl_ab(m,"LangOptions");
    class_<clang::Lexer> cl_ac(m,"Lexer");
    class_<clang::Preprocessor> cl_ad(m,"Preprocessor");
    class_<clang::ASTContext> cl_ae(m,"ASTContext");
    cl_ae.def("getTranslationUnitDecl", &clang::ASTContext::getTranslationUnitDecl)
        .def("getLangOpts", &clang::ASTContext::getLangOpts)
        .def("getCommentForDecl", &clang::ASTContext::getCommentForDecl)
        .def("getASTRecordLayout",&clang::ASTContext::getASTRecordLayout);
  
    class_<clang::SourceManager> cl_af(m,"SourceManager");
    cl_af.def("getPresumedLoc", &clang::SourceManager::getPresumedLoc, "((self ast-tooling:source-manager) source-location &optional (use-line-directives t))"_ll)
        .def("getFilename", &clang::SourceManager::getFilename)
        .def("getExpansionLoc", &clang::SourceManager::getExpansionLoc)
        .def("getExpansionLineNumber", &clang::SourceManager::getExpansionLineNumber, pureOutValue<2>())
        .def("getExpansionColumnNumber", &clang::SourceManager::getExpansionColumnNumber, pureOutValue<2>())
        .def("getSpellingLoc", &clang::SourceManager::getSpellingLoc)
        .def("getSpellingLineNumber", &clang::SourceManager::getSpellingLineNumber, pureOutValue<2>())
        .def("getSpellingColumnNumber", &clang::SourceManager::getSpellingColumnNumber, pureOutValue<2>())
        .def("getPresumedLineNumber", &clang::SourceManager::getPresumedLineNumber, pureOutValue<2>())
        .def("getPresumedColumnNumber", &clang::SourceManager::getPresumedColumnNumber, pureOutValue<2>());
#if 0 
    class_<clang::SourceLocation> cl_ag(m,"SourceLocation");
    cl_ag.def("isFileID", &clang::SourceLocation::isFileID)
        .def("printToString", &clang::SourceLocation::printToString);
    class_<clang::PresumedLoc> cl_ah(m,"PresumedLoc");
    cl_ah.def("isValid", &clang::PresumedLoc::isValid)
        .def("isInvalid", &clang::PresumedLoc::isInvalid)
        .def("PresumedLoc-getFilename", &clang::PresumedLoc::getFilename)
        .def("getLine", &clang::PresumedLoc::getLine)
        .def("getColumn", &clang::PresumedLoc::getColumn)
        .def("getIncludeLoc", &clang::PresumedLoc::getIncludeLoc);
#endif
    class_<clang::SourceRange> cl_ai(m,"SourceRange");
    cl_ai.def("getBegin", &clang::SourceRange::getBegin)
        .def("getEnd", &clang::SourceRange::getEnd);
    class_<clang::CharSourceRange> cl_aj(m,"CharSourceRange");
    cl_aj.def("isTokenRange",&clang::CharSourceRange::isTokenRange);
    cl_aj.def("isCharRange",&clang::CharSourceRange::isTokenRange);
    // Create a CharSourceRange from a pair of begin/end SourceLocations that contains a TokenRange
    m.def("newCharSourceRange-getTokenRange",
          (clang::CharSourceRange (*)(clang::SourceLocation, clang::SourceLocation)) & clang::CharSourceRange::getTokenRange);
    // Create a CharSourceRange from a pair of begin/end SourceLocations that contains a CharRange
    m.def("newCharSourceRange-getCharRange",
          (clang::CharSourceRange (*)(clang::SourceLocation, clang::SourceLocation)) & clang::CharSourceRange::getCharRange);
    // Create a CharSourceRange from a SourceRange that contains a TokenRange
    m.def("newCharSourceRange-getTokenRange-SourceRange",
          (clang::CharSourceRange (*)(clang::SourceRange)) & clang::CharSourceRange::getTokenRange);
    // Create a CharSourceRange from a SourceRange that contains a CharRange
    m.def("newCharSourceRange-getCharRange-SourceRange",
          (clang::CharSourceRange (*)(clang::SourceRange)) & clang::CharSourceRange::getCharRange);
    class_<clang::CompilerInstance>(m,"CompilerInstance")
        .def_constructor("make-CompilerInstance",constructor<>())
        .def("getASTContext", &clang::CompilerInstance::getASTContext);
    class_<clang::FrontendAction> cl_ak(m,"FrontendAction");
    class_<clang::ASTFrontendAction, clang::FrontendAction> cl_al(m,"Clang-ASTFrontendAction");
    class_<clang::SyntaxOnlyAction, clang::ASTFrontendAction> cl_am(m,"Clang-SyntaxOnlyAction");
    derivable_class_<DerivableASTFrontendAction, clang::ASTFrontendAction> dc_a(m,"ASTFrontendAction",create_default_constructor);
    dc_a.def("CreateASTConsumer", &DerivableASTFrontendAction::CreateASTConsumer, adopt<result>());
    class_<clang::tooling::ClangTool> cc_a(m,"ClangTool");
    cc_a.def_constructor("newClangTool", constructor<const clang::tooling::CompilationDatabase &, llvm::ArrayRef<std::string>>())
        .def("clearArgumentsAdjusters", &clang::tooling::ClangTool::clearArgumentsAdjusters)
        //            .  def("addArgumentsAdjuster",&clang::tooling::ClangTool::addArgumentsAdjuster)
        .def("appendArgumentsAdjuster", &clang::tooling::ClangTool::appendArgumentsAdjuster)
        .def("clangToolRun", &clang::tooling::ClangTool::run);
    m.def("buildASTs", +[](clang::tooling::ClangTool& tool ){
          std::vector<std::unique_ptr<clang::ASTUnit>> ASTs;
          tool.buildASTs(ASTs);
          ql::list ll;
          for (int i(0), iEnd(ASTs.size()); i < iEnd; ++i) {
            ll << clbind::Wrapper<clang::ASTUnit, std::unique_ptr<clang::ASTUnit>>::make_wrapper(std::move(ASTs[i]), reg::registered_class<clang::ASTUnit>::id);
          }
          return ll.result();
        });
    class_<clang::tooling::Replacement>(m,"Replacement")
        .def_constructor("newReplacement", constructor<clang::SourceManager &, const clang::CharSourceRange &, llvm::StringRef>())
        .def("toString", &clang::tooling::Replacement::toString)
        .def("getFilePath", &clang::tooling::Replacement::getFilePath)
        .def("replacement-apply", &clang::tooling::Replacement::apply);
    class_<clang::tooling::Range> cl_an(m,"Range");

    m.def("refactoring-tool-replacements-add", &ast_tooling__RefactoringToolReplacementsAdd);
    class_<clang::tooling::Replacements> cl_ao(m,"Replacements");
    m.def("replacements-as-list", &ast_tooling__ReplacementsAsList);
    class_<clang::tooling::RefactoringTool, clang::tooling::ClangTool> cl_ap(m,"RefactoringTool");
    cl_ap.def_constructor("newRefactoringTool", constructor<const clang::tooling::CompilationDatabase &, llvm::ArrayRef<std::string>>())
//        .def("getReplacements", &clang::tooling::RefactoringTool::getReplacements)
        .def("applyAllReplacements", &clang::tooling::RefactoringTool::applyAllReplacements)
        .def("runAndSave", &clang::tooling::RefactoringTool::runAndSave);
    class_<clang::Rewriter> cl_aq(m,"Rewriter");
    cl_aq.def_constructor("makeRewriter", constructor<clang::SourceManager &, const clang::LangOptions &>());
    class_<clang::ASTUnit> cl_ar(m,"ASTUnit");
    cl_ar.def("getASTContext", clang_ASTUnit_getASTContext); // (clang::ASTContext&(*)())&clang::ASTUnit::getASTContext)
    derivable_class_<DerivableSyntaxOnlyAction, clang::SyntaxOnlyAction> cl_as(m,"SyntaxOnlyAction",create_default_constructor);
    cl_as.def("CreateASTConsumer", &DerivableSyntaxOnlyAction::CreateASTConsumer);
    class_<clang::tooling::ToolAction> cl_at(m,"ToolAction");
    class_<clang::tooling::FrontendActionFactory, clang::tooling::ToolAction> cl_au(m,"Clang-FrontendActionFactory");
    m.def("newFrontendActionFactory", &ast_tooling__newFrontendActionFactory);
    derivable_class_<DerivableFrontendActionFactory, clang::tooling::FrontendActionFactory> cl_av(m,"FrontendActionFactory",create_default_constructor);
    cl_av.def("derivable-frontend-action-factory-create", &DerivableFrontendActionFactory::default_create);
    class_<clang::tooling::ArgumentsAdjuster> cl_aw(m,"ArgumentsAdjuster");
    m.def("getClangSyntaxOnlyAdjuster", &clang::tooling::getClangSyntaxOnlyAdjuster);
    m.def("getClangStripOutputAdjuster", &clang::tooling::getClangStripOutputAdjuster);

    // Don't need derivable_class_ ???????
    //            ,derivable_class_<DerivableArgumentsAdjuster,clang::tooling::ArgumentsAdjuster>("ArgumentsAdjuster")
    //            .    def("ArgumentsAdjuster-adjust",&DerivableArgumentsAdjuster::Adjust)

    /* Expose the Dynamic Matcher library */
  
    class_<clang::ast_matchers::dynamic::DynTypedMatcher> cl_ax(m,"DynTypedMatcher");
    //     class_<ParserValue>("ParserValue")
    //     .def_constructor("newParserValue", constructor<core::Cons_sp, const VariantValue &>()),
    class_<clang::ast_matchers::dynamic::VariantValue> cl_ay(m,"VariantValue");
    cl_ay.def_constructor("newVariantValueUnsigned", constructor<unsigned>())
        .def_constructor("newVariantValueString", constructor<std::string>())
        .def_constructor("newVariantValueMatcher", constructor<const clang::ast_matchers::dynamic::VariantMatcher &>());
    class_<clang::ast_matchers::dynamic::VariantMatcher> cl_az(m,"VariantMatcher");
    cl_az.def("getTypeAsString", &clang::ast_matchers::dynamic::VariantMatcher::getTypeAsString);
    m.def("getSingleMatcher", &ast_tooling__getSingleMatcher);
    class_<clang::ast_matchers::MatchFinder> cl_ba(m,"MatchFinder");
    cl_ba.def_constructor("newMatchFinder", constructor<>())
        .def("addDynamicMatcher", &clang::ast_matchers::MatchFinder::addDynamicMatcher) // TODO: Add a nurse/patient relationship for argument and object
        .def("matchAST", &clang::ast_matchers::MatchFinder::matchAST);
    m.def("match", &ast_tooling__match);
    // First argument is now a UniquePtr - so we have to handle it specially
//    m.def("runToolOnCode", &clang::tooling::runToolOnCode);
    class_<clang::ast_matchers::MatchFinder::MatchCallback> cl_bb(m,"MatchCallback-abstract");
    derivable_class_<DerivableMatchCallback, clang::ast_matchers::MatchFinder::MatchCallback> cl_bc(m,"MatchCallback",create_default_constructor);
    cl_bc.def("RUN", &DerivableMatchCallback::default_run)
        .def("onStartOfTranslationUnit", &DerivableMatchCallback::default_onStartOfTranslationUnit)
        .def("onEndOfTranslationUnit", &DerivableMatchCallback::default_onEndOfTranslationUnit);
    class_<clang::ast_matchers::MatchFinderMatchResult> cl_bd(m,"MatchResult");
    cl_bd.def("Nodes", &clang::ast_matchers::MatchFinderMatchResult::getNodes)
        .def("match-result-context", &clang::ast_matchers::MatchFinderMatchResult::getContext)
        .def("SourceManager", &clang::ast_matchers::MatchFinderMatchResult::getSourceManager);
    class_<clang::ast_matchers::BoundNodes> cl_be(m,"BoundNodes");
    m.def("IDToNodeMap", &ast_tooling__IDToNodeMap, DocString("IDToNodeMap - returns a HashTable of bound keyword symbols to wrapped nodes"), "(bound-nodes)"_ll);
    m.def("Lexer-getLocForEndOfToken", &clang::Lexer::getLocForEndOfToken);
    m.def("Lexer-getSourceText", &clang::Lexer::getSourceText, pureOutValue<4>());
    class_<clang::tooling::CompileCommand> cl_bf(m,"CompileCommand");
    cl_bf.property("CompileCommandDirectory", &clang::tooling::CompileCommand::Directory)
        .property("CompileCommandCommandLine", &clang::tooling::CompileCommand::CommandLine);
  }

  {
    // CLANG-COMMENTS pkgcc must be in its own scope
    package_ pkgcc("CLANG-COMMENTS", {}, {});
    scope_& ss = pkgcc.scope();
    class_<clang::comments::Comment> cl_bg(ss,"Comment");
    cl_bg.def("getSourceRange", &clang::comments::Comment::getSourceRange);
    class_<clang::comments::FullComment, clang::comments::Comment> cl_bh(ss,"FullComment");
  }
}


DOCGROUP(clasp);
CL_DEFUN core::T_sp ast_tooling__parse_dynamic_matcher(const string& matcher)
{
//  printf("%s:%d:%s got matcher %s\n", __FILE__, __LINE__, __FUNCTION__, matcher.c_str());
  llvm::StringRef matchersr(matcher);
  clang::ast_matchers::dynamic::Diagnostics error;
  llvm::Optional<clang::ast_matchers::dynamic::DynTypedMatcher> Matcher =
    clang::ast_matchers::dynamic::Parser::parseMatcherExpression(matchersr, NULL, NULL, &error);
  if (!Matcher) {
    SIMPLE_ERROR(("Could not parse expression %s") , matcher);
  }
  return translate::to_object<clang::ast_matchers::dynamic::DynTypedMatcher>::convert(*Matcher);
};

};
