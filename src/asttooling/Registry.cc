/*
    File: Registry.cc
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
//===--- Registry.cpp - Matcher registry -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===------------------------------------------------------------===//
///
/// \file
/// \brief Registry map populated at static initialization time.
///
//===------------------------------------------------------------===//

#include <clasp/core/common.h>
#include <clasp/core/str.h>
#include <clasp/asttooling/Registry.h>

#include <utility>
#include <clasp/core/common.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/asttooling/asttoolingPackage.h>
#include <clasp/asttooling/Marshallers.h>
#include <clang/ASTMatchers/ASTMatchers.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringRef.h>

namespace asttooling {

SYMBOL_EXPORT_SC_(AstToolingPkg,STARmatcher_namesSTAR);

/*! Many ASTMatchers like recordDecl() were renamed to cxxRecordDecl() with
messes with Clasp's name lispification.  lispify(cxxRecordDecl) --> CXX-RECORD-DECL
But the class that cxxRecordDecl() is supposed to match is CXXRECORD-DECL (lispify(CXXRecordDecl)) 
So I'll fix it here by converting names that start with "cxx" to start with "CXX" 
Also fix up CUDA and RV.*/
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


CL_DEFUN core::Symbol_sp ast_tooling__intern_matcher_keyword(const string& orig_name)
{
  core::Symbol_sp name = core::lispify_intern_keyword(ast_tooling__fix_matcher_name(orig_name));
  return name;
}

void add_matcher_name(const string& name, core::Symbol_sp symbol)
{
  core::List_sp one = core::Cons_O::createList(symbol,core::Str_O::create(name));
  _sym_STARmatcher_namesSTAR->defparameter(core::Cons_O::create(one,_sym_STARmatcher_namesSTAR->symbolValue()));
}

namespace RegMap {

using asttooling::MatcherDescriptor_O;

void RegistryMaps_O::_registerMatcher(const string& name,
                                      core::Symbol_sp MatcherName,
                                    gctools::smart_ptr<MatcherDescriptor_O> Callback) const {
  add_matcher_name(name,MatcherName);
#ifdef DEBUG_ON
  ConstructorMap::iterator pos = this->find(MatcherName);
  ASSERTF(pos == Constructors.end(), BF("The MatcherName %s has already had a constructor defined for it") % _rep_(MatcherName));
#endif
  Constructors.emplace_back(SymbolMatcherDescriptorPair_O(MatcherName, Callback));
  //Constructors[MatcherName] = Callback;
}



#define REGISTER_MATCHER(name)                                                             \
  _registerMatcher(#name,\
                   ast_tooling__intern_matcher_keyword(#name),\
                   makeMatcherAutoMarshall( ::clang::ast_matchers::name, ast_tooling__intern_matcher_keyword(#name)));
#define SPECIFIC_MATCHER_OVERLOAD(name, Id)            \
  static_cast<::clang::ast_matchers::name##_Type##Id>( \
      ::clang::ast_matchers::name)
#define REGISTER_OVERLOADED_2(name) \
  do { \
    gctools::smart_ptr<MatcherDescriptor_O> Callbacks[] = { \
        makeMatcherAutoMarshall(SPECIFIC_MATCHER_OVERLOAD(name, 0), \
                                ast_tooling__intern_matcher_keyword(#name)), \
        makeMatcherAutoMarshall(SPECIFIC_MATCHER_OVERLOAD(name, 1), \
                                ast_tooling__intern_matcher_keyword(#name))}; \
    _registerMatcher(#name,ast_tooling__intern_matcher_keyword(#name), \
                     gctools::GC<OverloadedMatcherDescriptor_O>::allocate(Callbacks) /*new OverloadedMatcherDescriptor(Callbacks)*/); \
  } while (0)

/// \brief Generate a registry map with all the known matchers.
RegistryMaps_O::RegistryMaps_O() : Initialized(false){};
void RegistryMaps_O::lazyInitialize() const {
  if (this->Initialized)
    return;
  // TODO: Here is the list of the missing matchers, grouped by reason.
  //
  // Need Variant/Parser fixes:
  // ofKind
  //
  // Polymorphic + argument overload:
  // findAll
  //
  // Other:
  // loc
  // equals
  // equalsNode


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

RegistryMaps_O::~RegistryMaps_O() {
  for (ConstructorMap::iterator it = Constructors.begin(),
                                end = Constructors.end();
       it != end; ++it) {
    //                delete it->second;
  }
}

//        static gctools::ManagedStatic<RegistryMaps> RegistryData;
gctools::smart_ptr<RegistryMaps_O> RegistryData;

} // RegMap namespace - was anonymous namespace
using namespace RegMap;

void convertArgs(gctools::Vec0<ParserValue> &args, core::Vector_sp lispArgs) {
  args.resize(lispArgs->length());
  for (int i(0), iEnd(lispArgs->length()); i < iEnd; i++) {
    args[i] = *(gc::As<core::WrappedPointer_sp>(lispArgs->elt(i))->cast<ParserValue>());
  }
}

// static
clang::ast_matchers::dynamic::VariantMatcher Registry::constructMatcher(core::Symbol_sp MatcherName,
                                                                        core::Cons_sp NameRange,
                                                                        core::Vector_sp Args,
                                                                        Diagnostics *Error) {
  printf("%s:%d Try constructMatcher name: %s NameRange: %s Args: %s\n",
         __FILE__, __LINE__, _rep_(MatcherName).c_str(), _rep_(NameRange).c_str(), _rep_(Args).c_str());
  RegistryMaps_O::const_iterator it = RegistryData->find(MatcherName);
  if (it == RegistryData->end()) {
    core::Symbol_sp sym = MatcherName;
    core::Str_sp strName = sym->symbolName();
    string symbolName = strName->get();
    core::Cons_sp NameRangeDup = NameRange;
    Error->addError(NameRangeDup, /*Error->*/ ET_RegistryNotFound) << symbolName;
    return clang::ast_matchers::dynamic::VariantMatcher();
  }
  gctools::Vec0<ParserValue> VArgs;
  //       gctools::StackRootedStlContainer<vector<ParserValue> > VArgs;
  convertArgs(VArgs, Args);
  auto b = VArgs.begin();
  return it->matcher->create(NameRange, ArrayRef<ParserValue>(&*b, VArgs.size()), Error);
}

// static
clang::ast_matchers::dynamic::VariantMatcher Registry::constructBoundMatcher(core::Symbol_sp MatcherName,
                                                                             core::Cons_sp NameRange,
                                                                             StringRef BindID,
                                                                             core::Vector_sp Args,
                                                                             Diagnostics *Error) {
  printf("%s:%d Try constructBoundMatcher name: %s NameRange: %s Args: %s  BindID: %s \n", __FILE__, __LINE__, _rep_(MatcherName).c_str(), _rep_(NameRange).c_str(), _rep_(Args).c_str(), BindID);
  clang::ast_matchers::dynamic::VariantMatcher Out = constructMatcher(MatcherName, NameRange, Args, Error);
  if (Out.isNull())
    return Out;
  llvm::Optional<DynTypedMatcher> Result = Out.getSingleMatcher();
  if (Result.hasValue()) {
    llvm::Optional<DynTypedMatcher> Bound = Result->tryBind(BindID);
    if (Bound.hasValue()) {
      return clang::ast_matchers::dynamic::VariantMatcher::SingleMatcher(*Bound);
    }
  }
  Error->addError(NameRange, /*Error->*/ ET_RegistryNotBindable);
  return clang::ast_matchers::dynamic::VariantMatcher();
}

void initialize_Registry() {
//  RegistryData = gctools::RootClassAllocator<RegistryMaps_O>::allocate();
  RegistryData = gctools::GC<RegistryMaps_O>::allocate();
  _sym_STARmatcher_namesSTAR->defparameter(_Nil<core::T_O>());
//  RegistryData->lazyInitialize();
}
};
