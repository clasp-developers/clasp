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
namespace RegMap {

using asttooling::internal::MatcherDescriptor;

void RegistryMaps::_registerMatcher(core::Symbol_sp MatcherName,
                                    gctools::tagged_pointer<MatcherDescriptor> Callback) const {
#ifdef DEBUG_ON
  ConstructorMap::iterator pos = this->find(MatcherName);
  ASSERTF(pos == Constructors.end(), BF("The MatcherName %s has already had a constructor defined for it") % _rep_(MatcherName));
#endif
  Constructors.emplace_back(SymbolMatcherDescriptorPair(MatcherName, Callback));
  //Constructors[MatcherName] = Callback;
}

#define REGISTER_MATCHER(name)                                                             \
  _registerMatcher(core::lispify_intern_keyword(#name), internal::makeMatcherAutoMarshall( \
                                                            ::clang::ast_matchers::name, core::lispify_intern_keyword(#name)));
#define SPECIFIC_MATCHER_OVERLOAD(name, Id)            \
  static_cast<::clang::ast_matchers::name##_Type##Id>( \
      ::clang::ast_matchers::name)
#define REGISTER_OVERLOADED_2(name)                                                                                                                                                                                                                                                    \
  do {                                                                                                                                                                                                                                                                                 \
    gctools::tagged_pointer<MatcherDescriptor> Callbacks[] = {internal::makeMatcherAutoMarshall(SPECIFIC_MATCHER_OVERLOAD(name, 0), core::lispify_intern_keyword(#name)), internal::makeMatcherAutoMarshall(SPECIFIC_MATCHER_OVERLOAD(name, 1), core::lispify_intern_keyword(#name))}; \
    _registerMatcher(core::lispify_intern_keyword(#name), gctools::ClassAllocator<internal::OverloadedMatcherDescriptor>::allocateClass(Callbacks) /*new internal::OverloadedMatcherDescriptor(Callbacks)*/);                                                                          \
  } while (0)

/// \brief Generate a registry map with all the known matchers.
RegistryMaps::RegistryMaps() : Initialized(false){};
void RegistryMaps::lazyInitialize() const {
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

  REGISTER_OVERLOADED_2(callee);
  REGISTER_OVERLOADED_2(hasPrefix);
  REGISTER_OVERLOADED_2(hasType);
  REGISTER_OVERLOADED_2(isDerivedFrom);
  REGISTER_OVERLOADED_2(isSameOrDerivedFrom);
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
  REGISTER_MATCHER(asString);
  REGISTER_MATCHER(asmStmt);
  REGISTER_MATCHER(atomicType);
  REGISTER_MATCHER(autoType);
  REGISTER_MATCHER(binaryOperator);
  REGISTER_MATCHER(bindTemporaryExpr);
  REGISTER_MATCHER(blockPointerType);
  REGISTER_MATCHER(boolLiteral);
  REGISTER_MATCHER(breakStmt);
  REGISTER_MATCHER(builtinType);
  REGISTER_MATCHER(cStyleCastExpr);
  REGISTER_MATCHER(callExpr);
  REGISTER_MATCHER(caseStmt);
  REGISTER_MATCHER(castExpr);
  REGISTER_MATCHER(catchStmt);
  REGISTER_MATCHER(characterLiteral);
  REGISTER_MATCHER(classTemplateDecl);
  REGISTER_MATCHER(classTemplateSpecializationDecl);
  REGISTER_MATCHER(complexType);
  REGISTER_MATCHER(compoundLiteralExpr);
  REGISTER_MATCHER(compoundStmt);
  REGISTER_MATCHER(conditionalOperator);
  REGISTER_MATCHER(constCastExpr);
  REGISTER_MATCHER(constantArrayType);
  REGISTER_MATCHER(constructExpr);
  REGISTER_MATCHER(constructorDecl);
  REGISTER_MATCHER(containsDeclaration);
  REGISTER_MATCHER(continueStmt);
  REGISTER_MATCHER(ctorInitializer);
  REGISTER_MATCHER(decl);
  REGISTER_MATCHER(declCountIs);
  REGISTER_MATCHER(declRefExpr);
  REGISTER_MATCHER(declStmt);
  REGISTER_MATCHER(declaratorDecl);
  REGISTER_MATCHER(defaultArgExpr);
  REGISTER_MATCHER(defaultStmt);
  REGISTER_MATCHER(deleteExpr);
  REGISTER_MATCHER(dependentSizedArrayType);
  REGISTER_MATCHER(destructorDecl);
  REGISTER_MATCHER(doStmt);
  REGISTER_MATCHER(dynamicCastExpr);
  REGISTER_MATCHER(eachOf);
  REGISTER_MATCHER(elaboratedType);
  REGISTER_MATCHER(enumConstantDecl);
  REGISTER_MATCHER(enumDecl);
  REGISTER_MATCHER(equalsBoundNode);
  REGISTER_MATCHER(explicitCastExpr);
  REGISTER_MATCHER(expr);
  REGISTER_MATCHER(fieldDecl);
  REGISTER_MATCHER(floatLiteral);
  REGISTER_MATCHER(forEach);
  REGISTER_MATCHER(forEachConstructorInitializer);
  REGISTER_MATCHER(forEachDescendant);
  REGISTER_MATCHER(forEachSwitchCase);
  REGISTER_MATCHER(forField);
  REGISTER_MATCHER(forRangeStmt);
  REGISTER_MATCHER(forStmt);
  REGISTER_MATCHER(friendDecl);
  REGISTER_MATCHER(functionDecl);
  REGISTER_MATCHER(functionTemplateDecl);
  REGISTER_MATCHER(functionType);
  REGISTER_MATCHER(functionalCastExpr);
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
  REGISTER_MATCHER(hasBase);
  REGISTER_MATCHER(hasBody);
  REGISTER_MATCHER(hasCanonicalType);
  REGISTER_MATCHER(hasCaseConstant);
  REGISTER_MATCHER(hasCondition);
  REGISTER_MATCHER(hasConditionVariableStatement);
  REGISTER_MATCHER(hasDeclContext);
  REGISTER_MATCHER(hasDeclaration);
  REGISTER_MATCHER(hasDeducedType);
  REGISTER_MATCHER(hasDescendant);
  REGISTER_MATCHER(hasDestinationType);
  REGISTER_MATCHER(hasEitherOperand);
  REGISTER_MATCHER(hasElementType);
  REGISTER_MATCHER(hasFalseExpression);
  REGISTER_MATCHER(hasImplicitDestinationType);
  REGISTER_MATCHER(hasIncrement);
  REGISTER_MATCHER(hasIndex);
  REGISTER_MATCHER(hasInitializer);
  REGISTER_MATCHER(hasLHS);
  REGISTER_MATCHER(hasLocalQualifiers);
  REGISTER_MATCHER(hasLoopInit);
  REGISTER_MATCHER(hasMethod);
  REGISTER_MATCHER(hasName);
  REGISTER_MATCHER(hasObjectExpression);
  REGISTER_MATCHER(hasOperatorName);
  REGISTER_MATCHER(hasOverloadedOperatorName);
  REGISTER_MATCHER(hasParameter);
  REGISTER_MATCHER(hasParent);
  REGISTER_MATCHER(hasQualifier);
  REGISTER_MATCHER(hasRHS);
  REGISTER_MATCHER(hasSingleDecl);
  REGISTER_MATCHER(hasSize);
  REGISTER_MATCHER(hasSizeExpr);
  REGISTER_MATCHER(hasSourceExpression);
  REGISTER_MATCHER(hasTargetDecl);
  REGISTER_MATCHER(hasTemplateArgument);
  REGISTER_MATCHER(hasTrueExpression);
  REGISTER_MATCHER(hasTypeLoc);
  REGISTER_MATCHER(hasUnaryOperand);
  REGISTER_MATCHER(hasValueType);
  REGISTER_MATCHER(ifStmt);
  REGISTER_MATCHER(ignoringImpCasts);
  REGISTER_MATCHER(ignoringParenCasts);
  REGISTER_MATCHER(ignoringParenImpCasts);
  REGISTER_MATCHER(implicitCastExpr);
  REGISTER_MATCHER(incompleteArrayType);
  REGISTER_MATCHER(initListExpr);
  REGISTER_MATCHER(innerType);
  REGISTER_MATCHER(integerLiteral);
  REGISTER_MATCHER(isArrow);
  REGISTER_MATCHER(isConst);
  REGISTER_MATCHER(isConstQualified);
  REGISTER_MATCHER(isDefinition);
  REGISTER_MATCHER(isExplicitTemplateSpecialization);
  REGISTER_MATCHER(isExternC);
  REGISTER_MATCHER(isImplicit);
  REGISTER_MATCHER(isInteger);
  REGISTER_MATCHER(isOverride);
  REGISTER_MATCHER(isPrivate);
  REGISTER_MATCHER(isProtected);
  REGISTER_MATCHER(isPublic);
  REGISTER_MATCHER(isTemplateInstantiation);
  REGISTER_MATCHER(isVirtual);
  REGISTER_MATCHER(isWritten);
  REGISTER_MATCHER(lValueReferenceType);
  REGISTER_MATCHER(labelStmt);
  REGISTER_MATCHER(lambdaExpr);
  REGISTER_MATCHER(matchesName);
  REGISTER_MATCHER(materializeTemporaryExpr);
  REGISTER_MATCHER(member);
  REGISTER_MATCHER(memberCallExpr);
  REGISTER_MATCHER(memberExpr);
  REGISTER_MATCHER(memberPointerType);
  REGISTER_MATCHER(methodDecl);
  REGISTER_MATCHER(namedDecl);
  REGISTER_MATCHER(namesType);
  REGISTER_MATCHER(namespaceDecl);
  REGISTER_MATCHER(nestedNameSpecifier);
  REGISTER_MATCHER(nestedNameSpecifierLoc);
  REGISTER_MATCHER(newExpr);
  REGISTER_MATCHER(nullPtrLiteralExpr);
  REGISTER_MATCHER(nullStmt);
  REGISTER_MATCHER(ofClass);
  REGISTER_MATCHER(on);
  REGISTER_MATCHER(onImplicitObjectArgument);
  REGISTER_MATCHER(operatorCallExpr);
  REGISTER_MATCHER(parameterCountIs);
  REGISTER_MATCHER(parenType);
  REGISTER_MATCHER(parmVarDecl);
  REGISTER_MATCHER(pointee);
  REGISTER_MATCHER(pointerType);
  REGISTER_MATCHER(qualType);
  REGISTER_MATCHER(rValueReferenceType);
  REGISTER_MATCHER(recordDecl);
  REGISTER_MATCHER(recordType);
  REGISTER_MATCHER(referenceType);
  REGISTER_MATCHER(refersToDeclaration);
  REGISTER_MATCHER(refersToType);
  REGISTER_MATCHER(reinterpretCastExpr);
  REGISTER_MATCHER(returnStmt);
  REGISTER_MATCHER(returns);
  REGISTER_MATCHER(sizeOfExpr);
  REGISTER_MATCHER(specifiesNamespace);
  REGISTER_MATCHER(specifiesType);
  REGISTER_MATCHER(specifiesTypeLoc);
  REGISTER_MATCHER(statementCountIs);
  REGISTER_MATCHER(staticCastExpr);
  REGISTER_MATCHER(stmt);
  REGISTER_MATCHER(stringLiteral);
  REGISTER_MATCHER(switchCase);
  REGISTER_MATCHER(switchStmt);
  REGISTER_MATCHER(templateSpecializationType);
  REGISTER_MATCHER(temporaryObjectExpr);
  REGISTER_MATCHER(thisExpr);
  REGISTER_MATCHER(throughUsingDecl);
  REGISTER_MATCHER(throwExpr);
  REGISTER_MATCHER(to);
  REGISTER_MATCHER(tryStmt);
  REGISTER_MATCHER(type);
  REGISTER_MATCHER(typeLoc);
  REGISTER_MATCHER(typedefType);
  REGISTER_MATCHER(unaryExprOrTypeTraitExpr);
  REGISTER_MATCHER(unaryOperator);
  REGISTER_MATCHER(unaryTransformType);
  REGISTER_MATCHER(unless);
  REGISTER_MATCHER(unresolvedConstructExpr);
  REGISTER_MATCHER(unresolvedUsingValueDecl);
  REGISTER_MATCHER(userDefinedLiteral);
  REGISTER_MATCHER(usingDecl);
  REGISTER_MATCHER(varDecl);
  REGISTER_MATCHER(variableArrayType);
  REGISTER_MATCHER(whileStmt);
  REGISTER_MATCHER(withInitializer);
}

RegistryMaps::~RegistryMaps() {
  for (ConstructorMap::iterator it = Constructors.begin(),
                                end = Constructors.end();
       it != end; ++it) {
    //                delete it->second;
  }
}

//        static gctools::ManagedStatic<RegistryMaps> RegistryData;
gctools::tagged_pointer<RegistryMaps> RegistryData;

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
  RegistryMaps::const_iterator it = RegistryData->find(MatcherName);
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
  clang::ast_matchers::dynamic::VariantMatcher Out = constructMatcher(MatcherName, NameRange, Args, Error);
  if (Out.isNull())
    return Out;
  llvm::Optional<internal::DynTypedMatcher> Result = Out.getSingleMatcher();
  if (Result.hasValue()) {
    llvm::Optional<internal::DynTypedMatcher> Bound = Result->tryBind(BindID);
    if (Bound.hasValue()) {
      return clang::ast_matchers::dynamic::VariantMatcher::SingleMatcher(*Bound);
    }
  }
  Error->addError(NameRange, /*Error->*/ ET_RegistryNotBindable);
  return clang::ast_matchers::dynamic::VariantMatcher();
}

void initialize_Registry() {
  RegistryData = gctools::RootClassAllocator<RegistryMaps>::allocate();
}
};
