/*
    File: Diagnostics.h
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
//===--- Diagnostics.h - Helper class for error diagnostics -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Diagnostics class to manage error messages.
///
//===----------------------------------------------------------------------===//

#ifndef asttooling_DIAGNOSTICS_H
#define asttooling_DIAGNOSTICS_H

#include <string>
#include <vector>

#include <clasp/core/common.h>
#include <clang/ASTMatchers/Dynamic/VariantValue.h>
#include <clang/Basic/LLVM.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/Twine.h>
#include <llvm/Support/raw_ostream.h>

namespace asttooling {

using clang::ast_matchers::dynamic::VariantValue;
using namespace llvm;

/// \brief A VariantValue instance annotated with its parser context.
class ParserValue {
public:
  ParserValue() : /*Text(),*/ Range(), Value() {}
  //    llvm::StringRef Text;
  core::Cons_sp Range;
  VariantValue Value;
  ParserValue(core::Cons_sp range, const VariantValue &value) : Range(range), Value(value){};
  virtual ~ParserValue(){};
  DECLARE_onHeapScanGCRoots();
};

/// \brief Parser context types.
enum ContextType {
  CT_MatcherArg = 0,
  CT_MatcherConstruct = 1
};

/// \brief All errors from the system.
enum ErrorType {
  ET_None = 0,

  ET_RegistryNotFound = 1,
  ET_RegistryWrongArgCount = 2,
  ET_RegistryWrongArgType = 3,
  ET_RegistryNotBindable = 4,
  ET_RegistryAmbiguousOverload = 5,

  ET_ParserStringError = 100,
  ET_ParserNoOpenParen = 101,
  ET_ParserNoCloseParen = 102,
  ET_ParserNoComma = 103,
  ET_ParserNoCode = 104,
  ET_ParserNotAMatcher = 105,
  ET_ParserInvalidToken = 106,
  ET_ParserMalformedBindExpr = 107,
  ET_ParserTrailingCode = 108,
  ET_ParserUnsignedError = 109,
  ET_ParserOverloadedType = 110
};

class OverloadContext;
class Context;

/// \brief Information stored for one frame of the context.
class ContextFrame {
public:
  ContextType Type;
  core::Cons_sp Range;
  std::vector<std::string> Args;
#ifdef USE_MPS
  DECLARE_onHeapScanGCRoots();
#endif
};

class Message {
public:
  core::Cons_sp Range;
  ErrorType Type;
  std::vector<std::string> Args;
};

/// \brief Information stored for each error found.
class ErrorContent {
public:
  gctools::Vec0<ContextFrame> ContextStack;
  gctools::Vec0<Message> Messages;
};

/// \brief Helper stream class.
class ArgStream {
public:
  ArgStream(std::vector<std::string> *Out) : Out(Out) {}
  template <class T>
  ArgStream &operator<<(const T &Arg) {
    return operator<<(Twine(Arg));
  }
  ArgStream &operator<<(const llvm::Twine &Arg);

private:
  std::vector<std::string> *Out;
};

/// \brief Helper class to manage error messages.
class Diagnostics {
  friend class OverloadContext;
  friend class Context;
  friend class ContextFrame;

public:
  /// \brief Add an error to the diagnostics.
  ///
  /// All the context information will be kept on the error message.
  /// \return a helper class to allow the caller to pass the arguments for the
  /// error message, using the << operator.
  ArgStream addError(core::Cons_sp Range, ErrorType Error);
  gctools::Vec0<ErrorContent>::iterator b = Errors.begin();
  gctools::Vec0<ErrorContent>::iterator e = Errors.end();
  ArrayRef<ErrorContent> errors() const { return ArrayRef<ErrorContent>(&(*b), &(*e)); }

  /// \brief Returns a simple string representation of each error.
  ///
  /// Each error only shows the error message without any context.
  void printToStream(llvm::raw_ostream &OS) const;
  std::string toString() const;

  /// \brief Returns the full string representation of each error.
  ///
  /// Each error message contains the full context.
  void printToStreamFull(llvm::raw_ostream &OS) const;
  std::string toStringFull() const;

private:
  /// \brief Helper function used by the constructors of ContextFrame.
  ArgStream pushContextFrame(ContextType Type, core::Cons_sp Range);

public:
  //    GC_RESULT onHeapScanGCRoots(GC_SCAN_ARGS_PROTOTYPE);

private:
  gctools::Vec0<ContextFrame> ContextStack;
  gctools::Vec0<ErrorContent> Errors;
};

/// \brief Class defining a parser context.
///
/// Used by the parser to specify (possibly recursive) contexts where the
/// parsing/construction can fail. Any error triggered within a context will
/// keep information about the context chain.
/// This class should be used as a RAII instance in the stack.
class Context {
public:
  /// \brief About to call the constructor for a matcher.
  enum ConstructMatcherEnum { ConstructMatcher };
  Context(ConstructMatcherEnum,
          Diagnostics *Error,
          core::Symbol_sp MatcherName,
          core::Cons_sp MatcherRange);
  /// \brief About to recurse into parsing one argument for a matcher.
  enum MatcherArgEnum { MatcherArg };
  Context(MatcherArgEnum,
          Diagnostics *Error,
          core::Symbol_sp MatcherName,
          core::Cons_sp MatcherRange, unsigned ArgNumber);
  ~Context();

private:
  Diagnostics *Error;
};

/// \brief Context for overloaded matcher construction.
///
/// This context will take care of merging all errors that happen within it
/// as "candidate" overloads for the same matcher.
class OverloadContext {
public:
  OverloadContext(Diagnostics *Error);
  virtual ~OverloadContext();
  DECLARE_onStackScanGCRoots();
  /// \brief Revert all errors that happened within this context.
  void revertErrors();

private:
  Diagnostics *Error;
  unsigned BeginIndex;
};

} // namespace asttooling

#endif // LLVM_CLANG_AST_MATCHERS_DYNAMIC_DIAGNOSTICS_H
