/*
    File: Diagnostics.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
//===--- Diagnostics.cpp - Helper class for error diagnostics -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <core/common.h>
#include <core/str.h>
#include <asttooling/Diagnostics.h>

namespace asttooling {

ArgStream Diagnostics::pushContextFrame(ContextType Type,
                                                     core::Cons_sp Range) {
  ContextStack.push_back(ContextFrame());
  ContextFrame& data = ContextStack.back();
  data.Type = Type;
  data.Range = Range;
  return ArgStream(&data.Args);
}

Context::Context(ConstructMatcherEnum,
                              Diagnostics *Error,
                              core::Symbol_sp MatcherName,
                              core::Cons_sp MatcherRange)
    : Error(Error) {
    Error->pushContextFrame(CT_MatcherConstruct, MatcherRange) << MatcherName->symbolName()->get();
}

Context::Context(MatcherArgEnum,
                              Diagnostics *Error,
                              core::Symbol_sp MatcherName,
                              core::Cons_sp MatcherRange,
                              unsigned ArgNumber)
    : Error(Error) {
  Error->pushContextFrame(CT_MatcherArg, MatcherRange) << ArgNumber
                                                       << MatcherName->symbolName()->get();
}

Context::~Context() { Error->ContextStack.pop_back(); }

OverloadContext::OverloadContext(Diagnostics *Error)
    : Error(Error), BeginIndex(Error->Errors.size()) {}

OverloadContext::~OverloadContext() {
  // Merge all errors that happened while in this context.
  if (BeginIndex < Error->Errors.size()) {
    ErrorContent &Dest = Error->Errors[BeginIndex];
    for (size_t i = BeginIndex + 1, e = Error->Errors.size(); i < e; ++i) {
      Dest.Messages.push_back(Error->Errors[i].Messages[0]);
    }
    Error->Errors.resize(BeginIndex + 1);
  }
}

void OverloadContext::revertErrors() {
  // Revert the errors.
  Error->Errors.resize(BeginIndex);
}

ArgStream &ArgStream::operator<<(const Twine &Arg) {
  Out->push_back(Arg.str());
  return *this;
}

    ArgStream Diagnostics::addError(core::Cons_sp Range,
                                             ErrorType Error) {
        ErrorContent ec;
        Errors.push_back(ec);
        ErrorContent &Last = Errors.back();
        Last.ContextStack = ContextStack;
        Last.Messages.push_back(Message());
        Last.Messages.back().Range = Range;
        Last.Messages.back().Type = Error;
        return ArgStream(&Last.Messages.back().Args);
    }

StringRef contextTypeToFormatString(ContextType Type) {
  switch (Type) {
    case CT_MatcherConstruct:
      return "Error building matcher $0.";
    case CT_MatcherArg:
      return "Error parsing argument $0 for matcher $1.";
  }
  llvm_unreachable("Unknown ContextType value.");
}

StringRef errorTypeToFormatString(ErrorType Type) {
  switch (Type) {
  case ET_RegistryNotFound:
    return "Matcher not found: $0";
  case ET_RegistryWrongArgCount:
    return "Incorrect argument count. (Expected = $0) != (Actual = $1)";
  case ET_RegistryWrongArgType:
    return "Incorrect type for arg $0. (Expected = $1) != (Actual = $2)";
  case ET_RegistryNotBindable:
    return "Matcher does not support binding.";
  case ET_RegistryAmbiguousOverload:
    // TODO: Add type info about the overload error.
    return "Ambiguous matcher overload.";

  case ET_ParserStringError:
    return "Error parsing string token: <$0>";
  case ET_ParserNoOpenParen:
    return "Error parsing matcher. Found token <$0> while looking for '('.";
  case ET_ParserNoCloseParen:
    return "Error parsing matcher. Found end-of-code while looking for ')'.";
  case ET_ParserNoComma:
    return "Error parsing matcher. Found token <$0> while looking for ','.";
  case ET_ParserNoCode:
    return "End of code found while looking for token.";
  case ET_ParserNotAMatcher:
    return "Input value is not a matcher expression.";
  case ET_ParserInvalidToken:
    return "Invalid token <$0> found when looking for a value.";
  case ET_ParserMalformedBindExpr:
    return "Malformed bind() expression.";
  case ET_ParserTrailingCode:
    return "Expected end of code.";
  case ET_ParserUnsignedError:
    return "Error parsing unsigned token: <$0>";
  case ET_ParserOverloadedType:
    return "Input value has unresolved overloaded type: $0";

  case ET_None:
    return "<N/A>";
  }
  llvm_unreachable("Unknown ErrorType value.");
}

void formatErrorString(StringRef FormatString, ArrayRef<std::string> Args,
                       llvm::raw_ostream &OS) {
  while (!FormatString.empty()) {
    std::pair<StringRef, StringRef> Pieces = FormatString.split("$");
    OS << Pieces.first.str();
    if (Pieces.second.empty()) break;

    const char Next = Pieces.second.front();
    FormatString = Pieces.second.drop_front();
    if (Next >= '0' && Next <= '9') {
      const unsigned Index = Next - '0';
      if (Index < Args.size()) {
        OS << Args[Index];
      } else {
        OS << "<Argument_Not_Provided>";
      }
    }
  }
}

    static void maybeAddLineAndColumn(core::Cons_sp Range,
                                  llvm::raw_ostream &OS) {
        OS << core::_rep_(Range);
    }

static void printContextFrameToStream(const ContextFrame &Frame,
                                      llvm::raw_ostream &OS) {
  maybeAddLineAndColumn(Frame.Range, OS);
  formatErrorString(contextTypeToFormatString(Frame.Type), Frame.Args, OS);
}

static void
printMessageToStream(const Message &Message,
                     const Twine Prefix, llvm::raw_ostream &OS) {
  maybeAddLineAndColumn(Message.Range, OS);
  OS << Prefix;
  formatErrorString(errorTypeToFormatString(Message.Type), Message.Args, OS);
}

static void printErrorContentToStream(const ErrorContent &Content,
                                      llvm::raw_ostream &OS) {
  if (Content.Messages.size() == 1) {
    printMessageToStream(Content.Messages[0], "", OS);
  } else {
    for (size_t i = 0, e = Content.Messages.size(); i != e; ++i) {
      if (i != 0) OS << "\n";
      printMessageToStream(Content.Messages[i],
                           "Candidate " + Twine(i + 1) + ": ", OS);
    }
  }
}

void Diagnostics::printToStream(llvm::raw_ostream &OS) const {
  for (size_t i = 0, e = Errors.size(); i != e; ++i) {
    if (i != 0) OS << "\n";
    printErrorContentToStream(Errors[i], OS);
  }
}

std::string Diagnostics::toString() const {
  std::string S;
  llvm::raw_string_ostream OS(S);
  printToStream(OS);
  return OS.str();
}

void Diagnostics::printToStreamFull(llvm::raw_ostream &OS) const {
  for (size_t i = 0, e = Errors.size(); i != e; ++i) {
    if (i != 0) OS << "\n";
    const ErrorContent &Error = Errors[i];
    for (size_t i = 0, e = Error.ContextStack.size(); i != e; ++i) {
      printContextFrameToStream(Error.ContextStack[i], OS);
      OS << "\n";
    }
    printErrorContentToStream(Error, OS);
  }
}

std::string Diagnostics::toStringFull() const {
  std::string S;
  llvm::raw_string_ostream OS(S);
  printToStreamFull(OS);
  return OS.str();
}

}  // namespace asttooling
