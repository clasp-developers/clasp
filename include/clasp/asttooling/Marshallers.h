/*
    File: Marshallers.h
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
//===--- Marshallers.h - Generic matcher function marshallers -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Functions templates and classes to wrap matcher construct functions.
///
/// A collection of template function and classes that provide a generic
/// marshalling layer on top of matcher construct functions.
/// These are used by the registry to export all marshaller constructors with
/// the same generic interface.
///
//===----------------------------------------------------------------------===//

#ifndef asttooling_MARSHALLERS_H
#define asttooling_MARSHALLERS_H

#include <string>

#include <clang/ASTMatchers/ASTMatchers.h>
#include <clasp/asttooling/Diagnostics.h>
#include <clang/ASTMatchers/Dynamic/VariantValue.h>
#include <clang/Basic/LLVM.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/Support/type_traits.h>

namespace asttooling {

namespace internal {

using namespace clang::ast_matchers::dynamic;

/// \brief Helper template class to just from argument type to the right is/get
///   functions in VariantValue.
/// Used to verify and extract the matcher arguments below.
template <class T>
struct ArgTypeTraits;
template <class T>
struct ArgTypeTraits<const T &> : public ArgTypeTraits<T> {
};

template <>
struct ArgTypeTraits<std::string> {
  static StringRef asString() { return "String"; }
  static bool is(const VariantValue &Value) { return Value.isString(); }
  static const std::string &get(const VariantValue &Value) {
    return Value.getString();
  }
};

template <>
struct ArgTypeTraits<StringRef> : public ArgTypeTraits<std::string> {
};

template <class T>
struct ArgTypeTraits<clang::ast_matchers::internal::Matcher<T>> {
  static std::string asString() {
    return (Twine("Matcher<") +
            clang::ast_type_traits::ASTNodeKind::getFromNodeKind<T>().asStringRef() +
            ">").str();
  }
  static bool is(const VariantValue &Value) {
    return Value.isMatcher() && Value.getMatcher().hasTypedMatcher<T>();
  }
  static clang::ast_matchers::internal::Matcher<T> get(const VariantValue &Value) {
    return Value.getMatcher().getTypedMatcher<T>();
  }
};

template <>
struct ArgTypeTraits<unsigned> {
  static std::string asString() { return "Unsigned"; }
  static bool is(const VariantValue &Value) { return Value.isUnsigned(); }
  static unsigned get(const VariantValue &Value) {
    return Value.getUnsigned();
  }
};

/// \brief Matcher descriptor interface.
///
/// Provides a \c create() method that constructs the matcher from the provided
/// arguments.
class MatcherDescriptor {
  struct metadata_always_fix_pointers_to_derived_classes;

public:
  virtual ~MatcherDescriptor() {}
  virtual VariantMatcher create(core::Cons_sp NameRange,
                                ArrayRef<ParserValue> Args,
                                Diagnostics *Error) const = 0;
};

/// \brief Simple callback implementation. Marshaller and function are provided.
///
/// This class wraps a function of arbitrary signature and a marshaller
/// function into a MatcherDescriptor.
/// The marshaller is in charge of taking the VariantValue arguments, checking
/// their types, unpacking them and calling the underlying function.
class FixedArgCountMatcherDescriptor : public MatcherDescriptor {
  FRIEND_GC_SCANNER(FixedArgCountMatcherDescriptor);

public:
  typedef VariantMatcher (*MarshallerType)(void (*Func)(),
                                           core::Symbol_sp MatcherName,
                                           core::Cons_sp NameRange,
                                           ArrayRef<ParserValue> Args,
                                           Diagnostics *Error);

  /// \param Marshaller Function to unpack the arguments and call \c Func
  /// \param Func Matcher construct function. This is the function that
  ///   compile-time matcher expressions would use to create the matcher.
  FixedArgCountMatcherDescriptor(MarshallerType Marshaller, void (*Func)(),
                                 core::Symbol_sp MatcherName)
      : Marshaller(Marshaller), Func(Func), MatcherName(MatcherName) {}

  VariantMatcher create(core::Cons_sp NameRange,
                        ArrayRef<ParserValue> Args,
                        Diagnostics *Error) const {
    return Marshaller(Func, MatcherName, NameRange, Args, Error);
  }

GCPRIVATE:
  const MarshallerType Marshaller;
  void (*const Func)();
  core::Symbol_sp MatcherName;
};

/// \brief Simple callback implementation. Free function is wrapped.
///
/// This class simply wraps a free function with the right signature to export
/// it as a MatcherDescriptor.
/// This allows us to have one implementation of the interface for as many free
/// functions as we want, reducing the number of symbols and size of the
/// object file.
class FreeFuncMatcherDescriptor : public MatcherDescriptor {
  FRIEND_GC_SCANNER(asttooling::internal::FreeFuncMatcherDescriptor);

public:
  typedef VariantMatcher (*RunFunc)(core::Symbol_sp MatcherName,
                                    core::Cons_sp NameRange,
                                    ArrayRef<ParserValue> Args,
                                    Diagnostics *Error);

  FreeFuncMatcherDescriptor(RunFunc Func, core::Symbol_sp MatcherName)
      : Func(Func), MatcherName(MatcherName) {}

  VariantMatcher create(core::Cons_sp NameRange,
                        ArrayRef<ParserValue> Args,
                        Diagnostics *Error) const {
    return Func(MatcherName, NameRange, Args, Error);
  }

GCPRIVATE:
  const RunFunc Func;
  core::Symbol_sp MatcherName;
};

/// \brief Helper macros to check the arguments on all marshaller functions.
#define CHECK_ARG_COUNT(count)                                                                \
  if (Args.size() != count) {                                                                 \
    Error->addError(NameRange, /*Error->*/ ET_RegistryWrongArgCount) << count << Args.size(); \
    return VariantMatcher();                                                                  \
  }

#define CHECK_ARG_TYPE(index, type)                                                                                                                                   \
  if (!ArgTypeTraits<type>::is(Args[index].Value)) {                                                                                                                  \
    Error->addError(Args[index].Range, /*Error->*/ ET_RegistryWrongArgType) << (index + 1) << ArgTypeTraits<type>::asString() << Args[index].Value.getTypeAsString(); \
    return VariantMatcher();                                                                                                                                          \
  }

/// \brief Helper methods to extract and merge all possible typed matchers
/// out of the polymorphic object.
template <class PolyMatcher>
static void mergePolyMatchers(const PolyMatcher &Poly,
                              std::vector<DynTypedMatcher> &Out,
                              clang::ast_matchers::internal::EmptyTypeList) {}

template <class PolyMatcher, class TypeList>
static void mergePolyMatchers(const PolyMatcher &Poly,
                              std::vector<DynTypedMatcher> &Out,
                              TypeList) {
  Out.push_back(clang::ast_matchers::internal::Matcher<typename TypeList::head>(Poly));
  mergePolyMatchers(Poly, Out, typename TypeList::tail());
}

/// \brief Convert the return values of the functions into a VariantMatcher.
///
/// There are 2 cases right now: The return value is a Matcher<T> or is a
/// polymorphic matcher. For the former, we just construct the VariantMatcher.
/// For the latter, we instantiate all the possible Matcher<T> of the poly
/// matcher.

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-function"
static VariantMatcher outvalueToVariantMatcher(const DynTypedMatcher &Matcher) {
  return VariantMatcher::SingleMatcher(Matcher);
}
#pragma clang diagnostic pop

template <typename T>
static VariantMatcher outvalueToVariantMatcher(const T &PolyMatcher,
                                               typename T::ReturnTypes * =
                                                   NULL) {
  std::vector<DynTypedMatcher> Matchers;
  mergePolyMatchers(PolyMatcher, Matchers, typename T::ReturnTypes());
  VariantMatcher Out = VariantMatcher::PolymorphicMatcher(Matchers);
  return Out;
}

/// \brief 0-arg marshaller function.
template <typename ReturnType>
static VariantMatcher matcherMarshall0(void (*Func)(), core::Symbol_sp MatcherName,
                                       core::Cons_sp NameRange,
                                       ArrayRef<ParserValue> Args,
                                       Diagnostics *Error) {
  typedef ReturnType (*FuncType)();
  CHECK_ARG_COUNT(0);
  return outvalueToVariantMatcher(reinterpret_cast<FuncType>(Func)());
}

/// \brief 1-arg marshaller function.
template <typename ReturnType, typename ArgType1>
static VariantMatcher matcherMarshall1(void (*Func)(), core::Symbol_sp MatcherName,
                                       core::Cons_sp NameRange,
                                       ArrayRef<ParserValue> Args,
                                       Diagnostics *Error) {
  typedef ReturnType (*FuncType)(ArgType1);
  CHECK_ARG_COUNT(1);
  CHECK_ARG_TYPE(0, ArgType1);
  return outvalueToVariantMatcher(reinterpret_cast<FuncType>(Func)(
      ArgTypeTraits<ArgType1>::get(Args[0].Value)));
}

/// \brief 2-arg marshaller function.
template <typename ReturnType, typename ArgType1, typename ArgType2>
static VariantMatcher matcherMarshall2(void (*Func)(), core::Symbol_sp MatcherName,
                                       core::Cons_sp NameRange,
                                       ArrayRef<ParserValue> Args,
                                       Diagnostics *Error) {
  typedef ReturnType (*FuncType)(ArgType1, ArgType2);
  CHECK_ARG_COUNT(2);
  CHECK_ARG_TYPE(0, ArgType1);
  CHECK_ARG_TYPE(1, ArgType2);
  return outvalueToVariantMatcher(reinterpret_cast<FuncType>(Func)(
      ArgTypeTraits<ArgType1>::get(Args[0].Value),
      ArgTypeTraits<ArgType2>::get(Args[1].Value)));
}

#undef CHECK_ARG_COUNT
#undef CHECK_ARG_TYPE

/// \brief Variadic marshaller function.
template <typename ResultT, typename ArgT,
          ResultT (*Func)(ArrayRef<const ArgT *>)>
VariantMatcher
variadicMatcherDescriptor(core::Symbol_sp MatcherName,
                          core::Cons_sp NameRange,
                          ArrayRef<ParserValue> Args, Diagnostics *Error) {
  //    int***i = ArgT(); i; // What is the type of ArgT???????   If Derived from MatcherDescriptor then use allocateClass
  ArgT **InnerArgs = new ArgT *[Args.size()]();

  bool HasError = false;
  for (size_t i = 0, e = Args.size(); i != e; ++i) {
    typedef ArgTypeTraits<ArgT> ArgTraits;
    const ParserValue &Arg = Args[i];
    const VariantValue &Value = Arg.Value;
    if (!ArgTraits::is(Value)) {
      Error->addError(Arg.Range, /*Error->*/ ET_RegistryWrongArgType)
          << (i + 1) << ArgTraits::asString() << Value.getTypeAsString();
      HasError = true;
      break;
    }
    InnerArgs[i] = new ArgT(ArgTraits::get(Value));
  }

  VariantMatcher Out;
  if (!HasError) {
    Out = outvalueToVariantMatcher(
        Func(ArrayRef<const ArgT *>(InnerArgs, Args.size())));
  }

  for (size_t i = 0, e = Args.size(); i != e; ++i) {
    delete InnerArgs[i];
  }
  delete[] InnerArgs;
  return Out;
}

/// \brief Helper class used to collect all the possible overloads of an
///   argument adaptative matcher function.
template <template <typename ToArg, typename FromArg> class ArgumentAdapterT,
          typename FromTypes, typename ToTypes>
class AdaptativeOverloadCollector {
public:
  AdaptativeOverloadCollector(core::Symbol_sp Name,
                              gctools::Vec0<gctools::tagged_pointer<MatcherDescriptor>> &Out)
      : Name(Name), Out(Out) {
    collect(FromTypes());
  }

private:
  typedef clang::ast_matchers::internal::ArgumentAdaptingMatcherFunc<
      ArgumentAdapterT, FromTypes, ToTypes> AdaptativeFunc;

  /// \brief End case for the recursion
  static void collect(clang::ast_matchers::internal::EmptyTypeList) {}

  /// \brief Recursive case. Get the overload for the head of the list, and
  ///   recurse to the tail.
  template <typename FromTypeList>
  inline void collect(FromTypeList);

  const core::Symbol_sp Name;
  gctools::Vec0<gctools::tagged_pointer<MatcherDescriptor>> &Out;
};

/// \brief MatcherDescriptor that wraps multiple "overloads" of the same
///   matcher.
///
/// It will try every overload and generate appropriate errors for when none or
/// more than one overloads match the arguments.
class OverloadedMatcherDescriptor : public MatcherDescriptor {
  FRIEND_GC_SCANNER(asttooling::internal::OverloadedMatcherDescriptor);

public:
  // gctools::tagged_pointer<OverloadedMatcherDescriptor>rrayRef<MatcherDescriptor *> Callbacks) : Overloads(Callbacks) {};

  //    gctools::tagged_pointer<OverloadedMatcherDescriptor>rrayRef<MatcherDescriptor *> Callbacks) : Overloads(Callbacks) {};
  OverloadedMatcherDescriptor(const gctools::Vec0<gctools::tagged_pointer<MatcherDescriptor>> Callbacks) {
    for (auto it = Callbacks.begin(); it != Callbacks.end(); ++it) {
      Overloads.push_back(*it);
    }
  }
  OverloadedMatcherDescriptor(ArrayRef<gctools::tagged_pointer<MatcherDescriptor>> Callbacks) {
    for (auto it = Callbacks.begin(); it != Callbacks.end(); ++it) {
      Overloads.push_back(*it);
    }
  }

  virtual ~OverloadedMatcherDescriptor() {
    printf("%s:%d In ~OverloadedMatcherDescriptor  - I think I need to dereference the MatcherDescriptors and pass them back to LLVM to delete them - This isn't compatible with the garbage collector and tagged pointers - I am going to have to make some big changes here to make this compatible with LLVM/Clang\n", __FILE__, __LINE__);
    //llvm::DeleteContainerPointers(Overloads);
  }

  virtual VariantMatcher create(core::Cons_sp NameRange,
                                ArrayRef<ParserValue> Args,
                                Diagnostics *Error) const {
    std::vector<VariantMatcher> Constructed;
    OverloadContext Ctx(Error);
    for (size_t i = 0, e = Overloads.size(); i != e; ++i) {
      VariantMatcher SubMatcher = Overloads[i]->create(NameRange, Args, Error);
      if (!SubMatcher.isNull()) {
        Constructed.push_back(SubMatcher);
      }
    }

    if (Constructed.empty())
      return VariantMatcher(); // No overload matched.
    // We ignore the errors if any matcher succeeded.
    Ctx.revertErrors();
    if (Constructed.size() > 1) {
      // More than one constructed. It is ambiguous.
      Error->addError(NameRange, /*Error->*/ ET_RegistryAmbiguousOverload);
      return VariantMatcher();
    }
    return Constructed[0];
  }

GCPRIVATE:
  gctools::Vec0<gctools::tagged_pointer<MatcherDescriptor>> Overloads;
};

/// \brief Variadic operator marshaller function.
class VariadicOperatorMatcherDescriptor : public MatcherDescriptor {
  FRIEND_GC_SCANNER(asttooling::internal::VariadicOperatorMatcherDescriptor);

public:
  typedef clang::ast_matchers::internal::DynTypedMatcher::VariadicOperator VarOp;
  VariadicOperatorMatcherDescriptor(unsigned MinCount, unsigned MaxCount,
                                    VarOp op, core::Symbol_sp MatcherName)
      : MinCount(MinCount), MaxCount(MaxCount), Op(op),
        MatcherName(MatcherName) {}

  virtual VariantMatcher create(core::Cons_sp NameRange,
                                ArrayRef<ParserValue> Args,
                                Diagnostics *Error) const {
    if (Args.size() < MinCount || MaxCount < Args.size()) {
      const std::string MaxStr =
          (MaxCount == UINT_MAX ? "" : Twine(MaxCount)).str();
      Error->addError(NameRange, /*Error->*/ ET_RegistryWrongArgCount)
          << ("(" + Twine(MinCount) + ", " + MaxStr + ")") << Args.size();
      return VariantMatcher();
    }

    std::vector<VariantMatcher> InnerArgs;
    for (size_t i = 0, e = Args.size(); i != e; ++i) {
      const ParserValue &Arg = Args[i];
      const VariantValue &Value = Arg.Value;
      if (!Value.isMatcher()) {
        Error->addError(Arg.Range, /*Error->*/ ET_RegistryWrongArgType)
            << (i + 1) << "Matcher<>" << Value.getTypeAsString();
        return VariantMatcher();
      }
      InnerArgs.push_back(Value.getMatcher());
    }
    return VariantMatcher::VariadicOperatorMatcher(Op, InnerArgs);
  }

GCPRIVATE:
  const unsigned MinCount;
  const unsigned MaxCount;
  const VarOp Op;
  core::Symbol_sp MatcherName;
};

/// Helper functions to select the appropriate marshaller functions.
/// They detect the number of arguments, arguments types and return type.

/// \brief 0-arg overload
template <typename ReturnType>
gc::tagged_pointer<MatcherDescriptor> makeMatcherAutoMarshall(ReturnType (*Func)(),
                                                              core::Symbol_sp MatcherName) {
#ifndef USE_NEW
  return gctools::ClassAllocator<FixedArgCountMatcherDescriptor>::allocateClass(matcherMarshall0<ReturnType>, reinterpret_cast<void (*)()>(Func), MatcherName);
#else
  return new FixedArgCountMatcherDescriptor(
      matcherMarshall0<ReturnType>, reinterpret_cast<void (*)()>(Func),
      MatcherName);
#endif
}

/// \brief 1-arg overload
template <typename ReturnType, typename ArgType1>
gc::tagged_pointer<MatcherDescriptor> makeMatcherAutoMarshall(ReturnType (*Func)(ArgType1),
                                                              core::Symbol_sp MatcherName) {
#ifndef USE_NEW
  return gctools::ClassAllocator<FixedArgCountMatcherDescriptor>::allocateClass(
      matcherMarshall1<ReturnType, ArgType1>,
      reinterpret_cast<void (*)()>(Func), MatcherName);
#else
  return new FixedArgCountMatcherDescriptor(
      matcherMarshall1<ReturnType, ArgType1>,
      reinterpret_cast<void (*)()>(Func), MatcherName);
#endif
}

/// \brief 2-arg overload
template <typename ReturnType, typename ArgType1, typename ArgType2>
gc::tagged_pointer<MatcherDescriptor> makeMatcherAutoMarshall(ReturnType (*Func)(ArgType1,
                                                                                 ArgType2),
                                                              core::Symbol_sp MatcherName) {
#ifndef USE_NEW
  return gctools::ClassAllocator<FixedArgCountMatcherDescriptor>::allocateClass(
      matcherMarshall2<ReturnType, ArgType1, ArgType2>,
      reinterpret_cast<void (*)()>(Func), MatcherName);
#else
  return new FixedArgCountMatcherDescriptor(
      matcherMarshall2<ReturnType, ArgType1, ArgType2>,
      reinterpret_cast<void (*)()>(Func), MatcherName);
#endif
}

/// \brief Variadic overload.
template <typename ResultT, typename ArgT,
          ResultT (*Func)(ArrayRef<const ArgT *>)>
gc::tagged_pointer<MatcherDescriptor>
makeMatcherAutoMarshall(llvm::VariadicFunction<ResultT, ArgT, Func> VarFunc,
                        core::Symbol_sp MatcherName) {
#ifndef USE_NEW
  return gctools::ClassAllocator<FreeFuncMatcherDescriptor>::allocateClass(
      &variadicMatcherDescriptor<ResultT, ArgT, Func>, MatcherName);
#else
  return new FreeFuncMatcherDescriptor(
      &variadicMatcherDescriptor<ResultT, ArgT, Func>, MatcherName);
#endif
}

/// \brief Argument adaptative overload.
template <template <typename ToArg, typename FromArg> class ArgumentAdapterT,
          typename FromTypes, typename ToTypes>
gc::tagged_pointer<MatcherDescriptor>
makeMatcherAutoMarshall(clang::ast_matchers::internal::ArgumentAdaptingMatcherFunc<
                            ArgumentAdapterT, FromTypes, ToTypes>,
                        core::Symbol_sp MatcherName) {
  gctools::Vec0<gctools::tagged_pointer<MatcherDescriptor>> Overloads;
  AdaptativeOverloadCollector<ArgumentAdapterT, FromTypes, ToTypes>(MatcherName,
                                                                    Overloads);
#ifndef USE_NEW
  return gctools::ClassAllocator<OverloadedMatcherDescriptor>::allocateClass(Overloads);
#else
  return new OverloadedMatcherDescriptor(Overloads);
#endif
}

template <template <typename ToArg, typename FromArg> class ArgumentAdapterT,
          typename FromTypes, typename ToTypes>
template <typename FromTypeList>
inline void AdaptativeOverloadCollector<ArgumentAdapterT, FromTypes,
                                        ToTypes>::collect(FromTypeList) {
  Out.push_back(makeMatcherAutoMarshall(
      &AdaptativeFunc::template create<typename FromTypeList::head>, Name));
  collect(typename FromTypeList::tail());
}

/// \brief Variadic operator overload.
template <unsigned MinCount, unsigned MaxCount>
gc::tagged_pointer<MatcherDescriptor>
makeMatcherAutoMarshall(clang::ast_matchers::internal::VariadicOperatorMatcherFunc<MinCount, MaxCount> Func,
                        core::Symbol_sp MatcherName) {
#ifndef USE_NEW
  return gctools::ClassAllocator<VariadicOperatorMatcherDescriptor>::allocateClass(MinCount, MaxCount, Func.Op,
                                                                                   MatcherName);
#else
  return new VariadicOperatorMatcherDescriptor(MinCount, MaxCount, Func.Func,
                                               MatcherName);
#endif
}

} // namespace internal
} // namespace asttooling

#endif // LLVM_CLANG_AST_MATCHERS_DYNAMIC_MARSHALLERS_H
