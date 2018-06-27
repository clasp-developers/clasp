/*
    File: function.h
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
// Copyright Daniel Wallin 2008. Use, modification and distribution is
// subject to the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef CLBIND_FUNCTION2_081014_HPP
#define CLBIND_FUNCTION2_081014_HPP
#include <clasp/clbind/clbindPackage.h>
#include <clasp/clbind/policies.h>
#include <clasp/clbind/details.h>
#include <clasp/clbind/scope.h>

namespace clbind {

class GoofBase {};
template <typename F>
class Goof : public GoofBase {};

template <>
class Goof<int(int, int)> : public GoofBase {
public:
  typedef int (*F)(int, int);
  Goof(F y){};
};

template <typename FunctionPtrType, typename Policies>
class VariadicFunctor : public core::Function_O {
public:
  typedef core::Function_O TemplatedBase;
  virtual size_t templatedSizeof() const { return sizeof(*this); };
};

// What was...
// #include <clbind_functoids.h>
// now becomes...
#include <clasp/clbind/clbind_functoids.h>
};

template <typename FunctionPtrType, typename Policies>
class gctools::GCStamp<clbind::VariadicFunctor<FunctionPtrType, Policies>> {
public:
  static gctools::GCStampEnum const Stamp = gctools::GCStamp<typename clbind::VariadicFunctor<FunctionPtrType, Policies>::TemplatedBase>::Stamp;
};

namespace clbind {

namespace detail {

template <typename FunctionPointerType>
struct CountFunctionArguments {
  enum { value = 0 };
};

template <typename RT, typename... ARGS>
struct CountFunctionArguments<RT (*)(ARGS...)> {
  enum { value = sizeof...(ARGS) };
};

template <class FunctionPointerType, class Policies>
struct function_registration : registration {
  function_registration(char const *name, FunctionPointerType f, Policies const &policies, string const &lambdalist, string const &declares, string const &docstring)
      : name(name), functionPtr(f), policies(policies), m_lambdalist(lambdalist), m_declares(declares), m_docstring(docstring) {}

  void register_() const {
    core::Symbol_sp symbol = core::lispify_intern(name, core::lisp_currentPackageName());
    core::FunctionDescription* fdesc = makeFunctionDescription(symbol);
    core::BuiltinClosure_sp functoid = gc::GC<VariadicFunctor<FunctionPointerType, Policies>>::allocate(fdesc,functionPtr);
    core::lisp_defun(symbol, core::lisp_currentPackageName(), functoid, m_lambdalist, m_declares, m_docstring, "=external=", 0, (CountFunctionArguments<FunctionPointerType>::value), GatherPureOutValues<Policies, -1>::gather());
  }

  char const *name;
  FunctionPointerType functionPtr;
  Policies policies;
  string m_lambdalist;
  string m_declares;
  string m_docstring;
};

} // namespace detail

template <typename F, class Policies>
scope def(char const *name, F f, Policies const &policies, string const &lambdalist = "", string const &declares = "", string const &docstring = "") {
  return scope(std::unique_ptr<detail::registration>(
      new detail::function_registration<F, Policies>(name, f, policies, lambdalist, declares, docstring)));
}

template <class F>
scope def(char const *name, F f, string const &lambdalist = "", string const &declares = "", string const &docstring = "") {
  return def(name, f, policies<>(), lambdalist, declares, docstring);
}

} // namespace clbind

#endif // CLBIND_FUNCTION2_081014_HPP
