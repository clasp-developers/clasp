#pragma once

/*
    File: policies.h
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

#include <boost/mpl/if.hpp>
#include <boost/mpl/and.hpp>
#include <boost/mpl/not.hpp>
#include <boost/mpl/bool.hpp>

//----------------------------------------------------------------------
//
// Policies
//

namespace clbind {

const int result = 32767;
const int this_ = 32768;
const int return_value_policy_reference_ = 32769;
const int return_value_policy_copy_ = 32710;

// Declares that a parameter is to be added to the multiple-value-return
// This should only be used for pass-by-reference parameters that the
// function modifies
// The first argument is <1>
template <int N> struct outValue {};

// Declare that a (pass-by-reference) parameter is a pure out value and
// should not be passed in from Lisp
// For functions the first argument is <1>

template <typename...> struct pureOutsPack {};

template <int N> struct pureOutValue {};

template <int N> struct adopt {};

enum return_value_policy { reference = return_value_policy_reference_, copy = return_value_policy_copy_ };

struct Keyword {
  std::string m_name;
  std::string m_default;
  Keyword(const std::string& val) : m_name(val), m_default(""){};
  template <class DefaultType> Keyword& operator=(DefaultType x) {
    std::stringstream ss;
    ss << x;
    this->m_default = ss.str();
    return *this;
  }
  Keyword& operator=(const char* x) {
    std::stringstream ss;
    ss << "\"" << x << "\"";
    this->m_default = ss.str();
    return *this;
  }
  Keyword& operator=(bool x) {
    std::stringstream ss;
    if (x) {
      this->m_default = "T";
    } else {
      this->m_default = "NIL";
    }
    return *this;
  }
  std::string asString() const {
    if (this->m_default == "") {
      return this->m_name;
    } else {
      std::stringstream ss;
      ss << "(" << this->m_name;
      ss << " " << this->m_default << ")";
      return ss.str();
    }
  }
};

struct LambdaList {
  std::string m_lambda_list;
  LambdaList(const std::string& val) : m_lambda_list(val){};
};

struct DocString {
  std::string m_doc_string;
  DocString(const std::string& val) : m_doc_string(val){};
};

struct AutoExport {
  bool m_auto_export;
  AutoExport(bool val) : m_auto_export(val){};
};

struct Setf {
  bool m_setf;
  Setf(bool val) : m_setf(val){};
};

inline AutoExport noAutoExport() { return AutoExport(false); }

inline Setf setf() { return Setf(true); }

template <class... PTypes> struct policies {
  std::vector<Keyword> m_keywords;
  std::string m_lambda_list;
  std::string m_doc_string;
  bool m_auto_export = true;
  bool m_setf = false;
  void describe() {
    fmt::print("{}:{} Descibing Policy\n", __FILE__, __LINE__);
    if (this->m_lambda_list != "") {
      fmt::print("lambda_list = {}\n", this->m_lambda_list);
    } else {
      fmt::print("keyword_list = {}\n", this->keywordList());
    }
    fmt::print("Docstring = {}\n", this->m_doc_string.c_str());
  }
  std::string keywordList() const {
    if (this->m_keywords.size() == 0) {
      return "";
    }
    std::stringstream ss;
    ss << "(&key ";
    for (size_t i = 0; i < m_keywords.size(); ++i) {
      ss << m_keywords[i].asString() << " ";
    }
    ss << ")";
    return ss.str();
  }

  std::string lambdaList() const {
    if (this->m_lambda_list != "") {
      return this->m_lambda_list;
    } else {
      return this->keywordList();
    }
  }
  std::string docstring() const { return this->m_doc_string; };
  std::string declares() const { return ""; };
  bool autoExport() const { return m_auto_export; };
  bool setf() const { return m_setf; };
};

template <class Policy, int N> void update_policy(Policy& policy, const adopt<N>& dummy) {
  // Do nothing - this is handled with types
}

template <class Policy, int N> void update_policy(Policy& policy, const outValue<N>& dummy) {
  // Do nothing - this is handled with types
}

template <class Policy, int N> void update_policy(Policy& policy, const pureOutValue<N>& dummy) {
  // Do nothing - this is handled with types
}

template <class Policy> void update_policy(Policy& policy, const return_value_policy& dummy) {
  // Do nothing - this is handled with types
}

template <class Policy> void update_policy(Policy& policy, const Keyword& keyword) { policy.m_keywords.push_back(keyword); }

template <class Policy> void update_policy(Policy& policy, const LambdaList& lambda_list) {
  policy.m_lambda_list = lambda_list.m_lambda_list;
}

template <class Policy> void update_policy(Policy& policy, const DocString& doc_string) {
  policy.m_doc_string = doc_string.m_doc_string;
}

template <class Policy> void update_policy(Policy& policy, const AutoExport& auto_export) {
  policy.m_auto_export = auto_export.m_auto_export;
}

template <class Policy> void update_policy(Policy& policy, const Setf& setf) { policy.m_setf = setf.m_setf; }

template <class Policy> void walk_policy(Policy& policy) {
  // Do nothing
}

template <class Policy, class PType, class... PTypes> void walk_policy(Policy& policy, PType arg, PTypes... args) {
  update_policy(policy, arg);
  walk_policy(policy, args...);
}

}; // namespace clbind

inline clbind::LambdaList operator"" _ll(const char* arg, size_t len) { return clbind::LambdaList(std::string(arg, len)); }

inline clbind::DocString operator"" _docstring(const char* arg, size_t len) { return clbind::DocString(std::string(arg, len)); }

inline clbind::Keyword operator"" _a(const char* arg, size_t len) { return clbind::Keyword(std::string(arg, len)); }
