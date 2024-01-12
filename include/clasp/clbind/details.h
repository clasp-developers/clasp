#pragma once

/*
    File: details.h
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
#include <clasp/clbind/policies.h>
#include <clasp/clbind/external_policies.h>
#include <type_traits>

namespace clbind {

namespace detail {

struct null_type {};

template <typename POL, typename T> struct IndexOf;

template <typename T> struct IndexOf<policies<>, T> {
  enum { value = -1 };
};

template <typename T> struct IndexOf<core::policy::clasp_policy, T> {
  enum { value = -1 };
};

template <typename Head, typename... Tail> struct IndexOf<policies<Head, Tail...>, Head> {
  enum { value = 0 };
};

template <typename Head, typename... Tail, typename T> struct IndexOf<policies<Head, Tail...>, T> {
private:
  enum { temp = IndexOf<policies<Tail...>, T>::value };

public:
  enum { value = temp == -1 ? -1 : 1 + temp };
};
}; // namespace detail
template <typename Pol, typename T> using IndexOf = typename detail::IndexOf<Pol, T>;

template <typename Set, typename T> struct Contains_ {
  using type = std::conditional_t<(IndexOf<Set, T>::value >= 0), std::true_type, std::false_type>;
};

template <typename Policies, int N> struct is_outValue {
  using type = std::disjunction<typename Contains_<Policies, pureOutValue<N>>::type, typename Contains_<Policies, outValue<N>>::type>;
};

template <typename Policies, int N> struct is_pureOutValue {
  using type = typename Contains_<Policies, pureOutValue<N>>::type;
};

template <typename Policies, int N> struct AdoptPointer {
  typedef std::conditional_t<Contains_<Policies, adopt<N>>::type::value, translate::adopt_pointer, translate::dont_adopt_pointer>
      type;
};

namespace detail {
template <typename POL, int Shift> struct GatherPureOutValues;

template <int Shift> struct GatherPureOutValues<policies<>, Shift> {
  static std::set<int> gather() {
    std::set<int> result;
    return result;
  };
};

template <typename... Tail, int N, int Shift> struct GatherPureOutValues<policies<pureOutValue<N>, Tail...>, Shift> {
  static std::set<int> gather() {
    std::set<int> result(GatherPureOutValues<policies<Tail...>, Shift>::gather());
    result.insert(N + Shift);
    return result;
  }
};

template <typename Head, typename... Tail, int Shift> struct GatherPureOutValues<policies<Head, Tail...>, Shift> {
  static std::set<int> gather() { return GatherPureOutValues<policies<Tail...>, Shift>::gather(); };
};
}; // namespace detail
template <typename Policies, int Shift> using GatherPureOutValues = typename detail::GatherPureOutValues<Policies, Shift>;

}; // namespace clbind
