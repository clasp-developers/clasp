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
#ifndef clbind_details_H
#define clbind_details_H

#include <clasp/clbind/policies.h>
#include <clasp/clbind/external_policies.h>

namespace clbind {

namespace detail {

struct null_type {};

template <int>
struct Int2Type {};

template <typename POL, typename T>
struct IndexOf;

template <typename T>
struct IndexOf<policies<>, T> {
  enum { value = -1 };
};

 template <typename T>
   struct IndexOf<core::policy::clasp_policy, T> {
   enum { value = -1 };
 };

template <typename Head, typename... Tail>
struct IndexOf<policies<Head, Tail...>, Head> {
  enum { value = 0 };
};

template <typename Head, typename... Tail, typename T>
struct IndexOf<policies<Head, Tail...>, T> {
private:
  enum { temp = IndexOf<policies<Tail...>, T>::value };

public:
  enum { value = temp == -1 ? -1 : 1 + temp };
};
};
template <typename Pol, typename T>
using IndexOf = typename detail::IndexOf<Pol, T>;

template <typename Cond>
struct IncWhen {
  static void go(ArgArray &args) {
    args++;
  }
};

template <>
struct IncWhen<std::false_type> {
  static void go(ArgArray &args) {
    // Do nothing
  }
};

template <typename OutValue, typename Adopt, typename ARG>
void ReturnValueWhen(core::MultipleValues &mv, int &idx, OutValue, Adopt, ARG &&a) {
  mv.emplace_back(translate::to_object<ARG, Adopt>::convert(a));
  ++idx;
  //        mv.valueSet(idx++,translate::to_object<ARG,Adopt>::convert(a));
}

template <typename Adopt, typename ARG>
void ReturnValueWhen(core::MultipleValues &mv, int &idx, std::false_type, Adopt, ARG &&a) {
  // Do nothing
}

template <bool B, typename X, typename Y>
struct if_c { typedef X type; };
template <typename X, typename Y>
struct if_c<false, X, Y> { typedef Y type; };

template <typename Set, typename T>
struct Contains_ {
  typedef typename if_c<(IndexOf<Set, T>::value >= 0),
                        std::true_type,
                        std::false_type>::type type;
};

template <typename Set, typename T>
struct DoesNotContain_ {
  typedef typename if_c<(IndexOf<Set, T>::value >= 0),
                        std::false_type,
                        std::true_type>::type type;
};

template <typename... Tail>
struct or_ {
  typedef std::true_type type;
};

template <typename... Tail>
struct or_<std::false_type, Tail...> {
  typedef typename or_<Tail...>::type type;
};

template <>
struct or_<> {
  typedef std::false_type type;
};

template <typename Policies, int N>
struct is_outValue {
  typedef typename or_<typename Contains_<Policies, pureOutValue<N>>::type, typename Contains_<Policies, outValue<N>>::type>::type type;
};

template <typename Policies, int N>
struct is_pureOutValue {
  typedef typename Contains_<Policies, pureOutValue<N>>::type type;
};

template <typename Policies, int N>
struct not_pureOutValue {
  typedef typename std::negation<typename Contains_<Policies, pureOutValue<N>>::type>::type type;
};

template <typename Policies, int N>
struct AdoptPointer {
  typedef typename boost::mpl::if_<typename Contains_<Policies, adopt<N>>::type, translate::adopt_pointer, translate::dont_adopt_pointer>::type type;
};

//
// Counts instances of pureOutValue<N> where N is an int in the list of policies<...>
//
#if 0
    namespace detail {
        template <typename POL> 
        struct CountPureOutValues;

        template <>
        struct CountPureOutValues< policies<> >
        {
            enum { value = 0 };
        };

        template <typename... Tail, int N>
        struct CountPureOutValues< policies<pureOutValue<N>,Tail...> >
        {
            enum { value = CountPureOutValues<policies<Tail...> >::value + 1 };
        };

        template <typename Head, typename... Tail>
        struct CountPureOutValues< policies<Head,Tail...> >
        {
            enum { value = CountPureOutValues<policies<Tail...> >::value };
        };

    };
    template <typename Policies>
    using CountPureOutValues = typename detail::CountPureOutValues<Policies>;
#endif

namespace detail {
template <typename POL, int Shift>
struct GatherPureOutValues;

template <int Shift>
struct GatherPureOutValues<policies<>, Shift> {
  static std::set<int> gather() {
    std::set<int> result;
    return result;
  };
};

template <typename... Tail, int N, int Shift>
struct GatherPureOutValues<policies<pureOutValue<N>, Tail...>, Shift> {
  static std::set<int> gather() {
    std::set<int> result(GatherPureOutValues<policies<Tail...>, Shift>::gather());
    result.insert(N + Shift);
    return result;
  }
};

template <typename Head, typename... Tail, int Shift>
struct GatherPureOutValues<policies<Head, Tail...>, Shift> {
  static std::set<int> gather() {
    return GatherPureOutValues<policies<Tail...>, Shift>::gather();
  };
};
};
template <typename Policies, int Shift>
using GatherPureOutValues = typename detail::GatherPureOutValues<Policies, Shift>;

using ssize_t = std::size_t;
using size_t  = std::size_t;



namespace detail {

template <bool B, typename T = void> using enable_if_t = typename std::enable_if<B, T>::type;
template <bool B, typename T, typename F> using conditional_t = typename std::conditional<B, T, F>::type;
template <typename T> using remove_cv_t = typename std::remove_cv<T>::type;
template <typename T> using remove_reference_t = typename std::remove_reference<T>::type;

template <typename...> struct void_t_impl { using type = void; };
template <typename... Ts> using void_t = typename void_t_impl<Ts...>::type;

/// Check if T looks like an input iterator
template <typename T, typename = void> struct is_input_iterator : std::false_type {};
template <typename T>
struct is_input_iterator<T, void_t<decltype(*std::declval<T &>()), decltype(++std::declval<T &>())>>
    : std::true_type {};


// Adaptor for converting arbitrary container arguments into a vector; implicitly convertible from
// any standard container (or C-style array) supporting std::begin/std::end, any singleton
// arithmetic type (if T is arithmetic), or explicitly constructible from an iterator pair.
template <typename T>
class any_container {
    std::vector<T> v;
public:
    any_container() = default;

    // Can construct from a pair of iterators
  template <typename It, typename = enable_if_t<is_input_iterator<It>::value>>
    any_container(It first, It last) : v(first, last) { }

    // Implicit conversion constructor from any arbitrary container type with values convertible to T
    template <typename Container, typename = enable_if_t<std::is_convertible<decltype(*std::begin(std::declval<const Container &>())), T>::value>>
    any_container(const Container &c) : any_container(std::begin(c), std::end(c)) { }

    // initializer_list's aren't deducible, so don't get matched by the above template; we need this
    // to explicitly allow implicit conversion from one:
    template <typename TIn, typename = enable_if_t<std::is_convertible<TIn, T>::value>>
    any_container(const std::initializer_list<TIn> &c) : any_container(c.begin(), c.end()) { }

    // Avoid copying if given an rvalue vector of the correct type.
    any_container(std::vector<T> &&v) : v(std::move(v)) { }

    // Moves the vector out of an rvalue any_container
    operator std::vector<T> &&() && { return std::move(v); }

    // Dereferencing obtains a reference to the underlying vector
    std::vector<T> &operator*() { return v; }
    const std::vector<T> &operator*() const { return v; }

    // -> lets you call methods on the underlying vector
    std::vector<T> *operator->() { return &v; }
    const std::vector<T> *operator->() const { return &v; }
};



};
};
#endif
