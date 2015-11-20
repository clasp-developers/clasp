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
};
#endif
