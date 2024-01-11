#pragma once

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

#include <tuple>
#include <utility>
#include <type_traits>
#include <iostream>
#include <functional>

// ============================================================
// ============================================================

template <typename FunctionPtrType> struct FunctionArgCount {};

template <typename RT, typename... ARGS> struct FunctionArgCount<RT (*)(ARGS...)> {
  enum { value = sizeof...(ARGS) };
};

// -------
// Calculate number of arguments to function
//

// ------------------------------------------------------------
//
// Define our tuple - a muple!
// muple<> is empty
// muple<Val<1>,Val<0>...> is a loaded muple
//

namespace clbind {

template <typename... T> struct muple {};

template <typename Tuple> struct mysizeof;

template <typename... Ts> struct mysizeof<muple<Ts...>> {
  enum { value = sizeof...(Ts) };
};

template <int N> struct Val {
  enum { value = N };
};
}; // namespace clbind

// ------------------------------------------------------------
//
// Extract the element of a muple
//
// I didn't end up using this either - but another good example
//
namespace clbind {
template <int N, typename... T> struct muple_element_impl;

template <> struct muple_element_impl<0> {
  typedef int type;
};

template <typename T0, typename... T> struct muple_element_impl<0, T0, T...> {
  typedef T0 type;
  enum { value = T0::value };
};
template <int N, typename T0, typename... T> struct muple_element_impl<N, T0, T...> {
  typedef typename muple_element_impl<N - 1, T...>::type type;
  enum { value = T0::value };
};

template <int N, typename Tuple> struct muple_element;

template <int N, typename... Ts> struct muple_element<N, muple<Ts...>> {
  enum { value = muple_element_impl<N, Ts...>::type::value };
  using type = typename muple_element_impl<N, Ts...>::type;
};
}; // namespace clbind
// ------------------------------------------------------------
//
// Append a value to a muple
//
namespace clbind {
template <typename Muple, int V> struct muple_append;

template <typename... Ms, int V> struct muple_append<muple<Ms...>, V> {
  using type = muple<Ms..., Val<V>>;
};
}; // namespace clbind

// ------------------------------------------------------------
//
// Add up the components of a tuple
//

namespace clbind {

template <class Muple> struct SumMuple {};

template <class T0, class... Ts> struct SumMuple<muple<T0, Ts...>> {
  enum { value = T0::value + SumMuple<muple<Ts...>>::value };
};

template <> struct SumMuple<muple<>> {
  enum { value = 0 };
};
}; // namespace clbind

// ------------------------------------------------------------
//
// Calculate a running sum and generate a muple of those sums
// For indices that have Val<0> insert 32767 in the muple.
//

namespace clbind {
template <typename Result, int Sum, typename Source> struct muple_runsum_impl;

template <typename Result, int Sum, typename... Ss> struct muple_runsum_impl<Result, Sum, muple<Val<0>, Ss...>> {
  using type = typename muple_runsum_impl<typename muple_append<Result, 32767>::type, Sum, muple<Ss...>>::type;
};

template <typename Result, int Sum, typename S0, typename... Ss> struct muple_runsum_impl<Result, Sum, muple<S0, Ss...>> {
  using type =
      typename muple_runsum_impl<typename muple_append<Result, Sum + S0::value>::type, Sum + S0::value, muple<Ss...>>::type;
};

template <typename Result, int Sum> struct muple_runsum_impl<Result, Sum, muple<>> {
  using type = Result;
};

template <int Start, typename Muple> struct muple_runsum;

template <int Start, typename Muple> struct muple_runsum {
  using type = typename muple_runsum_impl<muple<>, Start, Muple>::type;
};

}; // namespace clbind

// ------------------------------------------------------------
//
// Dump a muples value

namespace clbind {
template <typename... Ts> void print_muple(const std::string& name, const muple<Ts...>& muple) {
  (void)muple;
  std::cout << name << " -> ";
  ((std::cout << " " << Ts::value), ...);
  std::cout << std::endl;
}

}; // namespace clbind

// ------------------------------------------------------------
//
// Return Val<1> if there is a pureOutValue<x> or outValue<x> for
// argument x and return Val<0> if there is not.
//

namespace clbind {
namespace detail {
template <int N, typename POL> struct MapOutValuesImpl;

template <int N> struct MapOutValuesImpl<N, policies<>> {
  using type = Val<0>;
};

template <int N, typename... Tail> struct MapOutValuesImpl<N, policies<pureOutValue<N>, Tail...>> {
  using type = Val<1>;
};

template <int N, typename... Tail> struct MapOutValuesImpl<N, policies<outValue<N>, Tail...>> {
  using type = Val<1>;
};

template <int N, typename Head, typename... Tail> struct MapOutValuesImpl<N, policies<Head, Tail...>> {
  using type = typename MapOutValuesImpl<N, policies<Tail...>>::type;
};
}; // namespace detail
template <int N, typename Policies> struct MapOutValues {
  using type = typename detail::MapOutValuesImpl<N, Policies>::type;
};
}; // namespace clbind

// ------------------------------------------------------------
//
// Return Val<1> if there is not a pureOutValue<x> for
// argument x and return Val<0> if there is.
//

namespace clbind {
namespace detail {
template <int N, typename... Policies> struct MapNotPureOutValuesImpl;

template <int N> struct MapNotPureOutValuesImpl<N> {
  using type = Val<1>;
};

template <int N, typename Head, typename... Tail> struct MapNotPureOutValuesImpl<N, Head, Tail...> {
  using type = typename MapNotPureOutValuesImpl<N, Tail...>::type;
};

template <int N, typename... Tails> struct MapNotPureOutValuesImpl<N, pureOutValue<N>, Tails...> {
  using type = Val<0>;
};
}; // namespace detail
template <int N, typename Policies> struct MapNotPureOutValues;

template <int N, typename... Types> struct MapNotPureOutValues<N, policies<Types...>> {
  using type = typename detail::MapNotPureOutValuesImpl<N, Types...>::type;
};

template <int N> struct MapNotPureOutValues<N, core::policy::clasp_policy> {
  using type = typename detail::MapNotPureOutValuesImpl<N>::type;
};

}; // namespace clbind

// ------------------------------------------------------------
//
// MapPureOutValuesFalseOrTrue
// Calculate the value std::false_value or std::true_value from
//   whether or not the first index N appears in the policies<...> parameter pack
//   as pureOutValue<
// Map <0,policies<pureOutValue<0>> to std::false_value
// and <0,policies<pureOutValue<1>> to std::true_value
//

namespace clbind {
namespace detail {
template <int N, typename... Policies> struct MapPureOutValuesFalseOrTrueImpl;

template <int N> struct MapPureOutValuesFalseOrTrueImpl<N> {
  using type = std::true_type;
};

template <int N, typename... Tails> struct MapPureOutValuesFalseOrTrueImpl<N, pureOutValue<N>, Tails...> {
  using type = std::false_type;
};

template <int N, typename Head, typename... Tail> struct MapPureOutValuesFalseOrTrueImpl<N, Head, Tail...> {
  using type = typename MapPureOutValuesFalseOrTrueImpl<N, Tail...>::type;
};
}; // namespace detail
template <int N, typename Policies> struct MapPureOutValuesFalseOrTrue;

template <int N, typename... Types> struct MapPureOutValuesFalseOrTrue<N, policies<Types...>> {
  using type = typename detail::MapPureOutValuesFalseOrTrueImpl<N, Types...>::type;
};
}; // namespace clbind

// ------------------------------------------------------------
//
// Clasp clbind stuff is simpler - there are no outValue or pureOutValue  policies
//
// Generate a simple sequence starting at Start and running for Num values
//
namespace clbind {
namespace detail {
template <typename Result, int Start, typename Sequence> struct SimpleMuple_impl {};

template <typename Result, int Start> struct SimpleMuple_impl<Result, Start, std::integer_sequence<size_t>> {
  using type = Result;
};

template <typename Result, int Start, size_t I0, size_t... Is>
struct SimpleMuple_impl<Result, Start, std::integer_sequence<size_t, I0, Is...>> {
  using type =
      typename SimpleMuple_impl<typename muple_append<Result, Start + I0>::type, Start, std::integer_sequence<size_t, Is...>>::type;
};
}; // namespace detail

template <int Start, size_t Num> struct SimpleMuple {
  using type = typename detail::SimpleMuple_impl<muple<>, Start, std::make_index_sequence<Num>>::type;
};
}; // namespace clbind

// ------------------------------------------------------------
//
// inValueMaskMuple
//
// Construct a tuple of size_t using policies_<...>
// If there is a pureOutValue<x> where x is an index of an inValueMask argument then put 1
//   if not then put 0 in that index of the tuple.
//

// static constexpr int*** iii = MapNotPureOutValues<0,policies<pureOutValue<1>>>::type();

namespace clbind {
namespace detail {
template <typename Policies, typename Sequence> struct inValueMaskMuple_impl {};

template <typename Policies, size_t... Is> struct inValueMaskMuple_impl<Policies, std::integer_sequence<size_t, Is...>> {
  using type = muple<typename MapNotPureOutValues<Is, Policies>::type...>;
};

}; // namespace detail
template <int Num, typename Policies> struct inValueMaskMuple {
  using type = typename detail::inValueMaskMuple_impl<Policies, std::make_index_sequence<Num>>::type;
};

template <int Start, typename MaskMuple> struct inValueIndexMuple {
  using type = typename muple_runsum<Start, MaskMuple>::type;
};

}; // namespace clbind

// ------------------------------------------------------------
//
// inValueTrueFalseMaskMuple
//
// Construct a tuple of size_t using policies_<...>
// If there is a pureOutValue<x> where x is an index of an inValueMask argument then put 1
//   if not then put 0 in that index of the tuple.
//

// static constexpr int*** iii = MapNotPureOutValues<0,policies<pureOutValue<1>>>::type();

namespace clbind {
namespace detail {
template <typename Policies, typename Sequence> struct inValueTrueFalseMaskMuple_impl {};

template <typename Policies, size_t... Is> struct inValueTrueFalseMaskMuple_impl<Policies, std::integer_sequence<size_t, Is...>> {
  using type = pureOutsPack<typename MapPureOutValuesFalseOrTrue<Is, Policies>::type...>;
};

}; // namespace detail
template <int Num, typename Policies> struct inValueTrueFalseMaskPack {
  using type = typename detail::inValueTrueFalseMaskMuple_impl<Policies, std::make_index_sequence<Num>>::type;
};

}; // namespace clbind

// ------------------------------------------------------------
//
// outValueMaskMuple
//
// Construct a tuple of size_t using policies<...>
// If there is a pureOutValue<x> where x is an index of an outValueMask argument then put 1
//   if not then put 0 in that index of the tuple.
//
namespace clbind {
namespace detail {
template <typename Policies, typename Sequence> struct outValueMaskMuple_impl {};

template <typename Policies, size_t... Is> struct outValueMaskMuple_impl<Policies, std::integer_sequence<size_t, Is...>> {
  using type = muple<typename MapOutValues<Is, Policies>::type...>;
};
}; // namespace detail

template <int Num, typename Policies> struct outValueMaskMuple {
  using type = typename detail::outValueMaskMuple_impl<Policies, std::make_index_sequence<Num>>::type;
};

template <int Start, typename MaskMuple> struct outValueIndexMuple {
  using type = typename muple_runsum<Start, MaskMuple>::type;
};

}; // namespace clbind

// ------------------------------------------------------------
//
// Prepare arguments for calling
//

namespace clbind {

template <typename ValIndex, typename Type> struct prepare_argument {};

template <int Index, typename Type> struct prepare_argument<Val<Index>, Type> {
  using type = translate::from_object<Type, std::true_type>;
  static translate::from_object<Type, std::true_type> goFrame(gctools::Frame::ElementType* frame) {
    // Return an initialized from_object for the argument
    return translate::from_object<Type, std::true_type>(gctools::smart_ptr<core::T_O>((gctools::Tagged)(frame[Index])));
  }
#if 0
  static translate::from_object<Type,std::true_type> goArg(Type&& arg) {
    // Return an initialized from_object for the argument
    return translate::from_object<Type,std::true_type>(gctools::smart_ptr<core::T_O>((gctools::Tagged)arg));
  }
#endif
};

template <typename Type> struct prepare_argument<Val<32767>, Type> {
  using type = translate::from_object<Type, std::false_type>;
  static translate::from_object<Type, std::false_type> goFrame(gctools::Frame::ElementType* frame) {
    // Return an initialized from_object for the argument
    return translate::from_object<Type, std::false_type>(nil<core::T_O>());
  }
#if 0
  static translate::from_object<Type,std::false_type> goFrame(Type arg) {
    // Return an initialized from_object for the argument
    return translate::from_object<Type,std::false_type>(nil<core::T_O>());
  }
#endif
};

namespace detail {

template <typename MupleIndices, typename SequenceIndices, typename... Args> struct arg_tuple_impl {};

template <typename MupleIndices, size_t... Is, typename... Args>
struct arg_tuple_impl<MupleIndices, std::integer_sequence<size_t, Is...>, Args...> {
  using type = std::tuple<typename prepare_argument<typename muple_element<Is, MupleIndices>::type, Args>::type...>;
  static type goFrame(gctools::Frame::ElementType* frame) {
    return {(prepare_argument<typename muple_element<Is, MupleIndices>::type, Args>::goFrame(frame))...};
  }
#if 0
  static type goArgs(Args&&...args) {
    return { (prepare_argument<typename muple_element<Is,MupleIndices>::type,Args>::goArgs(std::forward<Args>(args)))... };
  }
#endif
};
}; // namespace detail

template <int Start, typename Policies, typename... ARGS> struct arg_tuple {
  using maskMuple = typename inValueMaskMuple<sizeof...(ARGS), Policies>::type;
  using indexMuple = typename inValueIndexMuple<Start - 1, maskMuple>::type;
  using type = typename detail::arg_tuple_impl<indexMuple, std::index_sequence_for<ARGS...>, ARGS...>::type;
  static type goFrame(gctools::Frame::ElementType* frame) {
    return detail::arg_tuple_impl<indexMuple, std::index_sequence_for<ARGS...>, ARGS...>::goFrame(frame);
  };
#if 0
  static type goArgs(Args&&...args) {
    return detail::arg_tuple_impl<indexMuple,
                                  std::index_sequence_for<ARGS...>,
                                  ARGS...>::goArgs(std::forward<Args>(args)...);
  };
#endif
};
}; // namespace clbind

// ------------------------------------------------------------
//
// Handle return values
//

namespace clbind {
template <typename Val, typename Policies, size_t Is, typename ArgTuple, typename Type> struct do_return {};

template <int Index, typename Policies, size_t Is, typename ArgTuple, typename Type>
struct do_return<Val<Index>, Policies, Is, ArgTuple, Type> {
  constexpr static void go(core::T_O** return_values, ArgTuple&& args) {
    // Return an initialized from_object for the argument
    auto preval = std::get<Is>(args)._v;
    auto val =
        translate::to_object<typename Type::DeclareType, typename AdoptPointer<Policies, Is>::type>::convert(preval); // ._val;
    return_values[Index] = val.raw_();
  }
};

template <typename Policies, size_t Is, typename ArgTuple, typename Type>
struct do_return<Val<32767>, Policies, Is, ArgTuple, Type> {
  constexpr static void go(core::T_O** return_values, ArgTuple&& args) {
    // Do nothing - this argument does not have a return value
  }
};

template <int Start, typename Policies, typename ArgTuple, typename Seq, typename... Types> struct return_multiple_values {};

template <int Start, typename Policies, typename ArgTuple, size_t... Is, typename... Types>
struct return_multiple_values<Start, Policies, ArgTuple, std::integer_sequence<size_t, Is...>, Types...> {
  using OutValueMaskMuple = typename outValueMaskMuple<sizeof...(Types), Policies>::type;
  using OutValueIndexMuple = typename outValueIndexMuple<Start - 1, OutValueMaskMuple>::type;
  static size_t go(ArgTuple&& args, core::T_O** outputs) {
    (do_return<typename muple_element<Is, OutValueIndexMuple>::type, Policies, Is, ArgTuple, Types>::go(
         outputs, std::forward<ArgTuple>(args)),
     ...);
    return SumMuple<OutValueMaskMuple>::value;
  }
};

template <int Start, typename Policies, typename ArgTuple, typename ArgTupleForTypes, typename Seq>
struct tuple_return_multiple_values {};

template <int Start, typename Policies, typename ArgTuple, typename... Types, size_t... Is>
struct tuple_return_multiple_values<Start, Policies, ArgTuple, std::tuple<Types...>, std::integer_sequence<size_t, Is...>> {
  using OutValueMaskMuple = typename outValueMaskMuple<sizeof...(Types), Policies>::type;
  using OutValueIndexMuple = typename outValueIndexMuple<Start - 1, OutValueMaskMuple>::type;
  static size_t go(ArgTuple&& args, core::T_O** outputs) {
    (do_return<typename muple_element<Is, OutValueIndexMuple>::type, Policies, Is, ArgTuple, Types>::go(
         outputs, std::forward<ArgTuple>(args)),
     ...);
    return SumMuple<OutValueMaskMuple>::value;
  }
};

}; // namespace clbind
// ============================================================
//
// apply
namespace clbind {
namespace detail {
template <class F, class ArgTuple, std::size_t... I> decltype(auto) apply_impl(F&& f, ArgTuple&& t, std::index_sequence<I...>) {
  // This is where the MAGIC happens!
  // Apply the function to the arguments in the tuple of from_object objects
  //  return std::invoke(std::forward<F>(f), (std::get<I>(std::forward<Args>(t))._v)...);
  return std::invoke(std::forward<F>(f), (std::get<I>(t)._v)...);
}
} // namespace detail

template <class F, class ArgTuple> decltype(auto) apply(F&& f, ArgTuple&& t) {
  //  int*** iii = std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{};
  return detail::apply_impl(std::forward<F>(f), std::forward<ArgTuple>(t),
                            std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{});
}
}; // namespace clbind

// ============================================================
// constructor_apply

// ============================================================
//
// apply
namespace clbind {
namespace detail {
template <class ConstructType, class Args, std::size_t... I>
constexpr decltype(auto) constructor_apply_impl(/*std::tuple<Args...>*/ Args&& t, std::index_sequence<I...>) {
  // This is where the MAGIC happens!
  // Apply the function to the arguments in the tuple of from_object objects
  ConstructType* obj(new ConstructType(std::get<I>(std::forward<Args>(t))._v...));
  return obj;
}
} // namespace detail

template <class ConstructType, class ArgTuple> constexpr decltype(auto) constructor_apply(ArgTuple&& t) {
  return detail::constructor_apply_impl<ConstructType>(std::forward<ArgTuple>(t),
                                                       std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{});
}
}; // namespace clbind

// ============================================================

namespace clbind {
namespace detail {
template <class F, class OT, class... Args, std::size_t... I>
constexpr decltype(auto) method_apply_impl(F&& f, OT&& object, std::tuple<Args...>&& t, std::index_sequence<I...>) {
  return std::invoke(std::forward<F>(f), std::forward<OT>(object), (get<I>(std::forward<std::tuple<Args...>>(t))._v)...);
  // Note: std::invoke is a C++17 feature
}
} // namespace detail

template <class F, class OT, class ArgTuple> constexpr decltype(auto) method_apply(F&& f, OT&& object, ArgTuple&& t) {
  //  int*** iii = std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{};
  return detail::method_apply_impl(std::forward<F>(f), std::forward<OT>(object), std::forward<ArgTuple>(t),
                                   std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{});
}
}; // namespace clbind

// ============================================================
//
// clasp external_method_apply
//

namespace clbind {
namespace detail {
template <class F, class OT, class... Args, std::size_t... I>
constexpr decltype(auto) external_method_apply_impl(F&& f, OT* objectP, std::tuple<Args...>&& t, std::index_sequence<I...>) {
  return std::invoke(std::forward<F>(f), objectP->wrappedPtr(), (get<I>(std::forward<std::tuple<Args...>>(t))._v)...);
  // Note: std::invoke is a C++17 feature
}
} // namespace detail

template <class F, class OT, class ArgTuple> constexpr decltype(auto) external_method_apply(F&& f, OT* objectP, ArgTuple&& t) {
  //  int*** iii = std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{};
  return detail::external_method_apply_impl(std::forward<F>(f), objectP, std::forward<ArgTuple>(t),
                                            std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{});
}
}; // namespace clbind

// ============================================================
//
// clbind_external_method_apply
//

namespace clbind {
namespace detail {
template <class F, class OT, class... Args, std::size_t... I>
constexpr decltype(auto) clbind_external_method_apply_impl(F&& f, OT* objectP, std::tuple<Args...>&& t, std::index_sequence<I...>) {
  return std::invoke(std::forward<F>(f), objectP, (get<I>(std::forward<std::tuple<Args...>>(t))._v)...);
  // Note: std::invoke is a C++17 feature
}
} // namespace detail

template <class F, class OT, class ArgTuple>
constexpr decltype(auto) clbind_external_method_apply(F&& f, OT* objectP, ArgTuple&& t) {
  //  int*** iii = std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{};
  return detail::clbind_external_method_apply_impl(std::forward<F>(f), objectP, std::forward<ArgTuple>(t),
                                                   std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{});
}
}; // namespace clbind
