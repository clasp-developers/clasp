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
// Append a value to a tuple
//
namespace clbind {
template <typename Tuple, int V> struct tuple_append;

template <typename... Ms, int V> struct tuple_append<std::tuple<Ms...>, V> {
  using type = std::tuple<Ms..., std::integral_constant<int, V>>;
};
}; // namespace clbind

// ------------------------------------------------------------
//
// Add up the components of a tuple
//

namespace clbind {

template <class Tuple> struct SumTuple {};

template <class T0, class... Ts> struct SumTuple<std::tuple<T0, Ts...>> {
  enum { value = T0::value + SumTuple<std::tuple<Ts...>>::value };
};

template <> struct SumTuple<std::tuple<>> {
  enum { value = 0 };
};
}; // namespace clbind

// ------------------------------------------------------------
//
// Calculate a running sum and generate a tuple of those sums
// For indices that have std::integral_constant<int, 0> insert 32767 in the tuple.
//

namespace clbind {
template <typename Result, int Sum, typename Source> struct tuple_runsum_impl;

template <typename Result, int Sum, typename... Ss> struct tuple_runsum_impl<Result, Sum, std::tuple<std::integral_constant<int, 0>, Ss...>> {
  using type = typename tuple_runsum_impl<typename tuple_append<Result, 32767>::type, Sum, std::tuple<Ss...>>::type;
};

template <typename Result, int Sum, typename S0, typename... Ss> struct tuple_runsum_impl<Result, Sum, std::tuple<S0, Ss...>> {
  using type =
    typename tuple_runsum_impl<typename tuple_append<Result, Sum + S0::value>::type, Sum + S0::value, std::tuple<Ss...>>::type;
};

template <typename Result, int Sum> struct tuple_runsum_impl<Result, Sum, std::tuple<>> {
  using type = Result;
};

template <int Start, typename Tuple> struct tuple_runsum;

template <int Start, typename Tuple> struct tuple_runsum {
  using type = typename tuple_runsum_impl<std::tuple<>, Start, Tuple>::type;
};

}; // namespace clbind

// ------------------------------------------------------------
//
// Return std::integral_constant<int, 1> if there is a pureOutValue<x> or outValue<x> for
// argument x and return std::integral_constant<int, 0> if there is not.
//

namespace clbind {
namespace detail {
template <int N, typename POL> struct MapOutValuesImpl;

template <int N> struct MapOutValuesImpl<N, policies<>> {
  using type = std::integral_constant<int, 0>;
};

template <int N, typename... Tail> struct MapOutValuesImpl<N, policies<pureOutValue<N>, Tail...>> {
  using type = std::integral_constant<int, 1>;
};

template <int N, typename... Tail> struct MapOutValuesImpl<N, policies<outValue<N>, Tail...>> {
  using type = std::integral_constant<int, 1>;
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
// Return std::integral_constant<int, 1> if there is not a pureOutValue<x> for
// argument x and return std::integral_constant<int, 0> if there is.
//

namespace clbind {
namespace detail {
template <int N, typename... Policies> struct MapNotPureOutValuesImpl;

template <int N> struct MapNotPureOutValuesImpl<N> {
  using type = std::integral_constant<int, 1>;
};

template <int N, typename Head, typename... Tail> struct MapNotPureOutValuesImpl<N, Head, Tail...> {
  using type = typename MapNotPureOutValuesImpl<N, Tail...>::type;
};

template <int N, typename... Tails> struct MapNotPureOutValuesImpl<N, pureOutValue<N>, Tails...> {
  using type = std::integral_constant<int, 0>;
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
// inValueMaskTuple
//
// Construct a tuple of size_t using policies_<...>
// If there is a pureOutValue<x> where x is an index of an inValueMask argument then put 1
//   if not then put 0 in that index of the tuple.
//

// static constexpr int*** iii = MapNotPureOutValues<0,policies<pureOutValue<1>>>::type();

namespace clbind {
namespace detail {
template <typename Policies, typename Sequence> struct inValueMaskTuple_impl {};

template <typename Policies, size_t... Is> struct inValueMaskTuple_impl<Policies, std::integer_sequence<size_t, Is...>> {
  using type = std::tuple<typename MapNotPureOutValues<Is, Policies>::type...>;
};

}; // namespace detail
template <int Num, typename Policies> struct inValueMaskTuple {
  using type = typename detail::inValueMaskTuple_impl<Policies, std::make_index_sequence<Num>>::type;
};

template <int Start, typename MaskTuple> struct inValueIndexTuple {
  using type = typename tuple_runsum<Start, MaskTuple>::type;
};

}; // namespace clbind

// ------------------------------------------------------------
//
// outValueMaskTuple
//
// Construct a tuple of size_t using policies<...>
// If there is a pureOutValue<x> where x is an index of an outValueMask argument then put 1
//   if not then put 0 in that index of the tuple.
//
namespace clbind {
namespace detail {
template <typename Policies, typename Sequence> struct outValueMaskTuple_impl {};

template <typename Policies, size_t... Is> struct outValueMaskTuple_impl<Policies, std::integer_sequence<size_t, Is...>> {
  using type = std::tuple<typename MapOutValues<Is, Policies>::type...>;
};
}; // namespace detail

template <int Num, typename Policies> struct outValueMaskTuple {
  using type = typename detail::outValueMaskTuple_impl<Policies, std::make_index_sequence<Num>>::type;
};

template <int Start, typename MaskTuple> struct outValueIndexTuple {
  using type = typename tuple_runsum<Start, MaskTuple>::type;
};

}; // namespace clbind

// ------------------------------------------------------------
//
// Prepare arguments for calling
//

namespace clbind {

template <typename ValIndex, typename Type> struct prepare_argument {};

template <int Index, typename Type> struct prepare_argument<std::integral_constant<int, Index>, Type> {
  using type = translate::from_object<Type, std::true_type>;
  static translate::from_object<Type, std::true_type> goFrame(gctools::Frame::ElementType* frame) {
    // Return an initialized from_object for the argument
    return translate::from_object<Type, std::true_type>(gctools::smart_ptr<core::T_O>((gctools::Tagged)(frame[Index])));
  }
  template <typename... Targs>
  static translate::from_object<Type,std::true_type> goArg(const std::tuple<Targs...>&& args) {
    // Return an initialized from_object for the argument
    return translate::from_object<Type,std::true_type>(gctools::smart_ptr<core::T_O>((gctools::Tagged)(std::get<Index>(args))));
  }
};

template <typename Type> struct prepare_argument<std::integral_constant<int, 32767>, Type&> {
  using type = translate::from_object<Type&, std::false_type>;
  static type goFrame(gctools::Frame::ElementType* frame) {
    // Return an uninitialized from_object for the argument.
    return type();
  }
  template <typename... Targs>
  static type goArg(const std::tuple<Targs...>&& args) {
    return type();
  }
};

namespace detail {

template <typename TupleIndices, typename SequenceIndices, typename... Args> struct arg_tuple_impl {};

template <typename TupleIndices, size_t... Is, typename... Args>
struct arg_tuple_impl<TupleIndices, std::integer_sequence<size_t, Is...>, Args...> {
  using type = std::tuple<typename prepare_argument<std::tuple_element_t<Is, TupleIndices>, Args>::type...>;
  static type goFrame(gctools::Frame::ElementType* frame) {
    return {(prepare_argument<std::tuple_element_t<Is, TupleIndices>, Args>::goFrame(frame))...};
  }
  template <typename... Targs>
  static type goArgs(const std::tuple<Targs...>&& args) {
    return { (prepare_argument<std::tuple_element_t<Is,TupleIndices>,Args>::goArg(std::forward<const std::tuple<Targs...>>(args)))... };
  }
};
}; // namespace detail

template <int Start, typename Policies, typename... ARGS> struct arg_tuple {
  using maskTuple = typename inValueMaskTuple<sizeof...(ARGS), Policies>::type;
  using indexTuple = typename inValueIndexTuple<Start - 1, maskTuple>::type;
  using type = typename detail::arg_tuple_impl<indexTuple, std::index_sequence_for<ARGS...>, ARGS...>::type;
  static type goFrame(gctools::Frame::ElementType* frame) {
    return detail::arg_tuple_impl<indexTuple, std::index_sequence_for<ARGS...>, ARGS...>::goFrame(frame);
  };
  template <typename... TARGS> // T_O* args
  static type goArgs(TARGS&&...args) {
    return detail::arg_tuple_impl<indexTuple,
                                  std::index_sequence_for<ARGS...>,
                                  ARGS...>::goArgs(std::forward_as_tuple(args...));
  };
};
}; // namespace clbind

// ------------------------------------------------------------
//
// Handle return values
//

namespace clbind {
template <typename Val, typename Policies, size_t Is, typename ArgTuple, typename Type> struct do_return {};

template <int Index, typename Policies, size_t Is, typename ArgTuple, typename Type>
struct do_return<std::integral_constant<int, Index>, Policies, Is, ArgTuple, Type> {
  constexpr static void go(core::T_O** return_values, ArgTuple&& args) {
    // Return an initialized from_object for the argument
    auto preval = std::get<Is>(args)._v;
    auto val =
        translate::to_object<typename Type::DeclareType, typename AdoptPointer<Policies, Is>::type>::convert(preval); // ._val;
    return_values[Index] = val.raw_();
  }
};

template <typename Policies, size_t Is, typename ArgTuple, typename Type>
struct do_return<std::integral_constant<int, 32767>, Policies, Is, ArgTuple, Type> {
  constexpr static void go(core::T_O** return_values, ArgTuple&& args) {
    // Do nothing - this argument does not have a return value
  }
};

template <int Start, typename Policies, typename ArgTuple, typename Seq, typename... Types> struct return_multiple_values {};

template <int Start, typename Policies, typename ArgTuple, size_t... Is, typename... Types>
struct return_multiple_values<Start, Policies, ArgTuple, std::integer_sequence<size_t, Is...>, Types...> {
  using OutValueMaskTuple = typename outValueMaskTuple<sizeof...(Types), Policies>::type;
  using OutValueIndexTuple = typename outValueIndexTuple<Start - 1, OutValueMaskTuple>::type;
  static size_t go(ArgTuple&& args, core::T_O** outputs) {
    (do_return<std::tuple_element_t<Is, OutValueIndexTuple>, Policies, Is, ArgTuple, Types>::go(
         outputs, std::forward<ArgTuple>(args)),
     ...);
    return SumTuple<OutValueMaskTuple>::value;
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
