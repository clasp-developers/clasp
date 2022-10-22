#include <tuple>
#include <utility>
#include <type_traits>
#include <iostream>
#include <functional>


// ============================================================
// ============================================================


template <typename FunctionPtrType>
struct FunctionArgCount {
};

template <typename RT, typename...ARGS>
struct FunctionArgCount< RT(*)(ARGS...)> {
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

template <typename...T>
struct muple {};

template <typename Tuple>
struct mysizeof;

template <typename...Ts>
struct mysizeof<muple<Ts...>> {
  enum { value = sizeof...(Ts)};
};

template <int N>
struct Val {
  enum { value = N };
};
};

// ------------------------------------------------------------
//
// Extract the element of a muple
//
// I didn't end up using this either - but another good example
//
namespace clbind {
template <int N, typename... T>
struct muple_element_impl;

template <>
struct muple_element_impl<0> {
  typedef int type;
};

template <typename T0, typename... T>
struct muple_element_impl<0, T0, T...> {
  typedef T0 type;
  enum { value = T0::value };
};
template <int N, typename T0, typename... T>
struct muple_element_impl<N, T0, T...> {
  typedef typename muple_element_impl<N-1, T...>::type type;
  enum { value = T0::value };
};

template <int N, typename Tuple>
struct muple_element;

template <int N, typename...Ts>
struct muple_element<N,muple<Ts...>> {
  enum { value = muple_element_impl<N,Ts...>::type::value };
  using type = typename muple_element_impl<N,Ts...>::type;
};
};
// ------------------------------------------------------------
//
// Append a value to a muple
//
namespace clbind {
template <typename Muple, int V>
struct muple_append;

template <typename...Ms, int V>
struct muple_append<muple<Ms...>,V> {
  using type = muple<Ms...,Val<V>>;
};
};





// ------------------------------------------------------------
//
// Add up the components of a tuple
//

namespace clbind {

template<class Muple>
struct SumMuple {};

template<class T0,class...Ts>
struct SumMuple<muple<T0,Ts...>> {
  enum { value = T0::value + SumMuple<muple<Ts...>>::value };
};

template <>
struct SumMuple<muple<>> {
  enum { value = 0 };
};
};

// ------------------------------------------------------------
//
// Calculate a running sum and generate a muple of those sums
// For indices that have Val<0> insert 32767 in the muple.
//

namespace clbind {
template <typename Result,int Sum,typename Source>
struct muple_runsum_impl;

template <typename Result,int Sum,typename... Ss>
struct muple_runsum_impl<Result,Sum,muple<Val<0>,Ss...>> {
  using type = typename muple_runsum_impl<typename muple_append<Result,32767>::type,Sum,muple<Ss...>>::type;
};

template <typename Result,int Sum,typename S0,typename... Ss>
struct muple_runsum_impl<Result,Sum,muple<S0,Ss...>> {
  using type = typename muple_runsum_impl<typename muple_append<Result,Sum+S0::value>::type,Sum+S0::value,muple<Ss...>>::type;
};

template <typename Result,int Sum>
struct muple_runsum_impl<Result,Sum,muple<>> {
  using type = Result;
};


template <int Start, typename Muple>
struct muple_runsum;

template <int Start, typename Muple>
struct muple_runsum {
  using type = typename muple_runsum_impl<muple<>, Start, Muple>::type;
};

};

// ------------------------------------------------------------
//
// Dump a muples value

namespace clbind {
template <typename... Ts>
void print_muple(const std::string& name, const muple<Ts...>& muple)
{
  (void)muple;
  std::cout << name << " -> ";
  ((std::cout << " " << Ts::value),...);
  std::cout << std::endl;
}

};

// ------------------------------------------------------------
//
// Return Val<1> if there is a pureOutValue<x> or outValue<x> for
// argument x and return Val<0> if there is not.
//

namespace clbind {
namespace detail {
template <int N, typename POL>
struct MapOutValuesImpl;

template <int N>
struct MapOutValuesImpl<N,policies<>> {
  using type = Val<0>;
};

template <int N, typename... Tail>
struct MapOutValuesImpl<N,policies<pureOutValue<N>, Tail...>> {
  using type = Val<1>;
};

template <int N, typename... Tail>
struct MapOutValuesImpl<N,policies<outValue<N>, Tail...>> {
  using type = Val<1>;
};

template <int N, typename Head, typename... Tail>
struct MapOutValuesImpl<N,policies<Head, Tail...>> {
  using type = typename MapOutValuesImpl<N,policies<Tail...>>::type;
};
};
template <int N, typename Policies>
struct MapOutValues {
  using type = typename detail::MapOutValuesImpl<N,Policies>::type;
};
};

// ------------------------------------------------------------
//
// Return Val<1> if there is not a pureOutValue<x> for
// argument x and return Val<0> if there is.
//

namespace clbind {
namespace detail {
template <int N, typename... Policies>
struct MapNotPureOutValuesImpl;

template <int N>
struct MapNotPureOutValuesImpl<N> {
  using type = Val<1>;
};

template <int N, typename Head, typename... Tail>
struct MapNotPureOutValuesImpl<N,Head, Tail...> {
  using type = typename MapNotPureOutValuesImpl<N,Tail...>::type;
};

template <int N,typename... Tails>
struct MapNotPureOutValuesImpl<N,pureOutValue<N>, Tails...> {
  using type = Val<0>;
};
};
template <int N, typename Policies>
struct MapNotPureOutValues;

template <int N, typename ...Types>
struct MapNotPureOutValues<N,policies<Types...>>{
  using type = typename detail::MapNotPureOutValuesImpl<N,Types...>::type;
};

template <int N>
struct MapNotPureOutValues<N,core::policy::clasp_policy>{
  using type = typename detail::MapNotPureOutValuesImpl<N>::type;
};

};


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
template <int N, typename... Policies>
struct MapPureOutValuesFalseOrTrueImpl;

template <int N>
struct MapPureOutValuesFalseOrTrueImpl<N> {
  using type = std::true_type;
};

template <int N,typename... Tails>
struct MapPureOutValuesFalseOrTrueImpl<N,pureOutValue<N>, Tails...> {
  using type = std::false_type;
};

template <int N, typename Head, typename... Tail>
struct MapPureOutValuesFalseOrTrueImpl<N,Head, Tail...> {
  using type = typename MapPureOutValuesFalseOrTrueImpl<N,Tail...>::type;
};
};
template <int N, typename Policies>
struct MapPureOutValuesFalseOrTrue;

template <int N, typename ...Types>
struct MapPureOutValuesFalseOrTrue<N,policies<Types...>>{
  using type = typename detail::MapPureOutValuesFalseOrTrueImpl<N,Types...>::type;
};
};

// ------------------------------------------------------------
//
// Clasp clbind stuff is simpler - there are no outValue or pureOutValue  policies
//
// Generate a simple sequence starting at Start and running for Num values
//
namespace clbind {
namespace detail {
template <typename Result, int Start, typename Sequence>
struct SimpleMuple_impl {};

template <typename Result, int Start>
struct SimpleMuple_impl<Result,Start,std::integer_sequence<size_t>> {
  using type = Result;
};

template <typename Result, int Start, size_t I0, size_t...Is>
struct SimpleMuple_impl<Result,Start,std::integer_sequence<size_t,I0,Is...>> {
  using type = typename SimpleMuple_impl<typename muple_append<Result,Start+I0>::type, Start, std::integer_sequence<size_t,Is...>>::type;
};
};

template <int Start, size_t Num>
struct SimpleMuple {
  using type = typename detail::SimpleMuple_impl<muple<>,Start,std::make_index_sequence<Num>>::type;
};
};

// ------------------------------------------------------------
//
// inValueMaskMuple
//
// Construct a tuple of size_t using policies_<...>
// If there is a pureOutValue<x> where x is an index of an inValueMask argument then put 1
//   if not then put 0 in that index of the tuple.
//

//static constexpr int*** iii = MapNotPureOutValues<0,policies<pureOutValue<1>>>::type();

namespace clbind {
namespace detail {
template <typename Policies, typename Sequence>
struct inValueMaskMuple_impl {};

template <typename Policies, size_t... Is>
struct inValueMaskMuple_impl<Policies,std::integer_sequence<size_t,Is...>> {
  using type = muple<typename MapNotPureOutValues<Is,Policies>::type...>;
};

};
template <int Num,typename Policies>
struct inValueMaskMuple {
  using type = typename detail::inValueMaskMuple_impl<Policies,std::make_index_sequence<Num>>::type;
};

template <int Start, typename MaskMuple>
struct inValueIndexMuple {
  using type = typename muple_runsum<Start,MaskMuple>::type;
};

};

// ------------------------------------------------------------
//
// inValueTrueFalseMaskMuple
//
// Construct a tuple of size_t using policies_<...>
// If there is a pureOutValue<x> where x is an index of an inValueMask argument then put 1
//   if not then put 0 in that index of the tuple.
//

//static constexpr int*** iii = MapNotPureOutValues<0,policies<pureOutValue<1>>>::type();

namespace clbind {
namespace detail {
template <typename Policies, typename Sequence>
struct inValueTrueFalseMaskMuple_impl {};

template <typename Policies, size_t... Is>
struct inValueTrueFalseMaskMuple_impl<Policies,std::integer_sequence<size_t,Is...>> {
  using type = pureOutsPack<typename MapPureOutValuesFalseOrTrue<Is,Policies>::type...>;
};

};
template <int Num,typename Policies>
struct inValueTrueFalseMaskPack {
  using type = typename detail::inValueTrueFalseMaskMuple_impl<Policies,std::make_index_sequence<Num>>::type;
};

};

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
template <typename Policies, typename Sequence>
struct outValueMaskMuple_impl {};

template <typename Policies, size_t... Is>
struct outValueMaskMuple_impl<Policies,std::integer_sequence<size_t,Is...>> {
  using type = muple<typename MapOutValues<Is,Policies>::type...>;
};
};

template <int Num, typename Policies>
struct outValueMaskMuple {
  using type = typename detail::outValueMaskMuple_impl<Policies,std::make_index_sequence<Num>>::type;
};
  

template <int Start, typename MaskMuple>
struct outValueIndexMuple {
  using type = typename muple_runsum<Start,MaskMuple>::type;
};

};


// ------------------------------------------------------------
//
// Prepare arguments for calling
//

namespace clbind {

template <typename ValIndex, typename Type>
struct prepare_argument {};

template <int Index, typename Type>
struct prepare_argument<Val<Index>,Type> {
  using type = translate::from_object<Type,std::true_type>;
  static translate::from_object<Type,std::true_type> goFrame(gctools::Frame::ElementType* frame) {
    // Return an initialized from_object for the argument
    return translate::from_object<Type,std::true_type>(gctools::smart_ptr<core::T_O>((gctools::Tagged)(frame[Index])));
  }
#if 0
  static translate::from_object<Type,std::true_type> goArg(Type&& arg) {
    // Return an initialized from_object for the argument
    return translate::from_object<Type,std::true_type>(gctools::smart_ptr<core::T_O>((gctools::Tagged)arg));
  }
#endif
};

template <typename Type>
struct prepare_argument<Val<32767>,Type> {
  using type = translate::from_object<Type,std::false_type>;
  static translate::from_object<Type,std::false_type> goFrame(gctools::Frame::ElementType* frame) {
    // Return an initialized from_object for the argument
    return translate::from_object<Type,std::false_type>(nil<core::T_O>());
  }
#if 0
  static translate::from_object<Type,std::false_type> goFrame(Type arg) {
    // Return an initialized from_object for the argument
    return translate::from_object<Type,std::false_type>(nil<core::T_O>());
  }
#endif
};

namespace detail {

template <typename MupleIndices, typename SequenceIndices, typename...Args>
struct arg_tuple_impl {};

template <typename MupleIndices, size_t...Is, typename...Args>
struct arg_tuple_impl<MupleIndices,std::integer_sequence<size_t,Is...>,Args...> {
  using type = std::tuple<typename prepare_argument<typename muple_element<Is,MupleIndices>::type,Args>::type...>;
  static type goFrame(gctools::Frame::ElementType* frame) {
    return { (prepare_argument<typename muple_element<Is,MupleIndices>::type,Args>::goFrame(frame))... };
  }
#if 0
  static type goArgs(Args&&...args) {
    return { (prepare_argument<typename muple_element<Is,MupleIndices>::type,Args>::goArgs(std::forward<Args>(args)))... };
  }
#endif
};
};

template <int Start, typename Policies, typename...ARGS>
struct arg_tuple {
  using maskMuple = typename inValueMaskMuple<sizeof...(ARGS),Policies>::type;
  using indexMuple = typename inValueIndexMuple<Start-1,maskMuple>::type;
  using type = typename detail::arg_tuple_impl<indexMuple,
                                               std::index_sequence_for<ARGS...>,
                                               ARGS...>::type;
  static type goFrame(gctools::Frame::ElementType* frame) {
    return detail::arg_tuple_impl<indexMuple,
                                  std::index_sequence_for<ARGS...>,
                                  ARGS...>::goFrame(frame);
  };
#if 0
  static type goArgs(Args&&...args) {
    return detail::arg_tuple_impl<indexMuple,
                                  std::index_sequence_for<ARGS...>,
                                  ARGS...>::goArgs(std::forward<Args>(args)...);
  };
#endif
};
};

// ------------------------------------------------------------
//
// Handle return values
//

namespace clbind {
template <typename Val, typename Policies, size_t Is, typename ArgTuple, typename Type>
struct do_return {};

template <int Index, typename Policies, size_t Is, typename ArgTuple, typename Type>
struct do_return<Val<Index>,Policies,Is,ArgTuple,Type> {
  constexpr static void go(core::T_O** return_values, ArgTuple&& args) {
    // Return an initialized from_object for the argument
    auto preval = std::get<Is>(args)._v;
    auto val = translate::to_object<typename Type::DeclareType,typename AdoptPointer<Policies,Is>::type>::convert(preval); // ._val;
    return_values[Index] = val.raw_();
  }
};

template <typename Policies,size_t Is, typename ArgTuple,typename Type>
struct do_return<Val<32767>,Policies,Is,ArgTuple,Type> {
  constexpr static void go(core::T_O** return_values, ArgTuple&& args) {
    // Do nothing - this argument does not have a return value
  }
};

template <int Start, typename Policies, typename ArgTuple,typename Seq,typename...Types>
struct return_multiple_values {};

template <int Start, typename Policies, typename ArgTuple,size_t...Is,typename...Types>
struct return_multiple_values<Start,Policies,ArgTuple,std::integer_sequence<size_t,Is...>,Types...> {
  using OutValueMaskMuple = typename outValueMaskMuple<sizeof...(Types),Policies>::type;
  using OutValueIndexMuple = typename outValueIndexMuple<Start-1,OutValueMaskMuple>::type;
  static size_t go(ArgTuple&& args, core::T_O** outputs) {
    (do_return<typename muple_element<Is,OutValueIndexMuple>::type,Policies,Is,ArgTuple,Types>::go(outputs,std::forward<ArgTuple>(args)) , ...);
    return SumMuple<OutValueMaskMuple>::value;
  }
};



template <int Start, typename Policies, typename ArgTuple, typename ArgTupleForTypes, typename Seq>
struct tuple_return_multiple_values {};

template <int Start, typename Policies, typename ArgTuple,typename...Types, size_t...Is>
struct tuple_return_multiple_values<Start,Policies,ArgTuple,std::tuple<Types...>,std::integer_sequence<size_t,Is...>> {
  using OutValueMaskMuple = typename outValueMaskMuple<sizeof...(Types),Policies>::type;
  using OutValueIndexMuple = typename outValueIndexMuple<Start-1,OutValueMaskMuple>::type;
  static size_t go(ArgTuple&& args, core::T_O** outputs) {
    (do_return<typename muple_element<Is,OutValueIndexMuple>::type,Policies,Is,ArgTuple,Types>::go(outputs,std::forward<ArgTuple>(args)) , ...);
    return SumMuple<OutValueMaskMuple>::value;
  }
};


};
// ============================================================
//
// apply 
namespace clbind {
namespace detail {
template <class F, class ArgTuple, std::size_t... I>
decltype(auto) apply_impl( F&& f, ArgTuple&& t, std::index_sequence<I...> )
{
  // This is where the MAGIC happens!
  // Apply the function to the arguments in the tuple of from_object objects
//  return std::invoke(std::forward<F>(f), (std::get<I>(std::forward<Args>(t))._v)...);
  return std::invoke(std::forward<F>(f), (std::get<I>(t)._v)...);
}
} // namespace detail 

template <class F, class ArgTuple>
decltype(auto) apply(F&& f, ArgTuple&& t)
{
  //  int*** iii = std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{};
  return detail::apply_impl(std::forward<F>(f), std::forward<ArgTuple>(t),
                            std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{});
}
};

// ============================================================
// constructor_apply

// ============================================================
//
// apply 
namespace clbind {
namespace detail {
template <class ConstructType, class Args, std::size_t... I>
constexpr decltype(auto) constructor_apply_impl( /*std::tuple<Args...>*/Args&& t, std::index_sequence<I...> )
{
  // This is where the MAGIC happens!
  // Apply the function to the arguments in the tuple of from_object objects
  ConstructType* obj(new ConstructType(std::get<I>(std::forward<Args>(t))._v...));
  return obj;
}
} // namespace detail 

template <class ConstructType, class ArgTuple>
constexpr decltype(auto) constructor_apply(ArgTuple&& t)
{
  return detail::constructor_apply_impl<ConstructType>(std::forward<ArgTuple>(t),
                            std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{});
}
};

// ============================================================

namespace clbind {
namespace detail {
template <class F, class OT, class... Args, std::size_t... I>
constexpr decltype(auto) method_apply_impl( F&& f, OT&& object, std::tuple<Args...>&& t, std::index_sequence<I...> )
{
  return std::invoke(std::forward<F>(f), std::forward<OT>(object), (get<I>(std::forward<std::tuple<Args...>>(t))._v)...);
  // Note: std::invoke is a C++17 feature
}
} // namespace detail

template <class F, class OT, class ArgTuple>
constexpr decltype(auto) method_apply(F&& f, OT&& object, ArgTuple&& t)
{
//  int*** iii = std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{};
  return detail::method_apply_impl(std::forward<F>(f), std::forward<OT>(object), std::forward<ArgTuple>(t),
                                   std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{});
}
};

// ============================================================
//
// clasp external_method_apply
//




namespace clbind {
namespace detail {
template <class F, class OT, class... Args, std::size_t... I>
constexpr decltype(auto) external_method_apply_impl( F&& f, OT* objectP, std::tuple<Args...>&& t, std::index_sequence<I...> )
{
  return std::invoke(std::forward<F>(f), objectP->wrappedPtr(), (get<I>(std::forward<std::tuple<Args...>>(t))._v)...);
  // Note: std::invoke is a C++17 feature
}
} // namespace detail

template <class F, class OT, class ArgTuple>
constexpr decltype(auto) external_method_apply(F&& f, OT* objectP, ArgTuple&& t)
{
//  int*** iii = std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{};
  return detail::external_method_apply_impl(std::forward<F>(f), objectP, std::forward<ArgTuple>(t),
                                            std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{});
}
};

// ============================================================
//
// clbind_external_method_apply
//


namespace clbind {
namespace detail {
template <class F, class OT, class... Args, std::size_t... I>
constexpr decltype(auto) clbind_external_method_apply_impl( F&& f, OT* objectP, std::tuple<Args...>&& t, std::index_sequence<I...> )
{
  return std::invoke(std::forward<F>(f), objectP, (get<I>(std::forward<std::tuple<Args...>>(t))._v)...);
  // Note: std::invoke is a C++17 feature
}
} // namespace detail

template <class F, class OT, class ArgTuple>
constexpr decltype(auto) clbind_external_method_apply(F&& f, OT* objectP, ArgTuple&& t)
{
//  int*** iii = std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{};
  return detail::clbind_external_method_apply_impl(std::forward<F>(f), objectP, std::forward<ArgTuple>(t),
                                            std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{});
}
};


// ============================================================


#ifdef TEST_APPLY
struct WrapperBase {
  virtual size_t entry(size_t numArgs, gctools::Frame::ElementType* inputs, gctools::Frame::ElementType* outputs)
  {
    std::cout << "WrapperBase::entry - entered entry point numArgs -> " << numArgs << "\n";
    std::cout << "You shouldn't call this one - you want the derived class\n";
    return 0;
  };
};

template <typename Func,typename Policies>
struct Wrapper : public WrapperBase {};

template <typename Policies, typename RT, typename... Args>
struct Wrapper<RT(*)(Args...),Policies> : public WrapperBase {
  typedef RT(*FuncType)(Args...);
  FuncType fptr;
  Wrapper(FuncType func, Policies policies) : fptr(func) {};
  virtual size_t entry(size_t numArgs, gctools::Frame::ElementType* inputs, gctools::Frame::ElementType* outputs) {
    std::cout << "The correct entry point numArgs = " << numArgs << "\n";
    auto args = clbind::arg_tuple<-1,Policies,Args...>::go(inputs);
    RT ret0 = clbind::apply(this->fptr,(args));
    outputs[0] = ret0;
    return clbind::return_multiple_values<0,Policies,decltype(args),std::index_sequence_for<Args...>,Args...>::go(std::move(args),outputs);
  };
};

template <typename Func,typename Policies>
WrapperBase* wrap(const std::string& name, Func func, Policies policies) {
  return new Wrapper<Func,Policies>(func,policies);
}

int weirdo(int x, int y, int& subtract, int& divide, int z) {
  std::cout << "Entered weirdo with x=" << x << " y=" << y << " z=" << z << std::endl;
  subtract = x-y+z;
  if (y==0) return 0;
  divide = x/y+z;
  int result0 = x+y+z;
  std::cout << "Returning " << result0 << " " << subtract << " " << divide << std::endl;
  return result0;
}

int main() // int argc, const char* argv[])
{
  std::cout << "Define the policies for a function that has two pure out values - as in..." << std::endl;
  std::cout << " bool weirdo(int x, int y, int& subtract, int& divide, int z)" << std::endl;
  std::cout << "This returns false if y==0 otherwise it" << std::endl;
  std::cout << " calculates subtract = x-y+z and divide = x/y+z" << std::endl;
  using pols = policies<pureOutValue<2>,pureOutValue<3>>; // index 0 is first arg
  using bar = clbind::inValueMaskMuple<5,pols>::type; // 5 arguments
  std::cout << "So the policies as a template type are:  policies<pureOutValue<2>,pureOutValue<3>>" << std::endl;
  std::cout << "  convert this into 1|0 mask - counting starts at 0" << std::endl;
  print_muple("bar", bar());
  using bar_runsum = clbind::muple_runsum<-1,bar>::type;
  std::cout << "Calculate index for argument (start at 0) and 32767 if none is passed in" << std::endl;
  print_muple("bar_runsum", bar_runsum());
  std::cout << "From Common Lisp this will be (weirdo x y z) -> (values return subtract divide)" << std::endl;
  std::cout << "  Map x to arg[0], y to arg[1] and z to arg[4] and arg[3] and arg[4] are uninitialized." << std::endl;

  using pols2 = policies<outValue<2>,pureOutValue<3>>; // index 0 is first arg
  using aaa = clbind::outValueMaskMuple<5,pols2>::type; // 5 arguments
  print_muple("aaa", aaa());
  using aaa_runsum = clbind::muple_runsum<0,aaa>::type;
  print_muple("aaa_runsum", aaa_runsum());

  using simple = clbind::SimpleMuple<9,10>::type;
  print_muple("simple",simple());

  WrapperBase* wrapped_weirdo = wrap("weirdo",&weirdo,policies<pureOutValue<2>,pureOutValue<3>>());

  int inputs[10];
  int outputs[10];
  gctools::Frame frame_inputs(inputs);
  gctools::Frame frame_outputs(outputs);
  for ( int i=0; i<9; i++ ) {
    frame_inputs[i] = -1;
    frame_outputs[i] = -1;
  }
  frame_inputs[0] = 10;
  frame_inputs[1] = 2;
  frame_inputs[2] = 20;
  for ( int i=0; i<9; i++ ) {
    std::cout << "frame_inputs["<<i<<"] -> " << frame_inputs[i] << std::endl;
  }
  int ret0 = wrapped_weirdo->entry(2,&frame_inputs[0],&frame_outputs[0]);
  std::cout << "ret0 -> " << ret0 << "\n";
  for ( int i=0; i<9; i++ ) {
    std::cout << "frame_outputs["<<i<<"] -> " << frame_outputs[i] << std::endl;
  }
}
#endif











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
