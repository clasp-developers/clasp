#ifndef apply_H
#define apply_H

#include <functional>
#include <clasp/core/lispDefinitions.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/activationFrame.h>
#include <clasp/clbind/details.h>
#include <clasp/clbind/external_policies.h>
#include <clasp/core/translators.h>
#include <clasp/core/glue.h>


//#define DEBUG_METHOIDS 1


namespace clbind {

// Actual implementation for a type
template <std::size_t _index, typename Policies, typename T>
class _arg_tuple_impl : public translate::from_object<T&,typename not_pureOutValue<Policies,_index>::type>
{
public:
  _arg_tuple_impl(gctools::Frame::ElementType* frame) : translate::from_object<T&,typename not_pureOutValue<Policies,_index>::type >(gctools::smart_ptr<core::T_O>((gctools::Tagged)(frame[_index]))) {
  };
    
#if 0
  _arg_tuple_impl(T &&v) {}
#endif
    
  T &get()
  {
    return this->_v;
  }
  template <typename IsOut=std::false_type, typename Adopt>
  void maybe_write_multiple_value_(core::MultipleValues& multiple_values)
  {
//          printf("%s:%d:%s NOT IsOut for _index: %lu\n", __FILE__, __LINE__, __FUNCTION__, _index);
//        printf("%s:%d    multiple_values.getSize() -> %lu\n", __FILE__, __LINE__, multiple_values.getSize());
  }
  template <>
  void maybe_write_multiple_value_<std::true_type,translate::dont_adopt_pointer>(core::MultipleValues& multiple_values)
  {
//        printf("%s:%d:%s IsOut/Dont-Adopt for _index: %lu\n", __FILE__, __LINE__, __FUNCTION__, _index);
    core::T_sp ret = translate::to_object<T,translate::dont_adopt_pointer>::convert(this->_v);
//        printf("%s:%d  ret -> %s\n", __FILE__, __LINE__, _rep_(ret).c_str());
    multiple_values.emplace_back(ret);
//        printf("%s:%d    multiple_values.getSize() -> %lu\n", __FILE__, __LINE__, multiple_values.getSize());
  }

  template <>
  void maybe_write_multiple_value_<std::true_type,translate::adopt_pointer>(core::MultipleValues& multiple_values)
  {
//        printf("%s:%d:%s IsOut/Adopt for _index: %lu\n", __FILE__, __LINE__, __FUNCTION__, _index);
    core::T_sp ret = translate::to_object<T,translate::dont_adopt_pointer>::convert(this->_v);
//        printf("%s:%d  ret -> %s\n", __FILE__, __LINE__, _rep_(ret).c_str());
    multiple_values.emplace_back(ret);
//        printf("%s:%d    multiple_values.getSize() -> %lu\n", __FILE__, __LINE__, multiple_values.getSize());
  }
    
};

// General template, will be used only when there is no arguments
template <std::size_t _index, typename Policies, typename... types>
class _arg_tuple_recurr_base
{
public:
  _arg_tuple_recurr_base(gctools::Frame::ElementType* frame) {};
  void write_multiple_values(core::MultipleValues& multiple_values) {};
};

// This is a partial specialization, so as long as there is at least one argument
// this specialization is preferred to the _arg_tuple_recurr_base<std::size_t, typename ...types>
template <std::size_t _index, typename Policies, typename L, typename... types>
class _arg_tuple_recurr_base<_index, Policies, L, types...> : public _arg_tuple_impl<_index, Policies, L>, public _arg_tuple_recurr_base<_index + 1, Policies, types...>
{
private:
  typedef _arg_tuple_impl<_index, Policies, L> head_base;
  typedef _arg_tuple_recurr_base<_index + 1, Policies, types...> tail_base;
public:
  _arg_tuple_recurr_base(gctools::Frame::ElementType* frame) : _arg_tuple_impl<_index, Policies, L>(frame),_arg_tuple_recurr_base<_index + 1, Policies, types...>(frame)
  {}

  void write_multiple_values(core::MultipleValues& multiple_values)
  {
//      printf("%s:%d:%s  is_outValue _index -> %s\n", __FILE__, __LINE__, __FUNCTION__, typeid(typename is_outValue<Policies,_index>::type).name());
//      printf("%s:%d:%s  Adopt -> %s\n", __FILE__, __LINE__, __FUNCTION__, typeid(typename AdoptPointer<Policies,_index>::type).name());
    this->head_base::template maybe_write_multiple_value_<typename is_outValue<Policies,_index>::type,typename AdoptPointer<Policies,_index>::type>(multiple_values);
    this->tail_base::write_multiple_values(multiple_values);
  }
};

template <typename Policies, typename... types>
class arg_tuple : public _arg_tuple_recurr_base<0, Policies, types...>
{
public:
  
    // The constructor pulls the values out of an array of T_O*
  template <typename... CArgs>
  arg_tuple(gctools::Frame::ElementType* frame) : _arg_tuple_recurr_base<0, Policies, types...>(frame)
  {
  }
#if 0
  template <typename... Args>
  friend bool operator==(arg_tuple<Args...> &t1, arg_tuple<Args...> &t2);
#endif
};

// extract_type_at is a class that, given a list of types and an index, defines a type member
// with the type of the index given from the list (zero based index).
// E.g. extract<1, int, double, float>::type == double
// For this we define ::type recursively, until we hit index zero, at that point there is a specialization
// that defines the member ::type, and stops the recursion
template <std::size_t index, typename Policies, typename L, typename... Args>
struct extract_type_at
{
  using type = typename extract_type_at<index - 1, Policies, Args...>::type;
};

// This is the stop type. If the index is zero, we define the member type to be the correspondent type
template <typename Policies, typename L, typename... Args>
struct extract_type_at<0, Policies, L, Args...>
{
  using type = L;
};

// Method to get the value of a arg_tuple, given an index
// We cast the arg_tuple to the base class that corresponds to the index
// and type for that index
template <std::size_t index, typename Policies, typename... Args>
auto &get(arg_tuple<Policies,Args...> &t)
{
  return (static_cast<_arg_tuple_impl<index, Policies, typename extract_type_at<index, Policies, Args...>::type> &>(t)).get();
}


namespace detail {
template <class F, class ArgTuple, std::size_t... I>
constexpr decltype(auto) apply_impl( F&& f, ArgTuple&& t, std::index_sequence<I...> )
{
  return std::invoke(std::forward<F>(f), get<I>(std::forward<ArgTuple>(t))...);
  // Note: std::invoke is a C++17 feature
}
} // namespace detail

template <class F, class ArgTuple>
constexpr decltype(auto) apply(F&& f, ArgTuple&& t)
{
//  int*** iii = std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{};
  return detail::apply_impl(std::forward<F>(f), std::forward<ArgTuple>(t),
                            std::make_index_sequence<std::tuple_size_v<std::decay_t<ArgTuple>>>{});
}

};

template <typename Policies, typename... Types>
struct std::tuple_size<clbind::arg_tuple<Policies,Types...>> : public std::integral_constant<std::size_t,sizeof...(Types)> {};


namespace clbind {

/*! Handle the apply of the function to the translated arguments in a Tuple and the return of the results
 */

/*! The prototype for apply_and_return  - this should handle the most general case required by clbind
    It should write outValue(s) and pureOutValue(s) into the multiple value return vector.
 */

template <typename RT, typename Pols, typename Func, typename Tuple>
struct apply_and_return {
  static gc::return_type go(core::MultipleValues& returnValues, Func fn, Tuple& tuple) {
    RT retval = clbind::apply(fn,tuple);
    returnValues.setSize(0);
    core::T_sp rv = translate::to_object<RT,typename AdoptPointer<Pols,result>::type >::convert(retval); 
    returnValues.emplace_back(rv);
    tuple.write_multiple_values(returnValues);
    return LCC_RETURN(rv.raw_(),returnValues.getSize());
  }
};


template <typename RT, typename Func, typename Tuple>
struct apply_and_return<RT,core::policy::clasp,Func,Tuple> {
  static gc::return_type go(core::MultipleValues& returnValues, Func fn, Tuple& tuple) {
    RT retval = clbind::apply(fn,tuple);
    printf("%s:%d Returning from apply_and_return\n", __FILE__, __LINE__ );
    return Values(translate::to_object<RT>::convert(retval));
  }
};

template <typename RT, typename Func, typename Tuple>
struct apply_and_return<gctools::multiple_values<RT>,core::policy::clasp,Func,Tuple> {
  static gc::return_type go(core::MultipleValues& returnValues, Func fn, Tuple& tuple) {
    gctools::multiple_values<RT> retval = clbind::apply(fn,tuple);
    printf("%s:%d Returning from apply_and_return gctools::multiple_values<RT>\n", __FILE__, __LINE__ );
    return retval.as_return_type();
  }
};


 template < typename Func, typename Tuple>
   struct apply_and_return<void,core::policy::clasp,Func,Tuple> {
  static LCC_RETURN go(core::MultipleValues& returnValues, Func fn, Tuple& tuple) {
    ::clbind::apply(fn,tuple);
    return Values0<core::T_O>();
  }
};


template <typename Pols, typename Func, typename Tuple>
struct apply_and_return<void,Pols,Func,Tuple> {
  static LCC_RETURN go(core::MultipleValues& returnValues, Func fn, Tuple& tuple) {
    apply(fn,tuple);
    return Values0<core::T_O>();
  }
};

template <typename RT, typename Pols, typename Func, typename Tuple>
struct apply_and_return<std::unique_ptr<RT>,Pols,Func,Tuple> {
  static gc::return_type go(core::MultipleValues& returnValues, Func fn, Tuple& tuple) {
    std::unique_ptr<RT> retval = clbind::apply(fn,tuple);
    returnValues.setSize(0);
        // When returning unique_ptr always adopt it
    core::T_sp rv = translate::to_object<std::unique_ptr<RT>,translate::adopt_pointer>::convert(std::move(retval)); 
    returnValues.emplace_back(rv);
    tuple.write_multiple_values(returnValues);
    return LCC_RETURN(rv.raw_(),returnValues.getSize());
  }
};

template <class T, class ArgTuple, size_t... Is>
T* construct_from_tuple_impl(ArgTuple&& tuple, std::index_sequence<Is...> ) {
  return new T{get<Is>(std::forward<ArgTuple>(tuple))...};
}

template <class T, class ArgTuple>
T* construct_from_tuple(ArgTuple& tuple) {
  return construct_from_tuple_impl<T>(tuple,
                                      std::make_index_sequence<std::tuple_size<std::decay_t<ArgTuple>>::value>{}
                                      );
}


};


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
#endif
