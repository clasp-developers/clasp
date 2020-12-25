#ifndef apply_H
#define apply_H
#include <tuple>
#include <utility>
#include <iostream>
#include <functional>

#include <clasp/core/lispDefinitions.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/activationFrame.h>
#include <clasp/clbind/details.h>
#include <clasp/clbind/external_policies.h>
#include <clasp/core/translators.h>
#include <clasp/core/glue.h>


#include <clasp/clbind/apply.cc>


namespace clbind {

/*! Handle the apply of the function to the translated arguments in a Tuple and the return of the results
 */

/*! The prototype for apply_and_return  - this should handle the most general case required by clbind
    It should write outValue(s) and pureOutValue(s) into the multiple value return vector.
 */
template <typename RT, typename Policies, typename Func, typename Tuple>
struct apply_and_return {};

template <typename RT, typename Policies, typename Func, typename...Args>
struct apply_and_return<RT,Policies,Func,std::tuple<Args...>> {
  using tuple_type = std::tuple<Args...>;
  static gctools::return_type go(core::MultipleValues& returnValues, Func&& fn, tuple_type&& tuple) {
    RT ret0 = clbind::apply(std::forward<Func>(fn),std::forward<tuple_type>(tuple));
    core::T_sp tret0 = translate::to_object<RT,typename AdoptPointer<Policies,result>::type >::convert(ret0);
    size_t num_returns = 1 + clbind::return_multiple_values<0,Policies,decltype(tuple),std::index_sequence_for<Args...>,Args...>::go(std::forward<tuple_type>(tuple),returnValues.returnValues(0));
    printf("%s:%d  RT apply_and_return  returning %lu multiple values\n", __FILE__, __LINE__, num_returns );
    gc::return_type result (tret0.raw_(),num_returns);
    return result;
  }
};

template <typename Policies, typename Func, typename...Args>
struct apply_and_return<void,Policies,Func,std::tuple<Args...>> {
  using tuple_type = std::tuple<Args...>;
  static gctools::return_type go(core::MultipleValues& returnValues, Func&& fn, tuple_type&& tuple) {
    clbind::apply(std::forward<Func>(fn),std::forward<tuple_type>(tuple));
    size_t num_returns = clbind::return_multiple_values<-1,Policies,decltype(tuple),std::index_sequence_for<Args...>,Args...>::go(std::forward<tuple_type>(tuple),returnValues.returnValues(0));
    printf("%s:%d  void apply_and_return  returning %lu multiple values\n", __FILE__, __LINE__, num_returns );
    core::T_mv result;
    result.readFromMultipleValue0();
    result.set_number_of_values(num_returns);
    return result.as_return_type();
  }
};

// ============================================================
//
//
template <typename Wrapper,typename Policies, typename Func, typename Tuple>
struct constructor_apply_and_return {};

template <typename WrapperType, typename Policies, typename ConstructType, typename...Args>
struct constructor_apply_and_return<WrapperType,Policies,ConstructType,std::tuple<Args...>> {
  using tuple_type = std::tuple<Args...>;
  static gctools::return_type go(core::MultipleValues& returnValues, tuple_type&& tuple) {
    ConstructType* naked_ptr = clbind::constructor_apply<ConstructType>(std::forward<tuple_type>(tuple));
    gctools::smart_ptr<WrapperType> ret = WrapperType::make_wrapper(naked_ptr,reg::registered_class<ConstructType>::id);   
    size_t num_returns = 1 + clbind::return_multiple_values<0,Policies,decltype(tuple),std::index_sequence_for<Args...>,Args...>::go(std::forward<tuple_type>(tuple),returnValues.returnValues(0));
    printf("%s:%d  void constructor_apply_and_return  returning %lu multiple values\n", __FILE__, __LINE__, num_returns );
    gc::return_type result(ret.raw_(),num_returns);
    return result;
  }
};


// ============================================================


template <typename RT, typename Policies, typename Func, typename Tuple>
struct clasp_apply_and_return {};


template <typename RT, typename Func, typename Tuple>
struct clasp_apply_and_return<RT,core::policy::clasp,Func,Tuple> {
  static gc::return_type go(core::MultipleValues& returnValues, Func&& fn, Tuple&& tuple) {
    RT retval = clbind::apply(std::forward<Func>(fn),std::forward<Tuple>(tuple)); // why forward?
    core::T_sp tretval = translate::to_object<RT>::convert(retval);
//    printf("%s:%d Returning from apply_and_return value-> %s\n", __FILE__, __LINE__, _rep_(tretval).c_str() );
    return Values(tretval);
  }
};

template <typename RT, typename Func, typename Tuple>
struct clasp_apply_and_return<gctools::multiple_values<RT>,core::policy::clasp,Func,Tuple> {
  static gc::return_type go(core::MultipleValues& returnValues, Func&& fn, Tuple&& tuple) {
    gctools::multiple_values<RT> retval = clbind::apply(std::forward<Func>(fn),std::forward<Tuple>(tuple));
//    printf("%s:%d Returning from apply_and_return gctools::multiple_values<RT> - first value -> %s\n", __FILE__, __LINE__, _rep_(retval).c_str() );
    return retval.as_return_type();
  }
};


 template < typename Func, typename Tuple>
 struct clasp_apply_and_return<void,core::policy::clasp,Func,Tuple> {
  static LCC_RETURN go(core::MultipleValues& returnValues, Func&& fn, Tuple&& tuple) {
    clbind::apply(std::forward<Func>(fn),std::forward<Tuple>(tuple));
    return Values0<core::T_O>();
  }
};


template <typename Policies, typename Func, typename Tuple>
struct apply_and_return<void,Policies,Func,Tuple> {
  static LCC_RETURN go(core::MultipleValues& returnValues, Func&& fn, Tuple&& tuple) {
    clbind::apply(std::forward<Func>(fn),std::forward<Tuple>(tuple));
    return Values0<core::T_O>();
  }
};

// clbind apply_and_return - this needs to handle multiple-values
template <typename RT, typename Policies, typename Func, typename Tuple>
struct apply_and_return<std::unique_ptr<RT>,Policies,Func,Tuple> {
  static gc::return_type go(core::MultipleValues& returnValues, Func&& fn, Tuple&& tuple) {
    std::unique_ptr<RT> retval = clbind::apply(std::move(fn),std::move(tuple));
    returnValues.setSize(0);
        // When returning unique_ptr always adopt it
    core::T_sp rv = translate::to_object<std::unique_ptr<RT>,translate::adopt_pointer>::convert(std::move(retval)); 
    returnValues.emplace_back(rv);
    tuple.write_multiple_values(returnValues);
    return LCC_RETURN(rv.raw_(),returnValues.getSize());
  }
};

};


namespace clbind {
template <typename RT, typename Policies, typename MethodType, typename OT, typename Tuple>
struct method_apply_and_return {
  static gc::return_type go(core::MultipleValues& returnValues, MethodType&& mptr, OT&& object, Tuple&& tuple) {
    RT retval = clbind::method_apply(std::forward<MethodType>(mptr), std::forward<OT>(object), std::forward<Tuple>(tuple) );
    core::T_sp rv = translate::to_object<RT,typename AdoptPointer<Policies,result>::type >::convert(retval); 
    returnValues.emplace_back(rv);
    printf("%s:%d Write the multiple values in tuple here\n", __FILE__, __LINE__ );
//    tuple.write_multiple_values<Policies>(returnValues);
    return LCC_RETURN(rv.raw_(),returnValues.getSize());
  }
};


template <typename MethodType, typename OT, typename Tuple>
struct method_apply_and_return<void,core::policy::clasp,MethodType,OT,Tuple> {
  static gc::return_type go(core::MultipleValues& returnValues, MethodType&& mptr, OT&& object, Tuple&& tuple) {
    clbind::method_apply(std::forward<MethodType>(mptr), std::forward<OT>(object), std::forward<Tuple>(tuple) );
    return Values0<core::T_O>();
  }
};

template <typename RT, typename MethodType, typename OT, typename Tuple>
struct method_apply_and_return<RT,core::policy::clasp,MethodType,OT,Tuple> {
  static gc::return_type go(core::MultipleValues& returnValues, MethodType&& mptr, OT&& object, Tuple&& tuple) {
    RT retval = clbind::method_apply(std::forward<MethodType>(mptr), std::forward<OT>(object), std::forward<Tuple>(tuple) );
    core::T_sp rv = translate::to_object<RT>::convert(retval);
    return Values(rv);
  }
};

template <typename RT, typename MethodType, typename OT, typename Tuple>
struct method_apply_and_return<gctools::multiple_values<RT>,core::policy::clasp,MethodType,OT,Tuple> {
  static gc::return_type go(core::MultipleValues& returnValues, MethodType&& mptr, OT&& object, Tuple&& tuple) {
    gctools::multiple_values<RT> retval = clbind::method_apply(std::forward<MethodType>(mptr), std::forward<OT>(object), std::forward<Tuple>(tuple) );
    return retval.as_return_type();
  }
};



};

// ============================================================
//
// clasp external_method_apply_and_return
//
namespace clbind {
template <typename Policies, typename RT, typename MethodType, typename OTExternal, typename Tuple>
struct external_method_apply_and_return {};

template <typename Policies, typename MethodType, typename OTExternal, typename...Args>
struct external_method_apply_and_return<Policies,void,MethodType,OTExternal,std::tuple<Args...>> {
  using tuple_type = std::tuple<Args...>;
  static gc::return_type go(core::MultipleValues& returnValues, MethodType&& mptr, OTExternal* objectP, tuple_type&& tuple) {
    clbind::external_method_apply(std::forward<MethodType>(mptr), objectP, std::forward<tuple_type>(tuple) );
    // Pass -1 as first template argument - it means that first out value will write to multiple value return vector at position 0
    size_t num_returns = clbind::return_multiple_values<-1,Policies,decltype(tuple),std::index_sequence_for<Args...>,Args...>::go(std::forward<tuple_type>(tuple),returnValues.returnValues(0));
//    printf("%s:%d  void external_method_apply_and_return  returning %lu multiple values\n", __FILE__, __LINE__, num_returns );
    core::T_mv result;
    result.readFromMultipleValue0();
    result.set_number_of_values(num_returns);
    return result.as_return_type();
  }
};

template <typename Policies, typename RT, typename MethodType, typename OTExternal, typename...Args>
struct external_method_apply_and_return<Policies,RT,MethodType,OTExternal,std::tuple<Args...>> {
  using tuple_type = std::tuple<Args...>;
  static gc::return_type go(core::MultipleValues& returnValues, MethodType&& mptr, OTExternal* objectP, tuple_type&& tuple) {
    RT ret0 = clbind::external_method_apply(std::forward<MethodType>(mptr),objectP, std::forward<tuple_type>(tuple) );
    core::T_sp tret0 = translate::to_object<RT,typename clbind::AdoptPointer<Policies,result>::type>::convert(ret0);
//    printf("%s:%d Returning first return value: %p\n", __FILE__, __LINE__, tret0.raw_()) ;
    // Pass 0 as first template argument - it means that first out value will write to multiple value return vector at position 1
    size_t num_returns = 1 + clbind::return_multiple_values<0,Policies,decltype(tuple),std::index_sequence_for<Args...>,Args...>::go(std::forward<tuple_type>(tuple),returnValues.returnValues(0));
//    printf("%s:%d  RT external_method_apply_and_return  returning %lu multiple values\n", __FILE__, __LINE__, num_returns );
    gc::return_type result (tret0.raw_(),num_returns);
    return result;
  }
};

};

// ============================================================
//
// clbind external_method_apply_and_return
//
namespace clbind {
template <typename Policies, typename RT, typename MethodType, typename OT, typename Tuple>
struct clbind_external_method_apply_and_return {};

template <typename Policies, typename MethodType, typename OT, typename...Args>
struct clbind_external_method_apply_and_return<Policies,void,MethodType,OT,std::tuple<Args...>> {
  using tuple_type = std::tuple<Args...>;
  static gc::return_type go(core::MultipleValues& returnValues, MethodType&& mptr, OT objectP, tuple_type&& tuple) {
    clbind::clbind_external_method_apply(std::forward<MethodType>(mptr), objectP, std::forward<tuple_type>(tuple) );
    // Pass -1 as first template argument - it means that first out value will write to multiple value return vector at position 0
    size_t num_returns = clbind::return_multiple_values<-1,Policies,decltype(tuple),std::index_sequence_for<Args...>,Args...>::go(std::forward<tuple_type>(tuple),returnValues.returnValues(0));
//    printf("%s:%d  void clbind_external_method_apply_and_return  returning %lu multiple values\n", __FILE__, __LINE__, num_returns );
    core::T_mv result;
    result.readFromMultipleValue0();
    result.set_number_of_values(num_returns);
    return result.as_return_type();
  }
};

template <typename Policies, typename RT, typename MethodType, typename OT, typename...Args>
struct clbind_external_method_apply_and_return<Policies,RT,MethodType,OT,std::tuple<Args...>> {
  using tuple_type = std::tuple<Args...>;
  static gc::return_type go(core::MultipleValues& returnValues, MethodType&& mptr, OT objectP, tuple_type&& tuple) {
    RT ret0 = clbind::clbind_external_method_apply(std::forward<MethodType>(mptr),objectP, std::forward<tuple_type>(tuple) );
    core::T_sp tret0 = translate::to_object<RT,typename clbind::AdoptPointer<Policies,result>::type>::convert(ret0);
//    printf("%s:%d Returning first return value: %p\n", __FILE__, __LINE__, tret0.raw_()) ;
    // Pass 0 as first template argument - it means that first out value will write to multiple value return vector at position 1
    size_t num_returns = 1 + clbind::return_multiple_values<0,Policies,decltype(tuple),std::index_sequence_for<Args...>,Args...>::go(std::forward<tuple_type>(tuple),returnValues.returnValues(0));
//    printf("%s:%d  RT clbind_external_method_apply_and_return  returning %lu multiple values\n", __FILE__, __LINE__, num_returns );
    gc::return_type result (tret0.raw_(),num_returns);
    return result;
  }
};

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
