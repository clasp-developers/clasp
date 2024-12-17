
/*
    File: exposeClasses1.cc
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

#ifndef SCRAPING // #endif at bottom

#include <clasp/gctools/exposeCommon.h>

template <class TheClass> void set_one_static_class_symbol(core::BootStrapCoreSymbolMap* symbols, const std::string& full_name) {
  std::string orig_package_part, orig_symbol_part;
  core::colon_split(full_name, orig_package_part, orig_symbol_part);
  std::string package_part, symbol_part;
  package_part = core::lispify_symbol_name(orig_package_part);
  symbol_part = core::lispify_symbol_name(orig_symbol_part);
  //  printf("%s:%d set_one_static_class_symbol --> %s:%s\n", __FILE__, __LINE__, package_part.c_str(), symbol_part.c_str() );
  core::SymbolStorage store;
  bool found = symbols->find_symbol(package_part, symbol_part, store);
  if (!found) {
    printf("%s:%d ERROR!!!! The static class symbol %s was not found orig_symbol_part=|%s| symbol_part=|%s|!\n", __FILE__, __LINE__,
           full_name.c_str(), orig_symbol_part.c_str(), symbol_part.c_str());
    abort();
  }
  if (store._PackageName != package_part) {
    printf("%s:%d For symbol %s there is a mismatch in the package desired %s and the one retrieved %s\n", __FILE__, __LINE__,
           full_name.c_str(), package_part.c_str(), store._PackageName.c_str());
    SIMPLE_ERROR("Mismatch of package when setting a class symbol");
  }
  //  printf("%s:%d Setting static_class_symbol to %s\n", __FILE__, __LINE__, _safe_rep_(store._Symbol).c_str());
  TheClass::set_static_class_symbol(store._Symbol);
}

template <class TheClass> NOINLINE void set_one_static_class_Header() {
  gctools::ShiftedStamp the_stamp = gctools::NextStampWtag(0 /* Get from the Stamp */, gctools::GCStamp<TheClass>::StampWtag);
  if (gctools::GCStamp<TheClass>::StampWtag != 0) {
    TheClass::static_ValueStampWtagMtag = gctools::Header_s::StampWtagMtag::make<TheClass>();
  } else {
    TheClass::static_ValueStampWtagMtag = gctools::Header_s::StampWtagMtag::make_unknown(the_stamp);
  }
}

template <class TheClass>
NOINLINE gc::smart_ptr<core::Instance_O> allocate_one_metaclass(gctools::UnshiftedStamp theStamp, core::Symbol_sp classSymbol,
                                                                core::Instance_sp metaClass) {
  core::SimpleFun_sp entryPoint =
      core::makeSimpleFunAndFunctionDescription<TheClass>(kw::_sym_create);
  auto cb = gctools::GC<TheClass>::allocate(entryPoint);
  gc::smart_ptr<core::Instance_O> class_val =
      core::Instance_O::createClassUncollectable(theStamp, metaClass, REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS, cb);
  class_val->__setup_stage1_with_sharedPtr_lisp_sid(class_val, classSymbol);
  //  reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<TheClass>::id,TheClass::static_classSymbol());
  //  TheClass::static_class = class_val;
  _lisp->boot_setf_findClass(classSymbol, class_val);
  //  core::core__setf_find_class(class_val,classSymbol);
  return class_val;
}

template <class TheClass> NOINLINE gc::smart_ptr<core::Instance_O> allocate_one_class(core::Instance_sp metaClass) {
  core::SimpleFun_sp entryPoint =
      core::makeSimpleFunAndFunctionDescription<core::WRAPPER_BuiltInObjectCreator<TheClass>>(nil<core::T_O>());
  core::Creator_sp cb = gc::As<core::Creator_sp>(gctools::GC<core::WRAPPER_BuiltInObjectCreator<TheClass>>::allocate(entryPoint));
  TheClass::set_static_creator(cb);
  gc::smart_ptr<core::Instance_O> class_val = core::Instance_O::createClassUncollectable(
      TheClass::static_ValueStampWtagMtag, metaClass, REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS, cb);
  class_val->__setup_stage1_with_sharedPtr_lisp_sid(class_val, TheClass::static_classSymbol());
  reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<TheClass>::id, TheClass::static_classSymbol());
  TheClass::setStaticClass(class_val);
  //  core::core__setf_find_class(class_val,TheClass::static_classSymbol()); //,true,nil<core::T_O>()
  _lisp->boot_setf_findClass(TheClass::static_classSymbol(), class_val);
  return class_val;
}

template <class TheMetaClass> struct TempClass {
  static gctools::smart_ptr<TheMetaClass> holder;
};

std::map<std::string, size_t> global_unshifted_nowhere_stamp_name_map;
std::vector<std::string> global_unshifted_nowhere_stamp_names;
// Keep track of the where information given an unshifted_nowhere_stamp
std::vector<size_t> global_unshifted_nowhere_stamp_where_map;
size_t _global_last_stamp = 0;

// #define DUMP_NAMES 1

void register_stamp_name(const std::string& stamp_name, gctools::UnshiftedStamp unshifted_stamp) {
  if (unshifted_stamp == 0)
    return;
  size_t stamp_where = gctools::Header_s::StampWtagMtag::get_stamp_where(unshifted_stamp);
  size_t stamp_num = gctools::Header_s::StampWtagMtag::make_nowhere_stamp(unshifted_stamp);
#ifdef DUMP_NAMES
  printf("%s:%d  stamp_num=%u  name=%s\n", __FILE__, __LINE__, stamp_num, stamp_name.c_str());
#endif
  global_unshifted_nowhere_stamp_name_map[stamp_name] = stamp_num;
  if (stamp_num >= global_unshifted_nowhere_stamp_names.size()) {
    global_unshifted_nowhere_stamp_names.resize(stamp_num + 1, "");
  }
  global_unshifted_nowhere_stamp_names[stamp_num] = stamp_name;
  if (stamp_num >= global_unshifted_nowhere_stamp_where_map.size()) {
    global_unshifted_nowhere_stamp_where_map.resize(stamp_num + 1, 0);
  }
  global_unshifted_nowhere_stamp_where_map[stamp_num] = stamp_where;
}

void define_builtin_cxx_class_names() {
#ifndef SCRAPING
#define GC_ENUM_NAMES
#if !defined(USE_PRECISE_GC)
#include INIT_CLASSES_INC_H
#else
#include CLASP_GC_CC
#endif
#undef GC_ENUM_NAMES
#endif
}

void create_packages() {
#define CREATE_ALL_PACKAGES
#ifndef SCRAPING
#include SYMBOLS_SCRAPED_INC_H
#endif
#undef CREATE_ALL_PACKAGES
}

// ------------------------------------------------------------
//
// Generate type specifier -> header value (range) map
//

template <typename TSingle> void add_single_typeq_test(const string& cname, core::HashTable_sp theMap) {
  [[maybe_unused]] Fixnum header_val = gctools::Header_s::StampWtagMtag::GenerateHeaderValue<TSingle>();
  //  printf("%s:%d Header value for type %s -> %lld    stamp: %u  flags: %zu\n", __FILE__, __LINE__,
  //  _rep_(TSingle::static_class_symbol).c_str(), header_val, gctools::GCStamp<TSingle>::Stamp, gctools::GCStamp<TSingle>::Flags);
  theMap->setf_gethash(TSingle::static_classSymbol(),
                       core::make_fixnum(gctools::Header_s::StampWtagMtag::GenerateHeaderValue<TSingle>()));
}

template <typename TRangeFirst, typename TRangeLast> void add_range_typeq_test(const string& cname, core::HashTable_sp theMap) {

  theMap->setf_gethash(
      TRangeFirst::static_classSymbol(),
      core::Cons_O::create(core::make_fixnum(gctools::Header_s::StampWtagMtag::GenerateHeaderValue<TRangeFirst>()),
                           core::make_fixnum(gctools::Header_s::StampWtagMtag::GenerateHeaderValue<TRangeLast>())));
}
template <typename TSingle> void add_single_typeq_test_instance(core::HashTable_sp theMap) {
  theMap->setf_gethash(TSingle::static_classSymbol(),
                       core::make_fixnum(gctools::Header_s::StampWtagMtag::GenerateHeaderValue<TSingle>()));
}

template <typename TRangeFirst, typename TRangeLast> void add_range_typeq_test_instance(core::HashTable_sp theMap) {
  theMap->setf_gethash(
      TRangeFirst::static_classSymbol(),
      core::Cons_O::create(core::make_fixnum(gctools::Header_s::StampWtagMtag::GenerateHeaderValue<TRangeFirst>()),
                           core::make_fixnum(gctools::Header_s::StampWtagMtag::GenerateHeaderValue<TRangeLast>())));
}

void initialize_typeq_map() {
  core::HashTable_sp classNameToLispName = core::HashTable_O::createEqual();
  core::HashTable_sp theTypeqMap = core::HashTable_O::createEq();
#define ADD_SINGLE_TYPEQ_TEST(type, stamp)                                                                                         \
  {                                                                                                                                \
    classNameToLispName->setf_gethash(core::SimpleBaseString_O::make(#type), type::static_classSymbol());                          \
    theTypeqMap->setf_gethash(type::static_classSymbol(),                                                                          \
                              core::make_fixnum(gctools::Header_s::StampWtagMtag::GenerateTypeqHeaderValue<type>()));              \
  }
#define ADD_RANGE_TYPEQ_TEST(type_low, type_high, stamp_low, stamp_high)                                                           \
  {                                                                                                                                \
    classNameToLispName->setf_gethash(core::SimpleBaseString_O::make(#type_low), type_low::static_classSymbol());                  \
    theTypeqMap->setf_gethash(                                                                                                     \
        type_low::static_classSymbol(),                                                                                            \
        core::Cons_O::create(core::make_fixnum(gctools::Header_s::StampWtagMtag::GenerateTypeqHeaderValue<type_low>()),            \
                             core::make_fixnum(gctools::Header_s::StampWtagMtag::GenerateTypeqHeaderValue<type_high>())));         \
  }
#ifndef SCRAPING
#if !defined(USE_PRECISE_GC)
#define GC_TYPEQ
#include INIT_CLASSES_INC_H // REPLACED CLASP_GC_CC
#undef GC_TYPEQ
#else
#define GC_TYPEQ
#include CLASP_GC_CC
#undef GC_TYPEQ
#endif
#endif
  core::_sym__PLUS_class_name_to_lisp_name_PLUS_->defparameter(classNameToLispName);
  core::_sym__PLUS_type_header_value_map_PLUS_->defparameter(theTypeqMap);
};

// ----------------------------------------------------------------------
//
// Expose classes and methods
//
// Code generated by scraper
//
//
#include <clasp/core/wrappers.h>
#include <clasp/core/external_wrappers.h>

#ifndef SCRAPING
// include INIT_CLASSES_INC_H despite USE_PRECISE_GC
#define EXPOSE_STATIC_CLASS_VARIABLES
#include INIT_CLASSES_INC_H
#undef EXPOSE_STATIC_CLASS_VARIABLES
#endif

void initialize_clasp_Kinds() {
#ifndef SCRAPING
  // include INIT_CLASSES_INC_H despite USE_PRECISE_GC
#define SET_CLASS_KINDS
#include INIT_CLASSES_INC_H
#undef SET_CLASS_KINDS
#endif
}

void initialize_allocate_metaclasses(core::BootStrapCoreSymbolMap& bootStrapCoreSymbolMap) {
  gctools::ShiftedStamp TheClass_stamp = gctools::NextStampWtag(gctools::Header_s::rack_wtag);
  ASSERT(gctools::Header_s::StampWtagMtag::is_rack_shifted_stamp(TheClass_stamp));
  gctools::ShiftedStamp TheBuiltInClass_stamp = gctools::NextStampWtag(gctools::Header_s::rack_wtag);
  ASSERT(gctools::Header_s::StampWtagMtag::is_rack_shifted_stamp(TheBuiltInClass_stamp));
  gctools::ShiftedStamp TheStandardClass_stamp = gctools::NextStampWtag(gctools::Header_s::rack_wtag);
  ASSERT(gctools::Header_s::StampWtagMtag::is_rack_shifted_stamp(TheStandardClass_stamp));
  gctools::ShiftedStamp TheStructureClass_stamp = gctools::NextStampWtag(gctools::Header_s::rack_wtag);
  ASSERT(gctools::Header_s::StampWtagMtag::is_rack_shifted_stamp(TheStructureClass_stamp));
  gctools::ShiftedStamp TheDerivableCxxClass_stamp = gctools::NextStampWtag(gctools::Header_s::rack_wtag);
  ASSERT(gctools::Header_s::StampWtagMtag::is_rack_shifted_stamp(TheDerivableCxxClass_stamp));
  gctools::ShiftedStamp TheClbindCxxClass_stamp = gctools::NextStampWtag(gctools::Header_s::rack_wtag);
  ASSERT(gctools::Header_s::StampWtagMtag::is_rack_shifted_stamp(TheClbindCxxClass_stamp));
  //  global_TheClassRep_stamp = gctools::GCStamp<clbind::ClassRep_O>::StampWtag;

  _lisp->_Roots._TheClass =
      allocate_one_metaclass<core::StandardClassCreator_O>(TheClass_stamp, cl::_sym_class, unbound<core::Instance_O>());
  _lisp->_Roots._TheBuiltInClass = allocate_one_metaclass<core::StandardClassCreator_O>(
      TheBuiltInClass_stamp, cl::_sym_built_in_class, unbound<core::Instance_O>());
  _lisp->_Roots._TheStandardClass = allocate_one_metaclass<core::StandardClassCreator_O>(
      TheStandardClass_stamp, cl::_sym_standard_class, unbound<core::Instance_O>());
  _lisp->_Roots._TheStructureClass = allocate_one_metaclass<core::StandardClassCreator_O>(
      TheStructureClass_stamp, cl::_sym_structure_class, unbound<core::Instance_O>());
  _lisp->_Roots._TheDerivableCxxClass = allocate_one_metaclass<core::DerivableCxxClassCreator_O>(
      TheDerivableCxxClass_stamp, core::_sym_derivable_cxx_class, unbound<core::Instance_O>());
  _lisp->_Roots._TheClbindCxxClass = allocate_one_metaclass<core::ClassRepCreator_O>(
      TheClbindCxxClass_stamp, core::_sym_clbind_cxx_class, unbound<core::Instance_O>());
  _lisp->_Roots._TheClass->_Class = _lisp->_Roots._TheStandardClass;
  _lisp->_Roots._TheBuiltInClass->_Class = _lisp->_Roots._TheStandardClass;
  _lisp->_Roots._TheStandardClass->_Class = _lisp->_Roots._TheStandardClass;
  _lisp->_Roots._TheStructureClass->_Class = _lisp->_Roots._TheStandardClass;
  _lisp->_Roots._TheDerivableCxxClass->_Class = _lisp->_Roots._TheStandardClass;
  _lisp->_Roots._TheClbindCxxClass->_Class = _lisp->_Roots._TheStandardClass;
#ifndef SCRAPING
  // include despite USE_PRECISE_GC
#define ALLOCATE_ALL_CLASSES
#include INIT_CLASSES_INC_H
#undef ALLOCATE_ALL_CLASSES
#endif
  core_T_O_var->setInstanceBaseClasses(nil<core::T_O>());
  // ClassRep_O is initialized like other class objects - but we need to save it in a special system-wide variable
  //  _lisp->_Roots._TheClassRep = clbind_ClassRep_O_var;

  create_packages();

  // have to do this before symbols are finalized so that keywords are all bound properly.
  gc::As<core::Package_sp>(_lisp->findPackage("KEYWORD"))->setKeywordPackage(true);

  //
  // Now this is done in startup
  //
  // define_builtin_cxx_class_names();

  bootStrapCoreSymbolMap.finish_setup_of_symbols();

// Define base classes
#ifndef SCRAPING
// import despite USE_PRECISE_GC
#define SET_BASES_ALL_CLASSES
#include INIT_CLASSES_INC_H
#undef SET_BASES_ALL_CLASSES
#endif

  // Define base classes
#ifndef SCRAPING
// import despite USE_PRECISE_GC
#define CALCULATE_CLASS_PRECEDENCE_ALL_CLASSES
#include INIT_CLASSES_INC_H
#undef CALCULATE_CLASS_PRECEDENCE_ALL_CLASSES
#endif

  _lisp->_Roots._TheClass->stamp_set(TheStandardClass_stamp);
  _lisp->_Roots._TheClass->instanceSet(core::Instance_O::REF_CLASS_SLOTS, nil<core::T_O>());
  _lisp->_Roots._TheClass->instanceSet(core::Instance_O::REF_CLASS_DIRECT_SLOTS, nil<core::T_O>());
  _lisp->_Roots._TheClass->instanceSet(core::Instance_O::REF_CLASS_DEFAULT_INITARGS, nil<core::T_O>());
  _lisp->_Roots._TheBuiltInClass->stamp_set(TheStandardClass_stamp);
  _lisp->_Roots._TheBuiltInClass->instanceSet(core::Instance_O::REF_CLASS_SLOTS, nil<core::T_O>());
  _lisp->_Roots._TheBuiltInClass->instanceSet(core::Instance_O::REF_CLASS_DIRECT_SLOTS, nil<core::T_O>());
  _lisp->_Roots._TheBuiltInClass->instanceSet(core::Instance_O::REF_CLASS_DEFAULT_INITARGS, nil<core::T_O>());
  _lisp->_Roots._TheStandardClass->stamp_set(TheStandardClass_stamp);
  _lisp->_Roots._TheStandardClass->instanceSet(core::Instance_O::REF_CLASS_SLOTS, nil<core::T_O>());
  _lisp->_Roots._TheStandardClass->instanceSet(core::Instance_O::REF_CLASS_DIRECT_SLOTS, nil<core::T_O>());
  _lisp->_Roots._TheStandardClass->instanceSet(core::Instance_O::REF_CLASS_DEFAULT_INITARGS, nil<core::T_O>());
  _lisp->_Roots._TheStructureClass->stamp_set(TheStandardClass_stamp);
  _lisp->_Roots._TheStructureClass->instanceSet(core::Instance_O::REF_CLASS_SLOTS, nil<core::T_O>());
  _lisp->_Roots._TheStructureClass->instanceSet(core::Instance_O::REF_CLASS_DIRECT_SLOTS, nil<core::T_O>());
  _lisp->_Roots._TheStructureClass->instanceSet(core::Instance_O::REF_CLASS_DEFAULT_INITARGS, nil<core::T_O>());
  _lisp->_Roots._TheDerivableCxxClass->stamp_set(TheStandardClass_stamp);
  _lisp->_Roots._TheDerivableCxxClass->instanceSet(core::Instance_O::REF_CLASS_SLOTS, nil<core::T_O>());
  _lisp->_Roots._TheDerivableCxxClass->instanceSet(core::Instance_O::REF_CLASS_DIRECT_SLOTS, nil<core::T_O>());
  _lisp->_Roots._TheDerivableCxxClass->instanceSet(core::Instance_O::REF_CLASS_DEFAULT_INITARGS, nil<core::T_O>());
  _lisp->_Roots._TheClbindCxxClass->stamp_set(TheStandardClass_stamp);
  _lisp->_Roots._TheClbindCxxClass->instanceSet(core::Instance_O::REF_CLASS_SLOTS, nil<core::T_O>());
  _lisp->_Roots._TheClbindCxxClass->instanceSet(core::Instance_O::REF_CLASS_DIRECT_SLOTS, nil<core::T_O>());
  _lisp->_Roots._TheClbindCxxClass->instanceSet(core::Instance_O::REF_CLASS_DEFAULT_INITARGS, nil<core::T_O>());

  _lisp->_Roots._TheBuiltInClass->setInstanceBaseClasses(core::Cons_O::createList(_lisp->_Roots._TheClass));
  _lisp->_Roots._TheStandardClass->setInstanceBaseClasses(core::Cons_O::createList(_lisp->_Roots._TheClass));
  _lisp->_Roots._TheStructureClass->setInstanceBaseClasses(core::Cons_O::createList(_lisp->_Roots._TheClass));
  _lisp->_Roots._TheDerivableCxxClass->setInstanceBaseClasses(core::Cons_O::createList(_lisp->_Roots._TheClass));
  _lisp->_Roots._TheClbindCxxClass->setInstanceBaseClasses(core::Cons_O::createList(_lisp->_Roots._TheClass));

  reg::lisp_registerClassSymbol<core::Character_I>(cl::_sym_character);
  reg::lisp_registerClassSymbol<core::Fixnum_I>(cl::_sym_fixnum);
  reg::lisp_registerClassSymbol<core::SingleFloat_I>(cl::_sym_single_float);
}
#endif // #ifndef SCRAPING at top
