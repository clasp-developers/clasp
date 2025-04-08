#pragma once

#define DEFINE_O_SMART_POINTERS(zclass)                                                                                            \
  class zclass##_O;                                                                                                                \
  typedef gctools::smart_ptr<zclass##_O> zclass##_sp; /* Stack pointers */                                                         \
  typedef gctools::multiple_values<zclass##_O> zclass##_mv;

// This ensures that smart pointers are only declared once
// for each compilation
#define SMART(zclass) DEFINE_O_SMART_POINTERS(zclass)

#define FORWARD(zclass) DEFINE_O_SMART_POINTERS(zclass)

namespace core {
class T_O;
typedef gctools::smart_ptr<T_O> T_sp;

class Number_O;
typedef gctools::smart_ptr<Number_O> Number_sp;

class Symbol_O;
typedef gctools::smart_ptr<Symbol_O> Symbol_sp;

class Pointer_O;
typedef gctools::smart_ptr<Pointer_O> Pointer_sp;

class SimpleVector_O;
typedef gctools::smart_ptr<SimpleVector_O> SimpleVector_sp;

class HashTable_O;
typedef gctools::smart_ptr<HashTable_O> HashTable_sp;

class Instance_O;
typedef gctools::smart_ptr<Instance_O> Instance_sp;

//    typedef T_mv (*ActivationFrameFunctionPtr)(ActivationFrame_sp);
class StringOutputStream_O;
typedef gctools::smart_ptr<StringOutputStream_O> StringOutputStream_sp;
class Bignum_O;
typedef gctools::smart_ptr<Bignum_O> Bignum_sp;
class Cache_O;
typedef gctools::smart_ptr<Cache_O> Cache_sp;
class Lisp;
class Symbol_O;
typedef gctools::smart_ptr<Symbol_O> Symbol_sp;

}; // namespace core

namespace core {
class T_O;
typedef T_O FIXNUM;
class Cons_O;
class General_O;
class Pointer_O;
class Number_O;
class Package_O;
class Integer_O;
class LoadTimeValues_O;
/* AMS pool classes */
class Symbol_O;
class Null_O;
class Stream_O;
class SourcePosInfo_O;
class FileScope_O;
class DynamicScopeManager;

class Function_O;
}; // namespace core

namespace core {
typedef gctools::smart_ptr<T_O> T_sp;
typedef T_sp SEQUENCE_sp;
typedef T_sp LIST_sp;
typedef gctools::smart_ptr<Pointer_O> Pointer_sp;
typedef gctools::smart_ptr<Cons_O> Cons_sp;
typedef gctools::smart_ptr<Function_O> Function_sp;
typedef gctools::smart_ptr<Package_O> Package_sp;
typedef gctools::smart_ptr<Stream_O> Stream_sp;
typedef gctools::smart_ptr<SourcePosInfo_O> SourcePosInfo_sp;
typedef gctools::smart_ptr<FileScope_O> FileScope_sp;
}; // namespace core

namespace core {
class Array_O;
class MDArray_O;
class MDArrayT_O;
class Str8Ns_O;
class SimpleBaseString_O;
class SimpleVector_O;
class BitVectorNs_O;
class SimpleBitVector_O;
// The common root class of Vector_O, String_O and BitVector_O is Array_O
typedef Array_O Vector_O;
typedef Array_O String_O;
typedef MDArray_O StringNs_O;
typedef BitVectorNs_O BitVector_O;
typedef Str8Ns_O Str_O;
typedef gc::smart_ptr<Array_O> Array_sp;
typedef gc::smart_ptr<MDArray_O> MDArray_sp;
typedef gc::smart_ptr<SimpleBaseString_O> SimpleBaseString_sp;
typedef gc::smart_ptr<SimpleBitVector_O> SimpleBitVector_sp;
typedef gc::smart_ptr<SimpleVector_O> SimpleVector_sp;
typedef gc::smart_ptr<BitVectorNs_O> BitVectorNs_sp;
typedef gc::smart_ptr<Str8Ns_O> Str8Ns_sp;
typedef gc::smart_ptr<MDArrayT_O> MDArrayT_sp;
// Use typedef to assign new smart_ptr to old types
// FIXME: Remove all of the old smart_ptr names and use the new ones everywhere
typedef Array_sp String_sp;
typedef MDArray_sp StringNs_sp;
typedef BitVectorNs_sp BitVector_sp;
typedef Str8Ns_sp Str_sp;
typedef Array_sp Vector_sp;
}; // namespace core

namespace mp {
class Process_O;
typedef gctools::smart_ptr<Process_O> Process_sp;
}; // namespace mp
