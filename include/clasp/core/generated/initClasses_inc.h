// start
// define cpp macros: SET_SYMBOL, CREATE_CLASS, SET_CLASS, DEFINE_BASE_CLASSES, DEFINE_CLASS_NAMES, EXPOSE_TO_CANDO 
// define cpp macro: ALL_STAGES to get the effect of defining all of the macros above
// define cpp macro: EXPOSE_PYTHON to expose python
 // class ActivationFrame_O : public Environment_O // RuntimeVisibleEnvironment_O
 // class ValueFrame_O : public ActivationFrame_O {
 // class FunctionFrame_O : public ActivationFrame_O {
 // class TagbodyFrame_O : public ActivationFrame_O {
 // class Array_O : public T_O {
 // class ArrayDisplaced_O : public Array_O {
 // class ArrayObjects_O : public Array_O {
 // class Bignum_O : public Integer_O {
 // class Binder_O : public T_O {
 // class BitVector_O : public Vector_O {
 // class SimpleBitVector_O : public BitVector_O {
 // class BuiltInClass_O : public Class_O {
 // class Character_dummy_O : public T_O {
// Associating namespace(cleavirEnv) with package(CleavirEnvPkg)
// Associating namespace(cleavirPrimops) with package(CleavirPrimopsPkg)
// Associating namespace(clos) with package(ClosPkg)
// Associating namespace(cl) with package(ClPkg)
// Associating namespace(cluser) with package(CommonLispUserPkg)
// Associating namespace(comp) with package(CompPkg)
 // class CandoException_O : public T_O {
 // class Cons_O : public T_O {
// Associating namespace(core) with package(CorePkg)
 // class Environment_O : public T_O {
 // class LexicalEnvironment_O : public Environment_O {
 // class RuntimeVisibleEnvironment_O : public LexicalEnvironment_O {
 // class ValueEnvironment_O : public RuntimeVisibleEnvironment_O {
 // class FunctionValueEnvironment_O : public RuntimeVisibleEnvironment_O {
 // class CompileTimeEnvironment_O : public LexicalEnvironment_O {
 // class UnwindProtectEnvironment_O : public CompileTimeEnvironment_O {
 // class BlockEnvironment_O : public CompileTimeEnvironment_O {
 // class CatchEnvironment_O : public CompileTimeEnvironment_O {
 // class FunctionContainerEnvironment_O : public CompileTimeEnvironment_O {
 // class TagbodyEnvironment_O : public RuntimeVisibleEnvironment_O {
 // class MacroletEnvironment_O : public CompileTimeEnvironment_O {
 // class SymbolMacroletEnvironment_O : public CompileTimeEnvironment_O {
 // class StackValueEnvironment_O : public CompileTimeEnvironment_O {
 // class GlueEnvironment_O : public Environment_O {
 // class Function_O : public T_O {
 // class CompiledFunction_O : public Function_O {
// Associating namespace(ext) with package(ExtPkg)
 // class ExternalObject_O : public T_O // StandardObject_O
 // class ForeignData_O : public ExternalObject_O // StandardObject_O
 // class Path_O : public T_O {
 // class DirectoryIterator_O : public Iterator_O {
 // class RecursiveDirectoryIterator_O : public Iterator_O {
 // class DirectoryEntry_O : public T_O {
 // class FileStatus_O : public T_O {
 // class ForwardReferencedClass_O : public Class_O {
 // class FuncallableStandardClass_O : public StdClass_O {
// Associating namespace(gray) with package(GrayPkg)
 // class HashTable_O : public T_O {
 // class HashTableEq_O : public HashTable_O {
 // class HashTableEql_O : public HashTable_O {
 // class HashTableEqual_O : public HashTable_O {
 // class HashTableEqualp_O : public HashTable_O {
 // class Instance_O : public Function_O {
 // class IntArray_O : public T_O {
 // class Iterator_O : public T_O {
// Associating namespace(kw) with package(KeywordPkg)
 // class LambdaListHandler_O : public T_O {
 // class Stream_O : public T_O {
 // class AnsiStream_O : public Stream_O {
 // class FileStream_O : public AnsiStream_O {
 // class IOFileStream_O : public FileStream_O {
 // class IOStreamStream_O : public FileStream_O {
 // class StringStream_O : public AnsiStream_O {
 // class StringOutputStream_O : public StringStream_O {
 // class StringInputStream_O : public StringStream_O {
 // class SynonymStream_O : public AnsiStream_O {
 // class TwoWayStream_O : public AnsiStream_O {
 // class BroadcastStream_O : public AnsiStream_O {
 // class ConcatenatedStream_O : public AnsiStream_O {
 // class EchoStream_O : public AnsiStream_O {
 // class String_O : public Vector_O {
 // class Vector_O : public Array_O {
 // class LoadTimeValues_O : public T_O {
 // class Class_O : public Specializer_O {
 // class Metaobject_O : public StandardObject_O {
 // class MicroHeap_O : public T_O {
 // class MultiStringBuffer_O : public T_O {
 // class VectorObjects_O : public Vector_O {
 // class VectorObjectsWithFillPtr_O : public VectorObjects_O {
 // class Null_O : public Symbol_O {
 // class Number_O : public T_O {
 // class Real_O : public Number_O {
 // class Rational_O : public Real_O {
 // class Integer_O : public Rational_O {
 // class Fixnum_dummy_O : public Integer_O {
 // class Float_O : public Real_O {
 // class ShortFloat_O : public Float_O {
 // class SingleFloat_dummy_O : public Float_O {
 // class DoubleFloat_O : public Float_O {
 // class Complex_O : public Number_O {
 // class Ratio_O : public Rational_O {
 // class ObjRef_O : public T_O {
 // class T_O : public _RootDummyClass {
 // class ObjectSet_O : public T_O {
 // class Package_O : public T_O {
 // class Pathname_O : public T_O {
 // class LogicalPathname_O : public Pathname_O {
 // class Pointer_O : public T_O {
 // class PosixTime_O : public T_O {
 // class PosixTimeDuration_O : public T_O {
 // class Reader_O : public T_O {
 // class ReadTable_O : public T_O {
 // class RegexMatch_O : public core::T_O {
 // class Regex_O : public core::T_O {
 // class SNode_O : public T_O {
 // class LeafSNode_O : public SNode_O {
 // class BranchSNode_O : public SNode_O {
 // class Archive_O : public T_O {
 // class LoadArchive_O : public Archive_O {
 // class SaveArchive_O : public Archive_O {
 // class SexpLoadArchive_O : public LoadArchive_O {
 // class SexpSaveArchive_O : public SaveArchive_O {
 // class SingleDispatchEffectiveMethodFunction_O : public Function_O {
 // class SingleDispatchGenericFunction_O : public Function_O {
 // class SingleDispatchMethod_O : public T_O {
 // class SmallMap_O : public T_O {
 // class SourceFileInfo_O : public T_O {
 // class SourcePosInfo_O : public T_O {
 // class SourceManager_O : public T_O {
 // class SpecialForm_O : public Function_O {
 // class Specializer_O : public Metaobject_O {
 // class StandardClass_O : public StdClass_O {
 // class StandardObject_O : public T_O {
 // class StdClass_O : public Class_O {
 // class Str_O : public String_O {
 // class StrWithFillPtr_O : public Str_O {
 // class StringList_O : public T_O {
 // class StringSet_O : public T_O {
 // class StructureClass_O : public Class_O {
 // class StructureObject_O : public T_O {
 // class Symbol_O : public T_O {
 // class SymbolList_O : public T_O {
 // class SymbolSet_O : public T_O {
 // class SymbolToEnumConverter_O : public T_O {
 // class LightUserData_O : public core::T_O // StandardObject_O
 // class UserData_O : public core::LightUserData_O // StandardObject_O
 // class VectorObjects_O : public Vector_O {
 // class VectorObjectsWithFillPtr_O : public VectorObjects_O {
 // class WeakHashTable_O : public T_O {
 // class WeakKeyHashTable_O : public WeakHashTable_O {
 // class WeakKeyMapping_O : public T_O {
 // class WeakPointer_O : public T_O {
 // class WrappedPointer_O : public core::T_O {
#ifdef HEADER_INCLUDES
#include "include/object.h"
#include "include/serialize.h"
#include "include/array.h"
#include "include/binder.h"
#include "include/conditions.h"
#include "include/character.h"
#include "include/cons.h"
#include "include/fileSystem.h"
#include "include/environment.h"
#include "include/externalObject.h"
#include "include/executables.h"
#include "include/hashTable.h"
#include "include/intArray.h"
#include "include/iterator.h"
#include "include/lambdaListHandler.h"
#include "include/userData.h"
#include "include/loadTimeValues.h"
#include "include/microHeap.h"
#include "include/multiStringBuffer.h"
#include "include/numbers.h"
#include "include/objRef.h"
#include "include/objectSet.h"
#include "include/package.h"
#include "include/pathname.h"
#include "include/pointer.h"
#include "include/posixTime.h"
#include "include/readtable.h"
#include "include/reader.h"
#include "include/regex.h"
#include "include/singleDispatchMethod.h"
#include "include/smallMap.h"
#include "include/sourceFileInfo.h"
#include "include/standardObject.h"
#include "include/lispStream.h"
#include "include/stringList.h"
#include "include/stringSet.h"
#include "include/structureObject.h"
#include "include/symbolList.h"
#include "include/symbolSet.h"
#include "include/symbolToEnumConverter.h"
#include "include/symbol.h"
#include "include/weakHashTable.h"
#include "include/weakKeyMapping.h"
#include "include/weakPointer.h"
#include "include/wrappedPointer.h"
#include "include/activationFrame.h"
#include "include/arrayDisplaced.h"
#include "include/arrayObjects.h"
#include "include/hashTableEq.h"
#include "include/hashTableEql.h"
#include "include/hashTableEqual.h"
#include "include/hashTableEqualp.h"
#include "include/instance.h"
#include "include/metaobject.h"
#include "include/null.h"
#include "include/singleDispatchEffectiveMethodFunction.h"
#include "include/singleDispatchGenericFunction.h"
#include "include/specialForm.h"
#include "include/lispVector.h"
#include "include/bitVector.h"
#include "include/sexpLoadArchive.h"
#include "include/sexpSaveArchive.h"
#include "include/specializer.h"
#include "include/lispString.h"
#include "include/newVectorObjects.h"
#include "include/metaClass.h"
#include "include/str.h"
#include "include/newVectorObjectsWithFillPtr.h"
#include "include/bignum.h"
#include "include/builtInClass.h"
#include "include/forwardReferencedClass.h"
#include "include/stdClass.h"
#include "include/strWithFillPtr.h"
#include "include/structureClass.h"
#include "include/funcallableStandardClass.h"
#include "include/standardClass.h"
#endif // HEADER_INCLUDES
#undef HEADER_INCLUDES
#if defined(SET_SYMBOL) || defined(ALL_STAGES)
// requires LOOKUP_SYMBOL(pkg,symbolName) be defined
core::T_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::T_O::static_packageName(),core::T_O::static_className()));
core::Archive_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Archive_O::static_packageName(),core::Archive_O::static_className()));
core::Array_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Array_O::static_packageName(),core::Array_O::static_className()));
core::Binder_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Binder_O::static_packageName(),core::Binder_O::static_className()));
core::CandoException_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::CandoException_O::static_packageName(),core::CandoException_O::static_className()));
core::Character_dummy_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Character_dummy_O::static_packageName(),core::Character_dummy_O::static_className()));
core::Cons_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Cons_O::static_packageName(),core::Cons_O::static_className()));
core::DirectoryEntry_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::DirectoryEntry_O::static_packageName(),core::DirectoryEntry_O::static_className()));
core::Environment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Environment_O::static_packageName(),core::Environment_O::static_className()));
core::ExternalObject_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ExternalObject_O::static_packageName(),core::ExternalObject_O::static_className()));
core::FileStatus_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FileStatus_O::static_packageName(),core::FileStatus_O::static_className()));
core::Function_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Function_O::static_packageName(),core::Function_O::static_className()));
core::HashTable_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::HashTable_O::static_packageName(),core::HashTable_O::static_className()));
core::IntArray_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::IntArray_O::static_packageName(),core::IntArray_O::static_className()));
core::Iterator_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Iterator_O::static_packageName(),core::Iterator_O::static_className()));
core::LambdaListHandler_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::LambdaListHandler_O::static_packageName(),core::LambdaListHandler_O::static_className()));
core::LightUserData_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::LightUserData_O::static_packageName(),core::LightUserData_O::static_className()));
core::LoadTimeValues_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::LoadTimeValues_O::static_packageName(),core::LoadTimeValues_O::static_className()));
core::MicroHeap_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::MicroHeap_O::static_packageName(),core::MicroHeap_O::static_className()));
core::MultiStringBuffer_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::MultiStringBuffer_O::static_packageName(),core::MultiStringBuffer_O::static_className()));
core::Number_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Number_O::static_packageName(),core::Number_O::static_className()));
core::ObjRef_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ObjRef_O::static_packageName(),core::ObjRef_O::static_className()));
core::ObjectSet_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ObjectSet_O::static_packageName(),core::ObjectSet_O::static_className()));
core::Package_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Package_O::static_packageName(),core::Package_O::static_className()));
core::Path_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Path_O::static_packageName(),core::Path_O::static_className()));
core::Pathname_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Pathname_O::static_packageName(),core::Pathname_O::static_className()));
core::Pointer_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Pointer_O::static_packageName(),core::Pointer_O::static_className()));
core::PosixTimeDuration_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::PosixTimeDuration_O::static_packageName(),core::PosixTimeDuration_O::static_className()));
core::PosixTime_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::PosixTime_O::static_packageName(),core::PosixTime_O::static_className()));
core::ReadTable_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ReadTable_O::static_packageName(),core::ReadTable_O::static_className()));
core::Reader_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Reader_O::static_packageName(),core::Reader_O::static_className()));
core::RegexMatch_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::RegexMatch_O::static_packageName(),core::RegexMatch_O::static_className()));
core::Regex_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Regex_O::static_packageName(),core::Regex_O::static_className()));
core::SNode_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SNode_O::static_packageName(),core::SNode_O::static_className()));
core::SingleDispatchMethod_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SingleDispatchMethod_O::static_packageName(),core::SingleDispatchMethod_O::static_className()));
core::SmallMap_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SmallMap_O::static_packageName(),core::SmallMap_O::static_className()));
core::SourceFileInfo_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SourceFileInfo_O::static_packageName(),core::SourceFileInfo_O::static_className()));
core::SourceManager_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SourceManager_O::static_packageName(),core::SourceManager_O::static_className()));
core::SourcePosInfo_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SourcePosInfo_O::static_packageName(),core::SourcePosInfo_O::static_className()));
core::StandardObject_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StandardObject_O::static_packageName(),core::StandardObject_O::static_className()));
core::Stream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Stream_O::static_packageName(),core::Stream_O::static_className()));
core::StringList_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StringList_O::static_packageName(),core::StringList_O::static_className()));
core::StringSet_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StringSet_O::static_packageName(),core::StringSet_O::static_className()));
core::StructureObject_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StructureObject_O::static_packageName(),core::StructureObject_O::static_className()));
core::SymbolList_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SymbolList_O::static_packageName(),core::SymbolList_O::static_className()));
core::SymbolSet_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SymbolSet_O::static_packageName(),core::SymbolSet_O::static_className()));
core::SymbolToEnumConverter_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SymbolToEnumConverter_O::static_packageName(),core::SymbolToEnumConverter_O::static_className()));
core::Symbol_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Symbol_O::static_packageName(),core::Symbol_O::static_className()));
core::WeakHashTable_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::WeakHashTable_O::static_packageName(),core::WeakHashTable_O::static_className()));
core::WeakKeyMapping_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::WeakKeyMapping_O::static_packageName(),core::WeakKeyMapping_O::static_className()));
core::WeakPointer_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::WeakPointer_O::static_packageName(),core::WeakPointer_O::static_className()));
core::WrappedPointer_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::WrappedPointer_O::static_packageName(),core::WrappedPointer_O::static_className()));
core::ActivationFrame_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ActivationFrame_O::static_packageName(),core::ActivationFrame_O::static_className()));
core::AnsiStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::AnsiStream_O::static_packageName(),core::AnsiStream_O::static_className()));
core::ArrayDisplaced_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ArrayDisplaced_O::static_packageName(),core::ArrayDisplaced_O::static_className()));
core::ArrayObjects_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ArrayObjects_O::static_packageName(),core::ArrayObjects_O::static_className()));
core::BranchSNode_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::BranchSNode_O::static_packageName(),core::BranchSNode_O::static_className()));
core::CompiledFunction_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::CompiledFunction_O::static_packageName(),core::CompiledFunction_O::static_className()));
core::Complex_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Complex_O::static_packageName(),core::Complex_O::static_className()));
core::DirectoryIterator_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::DirectoryIterator_O::static_packageName(),core::DirectoryIterator_O::static_className()));
core::ForeignData_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ForeignData_O::static_packageName(),core::ForeignData_O::static_className()));
core::GlueEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::GlueEnvironment_O::static_packageName(),core::GlueEnvironment_O::static_className()));
core::HashTableEq_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::HashTableEq_O::static_packageName(),core::HashTableEq_O::static_className()));
core::HashTableEql_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::HashTableEql_O::static_packageName(),core::HashTableEql_O::static_className()));
core::HashTableEqual_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::HashTableEqual_O::static_packageName(),core::HashTableEqual_O::static_className()));
core::HashTableEqualp_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::HashTableEqualp_O::static_packageName(),core::HashTableEqualp_O::static_className()));
core::Instance_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Instance_O::static_packageName(),core::Instance_O::static_className()));
core::LeafSNode_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::LeafSNode_O::static_packageName(),core::LeafSNode_O::static_className()));
core::LexicalEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::LexicalEnvironment_O::static_packageName(),core::LexicalEnvironment_O::static_className()));
core::LoadArchive_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::LoadArchive_O::static_packageName(),core::LoadArchive_O::static_className()));
core::LogicalPathname_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::LogicalPathname_O::static_packageName(),core::LogicalPathname_O::static_className()));
core::Metaobject_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Metaobject_O::static_packageName(),core::Metaobject_O::static_className()));
core::Null_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Null_O::static_packageName(),core::Null_O::static_className()));
core::Real_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Real_O::static_packageName(),core::Real_O::static_className()));
core::RecursiveDirectoryIterator_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::RecursiveDirectoryIterator_O::static_packageName(),core::RecursiveDirectoryIterator_O::static_className()));
core::SaveArchive_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SaveArchive_O::static_packageName(),core::SaveArchive_O::static_className()));
core::SingleDispatchEffectiveMethodFunction_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SingleDispatchEffectiveMethodFunction_O::static_packageName(),core::SingleDispatchEffectiveMethodFunction_O::static_className()));
core::SingleDispatchGenericFunction_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SingleDispatchGenericFunction_O::static_packageName(),core::SingleDispatchGenericFunction_O::static_className()));
core::SpecialForm_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SpecialForm_O::static_packageName(),core::SpecialForm_O::static_className()));
core::UserData_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::UserData_O::static_packageName(),core::UserData_O::static_className()));
core::Vector_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Vector_O::static_packageName(),core::Vector_O::static_className()));
core::WeakKeyHashTable_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::WeakKeyHashTable_O::static_packageName(),core::WeakKeyHashTable_O::static_className()));
core::BitVector_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::BitVector_O::static_packageName(),core::BitVector_O::static_className()));
core::BroadcastStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::BroadcastStream_O::static_packageName(),core::BroadcastStream_O::static_className()));
core::CompileTimeEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::CompileTimeEnvironment_O::static_packageName(),core::CompileTimeEnvironment_O::static_className()));
core::ConcatenatedStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ConcatenatedStream_O::static_packageName(),core::ConcatenatedStream_O::static_className()));
core::EchoStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::EchoStream_O::static_packageName(),core::EchoStream_O::static_className()));
core::FileStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FileStream_O::static_packageName(),core::FileStream_O::static_className()));
core::Float_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Float_O::static_packageName(),core::Float_O::static_className()));
core::FunctionFrame_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FunctionFrame_O::static_packageName(),core::FunctionFrame_O::static_className()));
core::Rational_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Rational_O::static_packageName(),core::Rational_O::static_className()));
core::RuntimeVisibleEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::RuntimeVisibleEnvironment_O::static_packageName(),core::RuntimeVisibleEnvironment_O::static_className()));
core::SexpLoadArchive_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SexpLoadArchive_O::static_packageName(),core::SexpLoadArchive_O::static_className()));
core::SexpSaveArchive_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SexpSaveArchive_O::static_packageName(),core::SexpSaveArchive_O::static_className()));
core::Specializer_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Specializer_O::static_packageName(),core::Specializer_O::static_className()));
core::StringStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StringStream_O::static_packageName(),core::StringStream_O::static_className()));
core::String_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::String_O::static_packageName(),core::String_O::static_className()));
core::SynonymStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SynonymStream_O::static_packageName(),core::SynonymStream_O::static_className()));
core::TagbodyFrame_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::TagbodyFrame_O::static_packageName(),core::TagbodyFrame_O::static_className()));
core::TwoWayStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::TwoWayStream_O::static_packageName(),core::TwoWayStream_O::static_className()));
core::ValueFrame_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ValueFrame_O::static_packageName(),core::ValueFrame_O::static_className()));
core::VectorObjects_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::VectorObjects_O::static_packageName(),core::VectorObjects_O::static_className()));
core::BlockEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::BlockEnvironment_O::static_packageName(),core::BlockEnvironment_O::static_className()));
core::CatchEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::CatchEnvironment_O::static_packageName(),core::CatchEnvironment_O::static_className()));
core::Class_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Class_O::static_packageName(),core::Class_O::static_className()));
core::DoubleFloat_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::DoubleFloat_O::static_packageName(),core::DoubleFloat_O::static_className()));
core::FunctionContainerEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FunctionContainerEnvironment_O::static_packageName(),core::FunctionContainerEnvironment_O::static_className()));
core::FunctionValueEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FunctionValueEnvironment_O::static_packageName(),core::FunctionValueEnvironment_O::static_className()));
core::IOFileStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::IOFileStream_O::static_packageName(),core::IOFileStream_O::static_className()));
core::IOStreamStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::IOStreamStream_O::static_packageName(),core::IOStreamStream_O::static_className()));
core::Integer_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Integer_O::static_packageName(),core::Integer_O::static_className()));
core::MacroletEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::MacroletEnvironment_O::static_packageName(),core::MacroletEnvironment_O::static_className()));
core::Ratio_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Ratio_O::static_packageName(),core::Ratio_O::static_className()));
core::ShortFloat_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ShortFloat_O::static_packageName(),core::ShortFloat_O::static_className()));
core::SimpleBitVector_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SimpleBitVector_O::static_packageName(),core::SimpleBitVector_O::static_className()));
core::SingleFloat_dummy_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SingleFloat_dummy_O::static_packageName(),core::SingleFloat_dummy_O::static_className()));
core::StackValueEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StackValueEnvironment_O::static_packageName(),core::StackValueEnvironment_O::static_className()));
core::Str_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Str_O::static_packageName(),core::Str_O::static_className()));
core::StringInputStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StringInputStream_O::static_packageName(),core::StringInputStream_O::static_className()));
core::StringOutputStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StringOutputStream_O::static_packageName(),core::StringOutputStream_O::static_className()));
core::SymbolMacroletEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SymbolMacroletEnvironment_O::static_packageName(),core::SymbolMacroletEnvironment_O::static_className()));
core::TagbodyEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::TagbodyEnvironment_O::static_packageName(),core::TagbodyEnvironment_O::static_className()));
core::UnwindProtectEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::UnwindProtectEnvironment_O::static_packageName(),core::UnwindProtectEnvironment_O::static_className()));
core::ValueEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ValueEnvironment_O::static_packageName(),core::ValueEnvironment_O::static_className()));
core::VectorObjectsWithFillPtr_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::VectorObjectsWithFillPtr_O::static_packageName(),core::VectorObjectsWithFillPtr_O::static_className()));
core::Bignum_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Bignum_O::static_packageName(),core::Bignum_O::static_className()));
core::BuiltInClass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::BuiltInClass_O::static_packageName(),core::BuiltInClass_O::static_className()));
core::Fixnum_dummy_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Fixnum_dummy_O::static_packageName(),core::Fixnum_dummy_O::static_className()));
core::ForwardReferencedClass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ForwardReferencedClass_O::static_packageName(),core::ForwardReferencedClass_O::static_className()));
core::StdClass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StdClass_O::static_packageName(),core::StdClass_O::static_className()));
core::StrWithFillPtr_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StrWithFillPtr_O::static_packageName(),core::StrWithFillPtr_O::static_className()));
core::StructureClass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StructureClass_O::static_packageName(),core::StructureClass_O::static_className()));
core::FuncallableStandardClass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FuncallableStandardClass_O::static_packageName(),core::FuncallableStandardClass_O::static_className()));
core::StandardClass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StandardClass_O::static_packageName(),core::StandardClass_O::static_className()));
#endif // SET_SYMBOL
#undef SET_SYMBOL
#if defined(CREATE_CLASS) || defined(ALL_STAGES)

    LOG(BF("Creating class[classcore__T_Oval]"));
    core::BuiltInClass_sp classcore__T_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__T_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__T_Oval,_lisp,core::T_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::T_O>::id,core::T_O::static_classSymbol());
    core::T_O::___staticClass = classcore__T_Oval;
#ifdef USE_MPS
    core::T_O::static_Kind = gctools::GCKind<core::T_O>::Kind;
#endif
    core::af_setf_findClass(classcore__T_Oval,core::T_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::T_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::T_O>>::allocateClass();
        core::T_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::T_O::static_className() % (void*)(core::T_O::static_allocator) );
    classcore__T_Oval->setCreator(core::T_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::T_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__T_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Archive_Oval]"));
    core::BuiltInClass_sp classcore__Archive_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Archive_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Archive_Oval,_lisp,core::Archive_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Archive_O>::id,core::Archive_O::static_classSymbol());
    core::Archive_O::___staticClass = classcore__Archive_Oval;
#ifdef USE_MPS
    core::Archive_O::static_Kind = gctools::GCKind<core::Archive_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Archive_Oval,core::Archive_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Archive_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Archive_O>>::allocateClass();
        core::Archive_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Archive_O::static_className() % (void*)(core::Archive_O::static_allocator) );
    classcore__Archive_Oval->setCreator(core::Archive_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Archive_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Archive_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Array_Oval]"));
    core::BuiltInClass_sp classcore__Array_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Array_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Array_Oval,_lisp,core::Array_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Array_O>::id,core::Array_O::static_classSymbol());
    core::Array_O::___staticClass = classcore__Array_Oval;
#ifdef USE_MPS
    core::Array_O::static_Kind = gctools::GCKind<core::Array_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Array_Oval,core::Array_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Array_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Array_O>>::allocateClass();
        core::Array_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Array_O::static_className() % (void*)(core::Array_O::static_allocator) );
    classcore__Array_Oval->setCreator(core::Array_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Array_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Array_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Binder_Oval]"));
    core::BuiltInClass_sp classcore__Binder_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Binder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Binder_Oval,_lisp,core::Binder_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Binder_O>::id,core::Binder_O::static_classSymbol());
    core::Binder_O::___staticClass = classcore__Binder_Oval;
#ifdef USE_MPS
    core::Binder_O::static_Kind = gctools::GCKind<core::Binder_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Binder_Oval,core::Binder_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Binder_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Binder_O>>::allocateClass();
        core::Binder_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Binder_O::static_className() % (void*)(core::Binder_O::static_allocator) );
    classcore__Binder_Oval->setCreator(core::Binder_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Binder_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Binder_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__CandoException_Oval]"));
    core::BuiltInClass_sp classcore__CandoException_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__CandoException_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__CandoException_Oval,_lisp,core::CandoException_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::CandoException_O>::id,core::CandoException_O::static_classSymbol());
    core::CandoException_O::___staticClass = classcore__CandoException_Oval;
#ifdef USE_MPS
    core::CandoException_O::static_Kind = gctools::GCKind<core::CandoException_O>::Kind;
#endif
    core::af_setf_findClass(classcore__CandoException_Oval,core::CandoException_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::CandoException_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::CandoException_O>>::allocateClass();
        core::CandoException_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::CandoException_O::static_className() % (void*)(core::CandoException_O::static_allocator) );
    classcore__CandoException_Oval->setCreator(core::CandoException_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::CandoException_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__CandoException_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Character_dummy_Oval]"));
    core::BuiltInClass_sp classcore__Character_dummy_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Character_dummy_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Character_dummy_Oval,_lisp,core::Character_dummy_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Character_dummy_O>::id,core::Character_dummy_O::static_classSymbol());
    core::Character_dummy_O::___staticClass = classcore__Character_dummy_Oval;
#ifdef USE_MPS
    core::Character_dummy_O::static_Kind = gctools::GCKind<core::Character_dummy_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Character_dummy_Oval,core::Character_dummy_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Character_dummy_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Character_dummy_O>>::allocateClass();
        core::Character_dummy_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Character_dummy_O::static_className() % (void*)(core::Character_dummy_O::static_allocator) );
    classcore__Character_dummy_Oval->setCreator(core::Character_dummy_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Character_dummy_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Character_dummy_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Cons_Oval]"));
    core::BuiltInClass_sp classcore__Cons_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Cons_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Cons_Oval,_lisp,core::Cons_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Cons_O>::id,core::Cons_O::static_classSymbol());
    core::Cons_O::___staticClass = classcore__Cons_Oval;
#ifdef USE_MPS
    core::Cons_O::static_Kind = gctools::GCKind<core::Cons_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Cons_Oval,core::Cons_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Cons_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Cons_O>>::allocateClass();
        core::Cons_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Cons_O::static_className() % (void*)(core::Cons_O::static_allocator) );
    classcore__Cons_Oval->setCreator(core::Cons_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Cons_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Cons_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__DirectoryEntry_Oval]"));
    core::BuiltInClass_sp classcore__DirectoryEntry_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__DirectoryEntry_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__DirectoryEntry_Oval,_lisp,core::DirectoryEntry_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::DirectoryEntry_O>::id,core::DirectoryEntry_O::static_classSymbol());
    core::DirectoryEntry_O::___staticClass = classcore__DirectoryEntry_Oval;
#ifdef USE_MPS
    core::DirectoryEntry_O::static_Kind = gctools::GCKind<core::DirectoryEntry_O>::Kind;
#endif
    core::af_setf_findClass(classcore__DirectoryEntry_Oval,core::DirectoryEntry_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::DirectoryEntry_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::DirectoryEntry_O>>::allocateClass();
        core::DirectoryEntry_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::DirectoryEntry_O::static_className() % (void*)(core::DirectoryEntry_O::static_allocator) );
    classcore__DirectoryEntry_Oval->setCreator(core::DirectoryEntry_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::DirectoryEntry_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__DirectoryEntry_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Environment_Oval]"));
    core::BuiltInClass_sp classcore__Environment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Environment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Environment_Oval,_lisp,core::Environment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Environment_O>::id,core::Environment_O::static_classSymbol());
    core::Environment_O::___staticClass = classcore__Environment_Oval;
#ifdef USE_MPS
    core::Environment_O::static_Kind = gctools::GCKind<core::Environment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Environment_Oval,core::Environment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Environment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Environment_O>>::allocateClass();
        core::Environment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Environment_O::static_className() % (void*)(core::Environment_O::static_allocator) );
    classcore__Environment_Oval->setCreator(core::Environment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Environment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Environment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__ExternalObject_Oval]"));
    core::BuiltInClass_sp classcore__ExternalObject_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__ExternalObject_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ExternalObject_Oval,_lisp,core::ExternalObject_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::ExternalObject_O>::id,core::ExternalObject_O::static_classSymbol());
    core::ExternalObject_O::___staticClass = classcore__ExternalObject_Oval;
#ifdef USE_MPS
    core::ExternalObject_O::static_Kind = gctools::GCKind<core::ExternalObject_O>::Kind;
#endif
    core::af_setf_findClass(classcore__ExternalObject_Oval,core::ExternalObject_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::ExternalObject_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::ExternalObject_O>>::allocateClass();
        core::ExternalObject_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::ExternalObject_O::static_className() % (void*)(core::ExternalObject_O::static_allocator) );
    classcore__ExternalObject_Oval->setCreator(core::ExternalObject_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::ExternalObject_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__ExternalObject_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__FileStatus_Oval]"));
    core::BuiltInClass_sp classcore__FileStatus_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__FileStatus_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FileStatus_Oval,_lisp,core::FileStatus_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::FileStatus_O>::id,core::FileStatus_O::static_classSymbol());
    core::FileStatus_O::___staticClass = classcore__FileStatus_Oval;
#ifdef USE_MPS
    core::FileStatus_O::static_Kind = gctools::GCKind<core::FileStatus_O>::Kind;
#endif
    core::af_setf_findClass(classcore__FileStatus_Oval,core::FileStatus_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::FileStatus_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::FileStatus_O>>::allocateClass();
        core::FileStatus_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::FileStatus_O::static_className() % (void*)(core::FileStatus_O::static_allocator) );
    classcore__FileStatus_Oval->setCreator(core::FileStatus_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::FileStatus_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__FileStatus_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Function_Oval]"));
    core::BuiltInClass_sp classcore__Function_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Function_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Function_Oval,_lisp,core::Function_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Function_O>::id,core::Function_O::static_classSymbol());
    core::Function_O::___staticClass = classcore__Function_Oval;
#ifdef USE_MPS
    core::Function_O::static_Kind = gctools::GCKind<core::Function_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Function_Oval,core::Function_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Function_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Function_O>>::allocateClass();
        core::Function_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Function_O::static_className() % (void*)(core::Function_O::static_allocator) );
    classcore__Function_Oval->setCreator(core::Function_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Function_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Function_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__HashTable_Oval]"));
    core::BuiltInClass_sp classcore__HashTable_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__HashTable_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__HashTable_Oval,_lisp,core::HashTable_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::HashTable_O>::id,core::HashTable_O::static_classSymbol());
    core::HashTable_O::___staticClass = classcore__HashTable_Oval;
#ifdef USE_MPS
    core::HashTable_O::static_Kind = gctools::GCKind<core::HashTable_O>::Kind;
#endif
    core::af_setf_findClass(classcore__HashTable_Oval,core::HashTable_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::HashTable_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::HashTable_O>>::allocateClass();
        core::HashTable_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::HashTable_O::static_className() % (void*)(core::HashTable_O::static_allocator) );
    classcore__HashTable_Oval->setCreator(core::HashTable_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::HashTable_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__HashTable_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__IntArray_Oval]"));
    core::BuiltInClass_sp classcore__IntArray_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__IntArray_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__IntArray_Oval,_lisp,core::IntArray_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::IntArray_O>::id,core::IntArray_O::static_classSymbol());
    core::IntArray_O::___staticClass = classcore__IntArray_Oval;
#ifdef USE_MPS
    core::IntArray_O::static_Kind = gctools::GCKind<core::IntArray_O>::Kind;
#endif
    core::af_setf_findClass(classcore__IntArray_Oval,core::IntArray_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::IntArray_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::IntArray_O>>::allocateClass();
        core::IntArray_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::IntArray_O::static_className() % (void*)(core::IntArray_O::static_allocator) );
    classcore__IntArray_Oval->setCreator(core::IntArray_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::IntArray_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__IntArray_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Iterator_Oval]"));
    core::BuiltInClass_sp classcore__Iterator_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Iterator_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Iterator_Oval,_lisp,core::Iterator_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Iterator_O>::id,core::Iterator_O::static_classSymbol());
    core::Iterator_O::___staticClass = classcore__Iterator_Oval;
#ifdef USE_MPS
    core::Iterator_O::static_Kind = gctools::GCKind<core::Iterator_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Iterator_Oval,core::Iterator_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Iterator_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Iterator_O>>::allocateClass();
        core::Iterator_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Iterator_O::static_className() % (void*)(core::Iterator_O::static_allocator) );
    classcore__Iterator_Oval->setCreator(core::Iterator_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Iterator_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Iterator_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__LambdaListHandler_Oval]"));
    core::BuiltInClass_sp classcore__LambdaListHandler_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__LambdaListHandler_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__LambdaListHandler_Oval,_lisp,core::LambdaListHandler_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::LambdaListHandler_O>::id,core::LambdaListHandler_O::static_classSymbol());
    core::LambdaListHandler_O::___staticClass = classcore__LambdaListHandler_Oval;
#ifdef USE_MPS
    core::LambdaListHandler_O::static_Kind = gctools::GCKind<core::LambdaListHandler_O>::Kind;
#endif
    core::af_setf_findClass(classcore__LambdaListHandler_Oval,core::LambdaListHandler_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::LambdaListHandler_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::LambdaListHandler_O>>::allocateClass();
        core::LambdaListHandler_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::LambdaListHandler_O::static_className() % (void*)(core::LambdaListHandler_O::static_allocator) );
    classcore__LambdaListHandler_Oval->setCreator(core::LambdaListHandler_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::LambdaListHandler_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__LambdaListHandler_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__LightUserData_Oval]"));
    core::BuiltInClass_sp classcore__LightUserData_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__LightUserData_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__LightUserData_Oval,_lisp,core::LightUserData_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::LightUserData_O>::id,core::LightUserData_O::static_classSymbol());
    core::LightUserData_O::___staticClass = classcore__LightUserData_Oval;
#ifdef USE_MPS
    core::LightUserData_O::static_Kind = gctools::GCKind<core::LightUserData_O>::Kind;
#endif
    core::af_setf_findClass(classcore__LightUserData_Oval,core::LightUserData_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::LightUserData_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::LightUserData_O>>::allocateClass();
        core::LightUserData_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::LightUserData_O::static_className() % (void*)(core::LightUserData_O::static_allocator) );
    classcore__LightUserData_Oval->setCreator(core::LightUserData_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::LightUserData_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__LightUserData_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__LoadTimeValues_Oval]"));
    core::BuiltInClass_sp classcore__LoadTimeValues_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__LoadTimeValues_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__LoadTimeValues_Oval,_lisp,core::LoadTimeValues_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::LoadTimeValues_O>::id,core::LoadTimeValues_O::static_classSymbol());
    core::LoadTimeValues_O::___staticClass = classcore__LoadTimeValues_Oval;
#ifdef USE_MPS
    core::LoadTimeValues_O::static_Kind = gctools::GCKind<core::LoadTimeValues_O>::Kind;
#endif
    core::af_setf_findClass(classcore__LoadTimeValues_Oval,core::LoadTimeValues_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::LoadTimeValues_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::LoadTimeValues_O>>::allocateClass();
        core::LoadTimeValues_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::LoadTimeValues_O::static_className() % (void*)(core::LoadTimeValues_O::static_allocator) );
    classcore__LoadTimeValues_Oval->setCreator(core::LoadTimeValues_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::LoadTimeValues_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__LoadTimeValues_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__MicroHeap_Oval]"));
    core::BuiltInClass_sp classcore__MicroHeap_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__MicroHeap_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__MicroHeap_Oval,_lisp,core::MicroHeap_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::MicroHeap_O>::id,core::MicroHeap_O::static_classSymbol());
    core::MicroHeap_O::___staticClass = classcore__MicroHeap_Oval;
#ifdef USE_MPS
    core::MicroHeap_O::static_Kind = gctools::GCKind<core::MicroHeap_O>::Kind;
#endif
    core::af_setf_findClass(classcore__MicroHeap_Oval,core::MicroHeap_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::MicroHeap_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::MicroHeap_O>>::allocateClass();
        core::MicroHeap_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::MicroHeap_O::static_className() % (void*)(core::MicroHeap_O::static_allocator) );
    classcore__MicroHeap_Oval->setCreator(core::MicroHeap_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::MicroHeap_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__MicroHeap_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__MultiStringBuffer_Oval]"));
    core::BuiltInClass_sp classcore__MultiStringBuffer_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__MultiStringBuffer_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__MultiStringBuffer_Oval,_lisp,core::MultiStringBuffer_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::MultiStringBuffer_O>::id,core::MultiStringBuffer_O::static_classSymbol());
    core::MultiStringBuffer_O::___staticClass = classcore__MultiStringBuffer_Oval;
#ifdef USE_MPS
    core::MultiStringBuffer_O::static_Kind = gctools::GCKind<core::MultiStringBuffer_O>::Kind;
#endif
    core::af_setf_findClass(classcore__MultiStringBuffer_Oval,core::MultiStringBuffer_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::MultiStringBuffer_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::MultiStringBuffer_O>>::allocateClass();
        core::MultiStringBuffer_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::MultiStringBuffer_O::static_className() % (void*)(core::MultiStringBuffer_O::static_allocator) );
    classcore__MultiStringBuffer_Oval->setCreator(core::MultiStringBuffer_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::MultiStringBuffer_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__MultiStringBuffer_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Number_Oval]"));
    core::BuiltInClass_sp classcore__Number_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Number_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Number_Oval,_lisp,core::Number_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Number_O>::id,core::Number_O::static_classSymbol());
    core::Number_O::___staticClass = classcore__Number_Oval;
#ifdef USE_MPS
    core::Number_O::static_Kind = gctools::GCKind<core::Number_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Number_Oval,core::Number_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Number_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Number_O>>::allocateClass();
        core::Number_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Number_O::static_className() % (void*)(core::Number_O::static_allocator) );
    classcore__Number_Oval->setCreator(core::Number_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Number_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Number_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__ObjRef_Oval]"));
    core::BuiltInClass_sp classcore__ObjRef_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__ObjRef_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ObjRef_Oval,_lisp,core::ObjRef_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::ObjRef_O>::id,core::ObjRef_O::static_classSymbol());
    core::ObjRef_O::___staticClass = classcore__ObjRef_Oval;
#ifdef USE_MPS
    core::ObjRef_O::static_Kind = gctools::GCKind<core::ObjRef_O>::Kind;
#endif
    core::af_setf_findClass(classcore__ObjRef_Oval,core::ObjRef_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::ObjRef_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::ObjRef_O>>::allocateClass();
        core::ObjRef_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::ObjRef_O::static_className() % (void*)(core::ObjRef_O::static_allocator) );
    classcore__ObjRef_Oval->setCreator(core::ObjRef_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::ObjRef_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__ObjRef_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__ObjectSet_Oval]"));
    core::BuiltInClass_sp classcore__ObjectSet_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__ObjectSet_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ObjectSet_Oval,_lisp,core::ObjectSet_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::ObjectSet_O>::id,core::ObjectSet_O::static_classSymbol());
    core::ObjectSet_O::___staticClass = classcore__ObjectSet_Oval;
#ifdef USE_MPS
    core::ObjectSet_O::static_Kind = gctools::GCKind<core::ObjectSet_O>::Kind;
#endif
    core::af_setf_findClass(classcore__ObjectSet_Oval,core::ObjectSet_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::ObjectSet_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::ObjectSet_O>>::allocateClass();
        core::ObjectSet_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::ObjectSet_O::static_className() % (void*)(core::ObjectSet_O::static_allocator) );
    classcore__ObjectSet_Oval->setCreator(core::ObjectSet_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::ObjectSet_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__ObjectSet_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Package_Oval]"));
    core::BuiltInClass_sp classcore__Package_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Package_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Package_Oval,_lisp,core::Package_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Package_O>::id,core::Package_O::static_classSymbol());
    core::Package_O::___staticClass = classcore__Package_Oval;
#ifdef USE_MPS
    core::Package_O::static_Kind = gctools::GCKind<core::Package_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Package_Oval,core::Package_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Package_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Package_O>>::allocateClass();
        core::Package_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Package_O::static_className() % (void*)(core::Package_O::static_allocator) );
    classcore__Package_Oval->setCreator(core::Package_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Package_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Package_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Path_Oval]"));
    core::BuiltInClass_sp classcore__Path_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Path_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Path_Oval,_lisp,core::Path_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Path_O>::id,core::Path_O::static_classSymbol());
    core::Path_O::___staticClass = classcore__Path_Oval;
#ifdef USE_MPS
    core::Path_O::static_Kind = gctools::GCKind<core::Path_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Path_Oval,core::Path_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Path_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Path_O>>::allocateClass();
        core::Path_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Path_O::static_className() % (void*)(core::Path_O::static_allocator) );
    classcore__Path_Oval->setCreator(core::Path_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Path_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Path_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Pathname_Oval]"));
    core::BuiltInClass_sp classcore__Pathname_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Pathname_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Pathname_Oval,_lisp,core::Pathname_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Pathname_O>::id,core::Pathname_O::static_classSymbol());
    core::Pathname_O::___staticClass = classcore__Pathname_Oval;
#ifdef USE_MPS
    core::Pathname_O::static_Kind = gctools::GCKind<core::Pathname_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Pathname_Oval,core::Pathname_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Pathname_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Pathname_O>>::allocateClass();
        core::Pathname_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Pathname_O::static_className() % (void*)(core::Pathname_O::static_allocator) );
    classcore__Pathname_Oval->setCreator(core::Pathname_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Pathname_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Pathname_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Pointer_Oval]"));
    core::BuiltInClass_sp classcore__Pointer_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Pointer_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Pointer_Oval,_lisp,core::Pointer_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Pointer_O>::id,core::Pointer_O::static_classSymbol());
    core::Pointer_O::___staticClass = classcore__Pointer_Oval;
#ifdef USE_MPS
    core::Pointer_O::static_Kind = gctools::GCKind<core::Pointer_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Pointer_Oval,core::Pointer_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Pointer_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Pointer_O>>::allocateClass();
        core::Pointer_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Pointer_O::static_className() % (void*)(core::Pointer_O::static_allocator) );
    classcore__Pointer_Oval->setCreator(core::Pointer_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Pointer_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Pointer_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__PosixTimeDuration_Oval]"));
    core::BuiltInClass_sp classcore__PosixTimeDuration_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__PosixTimeDuration_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__PosixTimeDuration_Oval,_lisp,core::PosixTimeDuration_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::PosixTimeDuration_O>::id,core::PosixTimeDuration_O::static_classSymbol());
    core::PosixTimeDuration_O::___staticClass = classcore__PosixTimeDuration_Oval;
#ifdef USE_MPS
    core::PosixTimeDuration_O::static_Kind = gctools::GCKind<core::PosixTimeDuration_O>::Kind;
#endif
    core::af_setf_findClass(classcore__PosixTimeDuration_Oval,core::PosixTimeDuration_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::PosixTimeDuration_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::PosixTimeDuration_O>>::allocateClass();
        core::PosixTimeDuration_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::PosixTimeDuration_O::static_className() % (void*)(core::PosixTimeDuration_O::static_allocator) );
    classcore__PosixTimeDuration_Oval->setCreator(core::PosixTimeDuration_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::PosixTimeDuration_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__PosixTimeDuration_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__PosixTime_Oval]"));
    core::BuiltInClass_sp classcore__PosixTime_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__PosixTime_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__PosixTime_Oval,_lisp,core::PosixTime_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::PosixTime_O>::id,core::PosixTime_O::static_classSymbol());
    core::PosixTime_O::___staticClass = classcore__PosixTime_Oval;
#ifdef USE_MPS
    core::PosixTime_O::static_Kind = gctools::GCKind<core::PosixTime_O>::Kind;
#endif
    core::af_setf_findClass(classcore__PosixTime_Oval,core::PosixTime_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::PosixTime_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::PosixTime_O>>::allocateClass();
        core::PosixTime_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::PosixTime_O::static_className() % (void*)(core::PosixTime_O::static_allocator) );
    classcore__PosixTime_Oval->setCreator(core::PosixTime_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::PosixTime_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__PosixTime_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__ReadTable_Oval]"));
    core::BuiltInClass_sp classcore__ReadTable_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__ReadTable_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ReadTable_Oval,_lisp,core::ReadTable_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::ReadTable_O>::id,core::ReadTable_O::static_classSymbol());
    core::ReadTable_O::___staticClass = classcore__ReadTable_Oval;
#ifdef USE_MPS
    core::ReadTable_O::static_Kind = gctools::GCKind<core::ReadTable_O>::Kind;
#endif
    core::af_setf_findClass(classcore__ReadTable_Oval,core::ReadTable_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::ReadTable_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::ReadTable_O>>::allocateClass();
        core::ReadTable_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::ReadTable_O::static_className() % (void*)(core::ReadTable_O::static_allocator) );
    classcore__ReadTable_Oval->setCreator(core::ReadTable_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::ReadTable_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__ReadTable_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Reader_Oval]"));
    core::BuiltInClass_sp classcore__Reader_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Reader_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Reader_Oval,_lisp,core::Reader_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Reader_O>::id,core::Reader_O::static_classSymbol());
    core::Reader_O::___staticClass = classcore__Reader_Oval;
#ifdef USE_MPS
    core::Reader_O::static_Kind = gctools::GCKind<core::Reader_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Reader_Oval,core::Reader_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Reader_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Reader_O>>::allocateClass();
        core::Reader_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Reader_O::static_className() % (void*)(core::Reader_O::static_allocator) );
    classcore__Reader_Oval->setCreator(core::Reader_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Reader_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Reader_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__RegexMatch_Oval]"));
    core::BuiltInClass_sp classcore__RegexMatch_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__RegexMatch_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__RegexMatch_Oval,_lisp,core::RegexMatch_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::RegexMatch_O>::id,core::RegexMatch_O::static_classSymbol());
    core::RegexMatch_O::___staticClass = classcore__RegexMatch_Oval;
#ifdef USE_MPS
    core::RegexMatch_O::static_Kind = gctools::GCKind<core::RegexMatch_O>::Kind;
#endif
    core::af_setf_findClass(classcore__RegexMatch_Oval,core::RegexMatch_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::RegexMatch_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::RegexMatch_O>>::allocateClass();
        core::RegexMatch_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::RegexMatch_O::static_className() % (void*)(core::RegexMatch_O::static_allocator) );
    classcore__RegexMatch_Oval->setCreator(core::RegexMatch_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::RegexMatch_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__RegexMatch_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Regex_Oval]"));
    core::BuiltInClass_sp classcore__Regex_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Regex_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Regex_Oval,_lisp,core::Regex_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Regex_O>::id,core::Regex_O::static_classSymbol());
    core::Regex_O::___staticClass = classcore__Regex_Oval;
#ifdef USE_MPS
    core::Regex_O::static_Kind = gctools::GCKind<core::Regex_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Regex_Oval,core::Regex_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Regex_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Regex_O>>::allocateClass();
        core::Regex_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Regex_O::static_className() % (void*)(core::Regex_O::static_allocator) );
    classcore__Regex_Oval->setCreator(core::Regex_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Regex_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Regex_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SNode_Oval]"));
    core::BuiltInClass_sp classcore__SNode_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SNode_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SNode_Oval,_lisp,core::SNode_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SNode_O>::id,core::SNode_O::static_classSymbol());
    core::SNode_O::___staticClass = classcore__SNode_Oval;
#ifdef USE_MPS
    core::SNode_O::static_Kind = gctools::GCKind<core::SNode_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SNode_Oval,core::SNode_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SNode_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SNode_O>>::allocateClass();
        core::SNode_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SNode_O::static_className() % (void*)(core::SNode_O::static_allocator) );
    classcore__SNode_Oval->setCreator(core::SNode_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SNode_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SNode_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SingleDispatchMethod_Oval]"));
    core::BuiltInClass_sp classcore__SingleDispatchMethod_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SingleDispatchMethod_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SingleDispatchMethod_Oval,_lisp,core::SingleDispatchMethod_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SingleDispatchMethod_O>::id,core::SingleDispatchMethod_O::static_classSymbol());
    core::SingleDispatchMethod_O::___staticClass = classcore__SingleDispatchMethod_Oval;
#ifdef USE_MPS
    core::SingleDispatchMethod_O::static_Kind = gctools::GCKind<core::SingleDispatchMethod_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SingleDispatchMethod_Oval,core::SingleDispatchMethod_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SingleDispatchMethod_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SingleDispatchMethod_O>>::allocateClass();
        core::SingleDispatchMethod_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SingleDispatchMethod_O::static_className() % (void*)(core::SingleDispatchMethod_O::static_allocator) );
    classcore__SingleDispatchMethod_Oval->setCreator(core::SingleDispatchMethod_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SingleDispatchMethod_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SingleDispatchMethod_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SmallMap_Oval]"));
    core::BuiltInClass_sp classcore__SmallMap_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SmallMap_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SmallMap_Oval,_lisp,core::SmallMap_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SmallMap_O>::id,core::SmallMap_O::static_classSymbol());
    core::SmallMap_O::___staticClass = classcore__SmallMap_Oval;
#ifdef USE_MPS
    core::SmallMap_O::static_Kind = gctools::GCKind<core::SmallMap_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SmallMap_Oval,core::SmallMap_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SmallMap_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SmallMap_O>>::allocateClass();
        core::SmallMap_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SmallMap_O::static_className() % (void*)(core::SmallMap_O::static_allocator) );
    classcore__SmallMap_Oval->setCreator(core::SmallMap_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SmallMap_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SmallMap_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SourceFileInfo_Oval]"));
    core::BuiltInClass_sp classcore__SourceFileInfo_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SourceFileInfo_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SourceFileInfo_Oval,_lisp,core::SourceFileInfo_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SourceFileInfo_O>::id,core::SourceFileInfo_O::static_classSymbol());
    core::SourceFileInfo_O::___staticClass = classcore__SourceFileInfo_Oval;
#ifdef USE_MPS
    core::SourceFileInfo_O::static_Kind = gctools::GCKind<core::SourceFileInfo_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SourceFileInfo_Oval,core::SourceFileInfo_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SourceFileInfo_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SourceFileInfo_O>>::allocateClass();
        core::SourceFileInfo_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SourceFileInfo_O::static_className() % (void*)(core::SourceFileInfo_O::static_allocator) );
    classcore__SourceFileInfo_Oval->setCreator(core::SourceFileInfo_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SourceFileInfo_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SourceFileInfo_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SourceManager_Oval]"));
    core::BuiltInClass_sp classcore__SourceManager_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SourceManager_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SourceManager_Oval,_lisp,core::SourceManager_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SourceManager_O>::id,core::SourceManager_O::static_classSymbol());
    core::SourceManager_O::___staticClass = classcore__SourceManager_Oval;
#ifdef USE_MPS
    core::SourceManager_O::static_Kind = gctools::GCKind<core::SourceManager_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SourceManager_Oval,core::SourceManager_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SourceManager_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SourceManager_O>>::allocateClass();
        core::SourceManager_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SourceManager_O::static_className() % (void*)(core::SourceManager_O::static_allocator) );
    classcore__SourceManager_Oval->setCreator(core::SourceManager_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SourceManager_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SourceManager_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SourcePosInfo_Oval]"));
    core::BuiltInClass_sp classcore__SourcePosInfo_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SourcePosInfo_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SourcePosInfo_Oval,_lisp,core::SourcePosInfo_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SourcePosInfo_O>::id,core::SourcePosInfo_O::static_classSymbol());
    core::SourcePosInfo_O::___staticClass = classcore__SourcePosInfo_Oval;
#ifdef USE_MPS
    core::SourcePosInfo_O::static_Kind = gctools::GCKind<core::SourcePosInfo_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SourcePosInfo_Oval,core::SourcePosInfo_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SourcePosInfo_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SourcePosInfo_O>>::allocateClass();
        core::SourcePosInfo_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SourcePosInfo_O::static_className() % (void*)(core::SourcePosInfo_O::static_allocator) );
    classcore__SourcePosInfo_Oval->setCreator(core::SourcePosInfo_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SourcePosInfo_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SourcePosInfo_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__StandardObject_Oval]"));
    StandardClass_sp classcore__StandardObject_Oval = StandardClass_O::createUncollectable();
    classcore__StandardObject_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StandardObject_Oval,_lisp,core::StandardObject_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::StandardObject_O>::id,core::StandardObject_O::static_classSymbol());
    core::StandardObject_O::___staticClass = classcore__StandardObject_Oval;
#ifdef USE_MPS
    core::StandardObject_O::static_Kind = gctools::GCKind<core::StandardObject_O>::Kind;
#endif
    core::af_setf_findClass(classcore__StandardObject_Oval,core::StandardObject_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::StandardObject_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::StandardObject_O>>::allocateClass();
        core::StandardObject_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::StandardObject_O::static_className() % (void*)(core::StandardObject_O::static_allocator) );
    classcore__StandardObject_Oval->setCreator(core::StandardObject_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::StandardObject_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__StandardObject_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Stream_Oval]"));
    core::BuiltInClass_sp classcore__Stream_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Stream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Stream_Oval,_lisp,core::Stream_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Stream_O>::id,core::Stream_O::static_classSymbol());
    core::Stream_O::___staticClass = classcore__Stream_Oval;
#ifdef USE_MPS
    core::Stream_O::static_Kind = gctools::GCKind<core::Stream_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Stream_Oval,core::Stream_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Stream_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Stream_O>>::allocateClass();
        core::Stream_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Stream_O::static_className() % (void*)(core::Stream_O::static_allocator) );
    classcore__Stream_Oval->setCreator(core::Stream_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Stream_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Stream_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__StringList_Oval]"));
    core::BuiltInClass_sp classcore__StringList_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__StringList_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StringList_Oval,_lisp,core::StringList_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::StringList_O>::id,core::StringList_O::static_classSymbol());
    core::StringList_O::___staticClass = classcore__StringList_Oval;
#ifdef USE_MPS
    core::StringList_O::static_Kind = gctools::GCKind<core::StringList_O>::Kind;
#endif
    core::af_setf_findClass(classcore__StringList_Oval,core::StringList_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::StringList_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::StringList_O>>::allocateClass();
        core::StringList_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::StringList_O::static_className() % (void*)(core::StringList_O::static_allocator) );
    classcore__StringList_Oval->setCreator(core::StringList_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::StringList_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__StringList_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__StringSet_Oval]"));
    core::BuiltInClass_sp classcore__StringSet_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__StringSet_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StringSet_Oval,_lisp,core::StringSet_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::StringSet_O>::id,core::StringSet_O::static_classSymbol());
    core::StringSet_O::___staticClass = classcore__StringSet_Oval;
#ifdef USE_MPS
    core::StringSet_O::static_Kind = gctools::GCKind<core::StringSet_O>::Kind;
#endif
    core::af_setf_findClass(classcore__StringSet_Oval,core::StringSet_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::StringSet_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::StringSet_O>>::allocateClass();
        core::StringSet_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::StringSet_O::static_className() % (void*)(core::StringSet_O::static_allocator) );
    classcore__StringSet_Oval->setCreator(core::StringSet_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::StringSet_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__StringSet_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__StructureObject_Oval]"));
    StructureClass_sp classcore__StructureObject_Oval = StructureClass_O::createUncollectable();
    classcore__StructureObject_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StructureObject_Oval,_lisp,core::StructureObject_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::StructureObject_O>::id,core::StructureObject_O::static_classSymbol());
    core::StructureObject_O::___staticClass = classcore__StructureObject_Oval;
#ifdef USE_MPS
    core::StructureObject_O::static_Kind = gctools::GCKind<core::StructureObject_O>::Kind;
#endif
    core::af_setf_findClass(classcore__StructureObject_Oval,core::StructureObject_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::StructureObject_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::StructureObject_O>>::allocateClass();
        core::StructureObject_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::StructureObject_O::static_className() % (void*)(core::StructureObject_O::static_allocator) );
    classcore__StructureObject_Oval->setCreator(core::StructureObject_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::StructureObject_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__StructureObject_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SymbolList_Oval]"));
    core::BuiltInClass_sp classcore__SymbolList_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SymbolList_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SymbolList_Oval,_lisp,core::SymbolList_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SymbolList_O>::id,core::SymbolList_O::static_classSymbol());
    core::SymbolList_O::___staticClass = classcore__SymbolList_Oval;
#ifdef USE_MPS
    core::SymbolList_O::static_Kind = gctools::GCKind<core::SymbolList_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SymbolList_Oval,core::SymbolList_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SymbolList_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SymbolList_O>>::allocateClass();
        core::SymbolList_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SymbolList_O::static_className() % (void*)(core::SymbolList_O::static_allocator) );
    classcore__SymbolList_Oval->setCreator(core::SymbolList_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SymbolList_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SymbolList_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SymbolSet_Oval]"));
    core::BuiltInClass_sp classcore__SymbolSet_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SymbolSet_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SymbolSet_Oval,_lisp,core::SymbolSet_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SymbolSet_O>::id,core::SymbolSet_O::static_classSymbol());
    core::SymbolSet_O::___staticClass = classcore__SymbolSet_Oval;
#ifdef USE_MPS
    core::SymbolSet_O::static_Kind = gctools::GCKind<core::SymbolSet_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SymbolSet_Oval,core::SymbolSet_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SymbolSet_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SymbolSet_O>>::allocateClass();
        core::SymbolSet_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SymbolSet_O::static_className() % (void*)(core::SymbolSet_O::static_allocator) );
    classcore__SymbolSet_Oval->setCreator(core::SymbolSet_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SymbolSet_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SymbolSet_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SymbolToEnumConverter_Oval]"));
    core::BuiltInClass_sp classcore__SymbolToEnumConverter_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SymbolToEnumConverter_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SymbolToEnumConverter_Oval,_lisp,core::SymbolToEnumConverter_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SymbolToEnumConverter_O>::id,core::SymbolToEnumConverter_O::static_classSymbol());
    core::SymbolToEnumConverter_O::___staticClass = classcore__SymbolToEnumConverter_Oval;
#ifdef USE_MPS
    core::SymbolToEnumConverter_O::static_Kind = gctools::GCKind<core::SymbolToEnumConverter_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SymbolToEnumConverter_Oval,core::SymbolToEnumConverter_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SymbolToEnumConverter_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SymbolToEnumConverter_O>>::allocateClass();
        core::SymbolToEnumConverter_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SymbolToEnumConverter_O::static_className() % (void*)(core::SymbolToEnumConverter_O::static_allocator) );
    classcore__SymbolToEnumConverter_Oval->setCreator(core::SymbolToEnumConverter_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SymbolToEnumConverter_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SymbolToEnumConverter_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Symbol_Oval]"));
    core::BuiltInClass_sp classcore__Symbol_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Symbol_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Symbol_Oval,_lisp,core::Symbol_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Symbol_O>::id,core::Symbol_O::static_classSymbol());
    core::Symbol_O::___staticClass = classcore__Symbol_Oval;
#ifdef USE_MPS
    core::Symbol_O::static_Kind = gctools::GCKind<core::Symbol_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Symbol_Oval,core::Symbol_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Symbol_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Symbol_O>>::allocateClass();
        core::Symbol_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Symbol_O::static_className() % (void*)(core::Symbol_O::static_allocator) );
    classcore__Symbol_Oval->setCreator(core::Symbol_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Symbol_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Symbol_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__WeakHashTable_Oval]"));
    core::BuiltInClass_sp classcore__WeakHashTable_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__WeakHashTable_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__WeakHashTable_Oval,_lisp,core::WeakHashTable_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::WeakHashTable_O>::id,core::WeakHashTable_O::static_classSymbol());
    core::WeakHashTable_O::___staticClass = classcore__WeakHashTable_Oval;
#ifdef USE_MPS
    core::WeakHashTable_O::static_Kind = gctools::GCKind<core::WeakHashTable_O>::Kind;
#endif
    core::af_setf_findClass(classcore__WeakHashTable_Oval,core::WeakHashTable_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::WeakHashTable_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::WeakHashTable_O>>::allocateClass();
        core::WeakHashTable_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::WeakHashTable_O::static_className() % (void*)(core::WeakHashTable_O::static_allocator) );
    classcore__WeakHashTable_Oval->setCreator(core::WeakHashTable_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::WeakHashTable_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__WeakHashTable_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__WeakKeyMapping_Oval]"));
    core::BuiltInClass_sp classcore__WeakKeyMapping_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__WeakKeyMapping_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__WeakKeyMapping_Oval,_lisp,core::WeakKeyMapping_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::WeakKeyMapping_O>::id,core::WeakKeyMapping_O::static_classSymbol());
    core::WeakKeyMapping_O::___staticClass = classcore__WeakKeyMapping_Oval;
#ifdef USE_MPS
    core::WeakKeyMapping_O::static_Kind = gctools::GCKind<core::WeakKeyMapping_O>::Kind;
#endif
    core::af_setf_findClass(classcore__WeakKeyMapping_Oval,core::WeakKeyMapping_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::WeakKeyMapping_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::WeakKeyMapping_O>>::allocateClass();
        core::WeakKeyMapping_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::WeakKeyMapping_O::static_className() % (void*)(core::WeakKeyMapping_O::static_allocator) );
    classcore__WeakKeyMapping_Oval->setCreator(core::WeakKeyMapping_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::WeakKeyMapping_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__WeakKeyMapping_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__WeakPointer_Oval]"));
    core::BuiltInClass_sp classcore__WeakPointer_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__WeakPointer_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__WeakPointer_Oval,_lisp,core::WeakPointer_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::WeakPointer_O>::id,core::WeakPointer_O::static_classSymbol());
    core::WeakPointer_O::___staticClass = classcore__WeakPointer_Oval;
#ifdef USE_MPS
    core::WeakPointer_O::static_Kind = gctools::GCKind<core::WeakPointer_O>::Kind;
#endif
    core::af_setf_findClass(classcore__WeakPointer_Oval,core::WeakPointer_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::WeakPointer_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::WeakPointer_O>>::allocateClass();
        core::WeakPointer_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::WeakPointer_O::static_className() % (void*)(core::WeakPointer_O::static_allocator) );
    classcore__WeakPointer_Oval->setCreator(core::WeakPointer_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::WeakPointer_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__WeakPointer_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__WrappedPointer_Oval]"));
    core::BuiltInClass_sp classcore__WrappedPointer_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__WrappedPointer_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__WrappedPointer_Oval,_lisp,core::WrappedPointer_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::WrappedPointer_O>::id,core::WrappedPointer_O::static_classSymbol());
    core::WrappedPointer_O::___staticClass = classcore__WrappedPointer_Oval;
#ifdef USE_MPS
    core::WrappedPointer_O::static_Kind = gctools::GCKind<core::WrappedPointer_O>::Kind;
#endif
    core::af_setf_findClass(classcore__WrappedPointer_Oval,core::WrappedPointer_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::WrappedPointer_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::WrappedPointer_O>>::allocateClass();
        core::WrappedPointer_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::WrappedPointer_O::static_className() % (void*)(core::WrappedPointer_O::static_allocator) );
    classcore__WrappedPointer_Oval->setCreator(core::WrappedPointer_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::WrappedPointer_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__WrappedPointer_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__ActivationFrame_Oval]"));
    core::BuiltInClass_sp classcore__ActivationFrame_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__ActivationFrame_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ActivationFrame_Oval,_lisp,core::ActivationFrame_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::ActivationFrame_O>::id,core::ActivationFrame_O::static_classSymbol());
    core::ActivationFrame_O::___staticClass = classcore__ActivationFrame_Oval;
#ifdef USE_MPS
    core::ActivationFrame_O::static_Kind = gctools::GCKind<core::ActivationFrame_O>::Kind;
#endif
    core::af_setf_findClass(classcore__ActivationFrame_Oval,core::ActivationFrame_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::ActivationFrame_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::ActivationFrame_O>>::allocateClass();
        core::ActivationFrame_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::ActivationFrame_O::static_className() % (void*)(core::ActivationFrame_O::static_allocator) );
    classcore__ActivationFrame_Oval->setCreator(core::ActivationFrame_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::ActivationFrame_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__ActivationFrame_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__AnsiStream_Oval]"));
    core::BuiltInClass_sp classcore__AnsiStream_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__AnsiStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__AnsiStream_Oval,_lisp,core::AnsiStream_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::AnsiStream_O>::id,core::AnsiStream_O::static_classSymbol());
    core::AnsiStream_O::___staticClass = classcore__AnsiStream_Oval;
#ifdef USE_MPS
    core::AnsiStream_O::static_Kind = gctools::GCKind<core::AnsiStream_O>::Kind;
#endif
    core::af_setf_findClass(classcore__AnsiStream_Oval,core::AnsiStream_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::AnsiStream_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::AnsiStream_O>>::allocateClass();
        core::AnsiStream_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::AnsiStream_O::static_className() % (void*)(core::AnsiStream_O::static_allocator) );
    classcore__AnsiStream_Oval->setCreator(core::AnsiStream_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::AnsiStream_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__AnsiStream_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__ArrayDisplaced_Oval]"));
    core::BuiltInClass_sp classcore__ArrayDisplaced_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__ArrayDisplaced_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ArrayDisplaced_Oval,_lisp,core::ArrayDisplaced_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::ArrayDisplaced_O>::id,core::ArrayDisplaced_O::static_classSymbol());
    core::ArrayDisplaced_O::___staticClass = classcore__ArrayDisplaced_Oval;
#ifdef USE_MPS
    core::ArrayDisplaced_O::static_Kind = gctools::GCKind<core::ArrayDisplaced_O>::Kind;
#endif
    core::af_setf_findClass(classcore__ArrayDisplaced_Oval,core::ArrayDisplaced_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::ArrayDisplaced_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::ArrayDisplaced_O>>::allocateClass();
        core::ArrayDisplaced_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::ArrayDisplaced_O::static_className() % (void*)(core::ArrayDisplaced_O::static_allocator) );
    classcore__ArrayDisplaced_Oval->setCreator(core::ArrayDisplaced_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::ArrayDisplaced_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__ArrayDisplaced_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__ArrayObjects_Oval]"));
    core::BuiltInClass_sp classcore__ArrayObjects_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__ArrayObjects_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ArrayObjects_Oval,_lisp,core::ArrayObjects_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::ArrayObjects_O>::id,core::ArrayObjects_O::static_classSymbol());
    core::ArrayObjects_O::___staticClass = classcore__ArrayObjects_Oval;
#ifdef USE_MPS
    core::ArrayObjects_O::static_Kind = gctools::GCKind<core::ArrayObjects_O>::Kind;
#endif
    core::af_setf_findClass(classcore__ArrayObjects_Oval,core::ArrayObjects_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::ArrayObjects_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::ArrayObjects_O>>::allocateClass();
        core::ArrayObjects_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::ArrayObjects_O::static_className() % (void*)(core::ArrayObjects_O::static_allocator) );
    classcore__ArrayObjects_Oval->setCreator(core::ArrayObjects_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::ArrayObjects_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__ArrayObjects_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__BranchSNode_Oval]"));
    core::BuiltInClass_sp classcore__BranchSNode_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__BranchSNode_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__BranchSNode_Oval,_lisp,core::BranchSNode_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::BranchSNode_O>::id,core::BranchSNode_O::static_classSymbol());
    core::BranchSNode_O::___staticClass = classcore__BranchSNode_Oval;
#ifdef USE_MPS
    core::BranchSNode_O::static_Kind = gctools::GCKind<core::BranchSNode_O>::Kind;
#endif
    core::af_setf_findClass(classcore__BranchSNode_Oval,core::BranchSNode_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::BranchSNode_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::BranchSNode_O>>::allocateClass();
        core::BranchSNode_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::BranchSNode_O::static_className() % (void*)(core::BranchSNode_O::static_allocator) );
    classcore__BranchSNode_Oval->setCreator(core::BranchSNode_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::BranchSNode_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__BranchSNode_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__CompiledFunction_Oval]"));
    core::BuiltInClass_sp classcore__CompiledFunction_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__CompiledFunction_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__CompiledFunction_Oval,_lisp,core::CompiledFunction_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::CompiledFunction_O>::id,core::CompiledFunction_O::static_classSymbol());
    core::CompiledFunction_O::___staticClass = classcore__CompiledFunction_Oval;
#ifdef USE_MPS
    core::CompiledFunction_O::static_Kind = gctools::GCKind<core::CompiledFunction_O>::Kind;
#endif
    core::af_setf_findClass(classcore__CompiledFunction_Oval,core::CompiledFunction_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::CompiledFunction_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::CompiledFunction_O>>::allocateClass();
        core::CompiledFunction_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::CompiledFunction_O::static_className() % (void*)(core::CompiledFunction_O::static_allocator) );
    classcore__CompiledFunction_Oval->setCreator(core::CompiledFunction_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::CompiledFunction_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__CompiledFunction_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Complex_Oval]"));
    core::BuiltInClass_sp classcore__Complex_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Complex_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Complex_Oval,_lisp,core::Complex_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Complex_O>::id,core::Complex_O::static_classSymbol());
    core::Complex_O::___staticClass = classcore__Complex_Oval;
#ifdef USE_MPS
    core::Complex_O::static_Kind = gctools::GCKind<core::Complex_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Complex_Oval,core::Complex_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Complex_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Complex_O>>::allocateClass();
        core::Complex_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Complex_O::static_className() % (void*)(core::Complex_O::static_allocator) );
    classcore__Complex_Oval->setCreator(core::Complex_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Complex_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Complex_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__DirectoryIterator_Oval]"));
    core::BuiltInClass_sp classcore__DirectoryIterator_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__DirectoryIterator_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__DirectoryIterator_Oval,_lisp,core::DirectoryIterator_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::DirectoryIterator_O>::id,core::DirectoryIterator_O::static_classSymbol());
    core::DirectoryIterator_O::___staticClass = classcore__DirectoryIterator_Oval;
#ifdef USE_MPS
    core::DirectoryIterator_O::static_Kind = gctools::GCKind<core::DirectoryIterator_O>::Kind;
#endif
    core::af_setf_findClass(classcore__DirectoryIterator_Oval,core::DirectoryIterator_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::DirectoryIterator_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::DirectoryIterator_O>>::allocateClass();
        core::DirectoryIterator_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::DirectoryIterator_O::static_className() % (void*)(core::DirectoryIterator_O::static_allocator) );
    classcore__DirectoryIterator_Oval->setCreator(core::DirectoryIterator_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::DirectoryIterator_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__DirectoryIterator_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__ForeignData_Oval]"));
    core::BuiltInClass_sp classcore__ForeignData_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__ForeignData_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ForeignData_Oval,_lisp,core::ForeignData_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::ForeignData_O>::id,core::ForeignData_O::static_classSymbol());
    core::ForeignData_O::___staticClass = classcore__ForeignData_Oval;
#ifdef USE_MPS
    core::ForeignData_O::static_Kind = gctools::GCKind<core::ForeignData_O>::Kind;
#endif
    core::af_setf_findClass(classcore__ForeignData_Oval,core::ForeignData_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::ForeignData_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::ForeignData_O>>::allocateClass();
        core::ForeignData_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::ForeignData_O::static_className() % (void*)(core::ForeignData_O::static_allocator) );
    classcore__ForeignData_Oval->setCreator(core::ForeignData_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::ForeignData_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__ForeignData_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__GlueEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__GlueEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__GlueEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__GlueEnvironment_Oval,_lisp,core::GlueEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::GlueEnvironment_O>::id,core::GlueEnvironment_O::static_classSymbol());
    core::GlueEnvironment_O::___staticClass = classcore__GlueEnvironment_Oval;
#ifdef USE_MPS
    core::GlueEnvironment_O::static_Kind = gctools::GCKind<core::GlueEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__GlueEnvironment_Oval,core::GlueEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::GlueEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::GlueEnvironment_O>>::allocateClass();
        core::GlueEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::GlueEnvironment_O::static_className() % (void*)(core::GlueEnvironment_O::static_allocator) );
    classcore__GlueEnvironment_Oval->setCreator(core::GlueEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::GlueEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__GlueEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__HashTableEq_Oval]"));
    core::BuiltInClass_sp classcore__HashTableEq_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__HashTableEq_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__HashTableEq_Oval,_lisp,core::HashTableEq_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::HashTableEq_O>::id,core::HashTableEq_O::static_classSymbol());
    core::HashTableEq_O::___staticClass = classcore__HashTableEq_Oval;
#ifdef USE_MPS
    core::HashTableEq_O::static_Kind = gctools::GCKind<core::HashTableEq_O>::Kind;
#endif
    core::af_setf_findClass(classcore__HashTableEq_Oval,core::HashTableEq_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::HashTableEq_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::HashTableEq_O>>::allocateClass();
        core::HashTableEq_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::HashTableEq_O::static_className() % (void*)(core::HashTableEq_O::static_allocator) );
    classcore__HashTableEq_Oval->setCreator(core::HashTableEq_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::HashTableEq_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__HashTableEq_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__HashTableEql_Oval]"));
    core::BuiltInClass_sp classcore__HashTableEql_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__HashTableEql_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__HashTableEql_Oval,_lisp,core::HashTableEql_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::HashTableEql_O>::id,core::HashTableEql_O::static_classSymbol());
    core::HashTableEql_O::___staticClass = classcore__HashTableEql_Oval;
#ifdef USE_MPS
    core::HashTableEql_O::static_Kind = gctools::GCKind<core::HashTableEql_O>::Kind;
#endif
    core::af_setf_findClass(classcore__HashTableEql_Oval,core::HashTableEql_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::HashTableEql_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::HashTableEql_O>>::allocateClass();
        core::HashTableEql_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::HashTableEql_O::static_className() % (void*)(core::HashTableEql_O::static_allocator) );
    classcore__HashTableEql_Oval->setCreator(core::HashTableEql_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::HashTableEql_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__HashTableEql_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__HashTableEqual_Oval]"));
    core::BuiltInClass_sp classcore__HashTableEqual_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__HashTableEqual_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__HashTableEqual_Oval,_lisp,core::HashTableEqual_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::HashTableEqual_O>::id,core::HashTableEqual_O::static_classSymbol());
    core::HashTableEqual_O::___staticClass = classcore__HashTableEqual_Oval;
#ifdef USE_MPS
    core::HashTableEqual_O::static_Kind = gctools::GCKind<core::HashTableEqual_O>::Kind;
#endif
    core::af_setf_findClass(classcore__HashTableEqual_Oval,core::HashTableEqual_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::HashTableEqual_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::HashTableEqual_O>>::allocateClass();
        core::HashTableEqual_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::HashTableEqual_O::static_className() % (void*)(core::HashTableEqual_O::static_allocator) );
    classcore__HashTableEqual_Oval->setCreator(core::HashTableEqual_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::HashTableEqual_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__HashTableEqual_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__HashTableEqualp_Oval]"));
    core::BuiltInClass_sp classcore__HashTableEqualp_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__HashTableEqualp_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__HashTableEqualp_Oval,_lisp,core::HashTableEqualp_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::HashTableEqualp_O>::id,core::HashTableEqualp_O::static_classSymbol());
    core::HashTableEqualp_O::___staticClass = classcore__HashTableEqualp_Oval;
#ifdef USE_MPS
    core::HashTableEqualp_O::static_Kind = gctools::GCKind<core::HashTableEqualp_O>::Kind;
#endif
    core::af_setf_findClass(classcore__HashTableEqualp_Oval,core::HashTableEqualp_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::HashTableEqualp_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::HashTableEqualp_O>>::allocateClass();
        core::HashTableEqualp_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::HashTableEqualp_O::static_className() % (void*)(core::HashTableEqualp_O::static_allocator) );
    classcore__HashTableEqualp_Oval->setCreator(core::HashTableEqualp_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::HashTableEqualp_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__HashTableEqualp_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Instance_Oval]"));
    core::BuiltInClass_sp classcore__Instance_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Instance_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Instance_Oval,_lisp,core::Instance_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Instance_O>::id,core::Instance_O::static_classSymbol());
    core::Instance_O::___staticClass = classcore__Instance_Oval;
#ifdef USE_MPS
    core::Instance_O::static_Kind = gctools::GCKind<core::Instance_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Instance_Oval,core::Instance_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Instance_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Instance_O>>::allocateClass();
        core::Instance_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Instance_O::static_className() % (void*)(core::Instance_O::static_allocator) );
    classcore__Instance_Oval->setCreator(core::Instance_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Instance_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Instance_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__LeafSNode_Oval]"));
    core::BuiltInClass_sp classcore__LeafSNode_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__LeafSNode_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__LeafSNode_Oval,_lisp,core::LeafSNode_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::LeafSNode_O>::id,core::LeafSNode_O::static_classSymbol());
    core::LeafSNode_O::___staticClass = classcore__LeafSNode_Oval;
#ifdef USE_MPS
    core::LeafSNode_O::static_Kind = gctools::GCKind<core::LeafSNode_O>::Kind;
#endif
    core::af_setf_findClass(classcore__LeafSNode_Oval,core::LeafSNode_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::LeafSNode_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::LeafSNode_O>>::allocateClass();
        core::LeafSNode_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::LeafSNode_O::static_className() % (void*)(core::LeafSNode_O::static_allocator) );
    classcore__LeafSNode_Oval->setCreator(core::LeafSNode_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::LeafSNode_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__LeafSNode_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__LexicalEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__LexicalEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__LexicalEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__LexicalEnvironment_Oval,_lisp,core::LexicalEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::LexicalEnvironment_O>::id,core::LexicalEnvironment_O::static_classSymbol());
    core::LexicalEnvironment_O::___staticClass = classcore__LexicalEnvironment_Oval;
#ifdef USE_MPS
    core::LexicalEnvironment_O::static_Kind = gctools::GCKind<core::LexicalEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__LexicalEnvironment_Oval,core::LexicalEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::LexicalEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::LexicalEnvironment_O>>::allocateClass();
        core::LexicalEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::LexicalEnvironment_O::static_className() % (void*)(core::LexicalEnvironment_O::static_allocator) );
    classcore__LexicalEnvironment_Oval->setCreator(core::LexicalEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::LexicalEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__LexicalEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__LoadArchive_Oval]"));
    core::BuiltInClass_sp classcore__LoadArchive_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__LoadArchive_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__LoadArchive_Oval,_lisp,core::LoadArchive_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::LoadArchive_O>::id,core::LoadArchive_O::static_classSymbol());
    core::LoadArchive_O::___staticClass = classcore__LoadArchive_Oval;
#ifdef USE_MPS
    core::LoadArchive_O::static_Kind = gctools::GCKind<core::LoadArchive_O>::Kind;
#endif
    core::af_setf_findClass(classcore__LoadArchive_Oval,core::LoadArchive_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::LoadArchive_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::LoadArchive_O>>::allocateClass();
        core::LoadArchive_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::LoadArchive_O::static_className() % (void*)(core::LoadArchive_O::static_allocator) );
    classcore__LoadArchive_Oval->setCreator(core::LoadArchive_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::LoadArchive_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__LoadArchive_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__LogicalPathname_Oval]"));
    core::BuiltInClass_sp classcore__LogicalPathname_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__LogicalPathname_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__LogicalPathname_Oval,_lisp,core::LogicalPathname_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::LogicalPathname_O>::id,core::LogicalPathname_O::static_classSymbol());
    core::LogicalPathname_O::___staticClass = classcore__LogicalPathname_Oval;
#ifdef USE_MPS
    core::LogicalPathname_O::static_Kind = gctools::GCKind<core::LogicalPathname_O>::Kind;
#endif
    core::af_setf_findClass(classcore__LogicalPathname_Oval,core::LogicalPathname_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::LogicalPathname_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::LogicalPathname_O>>::allocateClass();
        core::LogicalPathname_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::LogicalPathname_O::static_className() % (void*)(core::LogicalPathname_O::static_allocator) );
    classcore__LogicalPathname_Oval->setCreator(core::LogicalPathname_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::LogicalPathname_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__LogicalPathname_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Metaobject_Oval]"));
    StandardClass_sp classcore__Metaobject_Oval = StandardClass_O::createUncollectable();
    classcore__Metaobject_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Metaobject_Oval,_lisp,core::Metaobject_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Metaobject_O>::id,core::Metaobject_O::static_classSymbol());
    core::Metaobject_O::___staticClass = classcore__Metaobject_Oval;
#ifdef USE_MPS
    core::Metaobject_O::static_Kind = gctools::GCKind<core::Metaobject_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Metaobject_Oval,core::Metaobject_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Metaobject_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Metaobject_O>>::allocateClass();
        core::Metaobject_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Metaobject_O::static_className() % (void*)(core::Metaobject_O::static_allocator) );
    classcore__Metaobject_Oval->setCreator(core::Metaobject_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Metaobject_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Metaobject_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Null_Oval]"));
    core::BuiltInClass_sp classcore__Null_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Null_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Null_Oval,_lisp,core::Null_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Null_O>::id,core::Null_O::static_classSymbol());
    core::Null_O::___staticClass = classcore__Null_Oval;
#ifdef USE_MPS
    core::Null_O::static_Kind = gctools::GCKind<core::Null_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Null_Oval,core::Null_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Null_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Null_O>>::allocateClass();
        core::Null_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Null_O::static_className() % (void*)(core::Null_O::static_allocator) );
    classcore__Null_Oval->setCreator(core::Null_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Null_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Null_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Real_Oval]"));
    core::BuiltInClass_sp classcore__Real_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Real_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Real_Oval,_lisp,core::Real_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Real_O>::id,core::Real_O::static_classSymbol());
    core::Real_O::___staticClass = classcore__Real_Oval;
#ifdef USE_MPS
    core::Real_O::static_Kind = gctools::GCKind<core::Real_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Real_Oval,core::Real_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Real_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Real_O>>::allocateClass();
        core::Real_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Real_O::static_className() % (void*)(core::Real_O::static_allocator) );
    classcore__Real_Oval->setCreator(core::Real_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Real_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Real_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__RecursiveDirectoryIterator_Oval]"));
    core::BuiltInClass_sp classcore__RecursiveDirectoryIterator_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__RecursiveDirectoryIterator_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__RecursiveDirectoryIterator_Oval,_lisp,core::RecursiveDirectoryIterator_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::RecursiveDirectoryIterator_O>::id,core::RecursiveDirectoryIterator_O::static_classSymbol());
    core::RecursiveDirectoryIterator_O::___staticClass = classcore__RecursiveDirectoryIterator_Oval;
#ifdef USE_MPS
    core::RecursiveDirectoryIterator_O::static_Kind = gctools::GCKind<core::RecursiveDirectoryIterator_O>::Kind;
#endif
    core::af_setf_findClass(classcore__RecursiveDirectoryIterator_Oval,core::RecursiveDirectoryIterator_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::RecursiveDirectoryIterator_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::RecursiveDirectoryIterator_O>>::allocateClass();
        core::RecursiveDirectoryIterator_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::RecursiveDirectoryIterator_O::static_className() % (void*)(core::RecursiveDirectoryIterator_O::static_allocator) );
    classcore__RecursiveDirectoryIterator_Oval->setCreator(core::RecursiveDirectoryIterator_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::RecursiveDirectoryIterator_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__RecursiveDirectoryIterator_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SaveArchive_Oval]"));
    core::BuiltInClass_sp classcore__SaveArchive_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SaveArchive_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SaveArchive_Oval,_lisp,core::SaveArchive_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SaveArchive_O>::id,core::SaveArchive_O::static_classSymbol());
    core::SaveArchive_O::___staticClass = classcore__SaveArchive_Oval;
#ifdef USE_MPS
    core::SaveArchive_O::static_Kind = gctools::GCKind<core::SaveArchive_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SaveArchive_Oval,core::SaveArchive_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SaveArchive_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SaveArchive_O>>::allocateClass();
        core::SaveArchive_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SaveArchive_O::static_className() % (void*)(core::SaveArchive_O::static_allocator) );
    classcore__SaveArchive_Oval->setCreator(core::SaveArchive_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SaveArchive_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SaveArchive_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SingleDispatchEffectiveMethodFunction_Oval]"));
    core::BuiltInClass_sp classcore__SingleDispatchEffectiveMethodFunction_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SingleDispatchEffectiveMethodFunction_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SingleDispatchEffectiveMethodFunction_Oval,_lisp,core::SingleDispatchEffectiveMethodFunction_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SingleDispatchEffectiveMethodFunction_O>::id,core::SingleDispatchEffectiveMethodFunction_O::static_classSymbol());
    core::SingleDispatchEffectiveMethodFunction_O::___staticClass = classcore__SingleDispatchEffectiveMethodFunction_Oval;
#ifdef USE_MPS
    core::SingleDispatchEffectiveMethodFunction_O::static_Kind = gctools::GCKind<core::SingleDispatchEffectiveMethodFunction_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SingleDispatchEffectiveMethodFunction_Oval,core::SingleDispatchEffectiveMethodFunction_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SingleDispatchEffectiveMethodFunction_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SingleDispatchEffectiveMethodFunction_O>>::allocateClass();
        core::SingleDispatchEffectiveMethodFunction_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SingleDispatchEffectiveMethodFunction_O::static_className() % (void*)(core::SingleDispatchEffectiveMethodFunction_O::static_allocator) );
    classcore__SingleDispatchEffectiveMethodFunction_Oval->setCreator(core::SingleDispatchEffectiveMethodFunction_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SingleDispatchEffectiveMethodFunction_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SingleDispatchEffectiveMethodFunction_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SingleDispatchGenericFunction_Oval]"));
    core::BuiltInClass_sp classcore__SingleDispatchGenericFunction_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SingleDispatchGenericFunction_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SingleDispatchGenericFunction_Oval,_lisp,core::SingleDispatchGenericFunction_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SingleDispatchGenericFunction_O>::id,core::SingleDispatchGenericFunction_O::static_classSymbol());
    core::SingleDispatchGenericFunction_O::___staticClass = classcore__SingleDispatchGenericFunction_Oval;
#ifdef USE_MPS
    core::SingleDispatchGenericFunction_O::static_Kind = gctools::GCKind<core::SingleDispatchGenericFunction_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SingleDispatchGenericFunction_Oval,core::SingleDispatchGenericFunction_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SingleDispatchGenericFunction_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SingleDispatchGenericFunction_O>>::allocateClass();
        core::SingleDispatchGenericFunction_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SingleDispatchGenericFunction_O::static_className() % (void*)(core::SingleDispatchGenericFunction_O::static_allocator) );
    classcore__SingleDispatchGenericFunction_Oval->setCreator(core::SingleDispatchGenericFunction_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SingleDispatchGenericFunction_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SingleDispatchGenericFunction_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SpecialForm_Oval]"));
    core::BuiltInClass_sp classcore__SpecialForm_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SpecialForm_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SpecialForm_Oval,_lisp,core::SpecialForm_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SpecialForm_O>::id,core::SpecialForm_O::static_classSymbol());
    core::SpecialForm_O::___staticClass = classcore__SpecialForm_Oval;
#ifdef USE_MPS
    core::SpecialForm_O::static_Kind = gctools::GCKind<core::SpecialForm_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SpecialForm_Oval,core::SpecialForm_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SpecialForm_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SpecialForm_O>>::allocateClass();
        core::SpecialForm_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SpecialForm_O::static_className() % (void*)(core::SpecialForm_O::static_allocator) );
    classcore__SpecialForm_Oval->setCreator(core::SpecialForm_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SpecialForm_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SpecialForm_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__UserData_Oval]"));
    core::BuiltInClass_sp classcore__UserData_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__UserData_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__UserData_Oval,_lisp,core::UserData_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::UserData_O>::id,core::UserData_O::static_classSymbol());
    core::UserData_O::___staticClass = classcore__UserData_Oval;
#ifdef USE_MPS
    core::UserData_O::static_Kind = gctools::GCKind<core::UserData_O>::Kind;
#endif
    core::af_setf_findClass(classcore__UserData_Oval,core::UserData_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::UserData_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::UserData_O>>::allocateClass();
        core::UserData_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::UserData_O::static_className() % (void*)(core::UserData_O::static_allocator) );
    classcore__UserData_Oval->setCreator(core::UserData_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::UserData_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__UserData_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Vector_Oval]"));
    core::BuiltInClass_sp classcore__Vector_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Vector_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Vector_Oval,_lisp,core::Vector_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Vector_O>::id,core::Vector_O::static_classSymbol());
    core::Vector_O::___staticClass = classcore__Vector_Oval;
#ifdef USE_MPS
    core::Vector_O::static_Kind = gctools::GCKind<core::Vector_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Vector_Oval,core::Vector_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Vector_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Vector_O>>::allocateClass();
        core::Vector_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Vector_O::static_className() % (void*)(core::Vector_O::static_allocator) );
    classcore__Vector_Oval->setCreator(core::Vector_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Vector_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Vector_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__WeakKeyHashTable_Oval]"));
    core::BuiltInClass_sp classcore__WeakKeyHashTable_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__WeakKeyHashTable_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__WeakKeyHashTable_Oval,_lisp,core::WeakKeyHashTable_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::WeakKeyHashTable_O>::id,core::WeakKeyHashTable_O::static_classSymbol());
    core::WeakKeyHashTable_O::___staticClass = classcore__WeakKeyHashTable_Oval;
#ifdef USE_MPS
    core::WeakKeyHashTable_O::static_Kind = gctools::GCKind<core::WeakKeyHashTable_O>::Kind;
#endif
    core::af_setf_findClass(classcore__WeakKeyHashTable_Oval,core::WeakKeyHashTable_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::WeakKeyHashTable_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::WeakKeyHashTable_O>>::allocateClass();
        core::WeakKeyHashTable_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::WeakKeyHashTable_O::static_className() % (void*)(core::WeakKeyHashTable_O::static_allocator) );
    classcore__WeakKeyHashTable_Oval->setCreator(core::WeakKeyHashTable_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::WeakKeyHashTable_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__WeakKeyHashTable_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__BitVector_Oval]"));
    core::BuiltInClass_sp classcore__BitVector_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__BitVector_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__BitVector_Oval,_lisp,core::BitVector_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::BitVector_O>::id,core::BitVector_O::static_classSymbol());
    core::BitVector_O::___staticClass = classcore__BitVector_Oval;
#ifdef USE_MPS
    core::BitVector_O::static_Kind = gctools::GCKind<core::BitVector_O>::Kind;
#endif
    core::af_setf_findClass(classcore__BitVector_Oval,core::BitVector_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::BitVector_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::BitVector_O>>::allocateClass();
        core::BitVector_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::BitVector_O::static_className() % (void*)(core::BitVector_O::static_allocator) );
    classcore__BitVector_Oval->setCreator(core::BitVector_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::BitVector_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__BitVector_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__BroadcastStream_Oval]"));
    core::BuiltInClass_sp classcore__BroadcastStream_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__BroadcastStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__BroadcastStream_Oval,_lisp,core::BroadcastStream_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::BroadcastStream_O>::id,core::BroadcastStream_O::static_classSymbol());
    core::BroadcastStream_O::___staticClass = classcore__BroadcastStream_Oval;
#ifdef USE_MPS
    core::BroadcastStream_O::static_Kind = gctools::GCKind<core::BroadcastStream_O>::Kind;
#endif
    core::af_setf_findClass(classcore__BroadcastStream_Oval,core::BroadcastStream_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::BroadcastStream_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::BroadcastStream_O>>::allocateClass();
        core::BroadcastStream_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::BroadcastStream_O::static_className() % (void*)(core::BroadcastStream_O::static_allocator) );
    classcore__BroadcastStream_Oval->setCreator(core::BroadcastStream_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::BroadcastStream_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__BroadcastStream_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__CompileTimeEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__CompileTimeEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__CompileTimeEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__CompileTimeEnvironment_Oval,_lisp,core::CompileTimeEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::CompileTimeEnvironment_O>::id,core::CompileTimeEnvironment_O::static_classSymbol());
    core::CompileTimeEnvironment_O::___staticClass = classcore__CompileTimeEnvironment_Oval;
#ifdef USE_MPS
    core::CompileTimeEnvironment_O::static_Kind = gctools::GCKind<core::CompileTimeEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__CompileTimeEnvironment_Oval,core::CompileTimeEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::CompileTimeEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::CompileTimeEnvironment_O>>::allocateClass();
        core::CompileTimeEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::CompileTimeEnvironment_O::static_className() % (void*)(core::CompileTimeEnvironment_O::static_allocator) );
    classcore__CompileTimeEnvironment_Oval->setCreator(core::CompileTimeEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::CompileTimeEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__CompileTimeEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__ConcatenatedStream_Oval]"));
    core::BuiltInClass_sp classcore__ConcatenatedStream_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__ConcatenatedStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ConcatenatedStream_Oval,_lisp,core::ConcatenatedStream_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::ConcatenatedStream_O>::id,core::ConcatenatedStream_O::static_classSymbol());
    core::ConcatenatedStream_O::___staticClass = classcore__ConcatenatedStream_Oval;
#ifdef USE_MPS
    core::ConcatenatedStream_O::static_Kind = gctools::GCKind<core::ConcatenatedStream_O>::Kind;
#endif
    core::af_setf_findClass(classcore__ConcatenatedStream_Oval,core::ConcatenatedStream_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::ConcatenatedStream_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::ConcatenatedStream_O>>::allocateClass();
        core::ConcatenatedStream_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::ConcatenatedStream_O::static_className() % (void*)(core::ConcatenatedStream_O::static_allocator) );
    classcore__ConcatenatedStream_Oval->setCreator(core::ConcatenatedStream_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::ConcatenatedStream_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__ConcatenatedStream_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__EchoStream_Oval]"));
    core::BuiltInClass_sp classcore__EchoStream_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__EchoStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__EchoStream_Oval,_lisp,core::EchoStream_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::EchoStream_O>::id,core::EchoStream_O::static_classSymbol());
    core::EchoStream_O::___staticClass = classcore__EchoStream_Oval;
#ifdef USE_MPS
    core::EchoStream_O::static_Kind = gctools::GCKind<core::EchoStream_O>::Kind;
#endif
    core::af_setf_findClass(classcore__EchoStream_Oval,core::EchoStream_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::EchoStream_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::EchoStream_O>>::allocateClass();
        core::EchoStream_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::EchoStream_O::static_className() % (void*)(core::EchoStream_O::static_allocator) );
    classcore__EchoStream_Oval->setCreator(core::EchoStream_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::EchoStream_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__EchoStream_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__FileStream_Oval]"));
    core::BuiltInClass_sp classcore__FileStream_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__FileStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FileStream_Oval,_lisp,core::FileStream_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::FileStream_O>::id,core::FileStream_O::static_classSymbol());
    core::FileStream_O::___staticClass = classcore__FileStream_Oval;
#ifdef USE_MPS
    core::FileStream_O::static_Kind = gctools::GCKind<core::FileStream_O>::Kind;
#endif
    core::af_setf_findClass(classcore__FileStream_Oval,core::FileStream_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::FileStream_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::FileStream_O>>::allocateClass();
        core::FileStream_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::FileStream_O::static_className() % (void*)(core::FileStream_O::static_allocator) );
    classcore__FileStream_Oval->setCreator(core::FileStream_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::FileStream_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__FileStream_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Float_Oval]"));
    core::BuiltInClass_sp classcore__Float_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Float_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Float_Oval,_lisp,core::Float_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Float_O>::id,core::Float_O::static_classSymbol());
    core::Float_O::___staticClass = classcore__Float_Oval;
#ifdef USE_MPS
    core::Float_O::static_Kind = gctools::GCKind<core::Float_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Float_Oval,core::Float_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Float_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Float_O>>::allocateClass();
        core::Float_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Float_O::static_className() % (void*)(core::Float_O::static_allocator) );
    classcore__Float_Oval->setCreator(core::Float_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Float_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Float_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__FunctionFrame_Oval]"));
    core::BuiltInClass_sp classcore__FunctionFrame_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__FunctionFrame_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FunctionFrame_Oval,_lisp,core::FunctionFrame_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::FunctionFrame_O>::id,core::FunctionFrame_O::static_classSymbol());
    core::FunctionFrame_O::___staticClass = classcore__FunctionFrame_Oval;
#ifdef USE_MPS
    core::FunctionFrame_O::static_Kind = gctools::GCKind<core::FunctionFrame_O>::Kind;
#endif
    core::af_setf_findClass(classcore__FunctionFrame_Oval,core::FunctionFrame_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::FunctionFrame_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::FunctionFrame_O>>::allocateClass();
        core::FunctionFrame_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::FunctionFrame_O::static_className() % (void*)(core::FunctionFrame_O::static_allocator) );
    classcore__FunctionFrame_Oval->setCreator(core::FunctionFrame_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::FunctionFrame_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__FunctionFrame_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Rational_Oval]"));
    core::BuiltInClass_sp classcore__Rational_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Rational_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Rational_Oval,_lisp,core::Rational_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Rational_O>::id,core::Rational_O::static_classSymbol());
    core::Rational_O::___staticClass = classcore__Rational_Oval;
#ifdef USE_MPS
    core::Rational_O::static_Kind = gctools::GCKind<core::Rational_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Rational_Oval,core::Rational_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Rational_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Rational_O>>::allocateClass();
        core::Rational_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Rational_O::static_className() % (void*)(core::Rational_O::static_allocator) );
    classcore__Rational_Oval->setCreator(core::Rational_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Rational_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Rational_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__RuntimeVisibleEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__RuntimeVisibleEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__RuntimeVisibleEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__RuntimeVisibleEnvironment_Oval,_lisp,core::RuntimeVisibleEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::RuntimeVisibleEnvironment_O>::id,core::RuntimeVisibleEnvironment_O::static_classSymbol());
    core::RuntimeVisibleEnvironment_O::___staticClass = classcore__RuntimeVisibleEnvironment_Oval;
#ifdef USE_MPS
    core::RuntimeVisibleEnvironment_O::static_Kind = gctools::GCKind<core::RuntimeVisibleEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__RuntimeVisibleEnvironment_Oval,core::RuntimeVisibleEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::RuntimeVisibleEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::RuntimeVisibleEnvironment_O>>::allocateClass();
        core::RuntimeVisibleEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::RuntimeVisibleEnvironment_O::static_className() % (void*)(core::RuntimeVisibleEnvironment_O::static_allocator) );
    classcore__RuntimeVisibleEnvironment_Oval->setCreator(core::RuntimeVisibleEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::RuntimeVisibleEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__RuntimeVisibleEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SexpLoadArchive_Oval]"));
    core::BuiltInClass_sp classcore__SexpLoadArchive_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SexpLoadArchive_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SexpLoadArchive_Oval,_lisp,core::SexpLoadArchive_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SexpLoadArchive_O>::id,core::SexpLoadArchive_O::static_classSymbol());
    core::SexpLoadArchive_O::___staticClass = classcore__SexpLoadArchive_Oval;
#ifdef USE_MPS
    core::SexpLoadArchive_O::static_Kind = gctools::GCKind<core::SexpLoadArchive_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SexpLoadArchive_Oval,core::SexpLoadArchive_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SexpLoadArchive_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SexpLoadArchive_O>>::allocateClass();
        core::SexpLoadArchive_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SexpLoadArchive_O::static_className() % (void*)(core::SexpLoadArchive_O::static_allocator) );
    classcore__SexpLoadArchive_Oval->setCreator(core::SexpLoadArchive_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SexpLoadArchive_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SexpLoadArchive_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SexpSaveArchive_Oval]"));
    core::BuiltInClass_sp classcore__SexpSaveArchive_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SexpSaveArchive_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SexpSaveArchive_Oval,_lisp,core::SexpSaveArchive_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SexpSaveArchive_O>::id,core::SexpSaveArchive_O::static_classSymbol());
    core::SexpSaveArchive_O::___staticClass = classcore__SexpSaveArchive_Oval;
#ifdef USE_MPS
    core::SexpSaveArchive_O::static_Kind = gctools::GCKind<core::SexpSaveArchive_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SexpSaveArchive_Oval,core::SexpSaveArchive_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SexpSaveArchive_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SexpSaveArchive_O>>::allocateClass();
        core::SexpSaveArchive_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SexpSaveArchive_O::static_className() % (void*)(core::SexpSaveArchive_O::static_allocator) );
    classcore__SexpSaveArchive_Oval->setCreator(core::SexpSaveArchive_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SexpSaveArchive_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SexpSaveArchive_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Specializer_Oval]"));
    StandardClass_sp classcore__Specializer_Oval = StandardClass_O::createUncollectable();
    classcore__Specializer_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Specializer_Oval,_lisp,core::Specializer_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Specializer_O>::id,core::Specializer_O::static_classSymbol());
    core::Specializer_O::___staticClass = classcore__Specializer_Oval;
#ifdef USE_MPS
    core::Specializer_O::static_Kind = gctools::GCKind<core::Specializer_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Specializer_Oval,core::Specializer_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Specializer_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Specializer_O>>::allocateClass();
        core::Specializer_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Specializer_O::static_className() % (void*)(core::Specializer_O::static_allocator) );
    classcore__Specializer_Oval->setCreator(core::Specializer_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Specializer_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Specializer_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__StringStream_Oval]"));
    core::BuiltInClass_sp classcore__StringStream_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__StringStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StringStream_Oval,_lisp,core::StringStream_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::StringStream_O>::id,core::StringStream_O::static_classSymbol());
    core::StringStream_O::___staticClass = classcore__StringStream_Oval;
#ifdef USE_MPS
    core::StringStream_O::static_Kind = gctools::GCKind<core::StringStream_O>::Kind;
#endif
    core::af_setf_findClass(classcore__StringStream_Oval,core::StringStream_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::StringStream_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::StringStream_O>>::allocateClass();
        core::StringStream_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::StringStream_O::static_className() % (void*)(core::StringStream_O::static_allocator) );
    classcore__StringStream_Oval->setCreator(core::StringStream_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::StringStream_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__StringStream_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__String_Oval]"));
    core::BuiltInClass_sp classcore__String_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__String_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__String_Oval,_lisp,core::String_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::String_O>::id,core::String_O::static_classSymbol());
    core::String_O::___staticClass = classcore__String_Oval;
#ifdef USE_MPS
    core::String_O::static_Kind = gctools::GCKind<core::String_O>::Kind;
#endif
    core::af_setf_findClass(classcore__String_Oval,core::String_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::String_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::String_O>>::allocateClass();
        core::String_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::String_O::static_className() % (void*)(core::String_O::static_allocator) );
    classcore__String_Oval->setCreator(core::String_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::String_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__String_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SynonymStream_Oval]"));
    core::BuiltInClass_sp classcore__SynonymStream_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SynonymStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SynonymStream_Oval,_lisp,core::SynonymStream_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SynonymStream_O>::id,core::SynonymStream_O::static_classSymbol());
    core::SynonymStream_O::___staticClass = classcore__SynonymStream_Oval;
#ifdef USE_MPS
    core::SynonymStream_O::static_Kind = gctools::GCKind<core::SynonymStream_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SynonymStream_Oval,core::SynonymStream_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SynonymStream_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SynonymStream_O>>::allocateClass();
        core::SynonymStream_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SynonymStream_O::static_className() % (void*)(core::SynonymStream_O::static_allocator) );
    classcore__SynonymStream_Oval->setCreator(core::SynonymStream_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SynonymStream_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SynonymStream_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__TagbodyFrame_Oval]"));
    core::BuiltInClass_sp classcore__TagbodyFrame_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__TagbodyFrame_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__TagbodyFrame_Oval,_lisp,core::TagbodyFrame_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::TagbodyFrame_O>::id,core::TagbodyFrame_O::static_classSymbol());
    core::TagbodyFrame_O::___staticClass = classcore__TagbodyFrame_Oval;
#ifdef USE_MPS
    core::TagbodyFrame_O::static_Kind = gctools::GCKind<core::TagbodyFrame_O>::Kind;
#endif
    core::af_setf_findClass(classcore__TagbodyFrame_Oval,core::TagbodyFrame_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::TagbodyFrame_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::TagbodyFrame_O>>::allocateClass();
        core::TagbodyFrame_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::TagbodyFrame_O::static_className() % (void*)(core::TagbodyFrame_O::static_allocator) );
    classcore__TagbodyFrame_Oval->setCreator(core::TagbodyFrame_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::TagbodyFrame_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__TagbodyFrame_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__TwoWayStream_Oval]"));
    core::BuiltInClass_sp classcore__TwoWayStream_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__TwoWayStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__TwoWayStream_Oval,_lisp,core::TwoWayStream_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::TwoWayStream_O>::id,core::TwoWayStream_O::static_classSymbol());
    core::TwoWayStream_O::___staticClass = classcore__TwoWayStream_Oval;
#ifdef USE_MPS
    core::TwoWayStream_O::static_Kind = gctools::GCKind<core::TwoWayStream_O>::Kind;
#endif
    core::af_setf_findClass(classcore__TwoWayStream_Oval,core::TwoWayStream_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::TwoWayStream_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::TwoWayStream_O>>::allocateClass();
        core::TwoWayStream_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::TwoWayStream_O::static_className() % (void*)(core::TwoWayStream_O::static_allocator) );
    classcore__TwoWayStream_Oval->setCreator(core::TwoWayStream_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::TwoWayStream_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__TwoWayStream_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__ValueFrame_Oval]"));
    core::BuiltInClass_sp classcore__ValueFrame_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__ValueFrame_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ValueFrame_Oval,_lisp,core::ValueFrame_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::ValueFrame_O>::id,core::ValueFrame_O::static_classSymbol());
    core::ValueFrame_O::___staticClass = classcore__ValueFrame_Oval;
#ifdef USE_MPS
    core::ValueFrame_O::static_Kind = gctools::GCKind<core::ValueFrame_O>::Kind;
#endif
    core::af_setf_findClass(classcore__ValueFrame_Oval,core::ValueFrame_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::ValueFrame_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::ValueFrame_O>>::allocateClass();
        core::ValueFrame_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::ValueFrame_O::static_className() % (void*)(core::ValueFrame_O::static_allocator) );
    classcore__ValueFrame_Oval->setCreator(core::ValueFrame_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::ValueFrame_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__ValueFrame_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__VectorObjects_Oval]"));
    core::BuiltInClass_sp classcore__VectorObjects_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__VectorObjects_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__VectorObjects_Oval,_lisp,core::VectorObjects_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::VectorObjects_O>::id,core::VectorObjects_O::static_classSymbol());
    core::VectorObjects_O::___staticClass = classcore__VectorObjects_Oval;
#ifdef USE_MPS
    core::VectorObjects_O::static_Kind = gctools::GCKind<core::VectorObjects_O>::Kind;
#endif
    core::af_setf_findClass(classcore__VectorObjects_Oval,core::VectorObjects_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::VectorObjects_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::VectorObjects_O>>::allocateClass();
        core::VectorObjects_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::VectorObjects_O::static_className() % (void*)(core::VectorObjects_O::static_allocator) );
    classcore__VectorObjects_Oval->setCreator(core::VectorObjects_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::VectorObjects_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__VectorObjects_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__BlockEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__BlockEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__BlockEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__BlockEnvironment_Oval,_lisp,core::BlockEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::BlockEnvironment_O>::id,core::BlockEnvironment_O::static_classSymbol());
    core::BlockEnvironment_O::___staticClass = classcore__BlockEnvironment_Oval;
#ifdef USE_MPS
    core::BlockEnvironment_O::static_Kind = gctools::GCKind<core::BlockEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__BlockEnvironment_Oval,core::BlockEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::BlockEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::BlockEnvironment_O>>::allocateClass();
        core::BlockEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::BlockEnvironment_O::static_className() % (void*)(core::BlockEnvironment_O::static_allocator) );
    classcore__BlockEnvironment_Oval->setCreator(core::BlockEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::BlockEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__BlockEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__CatchEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__CatchEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__CatchEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__CatchEnvironment_Oval,_lisp,core::CatchEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::CatchEnvironment_O>::id,core::CatchEnvironment_O::static_classSymbol());
    core::CatchEnvironment_O::___staticClass = classcore__CatchEnvironment_Oval;
#ifdef USE_MPS
    core::CatchEnvironment_O::static_Kind = gctools::GCKind<core::CatchEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__CatchEnvironment_Oval,core::CatchEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::CatchEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::CatchEnvironment_O>>::allocateClass();
        core::CatchEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::CatchEnvironment_O::static_className() % (void*)(core::CatchEnvironment_O::static_allocator) );
    classcore__CatchEnvironment_Oval->setCreator(core::CatchEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::CatchEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__CatchEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Class_Oval]"));
    StandardClass_sp classcore__Class_Oval = StandardClass_O::createUncollectable();
    classcore__Class_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Class_Oval,_lisp,core::Class_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Class_O>::id,core::Class_O::static_classSymbol());
    core::Class_O::___staticClass = classcore__Class_Oval;
#ifdef USE_MPS
    core::Class_O::static_Kind = gctools::GCKind<core::Class_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Class_Oval,core::Class_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Class_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Class_O>>::allocateClass();
        core::Class_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Class_O::static_className() % (void*)(core::Class_O::static_allocator) );
    classcore__Class_Oval->setCreator(core::Class_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Class_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Class_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__DoubleFloat_Oval]"));
    core::BuiltInClass_sp classcore__DoubleFloat_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__DoubleFloat_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__DoubleFloat_Oval,_lisp,core::DoubleFloat_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::DoubleFloat_O>::id,core::DoubleFloat_O::static_classSymbol());
    core::DoubleFloat_O::___staticClass = classcore__DoubleFloat_Oval;
#ifdef USE_MPS
    core::DoubleFloat_O::static_Kind = gctools::GCKind<core::DoubleFloat_O>::Kind;
#endif
    core::af_setf_findClass(classcore__DoubleFloat_Oval,core::DoubleFloat_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::DoubleFloat_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::DoubleFloat_O>>::allocateClass();
        core::DoubleFloat_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::DoubleFloat_O::static_className() % (void*)(core::DoubleFloat_O::static_allocator) );
    classcore__DoubleFloat_Oval->setCreator(core::DoubleFloat_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::DoubleFloat_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__DoubleFloat_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__FunctionContainerEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__FunctionContainerEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__FunctionContainerEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FunctionContainerEnvironment_Oval,_lisp,core::FunctionContainerEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::FunctionContainerEnvironment_O>::id,core::FunctionContainerEnvironment_O::static_classSymbol());
    core::FunctionContainerEnvironment_O::___staticClass = classcore__FunctionContainerEnvironment_Oval;
#ifdef USE_MPS
    core::FunctionContainerEnvironment_O::static_Kind = gctools::GCKind<core::FunctionContainerEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__FunctionContainerEnvironment_Oval,core::FunctionContainerEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::FunctionContainerEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::FunctionContainerEnvironment_O>>::allocateClass();
        core::FunctionContainerEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::FunctionContainerEnvironment_O::static_className() % (void*)(core::FunctionContainerEnvironment_O::static_allocator) );
    classcore__FunctionContainerEnvironment_Oval->setCreator(core::FunctionContainerEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::FunctionContainerEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__FunctionContainerEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__FunctionValueEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__FunctionValueEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__FunctionValueEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FunctionValueEnvironment_Oval,_lisp,core::FunctionValueEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::FunctionValueEnvironment_O>::id,core::FunctionValueEnvironment_O::static_classSymbol());
    core::FunctionValueEnvironment_O::___staticClass = classcore__FunctionValueEnvironment_Oval;
#ifdef USE_MPS
    core::FunctionValueEnvironment_O::static_Kind = gctools::GCKind<core::FunctionValueEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__FunctionValueEnvironment_Oval,core::FunctionValueEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::FunctionValueEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::FunctionValueEnvironment_O>>::allocateClass();
        core::FunctionValueEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::FunctionValueEnvironment_O::static_className() % (void*)(core::FunctionValueEnvironment_O::static_allocator) );
    classcore__FunctionValueEnvironment_Oval->setCreator(core::FunctionValueEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::FunctionValueEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__FunctionValueEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__IOFileStream_Oval]"));
    core::BuiltInClass_sp classcore__IOFileStream_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__IOFileStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__IOFileStream_Oval,_lisp,core::IOFileStream_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::IOFileStream_O>::id,core::IOFileStream_O::static_classSymbol());
    core::IOFileStream_O::___staticClass = classcore__IOFileStream_Oval;
#ifdef USE_MPS
    core::IOFileStream_O::static_Kind = gctools::GCKind<core::IOFileStream_O>::Kind;
#endif
    core::af_setf_findClass(classcore__IOFileStream_Oval,core::IOFileStream_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::IOFileStream_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::IOFileStream_O>>::allocateClass();
        core::IOFileStream_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::IOFileStream_O::static_className() % (void*)(core::IOFileStream_O::static_allocator) );
    classcore__IOFileStream_Oval->setCreator(core::IOFileStream_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::IOFileStream_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__IOFileStream_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__IOStreamStream_Oval]"));
    core::BuiltInClass_sp classcore__IOStreamStream_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__IOStreamStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__IOStreamStream_Oval,_lisp,core::IOStreamStream_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::IOStreamStream_O>::id,core::IOStreamStream_O::static_classSymbol());
    core::IOStreamStream_O::___staticClass = classcore__IOStreamStream_Oval;
#ifdef USE_MPS
    core::IOStreamStream_O::static_Kind = gctools::GCKind<core::IOStreamStream_O>::Kind;
#endif
    core::af_setf_findClass(classcore__IOStreamStream_Oval,core::IOStreamStream_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::IOStreamStream_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::IOStreamStream_O>>::allocateClass();
        core::IOStreamStream_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::IOStreamStream_O::static_className() % (void*)(core::IOStreamStream_O::static_allocator) );
    classcore__IOStreamStream_Oval->setCreator(core::IOStreamStream_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::IOStreamStream_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__IOStreamStream_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Integer_Oval]"));
    core::BuiltInClass_sp classcore__Integer_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Integer_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Integer_Oval,_lisp,core::Integer_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Integer_O>::id,core::Integer_O::static_classSymbol());
    core::Integer_O::___staticClass = classcore__Integer_Oval;
#ifdef USE_MPS
    core::Integer_O::static_Kind = gctools::GCKind<core::Integer_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Integer_Oval,core::Integer_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Integer_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Integer_O>>::allocateClass();
        core::Integer_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Integer_O::static_className() % (void*)(core::Integer_O::static_allocator) );
    classcore__Integer_Oval->setCreator(core::Integer_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Integer_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Integer_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__MacroletEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__MacroletEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__MacroletEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__MacroletEnvironment_Oval,_lisp,core::MacroletEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::MacroletEnvironment_O>::id,core::MacroletEnvironment_O::static_classSymbol());
    core::MacroletEnvironment_O::___staticClass = classcore__MacroletEnvironment_Oval;
#ifdef USE_MPS
    core::MacroletEnvironment_O::static_Kind = gctools::GCKind<core::MacroletEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__MacroletEnvironment_Oval,core::MacroletEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::MacroletEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::MacroletEnvironment_O>>::allocateClass();
        core::MacroletEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::MacroletEnvironment_O::static_className() % (void*)(core::MacroletEnvironment_O::static_allocator) );
    classcore__MacroletEnvironment_Oval->setCreator(core::MacroletEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::MacroletEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__MacroletEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Ratio_Oval]"));
    core::BuiltInClass_sp classcore__Ratio_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Ratio_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Ratio_Oval,_lisp,core::Ratio_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Ratio_O>::id,core::Ratio_O::static_classSymbol());
    core::Ratio_O::___staticClass = classcore__Ratio_Oval;
#ifdef USE_MPS
    core::Ratio_O::static_Kind = gctools::GCKind<core::Ratio_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Ratio_Oval,core::Ratio_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Ratio_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Ratio_O>>::allocateClass();
        core::Ratio_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Ratio_O::static_className() % (void*)(core::Ratio_O::static_allocator) );
    classcore__Ratio_Oval->setCreator(core::Ratio_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Ratio_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Ratio_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__ShortFloat_Oval]"));
    core::BuiltInClass_sp classcore__ShortFloat_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__ShortFloat_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ShortFloat_Oval,_lisp,core::ShortFloat_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::ShortFloat_O>::id,core::ShortFloat_O::static_classSymbol());
    core::ShortFloat_O::___staticClass = classcore__ShortFloat_Oval;
#ifdef USE_MPS
    core::ShortFloat_O::static_Kind = gctools::GCKind<core::ShortFloat_O>::Kind;
#endif
    core::af_setf_findClass(classcore__ShortFloat_Oval,core::ShortFloat_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::ShortFloat_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::ShortFloat_O>>::allocateClass();
        core::ShortFloat_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::ShortFloat_O::static_className() % (void*)(core::ShortFloat_O::static_allocator) );
    classcore__ShortFloat_Oval->setCreator(core::ShortFloat_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::ShortFloat_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__ShortFloat_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SimpleBitVector_Oval]"));
    core::BuiltInClass_sp classcore__SimpleBitVector_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SimpleBitVector_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SimpleBitVector_Oval,_lisp,core::SimpleBitVector_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SimpleBitVector_O>::id,core::SimpleBitVector_O::static_classSymbol());
    core::SimpleBitVector_O::___staticClass = classcore__SimpleBitVector_Oval;
#ifdef USE_MPS
    core::SimpleBitVector_O::static_Kind = gctools::GCKind<core::SimpleBitVector_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SimpleBitVector_Oval,core::SimpleBitVector_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SimpleBitVector_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SimpleBitVector_O>>::allocateClass();
        core::SimpleBitVector_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SimpleBitVector_O::static_className() % (void*)(core::SimpleBitVector_O::static_allocator) );
    classcore__SimpleBitVector_Oval->setCreator(core::SimpleBitVector_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SimpleBitVector_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SimpleBitVector_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SingleFloat_dummy_Oval]"));
    core::BuiltInClass_sp classcore__SingleFloat_dummy_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SingleFloat_dummy_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SingleFloat_dummy_Oval,_lisp,core::SingleFloat_dummy_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SingleFloat_dummy_O>::id,core::SingleFloat_dummy_O::static_classSymbol());
    core::SingleFloat_dummy_O::___staticClass = classcore__SingleFloat_dummy_Oval;
#ifdef USE_MPS
    core::SingleFloat_dummy_O::static_Kind = gctools::GCKind<core::SingleFloat_dummy_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SingleFloat_dummy_Oval,core::SingleFloat_dummy_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SingleFloat_dummy_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SingleFloat_dummy_O>>::allocateClass();
        core::SingleFloat_dummy_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SingleFloat_dummy_O::static_className() % (void*)(core::SingleFloat_dummy_O::static_allocator) );
    classcore__SingleFloat_dummy_Oval->setCreator(core::SingleFloat_dummy_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SingleFloat_dummy_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SingleFloat_dummy_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__StackValueEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__StackValueEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__StackValueEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StackValueEnvironment_Oval,_lisp,core::StackValueEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::StackValueEnvironment_O>::id,core::StackValueEnvironment_O::static_classSymbol());
    core::StackValueEnvironment_O::___staticClass = classcore__StackValueEnvironment_Oval;
#ifdef USE_MPS
    core::StackValueEnvironment_O::static_Kind = gctools::GCKind<core::StackValueEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__StackValueEnvironment_Oval,core::StackValueEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::StackValueEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::StackValueEnvironment_O>>::allocateClass();
        core::StackValueEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::StackValueEnvironment_O::static_className() % (void*)(core::StackValueEnvironment_O::static_allocator) );
    classcore__StackValueEnvironment_Oval->setCreator(core::StackValueEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::StackValueEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__StackValueEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Str_Oval]"));
    core::BuiltInClass_sp classcore__Str_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Str_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Str_Oval,_lisp,core::Str_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Str_O>::id,core::Str_O::static_classSymbol());
    core::Str_O::___staticClass = classcore__Str_Oval;
#ifdef USE_MPS
    core::Str_O::static_Kind = gctools::GCKind<core::Str_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Str_Oval,core::Str_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Str_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Str_O>>::allocateClass();
        core::Str_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Str_O::static_className() % (void*)(core::Str_O::static_allocator) );
    classcore__Str_Oval->setCreator(core::Str_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Str_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Str_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__StringInputStream_Oval]"));
    core::BuiltInClass_sp classcore__StringInputStream_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__StringInputStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StringInputStream_Oval,_lisp,core::StringInputStream_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::StringInputStream_O>::id,core::StringInputStream_O::static_classSymbol());
    core::StringInputStream_O::___staticClass = classcore__StringInputStream_Oval;
#ifdef USE_MPS
    core::StringInputStream_O::static_Kind = gctools::GCKind<core::StringInputStream_O>::Kind;
#endif
    core::af_setf_findClass(classcore__StringInputStream_Oval,core::StringInputStream_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::StringInputStream_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::StringInputStream_O>>::allocateClass();
        core::StringInputStream_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::StringInputStream_O::static_className() % (void*)(core::StringInputStream_O::static_allocator) );
    classcore__StringInputStream_Oval->setCreator(core::StringInputStream_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::StringInputStream_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__StringInputStream_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__StringOutputStream_Oval]"));
    core::BuiltInClass_sp classcore__StringOutputStream_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__StringOutputStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StringOutputStream_Oval,_lisp,core::StringOutputStream_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::StringOutputStream_O>::id,core::StringOutputStream_O::static_classSymbol());
    core::StringOutputStream_O::___staticClass = classcore__StringOutputStream_Oval;
#ifdef USE_MPS
    core::StringOutputStream_O::static_Kind = gctools::GCKind<core::StringOutputStream_O>::Kind;
#endif
    core::af_setf_findClass(classcore__StringOutputStream_Oval,core::StringOutputStream_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::StringOutputStream_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::StringOutputStream_O>>::allocateClass();
        core::StringOutputStream_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::StringOutputStream_O::static_className() % (void*)(core::StringOutputStream_O::static_allocator) );
    classcore__StringOutputStream_Oval->setCreator(core::StringOutputStream_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::StringOutputStream_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__StringOutputStream_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__SymbolMacroletEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__SymbolMacroletEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__SymbolMacroletEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SymbolMacroletEnvironment_Oval,_lisp,core::SymbolMacroletEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::SymbolMacroletEnvironment_O>::id,core::SymbolMacroletEnvironment_O::static_classSymbol());
    core::SymbolMacroletEnvironment_O::___staticClass = classcore__SymbolMacroletEnvironment_Oval;
#ifdef USE_MPS
    core::SymbolMacroletEnvironment_O::static_Kind = gctools::GCKind<core::SymbolMacroletEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__SymbolMacroletEnvironment_Oval,core::SymbolMacroletEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::SymbolMacroletEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::SymbolMacroletEnvironment_O>>::allocateClass();
        core::SymbolMacroletEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::SymbolMacroletEnvironment_O::static_className() % (void*)(core::SymbolMacroletEnvironment_O::static_allocator) );
    classcore__SymbolMacroletEnvironment_Oval->setCreator(core::SymbolMacroletEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::SymbolMacroletEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__SymbolMacroletEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__TagbodyEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__TagbodyEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__TagbodyEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__TagbodyEnvironment_Oval,_lisp,core::TagbodyEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::TagbodyEnvironment_O>::id,core::TagbodyEnvironment_O::static_classSymbol());
    core::TagbodyEnvironment_O::___staticClass = classcore__TagbodyEnvironment_Oval;
#ifdef USE_MPS
    core::TagbodyEnvironment_O::static_Kind = gctools::GCKind<core::TagbodyEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__TagbodyEnvironment_Oval,core::TagbodyEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::TagbodyEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::TagbodyEnvironment_O>>::allocateClass();
        core::TagbodyEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::TagbodyEnvironment_O::static_className() % (void*)(core::TagbodyEnvironment_O::static_allocator) );
    classcore__TagbodyEnvironment_Oval->setCreator(core::TagbodyEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::TagbodyEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__TagbodyEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__UnwindProtectEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__UnwindProtectEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__UnwindProtectEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__UnwindProtectEnvironment_Oval,_lisp,core::UnwindProtectEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::UnwindProtectEnvironment_O>::id,core::UnwindProtectEnvironment_O::static_classSymbol());
    core::UnwindProtectEnvironment_O::___staticClass = classcore__UnwindProtectEnvironment_Oval;
#ifdef USE_MPS
    core::UnwindProtectEnvironment_O::static_Kind = gctools::GCKind<core::UnwindProtectEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__UnwindProtectEnvironment_Oval,core::UnwindProtectEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::UnwindProtectEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::UnwindProtectEnvironment_O>>::allocateClass();
        core::UnwindProtectEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::UnwindProtectEnvironment_O::static_className() % (void*)(core::UnwindProtectEnvironment_O::static_allocator) );
    classcore__UnwindProtectEnvironment_Oval->setCreator(core::UnwindProtectEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::UnwindProtectEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__UnwindProtectEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__ValueEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__ValueEnvironment_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__ValueEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ValueEnvironment_Oval,_lisp,core::ValueEnvironment_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::ValueEnvironment_O>::id,core::ValueEnvironment_O::static_classSymbol());
    core::ValueEnvironment_O::___staticClass = classcore__ValueEnvironment_Oval;
#ifdef USE_MPS
    core::ValueEnvironment_O::static_Kind = gctools::GCKind<core::ValueEnvironment_O>::Kind;
#endif
    core::af_setf_findClass(classcore__ValueEnvironment_Oval,core::ValueEnvironment_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::ValueEnvironment_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::ValueEnvironment_O>>::allocateClass();
        core::ValueEnvironment_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::ValueEnvironment_O::static_className() % (void*)(core::ValueEnvironment_O::static_allocator) );
    classcore__ValueEnvironment_Oval->setCreator(core::ValueEnvironment_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::ValueEnvironment_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__ValueEnvironment_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__VectorObjectsWithFillPtr_Oval]"));
    core::BuiltInClass_sp classcore__VectorObjectsWithFillPtr_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__VectorObjectsWithFillPtr_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__VectorObjectsWithFillPtr_Oval,_lisp,core::VectorObjectsWithFillPtr_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::VectorObjectsWithFillPtr_O>::id,core::VectorObjectsWithFillPtr_O::static_classSymbol());
    core::VectorObjectsWithFillPtr_O::___staticClass = classcore__VectorObjectsWithFillPtr_Oval;
#ifdef USE_MPS
    core::VectorObjectsWithFillPtr_O::static_Kind = gctools::GCKind<core::VectorObjectsWithFillPtr_O>::Kind;
#endif
    core::af_setf_findClass(classcore__VectorObjectsWithFillPtr_Oval,core::VectorObjectsWithFillPtr_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::VectorObjectsWithFillPtr_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::VectorObjectsWithFillPtr_O>>::allocateClass();
        core::VectorObjectsWithFillPtr_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::VectorObjectsWithFillPtr_O::static_className() % (void*)(core::VectorObjectsWithFillPtr_O::static_allocator) );
    classcore__VectorObjectsWithFillPtr_Oval->setCreator(core::VectorObjectsWithFillPtr_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::VectorObjectsWithFillPtr_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__VectorObjectsWithFillPtr_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Bignum_Oval]"));
    core::BuiltInClass_sp classcore__Bignum_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Bignum_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Bignum_Oval,_lisp,core::Bignum_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Bignum_O>::id,core::Bignum_O::static_classSymbol());
    core::Bignum_O::___staticClass = classcore__Bignum_Oval;
#ifdef USE_MPS
    core::Bignum_O::static_Kind = gctools::GCKind<core::Bignum_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Bignum_Oval,core::Bignum_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Bignum_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Bignum_O>>::allocateClass();
        core::Bignum_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Bignum_O::static_className() % (void*)(core::Bignum_O::static_allocator) );
    classcore__Bignum_Oval->setCreator(core::Bignum_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Bignum_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Bignum_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__BuiltInClass_Oval]"));
    StandardClass_sp classcore__BuiltInClass_Oval = StandardClass_O::createUncollectable();
    classcore__BuiltInClass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__BuiltInClass_Oval,_lisp,core::BuiltInClass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::BuiltInClass_O>::id,core::BuiltInClass_O::static_classSymbol());
    core::BuiltInClass_O::___staticClass = classcore__BuiltInClass_Oval;
#ifdef USE_MPS
    core::BuiltInClass_O::static_Kind = gctools::GCKind<core::BuiltInClass_O>::Kind;
#endif
    core::af_setf_findClass(classcore__BuiltInClass_Oval,core::BuiltInClass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::BuiltInClass_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::BuiltInClass_O>>::allocateClass();
        core::BuiltInClass_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::BuiltInClass_O::static_className() % (void*)(core::BuiltInClass_O::static_allocator) );
    classcore__BuiltInClass_Oval->setCreator(core::BuiltInClass_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::BuiltInClass_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__BuiltInClass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__Fixnum_dummy_Oval]"));
    core::BuiltInClass_sp classcore__Fixnum_dummy_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__Fixnum_dummy_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Fixnum_dummy_Oval,_lisp,core::Fixnum_dummy_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::Fixnum_dummy_O>::id,core::Fixnum_dummy_O::static_classSymbol());
    core::Fixnum_dummy_O::___staticClass = classcore__Fixnum_dummy_Oval;
#ifdef USE_MPS
    core::Fixnum_dummy_O::static_Kind = gctools::GCKind<core::Fixnum_dummy_O>::Kind;
#endif
    core::af_setf_findClass(classcore__Fixnum_dummy_Oval,core::Fixnum_dummy_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::Fixnum_dummy_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::Fixnum_dummy_O>>::allocateClass();
        core::Fixnum_dummy_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::Fixnum_dummy_O::static_className() % (void*)(core::Fixnum_dummy_O::static_allocator) );
    classcore__Fixnum_dummy_Oval->setCreator(core::Fixnum_dummy_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::Fixnum_dummy_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__Fixnum_dummy_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__ForwardReferencedClass_Oval]"));
    StandardClass_sp classcore__ForwardReferencedClass_Oval = StandardClass_O::createUncollectable();
    classcore__ForwardReferencedClass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ForwardReferencedClass_Oval,_lisp,core::ForwardReferencedClass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::ForwardReferencedClass_O>::id,core::ForwardReferencedClass_O::static_classSymbol());
    core::ForwardReferencedClass_O::___staticClass = classcore__ForwardReferencedClass_Oval;
#ifdef USE_MPS
    core::ForwardReferencedClass_O::static_Kind = gctools::GCKind<core::ForwardReferencedClass_O>::Kind;
#endif
    core::af_setf_findClass(classcore__ForwardReferencedClass_Oval,core::ForwardReferencedClass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::ForwardReferencedClass_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::ForwardReferencedClass_O>>::allocateClass();
        core::ForwardReferencedClass_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::ForwardReferencedClass_O::static_className() % (void*)(core::ForwardReferencedClass_O::static_allocator) );
    classcore__ForwardReferencedClass_Oval->setCreator(core::ForwardReferencedClass_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::ForwardReferencedClass_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__ForwardReferencedClass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__StdClass_Oval]"));
    StandardClass_sp classcore__StdClass_Oval = StandardClass_O::createUncollectable();
    classcore__StdClass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StdClass_Oval,_lisp,core::StdClass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::StdClass_O>::id,core::StdClass_O::static_classSymbol());
    core::StdClass_O::___staticClass = classcore__StdClass_Oval;
#ifdef USE_MPS
    core::StdClass_O::static_Kind = gctools::GCKind<core::StdClass_O>::Kind;
#endif
    core::af_setf_findClass(classcore__StdClass_Oval,core::StdClass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::StdClass_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::StdClass_O>>::allocateClass();
        core::StdClass_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::StdClass_O::static_className() % (void*)(core::StdClass_O::static_allocator) );
    classcore__StdClass_Oval->setCreator(core::StdClass_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::StdClass_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__StdClass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__StrWithFillPtr_Oval]"));
    core::BuiltInClass_sp classcore__StrWithFillPtr_Oval = core::BuiltInClass_O::createUncollectable();
    classcore__StrWithFillPtr_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StrWithFillPtr_Oval,_lisp,core::StrWithFillPtr_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::StrWithFillPtr_O>::id,core::StrWithFillPtr_O::static_classSymbol());
    core::StrWithFillPtr_O::___staticClass = classcore__StrWithFillPtr_Oval;
#ifdef USE_MPS
    core::StrWithFillPtr_O::static_Kind = gctools::GCKind<core::StrWithFillPtr_O>::Kind;
#endif
    core::af_setf_findClass(classcore__StrWithFillPtr_Oval,core::StrWithFillPtr_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::StrWithFillPtr_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::StrWithFillPtr_O>>::allocateClass();
        core::StrWithFillPtr_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::StrWithFillPtr_O::static_className() % (void*)(core::StrWithFillPtr_O::static_allocator) );
    classcore__StrWithFillPtr_Oval->setCreator(core::StrWithFillPtr_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::StrWithFillPtr_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__StrWithFillPtr_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__StructureClass_Oval]"));
    StandardClass_sp classcore__StructureClass_Oval = StandardClass_O::createUncollectable();
    classcore__StructureClass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StructureClass_Oval,_lisp,core::StructureClass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::StructureClass_O>::id,core::StructureClass_O::static_classSymbol());
    core::StructureClass_O::___staticClass = classcore__StructureClass_Oval;
#ifdef USE_MPS
    core::StructureClass_O::static_Kind = gctools::GCKind<core::StructureClass_O>::Kind;
#endif
    core::af_setf_findClass(classcore__StructureClass_Oval,core::StructureClass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::StructureClass_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::StructureClass_O>>::allocateClass();
        core::StructureClass_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::StructureClass_O::static_className() % (void*)(core::StructureClass_O::static_allocator) );
    classcore__StructureClass_Oval->setCreator(core::StructureClass_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::StructureClass_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__StructureClass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__FuncallableStandardClass_Oval]"));
    StandardClass_sp classcore__FuncallableStandardClass_Oval = StandardClass_O::createUncollectable();
    classcore__FuncallableStandardClass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FuncallableStandardClass_Oval,_lisp,core::FuncallableStandardClass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::FuncallableStandardClass_O>::id,core::FuncallableStandardClass_O::static_classSymbol());
    core::FuncallableStandardClass_O::___staticClass = classcore__FuncallableStandardClass_Oval;
#ifdef USE_MPS
    core::FuncallableStandardClass_O::static_Kind = gctools::GCKind<core::FuncallableStandardClass_O>::Kind;
#endif
    core::af_setf_findClass(classcore__FuncallableStandardClass_Oval,core::FuncallableStandardClass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::FuncallableStandardClass_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::FuncallableStandardClass_O>>::allocateClass();
        core::FuncallableStandardClass_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::FuncallableStandardClass_O::static_className() % (void*)(core::FuncallableStandardClass_O::static_allocator) );
    classcore__FuncallableStandardClass_Oval->setCreator(core::FuncallableStandardClass_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::FuncallableStandardClass_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__FuncallableStandardClass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classcore__StandardClass_Oval]"));
    StandardClass_sp classcore__StandardClass_Oval = StandardClass_O::createUncollectable();
    classcore__StandardClass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StandardClass_Oval,_lisp,core::StandardClass_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<core::StandardClass_O>::id,core::StandardClass_O::static_classSymbol());
    core::StandardClass_O::___staticClass = classcore__StandardClass_Oval;
#ifdef USE_MPS
    core::StandardClass_O::static_Kind = gctools::GCKind<core::StandardClass_O>::Kind;
#endif
    core::af_setf_findClass(classcore__StandardClass_Oval,core::StandardClass_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<core::StandardClass_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<core::StandardClass_O>>::allocateClass();
        core::StandardClass_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% core::StandardClass_O::static_className() % (void*)(core::StandardClass_O::static_allocator) );
    classcore__StandardClass_Oval->setCreator(core::StandardClass_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % core::StandardClass_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcore__StandardClass_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */
#endif // CREATE_CLASS
#undef CREATE_CLASS
#ifdef DUMP_INFO_CLASS // {
// Depends on nothing

    LOG(BF("---    dump_info   --- className: core::T_O @ %X") % classcore__T_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::T_O::static_className() % core::T_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Archive_O @ %X") % classcore__Archive_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Archive_O::static_className() % core::Archive_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Array_O @ %X") % classcore__Array_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Array_O::static_className() % core::Array_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Binder_O @ %X") % classcore__Binder_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Binder_O::static_className() % core::Binder_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::CandoException_O @ %X") % classcore__CandoException_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::CandoException_O::static_className() % core::CandoException_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Character_dummy_O @ %X") % classcore__Character_dummy_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Character_dummy_O::static_className() % core::Character_dummy_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Cons_O @ %X") % classcore__Cons_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Cons_O::static_className() % core::Cons_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::DirectoryEntry_O @ %X") % classcore__DirectoryEntry_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::DirectoryEntry_O::static_className() % core::DirectoryEntry_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Environment_O @ %X") % classcore__Environment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Environment_O::static_className() % core::Environment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ExternalObject_O @ %X") % classcore__ExternalObject_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ExternalObject_O::static_className() % core::ExternalObject_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FileStatus_O @ %X") % classcore__FileStatus_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FileStatus_O::static_className() % core::FileStatus_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Function_O @ %X") % classcore__Function_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Function_O::static_className() % core::Function_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::HashTable_O @ %X") % classcore__HashTable_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::HashTable_O::static_className() % core::HashTable_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::IntArray_O @ %X") % classcore__IntArray_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::IntArray_O::static_className() % core::IntArray_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Iterator_O @ %X") % classcore__Iterator_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Iterator_O::static_className() % core::Iterator_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::LambdaListHandler_O @ %X") % classcore__LambdaListHandler_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::LambdaListHandler_O::static_className() % core::LambdaListHandler_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::LightUserData_O @ %X") % classcore__LightUserData_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::LightUserData_O::static_className() % core::LightUserData_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::LoadTimeValues_O @ %X") % classcore__LoadTimeValues_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::LoadTimeValues_O::static_className() % core::LoadTimeValues_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::MicroHeap_O @ %X") % classcore__MicroHeap_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::MicroHeap_O::static_className() % core::MicroHeap_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::MultiStringBuffer_O @ %X") % classcore__MultiStringBuffer_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::MultiStringBuffer_O::static_className() % core::MultiStringBuffer_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Number_O @ %X") % classcore__Number_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Number_O::static_className() % core::Number_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ObjRef_O @ %X") % classcore__ObjRef_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ObjRef_O::static_className() % core::ObjRef_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ObjectSet_O @ %X") % classcore__ObjectSet_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ObjectSet_O::static_className() % core::ObjectSet_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Package_O @ %X") % classcore__Package_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Package_O::static_className() % core::Package_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Path_O @ %X") % classcore__Path_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Path_O::static_className() % core::Path_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Pathname_O @ %X") % classcore__Pathname_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Pathname_O::static_className() % core::Pathname_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Pointer_O @ %X") % classcore__Pointer_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Pointer_O::static_className() % core::Pointer_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::PosixTimeDuration_O @ %X") % classcore__PosixTimeDuration_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::PosixTimeDuration_O::static_className() % core::PosixTimeDuration_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::PosixTime_O @ %X") % classcore__PosixTime_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::PosixTime_O::static_className() % core::PosixTime_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ReadTable_O @ %X") % classcore__ReadTable_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ReadTable_O::static_className() % core::ReadTable_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Reader_O @ %X") % classcore__Reader_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Reader_O::static_className() % core::Reader_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::RegexMatch_O @ %X") % classcore__RegexMatch_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::RegexMatch_O::static_className() % core::RegexMatch_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Regex_O @ %X") % classcore__Regex_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Regex_O::static_className() % core::Regex_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SNode_O @ %X") % classcore__SNode_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SNode_O::static_className() % core::SNode_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SingleDispatchMethod_O @ %X") % classcore__SingleDispatchMethod_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SingleDispatchMethod_O::static_className() % core::SingleDispatchMethod_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SmallMap_O @ %X") % classcore__SmallMap_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SmallMap_O::static_className() % core::SmallMap_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SourceFileInfo_O @ %X") % classcore__SourceFileInfo_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SourceFileInfo_O::static_className() % core::SourceFileInfo_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SourceManager_O @ %X") % classcore__SourceManager_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SourceManager_O::static_className() % core::SourceManager_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SourcePosInfo_O @ %X") % classcore__SourcePosInfo_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SourcePosInfo_O::static_className() % core::SourcePosInfo_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StandardObject_O @ %X") % classcore__StandardObject_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StandardObject_O::static_className() % core::StandardObject_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Stream_O @ %X") % classcore__Stream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Stream_O::static_className() % core::Stream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StringList_O @ %X") % classcore__StringList_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StringList_O::static_className() % core::StringList_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StringSet_O @ %X") % classcore__StringSet_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StringSet_O::static_className() % core::StringSet_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StructureObject_O @ %X") % classcore__StructureObject_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StructureObject_O::static_className() % core::StructureObject_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SymbolList_O @ %X") % classcore__SymbolList_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SymbolList_O::static_className() % core::SymbolList_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SymbolSet_O @ %X") % classcore__SymbolSet_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SymbolSet_O::static_className() % core::SymbolSet_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SymbolToEnumConverter_O @ %X") % classcore__SymbolToEnumConverter_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SymbolToEnumConverter_O::static_className() % core::SymbolToEnumConverter_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Symbol_O @ %X") % classcore__Symbol_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Symbol_O::static_className() % core::Symbol_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::WeakHashTable_O @ %X") % classcore__WeakHashTable_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::WeakHashTable_O::static_className() % core::WeakHashTable_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::WeakKeyMapping_O @ %X") % classcore__WeakKeyMapping_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::WeakKeyMapping_O::static_className() % core::WeakKeyMapping_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::WeakPointer_O @ %X") % classcore__WeakPointer_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::WeakPointer_O::static_className() % core::WeakPointer_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::WrappedPointer_O @ %X") % classcore__WrappedPointer_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::WrappedPointer_O::static_className() % core::WrappedPointer_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ActivationFrame_O @ %X") % classcore__ActivationFrame_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ActivationFrame_O::static_className() % core::ActivationFrame_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::AnsiStream_O @ %X") % classcore__AnsiStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::AnsiStream_O::static_className() % core::AnsiStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ArrayDisplaced_O @ %X") % classcore__ArrayDisplaced_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ArrayDisplaced_O::static_className() % core::ArrayDisplaced_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ArrayObjects_O @ %X") % classcore__ArrayObjects_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ArrayObjects_O::static_className() % core::ArrayObjects_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::BranchSNode_O @ %X") % classcore__BranchSNode_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::BranchSNode_O::static_className() % core::BranchSNode_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::CompiledFunction_O @ %X") % classcore__CompiledFunction_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::CompiledFunction_O::static_className() % core::CompiledFunction_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Complex_O @ %X") % classcore__Complex_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Complex_O::static_className() % core::Complex_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::DirectoryIterator_O @ %X") % classcore__DirectoryIterator_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::DirectoryIterator_O::static_className() % core::DirectoryIterator_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ForeignData_O @ %X") % classcore__ForeignData_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ForeignData_O::static_className() % core::ForeignData_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::GlueEnvironment_O @ %X") % classcore__GlueEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::GlueEnvironment_O::static_className() % core::GlueEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::HashTableEq_O @ %X") % classcore__HashTableEq_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::HashTableEq_O::static_className() % core::HashTableEq_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::HashTableEql_O @ %X") % classcore__HashTableEql_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::HashTableEql_O::static_className() % core::HashTableEql_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::HashTableEqual_O @ %X") % classcore__HashTableEqual_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::HashTableEqual_O::static_className() % core::HashTableEqual_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::HashTableEqualp_O @ %X") % classcore__HashTableEqualp_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::HashTableEqualp_O::static_className() % core::HashTableEqualp_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Instance_O @ %X") % classcore__Instance_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Instance_O::static_className() % core::Instance_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::LeafSNode_O @ %X") % classcore__LeafSNode_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::LeafSNode_O::static_className() % core::LeafSNode_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::LexicalEnvironment_O @ %X") % classcore__LexicalEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::LexicalEnvironment_O::static_className() % core::LexicalEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::LoadArchive_O @ %X") % classcore__LoadArchive_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::LoadArchive_O::static_className() % core::LoadArchive_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::LogicalPathname_O @ %X") % classcore__LogicalPathname_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::LogicalPathname_O::static_className() % core::LogicalPathname_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Metaobject_O @ %X") % classcore__Metaobject_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Metaobject_O::static_className() % core::Metaobject_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Null_O @ %X") % classcore__Null_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Null_O::static_className() % core::Null_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Real_O @ %X") % classcore__Real_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Real_O::static_className() % core::Real_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::RecursiveDirectoryIterator_O @ %X") % classcore__RecursiveDirectoryIterator_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::RecursiveDirectoryIterator_O::static_className() % core::RecursiveDirectoryIterator_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SaveArchive_O @ %X") % classcore__SaveArchive_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SaveArchive_O::static_className() % core::SaveArchive_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SingleDispatchEffectiveMethodFunction_O @ %X") % classcore__SingleDispatchEffectiveMethodFunction_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SingleDispatchEffectiveMethodFunction_O::static_className() % core::SingleDispatchEffectiveMethodFunction_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SingleDispatchGenericFunction_O @ %X") % classcore__SingleDispatchGenericFunction_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SingleDispatchGenericFunction_O::static_className() % core::SingleDispatchGenericFunction_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SpecialForm_O @ %X") % classcore__SpecialForm_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SpecialForm_O::static_className() % core::SpecialForm_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::UserData_O @ %X") % classcore__UserData_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::UserData_O::static_className() % core::UserData_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Vector_O @ %X") % classcore__Vector_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Vector_O::static_className() % core::Vector_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::WeakKeyHashTable_O @ %X") % classcore__WeakKeyHashTable_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::WeakKeyHashTable_O::static_className() % core::WeakKeyHashTable_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::BitVector_O @ %X") % classcore__BitVector_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::BitVector_O::static_className() % core::BitVector_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::BroadcastStream_O @ %X") % classcore__BroadcastStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::BroadcastStream_O::static_className() % core::BroadcastStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::CompileTimeEnvironment_O @ %X") % classcore__CompileTimeEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::CompileTimeEnvironment_O::static_className() % core::CompileTimeEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ConcatenatedStream_O @ %X") % classcore__ConcatenatedStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ConcatenatedStream_O::static_className() % core::ConcatenatedStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::EchoStream_O @ %X") % classcore__EchoStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::EchoStream_O::static_className() % core::EchoStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FileStream_O @ %X") % classcore__FileStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FileStream_O::static_className() % core::FileStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Float_O @ %X") % classcore__Float_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Float_O::static_className() % core::Float_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FunctionFrame_O @ %X") % classcore__FunctionFrame_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FunctionFrame_O::static_className() % core::FunctionFrame_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Rational_O @ %X") % classcore__Rational_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Rational_O::static_className() % core::Rational_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::RuntimeVisibleEnvironment_O @ %X") % classcore__RuntimeVisibleEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::RuntimeVisibleEnvironment_O::static_className() % core::RuntimeVisibleEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SexpLoadArchive_O @ %X") % classcore__SexpLoadArchive_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SexpLoadArchive_O::static_className() % core::SexpLoadArchive_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SexpSaveArchive_O @ %X") % classcore__SexpSaveArchive_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SexpSaveArchive_O::static_className() % core::SexpSaveArchive_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Specializer_O @ %X") % classcore__Specializer_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Specializer_O::static_className() % core::Specializer_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StringStream_O @ %X") % classcore__StringStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StringStream_O::static_className() % core::StringStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::String_O @ %X") % classcore__String_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::String_O::static_className() % core::String_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SynonymStream_O @ %X") % classcore__SynonymStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SynonymStream_O::static_className() % core::SynonymStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::TagbodyFrame_O @ %X") % classcore__TagbodyFrame_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::TagbodyFrame_O::static_className() % core::TagbodyFrame_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::TwoWayStream_O @ %X") % classcore__TwoWayStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::TwoWayStream_O::static_className() % core::TwoWayStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ValueFrame_O @ %X") % classcore__ValueFrame_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ValueFrame_O::static_className() % core::ValueFrame_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::VectorObjects_O @ %X") % classcore__VectorObjects_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::VectorObjects_O::static_className() % core::VectorObjects_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::BlockEnvironment_O @ %X") % classcore__BlockEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::BlockEnvironment_O::static_className() % core::BlockEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::CatchEnvironment_O @ %X") % classcore__CatchEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::CatchEnvironment_O::static_className() % core::CatchEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Class_O @ %X") % classcore__Class_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Class_O::static_className() % core::Class_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::DoubleFloat_O @ %X") % classcore__DoubleFloat_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::DoubleFloat_O::static_className() % core::DoubleFloat_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FunctionContainerEnvironment_O @ %X") % classcore__FunctionContainerEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FunctionContainerEnvironment_O::static_className() % core::FunctionContainerEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FunctionValueEnvironment_O @ %X") % classcore__FunctionValueEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FunctionValueEnvironment_O::static_className() % core::FunctionValueEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::IOFileStream_O @ %X") % classcore__IOFileStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::IOFileStream_O::static_className() % core::IOFileStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::IOStreamStream_O @ %X") % classcore__IOStreamStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::IOStreamStream_O::static_className() % core::IOStreamStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Integer_O @ %X") % classcore__Integer_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Integer_O::static_className() % core::Integer_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::MacroletEnvironment_O @ %X") % classcore__MacroletEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::MacroletEnvironment_O::static_className() % core::MacroletEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Ratio_O @ %X") % classcore__Ratio_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Ratio_O::static_className() % core::Ratio_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ShortFloat_O @ %X") % classcore__ShortFloat_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ShortFloat_O::static_className() % core::ShortFloat_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SimpleBitVector_O @ %X") % classcore__SimpleBitVector_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SimpleBitVector_O::static_className() % core::SimpleBitVector_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SingleFloat_dummy_O @ %X") % classcore__SingleFloat_dummy_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SingleFloat_dummy_O::static_className() % core::SingleFloat_dummy_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StackValueEnvironment_O @ %X") % classcore__StackValueEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StackValueEnvironment_O::static_className() % core::StackValueEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Str_O @ %X") % classcore__Str_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Str_O::static_className() % core::Str_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StringInputStream_O @ %X") % classcore__StringInputStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StringInputStream_O::static_className() % core::StringInputStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StringOutputStream_O @ %X") % classcore__StringOutputStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StringOutputStream_O::static_className() % core::StringOutputStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SymbolMacroletEnvironment_O @ %X") % classcore__SymbolMacroletEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SymbolMacroletEnvironment_O::static_className() % core::SymbolMacroletEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::TagbodyEnvironment_O @ %X") % classcore__TagbodyEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::TagbodyEnvironment_O::static_className() % core::TagbodyEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::UnwindProtectEnvironment_O @ %X") % classcore__UnwindProtectEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::UnwindProtectEnvironment_O::static_className() % core::UnwindProtectEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ValueEnvironment_O @ %X") % classcore__ValueEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ValueEnvironment_O::static_className() % core::ValueEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::VectorObjectsWithFillPtr_O @ %X") % classcore__VectorObjectsWithFillPtr_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::VectorObjectsWithFillPtr_O::static_className() % core::VectorObjectsWithFillPtr_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Bignum_O @ %X") % classcore__Bignum_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Bignum_O::static_className() % core::Bignum_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::BuiltInClass_O @ %X") % classcore__BuiltInClass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::BuiltInClass_O::static_className() % core::BuiltInClass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Fixnum_dummy_O @ %X") % classcore__Fixnum_dummy_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Fixnum_dummy_O::static_className() % core::Fixnum_dummy_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ForwardReferencedClass_O @ %X") % classcore__ForwardReferencedClass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ForwardReferencedClass_O::static_className() % core::ForwardReferencedClass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StdClass_O @ %X") % classcore__StdClass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StdClass_O::static_className() % core::StdClass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StrWithFillPtr_O @ %X") % classcore__StrWithFillPtr_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StrWithFillPtr_O::static_className() % core::StrWithFillPtr_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StructureClass_O @ %X") % classcore__StructureClass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StructureClass_O::static_className() % core::StructureClass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FuncallableStandardClass_O @ %X") % classcore__FuncallableStandardClass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FuncallableStandardClass_O::static_className() % core::FuncallableStandardClass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StandardClass_O @ %X") % classcore__StandardClass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StandardClass_O::static_className() % core::StandardClass_O::static_classSymbol() );
#endif // } DUMP_INFO_CLASS
#undef DUMP_INFO_CLASS
#if defined(DEFINE_BASE_CLASSES) || defined(ALL_STAGES) // {
// Depends on nothing
classcore__Archive_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Array_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Binder_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__CandoException_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Character_dummy_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Cons_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__DirectoryEntry_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Environment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__ExternalObject_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__FileStatus_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Function_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__HashTable_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__IntArray_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Iterator_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__LambdaListHandler_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__LightUserData_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__LoadTimeValues_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__MicroHeap_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__MultiStringBuffer_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Number_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__ObjRef_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__ObjectSet_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Package_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Path_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Pathname_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Pointer_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__PosixTimeDuration_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__PosixTime_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__ReadTable_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Reader_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__RegexMatch_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Regex_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__SNode_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__SingleDispatchMethod_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__SmallMap_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__SourceFileInfo_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__SourceManager_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__SourcePosInfo_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__StandardObject_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Stream_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__StringList_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__StringSet_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__StructureObject_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__SymbolList_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__SymbolSet_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__SymbolToEnumConverter_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__Symbol_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__WeakHashTable_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__WeakKeyMapping_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__WeakPointer_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__WrappedPointer_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classcore__ActivationFrame_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Environment_O::static_classSymbol());
classcore__AnsiStream_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Stream_O::static_classSymbol());
classcore__ArrayDisplaced_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Array_O::static_classSymbol());
classcore__ArrayObjects_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Array_O::static_classSymbol());
classcore__BranchSNode_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::SNode_O::static_classSymbol());
classcore__CompiledFunction_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Function_O::static_classSymbol());
classcore__Complex_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Number_O::static_classSymbol());
classcore__DirectoryIterator_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Iterator_O::static_classSymbol());
classcore__ForeignData_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());
classcore__GlueEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Environment_O::static_classSymbol());
classcore__HashTableEq_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::HashTable_O::static_classSymbol());
classcore__HashTableEql_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::HashTable_O::static_classSymbol());
classcore__HashTableEqual_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::HashTable_O::static_classSymbol());
classcore__HashTableEqualp_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::HashTable_O::static_classSymbol());
classcore__Instance_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Function_O::static_classSymbol());
classcore__LeafSNode_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::SNode_O::static_classSymbol());
classcore__LexicalEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Environment_O::static_classSymbol());
classcore__LoadArchive_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Archive_O::static_classSymbol());
classcore__LogicalPathname_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Pathname_O::static_classSymbol());
classcore__Metaobject_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::StandardObject_O::static_classSymbol());
classcore__Null_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Symbol_O::static_classSymbol());
classcore__Real_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Number_O::static_classSymbol());
classcore__RecursiveDirectoryIterator_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Iterator_O::static_classSymbol());
classcore__SaveArchive_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Archive_O::static_classSymbol());
classcore__SingleDispatchEffectiveMethodFunction_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Function_O::static_classSymbol());
classcore__SingleDispatchGenericFunction_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Function_O::static_classSymbol());
classcore__SpecialForm_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Function_O::static_classSymbol());
classcore__UserData_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::LightUserData_O::static_classSymbol());
classcore__Vector_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Array_O::static_classSymbol());
classcore__WeakKeyHashTable_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::WeakHashTable_O::static_classSymbol());
classcore__BitVector_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Vector_O::static_classSymbol());
classcore__BroadcastStream_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::AnsiStream_O::static_classSymbol());
classcore__CompileTimeEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::LexicalEnvironment_O::static_classSymbol());
classcore__ConcatenatedStream_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::AnsiStream_O::static_classSymbol());
classcore__EchoStream_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::AnsiStream_O::static_classSymbol());
classcore__FileStream_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::AnsiStream_O::static_classSymbol());
classcore__Float_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Real_O::static_classSymbol());
classcore__FunctionFrame_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ActivationFrame_O::static_classSymbol());
classcore__Rational_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Real_O::static_classSymbol());
classcore__RuntimeVisibleEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::LexicalEnvironment_O::static_classSymbol());
classcore__SexpLoadArchive_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::LoadArchive_O::static_classSymbol());
classcore__SexpSaveArchive_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::SaveArchive_O::static_classSymbol());
classcore__Specializer_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Metaobject_O::static_classSymbol());
classcore__StringStream_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::AnsiStream_O::static_classSymbol());
classcore__String_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Vector_O::static_classSymbol());
classcore__SynonymStream_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::AnsiStream_O::static_classSymbol());
classcore__TagbodyFrame_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ActivationFrame_O::static_classSymbol());
classcore__TwoWayStream_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::AnsiStream_O::static_classSymbol());
classcore__ValueFrame_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::ActivationFrame_O::static_classSymbol());
classcore__VectorObjects_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Vector_O::static_classSymbol());
classcore__BlockEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__CatchEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__Class_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Specializer_O::static_classSymbol());
classcore__DoubleFloat_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Float_O::static_classSymbol());
classcore__FunctionContainerEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__FunctionValueEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::RuntimeVisibleEnvironment_O::static_classSymbol());
classcore__IOFileStream_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::FileStream_O::static_classSymbol());
classcore__IOStreamStream_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::FileStream_O::static_classSymbol());
classcore__Integer_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Rational_O::static_classSymbol());
classcore__MacroletEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__Ratio_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Rational_O::static_classSymbol());
classcore__ShortFloat_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Float_O::static_classSymbol());
classcore__SimpleBitVector_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::BitVector_O::static_classSymbol());
classcore__SingleFloat_dummy_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Float_O::static_classSymbol());
classcore__StackValueEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__Str_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::String_O::static_classSymbol());
classcore__StringInputStream_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::StringStream_O::static_classSymbol());
classcore__StringOutputStream_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::StringStream_O::static_classSymbol());
classcore__SymbolMacroletEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__TagbodyEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::RuntimeVisibleEnvironment_O::static_classSymbol());
classcore__UnwindProtectEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__ValueEnvironment_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::RuntimeVisibleEnvironment_O::static_classSymbol());
classcore__VectorObjectsWithFillPtr_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::VectorObjects_O::static_classSymbol());
classcore__Bignum_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Integer_O::static_classSymbol());
classcore__BuiltInClass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Class_O::static_classSymbol());
classcore__Fixnum_dummy_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Integer_O::static_classSymbol());
classcore__ForwardReferencedClass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Class_O::static_classSymbol());
classcore__StdClass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Class_O::static_classSymbol());
classcore__StrWithFillPtr_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Str_O::static_classSymbol());
classcore__StructureClass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::Class_O::static_classSymbol());
classcore__FuncallableStandardClass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::StdClass_O::static_classSymbol());
classcore__StandardClass_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::StdClass_O::static_classSymbol());
#endif // } DEFINE_BASE_CLASSES
#undef DEFINE_BASE_CLASSES
#if defined(DEFINE_CLASS_NAMES) || defined(ALL_STAGES) // {
// Depends on nothing

    classcore__T_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::T_O::static_classSymbol());

    classcore__Archive_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Archive_O::static_classSymbol());

    classcore__Array_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Array_O::static_classSymbol());

    classcore__Binder_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Binder_O::static_classSymbol());

    classcore__CandoException_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::CandoException_O::static_classSymbol());

    classcore__Character_dummy_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Character_dummy_O::static_classSymbol());

    classcore__Cons_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Cons_O::static_classSymbol());

    classcore__DirectoryEntry_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::DirectoryEntry_O::static_classSymbol());

    classcore__Environment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Environment_O::static_classSymbol());

    classcore__ExternalObject_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::ExternalObject_O::static_classSymbol());

    classcore__FileStatus_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::FileStatus_O::static_classSymbol());

    classcore__Function_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Function_O::static_classSymbol());

    classcore__HashTable_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::HashTable_O::static_classSymbol());

    classcore__IntArray_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::IntArray_O::static_classSymbol());

    classcore__Iterator_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Iterator_O::static_classSymbol());

    classcore__LambdaListHandler_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::LambdaListHandler_O::static_classSymbol());

    classcore__LightUserData_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::LightUserData_O::static_classSymbol());

    classcore__LoadTimeValues_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::LoadTimeValues_O::static_classSymbol());

    classcore__MicroHeap_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::MicroHeap_O::static_classSymbol());

    classcore__MultiStringBuffer_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::MultiStringBuffer_O::static_classSymbol());

    classcore__Number_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Number_O::static_classSymbol());

    classcore__ObjRef_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::ObjRef_O::static_classSymbol());

    classcore__ObjectSet_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::ObjectSet_O::static_classSymbol());

    classcore__Package_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Package_O::static_classSymbol());

    classcore__Path_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Path_O::static_classSymbol());

    classcore__Pathname_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Pathname_O::static_classSymbol());

    classcore__Pointer_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Pointer_O::static_classSymbol());

    classcore__PosixTimeDuration_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::PosixTimeDuration_O::static_classSymbol());

    classcore__PosixTime_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::PosixTime_O::static_classSymbol());

    classcore__ReadTable_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::ReadTable_O::static_classSymbol());

    classcore__Reader_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Reader_O::static_classSymbol());

    classcore__RegexMatch_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::RegexMatch_O::static_classSymbol());

    classcore__Regex_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Regex_O::static_classSymbol());

    classcore__SNode_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SNode_O::static_classSymbol());

    classcore__SingleDispatchMethod_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SingleDispatchMethod_O::static_classSymbol());

    classcore__SmallMap_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SmallMap_O::static_classSymbol());

    classcore__SourceFileInfo_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SourceFileInfo_O::static_classSymbol());

    classcore__SourceManager_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SourceManager_O::static_classSymbol());

    classcore__SourcePosInfo_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SourcePosInfo_O::static_classSymbol());

    classcore__StandardObject_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::StandardObject_O::static_classSymbol());

    classcore__Stream_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Stream_O::static_classSymbol());

    classcore__StringList_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::StringList_O::static_classSymbol());

    classcore__StringSet_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::StringSet_O::static_classSymbol());

    classcore__StructureObject_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::StructureObject_O::static_classSymbol());

    classcore__SymbolList_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SymbolList_O::static_classSymbol());

    classcore__SymbolSet_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SymbolSet_O::static_classSymbol());

    classcore__SymbolToEnumConverter_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SymbolToEnumConverter_O::static_classSymbol());

    classcore__Symbol_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Symbol_O::static_classSymbol());

    classcore__WeakHashTable_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::WeakHashTable_O::static_classSymbol());

    classcore__WeakKeyMapping_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::WeakKeyMapping_O::static_classSymbol());

    classcore__WeakPointer_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::WeakPointer_O::static_classSymbol());

    classcore__WrappedPointer_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::WrappedPointer_O::static_classSymbol());

    classcore__ActivationFrame_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::ActivationFrame_O::static_classSymbol());

    classcore__AnsiStream_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::AnsiStream_O::static_classSymbol());

    classcore__ArrayDisplaced_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::ArrayDisplaced_O::static_classSymbol());

    classcore__ArrayObjects_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::ArrayObjects_O::static_classSymbol());

    classcore__BranchSNode_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::BranchSNode_O::static_classSymbol());

    classcore__CompiledFunction_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::CompiledFunction_O::static_classSymbol());

    classcore__Complex_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Complex_O::static_classSymbol());

    classcore__DirectoryIterator_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::DirectoryIterator_O::static_classSymbol());

    classcore__ForeignData_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::ForeignData_O::static_classSymbol());

    classcore__GlueEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::GlueEnvironment_O::static_classSymbol());

    classcore__HashTableEq_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::HashTableEq_O::static_classSymbol());

    classcore__HashTableEql_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::HashTableEql_O::static_classSymbol());

    classcore__HashTableEqual_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::HashTableEqual_O::static_classSymbol());

    classcore__HashTableEqualp_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::HashTableEqualp_O::static_classSymbol());

    classcore__Instance_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Instance_O::static_classSymbol());

    classcore__LeafSNode_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::LeafSNode_O::static_classSymbol());

    classcore__LexicalEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::LexicalEnvironment_O::static_classSymbol());

    classcore__LoadArchive_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::LoadArchive_O::static_classSymbol());

    classcore__LogicalPathname_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::LogicalPathname_O::static_classSymbol());

    classcore__Metaobject_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Metaobject_O::static_classSymbol());

    classcore__Null_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Null_O::static_classSymbol());

    classcore__Real_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Real_O::static_classSymbol());

    classcore__RecursiveDirectoryIterator_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::RecursiveDirectoryIterator_O::static_classSymbol());

    classcore__SaveArchive_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SaveArchive_O::static_classSymbol());

    classcore__SingleDispatchEffectiveMethodFunction_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SingleDispatchEffectiveMethodFunction_O::static_classSymbol());

    classcore__SingleDispatchGenericFunction_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SingleDispatchGenericFunction_O::static_classSymbol());

    classcore__SpecialForm_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SpecialForm_O::static_classSymbol());

    classcore__UserData_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::UserData_O::static_classSymbol());

    classcore__Vector_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Vector_O::static_classSymbol());

    classcore__WeakKeyHashTable_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::WeakKeyHashTable_O::static_classSymbol());

    classcore__BitVector_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::BitVector_O::static_classSymbol());

    classcore__BroadcastStream_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::BroadcastStream_O::static_classSymbol());

    classcore__CompileTimeEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::CompileTimeEnvironment_O::static_classSymbol());

    classcore__ConcatenatedStream_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::ConcatenatedStream_O::static_classSymbol());

    classcore__EchoStream_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::EchoStream_O::static_classSymbol());

    classcore__FileStream_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::FileStream_O::static_classSymbol());

    classcore__Float_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Float_O::static_classSymbol());

    classcore__FunctionFrame_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::FunctionFrame_O::static_classSymbol());

    classcore__Rational_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Rational_O::static_classSymbol());

    classcore__RuntimeVisibleEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::RuntimeVisibleEnvironment_O::static_classSymbol());

    classcore__SexpLoadArchive_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SexpLoadArchive_O::static_classSymbol());

    classcore__SexpSaveArchive_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SexpSaveArchive_O::static_classSymbol());

    classcore__Specializer_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Specializer_O::static_classSymbol());

    classcore__StringStream_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::StringStream_O::static_classSymbol());

    classcore__String_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::String_O::static_classSymbol());

    classcore__SynonymStream_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SynonymStream_O::static_classSymbol());

    classcore__TagbodyFrame_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::TagbodyFrame_O::static_classSymbol());

    classcore__TwoWayStream_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::TwoWayStream_O::static_classSymbol());

    classcore__ValueFrame_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::ValueFrame_O::static_classSymbol());

    classcore__VectorObjects_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::VectorObjects_O::static_classSymbol());

    classcore__BlockEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::BlockEnvironment_O::static_classSymbol());

    classcore__CatchEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::CatchEnvironment_O::static_classSymbol());

    classcore__Class_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Class_O::static_classSymbol());

    classcore__DoubleFloat_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::DoubleFloat_O::static_classSymbol());

    classcore__FunctionContainerEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::FunctionContainerEnvironment_O::static_classSymbol());

    classcore__FunctionValueEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::FunctionValueEnvironment_O::static_classSymbol());

    classcore__IOFileStream_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::IOFileStream_O::static_classSymbol());

    classcore__IOStreamStream_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::IOStreamStream_O::static_classSymbol());

    classcore__Integer_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Integer_O::static_classSymbol());

    classcore__MacroletEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::MacroletEnvironment_O::static_classSymbol());

    classcore__Ratio_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Ratio_O::static_classSymbol());

    classcore__ShortFloat_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::ShortFloat_O::static_classSymbol());

    classcore__SimpleBitVector_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SimpleBitVector_O::static_classSymbol());

    classcore__SingleFloat_dummy_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SingleFloat_dummy_O::static_classSymbol());

    classcore__StackValueEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::StackValueEnvironment_O::static_classSymbol());

    classcore__Str_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Str_O::static_classSymbol());

    classcore__StringInputStream_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::StringInputStream_O::static_classSymbol());

    classcore__StringOutputStream_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::StringOutputStream_O::static_classSymbol());

    classcore__SymbolMacroletEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::SymbolMacroletEnvironment_O::static_classSymbol());

    classcore__TagbodyEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::TagbodyEnvironment_O::static_classSymbol());

    classcore__UnwindProtectEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::UnwindProtectEnvironment_O::static_classSymbol());

    classcore__ValueEnvironment_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::ValueEnvironment_O::static_classSymbol());

    classcore__VectorObjectsWithFillPtr_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::VectorObjectsWithFillPtr_O::static_classSymbol());

    classcore__Bignum_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Bignum_O::static_classSymbol());

    classcore__BuiltInClass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::BuiltInClass_O::static_classSymbol());

    classcore__Fixnum_dummy_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::Fixnum_dummy_O::static_classSymbol());

    classcore__ForwardReferencedClass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::ForwardReferencedClass_O::static_classSymbol());

    classcore__StdClass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::StdClass_O::static_classSymbol());

    classcore__StrWithFillPtr_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::StrWithFillPtr_O::static_classSymbol());

    classcore__StructureClass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::StructureClass_O::static_classSymbol());

    classcore__FuncallableStandardClass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::FuncallableStandardClass_O::static_classSymbol());

    classcore__StandardClass_Oval->__setupStage3NameAndCalculateClassPrecedenceList(core::StandardClass_O::static_classSymbol());
#endif // } DEFINE_CLASS_NAMES
#undef DEFINE_CLASS_NAMES
#if defined(EXPOSE_TO_CANDO) || defined(ALL_STAGES)
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__T_O(core::Lisp_sp); // base(s): set(['core::_RootDummyClass'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__T_O");
    core::Register_core__T_O(_lisp); // base(s): set(['core::_RootDummyClass'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__Archive_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Archive_O");
    core::Register_core__Archive_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Array_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Array_O");
    core::Register_core__Array_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__Binder_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Binder_O");
    core::Register_core__Binder_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__CandoException_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__CandoException_O");
    core::Register_core__CandoException_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Character_dummy_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Character_dummy_O");
    core::Register_core__Character_dummy_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Cons_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Cons_O");
    core::Register_core__Cons_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__DirectoryEntry_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__DirectoryEntry_O");
    core::Register_core__DirectoryEntry_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__Environment_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Environment_O");
    core::Register_core__Environment_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__ExternalObject_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__ExternalObject_O");
    core::Register_core__ExternalObject_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__FileStatus_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__FileStatus_O");
    core::Register_core__FileStatus_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Function_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Function_O");
    core::Register_core__Function_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__HashTable_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__HashTable_O");
    core::Register_core__HashTable_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__IntArray_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__IntArray_O");
    core::Register_core__IntArray_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__Iterator_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Iterator_O");
    core::Register_core__Iterator_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__LambdaListHandler_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__LambdaListHandler_O");
    core::Register_core__LambdaListHandler_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__LightUserData_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__LightUserData_O");
    core::Register_core__LightUserData_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__LoadTimeValues_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__LoadTimeValues_O");
    core::Register_core__LoadTimeValues_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__MicroHeap_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__MicroHeap_O");
    core::Register_core__MicroHeap_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__MultiStringBuffer_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__MultiStringBuffer_O");
    core::Register_core__MultiStringBuffer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Number_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Number_O");
    core::Register_core__Number_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__ObjRef_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__ObjRef_O");
    core::Register_core__ObjRef_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__ObjectSet_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__ObjectSet_O");
    core::Register_core__ObjectSet_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Package_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Package_O");
    core::Register_core__Package_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__Path_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Path_O");
    core::Register_core__Path_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Pathname_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Pathname_O");
    core::Register_core__Pathname_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__Pointer_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Pointer_O");
    core::Register_core__Pointer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__PosixTimeDuration_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__PosixTimeDuration_O");
    core::Register_core__PosixTimeDuration_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__PosixTime_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__PosixTime_O");
    core::Register_core__PosixTime_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__ReadTable_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__ReadTable_O");
    core::Register_core__ReadTable_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__Reader_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Reader_O");
    core::Register_core__Reader_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__RegexMatch_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__RegexMatch_O");
    core::Register_core__RegexMatch_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__Regex_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Regex_O");
    core::Register_core__Regex_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SNode_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SNode_O");
    core::Register_core__SNode_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SingleDispatchMethod_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SingleDispatchMethod_O");
    core::Register_core__SingleDispatchMethod_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SmallMap_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SmallMap_O");
    core::Register_core__SmallMap_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SourceFileInfo_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SourceFileInfo_O");
    core::Register_core__SourceFileInfo_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SourceManager_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SourceManager_O");
    core::Register_core__SourceManager_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SourcePosInfo_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SourcePosInfo_O");
    core::Register_core__SourcePosInfo_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__StandardObject_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__StandardObject_O");
    core::Register_core__StandardObject_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Stream_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Stream_O");
    core::Register_core__Stream_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__StringList_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__StringList_O");
    core::Register_core__StringList_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__StringSet_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__StringSet_O");
    core::Register_core__StringSet_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__StructureObject_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__StructureObject_O");
    core::Register_core__StructureObject_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SymbolList_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SymbolList_O");
    core::Register_core__SymbolList_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SymbolSet_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SymbolSet_O");
    core::Register_core__SymbolSet_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SymbolToEnumConverter_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SymbolToEnumConverter_O");
    core::Register_core__SymbolToEnumConverter_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Symbol_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Symbol_O");
    core::Register_core__Symbol_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__WeakHashTable_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__WeakHashTable_O");
    core::Register_core__WeakHashTable_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__WeakKeyMapping_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__WeakKeyMapping_O");
    core::Register_core__WeakKeyMapping_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__WeakPointer_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__WeakPointer_O");
    core::Register_core__WeakPointer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__WrappedPointer_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__WrappedPointer_O");
    core::Register_core__WrappedPointer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__ActivationFrame_O(core::Lisp_sp); // base(s): set(['core::Environment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__ActivationFrame_O");
    core::Register_core__ActivationFrame_O(_lisp); // base(s): set(['core::Environment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ExtPkg
#ifdef EXTERN_REGISTER
extern void Register_core__AnsiStream_O(core::Lisp_sp); // base(s): set(['core::Stream_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__AnsiStream_O");
    core::Register_core__AnsiStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ExtPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__ArrayDisplaced_O(core::Lisp_sp); // base(s): set(['core::Array_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__ArrayDisplaced_O");
    core::Register_core__ArrayDisplaced_O(_lisp); // base(s): set(['core::Array_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__ArrayObjects_O(core::Lisp_sp); // base(s): set(['core::Array_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__ArrayObjects_O");
    core::Register_core__ArrayObjects_O(_lisp); // base(s): set(['core::Array_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__BranchSNode_O(core::Lisp_sp); // base(s): set(['core::SNode_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__BranchSNode_O");
    core::Register_core__BranchSNode_O(_lisp); // base(s): set(['core::SNode_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__CompiledFunction_O(core::Lisp_sp); // base(s): set(['core::Function_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__CompiledFunction_O");
    core::Register_core__CompiledFunction_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Complex_O(core::Lisp_sp); // base(s): set(['core::Number_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Complex_O");
    core::Register_core__Complex_O(_lisp); // base(s): set(['core::Number_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__DirectoryIterator_O(core::Lisp_sp); // base(s): set(['core::Iterator_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__DirectoryIterator_O");
    core::Register_core__DirectoryIterator_O(_lisp); // base(s): set(['core::Iterator_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__ForeignData_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__ForeignData_O");
    core::Register_core__ForeignData_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__GlueEnvironment_O(core::Lisp_sp); // base(s): set(['core::Environment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__GlueEnvironment_O");
    core::Register_core__GlueEnvironment_O(_lisp); // base(s): set(['core::Environment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__HashTableEq_O(core::Lisp_sp); // base(s): set(['core::HashTable_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__HashTableEq_O");
    core::Register_core__HashTableEq_O(_lisp); // base(s): set(['core::HashTable_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__HashTableEql_O(core::Lisp_sp); // base(s): set(['core::HashTable_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__HashTableEql_O");
    core::Register_core__HashTableEql_O(_lisp); // base(s): set(['core::HashTable_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__HashTableEqual_O(core::Lisp_sp); // base(s): set(['core::HashTable_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__HashTableEqual_O");
    core::Register_core__HashTableEqual_O(_lisp); // base(s): set(['core::HashTable_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__HashTableEqualp_O(core::Lisp_sp); // base(s): set(['core::HashTable_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__HashTableEqualp_O");
    core::Register_core__HashTableEqualp_O(_lisp); // base(s): set(['core::HashTable_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__Instance_O(core::Lisp_sp); // base(s): set(['core::Function_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Instance_O");
    core::Register_core__Instance_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__LeafSNode_O(core::Lisp_sp); // base(s): set(['core::SNode_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__LeafSNode_O");
    core::Register_core__LeafSNode_O(_lisp); // base(s): set(['core::SNode_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__LexicalEnvironment_O(core::Lisp_sp); // base(s): set(['core::Environment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__LexicalEnvironment_O");
    core::Register_core__LexicalEnvironment_O(_lisp); // base(s): set(['core::Environment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__LoadArchive_O(core::Lisp_sp); // base(s): set(['core::Archive_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__LoadArchive_O");
    core::Register_core__LoadArchive_O(_lisp); // base(s): set(['core::Archive_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__LogicalPathname_O(core::Lisp_sp); // base(s): set(['core::Pathname_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__LogicalPathname_O");
    core::Register_core__LogicalPathname_O(_lisp); // base(s): set(['core::Pathname_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__Metaobject_O(core::Lisp_sp); // base(s): set(['core::StandardObject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Metaobject_O");
    core::Register_core__Metaobject_O(_lisp); // base(s): set(['core::StandardObject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Null_O(core::Lisp_sp); // base(s): set(['core::Symbol_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Null_O");
    core::Register_core__Null_O(_lisp); // base(s): set(['core::Symbol_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Real_O(core::Lisp_sp); // base(s): set(['core::Number_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Real_O");
    core::Register_core__Real_O(_lisp); // base(s): set(['core::Number_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__RecursiveDirectoryIterator_O(core::Lisp_sp); // base(s): set(['core::Iterator_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__RecursiveDirectoryIterator_O");
    core::Register_core__RecursiveDirectoryIterator_O(_lisp); // base(s): set(['core::Iterator_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SaveArchive_O(core::Lisp_sp); // base(s): set(['core::Archive_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SaveArchive_O");
    core::Register_core__SaveArchive_O(_lisp); // base(s): set(['core::Archive_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SingleDispatchEffectiveMethodFunction_O(core::Lisp_sp); // base(s): set(['core::Function_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SingleDispatchEffectiveMethodFunction_O");
    core::Register_core__SingleDispatchEffectiveMethodFunction_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SingleDispatchGenericFunction_O(core::Lisp_sp); // base(s): set(['core::Function_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SingleDispatchGenericFunction_O");
    core::Register_core__SingleDispatchGenericFunction_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SpecialForm_O(core::Lisp_sp); // base(s): set(['core::Function_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SpecialForm_O");
    core::Register_core__SpecialForm_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__UserData_O(core::Lisp_sp); // base(s): set(['core::LightUserData_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__UserData_O");
    core::Register_core__UserData_O(_lisp); // base(s): set(['core::LightUserData_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Vector_O(core::Lisp_sp); // base(s): set(['core::Array_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Vector_O");
    core::Register_core__Vector_O(_lisp); // base(s): set(['core::Array_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__WeakKeyHashTable_O(core::Lisp_sp); // base(s): set(['core::WeakHashTable_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__WeakKeyHashTable_O");
    core::Register_core__WeakKeyHashTable_O(_lisp); // base(s): set(['core::WeakHashTable_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__BitVector_O(core::Lisp_sp); // base(s): set(['core::Vector_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__BitVector_O");
    core::Register_core__BitVector_O(_lisp); // base(s): set(['core::Vector_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__BroadcastStream_O(core::Lisp_sp); // base(s): set(['core::AnsiStream_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__BroadcastStream_O");
    core::Register_core__BroadcastStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__CompileTimeEnvironment_O(core::Lisp_sp); // base(s): set(['core::LexicalEnvironment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__CompileTimeEnvironment_O");
    core::Register_core__CompileTimeEnvironment_O(_lisp); // base(s): set(['core::LexicalEnvironment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__ConcatenatedStream_O(core::Lisp_sp); // base(s): set(['core::AnsiStream_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__ConcatenatedStream_O");
    core::Register_core__ConcatenatedStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__EchoStream_O(core::Lisp_sp); // base(s): set(['core::AnsiStream_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__EchoStream_O");
    core::Register_core__EchoStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__FileStream_O(core::Lisp_sp); // base(s): set(['core::AnsiStream_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__FileStream_O");
    core::Register_core__FileStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Float_O(core::Lisp_sp); // base(s): set(['core::Real_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Float_O");
    core::Register_core__Float_O(_lisp); // base(s): set(['core::Real_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__FunctionFrame_O(core::Lisp_sp); // base(s): set(['core::ActivationFrame_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__FunctionFrame_O");
    core::Register_core__FunctionFrame_O(_lisp); // base(s): set(['core::ActivationFrame_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Rational_O(core::Lisp_sp); // base(s): set(['core::Real_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Rational_O");
    core::Register_core__Rational_O(_lisp); // base(s): set(['core::Real_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__RuntimeVisibleEnvironment_O(core::Lisp_sp); // base(s): set(['core::LexicalEnvironment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__RuntimeVisibleEnvironment_O");
    core::Register_core__RuntimeVisibleEnvironment_O(_lisp); // base(s): set(['core::LexicalEnvironment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SexpLoadArchive_O(core::Lisp_sp); // base(s): set(['core::LoadArchive_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SexpLoadArchive_O");
    core::Register_core__SexpLoadArchive_O(_lisp); // base(s): set(['core::LoadArchive_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SexpSaveArchive_O(core::Lisp_sp); // base(s): set(['core::SaveArchive_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SexpSaveArchive_O");
    core::Register_core__SexpSaveArchive_O(_lisp); // base(s): set(['core::SaveArchive_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__Specializer_O(core::Lisp_sp); // base(s): set(['core::Metaobject_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Specializer_O");
    core::Register_core__Specializer_O(_lisp); // base(s): set(['core::Metaobject_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__StringStream_O(core::Lisp_sp); // base(s): set(['core::AnsiStream_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__StringStream_O");
    core::Register_core__StringStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__String_O(core::Lisp_sp); // base(s): set(['core::Vector_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__String_O");
    core::Register_core__String_O(_lisp); // base(s): set(['core::Vector_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__SynonymStream_O(core::Lisp_sp); // base(s): set(['core::AnsiStream_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SynonymStream_O");
    core::Register_core__SynonymStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__TagbodyFrame_O(core::Lisp_sp); // base(s): set(['core::ActivationFrame_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__TagbodyFrame_O");
    core::Register_core__TagbodyFrame_O(_lisp); // base(s): set(['core::ActivationFrame_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__TwoWayStream_O(core::Lisp_sp); // base(s): set(['core::AnsiStream_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__TwoWayStream_O");
    core::Register_core__TwoWayStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__ValueFrame_O(core::Lisp_sp); // base(s): set(['core::ActivationFrame_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__ValueFrame_O");
    core::Register_core__ValueFrame_O(_lisp); // base(s): set(['core::ActivationFrame_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__VectorObjects_O(core::Lisp_sp); // base(s): set(['core::Vector_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__VectorObjects_O");
    core::Register_core__VectorObjects_O(_lisp); // base(s): set(['core::Vector_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__BlockEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__BlockEnvironment_O");
    core::Register_core__BlockEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__CatchEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__CatchEnvironment_O");
    core::Register_core__CatchEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Class_O(core::Lisp_sp); // base(s): set(['core::Specializer_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Class_O");
    core::Register_core__Class_O(_lisp); // base(s): set(['core::Specializer_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__DoubleFloat_O(core::Lisp_sp); // base(s): set(['core::Float_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__DoubleFloat_O");
    core::Register_core__DoubleFloat_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__FunctionContainerEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__FunctionContainerEnvironment_O");
    core::Register_core__FunctionContainerEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__FunctionValueEnvironment_O(core::Lisp_sp); // base(s): set(['core::RuntimeVisibleEnvironment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__FunctionValueEnvironment_O");
    core::Register_core__FunctionValueEnvironment_O(_lisp); // base(s): set(['core::RuntimeVisibleEnvironment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__IOFileStream_O(core::Lisp_sp); // base(s): set(['core::FileStream_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__IOFileStream_O");
    core::Register_core__IOFileStream_O(_lisp); // base(s): set(['core::FileStream_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__IOStreamStream_O(core::Lisp_sp); // base(s): set(['core::FileStream_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__IOStreamStream_O");
    core::Register_core__IOStreamStream_O(_lisp); // base(s): set(['core::FileStream_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Integer_O(core::Lisp_sp); // base(s): set(['core::Rational_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Integer_O");
    core::Register_core__Integer_O(_lisp); // base(s): set(['core::Rational_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__MacroletEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__MacroletEnvironment_O");
    core::Register_core__MacroletEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Ratio_O(core::Lisp_sp); // base(s): set(['core::Rational_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Ratio_O");
    core::Register_core__Ratio_O(_lisp); // base(s): set(['core::Rational_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__ShortFloat_O(core::Lisp_sp); // base(s): set(['core::Float_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__ShortFloat_O");
    core::Register_core__ShortFloat_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__SimpleBitVector_O(core::Lisp_sp); // base(s): set(['core::BitVector_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SimpleBitVector_O");
    core::Register_core__SimpleBitVector_O(_lisp); // base(s): set(['core::BitVector_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__SingleFloat_dummy_O(core::Lisp_sp); // base(s): set(['core::Float_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SingleFloat_dummy_O");
    core::Register_core__SingleFloat_dummy_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__StackValueEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__StackValueEnvironment_O");
    core::Register_core__StackValueEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Str_O(core::Lisp_sp); // base(s): set(['core::String_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Str_O");
    core::Register_core__Str_O(_lisp); // base(s): set(['core::String_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__StringInputStream_O(core::Lisp_sp); // base(s): set(['core::StringStream_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__StringInputStream_O");
    core::Register_core__StringInputStream_O(_lisp); // base(s): set(['core::StringStream_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__StringOutputStream_O(core::Lisp_sp); // base(s): set(['core::StringStream_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__StringOutputStream_O");
    core::Register_core__StringOutputStream_O(_lisp); // base(s): set(['core::StringStream_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__SymbolMacroletEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__SymbolMacroletEnvironment_O");
    core::Register_core__SymbolMacroletEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__TagbodyEnvironment_O(core::Lisp_sp); // base(s): set(['core::RuntimeVisibleEnvironment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__TagbodyEnvironment_O");
    core::Register_core__TagbodyEnvironment_O(_lisp); // base(s): set(['core::RuntimeVisibleEnvironment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__UnwindProtectEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__UnwindProtectEnvironment_O");
    core::Register_core__UnwindProtectEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__ValueEnvironment_O(core::Lisp_sp); // base(s): set(['core::RuntimeVisibleEnvironment_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__ValueEnvironment_O");
    core::Register_core__ValueEnvironment_O(_lisp); // base(s): set(['core::RuntimeVisibleEnvironment_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__VectorObjectsWithFillPtr_O(core::Lisp_sp); // base(s): set(['core::VectorObjects_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__VectorObjectsWithFillPtr_O");
    core::Register_core__VectorObjectsWithFillPtr_O(_lisp); // base(s): set(['core::VectorObjects_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Bignum_O(core::Lisp_sp); // base(s): set(['core::Integer_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Bignum_O");
    core::Register_core__Bignum_O(_lisp); // base(s): set(['core::Integer_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__BuiltInClass_O(core::Lisp_sp); // base(s): set(['core::Class_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__BuiltInClass_O");
    core::Register_core__BuiltInClass_O(_lisp); // base(s): set(['core::Class_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__Fixnum_dummy_O(core::Lisp_sp); // base(s): set(['core::Integer_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__Fixnum_dummy_O");
    core::Register_core__Fixnum_dummy_O(_lisp); // base(s): set(['core::Integer_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__ForwardReferencedClass_O(core::Lisp_sp); // base(s): set(['core::Class_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__ForwardReferencedClass_O");
    core::Register_core__ForwardReferencedClass_O(_lisp); // base(s): set(['core::Class_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClosPkg
#ifdef EXTERN_REGISTER
extern void Register_core__StdClass_O(core::Lisp_sp); // base(s): set(['core::Class_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__StdClass_O");
    core::Register_core__StdClass_O(_lisp); // base(s): set(['core::Class_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClosPkg
#ifdef Use_CorePkg
#ifdef EXTERN_REGISTER
extern void Register_core__StrWithFillPtr_O(core::Lisp_sp); // base(s): set(['core::Str_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__StrWithFillPtr_O");
    core::Register_core__StrWithFillPtr_O(_lisp); // base(s): set(['core::Str_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__StructureClass_O(core::Lisp_sp); // base(s): set(['core::Class_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__StructureClass_O");
    core::Register_core__StructureClass_O(_lisp); // base(s): set(['core::Class_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#ifdef Use_ClosPkg
#ifdef EXTERN_REGISTER
extern void Register_core__FuncallableStandardClass_O(core::Lisp_sp); // base(s): set(['core::StdClass_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__FuncallableStandardClass_O");
    core::Register_core__FuncallableStandardClass_O(_lisp); // base(s): set(['core::StdClass_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClosPkg
#ifdef Use_ClPkg
#ifdef EXTERN_REGISTER
extern void Register_core__StandardClass_O(core::Lisp_sp); // base(s): set(['core::StdClass_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_core__StandardClass_O");
    core::Register_core__StandardClass_O(_lisp); // base(s): set(['core::StdClass_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClPkg
#endif // EXPOSE_TO_CANDO
#undef EXPOSE_TO_CANDO
#ifdef EXPOSE_TO_PYTHON
#ifdef Use_ClPkg
extern void Call_exposePython_core__T_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__T_O");
	Call_exposePython_core__T_O(_lisp); // base(s): set(['core::_RootDummyClass'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Archive_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Archive_O");
	Call_exposePython_core__Archive_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Array_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Array_O");
	Call_exposePython_core__Array_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Binder_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Binder_O");
	Call_exposePython_core__Binder_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__CandoException_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__CandoException_O");
	Call_exposePython_core__CandoException_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Character_dummy_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Character_dummy_O");
	Call_exposePython_core__Character_dummy_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Cons_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Cons_O");
	Call_exposePython_core__Cons_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__DirectoryEntry_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__DirectoryEntry_O");
	Call_exposePython_core__DirectoryEntry_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Environment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Environment_O");
	Call_exposePython_core__Environment_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ExternalObject_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ExternalObject_O");
	Call_exposePython_core__ExternalObject_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FileStatus_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FileStatus_O");
	Call_exposePython_core__FileStatus_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Function_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Function_O");
	Call_exposePython_core__Function_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__HashTable_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__HashTable_O");
	Call_exposePython_core__HashTable_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__IntArray_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__IntArray_O");
	Call_exposePython_core__IntArray_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Iterator_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Iterator_O");
	Call_exposePython_core__Iterator_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__LambdaListHandler_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__LambdaListHandler_O");
	Call_exposePython_core__LambdaListHandler_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__LightUserData_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__LightUserData_O");
	Call_exposePython_core__LightUserData_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__LoadTimeValues_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__LoadTimeValues_O");
	Call_exposePython_core__LoadTimeValues_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__MicroHeap_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__MicroHeap_O");
	Call_exposePython_core__MicroHeap_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__MultiStringBuffer_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__MultiStringBuffer_O");
	Call_exposePython_core__MultiStringBuffer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Number_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Number_O");
	Call_exposePython_core__Number_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ObjRef_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ObjRef_O");
	Call_exposePython_core__ObjRef_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ObjectSet_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ObjectSet_O");
	Call_exposePython_core__ObjectSet_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Package_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Package_O");
	Call_exposePython_core__Package_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Path_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Path_O");
	Call_exposePython_core__Path_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Pathname_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Pathname_O");
	Call_exposePython_core__Pathname_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Pointer_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Pointer_O");
	Call_exposePython_core__Pointer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__PosixTimeDuration_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__PosixTimeDuration_O");
	Call_exposePython_core__PosixTimeDuration_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__PosixTime_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__PosixTime_O");
	Call_exposePython_core__PosixTime_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__ReadTable_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ReadTable_O");
	Call_exposePython_core__ReadTable_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Reader_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Reader_O");
	Call_exposePython_core__Reader_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__RegexMatch_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__RegexMatch_O");
	Call_exposePython_core__RegexMatch_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Regex_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Regex_O");
	Call_exposePython_core__Regex_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SNode_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SNode_O");
	Call_exposePython_core__SNode_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SingleDispatchMethod_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SingleDispatchMethod_O");
	Call_exposePython_core__SingleDispatchMethod_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SmallMap_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SmallMap_O");
	Call_exposePython_core__SmallMap_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SourceFileInfo_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SourceFileInfo_O");
	Call_exposePython_core__SourceFileInfo_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SourceManager_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SourceManager_O");
	Call_exposePython_core__SourceManager_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SourcePosInfo_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SourcePosInfo_O");
	Call_exposePython_core__SourcePosInfo_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__StandardObject_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StandardObject_O");
	Call_exposePython_core__StandardObject_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Stream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Stream_O");
	Call_exposePython_core__Stream_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StringList_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StringList_O");
	Call_exposePython_core__StringList_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StringSet_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StringSet_O");
	Call_exposePython_core__StringSet_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__StructureObject_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StructureObject_O");
	Call_exposePython_core__StructureObject_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SymbolList_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SymbolList_O");
	Call_exposePython_core__SymbolList_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SymbolSet_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SymbolSet_O");
	Call_exposePython_core__SymbolSet_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SymbolToEnumConverter_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SymbolToEnumConverter_O");
	Call_exposePython_core__SymbolToEnumConverter_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Symbol_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Symbol_O");
	Call_exposePython_core__Symbol_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__WeakHashTable_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__WeakHashTable_O");
	Call_exposePython_core__WeakHashTable_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__WeakKeyMapping_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__WeakKeyMapping_O");
	Call_exposePython_core__WeakKeyMapping_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__WeakPointer_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__WeakPointer_O");
	Call_exposePython_core__WeakPointer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__WrappedPointer_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__WrappedPointer_O");
	Call_exposePython_core__WrappedPointer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ActivationFrame_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ActivationFrame_O");
	Call_exposePython_core__ActivationFrame_O(_lisp); // base(s): set(['core::Environment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ExtPkg
extern void Call_exposePython_core__AnsiStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__AnsiStream_O");
	Call_exposePython_core__AnsiStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_ExtPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ArrayDisplaced_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ArrayDisplaced_O");
	Call_exposePython_core__ArrayDisplaced_O(_lisp); // base(s): set(['core::Array_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ArrayObjects_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ArrayObjects_O");
	Call_exposePython_core__ArrayObjects_O(_lisp); // base(s): set(['core::Array_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__BranchSNode_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__BranchSNode_O");
	Call_exposePython_core__BranchSNode_O(_lisp); // base(s): set(['core::SNode_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__CompiledFunction_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__CompiledFunction_O");
	Call_exposePython_core__CompiledFunction_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Complex_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Complex_O");
	Call_exposePython_core__Complex_O(_lisp); // base(s): set(['core::Number_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__DirectoryIterator_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__DirectoryIterator_O");
	Call_exposePython_core__DirectoryIterator_O(_lisp); // base(s): set(['core::Iterator_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ForeignData_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ForeignData_O");
	Call_exposePython_core__ForeignData_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__GlueEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__GlueEnvironment_O");
	Call_exposePython_core__GlueEnvironment_O(_lisp); // base(s): set(['core::Environment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__HashTableEq_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__HashTableEq_O");
	Call_exposePython_core__HashTableEq_O(_lisp); // base(s): set(['core::HashTable_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__HashTableEql_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__HashTableEql_O");
	Call_exposePython_core__HashTableEql_O(_lisp); // base(s): set(['core::HashTable_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__HashTableEqual_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__HashTableEqual_O");
	Call_exposePython_core__HashTableEqual_O(_lisp); // base(s): set(['core::HashTable_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__HashTableEqualp_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__HashTableEqualp_O");
	Call_exposePython_core__HashTableEqualp_O(_lisp); // base(s): set(['core::HashTable_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Instance_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Instance_O");
	Call_exposePython_core__Instance_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__LeafSNode_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__LeafSNode_O");
	Call_exposePython_core__LeafSNode_O(_lisp); // base(s): set(['core::SNode_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__LexicalEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__LexicalEnvironment_O");
	Call_exposePython_core__LexicalEnvironment_O(_lisp); // base(s): set(['core::Environment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__LoadArchive_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__LoadArchive_O");
	Call_exposePython_core__LoadArchive_O(_lisp); // base(s): set(['core::Archive_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__LogicalPathname_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__LogicalPathname_O");
	Call_exposePython_core__LogicalPathname_O(_lisp); // base(s): set(['core::Pathname_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Metaobject_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Metaobject_O");
	Call_exposePython_core__Metaobject_O(_lisp); // base(s): set(['core::StandardObject_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Null_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Null_O");
	Call_exposePython_core__Null_O(_lisp); // base(s): set(['core::Symbol_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Real_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Real_O");
	Call_exposePython_core__Real_O(_lisp); // base(s): set(['core::Number_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__RecursiveDirectoryIterator_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__RecursiveDirectoryIterator_O");
	Call_exposePython_core__RecursiveDirectoryIterator_O(_lisp); // base(s): set(['core::Iterator_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SaveArchive_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SaveArchive_O");
	Call_exposePython_core__SaveArchive_O(_lisp); // base(s): set(['core::Archive_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SingleDispatchEffectiveMethodFunction_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SingleDispatchEffectiveMethodFunction_O");
	Call_exposePython_core__SingleDispatchEffectiveMethodFunction_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SingleDispatchGenericFunction_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SingleDispatchGenericFunction_O");
	Call_exposePython_core__SingleDispatchGenericFunction_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SpecialForm_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SpecialForm_O");
	Call_exposePython_core__SpecialForm_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__UserData_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__UserData_O");
	Call_exposePython_core__UserData_O(_lisp); // base(s): set(['core::LightUserData_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Vector_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Vector_O");
	Call_exposePython_core__Vector_O(_lisp); // base(s): set(['core::Array_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__WeakKeyHashTable_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__WeakKeyHashTable_O");
	Call_exposePython_core__WeakKeyHashTable_O(_lisp); // base(s): set(['core::WeakHashTable_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__BitVector_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__BitVector_O");
	Call_exposePython_core__BitVector_O(_lisp); // base(s): set(['core::Vector_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__BroadcastStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__BroadcastStream_O");
	Call_exposePython_core__BroadcastStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__CompileTimeEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__CompileTimeEnvironment_O");
	Call_exposePython_core__CompileTimeEnvironment_O(_lisp); // base(s): set(['core::LexicalEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__ConcatenatedStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ConcatenatedStream_O");
	Call_exposePython_core__ConcatenatedStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__EchoStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__EchoStream_O");
	Call_exposePython_core__EchoStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__FileStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FileStream_O");
	Call_exposePython_core__FileStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Float_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Float_O");
	Call_exposePython_core__Float_O(_lisp); // base(s): set(['core::Real_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FunctionFrame_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FunctionFrame_O");
	Call_exposePython_core__FunctionFrame_O(_lisp); // base(s): set(['core::ActivationFrame_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Rational_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Rational_O");
	Call_exposePython_core__Rational_O(_lisp); // base(s): set(['core::Real_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__RuntimeVisibleEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__RuntimeVisibleEnvironment_O");
	Call_exposePython_core__RuntimeVisibleEnvironment_O(_lisp); // base(s): set(['core::LexicalEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SexpLoadArchive_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SexpLoadArchive_O");
	Call_exposePython_core__SexpLoadArchive_O(_lisp); // base(s): set(['core::LoadArchive_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SexpSaveArchive_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SexpSaveArchive_O");
	Call_exposePython_core__SexpSaveArchive_O(_lisp); // base(s): set(['core::SaveArchive_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Specializer_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Specializer_O");
	Call_exposePython_core__Specializer_O(_lisp); // base(s): set(['core::Metaobject_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__StringStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StringStream_O");
	Call_exposePython_core__StringStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__String_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__String_O");
	Call_exposePython_core__String_O(_lisp); // base(s): set(['core::Vector_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__SynonymStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SynonymStream_O");
	Call_exposePython_core__SynonymStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__TagbodyFrame_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__TagbodyFrame_O");
	Call_exposePython_core__TagbodyFrame_O(_lisp); // base(s): set(['core::ActivationFrame_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__TwoWayStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__TwoWayStream_O");
	Call_exposePython_core__TwoWayStream_O(_lisp); // base(s): set(['core::AnsiStream_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ValueFrame_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ValueFrame_O");
	Call_exposePython_core__ValueFrame_O(_lisp); // base(s): set(['core::ActivationFrame_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__VectorObjects_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__VectorObjects_O");
	Call_exposePython_core__VectorObjects_O(_lisp); // base(s): set(['core::Vector_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__BlockEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__BlockEnvironment_O");
	Call_exposePython_core__BlockEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__CatchEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__CatchEnvironment_O");
	Call_exposePython_core__CatchEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Class_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Class_O");
	Call_exposePython_core__Class_O(_lisp); // base(s): set(['core::Specializer_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__DoubleFloat_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__DoubleFloat_O");
	Call_exposePython_core__DoubleFloat_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FunctionContainerEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FunctionContainerEnvironment_O");
	Call_exposePython_core__FunctionContainerEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FunctionValueEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FunctionValueEnvironment_O");
	Call_exposePython_core__FunctionValueEnvironment_O(_lisp); // base(s): set(['core::RuntimeVisibleEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__IOFileStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__IOFileStream_O");
	Call_exposePython_core__IOFileStream_O(_lisp); // base(s): set(['core::FileStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__IOStreamStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__IOStreamStream_O");
	Call_exposePython_core__IOStreamStream_O(_lisp); // base(s): set(['core::FileStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Integer_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Integer_O");
	Call_exposePython_core__Integer_O(_lisp); // base(s): set(['core::Rational_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__MacroletEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__MacroletEnvironment_O");
	Call_exposePython_core__MacroletEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Ratio_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Ratio_O");
	Call_exposePython_core__Ratio_O(_lisp); // base(s): set(['core::Rational_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__ShortFloat_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ShortFloat_O");
	Call_exposePython_core__ShortFloat_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__SimpleBitVector_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SimpleBitVector_O");
	Call_exposePython_core__SimpleBitVector_O(_lisp); // base(s): set(['core::BitVector_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__SingleFloat_dummy_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SingleFloat_dummy_O");
	Call_exposePython_core__SingleFloat_dummy_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StackValueEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StackValueEnvironment_O");
	Call_exposePython_core__StackValueEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Str_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Str_O");
	Call_exposePython_core__Str_O(_lisp); // base(s): set(['core::String_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StringInputStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StringInputStream_O");
	Call_exposePython_core__StringInputStream_O(_lisp); // base(s): set(['core::StringStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StringOutputStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StringOutputStream_O");
	Call_exposePython_core__StringOutputStream_O(_lisp); // base(s): set(['core::StringStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SymbolMacroletEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SymbolMacroletEnvironment_O");
	Call_exposePython_core__SymbolMacroletEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__TagbodyEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__TagbodyEnvironment_O");
	Call_exposePython_core__TagbodyEnvironment_O(_lisp); // base(s): set(['core::RuntimeVisibleEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__UnwindProtectEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__UnwindProtectEnvironment_O");
	Call_exposePython_core__UnwindProtectEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ValueEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ValueEnvironment_O");
	Call_exposePython_core__ValueEnvironment_O(_lisp); // base(s): set(['core::RuntimeVisibleEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__VectorObjectsWithFillPtr_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__VectorObjectsWithFillPtr_O");
	Call_exposePython_core__VectorObjectsWithFillPtr_O(_lisp); // base(s): set(['core::VectorObjects_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Bignum_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Bignum_O");
	Call_exposePython_core__Bignum_O(_lisp); // base(s): set(['core::Integer_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__BuiltInClass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__BuiltInClass_O");
	Call_exposePython_core__BuiltInClass_O(_lisp); // base(s): set(['core::Class_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__Fixnum_dummy_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Fixnum_dummy_O");
	Call_exposePython_core__Fixnum_dummy_O(_lisp); // base(s): set(['core::Integer_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ForwardReferencedClass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ForwardReferencedClass_O");
	Call_exposePython_core__ForwardReferencedClass_O(_lisp); // base(s): set(['core::Class_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClosPkg
extern void Call_exposePython_core__StdClass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StdClass_O");
	Call_exposePython_core__StdClass_O(_lisp); // base(s): set(['core::Class_O'])
}
#endif // ifdef Use_ClosPkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StrWithFillPtr_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StrWithFillPtr_O");
	Call_exposePython_core__StrWithFillPtr_O(_lisp); // base(s): set(['core::Str_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__StructureClass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StructureClass_O");
	Call_exposePython_core__StructureClass_O(_lisp); // base(s): set(['core::Class_O'])
}
#endif // ifdef Use_ClPkg
#ifdef Use_ClosPkg
extern void Call_exposePython_core__FuncallableStandardClass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FuncallableStandardClass_O");
	Call_exposePython_core__FuncallableStandardClass_O(_lisp); // base(s): set(['core::StdClass_O'])
}
#endif // ifdef Use_ClosPkg
#ifdef Use_ClPkg
extern void Call_exposePython_core__StandardClass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StandardClass_O");
	Call_exposePython_core__StandardClass_O(_lisp); // base(s): set(['core::StdClass_O'])
}
#endif // ifdef Use_ClPkg
#endif // EXPOSE_TO_PYTHON
#undef EXPOSE_TO_PYTHON
#if defined(EXPAND_CLASS_MACROS)
_CLASS_MACRO(core::T_O)
_CLASS_MACRO(core::Archive_O)
_CLASS_MACRO(core::Array_O)
_CLASS_MACRO(core::Binder_O)
_CLASS_MACRO(core::CandoException_O)
_CLASS_MACRO(core::Character_dummy_O)
_CLASS_MACRO(core::Cons_O)
_CLASS_MACRO(core::DirectoryEntry_O)
_CLASS_MACRO(core::Environment_O)
_CLASS_MACRO(core::ExternalObject_O)
_CLASS_MACRO(core::FileStatus_O)
_CLASS_MACRO(core::Function_O)
_CLASS_MACRO(core::HashTable_O)
_CLASS_MACRO(core::IntArray_O)
_CLASS_MACRO(core::Iterator_O)
_CLASS_MACRO(core::LambdaListHandler_O)
_CLASS_MACRO(core::LightUserData_O)
_CLASS_MACRO(core::LoadTimeValues_O)
_CLASS_MACRO(core::MicroHeap_O)
_CLASS_MACRO(core::MultiStringBuffer_O)
_CLASS_MACRO(core::Number_O)
_CLASS_MACRO(core::ObjRef_O)
_CLASS_MACRO(core::ObjectSet_O)
_CLASS_MACRO(core::Package_O)
_CLASS_MACRO(core::Path_O)
_CLASS_MACRO(core::Pathname_O)
_CLASS_MACRO(core::Pointer_O)
_CLASS_MACRO(core::PosixTimeDuration_O)
_CLASS_MACRO(core::PosixTime_O)
_CLASS_MACRO(core::ReadTable_O)
_CLASS_MACRO(core::Reader_O)
_CLASS_MACRO(core::RegexMatch_O)
_CLASS_MACRO(core::Regex_O)
_CLASS_MACRO(core::SNode_O)
_CLASS_MACRO(core::SingleDispatchMethod_O)
_CLASS_MACRO(core::SmallMap_O)
_CLASS_MACRO(core::SourceFileInfo_O)
_CLASS_MACRO(core::SourceManager_O)
_CLASS_MACRO(core::SourcePosInfo_O)
_CLASS_MACRO(core::StandardObject_O)
_CLASS_MACRO(core::Stream_O)
_CLASS_MACRO(core::StringList_O)
_CLASS_MACRO(core::StringSet_O)
_CLASS_MACRO(core::StructureObject_O)
_CLASS_MACRO(core::SymbolList_O)
_CLASS_MACRO(core::SymbolSet_O)
_CLASS_MACRO(core::SymbolToEnumConverter_O)
_CLASS_MACRO(core::Symbol_O)
_CLASS_MACRO(core::WeakHashTable_O)
_CLASS_MACRO(core::WeakKeyMapping_O)
_CLASS_MACRO(core::WeakPointer_O)
_CLASS_MACRO(core::WrappedPointer_O)
_CLASS_MACRO(core::ActivationFrame_O)
_CLASS_MACRO(core::AnsiStream_O)
_CLASS_MACRO(core::ArrayDisplaced_O)
_CLASS_MACRO(core::ArrayObjects_O)
_CLASS_MACRO(core::BranchSNode_O)
_CLASS_MACRO(core::CompiledFunction_O)
_CLASS_MACRO(core::Complex_O)
_CLASS_MACRO(core::DirectoryIterator_O)
_CLASS_MACRO(core::ForeignData_O)
_CLASS_MACRO(core::GlueEnvironment_O)
_CLASS_MACRO(core::HashTableEq_O)
_CLASS_MACRO(core::HashTableEql_O)
_CLASS_MACRO(core::HashTableEqual_O)
_CLASS_MACRO(core::HashTableEqualp_O)
_CLASS_MACRO(core::Instance_O)
_CLASS_MACRO(core::LeafSNode_O)
_CLASS_MACRO(core::LexicalEnvironment_O)
_CLASS_MACRO(core::LoadArchive_O)
_CLASS_MACRO(core::LogicalPathname_O)
_CLASS_MACRO(core::Metaobject_O)
_CLASS_MACRO(core::Null_O)
_CLASS_MACRO(core::Real_O)
_CLASS_MACRO(core::RecursiveDirectoryIterator_O)
_CLASS_MACRO(core::SaveArchive_O)
_CLASS_MACRO(core::SingleDispatchEffectiveMethodFunction_O)
_CLASS_MACRO(core::SingleDispatchGenericFunction_O)
_CLASS_MACRO(core::SpecialForm_O)
_CLASS_MACRO(core::UserData_O)
_CLASS_MACRO(core::Vector_O)
_CLASS_MACRO(core::WeakKeyHashTable_O)
_CLASS_MACRO(core::BitVector_O)
_CLASS_MACRO(core::BroadcastStream_O)
_CLASS_MACRO(core::CompileTimeEnvironment_O)
_CLASS_MACRO(core::ConcatenatedStream_O)
_CLASS_MACRO(core::EchoStream_O)
_CLASS_MACRO(core::FileStream_O)
_CLASS_MACRO(core::Float_O)
_CLASS_MACRO(core::FunctionFrame_O)
_CLASS_MACRO(core::Rational_O)
_CLASS_MACRO(core::RuntimeVisibleEnvironment_O)
_CLASS_MACRO(core::SexpLoadArchive_O)
_CLASS_MACRO(core::SexpSaveArchive_O)
_CLASS_MACRO(core::Specializer_O)
_CLASS_MACRO(core::StringStream_O)
_CLASS_MACRO(core::String_O)
_CLASS_MACRO(core::SynonymStream_O)
_CLASS_MACRO(core::TagbodyFrame_O)
_CLASS_MACRO(core::TwoWayStream_O)
_CLASS_MACRO(core::ValueFrame_O)
_CLASS_MACRO(core::VectorObjects_O)
_CLASS_MACRO(core::BlockEnvironment_O)
_CLASS_MACRO(core::CatchEnvironment_O)
_CLASS_MACRO(core::Class_O)
_CLASS_MACRO(core::DoubleFloat_O)
_CLASS_MACRO(core::FunctionContainerEnvironment_O)
_CLASS_MACRO(core::FunctionValueEnvironment_O)
_CLASS_MACRO(core::IOFileStream_O)
_CLASS_MACRO(core::IOStreamStream_O)
_CLASS_MACRO(core::Integer_O)
_CLASS_MACRO(core::MacroletEnvironment_O)
_CLASS_MACRO(core::Ratio_O)
_CLASS_MACRO(core::ShortFloat_O)
_CLASS_MACRO(core::SimpleBitVector_O)
_CLASS_MACRO(core::SingleFloat_dummy_O)
_CLASS_MACRO(core::StackValueEnvironment_O)
_CLASS_MACRO(core::Str_O)
_CLASS_MACRO(core::StringInputStream_O)
_CLASS_MACRO(core::StringOutputStream_O)
_CLASS_MACRO(core::SymbolMacroletEnvironment_O)
_CLASS_MACRO(core::TagbodyEnvironment_O)
_CLASS_MACRO(core::UnwindProtectEnvironment_O)
_CLASS_MACRO(core::ValueEnvironment_O)
_CLASS_MACRO(core::VectorObjectsWithFillPtr_O)
_CLASS_MACRO(core::Bignum_O)
_CLASS_MACRO(core::BuiltInClass_O)
_CLASS_MACRO(core::Fixnum_dummy_O)
_CLASS_MACRO(core::ForwardReferencedClass_O)
_CLASS_MACRO(core::StdClass_O)
_CLASS_MACRO(core::StrWithFillPtr_O)
_CLASS_MACRO(core::StructureClass_O)
_CLASS_MACRO(core::FuncallableStandardClass_O)
_CLASS_MACRO(core::StandardClass_O)
#endif // EXPAND_CLASS_MACROS
#undef ALL_STAGES
// ---------------- after class initializers
#ifdef EXPOSE_TO_PYTHON
#endif // EXPOSE_TO_PYTHON
