// start
// define cpp macros: SET_SYMBOL, CREATE_CLASS, SET_CLASS, DEFINE_BASE_CLASSES, DEFINE_CLASS_NAMES, EXPOSE_TO_CANDO 
// define cpp macro: ALL_STAGES to get the effect of defining all of the macros above
// define cpp macro: EXPOSE_PYTHON to expose python
 // class ActivationFrame_O : public Environment_O
 // class ValueFrame_O : public ActivationFrame_O
 // class FunctionFrame_O : public ActivationFrame_O
 // class Archive_O : public T_O
 // class LoadArchive_O : public Archive_O
 // class SaveArchive_O : public Archive_O
 // class ArchiveMemoryManager_O : public T_O
 // class ArchiveError_O : public CandoException_O
 // class Array_O : public ArrayVirtual_O
 // class ArrayObjects_O : public Array_O
 // class ArrayVirtual_O : virtual public T_O
 // class Bignum_O : public Integer_O
 // class FormBinder_O : public T_O
 // class Binder_O : public T_O
 // class BitVector_O : public Vector_O
 // class SimpleBitVector_O : public BitVector_O
 // class BuiltInClass_O : public MetaClass_O
 // class CandoFormatSaveArchive_O : public SaveArchive_O
 // class Character_O : public T_O
 // class BaseChar_O : public Character_O
 // class StandardChar_O : public BaseChar_O
 // class ExtendedChar_O : public Character_O
 // class ConditionHandlerBase_O : public T_O
 // class HandlerCaseHandler_O : public ConditionHandlerBase_O
 // class HandlerBindHandler_O : public ConditionHandlerBase_O
 // class RestartCase_O : public T_O
 // class ConditionWithStackTrace_O : public ConditionBase_O
 // class CandoException_O : public ConditionWithStackTrace_O
 // class MathException_O : public CandoException_O
 // class TypeException_O : public CandoException_O
 // class FileNotFoundException_O : public CandoException_O
 // class LispError_O : public CandoException_O
 // class ContentException_O : public CandoException_O
 // class Condition_O : public ConditionWithStackTrace_O
 // class serious_condition_O : virtual public Condition_O
 // class Error_O : public serious_condition_O
 // class package_error_O : public Error_O
 // class file_error_O : public Error_O
 // class stream_error_O : virtual public Error_O
 // class end_of_file_O : public stream_error_O
 // class parse_error_O : virtual public Error_O
 //class reader_error_O : public stream_error_O, public parse_error_O
 // class warning_O : virtual public Condition_O
 // class style_warning_O : public warning_O
 // class storage_condition_O : public serious_condition_O
 // class simple_condition_O : virtual public Condition_O
 // class type_error_O : public Error_O
 //class simple_error_O : public Error_O, public simple_condition_O
 // class simple_type_error_O : virtual public type_error_O, virtual public simple_condition_O
 //class simple_warning_O : public simple_condition_O, public warning_O
 // class program_error_O : public Error_O
 // class argument_mismatch_error_O : public program_error_O
 // class too_few_arguments_error_O : public argument_mismatch_error_O
 // class too_many_arguments_error_O : public argument_mismatch_error_O
 // class odd_number_of_keyword_arguments_error_O : public argument_mismatch_error_O
 // class unrecognized_keyword_arguments_error_O : public argument_mismatch_error_O
 // class invalid_keyword_arguments_error_O : public argument_mismatch_error_O
 // class single_dispatch_arguments_error_O : public argument_mismatch_error_O
 // class control_error_O : public Error_O
 // class print_not_readable_O : public Error_O
 // class cell_error_O : public Error_O
 // class undefined_function_O : public cell_error_O
 // class unbound_variable_O : public cell_error_O
 // class unbound_slot_O : public cell_error_O
 // class arithmetic_error_O : public Error_O
 // class division_by_zero_O : public arithmetic_error_O
 // class floating_point_inexact_O : public arithmetic_error_O
 // class floating_point_overflow_O : public arithmetic_error_O
 // class floating_point_underflow_O : public arithmetic_error_O
 // class floating_point_invalid_operation_O : public arithmetic_error_O
 // class Cons_O : public List_O
 // class SourceCodeCons_O : public Cons_O
 // class CompiledBody_O : public Cons_O
// Associating namespace(core) with package(CorePkg)
 // class Environment_O : public T_O
 // class LexicalEnvironment_O : public Environment_O
 // class RuntimeVisibleEnvironment_O : public LexicalEnvironment_O
 // class ValueEnvironment_O : public RuntimeVisibleEnvironment_O
 // class FunctionValueEnvironment_O : public RuntimeVisibleEnvironment_O
 // class CompileTimeEnvironment_O : public LexicalEnvironment_O
 // class UnwindProtectEnvironment_O : public CompileTimeEnvironment_O
 // class BlockEnvironment_O : public CompileTimeEnvironment_O
 // class CatchEnvironment_O : public CompileTimeEnvironment_O
 // class FunctionContainerEnvironment_O : public CompileTimeEnvironment_O
 // class TagbodyEnvironment_O : public CompileTimeEnvironment_O
 // class MacroletEnvironment_O : public CompileTimeEnvironment_O
 // class SymbolMacroletEnvironment_O : public CompileTimeEnvironment_O
 // class GlueEnvironment_O : public Environment_O
 // class Function_O : public T_O
 // class Interpreted_O : public Function_O
 // class BuiltIn_O : public Function_O
 // class Closure_O : public Function_O
 // class ExternalObjectManager_O : public T_O
 // class ExternalObject_O : public T_O // StandardObject_O
 // class Path_O : public T_O
 // class DirectoryIterator_O : public Iterator_O
 // class RecursiveDirectoryIterator_O : public Iterator_O
 // class DirectoryEntry_O : public T_O
 // class FileStatus_O : public T_O
 // class ForwardReferencedClass_O : public MetaClass_O
 // class GenericFunction_O : public Function_O
 // class HashTable_O : public T_O
 // class HashTableEq_O : public HashTable_O
 // class HashTableEql_O : public HashTable_O
 // class HashTableEqual_O : public HashTable_O
 // class HashTableEqualp_O : public HashTable_O
 // class Instance_O : public T_O
 // class IntArray_O : public T_O
 // class IntegerKeyObjectDictionary_O : public T_O
 // class Iterator_O : public T_O
 // class LambdaListHandler_O : public T_O
 // class Lisp_O : public T_O
 // class List_O : public Sequence_O
 // class Stream_O : public T_O
 // class FileStream_O : public Stream_O
 // class FileInStream_O : public FileStream_O
 // class FileOutStream_O : public FileStream_O
 // class FileInCompressedStream_O : public FileStream_O
 // class FileOutCompressedStream_O : public FileStream_O
 // class StringStream_O : public Stream_O
 // class StringInStream_O : public StringStream_O
 // class StringOutStream_O : public StringStream_O
 // class FDStream_O : public Stream_O
 // class FDInStream_O : public FDStream_O
 // class FDOutStream_O : public FDStream_O
 // class SynonymStream_O : public Stream_O
 // class TwoWayStream_O : public Stream_O
 // class BroadcastStream_O : public Stream_O
 // class ConcatenatedStream_O : public Stream_O
 // class EchoStream_O : public Stream_O
 // class String_O : public Vector_O
 //class Vector_O : public Array_O, public Sequence_O
 // class LoadTimeValues_O : public VectorObjectsWithFillPtr_O
 // class MetaClass_O : public StandardObject_O
 // class MetaObject_O : public StandardObject_O
 // class Method_O : public T_O
 // class MethodCombination_O : public T_O
 // class MethodSet_O : public T_O
 // class MicroHeap_O : public T_O
 // class Model_O : public T_O
 // class MultiStringBuffer_O : public T_O
 // class MultipleValues_O : public T_O
 // class Stream_O : public T_O
 // class FileStream_O : public Stream_O
 // class StringStream_O : public Stream_O
 //class Null_O : public Symbol_O, public List_O
 // class Number_O : public T_O
 // class Real_O : public Number_O
 // class Rational_O : public Real_O
 // class Integer_O : public Rational_O
 // class Fixnum_O : public Integer_O
 // class Float_O : public Real_O
 // class ShortFloat_O : public Float_O
 // class SingleFloat_O : public Float_O
 // class DoubleFloat_O : public Float_O
 // class LongFloat_O : public Float_O
 // class Complex_O : public Number_O
 // class Ratio_O : public Rational_O
 // class Bool_O : public Number_O
 // class ObjRef_O : public T_O
 // class T_O : public _RootDummyClass
 // class ConditionBase_O : public T_O
 // class ObjectSet_O : public T_O
 // class Package_O : public T_O
 // class Package_O : public T_O
 // class Package_O : public T_O
 // class Pointer_O : public T_O
 // class PosixTime_O : public T_O
 // class PosixTimeDuration_O : public T_O
 // class QDomNode_O : public T_O
 // class Reader_O : public T_O
 // class ReadTable_O : public T_O
 // class Sequence_O : public SequenceVirtual_O
 // class SequenceVirtual_O : virtual public T_O
 // class SerializeError_O : public CandoException_O
 // class Serializer_O : public T_O
 // class WriteSerializer_O : public Serializer_O
 // class ReadSerializer_O : public Serializer_O
 // class SetfExpander_O : public T_O
 // class SlotValueSetfExpander_O : public SetfExpander_O
 // class FunctionNameSetfExpander_O : public SetfExpander_O
 // class FunctionSetfExpander_O : public SetfExpander_O
 // class SingleDispatchEffectiveMethodFunction_O : public Function_O
 // class SingleDispatchGenericFunction_O : public BuiltIn_O
 // class SingleDispatchMethod_O : public T_O
 // class SourceFileInfo_O : public T_O
 // class SpecialForm_O : public Function_O
 // class StandardClass_O : public MetaClass_O
 // class StandardGenericFunction_O : public GenericFunction_O
 // class StandardObject_O : public T_O
 // class Str_O : public String_O
 // class StringList_O : public T_O
 // class StringSet_O : public T_O
 // class StructureClass_O : public MetaClass_O
 // class StructureObject_O : public Model_O
 // class Symbol_O : virtual public T_O
 // class SymbolList_O : public T_O
 // class SymbolSet_O : public T_O
 // class SymbolToEnumConverter_O : public T_O
 // class SymbolVector_O : public T_O
 // class SymbolVectorMap_O : public T_O
 // class VectorObjects_O : public Vector_O
 // class VectorObjectsWithFillPtr_O : public VectorObjects_O
 // class WeakReference_O : public T_O
 // class XmlLoadArchive_O : public LoadArchive_O
 // class XmlSaveArchive_O : public SaveArchive_O
#ifdef HEADER_INCLUDES
#include "object.h"
#include "archiveMemoryManager.h"
#include "archive.h"
#include "arrayVirtual.h"
#include "binder.h"
#include "character.h"
#include "conditionHandlers.h"
#include "fileSystem.h"
#include "environment.h"
#include "externalObject.h"
#include "executables.h"
#include "hashTable.h"
#include "instance.h"
#include "intArray.h"
#include "integerKeyObjectDictionary.h"
#include "iterator.h"
#include "lambdaListHandler.h"
#include "lisp.h"
#include "methodCombination.h"
#include "methodSet.h"
#include "method.h"
#include "microHeap.h"
#include "model.h"
#include "multiStringBuffer.h"
#include "multipleValues.h"
#include "numbers.h"
#include "objRef.h"
#include "objectSet.h"
#include "package.h"
#include "pointer.h"
#include "posixTime.h"
#include "quickDom.h"
#include "readtable.h"
#include "reader.h"
#include "sequenceVirtual.h"
#include "serializer.h"
#include "setfExpander.h"
#include "singleDispatchMethod.h"
#include "sourceFileInfo.h"
#include "standardObject.h"
#include "lispStream.h"
#include "stringList.h"
#include "stringSet.h"
#include "symbolList.h"
#include "symbolSet.h"
#include "symbolToEnumConverter.h"
#include "symbolVector.h"
#include "symbol.h"
#include "weakReference.h"
#include "activationFrame.h"
#include "array.h"
#include "conditions.h"
#include "genericFunction.h"
#include "hashTableEq.h"
#include "hashTableEql.h"
#include "hashTableEqual.h"
#include "hashTableEqualp.h"
#include "metaClass.h"
#include "metaobject.h"
#include "sequence.h"
#include "singleDispatchEffectiveMethodFunction.h"
#include "specialForm.h"
#include "structureObject.h"
#include "arrayObjects.h"
#include "builtInClass.h"
#include "candoFormatSaveArchive.h"
#include "forwardReferencedClass.h"
#include "lispList.h"
#include "singleDispatchGenericFunction.h"
#include "standardClass.h"
#include "standardGenericFunction.h"
#include "structureClass.h"
#include "lispVector.h"
#include "xmlLoadArchive.h"
#include "xmlSaveArchive.h"
#include "archiveNode.h"
#include "bitVector.h"
#include "cons.h"
#include "null.h"
#include "lispString.h"
#include "vectorObjects.h"
#include "bignum.h"
#include "str.h"
#include "vectorObjectsWithFillPtr.h"
#include "loadTimeValues.h"
#endif // HEADER_INCLUDES
#undef HEADER_INCLUDES
#if defined(SET_SYMBOL) || defined(ALL_STAGES)
// requires LOOKUP_SYMBOL(pkg,symbolName) be defined
core::T_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::T_O::static_packageName(),core::T_O::static_className()));
core::ArchiveMemoryManager_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ArchiveMemoryManager_O::static_packageName(),core::ArchiveMemoryManager_O::static_className()));
core::Archive_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Archive_O::static_packageName(),core::Archive_O::static_className()));
core::ArrayVirtual_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ArrayVirtual_O::static_packageName(),core::ArrayVirtual_O::static_className()));
core::Binder_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Binder_O::static_packageName(),core::Binder_O::static_className()));
core::Character_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Character_O::static_packageName(),core::Character_O::static_className()));
core::ConditionBase_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ConditionBase_O::static_packageName(),core::ConditionBase_O::static_className()));
core::ConditionHandlerBase_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ConditionHandlerBase_O::static_packageName(),core::ConditionHandlerBase_O::static_className()));
core::DirectoryEntry_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::DirectoryEntry_O::static_packageName(),core::DirectoryEntry_O::static_className()));
core::Environment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Environment_O::static_packageName(),core::Environment_O::static_className()));
core::ExternalObjectManager_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ExternalObjectManager_O::static_packageName(),core::ExternalObjectManager_O::static_className()));
core::ExternalObject_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ExternalObject_O::static_packageName(),core::ExternalObject_O::static_className()));
core::FileStatus_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FileStatus_O::static_packageName(),core::FileStatus_O::static_className()));
core::FormBinder_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FormBinder_O::static_packageName(),core::FormBinder_O::static_className()));
core::Function_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Function_O::static_packageName(),core::Function_O::static_className()));
core::HashTable_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::HashTable_O::static_packageName(),core::HashTable_O::static_className()));
core::Instance_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Instance_O::static_packageName(),core::Instance_O::static_className()));
core::IntArray_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::IntArray_O::static_packageName(),core::IntArray_O::static_className()));
core::IntegerKeyObjectDictionary_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::IntegerKeyObjectDictionary_O::static_packageName(),core::IntegerKeyObjectDictionary_O::static_className()));
core::Iterator_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Iterator_O::static_packageName(),core::Iterator_O::static_className()));
core::LambdaListHandler_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::LambdaListHandler_O::static_packageName(),core::LambdaListHandler_O::static_className()));
core::Lisp_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Lisp_O::static_packageName(),core::Lisp_O::static_className()));
core::MethodCombination_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::MethodCombination_O::static_packageName(),core::MethodCombination_O::static_className()));
core::MethodSet_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::MethodSet_O::static_packageName(),core::MethodSet_O::static_className()));
core::Method_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Method_O::static_packageName(),core::Method_O::static_className()));
core::MicroHeap_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::MicroHeap_O::static_packageName(),core::MicroHeap_O::static_className()));
core::Model_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Model_O::static_packageName(),core::Model_O::static_className()));
core::MultiStringBuffer_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::MultiStringBuffer_O::static_packageName(),core::MultiStringBuffer_O::static_className()));
core::MultipleValues_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::MultipleValues_O::static_packageName(),core::MultipleValues_O::static_className()));
core::Number_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Number_O::static_packageName(),core::Number_O::static_className()));
core::ObjRef_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ObjRef_O::static_packageName(),core::ObjRef_O::static_className()));
core::ObjectSet_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ObjectSet_O::static_packageName(),core::ObjectSet_O::static_className()));
core::Package_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Package_O::static_packageName(),core::Package_O::static_className()));
core::Path_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Path_O::static_packageName(),core::Path_O::static_className()));
core::Pointer_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Pointer_O::static_packageName(),core::Pointer_O::static_className()));
core::PosixTimeDuration_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::PosixTimeDuration_O::static_packageName(),core::PosixTimeDuration_O::static_className()));
core::PosixTime_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::PosixTime_O::static_packageName(),core::PosixTime_O::static_className()));
core::QDomNode_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::QDomNode_O::static_packageName(),core::QDomNode_O::static_className()));
core::ReadTable_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ReadTable_O::static_packageName(),core::ReadTable_O::static_className()));
core::Reader_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Reader_O::static_packageName(),core::Reader_O::static_className()));
core::RestartCase_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::RestartCase_O::static_packageName(),core::RestartCase_O::static_className()));
core::SequenceVirtual_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SequenceVirtual_O::static_packageName(),core::SequenceVirtual_O::static_className()));
core::Serializer_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Serializer_O::static_packageName(),core::Serializer_O::static_className()));
core::SetfExpander_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SetfExpander_O::static_packageName(),core::SetfExpander_O::static_className()));
core::SingleDispatchMethod_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SingleDispatchMethod_O::static_packageName(),core::SingleDispatchMethod_O::static_className()));
core::SourceFileInfo_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SourceFileInfo_O::static_packageName(),core::SourceFileInfo_O::static_className()));
core::StandardObject_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StandardObject_O::static_packageName(),core::StandardObject_O::static_className()));
core::Stream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Stream_O::static_packageName(),core::Stream_O::static_className()));
core::StringList_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StringList_O::static_packageName(),core::StringList_O::static_className()));
core::StringSet_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StringSet_O::static_packageName(),core::StringSet_O::static_className()));
core::SymbolList_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SymbolList_O::static_packageName(),core::SymbolList_O::static_className()));
core::SymbolSet_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SymbolSet_O::static_packageName(),core::SymbolSet_O::static_className()));
core::SymbolToEnumConverter_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SymbolToEnumConverter_O::static_packageName(),core::SymbolToEnumConverter_O::static_className()));
core::SymbolVectorMap_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SymbolVectorMap_O::static_packageName(),core::SymbolVectorMap_O::static_className()));
core::SymbolVector_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SymbolVector_O::static_packageName(),core::SymbolVector_O::static_className()));
core::Symbol_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Symbol_O::static_packageName(),core::Symbol_O::static_className()));
core::WeakReference_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::WeakReference_O::static_packageName(),core::WeakReference_O::static_className()));
core::ActivationFrame_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ActivationFrame_O::static_packageName(),core::ActivationFrame_O::static_className()));
core::Array_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Array_O::static_packageName(),core::Array_O::static_className()));
core::BaseChar_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::BaseChar_O::static_packageName(),core::BaseChar_O::static_className()));
core::Bool_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Bool_O::static_packageName(),core::Bool_O::static_className()));
core::BroadcastStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::BroadcastStream_O::static_packageName(),core::BroadcastStream_O::static_className()));
core::BuiltIn_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::BuiltIn_O::static_packageName(),core::BuiltIn_O::static_className()));
core::Closure_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Closure_O::static_packageName(),core::Closure_O::static_className()));
core::Complex_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Complex_O::static_packageName(),core::Complex_O::static_className()));
core::ConcatenatedStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ConcatenatedStream_O::static_packageName(),core::ConcatenatedStream_O::static_className()));
core::ConditionWithStackTrace_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ConditionWithStackTrace_O::static_packageName(),core::ConditionWithStackTrace_O::static_className()));
core::DirectoryIterator_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::DirectoryIterator_O::static_packageName(),core::DirectoryIterator_O::static_className()));
core::EchoStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::EchoStream_O::static_packageName(),core::EchoStream_O::static_className()));
core::ExtendedChar_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ExtendedChar_O::static_packageName(),core::ExtendedChar_O::static_className()));
core::FDStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FDStream_O::static_packageName(),core::FDStream_O::static_className()));
core::FileStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FileStream_O::static_packageName(),core::FileStream_O::static_className()));
core::FunctionNameSetfExpander_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FunctionNameSetfExpander_O::static_packageName(),core::FunctionNameSetfExpander_O::static_className()));
core::FunctionSetfExpander_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FunctionSetfExpander_O::static_packageName(),core::FunctionSetfExpander_O::static_className()));
core::GenericFunction_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::GenericFunction_O::static_packageName(),core::GenericFunction_O::static_className()));
core::GlueEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::GlueEnvironment_O::static_packageName(),core::GlueEnvironment_O::static_className()));
core::HandlerBindHandler_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::HandlerBindHandler_O::static_packageName(),core::HandlerBindHandler_O::static_className()));
core::HandlerCaseHandler_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::HandlerCaseHandler_O::static_packageName(),core::HandlerCaseHandler_O::static_className()));
core::HashTableEq_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::HashTableEq_O::static_packageName(),core::HashTableEq_O::static_className()));
core::HashTableEql_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::HashTableEql_O::static_packageName(),core::HashTableEql_O::static_className()));
core::HashTableEqual_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::HashTableEqual_O::static_packageName(),core::HashTableEqual_O::static_className()));
core::HashTableEqualp_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::HashTableEqualp_O::static_packageName(),core::HashTableEqualp_O::static_className()));
core::Interpreted_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Interpreted_O::static_packageName(),core::Interpreted_O::static_className()));
core::LexicalEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::LexicalEnvironment_O::static_packageName(),core::LexicalEnvironment_O::static_className()));
core::LoadArchive_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::LoadArchive_O::static_packageName(),core::LoadArchive_O::static_className()));
core::MetaClass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::MetaClass_O::static_packageName(),core::MetaClass_O::static_className()));
core::MetaObject_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::MetaObject_O::static_packageName(),core::MetaObject_O::static_className()));
core::ReadSerializer_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ReadSerializer_O::static_packageName(),core::ReadSerializer_O::static_className()));
core::Real_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Real_O::static_packageName(),core::Real_O::static_className()));
core::RecursiveDirectoryIterator_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::RecursiveDirectoryIterator_O::static_packageName(),core::RecursiveDirectoryIterator_O::static_className()));
core::SaveArchive_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SaveArchive_O::static_packageName(),core::SaveArchive_O::static_className()));
core::Sequence_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Sequence_O::static_packageName(),core::Sequence_O::static_className()));
core::SingleDispatchEffectiveMethodFunction_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SingleDispatchEffectiveMethodFunction_O::static_packageName(),core::SingleDispatchEffectiveMethodFunction_O::static_className()));
core::SlotValueSetfExpander_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SlotValueSetfExpander_O::static_packageName(),core::SlotValueSetfExpander_O::static_className()));
core::SpecialForm_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SpecialForm_O::static_packageName(),core::SpecialForm_O::static_className()));
core::StringStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StringStream_O::static_packageName(),core::StringStream_O::static_className()));
core::StructureObject_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StructureObject_O::static_packageName(),core::StructureObject_O::static_className()));
core::SynonymStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SynonymStream_O::static_packageName(),core::SynonymStream_O::static_className()));
core::TwoWayStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::TwoWayStream_O::static_packageName(),core::TwoWayStream_O::static_className()));
core::WriteSerializer_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::WriteSerializer_O::static_packageName(),core::WriteSerializer_O::static_className()));
core::ArrayObjects_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ArrayObjects_O::static_packageName(),core::ArrayObjects_O::static_className()));
core::BuiltInClass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::BuiltInClass_O::static_packageName(),core::BuiltInClass_O::static_className()));
core::CandoException_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::CandoException_O::static_packageName(),core::CandoException_O::static_className()));
core::CandoFormatSaveArchive_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::CandoFormatSaveArchive_O::static_packageName(),core::CandoFormatSaveArchive_O::static_className()));
core::CompileTimeEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::CompileTimeEnvironment_O::static_packageName(),core::CompileTimeEnvironment_O::static_className()));
core::Condition_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Condition_O::static_packageName(),core::Condition_O::static_className()));
core::FDInStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FDInStream_O::static_packageName(),core::FDInStream_O::static_className()));
core::FDOutStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FDOutStream_O::static_packageName(),core::FDOutStream_O::static_className()));
core::FileInCompressedStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FileInCompressedStream_O::static_packageName(),core::FileInCompressedStream_O::static_className()));
core::FileInStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FileInStream_O::static_packageName(),core::FileInStream_O::static_className()));
core::FileOutCompressedStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FileOutCompressedStream_O::static_packageName(),core::FileOutCompressedStream_O::static_className()));
core::FileOutStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FileOutStream_O::static_packageName(),core::FileOutStream_O::static_className()));
core::Float_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Float_O::static_packageName(),core::Float_O::static_className()));
core::ForwardReferencedClass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ForwardReferencedClass_O::static_packageName(),core::ForwardReferencedClass_O::static_className()));
core::FunctionFrame_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FunctionFrame_O::static_packageName(),core::FunctionFrame_O::static_className()));
core::List_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::List_O::static_packageName(),core::List_O::static_className()));
core::Rational_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Rational_O::static_packageName(),core::Rational_O::static_className()));
core::RuntimeVisibleEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::RuntimeVisibleEnvironment_O::static_packageName(),core::RuntimeVisibleEnvironment_O::static_className()));
core::SingleDispatchGenericFunction_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SingleDispatchGenericFunction_O::static_packageName(),core::SingleDispatchGenericFunction_O::static_className()));
core::StandardChar_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StandardChar_O::static_packageName(),core::StandardChar_O::static_className()));
core::StandardClass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StandardClass_O::static_packageName(),core::StandardClass_O::static_className()));
core::StandardGenericFunction_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StandardGenericFunction_O::static_packageName(),core::StandardGenericFunction_O::static_className()));
core::StringInStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StringInStream_O::static_packageName(),core::StringInStream_O::static_className()));
core::StringOutStream_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StringOutStream_O::static_packageName(),core::StringOutStream_O::static_className()));
core::StructureClass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::StructureClass_O::static_packageName(),core::StructureClass_O::static_className()));
core::ValueFrame_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ValueFrame_O::static_packageName(),core::ValueFrame_O::static_className()));
core::Vector_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Vector_O::static_packageName(),core::Vector_O::static_className()));
core::XmlLoadArchive_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::XmlLoadArchive_O::static_packageName(),core::XmlLoadArchive_O::static_className()));
core::XmlSaveArchive_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::XmlSaveArchive_O::static_packageName(),core::XmlSaveArchive_O::static_className()));
core::ArchiveError_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ArchiveError_O::static_packageName(),core::ArchiveError_O::static_className()));
core::BitVector_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::BitVector_O::static_packageName(),core::BitVector_O::static_className()));
core::BlockEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::BlockEnvironment_O::static_packageName(),core::BlockEnvironment_O::static_className()));
core::CatchEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::CatchEnvironment_O::static_packageName(),core::CatchEnvironment_O::static_className()));
core::Cons_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Cons_O::static_packageName(),core::Cons_O::static_className()));
core::ContentException_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ContentException_O::static_packageName(),core::ContentException_O::static_className()));
core::DoubleFloat_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::DoubleFloat_O::static_packageName(),core::DoubleFloat_O::static_className()));
core::FileNotFoundException_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FileNotFoundException_O::static_packageName(),core::FileNotFoundException_O::static_className()));
core::FunctionContainerEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FunctionContainerEnvironment_O::static_packageName(),core::FunctionContainerEnvironment_O::static_className()));
core::FunctionValueEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::FunctionValueEnvironment_O::static_packageName(),core::FunctionValueEnvironment_O::static_className()));
core::Integer_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Integer_O::static_packageName(),core::Integer_O::static_className()));
core::LispError_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::LispError_O::static_packageName(),core::LispError_O::static_className()));
core::LongFloat_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::LongFloat_O::static_packageName(),core::LongFloat_O::static_className()));
core::MacroletEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::MacroletEnvironment_O::static_packageName(),core::MacroletEnvironment_O::static_className()));
core::MathException_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::MathException_O::static_packageName(),core::MathException_O::static_className()));
core::Null_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Null_O::static_packageName(),core::Null_O::static_className()));
core::Ratio_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Ratio_O::static_packageName(),core::Ratio_O::static_className()));
core::SerializeError_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SerializeError_O::static_packageName(),core::SerializeError_O::static_className()));
core::ShortFloat_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ShortFloat_O::static_packageName(),core::ShortFloat_O::static_className()));
core::SingleFloat_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SingleFloat_O::static_packageName(),core::SingleFloat_O::static_className()));
core::String_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::String_O::static_packageName(),core::String_O::static_className()));
core::SymbolMacroletEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SymbolMacroletEnvironment_O::static_packageName(),core::SymbolMacroletEnvironment_O::static_className()));
core::TagbodyEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::TagbodyEnvironment_O::static_packageName(),core::TagbodyEnvironment_O::static_className()));
core::TypeException_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::TypeException_O::static_packageName(),core::TypeException_O::static_className()));
core::UnwindProtectEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::UnwindProtectEnvironment_O::static_packageName(),core::UnwindProtectEnvironment_O::static_className()));
core::ValueEnvironment_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::ValueEnvironment_O::static_packageName(),core::ValueEnvironment_O::static_className()));
core::VectorObjects_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::VectorObjects_O::static_packageName(),core::VectorObjects_O::static_className()));
core::serious_condition_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::serious_condition_O::static_packageName(),core::serious_condition_O::static_className()));
core::simple_condition_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::simple_condition_O::static_packageName(),core::simple_condition_O::static_className()));
core::warning_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::warning_O::static_packageName(),core::warning_O::static_className()));
core::Bignum_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Bignum_O::static_packageName(),core::Bignum_O::static_className()));
core::CompiledBody_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::CompiledBody_O::static_packageName(),core::CompiledBody_O::static_className()));
core::Error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Error_O::static_packageName(),core::Error_O::static_className()));
core::Fixnum_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Fixnum_O::static_packageName(),core::Fixnum_O::static_className()));
core::SimpleBitVector_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SimpleBitVector_O::static_packageName(),core::SimpleBitVector_O::static_className()));
core::SourceCodeCons_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::SourceCodeCons_O::static_packageName(),core::SourceCodeCons_O::static_className()));
core::Str_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::Str_O::static_packageName(),core::Str_O::static_className()));
core::VectorObjectsWithFillPtr_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::VectorObjectsWithFillPtr_O::static_packageName(),core::VectorObjectsWithFillPtr_O::static_className()));
core::simple_warning_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::simple_warning_O::static_packageName(),core::simple_warning_O::static_className()));
core::storage_condition_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::storage_condition_O::static_packageName(),core::storage_condition_O::static_className()));
core::style_warning_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::style_warning_O::static_packageName(),core::style_warning_O::static_className()));
core::LoadTimeValues_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::LoadTimeValues_O::static_packageName(),core::LoadTimeValues_O::static_className()));
core::arithmetic_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::arithmetic_error_O::static_packageName(),core::arithmetic_error_O::static_className()));
core::cell_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::cell_error_O::static_packageName(),core::cell_error_O::static_className()));
core::control_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::control_error_O::static_packageName(),core::control_error_O::static_className()));
core::file_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::file_error_O::static_packageName(),core::file_error_O::static_className()));
core::package_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::package_error_O::static_packageName(),core::package_error_O::static_className()));
core::parse_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::parse_error_O::static_packageName(),core::parse_error_O::static_className()));
core::print_not_readable_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::print_not_readable_O::static_packageName(),core::print_not_readable_O::static_className()));
core::program_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::program_error_O::static_packageName(),core::program_error_O::static_className()));
core::simple_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::simple_error_O::static_packageName(),core::simple_error_O::static_className()));
core::stream_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::stream_error_O::static_packageName(),core::stream_error_O::static_className()));
core::type_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::type_error_O::static_packageName(),core::type_error_O::static_className()));
core::argument_mismatch_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::argument_mismatch_error_O::static_packageName(),core::argument_mismatch_error_O::static_className()));
core::division_by_zero_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::division_by_zero_O::static_packageName(),core::division_by_zero_O::static_className()));
core::end_of_file_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::end_of_file_O::static_packageName(),core::end_of_file_O::static_className()));
core::floating_point_inexact_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::floating_point_inexact_O::static_packageName(),core::floating_point_inexact_O::static_className()));
core::floating_point_invalid_operation_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::floating_point_invalid_operation_O::static_packageName(),core::floating_point_invalid_operation_O::static_className()));
core::floating_point_overflow_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::floating_point_overflow_O::static_packageName(),core::floating_point_overflow_O::static_className()));
core::floating_point_underflow_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::floating_point_underflow_O::static_packageName(),core::floating_point_underflow_O::static_className()));
core::reader_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::reader_error_O::static_packageName(),core::reader_error_O::static_className()));
core::simple_type_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::simple_type_error_O::static_packageName(),core::simple_type_error_O::static_className()));
core::unbound_slot_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::unbound_slot_O::static_packageName(),core::unbound_slot_O::static_className()));
core::unbound_variable_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::unbound_variable_O::static_packageName(),core::unbound_variable_O::static_className()));
core::undefined_function_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::undefined_function_O::static_packageName(),core::undefined_function_O::static_className()));
core::invalid_keyword_arguments_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::invalid_keyword_arguments_error_O::static_packageName(),core::invalid_keyword_arguments_error_O::static_className()));
core::odd_number_of_keyword_arguments_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::odd_number_of_keyword_arguments_error_O::static_packageName(),core::odd_number_of_keyword_arguments_error_O::static_className()));
core::single_dispatch_arguments_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::single_dispatch_arguments_error_O::static_packageName(),core::single_dispatch_arguments_error_O::static_className()));
core::too_few_arguments_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::too_few_arguments_error_O::static_packageName(),core::too_few_arguments_error_O::static_className()));
core::too_many_arguments_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::too_many_arguments_error_O::static_packageName(),core::too_many_arguments_error_O::static_className()));
core::unrecognized_keyword_arguments_error_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(core::unrecognized_keyword_arguments_error_O::static_packageName(),core::unrecognized_keyword_arguments_error_O::static_className()));
#endif // SET_SYMBOL
#undef SET_SYMBOL
#if defined(CREATE_CLASS) || defined(ALL_STAGES)
// Depends on undefinedMetaClass
    core::MetaClass_sp undefinedMetaClass; undefinedMetaClass.reset();

    LOG(BF("Creating class[classcore__T_Oval]"));
    core::BuiltInClass_sp classcore__T_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__T_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__T_Oval,_lisp,core::T_O::static_classSymbol());
    core::T_O::___staticMetaClass = classcore__T_Oval;
    _lisp->setf_findClass(core::T_O::static_classSymbol(),classcore__T_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::T_O>;
        core::T_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::T_O::static_className() % (void*)(core::T_O::static_newNil_callback()) );
    classcore__T_Oval->setInstance_newNil_callback(core::T_O::static_newNil_callback());
    {
        boost::shared_ptr<core::T_O> nil_for_class(new core::T_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::T_O::static_className() );
        core::T_O::_nil = nil_for_class;
        classcore__T_Oval->setInstanceNil(nil_for_class);
    }
    classcore__T_Oval->setSupportsSlots(core::T_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ArchiveMemoryManager_Oval]"));
    core::BuiltInClass_sp classcore__ArchiveMemoryManager_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ArchiveMemoryManager_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ArchiveMemoryManager_Oval,_lisp,core::ArchiveMemoryManager_O::static_classSymbol());
    core::ArchiveMemoryManager_O::___staticMetaClass = classcore__ArchiveMemoryManager_Oval;
    _lisp->setf_findClass(core::ArchiveMemoryManager_O::static_classSymbol(),classcore__ArchiveMemoryManager_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ArchiveMemoryManager_O>;
        core::ArchiveMemoryManager_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ArchiveMemoryManager_O::static_className() % (void*)(core::ArchiveMemoryManager_O::static_newNil_callback()) );
    classcore__ArchiveMemoryManager_Oval->setInstance_newNil_callback(core::ArchiveMemoryManager_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ArchiveMemoryManager_O> nil_for_class(new core::ArchiveMemoryManager_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ArchiveMemoryManager_O::static_className() );
        core::ArchiveMemoryManager_O::_nil = nil_for_class;
        classcore__ArchiveMemoryManager_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ArchiveMemoryManager_Oval->setSupportsSlots(core::ArchiveMemoryManager_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Archive_Oval]"));
    core::BuiltInClass_sp classcore__Archive_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Archive_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Archive_Oval,_lisp,core::Archive_O::static_classSymbol());
    core::Archive_O::___staticMetaClass = classcore__Archive_Oval;
    _lisp->setf_findClass(core::Archive_O::static_classSymbol(),classcore__Archive_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Archive_O>;
        core::Archive_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Archive_O::static_className() % (void*)(core::Archive_O::static_newNil_callback()) );
    classcore__Archive_Oval->setInstance_newNil_callback(core::Archive_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Archive_O> nil_for_class(new core::Archive_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Archive_O::static_className() );
        core::Archive_O::_nil = nil_for_class;
        classcore__Archive_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Archive_Oval->setSupportsSlots(core::Archive_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ArrayVirtual_Oval]"));
    core::BuiltInClass_sp classcore__ArrayVirtual_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ArrayVirtual_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ArrayVirtual_Oval,_lisp,core::ArrayVirtual_O::static_classSymbol());
    core::ArrayVirtual_O::___staticMetaClass = classcore__ArrayVirtual_Oval;
    _lisp->setf_findClass(core::ArrayVirtual_O::static_classSymbol(),classcore__ArrayVirtual_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ArrayVirtual_O>;
        core::ArrayVirtual_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ArrayVirtual_O::static_className() % (void*)(core::ArrayVirtual_O::static_newNil_callback()) );
    classcore__ArrayVirtual_Oval->setInstance_newNil_callback(core::ArrayVirtual_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ArrayVirtual_O> nil_for_class(new core::ArrayVirtual_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ArrayVirtual_O::static_className() );
        core::ArrayVirtual_O::_nil = nil_for_class;
        classcore__ArrayVirtual_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ArrayVirtual_Oval->setSupportsSlots(core::ArrayVirtual_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Binder_Oval]"));
    core::BuiltInClass_sp classcore__Binder_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Binder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Binder_Oval,_lisp,core::Binder_O::static_classSymbol());
    core::Binder_O::___staticMetaClass = classcore__Binder_Oval;
    _lisp->setf_findClass(core::Binder_O::static_classSymbol(),classcore__Binder_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Binder_O>;
        core::Binder_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Binder_O::static_className() % (void*)(core::Binder_O::static_newNil_callback()) );
    classcore__Binder_Oval->setInstance_newNil_callback(core::Binder_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Binder_O> nil_for_class(new core::Binder_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Binder_O::static_className() );
        core::Binder_O::_nil = nil_for_class;
        classcore__Binder_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Binder_Oval->setSupportsSlots(core::Binder_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Character_Oval]"));
    core::BuiltInClass_sp classcore__Character_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Character_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Character_Oval,_lisp,core::Character_O::static_classSymbol());
    core::Character_O::___staticMetaClass = classcore__Character_Oval;
    _lisp->setf_findClass(core::Character_O::static_classSymbol(),classcore__Character_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Character_O>;
        core::Character_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Character_O::static_className() % (void*)(core::Character_O::static_newNil_callback()) );
    classcore__Character_Oval->setInstance_newNil_callback(core::Character_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Character_O> nil_for_class(new core::Character_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Character_O::static_className() );
        core::Character_O::_nil = nil_for_class;
        classcore__Character_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Character_Oval->setSupportsSlots(core::Character_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ConditionBase_Oval]"));
    core::BuiltInClass_sp classcore__ConditionBase_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ConditionBase_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ConditionBase_Oval,_lisp,core::ConditionBase_O::static_classSymbol());
    core::ConditionBase_O::___staticMetaClass = classcore__ConditionBase_Oval;
    _lisp->setf_findClass(core::ConditionBase_O::static_classSymbol(),classcore__ConditionBase_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ConditionBase_O>;
        core::ConditionBase_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ConditionBase_O::static_className() % (void*)(core::ConditionBase_O::static_newNil_callback()) );
    classcore__ConditionBase_Oval->setInstance_newNil_callback(core::ConditionBase_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ConditionBase_O> nil_for_class(new core::ConditionBase_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ConditionBase_O::static_className() );
        core::ConditionBase_O::_nil = nil_for_class;
        classcore__ConditionBase_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ConditionBase_Oval->setSupportsSlots(core::ConditionBase_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ConditionHandlerBase_Oval]"));
    core::BuiltInClass_sp classcore__ConditionHandlerBase_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ConditionHandlerBase_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ConditionHandlerBase_Oval,_lisp,core::ConditionHandlerBase_O::static_classSymbol());
    core::ConditionHandlerBase_O::___staticMetaClass = classcore__ConditionHandlerBase_Oval;
    _lisp->setf_findClass(core::ConditionHandlerBase_O::static_classSymbol(),classcore__ConditionHandlerBase_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ConditionHandlerBase_O>;
        core::ConditionHandlerBase_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ConditionHandlerBase_O::static_className() % (void*)(core::ConditionHandlerBase_O::static_newNil_callback()) );
    classcore__ConditionHandlerBase_Oval->setInstance_newNil_callback(core::ConditionHandlerBase_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ConditionHandlerBase_O> nil_for_class(new core::ConditionHandlerBase_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ConditionHandlerBase_O::static_className() );
        core::ConditionHandlerBase_O::_nil = nil_for_class;
        classcore__ConditionHandlerBase_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ConditionHandlerBase_Oval->setSupportsSlots(core::ConditionHandlerBase_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__DirectoryEntry_Oval]"));
    core::BuiltInClass_sp classcore__DirectoryEntry_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__DirectoryEntry_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__DirectoryEntry_Oval,_lisp,core::DirectoryEntry_O::static_classSymbol());
    core::DirectoryEntry_O::___staticMetaClass = classcore__DirectoryEntry_Oval;
    _lisp->setf_findClass(core::DirectoryEntry_O::static_classSymbol(),classcore__DirectoryEntry_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::DirectoryEntry_O>;
        core::DirectoryEntry_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::DirectoryEntry_O::static_className() % (void*)(core::DirectoryEntry_O::static_newNil_callback()) );
    classcore__DirectoryEntry_Oval->setInstance_newNil_callback(core::DirectoryEntry_O::static_newNil_callback());
    {
        boost::shared_ptr<core::DirectoryEntry_O> nil_for_class(new core::DirectoryEntry_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::DirectoryEntry_O::static_className() );
        core::DirectoryEntry_O::_nil = nil_for_class;
        classcore__DirectoryEntry_Oval->setInstanceNil(nil_for_class);
    }
    classcore__DirectoryEntry_Oval->setSupportsSlots(core::DirectoryEntry_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Environment_Oval]"));
    core::BuiltInClass_sp classcore__Environment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Environment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Environment_Oval,_lisp,core::Environment_O::static_classSymbol());
    core::Environment_O::___staticMetaClass = classcore__Environment_Oval;
    _lisp->setf_findClass(core::Environment_O::static_classSymbol(),classcore__Environment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Environment_O>;
        core::Environment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Environment_O::static_className() % (void*)(core::Environment_O::static_newNil_callback()) );
    classcore__Environment_Oval->setInstance_newNil_callback(core::Environment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Environment_O> nil_for_class(new core::Environment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Environment_O::static_className() );
        core::Environment_O::_nil = nil_for_class;
        classcore__Environment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Environment_Oval->setSupportsSlots(core::Environment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ExternalObjectManager_Oval]"));
    core::BuiltInClass_sp classcore__ExternalObjectManager_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ExternalObjectManager_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ExternalObjectManager_Oval,_lisp,core::ExternalObjectManager_O::static_classSymbol());
    core::ExternalObjectManager_O::___staticMetaClass = classcore__ExternalObjectManager_Oval;
    _lisp->setf_findClass(core::ExternalObjectManager_O::static_classSymbol(),classcore__ExternalObjectManager_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ExternalObjectManager_O>;
        core::ExternalObjectManager_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ExternalObjectManager_O::static_className() % (void*)(core::ExternalObjectManager_O::static_newNil_callback()) );
    classcore__ExternalObjectManager_Oval->setInstance_newNil_callback(core::ExternalObjectManager_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ExternalObjectManager_O> nil_for_class(new core::ExternalObjectManager_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ExternalObjectManager_O::static_className() );
        core::ExternalObjectManager_O::_nil = nil_for_class;
        classcore__ExternalObjectManager_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ExternalObjectManager_Oval->setSupportsSlots(core::ExternalObjectManager_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ExternalObject_Oval]"));
    core::BuiltInClass_sp classcore__ExternalObject_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ExternalObject_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ExternalObject_Oval,_lisp,core::ExternalObject_O::static_classSymbol());
    core::ExternalObject_O::___staticMetaClass = classcore__ExternalObject_Oval;
    _lisp->setf_findClass(core::ExternalObject_O::static_classSymbol(),classcore__ExternalObject_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ExternalObject_O>;
        core::ExternalObject_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ExternalObject_O::static_className() % (void*)(core::ExternalObject_O::static_newNil_callback()) );
    classcore__ExternalObject_Oval->setInstance_newNil_callback(core::ExternalObject_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ExternalObject_O> nil_for_class(new core::ExternalObject_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ExternalObject_O::static_className() );
        core::ExternalObject_O::_nil = nil_for_class;
        classcore__ExternalObject_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ExternalObject_Oval->setSupportsSlots(core::ExternalObject_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FileStatus_Oval]"));
    core::BuiltInClass_sp classcore__FileStatus_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FileStatus_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FileStatus_Oval,_lisp,core::FileStatus_O::static_classSymbol());
    core::FileStatus_O::___staticMetaClass = classcore__FileStatus_Oval;
    _lisp->setf_findClass(core::FileStatus_O::static_classSymbol(),classcore__FileStatus_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FileStatus_O>;
        core::FileStatus_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FileStatus_O::static_className() % (void*)(core::FileStatus_O::static_newNil_callback()) );
    classcore__FileStatus_Oval->setInstance_newNil_callback(core::FileStatus_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FileStatus_O> nil_for_class(new core::FileStatus_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FileStatus_O::static_className() );
        core::FileStatus_O::_nil = nil_for_class;
        classcore__FileStatus_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FileStatus_Oval->setSupportsSlots(core::FileStatus_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FormBinder_Oval]"));
    core::BuiltInClass_sp classcore__FormBinder_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FormBinder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FormBinder_Oval,_lisp,core::FormBinder_O::static_classSymbol());
    core::FormBinder_O::___staticMetaClass = classcore__FormBinder_Oval;
    _lisp->setf_findClass(core::FormBinder_O::static_classSymbol(),classcore__FormBinder_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FormBinder_O>;
        core::FormBinder_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FormBinder_O::static_className() % (void*)(core::FormBinder_O::static_newNil_callback()) );
    classcore__FormBinder_Oval->setInstance_newNil_callback(core::FormBinder_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FormBinder_O> nil_for_class(new core::FormBinder_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FormBinder_O::static_className() );
        core::FormBinder_O::_nil = nil_for_class;
        classcore__FormBinder_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FormBinder_Oval->setSupportsSlots(core::FormBinder_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Function_Oval]"));
    core::BuiltInClass_sp classcore__Function_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Function_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Function_Oval,_lisp,core::Function_O::static_classSymbol());
    core::Function_O::___staticMetaClass = classcore__Function_Oval;
    _lisp->setf_findClass(core::Function_O::static_classSymbol(),classcore__Function_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Function_O>;
        core::Function_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Function_O::static_className() % (void*)(core::Function_O::static_newNil_callback()) );
    classcore__Function_Oval->setInstance_newNil_callback(core::Function_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Function_O> nil_for_class(new core::Function_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Function_O::static_className() );
        core::Function_O::_nil = nil_for_class;
        classcore__Function_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Function_Oval->setSupportsSlots(core::Function_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__HashTable_Oval]"));
    core::BuiltInClass_sp classcore__HashTable_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__HashTable_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__HashTable_Oval,_lisp,core::HashTable_O::static_classSymbol());
    core::HashTable_O::___staticMetaClass = classcore__HashTable_Oval;
    _lisp->setf_findClass(core::HashTable_O::static_classSymbol(),classcore__HashTable_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::HashTable_O>;
        core::HashTable_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::HashTable_O::static_className() % (void*)(core::HashTable_O::static_newNil_callback()) );
    classcore__HashTable_Oval->setInstance_newNil_callback(core::HashTable_O::static_newNil_callback());
    {
        boost::shared_ptr<core::HashTable_O> nil_for_class(new core::HashTable_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::HashTable_O::static_className() );
        core::HashTable_O::_nil = nil_for_class;
        classcore__HashTable_Oval->setInstanceNil(nil_for_class);
    }
    classcore__HashTable_Oval->setSupportsSlots(core::HashTable_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Instance_Oval]"));
    core::BuiltInClass_sp classcore__Instance_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Instance_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Instance_Oval,_lisp,core::Instance_O::static_classSymbol());
    core::Instance_O::___staticMetaClass = classcore__Instance_Oval;
    _lisp->setf_findClass(core::Instance_O::static_classSymbol(),classcore__Instance_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Instance_O>;
        core::Instance_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Instance_O::static_className() % (void*)(core::Instance_O::static_newNil_callback()) );
    classcore__Instance_Oval->setInstance_newNil_callback(core::Instance_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Instance_O> nil_for_class(new core::Instance_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Instance_O::static_className() );
        core::Instance_O::_nil = nil_for_class;
        classcore__Instance_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Instance_Oval->setSupportsSlots(core::Instance_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__IntArray_Oval]"));
    core::BuiltInClass_sp classcore__IntArray_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__IntArray_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__IntArray_Oval,_lisp,core::IntArray_O::static_classSymbol());
    core::IntArray_O::___staticMetaClass = classcore__IntArray_Oval;
    _lisp->setf_findClass(core::IntArray_O::static_classSymbol(),classcore__IntArray_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::IntArray_O>;
        core::IntArray_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::IntArray_O::static_className() % (void*)(core::IntArray_O::static_newNil_callback()) );
    classcore__IntArray_Oval->setInstance_newNil_callback(core::IntArray_O::static_newNil_callback());
    {
        boost::shared_ptr<core::IntArray_O> nil_for_class(new core::IntArray_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::IntArray_O::static_className() );
        core::IntArray_O::_nil = nil_for_class;
        classcore__IntArray_Oval->setInstanceNil(nil_for_class);
    }
    classcore__IntArray_Oval->setSupportsSlots(core::IntArray_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__IntegerKeyObjectDictionary_Oval]"));
    core::BuiltInClass_sp classcore__IntegerKeyObjectDictionary_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__IntegerKeyObjectDictionary_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__IntegerKeyObjectDictionary_Oval,_lisp,core::IntegerKeyObjectDictionary_O::static_classSymbol());
    core::IntegerKeyObjectDictionary_O::___staticMetaClass = classcore__IntegerKeyObjectDictionary_Oval;
    _lisp->setf_findClass(core::IntegerKeyObjectDictionary_O::static_classSymbol(),classcore__IntegerKeyObjectDictionary_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::IntegerKeyObjectDictionary_O>;
        core::IntegerKeyObjectDictionary_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::IntegerKeyObjectDictionary_O::static_className() % (void*)(core::IntegerKeyObjectDictionary_O::static_newNil_callback()) );
    classcore__IntegerKeyObjectDictionary_Oval->setInstance_newNil_callback(core::IntegerKeyObjectDictionary_O::static_newNil_callback());
    {
        boost::shared_ptr<core::IntegerKeyObjectDictionary_O> nil_for_class(new core::IntegerKeyObjectDictionary_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::IntegerKeyObjectDictionary_O::static_className() );
        core::IntegerKeyObjectDictionary_O::_nil = nil_for_class;
        classcore__IntegerKeyObjectDictionary_Oval->setInstanceNil(nil_for_class);
    }
    classcore__IntegerKeyObjectDictionary_Oval->setSupportsSlots(core::IntegerKeyObjectDictionary_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Iterator_Oval]"));
    core::BuiltInClass_sp classcore__Iterator_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Iterator_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Iterator_Oval,_lisp,core::Iterator_O::static_classSymbol());
    core::Iterator_O::___staticMetaClass = classcore__Iterator_Oval;
    _lisp->setf_findClass(core::Iterator_O::static_classSymbol(),classcore__Iterator_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Iterator_O>;
        core::Iterator_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Iterator_O::static_className() % (void*)(core::Iterator_O::static_newNil_callback()) );
    classcore__Iterator_Oval->setInstance_newNil_callback(core::Iterator_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Iterator_O> nil_for_class(new core::Iterator_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Iterator_O::static_className() );
        core::Iterator_O::_nil = nil_for_class;
        classcore__Iterator_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Iterator_Oval->setSupportsSlots(core::Iterator_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__LambdaListHandler_Oval]"));
    core::BuiltInClass_sp classcore__LambdaListHandler_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__LambdaListHandler_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__LambdaListHandler_Oval,_lisp,core::LambdaListHandler_O::static_classSymbol());
    core::LambdaListHandler_O::___staticMetaClass = classcore__LambdaListHandler_Oval;
    _lisp->setf_findClass(core::LambdaListHandler_O::static_classSymbol(),classcore__LambdaListHandler_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::LambdaListHandler_O>;
        core::LambdaListHandler_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::LambdaListHandler_O::static_className() % (void*)(core::LambdaListHandler_O::static_newNil_callback()) );
    classcore__LambdaListHandler_Oval->setInstance_newNil_callback(core::LambdaListHandler_O::static_newNil_callback());
    {
        boost::shared_ptr<core::LambdaListHandler_O> nil_for_class(new core::LambdaListHandler_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::LambdaListHandler_O::static_className() );
        core::LambdaListHandler_O::_nil = nil_for_class;
        classcore__LambdaListHandler_Oval->setInstanceNil(nil_for_class);
    }
    classcore__LambdaListHandler_Oval->setSupportsSlots(core::LambdaListHandler_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Lisp_Oval]"));
    core::BuiltInClass_sp classcore__Lisp_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Lisp_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Lisp_Oval,_lisp,core::Lisp_O::static_classSymbol());
    core::Lisp_O::___staticMetaClass = classcore__Lisp_Oval;
    _lisp->setf_findClass(core::Lisp_O::static_classSymbol(),classcore__Lisp_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Lisp_O>;
        core::Lisp_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Lisp_O::static_className() % (void*)(core::Lisp_O::static_newNil_callback()) );
    classcore__Lisp_Oval->setInstance_newNil_callback(core::Lisp_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Lisp_O> nil_for_class(new core::Lisp_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Lisp_O::static_className() );
        core::Lisp_O::_nil = nil_for_class;
        classcore__Lisp_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Lisp_Oval->setSupportsSlots(core::Lisp_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__MethodCombination_Oval]"));
    core::BuiltInClass_sp classcore__MethodCombination_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__MethodCombination_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__MethodCombination_Oval,_lisp,core::MethodCombination_O::static_classSymbol());
    core::MethodCombination_O::___staticMetaClass = classcore__MethodCombination_Oval;
    _lisp->setf_findClass(core::MethodCombination_O::static_classSymbol(),classcore__MethodCombination_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::MethodCombination_O>;
        core::MethodCombination_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::MethodCombination_O::static_className() % (void*)(core::MethodCombination_O::static_newNil_callback()) );
    classcore__MethodCombination_Oval->setInstance_newNil_callback(core::MethodCombination_O::static_newNil_callback());
    {
        boost::shared_ptr<core::MethodCombination_O> nil_for_class(new core::MethodCombination_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::MethodCombination_O::static_className() );
        core::MethodCombination_O::_nil = nil_for_class;
        classcore__MethodCombination_Oval->setInstanceNil(nil_for_class);
    }
    classcore__MethodCombination_Oval->setSupportsSlots(core::MethodCombination_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__MethodSet_Oval]"));
    core::BuiltInClass_sp classcore__MethodSet_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__MethodSet_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__MethodSet_Oval,_lisp,core::MethodSet_O::static_classSymbol());
    core::MethodSet_O::___staticMetaClass = classcore__MethodSet_Oval;
    _lisp->setf_findClass(core::MethodSet_O::static_classSymbol(),classcore__MethodSet_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::MethodSet_O>;
        core::MethodSet_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::MethodSet_O::static_className() % (void*)(core::MethodSet_O::static_newNil_callback()) );
    classcore__MethodSet_Oval->setInstance_newNil_callback(core::MethodSet_O::static_newNil_callback());
    {
        boost::shared_ptr<core::MethodSet_O> nil_for_class(new core::MethodSet_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::MethodSet_O::static_className() );
        core::MethodSet_O::_nil = nil_for_class;
        classcore__MethodSet_Oval->setInstanceNil(nil_for_class);
    }
    classcore__MethodSet_Oval->setSupportsSlots(core::MethodSet_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Method_Oval]"));
    core::BuiltInClass_sp classcore__Method_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Method_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Method_Oval,_lisp,core::Method_O::static_classSymbol());
    core::Method_O::___staticMetaClass = classcore__Method_Oval;
    _lisp->setf_findClass(core::Method_O::static_classSymbol(),classcore__Method_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Method_O>;
        core::Method_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Method_O::static_className() % (void*)(core::Method_O::static_newNil_callback()) );
    classcore__Method_Oval->setInstance_newNil_callback(core::Method_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Method_O> nil_for_class(new core::Method_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Method_O::static_className() );
        core::Method_O::_nil = nil_for_class;
        classcore__Method_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Method_Oval->setSupportsSlots(core::Method_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__MicroHeap_Oval]"));
    core::BuiltInClass_sp classcore__MicroHeap_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__MicroHeap_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__MicroHeap_Oval,_lisp,core::MicroHeap_O::static_classSymbol());
    core::MicroHeap_O::___staticMetaClass = classcore__MicroHeap_Oval;
    _lisp->setf_findClass(core::MicroHeap_O::static_classSymbol(),classcore__MicroHeap_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::MicroHeap_O>;
        core::MicroHeap_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::MicroHeap_O::static_className() % (void*)(core::MicroHeap_O::static_newNil_callback()) );
    classcore__MicroHeap_Oval->setInstance_newNil_callback(core::MicroHeap_O::static_newNil_callback());
    {
        boost::shared_ptr<core::MicroHeap_O> nil_for_class(new core::MicroHeap_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::MicroHeap_O::static_className() );
        core::MicroHeap_O::_nil = nil_for_class;
        classcore__MicroHeap_Oval->setInstanceNil(nil_for_class);
    }
    classcore__MicroHeap_Oval->setSupportsSlots(core::MicroHeap_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Model_Oval]"));
    core::BuiltInClass_sp classcore__Model_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Model_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Model_Oval,_lisp,core::Model_O::static_classSymbol());
    core::Model_O::___staticMetaClass = classcore__Model_Oval;
    _lisp->setf_findClass(core::Model_O::static_classSymbol(),classcore__Model_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Model_O>;
        core::Model_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Model_O::static_className() % (void*)(core::Model_O::static_newNil_callback()) );
    classcore__Model_Oval->setInstance_newNil_callback(core::Model_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Model_O> nil_for_class(new core::Model_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Model_O::static_className() );
        core::Model_O::_nil = nil_for_class;
        classcore__Model_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Model_Oval->setSupportsSlots(core::Model_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__MultiStringBuffer_Oval]"));
    core::BuiltInClass_sp classcore__MultiStringBuffer_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__MultiStringBuffer_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__MultiStringBuffer_Oval,_lisp,core::MultiStringBuffer_O::static_classSymbol());
    core::MultiStringBuffer_O::___staticMetaClass = classcore__MultiStringBuffer_Oval;
    _lisp->setf_findClass(core::MultiStringBuffer_O::static_classSymbol(),classcore__MultiStringBuffer_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::MultiStringBuffer_O>;
        core::MultiStringBuffer_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::MultiStringBuffer_O::static_className() % (void*)(core::MultiStringBuffer_O::static_newNil_callback()) );
    classcore__MultiStringBuffer_Oval->setInstance_newNil_callback(core::MultiStringBuffer_O::static_newNil_callback());
    {
        boost::shared_ptr<core::MultiStringBuffer_O> nil_for_class(new core::MultiStringBuffer_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::MultiStringBuffer_O::static_className() );
        core::MultiStringBuffer_O::_nil = nil_for_class;
        classcore__MultiStringBuffer_Oval->setInstanceNil(nil_for_class);
    }
    classcore__MultiStringBuffer_Oval->setSupportsSlots(core::MultiStringBuffer_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__MultipleValues_Oval]"));
    core::BuiltInClass_sp classcore__MultipleValues_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__MultipleValues_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__MultipleValues_Oval,_lisp,core::MultipleValues_O::static_classSymbol());
    core::MultipleValues_O::___staticMetaClass = classcore__MultipleValues_Oval;
    _lisp->setf_findClass(core::MultipleValues_O::static_classSymbol(),classcore__MultipleValues_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::MultipleValues_O>;
        core::MultipleValues_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::MultipleValues_O::static_className() % (void*)(core::MultipleValues_O::static_newNil_callback()) );
    classcore__MultipleValues_Oval->setInstance_newNil_callback(core::MultipleValues_O::static_newNil_callback());
    {
        boost::shared_ptr<core::MultipleValues_O> nil_for_class(new core::MultipleValues_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::MultipleValues_O::static_className() );
        core::MultipleValues_O::_nil = nil_for_class;
        classcore__MultipleValues_Oval->setInstanceNil(nil_for_class);
    }
    classcore__MultipleValues_Oval->setSupportsSlots(core::MultipleValues_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Number_Oval]"));
    core::BuiltInClass_sp classcore__Number_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Number_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Number_Oval,_lisp,core::Number_O::static_classSymbol());
    core::Number_O::___staticMetaClass = classcore__Number_Oval;
    _lisp->setf_findClass(core::Number_O::static_classSymbol(),classcore__Number_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Number_O>;
        core::Number_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Number_O::static_className() % (void*)(core::Number_O::static_newNil_callback()) );
    classcore__Number_Oval->setInstance_newNil_callback(core::Number_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Number_O> nil_for_class(new core::Number_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Number_O::static_className() );
        core::Number_O::_nil = nil_for_class;
        classcore__Number_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Number_Oval->setSupportsSlots(core::Number_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ObjRef_Oval]"));
    core::BuiltInClass_sp classcore__ObjRef_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ObjRef_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ObjRef_Oval,_lisp,core::ObjRef_O::static_classSymbol());
    core::ObjRef_O::___staticMetaClass = classcore__ObjRef_Oval;
    _lisp->setf_findClass(core::ObjRef_O::static_classSymbol(),classcore__ObjRef_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ObjRef_O>;
        core::ObjRef_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ObjRef_O::static_className() % (void*)(core::ObjRef_O::static_newNil_callback()) );
    classcore__ObjRef_Oval->setInstance_newNil_callback(core::ObjRef_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ObjRef_O> nil_for_class(new core::ObjRef_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ObjRef_O::static_className() );
        core::ObjRef_O::_nil = nil_for_class;
        classcore__ObjRef_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ObjRef_Oval->setSupportsSlots(core::ObjRef_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ObjectSet_Oval]"));
    core::BuiltInClass_sp classcore__ObjectSet_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ObjectSet_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ObjectSet_Oval,_lisp,core::ObjectSet_O::static_classSymbol());
    core::ObjectSet_O::___staticMetaClass = classcore__ObjectSet_Oval;
    _lisp->setf_findClass(core::ObjectSet_O::static_classSymbol(),classcore__ObjectSet_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ObjectSet_O>;
        core::ObjectSet_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ObjectSet_O::static_className() % (void*)(core::ObjectSet_O::static_newNil_callback()) );
    classcore__ObjectSet_Oval->setInstance_newNil_callback(core::ObjectSet_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ObjectSet_O> nil_for_class(new core::ObjectSet_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ObjectSet_O::static_className() );
        core::ObjectSet_O::_nil = nil_for_class;
        classcore__ObjectSet_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ObjectSet_Oval->setSupportsSlots(core::ObjectSet_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Package_Oval]"));
    core::BuiltInClass_sp classcore__Package_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Package_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Package_Oval,_lisp,core::Package_O::static_classSymbol());
    core::Package_O::___staticMetaClass = classcore__Package_Oval;
    _lisp->setf_findClass(core::Package_O::static_classSymbol(),classcore__Package_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Package_O>;
        core::Package_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Package_O::static_className() % (void*)(core::Package_O::static_newNil_callback()) );
    classcore__Package_Oval->setInstance_newNil_callback(core::Package_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Package_O> nil_for_class(new core::Package_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Package_O::static_className() );
        core::Package_O::_nil = nil_for_class;
        classcore__Package_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Package_Oval->setSupportsSlots(core::Package_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Path_Oval]"));
    core::BuiltInClass_sp classcore__Path_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Path_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Path_Oval,_lisp,core::Path_O::static_classSymbol());
    core::Path_O::___staticMetaClass = classcore__Path_Oval;
    _lisp->setf_findClass(core::Path_O::static_classSymbol(),classcore__Path_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Path_O>;
        core::Path_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Path_O::static_className() % (void*)(core::Path_O::static_newNil_callback()) );
    classcore__Path_Oval->setInstance_newNil_callback(core::Path_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Path_O> nil_for_class(new core::Path_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Path_O::static_className() );
        core::Path_O::_nil = nil_for_class;
        classcore__Path_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Path_Oval->setSupportsSlots(core::Path_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Pointer_Oval]"));
    core::BuiltInClass_sp classcore__Pointer_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Pointer_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Pointer_Oval,_lisp,core::Pointer_O::static_classSymbol());
    core::Pointer_O::___staticMetaClass = classcore__Pointer_Oval;
    _lisp->setf_findClass(core::Pointer_O::static_classSymbol(),classcore__Pointer_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Pointer_O>;
        core::Pointer_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Pointer_O::static_className() % (void*)(core::Pointer_O::static_newNil_callback()) );
    classcore__Pointer_Oval->setInstance_newNil_callback(core::Pointer_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Pointer_O> nil_for_class(new core::Pointer_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Pointer_O::static_className() );
        core::Pointer_O::_nil = nil_for_class;
        classcore__Pointer_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Pointer_Oval->setSupportsSlots(core::Pointer_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__PosixTimeDuration_Oval]"));
    core::BuiltInClass_sp classcore__PosixTimeDuration_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__PosixTimeDuration_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__PosixTimeDuration_Oval,_lisp,core::PosixTimeDuration_O::static_classSymbol());
    core::PosixTimeDuration_O::___staticMetaClass = classcore__PosixTimeDuration_Oval;
    _lisp->setf_findClass(core::PosixTimeDuration_O::static_classSymbol(),classcore__PosixTimeDuration_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::PosixTimeDuration_O>;
        core::PosixTimeDuration_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::PosixTimeDuration_O::static_className() % (void*)(core::PosixTimeDuration_O::static_newNil_callback()) );
    classcore__PosixTimeDuration_Oval->setInstance_newNil_callback(core::PosixTimeDuration_O::static_newNil_callback());
    {
        boost::shared_ptr<core::PosixTimeDuration_O> nil_for_class(new core::PosixTimeDuration_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::PosixTimeDuration_O::static_className() );
        core::PosixTimeDuration_O::_nil = nil_for_class;
        classcore__PosixTimeDuration_Oval->setInstanceNil(nil_for_class);
    }
    classcore__PosixTimeDuration_Oval->setSupportsSlots(core::PosixTimeDuration_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__PosixTime_Oval]"));
    core::BuiltInClass_sp classcore__PosixTime_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__PosixTime_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__PosixTime_Oval,_lisp,core::PosixTime_O::static_classSymbol());
    core::PosixTime_O::___staticMetaClass = classcore__PosixTime_Oval;
    _lisp->setf_findClass(core::PosixTime_O::static_classSymbol(),classcore__PosixTime_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::PosixTime_O>;
        core::PosixTime_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::PosixTime_O::static_className() % (void*)(core::PosixTime_O::static_newNil_callback()) );
    classcore__PosixTime_Oval->setInstance_newNil_callback(core::PosixTime_O::static_newNil_callback());
    {
        boost::shared_ptr<core::PosixTime_O> nil_for_class(new core::PosixTime_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::PosixTime_O::static_className() );
        core::PosixTime_O::_nil = nil_for_class;
        classcore__PosixTime_Oval->setInstanceNil(nil_for_class);
    }
    classcore__PosixTime_Oval->setSupportsSlots(core::PosixTime_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__QDomNode_Oval]"));
    core::BuiltInClass_sp classcore__QDomNode_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__QDomNode_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__QDomNode_Oval,_lisp,core::QDomNode_O::static_classSymbol());
    core::QDomNode_O::___staticMetaClass = classcore__QDomNode_Oval;
    _lisp->setf_findClass(core::QDomNode_O::static_classSymbol(),classcore__QDomNode_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::QDomNode_O>;
        core::QDomNode_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::QDomNode_O::static_className() % (void*)(core::QDomNode_O::static_newNil_callback()) );
    classcore__QDomNode_Oval->setInstance_newNil_callback(core::QDomNode_O::static_newNil_callback());
    {
        boost::shared_ptr<core::QDomNode_O> nil_for_class(new core::QDomNode_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::QDomNode_O::static_className() );
        core::QDomNode_O::_nil = nil_for_class;
        classcore__QDomNode_Oval->setInstanceNil(nil_for_class);
    }
    classcore__QDomNode_Oval->setSupportsSlots(core::QDomNode_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ReadTable_Oval]"));
    core::BuiltInClass_sp classcore__ReadTable_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ReadTable_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ReadTable_Oval,_lisp,core::ReadTable_O::static_classSymbol());
    core::ReadTable_O::___staticMetaClass = classcore__ReadTable_Oval;
    _lisp->setf_findClass(core::ReadTable_O::static_classSymbol(),classcore__ReadTable_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ReadTable_O>;
        core::ReadTable_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ReadTable_O::static_className() % (void*)(core::ReadTable_O::static_newNil_callback()) );
    classcore__ReadTable_Oval->setInstance_newNil_callback(core::ReadTable_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ReadTable_O> nil_for_class(new core::ReadTable_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ReadTable_O::static_className() );
        core::ReadTable_O::_nil = nil_for_class;
        classcore__ReadTable_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ReadTable_Oval->setSupportsSlots(core::ReadTable_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Reader_Oval]"));
    core::BuiltInClass_sp classcore__Reader_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Reader_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Reader_Oval,_lisp,core::Reader_O::static_classSymbol());
    core::Reader_O::___staticMetaClass = classcore__Reader_Oval;
    _lisp->setf_findClass(core::Reader_O::static_classSymbol(),classcore__Reader_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Reader_O>;
        core::Reader_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Reader_O::static_className() % (void*)(core::Reader_O::static_newNil_callback()) );
    classcore__Reader_Oval->setInstance_newNil_callback(core::Reader_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Reader_O> nil_for_class(new core::Reader_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Reader_O::static_className() );
        core::Reader_O::_nil = nil_for_class;
        classcore__Reader_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Reader_Oval->setSupportsSlots(core::Reader_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__RestartCase_Oval]"));
    core::BuiltInClass_sp classcore__RestartCase_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__RestartCase_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__RestartCase_Oval,_lisp,core::RestartCase_O::static_classSymbol());
    core::RestartCase_O::___staticMetaClass = classcore__RestartCase_Oval;
    _lisp->setf_findClass(core::RestartCase_O::static_classSymbol(),classcore__RestartCase_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::RestartCase_O>;
        core::RestartCase_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::RestartCase_O::static_className() % (void*)(core::RestartCase_O::static_newNil_callback()) );
    classcore__RestartCase_Oval->setInstance_newNil_callback(core::RestartCase_O::static_newNil_callback());
    {
        boost::shared_ptr<core::RestartCase_O> nil_for_class(new core::RestartCase_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::RestartCase_O::static_className() );
        core::RestartCase_O::_nil = nil_for_class;
        classcore__RestartCase_Oval->setInstanceNil(nil_for_class);
    }
    classcore__RestartCase_Oval->setSupportsSlots(core::RestartCase_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SequenceVirtual_Oval]"));
    core::BuiltInClass_sp classcore__SequenceVirtual_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SequenceVirtual_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SequenceVirtual_Oval,_lisp,core::SequenceVirtual_O::static_classSymbol());
    core::SequenceVirtual_O::___staticMetaClass = classcore__SequenceVirtual_Oval;
    _lisp->setf_findClass(core::SequenceVirtual_O::static_classSymbol(),classcore__SequenceVirtual_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SequenceVirtual_O>;
        core::SequenceVirtual_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SequenceVirtual_O::static_className() % (void*)(core::SequenceVirtual_O::static_newNil_callback()) );
    classcore__SequenceVirtual_Oval->setInstance_newNil_callback(core::SequenceVirtual_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SequenceVirtual_O> nil_for_class(new core::SequenceVirtual_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SequenceVirtual_O::static_className() );
        core::SequenceVirtual_O::_nil = nil_for_class;
        classcore__SequenceVirtual_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SequenceVirtual_Oval->setSupportsSlots(core::SequenceVirtual_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Serializer_Oval]"));
    core::BuiltInClass_sp classcore__Serializer_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Serializer_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Serializer_Oval,_lisp,core::Serializer_O::static_classSymbol());
    core::Serializer_O::___staticMetaClass = classcore__Serializer_Oval;
    _lisp->setf_findClass(core::Serializer_O::static_classSymbol(),classcore__Serializer_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Serializer_O>;
        core::Serializer_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Serializer_O::static_className() % (void*)(core::Serializer_O::static_newNil_callback()) );
    classcore__Serializer_Oval->setInstance_newNil_callback(core::Serializer_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Serializer_O> nil_for_class(new core::Serializer_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Serializer_O::static_className() );
        core::Serializer_O::_nil = nil_for_class;
        classcore__Serializer_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Serializer_Oval->setSupportsSlots(core::Serializer_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SetfExpander_Oval]"));
    core::BuiltInClass_sp classcore__SetfExpander_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SetfExpander_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SetfExpander_Oval,_lisp,core::SetfExpander_O::static_classSymbol());
    core::SetfExpander_O::___staticMetaClass = classcore__SetfExpander_Oval;
    _lisp->setf_findClass(core::SetfExpander_O::static_classSymbol(),classcore__SetfExpander_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SetfExpander_O>;
        core::SetfExpander_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SetfExpander_O::static_className() % (void*)(core::SetfExpander_O::static_newNil_callback()) );
    classcore__SetfExpander_Oval->setInstance_newNil_callback(core::SetfExpander_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SetfExpander_O> nil_for_class(new core::SetfExpander_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SetfExpander_O::static_className() );
        core::SetfExpander_O::_nil = nil_for_class;
        classcore__SetfExpander_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SetfExpander_Oval->setSupportsSlots(core::SetfExpander_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SingleDispatchMethod_Oval]"));
    core::BuiltInClass_sp classcore__SingleDispatchMethod_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SingleDispatchMethod_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SingleDispatchMethod_Oval,_lisp,core::SingleDispatchMethod_O::static_classSymbol());
    core::SingleDispatchMethod_O::___staticMetaClass = classcore__SingleDispatchMethod_Oval;
    _lisp->setf_findClass(core::SingleDispatchMethod_O::static_classSymbol(),classcore__SingleDispatchMethod_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SingleDispatchMethod_O>;
        core::SingleDispatchMethod_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SingleDispatchMethod_O::static_className() % (void*)(core::SingleDispatchMethod_O::static_newNil_callback()) );
    classcore__SingleDispatchMethod_Oval->setInstance_newNil_callback(core::SingleDispatchMethod_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SingleDispatchMethod_O> nil_for_class(new core::SingleDispatchMethod_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SingleDispatchMethod_O::static_className() );
        core::SingleDispatchMethod_O::_nil = nil_for_class;
        classcore__SingleDispatchMethod_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SingleDispatchMethod_Oval->setSupportsSlots(core::SingleDispatchMethod_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SourceFileInfo_Oval]"));
    core::BuiltInClass_sp classcore__SourceFileInfo_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SourceFileInfo_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SourceFileInfo_Oval,_lisp,core::SourceFileInfo_O::static_classSymbol());
    core::SourceFileInfo_O::___staticMetaClass = classcore__SourceFileInfo_Oval;
    _lisp->setf_findClass(core::SourceFileInfo_O::static_classSymbol(),classcore__SourceFileInfo_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SourceFileInfo_O>;
        core::SourceFileInfo_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SourceFileInfo_O::static_className() % (void*)(core::SourceFileInfo_O::static_newNil_callback()) );
    classcore__SourceFileInfo_Oval->setInstance_newNil_callback(core::SourceFileInfo_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SourceFileInfo_O> nil_for_class(new core::SourceFileInfo_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SourceFileInfo_O::static_className() );
        core::SourceFileInfo_O::_nil = nil_for_class;
        classcore__SourceFileInfo_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SourceFileInfo_Oval->setSupportsSlots(core::SourceFileInfo_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__StandardObject_Oval]"));
    StandardClass_sp classcore__StandardObject_Oval = StandardClass_O::create(undefinedMetaClass);
    classcore__StandardObject_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StandardObject_Oval,_lisp,core::StandardObject_O::static_classSymbol());
    core::StandardObject_O::___staticMetaClass = classcore__StandardObject_Oval;
    _lisp->setf_findClass(core::StandardObject_O::static_classSymbol(),classcore__StandardObject_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::StandardObject_O>;
        core::StandardObject_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::StandardObject_O::static_className() % (void*)(core::StandardObject_O::static_newNil_callback()) );
    classcore__StandardObject_Oval->setInstance_newNil_callback(core::StandardObject_O::static_newNil_callback());
    {
        boost::shared_ptr<core::StandardObject_O> nil_for_class(new core::StandardObject_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::StandardObject_O::static_className() );
        core::StandardObject_O::_nil = nil_for_class;
        classcore__StandardObject_Oval->setInstanceNil(nil_for_class);
    }
    classcore__StandardObject_Oval->setSupportsSlots(core::StandardObject_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Stream_Oval]"));
    core::BuiltInClass_sp classcore__Stream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Stream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Stream_Oval,_lisp,core::Stream_O::static_classSymbol());
    core::Stream_O::___staticMetaClass = classcore__Stream_Oval;
    _lisp->setf_findClass(core::Stream_O::static_classSymbol(),classcore__Stream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Stream_O>;
        core::Stream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Stream_O::static_className() % (void*)(core::Stream_O::static_newNil_callback()) );
    classcore__Stream_Oval->setInstance_newNil_callback(core::Stream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Stream_O> nil_for_class(new core::Stream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Stream_O::static_className() );
        core::Stream_O::_nil = nil_for_class;
        classcore__Stream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Stream_Oval->setSupportsSlots(core::Stream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__StringList_Oval]"));
    core::BuiltInClass_sp classcore__StringList_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__StringList_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StringList_Oval,_lisp,core::StringList_O::static_classSymbol());
    core::StringList_O::___staticMetaClass = classcore__StringList_Oval;
    _lisp->setf_findClass(core::StringList_O::static_classSymbol(),classcore__StringList_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::StringList_O>;
        core::StringList_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::StringList_O::static_className() % (void*)(core::StringList_O::static_newNil_callback()) );
    classcore__StringList_Oval->setInstance_newNil_callback(core::StringList_O::static_newNil_callback());
    {
        boost::shared_ptr<core::StringList_O> nil_for_class(new core::StringList_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::StringList_O::static_className() );
        core::StringList_O::_nil = nil_for_class;
        classcore__StringList_Oval->setInstanceNil(nil_for_class);
    }
    classcore__StringList_Oval->setSupportsSlots(core::StringList_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__StringSet_Oval]"));
    core::BuiltInClass_sp classcore__StringSet_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__StringSet_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StringSet_Oval,_lisp,core::StringSet_O::static_classSymbol());
    core::StringSet_O::___staticMetaClass = classcore__StringSet_Oval;
    _lisp->setf_findClass(core::StringSet_O::static_classSymbol(),classcore__StringSet_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::StringSet_O>;
        core::StringSet_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::StringSet_O::static_className() % (void*)(core::StringSet_O::static_newNil_callback()) );
    classcore__StringSet_Oval->setInstance_newNil_callback(core::StringSet_O::static_newNil_callback());
    {
        boost::shared_ptr<core::StringSet_O> nil_for_class(new core::StringSet_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::StringSet_O::static_className() );
        core::StringSet_O::_nil = nil_for_class;
        classcore__StringSet_Oval->setInstanceNil(nil_for_class);
    }
    classcore__StringSet_Oval->setSupportsSlots(core::StringSet_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SymbolList_Oval]"));
    core::BuiltInClass_sp classcore__SymbolList_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SymbolList_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SymbolList_Oval,_lisp,core::SymbolList_O::static_classSymbol());
    core::SymbolList_O::___staticMetaClass = classcore__SymbolList_Oval;
    _lisp->setf_findClass(core::SymbolList_O::static_classSymbol(),classcore__SymbolList_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SymbolList_O>;
        core::SymbolList_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SymbolList_O::static_className() % (void*)(core::SymbolList_O::static_newNil_callback()) );
    classcore__SymbolList_Oval->setInstance_newNil_callback(core::SymbolList_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SymbolList_O> nil_for_class(new core::SymbolList_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SymbolList_O::static_className() );
        core::SymbolList_O::_nil = nil_for_class;
        classcore__SymbolList_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SymbolList_Oval->setSupportsSlots(core::SymbolList_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SymbolSet_Oval]"));
    core::BuiltInClass_sp classcore__SymbolSet_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SymbolSet_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SymbolSet_Oval,_lisp,core::SymbolSet_O::static_classSymbol());
    core::SymbolSet_O::___staticMetaClass = classcore__SymbolSet_Oval;
    _lisp->setf_findClass(core::SymbolSet_O::static_classSymbol(),classcore__SymbolSet_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SymbolSet_O>;
        core::SymbolSet_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SymbolSet_O::static_className() % (void*)(core::SymbolSet_O::static_newNil_callback()) );
    classcore__SymbolSet_Oval->setInstance_newNil_callback(core::SymbolSet_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SymbolSet_O> nil_for_class(new core::SymbolSet_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SymbolSet_O::static_className() );
        core::SymbolSet_O::_nil = nil_for_class;
        classcore__SymbolSet_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SymbolSet_Oval->setSupportsSlots(core::SymbolSet_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SymbolToEnumConverter_Oval]"));
    core::BuiltInClass_sp classcore__SymbolToEnumConverter_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SymbolToEnumConverter_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SymbolToEnumConverter_Oval,_lisp,core::SymbolToEnumConverter_O::static_classSymbol());
    core::SymbolToEnumConverter_O::___staticMetaClass = classcore__SymbolToEnumConverter_Oval;
    _lisp->setf_findClass(core::SymbolToEnumConverter_O::static_classSymbol(),classcore__SymbolToEnumConverter_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SymbolToEnumConverter_O>;
        core::SymbolToEnumConverter_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SymbolToEnumConverter_O::static_className() % (void*)(core::SymbolToEnumConverter_O::static_newNil_callback()) );
    classcore__SymbolToEnumConverter_Oval->setInstance_newNil_callback(core::SymbolToEnumConverter_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SymbolToEnumConverter_O> nil_for_class(new core::SymbolToEnumConverter_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SymbolToEnumConverter_O::static_className() );
        core::SymbolToEnumConverter_O::_nil = nil_for_class;
        classcore__SymbolToEnumConverter_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SymbolToEnumConverter_Oval->setSupportsSlots(core::SymbolToEnumConverter_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SymbolVectorMap_Oval]"));
    core::BuiltInClass_sp classcore__SymbolVectorMap_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SymbolVectorMap_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SymbolVectorMap_Oval,_lisp,core::SymbolVectorMap_O::static_classSymbol());
    core::SymbolVectorMap_O::___staticMetaClass = classcore__SymbolVectorMap_Oval;
    _lisp->setf_findClass(core::SymbolVectorMap_O::static_classSymbol(),classcore__SymbolVectorMap_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SymbolVectorMap_O>;
        core::SymbolVectorMap_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SymbolVectorMap_O::static_className() % (void*)(core::SymbolVectorMap_O::static_newNil_callback()) );
    classcore__SymbolVectorMap_Oval->setInstance_newNil_callback(core::SymbolVectorMap_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SymbolVectorMap_O> nil_for_class(new core::SymbolVectorMap_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SymbolVectorMap_O::static_className() );
        core::SymbolVectorMap_O::_nil = nil_for_class;
        classcore__SymbolVectorMap_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SymbolVectorMap_Oval->setSupportsSlots(core::SymbolVectorMap_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SymbolVector_Oval]"));
    core::BuiltInClass_sp classcore__SymbolVector_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SymbolVector_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SymbolVector_Oval,_lisp,core::SymbolVector_O::static_classSymbol());
    core::SymbolVector_O::___staticMetaClass = classcore__SymbolVector_Oval;
    _lisp->setf_findClass(core::SymbolVector_O::static_classSymbol(),classcore__SymbolVector_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SymbolVector_O>;
        core::SymbolVector_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SymbolVector_O::static_className() % (void*)(core::SymbolVector_O::static_newNil_callback()) );
    classcore__SymbolVector_Oval->setInstance_newNil_callback(core::SymbolVector_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SymbolVector_O> nil_for_class(new core::SymbolVector_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SymbolVector_O::static_className() );
        core::SymbolVector_O::_nil = nil_for_class;
        classcore__SymbolVector_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SymbolVector_Oval->setSupportsSlots(core::SymbolVector_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Symbol_Oval]"));
    core::BuiltInClass_sp classcore__Symbol_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Symbol_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Symbol_Oval,_lisp,core::Symbol_O::static_classSymbol());
    core::Symbol_O::___staticMetaClass = classcore__Symbol_Oval;
    _lisp->setf_findClass(core::Symbol_O::static_classSymbol(),classcore__Symbol_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Symbol_O>;
        core::Symbol_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Symbol_O::static_className() % (void*)(core::Symbol_O::static_newNil_callback()) );
    classcore__Symbol_Oval->setInstance_newNil_callback(core::Symbol_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Symbol_O> nil_for_class(new core::Symbol_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Symbol_O::static_className() );
        core::Symbol_O::_nil = nil_for_class;
        classcore__Symbol_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Symbol_Oval->setSupportsSlots(core::Symbol_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__WeakReference_Oval]"));
    core::BuiltInClass_sp classcore__WeakReference_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__WeakReference_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__WeakReference_Oval,_lisp,core::WeakReference_O::static_classSymbol());
    core::WeakReference_O::___staticMetaClass = classcore__WeakReference_Oval;
    _lisp->setf_findClass(core::WeakReference_O::static_classSymbol(),classcore__WeakReference_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::WeakReference_O>;
        core::WeakReference_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::WeakReference_O::static_className() % (void*)(core::WeakReference_O::static_newNil_callback()) );
    classcore__WeakReference_Oval->setInstance_newNil_callback(core::WeakReference_O::static_newNil_callback());
    {
        boost::shared_ptr<core::WeakReference_O> nil_for_class(new core::WeakReference_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::WeakReference_O::static_className() );
        core::WeakReference_O::_nil = nil_for_class;
        classcore__WeakReference_Oval->setInstanceNil(nil_for_class);
    }
    classcore__WeakReference_Oval->setSupportsSlots(core::WeakReference_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ActivationFrame_Oval]"));
    core::BuiltInClass_sp classcore__ActivationFrame_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ActivationFrame_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ActivationFrame_Oval,_lisp,core::ActivationFrame_O::static_classSymbol());
    core::ActivationFrame_O::___staticMetaClass = classcore__ActivationFrame_Oval;
    _lisp->setf_findClass(core::ActivationFrame_O::static_classSymbol(),classcore__ActivationFrame_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ActivationFrame_O>;
        core::ActivationFrame_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ActivationFrame_O::static_className() % (void*)(core::ActivationFrame_O::static_newNil_callback()) );
    classcore__ActivationFrame_Oval->setInstance_newNil_callback(core::ActivationFrame_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ActivationFrame_O> nil_for_class(new core::ActivationFrame_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ActivationFrame_O::static_className() );
        core::ActivationFrame_O::_nil = nil_for_class;
        classcore__ActivationFrame_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ActivationFrame_Oval->setSupportsSlots(core::ActivationFrame_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Array_Oval]"));
    core::BuiltInClass_sp classcore__Array_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Array_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Array_Oval,_lisp,core::Array_O::static_classSymbol());
    core::Array_O::___staticMetaClass = classcore__Array_Oval;
    _lisp->setf_findClass(core::Array_O::static_classSymbol(),classcore__Array_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Array_O>;
        core::Array_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Array_O::static_className() % (void*)(core::Array_O::static_newNil_callback()) );
    classcore__Array_Oval->setInstance_newNil_callback(core::Array_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Array_O> nil_for_class(new core::Array_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Array_O::static_className() );
        core::Array_O::_nil = nil_for_class;
        classcore__Array_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Array_Oval->setSupportsSlots(core::Array_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__BaseChar_Oval]"));
    core::BuiltInClass_sp classcore__BaseChar_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__BaseChar_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__BaseChar_Oval,_lisp,core::BaseChar_O::static_classSymbol());
    core::BaseChar_O::___staticMetaClass = classcore__BaseChar_Oval;
    _lisp->setf_findClass(core::BaseChar_O::static_classSymbol(),classcore__BaseChar_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::BaseChar_O>;
        core::BaseChar_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::BaseChar_O::static_className() % (void*)(core::BaseChar_O::static_newNil_callback()) );
    classcore__BaseChar_Oval->setInstance_newNil_callback(core::BaseChar_O::static_newNil_callback());
    {
        boost::shared_ptr<core::BaseChar_O> nil_for_class(new core::BaseChar_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::BaseChar_O::static_className() );
        core::BaseChar_O::_nil = nil_for_class;
        classcore__BaseChar_Oval->setInstanceNil(nil_for_class);
    }
    classcore__BaseChar_Oval->setSupportsSlots(core::BaseChar_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Bool_Oval]"));
    core::BuiltInClass_sp classcore__Bool_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Bool_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Bool_Oval,_lisp,core::Bool_O::static_classSymbol());
    core::Bool_O::___staticMetaClass = classcore__Bool_Oval;
    _lisp->setf_findClass(core::Bool_O::static_classSymbol(),classcore__Bool_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Bool_O>;
        core::Bool_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Bool_O::static_className() % (void*)(core::Bool_O::static_newNil_callback()) );
    classcore__Bool_Oval->setInstance_newNil_callback(core::Bool_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Bool_O> nil_for_class(new core::Bool_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Bool_O::static_className() );
        core::Bool_O::_nil = nil_for_class;
        classcore__Bool_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Bool_Oval->setSupportsSlots(core::Bool_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__BroadcastStream_Oval]"));
    core::BuiltInClass_sp classcore__BroadcastStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__BroadcastStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__BroadcastStream_Oval,_lisp,core::BroadcastStream_O::static_classSymbol());
    core::BroadcastStream_O::___staticMetaClass = classcore__BroadcastStream_Oval;
    _lisp->setf_findClass(core::BroadcastStream_O::static_classSymbol(),classcore__BroadcastStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::BroadcastStream_O>;
        core::BroadcastStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::BroadcastStream_O::static_className() % (void*)(core::BroadcastStream_O::static_newNil_callback()) );
    classcore__BroadcastStream_Oval->setInstance_newNil_callback(core::BroadcastStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::BroadcastStream_O> nil_for_class(new core::BroadcastStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::BroadcastStream_O::static_className() );
        core::BroadcastStream_O::_nil = nil_for_class;
        classcore__BroadcastStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__BroadcastStream_Oval->setSupportsSlots(core::BroadcastStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__BuiltIn_Oval]"));
    core::BuiltInClass_sp classcore__BuiltIn_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__BuiltIn_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__BuiltIn_Oval,_lisp,core::BuiltIn_O::static_classSymbol());
    core::BuiltIn_O::___staticMetaClass = classcore__BuiltIn_Oval;
    _lisp->setf_findClass(core::BuiltIn_O::static_classSymbol(),classcore__BuiltIn_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::BuiltIn_O>;
        core::BuiltIn_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::BuiltIn_O::static_className() % (void*)(core::BuiltIn_O::static_newNil_callback()) );
    classcore__BuiltIn_Oval->setInstance_newNil_callback(core::BuiltIn_O::static_newNil_callback());
    {
        boost::shared_ptr<core::BuiltIn_O> nil_for_class(new core::BuiltIn_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::BuiltIn_O::static_className() );
        core::BuiltIn_O::_nil = nil_for_class;
        classcore__BuiltIn_Oval->setInstanceNil(nil_for_class);
    }
    classcore__BuiltIn_Oval->setSupportsSlots(core::BuiltIn_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Closure_Oval]"));
    core::BuiltInClass_sp classcore__Closure_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Closure_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Closure_Oval,_lisp,core::Closure_O::static_classSymbol());
    core::Closure_O::___staticMetaClass = classcore__Closure_Oval;
    _lisp->setf_findClass(core::Closure_O::static_classSymbol(),classcore__Closure_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Closure_O>;
        core::Closure_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Closure_O::static_className() % (void*)(core::Closure_O::static_newNil_callback()) );
    classcore__Closure_Oval->setInstance_newNil_callback(core::Closure_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Closure_O> nil_for_class(new core::Closure_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Closure_O::static_className() );
        core::Closure_O::_nil = nil_for_class;
        classcore__Closure_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Closure_Oval->setSupportsSlots(core::Closure_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Complex_Oval]"));
    core::BuiltInClass_sp classcore__Complex_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Complex_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Complex_Oval,_lisp,core::Complex_O::static_classSymbol());
    core::Complex_O::___staticMetaClass = classcore__Complex_Oval;
    _lisp->setf_findClass(core::Complex_O::static_classSymbol(),classcore__Complex_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Complex_O>;
        core::Complex_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Complex_O::static_className() % (void*)(core::Complex_O::static_newNil_callback()) );
    classcore__Complex_Oval->setInstance_newNil_callback(core::Complex_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Complex_O> nil_for_class(new core::Complex_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Complex_O::static_className() );
        core::Complex_O::_nil = nil_for_class;
        classcore__Complex_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Complex_Oval->setSupportsSlots(core::Complex_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ConcatenatedStream_Oval]"));
    core::BuiltInClass_sp classcore__ConcatenatedStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ConcatenatedStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ConcatenatedStream_Oval,_lisp,core::ConcatenatedStream_O::static_classSymbol());
    core::ConcatenatedStream_O::___staticMetaClass = classcore__ConcatenatedStream_Oval;
    _lisp->setf_findClass(core::ConcatenatedStream_O::static_classSymbol(),classcore__ConcatenatedStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ConcatenatedStream_O>;
        core::ConcatenatedStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ConcatenatedStream_O::static_className() % (void*)(core::ConcatenatedStream_O::static_newNil_callback()) );
    classcore__ConcatenatedStream_Oval->setInstance_newNil_callback(core::ConcatenatedStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ConcatenatedStream_O> nil_for_class(new core::ConcatenatedStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ConcatenatedStream_O::static_className() );
        core::ConcatenatedStream_O::_nil = nil_for_class;
        classcore__ConcatenatedStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ConcatenatedStream_Oval->setSupportsSlots(core::ConcatenatedStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ConditionWithStackTrace_Oval]"));
    core::BuiltInClass_sp classcore__ConditionWithStackTrace_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ConditionWithStackTrace_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ConditionWithStackTrace_Oval,_lisp,core::ConditionWithStackTrace_O::static_classSymbol());
    core::ConditionWithStackTrace_O::___staticMetaClass = classcore__ConditionWithStackTrace_Oval;
    _lisp->setf_findClass(core::ConditionWithStackTrace_O::static_classSymbol(),classcore__ConditionWithStackTrace_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ConditionWithStackTrace_O>;
        core::ConditionWithStackTrace_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ConditionWithStackTrace_O::static_className() % (void*)(core::ConditionWithStackTrace_O::static_newNil_callback()) );
    classcore__ConditionWithStackTrace_Oval->setInstance_newNil_callback(core::ConditionWithStackTrace_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ConditionWithStackTrace_O> nil_for_class(new core::ConditionWithStackTrace_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ConditionWithStackTrace_O::static_className() );
        core::ConditionWithStackTrace_O::_nil = nil_for_class;
        classcore__ConditionWithStackTrace_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ConditionWithStackTrace_Oval->setSupportsSlots(core::ConditionWithStackTrace_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__DirectoryIterator_Oval]"));
    core::BuiltInClass_sp classcore__DirectoryIterator_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__DirectoryIterator_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__DirectoryIterator_Oval,_lisp,core::DirectoryIterator_O::static_classSymbol());
    core::DirectoryIterator_O::___staticMetaClass = classcore__DirectoryIterator_Oval;
    _lisp->setf_findClass(core::DirectoryIterator_O::static_classSymbol(),classcore__DirectoryIterator_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::DirectoryIterator_O>;
        core::DirectoryIterator_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::DirectoryIterator_O::static_className() % (void*)(core::DirectoryIterator_O::static_newNil_callback()) );
    classcore__DirectoryIterator_Oval->setInstance_newNil_callback(core::DirectoryIterator_O::static_newNil_callback());
    {
        boost::shared_ptr<core::DirectoryIterator_O> nil_for_class(new core::DirectoryIterator_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::DirectoryIterator_O::static_className() );
        core::DirectoryIterator_O::_nil = nil_for_class;
        classcore__DirectoryIterator_Oval->setInstanceNil(nil_for_class);
    }
    classcore__DirectoryIterator_Oval->setSupportsSlots(core::DirectoryIterator_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__EchoStream_Oval]"));
    core::BuiltInClass_sp classcore__EchoStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__EchoStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__EchoStream_Oval,_lisp,core::EchoStream_O::static_classSymbol());
    core::EchoStream_O::___staticMetaClass = classcore__EchoStream_Oval;
    _lisp->setf_findClass(core::EchoStream_O::static_classSymbol(),classcore__EchoStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::EchoStream_O>;
        core::EchoStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::EchoStream_O::static_className() % (void*)(core::EchoStream_O::static_newNil_callback()) );
    classcore__EchoStream_Oval->setInstance_newNil_callback(core::EchoStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::EchoStream_O> nil_for_class(new core::EchoStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::EchoStream_O::static_className() );
        core::EchoStream_O::_nil = nil_for_class;
        classcore__EchoStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__EchoStream_Oval->setSupportsSlots(core::EchoStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ExtendedChar_Oval]"));
    core::BuiltInClass_sp classcore__ExtendedChar_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ExtendedChar_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ExtendedChar_Oval,_lisp,core::ExtendedChar_O::static_classSymbol());
    core::ExtendedChar_O::___staticMetaClass = classcore__ExtendedChar_Oval;
    _lisp->setf_findClass(core::ExtendedChar_O::static_classSymbol(),classcore__ExtendedChar_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ExtendedChar_O>;
        core::ExtendedChar_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ExtendedChar_O::static_className() % (void*)(core::ExtendedChar_O::static_newNil_callback()) );
    classcore__ExtendedChar_Oval->setInstance_newNil_callback(core::ExtendedChar_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ExtendedChar_O> nil_for_class(new core::ExtendedChar_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ExtendedChar_O::static_className() );
        core::ExtendedChar_O::_nil = nil_for_class;
        classcore__ExtendedChar_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ExtendedChar_Oval->setSupportsSlots(core::ExtendedChar_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FDStream_Oval]"));
    core::BuiltInClass_sp classcore__FDStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FDStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FDStream_Oval,_lisp,core::FDStream_O::static_classSymbol());
    core::FDStream_O::___staticMetaClass = classcore__FDStream_Oval;
    _lisp->setf_findClass(core::FDStream_O::static_classSymbol(),classcore__FDStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FDStream_O>;
        core::FDStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FDStream_O::static_className() % (void*)(core::FDStream_O::static_newNil_callback()) );
    classcore__FDStream_Oval->setInstance_newNil_callback(core::FDStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FDStream_O> nil_for_class(new core::FDStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FDStream_O::static_className() );
        core::FDStream_O::_nil = nil_for_class;
        classcore__FDStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FDStream_Oval->setSupportsSlots(core::FDStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FileStream_Oval]"));
    core::BuiltInClass_sp classcore__FileStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FileStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FileStream_Oval,_lisp,core::FileStream_O::static_classSymbol());
    core::FileStream_O::___staticMetaClass = classcore__FileStream_Oval;
    _lisp->setf_findClass(core::FileStream_O::static_classSymbol(),classcore__FileStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FileStream_O>;
        core::FileStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FileStream_O::static_className() % (void*)(core::FileStream_O::static_newNil_callback()) );
    classcore__FileStream_Oval->setInstance_newNil_callback(core::FileStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FileStream_O> nil_for_class(new core::FileStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FileStream_O::static_className() );
        core::FileStream_O::_nil = nil_for_class;
        classcore__FileStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FileStream_Oval->setSupportsSlots(core::FileStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FunctionNameSetfExpander_Oval]"));
    core::BuiltInClass_sp classcore__FunctionNameSetfExpander_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FunctionNameSetfExpander_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FunctionNameSetfExpander_Oval,_lisp,core::FunctionNameSetfExpander_O::static_classSymbol());
    core::FunctionNameSetfExpander_O::___staticMetaClass = classcore__FunctionNameSetfExpander_Oval;
    _lisp->setf_findClass(core::FunctionNameSetfExpander_O::static_classSymbol(),classcore__FunctionNameSetfExpander_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FunctionNameSetfExpander_O>;
        core::FunctionNameSetfExpander_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FunctionNameSetfExpander_O::static_className() % (void*)(core::FunctionNameSetfExpander_O::static_newNil_callback()) );
    classcore__FunctionNameSetfExpander_Oval->setInstance_newNil_callback(core::FunctionNameSetfExpander_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FunctionNameSetfExpander_O> nil_for_class(new core::FunctionNameSetfExpander_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FunctionNameSetfExpander_O::static_className() );
        core::FunctionNameSetfExpander_O::_nil = nil_for_class;
        classcore__FunctionNameSetfExpander_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FunctionNameSetfExpander_Oval->setSupportsSlots(core::FunctionNameSetfExpander_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FunctionSetfExpander_Oval]"));
    core::BuiltInClass_sp classcore__FunctionSetfExpander_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FunctionSetfExpander_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FunctionSetfExpander_Oval,_lisp,core::FunctionSetfExpander_O::static_classSymbol());
    core::FunctionSetfExpander_O::___staticMetaClass = classcore__FunctionSetfExpander_Oval;
    _lisp->setf_findClass(core::FunctionSetfExpander_O::static_classSymbol(),classcore__FunctionSetfExpander_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FunctionSetfExpander_O>;
        core::FunctionSetfExpander_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FunctionSetfExpander_O::static_className() % (void*)(core::FunctionSetfExpander_O::static_newNil_callback()) );
    classcore__FunctionSetfExpander_Oval->setInstance_newNil_callback(core::FunctionSetfExpander_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FunctionSetfExpander_O> nil_for_class(new core::FunctionSetfExpander_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FunctionSetfExpander_O::static_className() );
        core::FunctionSetfExpander_O::_nil = nil_for_class;
        classcore__FunctionSetfExpander_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FunctionSetfExpander_Oval->setSupportsSlots(core::FunctionSetfExpander_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__GenericFunction_Oval]"));
    core::BuiltInClass_sp classcore__GenericFunction_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__GenericFunction_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__GenericFunction_Oval,_lisp,core::GenericFunction_O::static_classSymbol());
    core::GenericFunction_O::___staticMetaClass = classcore__GenericFunction_Oval;
    _lisp->setf_findClass(core::GenericFunction_O::static_classSymbol(),classcore__GenericFunction_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::GenericFunction_O>;
        core::GenericFunction_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::GenericFunction_O::static_className() % (void*)(core::GenericFunction_O::static_newNil_callback()) );
    classcore__GenericFunction_Oval->setInstance_newNil_callback(core::GenericFunction_O::static_newNil_callback());
    {
        boost::shared_ptr<core::GenericFunction_O> nil_for_class(new core::GenericFunction_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::GenericFunction_O::static_className() );
        core::GenericFunction_O::_nil = nil_for_class;
        classcore__GenericFunction_Oval->setInstanceNil(nil_for_class);
    }
    classcore__GenericFunction_Oval->setSupportsSlots(core::GenericFunction_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__GlueEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__GlueEnvironment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__GlueEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__GlueEnvironment_Oval,_lisp,core::GlueEnvironment_O::static_classSymbol());
    core::GlueEnvironment_O::___staticMetaClass = classcore__GlueEnvironment_Oval;
    _lisp->setf_findClass(core::GlueEnvironment_O::static_classSymbol(),classcore__GlueEnvironment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::GlueEnvironment_O>;
        core::GlueEnvironment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::GlueEnvironment_O::static_className() % (void*)(core::GlueEnvironment_O::static_newNil_callback()) );
    classcore__GlueEnvironment_Oval->setInstance_newNil_callback(core::GlueEnvironment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::GlueEnvironment_O> nil_for_class(new core::GlueEnvironment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::GlueEnvironment_O::static_className() );
        core::GlueEnvironment_O::_nil = nil_for_class;
        classcore__GlueEnvironment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__GlueEnvironment_Oval->setSupportsSlots(core::GlueEnvironment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__HandlerBindHandler_Oval]"));
    core::BuiltInClass_sp classcore__HandlerBindHandler_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__HandlerBindHandler_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__HandlerBindHandler_Oval,_lisp,core::HandlerBindHandler_O::static_classSymbol());
    core::HandlerBindHandler_O::___staticMetaClass = classcore__HandlerBindHandler_Oval;
    _lisp->setf_findClass(core::HandlerBindHandler_O::static_classSymbol(),classcore__HandlerBindHandler_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::HandlerBindHandler_O>;
        core::HandlerBindHandler_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::HandlerBindHandler_O::static_className() % (void*)(core::HandlerBindHandler_O::static_newNil_callback()) );
    classcore__HandlerBindHandler_Oval->setInstance_newNil_callback(core::HandlerBindHandler_O::static_newNil_callback());
    {
        boost::shared_ptr<core::HandlerBindHandler_O> nil_for_class(new core::HandlerBindHandler_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::HandlerBindHandler_O::static_className() );
        core::HandlerBindHandler_O::_nil = nil_for_class;
        classcore__HandlerBindHandler_Oval->setInstanceNil(nil_for_class);
    }
    classcore__HandlerBindHandler_Oval->setSupportsSlots(core::HandlerBindHandler_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__HandlerCaseHandler_Oval]"));
    core::BuiltInClass_sp classcore__HandlerCaseHandler_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__HandlerCaseHandler_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__HandlerCaseHandler_Oval,_lisp,core::HandlerCaseHandler_O::static_classSymbol());
    core::HandlerCaseHandler_O::___staticMetaClass = classcore__HandlerCaseHandler_Oval;
    _lisp->setf_findClass(core::HandlerCaseHandler_O::static_classSymbol(),classcore__HandlerCaseHandler_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::HandlerCaseHandler_O>;
        core::HandlerCaseHandler_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::HandlerCaseHandler_O::static_className() % (void*)(core::HandlerCaseHandler_O::static_newNil_callback()) );
    classcore__HandlerCaseHandler_Oval->setInstance_newNil_callback(core::HandlerCaseHandler_O::static_newNil_callback());
    {
        boost::shared_ptr<core::HandlerCaseHandler_O> nil_for_class(new core::HandlerCaseHandler_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::HandlerCaseHandler_O::static_className() );
        core::HandlerCaseHandler_O::_nil = nil_for_class;
        classcore__HandlerCaseHandler_Oval->setInstanceNil(nil_for_class);
    }
    classcore__HandlerCaseHandler_Oval->setSupportsSlots(core::HandlerCaseHandler_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__HashTableEq_Oval]"));
    core::BuiltInClass_sp classcore__HashTableEq_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__HashTableEq_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__HashTableEq_Oval,_lisp,core::HashTableEq_O::static_classSymbol());
    core::HashTableEq_O::___staticMetaClass = classcore__HashTableEq_Oval;
    _lisp->setf_findClass(core::HashTableEq_O::static_classSymbol(),classcore__HashTableEq_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::HashTableEq_O>;
        core::HashTableEq_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::HashTableEq_O::static_className() % (void*)(core::HashTableEq_O::static_newNil_callback()) );
    classcore__HashTableEq_Oval->setInstance_newNil_callback(core::HashTableEq_O::static_newNil_callback());
    {
        boost::shared_ptr<core::HashTableEq_O> nil_for_class(new core::HashTableEq_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::HashTableEq_O::static_className() );
        core::HashTableEq_O::_nil = nil_for_class;
        classcore__HashTableEq_Oval->setInstanceNil(nil_for_class);
    }
    classcore__HashTableEq_Oval->setSupportsSlots(core::HashTableEq_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__HashTableEql_Oval]"));
    core::BuiltInClass_sp classcore__HashTableEql_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__HashTableEql_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__HashTableEql_Oval,_lisp,core::HashTableEql_O::static_classSymbol());
    core::HashTableEql_O::___staticMetaClass = classcore__HashTableEql_Oval;
    _lisp->setf_findClass(core::HashTableEql_O::static_classSymbol(),classcore__HashTableEql_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::HashTableEql_O>;
        core::HashTableEql_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::HashTableEql_O::static_className() % (void*)(core::HashTableEql_O::static_newNil_callback()) );
    classcore__HashTableEql_Oval->setInstance_newNil_callback(core::HashTableEql_O::static_newNil_callback());
    {
        boost::shared_ptr<core::HashTableEql_O> nil_for_class(new core::HashTableEql_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::HashTableEql_O::static_className() );
        core::HashTableEql_O::_nil = nil_for_class;
        classcore__HashTableEql_Oval->setInstanceNil(nil_for_class);
    }
    classcore__HashTableEql_Oval->setSupportsSlots(core::HashTableEql_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__HashTableEqual_Oval]"));
    core::BuiltInClass_sp classcore__HashTableEqual_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__HashTableEqual_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__HashTableEqual_Oval,_lisp,core::HashTableEqual_O::static_classSymbol());
    core::HashTableEqual_O::___staticMetaClass = classcore__HashTableEqual_Oval;
    _lisp->setf_findClass(core::HashTableEqual_O::static_classSymbol(),classcore__HashTableEqual_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::HashTableEqual_O>;
        core::HashTableEqual_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::HashTableEqual_O::static_className() % (void*)(core::HashTableEqual_O::static_newNil_callback()) );
    classcore__HashTableEqual_Oval->setInstance_newNil_callback(core::HashTableEqual_O::static_newNil_callback());
    {
        boost::shared_ptr<core::HashTableEqual_O> nil_for_class(new core::HashTableEqual_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::HashTableEqual_O::static_className() );
        core::HashTableEqual_O::_nil = nil_for_class;
        classcore__HashTableEqual_Oval->setInstanceNil(nil_for_class);
    }
    classcore__HashTableEqual_Oval->setSupportsSlots(core::HashTableEqual_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__HashTableEqualp_Oval]"));
    core::BuiltInClass_sp classcore__HashTableEqualp_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__HashTableEqualp_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__HashTableEqualp_Oval,_lisp,core::HashTableEqualp_O::static_classSymbol());
    core::HashTableEqualp_O::___staticMetaClass = classcore__HashTableEqualp_Oval;
    _lisp->setf_findClass(core::HashTableEqualp_O::static_classSymbol(),classcore__HashTableEqualp_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::HashTableEqualp_O>;
        core::HashTableEqualp_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::HashTableEqualp_O::static_className() % (void*)(core::HashTableEqualp_O::static_newNil_callback()) );
    classcore__HashTableEqualp_Oval->setInstance_newNil_callback(core::HashTableEqualp_O::static_newNil_callback());
    {
        boost::shared_ptr<core::HashTableEqualp_O> nil_for_class(new core::HashTableEqualp_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::HashTableEqualp_O::static_className() );
        core::HashTableEqualp_O::_nil = nil_for_class;
        classcore__HashTableEqualp_Oval->setInstanceNil(nil_for_class);
    }
    classcore__HashTableEqualp_Oval->setSupportsSlots(core::HashTableEqualp_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Interpreted_Oval]"));
    core::BuiltInClass_sp classcore__Interpreted_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Interpreted_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Interpreted_Oval,_lisp,core::Interpreted_O::static_classSymbol());
    core::Interpreted_O::___staticMetaClass = classcore__Interpreted_Oval;
    _lisp->setf_findClass(core::Interpreted_O::static_classSymbol(),classcore__Interpreted_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Interpreted_O>;
        core::Interpreted_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Interpreted_O::static_className() % (void*)(core::Interpreted_O::static_newNil_callback()) );
    classcore__Interpreted_Oval->setInstance_newNil_callback(core::Interpreted_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Interpreted_O> nil_for_class(new core::Interpreted_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Interpreted_O::static_className() );
        core::Interpreted_O::_nil = nil_for_class;
        classcore__Interpreted_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Interpreted_Oval->setSupportsSlots(core::Interpreted_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__LexicalEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__LexicalEnvironment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__LexicalEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__LexicalEnvironment_Oval,_lisp,core::LexicalEnvironment_O::static_classSymbol());
    core::LexicalEnvironment_O::___staticMetaClass = classcore__LexicalEnvironment_Oval;
    _lisp->setf_findClass(core::LexicalEnvironment_O::static_classSymbol(),classcore__LexicalEnvironment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::LexicalEnvironment_O>;
        core::LexicalEnvironment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::LexicalEnvironment_O::static_className() % (void*)(core::LexicalEnvironment_O::static_newNil_callback()) );
    classcore__LexicalEnvironment_Oval->setInstance_newNil_callback(core::LexicalEnvironment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::LexicalEnvironment_O> nil_for_class(new core::LexicalEnvironment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::LexicalEnvironment_O::static_className() );
        core::LexicalEnvironment_O::_nil = nil_for_class;
        classcore__LexicalEnvironment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__LexicalEnvironment_Oval->setSupportsSlots(core::LexicalEnvironment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__LoadArchive_Oval]"));
    core::BuiltInClass_sp classcore__LoadArchive_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__LoadArchive_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__LoadArchive_Oval,_lisp,core::LoadArchive_O::static_classSymbol());
    core::LoadArchive_O::___staticMetaClass = classcore__LoadArchive_Oval;
    _lisp->setf_findClass(core::LoadArchive_O::static_classSymbol(),classcore__LoadArchive_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::LoadArchive_O>;
        core::LoadArchive_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::LoadArchive_O::static_className() % (void*)(core::LoadArchive_O::static_newNil_callback()) );
    classcore__LoadArchive_Oval->setInstance_newNil_callback(core::LoadArchive_O::static_newNil_callback());
    {
        boost::shared_ptr<core::LoadArchive_O> nil_for_class(new core::LoadArchive_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::LoadArchive_O::static_className() );
        core::LoadArchive_O::_nil = nil_for_class;
        classcore__LoadArchive_Oval->setInstanceNil(nil_for_class);
    }
    classcore__LoadArchive_Oval->setSupportsSlots(core::LoadArchive_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__MetaClass_Oval]"));
    core::BuiltInClass_sp classcore__MetaClass_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__MetaClass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__MetaClass_Oval,_lisp,core::MetaClass_O::static_classSymbol());
    core::MetaClass_O::___staticMetaClass = classcore__MetaClass_Oval;
    _lisp->setf_findClass(core::MetaClass_O::static_classSymbol(),classcore__MetaClass_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::MetaClass_O>;
        core::MetaClass_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::MetaClass_O::static_className() % (void*)(core::MetaClass_O::static_newNil_callback()) );
    classcore__MetaClass_Oval->setInstance_newNil_callback(core::MetaClass_O::static_newNil_callback());
    {
        boost::shared_ptr<core::MetaClass_O> nil_for_class(new core::MetaClass_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::MetaClass_O::static_className() );
        core::MetaClass_O::_nil = nil_for_class;
        classcore__MetaClass_Oval->setInstanceNil(nil_for_class);
    }
    classcore__MetaClass_Oval->setSupportsSlots(core::MetaClass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__MetaObject_Oval]"));
    core::BuiltInClass_sp classcore__MetaObject_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__MetaObject_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__MetaObject_Oval,_lisp,core::MetaObject_O::static_classSymbol());
    core::MetaObject_O::___staticMetaClass = classcore__MetaObject_Oval;
    _lisp->setf_findClass(core::MetaObject_O::static_classSymbol(),classcore__MetaObject_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::MetaObject_O>;
        core::MetaObject_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::MetaObject_O::static_className() % (void*)(core::MetaObject_O::static_newNil_callback()) );
    classcore__MetaObject_Oval->setInstance_newNil_callback(core::MetaObject_O::static_newNil_callback());
    {
        boost::shared_ptr<core::MetaObject_O> nil_for_class(new core::MetaObject_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::MetaObject_O::static_className() );
        core::MetaObject_O::_nil = nil_for_class;
        classcore__MetaObject_Oval->setInstanceNil(nil_for_class);
    }
    classcore__MetaObject_Oval->setSupportsSlots(core::MetaObject_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ReadSerializer_Oval]"));
    core::BuiltInClass_sp classcore__ReadSerializer_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ReadSerializer_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ReadSerializer_Oval,_lisp,core::ReadSerializer_O::static_classSymbol());
    core::ReadSerializer_O::___staticMetaClass = classcore__ReadSerializer_Oval;
    _lisp->setf_findClass(core::ReadSerializer_O::static_classSymbol(),classcore__ReadSerializer_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ReadSerializer_O>;
        core::ReadSerializer_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ReadSerializer_O::static_className() % (void*)(core::ReadSerializer_O::static_newNil_callback()) );
    classcore__ReadSerializer_Oval->setInstance_newNil_callback(core::ReadSerializer_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ReadSerializer_O> nil_for_class(new core::ReadSerializer_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ReadSerializer_O::static_className() );
        core::ReadSerializer_O::_nil = nil_for_class;
        classcore__ReadSerializer_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ReadSerializer_Oval->setSupportsSlots(core::ReadSerializer_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Real_Oval]"));
    core::BuiltInClass_sp classcore__Real_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Real_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Real_Oval,_lisp,core::Real_O::static_classSymbol());
    core::Real_O::___staticMetaClass = classcore__Real_Oval;
    _lisp->setf_findClass(core::Real_O::static_classSymbol(),classcore__Real_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Real_O>;
        core::Real_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Real_O::static_className() % (void*)(core::Real_O::static_newNil_callback()) );
    classcore__Real_Oval->setInstance_newNil_callback(core::Real_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Real_O> nil_for_class(new core::Real_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Real_O::static_className() );
        core::Real_O::_nil = nil_for_class;
        classcore__Real_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Real_Oval->setSupportsSlots(core::Real_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__RecursiveDirectoryIterator_Oval]"));
    core::BuiltInClass_sp classcore__RecursiveDirectoryIterator_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__RecursiveDirectoryIterator_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__RecursiveDirectoryIterator_Oval,_lisp,core::RecursiveDirectoryIterator_O::static_classSymbol());
    core::RecursiveDirectoryIterator_O::___staticMetaClass = classcore__RecursiveDirectoryIterator_Oval;
    _lisp->setf_findClass(core::RecursiveDirectoryIterator_O::static_classSymbol(),classcore__RecursiveDirectoryIterator_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::RecursiveDirectoryIterator_O>;
        core::RecursiveDirectoryIterator_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::RecursiveDirectoryIterator_O::static_className() % (void*)(core::RecursiveDirectoryIterator_O::static_newNil_callback()) );
    classcore__RecursiveDirectoryIterator_Oval->setInstance_newNil_callback(core::RecursiveDirectoryIterator_O::static_newNil_callback());
    {
        boost::shared_ptr<core::RecursiveDirectoryIterator_O> nil_for_class(new core::RecursiveDirectoryIterator_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::RecursiveDirectoryIterator_O::static_className() );
        core::RecursiveDirectoryIterator_O::_nil = nil_for_class;
        classcore__RecursiveDirectoryIterator_Oval->setInstanceNil(nil_for_class);
    }
    classcore__RecursiveDirectoryIterator_Oval->setSupportsSlots(core::RecursiveDirectoryIterator_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SaveArchive_Oval]"));
    core::BuiltInClass_sp classcore__SaveArchive_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SaveArchive_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SaveArchive_Oval,_lisp,core::SaveArchive_O::static_classSymbol());
    core::SaveArchive_O::___staticMetaClass = classcore__SaveArchive_Oval;
    _lisp->setf_findClass(core::SaveArchive_O::static_classSymbol(),classcore__SaveArchive_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SaveArchive_O>;
        core::SaveArchive_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SaveArchive_O::static_className() % (void*)(core::SaveArchive_O::static_newNil_callback()) );
    classcore__SaveArchive_Oval->setInstance_newNil_callback(core::SaveArchive_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SaveArchive_O> nil_for_class(new core::SaveArchive_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SaveArchive_O::static_className() );
        core::SaveArchive_O::_nil = nil_for_class;
        classcore__SaveArchive_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SaveArchive_Oval->setSupportsSlots(core::SaveArchive_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Sequence_Oval]"));
    core::BuiltInClass_sp classcore__Sequence_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Sequence_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Sequence_Oval,_lisp,core::Sequence_O::static_classSymbol());
    core::Sequence_O::___staticMetaClass = classcore__Sequence_Oval;
    _lisp->setf_findClass(core::Sequence_O::static_classSymbol(),classcore__Sequence_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Sequence_O>;
        core::Sequence_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Sequence_O::static_className() % (void*)(core::Sequence_O::static_newNil_callback()) );
    classcore__Sequence_Oval->setInstance_newNil_callback(core::Sequence_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Sequence_O> nil_for_class(new core::Sequence_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Sequence_O::static_className() );
        core::Sequence_O::_nil = nil_for_class;
        classcore__Sequence_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Sequence_Oval->setSupportsSlots(core::Sequence_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SingleDispatchEffectiveMethodFunction_Oval]"));
    core::BuiltInClass_sp classcore__SingleDispatchEffectiveMethodFunction_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SingleDispatchEffectiveMethodFunction_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SingleDispatchEffectiveMethodFunction_Oval,_lisp,core::SingleDispatchEffectiveMethodFunction_O::static_classSymbol());
    core::SingleDispatchEffectiveMethodFunction_O::___staticMetaClass = classcore__SingleDispatchEffectiveMethodFunction_Oval;
    _lisp->setf_findClass(core::SingleDispatchEffectiveMethodFunction_O::static_classSymbol(),classcore__SingleDispatchEffectiveMethodFunction_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SingleDispatchEffectiveMethodFunction_O>;
        core::SingleDispatchEffectiveMethodFunction_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SingleDispatchEffectiveMethodFunction_O::static_className() % (void*)(core::SingleDispatchEffectiveMethodFunction_O::static_newNil_callback()) );
    classcore__SingleDispatchEffectiveMethodFunction_Oval->setInstance_newNil_callback(core::SingleDispatchEffectiveMethodFunction_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SingleDispatchEffectiveMethodFunction_O> nil_for_class(new core::SingleDispatchEffectiveMethodFunction_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SingleDispatchEffectiveMethodFunction_O::static_className() );
        core::SingleDispatchEffectiveMethodFunction_O::_nil = nil_for_class;
        classcore__SingleDispatchEffectiveMethodFunction_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SingleDispatchEffectiveMethodFunction_Oval->setSupportsSlots(core::SingleDispatchEffectiveMethodFunction_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SlotValueSetfExpander_Oval]"));
    core::BuiltInClass_sp classcore__SlotValueSetfExpander_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SlotValueSetfExpander_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SlotValueSetfExpander_Oval,_lisp,core::SlotValueSetfExpander_O::static_classSymbol());
    core::SlotValueSetfExpander_O::___staticMetaClass = classcore__SlotValueSetfExpander_Oval;
    _lisp->setf_findClass(core::SlotValueSetfExpander_O::static_classSymbol(),classcore__SlotValueSetfExpander_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SlotValueSetfExpander_O>;
        core::SlotValueSetfExpander_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SlotValueSetfExpander_O::static_className() % (void*)(core::SlotValueSetfExpander_O::static_newNil_callback()) );
    classcore__SlotValueSetfExpander_Oval->setInstance_newNil_callback(core::SlotValueSetfExpander_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SlotValueSetfExpander_O> nil_for_class(new core::SlotValueSetfExpander_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SlotValueSetfExpander_O::static_className() );
        core::SlotValueSetfExpander_O::_nil = nil_for_class;
        classcore__SlotValueSetfExpander_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SlotValueSetfExpander_Oval->setSupportsSlots(core::SlotValueSetfExpander_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SpecialForm_Oval]"));
    core::BuiltInClass_sp classcore__SpecialForm_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SpecialForm_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SpecialForm_Oval,_lisp,core::SpecialForm_O::static_classSymbol());
    core::SpecialForm_O::___staticMetaClass = classcore__SpecialForm_Oval;
    _lisp->setf_findClass(core::SpecialForm_O::static_classSymbol(),classcore__SpecialForm_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SpecialForm_O>;
        core::SpecialForm_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SpecialForm_O::static_className() % (void*)(core::SpecialForm_O::static_newNil_callback()) );
    classcore__SpecialForm_Oval->setInstance_newNil_callback(core::SpecialForm_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SpecialForm_O> nil_for_class(new core::SpecialForm_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SpecialForm_O::static_className() );
        core::SpecialForm_O::_nil = nil_for_class;
        classcore__SpecialForm_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SpecialForm_Oval->setSupportsSlots(core::SpecialForm_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__StringStream_Oval]"));
    core::BuiltInClass_sp classcore__StringStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__StringStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StringStream_Oval,_lisp,core::StringStream_O::static_classSymbol());
    core::StringStream_O::___staticMetaClass = classcore__StringStream_Oval;
    _lisp->setf_findClass(core::StringStream_O::static_classSymbol(),classcore__StringStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::StringStream_O>;
        core::StringStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::StringStream_O::static_className() % (void*)(core::StringStream_O::static_newNil_callback()) );
    classcore__StringStream_Oval->setInstance_newNil_callback(core::StringStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::StringStream_O> nil_for_class(new core::StringStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::StringStream_O::static_className() );
        core::StringStream_O::_nil = nil_for_class;
        classcore__StringStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__StringStream_Oval->setSupportsSlots(core::StringStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__StructureObject_Oval]"));
    StandardClass_sp classcore__StructureObject_Oval = StandardClass_O::create(undefinedMetaClass);
    classcore__StructureObject_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StructureObject_Oval,_lisp,core::StructureObject_O::static_classSymbol());
    core::StructureObject_O::___staticMetaClass = classcore__StructureObject_Oval;
    _lisp->setf_findClass(core::StructureObject_O::static_classSymbol(),classcore__StructureObject_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::StructureObject_O>;
        core::StructureObject_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::StructureObject_O::static_className() % (void*)(core::StructureObject_O::static_newNil_callback()) );
    classcore__StructureObject_Oval->setInstance_newNil_callback(core::StructureObject_O::static_newNil_callback());
    {
        boost::shared_ptr<core::StructureObject_O> nil_for_class(new core::StructureObject_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::StructureObject_O::static_className() );
        core::StructureObject_O::_nil = nil_for_class;
        classcore__StructureObject_Oval->setInstanceNil(nil_for_class);
    }
    classcore__StructureObject_Oval->setSupportsSlots(core::StructureObject_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SynonymStream_Oval]"));
    core::BuiltInClass_sp classcore__SynonymStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SynonymStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SynonymStream_Oval,_lisp,core::SynonymStream_O::static_classSymbol());
    core::SynonymStream_O::___staticMetaClass = classcore__SynonymStream_Oval;
    _lisp->setf_findClass(core::SynonymStream_O::static_classSymbol(),classcore__SynonymStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SynonymStream_O>;
        core::SynonymStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SynonymStream_O::static_className() % (void*)(core::SynonymStream_O::static_newNil_callback()) );
    classcore__SynonymStream_Oval->setInstance_newNil_callback(core::SynonymStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SynonymStream_O> nil_for_class(new core::SynonymStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SynonymStream_O::static_className() );
        core::SynonymStream_O::_nil = nil_for_class;
        classcore__SynonymStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SynonymStream_Oval->setSupportsSlots(core::SynonymStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__TwoWayStream_Oval]"));
    core::BuiltInClass_sp classcore__TwoWayStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__TwoWayStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__TwoWayStream_Oval,_lisp,core::TwoWayStream_O::static_classSymbol());
    core::TwoWayStream_O::___staticMetaClass = classcore__TwoWayStream_Oval;
    _lisp->setf_findClass(core::TwoWayStream_O::static_classSymbol(),classcore__TwoWayStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::TwoWayStream_O>;
        core::TwoWayStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::TwoWayStream_O::static_className() % (void*)(core::TwoWayStream_O::static_newNil_callback()) );
    classcore__TwoWayStream_Oval->setInstance_newNil_callback(core::TwoWayStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::TwoWayStream_O> nil_for_class(new core::TwoWayStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::TwoWayStream_O::static_className() );
        core::TwoWayStream_O::_nil = nil_for_class;
        classcore__TwoWayStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__TwoWayStream_Oval->setSupportsSlots(core::TwoWayStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__WriteSerializer_Oval]"));
    core::BuiltInClass_sp classcore__WriteSerializer_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__WriteSerializer_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__WriteSerializer_Oval,_lisp,core::WriteSerializer_O::static_classSymbol());
    core::WriteSerializer_O::___staticMetaClass = classcore__WriteSerializer_Oval;
    _lisp->setf_findClass(core::WriteSerializer_O::static_classSymbol(),classcore__WriteSerializer_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::WriteSerializer_O>;
        core::WriteSerializer_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::WriteSerializer_O::static_className() % (void*)(core::WriteSerializer_O::static_newNil_callback()) );
    classcore__WriteSerializer_Oval->setInstance_newNil_callback(core::WriteSerializer_O::static_newNil_callback());
    {
        boost::shared_ptr<core::WriteSerializer_O> nil_for_class(new core::WriteSerializer_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::WriteSerializer_O::static_className() );
        core::WriteSerializer_O::_nil = nil_for_class;
        classcore__WriteSerializer_Oval->setInstanceNil(nil_for_class);
    }
    classcore__WriteSerializer_Oval->setSupportsSlots(core::WriteSerializer_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ArrayObjects_Oval]"));
    core::BuiltInClass_sp classcore__ArrayObjects_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ArrayObjects_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ArrayObjects_Oval,_lisp,core::ArrayObjects_O::static_classSymbol());
    core::ArrayObjects_O::___staticMetaClass = classcore__ArrayObjects_Oval;
    _lisp->setf_findClass(core::ArrayObjects_O::static_classSymbol(),classcore__ArrayObjects_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ArrayObjects_O>;
        core::ArrayObjects_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ArrayObjects_O::static_className() % (void*)(core::ArrayObjects_O::static_newNil_callback()) );
    classcore__ArrayObjects_Oval->setInstance_newNil_callback(core::ArrayObjects_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ArrayObjects_O> nil_for_class(new core::ArrayObjects_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ArrayObjects_O::static_className() );
        core::ArrayObjects_O::_nil = nil_for_class;
        classcore__ArrayObjects_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ArrayObjects_Oval->setSupportsSlots(core::ArrayObjects_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__BuiltInClass_Oval]"));
    core::BuiltInClass_sp classcore__BuiltInClass_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__BuiltInClass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__BuiltInClass_Oval,_lisp,core::BuiltInClass_O::static_classSymbol());
    core::BuiltInClass_O::___staticMetaClass = classcore__BuiltInClass_Oval;
    _lisp->setf_findClass(core::BuiltInClass_O::static_classSymbol(),classcore__BuiltInClass_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::BuiltInClass_O>;
        core::BuiltInClass_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::BuiltInClass_O::static_className() % (void*)(core::BuiltInClass_O::static_newNil_callback()) );
    classcore__BuiltInClass_Oval->setInstance_newNil_callback(core::BuiltInClass_O::static_newNil_callback());
    {
        boost::shared_ptr<core::BuiltInClass_O> nil_for_class(new core::BuiltInClass_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::BuiltInClass_O::static_className() );
        core::BuiltInClass_O::_nil = nil_for_class;
        classcore__BuiltInClass_Oval->setInstanceNil(nil_for_class);
    }
    classcore__BuiltInClass_Oval->setSupportsSlots(core::BuiltInClass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__CandoException_Oval]"));
    core::BuiltInClass_sp classcore__CandoException_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__CandoException_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__CandoException_Oval,_lisp,core::CandoException_O::static_classSymbol());
    core::CandoException_O::___staticMetaClass = classcore__CandoException_Oval;
    _lisp->setf_findClass(core::CandoException_O::static_classSymbol(),classcore__CandoException_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::CandoException_O>;
        core::CandoException_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::CandoException_O::static_className() % (void*)(core::CandoException_O::static_newNil_callback()) );
    classcore__CandoException_Oval->setInstance_newNil_callback(core::CandoException_O::static_newNil_callback());
    {
        boost::shared_ptr<core::CandoException_O> nil_for_class(new core::CandoException_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::CandoException_O::static_className() );
        core::CandoException_O::_nil = nil_for_class;
        classcore__CandoException_Oval->setInstanceNil(nil_for_class);
    }
    classcore__CandoException_Oval->setSupportsSlots(core::CandoException_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__CandoFormatSaveArchive_Oval]"));
    core::BuiltInClass_sp classcore__CandoFormatSaveArchive_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__CandoFormatSaveArchive_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__CandoFormatSaveArchive_Oval,_lisp,core::CandoFormatSaveArchive_O::static_classSymbol());
    core::CandoFormatSaveArchive_O::___staticMetaClass = classcore__CandoFormatSaveArchive_Oval;
    _lisp->setf_findClass(core::CandoFormatSaveArchive_O::static_classSymbol(),classcore__CandoFormatSaveArchive_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::CandoFormatSaveArchive_O>;
        core::CandoFormatSaveArchive_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::CandoFormatSaveArchive_O::static_className() % (void*)(core::CandoFormatSaveArchive_O::static_newNil_callback()) );
    classcore__CandoFormatSaveArchive_Oval->setInstance_newNil_callback(core::CandoFormatSaveArchive_O::static_newNil_callback());
    {
        boost::shared_ptr<core::CandoFormatSaveArchive_O> nil_for_class(new core::CandoFormatSaveArchive_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::CandoFormatSaveArchive_O::static_className() );
        core::CandoFormatSaveArchive_O::_nil = nil_for_class;
        classcore__CandoFormatSaveArchive_Oval->setInstanceNil(nil_for_class);
    }
    classcore__CandoFormatSaveArchive_Oval->setSupportsSlots(core::CandoFormatSaveArchive_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__CompileTimeEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__CompileTimeEnvironment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__CompileTimeEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__CompileTimeEnvironment_Oval,_lisp,core::CompileTimeEnvironment_O::static_classSymbol());
    core::CompileTimeEnvironment_O::___staticMetaClass = classcore__CompileTimeEnvironment_Oval;
    _lisp->setf_findClass(core::CompileTimeEnvironment_O::static_classSymbol(),classcore__CompileTimeEnvironment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::CompileTimeEnvironment_O>;
        core::CompileTimeEnvironment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::CompileTimeEnvironment_O::static_className() % (void*)(core::CompileTimeEnvironment_O::static_newNil_callback()) );
    classcore__CompileTimeEnvironment_Oval->setInstance_newNil_callback(core::CompileTimeEnvironment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::CompileTimeEnvironment_O> nil_for_class(new core::CompileTimeEnvironment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::CompileTimeEnvironment_O::static_className() );
        core::CompileTimeEnvironment_O::_nil = nil_for_class;
        classcore__CompileTimeEnvironment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__CompileTimeEnvironment_Oval->setSupportsSlots(core::CompileTimeEnvironment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Condition_Oval]"));
    core::BuiltInClass_sp classcore__Condition_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Condition_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Condition_Oval,_lisp,core::Condition_O::static_classSymbol());
    core::Condition_O::___staticMetaClass = classcore__Condition_Oval;
    _lisp->setf_findClass(core::Condition_O::static_classSymbol(),classcore__Condition_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Condition_O>;
        core::Condition_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Condition_O::static_className() % (void*)(core::Condition_O::static_newNil_callback()) );
    classcore__Condition_Oval->setInstance_newNil_callback(core::Condition_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Condition_O> nil_for_class(new core::Condition_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Condition_O::static_className() );
        core::Condition_O::_nil = nil_for_class;
        classcore__Condition_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Condition_Oval->setSupportsSlots(core::Condition_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FDInStream_Oval]"));
    core::BuiltInClass_sp classcore__FDInStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FDInStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FDInStream_Oval,_lisp,core::FDInStream_O::static_classSymbol());
    core::FDInStream_O::___staticMetaClass = classcore__FDInStream_Oval;
    _lisp->setf_findClass(core::FDInStream_O::static_classSymbol(),classcore__FDInStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FDInStream_O>;
        core::FDInStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FDInStream_O::static_className() % (void*)(core::FDInStream_O::static_newNil_callback()) );
    classcore__FDInStream_Oval->setInstance_newNil_callback(core::FDInStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FDInStream_O> nil_for_class(new core::FDInStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FDInStream_O::static_className() );
        core::FDInStream_O::_nil = nil_for_class;
        classcore__FDInStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FDInStream_Oval->setSupportsSlots(core::FDInStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FDOutStream_Oval]"));
    core::BuiltInClass_sp classcore__FDOutStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FDOutStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FDOutStream_Oval,_lisp,core::FDOutStream_O::static_classSymbol());
    core::FDOutStream_O::___staticMetaClass = classcore__FDOutStream_Oval;
    _lisp->setf_findClass(core::FDOutStream_O::static_classSymbol(),classcore__FDOutStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FDOutStream_O>;
        core::FDOutStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FDOutStream_O::static_className() % (void*)(core::FDOutStream_O::static_newNil_callback()) );
    classcore__FDOutStream_Oval->setInstance_newNil_callback(core::FDOutStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FDOutStream_O> nil_for_class(new core::FDOutStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FDOutStream_O::static_className() );
        core::FDOutStream_O::_nil = nil_for_class;
        classcore__FDOutStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FDOutStream_Oval->setSupportsSlots(core::FDOutStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FileInCompressedStream_Oval]"));
    core::BuiltInClass_sp classcore__FileInCompressedStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FileInCompressedStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FileInCompressedStream_Oval,_lisp,core::FileInCompressedStream_O::static_classSymbol());
    core::FileInCompressedStream_O::___staticMetaClass = classcore__FileInCompressedStream_Oval;
    _lisp->setf_findClass(core::FileInCompressedStream_O::static_classSymbol(),classcore__FileInCompressedStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FileInCompressedStream_O>;
        core::FileInCompressedStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FileInCompressedStream_O::static_className() % (void*)(core::FileInCompressedStream_O::static_newNil_callback()) );
    classcore__FileInCompressedStream_Oval->setInstance_newNil_callback(core::FileInCompressedStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FileInCompressedStream_O> nil_for_class(new core::FileInCompressedStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FileInCompressedStream_O::static_className() );
        core::FileInCompressedStream_O::_nil = nil_for_class;
        classcore__FileInCompressedStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FileInCompressedStream_Oval->setSupportsSlots(core::FileInCompressedStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FileInStream_Oval]"));
    core::BuiltInClass_sp classcore__FileInStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FileInStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FileInStream_Oval,_lisp,core::FileInStream_O::static_classSymbol());
    core::FileInStream_O::___staticMetaClass = classcore__FileInStream_Oval;
    _lisp->setf_findClass(core::FileInStream_O::static_classSymbol(),classcore__FileInStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FileInStream_O>;
        core::FileInStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FileInStream_O::static_className() % (void*)(core::FileInStream_O::static_newNil_callback()) );
    classcore__FileInStream_Oval->setInstance_newNil_callback(core::FileInStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FileInStream_O> nil_for_class(new core::FileInStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FileInStream_O::static_className() );
        core::FileInStream_O::_nil = nil_for_class;
        classcore__FileInStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FileInStream_Oval->setSupportsSlots(core::FileInStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FileOutCompressedStream_Oval]"));
    core::BuiltInClass_sp classcore__FileOutCompressedStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FileOutCompressedStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FileOutCompressedStream_Oval,_lisp,core::FileOutCompressedStream_O::static_classSymbol());
    core::FileOutCompressedStream_O::___staticMetaClass = classcore__FileOutCompressedStream_Oval;
    _lisp->setf_findClass(core::FileOutCompressedStream_O::static_classSymbol(),classcore__FileOutCompressedStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FileOutCompressedStream_O>;
        core::FileOutCompressedStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FileOutCompressedStream_O::static_className() % (void*)(core::FileOutCompressedStream_O::static_newNil_callback()) );
    classcore__FileOutCompressedStream_Oval->setInstance_newNil_callback(core::FileOutCompressedStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FileOutCompressedStream_O> nil_for_class(new core::FileOutCompressedStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FileOutCompressedStream_O::static_className() );
        core::FileOutCompressedStream_O::_nil = nil_for_class;
        classcore__FileOutCompressedStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FileOutCompressedStream_Oval->setSupportsSlots(core::FileOutCompressedStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FileOutStream_Oval]"));
    core::BuiltInClass_sp classcore__FileOutStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FileOutStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FileOutStream_Oval,_lisp,core::FileOutStream_O::static_classSymbol());
    core::FileOutStream_O::___staticMetaClass = classcore__FileOutStream_Oval;
    _lisp->setf_findClass(core::FileOutStream_O::static_classSymbol(),classcore__FileOutStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FileOutStream_O>;
        core::FileOutStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FileOutStream_O::static_className() % (void*)(core::FileOutStream_O::static_newNil_callback()) );
    classcore__FileOutStream_Oval->setInstance_newNil_callback(core::FileOutStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FileOutStream_O> nil_for_class(new core::FileOutStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FileOutStream_O::static_className() );
        core::FileOutStream_O::_nil = nil_for_class;
        classcore__FileOutStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FileOutStream_Oval->setSupportsSlots(core::FileOutStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Float_Oval]"));
    core::BuiltInClass_sp classcore__Float_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Float_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Float_Oval,_lisp,core::Float_O::static_classSymbol());
    core::Float_O::___staticMetaClass = classcore__Float_Oval;
    _lisp->setf_findClass(core::Float_O::static_classSymbol(),classcore__Float_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Float_O>;
        core::Float_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Float_O::static_className() % (void*)(core::Float_O::static_newNil_callback()) );
    classcore__Float_Oval->setInstance_newNil_callback(core::Float_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Float_O> nil_for_class(new core::Float_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Float_O::static_className() );
        core::Float_O::_nil = nil_for_class;
        classcore__Float_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Float_Oval->setSupportsSlots(core::Float_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ForwardReferencedClass_Oval]"));
    core::BuiltInClass_sp classcore__ForwardReferencedClass_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ForwardReferencedClass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ForwardReferencedClass_Oval,_lisp,core::ForwardReferencedClass_O::static_classSymbol());
    core::ForwardReferencedClass_O::___staticMetaClass = classcore__ForwardReferencedClass_Oval;
    _lisp->setf_findClass(core::ForwardReferencedClass_O::static_classSymbol(),classcore__ForwardReferencedClass_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ForwardReferencedClass_O>;
        core::ForwardReferencedClass_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ForwardReferencedClass_O::static_className() % (void*)(core::ForwardReferencedClass_O::static_newNil_callback()) );
    classcore__ForwardReferencedClass_Oval->setInstance_newNil_callback(core::ForwardReferencedClass_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ForwardReferencedClass_O> nil_for_class(new core::ForwardReferencedClass_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ForwardReferencedClass_O::static_className() );
        core::ForwardReferencedClass_O::_nil = nil_for_class;
        classcore__ForwardReferencedClass_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ForwardReferencedClass_Oval->setSupportsSlots(core::ForwardReferencedClass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FunctionFrame_Oval]"));
    core::BuiltInClass_sp classcore__FunctionFrame_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FunctionFrame_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FunctionFrame_Oval,_lisp,core::FunctionFrame_O::static_classSymbol());
    core::FunctionFrame_O::___staticMetaClass = classcore__FunctionFrame_Oval;
    _lisp->setf_findClass(core::FunctionFrame_O::static_classSymbol(),classcore__FunctionFrame_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FunctionFrame_O>;
        core::FunctionFrame_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FunctionFrame_O::static_className() % (void*)(core::FunctionFrame_O::static_newNil_callback()) );
    classcore__FunctionFrame_Oval->setInstance_newNil_callback(core::FunctionFrame_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FunctionFrame_O> nil_for_class(new core::FunctionFrame_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FunctionFrame_O::static_className() );
        core::FunctionFrame_O::_nil = nil_for_class;
        classcore__FunctionFrame_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FunctionFrame_Oval->setSupportsSlots(core::FunctionFrame_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__List_Oval]"));
    core::BuiltInClass_sp classcore__List_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__List_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__List_Oval,_lisp,core::List_O::static_classSymbol());
    core::List_O::___staticMetaClass = classcore__List_Oval;
    _lisp->setf_findClass(core::List_O::static_classSymbol(),classcore__List_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::List_O>;
        core::List_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::List_O::static_className() % (void*)(core::List_O::static_newNil_callback()) );
    classcore__List_Oval->setInstance_newNil_callback(core::List_O::static_newNil_callback());
    {
        boost::shared_ptr<core::List_O> nil_for_class(new core::List_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::List_O::static_className() );
        core::List_O::_nil = nil_for_class;
        classcore__List_Oval->setInstanceNil(nil_for_class);
    }
    classcore__List_Oval->setSupportsSlots(core::List_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Rational_Oval]"));
    core::BuiltInClass_sp classcore__Rational_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Rational_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Rational_Oval,_lisp,core::Rational_O::static_classSymbol());
    core::Rational_O::___staticMetaClass = classcore__Rational_Oval;
    _lisp->setf_findClass(core::Rational_O::static_classSymbol(),classcore__Rational_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Rational_O>;
        core::Rational_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Rational_O::static_className() % (void*)(core::Rational_O::static_newNil_callback()) );
    classcore__Rational_Oval->setInstance_newNil_callback(core::Rational_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Rational_O> nil_for_class(new core::Rational_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Rational_O::static_className() );
        core::Rational_O::_nil = nil_for_class;
        classcore__Rational_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Rational_Oval->setSupportsSlots(core::Rational_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__RuntimeVisibleEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__RuntimeVisibleEnvironment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__RuntimeVisibleEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__RuntimeVisibleEnvironment_Oval,_lisp,core::RuntimeVisibleEnvironment_O::static_classSymbol());
    core::RuntimeVisibleEnvironment_O::___staticMetaClass = classcore__RuntimeVisibleEnvironment_Oval;
    _lisp->setf_findClass(core::RuntimeVisibleEnvironment_O::static_classSymbol(),classcore__RuntimeVisibleEnvironment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::RuntimeVisibleEnvironment_O>;
        core::RuntimeVisibleEnvironment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::RuntimeVisibleEnvironment_O::static_className() % (void*)(core::RuntimeVisibleEnvironment_O::static_newNil_callback()) );
    classcore__RuntimeVisibleEnvironment_Oval->setInstance_newNil_callback(core::RuntimeVisibleEnvironment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::RuntimeVisibleEnvironment_O> nil_for_class(new core::RuntimeVisibleEnvironment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::RuntimeVisibleEnvironment_O::static_className() );
        core::RuntimeVisibleEnvironment_O::_nil = nil_for_class;
        classcore__RuntimeVisibleEnvironment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__RuntimeVisibleEnvironment_Oval->setSupportsSlots(core::RuntimeVisibleEnvironment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SingleDispatchGenericFunction_Oval]"));
    core::BuiltInClass_sp classcore__SingleDispatchGenericFunction_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SingleDispatchGenericFunction_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SingleDispatchGenericFunction_Oval,_lisp,core::SingleDispatchGenericFunction_O::static_classSymbol());
    core::SingleDispatchGenericFunction_O::___staticMetaClass = classcore__SingleDispatchGenericFunction_Oval;
    _lisp->setf_findClass(core::SingleDispatchGenericFunction_O::static_classSymbol(),classcore__SingleDispatchGenericFunction_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SingleDispatchGenericFunction_O>;
        core::SingleDispatchGenericFunction_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SingleDispatchGenericFunction_O::static_className() % (void*)(core::SingleDispatchGenericFunction_O::static_newNil_callback()) );
    classcore__SingleDispatchGenericFunction_Oval->setInstance_newNil_callback(core::SingleDispatchGenericFunction_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SingleDispatchGenericFunction_O> nil_for_class(new core::SingleDispatchGenericFunction_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SingleDispatchGenericFunction_O::static_className() );
        core::SingleDispatchGenericFunction_O::_nil = nil_for_class;
        classcore__SingleDispatchGenericFunction_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SingleDispatchGenericFunction_Oval->setSupportsSlots(core::SingleDispatchGenericFunction_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__StandardChar_Oval]"));
    core::BuiltInClass_sp classcore__StandardChar_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__StandardChar_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StandardChar_Oval,_lisp,core::StandardChar_O::static_classSymbol());
    core::StandardChar_O::___staticMetaClass = classcore__StandardChar_Oval;
    _lisp->setf_findClass(core::StandardChar_O::static_classSymbol(),classcore__StandardChar_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::StandardChar_O>;
        core::StandardChar_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::StandardChar_O::static_className() % (void*)(core::StandardChar_O::static_newNil_callback()) );
    classcore__StandardChar_Oval->setInstance_newNil_callback(core::StandardChar_O::static_newNil_callback());
    {
        boost::shared_ptr<core::StandardChar_O> nil_for_class(new core::StandardChar_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::StandardChar_O::static_className() );
        core::StandardChar_O::_nil = nil_for_class;
        classcore__StandardChar_Oval->setInstanceNil(nil_for_class);
    }
    classcore__StandardChar_Oval->setSupportsSlots(core::StandardChar_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__StandardClass_Oval]"));
    StandardClass_sp classcore__StandardClass_Oval = StandardClass_O::create(undefinedMetaClass);
    classcore__StandardClass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StandardClass_Oval,_lisp,core::StandardClass_O::static_classSymbol());
    core::StandardClass_O::___staticMetaClass = classcore__StandardClass_Oval;
    _lisp->setf_findClass(core::StandardClass_O::static_classSymbol(),classcore__StandardClass_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::StandardClass_O>;
        core::StandardClass_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::StandardClass_O::static_className() % (void*)(core::StandardClass_O::static_newNil_callback()) );
    classcore__StandardClass_Oval->setInstance_newNil_callback(core::StandardClass_O::static_newNil_callback());
    {
        boost::shared_ptr<core::StandardClass_O> nil_for_class(new core::StandardClass_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::StandardClass_O::static_className() );
        core::StandardClass_O::_nil = nil_for_class;
        classcore__StandardClass_Oval->setInstanceNil(nil_for_class);
    }
    classcore__StandardClass_Oval->setSupportsSlots(core::StandardClass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__StandardGenericFunction_Oval]"));
    core::BuiltInClass_sp classcore__StandardGenericFunction_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__StandardGenericFunction_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StandardGenericFunction_Oval,_lisp,core::StandardGenericFunction_O::static_classSymbol());
    core::StandardGenericFunction_O::___staticMetaClass = classcore__StandardGenericFunction_Oval;
    _lisp->setf_findClass(core::StandardGenericFunction_O::static_classSymbol(),classcore__StandardGenericFunction_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::StandardGenericFunction_O>;
        core::StandardGenericFunction_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::StandardGenericFunction_O::static_className() % (void*)(core::StandardGenericFunction_O::static_newNil_callback()) );
    classcore__StandardGenericFunction_Oval->setInstance_newNil_callback(core::StandardGenericFunction_O::static_newNil_callback());
    {
        boost::shared_ptr<core::StandardGenericFunction_O> nil_for_class(new core::StandardGenericFunction_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::StandardGenericFunction_O::static_className() );
        core::StandardGenericFunction_O::_nil = nil_for_class;
        classcore__StandardGenericFunction_Oval->setInstanceNil(nil_for_class);
    }
    classcore__StandardGenericFunction_Oval->setSupportsSlots(core::StandardGenericFunction_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__StringInStream_Oval]"));
    core::BuiltInClass_sp classcore__StringInStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__StringInStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StringInStream_Oval,_lisp,core::StringInStream_O::static_classSymbol());
    core::StringInStream_O::___staticMetaClass = classcore__StringInStream_Oval;
    _lisp->setf_findClass(core::StringInStream_O::static_classSymbol(),classcore__StringInStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::StringInStream_O>;
        core::StringInStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::StringInStream_O::static_className() % (void*)(core::StringInStream_O::static_newNil_callback()) );
    classcore__StringInStream_Oval->setInstance_newNil_callback(core::StringInStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::StringInStream_O> nil_for_class(new core::StringInStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::StringInStream_O::static_className() );
        core::StringInStream_O::_nil = nil_for_class;
        classcore__StringInStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__StringInStream_Oval->setSupportsSlots(core::StringInStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__StringOutStream_Oval]"));
    core::BuiltInClass_sp classcore__StringOutStream_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__StringOutStream_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StringOutStream_Oval,_lisp,core::StringOutStream_O::static_classSymbol());
    core::StringOutStream_O::___staticMetaClass = classcore__StringOutStream_Oval;
    _lisp->setf_findClass(core::StringOutStream_O::static_classSymbol(),classcore__StringOutStream_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::StringOutStream_O>;
        core::StringOutStream_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::StringOutStream_O::static_className() % (void*)(core::StringOutStream_O::static_newNil_callback()) );
    classcore__StringOutStream_Oval->setInstance_newNil_callback(core::StringOutStream_O::static_newNil_callback());
    {
        boost::shared_ptr<core::StringOutStream_O> nil_for_class(new core::StringOutStream_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::StringOutStream_O::static_className() );
        core::StringOutStream_O::_nil = nil_for_class;
        classcore__StringOutStream_Oval->setInstanceNil(nil_for_class);
    }
    classcore__StringOutStream_Oval->setSupportsSlots(core::StringOutStream_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__StructureClass_Oval]"));
    StructureClass_sp classcore__StructureClass_Oval = StructureClass_O::create(undefinedMetaClass);
    classcore__StructureClass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__StructureClass_Oval,_lisp,core::StructureClass_O::static_classSymbol());
    core::StructureClass_O::___staticMetaClass = classcore__StructureClass_Oval;
    _lisp->setf_findClass(core::StructureClass_O::static_classSymbol(),classcore__StructureClass_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::StructureClass_O>;
        core::StructureClass_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::StructureClass_O::static_className() % (void*)(core::StructureClass_O::static_newNil_callback()) );
    classcore__StructureClass_Oval->setInstance_newNil_callback(core::StructureClass_O::static_newNil_callback());
    {
        boost::shared_ptr<core::StructureClass_O> nil_for_class(new core::StructureClass_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::StructureClass_O::static_className() );
        core::StructureClass_O::_nil = nil_for_class;
        classcore__StructureClass_Oval->setInstanceNil(nil_for_class);
    }
    classcore__StructureClass_Oval->setSupportsSlots(core::StructureClass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ValueFrame_Oval]"));
    core::BuiltInClass_sp classcore__ValueFrame_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ValueFrame_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ValueFrame_Oval,_lisp,core::ValueFrame_O::static_classSymbol());
    core::ValueFrame_O::___staticMetaClass = classcore__ValueFrame_Oval;
    _lisp->setf_findClass(core::ValueFrame_O::static_classSymbol(),classcore__ValueFrame_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ValueFrame_O>;
        core::ValueFrame_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ValueFrame_O::static_className() % (void*)(core::ValueFrame_O::static_newNil_callback()) );
    classcore__ValueFrame_Oval->setInstance_newNil_callback(core::ValueFrame_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ValueFrame_O> nil_for_class(new core::ValueFrame_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ValueFrame_O::static_className() );
        core::ValueFrame_O::_nil = nil_for_class;
        classcore__ValueFrame_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ValueFrame_Oval->setSupportsSlots(core::ValueFrame_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Vector_Oval]"));
    core::BuiltInClass_sp classcore__Vector_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Vector_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Vector_Oval,_lisp,core::Vector_O::static_classSymbol());
    core::Vector_O::___staticMetaClass = classcore__Vector_Oval;
    _lisp->setf_findClass(core::Vector_O::static_classSymbol(),classcore__Vector_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Vector_O>;
        core::Vector_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Vector_O::static_className() % (void*)(core::Vector_O::static_newNil_callback()) );
    classcore__Vector_Oval->setInstance_newNil_callback(core::Vector_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Vector_O> nil_for_class(new core::Vector_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Vector_O::static_className() );
        core::Vector_O::_nil = nil_for_class;
        classcore__Vector_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Vector_Oval->setSupportsSlots(core::Vector_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__XmlLoadArchive_Oval]"));
    core::BuiltInClass_sp classcore__XmlLoadArchive_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__XmlLoadArchive_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__XmlLoadArchive_Oval,_lisp,core::XmlLoadArchive_O::static_classSymbol());
    core::XmlLoadArchive_O::___staticMetaClass = classcore__XmlLoadArchive_Oval;
    _lisp->setf_findClass(core::XmlLoadArchive_O::static_classSymbol(),classcore__XmlLoadArchive_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::XmlLoadArchive_O>;
        core::XmlLoadArchive_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::XmlLoadArchive_O::static_className() % (void*)(core::XmlLoadArchive_O::static_newNil_callback()) );
    classcore__XmlLoadArchive_Oval->setInstance_newNil_callback(core::XmlLoadArchive_O::static_newNil_callback());
    {
        boost::shared_ptr<core::XmlLoadArchive_O> nil_for_class(new core::XmlLoadArchive_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::XmlLoadArchive_O::static_className() );
        core::XmlLoadArchive_O::_nil = nil_for_class;
        classcore__XmlLoadArchive_Oval->setInstanceNil(nil_for_class);
    }
    classcore__XmlLoadArchive_Oval->setSupportsSlots(core::XmlLoadArchive_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__XmlSaveArchive_Oval]"));
    core::BuiltInClass_sp classcore__XmlSaveArchive_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__XmlSaveArchive_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__XmlSaveArchive_Oval,_lisp,core::XmlSaveArchive_O::static_classSymbol());
    core::XmlSaveArchive_O::___staticMetaClass = classcore__XmlSaveArchive_Oval;
    _lisp->setf_findClass(core::XmlSaveArchive_O::static_classSymbol(),classcore__XmlSaveArchive_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::XmlSaveArchive_O>;
        core::XmlSaveArchive_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::XmlSaveArchive_O::static_className() % (void*)(core::XmlSaveArchive_O::static_newNil_callback()) );
    classcore__XmlSaveArchive_Oval->setInstance_newNil_callback(core::XmlSaveArchive_O::static_newNil_callback());
    {
        boost::shared_ptr<core::XmlSaveArchive_O> nil_for_class(new core::XmlSaveArchive_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::XmlSaveArchive_O::static_className() );
        core::XmlSaveArchive_O::_nil = nil_for_class;
        classcore__XmlSaveArchive_Oval->setInstanceNil(nil_for_class);
    }
    classcore__XmlSaveArchive_Oval->setSupportsSlots(core::XmlSaveArchive_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ArchiveError_Oval]"));
    core::BuiltInClass_sp classcore__ArchiveError_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ArchiveError_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ArchiveError_Oval,_lisp,core::ArchiveError_O::static_classSymbol());
    core::ArchiveError_O::___staticMetaClass = classcore__ArchiveError_Oval;
    _lisp->setf_findClass(core::ArchiveError_O::static_classSymbol(),classcore__ArchiveError_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ArchiveError_O>;
        core::ArchiveError_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ArchiveError_O::static_className() % (void*)(core::ArchiveError_O::static_newNil_callback()) );
    classcore__ArchiveError_Oval->setInstance_newNil_callback(core::ArchiveError_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ArchiveError_O> nil_for_class(new core::ArchiveError_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ArchiveError_O::static_className() );
        core::ArchiveError_O::_nil = nil_for_class;
        classcore__ArchiveError_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ArchiveError_Oval->setSupportsSlots(core::ArchiveError_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__BitVector_Oval]"));
    core::BuiltInClass_sp classcore__BitVector_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__BitVector_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__BitVector_Oval,_lisp,core::BitVector_O::static_classSymbol());
    core::BitVector_O::___staticMetaClass = classcore__BitVector_Oval;
    _lisp->setf_findClass(core::BitVector_O::static_classSymbol(),classcore__BitVector_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::BitVector_O>;
        core::BitVector_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::BitVector_O::static_className() % (void*)(core::BitVector_O::static_newNil_callback()) );
    classcore__BitVector_Oval->setInstance_newNil_callback(core::BitVector_O::static_newNil_callback());
    {
        boost::shared_ptr<core::BitVector_O> nil_for_class(new core::BitVector_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::BitVector_O::static_className() );
        core::BitVector_O::_nil = nil_for_class;
        classcore__BitVector_Oval->setInstanceNil(nil_for_class);
    }
    classcore__BitVector_Oval->setSupportsSlots(core::BitVector_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__BlockEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__BlockEnvironment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__BlockEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__BlockEnvironment_Oval,_lisp,core::BlockEnvironment_O::static_classSymbol());
    core::BlockEnvironment_O::___staticMetaClass = classcore__BlockEnvironment_Oval;
    _lisp->setf_findClass(core::BlockEnvironment_O::static_classSymbol(),classcore__BlockEnvironment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::BlockEnvironment_O>;
        core::BlockEnvironment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::BlockEnvironment_O::static_className() % (void*)(core::BlockEnvironment_O::static_newNil_callback()) );
    classcore__BlockEnvironment_Oval->setInstance_newNil_callback(core::BlockEnvironment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::BlockEnvironment_O> nil_for_class(new core::BlockEnvironment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::BlockEnvironment_O::static_className() );
        core::BlockEnvironment_O::_nil = nil_for_class;
        classcore__BlockEnvironment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__BlockEnvironment_Oval->setSupportsSlots(core::BlockEnvironment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__CatchEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__CatchEnvironment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__CatchEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__CatchEnvironment_Oval,_lisp,core::CatchEnvironment_O::static_classSymbol());
    core::CatchEnvironment_O::___staticMetaClass = classcore__CatchEnvironment_Oval;
    _lisp->setf_findClass(core::CatchEnvironment_O::static_classSymbol(),classcore__CatchEnvironment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::CatchEnvironment_O>;
        core::CatchEnvironment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::CatchEnvironment_O::static_className() % (void*)(core::CatchEnvironment_O::static_newNil_callback()) );
    classcore__CatchEnvironment_Oval->setInstance_newNil_callback(core::CatchEnvironment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::CatchEnvironment_O> nil_for_class(new core::CatchEnvironment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::CatchEnvironment_O::static_className() );
        core::CatchEnvironment_O::_nil = nil_for_class;
        classcore__CatchEnvironment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__CatchEnvironment_Oval->setSupportsSlots(core::CatchEnvironment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Cons_Oval]"));
    core::BuiltInClass_sp classcore__Cons_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Cons_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Cons_Oval,_lisp,core::Cons_O::static_classSymbol());
    core::Cons_O::___staticMetaClass = classcore__Cons_Oval;
    _lisp->setf_findClass(core::Cons_O::static_classSymbol(),classcore__Cons_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Cons_O>;
        core::Cons_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Cons_O::static_className() % (void*)(core::Cons_O::static_newNil_callback()) );
    classcore__Cons_Oval->setInstance_newNil_callback(core::Cons_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Cons_O> nil_for_class(new core::Cons_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Cons_O::static_className() );
        core::Cons_O::_nil = nil_for_class;
        classcore__Cons_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Cons_Oval->setSupportsSlots(core::Cons_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ContentException_Oval]"));
    core::BuiltInClass_sp classcore__ContentException_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ContentException_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ContentException_Oval,_lisp,core::ContentException_O::static_classSymbol());
    core::ContentException_O::___staticMetaClass = classcore__ContentException_Oval;
    _lisp->setf_findClass(core::ContentException_O::static_classSymbol(),classcore__ContentException_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ContentException_O>;
        core::ContentException_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ContentException_O::static_className() % (void*)(core::ContentException_O::static_newNil_callback()) );
    classcore__ContentException_Oval->setInstance_newNil_callback(core::ContentException_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ContentException_O> nil_for_class(new core::ContentException_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ContentException_O::static_className() );
        core::ContentException_O::_nil = nil_for_class;
        classcore__ContentException_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ContentException_Oval->setSupportsSlots(core::ContentException_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__DoubleFloat_Oval]"));
    core::BuiltInClass_sp classcore__DoubleFloat_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__DoubleFloat_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__DoubleFloat_Oval,_lisp,core::DoubleFloat_O::static_classSymbol());
    core::DoubleFloat_O::___staticMetaClass = classcore__DoubleFloat_Oval;
    _lisp->setf_findClass(core::DoubleFloat_O::static_classSymbol(),classcore__DoubleFloat_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::DoubleFloat_O>;
        core::DoubleFloat_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::DoubleFloat_O::static_className() % (void*)(core::DoubleFloat_O::static_newNil_callback()) );
    classcore__DoubleFloat_Oval->setInstance_newNil_callback(core::DoubleFloat_O::static_newNil_callback());
    {
        boost::shared_ptr<core::DoubleFloat_O> nil_for_class(new core::DoubleFloat_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::DoubleFloat_O::static_className() );
        core::DoubleFloat_O::_nil = nil_for_class;
        classcore__DoubleFloat_Oval->setInstanceNil(nil_for_class);
    }
    classcore__DoubleFloat_Oval->setSupportsSlots(core::DoubleFloat_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FileNotFoundException_Oval]"));
    core::BuiltInClass_sp classcore__FileNotFoundException_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FileNotFoundException_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FileNotFoundException_Oval,_lisp,core::FileNotFoundException_O::static_classSymbol());
    core::FileNotFoundException_O::___staticMetaClass = classcore__FileNotFoundException_Oval;
    _lisp->setf_findClass(core::FileNotFoundException_O::static_classSymbol(),classcore__FileNotFoundException_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FileNotFoundException_O>;
        core::FileNotFoundException_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FileNotFoundException_O::static_className() % (void*)(core::FileNotFoundException_O::static_newNil_callback()) );
    classcore__FileNotFoundException_Oval->setInstance_newNil_callback(core::FileNotFoundException_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FileNotFoundException_O> nil_for_class(new core::FileNotFoundException_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FileNotFoundException_O::static_className() );
        core::FileNotFoundException_O::_nil = nil_for_class;
        classcore__FileNotFoundException_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FileNotFoundException_Oval->setSupportsSlots(core::FileNotFoundException_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FunctionContainerEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__FunctionContainerEnvironment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FunctionContainerEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FunctionContainerEnvironment_Oval,_lisp,core::FunctionContainerEnvironment_O::static_classSymbol());
    core::FunctionContainerEnvironment_O::___staticMetaClass = classcore__FunctionContainerEnvironment_Oval;
    _lisp->setf_findClass(core::FunctionContainerEnvironment_O::static_classSymbol(),classcore__FunctionContainerEnvironment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FunctionContainerEnvironment_O>;
        core::FunctionContainerEnvironment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FunctionContainerEnvironment_O::static_className() % (void*)(core::FunctionContainerEnvironment_O::static_newNil_callback()) );
    classcore__FunctionContainerEnvironment_Oval->setInstance_newNil_callback(core::FunctionContainerEnvironment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FunctionContainerEnvironment_O> nil_for_class(new core::FunctionContainerEnvironment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FunctionContainerEnvironment_O::static_className() );
        core::FunctionContainerEnvironment_O::_nil = nil_for_class;
        classcore__FunctionContainerEnvironment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FunctionContainerEnvironment_Oval->setSupportsSlots(core::FunctionContainerEnvironment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__FunctionValueEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__FunctionValueEnvironment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__FunctionValueEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__FunctionValueEnvironment_Oval,_lisp,core::FunctionValueEnvironment_O::static_classSymbol());
    core::FunctionValueEnvironment_O::___staticMetaClass = classcore__FunctionValueEnvironment_Oval;
    _lisp->setf_findClass(core::FunctionValueEnvironment_O::static_classSymbol(),classcore__FunctionValueEnvironment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::FunctionValueEnvironment_O>;
        core::FunctionValueEnvironment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::FunctionValueEnvironment_O::static_className() % (void*)(core::FunctionValueEnvironment_O::static_newNil_callback()) );
    classcore__FunctionValueEnvironment_Oval->setInstance_newNil_callback(core::FunctionValueEnvironment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::FunctionValueEnvironment_O> nil_for_class(new core::FunctionValueEnvironment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::FunctionValueEnvironment_O::static_className() );
        core::FunctionValueEnvironment_O::_nil = nil_for_class;
        classcore__FunctionValueEnvironment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__FunctionValueEnvironment_Oval->setSupportsSlots(core::FunctionValueEnvironment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Integer_Oval]"));
    core::BuiltInClass_sp classcore__Integer_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Integer_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Integer_Oval,_lisp,core::Integer_O::static_classSymbol());
    core::Integer_O::___staticMetaClass = classcore__Integer_Oval;
    _lisp->setf_findClass(core::Integer_O::static_classSymbol(),classcore__Integer_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Integer_O>;
        core::Integer_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Integer_O::static_className() % (void*)(core::Integer_O::static_newNil_callback()) );
    classcore__Integer_Oval->setInstance_newNil_callback(core::Integer_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Integer_O> nil_for_class(new core::Integer_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Integer_O::static_className() );
        core::Integer_O::_nil = nil_for_class;
        classcore__Integer_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Integer_Oval->setSupportsSlots(core::Integer_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__LispError_Oval]"));
    core::BuiltInClass_sp classcore__LispError_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__LispError_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__LispError_Oval,_lisp,core::LispError_O::static_classSymbol());
    core::LispError_O::___staticMetaClass = classcore__LispError_Oval;
    _lisp->setf_findClass(core::LispError_O::static_classSymbol(),classcore__LispError_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::LispError_O>;
        core::LispError_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::LispError_O::static_className() % (void*)(core::LispError_O::static_newNil_callback()) );
    classcore__LispError_Oval->setInstance_newNil_callback(core::LispError_O::static_newNil_callback());
    {
        boost::shared_ptr<core::LispError_O> nil_for_class(new core::LispError_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::LispError_O::static_className() );
        core::LispError_O::_nil = nil_for_class;
        classcore__LispError_Oval->setInstanceNil(nil_for_class);
    }
    classcore__LispError_Oval->setSupportsSlots(core::LispError_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__LongFloat_Oval]"));
    core::BuiltInClass_sp classcore__LongFloat_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__LongFloat_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__LongFloat_Oval,_lisp,core::LongFloat_O::static_classSymbol());
    core::LongFloat_O::___staticMetaClass = classcore__LongFloat_Oval;
    _lisp->setf_findClass(core::LongFloat_O::static_classSymbol(),classcore__LongFloat_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::LongFloat_O>;
        core::LongFloat_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::LongFloat_O::static_className() % (void*)(core::LongFloat_O::static_newNil_callback()) );
    classcore__LongFloat_Oval->setInstance_newNil_callback(core::LongFloat_O::static_newNil_callback());
    {
        boost::shared_ptr<core::LongFloat_O> nil_for_class(new core::LongFloat_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::LongFloat_O::static_className() );
        core::LongFloat_O::_nil = nil_for_class;
        classcore__LongFloat_Oval->setInstanceNil(nil_for_class);
    }
    classcore__LongFloat_Oval->setSupportsSlots(core::LongFloat_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__MacroletEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__MacroletEnvironment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__MacroletEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__MacroletEnvironment_Oval,_lisp,core::MacroletEnvironment_O::static_classSymbol());
    core::MacroletEnvironment_O::___staticMetaClass = classcore__MacroletEnvironment_Oval;
    _lisp->setf_findClass(core::MacroletEnvironment_O::static_classSymbol(),classcore__MacroletEnvironment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::MacroletEnvironment_O>;
        core::MacroletEnvironment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::MacroletEnvironment_O::static_className() % (void*)(core::MacroletEnvironment_O::static_newNil_callback()) );
    classcore__MacroletEnvironment_Oval->setInstance_newNil_callback(core::MacroletEnvironment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::MacroletEnvironment_O> nil_for_class(new core::MacroletEnvironment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::MacroletEnvironment_O::static_className() );
        core::MacroletEnvironment_O::_nil = nil_for_class;
        classcore__MacroletEnvironment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__MacroletEnvironment_Oval->setSupportsSlots(core::MacroletEnvironment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__MathException_Oval]"));
    core::BuiltInClass_sp classcore__MathException_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__MathException_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__MathException_Oval,_lisp,core::MathException_O::static_classSymbol());
    core::MathException_O::___staticMetaClass = classcore__MathException_Oval;
    _lisp->setf_findClass(core::MathException_O::static_classSymbol(),classcore__MathException_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::MathException_O>;
        core::MathException_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::MathException_O::static_className() % (void*)(core::MathException_O::static_newNil_callback()) );
    classcore__MathException_Oval->setInstance_newNil_callback(core::MathException_O::static_newNil_callback());
    {
        boost::shared_ptr<core::MathException_O> nil_for_class(new core::MathException_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::MathException_O::static_className() );
        core::MathException_O::_nil = nil_for_class;
        classcore__MathException_Oval->setInstanceNil(nil_for_class);
    }
    classcore__MathException_Oval->setSupportsSlots(core::MathException_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Null_Oval]"));
    core::BuiltInClass_sp classcore__Null_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Null_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Null_Oval,_lisp,core::Null_O::static_classSymbol());
    core::Null_O::___staticMetaClass = classcore__Null_Oval;
    _lisp->setf_findClass(core::Null_O::static_classSymbol(),classcore__Null_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Null_O>;
        core::Null_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Null_O::static_className() % (void*)(core::Null_O::static_newNil_callback()) );
    classcore__Null_Oval->setInstance_newNil_callback(core::Null_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Null_O> nil_for_class(new core::Null_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Null_O::static_className() );
        core::Null_O::_nil = nil_for_class;
        classcore__Null_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Null_Oval->setSupportsSlots(core::Null_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Ratio_Oval]"));
    core::BuiltInClass_sp classcore__Ratio_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Ratio_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Ratio_Oval,_lisp,core::Ratio_O::static_classSymbol());
    core::Ratio_O::___staticMetaClass = classcore__Ratio_Oval;
    _lisp->setf_findClass(core::Ratio_O::static_classSymbol(),classcore__Ratio_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Ratio_O>;
        core::Ratio_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Ratio_O::static_className() % (void*)(core::Ratio_O::static_newNil_callback()) );
    classcore__Ratio_Oval->setInstance_newNil_callback(core::Ratio_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Ratio_O> nil_for_class(new core::Ratio_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Ratio_O::static_className() );
        core::Ratio_O::_nil = nil_for_class;
        classcore__Ratio_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Ratio_Oval->setSupportsSlots(core::Ratio_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SerializeError_Oval]"));
    core::BuiltInClass_sp classcore__SerializeError_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SerializeError_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SerializeError_Oval,_lisp,core::SerializeError_O::static_classSymbol());
    core::SerializeError_O::___staticMetaClass = classcore__SerializeError_Oval;
    _lisp->setf_findClass(core::SerializeError_O::static_classSymbol(),classcore__SerializeError_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SerializeError_O>;
        core::SerializeError_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SerializeError_O::static_className() % (void*)(core::SerializeError_O::static_newNil_callback()) );
    classcore__SerializeError_Oval->setInstance_newNil_callback(core::SerializeError_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SerializeError_O> nil_for_class(new core::SerializeError_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SerializeError_O::static_className() );
        core::SerializeError_O::_nil = nil_for_class;
        classcore__SerializeError_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SerializeError_Oval->setSupportsSlots(core::SerializeError_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ShortFloat_Oval]"));
    core::BuiltInClass_sp classcore__ShortFloat_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ShortFloat_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ShortFloat_Oval,_lisp,core::ShortFloat_O::static_classSymbol());
    core::ShortFloat_O::___staticMetaClass = classcore__ShortFloat_Oval;
    _lisp->setf_findClass(core::ShortFloat_O::static_classSymbol(),classcore__ShortFloat_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ShortFloat_O>;
        core::ShortFloat_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ShortFloat_O::static_className() % (void*)(core::ShortFloat_O::static_newNil_callback()) );
    classcore__ShortFloat_Oval->setInstance_newNil_callback(core::ShortFloat_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ShortFloat_O> nil_for_class(new core::ShortFloat_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ShortFloat_O::static_className() );
        core::ShortFloat_O::_nil = nil_for_class;
        classcore__ShortFloat_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ShortFloat_Oval->setSupportsSlots(core::ShortFloat_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SingleFloat_Oval]"));
    core::BuiltInClass_sp classcore__SingleFloat_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SingleFloat_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SingleFloat_Oval,_lisp,core::SingleFloat_O::static_classSymbol());
    core::SingleFloat_O::___staticMetaClass = classcore__SingleFloat_Oval;
    _lisp->setf_findClass(core::SingleFloat_O::static_classSymbol(),classcore__SingleFloat_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SingleFloat_O>;
        core::SingleFloat_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SingleFloat_O::static_className() % (void*)(core::SingleFloat_O::static_newNil_callback()) );
    classcore__SingleFloat_Oval->setInstance_newNil_callback(core::SingleFloat_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SingleFloat_O> nil_for_class(new core::SingleFloat_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SingleFloat_O::static_className() );
        core::SingleFloat_O::_nil = nil_for_class;
        classcore__SingleFloat_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SingleFloat_Oval->setSupportsSlots(core::SingleFloat_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__String_Oval]"));
    core::BuiltInClass_sp classcore__String_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__String_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__String_Oval,_lisp,core::String_O::static_classSymbol());
    core::String_O::___staticMetaClass = classcore__String_Oval;
    _lisp->setf_findClass(core::String_O::static_classSymbol(),classcore__String_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::String_O>;
        core::String_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::String_O::static_className() % (void*)(core::String_O::static_newNil_callback()) );
    classcore__String_Oval->setInstance_newNil_callback(core::String_O::static_newNil_callback());
    {
        boost::shared_ptr<core::String_O> nil_for_class(new core::String_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::String_O::static_className() );
        core::String_O::_nil = nil_for_class;
        classcore__String_Oval->setInstanceNil(nil_for_class);
    }
    classcore__String_Oval->setSupportsSlots(core::String_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SymbolMacroletEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__SymbolMacroletEnvironment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SymbolMacroletEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SymbolMacroletEnvironment_Oval,_lisp,core::SymbolMacroletEnvironment_O::static_classSymbol());
    core::SymbolMacroletEnvironment_O::___staticMetaClass = classcore__SymbolMacroletEnvironment_Oval;
    _lisp->setf_findClass(core::SymbolMacroletEnvironment_O::static_classSymbol(),classcore__SymbolMacroletEnvironment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SymbolMacroletEnvironment_O>;
        core::SymbolMacroletEnvironment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SymbolMacroletEnvironment_O::static_className() % (void*)(core::SymbolMacroletEnvironment_O::static_newNil_callback()) );
    classcore__SymbolMacroletEnvironment_Oval->setInstance_newNil_callback(core::SymbolMacroletEnvironment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SymbolMacroletEnvironment_O> nil_for_class(new core::SymbolMacroletEnvironment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SymbolMacroletEnvironment_O::static_className() );
        core::SymbolMacroletEnvironment_O::_nil = nil_for_class;
        classcore__SymbolMacroletEnvironment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SymbolMacroletEnvironment_Oval->setSupportsSlots(core::SymbolMacroletEnvironment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__TagbodyEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__TagbodyEnvironment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__TagbodyEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__TagbodyEnvironment_Oval,_lisp,core::TagbodyEnvironment_O::static_classSymbol());
    core::TagbodyEnvironment_O::___staticMetaClass = classcore__TagbodyEnvironment_Oval;
    _lisp->setf_findClass(core::TagbodyEnvironment_O::static_classSymbol(),classcore__TagbodyEnvironment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::TagbodyEnvironment_O>;
        core::TagbodyEnvironment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::TagbodyEnvironment_O::static_className() % (void*)(core::TagbodyEnvironment_O::static_newNil_callback()) );
    classcore__TagbodyEnvironment_Oval->setInstance_newNil_callback(core::TagbodyEnvironment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::TagbodyEnvironment_O> nil_for_class(new core::TagbodyEnvironment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::TagbodyEnvironment_O::static_className() );
        core::TagbodyEnvironment_O::_nil = nil_for_class;
        classcore__TagbodyEnvironment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__TagbodyEnvironment_Oval->setSupportsSlots(core::TagbodyEnvironment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__TypeException_Oval]"));
    core::BuiltInClass_sp classcore__TypeException_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__TypeException_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__TypeException_Oval,_lisp,core::TypeException_O::static_classSymbol());
    core::TypeException_O::___staticMetaClass = classcore__TypeException_Oval;
    _lisp->setf_findClass(core::TypeException_O::static_classSymbol(),classcore__TypeException_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::TypeException_O>;
        core::TypeException_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::TypeException_O::static_className() % (void*)(core::TypeException_O::static_newNil_callback()) );
    classcore__TypeException_Oval->setInstance_newNil_callback(core::TypeException_O::static_newNil_callback());
    {
        boost::shared_ptr<core::TypeException_O> nil_for_class(new core::TypeException_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::TypeException_O::static_className() );
        core::TypeException_O::_nil = nil_for_class;
        classcore__TypeException_Oval->setInstanceNil(nil_for_class);
    }
    classcore__TypeException_Oval->setSupportsSlots(core::TypeException_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__UnwindProtectEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__UnwindProtectEnvironment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__UnwindProtectEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__UnwindProtectEnvironment_Oval,_lisp,core::UnwindProtectEnvironment_O::static_classSymbol());
    core::UnwindProtectEnvironment_O::___staticMetaClass = classcore__UnwindProtectEnvironment_Oval;
    _lisp->setf_findClass(core::UnwindProtectEnvironment_O::static_classSymbol(),classcore__UnwindProtectEnvironment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::UnwindProtectEnvironment_O>;
        core::UnwindProtectEnvironment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::UnwindProtectEnvironment_O::static_className() % (void*)(core::UnwindProtectEnvironment_O::static_newNil_callback()) );
    classcore__UnwindProtectEnvironment_Oval->setInstance_newNil_callback(core::UnwindProtectEnvironment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::UnwindProtectEnvironment_O> nil_for_class(new core::UnwindProtectEnvironment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::UnwindProtectEnvironment_O::static_className() );
        core::UnwindProtectEnvironment_O::_nil = nil_for_class;
        classcore__UnwindProtectEnvironment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__UnwindProtectEnvironment_Oval->setSupportsSlots(core::UnwindProtectEnvironment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__ValueEnvironment_Oval]"));
    core::BuiltInClass_sp classcore__ValueEnvironment_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__ValueEnvironment_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__ValueEnvironment_Oval,_lisp,core::ValueEnvironment_O::static_classSymbol());
    core::ValueEnvironment_O::___staticMetaClass = classcore__ValueEnvironment_Oval;
    _lisp->setf_findClass(core::ValueEnvironment_O::static_classSymbol(),classcore__ValueEnvironment_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::ValueEnvironment_O>;
        core::ValueEnvironment_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::ValueEnvironment_O::static_className() % (void*)(core::ValueEnvironment_O::static_newNil_callback()) );
    classcore__ValueEnvironment_Oval->setInstance_newNil_callback(core::ValueEnvironment_O::static_newNil_callback());
    {
        boost::shared_ptr<core::ValueEnvironment_O> nil_for_class(new core::ValueEnvironment_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::ValueEnvironment_O::static_className() );
        core::ValueEnvironment_O::_nil = nil_for_class;
        classcore__ValueEnvironment_Oval->setInstanceNil(nil_for_class);
    }
    classcore__ValueEnvironment_Oval->setSupportsSlots(core::ValueEnvironment_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__VectorObjects_Oval]"));
    core::BuiltInClass_sp classcore__VectorObjects_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__VectorObjects_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__VectorObjects_Oval,_lisp,core::VectorObjects_O::static_classSymbol());
    core::VectorObjects_O::___staticMetaClass = classcore__VectorObjects_Oval;
    _lisp->setf_findClass(core::VectorObjects_O::static_classSymbol(),classcore__VectorObjects_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::VectorObjects_O>;
        core::VectorObjects_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::VectorObjects_O::static_className() % (void*)(core::VectorObjects_O::static_newNil_callback()) );
    classcore__VectorObjects_Oval->setInstance_newNil_callback(core::VectorObjects_O::static_newNil_callback());
    {
        boost::shared_ptr<core::VectorObjects_O> nil_for_class(new core::VectorObjects_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::VectorObjects_O::static_className() );
        core::VectorObjects_O::_nil = nil_for_class;
        classcore__VectorObjects_Oval->setInstanceNil(nil_for_class);
    }
    classcore__VectorObjects_Oval->setSupportsSlots(core::VectorObjects_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__serious_condition_Oval]"));
    core::BuiltInClass_sp classcore__serious_condition_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__serious_condition_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__serious_condition_Oval,_lisp,core::serious_condition_O::static_classSymbol());
    core::serious_condition_O::___staticMetaClass = classcore__serious_condition_Oval;
    _lisp->setf_findClass(core::serious_condition_O::static_classSymbol(),classcore__serious_condition_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::serious_condition_O>;
        core::serious_condition_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::serious_condition_O::static_className() % (void*)(core::serious_condition_O::static_newNil_callback()) );
    classcore__serious_condition_Oval->setInstance_newNil_callback(core::serious_condition_O::static_newNil_callback());
    {
        boost::shared_ptr<core::serious_condition_O> nil_for_class(new core::serious_condition_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::serious_condition_O::static_className() );
        core::serious_condition_O::_nil = nil_for_class;
        classcore__serious_condition_Oval->setInstanceNil(nil_for_class);
    }
    classcore__serious_condition_Oval->setSupportsSlots(core::serious_condition_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__simple_condition_Oval]"));
    core::BuiltInClass_sp classcore__simple_condition_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__simple_condition_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__simple_condition_Oval,_lisp,core::simple_condition_O::static_classSymbol());
    core::simple_condition_O::___staticMetaClass = classcore__simple_condition_Oval;
    _lisp->setf_findClass(core::simple_condition_O::static_classSymbol(),classcore__simple_condition_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::simple_condition_O>;
        core::simple_condition_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::simple_condition_O::static_className() % (void*)(core::simple_condition_O::static_newNil_callback()) );
    classcore__simple_condition_Oval->setInstance_newNil_callback(core::simple_condition_O::static_newNil_callback());
    {
        boost::shared_ptr<core::simple_condition_O> nil_for_class(new core::simple_condition_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::simple_condition_O::static_className() );
        core::simple_condition_O::_nil = nil_for_class;
        classcore__simple_condition_Oval->setInstanceNil(nil_for_class);
    }
    classcore__simple_condition_Oval->setSupportsSlots(core::simple_condition_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__warning_Oval]"));
    core::BuiltInClass_sp classcore__warning_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__warning_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__warning_Oval,_lisp,core::warning_O::static_classSymbol());
    core::warning_O::___staticMetaClass = classcore__warning_Oval;
    _lisp->setf_findClass(core::warning_O::static_classSymbol(),classcore__warning_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::warning_O>;
        core::warning_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::warning_O::static_className() % (void*)(core::warning_O::static_newNil_callback()) );
    classcore__warning_Oval->setInstance_newNil_callback(core::warning_O::static_newNil_callback());
    {
        boost::shared_ptr<core::warning_O> nil_for_class(new core::warning_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::warning_O::static_className() );
        core::warning_O::_nil = nil_for_class;
        classcore__warning_Oval->setInstanceNil(nil_for_class);
    }
    classcore__warning_Oval->setSupportsSlots(core::warning_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Bignum_Oval]"));
    core::BuiltInClass_sp classcore__Bignum_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Bignum_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Bignum_Oval,_lisp,core::Bignum_O::static_classSymbol());
    core::Bignum_O::___staticMetaClass = classcore__Bignum_Oval;
    _lisp->setf_findClass(core::Bignum_O::static_classSymbol(),classcore__Bignum_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Bignum_O>;
        core::Bignum_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Bignum_O::static_className() % (void*)(core::Bignum_O::static_newNil_callback()) );
    classcore__Bignum_Oval->setInstance_newNil_callback(core::Bignum_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Bignum_O> nil_for_class(new core::Bignum_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Bignum_O::static_className() );
        core::Bignum_O::_nil = nil_for_class;
        classcore__Bignum_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Bignum_Oval->setSupportsSlots(core::Bignum_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__CompiledBody_Oval]"));
    core::BuiltInClass_sp classcore__CompiledBody_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__CompiledBody_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__CompiledBody_Oval,_lisp,core::CompiledBody_O::static_classSymbol());
    core::CompiledBody_O::___staticMetaClass = classcore__CompiledBody_Oval;
    _lisp->setf_findClass(core::CompiledBody_O::static_classSymbol(),classcore__CompiledBody_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::CompiledBody_O>;
        core::CompiledBody_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::CompiledBody_O::static_className() % (void*)(core::CompiledBody_O::static_newNil_callback()) );
    classcore__CompiledBody_Oval->setInstance_newNil_callback(core::CompiledBody_O::static_newNil_callback());
    {
        boost::shared_ptr<core::CompiledBody_O> nil_for_class(new core::CompiledBody_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::CompiledBody_O::static_className() );
        core::CompiledBody_O::_nil = nil_for_class;
        classcore__CompiledBody_Oval->setInstanceNil(nil_for_class);
    }
    classcore__CompiledBody_Oval->setSupportsSlots(core::CompiledBody_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Error_Oval]"));
    core::BuiltInClass_sp classcore__Error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Error_Oval,_lisp,core::Error_O::static_classSymbol());
    core::Error_O::___staticMetaClass = classcore__Error_Oval;
    _lisp->setf_findClass(core::Error_O::static_classSymbol(),classcore__Error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Error_O>;
        core::Error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Error_O::static_className() % (void*)(core::Error_O::static_newNil_callback()) );
    classcore__Error_Oval->setInstance_newNil_callback(core::Error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Error_O> nil_for_class(new core::Error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Error_O::static_className() );
        core::Error_O::_nil = nil_for_class;
        classcore__Error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Error_Oval->setSupportsSlots(core::Error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Fixnum_Oval]"));
    core::BuiltInClass_sp classcore__Fixnum_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Fixnum_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Fixnum_Oval,_lisp,core::Fixnum_O::static_classSymbol());
    core::Fixnum_O::___staticMetaClass = classcore__Fixnum_Oval;
    _lisp->setf_findClass(core::Fixnum_O::static_classSymbol(),classcore__Fixnum_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Fixnum_O>;
        core::Fixnum_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Fixnum_O::static_className() % (void*)(core::Fixnum_O::static_newNil_callback()) );
    classcore__Fixnum_Oval->setInstance_newNil_callback(core::Fixnum_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Fixnum_O> nil_for_class(new core::Fixnum_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Fixnum_O::static_className() );
        core::Fixnum_O::_nil = nil_for_class;
        classcore__Fixnum_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Fixnum_Oval->setSupportsSlots(core::Fixnum_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SimpleBitVector_Oval]"));
    core::BuiltInClass_sp classcore__SimpleBitVector_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SimpleBitVector_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SimpleBitVector_Oval,_lisp,core::SimpleBitVector_O::static_classSymbol());
    core::SimpleBitVector_O::___staticMetaClass = classcore__SimpleBitVector_Oval;
    _lisp->setf_findClass(core::SimpleBitVector_O::static_classSymbol(),classcore__SimpleBitVector_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SimpleBitVector_O>;
        core::SimpleBitVector_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SimpleBitVector_O::static_className() % (void*)(core::SimpleBitVector_O::static_newNil_callback()) );
    classcore__SimpleBitVector_Oval->setInstance_newNil_callback(core::SimpleBitVector_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SimpleBitVector_O> nil_for_class(new core::SimpleBitVector_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SimpleBitVector_O::static_className() );
        core::SimpleBitVector_O::_nil = nil_for_class;
        classcore__SimpleBitVector_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SimpleBitVector_Oval->setSupportsSlots(core::SimpleBitVector_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__SourceCodeCons_Oval]"));
    core::BuiltInClass_sp classcore__SourceCodeCons_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__SourceCodeCons_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__SourceCodeCons_Oval,_lisp,core::SourceCodeCons_O::static_classSymbol());
    core::SourceCodeCons_O::___staticMetaClass = classcore__SourceCodeCons_Oval;
    _lisp->setf_findClass(core::SourceCodeCons_O::static_classSymbol(),classcore__SourceCodeCons_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::SourceCodeCons_O>;
        core::SourceCodeCons_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::SourceCodeCons_O::static_className() % (void*)(core::SourceCodeCons_O::static_newNil_callback()) );
    classcore__SourceCodeCons_Oval->setInstance_newNil_callback(core::SourceCodeCons_O::static_newNil_callback());
    {
        boost::shared_ptr<core::SourceCodeCons_O> nil_for_class(new core::SourceCodeCons_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::SourceCodeCons_O::static_className() );
        core::SourceCodeCons_O::_nil = nil_for_class;
        classcore__SourceCodeCons_Oval->setInstanceNil(nil_for_class);
    }
    classcore__SourceCodeCons_Oval->setSupportsSlots(core::SourceCodeCons_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__Str_Oval]"));
    core::BuiltInClass_sp classcore__Str_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__Str_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__Str_Oval,_lisp,core::Str_O::static_classSymbol());
    core::Str_O::___staticMetaClass = classcore__Str_Oval;
    _lisp->setf_findClass(core::Str_O::static_classSymbol(),classcore__Str_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::Str_O>;
        core::Str_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::Str_O::static_className() % (void*)(core::Str_O::static_newNil_callback()) );
    classcore__Str_Oval->setInstance_newNil_callback(core::Str_O::static_newNil_callback());
    {
        boost::shared_ptr<core::Str_O> nil_for_class(new core::Str_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::Str_O::static_className() );
        core::Str_O::_nil = nil_for_class;
        classcore__Str_Oval->setInstanceNil(nil_for_class);
    }
    classcore__Str_Oval->setSupportsSlots(core::Str_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__VectorObjectsWithFillPtr_Oval]"));
    core::BuiltInClass_sp classcore__VectorObjectsWithFillPtr_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__VectorObjectsWithFillPtr_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__VectorObjectsWithFillPtr_Oval,_lisp,core::VectorObjectsWithFillPtr_O::static_classSymbol());
    core::VectorObjectsWithFillPtr_O::___staticMetaClass = classcore__VectorObjectsWithFillPtr_Oval;
    _lisp->setf_findClass(core::VectorObjectsWithFillPtr_O::static_classSymbol(),classcore__VectorObjectsWithFillPtr_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::VectorObjectsWithFillPtr_O>;
        core::VectorObjectsWithFillPtr_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::VectorObjectsWithFillPtr_O::static_className() % (void*)(core::VectorObjectsWithFillPtr_O::static_newNil_callback()) );
    classcore__VectorObjectsWithFillPtr_Oval->setInstance_newNil_callback(core::VectorObjectsWithFillPtr_O::static_newNil_callback());
    {
        boost::shared_ptr<core::VectorObjectsWithFillPtr_O> nil_for_class(new core::VectorObjectsWithFillPtr_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::VectorObjectsWithFillPtr_O::static_className() );
        core::VectorObjectsWithFillPtr_O::_nil = nil_for_class;
        classcore__VectorObjectsWithFillPtr_Oval->setInstanceNil(nil_for_class);
    }
    classcore__VectorObjectsWithFillPtr_Oval->setSupportsSlots(core::VectorObjectsWithFillPtr_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__simple_warning_Oval]"));
    core::BuiltInClass_sp classcore__simple_warning_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__simple_warning_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__simple_warning_Oval,_lisp,core::simple_warning_O::static_classSymbol());
    core::simple_warning_O::___staticMetaClass = classcore__simple_warning_Oval;
    _lisp->setf_findClass(core::simple_warning_O::static_classSymbol(),classcore__simple_warning_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::simple_warning_O>;
        core::simple_warning_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::simple_warning_O::static_className() % (void*)(core::simple_warning_O::static_newNil_callback()) );
    classcore__simple_warning_Oval->setInstance_newNil_callback(core::simple_warning_O::static_newNil_callback());
    {
        boost::shared_ptr<core::simple_warning_O> nil_for_class(new core::simple_warning_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::simple_warning_O::static_className() );
        core::simple_warning_O::_nil = nil_for_class;
        classcore__simple_warning_Oval->setInstanceNil(nil_for_class);
    }
    classcore__simple_warning_Oval->setSupportsSlots(core::simple_warning_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__storage_condition_Oval]"));
    core::BuiltInClass_sp classcore__storage_condition_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__storage_condition_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__storage_condition_Oval,_lisp,core::storage_condition_O::static_classSymbol());
    core::storage_condition_O::___staticMetaClass = classcore__storage_condition_Oval;
    _lisp->setf_findClass(core::storage_condition_O::static_classSymbol(),classcore__storage_condition_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::storage_condition_O>;
        core::storage_condition_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::storage_condition_O::static_className() % (void*)(core::storage_condition_O::static_newNil_callback()) );
    classcore__storage_condition_Oval->setInstance_newNil_callback(core::storage_condition_O::static_newNil_callback());
    {
        boost::shared_ptr<core::storage_condition_O> nil_for_class(new core::storage_condition_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::storage_condition_O::static_className() );
        core::storage_condition_O::_nil = nil_for_class;
        classcore__storage_condition_Oval->setInstanceNil(nil_for_class);
    }
    classcore__storage_condition_Oval->setSupportsSlots(core::storage_condition_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__style_warning_Oval]"));
    core::BuiltInClass_sp classcore__style_warning_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__style_warning_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__style_warning_Oval,_lisp,core::style_warning_O::static_classSymbol());
    core::style_warning_O::___staticMetaClass = classcore__style_warning_Oval;
    _lisp->setf_findClass(core::style_warning_O::static_classSymbol(),classcore__style_warning_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::style_warning_O>;
        core::style_warning_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::style_warning_O::static_className() % (void*)(core::style_warning_O::static_newNil_callback()) );
    classcore__style_warning_Oval->setInstance_newNil_callback(core::style_warning_O::static_newNil_callback());
    {
        boost::shared_ptr<core::style_warning_O> nil_for_class(new core::style_warning_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::style_warning_O::static_className() );
        core::style_warning_O::_nil = nil_for_class;
        classcore__style_warning_Oval->setInstanceNil(nil_for_class);
    }
    classcore__style_warning_Oval->setSupportsSlots(core::style_warning_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__LoadTimeValues_Oval]"));
    core::BuiltInClass_sp classcore__LoadTimeValues_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__LoadTimeValues_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__LoadTimeValues_Oval,_lisp,core::LoadTimeValues_O::static_classSymbol());
    core::LoadTimeValues_O::___staticMetaClass = classcore__LoadTimeValues_Oval;
    _lisp->setf_findClass(core::LoadTimeValues_O::static_classSymbol(),classcore__LoadTimeValues_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::LoadTimeValues_O>;
        core::LoadTimeValues_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::LoadTimeValues_O::static_className() % (void*)(core::LoadTimeValues_O::static_newNil_callback()) );
    classcore__LoadTimeValues_Oval->setInstance_newNil_callback(core::LoadTimeValues_O::static_newNil_callback());
    {
        boost::shared_ptr<core::LoadTimeValues_O> nil_for_class(new core::LoadTimeValues_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::LoadTimeValues_O::static_className() );
        core::LoadTimeValues_O::_nil = nil_for_class;
        classcore__LoadTimeValues_Oval->setInstanceNil(nil_for_class);
    }
    classcore__LoadTimeValues_Oval->setSupportsSlots(core::LoadTimeValues_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__arithmetic_error_Oval]"));
    core::BuiltInClass_sp classcore__arithmetic_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__arithmetic_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__arithmetic_error_Oval,_lisp,core::arithmetic_error_O::static_classSymbol());
    core::arithmetic_error_O::___staticMetaClass = classcore__arithmetic_error_Oval;
    _lisp->setf_findClass(core::arithmetic_error_O::static_classSymbol(),classcore__arithmetic_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::arithmetic_error_O>;
        core::arithmetic_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::arithmetic_error_O::static_className() % (void*)(core::arithmetic_error_O::static_newNil_callback()) );
    classcore__arithmetic_error_Oval->setInstance_newNil_callback(core::arithmetic_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::arithmetic_error_O> nil_for_class(new core::arithmetic_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::arithmetic_error_O::static_className() );
        core::arithmetic_error_O::_nil = nil_for_class;
        classcore__arithmetic_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__arithmetic_error_Oval->setSupportsSlots(core::arithmetic_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__cell_error_Oval]"));
    core::BuiltInClass_sp classcore__cell_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__cell_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__cell_error_Oval,_lisp,core::cell_error_O::static_classSymbol());
    core::cell_error_O::___staticMetaClass = classcore__cell_error_Oval;
    _lisp->setf_findClass(core::cell_error_O::static_classSymbol(),classcore__cell_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::cell_error_O>;
        core::cell_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::cell_error_O::static_className() % (void*)(core::cell_error_O::static_newNil_callback()) );
    classcore__cell_error_Oval->setInstance_newNil_callback(core::cell_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::cell_error_O> nil_for_class(new core::cell_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::cell_error_O::static_className() );
        core::cell_error_O::_nil = nil_for_class;
        classcore__cell_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__cell_error_Oval->setSupportsSlots(core::cell_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__control_error_Oval]"));
    core::BuiltInClass_sp classcore__control_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__control_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__control_error_Oval,_lisp,core::control_error_O::static_classSymbol());
    core::control_error_O::___staticMetaClass = classcore__control_error_Oval;
    _lisp->setf_findClass(core::control_error_O::static_classSymbol(),classcore__control_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::control_error_O>;
        core::control_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::control_error_O::static_className() % (void*)(core::control_error_O::static_newNil_callback()) );
    classcore__control_error_Oval->setInstance_newNil_callback(core::control_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::control_error_O> nil_for_class(new core::control_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::control_error_O::static_className() );
        core::control_error_O::_nil = nil_for_class;
        classcore__control_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__control_error_Oval->setSupportsSlots(core::control_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__file_error_Oval]"));
    core::BuiltInClass_sp classcore__file_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__file_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__file_error_Oval,_lisp,core::file_error_O::static_classSymbol());
    core::file_error_O::___staticMetaClass = classcore__file_error_Oval;
    _lisp->setf_findClass(core::file_error_O::static_classSymbol(),classcore__file_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::file_error_O>;
        core::file_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::file_error_O::static_className() % (void*)(core::file_error_O::static_newNil_callback()) );
    classcore__file_error_Oval->setInstance_newNil_callback(core::file_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::file_error_O> nil_for_class(new core::file_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::file_error_O::static_className() );
        core::file_error_O::_nil = nil_for_class;
        classcore__file_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__file_error_Oval->setSupportsSlots(core::file_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__package_error_Oval]"));
    core::BuiltInClass_sp classcore__package_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__package_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__package_error_Oval,_lisp,core::package_error_O::static_classSymbol());
    core::package_error_O::___staticMetaClass = classcore__package_error_Oval;
    _lisp->setf_findClass(core::package_error_O::static_classSymbol(),classcore__package_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::package_error_O>;
        core::package_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::package_error_O::static_className() % (void*)(core::package_error_O::static_newNil_callback()) );
    classcore__package_error_Oval->setInstance_newNil_callback(core::package_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::package_error_O> nil_for_class(new core::package_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::package_error_O::static_className() );
        core::package_error_O::_nil = nil_for_class;
        classcore__package_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__package_error_Oval->setSupportsSlots(core::package_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__parse_error_Oval]"));
    core::BuiltInClass_sp classcore__parse_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__parse_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__parse_error_Oval,_lisp,core::parse_error_O::static_classSymbol());
    core::parse_error_O::___staticMetaClass = classcore__parse_error_Oval;
    _lisp->setf_findClass(core::parse_error_O::static_classSymbol(),classcore__parse_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::parse_error_O>;
        core::parse_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::parse_error_O::static_className() % (void*)(core::parse_error_O::static_newNil_callback()) );
    classcore__parse_error_Oval->setInstance_newNil_callback(core::parse_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::parse_error_O> nil_for_class(new core::parse_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::parse_error_O::static_className() );
        core::parse_error_O::_nil = nil_for_class;
        classcore__parse_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__parse_error_Oval->setSupportsSlots(core::parse_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__print_not_readable_Oval]"));
    core::BuiltInClass_sp classcore__print_not_readable_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__print_not_readable_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__print_not_readable_Oval,_lisp,core::print_not_readable_O::static_classSymbol());
    core::print_not_readable_O::___staticMetaClass = classcore__print_not_readable_Oval;
    _lisp->setf_findClass(core::print_not_readable_O::static_classSymbol(),classcore__print_not_readable_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::print_not_readable_O>;
        core::print_not_readable_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::print_not_readable_O::static_className() % (void*)(core::print_not_readable_O::static_newNil_callback()) );
    classcore__print_not_readable_Oval->setInstance_newNil_callback(core::print_not_readable_O::static_newNil_callback());
    {
        boost::shared_ptr<core::print_not_readable_O> nil_for_class(new core::print_not_readable_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::print_not_readable_O::static_className() );
        core::print_not_readable_O::_nil = nil_for_class;
        classcore__print_not_readable_Oval->setInstanceNil(nil_for_class);
    }
    classcore__print_not_readable_Oval->setSupportsSlots(core::print_not_readable_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__program_error_Oval]"));
    core::BuiltInClass_sp classcore__program_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__program_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__program_error_Oval,_lisp,core::program_error_O::static_classSymbol());
    core::program_error_O::___staticMetaClass = classcore__program_error_Oval;
    _lisp->setf_findClass(core::program_error_O::static_classSymbol(),classcore__program_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::program_error_O>;
        core::program_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::program_error_O::static_className() % (void*)(core::program_error_O::static_newNil_callback()) );
    classcore__program_error_Oval->setInstance_newNil_callback(core::program_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::program_error_O> nil_for_class(new core::program_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::program_error_O::static_className() );
        core::program_error_O::_nil = nil_for_class;
        classcore__program_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__program_error_Oval->setSupportsSlots(core::program_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__simple_error_Oval]"));
    core::BuiltInClass_sp classcore__simple_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__simple_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__simple_error_Oval,_lisp,core::simple_error_O::static_classSymbol());
    core::simple_error_O::___staticMetaClass = classcore__simple_error_Oval;
    _lisp->setf_findClass(core::simple_error_O::static_classSymbol(),classcore__simple_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::simple_error_O>;
        core::simple_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::simple_error_O::static_className() % (void*)(core::simple_error_O::static_newNil_callback()) );
    classcore__simple_error_Oval->setInstance_newNil_callback(core::simple_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::simple_error_O> nil_for_class(new core::simple_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::simple_error_O::static_className() );
        core::simple_error_O::_nil = nil_for_class;
        classcore__simple_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__simple_error_Oval->setSupportsSlots(core::simple_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__stream_error_Oval]"));
    core::BuiltInClass_sp classcore__stream_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__stream_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__stream_error_Oval,_lisp,core::stream_error_O::static_classSymbol());
    core::stream_error_O::___staticMetaClass = classcore__stream_error_Oval;
    _lisp->setf_findClass(core::stream_error_O::static_classSymbol(),classcore__stream_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::stream_error_O>;
        core::stream_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::stream_error_O::static_className() % (void*)(core::stream_error_O::static_newNil_callback()) );
    classcore__stream_error_Oval->setInstance_newNil_callback(core::stream_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::stream_error_O> nil_for_class(new core::stream_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::stream_error_O::static_className() );
        core::stream_error_O::_nil = nil_for_class;
        classcore__stream_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__stream_error_Oval->setSupportsSlots(core::stream_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__type_error_Oval]"));
    core::BuiltInClass_sp classcore__type_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__type_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__type_error_Oval,_lisp,core::type_error_O::static_classSymbol());
    core::type_error_O::___staticMetaClass = classcore__type_error_Oval;
    _lisp->setf_findClass(core::type_error_O::static_classSymbol(),classcore__type_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::type_error_O>;
        core::type_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::type_error_O::static_className() % (void*)(core::type_error_O::static_newNil_callback()) );
    classcore__type_error_Oval->setInstance_newNil_callback(core::type_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::type_error_O> nil_for_class(new core::type_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::type_error_O::static_className() );
        core::type_error_O::_nil = nil_for_class;
        classcore__type_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__type_error_Oval->setSupportsSlots(core::type_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__argument_mismatch_error_Oval]"));
    core::BuiltInClass_sp classcore__argument_mismatch_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__argument_mismatch_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__argument_mismatch_error_Oval,_lisp,core::argument_mismatch_error_O::static_classSymbol());
    core::argument_mismatch_error_O::___staticMetaClass = classcore__argument_mismatch_error_Oval;
    _lisp->setf_findClass(core::argument_mismatch_error_O::static_classSymbol(),classcore__argument_mismatch_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::argument_mismatch_error_O>;
        core::argument_mismatch_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::argument_mismatch_error_O::static_className() % (void*)(core::argument_mismatch_error_O::static_newNil_callback()) );
    classcore__argument_mismatch_error_Oval->setInstance_newNil_callback(core::argument_mismatch_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::argument_mismatch_error_O> nil_for_class(new core::argument_mismatch_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::argument_mismatch_error_O::static_className() );
        core::argument_mismatch_error_O::_nil = nil_for_class;
        classcore__argument_mismatch_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__argument_mismatch_error_Oval->setSupportsSlots(core::argument_mismatch_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__division_by_zero_Oval]"));
    core::BuiltInClass_sp classcore__division_by_zero_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__division_by_zero_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__division_by_zero_Oval,_lisp,core::division_by_zero_O::static_classSymbol());
    core::division_by_zero_O::___staticMetaClass = classcore__division_by_zero_Oval;
    _lisp->setf_findClass(core::division_by_zero_O::static_classSymbol(),classcore__division_by_zero_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::division_by_zero_O>;
        core::division_by_zero_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::division_by_zero_O::static_className() % (void*)(core::division_by_zero_O::static_newNil_callback()) );
    classcore__division_by_zero_Oval->setInstance_newNil_callback(core::division_by_zero_O::static_newNil_callback());
    {
        boost::shared_ptr<core::division_by_zero_O> nil_for_class(new core::division_by_zero_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::division_by_zero_O::static_className() );
        core::division_by_zero_O::_nil = nil_for_class;
        classcore__division_by_zero_Oval->setInstanceNil(nil_for_class);
    }
    classcore__division_by_zero_Oval->setSupportsSlots(core::division_by_zero_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__end_of_file_Oval]"));
    core::BuiltInClass_sp classcore__end_of_file_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__end_of_file_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__end_of_file_Oval,_lisp,core::end_of_file_O::static_classSymbol());
    core::end_of_file_O::___staticMetaClass = classcore__end_of_file_Oval;
    _lisp->setf_findClass(core::end_of_file_O::static_classSymbol(),classcore__end_of_file_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::end_of_file_O>;
        core::end_of_file_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::end_of_file_O::static_className() % (void*)(core::end_of_file_O::static_newNil_callback()) );
    classcore__end_of_file_Oval->setInstance_newNil_callback(core::end_of_file_O::static_newNil_callback());
    {
        boost::shared_ptr<core::end_of_file_O> nil_for_class(new core::end_of_file_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::end_of_file_O::static_className() );
        core::end_of_file_O::_nil = nil_for_class;
        classcore__end_of_file_Oval->setInstanceNil(nil_for_class);
    }
    classcore__end_of_file_Oval->setSupportsSlots(core::end_of_file_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__floating_point_inexact_Oval]"));
    core::BuiltInClass_sp classcore__floating_point_inexact_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__floating_point_inexact_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__floating_point_inexact_Oval,_lisp,core::floating_point_inexact_O::static_classSymbol());
    core::floating_point_inexact_O::___staticMetaClass = classcore__floating_point_inexact_Oval;
    _lisp->setf_findClass(core::floating_point_inexact_O::static_classSymbol(),classcore__floating_point_inexact_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::floating_point_inexact_O>;
        core::floating_point_inexact_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::floating_point_inexact_O::static_className() % (void*)(core::floating_point_inexact_O::static_newNil_callback()) );
    classcore__floating_point_inexact_Oval->setInstance_newNil_callback(core::floating_point_inexact_O::static_newNil_callback());
    {
        boost::shared_ptr<core::floating_point_inexact_O> nil_for_class(new core::floating_point_inexact_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::floating_point_inexact_O::static_className() );
        core::floating_point_inexact_O::_nil = nil_for_class;
        classcore__floating_point_inexact_Oval->setInstanceNil(nil_for_class);
    }
    classcore__floating_point_inexact_Oval->setSupportsSlots(core::floating_point_inexact_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__floating_point_invalid_operation_Oval]"));
    core::BuiltInClass_sp classcore__floating_point_invalid_operation_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__floating_point_invalid_operation_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__floating_point_invalid_operation_Oval,_lisp,core::floating_point_invalid_operation_O::static_classSymbol());
    core::floating_point_invalid_operation_O::___staticMetaClass = classcore__floating_point_invalid_operation_Oval;
    _lisp->setf_findClass(core::floating_point_invalid_operation_O::static_classSymbol(),classcore__floating_point_invalid_operation_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::floating_point_invalid_operation_O>;
        core::floating_point_invalid_operation_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::floating_point_invalid_operation_O::static_className() % (void*)(core::floating_point_invalid_operation_O::static_newNil_callback()) );
    classcore__floating_point_invalid_operation_Oval->setInstance_newNil_callback(core::floating_point_invalid_operation_O::static_newNil_callback());
    {
        boost::shared_ptr<core::floating_point_invalid_operation_O> nil_for_class(new core::floating_point_invalid_operation_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::floating_point_invalid_operation_O::static_className() );
        core::floating_point_invalid_operation_O::_nil = nil_for_class;
        classcore__floating_point_invalid_operation_Oval->setInstanceNil(nil_for_class);
    }
    classcore__floating_point_invalid_operation_Oval->setSupportsSlots(core::floating_point_invalid_operation_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__floating_point_overflow_Oval]"));
    core::BuiltInClass_sp classcore__floating_point_overflow_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__floating_point_overflow_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__floating_point_overflow_Oval,_lisp,core::floating_point_overflow_O::static_classSymbol());
    core::floating_point_overflow_O::___staticMetaClass = classcore__floating_point_overflow_Oval;
    _lisp->setf_findClass(core::floating_point_overflow_O::static_classSymbol(),classcore__floating_point_overflow_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::floating_point_overflow_O>;
        core::floating_point_overflow_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::floating_point_overflow_O::static_className() % (void*)(core::floating_point_overflow_O::static_newNil_callback()) );
    classcore__floating_point_overflow_Oval->setInstance_newNil_callback(core::floating_point_overflow_O::static_newNil_callback());
    {
        boost::shared_ptr<core::floating_point_overflow_O> nil_for_class(new core::floating_point_overflow_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::floating_point_overflow_O::static_className() );
        core::floating_point_overflow_O::_nil = nil_for_class;
        classcore__floating_point_overflow_Oval->setInstanceNil(nil_for_class);
    }
    classcore__floating_point_overflow_Oval->setSupportsSlots(core::floating_point_overflow_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__floating_point_underflow_Oval]"));
    core::BuiltInClass_sp classcore__floating_point_underflow_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__floating_point_underflow_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__floating_point_underflow_Oval,_lisp,core::floating_point_underflow_O::static_classSymbol());
    core::floating_point_underflow_O::___staticMetaClass = classcore__floating_point_underflow_Oval;
    _lisp->setf_findClass(core::floating_point_underflow_O::static_classSymbol(),classcore__floating_point_underflow_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::floating_point_underflow_O>;
        core::floating_point_underflow_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::floating_point_underflow_O::static_className() % (void*)(core::floating_point_underflow_O::static_newNil_callback()) );
    classcore__floating_point_underflow_Oval->setInstance_newNil_callback(core::floating_point_underflow_O::static_newNil_callback());
    {
        boost::shared_ptr<core::floating_point_underflow_O> nil_for_class(new core::floating_point_underflow_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::floating_point_underflow_O::static_className() );
        core::floating_point_underflow_O::_nil = nil_for_class;
        classcore__floating_point_underflow_Oval->setInstanceNil(nil_for_class);
    }
    classcore__floating_point_underflow_Oval->setSupportsSlots(core::floating_point_underflow_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__reader_error_Oval]"));
    core::BuiltInClass_sp classcore__reader_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__reader_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__reader_error_Oval,_lisp,core::reader_error_O::static_classSymbol());
    core::reader_error_O::___staticMetaClass = classcore__reader_error_Oval;
    _lisp->setf_findClass(core::reader_error_O::static_classSymbol(),classcore__reader_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::reader_error_O>;
        core::reader_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::reader_error_O::static_className() % (void*)(core::reader_error_O::static_newNil_callback()) );
    classcore__reader_error_Oval->setInstance_newNil_callback(core::reader_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::reader_error_O> nil_for_class(new core::reader_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::reader_error_O::static_className() );
        core::reader_error_O::_nil = nil_for_class;
        classcore__reader_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__reader_error_Oval->setSupportsSlots(core::reader_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__simple_type_error_Oval]"));
    core::BuiltInClass_sp classcore__simple_type_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__simple_type_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__simple_type_error_Oval,_lisp,core::simple_type_error_O::static_classSymbol());
    core::simple_type_error_O::___staticMetaClass = classcore__simple_type_error_Oval;
    _lisp->setf_findClass(core::simple_type_error_O::static_classSymbol(),classcore__simple_type_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::simple_type_error_O>;
        core::simple_type_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::simple_type_error_O::static_className() % (void*)(core::simple_type_error_O::static_newNil_callback()) );
    classcore__simple_type_error_Oval->setInstance_newNil_callback(core::simple_type_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::simple_type_error_O> nil_for_class(new core::simple_type_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::simple_type_error_O::static_className() );
        core::simple_type_error_O::_nil = nil_for_class;
        classcore__simple_type_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__simple_type_error_Oval->setSupportsSlots(core::simple_type_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__unbound_slot_Oval]"));
    core::BuiltInClass_sp classcore__unbound_slot_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__unbound_slot_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__unbound_slot_Oval,_lisp,core::unbound_slot_O::static_classSymbol());
    core::unbound_slot_O::___staticMetaClass = classcore__unbound_slot_Oval;
    _lisp->setf_findClass(core::unbound_slot_O::static_classSymbol(),classcore__unbound_slot_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::unbound_slot_O>;
        core::unbound_slot_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::unbound_slot_O::static_className() % (void*)(core::unbound_slot_O::static_newNil_callback()) );
    classcore__unbound_slot_Oval->setInstance_newNil_callback(core::unbound_slot_O::static_newNil_callback());
    {
        boost::shared_ptr<core::unbound_slot_O> nil_for_class(new core::unbound_slot_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::unbound_slot_O::static_className() );
        core::unbound_slot_O::_nil = nil_for_class;
        classcore__unbound_slot_Oval->setInstanceNil(nil_for_class);
    }
    classcore__unbound_slot_Oval->setSupportsSlots(core::unbound_slot_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__unbound_variable_Oval]"));
    core::BuiltInClass_sp classcore__unbound_variable_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__unbound_variable_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__unbound_variable_Oval,_lisp,core::unbound_variable_O::static_classSymbol());
    core::unbound_variable_O::___staticMetaClass = classcore__unbound_variable_Oval;
    _lisp->setf_findClass(core::unbound_variable_O::static_classSymbol(),classcore__unbound_variable_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::unbound_variable_O>;
        core::unbound_variable_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::unbound_variable_O::static_className() % (void*)(core::unbound_variable_O::static_newNil_callback()) );
    classcore__unbound_variable_Oval->setInstance_newNil_callback(core::unbound_variable_O::static_newNil_callback());
    {
        boost::shared_ptr<core::unbound_variable_O> nil_for_class(new core::unbound_variable_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::unbound_variable_O::static_className() );
        core::unbound_variable_O::_nil = nil_for_class;
        classcore__unbound_variable_Oval->setInstanceNil(nil_for_class);
    }
    classcore__unbound_variable_Oval->setSupportsSlots(core::unbound_variable_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__undefined_function_Oval]"));
    core::BuiltInClass_sp classcore__undefined_function_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__undefined_function_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__undefined_function_Oval,_lisp,core::undefined_function_O::static_classSymbol());
    core::undefined_function_O::___staticMetaClass = classcore__undefined_function_Oval;
    _lisp->setf_findClass(core::undefined_function_O::static_classSymbol(),classcore__undefined_function_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::undefined_function_O>;
        core::undefined_function_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::undefined_function_O::static_className() % (void*)(core::undefined_function_O::static_newNil_callback()) );
    classcore__undefined_function_Oval->setInstance_newNil_callback(core::undefined_function_O::static_newNil_callback());
    {
        boost::shared_ptr<core::undefined_function_O> nil_for_class(new core::undefined_function_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::undefined_function_O::static_className() );
        core::undefined_function_O::_nil = nil_for_class;
        classcore__undefined_function_Oval->setInstanceNil(nil_for_class);
    }
    classcore__undefined_function_Oval->setSupportsSlots(core::undefined_function_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__invalid_keyword_arguments_error_Oval]"));
    core::BuiltInClass_sp classcore__invalid_keyword_arguments_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__invalid_keyword_arguments_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__invalid_keyword_arguments_error_Oval,_lisp,core::invalid_keyword_arguments_error_O::static_classSymbol());
    core::invalid_keyword_arguments_error_O::___staticMetaClass = classcore__invalid_keyword_arguments_error_Oval;
    _lisp->setf_findClass(core::invalid_keyword_arguments_error_O::static_classSymbol(),classcore__invalid_keyword_arguments_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::invalid_keyword_arguments_error_O>;
        core::invalid_keyword_arguments_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::invalid_keyword_arguments_error_O::static_className() % (void*)(core::invalid_keyword_arguments_error_O::static_newNil_callback()) );
    classcore__invalid_keyword_arguments_error_Oval->setInstance_newNil_callback(core::invalid_keyword_arguments_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::invalid_keyword_arguments_error_O> nil_for_class(new core::invalid_keyword_arguments_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::invalid_keyword_arguments_error_O::static_className() );
        core::invalid_keyword_arguments_error_O::_nil = nil_for_class;
        classcore__invalid_keyword_arguments_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__invalid_keyword_arguments_error_Oval->setSupportsSlots(core::invalid_keyword_arguments_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__odd_number_of_keyword_arguments_error_Oval]"));
    core::BuiltInClass_sp classcore__odd_number_of_keyword_arguments_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__odd_number_of_keyword_arguments_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__odd_number_of_keyword_arguments_error_Oval,_lisp,core::odd_number_of_keyword_arguments_error_O::static_classSymbol());
    core::odd_number_of_keyword_arguments_error_O::___staticMetaClass = classcore__odd_number_of_keyword_arguments_error_Oval;
    _lisp->setf_findClass(core::odd_number_of_keyword_arguments_error_O::static_classSymbol(),classcore__odd_number_of_keyword_arguments_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::odd_number_of_keyword_arguments_error_O>;
        core::odd_number_of_keyword_arguments_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::odd_number_of_keyword_arguments_error_O::static_className() % (void*)(core::odd_number_of_keyword_arguments_error_O::static_newNil_callback()) );
    classcore__odd_number_of_keyword_arguments_error_Oval->setInstance_newNil_callback(core::odd_number_of_keyword_arguments_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::odd_number_of_keyword_arguments_error_O> nil_for_class(new core::odd_number_of_keyword_arguments_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::odd_number_of_keyword_arguments_error_O::static_className() );
        core::odd_number_of_keyword_arguments_error_O::_nil = nil_for_class;
        classcore__odd_number_of_keyword_arguments_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__odd_number_of_keyword_arguments_error_Oval->setSupportsSlots(core::odd_number_of_keyword_arguments_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__single_dispatch_arguments_error_Oval]"));
    core::BuiltInClass_sp classcore__single_dispatch_arguments_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__single_dispatch_arguments_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__single_dispatch_arguments_error_Oval,_lisp,core::single_dispatch_arguments_error_O::static_classSymbol());
    core::single_dispatch_arguments_error_O::___staticMetaClass = classcore__single_dispatch_arguments_error_Oval;
    _lisp->setf_findClass(core::single_dispatch_arguments_error_O::static_classSymbol(),classcore__single_dispatch_arguments_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::single_dispatch_arguments_error_O>;
        core::single_dispatch_arguments_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::single_dispatch_arguments_error_O::static_className() % (void*)(core::single_dispatch_arguments_error_O::static_newNil_callback()) );
    classcore__single_dispatch_arguments_error_Oval->setInstance_newNil_callback(core::single_dispatch_arguments_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::single_dispatch_arguments_error_O> nil_for_class(new core::single_dispatch_arguments_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::single_dispatch_arguments_error_O::static_className() );
        core::single_dispatch_arguments_error_O::_nil = nil_for_class;
        classcore__single_dispatch_arguments_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__single_dispatch_arguments_error_Oval->setSupportsSlots(core::single_dispatch_arguments_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__too_few_arguments_error_Oval]"));
    core::BuiltInClass_sp classcore__too_few_arguments_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__too_few_arguments_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__too_few_arguments_error_Oval,_lisp,core::too_few_arguments_error_O::static_classSymbol());
    core::too_few_arguments_error_O::___staticMetaClass = classcore__too_few_arguments_error_Oval;
    _lisp->setf_findClass(core::too_few_arguments_error_O::static_classSymbol(),classcore__too_few_arguments_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::too_few_arguments_error_O>;
        core::too_few_arguments_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::too_few_arguments_error_O::static_className() % (void*)(core::too_few_arguments_error_O::static_newNil_callback()) );
    classcore__too_few_arguments_error_Oval->setInstance_newNil_callback(core::too_few_arguments_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::too_few_arguments_error_O> nil_for_class(new core::too_few_arguments_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::too_few_arguments_error_O::static_className() );
        core::too_few_arguments_error_O::_nil = nil_for_class;
        classcore__too_few_arguments_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__too_few_arguments_error_Oval->setSupportsSlots(core::too_few_arguments_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__too_many_arguments_error_Oval]"));
    core::BuiltInClass_sp classcore__too_many_arguments_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__too_many_arguments_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__too_many_arguments_error_Oval,_lisp,core::too_many_arguments_error_O::static_classSymbol());
    core::too_many_arguments_error_O::___staticMetaClass = classcore__too_many_arguments_error_Oval;
    _lisp->setf_findClass(core::too_many_arguments_error_O::static_classSymbol(),classcore__too_many_arguments_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::too_many_arguments_error_O>;
        core::too_many_arguments_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::too_many_arguments_error_O::static_className() % (void*)(core::too_many_arguments_error_O::static_newNil_callback()) );
    classcore__too_many_arguments_error_Oval->setInstance_newNil_callback(core::too_many_arguments_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::too_many_arguments_error_O> nil_for_class(new core::too_many_arguments_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::too_many_arguments_error_O::static_className() );
        core::too_many_arguments_error_O::_nil = nil_for_class;
        classcore__too_many_arguments_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__too_many_arguments_error_Oval->setSupportsSlots(core::too_many_arguments_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classcore__unrecognized_keyword_arguments_error_Oval]"));
    core::BuiltInClass_sp classcore__unrecognized_keyword_arguments_error_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classcore__unrecognized_keyword_arguments_error_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcore__unrecognized_keyword_arguments_error_Oval,_lisp,core::unrecognized_keyword_arguments_error_O::static_classSymbol());
    core::unrecognized_keyword_arguments_error_O::___staticMetaClass = classcore__unrecognized_keyword_arguments_error_Oval;
    _lisp->setf_findClass(core::unrecognized_keyword_arguments_error_O::static_classSymbol(),classcore__unrecognized_keyword_arguments_error_Oval);
    {
        AllocatorCallback cb = &new_Nil<core::unrecognized_keyword_arguments_error_O>;
        core::unrecognized_keyword_arguments_error_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% core::unrecognized_keyword_arguments_error_O::static_className() % (void*)(core::unrecognized_keyword_arguments_error_O::static_newNil_callback()) );
    classcore__unrecognized_keyword_arguments_error_Oval->setInstance_newNil_callback(core::unrecognized_keyword_arguments_error_O::static_newNil_callback());
    {
        boost::shared_ptr<core::unrecognized_keyword_arguments_error_O> nil_for_class(new core::unrecognized_keyword_arguments_error_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % core::unrecognized_keyword_arguments_error_O::static_className() );
        core::unrecognized_keyword_arguments_error_O::_nil = nil_for_class;
        classcore__unrecognized_keyword_arguments_error_Oval->setInstanceNil(nil_for_class);
    }
    classcore__unrecognized_keyword_arguments_error_Oval->setSupportsSlots(core::unrecognized_keyword_arguments_error_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */
#endif // CREATE_CLASS
#undef CREATE_CLASS
#ifdef DUMP_INFO_CLASS // {
// Depends on nothing

    LOG(BF("---    dump_info   --- className: core::T_O @ %X") % classcore__T_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::T_O::static_className() % core::T_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ArchiveMemoryManager_O @ %X") % classcore__ArchiveMemoryManager_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ArchiveMemoryManager_O::static_className() % core::ArchiveMemoryManager_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Archive_O @ %X") % classcore__Archive_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Archive_O::static_className() % core::Archive_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ArrayVirtual_O @ %X") % classcore__ArrayVirtual_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ArrayVirtual_O::static_className() % core::ArrayVirtual_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Binder_O @ %X") % classcore__Binder_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Binder_O::static_className() % core::Binder_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Character_O @ %X") % classcore__Character_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Character_O::static_className() % core::Character_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ConditionBase_O @ %X") % classcore__ConditionBase_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ConditionBase_O::static_className() % core::ConditionBase_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ConditionHandlerBase_O @ %X") % classcore__ConditionHandlerBase_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ConditionHandlerBase_O::static_className() % core::ConditionHandlerBase_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::DirectoryEntry_O @ %X") % classcore__DirectoryEntry_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::DirectoryEntry_O::static_className() % core::DirectoryEntry_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Environment_O @ %X") % classcore__Environment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Environment_O::static_className() % core::Environment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ExternalObjectManager_O @ %X") % classcore__ExternalObjectManager_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ExternalObjectManager_O::static_className() % core::ExternalObjectManager_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ExternalObject_O @ %X") % classcore__ExternalObject_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ExternalObject_O::static_className() % core::ExternalObject_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FileStatus_O @ %X") % classcore__FileStatus_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FileStatus_O::static_className() % core::FileStatus_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FormBinder_O @ %X") % classcore__FormBinder_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FormBinder_O::static_className() % core::FormBinder_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Function_O @ %X") % classcore__Function_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Function_O::static_className() % core::Function_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::HashTable_O @ %X") % classcore__HashTable_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::HashTable_O::static_className() % core::HashTable_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Instance_O @ %X") % classcore__Instance_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Instance_O::static_className() % core::Instance_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::IntArray_O @ %X") % classcore__IntArray_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::IntArray_O::static_className() % core::IntArray_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::IntegerKeyObjectDictionary_O @ %X") % classcore__IntegerKeyObjectDictionary_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::IntegerKeyObjectDictionary_O::static_className() % core::IntegerKeyObjectDictionary_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Iterator_O @ %X") % classcore__Iterator_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Iterator_O::static_className() % core::Iterator_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::LambdaListHandler_O @ %X") % classcore__LambdaListHandler_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::LambdaListHandler_O::static_className() % core::LambdaListHandler_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Lisp_O @ %X") % classcore__Lisp_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Lisp_O::static_className() % core::Lisp_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::MethodCombination_O @ %X") % classcore__MethodCombination_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::MethodCombination_O::static_className() % core::MethodCombination_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::MethodSet_O @ %X") % classcore__MethodSet_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::MethodSet_O::static_className() % core::MethodSet_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Method_O @ %X") % classcore__Method_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Method_O::static_className() % core::Method_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::MicroHeap_O @ %X") % classcore__MicroHeap_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::MicroHeap_O::static_className() % core::MicroHeap_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Model_O @ %X") % classcore__Model_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Model_O::static_className() % core::Model_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::MultiStringBuffer_O @ %X") % classcore__MultiStringBuffer_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::MultiStringBuffer_O::static_className() % core::MultiStringBuffer_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::MultipleValues_O @ %X") % classcore__MultipleValues_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::MultipleValues_O::static_className() % core::MultipleValues_O::static_classSymbol() );

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

    LOG(BF("---    dump_info   --- className: core::Pointer_O @ %X") % classcore__Pointer_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Pointer_O::static_className() % core::Pointer_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::PosixTimeDuration_O @ %X") % classcore__PosixTimeDuration_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::PosixTimeDuration_O::static_className() % core::PosixTimeDuration_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::PosixTime_O @ %X") % classcore__PosixTime_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::PosixTime_O::static_className() % core::PosixTime_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::QDomNode_O @ %X") % classcore__QDomNode_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::QDomNode_O::static_className() % core::QDomNode_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ReadTable_O @ %X") % classcore__ReadTable_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ReadTable_O::static_className() % core::ReadTable_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Reader_O @ %X") % classcore__Reader_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Reader_O::static_className() % core::Reader_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::RestartCase_O @ %X") % classcore__RestartCase_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::RestartCase_O::static_className() % core::RestartCase_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SequenceVirtual_O @ %X") % classcore__SequenceVirtual_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SequenceVirtual_O::static_className() % core::SequenceVirtual_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Serializer_O @ %X") % classcore__Serializer_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Serializer_O::static_className() % core::Serializer_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SetfExpander_O @ %X") % classcore__SetfExpander_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SetfExpander_O::static_className() % core::SetfExpander_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SingleDispatchMethod_O @ %X") % classcore__SingleDispatchMethod_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SingleDispatchMethod_O::static_className() % core::SingleDispatchMethod_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SourceFileInfo_O @ %X") % classcore__SourceFileInfo_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SourceFileInfo_O::static_className() % core::SourceFileInfo_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StandardObject_O @ %X") % classcore__StandardObject_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StandardObject_O::static_className() % core::StandardObject_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Stream_O @ %X") % classcore__Stream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Stream_O::static_className() % core::Stream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StringList_O @ %X") % classcore__StringList_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StringList_O::static_className() % core::StringList_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StringSet_O @ %X") % classcore__StringSet_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StringSet_O::static_className() % core::StringSet_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SymbolList_O @ %X") % classcore__SymbolList_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SymbolList_O::static_className() % core::SymbolList_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SymbolSet_O @ %X") % classcore__SymbolSet_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SymbolSet_O::static_className() % core::SymbolSet_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SymbolToEnumConverter_O @ %X") % classcore__SymbolToEnumConverter_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SymbolToEnumConverter_O::static_className() % core::SymbolToEnumConverter_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SymbolVectorMap_O @ %X") % classcore__SymbolVectorMap_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SymbolVectorMap_O::static_className() % core::SymbolVectorMap_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SymbolVector_O @ %X") % classcore__SymbolVector_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SymbolVector_O::static_className() % core::SymbolVector_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Symbol_O @ %X") % classcore__Symbol_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Symbol_O::static_className() % core::Symbol_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::WeakReference_O @ %X") % classcore__WeakReference_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::WeakReference_O::static_className() % core::WeakReference_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ActivationFrame_O @ %X") % classcore__ActivationFrame_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ActivationFrame_O::static_className() % core::ActivationFrame_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Array_O @ %X") % classcore__Array_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Array_O::static_className() % core::Array_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::BaseChar_O @ %X") % classcore__BaseChar_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::BaseChar_O::static_className() % core::BaseChar_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Bool_O @ %X") % classcore__Bool_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Bool_O::static_className() % core::Bool_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::BroadcastStream_O @ %X") % classcore__BroadcastStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::BroadcastStream_O::static_className() % core::BroadcastStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::BuiltIn_O @ %X") % classcore__BuiltIn_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::BuiltIn_O::static_className() % core::BuiltIn_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Closure_O @ %X") % classcore__Closure_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Closure_O::static_className() % core::Closure_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Complex_O @ %X") % classcore__Complex_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Complex_O::static_className() % core::Complex_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ConcatenatedStream_O @ %X") % classcore__ConcatenatedStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ConcatenatedStream_O::static_className() % core::ConcatenatedStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ConditionWithStackTrace_O @ %X") % classcore__ConditionWithStackTrace_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ConditionWithStackTrace_O::static_className() % core::ConditionWithStackTrace_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::DirectoryIterator_O @ %X") % classcore__DirectoryIterator_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::DirectoryIterator_O::static_className() % core::DirectoryIterator_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::EchoStream_O @ %X") % classcore__EchoStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::EchoStream_O::static_className() % core::EchoStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ExtendedChar_O @ %X") % classcore__ExtendedChar_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ExtendedChar_O::static_className() % core::ExtendedChar_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FDStream_O @ %X") % classcore__FDStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FDStream_O::static_className() % core::FDStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FileStream_O @ %X") % classcore__FileStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FileStream_O::static_className() % core::FileStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FunctionNameSetfExpander_O @ %X") % classcore__FunctionNameSetfExpander_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FunctionNameSetfExpander_O::static_className() % core::FunctionNameSetfExpander_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FunctionSetfExpander_O @ %X") % classcore__FunctionSetfExpander_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FunctionSetfExpander_O::static_className() % core::FunctionSetfExpander_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::GenericFunction_O @ %X") % classcore__GenericFunction_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::GenericFunction_O::static_className() % core::GenericFunction_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::GlueEnvironment_O @ %X") % classcore__GlueEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::GlueEnvironment_O::static_className() % core::GlueEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::HandlerBindHandler_O @ %X") % classcore__HandlerBindHandler_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::HandlerBindHandler_O::static_className() % core::HandlerBindHandler_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::HandlerCaseHandler_O @ %X") % classcore__HandlerCaseHandler_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::HandlerCaseHandler_O::static_className() % core::HandlerCaseHandler_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::HashTableEq_O @ %X") % classcore__HashTableEq_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::HashTableEq_O::static_className() % core::HashTableEq_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::HashTableEql_O @ %X") % classcore__HashTableEql_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::HashTableEql_O::static_className() % core::HashTableEql_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::HashTableEqual_O @ %X") % classcore__HashTableEqual_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::HashTableEqual_O::static_className() % core::HashTableEqual_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::HashTableEqualp_O @ %X") % classcore__HashTableEqualp_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::HashTableEqualp_O::static_className() % core::HashTableEqualp_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Interpreted_O @ %X") % classcore__Interpreted_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Interpreted_O::static_className() % core::Interpreted_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::LexicalEnvironment_O @ %X") % classcore__LexicalEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::LexicalEnvironment_O::static_className() % core::LexicalEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::LoadArchive_O @ %X") % classcore__LoadArchive_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::LoadArchive_O::static_className() % core::LoadArchive_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::MetaClass_O @ %X") % classcore__MetaClass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::MetaClass_O::static_className() % core::MetaClass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::MetaObject_O @ %X") % classcore__MetaObject_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::MetaObject_O::static_className() % core::MetaObject_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ReadSerializer_O @ %X") % classcore__ReadSerializer_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ReadSerializer_O::static_className() % core::ReadSerializer_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Real_O @ %X") % classcore__Real_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Real_O::static_className() % core::Real_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::RecursiveDirectoryIterator_O @ %X") % classcore__RecursiveDirectoryIterator_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::RecursiveDirectoryIterator_O::static_className() % core::RecursiveDirectoryIterator_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SaveArchive_O @ %X") % classcore__SaveArchive_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SaveArchive_O::static_className() % core::SaveArchive_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Sequence_O @ %X") % classcore__Sequence_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Sequence_O::static_className() % core::Sequence_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SingleDispatchEffectiveMethodFunction_O @ %X") % classcore__SingleDispatchEffectiveMethodFunction_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SingleDispatchEffectiveMethodFunction_O::static_className() % core::SingleDispatchEffectiveMethodFunction_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SlotValueSetfExpander_O @ %X") % classcore__SlotValueSetfExpander_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SlotValueSetfExpander_O::static_className() % core::SlotValueSetfExpander_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SpecialForm_O @ %X") % classcore__SpecialForm_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SpecialForm_O::static_className() % core::SpecialForm_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StringStream_O @ %X") % classcore__StringStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StringStream_O::static_className() % core::StringStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StructureObject_O @ %X") % classcore__StructureObject_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StructureObject_O::static_className() % core::StructureObject_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SynonymStream_O @ %X") % classcore__SynonymStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SynonymStream_O::static_className() % core::SynonymStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::TwoWayStream_O @ %X") % classcore__TwoWayStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::TwoWayStream_O::static_className() % core::TwoWayStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::WriteSerializer_O @ %X") % classcore__WriteSerializer_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::WriteSerializer_O::static_className() % core::WriteSerializer_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ArrayObjects_O @ %X") % classcore__ArrayObjects_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ArrayObjects_O::static_className() % core::ArrayObjects_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::BuiltInClass_O @ %X") % classcore__BuiltInClass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::BuiltInClass_O::static_className() % core::BuiltInClass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::CandoException_O @ %X") % classcore__CandoException_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::CandoException_O::static_className() % core::CandoException_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::CandoFormatSaveArchive_O @ %X") % classcore__CandoFormatSaveArchive_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::CandoFormatSaveArchive_O::static_className() % core::CandoFormatSaveArchive_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::CompileTimeEnvironment_O @ %X") % classcore__CompileTimeEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::CompileTimeEnvironment_O::static_className() % core::CompileTimeEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Condition_O @ %X") % classcore__Condition_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Condition_O::static_className() % core::Condition_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FDInStream_O @ %X") % classcore__FDInStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FDInStream_O::static_className() % core::FDInStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FDOutStream_O @ %X") % classcore__FDOutStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FDOutStream_O::static_className() % core::FDOutStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FileInCompressedStream_O @ %X") % classcore__FileInCompressedStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FileInCompressedStream_O::static_className() % core::FileInCompressedStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FileInStream_O @ %X") % classcore__FileInStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FileInStream_O::static_className() % core::FileInStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FileOutCompressedStream_O @ %X") % classcore__FileOutCompressedStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FileOutCompressedStream_O::static_className() % core::FileOutCompressedStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FileOutStream_O @ %X") % classcore__FileOutStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FileOutStream_O::static_className() % core::FileOutStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Float_O @ %X") % classcore__Float_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Float_O::static_className() % core::Float_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ForwardReferencedClass_O @ %X") % classcore__ForwardReferencedClass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ForwardReferencedClass_O::static_className() % core::ForwardReferencedClass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FunctionFrame_O @ %X") % classcore__FunctionFrame_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FunctionFrame_O::static_className() % core::FunctionFrame_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::List_O @ %X") % classcore__List_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::List_O::static_className() % core::List_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Rational_O @ %X") % classcore__Rational_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Rational_O::static_className() % core::Rational_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::RuntimeVisibleEnvironment_O @ %X") % classcore__RuntimeVisibleEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::RuntimeVisibleEnvironment_O::static_className() % core::RuntimeVisibleEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SingleDispatchGenericFunction_O @ %X") % classcore__SingleDispatchGenericFunction_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SingleDispatchGenericFunction_O::static_className() % core::SingleDispatchGenericFunction_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StandardChar_O @ %X") % classcore__StandardChar_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StandardChar_O::static_className() % core::StandardChar_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StandardClass_O @ %X") % classcore__StandardClass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StandardClass_O::static_className() % core::StandardClass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StandardGenericFunction_O @ %X") % classcore__StandardGenericFunction_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StandardGenericFunction_O::static_className() % core::StandardGenericFunction_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StringInStream_O @ %X") % classcore__StringInStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StringInStream_O::static_className() % core::StringInStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StringOutStream_O @ %X") % classcore__StringOutStream_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StringOutStream_O::static_className() % core::StringOutStream_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::StructureClass_O @ %X") % classcore__StructureClass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::StructureClass_O::static_className() % core::StructureClass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ValueFrame_O @ %X") % classcore__ValueFrame_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ValueFrame_O::static_className() % core::ValueFrame_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Vector_O @ %X") % classcore__Vector_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Vector_O::static_className() % core::Vector_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::XmlLoadArchive_O @ %X") % classcore__XmlLoadArchive_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::XmlLoadArchive_O::static_className() % core::XmlLoadArchive_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::XmlSaveArchive_O @ %X") % classcore__XmlSaveArchive_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::XmlSaveArchive_O::static_className() % core::XmlSaveArchive_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ArchiveError_O @ %X") % classcore__ArchiveError_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ArchiveError_O::static_className() % core::ArchiveError_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::BitVector_O @ %X") % classcore__BitVector_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::BitVector_O::static_className() % core::BitVector_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::BlockEnvironment_O @ %X") % classcore__BlockEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::BlockEnvironment_O::static_className() % core::BlockEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::CatchEnvironment_O @ %X") % classcore__CatchEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::CatchEnvironment_O::static_className() % core::CatchEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Cons_O @ %X") % classcore__Cons_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Cons_O::static_className() % core::Cons_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ContentException_O @ %X") % classcore__ContentException_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ContentException_O::static_className() % core::ContentException_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::DoubleFloat_O @ %X") % classcore__DoubleFloat_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::DoubleFloat_O::static_className() % core::DoubleFloat_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FileNotFoundException_O @ %X") % classcore__FileNotFoundException_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FileNotFoundException_O::static_className() % core::FileNotFoundException_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FunctionContainerEnvironment_O @ %X") % classcore__FunctionContainerEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FunctionContainerEnvironment_O::static_className() % core::FunctionContainerEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::FunctionValueEnvironment_O @ %X") % classcore__FunctionValueEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::FunctionValueEnvironment_O::static_className() % core::FunctionValueEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Integer_O @ %X") % classcore__Integer_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Integer_O::static_className() % core::Integer_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::LispError_O @ %X") % classcore__LispError_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::LispError_O::static_className() % core::LispError_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::LongFloat_O @ %X") % classcore__LongFloat_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::LongFloat_O::static_className() % core::LongFloat_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::MacroletEnvironment_O @ %X") % classcore__MacroletEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::MacroletEnvironment_O::static_className() % core::MacroletEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::MathException_O @ %X") % classcore__MathException_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::MathException_O::static_className() % core::MathException_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Null_O @ %X") % classcore__Null_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Null_O::static_className() % core::Null_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Ratio_O @ %X") % classcore__Ratio_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Ratio_O::static_className() % core::Ratio_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SerializeError_O @ %X") % classcore__SerializeError_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SerializeError_O::static_className() % core::SerializeError_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ShortFloat_O @ %X") % classcore__ShortFloat_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ShortFloat_O::static_className() % core::ShortFloat_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SingleFloat_O @ %X") % classcore__SingleFloat_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SingleFloat_O::static_className() % core::SingleFloat_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::String_O @ %X") % classcore__String_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::String_O::static_className() % core::String_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SymbolMacroletEnvironment_O @ %X") % classcore__SymbolMacroletEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SymbolMacroletEnvironment_O::static_className() % core::SymbolMacroletEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::TagbodyEnvironment_O @ %X") % classcore__TagbodyEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::TagbodyEnvironment_O::static_className() % core::TagbodyEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::TypeException_O @ %X") % classcore__TypeException_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::TypeException_O::static_className() % core::TypeException_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::UnwindProtectEnvironment_O @ %X") % classcore__UnwindProtectEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::UnwindProtectEnvironment_O::static_className() % core::UnwindProtectEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::ValueEnvironment_O @ %X") % classcore__ValueEnvironment_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::ValueEnvironment_O::static_className() % core::ValueEnvironment_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::VectorObjects_O @ %X") % classcore__VectorObjects_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::VectorObjects_O::static_className() % core::VectorObjects_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::serious_condition_O @ %X") % classcore__serious_condition_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::serious_condition_O::static_className() % core::serious_condition_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::simple_condition_O @ %X") % classcore__simple_condition_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::simple_condition_O::static_className() % core::simple_condition_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::warning_O @ %X") % classcore__warning_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::warning_O::static_className() % core::warning_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Bignum_O @ %X") % classcore__Bignum_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Bignum_O::static_className() % core::Bignum_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::CompiledBody_O @ %X") % classcore__CompiledBody_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::CompiledBody_O::static_className() % core::CompiledBody_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Error_O @ %X") % classcore__Error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Error_O::static_className() % core::Error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Fixnum_O @ %X") % classcore__Fixnum_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Fixnum_O::static_className() % core::Fixnum_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SimpleBitVector_O @ %X") % classcore__SimpleBitVector_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SimpleBitVector_O::static_className() % core::SimpleBitVector_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::SourceCodeCons_O @ %X") % classcore__SourceCodeCons_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::SourceCodeCons_O::static_className() % core::SourceCodeCons_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::Str_O @ %X") % classcore__Str_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::Str_O::static_className() % core::Str_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::VectorObjectsWithFillPtr_O @ %X") % classcore__VectorObjectsWithFillPtr_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::VectorObjectsWithFillPtr_O::static_className() % core::VectorObjectsWithFillPtr_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::simple_warning_O @ %X") % classcore__simple_warning_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::simple_warning_O::static_className() % core::simple_warning_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::storage_condition_O @ %X") % classcore__storage_condition_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::storage_condition_O::static_className() % core::storage_condition_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::style_warning_O @ %X") % classcore__style_warning_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::style_warning_O::static_className() % core::style_warning_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::LoadTimeValues_O @ %X") % classcore__LoadTimeValues_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::LoadTimeValues_O::static_className() % core::LoadTimeValues_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::arithmetic_error_O @ %X") % classcore__arithmetic_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::arithmetic_error_O::static_className() % core::arithmetic_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::cell_error_O @ %X") % classcore__cell_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::cell_error_O::static_className() % core::cell_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::control_error_O @ %X") % classcore__control_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::control_error_O::static_className() % core::control_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::file_error_O @ %X") % classcore__file_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::file_error_O::static_className() % core::file_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::package_error_O @ %X") % classcore__package_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::package_error_O::static_className() % core::package_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::parse_error_O @ %X") % classcore__parse_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::parse_error_O::static_className() % core::parse_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::print_not_readable_O @ %X") % classcore__print_not_readable_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::print_not_readable_O::static_className() % core::print_not_readable_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::program_error_O @ %X") % classcore__program_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::program_error_O::static_className() % core::program_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::simple_error_O @ %X") % classcore__simple_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::simple_error_O::static_className() % core::simple_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::stream_error_O @ %X") % classcore__stream_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::stream_error_O::static_className() % core::stream_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::type_error_O @ %X") % classcore__type_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::type_error_O::static_className() % core::type_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::argument_mismatch_error_O @ %X") % classcore__argument_mismatch_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::argument_mismatch_error_O::static_className() % core::argument_mismatch_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::division_by_zero_O @ %X") % classcore__division_by_zero_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::division_by_zero_O::static_className() % core::division_by_zero_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::end_of_file_O @ %X") % classcore__end_of_file_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::end_of_file_O::static_className() % core::end_of_file_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::floating_point_inexact_O @ %X") % classcore__floating_point_inexact_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::floating_point_inexact_O::static_className() % core::floating_point_inexact_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::floating_point_invalid_operation_O @ %X") % classcore__floating_point_invalid_operation_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::floating_point_invalid_operation_O::static_className() % core::floating_point_invalid_operation_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::floating_point_overflow_O @ %X") % classcore__floating_point_overflow_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::floating_point_overflow_O::static_className() % core::floating_point_overflow_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::floating_point_underflow_O @ %X") % classcore__floating_point_underflow_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::floating_point_underflow_O::static_className() % core::floating_point_underflow_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::reader_error_O @ %X") % classcore__reader_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::reader_error_O::static_className() % core::reader_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::simple_type_error_O @ %X") % classcore__simple_type_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::simple_type_error_O::static_className() % core::simple_type_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::unbound_slot_O @ %X") % classcore__unbound_slot_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::unbound_slot_O::static_className() % core::unbound_slot_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::unbound_variable_O @ %X") % classcore__unbound_variable_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::unbound_variable_O::static_className() % core::unbound_variable_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::undefined_function_O @ %X") % classcore__undefined_function_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::undefined_function_O::static_className() % core::undefined_function_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::invalid_keyword_arguments_error_O @ %X") % classcore__invalid_keyword_arguments_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::invalid_keyword_arguments_error_O::static_className() % core::invalid_keyword_arguments_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::odd_number_of_keyword_arguments_error_O @ %X") % classcore__odd_number_of_keyword_arguments_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::odd_number_of_keyword_arguments_error_O::static_className() % core::odd_number_of_keyword_arguments_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::single_dispatch_arguments_error_O @ %X") % classcore__single_dispatch_arguments_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::single_dispatch_arguments_error_O::static_className() % core::single_dispatch_arguments_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::too_few_arguments_error_O @ %X") % classcore__too_few_arguments_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::too_few_arguments_error_O::static_className() % core::too_few_arguments_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::too_many_arguments_error_O @ %X") % classcore__too_many_arguments_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::too_many_arguments_error_O::static_className() % core::too_many_arguments_error_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: core::unrecognized_keyword_arguments_error_O @ %X") % classcore__unrecognized_keyword_arguments_error_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % core::unrecognized_keyword_arguments_error_O::static_className() % core::unrecognized_keyword_arguments_error_O::static_classSymbol() );
#endif // } DUMP_INFO_CLASS
#undef DUMP_INFO_CLASS
#if defined(DEFINE_BASE_CLASSES) || defined(ALL_STAGES) // {
// Depends on nothing
classcore__ArchiveMemoryManager_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Archive_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__ArrayVirtual_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Binder_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Character_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__ConditionBase_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__ConditionHandlerBase_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__DirectoryEntry_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Environment_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__ExternalObjectManager_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__ExternalObject_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__FileStatus_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__FormBinder_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Function_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__HashTable_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Instance_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__IntArray_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__IntegerKeyObjectDictionary_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Iterator_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__LambdaListHandler_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Lisp_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__MethodCombination_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__MethodSet_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Method_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__MicroHeap_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Model_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__MultiStringBuffer_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__MultipleValues_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Number_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__ObjRef_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__ObjectSet_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Package_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Path_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Pointer_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__PosixTimeDuration_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__PosixTime_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__QDomNode_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__ReadTable_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Reader_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__RestartCase_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__SequenceVirtual_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Serializer_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__SetfExpander_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__SingleDispatchMethod_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__SourceFileInfo_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__StandardObject_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Stream_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__StringList_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__StringSet_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__SymbolList_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__SymbolSet_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__SymbolToEnumConverter_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__SymbolVectorMap_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__SymbolVector_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__Symbol_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__WeakReference_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classcore__ActivationFrame_Oval->addInstanceBaseClass(core::Environment_O::static_classSymbol());
classcore__Array_Oval->addInstanceBaseClass(core::ArrayVirtual_O::static_classSymbol());
classcore__BaseChar_Oval->addInstanceBaseClass(core::Character_O::static_classSymbol());
classcore__Bool_Oval->addInstanceBaseClass(core::Number_O::static_classSymbol());
classcore__BroadcastStream_Oval->addInstanceBaseClass(core::Stream_O::static_classSymbol());
classcore__BuiltIn_Oval->addInstanceBaseClass(core::Function_O::static_classSymbol());
classcore__Closure_Oval->addInstanceBaseClass(core::Function_O::static_classSymbol());
classcore__Complex_Oval->addInstanceBaseClass(core::Number_O::static_classSymbol());
classcore__ConcatenatedStream_Oval->addInstanceBaseClass(core::Stream_O::static_classSymbol());
classcore__ConditionWithStackTrace_Oval->addInstanceBaseClass(core::ConditionBase_O::static_classSymbol());
classcore__DirectoryIterator_Oval->addInstanceBaseClass(core::Iterator_O::static_classSymbol());
classcore__EchoStream_Oval->addInstanceBaseClass(core::Stream_O::static_classSymbol());
classcore__ExtendedChar_Oval->addInstanceBaseClass(core::Character_O::static_classSymbol());
classcore__FDStream_Oval->addInstanceBaseClass(core::Stream_O::static_classSymbol());
classcore__FileStream_Oval->addInstanceBaseClass(core::Stream_O::static_classSymbol());
classcore__FunctionNameSetfExpander_Oval->addInstanceBaseClass(core::SetfExpander_O::static_classSymbol());
classcore__FunctionSetfExpander_Oval->addInstanceBaseClass(core::SetfExpander_O::static_classSymbol());
classcore__GenericFunction_Oval->addInstanceBaseClass(core::Function_O::static_classSymbol());
classcore__GlueEnvironment_Oval->addInstanceBaseClass(core::Environment_O::static_classSymbol());
classcore__HandlerBindHandler_Oval->addInstanceBaseClass(core::ConditionHandlerBase_O::static_classSymbol());
classcore__HandlerCaseHandler_Oval->addInstanceBaseClass(core::ConditionHandlerBase_O::static_classSymbol());
classcore__HashTableEq_Oval->addInstanceBaseClass(core::HashTable_O::static_classSymbol());
classcore__HashTableEql_Oval->addInstanceBaseClass(core::HashTable_O::static_classSymbol());
classcore__HashTableEqual_Oval->addInstanceBaseClass(core::HashTable_O::static_classSymbol());
classcore__HashTableEqualp_Oval->addInstanceBaseClass(core::HashTable_O::static_classSymbol());
classcore__Interpreted_Oval->addInstanceBaseClass(core::Function_O::static_classSymbol());
classcore__LexicalEnvironment_Oval->addInstanceBaseClass(core::Environment_O::static_classSymbol());
classcore__LoadArchive_Oval->addInstanceBaseClass(core::Archive_O::static_classSymbol());
classcore__MetaClass_Oval->addInstanceBaseClass(core::StandardObject_O::static_classSymbol());
classcore__MetaObject_Oval->addInstanceBaseClass(core::StandardObject_O::static_classSymbol());
classcore__ReadSerializer_Oval->addInstanceBaseClass(core::Serializer_O::static_classSymbol());
classcore__Real_Oval->addInstanceBaseClass(core::Number_O::static_classSymbol());
classcore__RecursiveDirectoryIterator_Oval->addInstanceBaseClass(core::Iterator_O::static_classSymbol());
classcore__SaveArchive_Oval->addInstanceBaseClass(core::Archive_O::static_classSymbol());
classcore__Sequence_Oval->addInstanceBaseClass(core::SequenceVirtual_O::static_classSymbol());
classcore__SingleDispatchEffectiveMethodFunction_Oval->addInstanceBaseClass(core::Function_O::static_classSymbol());
classcore__SlotValueSetfExpander_Oval->addInstanceBaseClass(core::SetfExpander_O::static_classSymbol());
classcore__SpecialForm_Oval->addInstanceBaseClass(core::Function_O::static_classSymbol());
classcore__StringStream_Oval->addInstanceBaseClass(core::Stream_O::static_classSymbol());
classcore__StructureObject_Oval->addInstanceBaseClass(core::Model_O::static_classSymbol());
classcore__SynonymStream_Oval->addInstanceBaseClass(core::Stream_O::static_classSymbol());
classcore__TwoWayStream_Oval->addInstanceBaseClass(core::Stream_O::static_classSymbol());
classcore__WriteSerializer_Oval->addInstanceBaseClass(core::Serializer_O::static_classSymbol());
classcore__ArrayObjects_Oval->addInstanceBaseClass(core::Array_O::static_classSymbol());
classcore__BuiltInClass_Oval->addInstanceBaseClass(core::MetaClass_O::static_classSymbol());
classcore__CandoException_Oval->addInstanceBaseClass(core::ConditionWithStackTrace_O::static_classSymbol());
classcore__CandoFormatSaveArchive_Oval->addInstanceBaseClass(core::SaveArchive_O::static_classSymbol());
classcore__CompileTimeEnvironment_Oval->addInstanceBaseClass(core::LexicalEnvironment_O::static_classSymbol());
classcore__Condition_Oval->addInstanceBaseClass(core::ConditionWithStackTrace_O::static_classSymbol());
classcore__FDInStream_Oval->addInstanceBaseClass(core::FDStream_O::static_classSymbol());
classcore__FDOutStream_Oval->addInstanceBaseClass(core::FDStream_O::static_classSymbol());
classcore__FileInCompressedStream_Oval->addInstanceBaseClass(core::FileStream_O::static_classSymbol());
classcore__FileInStream_Oval->addInstanceBaseClass(core::FileStream_O::static_classSymbol());
classcore__FileOutCompressedStream_Oval->addInstanceBaseClass(core::FileStream_O::static_classSymbol());
classcore__FileOutStream_Oval->addInstanceBaseClass(core::FileStream_O::static_classSymbol());
classcore__Float_Oval->addInstanceBaseClass(core::Real_O::static_classSymbol());
classcore__ForwardReferencedClass_Oval->addInstanceBaseClass(core::MetaClass_O::static_classSymbol());
classcore__FunctionFrame_Oval->addInstanceBaseClass(core::ActivationFrame_O::static_classSymbol());
classcore__List_Oval->addInstanceBaseClass(core::Sequence_O::static_classSymbol());
classcore__Rational_Oval->addInstanceBaseClass(core::Real_O::static_classSymbol());
classcore__RuntimeVisibleEnvironment_Oval->addInstanceBaseClass(core::LexicalEnvironment_O::static_classSymbol());
classcore__SingleDispatchGenericFunction_Oval->addInstanceBaseClass(core::BuiltIn_O::static_classSymbol());
classcore__StandardChar_Oval->addInstanceBaseClass(core::BaseChar_O::static_classSymbol());
classcore__StandardClass_Oval->addInstanceBaseClass(core::MetaClass_O::static_classSymbol());
classcore__StandardGenericFunction_Oval->addInstanceBaseClass(core::GenericFunction_O::static_classSymbol());
classcore__StringInStream_Oval->addInstanceBaseClass(core::StringStream_O::static_classSymbol());
classcore__StringOutStream_Oval->addInstanceBaseClass(core::StringStream_O::static_classSymbol());
classcore__StructureClass_Oval->addInstanceBaseClass(core::MetaClass_O::static_classSymbol());
classcore__ValueFrame_Oval->addInstanceBaseClass(core::ActivationFrame_O::static_classSymbol());
classcore__Vector_Oval->addInstanceBaseClass(core::Array_O::static_classSymbol());
classcore__Vector_Oval->addInstanceBaseClass(core::Sequence_O::static_classSymbol());
classcore__XmlLoadArchive_Oval->addInstanceBaseClass(core::LoadArchive_O::static_classSymbol());
classcore__XmlSaveArchive_Oval->addInstanceBaseClass(core::SaveArchive_O::static_classSymbol());
classcore__ArchiveError_Oval->addInstanceBaseClass(core::CandoException_O::static_classSymbol());
classcore__BitVector_Oval->addInstanceBaseClass(core::Vector_O::static_classSymbol());
classcore__BlockEnvironment_Oval->addInstanceBaseClass(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__CatchEnvironment_Oval->addInstanceBaseClass(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__Cons_Oval->addInstanceBaseClass(core::List_O::static_classSymbol());
classcore__ContentException_Oval->addInstanceBaseClass(core::CandoException_O::static_classSymbol());
classcore__DoubleFloat_Oval->addInstanceBaseClass(core::Float_O::static_classSymbol());
classcore__FileNotFoundException_Oval->addInstanceBaseClass(core::CandoException_O::static_classSymbol());
classcore__FunctionContainerEnvironment_Oval->addInstanceBaseClass(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__FunctionValueEnvironment_Oval->addInstanceBaseClass(core::RuntimeVisibleEnvironment_O::static_classSymbol());
classcore__Integer_Oval->addInstanceBaseClass(core::Rational_O::static_classSymbol());
classcore__LispError_Oval->addInstanceBaseClass(core::CandoException_O::static_classSymbol());
classcore__LongFloat_Oval->addInstanceBaseClass(core::Float_O::static_classSymbol());
classcore__MacroletEnvironment_Oval->addInstanceBaseClass(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__MathException_Oval->addInstanceBaseClass(core::CandoException_O::static_classSymbol());
classcore__Null_Oval->addInstanceBaseClass(core::Symbol_O::static_classSymbol());
classcore__Null_Oval->addInstanceBaseClass(core::List_O::static_classSymbol());
classcore__Ratio_Oval->addInstanceBaseClass(core::Rational_O::static_classSymbol());
classcore__SerializeError_Oval->addInstanceBaseClass(core::CandoException_O::static_classSymbol());
classcore__ShortFloat_Oval->addInstanceBaseClass(core::Float_O::static_classSymbol());
classcore__SingleFloat_Oval->addInstanceBaseClass(core::Float_O::static_classSymbol());
classcore__String_Oval->addInstanceBaseClass(core::Vector_O::static_classSymbol());
classcore__SymbolMacroletEnvironment_Oval->addInstanceBaseClass(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__TagbodyEnvironment_Oval->addInstanceBaseClass(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__TypeException_Oval->addInstanceBaseClass(core::CandoException_O::static_classSymbol());
classcore__UnwindProtectEnvironment_Oval->addInstanceBaseClass(core::CompileTimeEnvironment_O::static_classSymbol());
classcore__ValueEnvironment_Oval->addInstanceBaseClass(core::RuntimeVisibleEnvironment_O::static_classSymbol());
classcore__VectorObjects_Oval->addInstanceBaseClass(core::Vector_O::static_classSymbol());
classcore__serious_condition_Oval->addInstanceBaseClass(core::Condition_O::static_classSymbol());
classcore__simple_condition_Oval->addInstanceBaseClass(core::Condition_O::static_classSymbol());
classcore__warning_Oval->addInstanceBaseClass(core::Condition_O::static_classSymbol());
classcore__Bignum_Oval->addInstanceBaseClass(core::Integer_O::static_classSymbol());
classcore__CompiledBody_Oval->addInstanceBaseClass(core::Cons_O::static_classSymbol());
classcore__Error_Oval->addInstanceBaseClass(core::serious_condition_O::static_classSymbol());
classcore__Fixnum_Oval->addInstanceBaseClass(core::Integer_O::static_classSymbol());
classcore__SimpleBitVector_Oval->addInstanceBaseClass(core::BitVector_O::static_classSymbol());
classcore__SourceCodeCons_Oval->addInstanceBaseClass(core::Cons_O::static_classSymbol());
classcore__Str_Oval->addInstanceBaseClass(core::String_O::static_classSymbol());
classcore__VectorObjectsWithFillPtr_Oval->addInstanceBaseClass(core::VectorObjects_O::static_classSymbol());
classcore__simple_warning_Oval->addInstanceBaseClass(core::warning_O::static_classSymbol());
classcore__simple_warning_Oval->addInstanceBaseClass(core::simple_condition_O::static_classSymbol());
classcore__storage_condition_Oval->addInstanceBaseClass(core::serious_condition_O::static_classSymbol());
classcore__style_warning_Oval->addInstanceBaseClass(core::warning_O::static_classSymbol());
classcore__LoadTimeValues_Oval->addInstanceBaseClass(core::VectorObjectsWithFillPtr_O::static_classSymbol());
classcore__arithmetic_error_Oval->addInstanceBaseClass(core::Error_O::static_classSymbol());
classcore__cell_error_Oval->addInstanceBaseClass(core::Error_O::static_classSymbol());
classcore__control_error_Oval->addInstanceBaseClass(core::Error_O::static_classSymbol());
classcore__file_error_Oval->addInstanceBaseClass(core::Error_O::static_classSymbol());
classcore__package_error_Oval->addInstanceBaseClass(core::Error_O::static_classSymbol());
classcore__parse_error_Oval->addInstanceBaseClass(core::Error_O::static_classSymbol());
classcore__print_not_readable_Oval->addInstanceBaseClass(core::Error_O::static_classSymbol());
classcore__program_error_Oval->addInstanceBaseClass(core::Error_O::static_classSymbol());
classcore__simple_error_Oval->addInstanceBaseClass(core::Error_O::static_classSymbol());
classcore__simple_error_Oval->addInstanceBaseClass(core::simple_condition_O::static_classSymbol());
classcore__stream_error_Oval->addInstanceBaseClass(core::Error_O::static_classSymbol());
classcore__type_error_Oval->addInstanceBaseClass(core::Error_O::static_classSymbol());
classcore__argument_mismatch_error_Oval->addInstanceBaseClass(core::program_error_O::static_classSymbol());
classcore__division_by_zero_Oval->addInstanceBaseClass(core::arithmetic_error_O::static_classSymbol());
classcore__end_of_file_Oval->addInstanceBaseClass(core::stream_error_O::static_classSymbol());
classcore__floating_point_inexact_Oval->addInstanceBaseClass(core::arithmetic_error_O::static_classSymbol());
classcore__floating_point_invalid_operation_Oval->addInstanceBaseClass(core::arithmetic_error_O::static_classSymbol());
classcore__floating_point_overflow_Oval->addInstanceBaseClass(core::arithmetic_error_O::static_classSymbol());
classcore__floating_point_underflow_Oval->addInstanceBaseClass(core::arithmetic_error_O::static_classSymbol());
classcore__reader_error_Oval->addInstanceBaseClass(core::stream_error_O::static_classSymbol());
classcore__reader_error_Oval->addInstanceBaseClass(core::parse_error_O::static_classSymbol());
classcore__simple_type_error_Oval->addInstanceBaseClass(core::type_error_O::static_classSymbol());
classcore__unbound_slot_Oval->addInstanceBaseClass(core::cell_error_O::static_classSymbol());
classcore__unbound_variable_Oval->addInstanceBaseClass(core::cell_error_O::static_classSymbol());
classcore__undefined_function_Oval->addInstanceBaseClass(core::cell_error_O::static_classSymbol());
classcore__invalid_keyword_arguments_error_Oval->addInstanceBaseClass(core::argument_mismatch_error_O::static_classSymbol());
classcore__odd_number_of_keyword_arguments_error_Oval->addInstanceBaseClass(core::argument_mismatch_error_O::static_classSymbol());
classcore__single_dispatch_arguments_error_Oval->addInstanceBaseClass(core::argument_mismatch_error_O::static_classSymbol());
classcore__too_few_arguments_error_Oval->addInstanceBaseClass(core::argument_mismatch_error_O::static_classSymbol());
classcore__too_many_arguments_error_Oval->addInstanceBaseClass(core::argument_mismatch_error_O::static_classSymbol());
classcore__unrecognized_keyword_arguments_error_Oval->addInstanceBaseClass(core::argument_mismatch_error_O::static_classSymbol());
#endif // } DEFINE_BASE_CLASSES
#undef DEFINE_BASE_CLASSES
#if defined(DEFINE_CLASS_NAMES) || defined(ALL_STAGES) // {
 core::Package_sp _curPkg = _lisp->findPackage(CurrentPkg);
// Depends on nothing

    classcore__T_Oval->__setup_stage3_name(core::T_O::static_classSymbol());

    classcore__ArchiveMemoryManager_Oval->__setup_stage3_name(core::ArchiveMemoryManager_O::static_classSymbol());

    classcore__Archive_Oval->__setup_stage3_name(core::Archive_O::static_classSymbol());

    classcore__ArrayVirtual_Oval->__setup_stage3_name(core::ArrayVirtual_O::static_classSymbol());

    classcore__Binder_Oval->__setup_stage3_name(core::Binder_O::static_classSymbol());

    classcore__Character_Oval->__setup_stage3_name(core::Character_O::static_classSymbol());

    classcore__ConditionBase_Oval->__setup_stage3_name(core::ConditionBase_O::static_classSymbol());

    classcore__ConditionHandlerBase_Oval->__setup_stage3_name(core::ConditionHandlerBase_O::static_classSymbol());

    classcore__DirectoryEntry_Oval->__setup_stage3_name(core::DirectoryEntry_O::static_classSymbol());

    classcore__Environment_Oval->__setup_stage3_name(core::Environment_O::static_classSymbol());

    classcore__ExternalObjectManager_Oval->__setup_stage3_name(core::ExternalObjectManager_O::static_classSymbol());

    classcore__ExternalObject_Oval->__setup_stage3_name(core::ExternalObject_O::static_classSymbol());

    classcore__FileStatus_Oval->__setup_stage3_name(core::FileStatus_O::static_classSymbol());

    classcore__FormBinder_Oval->__setup_stage3_name(core::FormBinder_O::static_classSymbol());

    classcore__Function_Oval->__setup_stage3_name(core::Function_O::static_classSymbol());

    classcore__HashTable_Oval->__setup_stage3_name(core::HashTable_O::static_classSymbol());

    classcore__Instance_Oval->__setup_stage3_name(core::Instance_O::static_classSymbol());

    classcore__IntArray_Oval->__setup_stage3_name(core::IntArray_O::static_classSymbol());

    classcore__IntegerKeyObjectDictionary_Oval->__setup_stage3_name(core::IntegerKeyObjectDictionary_O::static_classSymbol());

    classcore__Iterator_Oval->__setup_stage3_name(core::Iterator_O::static_classSymbol());

    classcore__LambdaListHandler_Oval->__setup_stage3_name(core::LambdaListHandler_O::static_classSymbol());

    classcore__Lisp_Oval->__setup_stage3_name(core::Lisp_O::static_classSymbol());

    classcore__MethodCombination_Oval->__setup_stage3_name(core::MethodCombination_O::static_classSymbol());

    classcore__MethodSet_Oval->__setup_stage3_name(core::MethodSet_O::static_classSymbol());

    classcore__Method_Oval->__setup_stage3_name(core::Method_O::static_classSymbol());

    classcore__MicroHeap_Oval->__setup_stage3_name(core::MicroHeap_O::static_classSymbol());

    classcore__Model_Oval->__setup_stage3_name(core::Model_O::static_classSymbol());

    classcore__MultiStringBuffer_Oval->__setup_stage3_name(core::MultiStringBuffer_O::static_classSymbol());

    classcore__MultipleValues_Oval->__setup_stage3_name(core::MultipleValues_O::static_classSymbol());

    classcore__Number_Oval->__setup_stage3_name(core::Number_O::static_classSymbol());

    classcore__ObjRef_Oval->__setup_stage3_name(core::ObjRef_O::static_classSymbol());

    classcore__ObjectSet_Oval->__setup_stage3_name(core::ObjectSet_O::static_classSymbol());

    classcore__Package_Oval->__setup_stage3_name(core::Package_O::static_classSymbol());

    classcore__Path_Oval->__setup_stage3_name(core::Path_O::static_classSymbol());

    classcore__Pointer_Oval->__setup_stage3_name(core::Pointer_O::static_classSymbol());

    classcore__PosixTimeDuration_Oval->__setup_stage3_name(core::PosixTimeDuration_O::static_classSymbol());

    classcore__PosixTime_Oval->__setup_stage3_name(core::PosixTime_O::static_classSymbol());

    classcore__QDomNode_Oval->__setup_stage3_name(core::QDomNode_O::static_classSymbol());

    classcore__ReadTable_Oval->__setup_stage3_name(core::ReadTable_O::static_classSymbol());

    classcore__Reader_Oval->__setup_stage3_name(core::Reader_O::static_classSymbol());

    classcore__RestartCase_Oval->__setup_stage3_name(core::RestartCase_O::static_classSymbol());

    classcore__SequenceVirtual_Oval->__setup_stage3_name(core::SequenceVirtual_O::static_classSymbol());

    classcore__Serializer_Oval->__setup_stage3_name(core::Serializer_O::static_classSymbol());

    classcore__SetfExpander_Oval->__setup_stage3_name(core::SetfExpander_O::static_classSymbol());

    classcore__SingleDispatchMethod_Oval->__setup_stage3_name(core::SingleDispatchMethod_O::static_classSymbol());

    classcore__SourceFileInfo_Oval->__setup_stage3_name(core::SourceFileInfo_O::static_classSymbol());

    classcore__StandardObject_Oval->__setup_stage3_name(core::StandardObject_O::static_classSymbol());

    classcore__Stream_Oval->__setup_stage3_name(core::Stream_O::static_classSymbol());

    classcore__StringList_Oval->__setup_stage3_name(core::StringList_O::static_classSymbol());

    classcore__StringSet_Oval->__setup_stage3_name(core::StringSet_O::static_classSymbol());

    classcore__SymbolList_Oval->__setup_stage3_name(core::SymbolList_O::static_classSymbol());

    classcore__SymbolSet_Oval->__setup_stage3_name(core::SymbolSet_O::static_classSymbol());

    classcore__SymbolToEnumConverter_Oval->__setup_stage3_name(core::SymbolToEnumConverter_O::static_classSymbol());

    classcore__SymbolVectorMap_Oval->__setup_stage3_name(core::SymbolVectorMap_O::static_classSymbol());

    classcore__SymbolVector_Oval->__setup_stage3_name(core::SymbolVector_O::static_classSymbol());

    classcore__Symbol_Oval->__setup_stage3_name(core::Symbol_O::static_classSymbol());

    classcore__WeakReference_Oval->__setup_stage3_name(core::WeakReference_O::static_classSymbol());

    classcore__ActivationFrame_Oval->__setup_stage3_name(core::ActivationFrame_O::static_classSymbol());

    classcore__Array_Oval->__setup_stage3_name(core::Array_O::static_classSymbol());

    classcore__BaseChar_Oval->__setup_stage3_name(core::BaseChar_O::static_classSymbol());

    classcore__Bool_Oval->__setup_stage3_name(core::Bool_O::static_classSymbol());

    classcore__BroadcastStream_Oval->__setup_stage3_name(core::BroadcastStream_O::static_classSymbol());

    classcore__BuiltIn_Oval->__setup_stage3_name(core::BuiltIn_O::static_classSymbol());

    classcore__Closure_Oval->__setup_stage3_name(core::Closure_O::static_classSymbol());

    classcore__Complex_Oval->__setup_stage3_name(core::Complex_O::static_classSymbol());

    classcore__ConcatenatedStream_Oval->__setup_stage3_name(core::ConcatenatedStream_O::static_classSymbol());

    classcore__ConditionWithStackTrace_Oval->__setup_stage3_name(core::ConditionWithStackTrace_O::static_classSymbol());

    classcore__DirectoryIterator_Oval->__setup_stage3_name(core::DirectoryIterator_O::static_classSymbol());

    classcore__EchoStream_Oval->__setup_stage3_name(core::EchoStream_O::static_classSymbol());

    classcore__ExtendedChar_Oval->__setup_stage3_name(core::ExtendedChar_O::static_classSymbol());

    classcore__FDStream_Oval->__setup_stage3_name(core::FDStream_O::static_classSymbol());

    classcore__FileStream_Oval->__setup_stage3_name(core::FileStream_O::static_classSymbol());

    classcore__FunctionNameSetfExpander_Oval->__setup_stage3_name(core::FunctionNameSetfExpander_O::static_classSymbol());

    classcore__FunctionSetfExpander_Oval->__setup_stage3_name(core::FunctionSetfExpander_O::static_classSymbol());

    classcore__GenericFunction_Oval->__setup_stage3_name(core::GenericFunction_O::static_classSymbol());

    classcore__GlueEnvironment_Oval->__setup_stage3_name(core::GlueEnvironment_O::static_classSymbol());

    classcore__HandlerBindHandler_Oval->__setup_stage3_name(core::HandlerBindHandler_O::static_classSymbol());

    classcore__HandlerCaseHandler_Oval->__setup_stage3_name(core::HandlerCaseHandler_O::static_classSymbol());

    classcore__HashTableEq_Oval->__setup_stage3_name(core::HashTableEq_O::static_classSymbol());

    classcore__HashTableEql_Oval->__setup_stage3_name(core::HashTableEql_O::static_classSymbol());

    classcore__HashTableEqual_Oval->__setup_stage3_name(core::HashTableEqual_O::static_classSymbol());

    classcore__HashTableEqualp_Oval->__setup_stage3_name(core::HashTableEqualp_O::static_classSymbol());

    classcore__Interpreted_Oval->__setup_stage3_name(core::Interpreted_O::static_classSymbol());

    classcore__LexicalEnvironment_Oval->__setup_stage3_name(core::LexicalEnvironment_O::static_classSymbol());

    classcore__LoadArchive_Oval->__setup_stage3_name(core::LoadArchive_O::static_classSymbol());

    classcore__MetaClass_Oval->__setup_stage3_name(core::MetaClass_O::static_classSymbol());

    classcore__MetaObject_Oval->__setup_stage3_name(core::MetaObject_O::static_classSymbol());

    classcore__ReadSerializer_Oval->__setup_stage3_name(core::ReadSerializer_O::static_classSymbol());

    classcore__Real_Oval->__setup_stage3_name(core::Real_O::static_classSymbol());

    classcore__RecursiveDirectoryIterator_Oval->__setup_stage3_name(core::RecursiveDirectoryIterator_O::static_classSymbol());

    classcore__SaveArchive_Oval->__setup_stage3_name(core::SaveArchive_O::static_classSymbol());

    classcore__Sequence_Oval->__setup_stage3_name(core::Sequence_O::static_classSymbol());

    classcore__SingleDispatchEffectiveMethodFunction_Oval->__setup_stage3_name(core::SingleDispatchEffectiveMethodFunction_O::static_classSymbol());

    classcore__SlotValueSetfExpander_Oval->__setup_stage3_name(core::SlotValueSetfExpander_O::static_classSymbol());

    classcore__SpecialForm_Oval->__setup_stage3_name(core::SpecialForm_O::static_classSymbol());

    classcore__StringStream_Oval->__setup_stage3_name(core::StringStream_O::static_classSymbol());

    classcore__StructureObject_Oval->__setup_stage3_name(core::StructureObject_O::static_classSymbol());

    classcore__SynonymStream_Oval->__setup_stage3_name(core::SynonymStream_O::static_classSymbol());

    classcore__TwoWayStream_Oval->__setup_stage3_name(core::TwoWayStream_O::static_classSymbol());

    classcore__WriteSerializer_Oval->__setup_stage3_name(core::WriteSerializer_O::static_classSymbol());

    classcore__ArrayObjects_Oval->__setup_stage3_name(core::ArrayObjects_O::static_classSymbol());

    classcore__BuiltInClass_Oval->__setup_stage3_name(core::BuiltInClass_O::static_classSymbol());

    classcore__CandoException_Oval->__setup_stage3_name(core::CandoException_O::static_classSymbol());

    classcore__CandoFormatSaveArchive_Oval->__setup_stage3_name(core::CandoFormatSaveArchive_O::static_classSymbol());

    classcore__CompileTimeEnvironment_Oval->__setup_stage3_name(core::CompileTimeEnvironment_O::static_classSymbol());

    classcore__Condition_Oval->__setup_stage3_name(core::Condition_O::static_classSymbol());

    classcore__FDInStream_Oval->__setup_stage3_name(core::FDInStream_O::static_classSymbol());

    classcore__FDOutStream_Oval->__setup_stage3_name(core::FDOutStream_O::static_classSymbol());

    classcore__FileInCompressedStream_Oval->__setup_stage3_name(core::FileInCompressedStream_O::static_classSymbol());

    classcore__FileInStream_Oval->__setup_stage3_name(core::FileInStream_O::static_classSymbol());

    classcore__FileOutCompressedStream_Oval->__setup_stage3_name(core::FileOutCompressedStream_O::static_classSymbol());

    classcore__FileOutStream_Oval->__setup_stage3_name(core::FileOutStream_O::static_classSymbol());

    classcore__Float_Oval->__setup_stage3_name(core::Float_O::static_classSymbol());

    classcore__ForwardReferencedClass_Oval->__setup_stage3_name(core::ForwardReferencedClass_O::static_classSymbol());

    classcore__FunctionFrame_Oval->__setup_stage3_name(core::FunctionFrame_O::static_classSymbol());

    classcore__List_Oval->__setup_stage3_name(core::List_O::static_classSymbol());

    classcore__Rational_Oval->__setup_stage3_name(core::Rational_O::static_classSymbol());

    classcore__RuntimeVisibleEnvironment_Oval->__setup_stage3_name(core::RuntimeVisibleEnvironment_O::static_classSymbol());

    classcore__SingleDispatchGenericFunction_Oval->__setup_stage3_name(core::SingleDispatchGenericFunction_O::static_classSymbol());

    classcore__StandardChar_Oval->__setup_stage3_name(core::StandardChar_O::static_classSymbol());

    classcore__StandardClass_Oval->__setup_stage3_name(core::StandardClass_O::static_classSymbol());

    classcore__StandardGenericFunction_Oval->__setup_stage3_name(core::StandardGenericFunction_O::static_classSymbol());

    classcore__StringInStream_Oval->__setup_stage3_name(core::StringInStream_O::static_classSymbol());

    classcore__StringOutStream_Oval->__setup_stage3_name(core::StringOutStream_O::static_classSymbol());

    classcore__StructureClass_Oval->__setup_stage3_name(core::StructureClass_O::static_classSymbol());

    classcore__ValueFrame_Oval->__setup_stage3_name(core::ValueFrame_O::static_classSymbol());

    classcore__Vector_Oval->__setup_stage3_name(core::Vector_O::static_classSymbol());

    classcore__XmlLoadArchive_Oval->__setup_stage3_name(core::XmlLoadArchive_O::static_classSymbol());

    classcore__XmlSaveArchive_Oval->__setup_stage3_name(core::XmlSaveArchive_O::static_classSymbol());

    classcore__ArchiveError_Oval->__setup_stage3_name(core::ArchiveError_O::static_classSymbol());

    classcore__BitVector_Oval->__setup_stage3_name(core::BitVector_O::static_classSymbol());

    classcore__BlockEnvironment_Oval->__setup_stage3_name(core::BlockEnvironment_O::static_classSymbol());

    classcore__CatchEnvironment_Oval->__setup_stage3_name(core::CatchEnvironment_O::static_classSymbol());

    classcore__Cons_Oval->__setup_stage3_name(core::Cons_O::static_classSymbol());

    classcore__ContentException_Oval->__setup_stage3_name(core::ContentException_O::static_classSymbol());

    classcore__DoubleFloat_Oval->__setup_stage3_name(core::DoubleFloat_O::static_classSymbol());

    classcore__FileNotFoundException_Oval->__setup_stage3_name(core::FileNotFoundException_O::static_classSymbol());

    classcore__FunctionContainerEnvironment_Oval->__setup_stage3_name(core::FunctionContainerEnvironment_O::static_classSymbol());

    classcore__FunctionValueEnvironment_Oval->__setup_stage3_name(core::FunctionValueEnvironment_O::static_classSymbol());

    classcore__Integer_Oval->__setup_stage3_name(core::Integer_O::static_classSymbol());

    classcore__LispError_Oval->__setup_stage3_name(core::LispError_O::static_classSymbol());

    classcore__LongFloat_Oval->__setup_stage3_name(core::LongFloat_O::static_classSymbol());

    classcore__MacroletEnvironment_Oval->__setup_stage3_name(core::MacroletEnvironment_O::static_classSymbol());

    classcore__MathException_Oval->__setup_stage3_name(core::MathException_O::static_classSymbol());

    classcore__Null_Oval->__setup_stage3_name(core::Null_O::static_classSymbol());

    classcore__Ratio_Oval->__setup_stage3_name(core::Ratio_O::static_classSymbol());

    classcore__SerializeError_Oval->__setup_stage3_name(core::SerializeError_O::static_classSymbol());

    classcore__ShortFloat_Oval->__setup_stage3_name(core::ShortFloat_O::static_classSymbol());

    classcore__SingleFloat_Oval->__setup_stage3_name(core::SingleFloat_O::static_classSymbol());

    classcore__String_Oval->__setup_stage3_name(core::String_O::static_classSymbol());

    classcore__SymbolMacroletEnvironment_Oval->__setup_stage3_name(core::SymbolMacroletEnvironment_O::static_classSymbol());

    classcore__TagbodyEnvironment_Oval->__setup_stage3_name(core::TagbodyEnvironment_O::static_classSymbol());

    classcore__TypeException_Oval->__setup_stage3_name(core::TypeException_O::static_classSymbol());

    classcore__UnwindProtectEnvironment_Oval->__setup_stage3_name(core::UnwindProtectEnvironment_O::static_classSymbol());

    classcore__ValueEnvironment_Oval->__setup_stage3_name(core::ValueEnvironment_O::static_classSymbol());

    classcore__VectorObjects_Oval->__setup_stage3_name(core::VectorObjects_O::static_classSymbol());

    classcore__serious_condition_Oval->__setup_stage3_name(core::serious_condition_O::static_classSymbol());

    classcore__simple_condition_Oval->__setup_stage3_name(core::simple_condition_O::static_classSymbol());

    classcore__warning_Oval->__setup_stage3_name(core::warning_O::static_classSymbol());

    classcore__Bignum_Oval->__setup_stage3_name(core::Bignum_O::static_classSymbol());

    classcore__CompiledBody_Oval->__setup_stage3_name(core::CompiledBody_O::static_classSymbol());

    classcore__Error_Oval->__setup_stage3_name(core::Error_O::static_classSymbol());

    classcore__Fixnum_Oval->__setup_stage3_name(core::Fixnum_O::static_classSymbol());

    classcore__SimpleBitVector_Oval->__setup_stage3_name(core::SimpleBitVector_O::static_classSymbol());

    classcore__SourceCodeCons_Oval->__setup_stage3_name(core::SourceCodeCons_O::static_classSymbol());

    classcore__Str_Oval->__setup_stage3_name(core::Str_O::static_classSymbol());

    classcore__VectorObjectsWithFillPtr_Oval->__setup_stage3_name(core::VectorObjectsWithFillPtr_O::static_classSymbol());

    classcore__simple_warning_Oval->__setup_stage3_name(core::simple_warning_O::static_classSymbol());

    classcore__storage_condition_Oval->__setup_stage3_name(core::storage_condition_O::static_classSymbol());

    classcore__style_warning_Oval->__setup_stage3_name(core::style_warning_O::static_classSymbol());

    classcore__LoadTimeValues_Oval->__setup_stage3_name(core::LoadTimeValues_O::static_classSymbol());

    classcore__arithmetic_error_Oval->__setup_stage3_name(core::arithmetic_error_O::static_classSymbol());

    classcore__cell_error_Oval->__setup_stage3_name(core::cell_error_O::static_classSymbol());

    classcore__control_error_Oval->__setup_stage3_name(core::control_error_O::static_classSymbol());

    classcore__file_error_Oval->__setup_stage3_name(core::file_error_O::static_classSymbol());

    classcore__package_error_Oval->__setup_stage3_name(core::package_error_O::static_classSymbol());

    classcore__parse_error_Oval->__setup_stage3_name(core::parse_error_O::static_classSymbol());

    classcore__print_not_readable_Oval->__setup_stage3_name(core::print_not_readable_O::static_classSymbol());

    classcore__program_error_Oval->__setup_stage3_name(core::program_error_O::static_classSymbol());

    classcore__simple_error_Oval->__setup_stage3_name(core::simple_error_O::static_classSymbol());

    classcore__stream_error_Oval->__setup_stage3_name(core::stream_error_O::static_classSymbol());

    classcore__type_error_Oval->__setup_stage3_name(core::type_error_O::static_classSymbol());

    classcore__argument_mismatch_error_Oval->__setup_stage3_name(core::argument_mismatch_error_O::static_classSymbol());

    classcore__division_by_zero_Oval->__setup_stage3_name(core::division_by_zero_O::static_classSymbol());

    classcore__end_of_file_Oval->__setup_stage3_name(core::end_of_file_O::static_classSymbol());

    classcore__floating_point_inexact_Oval->__setup_stage3_name(core::floating_point_inexact_O::static_classSymbol());

    classcore__floating_point_invalid_operation_Oval->__setup_stage3_name(core::floating_point_invalid_operation_O::static_classSymbol());

    classcore__floating_point_overflow_Oval->__setup_stage3_name(core::floating_point_overflow_O::static_classSymbol());

    classcore__floating_point_underflow_Oval->__setup_stage3_name(core::floating_point_underflow_O::static_classSymbol());

    classcore__reader_error_Oval->__setup_stage3_name(core::reader_error_O::static_classSymbol());

    classcore__simple_type_error_Oval->__setup_stage3_name(core::simple_type_error_O::static_classSymbol());

    classcore__unbound_slot_Oval->__setup_stage3_name(core::unbound_slot_O::static_classSymbol());

    classcore__unbound_variable_Oval->__setup_stage3_name(core::unbound_variable_O::static_classSymbol());

    classcore__undefined_function_Oval->__setup_stage3_name(core::undefined_function_O::static_classSymbol());

    classcore__invalid_keyword_arguments_error_Oval->__setup_stage3_name(core::invalid_keyword_arguments_error_O::static_classSymbol());

    classcore__odd_number_of_keyword_arguments_error_Oval->__setup_stage3_name(core::odd_number_of_keyword_arguments_error_O::static_classSymbol());

    classcore__single_dispatch_arguments_error_Oval->__setup_stage3_name(core::single_dispatch_arguments_error_O::static_classSymbol());

    classcore__too_few_arguments_error_Oval->__setup_stage3_name(core::too_few_arguments_error_O::static_classSymbol());

    classcore__too_many_arguments_error_Oval->__setup_stage3_name(core::too_many_arguments_error_O::static_classSymbol());

    classcore__unrecognized_keyword_arguments_error_Oval->__setup_stage3_name(core::unrecognized_keyword_arguments_error_O::static_classSymbol());
#endif // } DEFINE_CLASS_NAMES
#undef DEFINE_CLASS_NAMES
#if defined(EXPOSE_TO_CANDO) || defined(ALL_STAGES)
#ifdef Use_CorePkg
extern void Register_core__T_O(core::Lisp_sp); // base(s): set(['core::_RootDummyClass'])
{_BLOCK_TRACE("initializing Register_core__T_O");
    Register_core__T_O(_lisp); // base(s): set(['core::_RootDummyClass'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ArchiveMemoryManager_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__ArchiveMemoryManager_O");
    Register_core__ArchiveMemoryManager_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Archive_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Archive_O");
    Register_core__Archive_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ArrayVirtual_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__ArrayVirtual_O");
    Register_core__ArrayVirtual_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Binder_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Binder_O");
    Register_core__Binder_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Character_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Character_O");
    Register_core__Character_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ConditionBase_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__ConditionBase_O");
    Register_core__ConditionBase_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ConditionHandlerBase_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__ConditionHandlerBase_O");
    Register_core__ConditionHandlerBase_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__DirectoryEntry_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__DirectoryEntry_O");
    Register_core__DirectoryEntry_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Environment_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Environment_O");
    Register_core__Environment_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ExternalObjectManager_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__ExternalObjectManager_O");
    Register_core__ExternalObjectManager_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ExternalObject_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__ExternalObject_O");
    Register_core__ExternalObject_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FileStatus_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__FileStatus_O");
    Register_core__FileStatus_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FormBinder_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__FormBinder_O");
    Register_core__FormBinder_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Function_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Function_O");
    Register_core__Function_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__HashTable_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__HashTable_O");
    Register_core__HashTable_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Instance_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Instance_O");
    Register_core__Instance_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__IntArray_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__IntArray_O");
    Register_core__IntArray_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__IntegerKeyObjectDictionary_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__IntegerKeyObjectDictionary_O");
    Register_core__IntegerKeyObjectDictionary_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Iterator_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Iterator_O");
    Register_core__Iterator_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__LambdaListHandler_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__LambdaListHandler_O");
    Register_core__LambdaListHandler_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Lisp_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Lisp_O");
    Register_core__Lisp_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__MethodCombination_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__MethodCombination_O");
    Register_core__MethodCombination_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__MethodSet_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__MethodSet_O");
    Register_core__MethodSet_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Method_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Method_O");
    Register_core__Method_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__MicroHeap_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__MicroHeap_O");
    Register_core__MicroHeap_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Model_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Model_O");
    Register_core__Model_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__MultiStringBuffer_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__MultiStringBuffer_O");
    Register_core__MultiStringBuffer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__MultipleValues_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__MultipleValues_O");
    Register_core__MultipleValues_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Number_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Number_O");
    Register_core__Number_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ObjRef_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__ObjRef_O");
    Register_core__ObjRef_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ObjectSet_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__ObjectSet_O");
    Register_core__ObjectSet_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Package_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Package_O");
    Register_core__Package_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Path_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Path_O");
    Register_core__Path_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Pointer_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Pointer_O");
    Register_core__Pointer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__PosixTimeDuration_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__PosixTimeDuration_O");
    Register_core__PosixTimeDuration_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__PosixTime_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__PosixTime_O");
    Register_core__PosixTime_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__QDomNode_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__QDomNode_O");
    Register_core__QDomNode_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ReadTable_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__ReadTable_O");
    Register_core__ReadTable_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Reader_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Reader_O");
    Register_core__Reader_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__RestartCase_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__RestartCase_O");
    Register_core__RestartCase_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SequenceVirtual_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__SequenceVirtual_O");
    Register_core__SequenceVirtual_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Serializer_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Serializer_O");
    Register_core__Serializer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SetfExpander_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__SetfExpander_O");
    Register_core__SetfExpander_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SingleDispatchMethod_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__SingleDispatchMethod_O");
    Register_core__SingleDispatchMethod_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SourceFileInfo_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__SourceFileInfo_O");
    Register_core__SourceFileInfo_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__StandardObject_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__StandardObject_O");
    Register_core__StandardObject_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Stream_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Stream_O");
    Register_core__Stream_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__StringList_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__StringList_O");
    Register_core__StringList_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__StringSet_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__StringSet_O");
    Register_core__StringSet_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SymbolList_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__SymbolList_O");
    Register_core__SymbolList_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SymbolSet_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__SymbolSet_O");
    Register_core__SymbolSet_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SymbolToEnumConverter_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__SymbolToEnumConverter_O");
    Register_core__SymbolToEnumConverter_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SymbolVectorMap_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__SymbolVectorMap_O");
    Register_core__SymbolVectorMap_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SymbolVector_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__SymbolVector_O");
    Register_core__SymbolVector_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Symbol_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__Symbol_O");
    Register_core__Symbol_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__WeakReference_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_core__WeakReference_O");
    Register_core__WeakReference_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ActivationFrame_O(core::Lisp_sp); // base(s): set(['core::Environment_O'])
{_BLOCK_TRACE("initializing Register_core__ActivationFrame_O");
    Register_core__ActivationFrame_O(_lisp); // base(s): set(['core::Environment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Array_O(core::Lisp_sp); // base(s): set(['core::ArrayVirtual_O'])
{_BLOCK_TRACE("initializing Register_core__Array_O");
    Register_core__Array_O(_lisp); // base(s): set(['core::ArrayVirtual_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__BaseChar_O(core::Lisp_sp); // base(s): set(['core::Character_O'])
{_BLOCK_TRACE("initializing Register_core__BaseChar_O");
    Register_core__BaseChar_O(_lisp); // base(s): set(['core::Character_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Bool_O(core::Lisp_sp); // base(s): set(['core::Number_O'])
{_BLOCK_TRACE("initializing Register_core__Bool_O");
    Register_core__Bool_O(_lisp); // base(s): set(['core::Number_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__BroadcastStream_O(core::Lisp_sp); // base(s): set(['core::Stream_O'])
{_BLOCK_TRACE("initializing Register_core__BroadcastStream_O");
    Register_core__BroadcastStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__BuiltIn_O(core::Lisp_sp); // base(s): set(['core::Function_O'])
{_BLOCK_TRACE("initializing Register_core__BuiltIn_O");
    Register_core__BuiltIn_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Closure_O(core::Lisp_sp); // base(s): set(['core::Function_O'])
{_BLOCK_TRACE("initializing Register_core__Closure_O");
    Register_core__Closure_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Complex_O(core::Lisp_sp); // base(s): set(['core::Number_O'])
{_BLOCK_TRACE("initializing Register_core__Complex_O");
    Register_core__Complex_O(_lisp); // base(s): set(['core::Number_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ConcatenatedStream_O(core::Lisp_sp); // base(s): set(['core::Stream_O'])
{_BLOCK_TRACE("initializing Register_core__ConcatenatedStream_O");
    Register_core__ConcatenatedStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ConditionWithStackTrace_O(core::Lisp_sp); // base(s): set(['core::ConditionBase_O'])
{_BLOCK_TRACE("initializing Register_core__ConditionWithStackTrace_O");
    Register_core__ConditionWithStackTrace_O(_lisp); // base(s): set(['core::ConditionBase_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__DirectoryIterator_O(core::Lisp_sp); // base(s): set(['core::Iterator_O'])
{_BLOCK_TRACE("initializing Register_core__DirectoryIterator_O");
    Register_core__DirectoryIterator_O(_lisp); // base(s): set(['core::Iterator_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__EchoStream_O(core::Lisp_sp); // base(s): set(['core::Stream_O'])
{_BLOCK_TRACE("initializing Register_core__EchoStream_O");
    Register_core__EchoStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ExtendedChar_O(core::Lisp_sp); // base(s): set(['core::Character_O'])
{_BLOCK_TRACE("initializing Register_core__ExtendedChar_O");
    Register_core__ExtendedChar_O(_lisp); // base(s): set(['core::Character_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FDStream_O(core::Lisp_sp); // base(s): set(['core::Stream_O'])
{_BLOCK_TRACE("initializing Register_core__FDStream_O");
    Register_core__FDStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FileStream_O(core::Lisp_sp); // base(s): set(['core::Stream_O'])
{_BLOCK_TRACE("initializing Register_core__FileStream_O");
    Register_core__FileStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FunctionNameSetfExpander_O(core::Lisp_sp); // base(s): set(['core::SetfExpander_O'])
{_BLOCK_TRACE("initializing Register_core__FunctionNameSetfExpander_O");
    Register_core__FunctionNameSetfExpander_O(_lisp); // base(s): set(['core::SetfExpander_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FunctionSetfExpander_O(core::Lisp_sp); // base(s): set(['core::SetfExpander_O'])
{_BLOCK_TRACE("initializing Register_core__FunctionSetfExpander_O");
    Register_core__FunctionSetfExpander_O(_lisp); // base(s): set(['core::SetfExpander_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__GenericFunction_O(core::Lisp_sp); // base(s): set(['core::Function_O'])
{_BLOCK_TRACE("initializing Register_core__GenericFunction_O");
    Register_core__GenericFunction_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__GlueEnvironment_O(core::Lisp_sp); // base(s): set(['core::Environment_O'])
{_BLOCK_TRACE("initializing Register_core__GlueEnvironment_O");
    Register_core__GlueEnvironment_O(_lisp); // base(s): set(['core::Environment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__HandlerBindHandler_O(core::Lisp_sp); // base(s): set(['core::ConditionHandlerBase_O'])
{_BLOCK_TRACE("initializing Register_core__HandlerBindHandler_O");
    Register_core__HandlerBindHandler_O(_lisp); // base(s): set(['core::ConditionHandlerBase_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__HandlerCaseHandler_O(core::Lisp_sp); // base(s): set(['core::ConditionHandlerBase_O'])
{_BLOCK_TRACE("initializing Register_core__HandlerCaseHandler_O");
    Register_core__HandlerCaseHandler_O(_lisp); // base(s): set(['core::ConditionHandlerBase_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__HashTableEq_O(core::Lisp_sp); // base(s): set(['core::HashTable_O'])
{_BLOCK_TRACE("initializing Register_core__HashTableEq_O");
    Register_core__HashTableEq_O(_lisp); // base(s): set(['core::HashTable_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__HashTableEql_O(core::Lisp_sp); // base(s): set(['core::HashTable_O'])
{_BLOCK_TRACE("initializing Register_core__HashTableEql_O");
    Register_core__HashTableEql_O(_lisp); // base(s): set(['core::HashTable_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__HashTableEqual_O(core::Lisp_sp); // base(s): set(['core::HashTable_O'])
{_BLOCK_TRACE("initializing Register_core__HashTableEqual_O");
    Register_core__HashTableEqual_O(_lisp); // base(s): set(['core::HashTable_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__HashTableEqualp_O(core::Lisp_sp); // base(s): set(['core::HashTable_O'])
{_BLOCK_TRACE("initializing Register_core__HashTableEqualp_O");
    Register_core__HashTableEqualp_O(_lisp); // base(s): set(['core::HashTable_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Interpreted_O(core::Lisp_sp); // base(s): set(['core::Function_O'])
{_BLOCK_TRACE("initializing Register_core__Interpreted_O");
    Register_core__Interpreted_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__LexicalEnvironment_O(core::Lisp_sp); // base(s): set(['core::Environment_O'])
{_BLOCK_TRACE("initializing Register_core__LexicalEnvironment_O");
    Register_core__LexicalEnvironment_O(_lisp); // base(s): set(['core::Environment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__LoadArchive_O(core::Lisp_sp); // base(s): set(['core::Archive_O'])
{_BLOCK_TRACE("initializing Register_core__LoadArchive_O");
    Register_core__LoadArchive_O(_lisp); // base(s): set(['core::Archive_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__MetaClass_O(core::Lisp_sp); // base(s): set(['core::StandardObject_O'])
{_BLOCK_TRACE("initializing Register_core__MetaClass_O");
    Register_core__MetaClass_O(_lisp); // base(s): set(['core::StandardObject_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__MetaObject_O(core::Lisp_sp); // base(s): set(['core::StandardObject_O'])
{_BLOCK_TRACE("initializing Register_core__MetaObject_O");
    Register_core__MetaObject_O(_lisp); // base(s): set(['core::StandardObject_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ReadSerializer_O(core::Lisp_sp); // base(s): set(['core::Serializer_O'])
{_BLOCK_TRACE("initializing Register_core__ReadSerializer_O");
    Register_core__ReadSerializer_O(_lisp); // base(s): set(['core::Serializer_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Real_O(core::Lisp_sp); // base(s): set(['core::Number_O'])
{_BLOCK_TRACE("initializing Register_core__Real_O");
    Register_core__Real_O(_lisp); // base(s): set(['core::Number_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__RecursiveDirectoryIterator_O(core::Lisp_sp); // base(s): set(['core::Iterator_O'])
{_BLOCK_TRACE("initializing Register_core__RecursiveDirectoryIterator_O");
    Register_core__RecursiveDirectoryIterator_O(_lisp); // base(s): set(['core::Iterator_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SaveArchive_O(core::Lisp_sp); // base(s): set(['core::Archive_O'])
{_BLOCK_TRACE("initializing Register_core__SaveArchive_O");
    Register_core__SaveArchive_O(_lisp); // base(s): set(['core::Archive_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Sequence_O(core::Lisp_sp); // base(s): set(['core::SequenceVirtual_O'])
{_BLOCK_TRACE("initializing Register_core__Sequence_O");
    Register_core__Sequence_O(_lisp); // base(s): set(['core::SequenceVirtual_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SingleDispatchEffectiveMethodFunction_O(core::Lisp_sp); // base(s): set(['core::Function_O'])
{_BLOCK_TRACE("initializing Register_core__SingleDispatchEffectiveMethodFunction_O");
    Register_core__SingleDispatchEffectiveMethodFunction_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SlotValueSetfExpander_O(core::Lisp_sp); // base(s): set(['core::SetfExpander_O'])
{_BLOCK_TRACE("initializing Register_core__SlotValueSetfExpander_O");
    Register_core__SlotValueSetfExpander_O(_lisp); // base(s): set(['core::SetfExpander_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SpecialForm_O(core::Lisp_sp); // base(s): set(['core::Function_O'])
{_BLOCK_TRACE("initializing Register_core__SpecialForm_O");
    Register_core__SpecialForm_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__StringStream_O(core::Lisp_sp); // base(s): set(['core::Stream_O'])
{_BLOCK_TRACE("initializing Register_core__StringStream_O");
    Register_core__StringStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__StructureObject_O(core::Lisp_sp); // base(s): set(['core::Model_O'])
{_BLOCK_TRACE("initializing Register_core__StructureObject_O");
    Register_core__StructureObject_O(_lisp); // base(s): set(['core::Model_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SynonymStream_O(core::Lisp_sp); // base(s): set(['core::Stream_O'])
{_BLOCK_TRACE("initializing Register_core__SynonymStream_O");
    Register_core__SynonymStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__TwoWayStream_O(core::Lisp_sp); // base(s): set(['core::Stream_O'])
{_BLOCK_TRACE("initializing Register_core__TwoWayStream_O");
    Register_core__TwoWayStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__WriteSerializer_O(core::Lisp_sp); // base(s): set(['core::Serializer_O'])
{_BLOCK_TRACE("initializing Register_core__WriteSerializer_O");
    Register_core__WriteSerializer_O(_lisp); // base(s): set(['core::Serializer_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ArrayObjects_O(core::Lisp_sp); // base(s): set(['core::Array_O'])
{_BLOCK_TRACE("initializing Register_core__ArrayObjects_O");
    Register_core__ArrayObjects_O(_lisp); // base(s): set(['core::Array_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__BuiltInClass_O(core::Lisp_sp); // base(s): set(['core::MetaClass_O'])
{_BLOCK_TRACE("initializing Register_core__BuiltInClass_O");
    Register_core__BuiltInClass_O(_lisp); // base(s): set(['core::MetaClass_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__CandoException_O(core::Lisp_sp); // base(s): set(['core::ConditionWithStackTrace_O'])
{_BLOCK_TRACE("initializing Register_core__CandoException_O");
    Register_core__CandoException_O(_lisp); // base(s): set(['core::ConditionWithStackTrace_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__CandoFormatSaveArchive_O(core::Lisp_sp); // base(s): set(['core::SaveArchive_O'])
{_BLOCK_TRACE("initializing Register_core__CandoFormatSaveArchive_O");
    Register_core__CandoFormatSaveArchive_O(_lisp); // base(s): set(['core::SaveArchive_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__CompileTimeEnvironment_O(core::Lisp_sp); // base(s): set(['core::LexicalEnvironment_O'])
{_BLOCK_TRACE("initializing Register_core__CompileTimeEnvironment_O");
    Register_core__CompileTimeEnvironment_O(_lisp); // base(s): set(['core::LexicalEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Condition_O(core::Lisp_sp); // base(s): set(['core::ConditionWithStackTrace_O'])
{_BLOCK_TRACE("initializing Register_core__Condition_O");
    Register_core__Condition_O(_lisp); // base(s): set(['core::ConditionWithStackTrace_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FDInStream_O(core::Lisp_sp); // base(s): set(['core::FDStream_O'])
{_BLOCK_TRACE("initializing Register_core__FDInStream_O");
    Register_core__FDInStream_O(_lisp); // base(s): set(['core::FDStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FDOutStream_O(core::Lisp_sp); // base(s): set(['core::FDStream_O'])
{_BLOCK_TRACE("initializing Register_core__FDOutStream_O");
    Register_core__FDOutStream_O(_lisp); // base(s): set(['core::FDStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FileInCompressedStream_O(core::Lisp_sp); // base(s): set(['core::FileStream_O'])
{_BLOCK_TRACE("initializing Register_core__FileInCompressedStream_O");
    Register_core__FileInCompressedStream_O(_lisp); // base(s): set(['core::FileStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FileInStream_O(core::Lisp_sp); // base(s): set(['core::FileStream_O'])
{_BLOCK_TRACE("initializing Register_core__FileInStream_O");
    Register_core__FileInStream_O(_lisp); // base(s): set(['core::FileStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FileOutCompressedStream_O(core::Lisp_sp); // base(s): set(['core::FileStream_O'])
{_BLOCK_TRACE("initializing Register_core__FileOutCompressedStream_O");
    Register_core__FileOutCompressedStream_O(_lisp); // base(s): set(['core::FileStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FileOutStream_O(core::Lisp_sp); // base(s): set(['core::FileStream_O'])
{_BLOCK_TRACE("initializing Register_core__FileOutStream_O");
    Register_core__FileOutStream_O(_lisp); // base(s): set(['core::FileStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Float_O(core::Lisp_sp); // base(s): set(['core::Real_O'])
{_BLOCK_TRACE("initializing Register_core__Float_O");
    Register_core__Float_O(_lisp); // base(s): set(['core::Real_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ForwardReferencedClass_O(core::Lisp_sp); // base(s): set(['core::MetaClass_O'])
{_BLOCK_TRACE("initializing Register_core__ForwardReferencedClass_O");
    Register_core__ForwardReferencedClass_O(_lisp); // base(s): set(['core::MetaClass_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FunctionFrame_O(core::Lisp_sp); // base(s): set(['core::ActivationFrame_O'])
{_BLOCK_TRACE("initializing Register_core__FunctionFrame_O");
    Register_core__FunctionFrame_O(_lisp); // base(s): set(['core::ActivationFrame_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__List_O(core::Lisp_sp); // base(s): set(['core::Sequence_O'])
{_BLOCK_TRACE("initializing Register_core__List_O");
    Register_core__List_O(_lisp); // base(s): set(['core::Sequence_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Rational_O(core::Lisp_sp); // base(s): set(['core::Real_O'])
{_BLOCK_TRACE("initializing Register_core__Rational_O");
    Register_core__Rational_O(_lisp); // base(s): set(['core::Real_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__RuntimeVisibleEnvironment_O(core::Lisp_sp); // base(s): set(['core::LexicalEnvironment_O'])
{_BLOCK_TRACE("initializing Register_core__RuntimeVisibleEnvironment_O");
    Register_core__RuntimeVisibleEnvironment_O(_lisp); // base(s): set(['core::LexicalEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SingleDispatchGenericFunction_O(core::Lisp_sp); // base(s): set(['core::BuiltIn_O'])
{_BLOCK_TRACE("initializing Register_core__SingleDispatchGenericFunction_O");
    Register_core__SingleDispatchGenericFunction_O(_lisp); // base(s): set(['core::BuiltIn_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__StandardChar_O(core::Lisp_sp); // base(s): set(['core::BaseChar_O'])
{_BLOCK_TRACE("initializing Register_core__StandardChar_O");
    Register_core__StandardChar_O(_lisp); // base(s): set(['core::BaseChar_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__StandardClass_O(core::Lisp_sp); // base(s): set(['core::MetaClass_O'])
{_BLOCK_TRACE("initializing Register_core__StandardClass_O");
    Register_core__StandardClass_O(_lisp); // base(s): set(['core::MetaClass_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__StandardGenericFunction_O(core::Lisp_sp); // base(s): set(['core::GenericFunction_O'])
{_BLOCK_TRACE("initializing Register_core__StandardGenericFunction_O");
    Register_core__StandardGenericFunction_O(_lisp); // base(s): set(['core::GenericFunction_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__StringInStream_O(core::Lisp_sp); // base(s): set(['core::StringStream_O'])
{_BLOCK_TRACE("initializing Register_core__StringInStream_O");
    Register_core__StringInStream_O(_lisp); // base(s): set(['core::StringStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__StringOutStream_O(core::Lisp_sp); // base(s): set(['core::StringStream_O'])
{_BLOCK_TRACE("initializing Register_core__StringOutStream_O");
    Register_core__StringOutStream_O(_lisp); // base(s): set(['core::StringStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__StructureClass_O(core::Lisp_sp); // base(s): set(['core::MetaClass_O'])
{_BLOCK_TRACE("initializing Register_core__StructureClass_O");
    Register_core__StructureClass_O(_lisp); // base(s): set(['core::MetaClass_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ValueFrame_O(core::Lisp_sp); // base(s): set(['core::ActivationFrame_O'])
{_BLOCK_TRACE("initializing Register_core__ValueFrame_O");
    Register_core__ValueFrame_O(_lisp); // base(s): set(['core::ActivationFrame_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Vector_O(core::Lisp_sp); // base(s): set(['core::Array_O', 'core::Sequence_O'])
{_BLOCK_TRACE("initializing Register_core__Vector_O");
    Register_core__Vector_O(_lisp); // base(s): set(['core::Array_O', 'core::Sequence_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__XmlLoadArchive_O(core::Lisp_sp); // base(s): set(['core::LoadArchive_O'])
{_BLOCK_TRACE("initializing Register_core__XmlLoadArchive_O");
    Register_core__XmlLoadArchive_O(_lisp); // base(s): set(['core::LoadArchive_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__XmlSaveArchive_O(core::Lisp_sp); // base(s): set(['core::SaveArchive_O'])
{_BLOCK_TRACE("initializing Register_core__XmlSaveArchive_O");
    Register_core__XmlSaveArchive_O(_lisp); // base(s): set(['core::SaveArchive_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ArchiveError_O(core::Lisp_sp); // base(s): set(['core::CandoException_O'])
{_BLOCK_TRACE("initializing Register_core__ArchiveError_O");
    Register_core__ArchiveError_O(_lisp); // base(s): set(['core::CandoException_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__BitVector_O(core::Lisp_sp); // base(s): set(['core::Vector_O'])
{_BLOCK_TRACE("initializing Register_core__BitVector_O");
    Register_core__BitVector_O(_lisp); // base(s): set(['core::Vector_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__BlockEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
{_BLOCK_TRACE("initializing Register_core__BlockEnvironment_O");
    Register_core__BlockEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__CatchEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
{_BLOCK_TRACE("initializing Register_core__CatchEnvironment_O");
    Register_core__CatchEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Cons_O(core::Lisp_sp); // base(s): set(['core::List_O'])
{_BLOCK_TRACE("initializing Register_core__Cons_O");
    Register_core__Cons_O(_lisp); // base(s): set(['core::List_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ContentException_O(core::Lisp_sp); // base(s): set(['core::CandoException_O'])
{_BLOCK_TRACE("initializing Register_core__ContentException_O");
    Register_core__ContentException_O(_lisp); // base(s): set(['core::CandoException_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__DoubleFloat_O(core::Lisp_sp); // base(s): set(['core::Float_O'])
{_BLOCK_TRACE("initializing Register_core__DoubleFloat_O");
    Register_core__DoubleFloat_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FileNotFoundException_O(core::Lisp_sp); // base(s): set(['core::CandoException_O'])
{_BLOCK_TRACE("initializing Register_core__FileNotFoundException_O");
    Register_core__FileNotFoundException_O(_lisp); // base(s): set(['core::CandoException_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FunctionContainerEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
{_BLOCK_TRACE("initializing Register_core__FunctionContainerEnvironment_O");
    Register_core__FunctionContainerEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__FunctionValueEnvironment_O(core::Lisp_sp); // base(s): set(['core::RuntimeVisibleEnvironment_O'])
{_BLOCK_TRACE("initializing Register_core__FunctionValueEnvironment_O");
    Register_core__FunctionValueEnvironment_O(_lisp); // base(s): set(['core::RuntimeVisibleEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Integer_O(core::Lisp_sp); // base(s): set(['core::Rational_O'])
{_BLOCK_TRACE("initializing Register_core__Integer_O");
    Register_core__Integer_O(_lisp); // base(s): set(['core::Rational_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__LispError_O(core::Lisp_sp); // base(s): set(['core::CandoException_O'])
{_BLOCK_TRACE("initializing Register_core__LispError_O");
    Register_core__LispError_O(_lisp); // base(s): set(['core::CandoException_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__LongFloat_O(core::Lisp_sp); // base(s): set(['core::Float_O'])
{_BLOCK_TRACE("initializing Register_core__LongFloat_O");
    Register_core__LongFloat_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__MacroletEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
{_BLOCK_TRACE("initializing Register_core__MacroletEnvironment_O");
    Register_core__MacroletEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__MathException_O(core::Lisp_sp); // base(s): set(['core::CandoException_O'])
{_BLOCK_TRACE("initializing Register_core__MathException_O");
    Register_core__MathException_O(_lisp); // base(s): set(['core::CandoException_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Null_O(core::Lisp_sp); // base(s): set(['core::Symbol_O', 'core::List_O'])
{_BLOCK_TRACE("initializing Register_core__Null_O");
    Register_core__Null_O(_lisp); // base(s): set(['core::Symbol_O', 'core::List_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Ratio_O(core::Lisp_sp); // base(s): set(['core::Rational_O'])
{_BLOCK_TRACE("initializing Register_core__Ratio_O");
    Register_core__Ratio_O(_lisp); // base(s): set(['core::Rational_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SerializeError_O(core::Lisp_sp); // base(s): set(['core::CandoException_O'])
{_BLOCK_TRACE("initializing Register_core__SerializeError_O");
    Register_core__SerializeError_O(_lisp); // base(s): set(['core::CandoException_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ShortFloat_O(core::Lisp_sp); // base(s): set(['core::Float_O'])
{_BLOCK_TRACE("initializing Register_core__ShortFloat_O");
    Register_core__ShortFloat_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SingleFloat_O(core::Lisp_sp); // base(s): set(['core::Float_O'])
{_BLOCK_TRACE("initializing Register_core__SingleFloat_O");
    Register_core__SingleFloat_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__String_O(core::Lisp_sp); // base(s): set(['core::Vector_O'])
{_BLOCK_TRACE("initializing Register_core__String_O");
    Register_core__String_O(_lisp); // base(s): set(['core::Vector_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SymbolMacroletEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
{_BLOCK_TRACE("initializing Register_core__SymbolMacroletEnvironment_O");
    Register_core__SymbolMacroletEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__TagbodyEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
{_BLOCK_TRACE("initializing Register_core__TagbodyEnvironment_O");
    Register_core__TagbodyEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__TypeException_O(core::Lisp_sp); // base(s): set(['core::CandoException_O'])
{_BLOCK_TRACE("initializing Register_core__TypeException_O");
    Register_core__TypeException_O(_lisp); // base(s): set(['core::CandoException_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__UnwindProtectEnvironment_O(core::Lisp_sp); // base(s): set(['core::CompileTimeEnvironment_O'])
{_BLOCK_TRACE("initializing Register_core__UnwindProtectEnvironment_O");
    Register_core__UnwindProtectEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__ValueEnvironment_O(core::Lisp_sp); // base(s): set(['core::RuntimeVisibleEnvironment_O'])
{_BLOCK_TRACE("initializing Register_core__ValueEnvironment_O");
    Register_core__ValueEnvironment_O(_lisp); // base(s): set(['core::RuntimeVisibleEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__VectorObjects_O(core::Lisp_sp); // base(s): set(['core::Vector_O'])
{_BLOCK_TRACE("initializing Register_core__VectorObjects_O");
    Register_core__VectorObjects_O(_lisp); // base(s): set(['core::Vector_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__serious_condition_O(core::Lisp_sp); // base(s): set(['core::Condition_O'])
{_BLOCK_TRACE("initializing Register_core__serious_condition_O");
    Register_core__serious_condition_O(_lisp); // base(s): set(['core::Condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__simple_condition_O(core::Lisp_sp); // base(s): set(['core::Condition_O'])
{_BLOCK_TRACE("initializing Register_core__simple_condition_O");
    Register_core__simple_condition_O(_lisp); // base(s): set(['core::Condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__warning_O(core::Lisp_sp); // base(s): set(['core::Condition_O'])
{_BLOCK_TRACE("initializing Register_core__warning_O");
    Register_core__warning_O(_lisp); // base(s): set(['core::Condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Bignum_O(core::Lisp_sp); // base(s): set(['core::Integer_O'])
{_BLOCK_TRACE("initializing Register_core__Bignum_O");
    Register_core__Bignum_O(_lisp); // base(s): set(['core::Integer_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__CompiledBody_O(core::Lisp_sp); // base(s): set(['core::Cons_O'])
{_BLOCK_TRACE("initializing Register_core__CompiledBody_O");
    Register_core__CompiledBody_O(_lisp); // base(s): set(['core::Cons_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Error_O(core::Lisp_sp); // base(s): set(['core::serious_condition_O'])
{_BLOCK_TRACE("initializing Register_core__Error_O");
    Register_core__Error_O(_lisp); // base(s): set(['core::serious_condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Fixnum_O(core::Lisp_sp); // base(s): set(['core::Integer_O'])
{_BLOCK_TRACE("initializing Register_core__Fixnum_O");
    Register_core__Fixnum_O(_lisp); // base(s): set(['core::Integer_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SimpleBitVector_O(core::Lisp_sp); // base(s): set(['core::BitVector_O'])
{_BLOCK_TRACE("initializing Register_core__SimpleBitVector_O");
    Register_core__SimpleBitVector_O(_lisp); // base(s): set(['core::BitVector_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__SourceCodeCons_O(core::Lisp_sp); // base(s): set(['core::Cons_O'])
{_BLOCK_TRACE("initializing Register_core__SourceCodeCons_O");
    Register_core__SourceCodeCons_O(_lisp); // base(s): set(['core::Cons_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__Str_O(core::Lisp_sp); // base(s): set(['core::String_O'])
{_BLOCK_TRACE("initializing Register_core__Str_O");
    Register_core__Str_O(_lisp); // base(s): set(['core::String_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__VectorObjectsWithFillPtr_O(core::Lisp_sp); // base(s): set(['core::VectorObjects_O'])
{_BLOCK_TRACE("initializing Register_core__VectorObjectsWithFillPtr_O");
    Register_core__VectorObjectsWithFillPtr_O(_lisp); // base(s): set(['core::VectorObjects_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__simple_warning_O(core::Lisp_sp); // base(s): set(['core::warning_O', 'core::simple_condition_O'])
{_BLOCK_TRACE("initializing Register_core__simple_warning_O");
    Register_core__simple_warning_O(_lisp); // base(s): set(['core::warning_O', 'core::simple_condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__storage_condition_O(core::Lisp_sp); // base(s): set(['core::serious_condition_O'])
{_BLOCK_TRACE("initializing Register_core__storage_condition_O");
    Register_core__storage_condition_O(_lisp); // base(s): set(['core::serious_condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__style_warning_O(core::Lisp_sp); // base(s): set(['core::warning_O'])
{_BLOCK_TRACE("initializing Register_core__style_warning_O");
    Register_core__style_warning_O(_lisp); // base(s): set(['core::warning_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__LoadTimeValues_O(core::Lisp_sp); // base(s): set(['core::VectorObjectsWithFillPtr_O'])
{_BLOCK_TRACE("initializing Register_core__LoadTimeValues_O");
    Register_core__LoadTimeValues_O(_lisp); // base(s): set(['core::VectorObjectsWithFillPtr_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__arithmetic_error_O(core::Lisp_sp); // base(s): set(['core::Error_O'])
{_BLOCK_TRACE("initializing Register_core__arithmetic_error_O");
    Register_core__arithmetic_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__cell_error_O(core::Lisp_sp); // base(s): set(['core::Error_O'])
{_BLOCK_TRACE("initializing Register_core__cell_error_O");
    Register_core__cell_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__control_error_O(core::Lisp_sp); // base(s): set(['core::Error_O'])
{_BLOCK_TRACE("initializing Register_core__control_error_O");
    Register_core__control_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__file_error_O(core::Lisp_sp); // base(s): set(['core::Error_O'])
{_BLOCK_TRACE("initializing Register_core__file_error_O");
    Register_core__file_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__package_error_O(core::Lisp_sp); // base(s): set(['core::Error_O'])
{_BLOCK_TRACE("initializing Register_core__package_error_O");
    Register_core__package_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__parse_error_O(core::Lisp_sp); // base(s): set(['core::Error_O'])
{_BLOCK_TRACE("initializing Register_core__parse_error_O");
    Register_core__parse_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__print_not_readable_O(core::Lisp_sp); // base(s): set(['core::Error_O'])
{_BLOCK_TRACE("initializing Register_core__print_not_readable_O");
    Register_core__print_not_readable_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__program_error_O(core::Lisp_sp); // base(s): set(['core::Error_O'])
{_BLOCK_TRACE("initializing Register_core__program_error_O");
    Register_core__program_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__simple_error_O(core::Lisp_sp); // base(s): set(['core::Error_O', 'core::simple_condition_O'])
{_BLOCK_TRACE("initializing Register_core__simple_error_O");
    Register_core__simple_error_O(_lisp); // base(s): set(['core::Error_O', 'core::simple_condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__stream_error_O(core::Lisp_sp); // base(s): set(['core::Error_O'])
{_BLOCK_TRACE("initializing Register_core__stream_error_O");
    Register_core__stream_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__type_error_O(core::Lisp_sp); // base(s): set(['core::Error_O'])
{_BLOCK_TRACE("initializing Register_core__type_error_O");
    Register_core__type_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__argument_mismatch_error_O(core::Lisp_sp); // base(s): set(['core::program_error_O'])
{_BLOCK_TRACE("initializing Register_core__argument_mismatch_error_O");
    Register_core__argument_mismatch_error_O(_lisp); // base(s): set(['core::program_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__division_by_zero_O(core::Lisp_sp); // base(s): set(['core::arithmetic_error_O'])
{_BLOCK_TRACE("initializing Register_core__division_by_zero_O");
    Register_core__division_by_zero_O(_lisp); // base(s): set(['core::arithmetic_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__end_of_file_O(core::Lisp_sp); // base(s): set(['core::stream_error_O'])
{_BLOCK_TRACE("initializing Register_core__end_of_file_O");
    Register_core__end_of_file_O(_lisp); // base(s): set(['core::stream_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__floating_point_inexact_O(core::Lisp_sp); // base(s): set(['core::arithmetic_error_O'])
{_BLOCK_TRACE("initializing Register_core__floating_point_inexact_O");
    Register_core__floating_point_inexact_O(_lisp); // base(s): set(['core::arithmetic_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__floating_point_invalid_operation_O(core::Lisp_sp); // base(s): set(['core::arithmetic_error_O'])
{_BLOCK_TRACE("initializing Register_core__floating_point_invalid_operation_O");
    Register_core__floating_point_invalid_operation_O(_lisp); // base(s): set(['core::arithmetic_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__floating_point_overflow_O(core::Lisp_sp); // base(s): set(['core::arithmetic_error_O'])
{_BLOCK_TRACE("initializing Register_core__floating_point_overflow_O");
    Register_core__floating_point_overflow_O(_lisp); // base(s): set(['core::arithmetic_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__floating_point_underflow_O(core::Lisp_sp); // base(s): set(['core::arithmetic_error_O'])
{_BLOCK_TRACE("initializing Register_core__floating_point_underflow_O");
    Register_core__floating_point_underflow_O(_lisp); // base(s): set(['core::arithmetic_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__reader_error_O(core::Lisp_sp); // base(s): set(['core::stream_error_O', 'core::parse_error_O'])
{_BLOCK_TRACE("initializing Register_core__reader_error_O");
    Register_core__reader_error_O(_lisp); // base(s): set(['core::stream_error_O', 'core::parse_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__simple_type_error_O(core::Lisp_sp); // base(s): set(['core::type_error_O'])
{_BLOCK_TRACE("initializing Register_core__simple_type_error_O");
    Register_core__simple_type_error_O(_lisp); // base(s): set(['core::type_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__unbound_slot_O(core::Lisp_sp); // base(s): set(['core::cell_error_O'])
{_BLOCK_TRACE("initializing Register_core__unbound_slot_O");
    Register_core__unbound_slot_O(_lisp); // base(s): set(['core::cell_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__unbound_variable_O(core::Lisp_sp); // base(s): set(['core::cell_error_O'])
{_BLOCK_TRACE("initializing Register_core__unbound_variable_O");
    Register_core__unbound_variable_O(_lisp); // base(s): set(['core::cell_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__undefined_function_O(core::Lisp_sp); // base(s): set(['core::cell_error_O'])
{_BLOCK_TRACE("initializing Register_core__undefined_function_O");
    Register_core__undefined_function_O(_lisp); // base(s): set(['core::cell_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__invalid_keyword_arguments_error_O(core::Lisp_sp); // base(s): set(['core::argument_mismatch_error_O'])
{_BLOCK_TRACE("initializing Register_core__invalid_keyword_arguments_error_O");
    Register_core__invalid_keyword_arguments_error_O(_lisp); // base(s): set(['core::argument_mismatch_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__odd_number_of_keyword_arguments_error_O(core::Lisp_sp); // base(s): set(['core::argument_mismatch_error_O'])
{_BLOCK_TRACE("initializing Register_core__odd_number_of_keyword_arguments_error_O");
    Register_core__odd_number_of_keyword_arguments_error_O(_lisp); // base(s): set(['core::argument_mismatch_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__single_dispatch_arguments_error_O(core::Lisp_sp); // base(s): set(['core::argument_mismatch_error_O'])
{_BLOCK_TRACE("initializing Register_core__single_dispatch_arguments_error_O");
    Register_core__single_dispatch_arguments_error_O(_lisp); // base(s): set(['core::argument_mismatch_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__too_few_arguments_error_O(core::Lisp_sp); // base(s): set(['core::argument_mismatch_error_O'])
{_BLOCK_TRACE("initializing Register_core__too_few_arguments_error_O");
    Register_core__too_few_arguments_error_O(_lisp); // base(s): set(['core::argument_mismatch_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__too_many_arguments_error_O(core::Lisp_sp); // base(s): set(['core::argument_mismatch_error_O'])
{_BLOCK_TRACE("initializing Register_core__too_many_arguments_error_O");
    Register_core__too_many_arguments_error_O(_lisp); // base(s): set(['core::argument_mismatch_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Register_core__unrecognized_keyword_arguments_error_O(core::Lisp_sp); // base(s): set(['core::argument_mismatch_error_O'])
{_BLOCK_TRACE("initializing Register_core__unrecognized_keyword_arguments_error_O");
    Register_core__unrecognized_keyword_arguments_error_O(_lisp); // base(s): set(['core::argument_mismatch_error_O'])
}
#endif // ifdef Use_CorePkg
#endif // EXPOSE_TO_CANDO
#undef EXPOSE_TO_CANDO
#ifdef EXPOSE_TO_PYTHON
#ifdef Use_CorePkg
extern void Call_exposePython_core__T_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__T_O");
	Call_exposePython_core__T_O(_lisp); // base(s): set(['core::_RootDummyClass'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ArchiveMemoryManager_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ArchiveMemoryManager_O");
	Call_exposePython_core__ArchiveMemoryManager_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Archive_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Archive_O");
	Call_exposePython_core__Archive_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ArrayVirtual_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ArrayVirtual_O");
	Call_exposePython_core__ArrayVirtual_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Binder_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Binder_O");
	Call_exposePython_core__Binder_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Character_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Character_O");
	Call_exposePython_core__Character_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ConditionBase_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ConditionBase_O");
	Call_exposePython_core__ConditionBase_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ConditionHandlerBase_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ConditionHandlerBase_O");
	Call_exposePython_core__ConditionHandlerBase_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
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
extern void Call_exposePython_core__ExternalObjectManager_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ExternalObjectManager_O");
	Call_exposePython_core__ExternalObjectManager_O(_lisp); // base(s): set(['core::T_O'])
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
#ifdef Use_CorePkg
extern void Call_exposePython_core__FormBinder_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FormBinder_O");
	Call_exposePython_core__FormBinder_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Function_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Function_O");
	Call_exposePython_core__Function_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__HashTable_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__HashTable_O");
	Call_exposePython_core__HashTable_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Instance_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Instance_O");
	Call_exposePython_core__Instance_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__IntArray_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__IntArray_O");
	Call_exposePython_core__IntArray_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__IntegerKeyObjectDictionary_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__IntegerKeyObjectDictionary_O");
	Call_exposePython_core__IntegerKeyObjectDictionary_O(_lisp); // base(s): set(['core::T_O'])
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
extern void Call_exposePython_core__Lisp_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Lisp_O");
	Call_exposePython_core__Lisp_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__MethodCombination_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__MethodCombination_O");
	Call_exposePython_core__MethodCombination_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__MethodSet_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__MethodSet_O");
	Call_exposePython_core__MethodSet_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Method_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Method_O");
	Call_exposePython_core__Method_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__MicroHeap_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__MicroHeap_O");
	Call_exposePython_core__MicroHeap_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Model_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Model_O");
	Call_exposePython_core__Model_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__MultiStringBuffer_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__MultiStringBuffer_O");
	Call_exposePython_core__MultiStringBuffer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__MultipleValues_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__MultipleValues_O");
	Call_exposePython_core__MultipleValues_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Number_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Number_O");
	Call_exposePython_core__Number_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
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
#ifdef Use_CorePkg
extern void Call_exposePython_core__Package_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Package_O");
	Call_exposePython_core__Package_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Path_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Path_O");
	Call_exposePython_core__Path_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
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
#ifdef Use_CorePkg
extern void Call_exposePython_core__QDomNode_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__QDomNode_O");
	Call_exposePython_core__QDomNode_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ReadTable_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ReadTable_O");
	Call_exposePython_core__ReadTable_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Reader_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Reader_O");
	Call_exposePython_core__Reader_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__RestartCase_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__RestartCase_O");
	Call_exposePython_core__RestartCase_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SequenceVirtual_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SequenceVirtual_O");
	Call_exposePython_core__SequenceVirtual_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Serializer_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Serializer_O");
	Call_exposePython_core__Serializer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SetfExpander_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SetfExpander_O");
	Call_exposePython_core__SetfExpander_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SingleDispatchMethod_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SingleDispatchMethod_O");
	Call_exposePython_core__SingleDispatchMethod_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SourceFileInfo_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SourceFileInfo_O");
	Call_exposePython_core__SourceFileInfo_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StandardObject_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StandardObject_O");
	Call_exposePython_core__StandardObject_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Stream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Stream_O");
	Call_exposePython_core__Stream_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
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
#ifdef Use_CorePkg
extern void Call_exposePython_core__SymbolVectorMap_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SymbolVectorMap_O");
	Call_exposePython_core__SymbolVectorMap_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SymbolVector_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SymbolVector_O");
	Call_exposePython_core__SymbolVector_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Symbol_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Symbol_O");
	Call_exposePython_core__Symbol_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__WeakReference_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__WeakReference_O");
	Call_exposePython_core__WeakReference_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ActivationFrame_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ActivationFrame_O");
	Call_exposePython_core__ActivationFrame_O(_lisp); // base(s): set(['core::Environment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Array_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Array_O");
	Call_exposePython_core__Array_O(_lisp); // base(s): set(['core::ArrayVirtual_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__BaseChar_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__BaseChar_O");
	Call_exposePython_core__BaseChar_O(_lisp); // base(s): set(['core::Character_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Bool_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Bool_O");
	Call_exposePython_core__Bool_O(_lisp); // base(s): set(['core::Number_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__BroadcastStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__BroadcastStream_O");
	Call_exposePython_core__BroadcastStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__BuiltIn_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__BuiltIn_O");
	Call_exposePython_core__BuiltIn_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Closure_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Closure_O");
	Call_exposePython_core__Closure_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Complex_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Complex_O");
	Call_exposePython_core__Complex_O(_lisp); // base(s): set(['core::Number_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ConcatenatedStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ConcatenatedStream_O");
	Call_exposePython_core__ConcatenatedStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ConditionWithStackTrace_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ConditionWithStackTrace_O");
	Call_exposePython_core__ConditionWithStackTrace_O(_lisp); // base(s): set(['core::ConditionBase_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__DirectoryIterator_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__DirectoryIterator_O");
	Call_exposePython_core__DirectoryIterator_O(_lisp); // base(s): set(['core::Iterator_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__EchoStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__EchoStream_O");
	Call_exposePython_core__EchoStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ExtendedChar_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ExtendedChar_O");
	Call_exposePython_core__ExtendedChar_O(_lisp); // base(s): set(['core::Character_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FDStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FDStream_O");
	Call_exposePython_core__FDStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FileStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FileStream_O");
	Call_exposePython_core__FileStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FunctionNameSetfExpander_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FunctionNameSetfExpander_O");
	Call_exposePython_core__FunctionNameSetfExpander_O(_lisp); // base(s): set(['core::SetfExpander_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FunctionSetfExpander_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FunctionSetfExpander_O");
	Call_exposePython_core__FunctionSetfExpander_O(_lisp); // base(s): set(['core::SetfExpander_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__GenericFunction_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__GenericFunction_O");
	Call_exposePython_core__GenericFunction_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__GlueEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__GlueEnvironment_O");
	Call_exposePython_core__GlueEnvironment_O(_lisp); // base(s): set(['core::Environment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__HandlerBindHandler_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__HandlerBindHandler_O");
	Call_exposePython_core__HandlerBindHandler_O(_lisp); // base(s): set(['core::ConditionHandlerBase_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__HandlerCaseHandler_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__HandlerCaseHandler_O");
	Call_exposePython_core__HandlerCaseHandler_O(_lisp); // base(s): set(['core::ConditionHandlerBase_O'])
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
extern void Call_exposePython_core__Interpreted_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Interpreted_O");
	Call_exposePython_core__Interpreted_O(_lisp); // base(s): set(['core::Function_O'])
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
#ifdef Use_CorePkg
extern void Call_exposePython_core__MetaClass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__MetaClass_O");
	Call_exposePython_core__MetaClass_O(_lisp); // base(s): set(['core::StandardObject_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__MetaObject_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__MetaObject_O");
	Call_exposePython_core__MetaObject_O(_lisp); // base(s): set(['core::StandardObject_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ReadSerializer_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ReadSerializer_O");
	Call_exposePython_core__ReadSerializer_O(_lisp); // base(s): set(['core::Serializer_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Real_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Real_O");
	Call_exposePython_core__Real_O(_lisp); // base(s): set(['core::Number_O'])
}
#endif // ifdef Use_CorePkg
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
extern void Call_exposePython_core__Sequence_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Sequence_O");
	Call_exposePython_core__Sequence_O(_lisp); // base(s): set(['core::SequenceVirtual_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SingleDispatchEffectiveMethodFunction_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SingleDispatchEffectiveMethodFunction_O");
	Call_exposePython_core__SingleDispatchEffectiveMethodFunction_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SlotValueSetfExpander_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SlotValueSetfExpander_O");
	Call_exposePython_core__SlotValueSetfExpander_O(_lisp); // base(s): set(['core::SetfExpander_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SpecialForm_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SpecialForm_O");
	Call_exposePython_core__SpecialForm_O(_lisp); // base(s): set(['core::Function_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StringStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StringStream_O");
	Call_exposePython_core__StringStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StructureObject_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StructureObject_O");
	Call_exposePython_core__StructureObject_O(_lisp); // base(s): set(['core::Model_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SynonymStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SynonymStream_O");
	Call_exposePython_core__SynonymStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__TwoWayStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__TwoWayStream_O");
	Call_exposePython_core__TwoWayStream_O(_lisp); // base(s): set(['core::Stream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__WriteSerializer_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__WriteSerializer_O");
	Call_exposePython_core__WriteSerializer_O(_lisp); // base(s): set(['core::Serializer_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ArrayObjects_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ArrayObjects_O");
	Call_exposePython_core__ArrayObjects_O(_lisp); // base(s): set(['core::Array_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__BuiltInClass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__BuiltInClass_O");
	Call_exposePython_core__BuiltInClass_O(_lisp); // base(s): set(['core::MetaClass_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__CandoException_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__CandoException_O");
	Call_exposePython_core__CandoException_O(_lisp); // base(s): set(['core::ConditionWithStackTrace_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__CandoFormatSaveArchive_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__CandoFormatSaveArchive_O");
	Call_exposePython_core__CandoFormatSaveArchive_O(_lisp); // base(s): set(['core::SaveArchive_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__CompileTimeEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__CompileTimeEnvironment_O");
	Call_exposePython_core__CompileTimeEnvironment_O(_lisp); // base(s): set(['core::LexicalEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Condition_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Condition_O");
	Call_exposePython_core__Condition_O(_lisp); // base(s): set(['core::ConditionWithStackTrace_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FDInStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FDInStream_O");
	Call_exposePython_core__FDInStream_O(_lisp); // base(s): set(['core::FDStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FDOutStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FDOutStream_O");
	Call_exposePython_core__FDOutStream_O(_lisp); // base(s): set(['core::FDStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FileInCompressedStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FileInCompressedStream_O");
	Call_exposePython_core__FileInCompressedStream_O(_lisp); // base(s): set(['core::FileStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FileInStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FileInStream_O");
	Call_exposePython_core__FileInStream_O(_lisp); // base(s): set(['core::FileStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FileOutCompressedStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FileOutCompressedStream_O");
	Call_exposePython_core__FileOutCompressedStream_O(_lisp); // base(s): set(['core::FileStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FileOutStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FileOutStream_O");
	Call_exposePython_core__FileOutStream_O(_lisp); // base(s): set(['core::FileStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Float_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Float_O");
	Call_exposePython_core__Float_O(_lisp); // base(s): set(['core::Real_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ForwardReferencedClass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ForwardReferencedClass_O");
	Call_exposePython_core__ForwardReferencedClass_O(_lisp); // base(s): set(['core::MetaClass_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FunctionFrame_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FunctionFrame_O");
	Call_exposePython_core__FunctionFrame_O(_lisp); // base(s): set(['core::ActivationFrame_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__List_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__List_O");
	Call_exposePython_core__List_O(_lisp); // base(s): set(['core::Sequence_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Rational_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Rational_O");
	Call_exposePython_core__Rational_O(_lisp); // base(s): set(['core::Real_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__RuntimeVisibleEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__RuntimeVisibleEnvironment_O");
	Call_exposePython_core__RuntimeVisibleEnvironment_O(_lisp); // base(s): set(['core::LexicalEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SingleDispatchGenericFunction_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SingleDispatchGenericFunction_O");
	Call_exposePython_core__SingleDispatchGenericFunction_O(_lisp); // base(s): set(['core::BuiltIn_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StandardChar_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StandardChar_O");
	Call_exposePython_core__StandardChar_O(_lisp); // base(s): set(['core::BaseChar_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StandardClass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StandardClass_O");
	Call_exposePython_core__StandardClass_O(_lisp); // base(s): set(['core::MetaClass_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StandardGenericFunction_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StandardGenericFunction_O");
	Call_exposePython_core__StandardGenericFunction_O(_lisp); // base(s): set(['core::GenericFunction_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StringInStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StringInStream_O");
	Call_exposePython_core__StringInStream_O(_lisp); // base(s): set(['core::StringStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StringOutStream_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StringOutStream_O");
	Call_exposePython_core__StringOutStream_O(_lisp); // base(s): set(['core::StringStream_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__StructureClass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__StructureClass_O");
	Call_exposePython_core__StructureClass_O(_lisp); // base(s): set(['core::MetaClass_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ValueFrame_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ValueFrame_O");
	Call_exposePython_core__ValueFrame_O(_lisp); // base(s): set(['core::ActivationFrame_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Vector_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Vector_O");
	Call_exposePython_core__Vector_O(_lisp); // base(s): set(['core::Array_O', 'core::Sequence_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__XmlLoadArchive_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__XmlLoadArchive_O");
	Call_exposePython_core__XmlLoadArchive_O(_lisp); // base(s): set(['core::LoadArchive_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__XmlSaveArchive_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__XmlSaveArchive_O");
	Call_exposePython_core__XmlSaveArchive_O(_lisp); // base(s): set(['core::SaveArchive_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ArchiveError_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ArchiveError_O");
	Call_exposePython_core__ArchiveError_O(_lisp); // base(s): set(['core::CandoException_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__BitVector_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__BitVector_O");
	Call_exposePython_core__BitVector_O(_lisp); // base(s): set(['core::Vector_O'])
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
#ifdef Use_CorePkg
extern void Call_exposePython_core__Cons_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Cons_O");
	Call_exposePython_core__Cons_O(_lisp); // base(s): set(['core::List_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ContentException_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ContentException_O");
	Call_exposePython_core__ContentException_O(_lisp); // base(s): set(['core::CandoException_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__DoubleFloat_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__DoubleFloat_O");
	Call_exposePython_core__DoubleFloat_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__FileNotFoundException_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__FileNotFoundException_O");
	Call_exposePython_core__FileNotFoundException_O(_lisp); // base(s): set(['core::CandoException_O'])
}
#endif // ifdef Use_CorePkg
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
extern void Call_exposePython_core__Integer_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Integer_O");
	Call_exposePython_core__Integer_O(_lisp); // base(s): set(['core::Rational_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__LispError_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__LispError_O");
	Call_exposePython_core__LispError_O(_lisp); // base(s): set(['core::CandoException_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__LongFloat_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__LongFloat_O");
	Call_exposePython_core__LongFloat_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__MacroletEnvironment_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__MacroletEnvironment_O");
	Call_exposePython_core__MacroletEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__MathException_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__MathException_O");
	Call_exposePython_core__MathException_O(_lisp); // base(s): set(['core::CandoException_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Null_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Null_O");
	Call_exposePython_core__Null_O(_lisp); // base(s): set(['core::Symbol_O', 'core::List_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Ratio_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Ratio_O");
	Call_exposePython_core__Ratio_O(_lisp); // base(s): set(['core::Rational_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SerializeError_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SerializeError_O");
	Call_exposePython_core__SerializeError_O(_lisp); // base(s): set(['core::CandoException_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__ShortFloat_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__ShortFloat_O");
	Call_exposePython_core__ShortFloat_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SingleFloat_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SingleFloat_O");
	Call_exposePython_core__SingleFloat_O(_lisp); // base(s): set(['core::Float_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__String_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__String_O");
	Call_exposePython_core__String_O(_lisp); // base(s): set(['core::Vector_O'])
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
	Call_exposePython_core__TagbodyEnvironment_O(_lisp); // base(s): set(['core::CompileTimeEnvironment_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__TypeException_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__TypeException_O");
	Call_exposePython_core__TypeException_O(_lisp); // base(s): set(['core::CandoException_O'])
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
extern void Call_exposePython_core__VectorObjects_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__VectorObjects_O");
	Call_exposePython_core__VectorObjects_O(_lisp); // base(s): set(['core::Vector_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__serious_condition_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__serious_condition_O");
	Call_exposePython_core__serious_condition_O(_lisp); // base(s): set(['core::Condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__simple_condition_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__simple_condition_O");
	Call_exposePython_core__simple_condition_O(_lisp); // base(s): set(['core::Condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__warning_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__warning_O");
	Call_exposePython_core__warning_O(_lisp); // base(s): set(['core::Condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Bignum_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Bignum_O");
	Call_exposePython_core__Bignum_O(_lisp); // base(s): set(['core::Integer_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__CompiledBody_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__CompiledBody_O");
	Call_exposePython_core__CompiledBody_O(_lisp); // base(s): set(['core::Cons_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Error_O");
	Call_exposePython_core__Error_O(_lisp); // base(s): set(['core::serious_condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Fixnum_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Fixnum_O");
	Call_exposePython_core__Fixnum_O(_lisp); // base(s): set(['core::Integer_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SimpleBitVector_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SimpleBitVector_O");
	Call_exposePython_core__SimpleBitVector_O(_lisp); // base(s): set(['core::BitVector_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__SourceCodeCons_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__SourceCodeCons_O");
	Call_exposePython_core__SourceCodeCons_O(_lisp); // base(s): set(['core::Cons_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__Str_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__Str_O");
	Call_exposePython_core__Str_O(_lisp); // base(s): set(['core::String_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__VectorObjectsWithFillPtr_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__VectorObjectsWithFillPtr_O");
	Call_exposePython_core__VectorObjectsWithFillPtr_O(_lisp); // base(s): set(['core::VectorObjects_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__simple_warning_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__simple_warning_O");
	Call_exposePython_core__simple_warning_O(_lisp); // base(s): set(['core::warning_O', 'core::simple_condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__storage_condition_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__storage_condition_O");
	Call_exposePython_core__storage_condition_O(_lisp); // base(s): set(['core::serious_condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__style_warning_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__style_warning_O");
	Call_exposePython_core__style_warning_O(_lisp); // base(s): set(['core::warning_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__LoadTimeValues_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__LoadTimeValues_O");
	Call_exposePython_core__LoadTimeValues_O(_lisp); // base(s): set(['core::VectorObjectsWithFillPtr_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__arithmetic_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__arithmetic_error_O");
	Call_exposePython_core__arithmetic_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__cell_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__cell_error_O");
	Call_exposePython_core__cell_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__control_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__control_error_O");
	Call_exposePython_core__control_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__file_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__file_error_O");
	Call_exposePython_core__file_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__package_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__package_error_O");
	Call_exposePython_core__package_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__parse_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__parse_error_O");
	Call_exposePython_core__parse_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__print_not_readable_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__print_not_readable_O");
	Call_exposePython_core__print_not_readable_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__program_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__program_error_O");
	Call_exposePython_core__program_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__simple_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__simple_error_O");
	Call_exposePython_core__simple_error_O(_lisp); // base(s): set(['core::Error_O', 'core::simple_condition_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__stream_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__stream_error_O");
	Call_exposePython_core__stream_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__type_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__type_error_O");
	Call_exposePython_core__type_error_O(_lisp); // base(s): set(['core::Error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__argument_mismatch_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__argument_mismatch_error_O");
	Call_exposePython_core__argument_mismatch_error_O(_lisp); // base(s): set(['core::program_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__division_by_zero_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__division_by_zero_O");
	Call_exposePython_core__division_by_zero_O(_lisp); // base(s): set(['core::arithmetic_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__end_of_file_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__end_of_file_O");
	Call_exposePython_core__end_of_file_O(_lisp); // base(s): set(['core::stream_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__floating_point_inexact_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__floating_point_inexact_O");
	Call_exposePython_core__floating_point_inexact_O(_lisp); // base(s): set(['core::arithmetic_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__floating_point_invalid_operation_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__floating_point_invalid_operation_O");
	Call_exposePython_core__floating_point_invalid_operation_O(_lisp); // base(s): set(['core::arithmetic_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__floating_point_overflow_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__floating_point_overflow_O");
	Call_exposePython_core__floating_point_overflow_O(_lisp); // base(s): set(['core::arithmetic_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__floating_point_underflow_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__floating_point_underflow_O");
	Call_exposePython_core__floating_point_underflow_O(_lisp); // base(s): set(['core::arithmetic_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__reader_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__reader_error_O");
	Call_exposePython_core__reader_error_O(_lisp); // base(s): set(['core::stream_error_O', 'core::parse_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__simple_type_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__simple_type_error_O");
	Call_exposePython_core__simple_type_error_O(_lisp); // base(s): set(['core::type_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__unbound_slot_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__unbound_slot_O");
	Call_exposePython_core__unbound_slot_O(_lisp); // base(s): set(['core::cell_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__unbound_variable_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__unbound_variable_O");
	Call_exposePython_core__unbound_variable_O(_lisp); // base(s): set(['core::cell_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__undefined_function_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__undefined_function_O");
	Call_exposePython_core__undefined_function_O(_lisp); // base(s): set(['core::cell_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__invalid_keyword_arguments_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__invalid_keyword_arguments_error_O");
	Call_exposePython_core__invalid_keyword_arguments_error_O(_lisp); // base(s): set(['core::argument_mismatch_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__odd_number_of_keyword_arguments_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__odd_number_of_keyword_arguments_error_O");
	Call_exposePython_core__odd_number_of_keyword_arguments_error_O(_lisp); // base(s): set(['core::argument_mismatch_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__single_dispatch_arguments_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__single_dispatch_arguments_error_O");
	Call_exposePython_core__single_dispatch_arguments_error_O(_lisp); // base(s): set(['core::argument_mismatch_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__too_few_arguments_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__too_few_arguments_error_O");
	Call_exposePython_core__too_few_arguments_error_O(_lisp); // base(s): set(['core::argument_mismatch_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__too_many_arguments_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__too_many_arguments_error_O");
	Call_exposePython_core__too_many_arguments_error_O(_lisp); // base(s): set(['core::argument_mismatch_error_O'])
}
#endif // ifdef Use_CorePkg
#ifdef Use_CorePkg
extern void Call_exposePython_core__unrecognized_keyword_arguments_error_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: core__unrecognized_keyword_arguments_error_O");
	Call_exposePython_core__unrecognized_keyword_arguments_error_O(_lisp); // base(s): set(['core::argument_mismatch_error_O'])
}
#endif // ifdef Use_CorePkg
#endif // EXPOSE_TO_PYTHON
#undef EXPOSE_TO_PYTHON
#undef ALL_STAGES
