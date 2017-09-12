
// Nothing for now

#include <clasp/core/core.h>
#include <clasp/core/object.h>
#include <clasp/core/array.h>
#include <clasp/core/instance.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/llvmo/intrinsics.h>

#if 0 // DEBUGGING
#define NO_UNWIND_BEGIN_BUILTINS() NO_UNWIND_BEGIN()
#define NO_UNWIND_END_BUILTINS() NO_UNWIND_END()
#define ENSURE_VALID_OBJECT_BUILTINS(x) ENSURE_VALID_OBJECT(x)
#else
#define NO_UNWIND_BEGIN_BUILTINS()
#define NO_UNWIND_END_BUILTINS()
#define ENSURE_VALID_OBJECT_BUILTINS(x) x
#endif

#define LINKAGE __attribute__ ((visibility ("default")))

#define BUILTIN_ATTRIBUTES __attribute__((always_inline))

extern "C" {

BUILTIN_ATTRIBUTES int foobar(int x) {return x*x*x*x;}

};

extern "C" {

BUILTIN_ATTRIBUTES void newTmv(core::T_mv *sharedP)
{
  new (sharedP) core::T_mv();
}

BUILTIN_ATTRIBUTES void cc_rewind_va_list(core::T_O* tagged_closure, va_list va_args, size_t* nargsP, void** register_save_areaP)
{NO_UNWIND_BEGIN_BUILTINS();
#if 0
  if (core::debug_InvocationHistoryFrame==3) {
    printf("%s:%d cc_rewind_va_list     va_args=%p     nargsP = %p      register_save_areaP = %p\n", __FILE__, __LINE__, va_args, nargsP, register_save_areaP );
  }
#endif
  LCC_REWIND_VA_LIST(va_args,register_save_areaP);
  *nargsP = (uintptr_t)register_save_areaP[1];
  NO_UNWIND_END_BUILTINS();
}

BUILTIN_ATTRIBUTES
core::T_O *va_symbolFunction(core::T_sp *symP) {
  core::Symbol_sp sym((gctools::Tagged)symP->raw_());
  unlikely_if (!sym->fboundp()) intrinsic_error(llvmo::noFunctionBoundToSymbol, sym);
  core::Function_sp func((gc::Tagged)(sym)->_Function.theObject);
  return func.raw_();
}


#if 0
BUILTIN_ATTRIBUTES core::T_sp *symbolValueReference(core::T_sp *symbolP)
{
  core::Symbol_sp sym((gctools::Tagged)ENSURE_VALID_OBJECT_BUILTINS(symbolP->raw_()));
  return sym->valueReference();
}
#endif


BUILTIN_ATTRIBUTES core::T_sp *lexicalValueReference(int depth, int index, core::ActivationFrame_sp *frameP)
{
  core::ActivationFrame_sp af = gctools::reinterpret_cast_smart_ptr<core::ActivationFrame_O>(*frameP);
  return const_cast<core::T_sp *>(&core::value_frame_lookup_reference(af, depth, index));
}

BUILTIN_ATTRIBUTES void sp_lexicalValueRead(core::T_sp *resultP, int depth, int index, core::ActivationFrame_sp *renvP)
{
  (*resultP) = core::value_frame_lookup_reference(*renvP,depth,index);
}
BUILTIN_ATTRIBUTES void mv_lexicalValueRead(core::T_mv *resultP, int depth, int index, core::ActivationFrame_sp *renvP)
{
  (*resultP) = core::value_frame_lookup_reference(*renvP,depth,index);
}


// The following two are only valid for non-simple arrays. Be careful!
BUILTIN_ATTRIBUTES core::T_O* cc_realArrayDisplacement(core::T_O* tarray) {
  core::MDArray_O* array = reinterpret_cast<core::MDArray_O*>(gctools::untag_general<core::T_O*>(tarray));
  return array->realDisplacedTo().raw_();
}
BUILTIN_ATTRIBUTES size_t cc_realArrayDisplacedIndexOffset(core::T_O* tarray) {
  core::MDArray_O* array = reinterpret_cast<core::MDArray_O*>(gctools::untag_general<core::T_O*>(tarray));
  return array->displacedIndexOffset();
}

BUILTIN_ATTRIBUTES size_t cc_arrayTotalSize(core::T_O* tarray) {
  core::MDArray_O* array = reinterpret_cast<core::MDArray_O*>(gctools::untag_general<core::T_O*>(tarray));
  return array->arrayTotalSize();
}

BUILTIN_ATTRIBUTES size_t cc_arrayRank(core::T_O* tarray) {
  core::MDArray_O* array = reinterpret_cast<core::MDArray_O*>(gctools::untag_general<core::T_O*>(tarray));
  return array->rank();
}

BUILTIN_ATTRIBUTES size_t cc_arrayDimension(core::T_O* tarray, size_t axis) {
  core::MDArray_O* array = reinterpret_cast<core::MDArray_O*>(gctools::untag_general<core::T_O*>(tarray));
  return array->arrayDimension(axis);
}

};

