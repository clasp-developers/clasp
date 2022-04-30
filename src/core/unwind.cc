#include <clasp/core/unwind.h>
#include <clasp/core/exceptions.h> // UNREACHABLE, unwind, errors
#include <clasp/core/evaluator.h> // eval::funcall
// for multiple value saving stuff
#include <clasp/gctools/multiple_value_pointers.h>

namespace core {

DynEnv_O::SearchStatus sjlj_unwind_search(DestDynEnv_sp dest) {
  for (T_sp iter = my_thread->_DynEnv; iter != dest;) {
    if (iter.notnilp()) {
      DynEnv_sp diter = gc::As_unsafe<DynEnv_sp>(iter);
      auto status = diter->search();
      if (status != DynEnv_O::Continue) return status;
      iter = diter->outer;
    } else return DynEnv_O::OutOfExtent;
  }
#ifdef UNWIND_INVALIDATE_STRICT
  if (!(dest->valid)) return DynEnv_O::Abandoned;
#endif
  return DynEnv_O::Proceed;
}

#ifdef UNWIND_INVALIDATE_STRICT
void sjlj_unwind_invalidate(DestDynEnv_sp dest) {
  for (T_sp iter = my_thread->_DynEnv;
       (iter != dest) && iter.notnilp();
       iter = gc::As_unsafe<DynEnv_sp>(iter)->outer)
    iter->invalidate();
}
#endif

[[noreturn]] void sjlj_unwind_proceed(DestDynEnv_sp dest, size_t index) {
  ThreadLocalState* thread = my_thread;
  T_sp here = thread->_DynEnv;
  thread->_UnwindDest = dest;
  thread->_UnwindDestIndex = index;
  for (T_sp iter = here; iter != dest;) {
    // We must have already searched, so we know this is a dynenv
    // and not the NIL sentinel.
    DynEnv_sp diter = gc::As_unsafe<DynEnv_sp>(iter);
    diter->proceed(dest, index);
    iter = diter->outer;
  }
  // Now actually jump. We need to replace the _DynEnv, but what we
  // switch it to depends on whether it's a tagbody or block.
  thread->_DynEnv = dest->unwound_dynenv();
  longjmp(*(dest->target), index);
}

[[noreturn]] void UnwindProtectDynEnv_O::proceed(DestDynEnv_sp dest,
                                                 size_t index) {
  my_thread->_DynEnv = this->outer;
  longjmp(*(this->target) , 1); // 1 irrelevant
}

void BindingDynEnv_O::proceed(DestDynEnv_sp dest, size_t index) {
  this->sym->set_threadLocalSymbolValue(this->old);
}

[[noreturn]] void sjlj_unwind(DestDynEnv_sp dest, size_t index) {
  switch (sjlj_unwind_search(dest)) {
  case DynEnv_O::OutOfExtent:
      NO_INITIALIZERS_ERROR(core::_sym_outOfExtentUnwind);
  case DynEnv_O::FallBack:
#ifdef UNWIND_INVALIDATE_STRICT
      sjlj_unwind_invalidate(dest);
#endif
      throw Unwind(dest->frame, index);
#ifdef UNWIND_INVALIDATE_STRICT
  case DynEnv_O::Abandoned:
      NO_INITIALIZERS_ERROR(core::_sym_abandonedUnwind);
#endif
  case DynEnv_O::Proceed:
#ifdef UNWIND_INVALIDATE_STRICT
      sjlj_unwind_invalidate(dest);
#endif
      sjlj_unwind_proceed(dest, index);
  default: UNREACHABLE();
  }
}

// Runtime interface.

CL_UNWIND_COOP(true);
CL_DEFUN T_mv core__sjlj_call_with_current_dynenv(Function_sp function) {
  return eval::funcall(function, my_thread->_DynEnv);
}

// Returns the nth dynenv, where 0 is the current, 1 is its parent, etc.
// If there is no dynenv that high up, returns NIL.
// This is exposed instead of the parent field directly so that we hide
// the details of how the dynenv stack is stored (i.e. in the dynenv or not).
CL_UNWIND_COOP(true);
CL_DEFUN T_mv core__sjlj_call_with_nth_dynenv(Function_sp function,
                                              size_t index) {
  T_sp iter = my_thread->_DynEnv;
  while (true) {
    if ((index == 0) || iter.nilp())
      return eval::funcall(iter);
    else {
      iter = gc::As_unsafe<DynEnv_sp>(iter)->outer;
      --index;
    }
  }
}

// Check the search status for a single dynenv.
CL_UNWIND_COOP(true);
CL_DEFUN int core__sjlj_dynenv_search_one(T_sp tde) {
  if (tde.nilp()) return 2;
  else {
    DynEnv_sp de = gc::As<DynEnv_sp>(tde);
    switch (de->search()) {
    case DynEnv_O::Continue: return 0;
    case DynEnv_O::Proceed: return 1;
    case DynEnv_O::OutOfExtent: return 2;
    case DynEnv_O::Abandoned: return 3;
    case DynEnv_O::FallBack: return 4;
    default: UNREACHABLE();
    }
  }
}

/* Check the search status for a given exit (i.e. determine whether
 * we need to fall back to C++ unwinder, or it's out of extent, or what) */
CL_UNWIND_COOP(true);
CL_DEFUN int core__sjlj_dynenv_search(DestDynEnv_sp dest) {
  switch (sjlj_unwind_search(dest)) {
  case DynEnv_O::Proceed: return 1;
  case DynEnv_O::OutOfExtent: return 2;
  case DynEnv_O::Abandoned: return 3;
  case DynEnv_O::FallBack: return 4;
  default: UNREACHABLE();
  }
}

CL_UNWIND_COOP(true);
CL_DEFUN T_mv core__sjlj_call_with_unknown_dynenv(Function_sp thunk) {
  gctools::StackAllocate<UnknownDynEnv_O> sa_ude(my_thread->_DynEnv);
  DynEnvPusher dep(my_thread, sa_ude.asSmartPtr());
  return eval::funcall(thunk);
}

struct SymbolValuePusher {
  Symbol_sp msym;
  T_sp old;
  SymbolValuePusher(Symbol_sp sym, T_sp val) {
    msym = sym;
    old = sym->threadLocalSymbolValue();
    msym->set_threadLocalSymbolValue(val);
  }
  ~SymbolValuePusher() { msym->set_threadLocalSymbolValue(old); }
};

CL_UNWIND_COOP(true);
CL_DEFUN T_mv core__sjlj_call_with_variable_bound(Symbol_sp sym, T_sp val,
                                                  Function_sp thunk) {
  T_sp old = sym->threadLocalSymbolValue();
  SymbolValuePusher svp(sym, val);
  gctools::StackAllocate<BindingDynEnv_O> sa_bde(my_thread->_DynEnv, sym, old);
  DynEnvPusher dep(my_thread, sa_bde.asSmartPtr());
  return eval::funcall(thunk);
}

CL_UNWIND_COOP(true);
CL_DEFUN T_mv core__sjlj_call_with_escape_continuation(Function_sp function) {
  return call_with_escape([&](BlockDynEnv_sp env){return eval::funcall(function, env);});
}

CL_UNWIND_COOP(true);
[[noreturn]] CL_DEFUN void core__sjlj_escape(BlockDynEnv_sp escape,
                                             Function_sp thunk) {
  // Call the thunk to get the return values.
  T_mv result = eval::funcall(thunk);
  // Save the return values.
  result.saveToMultipleValue0();
  // Go. Index is ignored for blocks.
  sjlj_unwind(escape, 1);
}

CL_UNWIND_COOP(true);
CL_DEFUN T_mv core__sjlj_funwind_protect(Function_sp thunk,
                                         Function_sp cleanup) {
  return funwind_protect([&]() { return eval::funcall(thunk); },
                         [&]() { eval::funcall(cleanup); });
}

}; // namespace core
