#include <chrono> // clock for unwind timing
#include <clasp/core/unwind.h>
#include <clasp/core/exceptions.h> // UNREACHABLE, unwind, errors
#include <clasp/core/evaluator.h>  // eval::funcall
#include <clasp/core/array.h>      // simple vector stuff
// for multiple value saving stuff
#include <clasp/gctools/multiple_value_pointers.h>

namespace core {

#ifdef DEBUG_DYN_ENV_STACK
bool global_debug_dyn_env_stack = false;
#endif

DynEnv_O::SearchStatus sjlj_unwind_search(LexDynEnv_sp dest) {
  DynEnv_O::SearchStatus status = DynEnv_O::Proceed;
  for (T_sp iter = my_thread->dynEnvStackGet();;) {
    if (iter.nilp())
      return DynEnv_O::OutOfExtent;
    else {
      Cons_sp c = gc::As_unsafe<Cons_sp>(iter);
      ASSERT(gc::IsA<DynEnv_sp>(ENSURE_VALID_OBJECT(CONS_CAR(c))));
      DynEnv_sp diter = gc::As_unsafe<DynEnv_sp>(ENSURE_VALID_OBJECT(CONS_CAR(c)));
      if (diter == dest)
        break;
      auto dstatus = diter->search();
      /* If we get a FallBack, we don't want to return that
       * immediately, since if we're out of extent that's more
       * important. */
      if (dstatus != DynEnv_O::Continue)
        status = dstatus;
      iter = CONS_CDR(iter);
    }
  }
#ifdef UNWIND_INVALIDATE_STRICT
  if (!(dest->valid))
    return DynEnv_O::Abandoned;
#endif
  return status;
}

DynEnv_O::SearchStatus sjlj_throw_search(T_sp tag, CatchDynEnv_sp& dest) {
  DynEnv_O::SearchStatus status = DynEnv_O::Proceed;
  for (T_sp iter = my_thread->dynEnvStackGet();;) {
    if (iter.nilp())
      return DynEnv_O::OutOfExtent;
    else {
      Cons_sp c = gc::As_unsafe<Cons_sp>(iter);
      ASSERT(gc::IsA<DynEnv_sp>(CONS_CAR(c)));
      DynEnv_sp diter = gc::As_unsafe<DynEnv_sp>(CONS_CAR(c));
      if (gc::IsA<CatchDynEnv_sp>(diter)) {
        CatchDynEnv_sp catc = gc::As_unsafe<CatchDynEnv_sp>(diter);
        if (tag == catc->tag) {
          dest = catc;
          return status;
        }
      }
      auto nstatus = diter->search();
      if (nstatus != DynEnv_O::Continue)
        status = nstatus;
      iter = CONS_CDR(iter);
    }
  }
  UNREACHABLE();
}

#ifdef UNWIND_INVALIDATE_STRICT
void sjlj_unwind_invalidate(DestDynEnv_sp dest) {
  for (T_sp iter = my_thread->dynEnvStackGet(); iter.notnilp() && (iter->car() != dest); iter = CONS_CDR(iter))
    iter->invalidate();
}
#endif

[[noreturn]] void sjlj_unwind_proceed(DestDynEnv_sp dest, size_t index) {
  ThreadLocalState* thread = my_thread;
  T_sp here = thread->dynEnvStackGet();
  thread->_UnwindDest = dest;
  thread->_UnwindDestIndex = index;
  // We must have already searched, so we know this is a dynenv
  // and not the NIL sentinel.
  for (T_sp iter = here;; iter = CONS_CDR(iter)) {
    ASSERT(iter.consp());
    ASSERT(gc::IsA<DynEnv_sp>(CONS_CAR(iter)));
    DynEnv_sp diter = gc::As_unsafe<DynEnv_sp>(CONS_CAR(iter));
    if (diter == dest) {
      // Now actually jump. We need to replace the dynEnvStackGet(), but what we
      // switch it to depends on whether it's a tagbody or block.
      if (dest->unwound_dynenv_p())
        thread->dynEnvStackSet(CONS_CDR(iter));
      else
        thread->dynEnvStackSet(iter);
      _longjmp(*(dest->target), index);
    } else {
      thread->dynEnvStackSet(iter);
      diter->proceed();
    }
  }
}

[[noreturn]] void UnwindProtectDynEnv_O::proceed() {
  my_thread->dynEnvStackSet(CONS_CDR(my_thread->dynEnvStackGet()));
  _longjmp(*(this->target), 1); // 1 irrelevant
}

void BindingDynEnv_O::proceed() { this->cell->unbind(this->old); }

[[noreturn]] void sjlj_unwind(LexDynEnv_sp dest, size_t index) {
  VM_INC_UNWIND_COUNTER(my_thread->_VM);
  switch (sjlj_unwind_search(dest)) {
  case DynEnv_O::OutOfExtent:
    NO_INITIALIZERS_ERROR(core::_sym_outOfExtentUnwind);
  case DynEnv_O::FallBack:
#ifdef UNWIND_INVALIDATE_STRICT
    sjlj_unwind_invalidate(dest);
#endif
    my_thread->_unwinds++;
    my_thread_low_level->_start_unwind = std::chrono::high_resolution_clock::now();
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
  default:
    UNREACHABLE();
  }
}

[[noreturn]] void sjlj_throw(T_sp tag) {
  VM_INC_THROW_COUNTER(my_thread->_VM);
  CatchDynEnv_sp dest;
  switch (sjlj_throw_search(tag, dest)) {
  case DynEnv_O::OutOfExtent:
    ERROR(core::_sym_noCatchTag, core::lisp_createList(kw::_sym_tag, tag));
  case DynEnv_O::FallBack:
#ifdef UNWIND_INVALIDATE_STRICT
    sjlj_unwind_invalidate(dest);
#endif
    my_thread->_unwinds++;
    my_thread_low_level->_start_unwind = std::chrono::high_resolution_clock::now();
    throw CatchThrow(tag);
#ifdef UNWIND_INVALIDATE_STRICT
  case DynEnv_O::Abandoned:
    NO_INITIALIZERS_ERROR(core::_sym_abandonedUnwind);
#endif
  case DynEnv_O::Proceed:
#ifdef UNWIND_INVALIDATE_STRICT
    sjlj_unwind_invalidate(dest);
#endif
    sjlj_unwind_proceed(dest, 1); // 1 irrelevant
  default:
    UNREACHABLE();
  }
}

// Runtime interface.

CL_UNWIND_COOP(true);
CL_DEFUN T_mv core__sjlj_call_with_current_dynenv(Function_sp function) {
  return eval::funcall(function, my_thread->dynEnvStackGet());
}

// Check the search status for a single dynenv.
CL_UNWIND_COOP(true);
CL_DEFUN int core__sjlj_dynenv_search_one(T_sp tde) {
  ASSERT(gc::IsA<DynEnv_sp>(tde));
  DynEnv_sp de = gc::As_unsafe<DynEnv_sp>(tde);
  switch (de->search()) {
  case DynEnv_O::Continue:
    return 0;
  case DynEnv_O::Proceed:
    return 1;
  case DynEnv_O::OutOfExtent:
    return 2;
  case DynEnv_O::Abandoned:
    return 3;
  case DynEnv_O::FallBack:
    return 4;
  default:
    UNREACHABLE();
  }
}

/* Check the search status for a given exit (i.e. determine whether
 * we need to fall back to C++ unwinder, or it's out of extent, or what) */
CL_UNWIND_COOP(true);
CL_DEFUN int core__sjlj_dynenv_search(LexDynEnv_sp dest) {
  switch (sjlj_unwind_search(dest)) {
  case DynEnv_O::Proceed:
    return 1;
  case DynEnv_O::OutOfExtent:
    return 2;
  case DynEnv_O::Abandoned:
    return 3;
  case DynEnv_O::FallBack:
    return 4;
  default:
    UNREACHABLE();
  }
}

CL_UNWIND_COOP(true);
CL_DEFUN T_mv core__sjlj_call_with_unknown_dynenv(Function_sp thunk) {
  gctools::StackAllocate<UnknownDynEnv_O> sa_ude;
  gctools::StackAllocate<Cons_O> sa_ec(sa_ude.asSmartPtr(), my_thread->dynEnvStackGet());
  DynEnvPusher dep(my_thread, sa_ec.asSmartPtr());
  return eval::funcall(thunk);
}

CL_UNWIND_COOP(true);
CL_DEFUN T_mv core__sjlj_call_with_variable_bound(Symbol_sp sym, T_sp val, Function_sp thunk) {
  return call_with_variable_bound(sym, val, [&]() { return eval::funcall(thunk); });
}

CL_UNWIND_COOP(true);
CL_DEFUN T_mv core__sjlj_call_with_escape_continuation(Function_sp function) {
  return call_with_escape([&](BlockDynEnv_sp env) { return eval::funcall(function, env); });
}

CL_UNWIND_COOP(true);
[[noreturn]] CL_DEFUN void core__sjlj_escape(BlockDynEnv_sp escape, Function_sp thunk) {
  // Call the thunk to get the return values.
  T_mv result = eval::funcall(thunk);
  // Save the return values.
  core::MultipleValues& mv = core::lisp_multipleValues();
  mv.saveToMultipleValue0(result);
  // checkme result.saveToMultipleValue0();
  // Go. Index is ignored for blocks.
  sjlj_unwind(escape, 1);
}

CL_UNWIND_COOP(true);
CL_DEFUN T_sp core__sjlj_ftagbody(SimpleVector_sp functions) {
  call_with_tagbody(
      [&](TagbodyDynEnv_sp tagbody, size_t index) { eval::funcall(gc::As<Function_sp>(functions->vref(index)), tagbody); });
  return nil<T_O>();
}

CL_UNWIND_COOP(true);
[[noreturn]] CL_DEFUN void core__sjlj_go(TagbodyDynEnv_sp tagbody, size_t index) { sjlj_unwind(tagbody, index); }

CL_UNWIND_COOP(true);
CL_DEFUN T_mv core__sjlj_funwind_protect(Function_sp thunk, Function_sp cleanup) {
  return funwind_protect([&]() { return eval::funcall(thunk); }, [&]() { eval::funcall(cleanup); });
}

CL_UNWIND_COOP(true);
CL_DEFUN T_mv core__sjlj_catch_function(T_sp tag, Function_sp thunk) {
  return call_with_catch(tag, [&]() { return eval::funcall(thunk); });
}

CL_UNWIND_COOP(true);
[[noreturn]] CL_DEFUN void core__sjlj_throw(T_sp tag, Function_sp thunk) {
  T_mv result = eval::funcall(thunk);
  core::MultipleValues& mv = core::lisp_multipleValues();
  mv.saveToMultipleValue0(result);
  // checkme result.saveToMultipleValue0();
  sjlj_throw(tag);
}

}; // namespace core
