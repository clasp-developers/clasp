#pragma once

#include <csetjmp>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/symbol.h>
#include <clasp/core/arguments.h> // DynamicScopeManager

namespace core {

#ifdef DEBUG_DYN_ENV_STACK
extern bool global_debug_dyn_env_stack;
#endif

FORWARD(DynEnv);
FORWARD(DestDynEnv);

class DynEnv_O : public General_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, DynEnv_O, "DynEnv", General_O);

public:
  typedef enum { Continue, Proceed, OutOfExtent, Abandoned, FallBack } SearchStatus;

public:
  DynEnv_O(){};
  virtual ~DynEnv_O(){};

public:
  /* Return information about this dynamic environment to the search
   * phase. Return values mean the following:
   * - Continue: Nothing important needs to be noted.
   * - OutOfExtent: The destination is not actually on the stack.
   * - Abandoned: See CLHS 5.2: We are in the middle of an outer
       unwinding operation, and the destination of this inner
       unwinding has been invalidated by the outer.
   * - FallBack: C++ code on the stack has unknown destructors or
       catch blocks, so the C++ unwinder must be used.
   * Proceed may not be returned by this function. It can only be
   * returned from sjlj_unwind_search to indicate we can proceed
   * with this unwinder (as opposed to the fallback C++ unwinder). */
  virtual SearchStatus search() const = 0;
#ifdef UNWIND_INVALIDATE_STRICT
  /* Mark this dynamic environment as abandoned per CLHS 5.2,
   * if applicable. */
  virtual void invalidate(){};
#endif
  /* Undo this environment. */
  virtual void proceed(){};
};

/* A dynenv of this class is used to warn our unwinder that there is
 * C++ code with unknown dynamic environment, e.g. because nontrivial
 * destructors need to be run, there are catch blocks, or we simply
 * don't know. */
class UnknownDynEnv_O : public DynEnv_O {
  LISP_CLASS(core, CorePkg, UnknownDynEnv_O, "UnknownDynEnv", DynEnv_O);

public:
  UnknownDynEnv_O() : DynEnv_O(){};
  virtual ~UnknownDynEnv_O(){};

public:
  virtual SearchStatus search() const { return FallBack; };
};

// Abstract class of dynenvs that can be destinations for unwinding.
class DestDynEnv_O : public DynEnv_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, DestDynEnv_O, "DestDynEnv", DynEnv_O);

public:
  jmp_buf* target;
#ifdef UNWIND_INVALIDATE_STRICT
  bool valid = true;
#endif
  DestDynEnv_O() : DynEnv_O(){};
  DestDynEnv_O(jmp_buf* a_target) : DynEnv_O(), target(a_target){};
  virtual ~DestDynEnv_O(){};
  virtual SearchStatus search() const { return Continue; };
  virtual void proceed(){};
#ifdef UNWIND_INVALIDATE_STRICT
  virtual void invalidate() { valid = false; }
#endif
  /* If true, when this dynenv is unwound to the new dynenv should be its parent.
   * Otherwise it is this dynenv. true = BLOCK, CATCH while false = TAGBODY */
  virtual bool unwound_dynenv_p() = 0;
};

// Abstract class of lexical destinations.
FORWARD(LexDynEnv);
class LexDynEnv_O : public DestDynEnv_O {
  LISP_ABSTRACT_CLASS(core, CorePkg, LexDynEnv_O, "LexDynEnv", DestDynEnv_O);

public:
  void* frame; // for fallback
  LexDynEnv_O() : DestDynEnv_O(){};
  LexDynEnv_O(void* a_frame, jmp_buf* target) : DestDynEnv_O(target), frame(a_frame){};
  virtual ~LexDynEnv_O(){};
};

// Dynenv for a CL:BLOCK.
FORWARD(BlockDynEnv);
class BlockDynEnv_O : public LexDynEnv_O {
  LISP_CLASS(core, CorePkg, BlockDynEnv_O, "BlockDynEnv", LexDynEnv_O);

public:
  using LexDynEnv_O::LexDynEnv_O; // inherit constructor
  static BlockDynEnv_sp create(void* frame, jmp_buf* target) { return gctools::GC<BlockDynEnv_O>::allocate(frame, target); }
  virtual ~BlockDynEnv_O(){};
  virtual bool unwound_dynenv_p() { return true; }
};

// Dynenv for a CL:TAGBODY.
FORWARD(TagbodyDynEnv);
class TagbodyDynEnv_O : public LexDynEnv_O {
  LISP_CLASS(core, CorePkg, TagbodyDynEnv_O, "TagbodyDynEnv", LexDynEnv_O);

public:
  using LexDynEnv_O::LexDynEnv_O;
  static TagbodyDynEnv_sp create(void* frame, jmp_buf* target) { return gctools::GC<TagbodyDynEnv_O>::allocate(frame, target); }
  virtual ~TagbodyDynEnv_O(){};
  virtual bool unwound_dynenv_p() { return false; }
};

FORWARD(CatchDynEnv);
class CatchDynEnv_O : public DestDynEnv_O {
  LISP_CLASS(core, CorePkg, CatchDynEnv_O, "CatchDynEnv", DestDynEnv_O);

public:
  T_sp tag;
  CatchDynEnv_O(jmp_buf* target, T_sp a_tag) : DestDynEnv_O(target), tag(a_tag) {}
  virtual ~CatchDynEnv_O(){};

public:
  virtual bool unwound_dynenv_p() { return true; }
};

// Dynenv for a CL:UNWIND-PROTECT cleanup.
class UnwindProtectDynEnv_O : public DynEnv_O {
  LISP_CLASS(core, CorePkg, UnwindProtectDynEnv_O, "UnwindProtectDynEnv", DynEnv_O);

public:
  UnwindProtectDynEnv_O(jmp_buf* a_target) : target(a_target){};
  virtual ~UnwindProtectDynEnv_O(){};

public:
  jmp_buf* target;

public:
  virtual SearchStatus search() const { return Continue; };
  [[noreturn]] virtual void proceed();
};

// Dynenv for a special variable binding.
FORWARD(BindingDynEnv);
class BindingDynEnv_O : public DynEnv_O {
  LISP_CLASS(core, CorePkg, BindingDynEnv_O, "BindingDynEnv", DynEnv_O);

public:
  BindingDynEnv_O(VariableCell_sp a_cell, T_sp a_old) : DynEnv_O(), cell(a_cell), old(a_old){};
  virtual ~BindingDynEnv_O(){};
  VariableCell_sp cell;
  T_sp old;
  virtual SearchStatus search() const { return Continue; };
  virtual void proceed();
};

// RAII helper for augmenting the dynamic environment.
struct DynEnvPusher {
  ThreadLocalState* mthread;
  List_sp stack;
  DynEnvPusher(ThreadLocalState* thread, List_sp newstack) {
    mthread = thread;
    stack = thread->dynEnvStackGet();
    thread->dynEnvStackSet(newstack);
  }
  ~DynEnvPusher() { mthread->dynEnvStackSet(stack); }
};

// Functions

/* Generally, unwinding proceeds in two steps, similar to the Itanium
 * unwinder. The first step checks through the dynamic environment to
 * ensure the destination is valid, and determines whether we can use
 * this faster unwinder or have to fall back to the slow C++ one.
 * The second step, armed with that information, actually jumps. */

/* Check through the dynamic environment for the destination.
 * This has no side effects and does not signal errors. */
DynEnv_O::SearchStatus sjlj_unwind_search(DestDynEnv_sp dest);
/* Actually carry out an unwinding to the given destination.
 * sjlj_unwind_search must have returned Proceed beforehand, or the
 * effects are undefined and bad things may happen.
 * This is intended to be used from unwind-protect cleanups, in order
 * to continue unwinding without needing to search again. */
[[noreturn]] void sjlj_unwind_proceed(DestDynEnv_sp dest, size_t index);
/* Carry out an unwinding to the given destination.
 * Takes care of the search aspect and signaling errors, so this is
 * the main entry point. */
[[noreturn]] void sjlj_unwind(LexDynEnv_sp dest, size_t index);
/* Carry out an unwinding to the given tag. */
[[noreturn]] void sjlj_throw(T_sp tag);
/* Convenience function for use in unwind-protect cleanups. */
[[noreturn]] inline void sjlj_continue_unwinding() {
  sjlj_unwind_proceed(gc::As_unsafe<DestDynEnv_sp>(my_thread->_UnwindDest), my_thread->_UnwindDestIndex);
}

/* Functional unwind protect. Provided as a template function to reduce
 * runtime overhead by essentially inlining. Both thunks should accept no
 * arguments, and protected_thunk should return a T_mv.
 * See core__sjlj_funwind_protect for example usage. */
template <typename Protf, typename Cleanupf> T_mv funwind_protect(Protf&& protected_thunk, Cleanupf&& cleanup_thunk) {
  jmp_buf target;
  T_mv result;
  if (_setjmp(target)) {
    // We have longjmped here. Clean up.
    // Remember to save return values, in case the cleanup thunk
    // messes with them.
    // Also save the unwind dest and index, in case the cleanup thunk
    // itself unwinds.
    T_sp dest = my_thread->_UnwindDest;
    size_t dindex = my_thread->_UnwindDestIndex;
    MultipleValues& multipleValues = lisp_multipleValues();
    size_t nvals = multipleValues.getSize();
    T_O* mv_temp[nvals];
    multipleValues.saveToTemp(nvals, mv_temp);
    cleanup_thunk();
    multipleValues.loadFromTemp(nvals, mv_temp);
    my_thread->_UnwindDestIndex = dindex;
    my_thread->_UnwindDest = dest;
    // Continue unwinding.
    sjlj_continue_unwinding();
  } else {
    // First time through. Set up the cleanup dynenv, then call
    // the thunk, then save its values and call the cleanup.
    try {
      gctools::StackAllocate<UnwindProtectDynEnv_O> sa_upde(&target);
      gctools::StackAllocate<Cons_O> sa_ec(sa_upde.asSmartPtr(), my_thread->dynEnvStackGet());
      DynEnvPusher dep(my_thread, sa_ec.asSmartPtr());
      result = protected_thunk();
    } catch (...) { // C++ unwind. Do the same shit then rethrow
      MultipleValues& multipleValues = core::lisp_multipleValues();
      size_t nvals = multipleValues.getSize();
      T_O* mv_temp[nvals];
      multipleValues.saveToTemp(nvals, mv_temp);
      cleanup_thunk();
      multipleValues.loadFromTemp(nvals, mv_temp);
      throw;
    }
    size_t nvals = result.number_of_values();
    T_O* mv_temp[nvals];
    returnTypeSaveToTemp(nvals, result.raw_(), mv_temp);
    cleanup_thunk();
    return returnTypeLoadFromTemp(nvals, mv_temp);
  }
}

/* Similarly, BLOCK. Note that the use of __builtin_frame_address is a bit
 * hairy here, and nesting this function without intervening frames may cause
 * strange issues. thunkf should accept a BlockDynEnv_sp. */
template <typename Blockf> T_mv call_with_escape(Blockf&& block) {
  jmp_buf target;
  void* frame = __builtin_frame_address(0);
  if (_setjmp(target)) {
    core::MultipleValues& mv = core::lisp_multipleValues();
    T_mv result = mv.readFromMultipleValue0(mv.getSize());
    return result;
    // checkme    return T_mv::createFromValues(); // abnormal return
  } else
    try {
      // the block dynenv is heap allocated, so that functions closing over it
      // can escape, and get a nice out-of-extent if they use it.
      // FIXME? An out-of-extent lexical dynenv may still have
      // pointers into the stack for stack allocated dynenvs which are
      // no longer live. This may present a problem for a precise
      // garbage collector.
      BlockDynEnv_sp env = BlockDynEnv_O::create(frame, &target);
      gctools::StackAllocate<Cons_O> sa_ec(env, my_thread->dynEnvStackGet());
      DynEnvPusher dep(my_thread, sa_ec.asSmartPtr());
      return block(env);
    } catch (Unwind& uw) {
      if (uw.getFrame() == frame) {
        core::MultipleValues& mv = core::lisp_multipleValues();
        T_mv result = mv.readFromMultipleValue0(mv.getSize());
        return result;
        // checkme return T_mv::createFromValues();
      } else
        throw;
    }
}

template <typename Tagbodyf> void call_with_tagbody(Tagbodyf&& tagbody) {
  jmp_buf target;
  void* frame = __builtin_frame_address(0);
  TagbodyDynEnv_sp env = TagbodyDynEnv_O::create(frame, &target);
  gctools::StackAllocate<Cons_O> sa_ec(env, my_thread->dynEnvStackGet());
  DynEnvPusher dep(my_thread, sa_ec.asSmartPtr());
  /* Per the standard, we can't store the result of setjmp in a variable or
   * anything. So we kind of fake it via the dest index we set ourselves. */
  size_t index = 0;
  if (_setjmp(target))
    index = my_thread->_UnwindDestIndex;
again:
  try {
    tagbody(env, index);
  } catch (Unwind& uw) {
    if (uw.getFrame() == frame) {
      // The thrower may not be cooperative, so reset the dynenv.
      // (DynEnvPusher takes care of this when we actually escape.)
      my_thread->dynEnvStackGet() = sa_ec.asSmartPtr();
      index = uw.index();
      goto again;
    } else
      throw;
  }
}

template <typename Catchf> T_mv call_with_catch(T_sp tag, Catchf&& cf) {
  jmp_buf target;
  if (_setjmp(target)) {
    core::MultipleValues& mv = core::lisp_multipleValues();
    T_mv result = mv.readFromMultipleValue0(mv.getSize());
    return result;
    // checkme return T_mv::createFromValues(); // abnormal return
  } else
    try {
      gctools::StackAllocate<CatchDynEnv_O> env(&target, tag);
      gctools::StackAllocate<Cons_O> sa_ec(env.asSmartPtr(), my_thread->dynEnvStackGet());
      DynEnvPusher dep(my_thread, sa_ec.asSmartPtr());
      return cf();
    } catch (CatchThrow& ct) {
      if (ct.getTag() != tag)
        throw;
      else {
        core::MultipleValues& mv = core::lisp_multipleValues();
        T_mv result = mv.readFromMultipleValue0(mv.getSize());
        return result;
        // checkme return T_mv::createFromValues();
      }
    }
}

template <typename Boundf> inline auto call_with_cell_bound(VariableCell_sp cell, T_sp val, Boundf&& bound) {
  DynamicScopeManager scope(cell, val);
  gctools::StackAllocate<BindingDynEnv_O> bde(cell, scope.oldBinding());
  gctools::StackAllocate<Cons_O> sa_ec(bde.asSmartPtr(), my_thread->dynEnvStackGet());
  DynEnvPusher dep(my_thread, sa_ec.asSmartPtr());
  return bound();
}

template <typename Boundf> auto call_with_variable_bound(Symbol_sp sym, T_sp val, Boundf&& bound) {
  return call_with_cell_bound(sym->ensureVariableCell(), val, bound);
}

}; // namespace core
