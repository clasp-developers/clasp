#pragma once

#include <concepts> // invocable
#include <clasp/gctools/memoryManagement.h>
#include <clasp/gctools/threadlocal.h>
#include <clasp/core/lisp.h> // _lisp
#include <clasp/core/mpPackage.h>

namespace gctools {

/* Walk all of the global (not per-thread) roots,
 * passing the address of each root */
template <std::invocable<Tagged*> RootWalkCallback>
void walkGlobalRoots(RootWalkCallback&& callback) {
  // luckily, just the one god object references everything else
  callback((Tagged*)&_lisp);
};

template <std::invocable<Tagged*> ThreadWalkCallback>
void walkThreadRoots(ThreadWalkCallback&& callback,
                     core::ThreadLocalState* tls) {
  tls->walkRoots(callback);
  tls->walkVMStack(callback);
  tls->walkControlStack([&](Tagged* tp) {
    // The control stack we have to walk conservatively.
    switch(ptag(*tp)) {
    case general_tag: {
      Header_s* header = (Header_s*)GeneralPtrToHeaderPtr(untag_object((void*)*tp));
      if (header->isValidGeneralObject())
        callback(tp);
    } break;
    case cons_tag: {
      ConsHeader_s* header = (ConsHeader_s*)ConsPtrToHeaderPtr(untag_object((void*)*tp));
      if (header->isValidConsObject())
        callback(tp);
    } break;
    default: callback(tp);
    }
  });
}

template <std::invocable<Tagged*> ThreadWalkCallback>
void walkAllThreadRoots(ThreadWalkCallback&& callback) {
  // NOTE we don't need the threads mutex since we must have stopped the world.
  for (auto cur : _lisp->_Roots._ActiveThreads) {
    mp::Process_sp proc = core::oCar(cur).as_assert<mp::Process_O>();
    core::ThreadLocalState* tls = proc->_ThreadInfo;
    walkThreadRoots(callback, tls);
  }
}

template <std::invocable<Tagged*> WalkCallback>
void walkRoots(WalkCallback&& callback) {
  walkGlobalRoots(callback);
  walkAllThreadRoots(callback);
}
  
}; // namespace gctools
