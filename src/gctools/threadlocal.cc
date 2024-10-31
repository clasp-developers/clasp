
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <signal.h>
#include <clasp/core/foundation.h>
#include <clasp/gctools/threadlocal.h>
#include <clasp/core/lisp.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/array.h>
#include <clasp/core/debugger.h>
#include <clasp/core/lispStream.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/code.h>
#include <clasp/core/unwind.h>                    // DynEnv stuff
#include <clasp/gctools/boehmGarbageCollection.h> // DynEnv stuff
#include <clasp/external/thread-pool/thread_pool.h>

THREAD_LOCAL gctools::ThreadLocalStateLowLevel* my_thread_low_level;
THREAD_LOCAL core::ThreadLocalState* my_thread;

namespace core {

#ifdef DEBUG_VIRTUAL_MACHINE
int global_debug_virtual_machine = 0;

CL_LAMBDA(val reset-counters);
CL_DEFUN void core__debug_virtual_machine(int val, bool reset_counters) {
  global_debug_virtual_machine = val;
  if (reset_counters) {
    VM_RESET_COUNTERS(my_thread->_VM);
  }
}

#endif

void VirtualMachine::error() {
  printf("%s:%d:%s There was an error encountered in the vm - put a breakpoint here to trap it\n", __FILE__, __LINE__,
         __FUNCTION__);
};

unsigned int* BignumExportBuffer::getOrAllocate(const mpz_class& bignum, int nail) {
  size_t size = _lisp->integer_ordering()._mpz_import_size;
  size_t numb = (size << 3) - nail; // *8
  size_t count = (mpz_sizeinbase(bignum.get_mpz_t(), 2) + numb - 1) / numb;
  size_t bytes = count * size;
  if (bytes > this->bufferSize) {
    if (this->buffer) {
      free(this->buffer);
    }
    this->buffer = (unsigned int*)malloc(bytes);
  }
  return this->buffer;
};

}; // namespace core

namespace core {

size_t DynamicBindingStack::new_binding_index() const {
#ifdef CLASP_THREADS
  RAIILock<mp::Mutex> mutex(mp::global_BindingIndexPoolMutex);
  if (mp::global_BindingIndexPool.size() != 0) {
    size_t index = mp::global_BindingIndexPool.back();
    mp::global_BindingIndexPool.pop_back();
    return index;
  }
  return mp::global_LastBindingIndex.fetch_add(1);
#else
  return 0;
#endif
};

void DynamicBindingStack::release_binding_index(size_t index) const {
#ifdef CLASP_THREADS
  RAIILock<mp::Mutex> mutex(mp::global_BindingIndexPoolMutex);
  mp::global_BindingIndexPool.push_back(index);
#endif
};

T_sp* DynamicBindingStack::thread_local_reference(const uint32_t index) const {
  unlikely_if(index >= this->_ThreadLocalBindings.size()) this->_ThreadLocalBindings.resize(index + 1,
                                                                                            no_thread_local_binding<T_O>());
  return &(this->_ThreadLocalBindings[index]);
}

T_sp DynamicBindingStack::thread_local_value(uint32_t index) const { return *thread_local_reference(index); }

void DynamicBindingStack::set_thread_local_value(T_sp value, uint32_t index) { *thread_local_reference(index) = value; }

bool DynamicBindingStack::thread_local_boundp(uint32_t index) const {
  if (index == NO_THREAD_LOCAL_BINDINGS)
    return false;
  else if (index >= this->_ThreadLocalBindings.size())
    return false;
  else if (gctools::tagged_no_thread_local_bindingp(_ThreadLocalBindings[index].raw_()))
    return false;
  else
    return true;
}

}; // namespace core

namespace gctools {
ThreadLocalStateLowLevel::ThreadLocalStateLowLevel(void* stack_top)
    : _StackTop(stack_top), _DisableInterrupts(false)
#ifdef DEBUG_RECURSIVE_ALLOCATIONS
      ,
      _RecursiveAllocationCounter(0)
#endif

          {};

ThreadLocalStateLowLevel::~ThreadLocalStateLowLevel(){};

}; // namespace gctools
namespace core {

VirtualMachine::VirtualMachine()
    : _Running(true)
#ifdef DEBUG_VIRTUAL_MACHINE
      ,
      _counter0(0), _unwind_counter(0), _throw_counter(0)
#endif
{
}

void VirtualMachine::startup() {
  size_t stackSpace = VirtualMachine::MaxStackWords * sizeof(T_O*);
  this->_stackBottom = (T_O**)gctools::RootClassAllocator<T_O>::allocateRootsAndZero(VirtualMachine::MaxStackWords);
  this->_stackTop = this->_stackBottom + VirtualMachine::MaxStackWords - 1;
  //  printf("%s:%d:%s vm._stackTop = %p\n", __FILE__, __LINE__, __FUNCTION__, this->_stackTop );
  size_t pageSize = getpagesize();
  uintptr_t stackGuardPage = ((uintptr_t)this->_stackTop - pageSize) / pageSize;
  uintptr_t stackGuard = stackGuardPage * pageSize;
  this->_stackGuard = (core::T_O**)stackGuard;
  //  printf("%s:%d:%s stackGuard = %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)stackGuard );
  this->_stackBytes = stackSpace;
  // Clear the stack memory
  memset(this->_stackBottom, 0, stackSpace);
  this->enable_guards();
  this->_stackPointer = this->_stackBottom;
  (*this->_stackPointer) = NULL;
}

void VirtualMachine::enable_guards() {
//  printf("%s:%d:%s pid %d\n", __FILE__, __LINE__, __FUNCTION__, getpid()  );
#if 0
  size_t pageSize = getpagesize();
  int mprotectResult = mprotect((void*)this->_stackGuard,pageSize,PROT_READ);
  if (mprotectResult!=0) {
    printf("%s:%d:%s mprotect failed with %d\n", __FILE__, __LINE__, __FUNCTION__, mprotectResult );
  }
#endif
}
void VirtualMachine::disable_guards() {
//  printf("%s:%d:%s pid %d\n", __FILE__, __LINE__, __FUNCTION__, getpid()  );
#if 0
  size_t pageSize = getpagesize();
  int mprotectResult = mprotect((void*)this->_stackGuard,pageSize,PROT_READ|PROT_WRITE);
  if (mprotectResult!=0) {
    printf("%s:%d:%s mprotect failed with %d\n", __FILE__, __LINE__, __FUNCTION__, mprotectResult );
  }
#endif
}

VirtualMachine::~VirtualMachine() {
#if 1
  this->disable_guards();
#endif
  gctools::RootClassAllocator<T_O>::freeRoots(this->_stackBottom);
}

// For main thread initialization - it happens too early and _Nil is undefined
// So this partially sets up the ThreadLocalState and the system must invoke
// ThreadLocalState::finish_initialization_main_thread() after the Nil symbol is
// in GC managed memory.
ThreadLocalState::ThreadLocalState(bool dummy)
  : _unwinds(0), _CleanupFunctions(NULL), _ObjectFiles(), _BufferStr8NsPool(), _BufferStrWNsPool(), _PendingSignalsP(false),
    // initialized with null pointers so that dequeue_interrupt
    // can see that the queue is not yet available.
    // Default-initializing an atomic default-initializes the underlying object
    // only in C++20 and beyond.
    _PendingInterruptsHead(), _PendingInterruptsTail(),
    _Breakstep(false), _BreakstepFrame(NULL), _DynEnvStackBottom(), _UnwindDest(), _DtreeInterpreterCallCount(0) {
  my_thread = this;
#ifdef _TARGET_OS_DARWIN
  pthread_threadid_np(NULL, &this->_Tid);
#else
  this->_Tid = 0;
#endif
  this->_xorshf_x = rand();
  this->_xorshf_y = rand();
  this->_xorshf_z = rand();
  sigemptyset(&this->_PendingSignals);
}

pid_t ThreadLocalState::safe_fork() {
  // Wrap fork in code that turns guards off and on
  this->_VM.disable_guards();
  pid_t result = fork();
  if (result == -1) {
    // error
    printf("%s:%d:%s fork failed errno = %d\n", __FILE__, __LINE__, __FUNCTION__, errno);
  } else if (result == 0) {
    // child
    this->_VM.enable_guards();
  } else {
    // parent
    this->_VM.enable_guards();
  }
  return result;
}

// This needs to be called at initialization immediately after Nil is allocated
// AND during image load once Nil is found in the image and relocated to its
// new position in the GC managed memory.
void ThreadLocalState::finish_initialization_main_thread(core::T_sp theNilObject) {
  if (!theNilObject.raw_()) {
    printf("%s:%d:%s reinitialize symbols the _Nil object is not defined!!!\n", __FILE__, __LINE__, __FUNCTION__);
    abort();
  }
  //  printf("%s:%d:%s reinitialize symbols here once _Nil is defined\n", __FILE__, __LINE__, __FUNCTION__ );
  // Reinitialize all threadlocal lists once NIL is defined
  // We work with theObject here directly because it's very early in the bootstrapping
  if (this->_ObjectFiles.theObject)
    goto ERR;
  if (this->_BufferStr8NsPool.theObject)
    goto ERR;
  if (this->_BufferStrWNsPool.theObject)
    goto ERR;
  if (this->_DynEnvStackBottom.theObject)
    goto ERR;
  if (this->_UnwindDest.theObject)
    goto ERR;
  this->_ObjectFiles.theObject = theNilObject.theObject;
  this->_BufferStr8NsPool.theObject = theNilObject.theObject;
  this->_BufferStrWNsPool.theObject = theNilObject.theObject;
  this->_DynEnvStackBottom.theObject = theNilObject.theObject;
  this->_UnwindDest.theObject = theNilObject.theObject;
  return;
ERR:
  printf("%s:%d:%s one of the reinitialize symbols was already initialized\n", __FILE__, __LINE__, __FUNCTION__);
  abort();
};

// This is for constructing ThreadLocalState for threads
ThreadLocalState::ThreadLocalState()
  : _unwinds(0), _ObjectFiles(nil<core::T_O>()), _CleanupFunctions(NULL), _Breakstep(false), _PendingSignalsP(false),
    _PendingInterruptsHead(), _PendingInterruptsTail(),
    _BreakstepFrame(NULL), _DynEnvStackBottom(nil<core::T_O>()), _UnwindDest(nil<core::T_O>()) {
  my_thread = this;
#ifdef _TARGET_OS_DARWIN
  pthread_threadid_np(NULL, &this->_Tid);
#else
  this->_Tid = 0;
#endif
  this->_BufferStr8NsPool.reset_(); // Can't use nil<core::T_O>(); - too early
  this->_BufferStrWNsPool.reset_();
  this->_xorshf_x = rand();
  this->_xorshf_y = rand();
  this->_xorshf_z = rand();
  sigemptyset(&this->_PendingSignals);
}

static void dumpDynEnvStack(T_sp stack) {
  size_t level = 0;
  intptr_t prev = 0;
  for (T_sp iter = stack;;) {
    if (level > 1000) {
      fprintf(stderr, "[hit length limit (1000)]\n");
      break;
    }
    if (iter.consp()) {
      fprintf(stderr, " level %3lu  %p", level, iter.raw_());
      if (prev != 0) {
        fprintf(stderr, " [delta %ld]\n", (intptr_t)iter.raw_() - prev);
      } else {
        fprintf(stderr, "\n");
      }
      prev = (intptr_t)iter.raw_();
    } else if (iter.nilp())
      break;
    else {
      fprintf(stderr, "level %3lu  %p [NOT A CONS!]\n", level, iter.raw_());
      break;
    }
    ++level;
    iter = CONS_CDR(iter);
  }
}

void ThreadLocalState::dynEnvStackTest(core::T_sp bot) const {
  uintptr_t approximate_sp = (uintptr_t)(__builtin_frame_address(0));
  if (bot.consp()) {
    T_sp turtle = bot;
    T_sp hare = CONS_CDR(bot);
    while (true) {
      if ((uintptr_t)turtle.raw_() <= approximate_sp) {
        fprintf(stderr, "%s:%d:%s: The DynEnvStack has conses from deallocated stack space\n", __FILE__, __LINE__, __FUNCTION__);
        dumpDynEnvStack(bot);
        abort();
      }
      if (turtle == hare) {
        fprintf(stderr, "%s:%d:%s: The DynEnvStack is circular\n", __FILE__, __LINE__, __FUNCTION__);
        // This will dump a whole 1000 frames. It could be made smarter,
        // but I don't think this is a likely scenario and it's just here to
        // avoid a nonterminating test which would be really damn annoying.
        dumpDynEnvStack(bot);
        abort();
      }
      if (!gc::IsA<core::DynEnv_sp>(ENSURE_VALID_OBJECT(CONS_CAR(turtle)))) {
        fprintf(stderr, "%s:%d:%s: The DynEnvStack contains a non-dynenv\n", __FILE__, __LINE__, __FUNCTION__);
        dumpDynEnvStack(bot);
        abort();
      }
      turtle = CONS_CDR(turtle);
      if (turtle.nilp())
        break;
      else if (!turtle.consp()) {
        fprintf(stderr, "%s:%d:%s: The DynEnvStack is a dotted list\n", __FILE__, __LINE__, __FUNCTION__);
        dumpDynEnvStack(bot);
        abort();
      }
      if (hare.consp()) {
        hare = CONS_CDR(hare);
        if (hare.consp())
          hare = CONS_CDR(hare);
      }
    }
  } else if (bot.notnilp()) {
    fprintf(stderr, "%s:%d:%s: The DynEnvStack is not a list\n", __FILE__, __LINE__, __FUNCTION__);
    dumpDynEnvStack(bot);
    abort();
  }
}
uint32_t ThreadLocalState::random() {
  unsigned long t;
  // This random number generator is ONLY used to initialize
  // the badges of general objects (currently, because cons cells
  // initialize their badge using the allocation address).
  // This generator should never return zero because
  // that would cause problems with boehm precise mode marking.
  do {
    this->_xorshf_x ^= this->_xorshf_x << 16;
    this->_xorshf_x ^= this->_xorshf_x >> 5;
    this->_xorshf_x ^= this->_xorshf_x << 1;
    t = this->_xorshf_x;
    this->_xorshf_x = this->_xorshf_y;
    this->_xorshf_y = this->_xorshf_z;
    this->_xorshf_z = t ^ this->_xorshf_x ^ this->_xorshf_y;
  } while (this->_xorshf_z == gctools::BaseHeader_s::BadgeStampWtagMtag::IllegalBadge ||
           this->_xorshf_z == gctools::BaseHeader_s::BadgeStampWtagMtag::NoBadge);
  uint32_t rnd = this->_xorshf_z & 0xFFFFFFFF;
  // printf("%s:%d:%s rnd = %u\n", __FILE__, __LINE__, __FUNCTION__, rnd );
  return rnd;
}

void ThreadLocalState::pushObjectFile(llvmo::ObjectFile_sp of) {
  this->_ObjectFiles = core::Cons_O::create(of, this->_ObjectFiles);
}

llvmo::ObjectFile_sp ThreadLocalState::topObjectFile() {
  core::T_sp of = this->_ObjectFiles;
  if (of.nilp()) {
    return unbound<llvmo::ObjectFile_O>();
  }
  // The following MUST be As_unsafe because we might be loading an image
  // and we can't check headers in that situation
  return gc::As_unsafe<llvmo::ObjectFile_sp>(CONS_CAR(of));
}

void ThreadLocalState::popObjectFile() {
  if (this->_ObjectFiles.consp()) {
    this->_ObjectFiles = CONS_CDR(this->_ObjectFiles);
    return;
  }
  SIMPLE_ERROR("There were no more object files");
}

// INTERRUPT QUEUE
// Very simple atomic queue, but I still had to consult with a paper:
// Valois, John D. "Implementing lock-free queues." Proceedings of the seventh international conference on Parallel and Distributed Computing Systems. 1994.
// The ABA problem mentioned there shouldn't matter since we always use fresh
// conses for the new tails. Technically I guess we could reallocate one by
// coincidence but that seems really unlikely?
void ThreadLocalState::enqueue_interrupt(core::T_sp interrupt) {
  core::Cons_sp record = core::Cons_O::create(interrupt, nil<core::T_O>());
  // relaxed because queueing an interrupt does not synchronize with queueing
  // another interrupt
  core::Cons_sp tail = _PendingInterruptsTail.load(std::memory_order_relaxed);
  while (true) {
    core::T_sp ntail = nil<core::T_O>();
    if (tail->cdrCAS(ntail, record, std::memory_order_release)) break;
    else tail = ntail.as_assert<core::Cons_O>();
  }
  _PendingInterruptsTail.compare_exchange_strong(tail, record,
                                                 std::memory_order_release);
}

// Pop a thing from the interrupt queue. Returns NIL if the queue is empty.
core::T_sp ThreadLocalState::dequeue_interrupt() {
  // Use acquire-release since sending an interrupt synchronizes-with processing
  // that interrupt.
  core::Cons_sp head = _PendingInterruptsHead.load(std::memory_order_acquire);
  core::T_sp next;
  core::Cons_sp cnext;
  do {
    next = head->cdr();
    if (next.nilp()) return next; // nothing to dequeue
    cnext = next.as_assert<core::Cons_O>();
  } while (!_PendingInterruptsHead.compare_exchange_weak(head, cnext,
                                                         std::memory_order_acq_rel));
  core::T_sp interrupt = cnext->car();
  // We need to keep the new head where it is so the queue is never empty
  // (empty queues make atomicity hard-to-impossible)
  // but we should spike the next to make the interrupt collectible later.
  cnext->rplaca(nil<core::T_O>());
  return interrupt;
}

void ThreadLocalState::startUpVM() { this->_VM.startup(); }

ThreadLocalState::~ThreadLocalState() {}

void thread_local_register_cleanup(const std::function<void(void)>& cleanup) {
  CleanupFunctionNode* node = new CleanupFunctionNode(cleanup, my_thread->_CleanupFunctions);
  //  printf("%s:%d:%s %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)node);
  my_thread->_CleanupFunctions = node;
}

void thread_local_invoke_and_clear_cleanup() {
  //  printf("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__);
  CleanupFunctionNode* node = my_thread->_CleanupFunctions;
  while (node) {
    node->_CleanupFunction();
    CleanupFunctionNode* next = node->_Next;
    delete node;
    node = next;
  }
  my_thread->_CleanupFunctions = NULL;
}

// Need to use LTO to inline this.
inline void registerTypesAllocated(size_t bytes) { my_thread->_BytesAllocated += bytes; }

void ThreadLocalState::initialize_thread(mp::Process_sp process, bool initialize_GCRoots = true) {
  //  printf("%s:%d Initialize all ThreadLocalState things this->%p\n",__FILE__, __LINE__, (void*)this);
  this->_Process = process;
  process->_ThreadInfo = this;
  this->_BFormatStringOutputStream = gc::As<StringOutputStream_sp>(clasp_make_string_output_stream());
#ifdef CLASP_UNICODE
  this->_WriteToStringOutputStream =
      gc::As<StringOutputStream_sp>(clasp_make_string_output_stream(STRING_OUTPUT_STREAM_DEFAULT_SIZE, 1));
#else
  this->_WriteToStringOutputStream = gc::As<StringOutputStream_sp>(clasp_make_string_output_stream());
#endif
  core::Cons_sp intqueue = core::Cons_O::create(nil<core::T_O>(), nil<core::T_O>());
  this->_PendingInterruptsHead.store(intqueue, std::memory_order_release);
  this->_PendingInterruptsTail.store(intqueue, std::memory_order_release);
};

}; // namespace core

uint32_t my_thread_random() { return my_thread->random(); }

namespace gctools {

DOCGROUP(clasp);
CL_DEFUN size_t gctools__thread_local_unwinds() { return my_thread->_unwinds; }

}; // namespace gctools
