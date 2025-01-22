#pragma once

#include <signal.h>
#include <functional>
#include <algorithm> // copy
#include <clasp/gctools/threadlocal.fwd.h>

typedef core::T_O* (*T_OStartUp)(core::T_O*);
typedef void (*voidStartUp)(void);

namespace core {

#ifdef DEBUG_DYN_ENV_STACK
extern bool global_debug_dyn_env_stack;
#endif

#define STARTUP_FUNCTION_CAPACITY_INIT 128
#define STARTUP_FUNCTION_CAPACITY_MULTIPLIER 2
struct StartUp {
  typedef enum { T_O_function, void_function } FunctionEnum;
  FunctionEnum _Type;
  size_t _Position;
  void* _Function;
  StartUp(){};
  StartUp(FunctionEnum type, size_t p, void* f) : _Type(type), _Position(p), _Function(f){};
  bool operator<(const StartUp& other) { return this->_Position < other._Position; }
};

struct StartupInfo {
  size_t _capacity;
  size_t _count;
  StartUp* _functions;

  StartupInfo() : _capacity(0), _count(0), _functions(NULL){};
};
}; // namespace core

namespace core {
struct CleanupFunctionNode {
  std::function<void(void)> _CleanupFunction;
  CleanupFunctionNode* _Next;
  CleanupFunctionNode(const std::function<void(void)>& cleanup, CleanupFunctionNode* next)
      : _CleanupFunction(cleanup), _Next(next){};
};
}; // namespace core

namespace llvmo {
class ObjectFile_O;
typedef gctools::smart_ptr<ObjectFile_O> ObjectFile_sp;
class CodeBase_O;
typedef gctools::smart_ptr<CodeBase_O> CodeBase_sp;

}; // namespace llvmo
namespace core {

#ifdef DEBUG_VIRTUAL_MACHINE
#define DVM_TRACE_FRAME 0b0001
extern int global_debug_virtual_machine;

#define VM_ASSERT_ALIGNED(vm, ptr)                                                                                                 \
  if (((uintptr_t)(ptr)) & 0x7) {                                                                                                  \
    printf("%s:%d:%s Unaligned pointer %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)(ptr));                                     \
    (vm).error();                                                                                                                  \
  }
#define VM_STACK_POINTER_CHECK(vm)                                                                                                 \
  if ((vm)._Running && _stackPointer && !((vm)._stackBottom <= _stackPointer && _stackPointer <= (vm)._stackTop)) {                \
    printf("%s:%d:%s _stackPointer %p is out of stack _stackTop %p _stackBottom %p\n", __FILE__, __LINE__, __FUNCTION__,           \
           (void*)(_stackPointer), (void*)((vm)._stackTop), (void*)((vm)._stackBottom));                                           \
    (vm).error();                                                                                                                  \
  }
#define VM_PC_CHECK(vm, pc, bytecode_start, bytecode_end)                                                                          \
  if ((uintptr_t)pc < (uintptr_t)bytecode_start || (uintptr_t)pc >= (uintptr_t)bytecode_end) {                                     \
    printf("%s:%d:%s vm._pc %p is outside of the bytecode vector range [ %p - %p ]\n", __FILE__, __LINE__, __FUNCTION__,           \
           (void*)pc, (void*)bytecode_start, (void*)bytecode_end);                                                                 \
    (vm).error();                                                                                                                  \
  }
#define VM_CHECK(vm) VM_STACK_POINTER_CHECK(vm);
#define VM_CURRENT_DATA(vm, data)                                                                                                  \
  { (vm)._data = data; }
#define VM_CURRENT_DATA1(vm, data)                                                                                                 \
  { (vm)._data1 = data; }
#define VM_INC_COUNTER0(vm)                                                                                                        \
  { (vm)._counter0++; }
#define VM_INC_UNWIND_COUNTER(vm)                                                                                                  \
  { (vm)._unwind_counter++; }
#define VM_INC_THROW_COUNTER(vm)                                                                                                   \
  { (vm)._throw_counter++; }
#define VM_RESET_COUNTERS(vm)                                                                                                      \
  {                                                                                                                                \
    (vm)._unwind_counter = 0;                                                                                                      \
    (vm)._throw_counter = 0;                                                                                                       \
    (vm)._counter0 = 0;                                                                                                            \
  }
#else
#define VM_ASSERT_ALIGNED(vm, ptr)
#define VM_STACK_POINTER_CHECK(vm)
#define VM_CHECK(vm)
#define VM_PC_CHECK(vm, pc, start, end)
#define VM_CURRENT_DATA(vm, data)
#define VM_CURRENT_DATA1(vm, data)
#define VM_INC_COUNTER0(vm)
#define VM_INC_UNWIND_COUNTER(vm)
#define VM_INC_THROW_COUNTER(vm)
#define VM_RESET_COUNTERS(vm)
#endif

struct VirtualMachine {
  // Stack size is kind of arbitrary, and really we should make it
  // grow and etc.
  static constexpr size_t MaxStackWords = 65536;
  bool _Running;
  core::T_O** _stackBottom = nullptr;
  size_t _stackBytes;
  core::T_O** _stackTop;
  core::T_O** _stackGuard;
  core::T_O** _stackPointer;
  // only used by debugger
  // has to be initialized because bytecode_call reads it
  core::T_O** _framePointer = nullptr;
#ifdef DEBUG_VIRTUAL_MACHINE
  core::T_O* _data;
  core::T_O* _data1;
  size_t _counter0;
  size_t _unwind_counter;
  size_t _throw_counter;
#endif
  core::T_O** _literals;
  unsigned char* _pc;

  void error();

  void enable_guards();
  void disable_guards();

  void startup();
  inline void shutdown() { this->_Running = false; }
  inline void push(core::T_O**& stackPointer, core::T_O* value) {
    stackPointer++;
    VM_CHECK(*this);
    VM_ASSERT_ALIGNED(*this, stackPointer);
    *stackPointer = value;
  }

  inline core::T_O* pop(core::T_O**& stackPointer) {
    core::T_O* value = *stackPointer;
    stackPointer--;
    VM_CHECK(*this);
    return value;
  }

  // Allocate a Vaslist object on the stack.
  inline core::T_O* alloca_vaslist1(core::T_O**& stackPointer, core::T_O** args, size_t nargs) {
    stackPointer += 2;
    *(stackPointer - 1) = (core::T_O*)args;
    *(stackPointer - 0) = Vaslist::make_shifted_nargs(nargs);
    VM_CHECK(*this);
    return gc::tag_vaslist<core::T_O*>((core::Vaslist*)(stackPointer - 1));
  }

  inline core::T_O* alloca_vaslist2(core::T_O**& stackPointer, core::T_O** args, size_t nargs) {
    core::T_O* vl = this->alloca_vaslist1(stackPointer, args, nargs);
    this->alloca_vaslist1(stackPointer, args, nargs);
    return vl;
  }

  // Drop NELEMS slots on the stack all in one go.
  inline void drop(core::T_O**& stackPointer, size_t nelems) {
    stackPointer -= nelems;
    VM_CHECK(*this);
    VM_ASSERT_ALIGNED(*this, stackPointer);
  }

  // Get a pointer to the nth element from the stack
  // i.e. 0 is most recently pushed, 1 the next most recent, etc.
  inline core::T_O** stackref(core::T_O**& stackPointer, ptrdiff_t n) {
    VM_CHECK(*this);
    VM_ASSERT_ALIGNED(*this, stackPointer);
    return stackPointer - n;
  }

  // Push a new frame with NLOCALS local variables.
  // Return the new stack pointer.
  inline T_O** push_frame(core::T_O** framePointer, size_t nlocals) {
#ifdef DEBUG_VIRTUAL_MACHINE
    if (global_debug_virtual_machine & DVM_TRACE_FRAME) {
      printf("\nFRAME PUSH %p %p %lu unwind_counter %lu throw_counter %lu\n", this->_data, this->_data1, this->_counter0,
             this->_unwind_counter, this->_throw_counter);
    }
#endif
    core::T_O** ret = framePointer + nlocals;
    VM_STACK_POINTER_CHECK(*this);
    VM_ASSERT_ALIGNED(*this, ret);
    return ret;
  }

  inline void setreg(T_O** framePointer, size_t base, core::T_O* value) { *(framePointer + base + 1) = value; }

  inline void savesp(T_O** framePointer, T_O**& stackPointer, size_t base) {
    *(framePointer + base + 1) = (core::T_O*)stackPointer;
  }

  inline void restoresp(T_O** framePointer, T_O**& stackPointer, size_t base) {
    stackPointer = (core::T_O**)(*(framePointer + base + 1));
  }

  // Copy N elements from SOURCE into the current frame's register file
  // starting at BASE.
  inline void copytoreg(core::T_O** framePointer, core::T_O** source, size_t n, size_t base) {
    VM_CHECK(*this);
    VM_ASSERT_ALIGNED(*this, source);
    std::copy(source, source + n, framePointer + base + 1);
  }

  // Fill OBJECT into N registers starting at BASE.
  inline void fillreg(core::T_O** framePointer, core::T_O* object, size_t n, size_t base) {
    VM_CHECK(*this);
    VM_ASSERT_ALIGNED(*this, framePointer + base + 1);
    std::fill(framePointer + base + 1, framePointer + base + n + 1, object);
  }

  // Get a pointer to the nth register in the current frame.
  inline core::T_O** reg(core::T_O** framePointer, size_t n) {
    VM_CHECK(*this);
    VM_ASSERT_ALIGNED(*this, framePointer + n + 1);
    return framePointer + n + 1;
  }

  // Compute how many elements are on the stack in the current frame
  // but which are not part of the register file.
  inline ptrdiff_t npushed(T_O** framePointer, T_O**& stackPointer, size_t nlocals) {
    VM_CHECK(*this);
    VM_ASSERT_ALIGNED(*this, stackPointer);
    VM_ASSERT_ALIGNED(*this, framePointer);
    return stackPointer - nlocals - framePointer;
  }

  // Copy the n most recent pushes to the given memory.
  // The most recent push goes to the end of the range.
  // Unlike copytoreg, here the destination is the pointer to the start
  // of the range regardless of stack growth direction.
  template <class OutputIter> inline void copyto(core::T_O**& stackPointer, size_t n, OutputIter dest) {
    VM_CHECK(*this);
    VM_ASSERT_ALIGNED(*this, stackPointer + 1 - n);
    std::copy(stackPointer + 1 - n, stackPointer + 1, dest);
  }

  VirtualMachine();
  ~VirtualMachine();
};

#define IHS_BACKTRACE_SIZE 16
struct ThreadLocalState {

  mp::Process_sp _Process;
  DynamicBindingStack _Bindings;
  std::atomic<core::Cons_sp> _PendingInterruptsHead;
  std::atomic<core::Cons_sp> _PendingInterruptsTail;
  sigset_t _PendingSignals;
  std::atomic<bool> _PendingSignalsP;
  std::atomic<bool> _BlockingP;
  List_sp _BufferStr8NsPool;
  List_sp _BufferStrWNsPool;
  StringOutputStream_sp _BFormatStringOutputStream;
  StringOutputStream_sp _WriteToStringOutputStream;
  MultipleValues _MultipleValues;
  void* _sigaltstack_buffer;
  size_t _unwinds;
  stack_t _original_stack;
  std::string _initializer_symbol;
  void* _object_file_start;
  size_t _object_file_size;
  gctools::GCRootsInModule* _GCRootsInModule;
  StartupInfo _Startup;
  bool _Breakstep; // Should we check for breaks?
  // What frame are we stepping over? NULL means step-into mode.
  void* _BreakstepFrame;
  // Stuff for SJLJ unwinding
  List_sp _DynEnvStackBottom;
  T_sp _UnwindDest;
  size_t _UnwindDestIndex;
#ifdef DEBUG_IHS
  // Save the last return address before IHS screws up
  void* _IHSBacktrace[IHS_BACKTRACE_SIZE];
#endif
  size_t _xorshf_x; // Marsaglia's xorshf generator
  size_t _xorshf_y;
  size_t _xorshf_z;
  CleanupFunctionNode* _CleanupFunctions;
  uint64_t _BytesAllocated;
  uint64_t _Tid;
  uintptr_t _BacktraceBasePointer;
  uint64_t _DtreeInterpreterCallCount;
  VirtualMachine _VM;

#ifdef DEBUG_MONITOR_SUPPORT
  // When enabled, maintain a thread-local map of strings to FILE*
  // used for logging. This is so that per-thread log files can be
  // generated.  These log files are automatically closed when the
  // thread exits.
  std::map<std::string, FILE*> _MonitorFiles;
#endif

public:
  // Methods
  ThreadLocalState(bool dummy);
  void finish_initialization_main_thread(core::T_sp theNilObject);
  ThreadLocalState();
  void initialize_thread(mp::Process_sp process, bool initialize_GCRoots);

  pid_t safe_fork();

  void dynEnvStackTest(core::T_sp val) const;
  void dynEnvStackSet(core::T_sp val) {
#ifdef DEBUG_DYN_ENV_STACK
    if (core::global_debug_dyn_env_stack)
      this->dynEnvStackTest(val);
#endif
    this->_DynEnvStackBottom = val;
  }
  core::T_sp dynEnvStackGet() const {
#ifdef DEBUG_DYN_ENV_STACK
    if (core::global_debug_dyn_env_stack)
      this->dynEnvStackTest(this->_DynEnvStackBottom);
#endif
    return this->_DynEnvStackBottom;
  }

  uint32_t random();

  inline DynamicBindingStack& bindings() { return this->_Bindings; };

  void startUpVM();

  inline void enqueue_signal(int signo) {
    // Called from signal handlers.
    sigaddset(&_PendingSignals, signo); // sigaddset is AS-safe
    // It's possible this handler could be interrupted between these two lines.
    // If it's interrupted and the signal is queued/the handler returns,
    // it doesn't matter. If it's interrupted and the handler escapes, we have
    // a queued signal without the flag being set, which is a little unfortunate
    // but not a huge deal - another signal will set the flag for one thing.
    // It also shouldn't be a problem since we only really jump from synchronously
    // delivered signals (segv, etc) which could only be signaled here if something
    // has gone very deeply wrong.
    // On GNU we have sigisemptyset which could be used instead of a separate flag,
    // and that would solve the problem, but that's only on GNU, plus it's
    // necessarily a little slower than a simple flag.
    _PendingSignalsP.store(true, std::memory_order_release);
  }
  inline bool pending_signals_p() {
    return _PendingSignalsP.load(std::memory_order_acquire);
  }
  inline void clear_pending_signals_p() {
    _PendingSignalsP.store(false, std::memory_order_release);
  }
  inline sigset_t* pending_signals() { return &_PendingSignals; }
  void enqueue_interrupt(core::T_sp interrupt);
  // Check if the interrupt queue is ready for dequeueing.
  inline bool interrupt_queue_validp() {
    return static_cast<bool>(_PendingInterruptsHead.load(std::memory_order_acquire));
  }
  core::T_sp dequeue_interrupt();
  inline void set_blockingp(bool b) { _BlockingP.store(b, std::memory_order_release); }
  inline bool blockingp() const { return _BlockingP.load(std::memory_order_acquire); }

  ~ThreadLocalState();
};

void thread_local_register_cleanup(const std::function<void(void)>& cleanup);
void thread_local_invoke_and_clear_cleanup();

}; // namespace core

namespace gctools {

void registerBytesAllocated(size_t bytes);
};

struct ThreadManager {
  struct Worker {
#ifdef USE_BOEHM
    GC_stack_base _StackBase;
#endif
    // Worker must be allocated at the top of the worker thread function
    // It uses RAII to register/deregister our thread
    Worker() {
//      printf("%s:%d:%s Starting pid %d\n", __FILE__, __LINE__, __FUNCTION__, getpid() );
#ifdef USE_BOEHM
      GC_get_stack_base(&this->_StackBase);
      GC_register_my_thread(&this->_StackBase);
#endif
    };
    ~Worker() {
      //      printf("%s:%d:%s Stopping pid %d\n", __FILE__, __LINE__, __FUNCTION__, getpid() );
#ifdef USE_BOEHM
      GC_unregister_my_thread();
#endif
    };
  };
  void register_thread(std::thread& th){
      // Do nothing for now
  };
  void unregister_thread(std::thread& th){
      //    printf("%s:%d:%s What do I do here pid %d\n", __FILE__, __LINE__, __FUNCTION__, getpid(); );
  };
};

template <typename T> class thread_pool;

namespace gctools {

extern thread_pool<ThreadManager>* global_thread_pool;

};
