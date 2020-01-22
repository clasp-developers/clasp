#ifndef gctools_threadlocal_H
#define gctools_threadlocal_H

#include <signal.h>
#include <clasp/gctools/threadlocal.fwd.h>



typedef core::T_O*(*fnStartUp)(core::T_O*);


namespace core {

#define STARTUP_FUNCTION_CAPACITY_INIT 128
#define STARTUP_FUNCTION_CAPACITY_MULTIPLIER 2
  struct Startup {
    size_t _Position;
    fnStartUp _Function;
    Startup() {};
  Startup(size_t p, fnStartUp f) : _Position(p), _Function(f) {};
    bool operator<(const Startup& other) {
      return this->_Position < other._Position;
    }
  };

  struct StartupInfo {
    size_t _capacity;
    size_t _count;
    Startup* _functions;

  StartupInfo() : _capacity(0), _count(0), _functions(NULL) {};
  };
};

namespace core {
#define IHS_BACKTRACE_SIZE 16
  struct InvocationHistoryFrame;
  struct ThreadLocalState {
    ThreadLocalState();
    void initialize_thread(mp::Process_sp process, bool initialize_GCRoots);
    void create_sigaltstack();
    void destroy_sigaltstack();
    void pushCatchTag(T_sp);
    
    uint64_t   _BytesAllocated;
    mp::Process_sp _Process;
    ObjectFile_sp  _ObjectFile; // Capture jitted object-file's with this
    uint64_t  _Tid;
    uintptr_t           _BacktraceBasePointer;
    DynamicBindingStack _Bindings;
    inline DynamicBindingStack& bindings() { return this->_Bindings; };
    List_sp _CatchTags;
    inline List_sp catchTags() { return this->_CatchTags; };
    inline void setCatchTags(List_sp tags) { this->_CatchTags = tags; };
    MultipleValues _MultipleValues;
    const InvocationHistoryFrame* _InvocationHistoryStackTop;
    gctools::GCRootsInModule*  _GCRoots;
    void* _sigaltstack_buffer;
    stack_t _original_stack;
    void*             _text_segment_start; // Temporarily store text segment start
    size_t            _text_segment_size; // store text segment size
    uintptr_t         _stackmap;
    size_t            _stackmap_size;
    StartupInfo       _Startup;
    void*             _ObjectFileStartUp;
#ifdef DEBUG_IHS
    // Save the last return address before IHS screws up
    void*                    _IHSBacktrace[IHS_BACKTRACE_SIZE];
#endif
#ifdef DEBUG_COUNT_ALLOCATIONS
    std::vector<size_t>    _CountAllocations;
    bool                   _BacktraceAllocationsP;
    Fixnum                 _BacktraceStamp;
    int                    _BacktraceFd;
#endif
#ifdef DEBUG_MONITOR_SUPPORT
    // When enabled, maintain a thread-local map of strings to FILE*
    // used for logging. This is so that per-thread log files can be
    // generated.  These log files are automatically closed when the
    // thread exits.
    std::map<std::string,FILE*> _MonitorFiles;
#endif
#if 1
// thread local caches work fine
    /*! SingleDispatchGenericFunction cache */
    Cache_sp _SingleDispatchMethodCachePtr;
#endif
    /*! Pending interrupts */
    List_sp _PendingInterrupts;
    /*! Save CONS records so we don't need to do allocations
        to add to _PendingInterrupts */
    List_sp _SparePendingInterruptRecords; // signal_queue on ECL
    mp::SpinLock _SparePendingInterruptRecordsSpinLock;
    /*------- per-thread data */
    List_sp _BufferStr8NsPool;
    List_sp _BufferStrWNsPool;
    StringOutputStream_sp _BFormatStringOutputStream;
    StringOutputStream_sp _WriteToStringOutputStream;
    Bignum_sp _BignumRegister0;
    Bignum_sp _BignumRegister1;
    Bignum_sp _BignumRegister2;
    Bignum_sp bigRegister0() { return this->_BignumRegister0; };
    Bignum_sp bigRegister1() { return this->_BignumRegister1; };
    Bignum_sp bigRegister2() { return this->_BignumRegister2; };
    ~ThreadLocalState();
  };


// Thing to maintain the list of valid catch tags correctly.
struct CatchTagPusher {
  ThreadLocalState* mthread;
  List_sp catch_tag_state;
  CatchTagPusher(ThreadLocalState* thread, T_sp tag) {
    mthread = thread;
    catch_tag_state = thread->catchTags();
    thread->pushCatchTag(tag);
  }
  ~CatchTagPusher() { mthread->setCatchTags(this->catch_tag_state); }
};


}; // namespace core


namespace gctools {

#ifdef DEBUG_COUNT_ALLOCATIONS
  void maybe_initialize_mythread_backtrace_allocations();
  void start_backtrace_allocations(const std::string& filename, Fixnum stamp);
  void stop_backtrace_allocations();
#endif

  void registerBytesAllocated(size_t bytes);
};




#endif
