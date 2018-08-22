#ifndef gctools_threadlocal_H
#define gctools_threadlocal_H

#include <signal.h>
#include <clasp/gctools/threadlocal.fwd.h>

namespace core {
#define IHS_BACKTRACE_SIZE 16
  struct InvocationHistoryFrame;
  struct ThreadLocalState {
    ThreadLocalState();
    void initialize_thread(mp::Process_sp process, bool initialize_GCRoots);
    void create_sigaltstack();
    void destroy_sigaltstack();
    
    uint64_t   _BytesAllocated;
    mp::Process_sp _Process;
    uint64_t  _Tid;
    DynamicBindingStack _Bindings;
    ExceptionStack _ExceptionStack;
    MultipleValues _MultipleValues;
    BignumExportBuffer _AsInt64Buffer;
    BignumExportBuffer _AsUint64Buffer;
    const InvocationHistoryFrame* _InvocationHistoryStackTop;
    gctools::GCRootsInModule*  _GCRoots;
    void* _sigaltstack_buffer;
    stack_t _original_stack;
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
    inline core::DynamicBindingStack& bindings() { return this->_Bindings; };
    inline ExceptionStack& exceptionStack() { return this->_ExceptionStack; };
    Bignum_sp bigRegister0() { return this->_BignumRegister0; };
    Bignum_sp bigRegister1() { return this->_BignumRegister1; };
    Bignum_sp bigRegister2() { return this->_BignumRegister2; };
    ~ThreadLocalState();
  };

};


namespace gctools {

#ifdef DEBUG_COUNT_ALLOCATIONS
  void maybe_initialize_mythread_backtrace_allocations();
  void start_backtrace_allocations(const std::string& filename, Fixnum stamp);
  void stop_backtrace_allocations();
#endif

  void registerBytesAllocated(size_t bytes);
};




#endif
