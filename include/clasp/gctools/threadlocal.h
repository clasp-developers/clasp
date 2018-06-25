#ifndef gctools_threadlocal_H
#define gctools_threadlocal_H


namespace core {
#define IHS_BACKTRACE_SIZE 16
  struct InvocationHistoryFrame;
  struct ThreadLocalState {
    ThreadLocalState(void* stack_top);
    void initialize_thread(mp::Process_sp process);
    int _DisableInterrupts;
#if defined(DEBUG_RECURSIVE_ALLOCATIONS)
    int _RecursiveAllocationCounter;
#endif
    mp::Process_sp _Process;
    uint64_t  _Tid;
    void* _StackTop;
    DynamicBindingStack _Bindings;
    ExceptionStack _ExceptionStack;
    MultipleValues _MultipleValues;
    BignumExportBuffer _AsInt64Buffer;
    BignumExportBuffer _AsUint64Buffer;
    const InvocationHistoryFrame* _InvocationHistoryStackTop;
#ifdef DEBUG_IHS
    // Save the last return address before IHS screws up
    void*                    _IHSBacktrace[IHS_BACKTRACE_SIZE];
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


namespace core {
 
};




#endif
