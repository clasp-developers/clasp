
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <fcntl.h>
#include <sys/types.h>
#include <signal.h>
#include <execinfo.h>
#include <clasp/core/foundation.h>
#include <clasp/gctools/threadlocal.h>
#include <clasp/core/lisp.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/array.h>
#include <clasp/core/debugger.h>
#include <clasp/core/lispStream.h>


THREAD_LOCAL core::ThreadLocalState* my_thread;

namespace core {
unsigned int *BignumExportBuffer::getOrAllocate(const mpz_class &bignum, int nail) {
  size_t size = _lisp->integer_ordering()._mpz_import_size;
  size_t numb = (size << 3) - nail; // *8
  size_t count = (mpz_sizeinbase(bignum.get_mpz_t(), 2) + numb - 1) / numb;
  size_t bytes = count * size;
  if (bytes > this->bufferSize) {
    if (this->buffer) {
      free(this->buffer);
    }
    this->buffer = (unsigned int *)malloc(bytes);
  }
  return this->buffer;
};





};




namespace core {

size_t DynamicBindingStack::new_binding_index()
{
#ifdef CLASP_THREADS
  RAIILock<mp::GlobalMutex> mutex(mp::global_BindingIndexPoolMutex);
  if ( mp::global_BindingIndexPool.size() != 0 ) {
    size_t index = mp::global_BindingIndexPool.back();
    mp::global_BindingIndexPool.pop_back();
    return index;
  }
  return mp::global_LastBindingIndex.fetch_add(1);
#else
  return 0;
#endif
};

void DynamicBindingStack::release_binding_index(size_t index)
{
#ifdef CLASP_THREADS
  RAIILock<mp::GlobalMutex> mutex(mp::global_BindingIndexPoolMutex);
  mp::global_BindingIndexPool.push_back(index);
#endif
};

T_sp* DynamicBindingStack::reference_raw_(Symbol_O* var,T_sp* globalValuePtr) {
#ifdef CLASP_THREADS
  if ( var->_Binding == NO_THREAD_LOCAL_BINDINGS ) {
    return globalValuePtr;
  }
  uintptr_clasp_t index = var->_Binding;
  // If it has a _Binding value but our table is not big enough, then expand the table.
  unlikely_if (index >= this->_ThreadLocalBindings.size()) {
    this->_ThreadLocalBindings.resize(index+1,_NoThreadLocalBinding<T_O>());
  }
  if (gctools::tagged_no_thread_local_bindingp(this->_ThreadLocalBindings[index].raw_())) {
    return globalValuePtr;
  }
  return &this->_ThreadLocalBindings[index];
#else
  return globalValuePtr;
#endif
}

const T_sp* DynamicBindingStack::reference_raw_(const Symbol_O* var,const T_sp* globalValuePtr) const{
#ifdef CLASP_THREADS
  if ( var->_Binding == NO_THREAD_LOCAL_BINDINGS ) {
    return globalValuePtr;
  }
  uintptr_clasp_t index = var->_Binding;
  // If it has a _Binding value but our table is not big enough, then expand the table.
  unlikely_if (index >= this->_ThreadLocalBindings.size()) {
    this->_ThreadLocalBindings.resize(index+1,_NoThreadLocalBinding<T_O>());
  }
  if (gctools::tagged_no_thread_local_bindingp(this->_ThreadLocalBindings[index].raw_())) {
    return globalValuePtr;
  }
  return &this->_ThreadLocalBindings[index];
#else
  return globalValuePtr;
#endif
}

SYMBOL_EXPORT_SC_(CorePkg,STARwatchDynamicBindingStackSTAR);
void DynamicBindingStack::push_with_value_coming(Symbol_sp var, T_sp* globalValuePtr) {
  T_sp* current_value_ptr = this->reference(var,globalValuePtr);
#ifdef CLASP_THREADS
  if ( var->_Binding == NO_THREAD_LOCAL_BINDINGS )
    var->_Binding = this->new_binding_index();
  uintptr_clasp_t index = var->_Binding;
  // If it has a _Binding value but our table is not big enough, then expand the table.
  unlikely_if (index >= this->_ThreadLocalBindings.size()) {
    this->_ThreadLocalBindings.resize(index+1,_NoThreadLocalBinding<T_O>());
  }
#ifdef DEBUG_DYNAMIC_BINDING_STACK // debugging
  if (  _sym_STARwatchDynamicBindingStackSTAR &&
       _sym_STARwatchDynamicBindingStackSTAR->boundP() &&
       _sym_STARwatchDynamicBindingStackSTAR->symbolValue().notnilp() ) {
    printf("%s:%d  DynamicBindingStack::push_with_value_coming[%zu] of %s\n", __FILE__, __LINE__, this->_Bindings.size(), var->formattedName(true).c_str());
  }
#endif
  this->_Bindings.emplace_back(var,this->_ThreadLocalBindings[index]);
  this->_ThreadLocalBindings[index] = *current_value_ptr;
#else
  this->_Bindings.emplace_back(var,var->symbolValueUnsafe());
#endif
}


void DynamicBindingStack::push_binding(Symbol_sp var, T_sp* globalValuePtr, T_sp value) {
#ifdef CLASP_THREADS
  if ( var->_Binding == NO_THREAD_LOCAL_BINDINGS )
    var->_Binding = this->new_binding_index();
  uintptr_clasp_t index = var->_Binding;
  // If it has a _Binding value but our table is not big enough, then expand the table.
  unlikely_if (index >= this->_ThreadLocalBindings.size()) {
    this->_ThreadLocalBindings.resize(index+1,_NoThreadLocalBinding<T_O>());
  }
#ifdef DEBUG_DYNAMIC_BINDING_STACK // debugging
  if (  _sym_STARwatchDynamicBindingStackSTAR &&
       _sym_STARwatchDynamicBindingStackSTAR->boundP() &&
       _sym_STARwatchDynamicBindingStackSTAR->symbolValue().notnilp() ) {
    printf("%s:%d  DynamicBindingStack::push_binding[%zu] of %s\n", __FILE__, __LINE__, this->_Bindings.size(), var->formattedName(true).c_str());
  }
#endif
  this->_Bindings.emplace_back(var,this->_ThreadLocalBindings[index]);
  this->_ThreadLocalBindings[index] = value;
#else
  this->_Bindings.emplace_back(var,var->symbolValueUnsafe());
  this->_GlobalValue = value;
#endif
}



void DynamicBindingStack::pop_binding() {
  DynamicBinding &bind = this->_Bindings.back();
#ifdef DEBUG_DYNAMIC_BINDING_STACK // debugging
  if (  _sym_STARwatchDynamicBindingStackSTAR &&
       _sym_STARwatchDynamicBindingStackSTAR->boundP() &&
       _sym_STARwatchDynamicBindingStackSTAR->symbolValue().notnilp() ) {
#if 0
    List_sp assoc = cl__assoc(bind._Var,_sym_STARwatchDynamicBindingStackSTAR->symbolValue(),_Nil<T_O>());
    if ( assoc.notnilp() ) {
      T_sp funcDesig = oCdr(assoc);
      if ( funcDesig.notnilp() ) {
        eval::funcall(funcDesig,bind._Var,_Nil<T_O>());
      } else {
        printf("%s:%d  *watch-dynamic-binding-stack* caught pop[%zu] of %s  overwriting value = %s\n", __FILE__, __LINE__, this->_Bindings.size()-1, _rep_(bind._Var).c_str(), _rep_(bind._Var->symbolValue()).c_str() );
      }
    }
#endif
    printf("%s:%d  DynamicBindingStack::pop_binding[%lu]  %s\n", __FILE__, __LINE__, this->_Bindings.size(),bind._Var->formattedName(true).c_str());
  }
#endif
#ifdef CLASP_THREADS
  ASSERT(this->_ThreadLocalBindings.size()>bind._Var->_Binding); 
  this->_ThreadLocalBindings[bind._Var->_Binding] = bind._Val;
  this->_Bindings.pop_back();
#else
  bind._Var->setf_symbolValue(bind._Val);
  this->_Bindings.pop_back();
#endif
}


};

namespace core {


ThreadLocalState::ThreadLocalState(void* stack_top) :  _DisableInterrupts(false), _StackTop(stack_top), _PendingInterrupts(_Nil<core::T_O>())
#ifdef DEBUG_RECURSIVE_ALLOCATIONS
                                                    , _RecursiveAllocationCounter(0)
#endif
{
  my_thread = this;
#ifdef _TARGET_OS_DARWIN
  pthread_threadid_np(NULL, &this->_Tid);
#else
  this->_Tid = 0;
#endif
  this->_InvocationHistoryStackTop = NULL;
  this->_BufferStr8NsPool.reset_(); // Can't use _Nil<core::T_O>(); - too early
  this->_BufferStrWNsPool.reset_();
}

ThreadLocalState::~ThreadLocalState() {
}
  
void ThreadLocalState::initialize_thread(mp::Process_sp process, bool initialize_GCRoots=true ) {
  if (initialize_GCRoots) {
    // The main process needs to initialize _GCRoots before classes are initialized.
    this->_GCRoots = new gctools::GCRootsInModule();
  }
//  printf("%s:%d Initialize all ThreadLocalState things this->%p\n",__FILE__, __LINE__, (void*)this);
  this->_Bindings.reserve(1024);
  this->_Process = process;
  process->_ThreadInfo = this;
  this->_BFormatStringOutputStream = clasp_make_string_output_stream();
  this->_WriteToStringOutputStream = clasp_make_string_output_stream();
  this->_BignumRegister0 = Bignum_O::create( (gc::Fixnum) 0);
  this->_BignumRegister1 = Bignum_O::create( (gc::Fixnum) 0);
  this->_BignumRegister2 = Bignum_O::create( (gc::Fixnum) 0);
  this->_SingleDispatchMethodCachePtr = gc::GC<Cache_O>::allocate();
  this->_SingleDispatchMethodCachePtr->setup(2, Lisp_O::SingleDispatchMethodCacheSize);
  this->_PendingInterrupts = _Nil<T_O>();
  this->_SparePendingInterruptRecords = cl__make_list(clasp_make_fixnum(16),_Nil<T_O>());
};

void ThreadLocalState::create_sigaltstack() {
#if 0
  // Set up a sigaltstack
  stack_t sigstk;
  size_t size = SIGNAL_STACK_SIZE+SIGSTKSZ;
  my_thread->_sigaltstack_buffer = (void*)malloc(size);
  if (!my_thread->_sigaltstack_buffer) perror("Could not allocate signal stack");
  sigstk.ss_size = SIGNAL_STACK_SIZE+SIGSTKSZ;
  sigstk.ss_flags = 0;
  if (sigaltstack(&sigstk,&my_thread->_original_stack) < 0) perror("sigaltstack problem");
#endif
}

void ThreadLocalState::destroy_sigaltstack()
{
#if 0
  if (sigaltstack(&my_thread->_original_stack, (stack_t *)0) < 0) perror("sigaltstack problem");
  free(my_thread->_sigaltstack_buffer);
#endif
}


};



namespace gctools {

#ifdef DEBUG_COUNT_ALLOCATIONS
void start_backtrace_allocations(const std::string& filename, Fixnum stamp) {
  int fd = open(filename.c_str(),O_WRONLY|O_CREAT,S_IRWXU);
  if (fd<0) {
    SIMPLE_ERROR(BF("Could not open file %s - %s") % filename % strerror(errno));
  }
  my_thread->_BacktraceStamp = stamp;
  my_thread->_BacktraceFd = fd;
  my_thread->_BacktraceAllocationsP = true;
}

void stop_backtrace_allocations() {
  close(my_thread->_BacktraceFd);
  my_thread->_BacktraceAllocationsP = false;
}

void count_allocation(stamp_t stamp) {
  if (my_thread->_CountAllocations.size() <= stamp) {
    my_thread->_CountAllocations.resize(stamp+1,0);
  }
  if (my_thread->_BacktraceAllocationsP) {
    if (my_thread->_BacktraceStamp == stamp) {
      void** buffer = NULL;
      int nptrs = core::safe_backtrace(buffer);
      backtrace_symbols_fd(buffer,nptrs,my_thread->_BacktraceFd);
      write(my_thread->_BacktraceFd,"\n",strlen("\n"));
    }
  }
  my_thread->_CountAllocations[stamp]++;
}

CL_DEFUN core::SimpleVector_sp gctools__allocation_counts()
{
  core::SimpleVector_sp counts = core::core__make_vector(_lisp->_true(),my_thread->_CountAllocations.size());
  for ( size_t i=0; i<my_thread->_CountAllocations.size(); ++i ) {
    (*counts)[i] = core::make_fixnum(my_thread->_CountAllocations[i]);
  }
  return counts;
}
  
#endif

};
