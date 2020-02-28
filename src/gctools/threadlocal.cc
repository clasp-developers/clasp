
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


THREAD_LOCAL gctools::ThreadLocalStateLowLevel* my_thread_low_level;
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

size_t DynamicBindingStack::new_binding_index() const
{
#ifdef CLASP_THREADS
  RAIILock<mp::Mutex> mutex(mp::global_BindingIndexPoolMutex);
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

void DynamicBindingStack::release_binding_index(size_t index) const
{
#ifdef CLASP_THREADS
  RAIILock<mp::Mutex> mutex(mp::global_BindingIndexPoolMutex);
  mp::global_BindingIndexPool.push_back(index);
#endif
};

// Ensure that a symbol's binding index is set to something coherent.
// NOTE: We can use memory_order_relaxed because (a) this is the only code in
// the system that deals with the _BindingIdx, and (b) the only guarantee we
// should need for this structure is modification order consistency.
uint32_t DynamicBindingStack::ensure_binding_index(const Symbol_O* var) const {
  uint32_t no_binding = NO_THREAD_LOCAL_BINDINGS;
  uint32_t binding_index = var->_BindingIdx.load(std::memory_order_relaxed);
  if (binding_index == no_binding) {
    // Get a new index and try to exchange it in.
    uint32_t new_index = this->new_binding_index();
    if (!(var->_BindingIdx.compare_exchange_strong(no_binding, new_index,
                                                   std::memory_order_relaxed))) {
      // Some other thread has beat us. That's fine - just use theirs (which is
      // now in no_binding), and release the one we just grabbed.
      this->release_binding_index(new_index);
      return no_binding;
    } else return new_index;
  } else return binding_index;
}

T_sp* DynamicBindingStack::thread_local_reference(const uint32_t index) const {
  unlikely_if (index >= this->_ThreadLocalBindings.size())
    this->_ThreadLocalBindings.resize(index+1,_NoThreadLocalBinding<T_O>());
  return &(this->_ThreadLocalBindings[index]);
}

T_sp DynamicBindingStack::thread_local_value(const Symbol_O* sym) const {
  // TODO: Rearrange this - in all cases, ensure_binding_index has already been called,
  // and should not be necessary.
  return *thread_local_reference(ensure_binding_index(sym));
}

void DynamicBindingStack::set_thread_local_value(T_sp value, const Symbol_O* sym) {
  *thread_local_reference(ensure_binding_index(sym)) = value;
}

bool DynamicBindingStack::thread_local_boundp(const Symbol_O* sym) const {
  uint32_t index = sym->_BindingIdx.load(std::memory_order_relaxed);
  if (index == NO_THREAD_LOCAL_BINDINGS) return false;
  else if (index >= this->_ThreadLocalBindings.size()) return false;
  else if (gctools::tagged_no_thread_local_bindingp(_ThreadLocalBindings[index].raw_()))
    return false;
  else return true;
}

};

namespace gctools {
ThreadLocalStateLowLevel::ThreadLocalStateLowLevel(void* stack_top) :
  _DisableInterrupts(false)
  ,  _StackTop(stack_top)
#ifdef DEBUG_RECURSIVE_ALLOCATIONS
  , _RecursiveAllocationCounter(0)
#endif
  
{};

ThreadLocalStateLowLevel::~ThreadLocalStateLowLevel()
{};

};
namespace core {

ThreadLocalState::ThreadLocalState() :
  _unwinds(0)
  , _stackmap(0)
  , _stackmap_size(0)
  , _PendingInterrupts(_Nil<core::T_O>())
  , _CatchTags(_Nil<core::T_O>())
  , _ObjectFileStartUp(NULL)
  , _CleanupFunctions(NULL)
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


void thread_local_register_cleanup(const std::function<void(void)>& cleanup)
{
  CleanupFunctionNode* node = new CleanupFunctionNode(cleanup,my_thread->_CleanupFunctions);
  my_thread->_CleanupFunctions = node;
}


void thread_local_invoke_and_clear_cleanup() {
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
inline void registerTypesAllocated(size_t bytes) {
  my_thread->_BytesAllocated += bytes;
}

void ThreadLocalState::initialize_thread(mp::Process_sp process, bool initialize_GCRoots=true ) {
  if (initialize_GCRoots) {
    // The main process needs to initialize _GCRoots before classes are initialized.
    this->_GCRoots = new gctools::GCRootsInModule();
  }
//  printf("%s:%d Initialize all ThreadLocalState things this->%p\n",__FILE__, __LINE__, (void*)this);
  this->_Process = process;
  process->_ThreadInfo = this;
  this->_BFormatStringOutputStream = gc::As<StringOutputStream_sp>(clasp_make_string_output_stream());
#ifdef CLASP_UNICODE
  this->_WriteToStringOutputStream = gc::As<StringOutputStream_sp>(clasp_make_string_output_stream(STRING_OUTPUT_STREAM_DEFAULT_SIZE,1));
#else
   this->_WriteToStringOutputStream = gc::As<StringOutputStream_sp>(clasp_make_string_output_stream());
#endif
  this->_BignumRegister0 = Bignum_O::create( (gc::Fixnum) 0);
  this->_BignumRegister1 = Bignum_O::create( (gc::Fixnum) 0);
  this->_BignumRegister2 = Bignum_O::create( (gc::Fixnum) 0);
  this->_SingleDispatchMethodCachePtr = gc::GC<Cache_O>::allocate();
  this->_SingleDispatchMethodCachePtr->setup(2, Lisp_O::SingleDispatchMethodCacheSize);
  this->_PendingInterrupts = _Nil<T_O>();
  this->_CatchTags = _Nil<T_O>();
  this->_SparePendingInterruptRecords = cl__make_list(clasp_make_fixnum(16),_Nil<T_O>());
};

void ThreadLocalState::create_sigaltstack() {
}

void ThreadLocalState::destroy_sigaltstack()
{
}

// Push a tag onto the list of active catches.
void ThreadLocalState::pushCatchTag(T_sp tag) {
  this->_CatchTags = Cons_O::create(tag, this->_CatchTags);
}

};



namespace gctools {

#ifdef DEBUG_COUNT_ALLOCATIONS
void maybe_initialize_mythread_backtrace_allocations()
{
  char *backtraceStamp = getenv("CLASP_BACKTRACE_ALLOCATIONS");
  if (backtraceStamp) {
    stringstream ss;
    ss << "/tmp/stamp" << backtraceStamp << ".backtraces";
    Fixnum stamp = strtol(backtraceStamp, &backtraceStamp, 10);
    start_backtrace_allocations(ss.str(),stamp);
    printf("%s:%d Starting backtrace_allocations to file %s for stamp %" PFixnum "\n", __FILE__, __LINE__, ss.str().c_str(), stamp );
  }
}
#endif

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
