
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
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/code.h>


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
    this->_ThreadLocalBindings.resize(index+1,no_thread_local_binding<T_O>());
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

bool DynamicBindingStack::thread_local_boundp(int32_t index) const {
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

// For main thread initialization - it happens too early and _Nil is undefined
// So this partially sets up the ThreadLocalState and the system must invoke
// ThreadLocalState::finish_initialization_main_thread() after the Nil symbol is
// in GC managed memory.
ThreadLocalState::ThreadLocalState(bool dummy) :
  _unwinds(0)
  , _CleanupFunctions(NULL)
  ,_PendingInterrupts()
  ,_CatchTags()
  ,_ObjectFiles()
  ,_BufferStr8NsPool()
  ,_BufferStrWNsPool()
{
  my_thread = this;
#ifdef _TARGET_OS_DARWIN
  pthread_threadid_np(NULL, &this->_Tid);
#else
  this->_Tid = 0;
#endif
  this->_xorshf_x = rand();
  this->_xorshf_y = rand();
  this->_xorshf_z = rand();
}

// This needs to be called at initialization immediately after Nil is allocated
// AND during image load once Nil is found in the image and relocated to its
// new position in the GC managed memory.
void ThreadLocalState::finish_initialization_main_thread(core::T_sp theNilObject) {
  if (!theNilObject.raw_()) {
    printf("%s:%d:%s reinitialize symbols the _Nil object is not defined!!!\n", __FILE__, __LINE__, __FUNCTION__ );
    abort();
  }
//  printf("%s:%d:%s reinitialize symbols here once _Nil is defined\n", __FILE__, __LINE__, __FUNCTION__ );
  // Reinitialize all threadlocal lists once NIL is defined
  // We work with theObject here directly because it's very early in the bootstrapping
  if (this->_PendingInterrupts.theObject) goto ERR;
  if (this->_CatchTags.theObject) goto ERR;
  if (this->_ObjectFiles.theObject) goto ERR;
  if (this->_BufferStr8NsPool.theObject) goto ERR;
  if (this->_BufferStrWNsPool.theObject) goto ERR;
  this->_PendingInterrupts.theObject = theNilObject.theObject;
  this->_CatchTags.theObject = theNilObject.theObject;
  this->_ObjectFiles.theObject = theNilObject.theObject;
  this->_BufferStr8NsPool.theObject = theNilObject.theObject;
  this->_BufferStrWNsPool.theObject = theNilObject.theObject;
  return;
 ERR:
  printf("%s:%d:%s one of the reinitialize symbols was already initialized\n", __FILE__, __LINE__, __FUNCTION__ );
  abort();
};

// This is for constructing ThreadLocalState for threads
ThreadLocalState::ThreadLocalState() :
  _unwinds(0)
  , _PendingInterrupts(nil<core::T_O>())
  , _CatchTags(nil<core::T_O>())
  , _ObjectFiles(nil<core::T_O>())
  , _CleanupFunctions(NULL)
{
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
  } while (this->_xorshf_z==0);
  uint32_t rnd = this->_xorshf_z&0xFFFFFFFF;
  // printf("%s:%d:%s rnd = %u\n", __FILE__, __LINE__, __FUNCTION__, rnd );
  return rnd;
}

void ThreadLocalState::pushObjectFile(llvmo::ObjectFile_sp of) {
  this->_ObjectFiles = core::Cons_O::create(of,this->_ObjectFiles);
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
  SIMPLE_ERROR(BF("There were no more object files"));
}

ThreadLocalState::~ThreadLocalState() {
}


void thread_local_register_cleanup(const std::function<void(void)>& cleanup)
{
  CleanupFunctionNode* node = new CleanupFunctionNode(cleanup,my_thread->_CleanupFunctions);
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
inline void registerTypesAllocated(size_t bytes) {
  my_thread->_BytesAllocated += bytes;
}

void ThreadLocalState::initialize_thread(mp::Process_sp process, bool initialize_GCRoots=true ) {
//  printf("%s:%d Initialize all ThreadLocalState things this->%p\n",__FILE__, __LINE__, (void*)this);
  this->_Process = process;
  process->_ThreadInfo = this;
  this->_BFormatStringOutputStream = gc::As<StringOutputStream_sp>(clasp_make_string_output_stream());
#ifdef CLASP_UNICODE
  this->_WriteToStringOutputStream = gc::As<StringOutputStream_sp>(clasp_make_string_output_stream(STRING_OUTPUT_STREAM_DEFAULT_SIZE,1));
#else
   this->_WriteToStringOutputStream = gc::As<StringOutputStream_sp>(clasp_make_string_output_stream());
#endif
  this->_PendingInterrupts = nil<T_O>();
  this->_CatchTags = nil<T_O>();
  this->_SparePendingInterruptRecords = cl__make_list(clasp_make_fixnum(16),nil<T_O>());
};

// Push a tag onto the list of active catches.
void ThreadLocalState::pushCatchTag(T_sp tag) {
  this->_CatchTags = Cons_O::create(tag, this->_CatchTags);
}

};


uint32_t my_thread_random() {
  return my_thread->random();
}

namespace gctools {

DOCGROUP(clasp)
CL_DEFUN size_t gctools__thread_local_unwinds()
{
  return
    my_thread->_unwinds;
}


};
