#ifndef core_queue_H_
#define core_queue_H_


#if defined(malloc)
#error "We are going to redefine malloc/free as macros that use the GC allocators"
#else
namespace gctools {
  extern void* malloc_uncollectable_and_zero(size_t size);
};
  #ifdef USE_BOEHM
    #define malloc(size) reinterpret_cast<void*>(gctools::malloc_uncollectable_and_zero(size))
    #define free(ptr) GC_FREE(ptr)
  #endif
  #ifdef USE_MPS
    #define malloc(size) NULL /* printf("%s:%d IMPLEMENT malloc(%lu)for mps\n", __FILE__, __LINE__, size);abort(); */
    #define free(ptr) /* nothing */
  #endif
#endif

#include <clasp/external/concurrentqueue/concurrentqueue.h>
#include <clasp/external/concurrentqueue/blockingconcurrentqueue.h>

#include <clasp/core/mpPackage.fwd.h>


namespace mp {

  struct TQueueElement {
    core::T_sp    _Value;
    explicit TQueueElement() {};
    explicit TQueueElement(core::T_sp value) : _Value(value) {};
    ~TQueueElement() {
      _Value.reset_();
    }
  };
      
  FORWARD(ConcurrentQueue);
  class ConcurrentQueue_O : public core::General_O {
    LISP_CLASS(mp, MpPkg, ConcurrentQueue_O, "ConcurrentQueue",core::General_O);
    ConcurrentQueue_O() {};
    virtual ~ConcurrentQueue_O(){
      //this->_Queue;
    };
  //	DEFAULT_CTOR_DTOR(ConcurrentQueue_O);
  protected: // instance variables here
    moodycamel::ConcurrentQueue<TQueueElement>   _Queue;
  public:
    static ConcurrentQueue_sp create(core::T_sp test); // set everything up with defaults
  public:
    CL_DEFMETHOD   bool queue_enqueue(core::T_sp object) {
      TQueueElement element(object);
      return this->_Queue.enqueue(element);
    };

    CL_DOCSTRING("Try to dequeue the next entry.  Return (values entry bool) where the second value is T if a value was available.");
    CL_DEFMETHOD   core::T_mv queue_dequeue() {
      TQueueElement element;
      bool dequeued = this->_Queue.try_dequeue(element);
      unlikely_if (!dequeued) {
        return Values(_Nil<core::T_O>(),_Nil<core::T_O>());
      }
      return Values(element._Value,_lisp->_true());
    };
      

    CL_DEFMETHOD size_t queue_size_approximate() {
      return this->_Queue.size_approx();
    }

    CL_DEFMETHOD bool  queue_is_lock_free() {
      return this->_Queue.is_lock_free();
    }
    
  };


  FORWARD(BlockingConcurrentQueue);
  class BlockingConcurrentQueue_O : public core::General_O {
    LISP_CLASS(mp, MpPkg, BlockingConcurrentQueue_O, "BlockingConcurrentQueue",core::General_O);
    BlockingConcurrentQueue_O() {};
    virtual ~BlockingConcurrentQueue_O(){
      //this->_Queue;
    };
  //	DEFAULT_CTOR_DTOR(BlockingConcurrentQueue_O);
  protected: // instance variables here
    moodycamel::BlockingConcurrentQueue<TQueueElement>   _Queue;
  public:
    static BlockingConcurrentQueue_sp create(core::T_sp test); // set everything up with defaults
  public:
    CL_DEFMETHOD   bool queue_enqueue(core::T_sp object) {
      TQueueElement element(object);
      return this->_Queue.enqueue(element);
    };

    CL_DOCSTRING("Try to dequeue the next entry.  Return (values entry bool) where the second value is T if a value was available.");
    CL_DEFMETHOD   core::T_mv queue_wait_dequeue_timed(core::T_sp wait_time_milliseconds) {
      TQueueElement element;
      bool dequeued;
      if (wait_time_milliseconds.fixnump()) {
        dequeued = this->_Queue.wait_dequeue_timed(element,std::chrono::milliseconds(wait_time_milliseconds.unsafe_fixnum()));
      } else {
        this->_Queue.wait_dequeue(element);
        dequeued = true;
      }
      unlikely_if (!dequeued) {
        return Values(_Nil<core::T_O>(),_Nil<core::T_O>());
      }
      return Values(element._Value,_lisp->_true());
    };

    CL_DOCSTRING("Try to dequeue the next entry.  Return (values entry bool) where the second value is T if a value was available.");
    CL_DEFMETHOD   core::T_sp queue_wait_dequeue() {
      TQueueElement element;
      this->_Queue.wait_dequeue(element);
      return element._Value;
    };


    CL_DEFMETHOD size_t queue_size_approximate() {
      return this->_Queue.size_approx();
    }

    CL_DEFMETHOD bool  queue_is_lock_free() {
      return this->_Queue.is_lock_free();
    }
    
  };
  

    };

#endif // core_queue_H_
