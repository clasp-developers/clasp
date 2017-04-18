/*
    File: mpPackage.h
*/

/*
Copyright (c) 2017, Christian E. Schafmeister
Copyright (c) 2017, Frank Goenninger, Goenninger B&T UG, Germany

CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

See directory 'clasp/licenses' for full details.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */

#if !defined( __CLASP_MP_QUEUE_H__ )
#define __CLASP_MP_QUEUE_H__

#include <clasp/core/foundation.h>
#include <clasp/core/queue.fwd.h>

namespace mp {

  FORWARD(Queue);

  class Queue_O : public core::CxxObject_O
  {
    LISP_CLASS( mp, MpPkg, Queue_O, "Queue", core::CxxObject_O );

  public: // LISP METHODS

    CL_LISPIFY_NAME("%enqueue");
    CL_LAMBDA(value queue);
    CL_DOCSTRING("doc(Adds VALUE to the end of QUEUE. Returns VALUE.)doc");
    CL_DEF_CLASS_METHOD static core::Values_sp PERCENTenqueue( core::T_sp value );

    CL_LISPIFY_NAME("%dequeue");
    CL_LAMBDA(queue);
    CL_DOCSTRING("doc(Retrieves the oldest value in QUEUE and returns it as the primary value, and T as secondary value. If the queue is empty, returns NIL as both primary and secondary value.)doc");
    CL_DEF_CLASS_METHOD static core::Values_sp PERCENTdequeue( void );

    CL_LISPIFY_NAME("%count");
    CL_LAMBDA(queue);
    CL_DOCSTRING("doc(Returns the number of objects in QUEUE. Mainly useful for manual examination of queue state, and in PRINT-OBJECT methods: inefficient as it must walk the entire queue.)doc");
    CL_DEF_CLASS_METHOD static core::Integer_sp PERCENTcount( void );

  public: // SLOTS

    core::Str_sp                               _Name;
    moodycamel::ConcurrentQueue< core::T_sp >  _Queue;
    core::ThreadLocalState *                   _ThreadInfo;

  public: // CONSSTRUCTORS

    Queue_O( core::T_sp name, core::List_sp initial_contents = _Nil<core::T_O>() );

  public: // C++ METHODS

    static Queue_sp create( std::string name );
    static Queue_sp create( core::T_sp name );

    inline uint64_t count( void )
    {
      return this->_Queue.size_approx();
    };

    string __repr__() const;

  };

  Queue_sp make_queue( std::string name );

  CL_LISPIFY_NAME("%make_queue");
  CL_LAMBDA(&key name initial-contents);
  CL_DOCSTRING("doc(Returns a new QUEUE with NAME.)doc");
  CL_DEFUN Queue_sp PERCENTmake_queue( core::T_sp name );

};

// GARBAGE COLLECTION CONFIG

  template <>
    struct gctools::GCInfo<mp::Queue_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = true;
    static GCInfo_policy constexpr Policy = normal;
  };

#endif
