// Copyright (C) 2013 Vicente J. Botet Escriba
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// 2013/09 Vicente J. Botet Escriba
//    Adapt to boost from CCIA C++11 implementation
//    first implementation of a simple pool thread using a vector of threads and a sync_queue.

#ifndef BOOST_THREAD_THREAD_POOL_HPP
#define BOOST_THREAD_THREAD_POOL_HPP

#include <boost/thread/detail/config.hpp>
#include <boost/thread/detail/delete.hpp>
#include <boost/thread/detail/move.hpp>
#include <boost/thread/scoped_thread.hpp>
#include <boost/thread/sync_queue.hpp>
#include <boost/thread/detail/work.hpp>
#include <boost/thread/csbl/vector.hpp>

#include <boost/config/abi_prefix.hpp>

namespace boost
{

  class thread_pool
  {
    /// type-erasure to store the works to do
    typedef  thread_detail::work work;
    /// the kind of stored threads are scoped threads to ensure that the threads are joined.
    /// A move aware vector type
    typedef scoped_thread<> thread_t;
    typedef csbl::vector<thread_t> thread_vector;

    /// the thread safe work queue
    sync_queue<work > work_queue;
    /// A move aware vector
    thread_vector threads;

  public:
    /**
     * Effects: try to execute one task.
     * Returns: whether a task has been executed.
     * Throws: whatever the current task constructor throws or the task() throws.
     */
    bool try_executing_one()
    {
      work task;
      try
      {
        if (work_queue.try_pull_front(task) == queue_op_status::success)
        {
          task();
          return true;
        }
        return false;
      }
      catch (std::exception& )
      {
        return false;
      }
      catch (...)
      {
        return false;
      }
    }
  private:
    /**
     * Effects: schedule one task or yields
     * Throws: whatever the current task constructor throws or the task() throws.
     */
    void schedule_one_or_yield()
    {
        if ( ! try_executing_one())
        {
          this_thread::yield();
        }
    }

    /**
     * The main loop of the worker threads
     */
    void worker_thread()
    {
      while (!closed())
      {
        schedule_one_or_yield();
      }
      while (try_executing_one())
      {
      }
    }

  public:
    /// thread_pool is not copyable.
    BOOST_THREAD_NO_COPYABLE(thread_pool)

    /**
     * \b Effects: creates a thread pool that runs closures on \c thread_count threads.
     *
     * \b Throws: Whatever exception is thrown while initializing the needed resources.
     */
    thread_pool(unsigned const thread_count = thread::hardware_concurrency())
    {
      try
      {
        threads.reserve(thread_count);
        for (unsigned i = 0; i < thread_count; ++i)
        {
#if 1
          thread th (&thread_pool::worker_thread, this);
          threads.push_back(thread_t(boost::move(th)));
#else
          threads.push_back(thread_t(&thread_pool::worker_thread, this));
#endif
        }
      }
      catch (...)
      {
        close();
        throw;
      }
    }
    /**
     * \b Effects: Destroys the thread pool.
     *
     * \b Synchronization: The completion of all the closures happen before the completion of the \c thread_pool destructor.
     */
    ~thread_pool()
    {
      // signal to all the worker threads that there will be no more submissions.
      close();
      // joins all the threads as the threads were scoped_threads
    }

    /**
     * \b Effects: close the \c thread_pool for submissions.
     * The worker threads will work until there is no more closures to run.
     */
    void close()
    {
      work_queue.close();
    }

    /**
     * \b Returns: whether the pool is closed for submissions.
     */
    bool closed()
    {
      return work_queue.closed();
    }

    /**
     * \b Requires: \c Closure is a model of \c Callable(void()) and a model of \c CopyConstructible/MoveConstructible.
     *
     * \b Effects: The specified \c closure will be scheduled for execution at some point in the future.
     * If invoked closure throws an exception the \c thread_pool will call \c std::terminate, as is the case with threads.
     *
     * \b Synchronization: completion of \c closure on a particular thread happens before destruction of thread's thread local variables.
     *
     * \b Throws: \c sync_queue_is_closed if the thread pool is closed.
     * Whatever exception that can be throw while storing the closure.
     */

#if defined(BOOST_NO_CXX11_RVALUE_REFERENCES)
    template <typename Closure>
    void submit(Closure & closure)
    {
      work w ((closure));
      work_queue.push_back(boost::move(w));
      //work_queue.push(work(closure)); // todo check why this doesn't work
    }
#endif
    void submit(void (*closure)())
    {
      work w ((closure));
      work_queue.push_back(boost::move(w));
      //work_queue.push_back(work(closure)); // todo check why this doesn't work
    }

    template <typename Closure>
    void submit(BOOST_THREAD_RV_REF(Closure) closure)
    {
      work w =boost::move(closure);
      work_queue.push_back(boost::move(w));
      //work_queue.push_back(work(boost::move(closure))); // todo check why this doesn't work
    }

    /**
     * \b Requires: This must be called from an scheduled task.
     *
     * \b Effects: reschedule functions until pred()
     */
    template <typename Pred>
    bool reschedule_until(Pred const& pred)
    {
      do {
        if ( ! try_executing_one())
        {
          return false;
        }
      } while (! pred());
      return true;
    }

  };

}

#include <boost/config/abi_suffix.hpp>

#endif
