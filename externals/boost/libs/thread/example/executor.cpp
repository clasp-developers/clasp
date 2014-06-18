// Copyright (C) 2012-2013 Vicente Botet
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#define BOOST_THREAD_VERSION 4
#define BOOST_THREAD_PROVIDES_EXECUTORS
#define BOOST_THREAD_USES_LOG_THREAD_ID
#define BOOST_THREAD_QUEUE_DEPRECATE_OLD

#include <boost/thread/thread_pool.hpp>
#include <boost/thread/user_scheduler.hpp>
#include <boost/thread/executor.hpp>
#include <boost/thread/future.hpp>
#include <boost/assert.hpp>
#include <string>
#include <boost/thread/caller_context.hpp>

void p1()
{
  std::cout << BOOST_CONTEXTOF << std::endl;
}

void p2()
{
  std::cout << BOOST_CONTEXTOF << std::endl;
}

int f1()
{
  std::cout << BOOST_CONTEXTOF << std::endl;
  boost::this_thread::sleep_for(boost::chrono::seconds(1));
  return 1;
}
int f2(int i)
{
  std::cout << BOOST_CONTEXTOF << std::endl;
  boost::this_thread::sleep_for(boost::chrono::seconds(2));
  return i + 1;
}

void submit_some(boost::executor& tp)
{
  tp.submit(&p1);
  tp.submit(&p2);
  tp.submit(&p1);
  tp.submit(&p2);
  tp.submit(&p1);
  tp.submit(&p2);
  tp.submit(&p1);
  tp.submit(&p2);
  tp.submit(&p1);
  tp.submit(&p2);
}

int main()
{
  std::cout << BOOST_CONTEXTOF << std::endl;
  {
    try
    {
      boost::executor_adaptor<boost::thread_pool> ea;
      submit_some(ea);
      {
        boost::future<int> t1 = boost::async(ea, &f1);
        boost::future<int> t2 = boost::async(ea, &f1);
        std::cout << BOOST_CONTEXTOF << " t1= " << t1.get() << std::endl;
        std::cout << BOOST_CONTEXTOF << " t2= " << t2.get() << std::endl;
      }
      submit_some(ea);
      {
        boost::thread_pool ea3(1);
        boost::future<int> t1 = boost::async(ea3, &f1);
        boost::future<int> t2 = boost::async(ea3, &f1);
        //boost::future<int> t2 = boost::async(ea3, f2, 1); // todo this doesn't compiles yet on C++11
        //boost::future<int> t2 = boost::async(ea3, boost::bind(f2, 1)); // todo this doesn't compiles yet on C++98
        std::cout << BOOST_CONTEXTOF << " t1= " << t1.get() << std::endl;
        std::cout << BOOST_CONTEXTOF << " t2= " << t2.get() << std::endl;
      }
      submit_some(ea);

      boost::executor_adaptor<boost::user_scheduler> ea2;
      submit_some(ea2);
      ea2.underlying_executor().run_queued_closures();

    }
    catch (std::exception& ex)
    {
      std::cout << "ERROR= " << ex.what() << "" << std::endl;
      return 1;
    }
    catch (...)
    {
      std::cout << " ERROR= exception thrown" << std::endl;
      return 2;
    }
  }
  std::cout << BOOST_CONTEXTOF << std::endl;
  return 0;
}
