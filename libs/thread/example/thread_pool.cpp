// Copyright (C) 2012-2013 Vicente Botet
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#define BOOST_THREAD_VERSION 4
#define BOOST_THREAD_USES_LOG
#define BOOST_THREAD_USES_LOG_THREAD_ID
#define BOOST_THREAD_QUEUE_DEPRECATE_OLD

#include <boost/thread/detail/log.hpp>
#include <boost/thread/thread_pool.hpp>
#include <boost/assert.hpp>
#include <string>

void p1()
{
  BOOST_THREAD_LOG
    << boost::this_thread::get_id()  << " P1" << BOOST_THREAD_END_LOG;
}

void p2()
{
  BOOST_THREAD_LOG
    << boost::this_thread::get_id()  << " P2" << BOOST_THREAD_END_LOG;
}

void submit_some(boost::thread_pool& tp) {
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
  BOOST_THREAD_LOG
    << boost::this_thread::get_id()  << " <MAIN" << BOOST_THREAD_END_LOG;
  {
    try
    {
      boost::thread_pool tp;
      submit_some(tp);
    }
    catch (std::exception& ex)
    {
      BOOST_THREAD_LOG
        << "ERRORRRRR " << ex.what() << "" << BOOST_THREAD_END_LOG;
      return 1;
    }
    catch (...)
    {
      BOOST_THREAD_LOG
        << " ERRORRRRR exception thrown" << BOOST_THREAD_END_LOG;
      return 2;
    }
  }
  BOOST_THREAD_LOG
    << boost::this_thread::get_id()  << "MAIN>" << BOOST_THREAD_END_LOG;
  return 0;
}
