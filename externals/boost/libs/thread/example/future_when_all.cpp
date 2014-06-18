// Copyright (C) 2012-2013 Vicente Botet
//
//  Distributed under the Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#define BOOST_THREAD_VERSION 4
#define BOOST_THREAD_USES_LOG
#define BOOST_THREAD_USES_LOG_THREAD_ID
#include <boost/config.hpp>

#if ! defined BOOST_THREAD_PROVIDES_FUTURE_WHEN_ALL_WHEN_ANY \
 && ! defined BOOST_THREAD_DONT_PROVIDE_FUTURE_WHEN_ALL_WHEN_ANY

#if ! defined(BOOST_NO_CXX11_VARIADIC_TEMPLATES) && \
    ! defined(BOOST_NO_CXX11_HDR_TUPLE)

#define BOOST_THREAD_PROVIDES_FUTURE_WHEN_ALL_WHEN_ANY
#endif
#endif

//    ! defined(BOOST_NO_SFINAE_EXPR) &&
//    ! defined(BOOST_NO_CXX11_RVALUE_REFERENCES) &&
//    ! defined(BOOST_NO_CXX11_AUTO) &&
//    ! defined(BOOST_NO_CXX11_DECLTYPE) &&
//    ! defined(BOOST_NO_CXX11_DECLTYPE_N3276) &&


#include <boost/thread/future.hpp>
#include <boost/thread/csbl/vector.hpp>
#include <boost/assert.hpp>
#include <boost/thread/detail/log.hpp>
#include <string>
#if defined BOOST_THREAD_PROVIDES_FUTURE_WHEN_ALL_WHEN_ANY

int p1()
{
  BOOST_THREAD_LOG
    << "P1" << BOOST_THREAD_END_LOG;
  boost::this_thread::sleep_for(boost::chrono::seconds(1));
  return 123;
}
int p1b()
{
  BOOST_THREAD_LOG
    << "P1b" << BOOST_THREAD_END_LOG;
  boost::this_thread::sleep_for(boost::chrono::seconds(1));
  return 321;
}

int p2(boost::future<int> f)
{
  BOOST_THREAD_LOG
    << " P2 " << BOOST_THREAD_END_LOG;
  try
  {
    return 2 * f.get();
  }
  catch (std::exception& ex)
  {
    BOOST_THREAD_LOG
      << "ERRORRRRR " << ex.what() << "" << BOOST_THREAD_END_LOG;
    BOOST_ASSERT(false);
  }
  catch (...)
  {
    BOOST_THREAD_LOG
      << " ERRORRRRR exception thrown" << BOOST_THREAD_END_LOG;
    BOOST_ASSERT(false);
  }
  BOOST_THREAD_LOG
    << "P2>" << BOOST_THREAD_END_LOG;
}
int p2s(boost::shared_future<int> f)
{
  BOOST_THREAD_LOG
    << "<P2" << BOOST_THREAD_END_LOG;
  try
  {
    return 2 * f.get();
  }
  catch (std::exception& ex)
  {
    BOOST_THREAD_LOG
      << "ERRORRRRR " << ex.what() << "" << BOOST_THREAD_END_LOG;
    BOOST_ASSERT(false);
  }
  catch (...)
  {
    BOOST_THREAD_LOG
      << " ERRORRRRR exception thrown" << BOOST_THREAD_END_LOG;
    BOOST_ASSERT(false);
  }
  BOOST_THREAD_LOG
    << "P2>" << BOOST_THREAD_END_LOG;
}

int main()
{
  BOOST_THREAD_LOG
    << "<MAIN" << BOOST_THREAD_END_LOG;
  {
    try
    {
      boost::future<int> f1 = boost::async(boost::launch::async, &p1);
      boost::future<int> f2 = boost::async(boost::launch::async, &p1b);
      boost::future<std::tuple<> > all0 = boost::when_all();
      boost::future<boost::csbl::vector<boost::future<int> > > all = boost::when_all(boost::move(f1), boost::move(f2));
      //(void) all.wait();
      boost::csbl::vector<boost::future<int> > res = all.get();
      BOOST_THREAD_LOG
        << res[0].get() <<" " << BOOST_THREAD_END_LOG;
      BOOST_THREAD_LOG
        << res[1].get() <<" " << BOOST_THREAD_END_LOG;
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
  {
    try
    {
      boost::future<int> f1 = boost::async(boost::launch::async, &p1);
      boost::future<int> f2 = boost::async(boost::launch::async, &p1b);
      boost::future<std::tuple<> > all0 = boost::when_any();
      boost::future<boost::csbl::vector<boost::future<int> > > all = boost::when_any(boost::move(f1), boost::move(f2));
      //(void) all.wait();
      boost::csbl::vector<boost::future<int> > res = all.get();
      BOOST_THREAD_LOG
        << res[0].get() <<" " << BOOST_THREAD_END_LOG;
      BOOST_THREAD_LOG
        << res[1].get() <<" " << BOOST_THREAD_END_LOG;
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
    << "MAIN>" << BOOST_THREAD_END_LOG;
  return 0;
}
#else
#include <boost/thread/csbl/vector.hpp>
using namespace boost;

void f(  boost::csbl::vector<future<int> > &//vec
    , BOOST_THREAD_RV_REF(future<int>) //f
    ) {
  //vec.push_back(boost::forward<future<int> >(f));
}
int main()
{
  boost::csbl::vector<future<int> > vec;
  f(vec, make_ready_future(0));
  return 0;
}
#endif
