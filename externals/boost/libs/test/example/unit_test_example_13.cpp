#define BOOST_TEST_MODULE system call test example
#include <boost/test/included/unit_test.hpp>

BOOST_AUTO_TEST_CASE( broken_test )
{
  BOOST_CHECK_EQUAL( ::system("ls"), 0 );
}

