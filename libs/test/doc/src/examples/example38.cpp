#define BOOST_TEST_MODULE example
#include <boost/test/included/unit_test.hpp>

//____________________________________________________________________________//

#include <cmath>

BOOST_AUTO_TEST_CASE( test )
{
    double res = std::sin( 45. ); // sin 45 radians is actually ~ 0.85, sin 45 degrees is ~0.707

    BOOST_WARN_MESSAGE( res < 0.71, "sin(45){" << res << "} is > 0.71. Arg is not in radian?" );
}

//____________________________________________________________________________//
