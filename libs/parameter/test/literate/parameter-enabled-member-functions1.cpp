
#line 1177 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
#include <boost/parameter.hpp>

BOOST_PARAMETER_NAME(arg1)
BOOST_PARAMETER_NAME(arg2)
using namespace boost::parameter;
#line 1166 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
struct callable2
{
    BOOST_PARAMETER_CONST_MEMBER_FUNCTION(
        (void), call, tag, (required (arg1,(int))(arg2,(int))))
    {
        call_impl(arg1,arg2);
    }
 private:
    void call_impl(int, int); // implemented elsewhere.
};
