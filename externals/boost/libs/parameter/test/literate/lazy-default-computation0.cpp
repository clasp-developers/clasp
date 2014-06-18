
#line 1911 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
#include <boost/parameter.hpp>
#include <string>

namespace parameter = boost::parameter;
#line 1894 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
BOOST_PARAMETER_NAME(s1)
BOOST_PARAMETER_NAME(s2)
BOOST_PARAMETER_NAME(s3)

template <class ArgumentPack>
std::string f(ArgumentPack const& args)
{
    std::string const& s1 = args[_s1];
    std::string const& s2 = args[_s2];
    typename parameter::binding<
        ArgumentPack,tag::s3,std::string
    >::type s3 = args[_s3|(s1+s2)]; // always constructs s1+s2
    return s3;
}

std::string x = f((_s1="hello,", _s2=" world", _s3="hi world"));
#line 1917 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
int main()
{}

