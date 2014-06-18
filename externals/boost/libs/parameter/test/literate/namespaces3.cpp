
#line 2161 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
#include <boost/parameter.hpp>
#include <iostream>
#line 2143 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
namespace lib
{
  namespace keywords
  {
     BOOST_PARAMETER_NAME(name)
     BOOST_PARAMETER_NAME(index)
  }

  BOOST_PARAMETER_FUNCTION(
    (int), f, keywords::tag,
    (optional (name,*,"bob")(index,(int),1))
  )
  {
      std::cout << name << ":" << index << std::endl;
      return index;
  }
}

#line 2170 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
using namespace lib::keywords;
int y = lib::f(_name = "bob", _index = 2);
#line 2172 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
int main() {}

