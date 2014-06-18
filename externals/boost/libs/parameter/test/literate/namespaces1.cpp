
#line 2100 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
#line 2085 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
#include <boost/parameter.hpp>
#include <iostream>
#line 2071 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
namespace lib
{
  BOOST_PARAMETER_NAME(name)
  BOOST_PARAMETER_NAME(index)

  BOOST_PARAMETER_FUNCTION(
    (int), f, tag,
    (optional (name,*,"bob")(index,(int),1))
  )
  {
      std::cout << name << ":" << index << std::endl;
      return index;
  }
}
#line 2111 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
using lib::_name;
using lib::_index;

int x = lib::f(_name = "jill", _index = 1);
#line 2120 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
int main() {}

