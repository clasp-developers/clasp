
#line 1248 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
#include <boost/parameter.hpp>
#include <iostream>
#line 1234 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
BOOST_PARAMETER_NAME(name)
BOOST_PARAMETER_NAME(index)

struct myclass_impl
{
    template <class ArgumentPack>
    myclass_impl(ArgumentPack const& args)
    {
        std::cout << "name = " << args[_name]
                  << "; index = " << args[_index | 42]
                  << std::endl;
    }
};

#line 1261 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
struct myclass : myclass_impl
{
    BOOST_PARAMETER_CONSTRUCTOR(
        myclass, (myclass_impl), tag
      , (required (name,*)) (optional (index,*))) // no semicolon
};


#line 1275 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
int main() {
#line 1272 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
myclass x("bob", 3);                     // positional
myclass y(_index = 12, _name = "sally"); // named
myclass z("june");                       // positional/defaulted
#line 1275 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
}

