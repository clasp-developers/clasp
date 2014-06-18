
#line 424 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
#include <boost/parameter/keyword.hpp>
#line 413 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
namespace graphs
{
  namespace tag { struct graph; } // keyword tag type

  namespace // unnamed
  {
    // A reference to the keyword object
    boost::parameter::keyword<tag::graph>& _graph
    = boost::parameter::keyword<tag::graph>::get();
  }
}
