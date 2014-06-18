
#line 1353 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
#include <boost/parameter.hpp>
#line 1344 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
namespace boost { namespace python {

namespace tag { struct class_type; } // keyword tag type
template <class T>
struct class_type
  : parameter::template_keyword<tag::class_type,T>
{};

}}
