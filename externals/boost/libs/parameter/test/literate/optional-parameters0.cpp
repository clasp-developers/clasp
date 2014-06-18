
#line 571 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
#include <boost/parameter.hpp>

namespace boost
{
  int vertex_index = 0;

  template <class T = int>
  struct dfs_visitor
  {};
}

BOOST_PARAMETER_NAME(graph)
BOOST_PARAMETER_NAME(visitor)
BOOST_PARAMETER_NAME(root_vertex)
BOOST_PARAMETER_NAME(index_map)
BOOST_PARAMETER_NAME(color_map)

BOOST_PARAMETER_FUNCTION((void), f, tag,
  (required (graph, *))
#line 563 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
(optional     (visitor,           *, boost::dfs_visitor<>())
    (root_vertex,       *, *vertices(graph).first)
    (index_map,         *, get(boost::vertex_index,graph))
    (in_out(color_map), *,
      default_color_map(num_vertices(graph), index_map) )
)
#line 592 "/home/daniel/dev/boost/trunk/libs/parameter/doc/index.rst"
) {}

