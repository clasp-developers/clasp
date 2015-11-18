/*
    File: open.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
// Copyright (c) 2003 Daniel Wallin and Arvid Norberg

// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.

#define CLBIND_BUILDING

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/clbind/cl_include.h>

#include <clasp/clbind/clbind.h>
#include <clasp/clbind/class_registry.h>
#include <clasp/clbind/symbolTable.h>
//#include <clasp/clbind/function.h>
//#include <clasp/clbind/get_main_thread.h>

namespace clbind {

//! Take the place of __clbind_cast_graph
detail::cast_graph *globalCastGraph;
//! Take the place of __clbind_class_id_map
detail::class_id_map *globalClassIdMap;
//! Take the place of __clbind_class_map
gctools::tagged_pointer<detail::class_map> globalClassMap;

namespace {
#if 0
  int make_property()
  {
      IMPLEMENT_ME();
#if 0
      int args = cl_gettop(L);

      if (args == 0 || args > 2)
      {
          cl_pushstring(L, "make_property() called with wrong number of arguments.");
          cl_error(L);
      }

      if (args == 1)
          cl_pushnil(L);

      cl_pushcclosure(L, &detail::property_tag, 2);
      return 1;
#endif
  }

  int main_thread_tag;

  int deprecated_super()
  {
      IMPLEMENT_ME();
#if 0
      cl_pushstring(L,
          "DEPRECATION: 'super' has been deprecated in favor of "
          "directly calling the base class __init() function. "
          "This error can be disabled by calling 'clbind::disable_super_deprecation()'."
      );
      cl_error(L);

      return 0;
#endif
  }

  int destroy_class_id_map()
  {
      IMPLEMENT_ME();
#if 0
      detail::class_id_map* m =
          (detail::class_id_map*)cl_touserdata(L, 1);
      m->~class_id_map();
      return 0;
#endif
  }

  int destroy_cast_graph()
  {
      IMPLEMENT_ME();
#if 0
      detail::cast_graph* g =
          (detail::cast_graph*)cl_touserdata(L, 1);
      g->~cast_graph();
      return 0;
#endif
  }

  int destroy_class_map()
  {
      IMPLEMENT_ME();
#if 0
      detail::class_map* m =
          (detail::class_map*)cl_touserdata(L, 1);
      m->~class_map();
      return 0;
#endif
  }
#endif
} // namespace unnamed

CLBIND_API int get_main_thread() {
  IMPLEMENT_ME();
#if 0
        cl_pushlightuserdata(L, &main_thread_tag);
        cl_rawget(L, CL_REGISTRYINDEX);
        cl_State* result = static_cast<cl_State*>(cl_touserdata(L, -1));
        cl_pop(L, 1);

        if (!result)
            throw std::runtime_error("Unable to get main thread, clbind::open() not called?");

        return result;
#endif
}

CLBIND_API void initialize_clbind() {
  ClassRegistry_sp registry = ClassRegistry_O::create();
  _sym_STARtheClassRegistrySTAR->defparameter(registry);
  globalClassIdMap = new detail::class_id_map();
  globalCastGraph = new detail::cast_graph();
  globalClassMap = gctools::RootClassAllocator<detail::class_map>::allocate();
}

} // namespace clbind
