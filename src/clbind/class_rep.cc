/*
    File: class_rep.cc
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
#include <clasp/core/package.h>
#include <clasp/clbind/cl_include.h>

//#include <clasp/clbind/detail/stack_utils.hpp>
//#include <clasp/clbind/detail/conversion_storage.hpp>
#include <clasp/clbind/clbind.h>
//#include <clasp/clbind/exception_handler.hpp>
//#include <clasp/clbind/get_main_thread.hpp>
//#include <utility>
#include <clasp/clbind/class_rep.h>
#include <clasp/core/wrappers.h>

using namespace clbind::detail;

namespace clbind {

void ClassRep_O::exposeCando(core::Lisp_sp lisp) {
  _G();
  core::class_<ClassRep_O>();
}
void ClassRep_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, ClassRep, "", "", _lisp);
#endif
}

EXPOSE_CLASS(clbind, ClassRep_O);

ClassRep_O::ClassRep_O(type_id const &type, const std::string &name, bool derivable)
    : m_type(type), m_name(name)
      //	, m_class_type(cpp_class)
      //	, m_operator_cache(0)
      ,
      m_casts(globalCastGraph) // Meister - luabind did this
      ,
      m_classes(globalClassIdMap) // Meister - luabind did this
      ,
      m_derivable(derivable) {
#if 0
	cl_newtable(L);
	handle(L, -1).swap(m_table);
	cl_newtable(L);
	handle(L, -1).swap(m_default_table);
	cl_pop(L, 2);

	class_registry* r = class_registry::get_registry(L);
	assert((r->cpp_class() != CL_NOREF) && "you must call clbind::open()");

	cl_rawgeti(L, CL_REGISTRYINDEX, r->cpp_class());
	cl_setmetatable(L, -2);

	cl_pushvalue(L, -1); // duplicate our user data
	m_self_ref.set(L);

	m_instance_metatable = r->cpp_instance();

        cl_pushstring(L, "__clbind_cast_graph");
        cl_gettable(L, CL_REGISTRYINDEX);
        m_casts = static_cast<cast_graph*>(cl_touserdata(L, -1));
        cl_pop(L, 1);

        cl_pushstring(L, "__clbind_class_id_map");
        cl_gettable(L, CL_REGISTRYINDEX);
        m_classes = static_cast<class_id_map*>(cl_touserdata(L, -1));
        cl_pop(L, 1);
#endif
}

ClassRep_O::ClassRep_O(const std::string &name, bool derivable)
    : m_type(typeid(reg::null_type)), m_name(name)
      //	, m_class_type(cl_class)
      //	, m_operator_cache(0)
      ,
      m_casts(globalCastGraph) // Meister - luabind did this
      ,
      m_classes(globalClassIdMap) // Meister - luabind did this
      ,
      m_derivable(derivable) {
#if 0
	cl_newtable(L);
	handle(L, -1).swap(m_table);
	cl_newtable(L);
	handle(L, -1).swap(m_default_table);
	cl_pop(L, 2);

	class_registry* r = class_registry::get_registry(L);
	assert((r->cpp_class() != CL_NOREF) && "you must call clbind::open()");

	cl_rawgeti(L, CL_REGISTRYINDEX, r->cl_class());
	cl_setmetatable(L, -2);
	cl_pushvalue(L, -1); // duplicate our user data
	m_self_ref.set(L);

	m_instance_metatable = r->cl_instance();

        cl_pushstring(L, "__clbind_cast_graph");
        cl_gettable(L, CL_REGISTRYINDEX);
        m_casts = static_cast<cast_graph*>(cl_touserdata(L, -1));
        cl_pop(L, 1);

        cl_pushstring(L, "__clbind_class_id_map");
        cl_gettable(L, CL_REGISTRYINDEX);
        m_classes = static_cast<class_id_map*>(cl_touserdata(L, -1));
        cl_pop(L, 1);
#endif
}

ClassRep_O::~ClassRep_O() {
}

#if 0
// leaves object on cl stack
    std::pair<void*,void*> 
    ClassRep_O::allocate() const
    {
	const int size = sizeof(object_rep);
	char* mem = static_cast<char*>(cl_newuserdata(L, size));
	return std::pair<void*,void*>(mem, (void*)0);
    }

    namespace
    {

        bool super_deprecation_disabled = false;

    } // namespace unnamed


// this is called as metamethod __call on the ClassRep_O.
    int ClassRep_O::constructor_dispatcher(cl_State* L)
    {
        ClassRep_O* cls = static_cast<ClassRep_O*>(cl_touserdata(L, 1));

        int args = cl_gettop(L);

        push_new_instance(L, cls);

        if (super_deprecation_disabled
            && cls->get_class_type() == ClassRep_O::cl_class
            && !cls->bases().empty())
        {
            cl_pushstring(L, "super");
            cl_pushvalue(L, 1);
            cl_pushvalue(L, -3);
            cl_pushcclosure(L, super_callback, 2);
            cl_settable(L, CL_GLOBALSINDEX);
        }

        cl_pushvalue(L, -1);
        cl_replace(L, 1);

        cls->get_table(L);
        cl_pushliteral(L, "__init");
        cl_gettable(L, -2);

        cl_insert(L, 1);

        cl_pop(L, 1);
        cl_insert(L, 1);

        cl_call(L, args, 0);

        if (super_deprecation_disabled)
        {
            cl_pushstring(L, "super");
            cl_pushnil(L);
            cl_settable(L, CL_GLOBALSINDEX);
        }

        return 1;
    }
#endif

void ClassRep_O::add_base_class(core::Fixnum_sp pointer_offset, ClassRep_sp base)
//const ClassRep_O::base_info& binfo)
{
  // If you hit this assert you are deriving from a type that is not registered
  // in cl. That is, in the class_<> you are giving a baseclass that isn't registered.
  // Please note that if you don't need to have access to the base class or the
  // conversion from the derived class to the base class, you don't need
  // to tell clbind that it derives.
  ASSERTF(base.objectp(), BF("You cannot derive from an unregistered type"));

  ClassRep_sp bcrep = base;
#if 0
	// import all static constants
	for (std::map<const char*, int, ltstr>::const_iterator i = bcrep->m_static_constants.begin(); 
             i != bcrep->m_static_constants.end(); ++i)
	{
            int& v = m_static_constants[i->first];
            v = i->second;
	}
#endif
  // also, save the baseclass info to be used for typecasts
  core::Cons_sp binfo = core::Cons_O::create(pointer_offset, base);
  m_bases.push_back(binfo);
}
#if 0
    CLBIND_API void clbind::disable_super_deprecation()
    {
        super_deprecation_disabled = true;
    }

    int ClassRep_O::super_callback(cl_State* L)
    {
	int args = cl_gettop(L);
		
	ClassRep_O* crep = static_cast<ClassRep_O*>(cl_touserdata(L, cl_upvalueindex(1)));
	ClassRep_O* base = crep->bases()[0].base;

	if (base->bases().empty())
	{
            cl_pushstring(L, "super");
            cl_pushnil(L);
            cl_settable(L, CL_GLOBALSINDEX);
	}
	else
	{
            cl_pushstring(L, "super");
            cl_pushlightuserdata(L, base);
            cl_pushvalue(L, cl_upvalueindex(2));
            cl_pushcclosure(L, super_callback, 2);
            cl_settable(L, CL_GLOBALSINDEX);
	}

	base->get_table(L);
	cl_pushstring(L, "__init");
	cl_gettable(L, -2);
	cl_insert(L, 1);
	cl_pop(L, 1);

	cl_pushvalue(L, cl_upvalueindex(2));
	cl_insert(L, 2);

	cl_call(L, args + 1, 0);

	// TODO: instead of clearing the global variable "super"
	// store it temporarily in the registry. maybe we should
	// have some kind of warning if the super global is used?
	cl_pushstring(L, "super");
	cl_pushnil(L);
	cl_settable(L, CL_GLOBALSINDEX);

	return 0;
    }



    int ClassRep_O::cl_settable_dispatcher(cl_State* L)
    {
	ClassRep_O* crep = static_cast<ClassRep_O*>(cl_touserdata(L, 1));

	// get first table
	crep->get_table(L);

	// copy key, value
	cl_pushvalue(L, -3);
	cl_pushvalue(L, -3);
	cl_rawset(L, -3);
	// pop table
	cl_pop(L, 1);

	// get default table
	crep->get_default_table(L);
	cl_replace(L, 1);
	cl_rawset(L, -3);

	crep->m_operator_cache = 0; // invalidate cache
	
	return 0;
    }

/*
  stack:
  1: ClassRep_O
  2: member name
*/
    int ClassRep_O::static_class_gettable(cl_State* L)
    {
	ClassRep_O* crep = static_cast<ClassRep_O*>(cl_touserdata(L, 1));

	// look in the static function table
	crep->get_default_table(L);
	cl_pushvalue(L, 2);
	cl_gettable(L, -2);
	if (!cl_isnil(L, -1)) return 1;
	else cl_pop(L, 2);

	const char* key = cl_tostring(L, 2);

	if (std::strlen(key) != cl_strlen(L, 2))
	{
            cl_pushnil(L);
            return 1;
	}

	std::map<const char*, int, ltstr>::const_iterator j = crep->m_static_constants.find(key);

	if (j != crep->m_static_constants.end())
	{
            cl_pushnumber(L, j->second);
            return 1;
	}

#ifndef CLBIND_NO_ERROR_CHECKING

	{
            std::string msg = "no static '";
            msg += key;
            msg += "' in class '";
            msg += crep->name();
            msg += "'";
            cl_pushstring(L, msg.c_str());
	}
	cl_error(L);

#endif

	cl_pushnil(L);

	return 1;
    }

    bool clbind::detail::is_ClassRep_O(cl_State* L, int index)
    {
	if (cl_getmetatable(L, index) == 0) return false;

	cl_pushstring(L, "__clbind_classrep");
	cl_gettable(L, -2);
	if (cl_toboolean(L, -1))
	{
            cl_pop(L, 2);
            return true;
	}

	cl_pop(L, 2);
	return false;
    }

    void clbind::detail::finalize(cl_State* L, ClassRep_O* crep)
    {
	if (crep->get_class_type() != ClassRep_O::cl_class) return;

//	cl_pushvalue(L, -1); // copy the object ref
	crep->get_table(L);
        cl_pushliteral(L, "__finalize");
	cl_gettable(L, -2);
	cl_remove(L, -2);

	if (cl_isnil(L, -1))
	{
            cl_pop(L, 1);
	}
	else
	{
            cl_pushvalue(L, -2);
            cl_call(L, 1, 0);
	}

	for (std::vector<ClassRep_O::base_info>::const_iterator 
                 i = crep->bases().begin(); i != crep->bases().end(); ++i)
	{
            if (i->base) finalize(L, i->base);
	}
    }

    void ClassRep_O::cache_operators(cl_State* L)
    {
	m_operator_cache = 0x1;

	for (int i = 0; i < number_of_operators; ++i)
	{
            get_table(L);
            cl_pushstring(L, get_operator_name(i));
            cl_rawget(L, -2);

            if (cl_isfunction(L, -1)) m_operator_cache |= 1 << (i + 1);

            cl_pop(L, 2);
	}
    }

    bool ClassRep_O::has_operator_in_cl(cl_State* L, int id)
    {
	if ((m_operator_cache & 0x1) == 0)
            cache_operators(L);

	const int mask = 1 << (id + 1);

	return (m_operator_cache & mask) != 0;
    }
#endif
};
