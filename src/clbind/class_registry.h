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


#ifndef CLBIND_CLASS_REGISTRY_HPP_INCLUDED
#define CLBIND_CLASS_REGISTRY_HPP_INCLUDED

#include <map>

#include <clbind/config.h>
#include <clbind/clbindPackage.h>
#include <clbind/open.h>
#include <clbind/typeid.h>

namespace clbind {


    FORWARD(ClassRep);

    FORWARD(ClassRegistry);
    class ClassRegistry_O : public core::T_O
    {
        LISP_BASE1(core::T_O);
        LISP_CLASS(clbind,ClbindPkg,ClassRegistry_O,"ClassRegistry");
        void initialize();
    public:
        ClassRegistry_O() {};
        virtual ~ClassRegistry_O() {};
    public:
        static ClassRegistry_sp get_registry();

#if 0
        int cpp_instance() const { return m_instance_metatable; }
        int cpp_class() const { return m_cpp_class_metatable; }

        int cl_instance() const { return m_instance_metatable; }
        int cl_class() const { return m_cl_class_metatable; }
        int cl_function() const { return m_cl_function_metatable; }
#endif
        void add_class(type_id const& info, ClassRep_sp crep);

        ClassRep_sp find_class(type_id const& info) const;

    private:

        /*! Index on the type_id.id converted to a core::Pointer and use EQL equality */
        core::HashTableEql_sp         m_classes;
//        std::map<type_id, ClassRep_sp> m_classes;


#if 0
        // this is a cl reference that points to the cl table
        // that is to be used as meta table for all C++ class 
        // instances. It is a kind of v-table.
        int m_instance_metatable;

        // this is a cl reference to the metatable to be used
        // for all classes defined in C++.
        int m_cpp_class_metatable;

        // this is a cl reference to the metatable to be used
        // for all classes defined in cl
        int m_cl_class_metatable;

        // this metatable only contains a destructor
        // for clbind::Detail::free_functions::function_rep
        int m_cl_function_metatable;
#endif
    };

}

#endif // CLBIND_CLASS_REGISTRY_HPP_INCLUDED

