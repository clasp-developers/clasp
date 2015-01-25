/*
    File: scope.h
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
// Copyright (c) 2004 Daniel Wallin and Arvid Norberg

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

#ifndef NEW_SCOPE_040211_HPP
#define NEW_SCOPE_040211_HPP

//#include "clbind/prefix.h"
#include <clasp/clbind/config.h>
#include <clasp/clbind/cl_include.h>
#include <memory>

namespace clbind {

    struct scope;

} // namespace clbind

namespace clbind { namespace detail {

    struct CLBIND_API registration
    {
        registration();
        virtual ~registration();

    public:
        virtual core::Creator* registerDefaultConstructor_() const {HARD_SUBCLASS_MUST_IMPLEMENT();};
    protected:
        virtual void register_() const = 0;

    private:
        friend struct ::clbind::scope;
        registration* m_next;
    };

}} // namespace clbind::detail

namespace clbind {

    struct CLBIND_API scope
    {
        scope();
        explicit scope(std::auto_ptr<detail::registration> reg);
        scope(scope const& other_);
        ~scope();

        scope& operator=(scope const& other_);

        scope& operator,(scope s);

        void register_() const;

    private:
        detail::registration* m_chain;
    };


    /*! Declare a package - provide the package name (which will be lispified) and a list
      of nicknames and a list of usePackageNames */
    class CLBIND_API package_
    {
    public:
        package_(string const& name, std::list<std::string> nicknames={}, std::list<string> usePackageNames={});
        void operator[](scope s);

    private:
        string             m_name;
        list<std::string>       m_nicknames;
        list<std::string>       m_usePackageNames;
    };

    inline package_ package(string const& name, std::list<std::string> nicknames={}, std::list<std::string> usePackageNames={})
    {
        return package_(name,nicknames,usePackageNames);
    }


} // namespace clbind

#endif // NEW_SCOPE_040211_HPP
