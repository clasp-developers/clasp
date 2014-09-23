/*
    File: tagged_operator_bool.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
//  This header intentionally has no include guards.
//
//  Modified by Christian Schafmeister 2013
//  Copyright (c) 2001-2009 Peter Dimov
//
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt

#if ( defined(__SUNPRO_CC) && BOOST_WORKAROUND(__SUNPRO_CC, < 0x570) ) || defined(__CINT__)

    operator bool () const
    {
        return px != 0;
    }

#elif defined( _MANAGED )

    static void unspecified_bool( this_type*** )
    {
    }

    typedef void (*unspecified_bool_type)( this_type*** );

    operator unspecified_bool_type() const // never throws
    {
        return px == 0? 0: unspecified_bool;
    }

#elif \
    ( defined(__MWERKS__) && BOOST_WORKAROUND(__MWERKS__, < 0x3200) ) || \
    ( defined(__GNUC__) && (__GNUC__ * 100 + __GNUC_MINOR__ < 304) ) || \
    ( defined(__SUNPRO_CC) && BOOST_WORKAROUND(__SUNPRO_CC, <= 0x590) )

    typedef T * (this_type::*unspecified_bool_type)() const;

    operator unspecified_bool_type() const // never throws
    {
        return px == 0? 0: &this_type::get;
    }

#else

    typedef T * this_type::*unspecified_bool_type;

    operator unspecified_bool_type() const // never throws
    {
        return px == 0? 0: &this_type::px;
    }

#endif

    // operator! is redundant, but some compilers need it
    bool operator! () const // never throws
    {
        return px == 0;
    }
