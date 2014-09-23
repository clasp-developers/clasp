/*
    File: smallMap.cc
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
#define	DEBUG_LEVEL_FULL


//
// (C) 2004 Christian E. Schafmeister
//


#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "smallMap.h"
#include "multipleValues.h"
#include "stringList.h"
#include "objectSet.h"
#include "environment.h"
#include "cons.h"
#include "symbolList.h"
#include "numbers.h"
#include "wrappers.h"



namespace core {



    
    
#define ARGS_core_makeSmallMap "()"
#define DECL_core_makeSmallMap ""
#define DOCS_core_makeSmallMap "makeSmallMap"
    SmallMap_sp core_makeSmallMap()
    {_G();
        GC_ALLOCATE(SmallMap_O,sm);
        return sm;
    };

    T_sp SmallMap_O::find(T_sp key, T_sp defval)
    {
        map_type::iterator it = this->map.find(key);
        if ( it == this->map.end() ) {
            return defval;
        }
        return it->second;
    }

    void SmallMap_O::setf(T_sp key, T_sp val)
    {
        pair<map_type::iterator,bool> found = this->map.insert(std::make_pair(key,val));
        found.first->second = val;
    }


    void SmallMap_O::exposeCando(Lisp_sp lisp)
    {
        class_<SmallMap_O>()
            .def("map_find", &SmallMap_O::find)
            .def("map_setf", &SmallMap_O::setf)
            .def("map_size", &SmallMap_O::size)
            .def("map_capacity", &SmallMap_O::capacity)
            ;   
        CoreDefun(makeSmallMap);
    }

    void SmallMap_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
        PYTHON_CLASS(CorePkg,SmallMap,"","",_lisp)
            ;
#endif
    }






    EXPOSE_CLASS(core,SmallMap_O);

};
