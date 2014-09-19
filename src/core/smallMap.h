       
       
//
// (C) 2004 Christian E. Schafmeister
//


#ifndef SmallMap_H
#define SmallMap_H
#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "lispVector.h"
#include "hashTableEq.h"
#include "cons.h"


namespace core
{
    SMART(SmallMap);
    class SmallMap_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,SmallMap_O,"SmallMap");
    private:
        typedef gctools::SmallMap<T_sp,T_sp> map_type;
        map_type                map;
    public:

        T_sp    find(T_sp key, T_sp defval);
        void    setf(T_sp key, T_sp val);
        int     size() const { return this->map.size(); };
        int     capacity() const { return this->map.capacity(); };

	DEFAULT_CTOR_DTOR(SmallMap_O);
    };


};


TRANSLATE(core::SmallMap_O);
#endif


