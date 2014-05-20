#ifndef _core_lispReader_H_
#define _core_lispReader_H_

#include "core/foundation.h"
#include "core/object.h"
#include "core/lispReader.h"


namespace core
{

    extern Cons_sp read_list(Stream_sp sin, char end_char, bool allow_consing_dot);

    extern T_mv lisp_object_query(Stream_sp, bool eofErrorP, T_sp eofValue, bool recursiveP);
    
    extern T_sp read_lisp_object(Stream_sp, bool eofErrorP, T_sp eofValue, bool recursiveP);
    
    extern void exposeCore_lisp_reader();
};
#endif /* _lispReader_H_ */
