#ifndef _core_bformat_H_
#define _core_bformat_H_

#include "core/foundation.h"
#include "core/object.h"

namespace core
{

    T_mv af_format(T_sp dest, T_sp control, Cons_sp args );
    T_mv af_bformat(T_sp dest, const string& control, Cons_sp args );




    void initialize_bformat(Lisp_sp lisp);

    
    
};
#endif /* _bformat_H_ */
