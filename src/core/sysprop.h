#ifndef	_core_sysprop_H
#define _core_sysprop_H

#include "core/object.h"
#include "corePackage.fwd.h"


namespace core
{


    T_mv af_put_sysprop(T_sp key, T_sp area, T_sp value);

    T_mv af_get_sysprop(T_sp key, T_sp area);




    void initialize_sysprop();

};

#endif /* _core_sysprop_H */


