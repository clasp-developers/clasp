#ifndef	_core_write_ugly_H
#define _core_write_ugly_H

#include "core/object.h"
#include "corePackage.fwd.h"
#include "core/character.fwd.h"
#include "wrappers.h"

namespace core
{

    T_sp write_ugly_object(T_sp x, T_sp stream);


    void initialize_write_ugly_object();

};
#endif
