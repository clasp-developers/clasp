#ifndef	_core_write_object_H
#define _core_write_object_H

#include "core/object.h"
#include "corePackage.fwd.h"
#include "core/character.fwd.h"
#include "wrappers.h"

namespace core
{

    bool will_print_as_hash(T_sp x);
    T_sp write_object(T_sp x, Stream_sp stream);

    void initialize_write_object();

};
#endif
