#ifndef _core_compiler_H_
#define _core_compiler_H_

#include "core/foundation.h"
#include "core/object.h"

namespace core
{


    T_sp varArgsList(int numArgs, ... );




    T_mv af_implicit_compile_hook_default(T_sp form, Environment_sp env);

    void initialize_compiler_primitives(Lisp_sp lisp);

    
    
};
#endif /* _compiler_H_ */
