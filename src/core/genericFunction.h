#ifndef _core_genericFunction_H_
#define _core_genericFunction_H_

#include "core/foundation.h"
#include "core/object.h"

namespace core
{

    T_mv notFuncallableDispatch( Instance_sp gf, int nargs, ArgArray args);
    
    T_mv generic_function_dispatch( Instance_sp gf, int nargs, ArgArray args);

    T_mv slotReaderDispatch( Instance_sp gf, int nargs, ArgArray args);

    T_mv slotWriterDispatch( Instance_sp gf, int nargs, ArgArray args);

    T_mv userFunctionDispatch( Instance_sp gf, int nargs, ArgArray args);


    void initialize_genericFunction();
    
};
#endif /* _core_genericFunction_H_ */
