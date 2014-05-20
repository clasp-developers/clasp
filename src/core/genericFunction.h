#ifndef _core_genericFunction_H_
#define _core_genericFunction_H_

#include "core/foundation.h"
#include "core/object.h"

namespace core
{

    T_mv notFuncallableDispatch( const Instance_O& gf, int nargs, ArgArray args);
    
    T_mv generic_function_dispatch( const Instance_O& gf, int nargs, ArgArray args);

    T_mv slotReaderDispatch( const Instance_O& gf, int nargs, ArgArray args);

    T_mv slotWriterDispatch( const Instance_O& gf, int nargs, ArgArray args);

    T_mv userFunctionDispatch( const Instance_O& gf, int nargs, ArgArray args);


    void initialize_genericFunction();
    
};
#endif /* _core_genericFunction_H_ */
