#ifndef _core_primitives_fwd_H
#define _core_primitives_fwd_H

namespace core
{

    T_sp af_makeCondition(T_sp datum, Cons_sp initializers);
    void af_error(T_sp datum, Cons_sp args );


};
#endif // _core_primitives_fwd_H
