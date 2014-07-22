#ifndef	lambdaListHandler_fwd_H
#define lambdaListHandler_fwd_H

namespace core
{

    FORWARD(LambdaListHandler);

    extern void lambdaListHandler_createBindings(core::LambdaListHandler_sp llh, core::DynamicScopeManager& scope, LISP_CALLING_CONVENTION_ARGS);

                                          
};
#endif
