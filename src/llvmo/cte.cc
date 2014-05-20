

#include <stdio.h>

#include "core/common.h"

typedef void (afFunctionPtrType*)(core::T_sp* resultP, core::ActivationFrame_sp* afP);

extern "C"
{
extern core::ActivationFrame_sp* activationFrameNil();
extern void invokeLlvmFunction(core::T_sp* resultP, afFunctionPtrType fn, core::ActivationFrame_sp* afP);

};


void repl(core::T_sp* resultP, core::ActivationFrame_sp* afP)
{
    printf("In repl\n");
}

void ___main___(core::T_sp* resultP, core::ActivationFrame_sp* afP)
{
    printf("In ___main___\n");
#if 0
    core::ActivationFrame_sp* afNilP = activationFrameNil();
    invokeLlvmFunction(resultP,&repl,afNilP);
#endif
}



