#include "core/foundation.h"
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/APSInt.h>



#include <clbind/clbind.h>


#ifdef USE_MPS
#define NAMESPACE_clbind_llvm
#include "main/gc_interface.h"
#undef NAMESPACE_clbind_clang
#endif


INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<llvm::APInt>)
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(clbind::Wrapper<llvm::APSInt>);

typedef clbind::Wrapper<llvm::APInt,std::unique_ptr<llvm::APInt> >      APInt_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(APInt_wrapper);
typedef clbind::Wrapper<llvm::APSInt,std::unique_ptr<llvm::APSInt> >      APSInt_wrapper;
INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(APSInt_wrapper);



namespace llvmo {

    using namespace clbind;


    void initialize_clbind_llvm_expose()    
    {
        package("LLVM") [
            class_<llvm::APInt>("APInt",no_default_constructor)
            .   def("toString",(std::string(llvm::APInt::*)(unsigned Radix, bool Signed) const) &llvm::APInt::toString)
            ,class_<llvm::APSInt,llvm::APInt>("APSInt",no_default_constructor)
            ];
    }
}

