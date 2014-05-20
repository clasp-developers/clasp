#include <stdio.h>
#include <core/foundation.h>
#include <core/object.h>
#include <core/holder.h>
#include <core/str.h>
#include <asttooling/testAST.h>


#if 0
namespace asttooling {


    template <class T>
    class Bar {
        void testFunction() {
            static core::T_sp _staticObj;
        };
    };



    void tinyFunc()
    {
        gctools::Vec0<core::T_sp>  _vecObjects;
        for ( int i(0); i<10; ++i ) {
            _vecObjects.push_back(core::Fixnum_O::create(i));
        }
        printf("Hi there, this is tinyFunc\n");
    }

};
#endif
