#ifndef core_load_H
#define core_load_H

#include "foundation.h"

namespace kw {
    extern core::Symbol_sp _sym_default;
}

namespace core {


    void af_loadSource(T_sp source, bool verbose, bool print, T_sp externalFormat);


    T_sp af_load(T_sp source,
		 T_sp verbose = _Nil<T_O>(),
		 T_sp print = _Nil<T_O>(),
		 T_sp if_does_not_exist = _Nil<T_O>(),
		 T_sp external_format = kw::_sym_default,
		 T_sp search_list = _Nil<T_O>() );

    void initialize_load();
};



#endif
