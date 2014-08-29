#ifndef	_core_print_H
#define _core_print_H

#include "core/object.h"
#include "corePackage.fwd.h"
#include "core/character.fwd.h"
#include "core/write_object.h"
#include "wrappers.h"

namespace core
{

    int clasp_print_base();
    int brcl_print_level();
    int brcl_print_length();
    bool brcl_print_radix();
    Symbol_sp clasp_print_case();
    bool clasp_print_gensym();
    bool brcl_print_array();
    bool clasp_print_readably();
    bool clasp_print_escape();
    bool brcl_print_circle();

    T_mv af_write(T_sp x, T_sp strm, T_sp array, T_sp base,
		  T_sp cas, T_sp escape, T_sp gensym, T_sp length,
		  T_sp level, T_sp lines, T_sp miser_width, T_sp pprint_dispatch,
		  T_sp pretty, T_sp radix, T_sp readability, T_sp right_margin );

    void initialize_print();

};
#endif
