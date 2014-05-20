#ifndef core_numberToString_H
#define core_numberToString_H

#include "foundation.h"
#include "strWithFillPtr.h"
namespace core {

    StrWithFillPtr_sp af_integerToString(StrWithFillPtr_sp buffer, Integer_sp integer,
					 Fixnum_sp base, bool radix, bool decimalp);

    void initialize_numberToString();


};
#endif
