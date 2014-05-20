#ifndef	_core_num_arith_H //[
#define	_core_num_arith_H

#include "clasp_gmpxx.h"
#include <math.h>


namespace core {


    Integer_sp brcl_gcd(Integer_sp x, Integer_sp y, int yidx=1);
    Integer_sp brcl_integer_divide(Integer_sp x, Integer_sp y);

    void initialize_num_arith();


};

#endif
