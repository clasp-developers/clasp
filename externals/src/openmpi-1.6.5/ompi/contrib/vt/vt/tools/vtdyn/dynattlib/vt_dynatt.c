/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifdef __GNUC__
# define INITROU dynatt_init
  void __attribute__ ((constructor)) INITROU(void);
#else
# define INITROU _init
#endif

#include "vt_dyninst.h"

void INITROU(void);

void INITROU()
{
  vt_dyn_attach();
}

