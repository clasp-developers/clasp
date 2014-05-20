/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal_config.h"

#include "opal/constants.h"
#include "opal/util/arch.h"

int32_t opal_arch_compute_local_id( uint32_t *me )
{
    *me = (OPAL_ARCH_HEADERMASK | OPAL_ARCH_UNUSEDMASK);

    /* Handle the size of long (can hold a pointer) */
    if( 8 == sizeof(long) )
        opal_arch_setmask( me, OPAL_ARCH_LONGIS64 );

    /* sizeof bool */
    if (1 == sizeof(bool) ) {
        opal_arch_setmask( me, OPAL_ARCH_BOOLIS8);
    } else if (2 == sizeof(bool)) {
        opal_arch_setmask( me, OPAL_ARCH_BOOLIS16);
    } else if (4 == sizeof(bool)) {
        opal_arch_setmask( me, OPAL_ARCH_BOOLIS32);
    }

    /* sizeof fortran logical
     *
     * RHC: technically, use of the ompi_ prefix is
     * an abstraction violation. However, this is actually
     * an error in our configure scripts that transcends
     * all the data types and eventually should be fixed.
     * The guilty part is f77_check.m4. Fixing it right
     * now is beyond a reasonable scope - this comment is
     * placed here to explain the abstraction break and
     * indicate that it will eventually be fixed
     */
    if (1 == sizeof(ompi_fortran_logical_t) ) {
        opal_arch_setmask( me, OPAL_ARCH_LOGICALIS8);
    } else if (2 == sizeof(ompi_fortran_logical_t)) {
        opal_arch_setmask( me, OPAL_ARCH_LOGICALIS16);
    } else if (4 == sizeof(ompi_fortran_logical_t)) {
        opal_arch_setmask( me, OPAL_ARCH_LOGICALIS32);
    }

    /* Initialize the information regarding the long double */
    if( 12 == sizeof(long double) )
        opal_arch_setmask( me, OPAL_ARCH_LONGDOUBLEIS96 );
    else if( 16 == sizeof(long double) )
        opal_arch_setmask( me, OPAL_ARCH_LONGDOUBLEIS128 );

    /* Big endian or little endian ? That's the question */
    if( opal_arch_isbigendian() )
        opal_arch_setmask( me, OPAL_ARCH_ISBIGENDIAN );

    /* What's the maximum exponent ? */
    if ( LDBL_MAX_EXP == 16384 )
        opal_arch_setmask( me, OPAL_ARCH_LDEXPSIZEIS15 );

    /* How about the length in bits of the mantissa */
    if ( LDBL_MANT_DIG == 64 )
        opal_arch_setmask( me, OPAL_ARCH_LDMANTDIGIS64 );
    else if ( LDBL_MANT_DIG == 105 )
        opal_arch_setmask( me, OPAL_ARCH_LDMANTDIGIS105 );
    else if ( LDBL_MANT_DIG == 106 )
        opal_arch_setmask( me, OPAL_ARCH_LDMANTDIGIS106 );
    else if ( LDBL_MANT_DIG == 107 )
        opal_arch_setmask( me, OPAL_ARCH_LDMANTDIGIS107 );
    else if ( LDBL_MANT_DIG == 113 )
        opal_arch_setmask( me, OPAL_ARCH_LDMANTDIGIS113 );

    /* Intel data representation or Sparc ? */
    if( opal_arch_ldisintel() )
        opal_arch_setmask( me, OPAL_ARCH_LDISINTEL );

    return OPAL_SUCCESS;
}

int32_t opal_arch_checkmask ( uint32_t *var, uint32_t mask )
{
    unsigned int tmpvar = *var;

    /* Check whether the headers are set correctly,
       or whether this is an erroneous integer */
    if( !((*var) & OPAL_ARCH_HEADERMASK) ) {
        if( (*var) & OPAL_ARCH_HEADERMASK2 ) {
            char* pcDest, *pcSrc;
            /* Both ends of this integer have the wrong settings,
               maybe its just the wrong endian-representation. Try
               to swap it and check again. If it looks now correct,
               keep this version of the variable
            */

            pcDest = (char *) &tmpvar;
            pcSrc  = (char *) var + 3;
            *pcDest++ = *pcSrc--;
            *pcDest++ = *pcSrc--;
            *pcDest++ = *pcSrc--;
            *pcDest++ = *pcSrc--;

            if( (tmpvar & OPAL_ARCH_HEADERMASK) && (!(tmpvar & OPAL_ARCH_HEADERMASK2)) ) {
                *var = tmpvar;
            } else
                return -1;
        } else
            return -1;
    }

    /* Here is the real evaluation of the bitmask */
    return ( ((*var) & mask) == mask );
}
