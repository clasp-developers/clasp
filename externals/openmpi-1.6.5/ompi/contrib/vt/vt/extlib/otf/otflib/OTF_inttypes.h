/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_inttypes.h
 *
 *  @brief Deals with all data type related issues.
 *
 *  \ingroup misc
 */


#ifndef OTF_INTTYPES_H
#define OTF_INTTYPES_H

#if defined(_WIN32) /* windows */
#	include "OTF_inttypes_win.h"
#else /* unix */
#	include "OTF_inttypes_unix.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/** Converts unsigned integers of 8, 16, 32 or 64 bit length into OTF counter values. */
uint64_t OTF_Unsigned2Counter( uint64_t value );
/** Converts OTF counter values to unsigned integers of 8, 16, 32 or 64 bit length. */
uint64_t OTF_Counter2Unsigned( uint64_t value );


/** Converts signed integers of 8, 16, 32 or 64 bit length to OTF counter values. */
uint64_t OTF_Signed2Counter( int64_t value );
/** Converts OTF counter values to signed integers of 8, 16, 32 or 64 bit length. */
int64_t OTF_Counter2Signed( uint64_t value );


/** Converts single precision floating point variables to OTF counter values. */
uint64_t OTF_Float2Counter( float value );
/** Converts OTF counter values to single precision floating point values. */
float OTF_Counter2Float( uint64_t value );


/** Converts double precision floating point values to OTF counter values. */
uint64_t OTF_Double2Counter( double value );
/** Converts OTF counter values to double precision floating point values. */
double OTF_Counter2Double( uint64_t value );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_INTTYPES_H */
