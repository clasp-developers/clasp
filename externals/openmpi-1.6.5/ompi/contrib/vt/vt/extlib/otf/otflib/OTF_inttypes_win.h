/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_inttypes_win.h
 *
 *  @brief Deals with all data type related issues.
 *
 *  \ingroup misc
 */


#ifndef OTF_INTTYPES_WIN_H
#define OTF_INTTYPES_WIN_H

	/* needed by otf */
	typedef unsigned char uint8_t;
	typedef signed int int32_t;
	typedef unsigned int uint32_t;
	typedef signed __int64 int64_t;
	typedef unsigned __int64 uint64_t;

	/* not needed by otf */
	typedef signed char int8_t;
	typedef signed short int16_t;
	typedef unsigned short uint16_t;

#	define OTF_UINT64_MAX (uint64_t)-1

#endif /* OTF_INTTYPES_WIN_H */
