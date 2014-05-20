/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Errno.h
 *
 *  @brief Provides two variables that contain information about the
 *  last error that occurred.
 *
 *  You can check the error variables after every otf functions call
 *  to find out if there was an error or not.
 *  
 *  <B>Variables:</B> <BR>
 *
 *  \code
 *  int   otf_errno   -  indicates whether an error occurred or not,
 *                       values can be OTF_NO_ERROR or OTF_ERROR
 *
 *  char* otf_strerr  -  contains the last error message with a
 *                       maximum length of OTF_ERR_LEN
 *  \endcode
 *
 *
 *  Both variables can be used after including otf.h
 * \ingroup misc
 */


#ifndef OTF_ERRNO_H
#define OTF_ERRNO_H


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/** @cond Errno.h */

/** the following lines is ignored by doxygen */

void OTF_Error( const char* format, ... );
void OTF_Warning( const char* format, ... );

/** @endcond */

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_ERRNO_H */

