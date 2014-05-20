/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Filenames.h
 * 
 *  @brief Handles file naming issues.
 *
 *  \ingroup internal
 */


#ifndef OTF_FILENAMES
#define OTF_FILENAMES


#include "OTF_inttypes.h"
#include "OTF_Definitions.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*#define OTF_FILETYPE_MASTER 		0
#define OTF_FILETYPE_GLOBAL_DEF 	1
#define OTF_FILETYPE_DEF 			2
#define OTF_FILETYPE_EVENT  		3
#define OTF_FILETYPE_SNAPS  		4
#define OTF_FILETYPE_STATS  		5*/
#define OTF_FILETYPE_MASTER 		16
#define OTF_FILETYPE_GLOBAL_DEF 	32
#define OTF_FILETYPE_DEF 			64
#define OTF_FILETYPE_EVENT  		128
#define OTF_FILETYPE_SNAPS  		256
#define OTF_FILETYPE_STATS  		512
#define OTF_FILETYPE_MARKER  		1024
#define OTF_FILETYPE_IOFSL_ALL		2048
#define OTF_FILETYPE_IOFSL_IDX		4096
#define OTF_FILETYPE_BITS 			(16+32+64+128+256+512+1024)
typedef uint32_t OTF_FileType;

/* zlevel has to be 0-9 */
#define OTF_FILECOMPRESSION_UNCOMPRESSED 	0
/* default compression level */
#define OTF_FILECOMPRESSION_COMPRESSED  	4
#define OTF_FILECOMPRESSION_BITS			(1+2+4+8)
typedef uint32_t OTF_FileCompression;


/** provide a name stub, a stream id and the type of file. 
the function will create the proper filename in 'ret'. ret may be 
pre-allocated with length 'l' or NULL with 'l=0'. in the latter case 
memory is allocated via malloc internally but has to be freed later on.
on success the resulting string is returned, return NULL indicates an error. */
char* OTF_getFilename( const char* namestub, uint32_t id, OTF_FileType type, 
	unsigned int l, char* ret );


/** strip the a filename from the ".otf" suffix if present */
char* OTF_stripFilename( const char* filename );


/** DEPRICATED Check whether a file exists or not. Return 1 on success. */
int OTF_fileExists( const char* filename );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_FILENAMES */
