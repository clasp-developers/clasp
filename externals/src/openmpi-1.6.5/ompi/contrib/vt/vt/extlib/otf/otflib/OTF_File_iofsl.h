/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_File.h
 * 
 *  @brief Provides a low-level API for accessing files.
 *
 *  \ingroup internal
 */


#ifndef OTF_FILE_IOFSL_H
#define OTF_FILE_IOFSL_H

#include "OTF_Filenames.h"
#include "OTF_File.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/** initialize a OTF_File object */
int OTF_File_iofsl_init( OTF_File* o );

/**
 * Finalize a OTF_File object
 * This frees all associated datastructures
 * including the file object itsself!
 **/
void OTF_File_iofsl_finalize( OTF_File* o );

/** there is no OTF_File_iofsl_open because it is just a plain wrapper for open_zlevel */

/** open a pseudo OTF_File that actually reads from the given memory buffer. 
The buffer is not copied, you need to manage it yourself! 
Don't touch it during access operations. */
OTF_File* OTF_File_iofsl_open_with_external_buffer( uint32_t len, const char* buffer, uint8_t is_compressed,
    OTF_FileMode mode );

/** Rename file pointed to by 'from' to file 'to'.
 *  If the filename describes a zoidfs file the zoidfs API is used.
 *  Otherwise standard POSIX rename is used.*/
int OTF_File_iofsl_rename(const char* from, const char* to);

int OTF_File_iofsl_access(const char* filename, int mode);

/** Remove the file according to the stream id encoded in the filename */
int OTF_File_iofsl_remove(const char* filename);

/** Clean up everything -- relevant only for multifile use to remove the data and index file */
int OTF_File_iofsl_clean(const char* filename);

/**
 * OTF_File_iofsl_write and OTF_File_iofsl_read do not exist
 * all IOFSL specific code is in the _internal versions
 */

/** seek absolute position in an OTF_File */
int OTF_File_iofsl_seek( OTF_File* file, uint64_t pos );

/** get absolut position from an OTF_File */
uint64_t OTF_File_iofsl_tell( OTF_File* file );

/** return the file size in bytes*/
uint64_t OTF_File_iofsl_size( OTF_File* file );

/** close OTF_File */
int OTF_File_iofsl_close( OTF_File* file );

/** return OTF_File status */
OTF_FileStatus OTF_File_iofsl_status( OTF_File* file );


/** suspend OTF_File - internal use only. */
void OTF_File_iofsl_suspend( OTF_File* file );

/** re-open the file when closed or suspended - internal use only. 
return 1 on success, 0 otherwise */
int OTF_File_iofsl_revive( OTF_File* file, OTF_FileMode mode );

void OTF_File_iofsl_setZBufferSize( OTF_File* file, uint32_t size );

/** For zoidfs finalize */
void OTF_File_iofsl_finalizeGlobal( void );

/** internal use */
OTF_File* OTF_File_iofsl_open_zlevel( const char* filename, OTF_FileManager* manager,
	OTF_FileMode mode, OTF_FileCompression compression );


/* internal function */
/* they are in the header file because they are called by OTF_File_write|read */
/** read 'length' bytes from underlying iofsl file  */
size_t OTF_File_iofsl_read_internal( OTF_File* file, void* dest, size_t length );


/** Wrapper around fwrite to issue calls to zoidfs_write if needed */
size_t OTF_File_iofsl_write_internal(OTF_File* file, const void* src, size_t length);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_FILE_H */
