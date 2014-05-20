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


#ifndef OTF_FILE_H
#define OTF_FILE_H


#include "OTF_FileManager.h"
#include "OTF_Filenames.h"

#include <stdio.h>

#ifdef HAVE_ZLIB

/* We cannot include the zlib.h due to possibly missing path to it
(e.g. zlib.h isn't located in a default compiler search path) */
/*#include <zlib.h>*/

/* macro to access the zlib object of struct_OTF_File with the correct type */
#define OTF_FILE_Z(file) ((z_stream*) (file)->z)


#endif /* HAVE_ZLIB */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/** mode determining what to do with a file */
enum enum_OTF_FileMode {

	OTF_FILEMODE_NOTHING = 0,
	OTF_FILEMODE_READ= 1, 
	OTF_FILEMODE_WRITE= 2, 
	OTF_FILEMODE_SEEK= 3
};
typedef enum enum_OTF_FileMode OTF_FileMode;

/** status of a file */
enum enum_OTF_FileStatus {

	OTF_FILESTATUS_UNKNOWN= 0,
	OTF_FILESTATUS_ACTIVE= 1,
	OTF_FILESTATUS_SUSPENDED= 2,
	OTF_FILESTATUS_CLOSED= 3
};
typedef enum enum_OTF_FileStatus OTF_FileStatus;

/* Needs to be in the header so we can use it from OTF_File_iofsl */
struct struct_OTF_File {

	/** own copy of filename */
	char* filename;

	/** actual file handle, it is NULL if file is currently closed,
	!= NULL otherwise */
	FILE* file;

#ifdef HAVE_ZLIB

	/** zlib object,
	The actual type z_stream* cannot be used here (see notes above).
	Use the macro OTF_FILE_Z() to access it with the correct type. */
	void* z;

	/** zlib entry buffer ... what a nice wordplay */
	unsigned char* zbuffer;

	uint32_t zbuffersize;

	/* This is for a workaround for zlib: At the time of the first implementation 
	of 'OTF_File_seek()' the documentation of zlib's 'inflateSync' said it would 
	jump to the next "full flush point", i.e. the point where zlib decompression 
	can be safely resumed after an error or -- in our case a file seek. 
	
	It turned out it is only a "possible full flush point" -- this is what zlib's 
	docu says now, after our inquiry with the zlib authors. It is also possible 
	that it finds a fake flush point, i.e. the compressed data looks like a flush 
	point but it is not. 
	
	Now this workaround relies on the likely case that resuming decompression at a 
	fake flush point will cause a decompression error during the next
	'OTF_File_read()' operation. Should the decompression error happen later or 
	not at all, garbage data will be produced. Then, OTF is out of luck and will 
	throw a parser error. One can resolve this by uncompressing the trace or the 
	stream in question with the 'otfdecompress' command.
	
	But back to the likely case where zlib decompress (the 'inflate()' routine) 
	produces an error. Then OTF can save the day like the following:
	
	1) Every time 'OTF_File_seek()' thinks it found a full flush point via 
	'inflateSync()' it stores the following byte position in 'zbuffer_seek_further'
	-- this is where it needs to continue to search for the next flush point later
	in case it turns out that it was a fake flush point. If 'zbuffer_seek_further' 
	is 0 it means there is no following position.
	
	2) In 'OTF_File_read()' the 'zbuffer' contents is decompressed with 'inflate()'. 
	If this produces an error AND there is a valid following position in 
	'zbuffer_seek_further', then call 'OTF_File_seek()' again with the position in
	'zbuffer_seek_further' and try to read again from the newly found flush point. 
	In this case 'OTF_File_read()' is called recursively until some data is read 
	successfully or the end of the file is reached.
	
	3) Every time after a successful read or reaching the end of the file, the value 
	or 'zbuffer_seek_further' is set to '0' such that the workaround is not activated 
	for successive reads. Thus, the workaround can only by triggered by the first read
	request after a seek operation. It the decompression error manifests itself later
	than the first read operation, then the workaround cannot fix anything. However, by 
	that time OTF should already have experienced parsing errors anyway.
	
	By Andreas Knuepfer, Thomas Ilsche, Matthias Jurenz. This was a fascinating puzzle! 
	*/
	uint64_t zbuffer_seek_further;


#endif /* HAVE_ZLIB */

	/** keep file pos when the real file is closed,
	undefined while file is open, == 0 before opened for the first time */
	uint64_t pos;

	OTF_FileMode mode;

	OTF_FileManager* manager;


	/** Reference to external buffer to read from instead of a real file.
	This is for reading of definitions only and has some limitations. */
	const char* externalbuffer;

	/** the current position in the 'externalbuffer' */
	uint64_t externalpos;
	/** the total length of the 'externalbuffer' */
	uint64_t externallen;

	OTF_File_iofsl *iofsl;
};


#ifdef HAVE_ZOIDFS
/** external variable indicating whether zoidfs was initialized */
extern uint8_t zoidfs_initialized;
#endif

/** initialize a OTF_File object */
void OTF_File_init( OTF_File* o );

/** finalize a OTF_File object */
void OTF_File_finalize( OTF_File* o );

/** open an OTF_File */
OTF_File* OTF_File_open( const char* filename, OTF_FileManager* manager,
	OTF_FileMode mode );

/** open a pseudo OTF_File that actually reads from the given memory buffer. 
The buffer is not copied, you need to manage it yourself! 
Don't touch it during access operations. */
OTF_File* OTF_File_open_with_external_buffer( uint32_t len, const char* buffer, uint8_t is_compressed, 
    OTF_FileMode mode );

/** Rename file pointed to by 'from' to file 'to'.
 *  If the filename describes a zoidfs file the zoidfs API is used.
 *  Otherwise standard POSIX rename is used.*/
int OTF_File_rename(const char* from, const char* to);

int OTF_File_access(const char* filename, int mode);

/** Remove the file according to the stream id encoded in the filename */
int OTF_File_remove(const char* filename);

/** Clean up everything -- relevant only for multifile use to remove the data and index file */
int OTF_File_clean(const char* filename);

/** OTF_File to an OTF_File */
size_t OTF_File_write( OTF_File* file, const void* ptr, size_t size );

/** read from an OTF_File */
size_t OTF_File_read( OTF_File* file, void* ptr, size_t size );

/** seek absolute position in an OTF_File */
int OTF_File_seek( OTF_File* file, uint64_t pos );

/** get absolut position from an OTF_File */
uint64_t OTF_File_tell( OTF_File* file );

/** return the file size in bytes*/
uint64_t OTF_File_size( OTF_File* file );

/** close OTF_File */
int OTF_File_close( OTF_File* file );

/** return OTF_File status */
OTF_FileStatus OTF_File_status( OTF_File* file );


/** suspend OTF_File - internal use only. */
void OTF_File_suspend( OTF_File* file );

/** re-open the file when closed or suspended - internal use only. 
return 1 on success, 0 otherwise */
int OTF_File_revive( OTF_File* file, OTF_FileMode mode );

void OTF_File_setZBufferSize( OTF_File* file, uint32_t size );


/** internal use */
OTF_File* OTF_File_open_zlevel( const char* filename, OTF_FileManager* manager,
	OTF_FileMode mode, OTF_FileCompression compression );


/* internal function */
/** read 'length' bytes from underlying file or from special memory buffer */
size_t OTF_File_read_internal( OTF_File* file, void* dest, size_t length );


/** Wrapper around fwrite to issue calls to zoidfs_write if needed */
size_t OTF_File_write_internal(OTF_File* file, const void* src, size_t length);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_FILE_H */
