/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
 also: patches by Rainer Keller, thanks a lot!
*/

/* macros to enable 64 bit file access. make sure all std headers are 
included AFTER this macro definitions */

/* config.h handles this now: #define _LARGEFILE_SOURCE
#define _LARGEFILE_SOURCE
#define _LARGEFILE64_SOURCE 
#define _LARGE_FILES
*/


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "OTF_Platform.h"
#include "OTF_inttypes.h"

#include <stdio.h>
#include <assert.h>
#include <errno.h>

#ifdef HAVE_STDLIB_H
	#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
	#include <string.h>
#endif

#ifdef HAVE_SYS_TYPES_H
	#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
	#include <sys/stat.h>
#endif

#ifdef HAVE_FCNTL_H
	#include <fcntl.h>
	#if !(defined(HAVE_DECL_O_NOATIME) && HAVE_DECL_O_NOATIME)
		#define O_NOATIME 0
	#endif
#endif

#ifdef HAVE_UNISTD_H
	#include <unistd.h>
#endif

#ifdef HAVE_IO_H
	#include <io.h>
#endif


/* vs does not know F_OK*/
#ifndef F_OK
	#define F_OK 00
#endif

#ifdef HAVE_ZLIB

#include <zlib.h>


#endif /* HAVE_ZLIB */

#include "OTF_File.h"
#include "OTF_File_iofsl.h"
#include "OTF_Platform.h"
#include "OTF_Definitions.h"
#include "OTF_Errno.h"

static int OTF_File_nameSuffixCmp(const char *filename, const char *suffix);


void OTF_File_init( OTF_File* file ) {
/* internal function, called only by OTF_File_open*,
 * no need (and possibility) to check for Iofsl */

	file->filename= NULL;
	file->file= NULL;
#ifdef HAVE_ZLIB
	file->z= NULL;
	file->zbuffer= NULL;
	file->zbuffersize= 1024*10;
	file->zbuffer_seek_further= 0;
#endif /* HAVE_ZLIB */
	file->pos= 0;
	file->mode= OTF_FILEMODE_NOTHING;
	file->manager= NULL;

	file->externalbuffer= NULL;
	file->externalpos= 0;
	file->externallen= 0;
	file->iofsl= NULL;
}


void OTF_File_finalize( OTF_File* file ) {
	/* internal function, called only by OTF_File_close,
	 * no need to check for Iofsl */

	file->filename= NULL;
	file->file= NULL;
#ifdef HAVE_ZLIB
	file->z= NULL;
	file->zbuffer= NULL;
	file->zbuffersize= 0;
	file->zbuffer_seek_further= 0;
#endif /* HAVE_ZLIB */
	file->pos= 0;
	file->mode= OTF_FILEMODE_NOTHING;
	file->manager= NULL;

	file->externalbuffer= NULL;
	file->externalpos= 0;
	file->externallen= 0;
}


OTF_File* OTF_File_open( const char* filename, 
	OTF_FileManager* manager, OTF_FileMode mode ) {
	return OTF_File_open_zlevel( filename, manager, mode, OTF_FILECOMPRESSION_COMPRESSED );
}


OTF_File* OTF_File_open_with_external_buffer( uint32_t len, const char* buffer, 
        uint8_t is_compressed, OTF_FileMode mode ) {

	OTF_File* ret;

	ret= (OTF_File*) malloc( sizeof(OTF_File) );
	if( NULL == ret ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	OTF_File_init( ret );

	ret->externalbuffer= buffer;
	ret->externalpos= 0;
	ret->externallen= (uint64_t) len;

	ret->mode = mode;

	if ( is_compressed ) {

#ifdef HAVE_ZLIB

		/* alloc zlib stuff */
		ret->z= malloc( sizeof(z_stream) );
		if( NULL == ret->z ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					  "no memory left.\n",
					  __FUNCTION__, __FILE__, __LINE__ );

			free( ret );
			ret= NULL;

			return NULL;
		}

		OTF_FILE_Z(ret)->next_in= NULL;
		OTF_FILE_Z(ret)->avail_in= 0;
		OTF_FILE_Z(ret)->zalloc= NULL;
		OTF_FILE_Z(ret)->zfree= NULL;
		OTF_FILE_Z(ret)->opaque= NULL;

		inflateInit( OTF_FILE_Z(ret) );

		ret->zbuffer= malloc( ret->zbuffersize );
		if( NULL == ret->zbuffer ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"no memory left.\n",
					__FUNCTION__, __FILE__, __LINE__ );

			free( ret->zbuffer );
			ret->zbuffer= NULL;
			free( ret->z );
			ret->z= NULL;
			free( ret );
			ret= NULL;

			return NULL;
		}

#else /* HAVE_ZLIB */

		free( ret );
		ret= NULL;

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"built without HAVE_ZLIB, still trying to open with compressed buffer.\n", 
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;

#endif /* HAVE_ZLIB */

	} else {

		/* normal, don't need any special setup */
	}

	ret->manager= NULL;

	return ret;
}


size_t OTF_File_write( OTF_File* file, const void* ptr, size_t size ) {


	size_t byteswritten = 0;

#ifdef HAVE_ZLIB
	int status;
#endif/* HAVE_ZLIB */

	/* IOFSL is used in the _internal functions */
	if ( NULL != file->externalbuffer ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
			"not yet supported in 'external buffer' mode.\n",
			__FUNCTION__, __FILE__, __LINE__ );
		return (size_t) -1;
	}


	if( OTF_FILEMODE_WRITE != file->mode ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"current file->mode is not OTF_FILEMODE_WRITE. writing forbidden.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}


	if( 0 == OTF_File_revive( file, OTF_FILEMODE_WRITE ) ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_File_revive() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

#ifdef HAVE_ZLIB

	if ( NULL != file->z ) {

		/* compress the data without using the ybuffer */
		OTF_FILE_Z(file)->avail_in = size;
		OTF_FILE_Z(file)->next_in = (void*)ptr;

		while (OTF_FILE_Z(file)->avail_in > 0) {

			status = deflate(OTF_FILE_Z(file), Z_FULL_FLUSH);
			if (status == Z_STREAM_ERROR) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"error in compressing, status %i.\n",
						__FUNCTION__, __FILE__, __LINE__, status );
				return byteswritten;
			}

			while (OTF_FILE_Z(file)->avail_out == 0) {

				size_t towrite = file->zbuffersize - OTF_FILE_Z(file)->avail_out;
				if (towrite != OTF_File_write_internal(file, file->zbuffer, towrite)) {

					OTF_Error( "ERROR in function %s, file: %s, line %i:\n",
							"Failed to write %u bytes to file!\n", 
							__FUNCTION__, __FILE__, __LINE__, towrite);
					return byteswritten;
				}
				OTF_FILE_Z(file)->avail_out = file->zbuffersize;
				OTF_FILE_Z(file)->next_out = file->zbuffer;
				status = deflate(OTF_FILE_Z(file), Z_FULL_FLUSH);
				if (status == Z_STREAM_ERROR) {

					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"error in compressing, status %i.\n",
							__FUNCTION__, __FILE__, __LINE__, status );
					assert(status != Z_STREAM_ERROR);
					return byteswritten;
				}
			}
			byteswritten = size - OTF_FILE_Z(file)->avail_in;
		}
	} else {

#endif /* HAVE_ZLIB */

		byteswritten= OTF_File_write_internal(file, ptr, size);
		if( byteswritten < size ) {

			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"less bytes written than expected %u < %u.\n",
					__FUNCTION__, __FILE__, __LINE__, (uint32_t) byteswritten,
					(uint32_t) size );

		}


#ifdef HAVE_ZLIB
	}
#endif /* HAVE_ZLIB */
    return byteswritten;

}


size_t OTF_File_read( OTF_File* file, void* ptr, size_t size ) {


#ifdef HAVE_ZLIB
	/* size_t read; */
	int status;
#endif /* HAVE_ZLIB */

	/* IOFSL is used in the _internal functions */

	if( OTF_FILEMODE_WRITE == file->mode ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"current file->mode is OTF_FILEMODE_WRITE. reading forbidden.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}
	
	if( 0 == OTF_File_revive( file, OTF_FILEMODE_READ ) ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_File_revive() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

#ifdef HAVE_ZLIB

	if ( NULL != file->z ) {

		OTF_FILE_Z(file)->next_out= ptr;
		OTF_FILE_Z(file)->avail_out= (uInt) size;

		while ( 0 < OTF_FILE_Z(file)->avail_out ) {

			if ( 0 == OTF_FILE_Z(file)->avail_in ) {

		
				OTF_FILE_Z(file)->avail_in= (uInt) OTF_File_read_internal( file, file->zbuffer, file->zbuffersize );
				OTF_FILE_Z(file)->next_in= file->zbuffer;
			}

			if ( 0 == OTF_FILE_Z(file)->avail_in ) {

				break;
			}

			status = inflate( OTF_FILE_Z(file), Z_SYNC_FLUSH );
			if ( status != Z_OK ) {

				/* see declaration of 'zbuffer_seek_further' in struct_OTF_File in OTF_File.h */
				if ( 0 != file->zbuffer_seek_further ) {

					if ( 0 == OTF_File_seek( file, file->zbuffer_seek_further ) ) {

						file->zbuffer_seek_further= 0;

						return 0;
					}

					return OTF_File_read( file, ptr, size );
				}
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"error in uncompressing, status %u.\n",
						__FUNCTION__, __FILE__, __LINE__, status );

				return 0;
			}
		}

		file->zbuffer_seek_further= 0;

		return size - OTF_FILE_Z(file)->avail_out;

	} else {

		return OTF_File_read_internal( file, ptr, size );
	}

#else /* HAVE_ZLIB */

		return OTF_File_read_internal( file, ptr, size );

#endif /* HAVE_ZLIB */
}


int OTF_File_seek( OTF_File* file, uint64_t pos ) {


	int ret;

#ifdef HAVE_ZLIB
	int sync;
	uint64_t read;
#endif /* HAVE_ZLIB */

	if ( NULL != file->iofsl ) {
		return OTF_File_iofsl_seek( file, pos );
	}

	if ( NULL != file->externalbuffer ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
			"not yet supported in 'external buffer' mode.\n",
			__FUNCTION__, __FILE__, __LINE__ );
		return -1;
	}


	if( OTF_FILEMODE_WRITE == file->mode ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"current file->mode is OTF_FILEMODE_WRITE. seeking forbidden.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return -1;
	}
		
	
	if( 0 == OTF_File_revive( file, OTF_FILEMODE_SEEK ) ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_File_revive() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return -1;
	}
	

	ret= fseeko( file->file, pos, SEEK_SET );
	
#ifdef HAVE_ZLIB

	if ( NULL != file->z && 0 == ret ) {

		do {

			/* OLD:
			read= fread( file->zbuffer, 1, file->zbuffersize, file->file );
			*/
			read= OTF_File_read_internal( file, file->zbuffer, file->zbuffersize );
			if ( 0 == read ) {

				return 0;
			}

			OTF_FILE_Z(file)->next_in= file->zbuffer;
			OTF_FILE_Z(file)->avail_in= (uInt) read;
			OTF_FILE_Z(file)->total_in= 0;

			/* re-initialize z object */
			inflateReset(OTF_FILE_Z(file));

			/* do not sync at very beginning of compressed stream because it 
			would skip the first block */
			sync= Z_OK;
			if ( 0 != pos ) {

				sync= inflateSync( OTF_FILE_Z(file) );
				if ( Z_OK == sync ) {

					/* see declaration of 'zbuffer_seek_further' in struct_OTF_File in OTF_File.h */
					file->zbuffer_seek_further= pos + OTF_FILE_Z(file)->total_in;
				}
			}

			if ( Z_OK == sync ) {

				return ret;
			}

			pos += read;

			if ( Z_BUF_ERROR == sync ) {
			
				continue;
			}
			
			if ( Z_DATA_ERROR == sync ) {

				/* do not break here, this might happen with larger zlib chunks
				return -1;
				*/
			}

			if ( Z_STREAM_ERROR == sync ) {
			
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"Z_STREAM_ERROR.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return -1;
			}

		} while ( 1 );
	}

#endif /* HAVE_ZLIB */

	return ret;
}


uint64_t OTF_File_tell( OTF_File* file ) {

	if ( NULL != file->iofsl ) {
		return OTF_File_iofsl_tell( file );
	}

	if ( NULL != file->externalbuffer ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
			"not yet supported in 'external buffer' mode.\n",
			__FUNCTION__, __FILE__, __LINE__ );
		return (uint64_t) -1;
	}


	if ( NULL != file->file ) {

		file->pos= ftello( file->file );
	}

	return file->pos;
}


uint64_t OTF_File_size( OTF_File* file ) {


	struct stat st;

	if ( NULL != file->iofsl ) {
		return OTF_File_iofsl_size( file );
	}

	if ( NULL != file->externalbuffer ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
			"not yet supported in 'external buffer' mode.\n",
			__FUNCTION__, __FILE__, __LINE__ );
		return (uint64_t) -1;
	}


	if ( stat( file->filename, &st ) == -1 ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"stat() failed: %s\n",
				__FUNCTION__, __FILE__, __LINE__,
				strerror(errno) );

		return 0;
	} else {

		return st.st_size;

	}
}


int OTF_File_close( OTF_File* file ) {


#ifdef HAVE_ZLIB
	size_t byteswritten;
	int status;
#endif /* HAVE_ZLIB */

	if ( NULL == file ) {
			
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"file has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );
				
		return 0;
	}

	if ( NULL != file->iofsl ) {
		return OTF_File_iofsl_close( file );
	}

#ifdef HAVE_ZLIB

	if ( NULL != file->z ) {

        if ( OTF_FILEMODE_WRITE != file->mode ) {

			inflateEnd( OTF_FILE_Z(file) );

		} else {

			size_t towrite;

			/* flush buffer */
			if( 0 == OTF_File_revive( file, OTF_FILEMODE_WRITE ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_File_revive() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				return 0;
			}

			status = deflate( OTF_FILE_Z(file), Z_FULL_FLUSH );
			if (status == Z_STREAM_ERROR) {

				OTF_Error( "ERROR in function %s, file %s, line %i\n"
						"deflate() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				return 0;
			}

			towrite = file->zbuffersize - OTF_FILE_Z(file)->avail_out;
			byteswritten = 0;
			if (towrite > 0)
				byteswritten = OTF_File_write_internal(file, file->zbuffer, towrite);
			if (towrite != byteswritten) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n"
						"Failed to write compressed buffer of size %lu\n",
						__FUNCTION__, __FILE__, __LINE__, towrite );
			}

			while (OTF_FILE_Z(file)->avail_out != file->zbuffersize) {

				OTF_FILE_Z(file)->avail_out = file->zbuffersize;
				OTF_FILE_Z(file)->next_out = file->zbuffer;
				deflate( OTF_FILE_Z(file), Z_FULL_FLUSH );
				assert(status != Z_STREAM_ERROR);

				towrite = file->zbuffersize - OTF_FILE_Z(file)->avail_out;
				if (towrite > 0)
					OTF_File_write_internal(file, file->zbuffer, towrite);
			}
			deflateEnd( OTF_FILE_Z(file) );
		}
		free( file->z );
		file->z = NULL;

		free( file->zbuffer );
		file->zbuffer = NULL;
	}

#endif /* HAVE_ZLIB */

	if ( NULL != file->file ) {

		OTF_FileManager_suspendFile( file->manager, file );
	}

	free( file->filename );
	
	OTF_File_finalize( file );

	free( file );
	file = NULL;
	
	return 1;
}


OTF_FileStatus OTF_File_status( OTF_File* file ) {

	if ( NULL != file->iofsl ) {
		return OTF_File_iofsl_status( file );
	}

	if ( NULL != file->externalbuffer ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"not yet supported in 'external buffer' mode.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		return OTF_FILESTATUS_UNKNOWN;
	}


	if ( NULL == file->file ) {

		if ( 0 == file->pos ) {

			return OTF_FILESTATUS_CLOSED;

		} else {

			return OTF_FILESTATUS_SUSPENDED;
		}
	}

	return OTF_FILESTATUS_ACTIVE;
}


void OTF_File_suspend( OTF_File* file ) {

	if ( NULL != file->iofsl ) {
		OTF_File_iofsl_suspend( file );
		return;
	}

	if ( NULL != file->externalbuffer ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"not yet supported in 'external buffer' mode.\n",
				__FUNCTION__, __FILE__, __LINE__ );
		return;
	}


	/* get status and close OS file */

	file->pos= ftello( file->file );
	fclose( file->file );
	file->file= NULL;
}


int OTF_File_revive( OTF_File* file, OTF_FileMode mode  ) {

	if ( NULL != file->iofsl ) {
		return OTF_File_iofsl_revive( file, mode );
	}

	if ( NULL != file->externalbuffer ) {

		/* no need to revive, everything is fine in 'external buffer' mode */
		return 1;
	}


	switch ( mode ) {

	case OTF_FILEMODE_READ :
	
		/* *** read *** */

		if ( NULL == file->file ) {

			/* file currently closed, aka open or reopen */

			if ( 0 == OTF_FileManager_guaranteeFile( file->manager ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_guaranteeFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}

			/* open first time, as we open O_RDONLY plus O_NOATIME, which fopen doesn't know, use open/fdopen  */
#ifdef _GNU_SOURCE
			{

				int fd;
				int retry_num = 5;
				int flags = O_RDONLY | O_NOATIME;

				while ( -1 == ( fd = open( file->filename, flags ) ) ) {

					/* if the user is not the owner of the file, open with O_NOATIME will fail with errno == EPERM;
					   try to open without O_NOATIME again to avoid this problem */
					if ( EPERM == errno ) {

						flags = O_RDONLY;
						continue;

						/* the file name might be stale, e.g. on Network File System (NFS) */
					} else if ( ESTALE == errno && 0 < --retry_num ) {

						sleep(1);
						continue;

					} else {

						/* show this error every time */
						OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
								"cannot open file %s for reading. Maybe the number of "
								"opened filehandles exceeds your system's limit\n",
								__FUNCTION__, __FILE__, __LINE__, file->filename );

						return 0;
					}

				}
	
				file->file= fdopen( fd, "r" );

			}
#else /* _GNU_SOURCE */
			file->file= fopen( file->filename, "rb" );
#endif /* _GNU_SOURCE */
			if( NULL == file->file ) {

				/* show this error every time */
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open file %s for reading. Maybe the number of "
						"opened filehandles exceeds your system's limit\n",
						__FUNCTION__, __FILE__, __LINE__, file->filename );

				return 0;
			}

			/* Upon repoen, seek to the current position */
			if ( 0 != file->pos ) {
				fseeko( file->file, file->pos, SEEK_SET );
			}

			
			if ( 0 == OTF_FileManager_registerFile( file->manager, file ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_registerFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}

		} else {

			/* file already opened */
			if ( 0 ==  OTF_FileManager_touchFile( file->manager, file ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_touchFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}
		}

		return 1;

	case OTF_FILEMODE_WRITE :

		/* *** write *** */

		if ( NULL == file->file ) {

			/* file currently closed */

			if ( 0 == OTF_FileManager_guaranteeFile( file->manager ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_guaranteeFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}

			if ( 0 != file->pos ) {

				/* re-open */

				file->file= fopen( file->filename, "ab" );
				if( NULL == file->file ) {
					
					/* show this error every time */
					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open file %s for writing. Maybe the number of "
						"opened filehandles exceeds your system's limit\n",
						__FUNCTION__, __FILE__, __LINE__, file->filename );
				
					return 0;
				}

			} else {

				/* open first time */

				file->file= fopen( file->filename, "wb" );
				if( NULL == file->file ) {
					
					/* show this error every time */
					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open file %s for writing. Maybe the number of "
						"opened filehandles exceeds your system's limit\n",
						__FUNCTION__, __FILE__, __LINE__, file->filename );
				
					return 0;
				}
			}

			if ( 0 == OTF_FileManager_registerFile( file->manager, file ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_registerFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}

		} else {

			/* file already opened */
			if ( 0 ==  OTF_FileManager_touchFile( file->manager, file ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_touchFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}
		}

		return 1;

	case OTF_FILEMODE_SEEK :
	
		/* *** seek *** */

		if ( NULL == file->file ) {

			/* file currently closed */

			if ( 0 == OTF_FileManager_guaranteeFile( file->manager ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_guaranteeFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				return 0;
			}

			if ( 0 != file->pos ) {

				/* re-open */

				file->file= fopen( file->filename, "rb" );
				if( NULL == file->file ) {
					
					/* show this error every time */
					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open file %s for reading. Maybe the number of "
						"opened filehandles exceeds your system's limit\n",
						__FUNCTION__, __FILE__, __LINE__, file->filename );
				
					return 0;
				}

				/* dont need to seek to the saved position because there 
				will be another seek anyway*/
				/*
				fseeko( file->file, file->pos, SEEK_SET );
				*/

			} else {

				/* open first time */

				file->file= fopen( file->filename, "rb" );
				if( NULL == file->file ) {
					
					/* show this error every time */
					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open file %s for reading. Maybe the number of "
						"opened filehandles exceeds your system's limit\n",
						__FUNCTION__, __FILE__, __LINE__, file->filename );
				
					return 0;
				}
			}

			if ( 0 == OTF_FileManager_registerFile( file->manager, file ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_registerFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}

		} else {

			/* file already opened */
			if ( 0 ==  OTF_FileManager_touchFile( file->manager, file ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_touchFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				
				return 0;
			}
		}

		return 1;


	default:

		/* *** unknown mode *** */

		return 0;
	}
}


void OTF_File_setZBufferSize( OTF_File* file, uint32_t size ) {
	/* no specific IOFSL version */

#ifdef HAVE_ZLIB
	
	if( NULL != file->z ) {
        void *tmp;
		if ( 32 > size ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"intended zbuffer size %u is too small, rejected.\n",
					__FUNCTION__, __FILE__, __LINE__, size );
			
			return;
	
		} else if ( 512 > size ) {
		
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"zbuffer size %u is very small, accepted though.\n",
					__FUNCTION__, __FILE__, __LINE__, size );
	
		} else if ( 10 * 1024 *1024 < size ) {
	
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"zbuffer size %u is rather big, accepted though.\n",
					__FUNCTION__, __FILE__, __LINE__, size );

		}

        /* use realloc instead of free()/malloc() */
		/*if( NULL != file->zbuffer ) {
			free( file->zbuffer );
		}*/
        tmp = realloc( file->zbuffer, size );
        if (tmp == NULL)
        {
            OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"No memory left to reallocate zlib buffer.\n",
				__FUNCTION__, __FILE__, __LINE__ );
            return;
        }
        file->zbuffer = tmp;
		file->zbuffersize= size;
        OTF_FILE_Z(file)->avail_out = size;
        OTF_FILE_Z(file)->next_out  = OTF_FILE_Z(file)->next_in = file->zbuffer;
		
	
	}

#endif /* HAVE_ZLIB */
}

/**
 * returns 0 if suffix is-suffix of filename
 *         true if suffix is not NOT a suffix of filename
 */
static int OTF_File_nameSuffixCmp(const char *filename, const char *suffix) {
	size_t filename_length, suffix_length;
	assert( filename && suffix );
	filename_length = strlen( filename );
	suffix_length = strlen( suffix );
	if ( suffix_length >  filename_length ) {
		return 1;
	}
	return strncmp( filename + filename_length - suffix_length, suffix, suffix_length);
}


OTF_File* OTF_File_open_zlevel( const char* filename, OTF_FileManager* manager,
	OTF_FileMode mode, OTF_FileCompression zlevel ) {


	uint32_t len;
	OTF_File* ret;

	/* Check input parameters */
	if( NULL == filename ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no filename has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}
	if( NULL == manager ) {
		
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"manager has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	if ( OTF_FileManager_isIofsl( manager ) ) {
		/* open all files except *.otf and global definitions/markers with iofsl */
		if ( OTF_File_nameSuffixCmp( filename, ".otf" )
			&& OTF_File_nameSuffixCmp( filename, ".0.def" )
			&& OTF_File_nameSuffixCmp( filename, ".0.def.z" )
			&& OTF_File_nameSuffixCmp( filename, ".0.marker" )
			&& OTF_File_nameSuffixCmp( filename, ".0.marker.z" ) ) {
			return OTF_File_iofsl_open_zlevel( filename, manager, mode, zlevel );
		}
	}

	ret= (OTF_File*) malloc( sizeof(OTF_File) );
	if( NULL == ret ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	OTF_File_init( ret );

	len= (uint32_t) strlen( filename );
	ret->filename= malloc( len +3 );
	if( NULL == ret->filename ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		free( ret );
		ret= NULL;
		
		return NULL;
	}
	
	strncpy( ret->filename, filename, len +1 );
	
	ret->mode = mode;

	if ( OTF_FILEMODE_READ == mode || OTF_FILEMODE_SEEK == mode ) {

#ifdef HAVE_ZLIB

		if ( 0 != access( ret->filename, F_OK ) ) {

			/* file not found, try '.z' suffix */

			strncpy( ret->filename +len, ".z", 3 );

			if ( 0 != access( ret->filename, F_OK ) ) {

				/* file still not found, give up */
				free( ret->filename );
				ret->filename= NULL;
				free( ret );
				ret= NULL;

				return ret;
			}

			ret->z= malloc( sizeof(z_stream) );
			if( NULL == ret->z ) {
		
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"no memory left.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				free( ret->filename );
				ret->filename= NULL;
				free( ret );
				ret= NULL;
				
				return NULL;
			}

			OTF_FILE_Z(ret)->next_in= NULL;
			OTF_FILE_Z(ret)->avail_in= 0;
			OTF_FILE_Z(ret)->zalloc= NULL;
			OTF_FILE_Z(ret)->zfree= NULL;
			OTF_FILE_Z(ret)->opaque= NULL;

			inflateInit( OTF_FILE_Z(ret) );

			ret->zbuffer= malloc( ret->zbuffersize );

			if( NULL == ret->zbuffer ) {
		
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"no memory left.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				free( ret->zbuffer );
				ret->zbuffer= NULL;

				free( ret->z );
				ret->z= NULL;
				free( ret->filename );
				ret->filename= NULL;
				free( ret );
				ret= NULL;

				return NULL;
			}
		}

#else /* HAVE_ZLIB */

		if ( 0 != access( ret->filename, F_OK ) ) {

			strncpy( ret->filename +len, ".z", 3 );

			if ( 0 == access( ret->filename, F_OK ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open %s. Zlib is not enabled.\n",
						__FUNCTION__, __FILE__, __LINE__, ret->filename );

			}

			/* file still not found, give up */
			free( ret->filename );
			ret->filename= NULL;
			free( ret );
			ret= NULL;

			return ret;
		}

#endif /* HAVE_ZLIB */

	} else {

		/* filemode write */

#ifdef HAVE_ZLIB

		/* is a .z appended to the file name */
		if ( len > 2 && 0 == strcmp( ret->filename + len - 2, ".z" ) ) {
		
			ret->z= malloc( sizeof(z_stream) );
			if( NULL == ret->z ) {
		
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"no memory left.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				free( ret->filename );
				ret->filename= NULL;
				free( ret );
				ret= NULL;
				
				return NULL;
			}

			OTF_FILE_Z(ret)->next_in= NULL;
			OTF_FILE_Z(ret)->avail_in= 0;
			OTF_FILE_Z(ret)->zalloc= NULL;
			OTF_FILE_Z(ret)->zfree= NULL;
			OTF_FILE_Z(ret)->opaque= NULL;

			deflateInit( OTF_FILE_Z(ret), zlevel );

			ret->zbuffer= malloc( ret->zbuffersize );
			if( NULL == ret->zbuffer ) {
		
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"no memory left.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				free( ret->z );
				ret->z= NULL;
				free( ret->filename );
				ret->filename= NULL;
				free( ret );
				ret= NULL;
				
				return NULL;
			}
		}
#endif /* HAVE_ZLIB */

	}

	ret->manager= manager;

	return ret;
}

size_t OTF_File_write_internal( OTF_File* file, const void* src, size_t length ) {
	if ( NULL != file->iofsl )
		return OTF_File_iofsl_write_internal( file, src, length );
        return fwrite(src, 1, length, file->file);
}

size_t OTF_File_read_internal( OTF_File* file, void* dest, size_t length ) {
    uint64_t actual_length;


    if ( NULL != file->iofsl )
	return OTF_File_iofsl_read_internal( file, dest, length );

    /* default behavior first */
    if ( NULL == file->externalbuffer ) 
        return fread( dest, 1, length, file->file );

    /* now for the special case: read from the external buffer */


    actual_length= file->externallen - file->externalpos;
    actual_length= ( length <= actual_length ) ? length : actual_length;

    memcpy( dest, file->externalbuffer + file->externalpos, actual_length );
    file->externalpos += actual_length;

    return actual_length;
}
