/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Joseph Schuchart, Thomas Ilsche
 strongly based on OTF_File.c by Authors:
 Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
 also: patches by Rainer Keller, thanks a lot!
 */

/*
 * General notes about this file:
 * IOFSL in this context means the support for writing OTF through
 * an I/O forwarding capability.
 * This also includes the use of so called 'multifiles'
 * meaning that data from multiple OTF_File 'streams' is aggregated
 * into a single physical file (by the use of the forwarding network's
 * atomic append capability.
 * Multifiles also use index files that contain information which
 * stream has a data chunk at which position in the single multifile
 * multifile split means that each one of a set of forwarding servers
 * uses one physical file
 * N OTF_Files, M forwarding servers and F files,
 * with N >> M and F = M
 * no multifile split, means F = 1
 * ZOIDS is the library to WRITE data using the IOFSL architecture,
 * no ZOIDFS support means we cannot writ data, but we can still use
 * the general IOFSL features to READ data using posix
 * (and we correctly use the index files etc.)
 * NOTE: Mixing Externalbuffer and IOFSL is not supported
 * (It does not make sense, does it?)
 */

#define IOFSL_INDEX_FMT "%s:%llu:%llu\n"
#define IOFSL_INDEX_SCAN_FMT "%[^:]:%lu:%lu"
#define IOFSL_INDEX_SKIPCHARS "%*[ \n]"

#ifdef HAVE_CONFIG_H
	#include "config.h"
#endif

#include "OTF_Platform.h"
#include "OTF_inttypes.h"
#include "OTF_File_iofsl.h"

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

#ifdef HAVE_SYS_TIME_H
	#include <sys/time.h>
#endif

#ifdef HAVE_CTYPE_H
	#include <ctype.h>
#endif

/* vs does not know F_OK*/
#ifndef F_OK
	#define F_OK 00
#endif

#ifdef HAVE_ZLIB
	#include <zlib.h>
#endif /* HAVE_ZLIB */

#ifdef HAVE_ZOIDFS
	#include <zoidfs.h>
	#include <zoidfs-hints.h>
#endif

#include "OTF_File.h"
#include "OTF_Platform.h"
#include "OTF_Definitions.h"
#include "OTF_Errno.h"

typedef struct struct_IndexEntry {
	uint64_t offset;
	uint64_t length;
} IndexEntry;

struct struct_OTF_File_iofsl {
	/** are we working with zoidfs? */
	uint32_t flags;
	OTF_IofslMode mode;

#ifdef HAVE_ZOIDFS
	/** the zoidfs filehandle **/
	zoidfs_handle_t* zoidfs_fh;
	/** the hint for use with atomic append */
	zoidfs_op_hint_t* zoidfs_hint;
	/** the zoidfs filehandle of the index file **/
	zoidfs_handle_t* zoidfs_index_fh;
	/* the server to use for this file */
	char* zoidfs_server;
#endif /* HAVE_ZOIDFS */

	uint32_t server_num;
	/* right now this is borrowed from the FileManager rather than a copy */
	char**   server_list;

	int server_id;
	/* the ID of the stream -- possibly computed from the filename */
	uint32_t id;
	uint32_t streamid_bits;

	/** File handle for index file, used for reading **/
	FILE* indexfile;
	/** the id of the stream - not encoded in filename anymore **/
	char* id_str;

	char* multifile_name;
	char* indexfile_name;

	/** how many bytes are left in the current chunk? **/
	size_t data_left;

	/**
	 * A buffer for the index file
	 * The buffer contains offset and length for each index entry
	 */
	IndexEntry* index_buffer;
	/* The next index that will be read, not the current one */
	uint32_t index_buffer_pos;
	/* size in number of IndexEntrys, not bytes! */
	uint32_t index_buffer_length;

	/** the current position in the zoidfs file */
	size_t pos;
};

static int OTF_File_iofsl_prepareFilename( OTF_File *file );
static int OTF_File_iofsl_findIndexEntry( FILE* index, const char* id_str,
		off_t* index_entry_len, uint64_t *offset, uint64_t *length,
		uint8_t rewind );
static int OTF_File_iofsl_getIndexEntries( OTF_File* file );
static void  OTF_File_iofsl_assignServer( OTF_File* file );

#ifdef HAVE_ZOIDFS
static void  OTF_File_iofsl_checkServer( OTF_File* file );
static int   OTF_File_iofsl_flushIndexBuffer( OTF_File* file );
static int   OTF_File_iofsl_resetHint( OTF_File* file );
uint8_t zoidfs_initialized = 0;
static const char* zoidfs_server_env = "ZOIDFS_ION_NAME";
#endif

int OTF_File_iofsl_init( OTF_File* file ) {

	file->filename = NULL;
	file->file = NULL;
#ifdef HAVE_ZLIB
	file->z = NULL;
	file->zbuffer = NULL;
	file->zbuffersize = OTF_ZBUFFER_DEFAULTSIZE;
#endif /* HAVE_ZLIB */
	file->pos = 0;
	file->mode = OTF_FILEMODE_NOTHING;
	file->manager = NULL;

	file->externalbuffer = NULL;
	file->externalpos = 0;
	file->externallen = 0;

	file->iofsl = (OTF_File_iofsl*) malloc( sizeof(OTF_File_iofsl) );
	if ( NULL == file->iofsl ) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n", __FUNCTION__, __FILE__, __LINE__ );
		return -1;
	}
	file->iofsl->flags = 0;
	file->iofsl->mode = OTF_IOFSL_DISABLED;
	file->iofsl->server_num = 0;
	file->iofsl->server_list = NULL;
	file->iofsl->server_id = -1;
	file->iofsl->id = 0;
	file->iofsl->streamid_bits = 0;
	file->iofsl->indexfile = NULL;
	file->iofsl->id_str = NULL;
	file->iofsl->multifile_name = NULL;
	file->iofsl->indexfile_name = NULL;
	file->iofsl->data_left = 0;
	file->iofsl->index_buffer = NULL;
	file->iofsl->index_buffer_pos = 0;
	file->iofsl->index_buffer_length = OTF_IOFSL_INDEX_BUFFER_DEFAULTLENGTH;
	file->iofsl->pos = 0;
#ifdef HAVE_ZOIDFS
	file->iofsl->zoidfs_fh = NULL;
	file->iofsl->zoidfs_hint = NULL;
	file->iofsl->zoidfs_index_fh = NULL;
	file->iofsl->zoidfs_server = NULL;
#endif /* HAVE_ZOIDFS */
	return 0;
}

void OTF_File_iofsl_finalize( OTF_File* file ) {
	assert( file->file == NULL );
#ifdef HAVE_ZLIB

	if ( file->z != NULL ) {
		free( file->z );
		file->z = NULL;
	}

	if (file->zbuffer != NULL) {
		free( file->zbuffer );
		file->zbuffer = NULL;
	}

#endif /* HAVE_ZLIB */
	file->pos = 0;
	file->manager = NULL;
	file->externalbuffer = NULL;
	file->externalpos = 0;
	file->externallen = 0;

	if ( file->iofsl ) {
		if ( file->iofsl->index_buffer != NULL ) {
			free( file->iofsl->index_buffer );
			file->iofsl->index_buffer = NULL;
		}

	#ifdef HAVE_ZOIDFS
/* is just a pointer copied from an outside list of charpointers
 * We are not responsible
		if ( file->iofsl->zoidf_sserver != NULL) {
			free(file->iofsl->zoidfs_server);
			file->iofsl->zoidfs_server = NULL;
		}
*/
		if ( file->iofsl->zoidfs_hint != NULL && file->mode == OTF_FILEMODE_WRITE) {
			/* the hint must be present in order to write the buffer
			   it gets initialized before the files are opened */
			int ret = OTF_File_iofsl_resetHint( file );
			if( ret != ZFS_OK ) {
				OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n"
						"could not reset zoidfs hint (%d) ",
						__FUNCTION__, __FILE__, __LINE__, ret );
			}
			if ( file->iofsl->zoidfs_fh != NULL ) {
				zoidfs_commit( file->iofsl->zoidfs_fh, file->iofsl->zoidfs_hint );
				free( file->iofsl->zoidfs_fh );
				file->iofsl->zoidfs_fh = NULL;
			}
			if ( file->iofsl->zoidfs_index_fh != NULL ) {
				zoidfs_commit(file->iofsl->zoidfs_index_fh, file->iofsl->zoidfs_hint);
				free(file->iofsl->zoidfs_index_fh);
				file->iofsl->zoidfs_index_fh = NULL;
			}
			zoidfs_hint_free( file->iofsl->zoidfs_hint );
			free( file->iofsl->zoidfs_hint );
			file->iofsl->zoidfs_hint = NULL;
		}
	#endif /* HAVE_ZOIDFS */

		if ( file->iofsl->indexfile != NULL ) {
			fclose( file->iofsl->indexfile );
			file->iofsl->indexfile = NULL;
		}

		if ( file->iofsl->id_str != NULL ) {
			free( file->iofsl->id_str );
			file->iofsl->id_str = NULL;
		}

		if ( file->iofsl->multifile_name != NULL ) {
			free( file->iofsl->multifile_name );
			file->iofsl->multifile_name = NULL;
		}

		if ( file->iofsl->indexfile_name != NULL ) {
			free( file->iofsl->indexfile_name );
			file->iofsl->indexfile_name = NULL;
		}

		free( file->iofsl );
	}

	file->mode = OTF_FILEMODE_NOTHING;

	if ( file->filename != NULL ) {
		free( file->filename );
		file->filename = NULL;
	}

	free( file );
}

void OTF_File_iofsl_finalizeGlobal( void ) {
#ifdef HAVE_ZOIDFS
	if ( zoidfs_initialized ) {
		zoidfs_initialized= 0;
		zoidfs_finalize();
	}
#endif
}

static int OTF_File_iofsl_findIndexEntry( FILE* index, const char* id_str,
		off_t* index_entry_len, uint64_t *offset, uint64_t *length,
		uint8_t rewind ) {
	uint8_t found = 0;

	do {
		int ret;
		long unsigned off;
		long unsigned len;
		char tmp_str[100];
		off_t index_start_pos;
		off_t index_end_pos;
		/* skip \n and \0 */
		ret = fscanf( index, IOFSL_INDEX_SKIPCHARS );

		if ( rewind || index_entry_len != NULL )
			index_start_pos = ftello( index );

		ret = fscanf( index, IOFSL_INDEX_SCAN_FMT, tmp_str, &off, &len );

		if ( ret == EOF ) {
			break;
		} else if ( ret != 3 ) {
			OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n"
					"Failed to parse index entry at position %li\n",
					__FUNCTION__, __FILE__, __LINE__, ftello( index ) );
			break;
		}

		if ( index_entry_len != NULL )
			index_end_pos = ftello( index );

		if ( strcmp( tmp_str, id_str ) == 0 ) {
			found = 1;

			if ( rewind ) {
				/* seek back to the beginning of the entry */fseeko( index,
						index_start_pos, SEEK_SET );
			}

			if ( index_entry_len != NULL )
				*index_entry_len = index_end_pos - index_start_pos;

			if ( offset != NULL )
				*offset = off;

			if ( length != NULL )
				*length = len;
		}
	} while ( !found );

	if ( found )
		return 0;
	else
		return -1;
}

static int OTF_File_iofsl_getIndexEntries( OTF_File* file ) {
	OTF_File_iofsl *iofsl;
	size_t pos;

	if ( file == NULL || file->iofsl == NULL )
		return -1; /* tilsche: Do we really need to be THIS graceful? */

	iofsl = file->iofsl;
	if ( iofsl->id_str == NULL )
		return 0; /* there are no indices to read */

	if ( iofsl->indexfile == NULL )
		return -1; /* if there are indices there has to be a buffer */

	pos = iofsl->index_buffer_pos;

	rewind( iofsl->indexfile );

	do {
		IndexEntry* entry;
		int ret;

		/* reallocate the entry buffer if necessary */
		if ( pos >= iofsl->index_buffer_length ) {
			IndexEntry* tmp = (IndexEntry*) realloc(
					iofsl->index_buffer,
					sizeof(IndexEntry) * ( iofsl->index_buffer_length
							+ OTF_IOFSL_INDEX_BUFFER_DEFAULTLENGTH ) );

			if ( tmp == NULL ) {
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n"
						"Failed to allocate %lu bytes of memory!",
						__FUNCTION__, __FILE__, __LINE__,
						sizeof(IndexEntry) *
						( iofsl->index_buffer_length + OTF_IOFSL_INDEX_BUFFER_DEFAULTLENGTH ) );
				return -1;
			}

			iofsl->index_buffer = tmp;

			iofsl->index_buffer_length += OTF_IOFSL_INDEX_BUFFER_DEFAULTLENGTH;
		}

		entry = &( iofsl->index_buffer[pos] );

		ret = OTF_File_iofsl_findIndexEntry( iofsl->indexfile, iofsl->id_str, NULL,
				&( entry->offset ), &( entry->length ), 0 );

		if ( ret == -1 ) {
			/* thats it, nothing more to read */
			entry->offset = 0;
			entry->length = 0;
			break;
		}

		pos++;
	} while ( 1 );

	if ( pos == 0 ) {
/*	this gives false warnings for defintion files of unified traces
 * 	we cannot separate between errors and just not existing files anymore
 		OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n"
				"Did not find any index entries for file %s in index file %s with ID %s!\n"
				"Maybe the file does not contain this stream?\n",
				__FUNCTION__, __FILE__, __LINE__,\
				file->filename, iofsl->indexfile_name, iofsl->id_str );
				*/
		return -1;
	}

	return 0;
}

/**
 * OTF_File_write and
 * OTF_File_read
 * have no iofsl counterpart. The IOFSL specifics are handled in
 * OTF_File_[iofsl_](read|write)_internal
 */

int OTF_File_iofsl_seek( OTF_File* file, uint64_t pos ) {

	int ret = 0;

#ifdef HAVE_ZLIB
	int sync;
	uint64_t read;
#endif /* HAVE_ZLIB */

	OTF_File_iofsl *iofsl;
	size_t size_sum;
	off_t real_pos;

	iofsl = file->iofsl;

	if ( OTF_FILEMODE_WRITE == file->mode ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"current file->mode is OTF_FILEMODE_WRITE. seeking forbidden.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return -1;
	}

	if ( 0 == OTF_File_iofsl_revive( file, OTF_FILEMODE_SEEK ) ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"OTF_File_revive() failed.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return -1;
	}

	if ( iofsl->index_buffer == NULL ) {
		OTF_Error( "ERROR missing index buffer for seeking. ");
		return -1;
	}

	size_sum = 0;
	real_pos = 0;
	iofsl->index_buffer_pos = 0;

	while (1) {
		IndexEntry* entry = &(iofsl->index_buffer[iofsl->index_buffer_pos]);
		iofsl->index_buffer_pos++;

		if ( entry->length > 0 ) {
			/* requested position is behind this chunk */
			if ( pos >= ( entry->length + size_sum ) ) {
				size_sum += entry->length;
				continue;
			}

			/* requested position is in this chunk */
			if ( pos >= size_sum && pos < ( size_sum + entry->length ) ) {
				real_pos = entry->offset + (pos - size_sum);
				iofsl->data_left = entry->length - (pos - size_sum);
				break;
			}

			/* something went wrong */
			OTF_Error( "ERROR: OTF_File_seek: Failed to seek to position %lu\n", pos);

			return -1;
		} else {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Some index entries seem to be missing in index buffer "
					"for file %s in %s with ID %s while seeking to position %lu!\n",
					__FUNCTION__, __FILE__, __LINE__,
					file->filename, iofsl->indexfile_name, iofsl->id_str, pos);
			return -1;
		}
	}

	if ( real_pos == 0 && pos != 0 ) {
		OTF_Error( "ERROR: Seek in file '%s' / '%s' requested to position "
				"%lu but there are only %lu bytes available!\n",
				file->filename, iofsl->multifile_name, pos, size_sum);
		return -1;
	}

	ret = fseeko(file->file, real_pos, SEEK_SET);

#ifdef HAVE_ZLIB
	if ( NULL != file->z && 0 == ret ) {
		do {
			read = OTF_File_iofsl_read_internal( file, file->zbuffer, file->zbuffersize );

			OTF_FILE_Z(file)->next_in = file->zbuffer;
			OTF_FILE_Z(file)->avail_in = (uInt) read;
			OTF_FILE_Z(file)->total_in = 0;

			/* re-initialize z object */
			inflateReset(OTF_FILE_Z(file));

			/* do not sync at very beginning of compressed stream because it
			 would skip the first block */
			sync = Z_OK;

			if ( 0 != pos ) {
				sync = inflateSync( OTF_FILE_Z(file) );
			}

			if ( Z_OK == sync ) {
				return ret;
			}

			if ( Z_BUF_ERROR == sync ) {

				continue;
			}

			if ( Z_DATA_ERROR == sync ) {
				/* do not emit a warning, this happens with larger zbuffers */
				/* --> Chunks written were larger than the chunk we read here
				 * so zlib cannot find a valid flush point in this chunk
				 * --> Try the next chunk */
				continue;
			}

			if ( Z_STREAM_ERROR == sync ) {
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"Z_STREAM_ERROR.\n",
						__FUNCTION__, __FILE__, __LINE__ );
				return -1;
			}

		}
		while (1);
	}
#endif /* HAVE_ZLIB */
	return ret;
}

uint64_t OTF_File_iofsl_tell( OTF_File* file ) {

	if ( file->mode != OTF_FILEMODE_READ ) {
		return file->iofsl->pos;
	} else if ( file->mode == OTF_FILEMODE_READ ) {
		if ( file->iofsl->index_buffer != NULL ) {
			off_t cur_data_pos = ftello( file->file );
			size_t pos = 0;
			size_t index_pos = 0;

			while ( 1 ) {
				IndexEntry* entry = &( file->iofsl->index_buffer[index_pos] );
				index_pos++;

				if ( entry->length > 0 ) {
					/* Think carefully about it before changing the comparisons! */
					/* current position is behind this chunk */
					if ( (size_t)cur_data_pos > ( entry->offset + entry->length ) ) {
						pos += entry->length;
						continue;
					}

					/* current position is in this chunk */
					if ( (size_t)cur_data_pos >= entry->offset
							&& (size_t)cur_data_pos <= ( entry->offset + entry->length ) ) {
						pos += cur_data_pos - entry->offset;
						break;
					}

					/* current position is before this chunk -- this should not happen! */
					OTF_Error( "ERROR: OTF_File_iofsl_tell: Current position could not be evaluated!");

					return -1;
				} else {
					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"Failed to find index entry for data position %lu\n",
							__FUNCTION__, __FILE__, __LINE__, cur_data_pos);
					return -1;
				}
			}
			return pos;
		}
	}

	if ( NULL != file->file ) {

		file->pos = ftello( file->file );
	}

	return file->pos;
}

uint64_t OTF_File_iofsl_size( OTF_File* file ) {

	/* Iterate over the index file and sum up sizes of all chunks */
	if (file->iofsl->index_buffer != NULL)
	{
		size_t sum = 0;

		IndexEntry* entries = file->iofsl->index_buffer;

		int i = 0;
		do {
			sum += (entries[i++]).length;
		}
		while (entries[i].length > 0);

		return sum;
	}
	return 0;
}

int OTF_File_iofsl_close( OTF_File* file ) {

#ifdef HAVE_ZLIB
	size_t byteswritten;
	int status;
#endif /* HAVE_ZLIB */

	if ( NULL == file ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"file has not been specified.\n", __FUNCTION__, __FILE__, __LINE__ );

		return 0;
	}

#ifdef HAVE_ZLIB

	if ( NULL != file->z )  {

		if ( OTF_FILEMODE_WRITE != file->mode ) {

			inflateEnd( OTF_FILE_Z(file) );

		} else {

			size_t towrite;

			/* flush buffer */
			if ( 0 == OTF_File_iofsl_revive( file, OTF_FILEMODE_WRITE ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_File_iofsl_revive() failed.\n",
						__FUNCTION__, __FILE__, __LINE__);

				return 0;
			}

			status = deflate( OTF_FILE_Z(file), Z_FULL_FLUSH );
			if ( status == Z_STREAM_ERROR ) {

				OTF_Error( "ERROR in function %s, file %s, line %i\n"
					"deflate() failed.\n",
					__FUNCTION__, __FILE__, __LINE__ );
				return 0;
			}

			towrite = file->zbuffersize - OTF_FILE_Z(file)->avail_out;
			byteswritten = 0;

			if ( towrite > 0 )
				byteswritten = OTF_File_iofsl_write_internal( file, file->zbuffer, towrite );

			if ( towrite != byteswritten ) {
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n"
						"Failed to write compressed buffer of size %lu\n",
						__FUNCTION__, __FILE__, __LINE__, towrite );
			}

			while ( OTF_FILE_Z(file)->avail_out != file->zbuffersize ) {
				OTF_FILE_Z(file)->avail_out = file->zbuffersize;
				OTF_FILE_Z(file)->next_out = file->zbuffer;
				deflate( OTF_FILE_Z(file), Z_FULL_FLUSH );
				assert( status != Z_STREAM_ERROR );
				towrite = file->zbuffersize - OTF_FILE_Z(file)->avail_out;

				if ( towrite > 0 )
					OTF_File_iofsl_write_internal( file, file->zbuffer, towrite );
			}

			deflateEnd( OTF_FILE_Z(file) );
		}

	}

#endif /* HAVE_ZLIB */

	if ( file->mode == OTF_FILEMODE_WRITE )
	{
#ifdef HAVE_ZOIDFS
		int ret = OTF_File_iofsl_flushIndexBuffer( file );
		if ( ret != 1 ) {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"could not flush iofsl index buffer (%d) ",
					__FUNCTION__, __FILE__, __LINE__, ret );
			return 0;
		}
#else
		OTF_Error("Cannot write using IOFSL without ZOIDFS.");
		return 0;
#endif
	}

	if ( NULL != file->file ) {

		OTF_FileManager_suspendFile( file->manager, file );
	}

	OTF_File_iofsl_finalize( file );
	file = NULL;

	return 1;
}

OTF_FileStatus OTF_File_iofsl_status( OTF_File* file ) {
	return OTF_FILESTATUS_ACTIVE;
}

void OTF_File_iofsl_suspend( OTF_File* file ) {

	/* IOFSL files are not suspended */
	if ( file->mode == OTF_FILEMODE_WRITE ) {
		return;
	}

	/* TODO Check if that is correct */
	/* get status and close OS file */

	file->pos = ftello( file->file );

	fclose( file->file );

	file->file = NULL;
}

int OTF_File_iofsl_revive( OTF_File* file, OTF_FileMode mode ) {

	/* TODO check for correctness */
	switch ( mode ) {

	case OTF_FILEMODE_READ:

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

				while ( -1 == ( fd = open( file->iofsl->multifile_name, flags ) ) ) {

					/* if the user is not the owner of the file, open with
					 * O_NOATIME will fail with errno == EPERM;
					 * try to open without O_NOATIME again to avoid this problem */
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
								__FUNCTION__, __FILE__, __LINE__,
								file->iofsl->multifile_name );

						return 0;
					}

				}

				file->file = fdopen( fd, "r" );

			}

#else /* _GNU_SOURCE */
			file->file = fopen( file->iofsl->multifile_name, "rb" );

#endif /* _GNU_SOURCE */
			if ( NULL == file->file ) {

				/* show this error every time */
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open file %s for reading. Maybe the number of "
						"opened filehandles exceeds your system's limit\n",
						__FUNCTION__, __FILE__, __LINE__, file->iofsl->multifile_name );

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
			if ( 0 == OTF_FileManager_touchFile( file->manager, file ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"OTF_FileManager_touchFile() failed.\n",
						__FUNCTION__, __FILE__, __LINE__ );

				return 0;
			}
		}

		return 1;

	case OTF_FILEMODE_WRITE:

		/* this is IOFSL specific behavior! */
		/* only revive files that are to be read */
		return 1;

	case OTF_FILEMODE_SEEK:

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

				file->file = fopen( file->iofsl->multifile_name, "rb" );

				if ( NULL == file->file ) {

					/* show this error every time */
					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"cannot open file %s for reading. Maybe the number of "
							"opened filehandles exceeds your system's limit\n",
							__FUNCTION__, __FILE__, __LINE__, file->iofsl->multifile_name );

					return 0;
				}

				/* dont need to seek to the saved position because there
				 will be another seek anyway*/
				/*
				 fseeko( file->file, file->pos, SEEK_SET );
				 */

			} else {

				/* open first time */

				file->file = fopen( file->iofsl->multifile_name, "rb" );

				if ( NULL == file->file ) {

					/* show this error every time */
					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"cannot open file %s for reading. Maybe the number of "
							"opened filehandles exceeds your system's limit\n",
							__FUNCTION__, __FILE__, __LINE__, file->iofsl->multifile_name );

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
			if ( 0 == OTF_FileManager_touchFile( file->manager, file ) ) {

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

/* No implementation of
 * OTF_File_iofsl_setZBufferSize
 * because it is no different for IOFSL
 */

OTF_File* OTF_File_iofsl_open_zlevel( const char* filename, OTF_FileManager* manager,
		OTF_FileMode mode, OTF_FileCompression zlevel ) {

	uint32_t len;
	OTF_File* ret;
	int status;

	/* Check input parameters */

	if ( NULL == filename ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no filename has been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	if ( NULL == manager ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"manager has not been specified.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	ret = (OTF_File*) malloc( sizeof(OTF_File) );

	if ( NULL == ret ) {

		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n", __FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	if ( 0 != OTF_File_iofsl_init( ret ) ) {
		return NULL;
	}

	len = (uint32_t) strlen( filename );

	/* Begin IOFSL specific code */
	OTF_FileManager_getIofsl( manager, &(ret->iofsl->server_num),
			&(ret->iofsl->server_list), &(ret->iofsl->mode),
			&(ret->iofsl->flags), &(ret->iofsl->index_buffer_length),
			&(ret->iofsl->streamid_bits) );
	if ( ret->iofsl->index_buffer_length == 0 ) {
		ret->iofsl->index_buffer_length = OTF_IOFSL_INDEX_BUFFER_DEFAULTLENGTH;
	}

	/* We do not mess with this filename, but rather with iofsl->multifile_name, copy it anyway */
	ret->filename = strdup( filename );
	status = OTF_File_iofsl_prepareFilename( ret );
	if ( status == 0 ) return NULL;
	/* prepareFilename also does 	OTF_File_iofsl_assignServer(ret); */



	ret->iofsl->index_buffer = malloc( ret->iofsl->index_buffer_length * sizeof(IndexEntry) );
	assert( ret->iofsl->index_buffer != NULL );


	/* End IOFSL specific code */
	ret->mode = mode;
	if ( OTF_FILEMODE_READ == mode || OTF_FILEMODE_SEEK == mode ) {

#ifdef HAVE_ZLIB
		if ( 0 != access( ret->iofsl->multifile_name , F_OK ) ) {
			/* file not found, try '.z' suffix */
			len = strlen(ret->iofsl->multifile_name);
			strncpy(ret->iofsl->multifile_name + len, ".z", 3);

			if ( 0 != access( ret->iofsl->multifile_name, F_OK ) ) {
				/* file still not found, give up */
				OTF_File_iofsl_finalize( ret );
				return NULL;
			}
			strncpy(ret->iofsl->indexfile_name + len, ".z", 3);
			len = strlen( ret->iofsl->multifile_name );

			if ( len > 2 && strcmp( ret->iofsl->multifile_name + len - 2, ".z" ) == 0 ) {
				ret->z = malloc( sizeof(z_stream) );

				if ( NULL == ret->z ) {

					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"no memory left.\n",
							__FUNCTION__, __FILE__, __LINE__);

					OTF_File_iofsl_finalize( ret );
					return NULL;
				}

				OTF_FILE_Z(ret)->next_in = NULL;

				OTF_FILE_Z(ret)->avail_in = 0;
				OTF_FILE_Z(ret)->zalloc = NULL;
				OTF_FILE_Z(ret)->zfree = NULL;
				OTF_FILE_Z(ret)->opaque = NULL;

				inflateInit( OTF_FILE_Z(ret) );

				ret->zbuffer = malloc( ret->zbuffersize );
				OTF_FILE_Z(ret)->avail_in = 0;
				OTF_FILE_Z(ret)->next_in = ret->zbuffer;

				if ( NULL == ret->zbuffer ) {

					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"no memory left.\n",
							__FUNCTION__, __FILE__, __LINE__);

					OTF_File_iofsl_finalize( ret );
					return NULL;
				}
			}
		}

#else /* HAVE_ZLIB */
		if ( 0 != access( ret->iofsl->multifile_name, F_OK ) ) {

			strncpy( ret->iofsl->multifile_name + len, ".z", 3 );

			if ( 0 == access( ret->iofsl->multifile_name, F_OK ) ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"cannot open %s. Zlib is not enabled.\n",
						__FUNCTION__, __FILE__, __LINE__, ret->iofsl->multifile_name );
			}
			/* file still not found, give up */
			OTF_File_iofsl_finalize( ret );
			return ret;

		}
#endif /* HAVE_ZLIB */

		/* Finally we read the index file and write all indices to buffer */
		/* IOFSL specific */
		assert( ret->iofsl->id_str != NULL );
		assert( ret->iofsl->indexfile_name != NULL);

		ret->iofsl->indexfile = fopen( ret->iofsl->indexfile_name, "rb" );

		if ( ret->iofsl->indexfile == NULL ) {
			OTF_Error( "ERROR: in function %s, file: %s, line: %i:\n "
					"Failed to open index file '%s' for reading!\n",
					__FUNCTION__, __FILE__, __LINE__, ret->iofsl->indexfile_name );
			OTF_File_iofsl_finalize( ret );
			ret = NULL;
			return ret;
		}

		if ( OTF_File_iofsl_getIndexEntries( ret ) != 0 ) {
			OTF_File_iofsl_finalize( ret );
			ret = NULL;
			return ret;
		}

		/* index file is not needed anymore --> close it */
		fclose( ret->iofsl->indexfile );
		ret->iofsl->indexfile = NULL;
	} else {
#ifdef HAVE_ZOIDFS
		int tmp, created;
		zoidfs_sattr_t sattr;
		struct timeval now;

		/* filemode write */
		if ( !zoidfs_initialized ) {
			OTF_File_iofsl_checkServer( ret );
			if ( ZFS_OK != zoidfs_init() )
			{
				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"Failed to initialize connection to ZOIDFS server: %s!\n",
						__FUNCTION__, __FILE__, __LINE__, ret->iofsl->zoidfs_server);
				OTF_File_iofsl_finalize( ret );
				return NULL;
			}

			zoidfs_initialized = 1;
		}

		created = 0;
		memset( &sattr, 0, sizeof(sattr) );

		/* set the attrs */
		sattr.mask = ZOIDFS_ATTR_SETABLE;
		sattr.mode = 0644;
		sattr.uid = getuid();
		sattr.gid = getgid();

		gettimeofday( &now, NULL );
		sattr.atime.seconds = now.tv_sec;
		sattr.atime.nseconds = now.tv_usec;
		sattr.mtime.seconds = now.tv_sec;
		sattr.mtime.nseconds = now.tv_usec;

		OTF_File_iofsl_checkServer( ret );
		/* create the hint */
		ret->iofsl->zoidfs_hint = malloc( sizeof(zoidfs_op_hint_t) );
		if ( ret->iofsl->zoidfs_hint == NULL ) {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Failed to allocate %d bytes of memory.\n",
					__FUNCTION__, __FILE__, __LINE__, sizeof(zoidfs_op_hint_t));
			OTF_File_iofsl_finalize( ret );
			return NULL;
		}
		zoidfs_hint_create( ret->iofsl->zoidfs_hint );

		/* create the multifile */
		ret->iofsl->zoidfs_fh = malloc( sizeof(zoidfs_handle_t) );
		if ( ret->iofsl->zoidfs_fh == NULL ) {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Failed to allocate %d bytes of memory.\n",
					__FUNCTION__, __FILE__, __LINE__, sizeof(zoidfs_handle_t));
			OTF_File_iofsl_finalize( ret );
			return NULL;
		}

		assert( ret->iofsl->multifile_name != NULL );
		tmp = zoidfs_create( NULL, NULL, ret->iofsl->multifile_name, &sattr,
				ret->iofsl->zoidfs_fh, &created, ZOIDFS_NO_OP_HINT );
		if ( tmp != ZFS_OK ) {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Failed to create file '%s' with reason %i using ZOIDFS server %s!\n",
					__FUNCTION__, __FILE__, __LINE__, ret->iofsl->multifile_name,
					tmp, getenv( zoidfs_server_env ));
			/* free the handle here to avoid writing to
			 * a file that is not created */
			free( ret->iofsl->zoidfs_fh );
			ret->iofsl->zoidfs_fh = NULL;
			OTF_File_iofsl_finalize( ret );
			return NULL;
		}

		/* create the index file */
		assert( ret->iofsl->indexfile_name != NULL );
		OTF_File_iofsl_checkServer( ret );
		ret->iofsl->zoidfs_index_fh = malloc( sizeof(zoidfs_handle_t) );
		if ( ret->iofsl->zoidfs_index_fh == NULL ) {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Failed to allocate %d bytes of memory.\n",
					__FUNCTION__, __FILE__, __LINE__, sizeof(zoidfs_handle_t));
			OTF_File_iofsl_finalize( ret );
			return NULL;
		}
		tmp = zoidfs_create( NULL, NULL, ret->iofsl->indexfile_name, &sattr,
				ret->iofsl->zoidfs_index_fh, &created, ZOIDFS_NO_OP_HINT );

		if ( tmp != ZFS_OK ) {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Failed to create index file '%s' with reason %i using ZOIDFS!\n",
					__FUNCTION__, __FILE__, __LINE__, ret->iofsl->indexfile_name, tmp);
			zoidfs_remove( NULL, NULL, ret->filename, NULL, ZOIDFS_NO_OP_HINT );
			/* free the handle here to avoid writing to
			 * a file that is not created */
			free( ret->iofsl->zoidfs_index_fh );
			ret->iofsl->zoidfs_index_fh = NULL;
			OTF_File_iofsl_finalize( ret );
			return NULL;
		}

#ifdef HAVE_ZLIB
		/* is a .z appended to the file name */
		len = strlen( ret->filename );

		if ( len > 2 && 0 == strcmp( ret->filename + len - 2, ".z") ) {

			ret->z = malloc( sizeof(z_stream) );

			if ( NULL == ret->z ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"no memory left.\n",
						__FUNCTION__, __FILE__, __LINE__);

				OTF_File_iofsl_finalize( ret );
				return NULL;
			}

			OTF_FILE_Z(ret)->next_in = NULL;

			OTF_FILE_Z(ret)->avail_in = 0;
			OTF_FILE_Z(ret)->zalloc = NULL;
			OTF_FILE_Z(ret)->zfree = NULL;
			OTF_FILE_Z(ret)->opaque = NULL;
			OTF_FILE_Z(ret)->avail_out = ret->zbuffersize;
			OTF_FILE_Z(ret)->next_out = ret->zbuffer;

			deflateInit( OTF_FILE_Z(ret), zlevel );

			ret->zbuffer = malloc(ret->zbuffersize);

			if ( NULL == ret->zbuffer ) {

				OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
						"no memory left.\n",
						__FUNCTION__, __FILE__, __LINE__);

				OTF_File_iofsl_finalize( ret );
				return NULL;
			}
		}
#endif /* HAVE_ZLIB */
#else /* HAVE_ZOIDFS */
		OTF_Error( "ERROR opening stream for writing."
				" Zoidfs / iofsl writing not supported by this installation.\n" );
		OTF_File_iofsl_finalize( ret );
		return NULL;
#endif /* HAVE_ZOIDFS */

	}
	ret->manager = manager;

	return ret;
}

size_t OTF_File_iofsl_read_internal( OTF_File* file, void* vdest, size_t length ) {
	uint64_t actual_length = 0;
	size_t toread;
	size_t read;
	char *dest = (char *)vdest; /* to allow pointer arithmetic */

	/* No support in iofsl */
	assert( NULL == file->externalbuffer );

	if ( file->file == NULL ) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"File not open!\n",
				__FUNCTION__, __FILE__, __LINE__ );
		return 0;
	}

	if ( file->iofsl->index_buffer == NULL ) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"Index buffer does not exist!\n",
				__FUNCTION__, __FILE__, __LINE__);
		return 0;
	}

	/* first read the rest from the last chunk if any */
	if ( file->iofsl->data_left > 0 ) {
		toread = ( file->iofsl->data_left > length ) ? length : file->iofsl->data_left;
		read = fread( dest, 1, toread, file->file );

		if ( read != toread ) {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Expected to read %lu bytes but read only %lu\n",
					__FUNCTION__, __FILE__, __LINE__, toread, read);
		}

		length -= read;

		dest += read;
		file->iofsl->data_left -= read;
		actual_length = read;
	}

	/* now read the next chunk if required */
	while ( length > 0 ) {
		long unsigned offset = 0;
		long unsigned size = 0;

		IndexEntry* entry = &(file->iofsl->index_buffer[file->iofsl->index_buffer_pos]);

		if ( entry->length == 0 ) {
			return actual_length;
		}

		offset = entry->offset;

		size = entry->length;
		file->iofsl->index_buffer_pos++;

		/* Seek to the correct position */
		fseek( file->file, offset, SEEK_SET );
		file->iofsl->data_left = size;

		/* now read as many bytes as required / possible */
		toread = ( size > length ) ? length : size;
		read = fread( dest, 1, toread, file->file );

		if ( read != toread ) {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Expected to read %lu bytes but read only %lu\n",
					__FUNCTION__, __FILE__, __LINE__, toread, read);
			return actual_length;
		}

		length -= read;

		dest += read;
		file->iofsl->data_left -= read;
		actual_length += read;
	}

	return actual_length;
}

size_t OTF_File_iofsl_write_internal( OTF_File* file, const void* src, size_t length ) {
#if defined(HAVE_ZOIDFS)
	zoidfs_file_ofs_t newoff = 0;

	int ret;
	int flag = 0;
	IndexEntry* entry;
	char value[ZOIDFS_ATOMIC_APPEND_OFFSET_MAX_BYTES]; /* length defined in zoidfs-hints.h */

	if ( length == 0 ) {
		OTF_Warning( "WARNING in function %s, file: %s, line: %i:\n "
				"ignoring zero sized write.\n",
				__FUNCTION__, __FILE__, __LINE__);
		return 0;
	}

	OTF_File_iofsl_checkServer( file );
	ret = OTF_File_iofsl_resetHint( file );
	if ( ret != ZFS_OK ) return 0;

	ret = zoidfs_write( file->iofsl->zoidfs_fh, 1, &src, &length, 1,
			&newoff, (zoidfs_file_size_t *)&length, file->iofsl->zoidfs_hint );

	if ( ret != ZFS_OK ) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"zoidfs_write returned bad value: %i\n",
				__FUNCTION__, __FILE__, __LINE__, ret);
		return 0;
	}

	ret = zoidfs_hint_get( *(file->iofsl->zoidfs_hint), ZOIDFS_ATOMIC_APPEND_OFFSET,
			ZOIDFS_ATOMIC_APPEND_OFFSET_MAX_BYTES, value, &flag );

	if ( ret != ZFS_OK ) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"Failed to retrieve append offset!\n",
				__FUNCTION__, __FILE__, __LINE__);
		return 0;
	}

	newoff = (long unsigned)atol( value );

	/** do not try to write to the index file for global files (e.g. *.otf) */
	assert( file->iofsl->id_str != NULL );
	assert( NULL != file->iofsl->index_buffer );
	/* reallocate buffer if necessary */
	if ( file->iofsl->index_buffer_pos >= file->iofsl->index_buffer_length - 1 ) {
		IndexEntry* tmp = (IndexEntry*) realloc( file->iofsl->index_buffer,
					sizeof(IndexEntry) * ( file->iofsl->index_buffer_length
							+ OTF_IOFSL_INDEX_BUFFER_DEFAULTLENGTH ) );

		if ( tmp == NULL ) {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Failed to allocate %lu bytes of memory!",
					__FUNCTION__, __FILE__, __LINE__,
					sizeof(IndexEntry) * (file->iofsl->index_buffer_length
							+ OTF_IOFSL_INDEX_BUFFER_DEFAULTLENGTH ) );
			return 0;
		}

		file->iofsl->index_buffer = tmp;
		file->iofsl->index_buffer_length += OTF_IOFSL_INDEX_BUFFER_DEFAULTLENGTH;
	}

	entry = &(file->iofsl->index_buffer[file->iofsl->index_buffer_pos]);

	entry->offset = newoff;
	entry->length = length;
	file->iofsl->index_buffer_pos++;
	/* mark this entry as the last one */
	entry = &(file->iofsl->index_buffer[file->iofsl->index_buffer_pos]);
	entry->offset = 0;
	entry->length = 0;

	return length;
#else
	OTF_Error("ERROR, no IOFSL write support without zoidfs.");
	return 0;
#endif /* HAVE_ZOIDFS */
}

/**
 * Functions that exist only for OTF_File_iofsl
 */
static unsigned int htoi(const char *ptr)
{
	unsigned int value = 0;
	char ch = *ptr;

	while (ch == ' ' || ch == '\t')
	ch = *(++ptr);

	for (;;) {

		if (ch >= '0' && ch <= '9')
		value = (value << 4) + (ch - '0');
		else if (ch >= 'A' && ch <= 'F')
		value = (value << 4) + (ch - 'A' + 10);
		else if (ch >= 'a' && ch <= 'f')
		value = (value << 4) + (ch - 'a' + 10);
		else
		return value;

		ch = *(++ptr);
	}
}

/**
 * Internal function used by management layer to map the 'otf filename'
 * to the physical filename
 * e.g.
 * for VT LDIR files
 * file->name ==
 * fooooooooo.1e8dce3f.255991.1.def.z
 * ^prefix    ^nodeid  ^pid   ^ ^suffix
 *                            |stream id
 * => file->iofsl->multifile_name = fooooooooo.all.def.z
 * => file->iofsl->indexfile_name = fooooooooo.idx.def.z
 * => file->iofsl->id      = nodeid + pid
 *
 * for VT GDIR files
 * file->name ==
 * fooooooooo.123.def.z
 * ^prefix    ^   ^suffix
 *            |stream id
 * => file->iofsl->multifile_name = fooooooooo.all.def.z
 * => file->iofsl->indexfile_name = fooooooooo.idx.def.z
 * => file->iofsl->id      = stream_id
 *
 * @return 1 on success, 0 on failure
 */
int OTF_File_iofsl_prepareFilename( OTF_File *file )
{
	uint32_t streamid;
	size_t streamid_index;
	size_t id_str_len;
	uint8_t found_suffix;
	const char* post_streamid_pos;

	/* shortcut because we need it often */
	const char* filename = file->filename;
	OTF_File_iofsl *iofsl = file->iofsl;

	/* 2 (for .1. -> .all.), + 8 (.9999999 'split') + 2 for .z */
	size_t newfile_name_length = strlen(filename) + 2 + 8 + 2;

	assert( iofsl->multifile_name == NULL );
	assert( iofsl->indexfile_name == NULL );
	iofsl->multifile_name = calloc(newfile_name_length, sizeof(char));
	iofsl->indexfile_name = calloc(newfile_name_length, sizeof(char));
	if ( iofsl->multifile_name == NULL || iofsl->indexfile_name == NULL ) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__);
		return 0;
	}


	/* we need to replace the stream id with 'all' */
	/* search from the end of the string for all '.' until we find a digit behind one of them */
	streamid_index = strlen(filename);

	/* remember if we found the suffix def or events */
	found_suffix = 0;

	while ( streamid_index > 0 ) {
		if ( filename[--streamid_index] == '.' ) {
			/* if you find the suffixes 'z', 'def' or 'events' */
			if ( filename[streamid_index+1] == 'z' ) continue;

			if ( !found_suffix &&
					( strncmp( &(filename[streamid_index+1]), "events", 6 ) == 0 ||
					strncmp( &(filename[streamid_index+1]), "def", 3 ) == 0 ) ) {
				found_suffix = 1;
				continue;
			}

			/* this is the ID of the rank or thread */
			if ( isxdigit( filename[streamid_index+1] ) ) {
				break;
			}
		}
	}

	/* name_index now points to the dot before the suspected stream id part */

	/* points to the first dot after the stream id */
	post_streamid_pos = strchr(&filename[streamid_index+1], '.');

	/* first assume it's the stream number */
	streamid = htoi( &filename[streamid_index+1] );

	streamid_index++;
	/* streamid_index now really points to the beginning of the stream id part */
	/* creat the final new name from 3 parts, replacing the stream id with 'all' */
	assert( streamid_index + strlen("all") + strlen( post_streamid_pos )
		< newfile_name_length );
	strncpy( iofsl->multifile_name , filename, streamid_index );
	strcat( iofsl->multifile_name , "all" );
	strcat( iofsl->multifile_name , post_streamid_pos );

	assert( streamid_index + strlen("idx") + strlen( post_streamid_pos )
		< newfile_name_length );
	strncpy( iofsl->indexfile_name, filename, streamid_index );
	strcat( iofsl->indexfile_name, "idx" );
	strcat( iofsl->indexfile_name, post_streamid_pos );

	assert ( iofsl->id_str == NULL );
	id_str_len = post_streamid_pos - &(filename[streamid_index]);
	iofsl->id_str = malloc( id_str_len + 1 );

	if ( iofsl->id_str == NULL ) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"Failed to allocate %lu Bytes if memory\n",
				__FUNCTION__, __FILE__, __LINE__, id_str_len + 1);
		return 0;
	}

	strncpy( iofsl->id_str, &(filename[streamid_index]), id_str_len );
	iofsl->id_str[id_str_len] = '\0';

	/* For vtunify it is very important so that the master process
	 that handles ids 1 and the defs (0) use the same IOFSL server
	 for all their writing - otherwise hell breaks loose */
	if ( streamid == 0 ) streamid++;
	/* Apply the magic bitmask to filter for the process discarding the thread id
	 * to make sure all threads in a process use the same target forwarding server */
	iofsl->id = streamid & iofsl->streamid_bits;

	/* this will prepare the iofsl->server_id based on id and server infos */
	OTF_File_iofsl_assignServer(file);

	if ( iofsl->mode == OTF_IOFSL_MULTIFILE_SPLIT ) {
		if ( iofsl->server_id >= 0 && iofsl->id_str != NULL ) {
			size_t len = strlen( iofsl->multifile_name );
			assert( len == strlen( iofsl->indexfile_name ) );
			/* append server id and '.z' if required */

			if ( strcmp( iofsl->multifile_name + len - 2, ".z" ) == 0 ) {
				sprintf( iofsl->multifile_name + len - 2, ".%i.z", iofsl->server_id );
				sprintf( iofsl->indexfile_name + len - 2, ".%i.z", iofsl->server_id );
			} else {
				sprintf( iofsl->multifile_name + len, ".%i", iofsl->server_id );
				sprintf( iofsl->indexfile_name + len, ".%i", iofsl->server_id );
			}
		}
	}

	return 1;
}

/*
 * Assignes a server to an OTF_File
 * Assumes to be called only once
 * Uses an environment variable containing comma separated server names
 * Based on the server_id = iofsl->id % num_servers
 * Returns the server_id used
 * */
static void OTF_File_iofsl_assignServer( OTF_File* file )
{
	OTF_File_iofsl *iofsl = file->iofsl;
	iofsl->server_id = iofsl->id % iofsl->server_num;

#ifdef HAVE_ZOIDFS
	if ( iofsl->zoidfs_server != NULL ) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"Server already assigned for file %s (%s)\n",
				__FUNCTION__, __FILE__, __LINE__, file->filename, file->iofsl->zoidfs_server );
		return;
	}

	if ( iofsl->server_list ) {
		iofsl->zoidfs_server = iofsl->server_list[iofsl->server_id];
	}
#endif
}

/*
 * Checks if the environment Variable for the server is set
 * and resets if required
 * Assumes assignServer is called on the file before
 */
#ifdef HAVE_ZOIDFS
static int OTF_File_iofsl_writeIndexBuffer(OTF_File* file, char* strbuf, size_t len)
{
	zoidfs_file_ofs_t newoff;
	int ret;
	const void* idxptr;
	/* prepare the hint */
	ret = OTF_File_iofsl_resetHint(file);
	if (ret != ZFS_OK) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"Failed to reset iofsl hint (%i)\n",
				__FUNCTION__, __FILE__, __LINE__, ret);
		return -1;
	}

	newoff = 0;

	/* write the index entry */
	idxptr = (void*)strbuf;
	OTF_File_iofsl_checkServer(file);
	ret = zoidfs_write( file->iofsl->zoidfs_index_fh, 1, &(idxptr), &len, 1,
			&newoff, (zoidfs_file_size_t *)&len, file->iofsl->zoidfs_hint );

	if (ret != ZFS_OK) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"Failed to write index file entry (%i)\n",
				__FUNCTION__, __FILE__, __LINE__, ret);
		return -1;
	}

	return 0;
}
/** return 1 on success 0 on failure */
int OTF_File_iofsl_flushIndexBuffer(OTF_File* file)
{
	if ( file != NULL && file->iofsl->index_buffer != NULL ) {
		uint32_t i;
		size_t bufsz = OTF_IOFSL_INDEX_BUFFER_DEFAULTLENGTH;
		char* strbuf = malloc(bufsz);
		int ret;
		size_t strpos = 0;

		OTF_File_iofsl_checkServer( file );

		assert(strbuf != NULL);
		/* iterate over the entry and generate all entries */
		for (i = 0; i < file->iofsl->index_buffer_pos; i++) {
			IndexEntry* entry = &(file->iofsl->index_buffer[i]);
			ret = snprintf(strbuf + strpos, (bufsz - strpos), IOFSL_INDEX_FMT,
					file->iofsl->id_str, (unsigned long long int)entry->offset,
					(unsigned long long int)entry->length);
			/* flush buffer if necessary */

			if ( (size_t)ret >= (bufsz - strpos) ) {
				ret = OTF_File_iofsl_writeIndexBuffer( file, strbuf, strpos );
				if (ret != 0) {
					OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
							"Failed to write index buffer (%i)\n",
							__FUNCTION__, __FILE__, __LINE__, ret);
					return 0;
				}
				strpos = 0;
				/* repeat the operation */
				ret = snprintf(strbuf + strpos, (bufsz - strpos), IOFSL_INDEX_FMT,
						file->iofsl->id_str, (unsigned long long int)entry->offset,
						(unsigned long long int)entry->length);
			}

			strpos += ret;
		}

		ret = OTF_File_iofsl_writeIndexBuffer( file, strbuf, strpos );

		if (ret != ZFS_OK) {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Failed to write index buffer (%i)\n",
					__FUNCTION__, __FILE__, __LINE__, ret);
			return 0;
		}
		return 1;
	}
	return 0;
}

/**
 * Check if the environment variable to pick the right zoidfs server is set
 * and set if it is required.
 * Changing the server during runtime is not really supported
 * due to assumptionsin the ZOIDFS layer. It will try but yield an error
 * The main purpose of this function is to make sure two OTF files are not
 * written using different servers by the same process as hell might break loose.
 */
static void OTF_File_iofsl_checkServer(OTF_File* file)
{
	static int last_id;
	if ( file->iofsl->zoidfs_server == NULL || file->iofsl->server_id < 0 ) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"Server or id is not set for file '%s' / '%s'.\n",
				__FUNCTION__, __FILE__, __LINE__, file->filename, file->iofsl->multifile_name );
	}

	/* Using static variables is not exactly pretty, but using environment variables isn't either. */
	last_id = -1;
	if ( last_id != file->iofsl->server_id ) {
		if ( 0 != setenv( zoidfs_server_env, file->iofsl->zoidfs_server, 1 ) ) {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Failed to set environment variable %s\n",
					__FUNCTION__, __FILE__, __LINE__, zoidfs_server_env );
		}

		if ( last_id != -1 ) {
			/* something went wrong, this should not happen under nice circumstances */
			/* A different server env var was already set */
			/* Probably our host process, such as vtunify-mpi, works on different streams */
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Overwriting conflicting zoidfs server %i => %i ['%s' / '%s'].\n",
					__FUNCTION__, __FILE__, __LINE__,
					last_id, file->iofsl->server_id,
					file->filename, file->iofsl->multifile_name);

		}
		last_id = file->iofsl->server_id;
	}
}

/* Deletes all existing hints and sets the AA and (if required)
 * the non-blocking hint.
 */
static int OTF_File_iofsl_resetHint( OTF_File* file )
{
	int ret;
	/* prepare the hint */
	ret = zoidfs_hint_delete_all( *(file->iofsl->zoidfs_hint) );

	if ( ret != ZFS_OK ) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"Failed to delete all hints before setting a new one: %i\n",
				__FUNCTION__, __FILE__, __LINE__, ret);
		return -1;
	}

	ret = zoidfs_hint_set( *(file->iofsl->zoidfs_hint), ZOIDFS_ATOMIC_APPEND, ZOIDFS_HINT_ENABLED, 0 );

	if ( ret != ZFS_OK ) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"Failed to set atomic hint: %i!\n",
				__FUNCTION__, __FILE__, __LINE__, ret);
		return -1;
	}

	if ( file->iofsl->flags & OTF_IOFSL_FLAG_NONBLOCKING ) {
		ret = zoidfs_hint_set( *(file->iofsl->zoidfs_hint), ZOIDFS_NONBLOCK_SERVER_IO, ZOIDFS_HINT_ENABLED, 0 );

		if ( ret != ZFS_OK ) {
			OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
					"Failed to set nonblockin IO hint (%i)!\n",
					__FUNCTION__, __FILE__, __LINE__, ret );
		}
	}
	return ret;
}
#endif /* HAVE_ZOIDFS */

