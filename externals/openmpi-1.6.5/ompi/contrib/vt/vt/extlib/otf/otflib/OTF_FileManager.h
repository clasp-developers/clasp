/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_FileManager.h
 * 
 *  @brief Manages file handles.
 *
 *  i.e. Opens, closes and suspends files, if there are not enough
 *  filehandles available.
 *
 *  \ingroup fm
 */

/**
 * \defgroup fm File Manager Interface
 *
 * The file manager schedules an unlimited number OTF_Files to a limited
 * number of actual open OS files. Therefore all open are registered with
 * this manager. When a file is requested while no more OS files are
 * available any of the other files are suspended, i.i. the OS file is
 * closed.
 */


#ifndef OTF_FILEMANAGER_H
#define OTF_FILEMANAGER_H


#include "OTF_inttypes.h"
#include "OTF_Definitions.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

struct struct_OTF_File;
typedef struct struct_OTF_File OTF_File;
typedef struct struct_OTF_File_iofsl OTF_File_iofsl;

struct struct_OTF_FileManager;
/** file manager object \ingroup fm */
typedef struct struct_OTF_FileManager OTF_FileManager;

/** Generates a new file manager with a maximum number of files that are allowed
to be open simultaneously. \ingroup fm */
OTF_FileManager* OTF_FileManager_open( uint32_t number );

/** Closes the file manager \ingroup fm */
void OTF_FileManager_close( OTF_FileManager* m );

/** Returns the number of files currently open. */
uint32_t OTF_FileManager_getCount( OTF_FileManager* m );

/** Returns the numbner of files allowed to be open simultaneously. */
uint32_t OTF_FileManager_getNumber( OTF_FileManager* m );

/** Sets the number of files allowed to be open simultaneously. */
uint32_t OTF_FileManager_setNumber( OTF_FileManager* m, uint32_t number );

/** Ensure there is a free file handle available after this call. 
return 1 on success, 0 otherwise (which is not supposed to happen) */
int OTF_FileManager_guaranteeFile( OTF_FileManager* m );

/** Registers the 'file' as open. Return 1 on success, 0 otherwise. */
int OTF_FileManager_registerFile( OTF_FileManager* m, OTF_File* file );

/** Marks currently opened 'file' as used which is important for the 
scheduling strategy, i.e. the internal decision which file to suspend next.
return 1 on success or 0 for an suspended file. */
int OTF_FileManager_touchFile( OTF_FileManager* m, OTF_File* file );

/** Suspend an open file explicitly. this may be called externaly or 
internally. Return 1 on success, 0 otherwise. */
int OTF_FileManager_suspendFile( OTF_FileManager* m, OTF_File* file );

/**
 * @param enabled - enable or disable iofsl
 * @param server_num - number of servers used
 * @param server_list - comma separated string containing the actual server adresses, only used by writing
 * @param mode - enum, either OTF_IOFSL_MULTIFILE or OTF_IOFSL_MULTIFILE_SPLIT
 * @param flags - e.g. nonblocking
 * @param index_buffer_length - length of the index buffer (16 byte entries)
 * @param streamid_bits - magic bitmask that is used to filter out process id from a stream id
 * @return 1 on success, 0 on failure
 */
int OTF_FileManager_setIofsl( OTF_FileManager *m, uint32_t server_num, char **server_list, OTF_IofslMode mode, uint32_t flags, uint32_t index_buffer_length, uint32_t streamid_bits );
/**
 * similar to @OTF_FileManager_setIofsl, but with reference parameters
 * return 1 if enabled, 0 if disabled
 */
int OTF_FileManager_getIofsl( OTF_FileManager *m, uint32_t *server_num, char ***server_list, OTF_IofslMode *mode, uint32_t *flags, uint32_t *index_buffer_length, uint32_t *streamid_bits );

/**
 * Check if iofsl is enabled on this filemanager
 */
int OTF_FileManager_isIofsl( OTF_FileManager *m );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_FILEMANAGER_H */

