/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_MasterControl.h
 *
 *  @brief Provides access to process-stream-mapping, which are located in
 *  .otf files.
 *
 *  \ingroup mc
 */

/** \defgroup mc Master Control Interface 
 *
 * This interface is dedicated to managing master control files ( *.otf )
 * A master control file contains a mapping from process streams
 * to processes and vice versa.
 *
 * \section mc_example A simple Example
 *
 *  This is example will create a simple mapping and read it again to
 *  show how reading and writing works with the master control file.
 *
 *  The need for creating your own master control file arises when you are not
 *  using the highlevel reader/writer, but the stream reader/writer.
 *
 *
 *  \code
 *  #include <stdio.h>
 *  #include <assert.h>
 *  #include "otf.h"
 *  
 *  int main( int argc, char** argv ) {
 *  \endcode
 *
 *      Declare a couple of variables.
 *      \code
 *      uint32_t streamcount;
 *      uint32_t a;
 *      uint32_t i;
 *      OTF_MapEntry* entry;
 *      OTF_FileManager* manager;
 *      OTF_MasterControl* mc;
 *      \endcode
 *  
 *      Initialize the file manager and the mastercontrol.
 *      \code
 *      manager= OTF_FileManager_open( 100 );
 *      assert( manager );
 *  
 *      mc = OTF_MasterControl_new( manager );
 *      \endcode
 *  
 *      Add four processes (100,101,102,103) to two streams (1,2)
 *      \code
 *      OTF_MasterControl_append( mc, 1, 100 );
 *      OTF_MasterControl_append( mc, 1, 101 );
 *      OTF_MasterControl_append( mc, 2, 102 );
 *      OTF_MasterControl_append( mc, 2, 103 );
 *      \endcode
 *  
 *      Write everything to the file "mytrace.otf"
 *      and close the master control.
 *      \code
 *      OTF_MasterControl_write( mc, "mytrace" );
 *      OTF_MasterControl_close( mc );
 *      \endcode
 *
 *      Now initialize the master control structure and read the
 *      newly written master control file.
 *      \code
 *      mc = OTF_MasterControl_new( manager );
 *      OTF_MasterControl_read( mc, "mytrace" );
 *      \endcode
 *  
 *      Visit all stream-process pairs and print them out
 *      \code
 *      streamcount = OTF_MasterControl_getCount( mc );
 *  
 *      for( i = 0; i < streamcount; ++i ) {
 *          entry = OTF_MasterControl_getEntryByIndex( mc, i );
 *          for( a = 0; a < entry->n; ++a ) {
 *              printf( " stream %u contains process %u\n", entry->argument,
 *                      entry->values[a] );
 *          }
 *      }
 *      \endcode
 *
 *      Clean everything up before exiting the program
 *      \code
 *      OTF_MasterControl_close( mc );
 *
 *  
 *      return 0;
 *  }
 *  \endcode
 *
 * Compile this using $ gcc -o test test.c `otfconfig --libs`.
 *
 * The program will show this output:
 * \verbatim
 *  stream 1 contains process 100
 *  stream 1 contains process 101
 *  stream 2 contains process 102
 *  stream 2 contains process 103 \endverbatim
 *
 * */

#ifndef OTF_MASTERCONTROL_H
#define OTF_MASTERCONTROL_H


#include <stdlib.h>
#include <stdio.h>
#include <string.h>


#include "OTF_inttypes.h"


#include "OTF_File.h"
#include "OTF_Filenames.h"
#include "OTF_WBuffer.h"
#include "OTF_RBuffer.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/** entry for 1:n mapping */
struct struct_OTF_MapEntry {

	/**	Unique argument ids. (stream ids) */
	uint32_t argument;

	/**	Current number of entries in 'values'. */
	uint32_t n;

	/**	Current size of array 'values'. */
	uint32_t s;

	/**	List of size 's' containing 'n' sorted entries of unique value ids
	(process ids). */
	uint32_t* values;
};
/** entry for 1:n mapping \ingroup mc */
typedef struct struct_OTF_MapEntry OTF_MapEntry;


/** entry for 1:1 mapping */
struct struct_OTF_Pair {

	uint32_t argument;
	uint32_t value;
};
/** entry for 1:1 mapping \ingroup mc */
typedef struct struct_OTF_Pair OTF_Pair;


/**	Data structure that collects the information about which stream contains
	which parts of a multi-file trace. This is supposed to be an 1:1 copy of 
	the information in the master control file. */
struct struct_OTF_MasterControl {


	/**	Current number of entries in 'map'. */
	uint32_t n;

	/**	Current size of array 'map'. */
	uint32_t s;

	/**	Mapping from stream ids (argument) to process ids (values).
		1:n mapping since a stream can contain multiple processes.
		This mapping is the authoritative source as listed in the master
		control file. */
	OTF_MapEntry* map;


	/**	Current number of entries in 'rmap'. */
	uint32_t rn;

	/**	Current size of array 'rmap'. */
	uint32_t rs;

	/**	Reverse mapping to 'map'. this is an 1:1 mapping since every
		process belongs to exactly one stream.
		This recursive mapping is to be derived from the authoritative source
		in 'map'. */
	OTF_Pair* rmap;

	/** file handle manager */
	OTF_FileManager* manager;
};
/** master control object. This object contains all information of the master
 * control file. \ingroup mc */
typedef struct struct_OTF_MasterControl OTF_MasterControl;


/**
 * Creates an empty OTF_MasterControl object.
 * The returned object must be freed by OTF_MasterControl_close().
 *
 * @param  manager   File handle manager.
 *
 * @return           newly created master control object
 *
 * \ingroup mc
 */
OTF_MasterControl* OTF_MasterControl_new( OTF_FileManager* manager );


/**
 * INTERFACE CHANGED!
 * Read a master control file according to namestub, reset the _existing_
 * OTF_MasterControl structure and fill it according to the file.
 *
 * @param mc    Pointer to an initialized OTF_Mastercontrol object. See also
 *              OTF_MasterControl_new().
 *
 * @return      1 on success, 0 if an error occurs
 *
 * \ingroup mc
 */
int OTF_MasterControl_read(OTF_MasterControl* mc, const char* namestub );


/**	Destructor, delete OTF_MasterControl object. DEPRECATED \ingroup mc*/
void OTF_MasterControl_finish( OTF_MasterControl* mc );

/**
 * Deletes a OTF_MasterControl object.
 *
 * @param mc    Pointer to an initialized OTF_Mastercontrol object. See also
 *              OTF_MasterControl_new().
 *
 * \ingroup mc
 */
void OTF_MasterControl_close( OTF_MasterControl* mc );

/**
 * Makes a clone of an existing OTF_MasterControl object.
 *
 * @param mc        Pointer to an initialized OTF_Mastercontrol object.
 *                  See also OTF_MasterControl_new().
 * @param manager   File handle manager.
 *
 * @return          Cloned master control object
 *
 * \ingroup mc
 */
OTF_MasterControl* OTF_MasterControl_clone( OTF_MasterControl* mc,
	OTF_FileManager* manager );

/**
 * Append the mapping argument -> value to the master control structure,
 *
 * @param mc        Pointer to an initialized OTF_Mastercontrol object. See also
 *                  OTF_MasterControl_new().
 * @param argument  Represents the stream identifier.
 * @param value     Represents the process identifier.
 *
 * @return          1 on sucess, 0 if an error occurs (e.g. the new pair
 *                  conflicts with the current mapping).
 *
 * \ingroup mc
 */
int OTF_MasterControl_append( OTF_MasterControl* mc,
	uint32_t argument, uint32_t value );


/**
 * Append the mapping argument -> ( list of l values ) to the master control
 * structure. This is equal to calling 'OTF_MasterControl_append()' with every
 * value in list separately.
 *
 * @param mc        Pointer to an initialized OTF_Mastercontrol object. See also
 *                  OTF_MasterControl_new().
 * @param argument  Represents the stream identifier.
 * @param l         Number of elements in the value array.
 * @param values    Array of process identifiers belonging to the stream.
 *
 * @return          1 on success, 0 if an error occurs (e.g. the new entries
 *                  conflict with the current mapping).
 *
 *
 *
 * \ingroup mc
 */
int OTF_MasterControl_appendList( OTF_MasterControl* mc, uint32_t argument,
	uint32_t l, uint32_t* values );


/**
 * Returns the argument to the given value. If no mapping was defined
 * make up a new one.
 *
 * @param mc        Pointer to an initialized OTF_Mastercontrol object. See also
 *                  OTF_MasterControl_new().
 * @param value     Represents the process identifier.
 *
 * @return          Argument to which the value belongs to.
 *
 * \ingroup mc
 */
uint32_t OTF_MasterControl_mapReverse( OTF_MasterControl* mc, uint32_t value );


/**
 * Writes a master control file with the current contents of the given
 * object.
 *
 * @param mc        Pointer to an initialized OTF_Mastercontrol object. See also
 *                  OTF_MasterControl_new().
 * @param namestub  File name prefix which.
 *
 * @return          1 on success, 0 if an error occurs.
 * 
 * \ingroup mc
 */
int OTF_MasterControl_write( OTF_MasterControl* mc, const char* namestub );


/**
 * Checks if the current mapping is consistent in itself.
 *
 * @param mc  Pointer to an initialized OTF_Mastercontrol object. See also
 *            OTF_MasterControl_new().
 *
 * @return    1 on success, 0 if the mapping is not consistent in itself.
 *
 * \ingroup mc
 */
int OTF_MasterControl_check( OTF_MasterControl* mc );


/**
 * Prints the mapping to stderr.
 *
 * @param mc  Pointer to an initialized OTF_Mastercontrol object. See also
 *            OTF_MasterControl_new().
 *
 * \ingroup mc
 */
void OTF_MasterControl_print( OTF_MasterControl* mc );


/**
 * Returns the entry for the given argument.
 *
 * @param mc        Pointer to an initialized OTF_Mastercontrol object. See also
 *                  OTF_MasterControl_new().
 * @param argument  Represents the stream identifier.
 *
 * @return          The map entry belonging to the argument, or NULL if an error
 *                  occurs.
 *
 * \ingroup mc
 */
OTF_MapEntry* OTF_MasterControl_getEntry( OTF_MasterControl* mc,
	uint32_t argument );


/**
 * Returns the entry for the given index.
 *
 * @param mc        Pointer to an initialized OTF_Mastercontrol object. See also
 *                  OTF_MasterControl_new().
 * @param index     Index of the entry to return.
 *
 * @return          The map entry belonging to the index, or NULL if the index
 *                  exceeds the mapping.
 *
 * \ingroup mc
 */
OTF_MapEntry* OTF_MasterControl_getEntryByIndex( OTF_MasterControl* mc,
	uint32_t index );


/**
 * Returns a pair of value and argument for the given index.
 *
 * @param mc        Pointer to an initialized OTF_Mastercontrol object. See also
 *                  OTF_MasterControl_new().
 * @param index     Index of the pair in the reverse mapping.
 *
 * @return          Pair of value and argument belonging
 *                  to the index, or NULL if the index exceeds the reverse
 *                  mapping.
 *
 * \ingroup mc
 */
OTF_Pair* OTF_MasterControl_getREntryByIndex( OTF_MasterControl* mc,
	uint32_t index );


/**
 * Returns the number of arguments in the current list.
 *
 * @param mc   Pointer to an initialized OTF_Mastercontrol object. See also
 *             OTF_MasterControl_new().
 * 
 * @return     Number of entrys in the current list.
 *             (equals the number of streams)
 *
 * \ingroup mc
 */
uint32_t OTF_MasterControl_getCount( OTF_MasterControl* mc );

/**
 * Returns the number of arguments in current reverse list.
 *
 * @param mc   Pointer to an initialized OTF_Mastercontrol object. See also
 *             OTF_MasterControl_new().
 * 
 * @return     Number of entrys in the current reverse list.
 *             (equals the number of processes)
 *
 * \ingroup mc
 */
uint32_t OTF_MasterControl_getrCount( OTF_MasterControl* mc );

/**
 * Returns the number of values for the given argument.
 *
 * @param mc        Pointer to an initialized OTF_Mastercontrol object. See also
 *                  OTF_MasterControl_new().
 * @param argument  Represents the stream identifier.
 *
 * @return          Number of values for the given argument, or
 *                  0 if the argument is unknown.
 *
 * \ingroup mc
 */
uint32_t OTF_MasterControl_getValueCount( OTF_MasterControl* mc,
	uint32_t argument );


/**
 * Returns a pointer to the value array for 'argument'. Get size of list with
 * 'OTF_MasterControl_getValueCount()'.
 *
 * @param mc        Pointer to an initialized OTF_Mastercontrol object. See also
 *                  OTF_MasterControl_new().
 * @param argument  Represents the stream identifier.
 *
 * @return          Pointer to the value array for 'argument', or NULL if an
 *                  error occurs.
 *
 * \ingroup mc
 */
uint32_t* OTF_MasterControl_getValues( OTF_MasterControl* mc, 
	uint32_t argument );


/**
 * Returns a previously unused argument. Of course, one cannot avoid
 * collisions with arguments explicitly defined later on.
 *
 * @param mc        Pointer to an initialized OTF_Mastercontrol object. See also
 *                  OTF_MasterControl_new().
 *
 * @return          Unused argument.
 *
 * \ingroup mc
 */
uint32_t OTF_MasterControl_getNewStreamId( OTF_MasterControl* mc );


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_MASTERCONTROL_H */

