/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include "OTF_Platform.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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


#include "OTF_Filenames.h"



#define OTF_FILENAMESUFFIX_MAIN		"otf"
#define OTF_FILENAMESUFFIX_DEF		"def"
#define OTF_FILENAMESUFFIX_EVENTS	"events"
#define OTF_FILENAMESUFFIX_SNAPS	"snaps"
#define OTF_FILENAMESUFFIX_STATS	"stats"
#define OTF_FILENAMESUFFIX_MARKER	"marker"

#include "OTF_Errno.h"


char* OTF_getFilename( const char* namestub, uint32_t id, OTF_FileType type,
		unsigned int l, char* ret ) {

	char* zsuffix = ((type&OTF_FILECOMPRESSION_BITS) > 0 && (type&OTF_FILECOMPRESSION_BITS) <= 9 ) ? ".z" : "";

	if ( ( NULL == ret ) || ( 0 == l ) ) {

		l= (unsigned int) strlen( namestub ) + 1 + 1 + 10 + 1 + 10 + 2; /* is this right ??? - at least it´s more than enough */
		ret= (char*) malloc( l * sizeof(char) );
	}

	if ( ( type & OTF_FILETYPE_IOFSL_ALL ) || ( type & OTF_FILETYPE_IOFSL_IDX ) ) {
		char *midfix = ( type & OTF_FILETYPE_IOFSL_ALL ) ? "all" : "idx";
		switch ( type&OTF_FILETYPE_BITS ) {
		case OTF_FILETYPE_MASTER:

			/* mastercontrol file stays uncompressed even with compression or iofsl*/
			snprintf( ret, l, "%s.%s", namestub, OTF_FILENAMESUFFIX_MAIN );
			break;

		case OTF_FILETYPE_GLOBAL_DEF:

			snprintf( ret, l, "%s.%s%s", namestub, OTF_FILENAMESUFFIX_DEF,
				zsuffix );
			break;

		case OTF_FILETYPE_DEF:

			snprintf( ret, l, "%s.%s.%s.%i%s", namestub, midfix, OTF_FILENAMESUFFIX_DEF,
				id, zsuffix );
			break;

		case OTF_FILETYPE_EVENT:

			snprintf( ret, l, "%s.%s.%s.%i%s", namestub, midfix, OTF_FILENAMESUFFIX_EVENTS,
				id, zsuffix );
			break;

		case OTF_FILETYPE_SNAPS:

			snprintf( ret, l, "%s.%s.%s.%i%s", namestub, midfix, OTF_FILENAMESUFFIX_SNAPS,
				id, zsuffix );
			break;

		case OTF_FILETYPE_STATS:
	
			snprintf( ret, l, "%s.%s.%s.%i%s", namestub, midfix, OTF_FILENAMESUFFIX_STATS,
				id, zsuffix );
			break;

		case OTF_FILETYPE_MARKER:

			snprintf( ret, l, "%s.%s.%s.%i%s", namestub, midfix, OTF_FILENAMESUFFIX_MARKER,
				id, zsuffix );
			break;

		default:
			free(ret);
			ret = NULL;
			return NULL;
		}

	return ret;
	}

	switch ( type&OTF_FILETYPE_BITS ) {
	case OTF_FILETYPE_MASTER:

		/* mastercontrol file stays uncompressed even with compression */
		snprintf( ret, l, "%s.%s", namestub, OTF_FILENAMESUFFIX_MAIN );
		break;

	case OTF_FILETYPE_GLOBAL_DEF:

		snprintf( ret, l, "%s.%s%s", namestub, OTF_FILENAMESUFFIX_DEF,
			zsuffix );
		break;

	case OTF_FILETYPE_DEF:

		snprintf( ret, l, "%s.%x.%s%s", namestub, id, OTF_FILENAMESUFFIX_DEF,
			zsuffix );
		break;

	case OTF_FILETYPE_EVENT:

		snprintf( ret, l, "%s.%x.%s%s", namestub, id, OTF_FILENAMESUFFIX_EVENTS,
			zsuffix );
		break;

	case OTF_FILETYPE_SNAPS:

		snprintf( ret, l, "%s.%x.%s%s", namestub, id, OTF_FILENAMESUFFIX_SNAPS,
			zsuffix );
		break;

	case OTF_FILETYPE_STATS:

		snprintf( ret, l, "%s.%x.%s%s", namestub, id, OTF_FILENAMESUFFIX_STATS,
			zsuffix );
		break;

	case OTF_FILETYPE_MARKER:

		snprintf( ret, l, "%s.%x.%s%s", namestub, id, OTF_FILENAMESUFFIX_MARKER,
			zsuffix );
		break;

	default:
		free(ret);
		ret = NULL;
		return NULL;
	}

	return ret;
}


/** strip the a filename from the ".otf" suffix if present */
char* OTF_stripFilename( const char* filename ) {


	char* ret= strdup( filename );
	char* p= ret;

	if( NULL == p ) {
	
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"no memory left.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		return NULL;
	}

	/* find last '.' and compare remainder with OTF_FILENAMESUFFIX_MAIN */
	p= strrchr( ret, '.' );
	if ( NULL != p && 0 == strcmp( p + 1, OTF_FILENAMESUFFIX_MAIN ) ) {
		*p= '\0';
	}

	/* fail if the resulting filename is empty */
	if ( '\0' == *ret ) {
		OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
				"empty filename base.\n",
				__FUNCTION__, __FILE__, __LINE__ );

		free( ret );
		return NULL;
	}

	return ret;
}


int OTF_fileExists( const char* filename ) {

	if ( 0 == access( filename, F_OK ) ) {

		return 1;

	} else {

		return 0;
	}
}

