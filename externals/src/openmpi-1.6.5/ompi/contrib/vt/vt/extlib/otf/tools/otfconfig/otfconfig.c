/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include <stdio.h>
#include <string.h>

#include "OTF_inttypes.h"
#include "OTF_Platform.h"
#include "otf.h"

#define SHOW_HELPTEXT { \
	int l = 0; while( Helptext[l] ) { printf( "%s", Helptext[l++] ); } }

static const char* Helptext[] = {
"                                                                           \n",
" otfconfig - Show parameters of the OTF configuration.                     \n",
"                                                                           \n",
" Syntax: otfconfig [options]                                               \n",
"                                                                           \n",
"   options:                                                                \n",
"      -h, --help    show this help message                                 \n",
"      -V, --version show OTF version                                       \n",
"      --have-zlib   is zlib enabled                                        \n",
"      --have-zoidfs is ZOIDFS for IOFSL enabled                            \n",
"      --includes    path to the otf headers                                \n",
"      --libs        libline needed for linking otf                         \n",
"      --sizes       print size of integer types                            \n",
"                                                                           \n",
NULL };

int main( int argc, char** argv ) {


	int i;
	char includes[1024]= "";
	char libs[1024]= "";


	if( argc == 1 ) {

		SHOW_HELPTEXT;
		return 0;
	}

	for( i= 1; i < argc; ++i ) {

		if( 0 == strcmp( argv[i], "-h" ) ||
		    0 == strcmp( argv[i], "--help" ) ) {

			SHOW_HELPTEXT;
			return 0;

		} else if ( 0 == strcmp( argv[i], "-V" ) ||
		            0 == strcmp( argv[i], "--version" ) ) {

  			printf( "%u.%u.%u \"%s\"\n",
				OTF_VERSION_MAJOR, OTF_VERSION_MINOR,
				OTF_VERSION_SUB, OTF_VERSION_STRING) ;
			return 0;

		} else if ( 0 == strcmp( argv[i], "--have-zlib" ) ) {

#ifdef HAVE_ZLIB
			printf( "yes\n" );
#else /* HAVE_ZLIB */
			printf( "no\n" );
#endif /* HAVE_ZLIB */
			return 0;

		} else if ( 0 == strcmp( argv[i], "--have-zoidfs" ) ) {

#ifdef HAVE_ZOIDFS
			printf( "yes\n" );
#else /* HAVE_ZOIDFS */
			printf( "no\n" );
#endif /* HAVE_ZOIDFS */
			return 0;

		} else if ( 0 == strcmp( argv[i], "--includes" ) ) {

			if ( !(*includes) ) {

				strncpy( includes, "-I"OTFCONFIG_INCLUDEDIR,
					 sizeof( includes) - 1 );
			}

		} else if ( 0 == strcmp( argv[i], "--libs" ) ) {

		        if ( !(*libs) ) {

				strncpy( libs, "-L"OTFCONFIG_LIBDIR" "
					 OTFCONFIG_LIBS, sizeof( libs ) - 1 );
			}

		} else if ( 0 == strcmp( argv[i], "--sizes" ) ) {

			/* print size of integer types */
			printf( " sizeof(%s)= %llu\n", "  int8_t  ",
				(long long unsigned) sizeof(int8_t) );
			printf( " sizeof(%s)= %llu\n", "  int16_t ",
				(long long unsigned) sizeof(int16_t) );
			printf( " sizeof(%s)= %llu\n", "  int32_t ",
				(long long unsigned) sizeof(int32_t) );
			printf( " sizeof(%s)= %llu\n", "  int64_t ",
				(long long unsigned) sizeof(int64_t) );
			printf( " sizeof(%s)= %llu\n", " uint8_t  ",
				(long long unsigned) sizeof(uint8_t) );
			printf( " sizeof(%s)= %llu\n", " uint16_t ",
				(long long unsigned) sizeof(uint16_t) );
			printf( " sizeof(%s)= %llu\n", " uint32_t ",
				(long long unsigned) sizeof(uint32_t) );
			printf( " sizeof(%s)= %llu\n", " uint64_t ",
				(long long unsigned) sizeof(uint64_t) );

			return 0;
		}
	}

	if ( *includes ) {

		printf( "%s%c", includes, *libs ? ' ' : '\n' );
	}
	if ( *libs ) {

		printf( "%s\n", libs );
	}

	return 0;
}
