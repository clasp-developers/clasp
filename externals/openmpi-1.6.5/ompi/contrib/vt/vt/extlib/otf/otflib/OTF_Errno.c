#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include "OTF_Definitions.h"
#include "OTF_Errno.h"

#include <stdarg.h>
#include <stdio.h>


char otf_strerr[OTF_ERR_LEN] = "No errors occurred.";
int otf_errno = OTF_NO_ERROR;

void OTF_Error( const char* format, ... ) {

	va_list ap;
	va_start( ap, format );

	vsnprintf( otf_strerr, OTF_ERR_LEN, format, ap );
	otf_errno = OTF_ERROR;
#ifdef OTF_VERBOSE
	fprintf( stderr, "%s", otf_strerr );
#endif /* OTF_VERBOSE */

	va_end( ap );
}

void OTF_Warning( const char* format, ... ) {

	va_list ap;
	va_start( ap, format );

#ifdef OTF_VERBOSE
	vfprintf( stderr, format, ap );
#endif /* OTF_VERBOSE */

	va_end( ap );
}
