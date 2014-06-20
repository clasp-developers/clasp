/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#include <map>
#include <otf.h>

using namespace std;

#include "otfshrink.h"

/* required as FirstHandlerArg later on */
typedef struct {

	OTF_Writer *writer;

	/* currntly, 'writer' is the only member but I want to keep this 
	struct to transport everything which is now in global variables */

} firstarg;


int handleDefProcess (void *userData, uint32_t stream, uint32_t process,
		 const char *name, uint32_t parent, OTF_KeyValueList* list);

int handleDefProcessGroup (void *userData, uint32_t stream, uint32_t procGroup,
		const char *name, uint32_t numberOfProcs, const uint32_t *procs,
		OTF_KeyValueList* list);

int handleDefProcessSubstitutes (void* userData, uint32_t stream,
		uint32_t representative, uint32_t numberOfProcs,
		const uint32_t* procs, OTF_KeyValueList* list);
