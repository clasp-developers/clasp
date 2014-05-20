#ifndef _RFG_GROUPS_H
#define _RFG_GROUPS_H

#include "vt_inttypes.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef struct RFG_Groups_struct RFG_Groups;

/* initalizes RFG groups object */
RFG_Groups* RFG_Groups_init( void );

/* cleanup RFG groups object */
int RFG_Groups_free( RFG_Groups* groups );

/* sets group definition file name */
int RFG_Groups_setDefFile( RFG_Groups* groups, const char* fileName );

/* reads group definition file */
int RFG_Groups_readDefFile( RFG_Groups* groups );

/* adds group assignment */
int RFG_Groups_addAssign( RFG_Groups* groups, const char* groupName,
			  const char* pattern );

/* gets group name by region name */
int RFG_Groups_get( RFG_Groups* groups, const char* regionName,
		    char** r_groupName );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _RFG_GROUPS_H */
