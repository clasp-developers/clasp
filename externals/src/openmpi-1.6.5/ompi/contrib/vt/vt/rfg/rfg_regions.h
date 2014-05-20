#ifndef _RFG_REGIONS_H
#define _RFG_REGIONS_H

#include "rfg_filter.h"
#include "rfg_groups.h"

#include "vt_inttypes.h"
#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef struct RFG_Regions_struct RFG_Regions;

/* data structure for region info */

typedef struct RFG_RegionInfo_struct
{
  /* region id */
  uint32_t regionId;

  /* group name */
  char* groupName;

  /* region name */
  char* regionName;

  /* call limit */
  int32_t callLimit;

  /* call limit count down */
  int32_t callLimitCD;

  /* stack level bounds */
  uint32_t stackBounds[2];

  /* flags bitmask (group, recursiveness) */
  uint8_t flags;

} RFG_RegionInfo;

/* data structure for call-path info */

typedef struct RFG_CallPathInfo_struct
{
  /* hash value of region id array */
  uint32_t hash;

  /* number of region ids in call path */
  uint32_t size;

  /* array of region ids in call-path */
  uint32_t regionIds[RFG_FILTER_MAX_CPATH_SIZE];

  /* call limit */
  int32_t callLimit;

  /* call limit count down */
  int32_t callLimitCD;

} RFG_CallPathInfo;

/* initializes RFG regions object */
RFG_Regions* RFG_Regions_init( void );

/* duplicates RFG regions object */
RFG_Regions* RFG_Regions_dup( const RFG_Regions* oldRegions );

/* cleanup RFG regions object */
int RFG_Regions_free( RFG_Regions* regions );

/* get RFG filter object associated with given regions object */
RFG_Filter* RFG_Regions_getFilter( RFG_Regions* regions );

/* get RFG groups object associated with given regions object */
RFG_Groups* RFG_Regions_getGroups( RFG_Regions* groups );

/* sets pointer to a function which generates region ids */
int RFG_Regions_setRegionIdGenFunc( RFG_Regions* regions,
                                    uint32_t (*func)(void) );

/* gets region id by region name, if it's generated during reading call-path
   filter rules */
uint32_t RFG_Regions_getRegionId( RFG_Regions* regions,
                                  const char* regionName );

/* sets region filter definition file */
int RFG_Regions_setFilterDefFile( RFG_Regions* regions, const char* fileName );

/* sets region grouping definition file */
int RFG_Regions_setGroupsDefFile( RFG_Regions* regions, const char* fileName );

/* reads region filter definition file
   if rank != -1, read file with MPI-rank specific entries,
   if isRankOff != 0 after the call, then tracing should be disabled
   completely for the current rank, existing information should be discarded. */
int RFG_Regions_readFilterDefFile( RFG_Regions* regions, int rank,
                                   uint8_t* r_isRankOff );

/* reads region grouping definition file */
int RFG_Regions_readGroupsDefFile( RFG_Regions* regions );

/* adds group assignment */
int RFG_Regions_addGroupAssign( RFG_Regions* regions, const char* groupName,
                                int n, ... );

/* function that should be called if a region enter event invoked */
int RFG_Regions_stackPush( RFG_Regions* regions, uint32_t regionId,
                           RFG_RegionInfo** r_regionInfo,
                           RFG_CallPathInfo** r_cpathInfo,
                           uint8_t* r_wasApproved );

/* function that should be called if a region leave event invoked */
int RFG_Regions_stackPop( RFG_Regions* regions, RFG_RegionInfo** r_regionInfo,
                          RFG_CallPathInfo** r_cpathInfo,
                          uint8_t* r_wasApproved );

/* adds region */
RFG_RegionInfo* RFG_Regions_add( RFG_Regions* regions, uint32_t regionId,
                                 const char* regionName,
                                 const char* groupName );

/* gets region info by region id */
RFG_RegionInfo* RFG_Regions_get( RFG_Regions* regions, uint32_t regionId );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _RFG_REGIONS_H */
