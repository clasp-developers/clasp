#ifndef _RFG_FILTER_H
#define _RFG_FILTER_H

#include "vt_inttypes.h"

/* bits for filter flags bitmask*/
#define RFG_FILTER_FLAG_GROUP     1
#define RFG_FILTER_FLAG_RECURSIVE 2

/* max. number of regions in a call path */
#define RFG_FILTER_MAX_CPATH_SIZE 0x80

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef struct RFG_Filter_struct RFG_Filter;

/* data structure for call-path filter rules */

typedef struct RFG_FilterCallPathRules_struct
{
  /* hash value of region id array */
  uint32_t hash;

  /* number of region ids in call path */
  uint32_t size;

  /* array of region ids in call-path */
  uint32_t regionIds[RFG_FILTER_MAX_CPATH_SIZE];

  /* call limit */
  int32_t callLimit;

} RFG_FilterCallPathRules;

/* initializes RFG filter object */
RFG_Filter* RFG_Filter_init( void );

/* cleanup RFG filter object */
int RFG_Filter_free( RFG_Filter* filter );

/* reset filter rules */
int RFG_Filter_reset( RFG_Filter* filter );

/* sets pointer to a external function which generates region ids */
int RFG_Filter_setRegionIdGenFunc( RFG_Filter* filter, uint32_t (*func)(void) );

/* gets region id by region name, if it's generated during reading call-path
   filter rules */
uint32_t RFG_Filter_getRegionId( RFG_Filter* filter, const char* regionName );

/* sets filter definition file name */
int RFG_Filter_setDefFile( RFG_Filter* filter, const char* fileName );

/* reads region filter definition file
   if rank != -1, read file with MPI-rank specific entries,
   if isRankOff != 0 after the call, then tracing should be disabled
   completely for the current rank, existing information should be discarded. */
int RFG_Filter_readDefFile( RFG_Filter* filter, int rank,
                            uint8_t* r_isRankOff );

/* adds region filter rules */
int RFG_Filter_addRegionRules( RFG_Filter* filter, const char* pattern,
                               int32_t callLimit, uint32_t* stackBounds,
                               uint8_t flags );

/* gets region filter rules by region/group name */
int RFG_Filter_getRegionRules( RFG_Filter* filter, const char* regionName,
                               const char* groupName, int32_t* r_callLimit,
                               uint32_t* r_stackBounds, uint8_t* r_flags );

/* adds call-path filter rules
   translates given region names into ids (r_regionIds) and generates
   a hash value for them (r_hash) */
int RFG_Filter_addCallPathRules( RFG_Filter* filter, uint32_t size,
                                 const char** regionNames, int32_t callLimit,
                                 uint32_t* r_hash, uint32_t** r_regionIds );

/* gets call-path filter rules (i.e. call limit)
   Note: other than RFG_Filter_getRegionRules, this function returns 0 if no
   matching filter rule was found */
int RFG_Filter_getCallPathRules( RFG_Filter* filter, uint32_t hash,
                                 uint32_t size, const uint32_t* regionIds,
                                 int32_t* r_callLimit );

/* gets all call-path filter rules */
int RFG_Filter_getAllCallPathRules( RFG_Filter* filter,
                                    uint32_t* r_numRules,
                                    RFG_FilterCallPathRules** r_rules );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _RFG_FILTER_H */
