#include "config.h"

#include "rfg_regions.h"
#include "rfg_filter.h"
#include "rfg_groups.h"

#include "vt_inttypes.h"

#include "util/hash.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define REGION_INFO_HASH_MAX 0x400 /* size of hash table for region infos */
#define CPATH_INFO_HASH_MAX  0x400 /* size of hash table for call-path infos */

#define STACK_BSIZE          0x80  /* call stack block size */

/* hash node data structure for region info */

typedef struct RFG_RegionInfoHN_struct
{
  RFG_RegionInfo info;
  struct RFG_RegionInfoHN_struct* next;

} RFG_RegionInfoHN;

/* hash node data structure for call-path info */

typedef struct RFG_CallPathInfoHN_struct
{
  RFG_CallPathInfo info;
  struct RFG_CallPathInfoHN_struct* next;

} RFG_CallPathInfoHN;

/* data structure for call stack entry */

typedef struct RFG_RegionStackEntry_struct
{
  /* region info */
  RFG_RegionInfo* region_info;

  /* call-path info
     (only set if a filter rule is available for the current call path) */
  RFG_CallPathInfo* cpath_info;

  /* indicator flag whether region was approved when pushed to stack */
  uint8_t was_approved;

} RFG_RegionStackEntry;

/* data structure for call stack */

typedef struct RFG_RegionStack_struct
{
  /* call stack entries */
  RFG_RegionStackEntry* entries;

  /* hash values of current call path */
  uint32_t cpath_hashes[RFG_FILTER_MAX_CPATH_SIZE+1];

  /* region ids of current call path */
  uint32_t cpath_region_ids[RFG_FILTER_MAX_CPATH_SIZE+1];

  /* last stack position where the current call path is equal to
     the previous one */
  uint32_t cpath_last_equal_pos;

  /* current call stack position */
  uint32_t pos;

  /* allocated call stack size */
  uint32_t size;

} RFG_RegionStack;

/* main data structure for RFG Regions */

struct RFG_Regions_struct
{
  /* RFG filter object */
  RFG_Filter* filter;

  /* RFG groups object */
  RFG_Groups* groups;

  /* call stack */
  RFG_RegionStack* stack;

  /* number of region infos in hash table */
  uint32_t num_region_infos;

  /* hash table for region infos */
  RFG_RegionInfoHN* region_infos[REGION_INFO_HASH_MAX];

  /* number of call-path infos in hash table */
  uint32_t num_cpath_infos;

  /* hash table for call-path infos */
  RFG_CallPathInfoHN* cpath_infos[CPATH_INFO_HASH_MAX];

  /* recursive filter activation counter */
  /* unnecessary within VampirTrace; commented out to reduce overhead
  uint32_t recursive_filter_active_cnt;*/

};

/* puts a new region info to given hash table */
static RFG_RegionInfoHN* region_info_hash_put(
  RFG_RegionInfoHN** htab, uint32_t regionId, const char* regionName,
  const char* groupName, int32_t callLimit, uint32_t* stackBounds,
  uint8_t flags )
{
  uint32_t idx = regionId & ( REGION_INFO_HASH_MAX - 1 );

  RFG_RegionInfoHN* add =
    ( RFG_RegionInfoHN* )malloc( sizeof( RFG_RegionInfoHN ) );

  add->info.regionId       = regionId;
  add->info.groupName      = ( groupName != NULL ) ? strdup( groupName ) : NULL;
  add->info.regionName     = strdup( regionName );
  add->info.callLimit      = callLimit;
  add->info.callLimitCD    = callLimit;
  add->info.stackBounds[0] = stackBounds[0];
  add->info.stackBounds[1] = stackBounds[1];
  add->info.flags          = flags;
  add->next                = htab[idx];
  htab[idx]                = add;

  return add;
}

/* search region info in given hash table and return them */
static RFG_RegionInfoHN* region_info_hash_get(
  RFG_RegionInfoHN** htab, uint32_t regionId )
{
  uint32_t idx = regionId & ( REGION_INFO_HASH_MAX - 1 );

  RFG_RegionInfoHN* curr = htab[idx];
  while( curr )
  {
    if( curr->info.regionId == regionId )
      return curr;
    curr = curr->next;
  }

  return NULL;
}

/* frees region info hash table */
static void region_info_hash_free( RFG_RegionInfoHN** htab )
{
  uint32_t i;

  for( i = 0; i < REGION_INFO_HASH_MAX; i++ )
  {
    while( htab[i] )
    {
      RFG_RegionInfoHN* tmp = htab[i]->next;
      if( htab[i]->info.groupName )
        free( htab[i]->info.groupName );
      free( htab[i]->info.regionName );
      free( htab[i] );
      htab[i] = tmp;
    }
  }
}

/* puts a new call-path info to given hash table */
static RFG_CallPathInfoHN* cpath_info_hash_put(
  RFG_CallPathInfoHN** htab, uint32_t hash, uint32_t size,
  const uint32_t* regionIds, int32_t callLimit )
{
  uint32_t idx = hash & ( CPATH_INFO_HASH_MAX - 1 );

  RFG_CallPathInfoHN* add =
    ( RFG_CallPathInfoHN* )malloc( sizeof( RFG_CallPathInfoHN ) );

  add->info.hash        = hash;
  add->info.size        = size;
  memcpy( add->info.regionIds, regionIds, size * sizeof( uint32_t ) );
  add->info.callLimit   = callLimit;
  add->info.callLimitCD = callLimit;
  add->next             = htab[idx];
  htab[idx]             = add;

  return add;
}

/* search call-path info in given hash table and return them */
static RFG_CallPathInfoHN* cpath_info_hash_get(
  RFG_CallPathInfoHN** htab, uint32_t hash, uint32_t size,
  const uint32_t* regionIds )
{
  uint32_t idx = hash & ( CPATH_INFO_HASH_MAX - 1 );

  RFG_CallPathInfoHN* curr = htab[idx];
  while( curr )
  {
    if( curr->info.hash == hash && curr->info.size == size &&
        memcmp( curr->info.regionIds, regionIds,
          size * sizeof( uint32_t ) ) == 0 )
    {
      return curr;
    }
    curr = curr->next;
  }

  return NULL;
}

/* frees call-path info hash table */
static void cpath_info_hash_free( RFG_CallPathInfoHN** htab )
{
  uint32_t i;

  for( i = 0; i < CPATH_INFO_HASH_MAX; i++ )
  {
    while( htab[i] )
    {
      RFG_CallPathInfoHN* tmp = htab[i]->next;
      free( htab[i] );
      htab[i] = tmp;
    }
  }
}

/* initializes call stack */
static int stack_init( RFG_RegionStack** stack )
{
  /* allocate memory for call stack object */

  *stack = ( RFG_RegionStack* )calloc( 1, sizeof( RFG_RegionStack ) );
  if( !(*stack) )
    return 0;

  /* allocate memory for the call stack entries */

  (*stack)->entries =
    ( RFG_RegionStackEntry* )calloc( STACK_BSIZE, sizeof( RFG_RegionStack ) );
  if( !(*stack)->entries )
  {
    free( *stack );
    return 0;
  }
  (*stack)->size = STACK_BSIZE;

  return 1;
}

/* enlarges call stack */
static int stack_enlarge( RFG_RegionStack* stack )
{
  if( !stack )
    return 0;

  /* reallocate memory for call stack (size + STACK_BSIZE) */

  stack->entries =
    ( RFG_RegionStackEntry* )realloc( stack->entries,
      ( stack->size + STACK_BSIZE ) * sizeof( RFG_RegionStackEntry ) );
  if( !stack->entries )
    return 0;

  /* initialize new allocated memory */

  memset( stack->entries + stack->size, 0,
    STACK_BSIZE * sizeof( RFG_RegionStackEntry ) );

  /* update call stack size */
  stack->size += STACK_BSIZE;

  return 1;
}

RFG_Regions* RFG_Regions_init()
{
  RFG_Regions* ret;

  /* allocate memory for RFG regions object */

  if( !( ret = ( RFG_Regions* )calloc( 1, sizeof( RFG_Regions ) ) ) )
    return NULL;

  /* initialize call stack */

  if( !stack_init( &(ret->stack) ) )
  {
    free( ret );
    return NULL;
  }

  /* initialize RFG filter object */

  if( !( ret->filter = RFG_Filter_init() ) )
  {
    free( ret );
    return NULL;
  }

  /* initialize RFG groups object */

  if( !( ret->groups = RFG_Groups_init() ) )
  {
    free( ret );
    return NULL;
  }

  return ret;
}

/* duplicate RFG regions object */
RFG_Regions* RFG_Regions_dup( const RFG_Regions* oldRegions )
{
  RFG_Regions* ret = RFG_Regions_init();

  if( ret )
  {
    uint32_t i;

    /* copy region info hash table */

    if( oldRegions->num_region_infos > 0 )
    {
      for( i = 0; i < REGION_INFO_HASH_MAX; i++ )
      {
        RFG_RegionInfoHN* curr = oldRegions->region_infos[i];
        while( curr )
        {
          region_info_hash_put( ret->region_infos, curr->info.regionId,
            curr->info.regionName, curr->info.groupName, curr->info.callLimit,
            curr->info.stackBounds, curr->info.flags );
          ret->num_region_infos++;

          curr = curr->next;
        }
      }
    }

    /* copy call-path info hash table */

    if( oldRegions->num_cpath_infos > 0 )
    {
      for( i = 0; i < CPATH_INFO_HASH_MAX; i++ )
      {
        RFG_CallPathInfoHN* curr = oldRegions->cpath_infos[i];
        while( curr )
        {
          cpath_info_hash_put( ret->cpath_infos, curr->info.hash,
            curr->info.size, curr->info.regionIds, curr->info.callLimit );
          ret->num_cpath_infos++;

          curr = curr->next;
        }
      }
    }
  }

  return ret;
}

int RFG_Regions_free( RFG_Regions* regions )
{
  int ret = 1;
  
  if( !regions )
    return 0;

  /* free objects for region filter and grouping */
 
  if( !RFG_Filter_free( regions->filter ) ) ret = 0;
  if( !RFG_Groups_free( regions->groups ) ) ret = 0;

  /* free call stack */

  free( regions->stack->entries );
  free( regions->stack );

  /* free hash tables for regions and call-path infos */

  region_info_hash_free( regions->region_infos );
  regions->num_region_infos = 0;
  cpath_info_hash_free( regions->cpath_infos );
  regions->num_cpath_infos = 0;

  /* free self */

  free( regions );
  regions = NULL;

  return ret;
}

RFG_Filter* RFG_Regions_getFilter( RFG_Regions* regions )
{
  if( !regions || !regions->filter )
    return NULL;

  return regions->filter;
}

RFG_Groups* RFG_Regions_getGroups( RFG_Regions* regions )
{
  if( !regions || !regions->groups )
    return NULL;

  return regions->groups;
}

int RFG_Regions_setRegionIdGenFunc( RFG_Regions* regions,
                                    uint32_t (*func)(void) )
{
  if( !regions || !regions->filter )
    return 0;

  return RFG_Filter_setRegionIdGenFunc( regions->filter, func );
}

uint32_t RFG_Regions_getRegionId( RFG_Regions* regions, const char* regionName )
{
  if( !regions || !regions->filter )
    return 0;

  return RFG_Filter_getRegionId( regions->filter, regionName );
}

int RFG_Regions_setFilterDefFile( RFG_Regions* regions, const char* fileName )
{
  if( !regions || !regions->filter )
    return 0;

  return RFG_Filter_setDefFile( regions->filter, fileName );
}

int RFG_Regions_setGroupsDefFile( RFG_Regions* regions, const char* fileName )
{
  if( !regions || !regions->groups )
    return 0;

  return RFG_Groups_setDefFile( regions->groups, fileName );
}

int RFG_Regions_readFilterDefFile( RFG_Regions* regions, int rank,
                                   uint8_t* r_isRankOff )
{
  int ret;

  if( !regions || !regions->filter )
    return 0;

  /* discard previous read call-path filter rules */

  if( regions->num_cpath_infos > 0 )
  {
    cpath_info_hash_free( regions->cpath_infos );
    regions->num_cpath_infos = 0;
  }

  /* read region filter definition file */

  ret = RFG_Filter_readDefFile( regions->filter, rank, r_isRankOff );

  /* create call-path infos from read call-path filter rules */

  if( ret )
  {
    uint32_t ncpath_rules;
    RFG_FilterCallPathRules* cpath_rules;

    /* get all call-path filter rules */
    ret =
      RFG_Filter_getAllCallPathRules( regions->filter, &ncpath_rules,
        &cpath_rules );

    /* create call-path infos */

    if( ret )
    {
      uint32_t i;

      for( i = 0; i < ncpath_rules; i++ )
      {
        cpath_info_hash_put( regions->cpath_infos, cpath_rules[i].hash,
          cpath_rules[i].size, cpath_rules[i].regionIds,
          cpath_rules[i].callLimit );
        regions->num_cpath_infos++;
      }
      free( cpath_rules );
    }
  }

  return ret;
}

int RFG_Regions_readGroupsDefFile( RFG_Regions* regions )
{
  if( !regions || !regions->groups )
    return 0;

  return RFG_Groups_readDefFile( regions->groups );
}

int RFG_Regions_addGroupAssign( RFG_Regions* regions, const char* groupName,
                                int n, ... )
{
  va_list ap;
  int i;

  if( !regions || !regions->groups )
    return 0;

  va_start(ap, n);

  for( i = 0; i < n; i++ )
  {
    if( !RFG_Groups_addAssign( regions->groups, groupName, va_arg(ap, char*) ) )
    {
      va_end(ap);
      return 0;
    }
  }

  va_end(ap);

  return 1;
}

int RFG_Regions_stackPush( RFG_Regions* regions, uint32_t regionId,
                           RFG_RegionInfo** r_regionInfo,
                           RFG_CallPathInfo** r_cpathInfo,
                           uint8_t* r_wasApproved )
{
  RFG_RegionStackEntry* top;
  uint32_t* pos;

  if( !regions || !regions->stack )
    return 0;

  pos = &(regions->stack->pos);

  /* enlarge call stack, if necessary */

  if( (*pos)+1 == regions->stack->size )
  {
    if( !stack_enlarge( regions->stack ) )
    {
      fprintf( stderr,
        "RFG_Regions_stackPush(): Error: Could not enlarge stack size\n" );
      return 0;
    }
  }

  /* get pointer to the getting top of the call stack */
  top = &(regions->stack->entries[(*pos)+1]);

  /* update region info, if necessary */

  if( !top->region_info || top->region_info->regionId != regionId )
  {
    RFG_RegionInfoHN* region_info_hn =
      region_info_hash_get( regions->region_infos, regionId );
    if( !region_info_hn )
      return 0;

    top->region_info = &(region_info_hn->info);
  }

  /* increment call stack position */
  (*pos)++;

  /* update call-path info, if ... */

  if( /* ... we have any call-path filter rules, ... */
      regions->num_cpath_infos > 0 &&
      /* ... the new stack position does not exceed the max. number of regions
         in a call path, ... */
      *pos < RFG_FILTER_MAX_CPATH_SIZE+1 &&
      /* ... and the current call-path isn't equal to the previous one */
      ( regions->stack->cpath_region_ids[*pos] != regionId ||
        regions->stack->cpath_last_equal_pos < *pos ) )
  {
    RFG_CallPathInfoHN* cpath_info_hn;

    /* update last stack position where the current call path is equal to
       the previous one */
    regions->stack->cpath_last_equal_pos = *pos;

    /* generate hash value of current call path */
    regions->stack->cpath_hashes[*pos] =
      vt_hashtriple( regionId, 0, 0, regions->stack->cpath_hashes[(*pos)-1] );
    /* add region id to array */
    regions->stack->cpath_region_ids[*pos] = regionId;

    /* search matching call-path info in hash table */

    cpath_info_hn =
      cpath_info_hash_get( regions->cpath_infos,
        regions->stack->cpath_hashes[*pos], *pos,
        regions->stack->cpath_region_ids+1 );
    if( cpath_info_hn )
      top->cpath_info = &(cpath_info_hn->info);
    else
      top->cpath_info = NULL;
  }

  /* reject or approve the region enter event ... */

  /* ... either by the call-path filter rules (if available) */
  if( top->cpath_info )
  {
    if( /*regions->recursive_filter_active_cnt > 0 ||*/
        top->cpath_info->callLimitCD == 0 )
    {
      top->was_approved = 0;

      /* call-paths are always filtered recursively; increment recursive
         filter activation counter */
      /*regions->recursive_filter_active_cnt++;*/
    }
    else
    {
      top->was_approved = 1;

      /* decrement call-path's and region's call limit */

      if( top->cpath_info->callLimitCD > 0 )
        top->cpath_info->callLimitCD--;
      if( top->region_info->callLimitCD > 0 )
        top->region_info->callLimitCD--;
    }
  }
  /* ... or by the region filter rules */
  else
  {
    if( /*regions->recursive_filter_active_cnt > 0 ||*/
        top->region_info->callLimitCD == 0 ||
        *pos < top->region_info->stackBounds[0] ||
        *pos > top->region_info->stackBounds[1] )
    {
      top->was_approved = 0;

      /* increment recursive filter activation counter, if region shall be
         filtered recursively */
      /*if( (top->region_info->flags & RFG_FILTER_FLAG_RECURSIVE) != 0 )
        regions->recursive_filter_active_cnt++;*/
    }
    else
    {
      top->was_approved = 1;

      /* decrement region's call limit */
      if( top->region_info->callLimitCD > 0 )
        top->region_info->callLimitCD--;
    }
  }

  if( r_regionInfo )
    *r_regionInfo = top->region_info;
  if( r_cpathInfo )
    *r_cpathInfo = top->cpath_info;
  if( r_wasApproved )
    *r_wasApproved = top->was_approved;

  return 1;
}

int RFG_Regions_stackPop( RFG_Regions* regions, RFG_RegionInfo** r_regionInfo,
                          RFG_CallPathInfo** r_cpathInfo,
                          uint8_t* r_wasApproved )
{
  RFG_RegionStackEntry* top;

  if( !regions || !regions->stack )
    return 0;

  if( regions->stack->pos == 0 )
  {
    fprintf( stderr, "RFG_Regions_stackPop(): Error: Stack underflow\n" );
    return 0;
  }

  /* get pointer to the getting top of the call stack and decrement call stack
     position */
  top = &(regions->stack->entries[regions->stack->pos--]);

  /* decrement recursive filter activation counter, if region was rejected and
     filtered recursively */

  /*if( !top->was_approved &&
      ( top->cpath_info ||
        (top->region_info->flags & RFG_FILTER_FLAG_RECURSIVE) != 0 ) )
  {
    if( regions->recursive_filter_active_cnt == 0 )
    {
      fprintf( stderr,
        "RFG_Regions_stackPop(): Error: Underflow of recursive filter "
        "activation counter\n" );
      return 0;
    }

    regions->recursive_filter_active_cnt--;
  } */

  if( r_regionInfo )
    *r_regionInfo = top->region_info;
  if( r_cpathInfo )
    *r_cpathInfo = top->cpath_info;
  if( r_wasApproved )
    *r_wasApproved = top->was_approved;

  return 1;
}

RFG_RegionInfo* RFG_Regions_add( RFG_Regions* regions, uint32_t regionId,
                                 const char* regionName,
                                 const char* defaultGroupName )
{
  RFG_RegionInfoHN* region_info_hn;

  if( !regions )
    return NULL;

  if( !regionName || *regionName == '\0' )
  {
    fprintf( stderr, "RFG_Regions_add(): Error: Empty region name\n" );
    return NULL;
  }

  if( !defaultGroupName || *defaultGroupName == '\0' )
  {
    fprintf( stderr, "RFG_Regions_add(): Error: Empty default group name\n" );
    return NULL;
  }

  /* look for an already existing region info */
  region_info_hn = region_info_hash_get( regions->region_infos, regionId );

  /* create a new one, if not exist */

  if( !region_info_hn )
  {
    char*    group_name = NULL;
    int32_t  call_limit;
    uint32_t stack_bounds[2];
    uint8_t  flags;

    /* get group information of this region */

    if( !RFG_Groups_get( regions->groups, regionName, &group_name ) )
      return NULL;

    /* set group name to given default, if no group information present */

    if( !group_name )
      group_name = (char*)defaultGroupName;

    /* get filter information of this region */

    if( !RFG_Filter_getRegionRules( regions->filter, regionName, group_name,
          &call_limit, stack_bounds, &flags ) )
    {
      return NULL;
    }

    /* add region info to hash table */

    region_info_hn =
      region_info_hash_put( regions->region_infos, regionId, regionName,
        group_name, call_limit, stack_bounds, flags );
    regions->num_region_infos++;
  }

  return &(region_info_hn->info);
}

RFG_RegionInfo* RFG_Regions_get( RFG_Regions* regions, uint32_t regionId )
{
  RFG_RegionInfoHN* region_info_hn;

  if( !regions )
    return NULL;

  region_info_hn = region_info_hash_get( regions->region_infos, regionId );
  if( !region_info_hn )
    return NULL;
  else
    return &(region_info_hn->info);
}
