#include "config.h"

#include "rfg_filter.h"

#include "vt_inttypes.h"

#include "util/hash.h"

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#define MAX_LINE_LEN           0x20000 /* max. file line length */

#define CPATH_RULES_HASH_MAX   0x400   /* size of hash table for call-path
                                          filter rules */
#define CPATH_REGIONS_HASH_MAX 0x400   /* size of hash table for call-path
                                          region names/ids */

/* data structure for region filter rules */

typedef struct RFG_FilterRegionRules_struct
{
  /* call limit */
  int32_t call_limit;

  /* stack level bounds */
  uint32_t stack_bounds[2];

  /* flags bitmask (group, recursiveness) */
  uint8_t flags;

  /* region pattern */
  char* pattern;

} RFG_FilterRegionRules;

/* hash node data structure for call-path filter rules */

typedef struct RFG_FilterCallPathRulesHN_struct
{
  RFG_FilterCallPathRules rules;
  struct RFG_FilterCallPathRulesHN_struct* next;

} RFG_FilterCallPathRulesHN;

/* hash node data structure for mapping call-path region names to ids */

typedef struct RFG_FilterCallPathRegionHN_struct
{
  char*    name; /* region name */
  uint32_t id;   /* assigned id */
  struct RFG_FilterCallPathRegionHN_struct* next;

} RFG_FilterCallPathRegionHN;

/* main data structure for RFG Filter */

struct RFG_Filter_struct
{
  /* name of filter definition file */
  char* file_name;

  /* content of filter file */
  char* file_content;

  /* buffer size of filter file content */
  size_t file_content_size;

  /* number of region filter rules */
  uint32_t num_region_rules;

  /* array of region filter rules */
  RFG_FilterRegionRules* region_rules;

  /* pointer to external function for generating region ids */
  uint32_t (*cpath_get_region_id)(void);

  /* region id counter
     (only used if we have no external function for generating) */
  uint32_t cpath_region_id_cnt;

  /* number of call-path filter rules */
  uint32_t num_cpath_rules;

  /* hash table for call-path filter rules */
  RFG_FilterCallPathRulesHN* cpath_rules[CPATH_RULES_HASH_MAX];

  /* hash table for mapping call-path region names to ids */
  RFG_FilterCallPathRegionHN* cpath_regions[CPATH_REGIONS_HASH_MAX];

};

static int get_file_content( RFG_Filter* filter )
{
  int ret = 1;

  int fd;

  if( !filter || !filter->file_name || *(filter->file_name) == '\0' )
    return 0;

  /* open the filter file */
  if( ( fd = open( filter->file_name, O_RDONLY ) ) == -1 )
    return 0;

  do
  {
    struct stat file_stat;

    /* get the file size */
    if( fstat(fd, &file_stat) == -1 || file_stat.st_size == 0 )
    {
      ret = 0;
      break;
    }

    filter->file_content_size = file_stat.st_size;

    /* allocate the buffer for storing the filter file content */
    filter->file_content = (char*)malloc( (filter->file_content_size + 1) * sizeof( char ) );
    if( !filter->file_content )
    {
      ret = 0;
      break;
    }

    /* read the filter file */
    if( read(fd, filter->file_content, filter->file_content_size ) == -1 )
    {
      ret = 0;
      break;
    }

    filter->file_content[ filter->file_content_size ] = '\0';
    
  } while( 0 );

  /* close the filter file */
  close( fd );

  return ret;
}

static int get_file_content_line( RFG_Filter* filter, char* buf,
                                  size_t bufsize, size_t* pos )
{
  size_t i;

  if( !filter || !filter->file_content )
    return 0;


  if( *pos >= filter->file_content_size )
    return 0;

  for( i = 0; i < bufsize && *pos < filter->file_content_size; i++ )
  {
    buf[i] = filter->file_content[(*pos)++];
    if( buf[i] == '\n' )
    {
      buf[i+1] = '\0';
      break;
    }
  }

  return 1;
}

static void cpath_rules_hash_put(
  RFG_FilterCallPathRulesHN** htab, uint32_t hash, uint32_t size,
  const uint32_t* regionIds, int32_t callLimit )
{
  uint32_t idx = hash & ( CPATH_RULES_HASH_MAX - 1 );

  RFG_FilterCallPathRulesHN* add =
    ( RFG_FilterCallPathRulesHN* )malloc( sizeof( RFG_FilterCallPathRulesHN ) );

  add->rules.hash      = hash;
  add->rules.size      = size;
  memcpy( add->rules.regionIds, regionIds, size * sizeof( uint32_t ) );
  add->rules.callLimit = callLimit;
  add->next            = htab[idx];
  htab[idx]            = add;
}

static RFG_FilterCallPathRulesHN* cpath_rules_hash_get(
  RFG_FilterCallPathRulesHN** htab, uint32_t hash, uint32_t size,
  const uint32_t* rids )
{
  uint32_t id = hash & ( CPATH_RULES_HASH_MAX - 1 );

  RFG_FilterCallPathRulesHN* curr = htab[id];
  while( curr )
  {
    if( curr->rules.hash == hash && curr->rules.size == size &&
        memcmp( curr->rules.regionIds, rids, size * sizeof( uint32_t ) ) == 0 )
    {
      return curr;
    }
    curr = curr->next;
  }

  return NULL;
}

static void cpath_rules_hash_free( RFG_FilterCallPathRulesHN** htab )
{
  uint32_t i;

  for( i = 0; i < CPATH_RULES_HASH_MAX; i++ )
  {
    while( htab[i] )
    {
      RFG_FilterCallPathRulesHN* tmp = htab[i]->next;
      free( htab[i] );
      htab[i] = tmp;
    }
  }
}

static void cpath_regions_hash_put( RFG_FilterCallPathRegionHN** htab,
  const char* name, uint32_t id )
{
  uint32_t idx =
    vt_hash( name, strlen( name ), 0 ) & ( CPATH_REGIONS_HASH_MAX - 1 );

  RFG_FilterCallPathRegionHN* add =
    (RFG_FilterCallPathRegionHN*)malloc(
      sizeof( RFG_FilterCallPathRegionHN ) );

  add->name = strdup( name );
  add->id   = id;
  add->next = htab[idx];
  htab[idx] = add;
}

static RFG_FilterCallPathRegionHN* cpath_regions_hash_get(
  RFG_FilterCallPathRegionHN** htab, const char* name )
{
  uint32_t idx =
    vt_hash( name, strlen( name ), 0 ) & ( CPATH_REGIONS_HASH_MAX - 1 );

  RFG_FilterCallPathRegionHN* curr = htab[idx];
  while( curr )
  {
    if( strcmp( curr->name , name ) == 0 )
      return curr;
    curr = curr->next;
  }

  return NULL;
}

static void cpath_regions_hash_free( RFG_FilterCallPathRegionHN** htab )
{
  uint32_t i;

  for( i = 0; i < CPATH_REGIONS_HASH_MAX; i++ )
  {
    while( htab[i] )
    {
      RFG_FilterCallPathRegionHN* tmp = htab[i]->next;
      free( htab[i]->name );
      free( htab[i] );
      htab[i] = tmp;
    }
  }
}

static uint32_t cpath_get_region_id( RFG_Filter* filter, const char* name )
{
  uint32_t id;

  RFG_FilterCallPathRegionHN* cpath_region;

  if( !filter || !name || *name == '\0' )
    return 0;

  /* look for already generated region id */
  cpath_region = cpath_regions_hash_get( filter->cpath_regions, name );

  /* return them, if found */
  if( cpath_region )
  {
    id = cpath_region->id;
  }
  /* otherwise, generate a new one */
  else
  {
    if( filter->cpath_get_region_id )
      id = filter->cpath_get_region_id();
    else
      id = filter->cpath_region_id_cnt++;

    cpath_regions_hash_put( filter->cpath_regions, name, id );
  }

  return id;
}

RFG_Filter* RFG_Filter_init()
{
  RFG_Filter* ret;

  /* allocate memory for RFG filter object */

  if( !( ret = ( RFG_Filter* )calloc( 1, sizeof( RFG_Filter ) ) ) )
    return NULL;

  /* initialize region id counter */
  ret->cpath_region_id_cnt = 1;

  return ret;
}

int RFG_Filter_free( RFG_Filter* filter )
{
  if( !filter )
    return 0;

  /* reset filter rules */
  if( !RFG_Filter_reset( filter ) )
    return 0;

  /* free filter definition file name */

  if( filter->file_name )
    free( filter->file_name );

  /* free filter file content buffer */

  if( filter->file_content )
    free( filter->file_content );

  /* free hash table for call-path regions/ids */
  cpath_regions_hash_free( filter->cpath_regions );

  /* free self */

  free( filter );
  filter = NULL;

  return 1;
}

int RFG_Filter_reset( RFG_Filter* filter )
{
  uint32_t i;

  if( !filter )
    return 0;

  if( filter->num_region_rules > 0 )
  {
    /* free array of region filter rules */

    for( i = 0; i < filter->num_region_rules; i++ )
      free( filter->region_rules[i].pattern );
    free( filter->region_rules );

    filter->region_rules = NULL;
    filter->num_region_rules = 0;
  }

  /* free hash table for call-path filter rules */

  cpath_rules_hash_free( filter->cpath_rules );
  filter->num_cpath_rules = 0;

  return 1;
}

int RFG_Filter_setRegionIdGenFunc( RFG_Filter* filter, uint32_t (*func)(void) )
{
  if( !filter )
    return 0;

  if( !func )
  {
    fprintf( stderr,
      "RFG_Filter_setRegionIdGenFunc(): Error: Invalid function pointer\n" );
    return 0;
  }

  filter->cpath_get_region_id = func;

  return 1;
}

uint32_t RFG_Filter_getRegionId( RFG_Filter* filter, const char* regionName )
{
  RFG_FilterCallPathRegionHN* cpath_region;

  if( !filter )
    return 0;

  if( !regionName || *regionName == '\0' )
  {
    fprintf( stderr, "RFG_Filter_getRegionId(): Error: Empty region name\n" );
    return 0;
  }

  cpath_region = cpath_regions_hash_get( filter->cpath_regions, regionName );
  if( cpath_region )
    return cpath_region->id;

  return 0;
}

int RFG_Filter_setDefFile( RFG_Filter* filter, const char* fileName )
{
  if( !filter )
    return 0;

  if( !fileName || *fileName == '\0' )
  {
    fprintf( stderr, "RFG_Filter_setDefFile(): Error: Empty file name\n" );
    return 0;
  }

  /* if a filter definition file already set, then free this */

  if( filter->file_name )
    free( filter->file_name );

  /* set new filter definition file */
  filter->file_name = strdup( fileName );

  return 1;
}

int RFG_Filter_readDefFile( RFG_Filter* filter, int rank, uint8_t* r_isRankOff )
{
  char*    line;
  uint32_t lineno = 0;
  size_t   pos = 0;
  uint8_t  parse_err = 0;
  uint8_t  l_is_rank_off = 0;
  uint8_t  includes_current_rank = 1;

  if( !filter || !filter->file_name )
    return 0;

  /* reset filter rules */
  if( !RFG_Filter_reset( filter ) )
    return 0;

  /* get filter file content, if necessary */

  if( !filter->file_content )
  {
    if( !get_file_content( filter ) )
    {
      fprintf( stderr,
               "RFG_Filter_readDefFile(): Error: Could not read file '%s'\n",
               filter->file_name );
      return 0;
    }
  }

  /* allocate memory for line */

  line = ( char* )malloc( MAX_LINE_LEN * sizeof( char ) );
  if( !line ) return 0;

  /* read lines */
  while( !l_is_rank_off && !parse_err &&
         get_file_content_line( filter, line, MAX_LINE_LEN, &pos ) )
  {
    char* p;
    char* q;

    /* increment line number */
    lineno++;

    /* remove newline */
    if( strlen(line) > 0 && line[strlen(line)-1] == '\n' )
      line[strlen(line)-1] = '\0';

    /* remove leading and trailing spaces from line */
    vt_strtrim( line );

    /* cut possible comment from line */

    p = strchr( line, '#' );
    if( p ) *p = '\0';

    /* continue if line is empty */
    if( strlen( line ) == 0 )
      continue;

    if( line[0] == '@' )
    {
      int a = -1;
      int b = -1;
      uint8_t is_rank_off_rule = 0;

      /* check whether selected ranks shall be disabled */
      if( ( p = strstr( line, "--" ) ) )
      {
        /* cut "-- OFF" from line */
        *p = '\0';

        p += 2;
        while( *p == ' ' || *p == '\t' ) p++;
        q = p;
        while( *q != '\0' ) { *q = tolower( *q ); q++; }

        if( strcmp( p, "off" ) == 0 )
        {
          is_rank_off_rule = 1;
        }
        else
        {
          parse_err = 1;
          break;
        }
      }

      /* no specific rank given? */
      if( rank == -1 )
      {
        /* continue reading, if selected ranks shall be disabled
           (non-rank-specific filter rules can follow) */
        if( is_rank_off_rule )
          continue;
        /* otherwise, stop reading
           (all the following filter rules are rank-specific which will be
            read later) */
        else
          break;
      }

      /* parse rank selection
         If current rank is included, then read the following filter rules.
         Otherwise, jump to next @ clause. */

      p = line + 1;

      includes_current_rank = 0;

      while( 1 )
      {
        while( *p == ' ' || *p == '\t' ) p++;

        if( *p >= '0' && *p <= '9' )
        {
          errno = 0;
          a = strtol( p, &q, 10 );
          p = q;
          if( errno != 0 )
          {
            parse_err = 1;
            break;
          }
        }
        else if( *p == '-' && *(p+1) != '\0' && a != -1 )
        {
          p++;

          errno = 0;
          b = strtol( p, &q, 10 );
          p = q;
          if( errno != 0 )
          {
            parse_err = 1;
            break;
          }
        }
        else if( (*p == ';' || *p == ',') && a != -1 )
        {
          p++;

          if( a == rank || (a < rank && rank <= b ) )
          {
            includes_current_rank = 1;
            if( is_rank_off_rule )
            {
              l_is_rank_off = 1; /* deactivate this rank completely */
              break;
            }
          }
          else if( is_rank_off_rule )
          {
            includes_current_rank = 1;
          }

          a = b = -1;
        }
        else if( *p == '\0' && a != -1 )
        {
          if( a == rank || (a < rank && rank <= b ) )
          {
            includes_current_rank = 1;
            if( is_rank_off_rule )
              l_is_rank_off = 1; /* deactivate this rank completely */
          }
          else if( is_rank_off_rule )
          {
            includes_current_rank = 1;
          }

          break;
        }
        else
        {
          parse_err = 1;
          break;
        }
      }
    }
    else
    {
      int32_t  climit;
      uint32_t sbounds[2] = { 1, (uint32_t)-1 };
      uint8_t  flags = 0;
      uint8_t  is_cpath_rule = 0;
      uint32_t num_cpath_regions = 0;
      char*    cpath_regions[RFG_FILTER_MAX_CPATH_SIZE];

      /* search for '--'
         e.g. "func1;func2;func3 -- 1000 S:5-10 R"
                                 p
      */

      if( !( p = strstr( line, "--" ) ) )
      {
        parse_err = 1;
        break;
      }

      /* cut call limit, stack level bounds, and flags from line
         e.g.   "func1;func2;func3 -- 1000 S:5-10 R"
             => "func1;func2;func3"
      */
      *p = '\0';

      /* split remaining line at ' ' to get call limit, stack level bounds,
         and flags */

      if( !( p = strtok( p+2, " " ) ) )
      {
        parse_err = 1;
        break;
      }

      /* parse call limit */
      {
        long l_climit;

        l_climit = strtol( p, &q, 10 );
        if( p == q || l_climit == LONG_MIN || l_climit == LONG_MAX ||
            l_climit < -1 )
        {
          parse_err = 1;
          break;
        }

        climit = (int32_t)l_climit;
      }

      /* parse stack level bounds and flags */
      while( !parse_err && ( p = strtok( NULL, " " ) ) )
      {
        /* stack level bounds */
        if( strlen( p ) > 2 && tolower( *p ) == 's' && *(p+1) == ':' )
        {
          long l_bounds[2] = { 1 };

          p = p+2;

          /* parse maximum (or minimum) stack level bound */

          l_bounds[1] = strtol( p, &q, 10 );
          if( p == q || l_bounds[1] == LONG_MIN || l_bounds[1] == LONG_MAX ||
              l_bounds[1] < 1 )
          {
            parse_err = 1;
            break;
          }

          p = q;

          /* minimum stack level bound specified? */
          if( *p == '-' )
          {
            p++;
            l_bounds[0] = l_bounds[1];

            /* parse maximum stack level bound */

            l_bounds[1] = strtol( p, &q, 10 );
            if( p == q || l_bounds[1] == LONG_MIN || l_bounds[1] == LONG_MAX ||
                l_bounds[1] < l_bounds[0] )
            {
              parse_err = 1;
              break;
            }

            p = q;
          }

          if( strlen( p ) > 1 || ( *p != '\0' && *p != '\t' ) )
          {
            parse_err = 1;
            break;
          }

          sbounds[0] = (uint32_t)l_bounds[0];
          sbounds[1] = (uint32_t)l_bounds[1];
        }
        /* group flag */
        else if( strlen( p ) == 1 && tolower( *p ) == 'g' )
        {
          flags |= RFG_FILTER_FLAG_GROUP;
        }
        /* recursiveness flag */
        else if( strlen( p ) == 1 && tolower( *p ) == 'r' )
        {
          flags |= RFG_FILTER_FLAG_RECURSIVE;
        }
        /* call-path flag */
        else if( strlen( p ) == 1 && tolower( *p ) == 'c' )
        {
          is_cpath_rule = 1;
        }
        else
        {
          parse_err = 1;
          break;
        }
      }

      /* call-path flag must not be mixed with other flags
         (except recursiveness) or stack level bounds */
      parse_err = ( parse_err || ( is_cpath_rule &&
        ( ( flags != 0 && flags != RFG_FILTER_FLAG_RECURSIVE ) ||
          sbounds[0] != 1 || sbounds[1] != (uint32_t)-1 ) ) );

      if( parse_err )
        break;

      /* split line at ';' to get pattern/region names */

      p = strtok( line, ";" );
      do
      {
        char* pattern_or_rname;

        if( !p )
        {
          parse_err = 1;
          break;
        }

        pattern_or_rname = strdup( p );
        vt_strtrim( pattern_or_rname );

        if( *pattern_or_rname != '\0' && includes_current_rank )
        {
          if( is_cpath_rule )
          {
             /* max. number of regions in a call path exceeded? */
             if( num_cpath_regions + 1 > RFG_FILTER_MAX_CPATH_SIZE )
             {
                free( pattern_or_rname );
                parse_err = 1;
                break;
             }
             /* add region to array of call-path region names */
             cpath_regions[num_cpath_regions++] = pattern_or_rname;
          }
          else
          {
            /* add region filter rules */
            RFG_Filter_addRegionRules( filter, pattern_or_rname, climit,
                                       sbounds, flags );

            free( pattern_or_rname );
          }
        }
        else
        {
          free( pattern_or_rname );
        }

      } while( ( p = strtok( 0, ";" ) ) );

      if( is_cpath_rule )
      {
        if( !parse_err )
        {
          /* add call-path filter rules */
          RFG_Filter_addCallPathRules( filter, num_cpath_regions,
                                       (const char**)cpath_regions, climit,
                                       NULL, NULL );
        }

        /* free array of call-path region names */

        do free( cpath_regions[--num_cpath_regions] );
        while( num_cpath_regions > 0 );
      }
    }
  }

  free( line );

  if( parse_err )
  {
    fprintf( stderr, "%s:%u: Could not be parsed\n",
      filter->file_name, lineno );
    return 0;
  }

  if( r_isRankOff )
    *r_isRankOff = l_is_rank_off;

  return 1;
}

int RFG_Filter_addRegionRules( RFG_Filter* filter, const char* pattern,
                               int32_t callLimit, uint32_t* stackBounds,
                               uint8_t flags )
{
  if( !filter )
    return 0;

  if( !pattern || *pattern == '\0' )
  {
    fprintf( stderr,
      "RFG_Filter_addRegionRules(): Error: Empty region/group pattern\n" );
    return 0;
  }

  /* enlarge array of filter region filter rules */

  filter->region_rules =
    (RFG_FilterRegionRules*)realloc( filter->region_rules,
      ( filter->num_region_rules + 1 ) * sizeof( RFG_FilterRegionRules ) );
  if( !filter->region_rules )
    return 0;

  /* add new region filter rules */

  filter->region_rules[filter->num_region_rules].call_limit = callLimit;
  filter->region_rules[filter->num_region_rules].stack_bounds[0] = 1;
  filter->region_rules[filter->num_region_rules].stack_bounds[1] = (uint32_t)-1;
  if( stackBounds )
  {
     filter->region_rules[filter->num_region_rules].stack_bounds[0] =
       stackBounds[0];
     filter->region_rules[filter->num_region_rules].stack_bounds[1] =
       stackBounds[1];
  }
  filter->region_rules[filter->num_region_rules].flags = flags;
  filter->region_rules[filter->num_region_rules].pattern = strdup( pattern );
  filter->num_region_rules++;

  return 1;
}

int RFG_Filter_getRegionRules( RFG_Filter* filter, const char* regionName,
                               const char* groupName, int32_t* r_callLimit,
                               uint32_t* r_stackBounds, uint8_t* r_flags )
{
  uint32_t i;

  if( !filter )
    return 0;

  if( !regionName && !groupName )
  {
    fprintf( stderr,
      "RFG_Filter_getRegionRules(): Error: Empty region and group name\n" );
    return 0;
  }

  /* initialize return parameters by defaults */

  if( r_callLimit )
  {
    *r_callLimit = -1;
  }
  if( r_stackBounds )
  {
    r_stackBounds[0] = 1;
    r_stackBounds[1] = (uint32_t)-1;
  }
  if( r_flags )
  {
    *r_flags = 0;
  }

  /* search for matching filter rule either by ... */
  for( i = 0; i < filter->num_region_rules; i++ )
  {
    const uint8_t is_group_rule =
      (filter->region_rules[i].flags & RFG_FILTER_FLAG_GROUP) != 0;

    if( /* ... region group name */
        ( is_group_rule && groupName &&
          fnmatch( filter->region_rules[i].pattern, groupName, 0 ) == 0 ) ||
        /* ... or by region name */
        ( !is_group_rule && regionName &&
          fnmatch( filter->region_rules[i].pattern, regionName, 0 ) == 0 ) )
    {
      /* set return parameters regarding to found filter rules */
      if( r_callLimit )
      {
        *r_callLimit = filter->region_rules[i].call_limit;
      }
      if( r_stackBounds )
      {
        r_stackBounds[0] = filter->region_rules[i].stack_bounds[0];
        r_stackBounds[1] = filter->region_rules[i].stack_bounds[1];
      }
      if( r_flags )
      {
        *r_flags = filter->region_rules[i].flags;
      }

      /* abort searching on first matching filter rule */
      break;
    }
  }

  return 1;
}

int RFG_Filter_addCallPathRules( RFG_Filter* filter, uint32_t size,
                                 const char** regionNames, int32_t callLimit,
                                 uint32_t* r_hash, uint32_t** r_regionIds )
{
  uint32_t l_hash;
  uint32_t l_region_ids[RFG_FILTER_MAX_CPATH_SIZE];
  uint32_t i;

  if( !filter )
    return 0;

  if( size == 0 || size > RFG_FILTER_MAX_CPATH_SIZE )
  {
    fprintf( stderr,
      "RFG_Filter_addCallPathRules(): Error: Invalid call path size\n" );
    return 0;
  }

  if( !regionNames )
  {
    fprintf( stderr,
      "RFG_Filter_addCallPathRules(): Error: Empty region name array\n" );
    return 0;
  }

  /* translate given region names to ids and generate a hash value for them */

  for( i = 0, l_hash = 0; i < size; i++ )
  {
    if( !regionNames[i] || *(regionNames[i]) == '\0' )
    {
      fprintf( stderr,
        "RFG_Filter_addCallPathRules(): Error: Empty region name\n" );
      return 0;
    }
    l_region_ids[i] = cpath_get_region_id( filter, regionNames[i] );
    l_hash = vt_hashtriple( l_region_ids[i], 0, 0, l_hash );
  }

  /* add call-path filter rules (only once) */

  if( !cpath_rules_hash_get( filter->cpath_rules, l_hash, size, l_region_ids ) )
  {
    cpath_rules_hash_put( filter->cpath_rules, l_hash, size, l_region_ids,
      callLimit );
    filter->num_cpath_rules++;
  }

  if( r_hash )
    *r_hash = l_hash;
  if( r_regionIds )
    memcpy( *r_regionIds, l_region_ids, size * sizeof( uint32_t ) );

  return 1;
}

int RFG_Filter_getCallPathRules( RFG_Filter* filter, uint32_t hash,
                                 uint32_t size, const uint32_t* regionIds,
                                 int32_t* r_callLimit )
{
  RFG_FilterCallPathRulesHN* cpath_rules_hn;

  if( !filter || !r_callLimit )
     return 0;

  if( size == 0 || size > RFG_FILTER_MAX_CPATH_SIZE )
  {
    fprintf( stderr,
      "RFG_Filter_getCallPathRules(): Error: Invalid call path size\n" );
    return 0;
  }

  if( !regionIds )
  {
    fprintf( stderr,
      "RFG_Filter_getCallPathRules(): Error: Empty region id array\n" );
    return 0;
  }

  cpath_rules_hn =
    cpath_rules_hash_get( filter->cpath_rules, hash, size, regionIds );
  if( !cpath_rules_hn )
    return 0;

  *r_callLimit = cpath_rules_hn->rules.callLimit;

  return 1;
}

int RFG_Filter_getAllCallPathRules( RFG_Filter* filter,
                                    uint32_t* r_numRules,
                                    RFG_FilterCallPathRules** r_rules )
{
  uint32_t i;

  if( !filter || !r_numRules || !r_rules )
    return 0;

  *r_numRules = filter->num_cpath_rules;
  *r_rules = NULL;

  if( *r_numRules > 0 )
  {
    uint32_t j;

    /* allocate memory for the array of call-path filter rules to be returned */

    *r_rules =
      (RFG_FilterCallPathRules*)malloc(
        *r_numRules * sizeof( RFG_FilterCallPathRules ) );
    if( !*r_rules )
      return 0;

    /* get call-path filter rules from hash table and store them in the array */

    for( i = 0, j = 0; i < CPATH_RULES_HASH_MAX; i++ )
    {
      RFG_FilterCallPathRulesHN* curr = filter->cpath_rules[i];
      while( curr )
      {
        (*r_rules)[j].hash      = curr->rules.hash;
        (*r_rules)[j].size      = curr->rules.size;
        memcpy( (*r_rules)[j].regionIds, curr->rules.regionIds,
          curr->rules.size * sizeof( uint32_t ) );
        (*r_rules)[j].callLimit = curr->rules.callLimit;

        j++;

        curr = curr->next;
      }
    }
  }

  return 1;
}
