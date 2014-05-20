#include "config.h"

#include "rfg_groups.h"

#include "vt_inttypes.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MAX_LINE_LEN 0x20000 /* max. file line length */

/* data structure for group assignments */

typedef struct RFG_GroupsAssign_struct
{
  /* group name */
  char* group_name;

  /* number of assigned pattern */
  uint32_t num_pattern;

  /* array of assigned pattern */
  char** pattern;

} RFG_GroupsAssign;

/* main data structure for RFG Groups */

struct RFG_Groups_struct
{
  /* name of group definition file */
  char* file_name;

  /* number of group assignments */
  uint32_t num_assigns;

  /* array of group assignments */
  RFG_GroupsAssign* assigns;

};

RFG_Groups* RFG_Groups_init()
{
  RFG_Groups* ret;

  /* allocate memory for RFG groups object */
  ret = ( RFG_Groups* )calloc( 1, sizeof( RFG_Groups ) );

  return ret;
}

int RFG_Groups_free( RFG_Groups* groups )
{
  uint32_t i;
  uint32_t j;

  if( !groups )
    return 0;

  /* free group definition file name */

  if( groups->file_name )
    free( groups->file_name );

  /* free array of group assignments */

  for( i = 0; i < groups->num_assigns; i++ )
  {
    for( j = 0; j < groups->assigns[i].num_pattern; j++ )
      free( groups->assigns[i].pattern[j] );

    free( groups->assigns[i].group_name );
    free( groups->assigns[i].pattern );
  }

  free( groups->assigns );

  /* free self */

  free( groups );
  groups = NULL;

  return 1;
}

int RFG_Groups_setDefFile( RFG_Groups* groups, const char* fileName )
{
  if( !groups )
    return 0;

  if( !fileName || *fileName == '\0' )
  {
    fprintf( stderr, "RFG_Groups_setDefFile(): Error: Empty file name\n" );
    return 0;
  }

  /* if a group definition file already set, then free this */

  if( groups->file_name )
    free( groups->file_name );

  /* set new group definition file */

  groups->file_name = strdup( fileName );

  return 1;
}

int RFG_Groups_readDefFile( RFG_Groups* groups )
{
  FILE*    f;
  char*    line;
  uint32_t lineno = 0;
  uint8_t  parse_err = 0;

  if( !groups )
    return 0;

  if( !groups->file_name )
    return 1;

  /* open group definition file */

  f = fopen( groups->file_name, "r" );
  if( !f )
  {
    fprintf( stderr,
      "RFG_Groups_readDefFile(): Error: Could not open file '%s'\n",
      groups->file_name );
    return 0;
  }

  line = ( char* )malloc( MAX_LINE_LEN * sizeof( char ) );
  if( !line )
  {
    fclose( f );
    return 0;
  }

  /* read lines */

  while( !parse_err && fgets( line, MAX_LINE_LEN, f ) )
  {
    char* group;
    char* p;

    /* increment line number */
    lineno++;

    /* remove newline */
    if( strlen( line ) > 0 && line[strlen(line)-1] == '\n' )
      line[strlen(line)-1] = '\0';

    /* remove leading and trailing spaces from line */
    vt_strtrim( line );

    /* cut possible comment from line */

    p = strchr( line, '#' );
    if( p )
      *p = '\0';

    /* continue if line is empty */
    if( strlen( line ) == 0 )
      continue;

    /* search for '='
       e.g. "GROUP=func1;func2;func3"
                  p
    */

    p = strchr( line, '=' );
    if( !p )
    {
      parse_err = 1;
      break;
    }

    /* cut group name from line 
       e.g.   "GROUP=func1;func2;func3"
           => "GROUP"
    */

    *p = '\0';

    group = strdup( line );
    vt_strtrim( group );

    /* split remaining line at ';' to get pattern */

    p = strtok( p+1, ";" );
    do
    {
      char* pattern;

      if( !p )
      {
	parse_err = 1;
	break;
      }

      pattern = strdup( p );
      vt_strtrim( pattern );

      /* add group assignment */

      if( strlen( pattern ) > 0 )
	RFG_Groups_addAssign( groups, group, pattern );

      free( pattern );

    } while( ( p = strtok( 0, ";" ) ) );

    free( group );
  }

  if( parse_err )
  {
    fprintf( stderr, "%s:%u: Could not be parsed\n",
      groups->file_name, lineno );
  }

  free( line );

  fclose( f );

  return parse_err ? 0 : 1;
}

int RFG_Groups_addAssign( RFG_Groups* groups, const char* groupName,
			  const char* pattern )
{
  uint32_t i;
  RFG_GroupsAssign* entry = NULL;

  if( !groups )
    return 0;

  if( !groupName || *groupName == '\0' )
  {
    fprintf( stderr, "RFG_Groups_addAssign(): Error: Empty group name\n" );
    return 0;
  }

  if( !pattern || *pattern == '\0' )
  {
    fprintf( stderr, "RFG_Groups_addAssign(): Error: Empty region pattern\n" );
    return 0;
  }

  /* search group assignment by group name */

  for( i = 0; i < groups->num_assigns; i++ )
  {
    if( strcmp( groups->assigns[i].group_name, groupName ) == 0 )
    {
      entry = &(groups->assigns[i]);
      break;
    }
  }

  /* if no entry found, then allocate new group assignment entry */

  if( !entry )
  {
    if( !groups->assigns )
    {
      groups->assigns =
	( RFG_GroupsAssign* )malloc( sizeof( RFG_GroupsAssign ) );
    }
    else
    {
      groups->assigns =
	(RFG_GroupsAssign* )realloc( groups->assigns,
	  ( groups->num_assigns + 1 ) * sizeof( RFG_GroupsAssign ) );
    }

    if( !groups->assigns )
      return 0;

    entry = &(groups->assigns[groups->num_assigns++]);
    entry->group_name = strdup( groupName );
    entry->num_pattern = 0;
    entry->pattern = NULL;
  }

  /* add pattern to group */

  if( !entry->pattern )
  {
    entry->pattern = ( char** )malloc( sizeof( char* ) );
  }
  else
  {
    entry->pattern = ( char** )realloc( entry->pattern,
      ( entry->num_pattern + 1 ) * sizeof( char* ) );
  }
  if( !entry->pattern )
    return 0;

  entry->pattern[entry->num_pattern++] = strdup( pattern );

  return 1;
}

int RFG_Groups_get( RFG_Groups* groups, const char* regionName,
		    char** r_groupName )
{
  uint32_t i;
  uint32_t j;

  if( !groups )
    return 0;

  if( !regionName || *regionName == '\0' )
  {
    fprintf( stderr, "RFG_Groups_get(): Error: Empty region name\n" );
    return 0;
  }

  /* search for matching pattern by region name */

  for( i = 0; i < groups->num_assigns; i++ )
  {
    for( j = 0; j < groups->assigns[i].num_pattern; j++ )
    {
      if( fnmatch( groups->assigns[i].pattern[j], regionName, 0 ) == 0 )
      {
	*r_groupName = groups->assigns[i].group_name;
	return 1;
      }
    }
  }

  *r_groupName = NULL;

  return 1;
}
