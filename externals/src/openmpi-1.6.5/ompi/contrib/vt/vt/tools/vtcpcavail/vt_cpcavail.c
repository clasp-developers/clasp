/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "libcpc.h"

const char* ExeName = "vtcpcavail";

cpc_t* cpc = NULL;
char*  events[100];
int    nevents = 0;

void add_event( void *arg, const char* event );
void show_events(void);
void free_events(void);
int  sort_cmp( const void* a, const void* b );

int main(int argc, char** argv)
{
   cpc_t* cpc = NULL;

   if( ( cpc = cpc_open( CPC_VER_CURRENT ) ) == NULL )
   {
      fprintf( stderr, "%s: cpc_open: %s\n", ExeName,
	       strerror(errno) );
      exit(1);
   }

   printf( "CPU performance counter interface: %s\n", cpc_cciname(cpc) );
   printf( "Number of concurrently readable performance counters on the CPU: %u\n\n", cpc_npic(cpc) );
   printf( "Available events:\n" );
   
   cpc_walk_events_all( cpc, NULL, &add_event );
   
   show_events();
   free_events();

   printf( "\n%s\n", cpc_cpuref(cpc) );
   
   cpc_close( cpc );

   return 0;
}

void add_event( void *arg, const char* event )
{
   events[nevents++] = strdup( event );
}

void show_events(void)
{
   int i;
   
   if( nevents == 0 ) return;
   
   qsort( events, nevents, sizeof(char*), &sort_cmp );
   
   for( i = 0; i < nevents; i++ )
      printf( "%s\n", events[i] );
}

void free_events(void)
{
   int i;

   for( i = 0; i < nevents; i++ )
      free( events[i] );
}

int sort_cmp( const void* a, const void* b )
{
   char* aa = *(char**)a;
   char* bb = *(char**)b;

   return strcmp( aa, bb );
}
