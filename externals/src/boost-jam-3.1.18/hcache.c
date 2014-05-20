/*
 * This file has been donated to Jam.
 */

# include "jam.h"
# include "lists.h"
# include "parse.h"
# include "rules.h"
# include "regexp.h"
# include "headers.h"
# include "newstr.h"
# include "hash.h"
# include "hcache.h"
# include "variable.h"
# include "search.h"

#ifdef OPT_HEADER_CACHE_EXT

/*
 * Craig W. McPheeters, Alias|Wavefront.
 *
 * hcache.c hcache.h - handle cacheing of #includes in source files.
 *
 * Create a cache of files scanned for headers. When starting jam, look for the
 * cache file and load it if present. When finished the binding phase, create a
 * new header cache. The cache contains files, their timestamps and the header
 * files found in their scan. During the binding phase of jam, look in the
 * header cache first for the headers contained in a file. If the cache is
 * present and valid, use its contents. This results in dramatic speedups with
 * large projects (eg. 3min -> 1min startup for one project.)
 *
 * External routines:
 *    hcache_init() - read and parse the local .jamdeps file.
 *    hcache_done() - write a new .jamdeps file.
 *    hcache()      - return list of headers on target. Use cache or do a scan.
 *
 * The dependency file format is an ASCII file with 1 line per target. Each line
 * has the following fields:
 * @boundname@ timestamp @file@ @file@ @file@ ... \n
 */

typedef struct hcachedata HCACHEDATA ;

struct hcachedata
{
    char       * boundname;
    time_t       time;
    LIST       * includes;
    LIST       * hdrscan;    /* the HDRSCAN value for this target */
    int          age;        /* if too old, we'll remove it from cache */
    HCACHEDATA * next;
};


static struct hash * hcachehash = 0;
static HCACHEDATA  * hcachelist = 0;

static int queries = 0;
static int hits = 0;

#define CACHE_FILE_VERSION "version 4"
#define CACHE_RECORD_HEADER "header"
#define CACHE_RECORD_END "end"


/*
 * Return the name of the header cache file. May return NULL.
 *
 * The user sets this by setting the HCACHEFILE variable in a Jamfile. We cache
 * the result so the user can not change the cache file during header scanning.
 */

static char * cache_name( void )
{
    static char * name = 0;
    if ( !name )
    {
        LIST * hcachevar = var_get( "HCACHEFILE" );

        if ( hcachevar )
        {
            TARGET * t = bindtarget( hcachevar->string );

            pushsettings( t->settings );
            /* Do not expect the cache file to be generated, so pass 0 as the
             * third argument to search. Expect the location to be specified via
             * LOCATE, so pass 0 as the fourth arugment.
             */
            t->boundname = search( t->name, &t->time, 0, 0 );
            popsettings( t->settings );

            if ( hcachevar )
                name = copystr( t->boundname );
        }
    }
    return name;
}


/*
 * Return the maximum age a cache entry can have before it is purged ftom the
 * cache.
 */

static int cache_maxage( void )
{
    int age = 100;
    LIST * var = var_get( "HCACHEMAXAGE" );
    if ( var )
    {
        age = atoi( var->string );
        if ( age < 0 )
            age = 0;
    }
    return age;
}


/*
 * Read a netstring. The caveat is that the string can not contain ASCII 0. The
 * returned value is as returned by newstr(), so it need not be freed.
 */

char * read_netstring( FILE * f )
{
    unsigned long len;
    static char * buf = NULL;
    static unsigned long buf_len = 0;

    if ( fscanf( f, " %9lu", &len ) != 1 )
        return NULL;
    if ( fgetc( f ) != (int)'\t' )
        return NULL;

    if ( len > 1024 * 64 )
        return NULL;  /* sanity check */

    if ( len > buf_len )
    {
        unsigned long new_len = buf_len * 2;
        if ( new_len < len )
            new_len = len;
        buf = (char *)BJAM_REALLOC( buf, new_len + 1 );
        if ( buf )
            buf_len = new_len;
    }

    if ( !buf )
        return NULL;

    if ( fread( buf, 1, len, f ) != len )
        return NULL;
    if ( fgetc( f ) != (int)'\n' )
        return NULL;

    buf[ len ] = 0;
    return newstr( buf );
}


/*
 * Write a netstring.
 */

void write_netstring( FILE * f, char const * s )
{
    if ( !s )
        s = "";
    fprintf( f, "%lu\t%s\n", (long unsigned)strlen( s ), s );
}


void hcache_init()
{
    HCACHEDATA   cachedata;
    HCACHEDATA * c;
    FILE       * f;
    char       * version;
    int          header_count = 0;
    char       * hcachename;

    hcachehash = hashinit( sizeof( HCACHEDATA ), "hcache" );

    if ( !( hcachename = cache_name() ) )
        return;

    if ( !( f = fopen( hcachename, "rb" ) ) )
        return;

    version = read_netstring( f );
    if ( !version || strcmp( version, CACHE_FILE_VERSION ) )
    {
        fclose( f );
        return;
    }

    while ( 1 )
    {
        char * record_type;
        char * time_str;
        char * age_str;
        char * includes_count_str;
        char * hdrscan_count_str;
        int    i;
        int    count;
        LIST * l;

        record_type = read_netstring( f );
        if ( !record_type )
        {
            fprintf( stderr, "invalid %s\n", hcachename );
            goto bail;
        }
        if ( !strcmp( record_type, CACHE_RECORD_END ) )
            break;
        if ( strcmp( record_type, CACHE_RECORD_HEADER ) )
        {
            fprintf( stderr, "invalid %s with record separator <%s>\n",
                hcachename, record_type ? record_type : "<null>" );
            goto bail;
        }

        c = &cachedata;

        c->boundname       = read_netstring( f );
        time_str           = read_netstring( f );
        age_str            = read_netstring( f );
        includes_count_str = read_netstring( f );

        if ( !c->boundname || !time_str || !age_str || !includes_count_str )
        {
            fprintf( stderr, "invalid %s\n", hcachename );
            goto bail;
        }

        c->time = atoi( time_str );
        c->age = atoi( age_str ) + 1;

        count = atoi( includes_count_str );
        for ( l = 0, i = 0; i < count; ++i )
        {
            char * s = read_netstring( f );
            if ( !s )
            {
                fprintf( stderr, "invalid %s\n", hcachename );
                goto bail;
            }
            l = list_new( l, s );
        }
        c->includes = l;

        hdrscan_count_str = read_netstring( f );
        if ( !includes_count_str )
        {
            list_free( c->includes );
            fprintf( stderr, "invalid %s\n", hcachename );
            goto bail;
        }

        count = atoi( hdrscan_count_str );
        for ( l = 0, i = 0; i < count; ++i )
        {
            char * s = read_netstring( f );
            if ( !s )
            {
                fprintf( stderr, "invalid %s\n", hcachename );
                goto bail;
            }
            l = list_new( l, s );
        }
        c->hdrscan = l;

        if ( !hashenter( hcachehash, (HASHDATA * *)&c ) )
        {
            fprintf( stderr, "can't insert header cache item, bailing on %s\n",
                hcachename );
            goto bail;
        }

        c->next = hcachelist;
        hcachelist = c;

        ++header_count;
    }

    if ( DEBUG_HEADER )
        printf( "hcache read from file %s\n", hcachename );

 bail:
    fclose( f );
}


void hcache_done()
{
    FILE       * f;
    HCACHEDATA * c;
    int          header_count = 0;
    char       * hcachename;
    int          maxage;

    if ( !hcachehash )
        return;

    if ( !( hcachename = cache_name() ) )
        return;

    if ( !( f = fopen( hcachename, "wb" ) ) )
        return;

    maxage = cache_maxage();

    /* Print out the version. */
    write_netstring( f, CACHE_FILE_VERSION );

    c = hcachelist;
    for ( c = hcachelist; c; c = c->next )
    {
        LIST * l;
        char   time_str[ 30 ];
        char   age_str[ 30 ];
        char   includes_count_str[ 30 ];
        char   hdrscan_count_str[ 30 ];

        if ( maxage == 0 )
            c->age = 0;
        else if ( c->age > maxage )
            continue;

        sprintf( includes_count_str, "%lu", (long unsigned) list_length( c->includes ) );
        sprintf( hdrscan_count_str, "%lu", (long unsigned) list_length( c->hdrscan ) );
        sprintf( time_str, "%lu", (long unsigned) c->time );
        sprintf( age_str, "%lu", (long unsigned) c->age );

        write_netstring( f, CACHE_RECORD_HEADER );
        write_netstring( f, c->boundname );
        write_netstring( f, time_str );
        write_netstring( f, age_str );
        write_netstring( f, includes_count_str );
        for ( l = c->includes; l; l = list_next( l ) )
            write_netstring( f, l->string );
        write_netstring( f, hdrscan_count_str );
        for ( l = c->hdrscan; l; l = list_next( l ) )
            write_netstring( f, l->string );
        fputs( "\n", f );
        ++header_count;
    }
    write_netstring( f, CACHE_RECORD_END );

    if ( DEBUG_HEADER )
        printf( "hcache written to %s.   %d dependencies, %.0f%% hit rate\n",
            hcachename, header_count, queries ? 100.0 * hits / queries : 0 );

    fclose ( f );
}


LIST * hcache( TARGET * t, int rec, regexp * re[], LIST * hdrscan )
{
    HCACHEDATA cachedata;
    HCACHEDATA * c = &cachedata;

    LIST * l = 0;

    ++queries;

    c->boundname = t->boundname;

    if (hashcheck (hcachehash, (HASHDATA **) &c))
    {
    if (c->time == t->time)
    {
        LIST *l1 = hdrscan, *l2 = c->hdrscan;
        while (l1 && l2) {
        if (l1->string != l2->string) {
            l1 = NULL;
        } else {
            l1 = list_next(l1);
            l2 = list_next(l2);
        }
        }
        if (l1 || l2) {
        if (DEBUG_HEADER)
            printf("HDRSCAN out of date in cache for %s\n",
               t->boundname);

        printf("HDRSCAN out of date for %s\n", t->boundname);
        printf(" real  : ");
        list_print(hdrscan);
        printf("\n cached: ");
        list_print(c->hdrscan);
        printf("\n");

        list_free(c->includes);
        list_free(c->hdrscan);
        c->includes = 0;
        c->hdrscan = 0;
        } else {
        if (DEBUG_HEADER)
            printf ("using header cache for %s\n", t->boundname);
        c->age = 0;
        ++hits;
        l = list_copy (0, c->includes);
        return l;
        }
    } else {
        if (DEBUG_HEADER)
            printf ("header cache out of date for %s\n", t->boundname);
        list_free (c->includes);
        list_free(c->hdrscan);
        c->includes = 0;
        c->hdrscan = 0;
    }
    } else {
    if (hashenter (hcachehash, (HASHDATA **)&c)) {
        c->boundname = newstr (c->boundname);
        c->next = hcachelist;
        hcachelist = c;
    }
    }

    /* 'c' points at the cache entry. Its out of date. */

    l = headers1 (0, t->boundname, rec, re);

    c->time = t->time;
    c->age = 0;
    c->includes = list_copy (0, l);
    c->hdrscan = list_copy(0, hdrscan);

    return l;
}

#endif
