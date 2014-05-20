# include "jam.h"
# include "pathsys.h"
# include "strings.h"
# include "newstr.h"
# include "filesys.h"
# include "lists.h"

void file_build1( PATHNAME * f, string * file )
{
    if ( DEBUG_SEARCH )
    {
        printf("build file: ");
        if ( f->f_root.len )
            printf( "root = '%.*s' ", f->f_root.len, f->f_root.ptr );
        if ( f->f_dir.len )
            printf( "dir = '%.*s' ", f->f_dir.len, f->f_dir.ptr );
        if ( f->f_base.len )
            printf( "base = '%.*s' ", f->f_base.len, f->f_base.ptr );
        printf( "\n" );
    }

    /* Start with the grist.  If the current grist isn't */
    /* surrounded by <>'s, add them. */

    if ( f->f_grist.len )
    {
        if ( f->f_grist.ptr[0] != '<' )
            string_push_back( file, '<' );
        string_append_range(
            file, f->f_grist.ptr, f->f_grist.ptr + f->f_grist.len );
        if ( file->value[file->size - 1] != '>' )
            string_push_back( file, '>' );
    }
}

static struct hash * filecache_hash = 0;
static file_info_t filecache_finfo;

file_info_t * file_info(char * filename)
{
    file_info_t *finfo = &filecache_finfo;

    if ( !filecache_hash )
        filecache_hash = hashinit( sizeof( file_info_t ), "file_info" );

    finfo->name = filename;
    finfo->is_file = 0;
    finfo->is_dir = 0;
    finfo->size = 0;
    finfo->time = 0;
    finfo->files = 0;
    if ( hashenter( filecache_hash, (HASHDATA**)&finfo ) )
    {
        /* printf( "file_info: %s\n", filename ); */
        finfo->name = newstr( finfo->name );
    }

    return finfo;
}

static LIST * files_to_remove = L0;

static void remove_files_atexit(void)
{
    /* we do pop front in case this exit function is called
       more than once */
    while ( files_to_remove )
    {
        remove( files_to_remove->string );
        files_to_remove = list_pop_front( files_to_remove );
    }
}

void file_done()
{
    remove_files_atexit();
    hashdone( filecache_hash );
}

void file_remove_atexit( const char * path )
{
    files_to_remove = list_new( files_to_remove, newstr((char*)path) );
}
