/* Copyright Vladimir Prus 2003. Distributed under the Boost */
/* Software License, Version 1.0. (See accompanying */
/* file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt) */

#include "../native.h"
#include "../timestamp.h"
#include "../newstr.h"
#include "../strings.h"
#include "../regexp.h"
#include "../compile.h"

/*
rule transform ( list * : pattern : indices * )
{
    indices ?= 1 ;
    local result ;
    for local e in $(list)
    {
        local m = [ MATCH $(pattern) : $(e) ] ;
        if $(m)
        {
            result += $(m[$(indices)]) ;
        }        
    }
    return $(result) ;
}
*/
LIST *regex_transform( PARSE *parse, FRAME *frame )
{
    LIST* l = lol_get( frame->args, 0 );    
    LIST* pattern = lol_get( frame->args, 1 );    
    LIST* indices_list = lol_get(frame->args, 2);
    int* indices = 0;
    int size;
    int* p;
    LIST* result = 0;

    string buf[1];
    string_new(buf);

    if (indices_list)
    {
        size = list_length(indices_list);
        indices = (int*)BJAM_MALLOC(size*sizeof(int));
        for(p = indices; indices_list; indices_list = indices_list->next)
        {
            *p++ = atoi(indices_list->string);
        }        
    }
    else 
    {
        size = 1;
        indices = (int*)BJAM_MALLOC(sizeof(int));
        *indices = 1;
    }

    {
        /* Result is cached and intentionally never freed */
        regexp *re = regex_compile( pattern->string );

        for(; l; l = l->next)
        {
            if( regexec( re, l->string ) )
            {
                int i = 0;
                for(; i < size; ++i)
                {
                    int index = indices[i];
                    /* Skip empty submatches. Not sure it's right in all cases,
                       but surely is right for the case for which this routine
                       is optimized -- header scanning.
                    */
                    if (re->startp[index] != re->endp[index])
                    {
                        string_append_range( buf, re->startp[index], re->endp[index] );
                        result = list_new( result, newstr( buf->value ) ); 
                        string_truncate( buf, 0 );
                    }
                }
            }
        }
        string_free( buf );
    }

    BJAM_FREE(indices);
    
    return result;
}

void init_regex()
{
    {
        char* args[] = { "list", "*", ":", "pattern", ":", "indices", "*", 0 };
        declare_native_rule("regex", "transform", args, regex_transform, 2);
    }
}
