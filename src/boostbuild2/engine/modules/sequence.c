/*
 * Copyright Vladimir Prus 2003.
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 */

#include "../native.h"
#include "../object.h"

#include <stdlib.h>


#ifndef max
# define max(a,b) ((a)>(b)?(a):(b))
#endif


LIST * sequence_select_highest_ranked( FRAME * frame, int flags )
{
   /* Returns all of 'elements' for which corresponding element in parallel */
   /* list 'rank' is equal to the maximum value in 'rank'.                  */

    LIST * const elements = lol_get( frame->args, 0 );
    LIST * const rank = lol_get( frame->args, 1 );

    LIST * result = L0;
    int highest_rank = -1;

    {
        LISTITER iter = list_begin( rank );
        LISTITER const end = list_end( rank );
        for ( ; iter != end; iter = list_next( iter ) )
        {
            int const current = atoi( object_str( list_item( iter ) ) );
            highest_rank = max( highest_rank, current );
        }
    }

    {
        LISTITER iter = list_begin( rank );
        LISTITER const end = list_end( rank );
        LISTITER elements_iter = list_begin( elements );
        LISTITER const elements_end = list_end( elements );
        for ( ; iter != end; iter = list_next( iter ), elements_iter =
            list_next( elements_iter ) )
            if ( atoi( object_str( list_item( iter ) ) ) == highest_rank )
                result = list_push_back( result, object_copy( list_item(
                    elements_iter ) ) );
    }

    return result;
}

void init_sequence()
{
    char const * args[] = { "elements", "*", ":", "rank", "*", 0 };
    declare_native_rule( "sequence", "select-highest-ranked", args,
                        sequence_select_highest_ranked, 1 );
}
