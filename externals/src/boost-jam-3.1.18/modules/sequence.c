/* Copyright Vladimir Prus 2003. Distributed under the Boost */
/* Software License, Version 1.0. (See accompanying */
/* file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt) */

#include "../native.h"

# ifndef max
# define max( a,b ) ((a)>(b)?(a):(b))
# endif


LIST *sequence_select_highest_ranked( PARSE *parse, FRAME *frame )
{
   /* Returns all of 'elements' for which corresponding element in parallel */
   /* list 'rank' is equal to the maximum value in 'rank'.                  */

    LIST* elements = lol_get( frame->args, 0 );    
    LIST* rank = lol_get( frame->args, 1 );    
    
    LIST* result = 0;
    LIST* tmp;
    int highest_rank = -1;

    for (tmp = rank; tmp; tmp = tmp->next)
        highest_rank = max(highest_rank, atoi(tmp->string));

    for (; rank; rank = rank->next, elements = elements->next)
        if (atoi(rank->string) == highest_rank)
            result = list_new(result, elements->string);

    return result;
}

void init_sequence()
{
    {
        char* args[] = { "elements", "*", ":", "rank", "*", 0 };
        declare_native_rule("sequence", "select-highest-ranked", args, 
                            sequence_select_highest_ranked, 1);
    }

}
