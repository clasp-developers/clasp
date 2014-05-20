/* Copyright Vladimir Prus 2003. Distributed under the Boost */
/* Software License, Version 1.0. (See accompanying */
/* file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt) */

#include "../native.h"
#include "../timestamp.h"
#include "../newstr.h"

LIST *path_exists( PARSE *parse, FRAME *frame )
{
    LIST* l = lol_get( frame->args, 0 );    

    time_t time;
    timestamp(l->string, &time);
    if (time != 0)
    {
        return list_new(0, newstr("true"));
    }
    else
    {
        return L0;
    }
}

void init_path()
{
    {
        char* args[] = { "location", 0 };
        declare_native_rule("path", "exists", args, path_exists, 1);
    }

}
