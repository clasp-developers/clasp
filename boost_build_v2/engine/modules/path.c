/* Copyright Vladimir Prus 2003.
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 */

#include "../constants.h"
#include "../frames.h"
#include "../lists.h"
#include "../native.h"
#include "../timestamp.h"


LIST * path_exists( FRAME * frame, int flags )
{
    timestamp time;
    timestamp_from_path( &time, list_front( lol_get( frame->args, 0 ) ) );
    return timestamp_empty( &time ) ? L0 : list_new( constant_true );
}


void init_path()
{
    char const * args[] = { "location", 0 };
    declare_native_rule( "path", "exists", args, path_exists, 1 );
}
