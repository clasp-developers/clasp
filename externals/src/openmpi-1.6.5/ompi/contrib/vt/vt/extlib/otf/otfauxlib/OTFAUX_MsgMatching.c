#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

#include <otf.h>

#include "otfaux.h"

OTFAUX_MsgMatching_Context*
OTFAUX_MsgMatching_create( void )
{
    return OTFAUX_State_create();
}

void
OTFAUX_MsgMatching_destroy( OTFAUX_MsgMatching_Context* mm_context )
{
    OTFAUX_State_destroy( mm_context );
}

void
OTFAUX_MsgMatching_enqueueRecv( OTFAUX_MsgMatching_Context* mm_context,
                                uint64_t sender,
                                uint64_t receiver,
                                uint32_t tag,
                                uint32_t comm,
                                uint64_t time,
                                uint32_t size,
                                uint32_t scl )
{
    OTFAUX_State_enqueueRecvMsg( mm_context,
                                 time,
                                 receiver,
                                 sender,
                                 comm,
                                 tag,
                                 size,
                                 scl );
}

int
OTFAUX_MsgMatching_matchSend( OTFAUX_MsgMatching_Context* mm_context,
                              uint64_t sender,
                              uint64_t receiver,
                              uint32_t tag,
                              uint32_t comm,
                              uint64_t* ptime,
                              uint32_t* psize,
                              uint32_t* pscl )
{
    /*
     * return 0 if no matching receive was found,
     * which is 2 from OTFAUX_State_processSendMsg
     */
    return 1 == OTFAUX_State_processSendMsg( mm_context,
                                             0,
                                             sender,
                                             receiver,
                                             comm,
                                             tag,
                                             0,
                                             0,
                                             ptime,
                                             psize,
                                             pscl,
                                             NULL );
}

/** release empty queues and recv in free list */
void
OTFAUX_MsgMatching_releaseMemory( OTFAUX_MsgMatching_Context* mm_context )
{
    ( void )mm_context;
    /* nothing to do */
}
