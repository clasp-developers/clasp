#ifndef OTFAUX_MSGMATCHING_H
#define OTFAUX_MSGMATCHING_H

#include <otf.h>

#include <OTFAUX_State.h>

/**
 *  @file otfauxlib/OTFAUX_MsgMatching.h
 *
 *  @brief Provides a module to match MPI P2P massages.
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/**
 * @defgroup msgmatch Module to match messages.
 *
 * @{
 */

/** Opaque type for using the matching module. */
typedef OTFAUX_State OTFAUX_MsgMatching_Context;

/** Create a context for matching messages. */
OTFAUX_MsgMatching_Context* OTFAUX_MsgMatching_create( void );

/** Destroy a context previously created with @a OTFAUX_MsgMatching_Create. */
void OTFAUX_MsgMatching_destroy( OTFAUX_MsgMatching_Context* mm_context );

/** Provide a recv event for matching. */
void OTFAUX_MsgMatching_enqueueRecv( OTFAUX_MsgMatching_Context* mm_context,
                                     uint64_t sender,
                                     uint64_t receiver,
                                     uint32_t tag,
                                     uint32_t comm,
                                     uint64_t time,
                                     uint32_t size,
                                     uint32_t scl );

/**
 * Try to match a send with the corresponding recv.
 *
 * @return 1 for success
 *         0 for no recv
 */
int OTFAUX_MsgMatching_matchSend( OTFAUX_MsgMatching_Context* mm_context,
                                  uint64_t sender,
                                  uint64_t receiver,
                                  uint32_t tag,
                                  uint32_t comm,
                                  uint64_t* ptime,
                                  uint32_t* psize,
                                  uint32_t* pscl );

/**
 * If you think the OTFAUX_MsgMatching_Context holds to much memory, you can
 * order him to release unused memory.
 *
 */
void OTFAUX_MsgMatching_releaseMemory( OTFAUX_MsgMatching_Context* mm_context );


/**
 * @}
 */


/**
 * @defgroup otfauxtud Special KeyValue names/types for messages matching used
 *                     by the TU Dresden
 *
 * @{
 */

#define OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_TIME_NAME "TUD::p2p-received-time"
#define OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_TIME_TYPE OTF_UINT64
#define OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SIZE_NAME "TUD::p2p-received-size"
#define OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SIZE_TYPE OTF_UINT32
#define OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SCL_NAME  "TUD::p2p-received-scl"
#define OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SCL_TYPE  OTF_UINT32

/**
 * @}
 */


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTFAUX_MSGMATCHING_H */
