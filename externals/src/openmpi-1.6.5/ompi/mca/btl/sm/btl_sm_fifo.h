#ifndef MCA_BTL_SM_FIFO_H
#define MCA_BTL_SM_FIFO_H

#include "btl_sm.h"
#include "btl_sm_endpoint.h"

static void
add_pending(struct mca_btl_base_endpoint_t *ep, void *data, bool resend)
{
    int rc;
    btl_sm_pending_send_item_t *si;
    opal_free_list_item_t *i;
    OPAL_FREE_LIST_GET(&mca_btl_sm_component.pending_send_fl, i, rc);

    /* don't handle error for now */
    assert(i != NULL && rc == OMPI_SUCCESS);

    si = (btl_sm_pending_send_item_t*)i;
    si->data = data;

    OPAL_THREAD_ADD32(&mca_btl_sm_component.num_pending_sends, +1);

    /* if data was on pending send list then prepend it to the list to
     * minimize reordering */
    OPAL_THREAD_LOCK(&ep->endpoint_lock);
    if (resend)
        opal_list_prepend(&ep->pending_sends, (opal_list_item_t*)si);
    else
        opal_list_append(&ep->pending_sends, (opal_list_item_t*)si);
    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
}

/*
 * FIFO_MAP(x) defines which FIFO on the receiver should be used
 * by sender rank x.  The map is some many-to-one hash.
 *
 * FIFO_MAP_NUM(n) defines how many FIFOs the receiver has for
 * n senders.
 *
 * That is,
 *
 *      for all    0 <= x < n:
 *
 *              0 <= FIFO_MAP(x) < FIFO_MAP_NUM(n)
 *
 * For example, using some power-of-two nfifos, we could have
 *
 *    FIFO_MAP(x)     = x & (nfifos-1)
 *    FIFO_MAP_NUM(n) = min(nfifos,n)
 *
 * Interesting limits include:
 *
 *    nfifos very large:  In this case, each sender has its
 *       own dedicated FIFO on each receiver and the receiver
 *       has one FIFO per sender.
 *
 *    nfifos == 1:  In this case, all senders use the same
 *       FIFO and each receiver has just one FIFO for all senders.
 */
#define FIFO_MAP(x)     ((x) & (mca_btl_sm_component.nfifos - 1))
#define FIFO_MAP_NUM(n) ( (mca_btl_sm_component.nfifos) < (n) ? (mca_btl_sm_component.nfifos) : (n) )


#define MCA_BTL_SM_FIFO_WRITE(endpoint_peer, my_smp_rank,               \
                              peer_smp_rank, hdr, resend, retry_pending_sends, rc)        \
do {                                                                    \
    sm_fifo_t* fifo = &(mca_btl_sm_component.fifo[peer_smp_rank][FIFO_MAP(my_smp_rank)]); \
                                                                        \
    if ( retry_pending_sends ) {                                        \
        if ( 0 < opal_list_get_size(&endpoint_peer->pending_sends) ) {  \
            btl_sm_process_pending_sends(endpoint_peer);                \
        }                                                               \
    }                                                                   \
                                                                        \
    opal_atomic_lock(&(fifo->head_lock));                               \
    /* post fragment */                                                 \
    if(sm_fifo_write(hdr, fifo) != OMPI_SUCCESS) {                      \
        add_pending(endpoint_peer, hdr, resend);                        \
        rc = OMPI_ERR_RESOURCE_BUSY;                                    \
    } else {                                                            \
        MCA_BTL_SM_SIGNAL_PEER(endpoint_peer);                          \
        rc = OMPI_SUCCESS;                                              \
    }                                                                   \
    opal_atomic_unlock(&(fifo->head_lock));                             \
} while(0)

#endif
