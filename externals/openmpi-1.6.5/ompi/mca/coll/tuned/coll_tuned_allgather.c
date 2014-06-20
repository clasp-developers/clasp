/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "coll_tuned.h"
#include "coll_tuned_topo.h"
#include "coll_tuned_util.h"


/*
 * ompi_coll_tuned_allgather_intra_bruck
 *
 * Function:     allgather using O(log(N)) steps.
 * Accepts:      Same arguments as MPI_Allgather
 * Returns:      MPI_SUCCESS or error code
 *
 * Description:  Variation to All-to-all algorithm described by Bruck et al.in
 *               "Efficient Algorithms for All-to-all Communications
 *                in Multiport Message-Passing Systems"
 * Memory requirements:  non-zero ranks require shift buffer to perform final
 *               step in the algorithm.
 * 
 * Example on 6 nodes:
 *   Initialization: everyone has its own buffer at location 0 in rbuf
 *                   This means if user specified MPI_IN_PLACE for sendbuf
 *                   we must copy our block from recvbuf to begining!
 *    #     0      1      2      3      4      5
 *         [0]    [1]    [2]    [3]    [4]    [5]
 *   Step 0: send message to (rank - 2^0), receive message from (rank + 2^0)
 *    #     0      1      2      3      4      5
 *         [0]    [1]    [2]    [3]    [4]    [5]
 *         [1]    [2]    [3]    [4]    [5]    [0]
 *   Step 1: send message to (rank - 2^1), receive message from (rank + 2^1)
 *           message contains all blocks from location 0 to 2^1*block size
 *    #     0      1      2      3      4      5
 *         [0]    [1]    [2]    [3]    [4]    [5]
 *         [1]    [2]    [3]    [4]    [5]    [0]
 *         [2]    [3]    [4]    [5]    [0]    [1]
 *         [3]    [4]    [5]    [0]    [1]    [2]
 *   Step 2: send message to (rank - 2^2), receive message from (rank + 2^2)
 *           message size is "all remaining blocks" 
 *    #     0      1      2      3      4      5
 *         [0]    [1]    [2]    [3]    [4]    [5]
 *         [1]    [2]    [3]    [4]    [5]    [0]
 *         [2]    [3]    [4]    [5]    [0]    [1]
 *         [3]    [4]    [5]    [0]    [1]    [2]
 *         [4]    [5]    [0]    [1]    [2]    [3]
 *         [5]    [0]    [1]    [2]    [3]    [4]
 *    Finalization: Do a local shift to get data in correct place
 *    #     0      1      2      3      4      5
 *         [0]    [0]    [0]    [0]    [0]    [0]
 *         [1]    [1]    [1]    [1]    [1]    [1]
 *         [2]    [2]    [2]    [2]    [2]    [2]
 *         [3]    [3]    [3]    [3]    [3]    [3]
 *         [4]    [4]    [4]    [4]    [4]    [4]
 *         [5]    [5]    [5]    [5]    [5]    [5]
 */
int ompi_coll_tuned_allgather_intra_bruck(void *sbuf, int scount,
                                          struct ompi_datatype_t *sdtype,
                                          void* rbuf, int rcount,
                                          struct ompi_datatype_t *rdtype,
                                          struct ompi_communicator_t *comm,
					  mca_coll_base_module_t *module)
{
   int line = -1;
   int rank, size;
   int sendto, recvfrom, distance, blockcount;
   int err = 0;
   ptrdiff_t slb, rlb, sext, rext;
   char *tmpsend = NULL, *tmprecv = NULL;

   size = ompi_comm_size(comm);
   rank = ompi_comm_rank(comm);

   OPAL_OUTPUT((ompi_coll_tuned_stream,
                "coll:tuned:allgather_intra_bruck rank %d", rank));

   err = ompi_datatype_get_extent (sdtype, &slb, &sext);
   if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   err = ompi_datatype_get_extent (rdtype, &rlb, &rext);
   if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   /* Initialization step:
      - if send buffer is not MPI_IN_PLACE, copy send buffer to block 0 of 
      receive buffer, else
      - if rank r != 0, copy r^th block from receive buffer to block 0.
   */
   tmprecv = (char*) rbuf;
   if (MPI_IN_PLACE != sbuf) {
      tmpsend = (char*) sbuf;
      err = ompi_datatype_sndrcv(tmpsend, scount, sdtype, tmprecv, rcount, rdtype);
      if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }

   } else if (0 != rank) {
      tmpsend = ((char*)rbuf) + rank * rcount * rext;
      err = ompi_datatype_copy_content_same_ddt(rdtype, rcount, tmprecv, tmpsend);
      if (err < 0) { line = __LINE__; goto err_hndl; }
   }
   
   /* Communication step:
      At every step i, rank r:
      - doubles the distance
      - sends message which starts at begining of rbuf and has size 
      (blockcount * rcount) to rank (r - distance)
      - receives message of size blockcount * rcount from rank (r + distance)
      at location (rbuf + distance * rcount * rext)
      - blockcount doubles until last step when only the remaining data is 
      exchanged.
   */
   blockcount = 1;
   tmpsend = (char*) rbuf;
   for (distance = 1; distance < size; distance<<=1) {

      recvfrom = (rank + distance) % size;
      sendto = (rank - distance + size) % size;

      tmprecv = tmpsend + distance * rcount * rext;

      if (distance <= (size >> 1)) {
         blockcount = distance;
      } else { 
         blockcount = size - distance;
      }

      /* Sendreceive */
      err = ompi_coll_tuned_sendrecv(tmpsend, blockcount * rcount, rdtype, 
                                     sendto, MCA_COLL_BASE_TAG_ALLGATHER,
                                     tmprecv, blockcount * rcount, rdtype, 
                                     recvfrom, MCA_COLL_BASE_TAG_ALLGATHER,
                                     comm, MPI_STATUS_IGNORE, rank);
      if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   }

   /* Finalization step:
      On all nodes except 0, data needs to be shifted locally:
      - create temprary shift buffer, 
      see discussion in coll_basic_reduce.c about the size and begining 
      of temporary buffer.
      - copy blocks [0 .. (size - rank - 1)] in rbuf to shift buffer
      - move blocks [(size - rank) .. size] in rbuf to begining of rbuf
      - copy blocks from shift buffer starting at block [rank] in rbuf.
   */
   if (0 != rank) {
      ptrdiff_t true_extent, true_lb;
      char *free_buf = NULL, *shift_buf = NULL;

      err = ompi_datatype_get_true_extent(rdtype, &true_lb, &true_extent);
      if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

      free_buf = (char*) calloc(((true_extent + true_lb +
                                  ((size - rank) * rcount - 1) * rext)),
                                sizeof(char));
      if (NULL == free_buf) { 
         line = __LINE__; err = OMPI_ERR_OUT_OF_RESOURCE; goto err_hndl; 
      }
      shift_buf = free_buf - rlb;
      
      tmpsend = (char*) rbuf;
      err = ompi_datatype_copy_content_same_ddt(rdtype, ((size - rank) * rcount),
                                           shift_buf, tmpsend);
      if (err < 0) { line = __LINE__; goto err_hndl;  }

      tmprecv = (char*) rbuf;
      tmpsend = (char*) rbuf + (size - rank) * rcount * rext;

      err = ompi_datatype_copy_content_same_ddt(rdtype, rank * rcount, 
                                           tmprecv, tmpsend);
      if (err < 0) { line = __LINE__; goto err_hndl;  }

      tmprecv = (char*) rbuf + rank * rcount * rext;
      err = ompi_datatype_copy_content_same_ddt(rdtype, (size - rank) * rcount, 
                                           tmprecv, shift_buf);
      if (err < 0) { line = __LINE__; goto err_hndl;  }

      free(free_buf);
   }

   return OMPI_SUCCESS;

 err_hndl:
   OPAL_OUTPUT((ompi_coll_tuned_stream,  "%s:%4d\tError occurred %d, rank %2d",
                __FILE__, line, err, rank));
   return err;
}

/*
 * ompi_coll_tuned_allgather_intra_recursivedoubling
 *
 * Function:     allgather using O(log(N)) steps.
 * Accepts:      Same arguments as MPI_Allgather
 * Returns:      MPI_SUCCESS or error code
 *
 * Description:  Recursive doubling algorithm for MPI_Allgather implementation.
 *               This algorithm is used in MPICH-2 for small- and medium-sized
 *               messages on power-of-two processes.
 *
 * Limitation:   Current implementation only works on power-of-two number of 
 *               processes.  
 *               In case this algorithm is invoked on non-power-of-two
 *               processes, Bruck algorithm will be invoked.
 * 
 * Memory requirements:
 *               No additional memory requirements beyond user-supplied buffers.
 * 
 * Example on 4 nodes:
 *   Initialization: everyone has its own buffer at location rank in rbuf
 *    #     0      1      2      3 
 *         [0]    [ ]    [ ]    [ ]
 *         [ ]    [1]    [ ]    [ ]
 *         [ ]    [ ]    [2]    [ ]
 *         [ ]    [ ]    [ ]    [3]
 *   Step 0: exchange data with (rank ^ 2^0)
 *    #     0      1      2      3 
 *         [0]    [0]    [ ]    [ ]
 *         [1]    [1]    [ ]    [ ]
 *         [ ]    [ ]    [2]    [2]
 *         [ ]    [ ]    [3]    [3]
 *   Step 1: exchange data with (rank ^ 2^1) (if you can)
 *    #     0      1      2      3 
 *         [0]    [0]    [0]    [0]
 *         [1]    [1]    [1]    [1]
 *         [2]    [2]    [2]    [2]
 *         [3]    [3]    [3]    [3]
 *
 *  TODO: Modify the algorithm to work with any number of nodes.
 *        We can modify code to use identical implementation like MPICH-2:
 *        - using recursive-halving algorithm, at the end of each step, 
 *          determine if there are nodes who did not exchange their data in that
 *          step, and send them appropriate messages.
 */
int 
ompi_coll_tuned_allgather_intra_recursivedoubling(void *sbuf, int scount,
                                                  struct ompi_datatype_t *sdtype,
                                                  void* rbuf, int rcount,
                                                  struct ompi_datatype_t *rdtype,
                                                  struct ompi_communicator_t *comm,
						  mca_coll_base_module_t *module)
{
   int line = -1;
   int rank, size, pow2size;
   int remote, distance, sendblocklocation;
   int err = 0;
   ptrdiff_t slb, rlb, sext, rext;
   char *tmpsend = NULL, *tmprecv = NULL;

   size = ompi_comm_size(comm);
   rank = ompi_comm_rank(comm);

   for (pow2size  = 1; pow2size <= size; pow2size <<=1);
   pow2size >>=1;

   /* Current implementation only handles power-of-two number of processes.
      If the function was called on non-power-of-two number of processes, 
      print warning and call bruck allgather algorithm with same parameters.
    */
   if (pow2size != size) {
      OPAL_OUTPUT((ompi_coll_tuned_stream,
                   "coll:tuned:allgather_intra_recursivedoubling WARNING: non-pow-2 size %d, switching to bruck algorithm", 
                   size));

      return ompi_coll_tuned_allgather_intra_bruck(sbuf, scount, sdtype, 
                                                   rbuf, rcount, rdtype,
						   comm, module);
   }

   OPAL_OUTPUT((ompi_coll_tuned_stream,
                "coll:tuned:allgather_intra_recursivedoubling rank %d, size %d", 
                rank, size));

   err = ompi_datatype_get_extent (sdtype, &slb, &sext);
   if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   err = ompi_datatype_get_extent (rdtype, &rlb, &rext);
   if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   /* Initialization step:
      - if send buffer is not MPI_IN_PLACE, copy send buffer to block 0 of 
      receive buffer
   */
   if (MPI_IN_PLACE != sbuf) {
      tmpsend = (char*) sbuf;
      tmprecv = (char*) rbuf + rank * rcount * rext;
      err = ompi_datatype_sndrcv(tmpsend, scount, sdtype, tmprecv, rcount, rdtype);
      if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }

   } 
   
   /* Communication step:
      At every step i, rank r:
      - exchanges message with rank remote = (r ^ 2^i).

   */
   sendblocklocation = rank;
   for (distance = 0x1; distance < size; distance<<=1) {
      remote = rank ^ distance;

      if (rank < remote) {
         tmpsend = (char*)rbuf + sendblocklocation * rcount * rext;
         tmprecv = (char*)rbuf + (sendblocklocation + distance) * rcount * rext;
      } else {
         tmpsend = (char*)rbuf + sendblocklocation * rcount * rext;
         tmprecv = (char*)rbuf + (sendblocklocation - distance) * rcount * rext;
         sendblocklocation -= distance;
      }

      /* Sendreceive */
      err = ompi_coll_tuned_sendrecv(tmpsend, distance * rcount, rdtype,
                                     remote, MCA_COLL_BASE_TAG_ALLGATHER,
                                     tmprecv, distance * rcount, rdtype,
                                     remote, MCA_COLL_BASE_TAG_ALLGATHER,
                                     comm, MPI_STATUS_IGNORE, rank);
      if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   }

   return OMPI_SUCCESS;

 err_hndl:
   OPAL_OUTPUT((ompi_coll_tuned_stream,  "%s:%4d\tError occurred %d, rank %2d",
                __FILE__, line, err, rank));
   return err;
}



/*
 * ompi_coll_tuned_allgather_intra_ring
 *
 * Function:     allgather using O(N) steps.
 * Accepts:      Same arguments as MPI_Allgather
 * Returns:      MPI_SUCCESS or error code
 *
 * Description:  Ring algorithm for all gather.
 *               At every step i, rank r receives message from rank (r - 1)
 *               containing data from rank (r - i - 1) and sends message to rank
 *               (r + 1) containing data from rank (r - i), with wrap arounds.
 * Memory requirements:
 *               No additional memory requirements.
 * 
 */
int ompi_coll_tuned_allgather_intra_ring(void *sbuf, int scount,
                                         struct ompi_datatype_t *sdtype,
                                         void* rbuf, int rcount,
                                         struct ompi_datatype_t *rdtype,
                                         struct ompi_communicator_t *comm,
					 mca_coll_base_module_t *module)
{
   int line = -1;
   int rank, size;
   int sendto, recvfrom, i, recvdatafrom, senddatafrom;
   int err = 0;
   ptrdiff_t slb, rlb, sext, rext;
   char *tmpsend = NULL, *tmprecv = NULL;

   size = ompi_comm_size(comm);
   rank = ompi_comm_rank(comm);

   OPAL_OUTPUT((ompi_coll_tuned_stream,
                "coll:tuned:allgather_intra_ring rank %d", rank));

   err = ompi_datatype_get_extent (sdtype, &slb, &sext);
   if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   err = ompi_datatype_get_extent (rdtype, &rlb, &rext);
   if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   /* Initialization step:
      - if send buffer is not MPI_IN_PLACE, copy send buffer to appropriate block
        of receive buffer
   */
   tmprecv = (char*) rbuf + rank * rcount * rext;
   if (MPI_IN_PLACE != sbuf) {
      tmpsend = (char*) sbuf;
      err = ompi_datatype_sndrcv(tmpsend, scount, sdtype, tmprecv, rcount, rdtype);
      if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }
   } 
   
   /* Communication step:
      At every step i: 0 .. (P-1), rank r:
      - receives message from [(r - 1 + size) % size] containing data from rank
        [(r - i - 1 + size) % size]
      - sends message to rank [(r + 1) % size] containing data from rank
        [(r - i + size) % size]
      - sends message which starts at begining of rbuf and has size 
   */
   sendto = (rank + 1) % size;
   recvfrom  = (rank - 1 + size) % size;

   for (i = 0; i < size - 1; i++) {
      recvdatafrom = (rank - i - 1 + size) % size;
      senddatafrom = (rank - i + size) % size;

      tmprecv = (char*)rbuf + recvdatafrom * rcount * rext;
      tmpsend = (char*)rbuf + senddatafrom * rcount * rext;

      /* Sendreceive */
      err = ompi_coll_tuned_sendrecv(tmpsend, rcount, rdtype, sendto,
                                     MCA_COLL_BASE_TAG_ALLGATHER,
                                     tmprecv, rcount, rdtype, recvfrom,
                                     MCA_COLL_BASE_TAG_ALLGATHER,
                                     comm, MPI_STATUS_IGNORE, rank);
      if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   }

   return OMPI_SUCCESS;

 err_hndl:
   OPAL_OUTPUT((ompi_coll_tuned_stream,  "%s:%4d\tError occurred %d, rank %2d",
                __FILE__, line, err, rank));
   return err;
}

/*
 * ompi_coll_tuned_allgather_intra_neighborexchange
 *
 * Function:     allgather using N/2 steps (O(N))
 * Accepts:      Same arguments as MPI_Allgather
 * Returns:      MPI_SUCCESS or error code
 *
 * Description:  Neighbor Exchange algorithm for allgather.
 *               Described by Chen et.al. in 
 *               "Performance Evaluation of Allgather Algorithms on 
 *                Terascale Linux Cluster with Fast Ethernet",
 *               Proceedings of the Eighth International Conference on 
 *               High-Performance Computing inn Asia-Pacific Region
 *               (HPCASIA'05), 2005
 * 
 *               Rank r exchanges message with one of its neighbors and
 *               forwards the data further in the next step.
 *
 *               No additional memory requirements.
 * 
 * Limitations:  Algorithm works only on even number of processes.
 *               For odd number of processes we switch to ring algorithm.
 * 
 * Example on 6 nodes:
 *  Initial state
 *    #     0      1      2      3      4      5
 *         [0]    [ ]    [ ]    [ ]    [ ]    [ ]
 *         [ ]    [1]    [ ]    [ ]    [ ]    [ ]
 *         [ ]    [ ]    [2]    [ ]    [ ]    [ ]
 *         [ ]    [ ]    [ ]    [3]    [ ]    [ ]
 *         [ ]    [ ]    [ ]    [ ]    [4]    [ ]
 *         [ ]    [ ]    [ ]    [ ]    [ ]    [5]
 *   Step 0:
 *    #     0      1      2      3      4      5
 *         [0]    [0]    [ ]    [ ]    [ ]    [ ]
 *         [1]    [1]    [ ]    [ ]    [ ]    [ ]
 *         [ ]    [ ]    [2]    [2]    [ ]    [ ]
 *         [ ]    [ ]    [3]    [3]    [ ]    [ ]
 *         [ ]    [ ]    [ ]    [ ]    [4]    [4]
 *         [ ]    [ ]    [ ]    [ ]    [5]    [5]
 *   Step 1:
 *    #     0      1      2      3      4      5
 *         [0]    [0]    [0]    [ ]    [ ]    [0]
 *         [1]    [1]    [1]    [ ]    [ ]    [1]
 *         [ ]    [2]    [2]    [2]    [2]    [ ]
 *         [ ]    [3]    [3]    [3]    [3]    [ ]
 *         [4]    [ ]    [ ]    [4]    [4]    [4]
 *         [5]    [ ]    [ ]    [5]    [5]    [5]
 *   Step 2:
 *    #     0      1      2      3      4      5
 *         [0]    [0]    [0]    [0]    [0]    [0]
 *         [1]    [1]    [1]    [1]    [1]    [1]
 *         [2]    [2]    [2]    [2]    [2]    [2]
 *         [3]    [3]    [3]    [3]    [3]    [3]
 *         [4]    [4]    [4]    [4]    [4]    [4]
 *         [5]    [5]    [5]    [5]    [5]    [5]
 */
int 
ompi_coll_tuned_allgather_intra_neighborexchange(void *sbuf, int scount,
                                                 struct ompi_datatype_t *sdtype,
                                                 void* rbuf, int rcount,
                                                 struct ompi_datatype_t *rdtype,
                                                 struct ompi_communicator_t *comm,
						 mca_coll_base_module_t *module)
{
   int line = -1;
   int rank, size;
   int neighbor[2], offset_at_step[2], recv_data_from[2], send_data_from;
   int i, even_rank;
   int err = 0;
   ptrdiff_t slb, rlb, sext, rext;
   char *tmpsend = NULL, *tmprecv = NULL;

   size = ompi_comm_size(comm);
   rank = ompi_comm_rank(comm);

   if (size % 2) {
      OPAL_OUTPUT((ompi_coll_tuned_stream,
                   "coll:tuned:allgather_intra_neighborexchange WARNING: odd size %d, switching to ring algorithm", 
                   size));
      return ompi_coll_tuned_allgather_intra_ring(sbuf, scount, sdtype,
                                                  rbuf, rcount, rdtype,
                                                  comm, module);
   }

   OPAL_OUTPUT((ompi_coll_tuned_stream,
                "coll:tuned:allgather_intra_neighborexchange rank %d", rank));

   err = ompi_datatype_get_extent (sdtype, &slb, &sext);
   if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   err = ompi_datatype_get_extent (rdtype, &rlb, &rext);
   if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   /* Initialization step:
      - if send buffer is not MPI_IN_PLACE, copy send buffer to appropriate block
        of receive buffer
   */
   tmprecv = (char*) rbuf + rank * rcount * rext;
   if (MPI_IN_PLACE != sbuf) {
      tmpsend = (char*) sbuf;
      err = ompi_datatype_sndrcv(tmpsend, scount, sdtype, tmprecv, rcount, rdtype);
      if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }
   } 

   /* Determine neighbors, order in which blocks will arrive, etc. */
   even_rank = !(rank % 2);
   if (even_rank) {
      neighbor[0] = (rank + 1) % size;
      neighbor[1] = (rank - 1 + size) % size;
      recv_data_from[0] = rank;
      recv_data_from[1] = rank;
      offset_at_step[0] = (+2);
      offset_at_step[1] = (-2);
   } else {
      neighbor[0] = (rank - 1 + size) % size;
      neighbor[1] = (rank + 1) % size;
      recv_data_from[0] = neighbor[0];
      recv_data_from[1] = neighbor[0];
      offset_at_step[0] = (-2);
      offset_at_step[1] = (+2);
   }

   /* Communication loop:
      - First step is special: exchange a single block with neighbor[0].
      - Rest of the steps: 
        update recv_data_from according to offset, and 
        exchange two blocks with appropriate neighbor.
        the send location becomes previous receve location.
   */
   tmprecv = (char*)rbuf + neighbor[0] * rcount * rext;
   tmpsend = (char*)rbuf + rank * rcount * rext;
   /* Sendreceive */
   err = ompi_coll_tuned_sendrecv(tmpsend, rcount, rdtype, neighbor[0],
                                  MCA_COLL_BASE_TAG_ALLGATHER,
                                  tmprecv, rcount, rdtype, neighbor[0],
                                  MCA_COLL_BASE_TAG_ALLGATHER,
                                  comm, MPI_STATUS_IGNORE, rank);
   if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   /* Determine initial sending location */
   if (even_rank) {
      send_data_from = rank;
   } else {
      send_data_from = recv_data_from[0];
   }

   for (i = 1; i < (size / 2); i++) {
      const int i_parity = i % 2;
      recv_data_from[i_parity] = 
         (recv_data_from[i_parity] + offset_at_step[i_parity] + size) % size;

      tmprecv = (char*)rbuf + recv_data_from[i_parity] * rcount * rext;
      tmpsend = (char*)rbuf + send_data_from * rcount * rext;
      
      /* Sendreceive */
      err = ompi_coll_tuned_sendrecv(tmpsend, 2 * rcount, rdtype, 
                                     neighbor[i_parity], 
                                     MCA_COLL_BASE_TAG_ALLGATHER,
                                     tmprecv, 2 * rcount, rdtype,
                                     neighbor[i_parity],
                                     MCA_COLL_BASE_TAG_ALLGATHER,
                                     comm, MPI_STATUS_IGNORE, rank);
      if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

      send_data_from = recv_data_from[i_parity];
   }

   return OMPI_SUCCESS;

 err_hndl:
   OPAL_OUTPUT((ompi_coll_tuned_stream,  "%s:%4d\tError occurred %d, rank %2d",
                __FILE__, line, err, rank));
   return err;
}


int ompi_coll_tuned_allgather_intra_two_procs(void *sbuf, int scount,
                                              struct ompi_datatype_t *sdtype,
                                              void* rbuf, int rcount,
                                              struct ompi_datatype_t *rdtype,
                                              struct ompi_communicator_t *comm,
					      mca_coll_base_module_t *module)
{
   int line = -1, err = 0;
   int rank;
   int remote;
   char *tmpsend = NULL, *tmprecv = NULL;
   ptrdiff_t sext, rext, lb;

   rank = ompi_comm_rank(comm);

   OPAL_OUTPUT((ompi_coll_tuned_stream,
                "ompi_coll_tuned_allgather_intra_two_procs rank %d", rank));

   err = ompi_datatype_get_extent (sdtype, &lb, &sext);
   if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   err = ompi_datatype_get_extent (rdtype, &lb, &rext);
   if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   /* Exchange data:
      - compute source and destinations
      - send receive data
    */
   remote  = rank ^ 0x1;

   tmpsend = (char*)sbuf;
   if (MPI_IN_PLACE == sbuf) {
      tmpsend = (char*)rbuf + rank * rcount * rext;
      scount = rcount;
      sdtype = rdtype;
   }
   tmprecv = (char*)rbuf + remote * rcount * rext;

   err = ompi_coll_tuned_sendrecv(tmpsend, scount, sdtype, remote,
                                  MCA_COLL_BASE_TAG_ALLGATHER,
                                  tmprecv, rcount, rdtype, remote,
                                  MCA_COLL_BASE_TAG_ALLGATHER,
                                  comm, MPI_STATUS_IGNORE, rank);
   if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl; }

   /* Place your data in correct location if necessary */
   if (MPI_IN_PLACE != sbuf) {
      err = ompi_datatype_sndrcv((char*)sbuf, scount, sdtype, 
                            (char*)rbuf + rank * rcount * rext, rcount, rdtype);
      if (MPI_SUCCESS != err) { line = __LINE__; goto err_hndl;  }
   }

   return MPI_SUCCESS;

 err_hndl:
   OPAL_OUTPUT((ompi_coll_tuned_stream, "%s:%4d\tError occurred %d, rank %2d",
                __FILE__, line, err, rank));
   return err;
}


/*
 * Linear functions are copied from the BASIC coll module
 * they do not segment the message and are simple implementations
 * but for some small number of nodes and/or small data sizes they 
 * are just as fast as tuned/tree based segmenting operations 
 * and as such may be selected by the decision functions
 * These are copied into this module due to the way we select modules
 * in V1. i.e. in V2 we will handle this differently and so will not
 * have to duplicate code.
 * JPG following the examples from other coll_tuned implementations. Dec06.
 */

/* copied function (with appropriate renaming) starts here */

/*
 *    allgather_intra_basic_linear
 *
 *    Function:    - allgather using other MPI collections
 *    Accepts:    - same as MPI_Allgather()
 *    Returns:    - MPI_SUCCESS or error code
 */
int
ompi_coll_tuned_allgather_intra_basic_linear(void *sbuf, int scount,
                                             struct ompi_datatype_t *sdtype, 
                                             void *rbuf,
                                             int rcount, 
                                             struct ompi_datatype_t *rdtype,
                                             struct ompi_communicator_t *comm,
					     mca_coll_base_module_t *module)
{
    int err;
    ptrdiff_t lb, extent;

    /* Handle MPI_IN_PLACE (see explanantion in reduce.c for how to
       allocate temp buffer) -- note that rank 0 can use IN_PLACE
       natively, and we can just alias the right position in rbuf
       as sbuf and avoid using a temporary buffer if gather is
       implemented correctly */
    if (MPI_IN_PLACE == sbuf && 0 != ompi_comm_rank(comm)) {
       ompi_datatype_get_extent(rdtype, &lb, &extent);
       sbuf = ((char*) rbuf) + (ompi_comm_rank(comm) * extent * rcount);
       sdtype = rdtype;
       scount = rcount;
    } 

    /* Gather and broadcast. */

    err = comm->c_coll.coll_gather(sbuf, scount, sdtype,
				   rbuf, rcount, rdtype,
				   0, comm, comm->c_coll.coll_gather_module);
    if (MPI_SUCCESS == err) {
        err = comm->c_coll.coll_bcast(rbuf, rcount * ompi_comm_size(comm), rdtype,
				      0, comm, comm->c_coll.coll_bcast_module);
    }

    /* All done */

    return err;
}

/* copied function (with appropriate renaming) ends here */

/* The following are used by dynamic and forced rules */

/* publish details of each algorithm and if its forced/fixed/locked in */
/* as you add methods/algorithms you must update this and the query/map 
   routines */

/* this routine is called by the component only */
/* this makes sure that the mca parameters are set to their initial values 
   and perms */
/* module does not call this they call the forced_getvalues routine instead */

int 
ompi_coll_tuned_allgather_intra_check_forced_init(coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices)
{
   int max_alg = 6, requested_alg;
   
   ompi_coll_tuned_forced_max_algorithms[ALLGATHER] = max_alg;
   
   mca_base_param_reg_int (&mca_coll_tuned_component.super.collm_version,
                           "allgather_algorithm_count",
                           "Number of allgather algorithms available",
                           false, true, max_alg, NULL);
   
    mca_param_indices->algorithm_param_index
       = mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                                "allgather_algorithm",
                                "Which allgather algorithm is used. Can be locked down to choice of: 0 ignore, 1 basic linear, 2 bruck, 3 recursive doubling, 4 ring, 5 neighbor exchange, 6: two proc only.",
                                false, false, 0, NULL);
    if (mca_param_indices->algorithm_param_index < 0) {
        return mca_param_indices->algorithm_param_index;
    }
    mca_base_param_lookup_int(mca_param_indices->algorithm_param_index, 
                              &(requested_alg));
    if( 0 > requested_alg || requested_alg > max_alg ) {
       if( 0 == ompi_comm_rank( MPI_COMM_WORLD ) ) {
          opal_output( 0, "Allgather algorithm #%d is not available (range [0..%d]). Switching back to ignore(0)\n",
                       requested_alg, max_alg );
       }
       mca_base_param_set_int( mca_param_indices->algorithm_param_index, 0);
    }
    
    mca_param_indices->segsize_param_index
       = mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                                "allgather_algorithm_segmentsize",
                                "Segment size in bytes used by default for allgather algorithms. Only has meaning if algorithm is forced and supports segmenting. 0 bytes means no segmentation. Currently, available algorithms do not support segmentation.",
                                false, false, 0, NULL);
    
    mca_param_indices->tree_fanout_param_index
       = mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                                "allgather_algorithm_tree_fanout",
                                "Fanout for n-tree used for allgather algorithms. Only has meaning if algorithm is forced and supports n-tree topo based operation. Currently, available algorithms do not support n-tree topologies.",
                                false, false, 
                                ompi_coll_tuned_init_tree_fanout, /* get system wide default */
                                NULL);

    mca_param_indices->chain_fanout_param_index
       = mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                                "allgather_algorithm_chain_fanout",
                                "Fanout for chains used for allgather algorithms. Only has meaning if algorithm is forced and supports chain topo based operation. Currently, available algorithms do not support chain topologies.",
                                false, false, 
                                ompi_coll_tuned_init_chain_fanout, /* get system wide default */
                                NULL);

    return (MPI_SUCCESS);
}

int ompi_coll_tuned_allgather_intra_do_forced(void *sbuf, int scount,
                                              struct ompi_datatype_t *sdtype,
                                              void* rbuf, int rcount,
                                              struct ompi_datatype_t *rdtype,
                                              struct ompi_communicator_t *comm,
					      mca_coll_base_module_t *module)
{
    mca_coll_tuned_module_t *tuned_module = (mca_coll_tuned_module_t*) module;
    mca_coll_tuned_comm_t *data = tuned_module->tuned_data;

    OPAL_OUTPUT((ompi_coll_tuned_stream,
		 "coll:tuned:allgather_intra_do_forced selected algorithm %d",
		 data->user_forced[ALLGATHER].algorithm));

    switch (data->user_forced[ALLGATHER].algorithm) {
    case (0):   
	return ompi_coll_tuned_allgather_intra_dec_fixed (sbuf, scount, sdtype, 
							  rbuf, rcount, rdtype, 
							  comm, module);
    case (1):   
	return ompi_coll_tuned_allgather_intra_basic_linear (sbuf, scount, sdtype,
							     rbuf, rcount, rdtype,
							     comm, module);
    case (2):   
	return ompi_coll_tuned_allgather_intra_bruck (sbuf, scount, sdtype, 
						      rbuf, rcount, rdtype,
						      comm, module);
    case (3):   
	return ompi_coll_tuned_allgather_intra_recursivedoubling (sbuf, scount, sdtype, 
								  rbuf, rcount, rdtype, 
								  comm, module);
    case (4):
	return ompi_coll_tuned_allgather_intra_ring (sbuf, scount, sdtype, 
						     rbuf, rcount, rdtype,
						     comm, module);
    case (5):
	return ompi_coll_tuned_allgather_intra_neighborexchange (sbuf, scount, sdtype, 
								 rbuf, rcount, rdtype, 
								 comm, module);
    case (6):
	return ompi_coll_tuned_allgather_intra_two_procs (sbuf, scount, sdtype, 
							  rbuf, rcount, rdtype, 
							  comm, module);
    default:
	OPAL_OUTPUT((ompi_coll_tuned_stream,
		     "coll:tuned:allgather_intra_do_forced attempt to select algorithm %d when only 0-%d is valid?", 
		     data->user_forced[ALLGATHER].algorithm,
		     ompi_coll_tuned_forced_max_algorithms[ALLGATHER]));
	return (MPI_ERR_ARG);
    } /* switch */

}


int ompi_coll_tuned_allgather_intra_do_this(void *sbuf, int scount,
					    struct ompi_datatype_t *sdtype,
					    void* rbuf, int rcount,
					    struct ompi_datatype_t *rdtype,
					    struct ompi_communicator_t *comm,
					    mca_coll_base_module_t *module,
					    int algorithm, int faninout, int segsize)
{
   OPAL_OUTPUT((ompi_coll_tuned_stream,
               "coll:tuned:allgather_intra_do_this selected algorithm %d topo faninout %d segsize %d", 
                algorithm, faninout, segsize));
   
   switch (algorithm) {
   case (0):   
      return ompi_coll_tuned_allgather_intra_dec_fixed(sbuf, scount, sdtype, 
                                                       rbuf, rcount, rdtype, 
                                                       comm, module);
   case (1):   
      return ompi_coll_tuned_allgather_intra_basic_linear(sbuf, scount, sdtype,
                                                          rbuf, rcount, rdtype,
                                                          comm, module);
   case (2): 
      return ompi_coll_tuned_allgather_intra_bruck(sbuf, scount, sdtype, 
                                                   rbuf, rcount, rdtype,
						   comm, module);
   case (3): 
      return ompi_coll_tuned_allgather_intra_recursivedoubling(sbuf, scount, sdtype, 
                                                               rbuf, rcount, rdtype, 
                                                               comm, module);
   case (4): 
      return ompi_coll_tuned_allgather_intra_ring(sbuf, scount, sdtype, 
                                                  rbuf, rcount, rdtype,
						  comm, module);
   case (5): 
      return ompi_coll_tuned_allgather_intra_neighborexchange(sbuf, scount, sdtype, 
                                                              rbuf, rcount, rdtype, 
                                                              comm, module);
   case (6):
      return ompi_coll_tuned_allgather_intra_two_procs (sbuf, scount, sdtype, 
                                                        rbuf, rcount, rdtype, 
                                                        comm, module);
   default:
      OPAL_OUTPUT((ompi_coll_tuned_stream,
                   "coll:tuned:allgather_intra_do_this attempt to select algorithm %d when only 0-%d is valid?", 
                   algorithm, 
                   ompi_coll_tuned_forced_max_algorithms[ALLGATHER]));
      return (MPI_ERR_ARG);
   } /* switch */
}
