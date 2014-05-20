/**
 * MPI-calls of MPI-1.2-standard (Complete C Language Binding)
 *
 * @see MPI-1.2 standard
 *
 * @author Bettina Krammer, Katrin Bidmon, Matthias Mueller, Tobias Hilbrich
 *         Matthias Jurenz
 *
 * For Fortran we need additional information for the arguments.
 * That is necessary for the handle conversion for MPI-Implementations like
 * OpenMPI.
 * We need for all MPI-Handles of pointer type information how the argument
 * is used. That means we have to know whether it is:
 *      * an out single value
 *      * an in-out single value
 *      * an array for input purpoeses
 *      * an out array for output purposes
 *      * an in-out array
 * For the arrays we additionally need to know their size !
 * We have to denote all this in the argument name, so we will append at the 
 * end of these argument names:
 *      * _CLASS_SINGLE_IN for an in single value
 *        (this is often used for requests)
 *      * _CLASS_SINGLE_OUT for an out single value
 *      * _CLASS_SINGLE_IO for an in-out single value
 *      * _CLASS_BUFFER a buffer that can be MPI_BOTTOM
 *      * _CLASS_BUFFER_IN_PLACE a buffer that can be MPI_BOTTOM
 *        and/or MPI_IN_PLACE
 *      * _CLASS_ARRAY_IN_sizeargument an array for input purpoeses
 *      * _CLASS_ARRAY_OUT_sizeargument an out array for output purposes
 *      * _CLASS_ARRAY_IO_sizeargument an in-out array
 * Where "sizeargument" is the fixed numeric size or the argument speci-
 * fing the array size.
 * Otherwise we would have to create the wrapper manually.
 *
 **/

VT_MPI_INT MPI_Abort(MPI_Comm comm, VT_MPI_INT errorcode);
VT_MPI_INT MPI_Address(CONST void* location, MPI_Aint* address_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Allgather(CONST void* sendbuf_CLASS_BUFFER_IN_PLACE, VT_MPI_INT sendcount, MPI_Datatype sendtype, void* recvbuf_CLASS_BUFFER, VT_MPI_INT recvcount, MPI_Datatype recvtype, MPI_Comm comm); /*COLL_ALL2ALL*/
VT_MPI_INT MPI_Allgatherv(CONST void* sendbuf_CLASS_BUFFER_IN_PLACE, VT_MPI_INT sendcount, MPI_Datatype sendtype, void* recvbuf_CLASS_BUFFER, CONST VT_MPI_INT* recvcounts, CONST VT_MPI_INT* displs, MPI_Datatype recvtype, MPI_Comm comm); /*COLL_ALL2ALL*/
VT_MPI_INT MPI_Allreduce(CONST void* sendbuf_CLASS_BUFFER_IN_PLACE, void* recvbuf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm); /*COLL_ALL2ALL*/
VT_MPI_INT MPI_Alltoall(CONST void* sendbuf_CLASS_BUFFER, VT_MPI_INT sendcount, MPI_Datatype sendtype, void* recvbuf_CLASS_BUFFER, VT_MPI_INT recvcount, MPI_Datatype recvtype, MPI_Comm comm); /*COLL_ALL2ALL*/
VT_MPI_INT MPI_Alltoallv(CONST void* sendbuf_CLASS_BUFFER, CONST VT_MPI_INT* sendcounts, CONST VT_MPI_INT* sdispls, MPI_Datatype sendtype, void* recvbuf_CLASS_BUFFER, CONST VT_MPI_INT* recvcounts, CONST VT_MPI_INT* rdispls, MPI_Datatype recvtype, MPI_Comm comm); /*COLL_ALL2ALL*/
VT_MPI_INT MPI_Attr_delete(MPI_Comm comm, VT_MPI_INT keyval);
VT_MPI_INT MPI_Attr_get(MPI_Comm comm, VT_MPI_INT keyval, void* attribute_val, VT_MPI_INT* flag);
VT_MPI_INT MPI_Attr_put(MPI_Comm comm, VT_MPI_INT keyval, void* attribute_val);
VT_MPI_INT MPI_Barrier(MPI_Comm comm); /*COLL_BARRIER*/
VT_MPI_INT MPI_Bcast(void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT root, MPI_Comm comm); /*COLL_ONE2ALL*/
VT_MPI_INT MPI_Bsend(void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm);
VT_MPI_INT MPI_Bsend_init(CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Buffer_attach(void* buf, VT_MPI_INT size);
VT_MPI_INT MPI_Buffer_detach(void* buf, VT_MPI_INT* size);
VT_MPI_INT MPI_Cancel(MPI_Request* request_CLASS_SINGLE_IN);
VT_MPI_INT MPI_Cart_coords(MPI_Comm comm, VT_MPI_INT rank, VT_MPI_INT maxdims, VT_MPI_INT* coords);
VT_MPI_INT MPI_Cart_create(MPI_Comm comm_old, VT_MPI_INT ndims, CONST VT_MPI_INT* dims, CONST VT_MPI_INT* periods, VT_MPI_INT reorder, MPI_Comm* comm_cart_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Cart_get(MPI_Comm comm, VT_MPI_INT maxdims, VT_MPI_INT* dims, VT_MPI_INT* periods, VT_MPI_INT* coords);
VT_MPI_INT MPI_Cart_map(MPI_Comm comm, VT_MPI_INT ndims, CONST VT_MPI_INT* dims, CONST VT_MPI_INT* periods, VT_MPI_INT* newrank);
VT_MPI_INT MPI_Cart_rank(MPI_Comm comm, CONST VT_MPI_INT* coords, VT_MPI_INT* rank);
VT_MPI_INT MPI_Cart_shift(MPI_Comm comm, VT_MPI_INT direction, VT_MPI_INT disp, VT_MPI_INT* rank_source, VT_MPI_INT* rank_dest);
VT_MPI_INT MPI_Cart_sub(MPI_Comm comm, CONST VT_MPI_INT* remain_dims, MPI_Comm* newcomm_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Cartdim_get(MPI_Comm comm, VT_MPI_INT* ndims);
VT_MPI_INT MPI_Comm_compare(MPI_Comm comm1, MPI_Comm comm2, VT_MPI_INT* _result);
VT_MPI_INT MPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm* newcomm_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Comm_dup(MPI_Comm comm, MPI_Comm* newcomm_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Comm_free(MPI_Comm* comm_CLASS_SINGLE_IO);
VT_MPI_INT MPI_Comm_group(MPI_Comm comm, MPI_Group* group_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Comm_rank(MPI_Comm comm, VT_MPI_INT* rank);
VT_MPI_INT MPI_Comm_remote_group(MPI_Comm comm, MPI_Group* group_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Comm_remote_size(MPI_Comm comm, VT_MPI_INT* size);
VT_MPI_INT MPI_Comm_size(MPI_Comm comm, VT_MPI_INT* size);
VT_MPI_INT MPI_Comm_split(MPI_Comm comm, VT_MPI_INT color, VT_MPI_INT key, MPI_Comm* newcomm_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Comm_test_inter(MPI_Comm comm, VT_MPI_INT* flag);
VT_MPI_INT MPI_Dims_create(VT_MPI_INT nnodes, VT_MPI_INT ndims, VT_MPI_INT* dims);
VT_MPI_INT MPI_Errhandler_create(MPI_Handler_function* function, MPI_Errhandler* errhandler_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Errhandler_free(MPI_Errhandler* errhandler_CLASS_SINGLE_IN);
VT_MPI_INT MPI_Errhandler_get(MPI_Comm comm, MPI_Errhandler* errhandler_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Errhandler_set(MPI_Comm comm, MPI_Errhandler errhandler);
VT_MPI_INT MPI_Error_class(VT_MPI_INT errorcode, VT_MPI_INT* errorclass);
VT_MPI_INT MPI_Error_string(VT_MPI_INT errorcode, char* string_CLASS_SINGLE_OUT, VT_MPI_INT* resultlen);
VT_MPI_INT MPI_Finalize(void);
VT_MPI_INT MPI_Gather(CONST void* sendbuf_CLASS_BUFFER_IN_PLACE, VT_MPI_INT sendcount, MPI_Datatype sendtype, void* recvbuf_CLASS_BUFFER, VT_MPI_INT recvcount, MPI_Datatype recvtype, VT_MPI_INT root, MPI_Comm comm); /*COLL_ALL2ONE*/
VT_MPI_INT MPI_Gatherv(CONST void* sendbuf_CLASS_BUFFER_IN_PLACE, VT_MPI_INT sendcount, MPI_Datatype sendtype, void* recvbuf_CLASS_BUFFER, CONST VT_MPI_INT* recvcounts, CONST VT_MPI_INT* displs, MPI_Datatype recvtype, VT_MPI_INT root, MPI_Comm comm); /*COLL_ALL2ONE*/
VT_MPI_INT MPI_Get_count(CONST MPI_Status* status_CLASS_SINGLE_IN, MPI_Datatype datatype, VT_MPI_INT* count);
VT_MPI_INT MPI_Get_elements(CONST MPI_Status* status_CLASS_SINGLE_IN, MPI_Datatype datatype, VT_MPI_INT* count);
VT_MPI_INT MPI_Get_processor_name(char* name_CLASS_SINGLE_OUT, VT_MPI_INT* resultlen);
VT_MPI_INT MPI_Get_version(VT_MPI_INT* version, VT_MPI_INT* subversion);
VT_MPI_INT MPI_Graph_create(MPI_Comm comm_old, VT_MPI_INT nnodes, CONST VT_MPI_INT* index, CONST VT_MPI_INT* edges, VT_MPI_INT reorder, MPI_Comm* comm_graph_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Graph_get(MPI_Comm comm, VT_MPI_INT maxindex, VT_MPI_INT maxedges, VT_MPI_INT* index, VT_MPI_INT* edges);
VT_MPI_INT MPI_Graph_map(MPI_Comm comm, VT_MPI_INT nnodes, CONST VT_MPI_INT* index, CONST VT_MPI_INT* edges, VT_MPI_INT* newrank);
VT_MPI_INT MPI_Graph_neighbors(MPI_Comm comm, VT_MPI_INT rank, VT_MPI_INT maxneighbors, VT_MPI_INT* neighbors);
VT_MPI_INT MPI_Graph_neighbors_count(MPI_Comm comm, VT_MPI_INT rank, VT_MPI_INT* nneighbors);
VT_MPI_INT MPI_Graphdims_get(MPI_Comm comm, VT_MPI_INT* nnodes, VT_MPI_INT* nedges);
VT_MPI_INT MPI_Group_compare(MPI_Group group1,MPI_Group group2, VT_MPI_INT* _result);
VT_MPI_INT MPI_Group_difference(MPI_Group group1, MPI_Group group2, MPI_Group* newgroup_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Group_excl(MPI_Group group, VT_MPI_INT n, CONST VT_MPI_INT* ranks, MPI_Group* newgroup_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Group_free(MPI_Group* group_CLASS_SINGLE_IO);
VT_MPI_INT MPI_Group_incl(MPI_Group group, VT_MPI_INT n, CONST VT_MPI_INT* ranks, MPI_Group* newgroup_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Group_intersection(MPI_Group group1, MPI_Group group2, MPI_Group* newgroup_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Group_range_excl(MPI_Group group, VT_MPI_INT n, VT_MPI_INT ranges[][3], MPI_Group* newgroup_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Group_range_incl(MPI_Group group, VT_MPI_INT n, VT_MPI_INT ranges[][3], MPI_Group* newgroup_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Group_rank(MPI_Group group, VT_MPI_INT* rank);
VT_MPI_INT MPI_Group_size(MPI_Group group, VT_MPI_INT* size);
VT_MPI_INT MPI_Group_translate_ranks(MPI_Group group1, VT_MPI_INT n, CONST VT_MPI_INT* ranks1, MPI_Group group2, VT_MPI_INT* ranks2);
VT_MPI_INT MPI_Group_union(MPI_Group group1, MPI_Group group2, MPI_Group* newgroup_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Ibsend(CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Init(VT_MPI_INT* argc, char*** argv);
VT_MPI_INT MPI_Initialized(VT_MPI_INT* flag);
VT_MPI_INT MPI_Intercomm_create(MPI_Comm local_comm, VT_MPI_INT local_leader, MPI_Comm peer_comm, VT_MPI_INT remote_leader, VT_MPI_INT tag, MPI_Comm* newintercomm_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Intercomm_merge(MPI_Comm intercomm, VT_MPI_INT high, MPI_Comm* newintracomm_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Iprobe(VT_MPI_INT source, VT_MPI_INT tag, MPI_Comm comm, VT_MPI_INT* flag, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Irecv(void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT source, VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Irsend(CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Isend(CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Issend(CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Keyval_create(MPI_Copy_function* copy_fn, MPI_Delete_function* delete_fn, VT_MPI_INT* keyval, void* extra_state);
VT_MPI_INT MPI_Keyval_free(VT_MPI_INT* keyval);
VT_MPI_INT MPI_Op_create(MPI_User_function* function, VT_MPI_INT commute, MPI_Op* op_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Op_free( MPI_Op* op_CLASS_SINGLE_IO);
VT_MPI_INT MPI_Pack(CONST void* inbuf_CLASS_BUFFER, VT_MPI_INT incount, MPI_Datatype datatype, void* outbuf, VT_MPI_INT outsize, VT_MPI_INT* position,  MPI_Comm comm);
VT_MPI_INT MPI_Pack_size(VT_MPI_INT incount, MPI_Datatype datatype, MPI_Comm comm, VT_MPI_INT* size);
VT_MPI_INT MPI_Pcontrol(VT_MPI_INT level, ...);
VT_MPI_INT MPI_Probe(VT_MPI_INT source, VT_MPI_INT tag, MPI_Comm comm, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Recv(void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT source, VT_MPI_INT tag, MPI_Comm comm, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Recv_init(void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT source, VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Reduce(CONST void* sendbuf_CLASS_BUFFER_IN_PLACE, void* recvbuf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Op op, VT_MPI_INT root, MPI_Comm comm); /*COLL_ALL2ONE*/
VT_MPI_INT MPI_Reduce_scatter(CONST void* sendbuf_CLASS_BUFFER_IN_PLACE, void* recvbuf_CLASS_BUFFER, CONST VT_MPI_INT* recvcounts, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm); /*COLL_ALL2ALL*/
VT_MPI_INT MPI_Request_free(MPI_Request* request_CLASS_SINGLE_IO);
VT_MPI_INT MPI_Rsend(CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm);
VT_MPI_INT MPI_Rsend_init(CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Scan(CONST void* sendbuf_CLASS_BUFFER_IN_PLACE, void* recvbuf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm); /*COLL_ALL2ALL*/
VT_MPI_INT MPI_Scatter(CONST void* sendbuf_CLASS_BUFFER, VT_MPI_INT sendcount, MPI_Datatype sendtype, void* recvbuf_CLASS_BUFFER_IN_PLACE, VT_MPI_INT recvcount, MPI_Datatype recvtype, VT_MPI_INT root, MPI_Comm comm); /*COLL_ONE2ALL*/
VT_MPI_INT MPI_Scatterv(CONST void* sendbuf_CLASS_BUFFER, CONST VT_MPI_INT* sendcounts, CONST VT_MPI_INT* displs, MPI_Datatype sendtype, void* recvbuf_CLASS_BUFFER_IN_PLACE, VT_MPI_INT recvcount, MPI_Datatype recvtype, VT_MPI_INT root, MPI_Comm comm); /*COLL_ONE2ALL*/
VT_MPI_INT MPI_Send(CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm);
VT_MPI_INT MPI_Send_init(CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Sendrecv(CONST void* sendbuf_CLASS_BUFFER, VT_MPI_INT sendcount, MPI_Datatype sendtype, VT_MPI_INT dest, VT_MPI_INT sendtag, void* recvbuf_CLASS_BUFFER, VT_MPI_INT recvcount, MPI_Datatype recvtype, VT_MPI_INT source, VT_MPI_INT recvtag, MPI_Comm comm, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Sendrecv_replace(void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT dest, VT_MPI_INT sendtag, VT_MPI_INT source, VT_MPI_INT recvtag, MPI_Comm comm, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Ssend(CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm);
VT_MPI_INT MPI_Ssend_init(CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, VT_MPI_INT dest, VT_MPI_INT tag, MPI_Comm comm, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Start(MPI_Request* request_CLASS_SINGLE_IO);
VT_MPI_INT MPI_Startall(VT_MPI_INT count, MPI_Request* array_of_requests_CLASS_ARRAY_IO_count);
VT_MPI_INT MPI_Test(MPI_Request* request_CLASS_SINGLE_IO, VT_MPI_INT* flag, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Test_cancelled(CONST MPI_Status* status_CLASS_SINGLE_IN, VT_MPI_INT* flag);
VT_MPI_INT MPI_Testall(VT_MPI_INT count, MPI_Request* array_of_requests_CLASS_ARRAY_IO_count, VT_MPI_INT* flag, MPI_Status* array_of_statuses_CLASS_ARRAY_OUT_count);
VT_MPI_INT MPI_Testany(VT_MPI_INT count, MPI_Request* array_of_requests_CLASS_ARRAY_IO_count, VT_MPI_INT* index_CLASS_ARRAYINDEX_OUT_SINGLE_CONDITION_flag, VT_MPI_INT* flag, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Testsome(VT_MPI_INT incount, MPI_Request* array_of_requests_CLASS_ARRAY_IO_incount, VT_MPI_INT* outcount, VT_MPI_INT* array_of_indices_CLASS_ARRAYINDEX_OUT_ARRAY_outcount, MPI_Status* array_of_statuses_CLASS_ARRAY_OUT_outcount);
VT_MPI_INT MPI_Topo_test(MPI_Comm comm, VT_MPI_INT* status);
VT_MPI_INT MPI_Type_commit(MPI_Datatype* datatype_CLASS_SINGLE_IO);
VT_MPI_INT MPI_Type_contiguous(VT_MPI_INT count, MPI_Datatype oldtype, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_extent(MPI_Datatype datatype, MPI_Aint* extent_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_free(MPI_Datatype* datatype_CLASS_SINGLE_IO);
VT_MPI_INT MPI_Type_hindexed(VT_MPI_INT count, CONST VT_MPI_INT* array_of_blocklengths, CONST MPI_Aint* array_of_displacements_CLASS_ARRAY_IN_count, MPI_Datatype oldtype, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_hvector(VT_MPI_INT count, VT_MPI_INT blocklength, MPI_Aint stride, MPI_Datatype oldtype, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_indexed(VT_MPI_INT count, CONST VT_MPI_INT* array_of_blocklengths, CONST VT_MPI_INT* array_of_displacements, MPI_Datatype oldtype, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_lb(MPI_Datatype datatype, MPI_Aint* displacement_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_size(MPI_Datatype datatype, VT_MPI_INT* size);
VT_MPI_INT MPI_Type_struct(VT_MPI_INT count, CONST VT_MPI_INT* array_of_blocklengths, CONST MPI_Aint* array_of_displacements_CLASS_ARRAY_IN_count, CONST MPI_Datatype* array_of_types_CLASS_ARRAY_IN_count, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_ub(MPI_Datatype datatype, MPI_Aint* displacement_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_vector(VT_MPI_INT count, VT_MPI_INT blocklength, VT_MPI_INT stride, MPI_Datatype oldtype, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Unpack(CONST void* inbuf, VT_MPI_INT insize, VT_MPI_INT* position, void* outbuf_CLASS_BUFFER, VT_MPI_INT outcount, MPI_Datatype datatype, MPI_Comm comm);
VT_MPI_INT MPI_Wait(MPI_Request* request_CLASS_SINGLE_IO, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Waitall(VT_MPI_INT count, MPI_Request* array_of_requests_CLASS_ARRAY_IO_count, MPI_Status* array_of_statuses_CLASS_ARRAY_OUT_count);
VT_MPI_INT MPI_Waitany(VT_MPI_INT count, MPI_Request* array_of_requests_CLASS_ARRAY_IO_count, VT_MPI_INT* index_CLASS_ARRAYINDEX_OUT_SINGLE_NOCONDITION, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Waitsome(VT_MPI_INT incount, MPI_Request* array_of_requests_CLASS_ARRAY_IO_incount, VT_MPI_INT* outcount, VT_MPI_INT* array_of_indices_CLASS_ARRAYINDEX_OUT_ARRAY_outcount, MPI_Status* array_of_statuses_CLASS_ARRAY_OUT_outcount);
double MPI_Wtick(void);
double MPI_Wtime(void);
