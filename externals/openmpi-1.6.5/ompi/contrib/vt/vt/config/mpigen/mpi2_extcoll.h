/**
 * MPI-calls of MPI-2-standard (Extended Collective Operations)
 *
 * @see MPI-2 standard
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

VT_MPI_INT MPI_Alltoallw(CONST void* sendbuf_CLASS_BUFFER, CONST VT_MPI_INT* sendcounts, CONST VT_MPI_INT* sdispls, CONST MPI_Datatype* sendtypes_CLASS_ARRAY_IN_sendcounts, void* recvbuf_CLASS_BUFFER, CONST VT_MPI_INT* recvcounts, CONST VT_MPI_INT* rdispls, CONST MPI_Datatype* recvtypes_CLASS_ARRAY_IN_recvcounts, MPI_Comm comm); /*COLL_ALL2ALL*/
VT_MPI_INT MPI_Exscan(CONST void* sendbuf_CLASS_BUFFER, void* recvbuf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm); /*COLL_ALL2ALL*/
