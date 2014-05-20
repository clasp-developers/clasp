/**
 * MPI-calls of MPI-2-standard (One-Sided Communications)
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

VT_MPI_INT MPI_Accumulate(CONST void* origin_addr_CLASS_BUFFER, VT_MPI_INT origin_count, MPI_Datatype origin_datatype, VT_MPI_INT target_rank, MPI_Aint target_disp, VT_MPI_INT target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win);
VT_MPI_INT MPI_Get(void* origin_addr_CLASS_BUFFER, VT_MPI_INT origin_count, MPI_Datatype origin_datatype, VT_MPI_INT target_rank, MPI_Aint target_disp, VT_MPI_INT target_count, MPI_Datatype target_datatype, MPI_Win win);
VT_MPI_INT MPI_Put(CONST void* origin_addr_CLASS_BUFFER, VT_MPI_INT origin_count, MPI_Datatype origin_datatype, VT_MPI_INT target_rank, MPI_Aint target_disp, VT_MPI_INT target_count, MPI_Datatype target_datatype, MPI_Win win);
VT_MPI_INT MPI_Win_complete(MPI_Win win);
VT_MPI_INT MPI_Win_create(void* base, MPI_Aint size, VT_MPI_INT disp_unit, MPI_Info info, MPI_Comm comm, MPI_Win* win_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Win_fence(VT_MPI_INT assert, MPI_Win win);
VT_MPI_INT MPI_Win_free(MPI_Win* win_CLASS_SINGLE_IO);
VT_MPI_INT MPI_Win_get_group(MPI_Win win, MPI_Group* group_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Win_lock(VT_MPI_INT lock_type, VT_MPI_INT rank, VT_MPI_INT assert, MPI_Win win);
VT_MPI_INT MPI_Win_post(MPI_Group group, VT_MPI_INT assert, MPI_Win win);
VT_MPI_INT MPI_Win_start(MPI_Group group, VT_MPI_INT assert, MPI_Win win);
VT_MPI_INT MPI_Win_test(MPI_Win win, VT_MPI_INT* flag);
VT_MPI_INT MPI_Win_unlock(VT_MPI_INT rank, MPI_Win win);
VT_MPI_INT MPI_Win_wait(MPI_Win win);
