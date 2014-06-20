/**
 * MPI-calls of MPI-2-standard (Process Creation and Management)
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

VT_MPI_INT MPI_Close_port(CONST char* port_name_CLASS_SINGLE_IN);
VT_MPI_INT MPI_Comm_accept(CONST char* port_name_CLASS_SINGLE_IN, MPI_Info info, VT_MPI_INT root, MPI_Comm comm, MPI_Comm* newcomm_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Comm_connect(CONST char* port_name_CLASS_SINGLE_IN, MPI_Info info, VT_MPI_INT root, MPI_Comm comm, MPI_Comm* newcomm_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Comm_disconnect(MPI_Comm* comm_CLASS_SINGLE_IO);
VT_MPI_INT MPI_Comm_get_parent(MPI_Comm* parent_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Comm_join(VT_MPI_INT fd, MPI_Comm* intercomm_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Comm_spawn(const char* command_CLASS_SINGLE_IN, char** argv, VT_MPI_INT maxprocs, MPI_Info info, VT_MPI_INT root, MPI_Comm comm, MPI_Comm* intercomm_CLASS_SINGLE_OUT, VT_MPI_INT* array_of_errcodes);
VT_MPI_INT MPI_Comm_spawn_multiple(VT_MPI_INT count, char** array_of_commands, char*** array_of_argv, CONST VT_MPI_INT* array_of_maxprocs, CONST MPI_Info* array_of_info_CLASS_ARRAY_IN_count, VT_MPI_INT root, MPI_Comm comm, MPI_Comm* intercomm_CLASS_SINGLE_OUT, VT_MPI_INT* array_of_errcodes);
VT_MPI_INT MPI_Lookup_name(CONST char* service_name_CLASS_SINGLE_IN, MPI_Info info, char* port_name_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Open_port(MPI_Info info, char* port_name_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Publish_name(CONST char* service_name_CLASS_SINGLE_IN, MPI_Info info, CONST char* port_name_CLASS_SINGLE_IN);
VT_MPI_INT MPI_Unpublish_name(CONST char* service_name_CLASS_SINGLE_IN, MPI_Info info, CONST char* port_name_CLASS_SINGLE_IN);
