/**
 * MPI-calls of MPI-2-standard (Miscellany and External Interfaces)
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

/* Miscellany */
VT_MPI_INT MPI_Alloc_mem(MPI_Aint size, MPI_Info info, void* baseptr);
VT_MPI_INT MPI_Comm_create_errhandler(MPI_Comm_errhandler_fn* function, MPI_Errhandler* errhandler_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Comm_get_errhandler(MPI_Comm comm, MPI_Errhandler* errhandler_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler);
VT_MPI_INT MPI_File_call_errhandler(MPI_File fh, VT_MPI_INT errorcode);
VT_MPI_INT MPI_File_create_errhandler(MPI_File_errhandler_fn* function_CLASS_SINGLE_IN, MPI_Errhandler* errhandler_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_get_errhandler(MPI_File file, MPI_Errhandler* errhandler_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_set_errhandler(MPI_File file, MPI_Errhandler errhandler);
VT_MPI_INT MPI_Finalized(VT_MPI_INT* flag);
VT_MPI_INT MPI_Free_mem(void* base);
VT_MPI_INT MPI_Get_address(CONST void* location, MPI_Aint* address_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Info_create(MPI_Info* info_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Info_delete(MPI_Info info, CONST char* key_CLASS_SINGLE_IN);
VT_MPI_INT MPI_Info_dup(MPI_Info info, MPI_Info* newinfo_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Info_free(MPI_Info* info_CLASS_SINGLE_IO);
VT_MPI_INT MPI_Info_get(MPI_Info info, CONST char* key_CLASS_SINGLE_IN, VT_MPI_INT valuelen, char* value_CLASS_SINGLE_OUT, VT_MPI_INT* flag);
VT_MPI_INT MPI_Info_get_nkeys(MPI_Info info, VT_MPI_INT* nkeys);
VT_MPI_INT MPI_Info_get_nthkey(MPI_Info info, VT_MPI_INT n, char* key_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Info_get_valuelen(MPI_Info info, CONST char* key_CLASS_SINGLE_IN, VT_MPI_INT* valuelen, VT_MPI_INT* flag);
VT_MPI_INT MPI_Info_set(MPI_Info info, CONST char* key_CLASS_SINGLE_IN, CONST char* value_CLASS_SINGLE_IN);
VT_MPI_INT MPI_Pack_external(CONST char* datarep_CLASS_SINGLE_IN, CONST void* inbuf, VT_MPI_INT incount, MPI_Datatype datatype, void* outbuf, MPI_Aint outsize, MPI_Aint* position_CLASS_SINGLE_IO);
VT_MPI_INT MPI_Pack_external_size(CONST char* datarep_CLASS_SINGLE_IN, VT_MPI_INT incount, MPI_Datatype datatype, MPI_Aint* size_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Request_get_status(MPI_Request request, VT_MPI_INT* flag, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_create_darray(VT_MPI_INT size, VT_MPI_INT rank, VT_MPI_INT ndims, CONST VT_MPI_INT* array_of_gsizes, CONST VT_MPI_INT* array_of_distribs, CONST VT_MPI_INT* array_of_dargs, CONST VT_MPI_INT* array_of_psizes, VT_MPI_INT order, MPI_Datatype oldtype, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_create_hindexed(VT_MPI_INT count, CONST VT_MPI_INT* array_of_blocklengths, CONST MPI_Aint* array_of_displacements_CLASS_ARRAY_IN_count, MPI_Datatype oldtype, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_create_hvector(VT_MPI_INT count, VT_MPI_INT blocklength, MPI_Aint stride, MPI_Datatype oldtype, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_create_indexed_block(VT_MPI_INT count, VT_MPI_INT blocklength, CONST VT_MPI_INT* array_of_displacements, MPI_Datatype oldtype, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_create_resized(MPI_Datatype oldtype, MPI_Aint lb, MPI_Aint extent, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_create_struct(VT_MPI_INT count, CONST VT_MPI_INT* array_of_blocklengths, CONST MPI_Aint* array_of_displacements_CLASS_ARRAY_IN_count, CONST MPI_Datatype* array_of_types_CLASS_ARRAY_IN_count, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_create_subarray(VT_MPI_INT ndims, CONST VT_MPI_INT* array_of_sizes, CONST VT_MPI_INT* array_of_subsizes, CONST VT_MPI_INT* array_of_starts, VT_MPI_INT order, MPI_Datatype oldtype, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_get_extent(MPI_Datatype datatype, MPI_Aint* lb_CLASS_SINGLE_OUT, MPI_Aint* extent_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_get_true_extent(MPI_Datatype datatype, MPI_Aint* true_lb_CLASS_SINGLE_OUT, MPI_Aint* true_extent_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Unpack_external(CONST char* datarep_CLASS_SINGLE_IN, CONST void* inbuf, MPI_Aint insize, MPI_Aint* position_CLASS_SINGLE_IO, void* outbuf, VT_MPI_INT outcount, MPI_Datatype datatype);
VT_MPI_INT MPI_Win_create_errhandler(MPI_Win_errhandler_fn* function, MPI_Errhandler* errhandler_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Win_get_errhandler(MPI_Win win, MPI_Errhandler* errhandler_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Win_set_errhandler(MPI_Win win, MPI_Errhandler errhandler);

/* External Interfaces */
VT_MPI_INT MPI_Add_error_class(VT_MPI_INT* errorclass);
VT_MPI_INT MPI_Add_error_code(VT_MPI_INT errorclass, VT_MPI_INT* errorcode);
VT_MPI_INT MPI_Add_error_string(VT_MPI_INT errorcode, CONST char* string_CLASS_SINGLE_IN);
VT_MPI_INT MPI_Comm_call_errhandler(MPI_Comm comm, VT_MPI_INT errorcode);
VT_MPI_INT MPI_Comm_create_keyval(MPI_Comm_copy_attr_function* comm_copy_attr_fn, MPI_Comm_delete_attr_function* comm_delete_attr_fn, VT_MPI_INT* comm_keyval, void* extra_state);
VT_MPI_INT MPI_Comm_delete_attr(MPI_Comm comm, VT_MPI_INT comm_keyval);
VT_MPI_INT MPI_Comm_free_keyval(VT_MPI_INT* comm_keyval);
VT_MPI_INT MPI_Comm_get_attr(MPI_Comm comm, VT_MPI_INT comm_keyval, void* attribute_val, VT_MPI_INT* flag);
VT_MPI_INT MPI_Comm_get_name(MPI_Comm comm, char* comm_name_CLASS_SINGLE_OUT, VT_MPI_INT* resultlen);
VT_MPI_INT MPI_Comm_set_attr(MPI_Comm comm, VT_MPI_INT comm_keyval, void* attribute_val);
VT_MPI_INT MPI_Comm_set_name(MPI_Comm comm, CONST char* comm_name_CLASS_SINGLE_IN);
VT_MPI_INT MPI_Grequest_complete(MPI_Request request);
VT_MPI_INT MPI_Grequest_start(MPI_Grequest_query_function* query_fn, MPI_Grequest_free_function* free_fn, MPI_Grequest_cancel_function* cancel_fn, void* extra_state, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Status_set_cancelled(MPI_Status* status_CLASS_SINGLE_IO, VT_MPI_INT flag);
VT_MPI_INT MPI_Status_set_elements(MPI_Status* status_CLASS_SINGLE_IO, MPI_Datatype datatype, VT_MPI_INT count);
VT_MPI_INT MPI_Type_create_keyval(MPI_Type_copy_attr_function* type_copy_attr_fn, MPI_Type_delete_attr_function* type_delete_attr_fn, VT_MPI_INT* type_keyval, void* extra_state);
VT_MPI_INT MPI_Type_delete_attr(MPI_Datatype type, VT_MPI_INT type_keyval);
VT_MPI_INT MPI_Type_dup(MPI_Datatype type, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_free_keyval(VT_MPI_INT* type_keyval);
VT_MPI_INT MPI_Type_get_attr(MPI_Datatype type, VT_MPI_INT type_keyval, void* attribute_val, VT_MPI_INT* flag);
VT_MPI_INT MPI_Type_get_contents(MPI_Datatype datatype, VT_MPI_INT max_integers, VT_MPI_INT max_addresses, VT_MPI_INT max_datatypes, VT_MPI_INT* array_of_integers, MPI_Aint* array_of_addresses_CLASS_ARRAY_OUT_max_addresses, MPI_Datatype* array_of_datatypes_CLASS_ARRAY_OUT_max_datatypes);
VT_MPI_INT MPI_Type_get_envelope(MPI_Datatype datatype, VT_MPI_INT* num_integers, VT_MPI_INT* num_addresses, VT_MPI_INT* num_datatypes, VT_MPI_INT* combiner);
VT_MPI_INT MPI_Type_get_name(MPI_Datatype type, char* type_name_CLASS_SINGLE_OUT, VT_MPI_INT* resultlen);
VT_MPI_INT MPI_Type_set_attr(MPI_Datatype type, VT_MPI_INT type_keyval, void* attribute_val);
VT_MPI_INT MPI_Type_set_name(MPI_Datatype type, CONST char* type_name_CLASS_SINGLE_IN);
VT_MPI_INT MPI_Win_call_errhandler(MPI_Win win, VT_MPI_INT errorcode);
VT_MPI_INT MPI_Win_create_keyval(MPI_Win_copy_attr_function* win_copy_attr_fn, MPI_Win_delete_attr_function* win_delete_attr_fn, VT_MPI_INT* win_keyval, void* extra_state);
VT_MPI_INT MPI_Win_delete_attr(MPI_Win win, VT_MPI_INT win_keyval);
VT_MPI_INT MPI_Win_free_keyval(VT_MPI_INT* win_keyval);
VT_MPI_INT MPI_Win_get_attr(MPI_Win win, VT_MPI_INT win_keyval, void* attribute_val, VT_MPI_INT* flag);
VT_MPI_INT MPI_Win_get_name(MPI_Win win, char* win_name_CLASS_SINGLE_OUT, VT_MPI_INT* resultlen);
VT_MPI_INT MPI_Win_set_attr(MPI_Win win, VT_MPI_INT win_keyval, void* attribute_val);
VT_MPI_INT MPI_Win_set_name(MPI_Win win, CONST char* win_name_CLASS_SINGLE_IN);

/* Language Bindings */
VT_MPI_INT MPI_Type_create_f90_complex(VT_MPI_INT p, VT_MPI_INT r, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_create_f90_integer(VT_MPI_INT r, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_create_f90_real(VT_MPI_INT p, VT_MPI_INT r, MPI_Datatype* newtype_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_Type_match_size(VT_MPI_INT typeclass, VT_MPI_INT size, MPI_Datatype* type_CLASS_SINGLE_OUT);
