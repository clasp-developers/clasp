/**
 * MPI-calls of MPI-2-standard (I/O)
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

VT_MPI_INT MPI_File_open(MPI_Comm comm, CONST char* filename_CLASS_SINGLE_IN, VT_MPI_INT amode, MPI_Info info, MPI_File* fh_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_close(MPI_File* fh_CLASS_SINGLE_IO);
VT_MPI_INT MPI_File_delete(CONST char* filename_CLASS_SINGLE_IN, MPI_Info info);
VT_MPI_INT MPI_File_set_size(MPI_File fh, MPI_Offset size);
VT_MPI_INT MPI_File_preallocate(MPI_File fh, MPI_Offset size);
VT_MPI_INT MPI_File_get_size(MPI_File fh, MPI_Offset* size_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_get_group(MPI_File fh, MPI_Group* group_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_get_amode(MPI_File fh, VT_MPI_INT* amode);
VT_MPI_INT MPI_File_set_info(MPI_File fh, MPI_Info info);
VT_MPI_INT MPI_File_get_info(MPI_File fh, MPI_Info* info_used_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_set_view(MPI_File fh, MPI_Offset disp, MPI_Datatype etype, MPI_Datatype filetype, CONST char* datarep_CLASS_SINGLE_IN, MPI_Info info);
VT_MPI_INT MPI_File_get_view(MPI_File fh, MPI_Offset* disp_CLASS_SINGLE_OUT, MPI_Datatype* etype_CLASS_SINGLE_OUT, MPI_Datatype* filetype_CLASS_SINGLE_OUT, char* datarep_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_read_at(MPI_File fh, MPI_Offset offset, void* buf, VT_MPI_INT count, MPI_Datatype datatype, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_read_at_all(MPI_File fh, MPI_Offset offset, void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_write_at(MPI_File fh, MPI_Offset offset, CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_write_at_all(MPI_File fh, MPI_Offset offset, CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_iread_at(MPI_File fh, MPI_Offset offset, void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_iwrite_at(MPI_File fh, MPI_Offset offset, CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_read(MPI_File fh, void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_read_all(MPI_File fh, void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_write(MPI_File fh, CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_write_all(MPI_File fh, CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_iread(MPI_File fh, void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_iwrite(MPI_File fh, CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_seek(MPI_File fh, MPI_Offset offset, VT_MPI_INT whence);
VT_MPI_INT MPI_File_get_position(MPI_File fh, MPI_Offset* offset_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_get_byte_offset(MPI_File fh, MPI_Offset offset, MPI_Offset* disp_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_read_shared(MPI_File fh, void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_write_shared(MPI_File fh, CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_iread_shared(MPI_File fh, void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_iwrite_shared(MPI_File fh, CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Request* request_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_read_ordered(MPI_File fh, void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_write_ordered(MPI_File fh, CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_seek_shared(MPI_File fh, MPI_Offset offset, VT_MPI_INT whence);
VT_MPI_INT MPI_File_get_position_shared(MPI_File fh, MPI_Offset* offset_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_read_at_all_begin(MPI_File fh, MPI_Offset offset, void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype);
VT_MPI_INT MPI_File_read_at_all_end(MPI_File fh, void* buf, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_write_at_all_begin(MPI_File fh, MPI_Offset offset, CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype);
VT_MPI_INT MPI_File_write_at_all_end(MPI_File fh, CONST void* buf, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_read_all_begin(MPI_File fh, void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype);
VT_MPI_INT MPI_File_read_all_end(MPI_File fh, void* buf, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_write_all_begin(MPI_File fh, CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype);
VT_MPI_INT MPI_File_write_all_end(MPI_File fh, CONST void* buf, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_read_ordered_begin(MPI_File fh, void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype);
VT_MPI_INT MPI_File_read_ordered_end(MPI_File fh, void* buf, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_write_ordered_begin(MPI_File fh, CONST void* buf_CLASS_BUFFER, VT_MPI_INT count, MPI_Datatype datatype);
VT_MPI_INT MPI_File_write_ordered_end(MPI_File fh, CONST void* buf, MPI_Status* status_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_get_type_extent(MPI_File fh, MPI_Datatype datatype, MPI_Aint* extent_CLASS_SINGLE_OUT);
VT_MPI_INT MPI_File_set_atomicity(MPI_File fh, VT_MPI_INT flag);
VT_MPI_INT MPI_File_get_atomicity(MPI_File fh, VT_MPI_INT* flag);
VT_MPI_INT MPI_File_sync(MPI_File fh);
VT_MPI_INT MPI_Register_datarep(CONST char* datarep_CLASS_SINGLE_IN, MPI_Datarep_conversion_function* read_conversion_fn, MPI_Datarep_conversion_function* write_conversion_fn, MPI_Datarep_extent_function* dtype_file_extent_fn, void* extra_state);
