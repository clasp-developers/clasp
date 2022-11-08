/*! byte-code interpreter included here.

To build the machine use:
(0) Erase the code below
(1) ./waf build_rboehm
(2) (literal::build-c++-machine)
(3) copy the result below
 */

#ifdef DEFINE_PARSERS
void parse_ltvc_make_nil(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_nil\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  ltvc_make_nil( roots, tag, index);
};
void parse_ltvc_make_t(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_t\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  ltvc_make_t( roots, tag, index);
};
void parse_ltvc_make_ratio(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_ratio\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg2 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg3 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_make_ratio( roots, tag, index, arg2, arg3);
};
void parse_ltvc_make_complex(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_complex\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg2 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg3 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_make_complex( roots, tag, index, arg2, arg3);
};
void parse_ltvc_make_cons(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_cons\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  ltvc_make_cons( roots, tag, index);
};
void parse_ltvc_rplaca(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_rplaca\n", __FILE__, __LINE__, __FUNCTION__);
  T_O* arg0 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg1 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_rplaca( roots, arg0, arg1);
};
void parse_ltvc_rplacd(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_rplacd\n", __FILE__, __LINE__, __FUNCTION__);
  T_O* arg0 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg1 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_rplacd( roots, arg0, arg1);
};
void parse_ltvc_make_list(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_list\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  size_t arg2 = ltvc_read_size_t( bytecode, byteend, log );
  ltvc_make_list( roots, tag, index, arg2);
};
void parse_ltvc_fill_list(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_fill_list\n", __FILE__, __LINE__, __FUNCTION__);
  T_O* arg0 = ltvc_read_object(roots,  bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  Cons_O* varargs = ltvc_read_list( roots, index, bytecode, byteend, log );
  ltvc_fill_list_varargs( roots, arg0, index, varargs);
};
void parse_ltvc_make_array(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_array\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg2 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg3 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_make_array( roots, tag, index, arg2, arg3);
};
void parse_ltvc_setf_row_major_aref(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_setf_row_major_aref\n", __FILE__, __LINE__, __FUNCTION__);
  T_O* arg0 = ltvc_read_object(roots,  bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg2 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_setf_row_major_aref( roots, arg0, index, arg2);
};
void parse_ltvc_make_hash_table(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_hash_table\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg2 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_make_hash_table( roots, tag, index, arg2);
};
void parse_ltvc_setf_gethash(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_setf_gethash\n", __FILE__, __LINE__, __FUNCTION__);
  T_O* arg0 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg1 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg2 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_setf_gethash( roots, arg0, arg1, arg2);
};
void parse_ltvc_make_fixnum(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_fixnum\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  uintptr_t arg2 = ltvc_read_size_t( bytecode, byteend, log );
  ltvc_make_fixnum( roots, tag, index, arg2);
};
void parse_ltvc_make_package(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_package\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg2 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_make_package( roots, tag, index, arg2);
};
void parse_ltvc_make_next_bignum(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_next_bignum\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg2 = ltvc_read_bignum( bytecode, byteend, log );
  ltvc_make_next_bignum( roots, tag, index, arg2);
};
void parse_ltvc_make_bitvector(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_bitvector\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg2 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_make_bitvector( roots, tag, index, arg2);
};
void parse_ltvc_make_symbol(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_symbol\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg2 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg3 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_make_symbol( roots, tag, index, arg2, arg3);
};
void parse_ltvc_make_character(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_character\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  uintptr_t arg2 = ltvc_read_size_t( bytecode, byteend, log );
  ltvc_make_character( roots, tag, index, arg2);
};
void parse_ltvc_make_base_string(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_base_string\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  string arg2 = ltvc_read_string( bytecode, byteend, log );
  ltvc_make_base_string( roots, tag, index, arg2.c_str());
};
void parse_ltvc_make_pathname(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_pathname\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg2 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg3 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg4 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg5 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg6 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg7 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_make_pathname( roots, tag, index, arg2, arg3, arg4, arg5, arg6, arg7);
};
void parse_ltvc_make_function_description(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_function_description\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg2 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg3 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg4 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg5 = ltvc_read_object(roots,  bytecode, byteend, log );
  T_O* arg6 = ltvc_read_object(roots,  bytecode, byteend, log );
  size_t arg7 = ltvc_read_size_t( bytecode, byteend, log );
  size_t arg8 = ltvc_read_size_t( bytecode, byteend, log );
  size_t arg9 = ltvc_read_size_t( bytecode, byteend, log );
  ltvc_make_function_description( roots, tag, index, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
};
void parse_ltvc_make_global_entry_point(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_global_entry_point\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  size_t arg2 = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg3 = ltvc_read_object(roots,  bytecode, byteend, log );
  size_t arg4 = ltvc_read_size_t( bytecode, byteend, log );
  ltvc_make_global_entry_point( roots, tag, index, arg2, arg3, arg4);
};
void parse_ltvc_make_local_entry_point(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_local_entry_point\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  size_t arg2 = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg3 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_make_local_entry_point( roots, tag, index, arg2, arg3);
};
void parse_ltvc_make_random_state(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_random_state\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg2 = ltvc_read_object(roots,  bytecode, byteend, log );
  ltvc_make_random_state( roots, tag, index, arg2);
};
void parse_ltvc_make_float(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_float\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  float arg2 = ltvc_read_float( bytecode, byteend, log );
  ltvc_make_float( roots, tag, index, arg2);
};
void parse_ltvc_make_double(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_double\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  double arg2 = ltvc_read_double( bytecode, byteend, log );
  ltvc_make_double( roots, tag, index, arg2);
};
void parse_ltvc_make_closurette(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_make_closurette\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  size_t arg2 = ltvc_read_size_t( bytecode, byteend, log );
  ltvc_make_closurette( roots, tag, index, arg2);
};
void parse_ltvc_set_mlf_creator_funcall(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_set_mlf_creator_funcall\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  size_t arg2 = ltvc_read_size_t( bytecode, byteend, log );
  string arg3 = ltvc_read_string( bytecode, byteend, log );
  ltvc_set_mlf_creator_funcall( roots, tag, index, arg2, arg3.c_str());
};
void parse_ltvc_mlf_init_funcall(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_mlf_init_funcall\n", __FILE__, __LINE__, __FUNCTION__);
  size_t arg0 = ltvc_read_size_t( bytecode, byteend, log );
  string arg1 = ltvc_read_string( bytecode, byteend, log );
  ltvc_mlf_init_funcall( roots, arg0, arg1.c_str());
};
void parse_ltvc_mlf_init_basic_call(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_mlf_init_basic_call\n", __FILE__, __LINE__, __FUNCTION__);
  T_O* arg0 = ltvc_read_object(roots,  bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  Cons_O* varargs = ltvc_read_list( roots, index, bytecode, byteend, log );
  ltvc_mlf_init_basic_call_varargs( roots, arg0, index, varargs);
};
void parse_ltvc_mlf_create_basic_call(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_mlf_create_basic_call\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  T_O* arg2 = ltvc_read_object(roots,  bytecode, byteend, log );
  size_t arg3 = ltvc_read_size_t( bytecode, byteend, log );
  Cons_O* varargs = ltvc_read_list( roots, arg3, bytecode, byteend, log );
  ltvc_mlf_create_basic_call_varargs( roots, tag, index, arg2, arg3, varargs);
};
void parse_ltvc_set_ltv_funcall(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_set_ltv_funcall\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( bytecode, byteend, log );
  size_t index = ltvc_read_size_t( bytecode, byteend, log );
  size_t arg2 = ltvc_read_size_t( bytecode, byteend, log );
  string arg3 = ltvc_read_string( bytecode, byteend, log );
  ltvc_set_ltv_funcall( roots, tag, index, arg2, arg3.c_str());
};
void parse_ltvc_toplevel_funcall(gctools::GCRootsInModule* roots, char*& bytecode, char* byteend, bool log) {
  if (log) printf("%s:%d:%s parse_ltvc_toplevel_funcall\n", __FILE__, __LINE__, __FUNCTION__);
  size_t arg0 = ltvc_read_size_t( bytecode, byteend, log );
  string arg1 = ltvc_read_string( bytecode, byteend, log );
  ltvc_toplevel_funcall( roots, arg0, arg1.c_str());
};
#endif // DEFINE_PARSERS
#ifdef DEFINE_SWITCH
  case 65: parse_ltvc_make_nil(roots,bytecode,byteend,log);
           break;
  case 66: parse_ltvc_make_t(roots,bytecode,byteend,log);
           break;
  case 67: parse_ltvc_make_ratio(roots,bytecode,byteend,log);
           break;
  case 68: parse_ltvc_make_complex(roots,bytecode,byteend,log);
           break;
  case 69: parse_ltvc_make_cons(roots,bytecode,byteend,log);
           break;
  case 70: parse_ltvc_rplaca(roots,bytecode,byteend,log);
           break;
  case 71: parse_ltvc_rplacd(roots,bytecode,byteend,log);
           break;
  case 72: parse_ltvc_make_list(roots,bytecode,byteend,log);
           break;
  case 73: parse_ltvc_fill_list(roots,bytecode,byteend,log);
           break;
  case 74: parse_ltvc_make_array(roots,bytecode,byteend,log);
           break;
  case 75: parse_ltvc_setf_row_major_aref(roots,bytecode,byteend,log);
           break;
  case 76: parse_ltvc_make_hash_table(roots,bytecode,byteend,log);
           break;
  case 77: parse_ltvc_setf_gethash(roots,bytecode,byteend,log);
           break;
  case 78: parse_ltvc_make_fixnum(roots,bytecode,byteend,log);
           break;
  case 79: parse_ltvc_make_package(roots,bytecode,byteend,log);
           break;
  case 80: parse_ltvc_make_next_bignum(roots,bytecode,byteend,log);
           break;
  case 81: parse_ltvc_make_bitvector(roots,bytecode,byteend,log);
           break;
  case 82: parse_ltvc_make_symbol(roots,bytecode,byteend,log);
           break;
  case 83: parse_ltvc_make_character(roots,bytecode,byteend,log);
           break;
  case 84: parse_ltvc_make_base_string(roots,bytecode,byteend,log);
           break;
  case 85: parse_ltvc_make_pathname(roots,bytecode,byteend,log);
           break;
  case 86: parse_ltvc_make_function_description(roots,bytecode,byteend,log);
           break;
  case 87: parse_ltvc_make_global_entry_point(roots,bytecode,byteend,log);
           break;
  case 88: parse_ltvc_make_local_entry_point(roots,bytecode,byteend,log);
           break;
  case 89: parse_ltvc_make_random_state(roots,bytecode,byteend,log);
           break;
  case 90: parse_ltvc_make_float(roots,bytecode,byteend,log);
           break;
  case 91: parse_ltvc_make_double(roots,bytecode,byteend,log);
           break;
  case 92: parse_ltvc_make_closurette(roots,bytecode,byteend,log);
           break;
  case 93: parse_ltvc_set_mlf_creator_funcall(roots,bytecode,byteend,log);
           break;
  case 94: parse_ltvc_mlf_init_funcall(roots,bytecode,byteend,log);
           break;
  case 95: parse_ltvc_mlf_init_basic_call(roots,bytecode,byteend,log);
           break;
  case 96: parse_ltvc_mlf_create_basic_call(roots,bytecode,byteend,log);
           break;
  case 97: parse_ltvc_set_ltv_funcall(roots,bytecode,byteend,log);
           break;
  case 98: parse_ltvc_toplevel_funcall(roots,bytecode,byteend,log);
           break;
#endif // DEFINE_SWITCH
