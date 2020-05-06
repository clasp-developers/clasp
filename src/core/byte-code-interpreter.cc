/*! byte-code interpreter included here.

To build the machine use:
(0) change the #if 1 below to #if 0
(1) ./waf build_rboehm
(2) (literal::build-c++-machine)
(3) copy the result below

 */
#if 1

#ifdef DEFINE_PARSERS
void parse_ltvc_make_nil(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_nil\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  ltvc_make_nil( roots, tag, index);
};
void parse_ltvc_make_t(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_t\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  ltvc_make_t( roots, tag, index);
};
void parse_ltvc_make_ratio(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_ratio\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  T_O* arg3 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_make_ratio( roots, tag, index, arg2, arg3);
};
void parse_ltvc_make_complex(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_complex\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  T_O* arg3 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_make_complex( roots, tag, index, arg2, arg3);
};
void parse_ltvc_make_cons(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_cons\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  ltvc_make_cons( roots, tag, index);
};
void parse_ltvc_rplaca(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_rplaca\n", __FILE__, __LINE__, __FUNCTION__);
  T_O* arg0 = ltvc_read_object(roots,  fin, log, byte_index );
  T_O* arg1 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_rplaca( roots, arg0, arg1);
};
void parse_ltvc_rplacd(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_rplacd\n", __FILE__, __LINE__, __FUNCTION__);
  T_O* arg0 = ltvc_read_object(roots,  fin, log, byte_index );
  T_O* arg1 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_rplacd( roots, arg0, arg1);
};
void parse_ltvc_make_list(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_list\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  size_t arg2 = ltvc_read_size_t( fin, log, byte_index );
  ltvc_make_list( roots, tag, index, arg2);
};
void parse_ltvc_fill_list(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_fill_list\n", __FILE__, __LINE__, __FUNCTION__);
  T_O* arg0 = ltvc_read_object(roots,  fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  Cons_O* varargs = ltvc_read_list( roots, index, fin, log, byte_index );
  ltvc_fill_list_varargs( roots, arg0, index, varargs);
};
void parse_ltvc_make_array(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_array\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  T_O* arg3 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_make_array( roots, tag, index, arg2, arg3);
};
void parse_ltvc_setf_row_major_aref(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_setf_row_major_aref\n", __FILE__, __LINE__, __FUNCTION__);
  T_O* arg0 = ltvc_read_object(roots,  fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_setf_row_major_aref( roots, arg0, index, arg2);
};
void parse_ltvc_make_hash_table(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_hash_table\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_make_hash_table( roots, tag, index, arg2);
};
void parse_ltvc_setf_gethash(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_setf_gethash\n", __FILE__, __LINE__, __FUNCTION__);
  T_O* arg0 = ltvc_read_object(roots,  fin, log, byte_index );
  T_O* arg1 = ltvc_read_object(roots,  fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_setf_gethash( roots, arg0, arg1, arg2);
};
void parse_ltvc_make_fixnum(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_fixnum\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  uintptr_t arg2 = ltvc_read_size_t( fin, log, byte_index );
  ltvc_make_fixnum( roots, tag, index, arg2);
};
void parse_ltvc_make_package(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_package\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_make_package( roots, tag, index, arg2);
};
void parse_ltvc_make_bignum(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_bignum\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_make_bignum( roots, tag, index, arg2);
};
void parse_ltvc_make_bitvector(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_bitvector\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_make_bitvector( roots, tag, index, arg2);
};
void parse_ltvc_make_symbol(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_symbol\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  T_O* arg3 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_make_symbol( roots, tag, index, arg2, arg3);
};
void parse_ltvc_make_character(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_character\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  uintptr_t arg2 = ltvc_read_size_t( fin, log, byte_index );
  ltvc_make_character( roots, tag, index, arg2);
};
void parse_ltvc_make_base_string(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_base_string\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  string arg2 = ltvc_read_string( fin, log, byte_index );
  ltvc_make_base_string( roots, tag, index, arg2.c_str());
};
void parse_ltvc_make_pathname(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_pathname\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  T_O* arg3 = ltvc_read_object(roots,  fin, log, byte_index );
  T_O* arg4 = ltvc_read_object(roots,  fin, log, byte_index );
  T_O* arg5 = ltvc_read_object(roots,  fin, log, byte_index );
  T_O* arg6 = ltvc_read_object(roots,  fin, log, byte_index );
  T_O* arg7 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_make_pathname( roots, tag, index, arg2, arg3, arg4, arg5, arg6, arg7);
};
void parse_ltvc_make_random_state(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_random_state\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  ltvc_make_random_state( roots, tag, index, arg2);
};
void parse_ltvc_make_float(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_float\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  float arg2 = ltvc_read_float( fin, log, byte_index );
  ltvc_make_float( roots, tag, index, arg2);
};
void parse_ltvc_make_double(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_make_double\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  double arg2 = ltvc_read_double( fin, log, byte_index );
  ltvc_make_double( roots, tag, index, arg2);
};
void parse_ltvc_enclose(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_enclose\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  size_t arg3 = ltvc_read_size_t( fin, log, byte_index );
  ltvc_enclose( roots, tag, index, arg2, arg3);
};
void parse_ltvc_set_mlf_creator_funcall(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_set_mlf_creator_funcall\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  size_t arg2 = ltvc_read_size_t( fin, log, byte_index );
  string arg3 = ltvc_read_string( fin, log, byte_index );
  ltvc_set_mlf_creator_funcall( roots, tag, index, arg2, arg3.c_str());
};
void parse_ltvc_mlf_init_funcall(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_mlf_init_funcall\n", __FILE__, __LINE__, __FUNCTION__);
  size_t arg0 = ltvc_read_size_t( fin, log, byte_index );
  string arg1 = ltvc_read_string( fin, log, byte_index );
  ltvc_mlf_init_funcall( roots, arg0, arg1.c_str());
};
void parse_ltvc_mlf_init_basic_call(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_mlf_init_basic_call\n", __FILE__, __LINE__, __FUNCTION__);
  T_O* arg0 = ltvc_read_object(roots,  fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  Cons_O* varargs = ltvc_read_list( roots, index, fin, log, byte_index );
  ltvc_mlf_init_basic_call_varargs( roots, arg0, index, varargs);
};
void parse_ltvc_mlf_create_basic_call(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_mlf_create_basic_call\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  T_O* arg2 = ltvc_read_object(roots,  fin, log, byte_index );
  size_t arg3 = ltvc_read_size_t( fin, log, byte_index );
  Cons_O* varargs = ltvc_read_list( roots, arg3, fin, log, byte_index );
  ltvc_mlf_create_basic_call_varargs( roots, tag, index, arg2, arg3, varargs);
};
void parse_ltvc_set_ltv_funcall(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_set_ltv_funcall\n", __FILE__, __LINE__, __FUNCTION__);
  char tag = ltvc_read_char( fin, log, byte_index );
  size_t index = ltvc_read_size_t( fin, log, byte_index );
  size_t arg2 = ltvc_read_size_t( fin, log, byte_index );
  string arg3 = ltvc_read_string( fin, log, byte_index );
  ltvc_set_ltv_funcall( roots, tag, index, arg2, arg3.c_str());
};
void parse_ltvc_toplevel_funcall(gctools::GCRootsInModule* roots, T_sp fin, bool log, size_t& byte_index) {
  if (log) printf("%s:%d:%s parse_ltvc_toplevel_funcall\n", __FILE__, __LINE__, __FUNCTION__);
  size_t arg0 = ltvc_read_size_t( fin, log, byte_index );
  string arg1 = ltvc_read_string( fin, log, byte_index );
  ltvc_toplevel_funcall( roots, arg0, arg1.c_str());
};
#endif // DEFINE_PARSERS
#ifdef DEFINE_SWITCH
  case 65: parse_ltvc_make_nil(roots,fin,log,byte_index);
           break;
  case 66: parse_ltvc_make_t(roots,fin,log,byte_index);
           break;
  case 67: parse_ltvc_make_ratio(roots,fin,log,byte_index);
           break;
  case 68: parse_ltvc_make_complex(roots,fin,log,byte_index);
           break;
  case 69: parse_ltvc_make_cons(roots,fin,log,byte_index);
           break;
  case 70: parse_ltvc_rplaca(roots,fin,log,byte_index);
           break;
  case 71: parse_ltvc_rplacd(roots,fin,log,byte_index);
           break;
  case 72: parse_ltvc_make_list(roots,fin,log,byte_index);
           break;
  case 73: parse_ltvc_fill_list(roots,fin,log,byte_index);
           break;
  case 74: parse_ltvc_make_array(roots,fin,log,byte_index);
           break;
  case 75: parse_ltvc_setf_row_major_aref(roots,fin,log,byte_index);
           break;
  case 76: parse_ltvc_make_hash_table(roots,fin,log,byte_index);
           break;
  case 77: parse_ltvc_setf_gethash(roots,fin,log,byte_index);
           break;
  case 78: parse_ltvc_make_fixnum(roots,fin,log,byte_index);
           break;
  case 79: parse_ltvc_make_package(roots,fin,log,byte_index);
           break;
  case 80: parse_ltvc_make_bignum(roots,fin,log,byte_index);
           break;
  case 81: parse_ltvc_make_bitvector(roots,fin,log,byte_index);
           break;
  case 82: parse_ltvc_make_symbol(roots,fin,log,byte_index);
           break;
  case 83: parse_ltvc_make_character(roots,fin,log,byte_index);
           break;
  case 84: parse_ltvc_make_base_string(roots,fin,log,byte_index);
           break;
  case 85: parse_ltvc_make_pathname(roots,fin,log,byte_index);
           break;
  case 86: parse_ltvc_make_random_state(roots,fin,log,byte_index);
           break;
  case 87: parse_ltvc_make_float(roots,fin,log,byte_index);
           break;
  case 88: parse_ltvc_make_double(roots,fin,log,byte_index);
           break;
  case 89: parse_ltvc_enclose(roots,fin,log,byte_index);
           break;
  case 90: parse_ltvc_set_mlf_creator_funcall(roots,fin,log,byte_index);
           break;
  case 91: parse_ltvc_mlf_init_funcall(roots,fin,log,byte_index);
           break;
  case 92: parse_ltvc_mlf_init_basic_call(roots,fin,log,byte_index);
           break;
  case 93: parse_ltvc_mlf_create_basic_call(roots,fin,log,byte_index);
           break;
  case 94: parse_ltvc_set_ltv_funcall(roots,fin,log,byte_index);
           break;
  case 95: parse_ltvc_toplevel_funcall(roots,fin,log,byte_index);
           break;
#endif // DEFINE_SWITCH

#endif
