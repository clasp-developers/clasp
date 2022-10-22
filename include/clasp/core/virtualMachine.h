#ifdef VM_CODES

enum vm_codes {
   vm_ref=0,
   vm_const=1,
   vm_closure=2,
   vm_call=3,
   vm_call_receive_one=4,
   vm_call_receive_fixed=5,
   vm_bind=6,
   vm_set=7,
   vm_make_cell=8,
   vm_cell_ref=9,
   vm_cell_set=10,
   vm_make_closure=11,
   vm_make_uninitialized_closure=12,
   vm_initialize_closure=13,
   vm_return=14,
   vm_bind_required_args=15,
   vm_bind_optional_args=16,
   vm_listify_rest_args=17,
   vm_vaslistify_rest_args=18,
   vm_parse_key_args=19,
   vm_jump_8=20,
   vm_jump_16=21,
   vm_jump_24=22,
   vm_jump_if_8=23,
   vm_jump_if_16=24,
   vm_jump_if_24=25,
   vm_jump_if_supplied_8=26,
   vm_jump_if_supplied_16=27,
   vm_check_arg_count_LE=28,
   vm_check_arg_count_GE=29,
   vm_check_arg_count_EQ=30,
   vm_push_values=31,
   vm_append_values=32,
   vm_pop_values=33,
   vm_mv_call=34,
   vm_mv_call_receive_one=35,
   vm_mv_call_receive_fixed=36,
   vm_save_sp=37,
   vm_restore_sp=38,
   vm_entry=39,
   vm_exit_8=40,
   vm_exit_16=41,
   vm_exit_24=42,
   vm_entry_close=43,
   vm_catch_8=44,
   vm_catch_16=45,
   vm_throw=46,
   vm_catch_close=47,
   vm_special_bind=48,
   vm_symbol_value=49,
   vm_symbol_value_set=50,
   vm_unbind=51,
   vm_progv=52,
   vm_fdefinition=53,
   vm_nil=54,
   vm_eq=55,
   vm_push=56,
   vm_pop=57,
   vm_long=58 };

#endif // VM_CODES
#ifdef PYTHON_OPCODES
R"opcodes(
new_instr( "ref", 0, [ 1], [ 2] )
new_instr( "const", 1, [ 9], [ 10] )
new_instr( "closure", 2, [ 1], [ 2] )
new_instr( "call", 3, [ 1], [ 2] )
new_instr( "call-receive-one", 4, [ 1], [ 2] )
new_instr( "call-receive-fixed", 5, [ 1,  1], [ 2,  2] )
new_instr( "bind", 6, [ 1,  1], [ 2,  2] )
new_instr( "set", 7, [ 1], [ 2] )
new_instr( "make-cell", 8, [], [] )
new_instr( "cell-ref", 9, [], [] )
new_instr( "cell-set", 10, [], [] )
new_instr( "make-closure", 11, [ 9], [ 10] )
new_instr( "make-uninitialized-closure", 12, [ 9], [ 10] )
new_instr( "initialize-closure", 13, [ 1], [ 2] )
new_instr( "return", 14, [], [] )
new_instr( "bind-required-args", 15, [ 1], [ 2] )
new_instr( "bind-optional-args", 16, [ 1,  1], [ 2,  2] )
new_instr( "listify-rest-args", 17, [ 1], [ 2] )
new_instr( "vaslistify-rest-args", 18, [ 1], [] )
new_instr( "parse-key-args", 19, [ 1,  1,  25,  1], [ 2,  2,  26,  2] )
new_instr( "jump-8", 20, [ 17], [] )
new_instr( "jump-16", 21, [ 18], [] )
new_instr( "jump-24", 22, [ 19], [] )
new_instr( "jump-if-8", 23, [ 17], [] )
new_instr( "jump-if-16", 24, [ 18], [] )
new_instr( "jump-if-24", 25, [ 19], [] )
new_instr( "jump-if-supplied-8", 26, [ 1,  17], [] )
new_instr( "jump-if-supplied-16", 27, [ 1,  18], [] )
new_instr( "check-arg-count-LE", 28, [ 1], [ 2] )
new_instr( "check-arg-count-GE", 29, [ 1], [ 2] )
new_instr( "check-arg-count-EQ", 30, [ 1], [ 2] )
new_instr( "push-values", 31, [], [] )
new_instr( "append-values", 32, [], [] )
new_instr( "pop-values", 33, [], [] )
new_instr( "mv-call", 34, [], [] )
new_instr( "mv-call-receive-one", 35, [], [] )
new_instr( "mv-call-receive-fixed", 36, [ 1], [ 2] )
new_instr( "save-sp", 37, [ 1], [] )
new_instr( "restore-sp", 38, [ 1], [] )
new_instr( "entry", 39, [ 1], [] )
new_instr( "exit-8", 40, [ 17], [] )
new_instr( "exit-16", 41, [ 18], [] )
new_instr( "exit-24", 42, [ 19], [] )
new_instr( "entry-close", 43, [], [] )
new_instr( "catch-8", 44, [], [] )
new_instr( "catch-16", 45, [], [] )
new_instr( "throw", 46, [], [] )
new_instr( "catch-close", 47, [], [] )
new_instr( "special-bind", 48, [ 9], [ 10] )
new_instr( "symbol-value", 49, [ 9], [ 10] )
new_instr( "symbol-value-set", 50, [ 9], [ 10] )
new_instr( "unbind", 51, [], [] )
new_instr( "progv", 52, [], [] )
new_instr( "fdefinition", 53, [ 9], [ 10] )
new_instr( "nil", 54, [], [] )
new_instr( "eq", 55, [], [] )
new_instr( "push", 56, [], [] )
new_instr( "pop", 57, [], [] )
new_instr( "long", 58, [], [] )
)opcodes"
#endif
#ifdef GF_BYTECODE_VM
#define DTREE_OP_MISS 0

#define DTREE_OP_ADVANCE 1

#define DTREE_OP_TAG_TEST 2
#define DTREE_FIXNUM_TAG_OFFSET 1
#define DTREE_SINGLE_FLOAT_TAG_OFFSET 2
#define DTREE_CHARACTER_TAG_OFFSET 3
#define DTREE_CONS_TAG_OFFSET 4
#define DTREE_GENERAL_TAG_OFFSET 5

#define DTREE_OP_STAMP_READ 3
#define DTREE_READ_HEADER_OFFSET 1
#define DTREE_READ_OTHER_OFFSET 2

#define DTREE_OP_LT_BRANCH 4
#define DTREE_LT_PIVOT_OFFSET 1
#define DTREE_LT_LEFT_OFFSET 2
#define DTREE_LT_RIGHT_OFFSET 3

#define DTREE_OP_EQ_CHECK 5
#define DTREE_EQ_PIVOT_OFFSET 1
#define DTREE_EQ_NEXT_OFFSET 2

#define DTREE_OP_RANGE_CHECK 6
#define DTREE_RANGE_MIN_OFFSET 1
#define DTREE_RANGE_MAX_OFFSET 2
#define DTREE_RANGE_NEXT_OFFSET 3

#define DTREE_OP_EQL 7
#define DTREE_EQL_OBJECT_OFFSET 1
#define DTREE_EQL_BRANCH_OFFSET 2
#define DTREE_EQL_NEXT_OFFSET 3

#define DTREE_OP_SLOT_READ 8
#define DTREE_SLOT_READER_INDEX_OFFSET 1
#define DTREE_SLOT_READER_SLOT_NAME_OFFSET 2

#define DTREE_OP_SLOT_WRITE 9
#define DTREE_SLOT_WRITER_INDEX_OFFSET 1

#define DTREE_OP_CAR 10
#define DTREE_CAR_READER_INDEX_OFFSET 1
#define DTREE_CAR_READER_CAR_NAME_OFFSET 2

#define DTREE_OP_RPLACA 11
#define DTREE_RPLACA_WRITER_INDEX_OFFSET 1

#define DTREE_OP_EFFECTIVE_METHOD 12
#define DTREE_EFFECTIVE_METHOD_OFFSET 1

#define DTREE_OP_FARG0 13

#define DTREE_OP_FARG1 14

#define DTREE_OP_FARG2 15

#define DTREE_OP_FARG3 16

#define DTREE_OP_FARG4 17

#define DTREE_OP_ARGN 18
#define DTREE_ARGN_OFFSET 1
#define DTREE_ARGN_NEXT_OFFSET 2

#define DTREE_OP_COUNT 19
#endif // GF_BYTECODE_VM
#ifdef GF_BYTECODE_VM_NAMES
  case DTREE_OP_MISS: return "DTREE_OP_MISS";
  case DTREE_OP_ADVANCE: return "DTREE_OP_ADVANCE";
  case DTREE_OP_TAG_TEST: return "DTREE_OP_TAG_TEST";
  case DTREE_OP_STAMP_READ: return "DTREE_OP_STAMP_READ";
  case DTREE_OP_LT_BRANCH: return "DTREE_OP_LT_BRANCH";
  case DTREE_OP_EQ_CHECK: return "DTREE_OP_EQ_CHECK";
  case DTREE_OP_RANGE_CHECK: return "DTREE_OP_RANGE_CHECK";
  case DTREE_OP_EQL: return "DTREE_OP_EQL";
  case DTREE_OP_SLOT_READ: return "DTREE_OP_SLOT_READ";
  case DTREE_OP_SLOT_WRITE: return "DTREE_OP_SLOT_WRITE";
  case DTREE_OP_CAR: return "DTREE_OP_CAR";
  case DTREE_OP_RPLACA: return "DTREE_OP_RPLACA";
  case DTREE_OP_EFFECTIVE_METHOD: return "DTREE_OP_EFFECTIVE_METHOD";
  case DTREE_OP_FARG0: return "DTREE_OP_FARG0";
  case DTREE_OP_FARG1: return "DTREE_OP_FARG1";
  case DTREE_OP_FARG2: return "DTREE_OP_FARG2";
  case DTREE_OP_FARG3: return "DTREE_OP_FARG3";
  case DTREE_OP_FARG4: return "DTREE_OP_FARG4";
  case DTREE_OP_ARGN: return "DTREE_OP_ARGN";
#endif // GF_BYTECODE_VM_NAMES
// This is where I dump the python GF bytecode VM
