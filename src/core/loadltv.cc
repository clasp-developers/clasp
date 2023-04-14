#include <array>
#include <vector>
#include <bit>
#include <clasp/core/foundation.h>
#include <clasp/core/ql.h> // ql::list
#include <clasp/core/primitives.h> // cl__fdefinition
#include <clasp/core/designators.h>
#include <clasp/core/bytecode.h> // modules, functions
#include <clasp/core/lispStream.h> // I/O
#include <clasp/core/hashTable.h> // making hash tables
#include <clasp/core/bignum.h> // making bignums
#include <clasp/core/package.h> // making packages
#include <clasp/core/pathname.h> // making pathnames
#include <clasp/core/unixfsys.h> // cl__truename
#include <clasp/llvmo/llvmoPackage.h> // cmp__compile_trampoline

// FIXME: Move these to the generated file thingie
#define LTV_OP_NIL 65
#define LTV_OP_T 66 
#define LTV_OP_CONS 69
#define LTV_OP_RPLACA 70
#define LTV_OP_RPLACD 71
#define LTV_OP_MAKE_ARRAY 74
#define LTV_OP_SRMA 75
#define LTV_OP_HASHT 76
#define LTV_OP_SHASH 77
#define LTV_OP_SB64 78
#define LTV_OP_PACKAGE 79
#define LTV_OP_BIGNUM 80
#define LTV_OP_FLOAT 90
#define LTV_OP_DOUBLE 91
#define LTV_OP_RATIO 67
#define LTV_OP_COMPLEX 68
#define LTV_OP_SYMBOL 81
#define LTV_OP_INTERN 82
#define LTV_OP_CHARACTER 83
#define LTV_OP_PATHNAME 85
#define LTV_OP_BCFUNC 87
#define LTV_OP_BCMOD 88
#define LTV_OP_SLITS 89
#define LTV_OP_FDEF 95
#define LTV_OP_CREATE 93
#define LTV_OP_INIT 94
#define LTV_OP_CLASS 98
#define LTV_OP_ATTR 255

namespace core {

#define BC_MAGIC 0x8d7498b1

// FIXME: Use an anonymous namespace, or something, rather than all this static

static inline uint8_t read_u8(Stream_sp stream) {
  return clasp_read_byte(stream).unsafe_fixnum();
}

static inline int8_t read_s8(Stream_sp stream) {
  uint8_t byte = read_u8(stream);
  union { uint8_t u; int8_t i; } converter;
  converter.u = byte;
  return converter.i;
}

static inline uint16_t read_u16(Stream_sp stream) {
  // Ideally we'd want to use something like read-sequence here.
  uint16_t high = read_u8(stream);
  uint16_t low = read_u8(stream);
  return (high << 8) | low;
}

static inline int16_t read_s16(Stream_sp stream) {
  uint16_t hw = read_u16(stream);
  union { uint16_t u; int16_t i; } converter;
  converter.u = hw;
  return converter.i;
}

static inline uint32_t read_u32(Stream_sp stream) {
  uint32_t b0 = read_u8(stream);
  uint32_t b1 = read_u8(stream);
  uint32_t b2 = read_u8(stream);
  uint32_t b3 = read_u8(stream);
  return (b0 << 24) | (b1 << 16) | (b2 << 8) | (b3 << 0);
}

CL_DEFUN uint32_t test_read_u32(Stream_sp stream) {
  return read_u32(stream);
}

static inline int32_t read_s32(Stream_sp stream) {
  uint32_t w = read_u32(stream);
  union { uint32_t u; int32_t i; } converter;
  converter.u = w;
  return converter.i;
}

static inline uint64_t read_u64(Stream_sp stream) {
  uint64_t b0 = read_u8(stream);
  uint64_t b1 = read_u8(stream);
  uint64_t b2 = read_u8(stream);
  uint64_t b3 = read_u8(stream);
  uint64_t b4 = read_u8(stream);
  uint64_t b5 = read_u8(stream);
  uint64_t b6 = read_u8(stream);
  uint64_t b7 = read_u8(stream);
  return (b0 << 56) | (b1 << 48) | (b2 << 40) | (b3 << 32)
    | (b4 << 24) | (b5 << 16) | (b6 << 8) | (b7 << 0);
}

static inline int64_t read_s64(Stream_sp stream) {
  uint64_t dw = read_u64(stream);
  union { uint64_t u; int64_t i; } converter;
  converter.u = dw;
  return converter.i;
}

static inline float read_f32(Stream_sp stream) {
  union { float f; uint32_t i; } converter;
  converter.i = read_u32(stream);
  return converter.f;
}

static inline double read_f64(Stream_sp stream) {
  union { double d; uint64_t i; } converter;
  converter.i = read_u64(stream);
  return converter.d;
}

static inline uint8_t read_opcode(Stream_sp stream) {
  return read_u8(stream);
}

// Read an n-byte unsigned integer.
static inline size_t read_index(Stream_sp stream, uint8_t nbytes) {
  switch (nbytes) {
  case 1: return read_u8(stream);
  case 2: return read_u16(stream);
  case 4: return read_u32(stream);
  case 8: return read_u64(stream);
  default: UNREACHABLE();
  }
}

void load_magic(Stream_sp stream) {
  uint32_t magic = read_u32(stream);
  if (magic != BC_MAGIC)
    SIMPLE_ERROR("Invalid FASL: incorrect magic number 0x%" PRIx32, magic);
}

// versions are std::arrays so that we can compare them.
typedef std::array<uint16_t, 2> BCVersion;

const BCVersion min_version = {0, 8};
const BCVersion max_version = {0, 8};

BCVersion load_version(Stream_sp stream) {
  // C++ guarantees sequencing in the aggregate initialization.
  BCVersion version = {read_u16(stream), read_u16(stream)};
  if ((min_version <= version) && (version <= max_version))
    return version;
  else
    // FIXME: Condition classes
    SIMPLE_ERROR("FASL version %" PRIu16 ".%" PRIu16 " is out of range of this loader",
                 version[0], version[1]);
}

void check_initialization(std::vector<bool>& initflags) {
  // bool vectors are apparently stupid and weird so using std algorithms
  // may not work. so we do something stupid.
  for (size_t i = 0; i < initflags.size(); ++i)
    if (!initflags[i]) // not initialized
      SIMPLE_ERROR("Invalid FASL: did not initialize object #%zu", i);
}

static T_sp get_ltv(size_t index,
                    SimpleVector_sp literals, std::vector<bool>& initflags) {
  if (index >= initflags.size())
    SIMPLE_ERROR("Invalid FASL: requested object #%zu, which is out of range",
                 index);
  if (!initflags[index])
    SIMPLE_ERROR("Invalid FASL: requested object #%zu, which has not yet been initialized", index);
  return (*literals)[index];
}

static void set_ltv(T_sp value, size_t index,
                    SimpleVector_sp literals, std::vector<bool>& initflags) {
  if (index >= initflags.size())
    SIMPLE_ERROR("Invalid FASL: Tried to set object #%zu, which is out of range",
                 index);
  if (initflags[index])
    SIMPLE_ERROR("Invalid FASL: Tried to set object #%zu, which has already been initialized",
                 index);
  (*literals)[index] = value;
  initflags[index] = true;
}

static void ltv_op_nil(Stream_sp stream, SimpleVector_sp literals,
                       std::vector<bool>& initflags, uint8_t index_bytes) {
  set_ltv(nil<T_O>(), read_index(stream, index_bytes), literals, initflags);
}

static void ltv_op_t(Stream_sp stream, SimpleVector_sp literals,
                     std::vector<bool>& initflags, uint8_t index_bytes) {
  set_ltv(cl::_sym_T_O, read_index(stream, index_bytes), literals, initflags);
}

static void ltv_op_cons(Stream_sp stream, SimpleVector_sp literals,
                        std::vector<bool>& initflags, uint8_t index_bytes) {
  set_ltv(Cons_O::create(nil<T_O>(), nil<T_O>()),
          read_index(stream, index_bytes), literals, initflags);
}

static void ltv_op_rplaca(Stream_sp stream, SimpleVector_sp literals,
                          std::vector<bool>& initflags, uint8_t index_bytes) {
  Cons_sp c = gc::As<Cons_sp>(get_ltv(read_index(stream, index_bytes), literals, initflags));
  c->rplaca(get_ltv(read_index(stream, index_bytes), literals, initflags));
}

static void ltv_op_rplacd(Stream_sp stream, SimpleVector_sp literals,
                          std::vector<bool>& initflags, uint8_t index_bytes) {
  Cons_sp c = gc::As<Cons_sp>(get_ltv(read_index(stream, index_bytes), literals, initflags));
  c->rplacd(get_ltv(read_index(stream, index_bytes), literals, initflags));
}

enum class UAETCode : uint8_t {
    nil            = 0b00000000,
    base_char      = 0b10000000,
    character      = 0b11000000,
    short_float    = 0b10100000,
    single_float   = 0b00100000,
    double_float   = 0b01100000,
    long_float     = 0b11100000,
    complex_short  = 0b10110000,
    complex_single = 0b00110000,
    complex_double = 0b01110000,
    complex_long   = 0b11110000,
    bit            = 0b00000001,
    ub2            = 0b00000010,
    ub4            = 0b00000011,
    ub8            = 0b00000100,
    ub16           = 0b00000101,
    ub32           = 0b00000110,
    ub64           = 0b00000111,
    sb8            = 0b10000100,
    sb16           = 0b10000101,
    sb32           = 0b10000110,
    sb64           = 0b10000111,
    t              = 0b11111111
};

static T_sp decode_uaet(uint8_t code) {
  switch (UAETCode{code}) {
  case UAETCode::nil: return nil<T_O>();
  case UAETCode::base_char: return cl::_sym_base_char;
  case UAETCode::character: return cl::_sym_character;
  //case UAETCode::short_float: return cl::_sym_ShortFloat_O;
  case UAETCode::single_float: return cl::_sym_single_float;
  case UAETCode::double_float: return cl::_sym_DoubleFloat_O;
  //case UAETCode::long_float: return cl::_sym_LongFloat_O;
  //case UAETCode::complex_short:
  //case UAETCode::complex_single:
  //case UAETCode::complex_double:
  //case UAETCode::complex_long:
  case UAETCode::bit: return cl::_sym_bit;
  case UAETCode::ub2: return ext::_sym_byte2;
  case UAETCode::ub4: return ext::_sym_byte4;
  case UAETCode::ub8: return ext::_sym_byte8;
  case UAETCode::ub16: return ext::_sym_byte16;
  case UAETCode::ub32: return ext::_sym_byte32;
  case UAETCode::ub64: return ext::_sym_byte64;
  case UAETCode::sb8: return ext::_sym_integer8;
  case UAETCode::sb16: return ext::_sym_integer16;
  case UAETCode::sb32: return ext::_sym_integer32;
  case UAETCode::sb64: return ext::_sym_integer64;
  case UAETCode::t: return cl::_sym_T_O;
  default: SIMPLE_ERROR("Invalid FASL: Unknown UAET code %" PRIx8, code);
  }
}

static void fill_sub_byte(Array_sp array, size_t total_size,
                          size_t nbits, Stream_sp stream) {
  // FIXME: Very inefficient.
  // In a best case scenario we can move in entire bit_array_words at a time,
  // probably?
  size_t perbyte = 8 / nbits;
  size_t full_bytes = total_size / 8;
  for (size_t byte_index = 0; byte_index < full_bytes; ++byte_index) {
    size_t index = perbyte * byte_index;
    uint8_t byte = read_u8(stream);
    for (size_t j = 0; j < perbyte; ++j) {
      size_t bit_index = nbits * (perbyte - j - 1);
      uint8_t mask = (1 << nbits) - 1;
      uint8_t bits = (byte & (mask << bit_index)) >> bit_index;
      array->rowMajorAset(index + j, clasp_make_fixnum(bits));
    }
  }
  // write remainder
  size_t remainder = total_size % 8;
  size_t index = perbyte * full_bytes;
  uint8_t byte = read_u8(stream); // should this be read when remainder = 0?
  for (size_t j = 0; j < remainder; ++j) {
    size_t bit_index = nbits * (perbyte - j - 1);
    uint8_t mask = (1 << nbits) - 1;
    uint8_t bits = (byte & (mask << bit_index)) >> bit_index;
    array->rowMajorAset(index + j, clasp_make_fixnum(bits));
  }
}

static void fill_array(Array_sp array, size_t total_size,
                       uint8_t packing, Stream_sp stream) {
  // FIXME: Inefficient.
  // Really we ought to be able to read(2) stuff in directly sometimes.
  // And can we do the simple form here for multidimensional arrays?
#define READ_ARRAY(BaseType, EXPR, EXTEXPR)\
  if (gc::IsA<BaseType>(array)) {\
    BaseType sv = gc::As_unsafe<BaseType>(array);\
    for (size_t i = 0; i < total_size; ++i)\
      (*sv)[i] = (EXPR);\
  } else {\
    for (size_t i = 0; i < total_size; ++i)\
      array->rowMajorAset(i, (EXTEXPR));\
  }
  switch (UAETCode{packing}) {
  case UAETCode::nil: break;
  case UAETCode::base_char:
      READ_ARRAY(SimpleBaseString_sp, read_u8(stream), clasp_make_character(read_u8(stream)));
      break;
  case UAETCode::character:
      READ_ARRAY(SimpleCharacterString_sp, read_u32(stream), clasp_make_character(read_u32(stream)));
      break;
  case UAETCode::single_float:
      READ_ARRAY(SimpleVector_float_sp, read_f32(stream), clasp_make_single_float(read_f32(stream)));
      break;
  case UAETCode::double_float:
      READ_ARRAY(SimpleVector_double_sp, read_f64(stream), clasp_make_double_float(read_f64(stream)));
      break;
  case UAETCode::bit: fill_sub_byte(array, total_size, 1, stream); break;
  case UAETCode::ub2: fill_sub_byte(array, total_size, 2, stream); break;
  case UAETCode::ub4: fill_sub_byte(array, total_size, 4, stream); break;
  case UAETCode::ub8:
      READ_ARRAY(SimpleVector_byte8_t_sp, read_u8(stream), clasp_make_fixnum(read_u8(stream)));
      break;
  case UAETCode::ub16:
      READ_ARRAY(SimpleVector_byte16_t_sp, read_u16(stream), clasp_make_fixnum(read_u16(stream)));
      break;
  case UAETCode::ub32:
      READ_ARRAY(SimpleVector_byte32_t_sp, read_u32(stream), clasp_make_fixnum(read_u32(stream)));
      break;
  case UAETCode::ub64:
      READ_ARRAY(SimpleVector_byte64_t_sp, read_u64(stream), Integer_O::create(read_u64(stream)));
      break;
  case UAETCode::sb8:
      READ_ARRAY(SimpleVector_int8_t_sp, read_s8(stream), clasp_make_fixnum(read_s8(stream)));
      break;
  case UAETCode::sb16:
      READ_ARRAY(SimpleVector_int16_t_sp, read_s16(stream), clasp_make_fixnum(read_s16(stream)));
      break;
  case UAETCode::sb32:
      READ_ARRAY(SimpleVector_int32_t_sp, read_s32(stream), clasp_make_fixnum(read_s32(stream)));
      break;
  case UAETCode::sb64:
      READ_ARRAY(SimpleVector_int64_t_sp, read_s64(stream), Integer_O::create(read_s64(stream)));
      break;
  case UAETCode::t: break; // handled by setf row-major-aref
  default: SIMPLE_ERROR("Not implemented: packing code %" PRIx8, packing);
  }
#undef READ_ARRAY
}

static void ltv_op_array(Stream_sp stream, SimpleVector_sp literals,
                         std::vector<bool>& initflags, uint8_t index_bytes) {
  // FIXME: This is pretty inefficient, including consing way more than it
  // ought to. We don't really have C++ equivalents for make-array.
  size_t index = read_index(stream, index_bytes);
  uint8_t uaet_code = read_u8(stream);
  T_sp uaet = decode_uaet(uaet_code);
  uint8_t packing_code = read_u8(stream);
  uint8_t rank = read_u8(stream);
  size_t total = 1;
  // FIXME: Shouldn't cons a list, but we don't have a lower level maker
  // that can handle all the different element types.
  ql::list dims;
  for (size_t i = 0; i < rank; ++i) {
    uint16_t dim = read_u16(stream);
    dims << clasp_make_fixnum(dim);
    total *= dim;
  }
  Array_sp arr
    = (rank == 1)
    // very unsure about the cast, but this is an ambiguous ?: otherwise
    ? gc::As<Array_sp>(core__make_vector(uaet, total, false, nil<T_O>(), nil<T_O>(),
                                         clasp_make_fixnum(0), nil<T_O>(), false))
    : gc::As<Array_sp>(core__make_mdarray(dims.cons(), uaet, false, nil<T_O>(),
                                          clasp_make_fixnum(0), nil<T_O>(), false));
  set_ltv(arr, index, literals, initflags);
  fill_array(arr, total, packing_code, stream);
}

static void ltv_op_srma(Stream_sp stream, SimpleVector_sp literals,
                        std::vector<bool>& initflags, uint8_t index_bytes) {
  Array_sp arr = gc::As<Array_sp>(get_ltv(read_index(stream, index_bytes), literals, initflags));
  size_t aindex = read_u16(stream);
  T_sp value = get_ltv(read_index(stream, index_bytes), literals, initflags);
  arr->rowMajorAset(aindex, value);
}

static void ltv_op_hasht(Stream_sp stream, SimpleVector_sp literals,
                         std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  uint8_t testcode = read_u8(stream);
  uint16_t count = read_u16(stream);
  // Resolve test
  T_sp test = nil<T_O>();
  switch (testcode) {
  case 0b00: test = cl::_sym_eq; break;
  case 0b01: test = cl::_sym_eql; break;
  case 0b10: test = cl::_sym_equal; break;
  case 0b11: test = cl::_sym_equalp; break;
  default: SIMPLE_ERROR("Unknown hash table test code %" PRIx8, testcode);
  }
  set_ltv(cl__make_hash_table(test, clasp_make_fixnum(count),
                              clasp_make_single_float(2.0),
                              clasp_make_single_float(0.7),
                              nil<T_O>(), nil<T_O>(), nil<T_O>(), nil<T_O>()),
          index, literals, initflags);
}

static void ltv_op_shash(Stream_sp stream, SimpleVector_sp literals,
                         std::vector<bool>& initflags, uint8_t index_bytes) {
  HashTableBase_sp ht = gc::As<HashTableBase_sp>(get_ltv(read_index(stream, index_bytes), literals, initflags));
  T_sp key = get_ltv(read_index(stream, index_bytes), literals, initflags);
  T_sp val = get_ltv(read_index(stream, index_bytes), literals, initflags);
  ht->hash_table_setf_gethash(key, val);  
}

static void ltv_op_sb64(Stream_sp stream, SimpleVector_sp literals,
                        std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  set_ltv(Integer_O::create(read_s64(stream)), index, literals, initflags);
}

static void ltv_op_package(Stream_sp stream, SimpleVector_sp literals,
                           std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  String_sp name = gc::As<String_sp>(get_ltv(read_index(stream, index_bytes), literals, initflags));
  set_ltv(_lisp->findPackage(name->get_std_string()),
          index, literals, initflags);
}

static void ltv_op_bignum(Stream_sp stream, SimpleVector_sp literals,
                          std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  int64_t ssize = read_s64(stream);
  mp_limb_t limbs[std::abs(ssize)];
  for (size_t i = std::abs(ssize); i > 0; --i)
    limbs[i - 1] = read_u64(stream);
  set_ltv(bignum_result(ssize, limbs), index, literals, initflags);
}

static void ltv_op_float(Stream_sp stream, SimpleVector_sp literals,
                         std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  set_ltv(clasp_make_single_float(read_f32(stream)),
          index, literals, initflags);
}

static void ltv_op_double(Stream_sp stream, SimpleVector_sp literals,
                          std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  set_ltv(clasp_make_double_float(read_f64(stream)),
          index, literals, initflags);
}

static void ltv_op_ratio(Stream_sp stream, SimpleVector_sp literals,
                         std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  Integer_sp num = gc::As<Integer_sp>(get_ltv(read_index(stream, index_bytes),
                                              literals, initflags));
  Integer_sp den = gc::As<Integer_sp>(get_ltv(read_index(stream, index_bytes),
                                              literals, initflags));
  set_ltv(contagion_div(num, den), index, literals, initflags);
}

static void ltv_op_complex(Stream_sp stream, SimpleVector_sp literals,
                           std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  Real_sp real = gc::As<Real_sp>(get_ltv(read_index(stream, index_bytes),
                                         literals, initflags));
  Real_sp imag = gc::As<Real_sp>(get_ltv(read_index(stream, index_bytes),
                                         literals, initflags));
  set_ltv(clasp_make_complex(real, imag), index, literals, initflags);
}

static void ltv_op_symbol(Stream_sp stream, SimpleVector_sp literals,
                          std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  SimpleString_sp name
    = gc::As<SimpleString_sp>(get_ltv(read_index(stream, index_bytes),
                                      literals, initflags));
  set_ltv(Symbol_O::create(name), index, literals, initflags);
}

static void ltv_op_intern(Stream_sp stream, SimpleVector_sp literals,
                          std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  Package_sp pack = gc::As<Package_sp>(get_ltv(read_index(stream, index_bytes),
                                               literals, initflags));
  SimpleString_sp name
    = gc::As<SimpleString_sp>(get_ltv(read_index(stream, index_bytes),
                                      literals, initflags));
  set_ltv(pack->intern(name), index, literals, initflags);
}

static void ltv_op_character(Stream_sp stream, SimpleVector_sp literals,
                             std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  uint32_t code = read_u32(stream);
  set_ltv(clasp_make_character(code), index, literals, initflags);
}

static void ltv_op_pathname(Stream_sp stream, SimpleVector_sp literals,
                            std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  T_sp host = get_ltv(read_index(stream, index_bytes), literals, initflags);
  T_sp device = get_ltv(read_index(stream, index_bytes), literals, initflags);
  T_sp directory = get_ltv(read_index(stream, index_bytes), literals, initflags);
  T_sp name = get_ltv(read_index(stream, index_bytes), literals, initflags);
  T_sp type = get_ltv(read_index(stream, index_bytes), literals, initflags);
  T_sp version = get_ltv(read_index(stream, index_bytes), literals, initflags);
  set_ltv(Pathname_O::makePathname(host, device, directory,
                                   name, type, version, kw::_sym_local),
          index, literals, initflags);
}

static void ltv_op_bcfunc(Stream_sp stream, SimpleVector_sp literals,
                          std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  uint32_t entry_point = read_u32(stream);
  uint32_t final_size = read_u32(stream);
  uint16_t nlocals = read_u16(stream);
  uint16_t nclosed = read_u16(stream);
  BytecodeModule_sp module = gc::As<BytecodeModule_sp>(get_ltv(read_index(stream, index_bytes), literals, initflags));
  T_sp name = get_ltv(read_index(stream, index_bytes), literals, initflags);
  T_sp lambda_list = get_ltv(read_index(stream, index_bytes), literals, initflags);
  T_sp docstring = get_ltv(read_index(stream, index_bytes), literals, initflags);
  FunctionDescription_sp fdesc
    = makeFunctionDescription(name, lambda_list, docstring, nil<T_O>(),
                              nil<T_O>(), -1, -1, -1);
  GlobalBytecodeSimpleFun_sp fun
    = core__makeGlobalBytecodeSimpleFun(fdesc, module, nlocals, nclosed,
                                        entry_point, final_size,
                                        llvmo::cmp__compile_trampoline(name));
  set_ltv(fun, index, literals, initflags);
}

static void ltv_op_bcmod(Stream_sp stream, SimpleVector_sp literals,
                         std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  uint32_t len = read_u32(stream);
  BytecodeModule_sp mod = BytecodeModule_O::make();
  SimpleVector_byte8_t_sp bytes = SimpleVector_byte8_t_O::make(len);
  mod->setf_bytecode(bytes);
  cl__read_sequence(bytes, stream, clasp_make_fixnum(0), nil<T_O>());
  set_ltv(mod, index, literals, initflags);
}

static void ltv_op_slits(Stream_sp stream, SimpleVector_sp literals,
                         std::vector<bool>& initflags, uint8_t index_bytes) {
  BytecodeModule_sp mod = gc::As<BytecodeModule_sp>(get_ltv(read_index(stream, index_bytes), literals, initflags));
  uint16_t len = read_u16(stream);
  SimpleVector_sp lits = SimpleVector_O::make(len);
  mod->setf_literals(lits);
  for (size_t i = 0; i < len; ++i)
    (*lits)[i] = get_ltv(read_index(stream, index_bytes), literals, initflags);
}

static void ltv_op_fdef(Stream_sp stream, SimpleVector_sp literals,
                        std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  T_sp name = get_ltv(read_index(stream, index_bytes), literals, initflags);
  set_ltv(cl__fdefinition(name), index, literals, initflags);
}

static void ltv_op_create(Stream_sp stream, SimpleVector_sp literals,
                          std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  Function_sp func = coerce::functionDesignator(get_ltv(read_index(stream, index_bytes), literals, initflags));
  uint16_t nargs = read_u16(stream);
  T_O* args[nargs];
  for (size_t i = 0; i < nargs; ++i)
    args[i] = get_ltv(read_index(stream, index_bytes), literals, initflags).raw_();
  T_sp res = funcall_general<Function_O>((gc::Tagged)(func.raw_()), nargs, args);
  set_ltv(res, index, literals, initflags);
}

static void ltv_op_init(Stream_sp stream, SimpleVector_sp literals,
                        std::vector<bool>& initflags, uint8_t index_bytes) {
  Function_sp func = coerce::functionDesignator(get_ltv(read_index(stream, index_bytes), literals, initflags));
  uint16_t nargs = read_u16(stream);
  T_O* args[nargs];
  for (size_t i = 0; i < nargs; ++i)
    args[i] = get_ltv(read_index(stream, index_bytes), literals, initflags).raw_();
  funcall_general<Function_O>((gc::Tagged)(func.raw_()), nargs, args);
}

static void ltv_op_class(Stream_sp stream, SimpleVector_sp literals,
                         std::vector<bool>& initflags, uint8_t index_bytes) {
  size_t index = read_index(stream, index_bytes);
  Symbol_sp name = gc::As<Symbol_sp>(get_ltv(read_index(stream, index_bytes), literals, initflags));
  set_ltv(cl__find_class(name, true, nil<T_O>()), index, literals, initflags);
}

static void ltv_op_attribute(Stream_sp stream, SimpleVector_sp literals,
                             std::vector<bool>& initflags, uint8_t index_bytes) {
  String_sp name = gc::As<String_sp>(get_ltv(read_index(stream, index_bytes),
                                             literals, initflags));
  uint32_t attrbytes = read_u32(stream);
  // TODO: Actually load any attributes.
  for (size_t i = 0; i < attrbytes; ++i) read_u8(stream);
}

void load_instruction(Stream_sp stream, SimpleVector_sp literals,
                      std::vector<bool>& initflags, uint8_t index_bytes) {
  uint8_t opcode = read_opcode(stream);
  //printf("op %" PRIx8 "\n", opcode);
#define CALL_LOADER(NAME) ltv_op_##NAME(stream, literals, initflags, index_bytes)
  switch (opcode) {
  case LTV_OP_NIL: CALL_LOADER(nil); break;
  case LTV_OP_T: CALL_LOADER(t); break;
  case LTV_OP_CONS: CALL_LOADER(cons); break;
  case LTV_OP_RPLACA: CALL_LOADER(rplaca); break;
  case LTV_OP_RPLACD: CALL_LOADER(rplacd); break;
  case LTV_OP_MAKE_ARRAY: CALL_LOADER(array); break;
  case LTV_OP_SRMA: CALL_LOADER(srma); break; // (setf row-major-aref)
  case LTV_OP_HASHT: CALL_LOADER(hasht); break; // make-hash-table
  case LTV_OP_SHASH: CALL_LOADER(shash); break; // (setf gethash)
  case LTV_OP_SB64: CALL_LOADER(sb64); break;
  case LTV_OP_PACKAGE: CALL_LOADER(package); break;
  case LTV_OP_BIGNUM: CALL_LOADER(bignum); break;
  case LTV_OP_FLOAT: CALL_LOADER(float); break;
  case LTV_OP_DOUBLE: CALL_LOADER(double); break;
  case LTV_OP_RATIO: CALL_LOADER(ratio); break;
  case LTV_OP_COMPLEX: CALL_LOADER(complex); break;
  case LTV_OP_SYMBOL: CALL_LOADER(symbol); break;
  case LTV_OP_INTERN: CALL_LOADER(intern); break;
  case LTV_OP_CHARACTER: CALL_LOADER(character); break;
  case LTV_OP_PATHNAME: CALL_LOADER(pathname); break;
  case LTV_OP_BCFUNC: CALL_LOADER(bcfunc); break;
  case LTV_OP_BCMOD: CALL_LOADER(bcmod); break;
  case LTV_OP_SLITS: CALL_LOADER(slits); break; // setf literals
  case LTV_OP_FDEF: CALL_LOADER(fdef); break;
  case LTV_OP_CREATE: CALL_LOADER(create); break; // funcall-create
  case LTV_OP_INIT: CALL_LOADER(init); break; // funcall-initialize
  case LTV_OP_CLASS: CALL_LOADER(class); break;
  case LTV_OP_ATTR: CALL_LOADER(attribute); break;
  default: SIMPLE_ERROR("Unknown opcode %" PRIx8, opcode);
  }
#undef CALL_LOADER
}

CL_DEFUN void load_bytecode_stream(Stream_sp stream) {
  load_magic(stream);
  BCVersion vers = load_version(stream);
  uint64_t nobjs = read_u64(stream);
  uint64_t ninsts = read_u64(stream);
  uint8_t index_bytes = (std::bit_width(nobjs) + CHAR_BIT - 1) / CHAR_BIT;
  SimpleVector_sp literals = SimpleVector_O::make(nobjs);
  std::vector<bool> initflags(nobjs, false);
  for (size_t i = 0; i < ninsts; ++i)
    load_instruction(stream, literals, initflags, index_bytes);
  // TODO: Check EOF
  check_initialization(initflags);
}

CL_DEFUN bool load_bytecode(T_sp filename, bool verbose, bool print,
                            T_sp external_format) {
  T_sp strm = cl__open(filename, kw::_sym_input, ext::_sym_byte8,
                       nil<T_O>(), false, nil<T_O>(), false,
                       external_format, nil<T_O>());
  if (strm.nilp()) return false;
  DynamicScopeManager lpscope(cl::_sym_STARloadPathnameSTAR, cl__pathname(filename));
  DynamicScopeManager ltscope(cl::_sym_STARloadTruenameSTAR, cl__truename(filename));
  load_bytecode_stream(gc::As<Stream_sp>(strm));
  cl__close(strm);
  return true;
}

CL_DEFUN bool load_bytecodel(T_sp filename, bool verbose, bool print,
                             T_sp external_format) {
  T_sp strm = cl__open(filename, kw::_sym_input, ext::_sym_byte8,
                       nil<T_O>(), false, nil<T_O>(), false,
                       external_format, nil<T_O>());
  if (strm.nilp()) return false;
  DynamicScopeManager lpscope(cl::_sym_STARloadPathnameSTAR, cl__pathname(filename));
  DynamicScopeManager ltscope(cl::_sym_STARloadTruenameSTAR, cl__truename(filename));
  int i = 0;
  while (cl__listen(strm)) {
    fmt::print("seg {}\n", ++i);
    load_bytecode_stream(gc::As<Stream_sp>(strm));
  }
  cl__close(strm);
  return true;
}

}; // namespace
