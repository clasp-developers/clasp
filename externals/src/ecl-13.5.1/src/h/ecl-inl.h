/* -*- mode: c; c-basic-offset: 8 -*- */

#ifndef ECL_ECL_INL_H
#define ECL_ECL_INL_H

/*
 * Loops over a proper list. Complains on circularity
 */
#define loop_for_in_no_circle(list) { \
  cl_object __slow; \
  bool __flag = TRUE; \
  for (__slow = list; !ecl_endp(list); list = ECL_CONS_CDR(list)) { \
    if ((__flag = !__flag)) { \
      if (__slow == list) FEcircular_list(list); \
      __slow = ECL_CONS_CDR(__slow); \
    }

/*
 * Loops over a proper list
 */
#define loop_for_in(list) { \
  const cl_object __ecl_l0 = list; \
  for (; list != ECL_NIL; list = ECL_CONS_CDR(list)) { \
    if (ecl_unlikely(!ECL_LISTP(list))) FEtype_error_proper_list(__ecl_l0);

#define end_loop_for_in }}

/*
 * Loops over a dotted list. Complains on circularity.
 */
#define loop_for_on_no_circle(list) \
  if (!CONSP(list)) { \
    if (list != ECL_NIL) FEtype_error_list(list); \
  }else { \
    cl_object __slow; \
    bool __flag = TRUE; \
    for (__slow = list; CONSP(list); list = ECL_CONS_CDR(list)) { \
      if ((__flag = !__flag)) { \
        if (__slow == list) FEcircular_list(list); \
        __slow = CDR(__slow); \
      }

/*
 * Loops over a list. Ignores errors.
 */
#define loop_for_on_unsafe(list) \
  for (; ECL_CONSP(list); list = ECL_CONS_CDR(list)) {
#define end_loop_for_on_unsafe(list) }

/*
 * Loops over a dotted list
 */
#define loop_for_on(list) \
  if (Null(list)) { \
    (void)0; \
  } else if (ecl_unlikely(!ECL_LISTP(list))) { \
    FEtype_error_list(list); \
  } else do {
#define end_loop_for_on(list) } while (list = ECL_CONS_CDR(list), ECL_CONSP(list))

/*
 * Static constant definition.
 */
#ifdef __cplusplus
#define ecl_cast_ptr(type,n) reinterpret_cast<type>((void*)n)
#else
#define ecl_cast_ptr(type,n) ((type)(n))
#endif

#define ecl_def_string_array(name,static,const)                         \
        static const union {                                            \
                struct ecl_base_string elt;                             \
                cl_fixnum padding[(sizeof(struct ecl_base_string)+3)/4*4]; \
        } name[]

#define ecl_def_string_array_elt(chars) {                      \
                (int8_t)t_base_string, 0, ecl_aet_bc, 0,            \
                        ECL_NIL, (cl_index)(sizeof(chars))-1,      \
                        (cl_index)(sizeof(chars))-1,            \
                        (ecl_base_char*)(chars) }

#define ecl_def_ct_base_string(name,chars,len,static,const)     \
        static const struct ecl_base_string name ## _data = {    \
                (int8_t)t_base_string, 0, ecl_aet_bc, 0,            \
                ECL_NIL, (cl_index)(len), (cl_index)(len),         \
                (ecl_base_char*)(chars) };                      \
        static const cl_object name = (cl_object)(& name ## _data)

#define ecl_def_ct_single_float(name,f,static,const)            \
        static const struct ecl_singlefloat name ## _data = {    \
                (int8_t)t_singlefloat, 0, 0, 0,                 \
                (float)(f) };                                   \
        static const cl_object name = (cl_object)(& name ## _data)

#define ecl_def_ct_double_float(name,f,static,const)            \
        static const struct ecl_doublefloat name ## _data = {    \
                (int8_t)t_doublefloat, 0, 0, 0,                 \
                (double)(f) };                                  \
        static const cl_object name = (cl_object)(& name ## _data)

#define ecl_def_ct_long_float(name,f,static,const)			\
        static const struct ecl_long_float name ## _data = {		\
                (int8_t)t_longfloat, 0, 0, 0,				\
                (long double)(f) };					\
        static const cl_object name = (cl_object)(& name ## _data)

#define ecl_def_ct_ratio(name,num,den,static,const)			\
        static const struct ecl_ratio name ## _data = {			\
                (int8_t)t_ratio, 0, 0, 0,				\
                den, num };						\
        static const cl_object name = (cl_object)(& name ## _data)

#define ecl_def_ct_complex(name,real,imag,static,const)			\
        static const struct ecl_complex name ## _data = {		\
                (int8_t)t_complex, 0, 0, 0,				\
                (cl_object)real, (cl_object)imag };			\
        static const cl_object name = (cl_object)(& name ## _data)

#define ecl_def_ct_vector(name,type,raw,len,static,const)               \
        static const struct ecl_vector name ## _data = {                 \
                (int8_t)t_vector, 0, (type), 0,                         \
                ECL_NIL, (cl_index)(len), (cl_index)(len),                 \
                ecl_cast_ptr(cl_object*,raw), 0 };                      \
        static const cl_object name = (cl_object)(& name ## _data)

#ifdef ECL_SSE2

/*
 * Static SSE constant.
 */

#define ecl_def_ct_sse_pack(name,type,v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15) \
        static const struct ecl_sse_pack name ## _data = {                 \
                (int8_t)t_sse_pack, 0, (type), 0,                         \
                {{v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15}} \
        }; \
        static const cl_object name = (cl_object)(& name ## _data)

/*
 * Missing SSE intrinsics
 */

#if (defined(__INTEL_COMPILER) ? __INTEL_COMPILER < 810 : defined(_MSC_VER) && (_MSC_VER < 1500))

__forceinline __m128i _mm_castps_si128(__m128 a) { union { __m128 f; __m128i i;} c; c.f = a; return c.i; }
__forceinline __m128 _mm_castsi128_ps(__m128i a) { union { __m128 f; __m128i i;} c; c.i = a; return c.f; }
__forceinline __m128i _mm_castpd_si128(__m128d a) { union { __m128d d; __m128i i;} c; c.d = a; return c.i; }
__forceinline __m128d _mm_castsi128_pd(__m128i a) { union { __m128d d; __m128i i;} c; c.i = a; return c.d; }
__forceinline __m128d _mm_castps_pd(__m128 a) { union { __m128d d; __m128 f;} c; c.f = a; return c.d; }
__forceinline __m128 _mm_castpd_ps(__m128d a) { union { __m128d d; __m128 f;} c; c.d = a; return c.f; }

#elif defined(__GNUC__) && (__GNUC__ < 4) && !defined(__INTEL_COMPILER)

// Copied from GCC 4 headers:
extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_castpd_ps(__m128d __A) { return (__m128) __A; }

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_castpd_si128(__m128d __A) { return (__m128i) __A; }

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_castps_pd(__m128 __A) { return (__m128d) __A; }

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_castps_si128(__m128 __A) { return (__m128i) __A; }

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_castsi128_ps(__m128i __A) { return (__m128) __A; }

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_castsi128_pd(__m128i __A) { return (__m128d) __A; }

#endif

#endif /* ECL_SSE2 */

#endif /* !ECL_ECL_INL_H */
