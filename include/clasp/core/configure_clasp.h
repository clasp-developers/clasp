#pragma once
/*
    File: config.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister

CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

See directory 'clasp/licenses' for full details.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */

// ----------------------------------------------------------------------------
// Set system wide debug flags here
// ----------------------------------------------------------------------------

#if 0
//// DEBUG_RUNTIME defined and type errors print more info
//#define DEBUG_RUNTIME 1
//#define DEBUG_BADGE_SSL 1
//#define DEBUG_OBJECT_FILES 1
//#define DEBUG_VALUES 1
//#define DEBUG_VASLIST 1
//#define DEBUG_EVALUATE 1
//#define DEBUG_DTORS 1
//#define DEBUG_DYN_ENV_STACK 1
//#define DEBUG_DTREE_INTERPRETER
//#define DEBUG_VIRTUAL_MACHINE 1
//#define DEBUG_DRAG_CXX_CALLS 1 // Slows down all calls in CXX wrappers - study impact
//#define DEBUG_DRAG_NATIVE_CALLS 1
//#define DEBUG_DRAG_INTERPRET_DTREE 1
//#define DEBUG_DRAG_CONS_ALLOCATION 1
//#define DEBUG_DRAG_GENERAL_ALLOCATION 1
/* 0 == no CHECK_FRAME, 1 == fast CHECK_FRAME, 2 == slow CHECK_FRAME */
#define DEBUG_FRAME() 0
// If ANY flags above are set - then set this one to print a message at startup
#define DEBUG_FLAGS_SET 1
#else
#define DEBUG_FRAME() 0
#endif

// ----------------------------------------------------------------------------
//  SYSTEM INCLUDES
// ----------------------------------------------------------------------------

#include <cinttypes>
#include <climits>

// ----------------------------------------------------------------------------
//  SANITY CHECK FOR SUPPORTED PLATFORMS AND ADDRESS MODELS
// ----------------------------------------------------------------------------

#if !defined(_TARGET_OS_DARWIN) && !defined(_TARGET_OS_LINUX) && !defined(_TARGET_OS_FREEBSD)
#error "We are sorry but this platform is not supported yet."
#endif

#if !defined(_ADDRESS_MODEL_64) && !defined(_ADDRESS_MODEL_32)
#error "We are sorry but this address model is not supported yet."
#endif

#if defined(_TARGET_OS_DARWIN) && defined(__aarch64__)
#define CLASP_APPLE_SILICON 1
#endif

// ----------------------------------------------------------------------------
//  CLASP FUNDAMENTAL DEFINITIONS
// ----------------------------------------------------------------------------

#define BYTE_SIZE 8
#define POLL_TICKS_PER_GC 1024000

// ----------------------------------------------------------------------------
//  ADDRESS-MODEL DEPENDENT CLASP FUNDAMENTAL DEFINITIONS
// ----------------------------------------------------------------------------

#if defined(_ADDRESS_MODEL_64)

#define __EX(var) #var
#define CXX_MACRO_STRING(var) __EX(var)
#define CLASP_GCROOTS_IN_MODULE(NAME) __clasp_gcroots_in_module_##NAME
#define GCROOTS_IN_MODULE_NAME CXX_MACRO_STRING(CLASP_GCROOTS_IN_MODULE())
#define CLASP_LITERALS(NAME) __clasp_literals_##NAME
#define LITERALS_NAME CXX_MACRO_STRING(CLASP_LITERALS())
#define INTPTR_BITS 64

typedef uint64_t bit_array_word; // "word" for bit array purposes (see gcbitarray.h)
#define BIT_ARRAY_WORD_BITS 64   // = sizeof(bit_array_word)*CHAR_BIT
// NOTE: C++11 says long long is at least 64 bits, so this probably works.
#define bit_array_word_popcount __builtin_popcountll
#define bit_array_word_clz __builtin_clzll

typedef int64_t Fixnum;         // Signed Fixnum immediate value
#define CLASP_FIXNUM_IS_INT64 1 // == true
#define fixnum_popcount __builtin_popcountll
#define fixnum_clz __builtin_clzll
#define fixnum_clrsb __builtin_clrsbll

#if defined(_TARGET_OS_DARWIN)
#define Ptagged_stamp_t PRIuPTR
#define PFixnum "lld"
#define Plu "lu"
#define PRu "llu"
#define PRi "lld"
#define PRF "lld"
#define PRFoctal "llo"
#define PRFhex "llX"
#define Puint "u"
#define PRsize_t "lu"
#endif
#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)
#define Ptagged_stamp_t PRIuPTR
#define PFixnum "ld"
#define Plu "lu"
#define PRu "lu"
#define PRi "ld"
#define PRF "ld"
#define PRFoctal "lo"
#define Puint "u"
#define PRFhex "lX"
#define PRsize_t "lu"
#endif

#else

#if defined(_ADDRESS_MODEL_32)

// For now, error out on 32 bit model as not really supported / tested
#error "32 bit address model not supported so far."

#define INTPTR_BITS 32

typedef int32_t Fixnum; // Signed Fixnum immediate value
#define CLASP_FIXNUM_IS_INT32 1 // == true
#define fixnum_popcount __builtin_popcountl
#define fixnum_clz __builtin_clzl
#define fixnum_clrsb __builtin_clrsbl

#define PRu "lu"
#define PRi "ld"
#define PRF "ld"
#define PRFoctal "lo"
#define PRFhex "lX"
#define PRsize_t "lu"

#endif // _ADDRESS_MODEL_32

#endif // _ADDRESS_MODEL_64

// ----------------------------------------------------------------------------
//  PLATFORM INDEPENDENT CLASP FUNDAMENTAL DEFINITIONS
// ----------------------------------------------------------------------------

#undef CLASP_LONG_LONG_IS_INT64
#undef CLASP_UNSIGNED_LONG_LONG_IS_UINT64

#if defined(_TARGET_OS_DARWIN)

#define CLASP_LONG_LONG_IS_INT64 1
#define CLASP_UNSIGNED_LONG_LONG_IS_UINT64 1

#endif

#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)

#undef CLASP_LONG_LONG_IS_INT64
#undef CLASP_UNSIGNED_LONG_LONG_IS_UINT64

#endif

// ----------------------------------------------------------------------------
//  ADDRESS-MODEL INDEPENDENT CLASP FUNDAMENTAL DEFINITIONS
// ----------------------------------------------------------------------------

#define GMP_LONG(z) (static_cast<long>(z))
#define GMP_ULONG(z) (static_cast<unsigned long>(z))

// ----------------------------------------------------------------------------
//  CHARACTER TYPES
// ----------------------------------------------------------------------------

typedef unsigned char claspChar;
typedef char32_t claspCharacter;
#define CLASP_CHAR(x) ((x)&0xff)

// ----------------------------------------------------------------------------
//  DIR SEP
// ----------------------------------------------------------------------------

#define DIR_SEPARATOR "/"
#define DIR_SEPARATOR_CHAR '/'
#define IS_DIR_SEPARATOR(c) ((c) == DIR_SEPARATOR_CHAR)

// ----------------------------------------------------------------------------
//  NAMESTRING HANDLING
// ----------------------------------------------------------------------------

#define CLASP_NAMESTRING_TRUNCATE_IF_ERROR 1
#define CLASP_NAMESTRING_FORCE_BASE_STRING 2

// ----------------------------------------------------------------------------
//  O/S LEVEL SYSTEM CAPABILITIES' DEFINITIONS
// ----------------------------------------------------------------------------

/*! Configure if system has getpwnam */

#define HAVE_PWD_H 1

// Thread local stack size
// Compiling minimal clasp or full clasp didn't require more than 24Kb
#define THREAD_LOCAL_CL_STACK_MIN_SIZE (size_t)(16 * 1024)

#if !defined(PATH_MAX)
#define CLASP_MAXPATHLEN 1024
#else
#define CLASP_MAXPATHLEN PATH_MAX
#endif

/*! Used to allocate a large string buffer */
#define BUFFER_STRING_SIZE 4192

/*! For now define HAVE_LSTAT here  - I think we have lstat on OS X and linux*/
#define HAVE_LSTAT 1

#define HAVE_DIRENT_H 1

/*! Don't use LongFloat - they are doubles */
// #define CLASP_LONG_FLOAT 1

#define CLASP_ARRAY_DIMENSION_LIMIT (1024 * 1024)
#define CLASP_ARRAY_RANK_LIMIT 8

/*! For general entry point - pass four arguments in registers, the rest in memory */
#define LCC_ARGS_IN_REGISTERS 4

/*! Return 1 pointer in register */
#define LCC_RETURN_VALUES_IN_REGISTERS() 1

/*! Range of fixed entry point aritys
    This will be used for the calling convention.
    Arity from ENTRY_POINT_ARITY_BEGIN to (ENTRY_POINT_ARITY_END-1) are supported

ENTRY_POINT_MAX_ARGS_IN_REGISTER_SAVE_AREA includes the closure
*/
#define LCC_WORDS_IN_REGISTER_SAVE_AREA 6
#define ENTRY_POINT_ARITY_BEGIN 0 // MUST ALWAYS BE ZERO - Or I need to fix a few places in the code
#define ENTRY_POINT_ARITY_END 6   // Must be one past the highest arity entry point
//! One entry point for each arity and one for general
#define NUMBER_OF_ENTRY_POINTS ENTRY_POINT_ARITY_END - ENTRY_POINT_ARITY_BEGIN + 1
#define STACKMAP_REGISTER_SAVE_AREA_MAGIC_NUMBER 0xDEAD0000
#define STACKMAP_REGISTER_SAVE_AREA_MASK 0xFFFF0000
#define STACKMAP_ARITY_CODE_MASK 0x0000000F

/*! Maximum number of multiple values */
#define MULTIPLE_VALUES_LIMIT 8192
/*! Maximum number of arguments that can be passed. Current compiler optimations require that
    MULTIPLE_VALUES_LIMIT >= CALL_ARGUMENTS_LIMIT */
#define CALL_ARGUMENTS_LIMIT 8192

#define CHAR_CODE_LIMIT 1114112

#define CLASP_INTERNAL_TIME_UNITS_PER_SECOND 1000000000

#define CLASP_CHAR_CODE_LINEFEED '\n'
#define CLASP_CHAR_CODE_NEWLINE '\n'
#define CLASP_CHAR_CODE_RETURN '\r'

// Use unix 'select' or 'poll'
// select has problems in Xcode 11.4.1 (
// #define HAVE_SELECT 1
#define HAVE_POLL 1
#define HAVE_FSEEKO 1

#ifndef HAVE_FSEEKO

#define clasp_off_t size_t
#define clasp_fseeko fseek
#define clasp_ftello ftell

#else

#define clasp_off_t off_t
#define clasp_fseeko fseeko
#define clasp_ftello ftello

#endif

#define clasp_startup_FUNCTION_NAME "clasp_startup"
#define clasp_shutdown_FUNCTION_NAME "clasp_shutdown"

// On linux and OS X we have mkstemp so use it
#define HAVE_MKSTEMP
#define HAVE_MKDTEMP

#if defined(DEBUG_ASSERT_TYPE_CAST) && defined(USE_PRECISE_GC)
// DO_ASSERT_TYPE_CAST only works with USE_PRECISE_GC
#define DO_ASSERT_TYPE_CAST 1
#else
                        // Do nothing - we can't DO_ASSERT_TYPE_CAST if
#endif
