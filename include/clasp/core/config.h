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
#ifndef core_config_H
#define core_config_H

#include <cstdint>

#define BYTE_SIZE 8

#define POLL_TICKS_PER_GC 1024000

#if defined(_ADDRESS_MODEL_64)
#define INTPTR_BITS 64
typedef uintptr_t cl_intptr_t;
#else // POINTER_BITS <=32
#define INTPTR_BITS 32
typedef uintptr_t cl_intptr_t;
#endif // POINTER_BITS

#define CHAR_CODE_LIMIT 256 /* ASCII or unicode character code limit */

typedef char claspChar;
typedef unsigned int claspCharacter;
#define CLASP_CHAR(x) ((x)&0xff)

#define IS_DIR_SEPARATOR(c) ((c) == '/')
#define DIR_SEPARATOR "/"
#define DIR_SEPARATOR_CHAR '/'

#define CLASP_NAMESTRING_TRUNCATE_IF_ERROR 1
#define CLASP_NAMESTRING_FORCE_BASE_STRING 2

/*! Configure if system has getpwnam */

#define HAVE_PWD_H 1

// 64Kilobytes for thread local stack size
// Compiling minimal clasp or full clasp didn't require more than 24Kb
#define THREAD_LOCAL_CL_STACK_MIN_SIZE (size_t)(16 * 1024)

// OS X 10.6   LINUX 4096???
#define CLASP_MAXPATHLEN 1024

/*! TODO: Tie this to the Fixnum tagged pointer implementation */
//#define FIXNUM_BITS 63

//#define MOST_POSITIVE_FIXNUM std::numeric_limits<int>::max()
//#define MOST_NEGATIVE_FIXNUM std::numeric_limits<int>::min()

/*! Used to allocate a large string buffer */
#define BUFFER_STRING_SIZE 4192

/*! For now define HAVE_LSTAT here  - I think we have lstat on OS X and linux*/
#define HAVE_LSTAT 1

#define HAVE_DIRENT_H 1

/*! Don't use LongFloat - they are doubles */
//#define CLASP_LONG_FLOAT 1

// Use an array on the stack to store multiple_values
#define USE_MULTIPLE_VALUES_ARRAY

#define CLASP_ARRAY_DIMENSION_LIMIT (1024 * 1024)
#define CLASP_ARRAY_RANK_LIMIT 8

/*! Pass four arguments in registers, the rest in memory */
#define LCC_ARGS_IN_REGISTERS 3

/*! Maximum number of arguments that can be passed */
#define CALL_ARGUMENTS_LIMIT 64

#define CHAR_CODE_LIMIT 256

#define CLASP_INTERNAL_TIME_UNITS_PER_SECOND 1000
#endif

#define CLASP_CHAR_CODE_LINEFEED '\n'
#define CLASP_CHAR_CODE_NEWLINE '\n'
#define CLASP_CHAR_CODE_RETURN '\r'

// This should be set up by autoconf or bjam

#define HAVE_SELECT 1
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

// On linux and OS X we have mkstemp so use it
#define HAVE_MKSTEMP
