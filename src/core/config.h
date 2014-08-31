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


#define CHAR_CODE_LIMIT	256 /* ASCII or unicode character code limit */

typedef char claspChar;
typedef int claspCharacter; 
#define BRCL_CHAR(x) ((x)&0xff)


#define IS_DIR_SEPARATOR(c) ((c)=='/')
#define DIR_SEPARATOR "/"
#define DIR_SEPARATOR_CHAR '/'

#define BRCL_NAMESTRING_TRUNCATE_IF_ERROR 1
#define BRCL_NAMESTRING_FORCE_BASE_STRING 2

/*! Configure if system has getpwnam */

#define HAVE_PWD_H	1


// OS X 10.6   LINUX 4096???
#define MAXPATHLEN	250

/*! TODO: Tie this to the Fixnum tagged pointer implementation */
#define FIXNUM_BITS 32

#define MOST_POSITIVE_FIXNUM std::numeric_limits<int>::max()
#define MOST_NEGATIVE_FIXNUM std::numeric_limits<int>::min()

/*! Used to allocate a large string buffer */
#define BUFFER_STRING_SIZE	4192


/*! For now define HAVE_LSTAT here  - I think we have lstat on OS X and linux*/
#define HAVE_LSTAT 1

#define HAVE_DIRENT_H 1


/*! Don't use LongFloat - they are doubles */
//#define CLASP_LONG_FLOAT 1


#define BRCL_ARRAY_DIMENSION_LIMIT (1024*1024)

/*! Maximum number of arguments that can be passed */
#define CALL_ARGUMENTS_LIMIT	64

#define BRCL_INTERNAL_TIME_UNITS_PER_SECOND 1000
#endif


#define CLASP_CHAR_CODE_LINEFEED '\n'
#define CLASP_CHAR_CODE_NEWLINE '\n'
#define CLASP_CHAR_CODE_RETURN '\r'


// This should be set up by autoconf or bjam

#define HAVE_SELECT 1
#define HAVE_FSEEKO 1

#ifndef HAVE_FSEEKO
#define clasp_off_t int
#define clasp_fseeko fseek
#define clasp_ftello ftell
#else
#define clasp_off_t off_t
#define clasp_fseeko fseeko
#define clasp_ftello ftello
#endif
