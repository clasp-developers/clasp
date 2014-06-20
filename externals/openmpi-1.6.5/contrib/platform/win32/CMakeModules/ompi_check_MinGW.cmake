# -*- cmake-script -*-
#
# Copyright (c) 2007-2011 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

######################################################################
#
# OMPI_MinGW
#
# Keep all the MinGW checks in one place.
#
######################################################################

# Get current time and date.
EXECUTE_PROCESS(COMMAND cmd /C time /t
                OUTPUT_VARIABLE    CURRENT_TIME)
EXECUTE_PROCESS(COMMAND cmd /C date /t
                OUTPUT_VARIABLE    CURRENT_DATE)

STRING (REPLACE "\n" "" CURRENT_TIME ${CURRENT_TIME})
STRING (REPLACE "\n" "" CURRENT_DATE ${CURRENT_DATE})
STRING (REGEX MATCH [.-/\0-9]+ CURRENT_DATE ${CURRENT_DATE})
SET (OMPI_CONFIGURE_DATE "${CURRENT_TIME} ${CURRENT_DATE}" CACHE INTERNAL "OMPI_CONFIGURE_DATE")
SET (OMPI_BUILD_DATE "${CURRENT_TIME} ${CURRENT_DATE}" CACHE INTERNAL "OMPI_BUILD_DATE")

OMPI_DEF(OMPI_CONFIGURE_DATE "${CURRENT_TIME} ${CURRENT_DATE}" "Configuration date." 1 1)
OMPI_DEF(OMPI_BUILD_DATE "${CURRENT_TIME} ${CURRENT_DATE}" "Build date." 1 1)

# Set up compiler information.
EXECUTE_PROCESS(COMMAND gcc --version OUTPUT_VARIABLE GCC_VERSION)
STRING (REGEX MATCH [.0-9]*\n GCC_VERSION ${GCC_VERSION})
STRING (REPLACE "\n" "" GCC_VERSION ${GCC_VERSION})

OMPI_DEF(COMPILER_FAMILYNAME GNU "Compiler family name" 1 1)
OMPI_DEF(COMPILER_VERSION ${GCC_VERSION} "Compiler version" 0 1)
OMPI_DEF(OPAL_BUILD_PLATFORM_COMPILER_FAMILYID 1 "Compiler family ID" 0 1)
OMPI_DEF(OPAL_BUILD_PLATFORM_COMPILER_FAMILYNAME ${COMPILER_FAMILYNAME} "Compiler family name" 0 1)
OMPI_DEF(OPAL_BUILD_PLATFORM_COMPILER_VERSION_STR ${GCC_VERSION} "Compiler version" 0 1)

IF(NOT MINGW_CHECK_DONE)

  MESSAGE( STATUS "Start MinGW specific detection....")

  GET_FILENAME_COMPONENT(CC ${CMAKE_C_COMPILER} NAME)
  GET_FILENAME_COMPONENT(CXX ${CMAKE_CXX_COMPILER} NAME)

  # Default compiler settings.
  SET(OMPI_C_OPTION_COMPILE "-c" CACHE INTERNAL
    "C compiler option for compiling without linking.")
  SET(OMPI_C_OUTPUT_OBJ "-o" CACHE INTERNAL
    "C compiler option for setting object file name.")
  SET(OMPI_C_OUTPUT_EXE "-o" CACHE INTERNAL
    "C compiler option for setting executable file name.")
  SET(OMPI_C_LAN_FLAG "-Wno-write-strings -x c" CACHE INTERNAL
    "C compiler option for compiling source as C.")
  SET(OMPI_CXX_LAN_FLAG "-Wno-write-strings -x c++" CACHE INTERNAL
    "C compiler option for compiling source as C++.")
  SET(OMPI_C_DEF_PRE "-D" CACHE INTERNAL
    "C compiler option for preprocessor definition.")
  SET(OMPI_C_MD_DEBUG " " CACHE INTERNAL
    "C compiler option for Multi-thread Debug DLL.")
  SET(OMPI_C_MD " " CACHE INTERNAL
    "C compiler option for Multi-thread DLL.")
  SET(OMPI_CXX_EXCEPTION " " CACHE INTERNAL
    "C compiler option for C++ exceptions.")
  SET(OMPI_C_INCLUDE_DIR "-I" CACHE INTERNAL
    "C compiler option for including directory.")
  SET(OMPI_C_LIB_DIR "-L" CACHE INTERNAL
    "C compiler option for including directory.")
  SET(OMPI_LIB_CMD "ar -rcs" CACHE INTERNAL
    "command line for making static libraries.")
  #NOTE: the space in the end of the option is important
  SET(OMPI_LIB_CMD_OUTPUT "" CACHE INTERNAL
    "Output option for making static libraries.")

  SET(DUMP_UTIL "nm" CACHE INTERNAL "the dumpbin application.")
  #SET(DUMP_UTIL_OPT "> " CACHE INTERNAL "the dumpbin application options.")

  FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/gcc_test.c
    "int main() {return 0;}")

  TRY_COMPILE(GCC_OK ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/
    ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/gcc_test.c)

  IF(GCC_OK)

    # The atomic functions are defined in a very unuasual manner.
    # Some of them are intrinsic defined in windows.h others are
    # exported by kernel32.dll. If we force the usage of TRY_RUN
    # here we will check for both in same time: compilation and run.

    SET(FUNCTION_LIST Exchange ExchangeAcquire ExchangeRelease Exchange64)

    FOREACH(FUNCTION ${FUNCTION_LIST})
      MESSAGE( STATUS "Checking for InterlockedCompare${FUNCTION}...")
      
      IF(FUNCTION STREQUAL "Exchange64")
        SET(64BITS_TYPE "LONGLONG" CACHE INTERNAL "64bits type longlong")
      ELSE(FUNCTION STREQUAL "Exchange64")
        SET(64BITS_TYPE "LONG" CACHE INTERNAL "64bits type long")
      ENDIF(FUNCTION STREQUAL "Exchange64")

      STRING(TOUPPER ${FUNCTION} FUNCTION_NAME)
      FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/InterlockedCompare${FUNCTION}_test.c
        "int main() {\n"
        "    ${64BITS_TYPE} dest = 0, exchange = 1, comperand = 0;\n"
        "    SetErrorMode(SEM_FAILCRITICALERRORS);\n"
        "    InterlockedCompare${FUNCTION}( &dest, exchange, comperand );\n"
        "    return (int)dest;\n"
        "    }\n")

      TRY_RUN (HAVE_INTERLOCKEDCOMPARE${FUNCTION_NAME} COMPILE_RESULT 
        "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/" 
        "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/InterlockedCompare${FUNCTION}_test.c")

      IF(HAVE_INTERLOCKEDCOMPARE${FUNCTION_NAME} STREQUAL "FAILED_TO_RUN" OR COMPILE_RESULT EQUAL FALSE)
        SET (HAVE_INTERLOCKEDCOMPARE${FUNCTION_NAME} 0 CACHE INTERNAL "HAVE_INTERLOCKEDCOMPARE${FUNCTION_NAME}")
        MESSAGE( STATUS "Checking for InterlockedCompare${FUNCTION}...failed")
      ELSE(HAVE_INTERLOCKEDCOMPARE${FUNCTION_NAME} STREQUAL "FAILED_TO_RUN" OR COMPILE_RESULT EQUAL FALSE)
        MESSAGE( STATUS "Checking for InterlockedCompare${FUNCTION}...done")
      ENDIF(HAVE_INTERLOCKEDCOMPARE${FUNCTION_NAME} STREQUAL "FAILED_TO_RUN" OR COMPILE_RESULT EQUAL FALSE)

    ENDFOREACH(FUNCTION)

  ELSE(GCC_OK)
    MESSAGE(FATAL_ERROR "No working GCC compiler found. Please check if Visual Studio VC is correctly installed.")
  ENDIF(GCC_OK)

  SET(MINGW_CHECK_DONE TRUE CACHE INTERNAL "MinGW check finished.")

ENDIF(NOT MINGW_CHECK_DONE)

OMPI_DEF_VAR(HAVE_INTERLOCKEDCOMPAREEXCHANGE "Whether we support 32 bits atomic operations on Windows" 0 0)
IF(${OMPI_COMPILER_BIT} STREQUAL "64")
  OMPI_DEF_VAR(HAVE_INTERLOCKEDCOMPAREEXCHANGE64 "Whether we support 64 bits atomic operations on Windows" 0 0)
ENDIF(${OMPI_COMPILER_BIT} STREQUAL "64")
OMPI_DEF_VAR(HAVE_INTERLOCKEDCOMPAREEXCHANGEACQUIRE "Whether we support 32 bits atomic operations on Windows" 0 0)
OMPI_DEF_VAR(HAVE_INTERLOCKEDCOMPAREEXCHANGERELEASE "Whether we support 32 bits atomic operations on Windows" 0 0)

# a few definitions for shared memory support
OMPI_DEF(MCA_COMMON_SM_WINDOWS 1 "Whether we have shared memory support for Windows or not." 0 1)
OMPI_DEF(MCA_COMMON_SM_SYSV 0 "Whether we have shared memory support for SYSV or not." 0 1)
OMPI_DEF(MCA_COMMON_SM_POSIX 0 "Whether we have shared memory support for POSIX or not." 0 1)
OMPI_DEF(OPAL_HAVE_POSIX_THREADS 1 "Do we have POSIX threads." 0 1)

OMPI_CHECK_INCLUDE_FILE (sys/uio.h HAVE_SYS_UIO_H)

OMPI_CHECK_INCLUDE_FILE (sys/time.h HAVE_SYS_TIME_H)

OMPI_CHECK_INCLUDE_FILE (windows.h HAVE_WINDOWS_H)

OMPI_CHECK_INCLUDE_FILE (winsock2.h HAVE_WINSOCK2_H)

OMPI_CHECK_INCLUDE_FILE (wdm.h HAVE_WDM_H)

OMPI_CHECK_INCLUDE_FILE (malloc.h HAVE_MALLOC_H)

OMPI_CHECK_INCLUDE_FILE (memory.h HAVE_MEMORY_H)

OMPI_CHECK_INCLUDE_FILE (signal.h HAVE_SIGNAL_H)

OMPI_CHECK_INCLUDE_FILE (stdarg.h HAVE_STDARG_H)

OMPI_CHECK_INCLUDE_FILE (stdint.h HAVE_STDINT_H)

OMPI_CHECK_INCLUDE_FILE (stdlib.h HAVE_STDLIB_H)

OMPI_CHECK_INCLUDE_FILE (string.h HAVE_STRING_H)

OMPI_CHECK_INCLUDE_FILE (sys/stat.h HAVE_SYS_STAT_H)

OMPI_CHECK_INCLUDE_FILE (sys/types.h HAVE_SYS_TYPES_H)

OMPI_CHECK_INCLUDE_FILE (time.h HAVE_TIME_H)

OMPI_CHECK_INCLUDE_FILE(stddef.h OPAL_STDC_HEADERS)

OMPI_CHECK_FUNCTION_EXISTS (ceil HAVE_CEIL)

OMPI_CHECK_FUNCTION_EXISTS (execve HAVE_EXECVE)

OMPI_CHECK_FUNCTION_EXISTS (isatty HAVE_ISATTY)

OMPI_CHECK_FUNCTION_EXISTS (vsnprintf HAVE_VSNPRINTF)

CHECK_C_TYPE_EXISTS(socklen_t SOCKLEN_T "winsock2.h;ws2tcpip.h")

CHECK_C_TYPE_EXISTS("struct sockaddr_in" STRUCT_SOCKADDR_IN "winsock2.h")

CHECK_C_TYPE_EXISTS("struct sockaddr_in6" STRUCT_SOCKADDR_IN6 "ws2tcpip.h")

CHECK_C_TYPE_EXISTS("struct sockaddr_storage" STRUCT_SOCKADDR_STORAGE "winsock2.h;ws2tcpip.h")

OMPI_CHECK_SYMBOL_EXISTS (AF_UNSPEC winsock2.h HAVE_DECL_AF_UNSPEC)

OMPI_CHECK_SYMBOL_EXISTS (PF_UNSPEC winsock2.h HAVE_DECL_PF_UNSPEC)

OMPI_CHECK_SYMBOL_EXISTS (AF_INET6 winsock2.h HAVE_DECL_AF_INET6)

OMPI_CHECK_SYMBOL_EXISTS (PF_INET6 winsock2.h HAVE_DECL_PF_INET6)
