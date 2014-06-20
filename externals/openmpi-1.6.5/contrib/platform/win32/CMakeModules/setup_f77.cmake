# Copyright (c) 2008-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# first try to find a f77 compiler, will be checked when f77 support is enabled.

# There might be a bug in CMake, the CMAKE_GENERATOR_FC is set to "ifort" by default,
# which causes CMake can't find the correct Fortran compiler.
# We have to set CMAKE_GENERATOR_FC empty.
SET(CMAKE_GENERATOR_FC "")
include(CMakeDetermineFortranCompiler)
include(CMakeFortranInformation)

IF(OMPI_WANT_F77_BINDINGS AND NOT F77_SETUP_DONE)

  # Get the size of a C size_t; that's the size of the Fortran
  # MPI_OFFSET_KIND
  MESSAGE( STATUS "Checking size of C size_t...")
  FILE (WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_sizeof_sizet.c"
        "#include <stddef.h>
       int main() {return sizeof(size_t);}
      ")
  TRY_RUN(SIZEOF_C_SIZE_T COMPILE_RESULT "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/"
        "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/check_sizeof_sizet.c")
  SET(OMPI_MPI_OFFSET_KIND ${SIZEOF_C_SIZE_T} CACHE INTERNAL "MPI_OFFSET_KIND")

  # MPI_INTEGER_KIND is always 4
  SET(OMPI_MPI_INTEGER_KIND 4 CACHE INTERNAL "MPI_INTEGER_KIND") 

  # MPI_ADDRESS_KIND is the size of a C (void*), which is built-in to
  # Cmake.
  SET(OMPI_MPI_ADDRESS_KIND ${CMAKE_SIZEOF_VOID_P} CACHE INTERNAL "MPI_ADDRESS_KIND") 

  # Finally, set the number of integers in MPI_STATUS_SIZE (it's 4
  # integers plus 1 size_t).
  MATH( EXPR __ompi_f_status "(4+(${SIZEOF_C_SIZE_T}/4))")
  SET(OMPI_FORTRAN_STATUS_SIZE ${__ompi_f_status} CACHE INTERNAL "MPI_STATUS_SIZE") 

  GET_FILENAME_COMPONENT(F77_NAME ${CMAKE_Fortran_COMPILER} NAME)
  GET_FILENAME_COMPONENT(F77_PATH ${CMAKE_Fortran_COMPILER} PATH)

  SET(F77 ${F77_NAME} CACHE INTERNAL "Name of the fortran compiler.")

  # Default compiler settings.
  IF(${F77} STREQUAL "ifort.exe")
    #settings for Intel Fortran
    SET(F77_OPTION_COMPILE "/c" CACHE INTERNAL
      "Fortran compiler option for compiling without linking.")
    SET(F77_OUTPUT_OBJ "/Fo" CACHE INTERNAL
      "Fortran compiler option for setting object file name.")
    SET(F77_OUTPUT_EXE "/Fe" CACHE INTERNAL
      "Fortran compiler option for setting executable file name.")
    SET(F77_DYNAMIC_FLAG_DEBUG "/MDd" CACHE INTERNAL
      "Compile flag for using dynamically-loaded, multithread C runtime (Debug).")
    SET(F77_DYNAMIC_FLAG "/MD" CACHE INTERNAL
      "Compile flag for using dynamically-loaded, multithread C runtime.")

    IF(NOT "$ENV{IFORT_COMPILER11}" STREQUAL "")
      SET(IFORT_LIB_PATH "$ENV{IFORT_COMPILER11}/lib/")
    ELSEIF(NOT "$ENV{IFORT_COMPILER12}" STREQUAL "")
      SET(IFORT_LIB_PATH "$ENV{IFORT_COMPILER12}/compiler/lib/")
    ENDIF(NOT "$ENV{IFORT_COMPILER11}" STREQUAL "")

    IF(CMAKE_CL_64)
      SET(F77_LIB_PATH "${IFORT_LIB_PATH}/intel64")
    ELSE(CMAKE_CL_64)
      SET(F77_LIB_PATH "${IFORT_LIB_PATH}/ia32")
    ENDIF(CMAKE_CL_64)

    IF(NOT F77_LIB_PATH)
      IF(CMAKE_CL_64)
        FIND_LIBRARY(F77_IFCONSOL_LIB ifconsol.lib PATHS ${F77_PATH}/../../intel64)
      ELSE(CMAKE_CL_64)
        FIND_LIBRARY(F77_IFCONSOL_LIB ifconsol.lib PATHS ${F77_PATH}/../../ia32)
      ENDIF(CMAKE_CL_64)
      GET_FILENAME_COMPONENT(F77_LIB_PATH ${F77_IFCONSOL_LIB} PATH)
      UNSET(F77_IFCONSOL_LIB CACHE)
    ELSE(NOT F77_LIB_PATH)
      STRING(REPLACE "\\" "/" F77_LIB_PATH ${F77_LIB_PATH})
    ENDIF(NOT F77_LIB_PATH)
  ELSEIF(${F77} STREQUAL "g95.exe")
    #settings for G95
    SET(F77_OPTION_COMPILE "-c" CACHE INTERNAL
      "Fortran compiler option for compiling without linking.")
    SET(F77_OUTPUT_OBJ "-o" CACHE INTERNAL
      "Fortran compiler option for setting object file name.")
    SET(F77_OUTPUT_EXE "-o" CACHE INTERNAL
      "Fortran compiler option for setting executable file name.")
  ELSE(${F77} STREQUAL "ifort.exe")
    # in other case, let user specify their fortran configrations.
    SET(F77_OPTION_COMPILE "-c" CACHE STRING
      "Fortran compiler option for compiling without linking.")
    SET(F77_OUTPUT_OBJ "-o" CACHE STRING
      "Fortran compiler option for setting object file name.")
    SET(F77_OUTPUT_EXE "-o" CACHE STRING
      "Fortran compiler option for setting executable file name.")
    SET(F77_LIB_PATH "" CACHE PATH
      "Library path for the fortran compiler")
    SET(F77_INCLUDE_PATH "" CACHE PATH
      "Include path for the fortran compiler")
  ENDIF(${F77} STREQUAL "ifort.exe")

  # Export env variables for fortran compiler.
  SET(ENV{PATH} "${C_COMPILER_PATH};${F77_PATH};$ENV{PATH}")
  SET(ENV{LIB} "${C_COMPILER_LIB};${F77_LIB_PATH};$ENV{LIB}")
  SET(ENV{INCLUDE} "${C_COMPILER_INCLUDE};${F77_INCLUDE_PATH};$ENV{INCLUDE}")
  SET(ENV{LIBPATH} "${C_COMPILER_LIBPATH};$ENV{LIBPATH}")

  # make sure the compiler actually works, if not cross-compiling
  MESSAGE(STATUS "Checking for working Fortran compiler...")
  FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testFortranCompiler.f
       "\t PROGRAM TESTFortran \n"
       "\t PRINT *, 'Hello' \n"
       "\t END \n")

  # lets use execute_process to run the compile test
  EXECUTE_PROCESS(COMMAND ${F77} testFortranCompiler.f
                  WORKING_DIRECTORY  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp
                  OUTPUT_VARIABLE    OUTPUT
                  RESULT_VARIABLE    RESULT
                  ERROR_VARIABLE     ERROR)


  IF(RESULT)
    SET(F77_SETUP_DONE FALSE CACHE INTERNAL "f77 setup done.")
    MESSAGE(STATUS "${OUTPUT}\n${ERROR}")
    MESSAGE(STATUS "Fortran compiler ${F77} can't compile a simple fortran program.")
    MESSAGE(FATAL_ERROR "Cannot continue. Please check Fortran compiler installation, or disable Fortran 77 support.")
  ELSE(RESULT)
    MESSAGE(STATUS "Checking for working Fortran compiler...${F77}")
    SET(F77_SETUP_DONE TRUE CACHE INTERNAL "f77 setup done.")
  ENDIF(RESULT)

  INCLUDE(F77_find_ext_symbol_convention)
  # make sure we know the linking convention
  # this macro will also test linking with C code
  OMPI_F77_FIND_EXT_SYMBOL_CONVENTION()

ELSEIF(NOT OMPI_WANT_F77_BINDINGS)
    SET(OMPI_F77_DOUBLE_UNDERSCORE 0
      CACHE INTERNAL "external symbol convention - double underscore")
    SET(OMPI_F77_SINGLE_UNDERSCORE 0
      CACHE INTERNAL "external symbol convention - single underscore")
    SET(OMPI_F77_CAPS 0
      CACHE INTERNAL "external symbol convention - captital")
    SET(OMPI_F77_PLAIN 0
      CACHE INTERNAL "external symbol convention - plain")
    
    UNSET(SYMBOL_CONVENTION_CHECK_DONE CACHE)
    UNSET(F77_OPTION_COMPILE CACHE)
    UNSET(F77_OUTPUT_OBJ CACHE)
    UNSET(F77_OUTPUT_EXE CACHE)
    UNSET(F77_LIB_PATH CACHE)
    UNSET(F77_INCLUDE_PATH CACHE)
    UNSET(F77_IFCONSOL_LIB CACHE)
    UNSET(F77_SETUP_DONE CACHE)
ENDIF(OMPI_WANT_F77_BINDINGS AND NOT F77_SETUP_DONE)

# a few definitions needed by OMPI_F77_FIND_EXT_SYMBOL_CONVENTION check.
OMPI_DEF_VAR(OMPI_F77_DOUBLE_UNDERSCORE "Whether fortran symbols have a trailing double underscore or not." 0 1)
OMPI_DEF_VAR(OMPI_F77_SINGLE_UNDERSCORE "Whether fortran symbols have a trailing single underscore or not." 0 1)
OMPI_DEF_VAR(OMPI_F77_CAPS "Whether fortran symbols are all caps or not." 0 1)
OMPI_DEF_VAR(OMPI_F77_PLAIN "Whether fortran symbols have no trailing underscore or not." 0 1)

