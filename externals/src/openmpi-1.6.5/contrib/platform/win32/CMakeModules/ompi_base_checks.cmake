#
# Copyright (c) 2010      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# this file contains 
#    1. several wrappers for CMake macros, so that they will
#       do the job and write the configure temeplate file too.
#    2. a macro that checks the data types

# check include file
MACRO(OMPI_CHECK_INCLUDE_FILE FILE_NAME VAR_NAME)

  CHECK_INCLUDE_FILE(${FILE_NAME} ${VAR_NAME})
  OMPI_DEF_VAR(${VAR_NAME} "Define to 1 if you have <${FILE_NAME}> header file." 0 0)

ENDMACRO(OMPI_CHECK_INCLUDE_FILE FILE_NAME VAR_NAME)


# check if function exists
MACRO(OMPI_CHECK_FUNCTION_EXISTS FUNCTION_NAME VAR_NAME)

  CHECK_FUNCTION_EXISTS(${FUNCTION_NAME} ${VAR_NAME})
  OMPI_DEF_VAR(${VAR_NAME} "Define to 1 if you have the `${FUNCTION_NAME}' function." 0 0)

ENDMACRO(OMPI_CHECK_FUNCTION_EXISTS FUNCTION_NAME VAR_NAME)


# check if symbol exists
MACRO(OMPI_CHECK_SYMBOL_EXISTS SYMBOL_NAME FILE_NAMES VAR_NAME)

  CHECK_SYMBOL_EXISTS(${SYMBOL_NAME} "${FILE_NAMES}" ${VAR_NAME})
  OMPI_DEF_VAR(${VAR_NAME} "Define to 1 if you have the `${SYMBOL_NAME}' function." 0 0)

ENDMACRO(OMPI_CHECK_SYMBOL_EXISTS SYMBOL_NAME FILE_NAMES VAR_NAME)


# check if structure has member
MACRO(OMPI_CHECK_STRUCT_HAS_MEMBER STRUC_NAME MEMBER_NAME FILE_NAMES VAR_NAME)

  CHECK_SYMBOL_EXISTS(${STRUC_NAME} ${MEMBER_NAME} "${FILE_NAMES}" ${VAR_NAME})
  OMPI_DEF_VAR(${VAR_NAME} "Define to 1 if `${MEMBER_NAME}' is member of ${STRUC_NAME}." 0 0)

ENDMACRO(OMPI_CHECK_STRUCT_HAS_MEMBER STRUC_NAME MEMBER_NAME FILE_NAMES VAR_NAME)


# check type size and alignment
MACRO(OMPI_CHECK_TYPES TYPE_NAME VAR_NAME DEFAULT_TYPE LAN)

  CHECK_TYPE_SIZE(${TYPE_NAME} ${VAR_NAME})

  IF(NOT ${VAR_NAME}_CHECK_DONE)

    IF (HAVE_${VAR_NAME})
      SET(SIZEOF_${VAR_NAME} ${${VAR_NAME}} CACHE INTERNAL "Size of `${TYPE_NAME}'")
      C_GET_ALIGNMENT(${TYPE_NAME} ${LAN} ${VAR_NAME})
    ELSE(HAVE_${VAR_NAME})
      IF(NOT ${DEFAULT_TYPE} STREQUAL "" AND NOT ${DEFAULT_TYPE} STREQUAL "none")

        #Get the variable name of the default type size.
        STRING(TOUPPER "${DEFAULT_TYPE}" DEFAULT_TYPE_VAR)
        STRING(REPLACE " " "_" DEFAULT_TYPE_VAR ${DEFAULT_TYPE_VAR})

        IF(${SIZEOF_${DEFAULT_TYPE_VAR}} GREATER 0)
          MESSAGE(STATUS "Define it as '${DEFAULT_TYPE}'.")

          SET(${TYPE_NAME} ${DEFAULT_TYPE} CACHE INTERNAL "System support type for `${TYPE_NAME}'")
          SET(SIZEOF_${VAR_NAME} ${SIZEOF_${DEFAULT_TYPE_VAR}} CACHE INTERNAL "Size of `${TYPE_NAME}'")
          SET(OPAL_ALIGNMENT_${VAR_NAME} ${OPAL_ALIGNMENT_${DEFAULT_TYPE_VAR}})
        ENDIF(${SIZEOF_${DEFAULT_TYPE_VAR}} GREATER 0)

      ENDIF(NOT ${DEFAULT_TYPE} STREQUAL "" AND NOT ${DEFAULT_TYPE} STREQUAL "none")
    ENDIF (HAVE_${VAR_NAME})

    SET(${VAR_NAME}_CHECK_DONE 1 CACHE INTERNAL "check for `${VAR_NAME}' is done.")

  ENDIF(NOT ${VAR_NAME}_CHECK_DONE)

  IF(NOT ${DEFAULT_TYPE} STREQUAL "" AND NOT ${DEFAULT_TYPE} STREQUAL "none")
    OMPI_DEF_VAR(${TYPE_NAME} "Define to `${DEFAULT_TYPE}' if system types does not define." 0 0)
  ENDIF(NOT ${DEFAULT_TYPE} STREQUAL "" AND NOT ${DEFAULT_TYPE} STREQUAL "none")

  #CMake automatically sets the HAVE_type to TURE or FALSE,
  #but we need a number.
  IF(HAVE_${VAR_NAME})
    SET(HAVE_${VAR_NAME} 1)
  ENDIF(HAVE_${VAR_NAME})

  OMPI_DEF_VAR(SIZEOF_${VAR_NAME} "The size of `${TYPE_NAME}'" 0 0)
  OMPI_DEF_VAR(HAVE_${VAR_NAME} "Define to 1 if the system has the type `${TYPE_NAME}'" 0 0)
  OMPI_DEF_VAR(OPAL_ALIGNMENT_${VAR_NAME} "Alignment of type `${TYPE_NAME}'." 0 0)

ENDMACRO(OMPI_CHECK_TYPES TYPE VAR_NAME DEFAULT_TYPE LAN)
