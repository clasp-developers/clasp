#
# Copyright (c) 2009-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

#
# Functions that are re-written from config/opal_functions.m4, and to be continued...
#


# OPAL_WITH_OPTION_MIN_MAX_VALUE(NAME,DEFAULT_VALUE,LOWER_BOUND,UPPER_BOUND)
# Defines a variable OPAL_MAX_xxx, with "xxx" being specified as the first parameter.
# If not set at configure-time, the default-value is assumed.
# If set, value is checked against lower and upper bound
#
MACRO(OPAL_WITH_OPTION_MIN_MAX_VALUE NAME DEFAULT_VALUE LOWER_BOUND UPPER_BOUND)

  STRING(REPLACE "_" " " NAME_HELPER ${NAME})
  STRING(TOUPPER "OPAL_MAX_${NAME}" OPTION_VALUE_VAR)

  IF(NOT ${OPTION_VALUE_VAR})
    MESSAGE(STATUS "Checking maximum length of ${NAME_HELPER}...")
    MESSAGE(STATUS "Use default value ${DEFAULT_VALUE}.")

    SET(${NAME}_OLD_VALUE ${DEFAULT_VALUE} CACHE INTERNAL "the value from the last configure.")
  ENDIF(NOT ${OPTION_VALUE_VAR})

  # check if the value is changed.
  IF(NOT ${${OPTION_VALUE_VAR}} EQUAL ${NAME}_OLD_VALUE)
    MESSAGE(STATUS "Checking maximum length of ${NAME_HELPER}...")

    # check if the value is in the range.
    IF(NOT ${${OPTION_VALUE_VAR}} EQUAL 0 AND NOT ${${OPTION_VALUE_VAR}} EQUAL no AND NOT ${${OPTION_VALUE_VAR}} EQUAL NO)

      IF(${${OPTION_VALUE_VAR}} LESS ${LOWER_BOUND} OR ${${OPTION_VALUE_VAR}} GREATER ${UPPER_BOUND})
        MESSAGE(FATAL_ERROR "${OPTION_VALUE_VAR} is out of range(${LOWER_BOUND}-${UPPER_BOUND}). Cannot continue.")
      ENDIF(${${OPTION_VALUE_VAR}} LESS ${LOWER_BOUND} OR ${${OPTION_VALUE_VAR}} GREATER ${UPPER_BOUND})

    ELSE(NOT ${${OPTION_VALUE_VAR}} EQUAL 0 AND NOT ${${OPTION_VALUE_VAR}} EQUAL no AND NOT ${${OPTION_VALUE_VAR}} EQUAL NO)
      MESSAGE(FATAL_ERROR "Bad value for ${OPTION_VALUE_VAR}. Cannot continue.")
    ENDIF(NOT ${${OPTION_VALUE_VAR}} EQUAL 0 AND NOT ${${OPTION_VALUE_VAR}} EQUAL no AND NOT ${${OPTION_VALUE_VAR}} EQUAL NO)

    SET(${NAME}_OLD_VALUE ${${OPTION_VALUE_VAR}} CACHE INTERNAL "the value from the last configure.")

  ENDIF(NOT ${${OPTION_VALUE_VAR}} EQUAL ${NAME}_OLD_VALUE)
  
  OMPI_DEF_CACHE_VAR(${OPTION_VALUE_VAR} ${DEFAULT_VALUE} STRING
    "maximum length of ${NAME} (${LOWER_BOUND}-${UPPER_BOUND}). (Default: ${DEFAULT_VALUE}.)." 0 1)

ENDMACRO(OPAL_WITH_OPTION_MIN_MAX_VALUE NAME DEFAULT_VALUE LOWER_BOUND UPPER_BOUND)
