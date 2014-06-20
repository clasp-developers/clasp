# Copyright (c) 2008-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_FIND_TYPE(type, [list of c types], abort if not found,
#                target size, variable to set)
# -----------------------------------------------------------

MACRO(OMPI_FIND_TYPE TYPE TYPE_LIST ABORT TARGET_SIZE OUTPUT_VARIABLE)
  SET(oft_abort_on_fail "${ABORT}")
  SET(oft_target_size ${TARGET_SIZE})

  
  MESSAGE(STATUS "Check corresponding C type of ${TYPE}...")
  SET(oft_real_type "")

  IF(NOT "${oft_target_size}" STREQUAL "")

    FOREACH(oft_type ${TYPE_LIST})
        STRING(REGEX REPLACE " " "_" oft_type_name ${oft_type})
        STRING(TOUPPER ${oft_type_name} oft_type_name)
        IF(SIZEOF_${oft_type_name} EQUAL oft_target_size)
          SET(oft_real_type ${oft_type})
          BREAK()
        ENDIF(SIZEOF_${oft_type_name} EQUAL oft_target_size)
    ENDFOREACH(oft_type ${TYPE_LIST})

  ENDIF(NOT "${oft_target_size}" STREQUAL "")

  IF("${oft_real_type}" STREQUAL "")
    SET(OUTPUT_VARIABLE "")
    MESSAGE(STATUS "*** Did not find corresponding C type of ${TYPE}")
    IF("${oft_abort_on_fail}" STREQUAL "yes")
      MESSAGE(FATAL_ERROR "Cannot continue.")
    ENDIF("${oft_abort_on_fail}" STREQUAL "yes")
  ELSE("${oft_real_type}" STREQUAL "")
    SET(${OUTPUT_VARIABLE} "${oft_real_type}")
    MESSAGE(STATUS "Check corresponding C type of ${TYPE}... ${oft_real_type}") 
  ENDIF("${oft_real_type}" STREQUAL "")

ENDMACRO(OMPI_FIND_TYPE TYPE TYPE_LIST ABORT TARGET_SIZE OUTPUT_VARIABLE)
