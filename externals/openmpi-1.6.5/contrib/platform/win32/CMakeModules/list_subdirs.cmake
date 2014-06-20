#
# Copyright (c) 2009-2010    High Performance Computing Center Stuttgart, 
#                            University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# list the sub directories of current directories
# save the list of subdirs in OUTPUT_VARIABLE

MACRO(CHECK_SUBDIRS CURRENT_DIR OUTPUT_VARIABLE)

  # change the path into standard Windows format,
  # so that we can support UNC paths too.
  STRING(REPLACE "/" "\\" DIR_FORMATTED ${CURRENT_DIR})

  EXECUTE_PROCESS (COMMAND cmd /C dir /AD /B ${DIR_FORMATTED}
                   WORKING_DIRECTORY  ${PROJECT_BINARY_DIR}
                   OUTPUT_VARIABLE    OUTPUT
                   RESULT_VARIABLE    RESULT
                   ERROR_VARIABLE     ERROR)

  IF(NOT "${OUTPUT}" STREQUAL "")
    STRING (REGEX MATCHALL "[a-zA-Z0-9_]+" ${OUTPUT_VARIABLE} ${OUTPUT})
  ENDIF(NOT "${OUTPUT}" STREQUAL "")

ENDMACRO(CHECK_SUBDIRS CURRENT_DIR OUTPUT_VARIABLE)
