#
# Copyright (c) 2007-2009 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


#
#  FLEX_FOUND - system has Flex
#  FLEX_EXECUTABLE - path of the flex executable
#  FLEX_VERSION - the version string, like "2.5.31"
#

MACRO(FIND_FLEX)
  
  IF(NOT FLEX_FOUND)
    MESSAGE(STATUS "Check for working flex...")

    # first find out if it's already installed somewhere
    FIND_PROGRAM(FLEX_EXECUTABLE_SYS NAMES flex)

    IF (WIN32)
      IF(FLEX_EXECUTABLE_SYS)
        SET(FLEX_EXECUTABLE ${FLEX_EXECUTABLE_SYS} CACHE FILEPATH "Flex")
      ELSE(FLEX_EXECUTABLE_SYS)
        IF(EXISTS ${CMAKE_SOURCE_DIR}/contrib/platform/win32/bin/flex.exe)
          # in case that no flex is installed, use our own version
          SET(FLEX_EXECUTABLE "${CMAKE_SOURCE_DIR}/contrib/platform/win32/bin/flex.exe" CACHE FILEPATH "Flex")
        ENDIF(EXISTS ${CMAKE_SOURCE_DIR}/contrib/platform/win32/bin/flex.exe)
      ENDIF(FLEX_EXECUTABLE_SYS)
    ELSE(WIN32)
      # nothing to do here at moment.
    ENDIF(WIN32)

    IF(FLEX_EXECUTABLE)
      SET(FLEX_FOUND TRUE CACHE INTERNAL "found flex")
      
      EXECUTE_PROCESS(COMMAND ${FLEX_EXECUTABLE} --version
        OUTPUT_VARIABLE _FLEX_VERSION
        )
      STRING (REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+" FLEX_VERSION "${_FLEX_VERSION}")
    ENDIF(FLEX_EXECUTABLE)
    
    IF(FLEX_FOUND)
      IF(NOT Flex_FIND_QUIETLY)
        MESSAGE(STATUS "Check for working flex...done")
      ENDIF(NOT Flex_FIND_QUIETLY)
    ELSE(FLEX_FOUND)
      IF(Flex_FIND_REQUIRED)
        MESSAGE(STATUS "Check for working flex...failed")
      ENDIF(Flex_FIND_REQUIRED)
    ENDIF(FLEX_FOUND)
  ENDIF(NOT FLEX_FOUND)
ENDMACRO(FIND_FLEX)

#
# Generate the corresponding C file from the lex file,
# and add it in to the source list for the target.
#

MACRO(ADD_FLEX_FILE _sourcelist _source _prefix _output_dir)

  GET_FILENAME_COMPONENT(_in ${_source} ABSOLUTE)
  GET_FILENAME_COMPONENT(_basename ${_source} NAME_WE)


  STRING(LENGTH "${_prefix}" _prefix_length)
  IF(NOT _prefix_length EQUAL 0)
    SET(_out ${_output_dir}/${_basename}.c)
    SET(_args -P${_prefix})
  ELSE(NOT _prefix_length EQUAL 0)
    SET(_out ${_output_dir}/${_basename}.c)
  ENDIF(NOT _prefix_length EQUAL 0)

  #MESSAGE("${FLEX_EXECUTABLE} -o${_out} ${_args} ${_in}")

  IF(NOT DEFINED ${_basename}_DONE)

    MESSAGE(STATUS "parse ${_basename} with flex...")

    FILE(MAKE_DIRECTORY ${_output_dir})
    EXECUTE_PROCESS(
      COMMAND ${FLEX_EXECUTABLE} -o${_out} ${_args} ${_in}
      OUTPUT_VARIABLE    OUTPUT
      RESULT_VARIABLE    RESULT
      ERROR_VARIABLE     ERROR
      )

    IF (NOT ${RESULT} STREQUAL "1")
      MESSAGE(STATUS "${ERROR}parse ${_basename} with flex...done")
    ELSE (NOT ${RESULT} STREQUAL "1")
      MESSAGE(FATAL_ERROR "${ERROR}parse ${_basename} with flex...failed")
    ENDIF (NOT ${RESULT} STREQUAL "1")

    SET(${_basename}_DONE TRUE CACHE INTERNAL "${_basename} flex parse done")

  ENDIF(NOT DEFINED ${_basename}_DONE)

  SET(${_sourcelist} ${${_sourcelist}} ${_out} )
  SET_SOURCE_FILES_PROPERTIES(${_out} PROPERTIES COMPILE_DEFINITIONS YY_NO_UNISTD_H)

ENDMACRO(ADD_FLEX_FILE)
