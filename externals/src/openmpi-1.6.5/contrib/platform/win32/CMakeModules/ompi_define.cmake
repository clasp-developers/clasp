#
# Copyright (c) 2010      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

#Generate content for opal_config.h.cmake
#
#  NAME:        name of the variable to be defined in configure file
#  VALUE:       value to be defined for the variable
#  DESC:        description of the definition
#  IS_STR:      whether this variable should be defined with quotation marks
#  FORCE_DEF:   whether use "#cmakedefiine" or just "#define" in the line
#  VAR_FORMAT:  whether we should use the variable name instead of the value
#               in the template file.

MACRO(APPEND_CONFIG_FILE NAME VAR_NAME VALUE DESC IS_STR FORCE_DEF VAR_FORMAT)

  #We don't want to always generate the template file, but only when required.
  IF(NOT WRITE_CONFIG_DONE)

    UNSET(APPEND_STRING)

    IF(${VAR_FORMAT})
      IF(${IS_STR})
        IF(${FORCE_DEF})
          SET(APPEND_STRING "/* ${DESC} */\n#define ${NAME} \"\${${VAR_NAME}}\"\n\n")
        ELSE(${FORCE_DEF})
          SET(APPEND_STRING "/* ${DESC} */\n#cmakedefine ${NAME} \"\${${VAR_NAME}}\"\n\n")
        ENDIF(${FORCE_DEF})
      ELSE(${IS_STR})
        IF(${FORCE_DEF})
          SET(APPEND_STRING "/* ${DESC} */\n#define ${NAME} \${${VAR_NAME}}\n\n")
        ELSE(${FORCE_DEF})
          SET(APPEND_STRING "/* ${DESC} */\n#cmakedefine ${NAME} \${${VAR_NAME}}\n\n")
        ENDIF(${FORCE_DEF})
      ENDIF(${IS_STR})
    ELSE(${VAR_FORMAT})
      IF(${IS_STR})
        IF(${FORCE_DEF})
          SET(APPEND_STRING "/* ${DESC} */\n#define ${NAME} \"${VALUE}\"\n\n")
        ELSE(${FORCE_DEF})
          SET(APPEND_STRING "/* ${DESC} */\n#cmakedefine ${NAME} \"${VALUE}\"\n\n")
        ENDIF(${FORCE_DEF})
      ELSE(${IS_STR})
        IF(${FORCE_DEF})
          SET(APPEND_STRING "/* ${DESC} */\n#define ${NAME} ${VALUE}\n\n")
        ELSE(${FORCE_DEF})
          SET(APPEND_STRING "/* ${DESC} */\n#cmakedefine ${NAME} ${VALUE}\n\n")
        ENDIF(${FORCE_DEF})
      ENDIF(${IS_STR})
    ENDIF(${VAR_FORMAT})

    FILE(APPEND ${OpenMPI_BINARY_DIR}/opal/include/opal_config.h.cmake
      ${APPEND_STRING})
  ENDIF(NOT WRITE_CONFIG_DONE)

ENDMACRO(APPEND_CONFIG_FILE NAME VAR_NAME VALUE DESC IS_STR FORCE_DEF VAR_FORMAT)


#define a name with value and
#write the line in template file.
MACRO(OMPI_DEF NAME VALUE DESC IS_STR FORCE_DEF)

  SET(${NAME} ${VALUE})

  APPEND_CONFIG_FILE(${NAME} ${NAME} "${VALUE}" ${DESC} ${IS_STR} ${FORCE_DEF} 0)

ENDMACRO(OMPI_DEF NAME VALUE DESC IS_STR FORCE_DEF)


#define a name with variable and
#write the line in template file.
MACRO(OMPI_DEF_VAR NAME DESC IS_STR FORCE_DEF)

  APPEND_CONFIG_FILE(${NAME} ${NAME} "" ${DESC} ${IS_STR} ${FORCE_DEF} 1)

ENDMACRO(OMPI_DEF_VAR NAME DESC IS_STR FORCE_DEF)


#define/cache a name with value and
#write the line in template file.
MACRO(OMPI_DEF_CACHE NAME VALUE CACHE_TYPE DESC IS_STR FORCE_DEF)

  SET(${NAME} ${VALUE} CACHE ${CACHE_TYPE} "${DESC}")

  APPEND_CONFIG_FILE(${NAME} ${NAME} ${VALUE} ${DESC} ${IS_STR} ${FORCE_DEF} 0)

ENDMACRO(OMPI_DEF_CACHE NAME VALUE OPT DESC IS_STR FORCE_DEF)


#define/cache a name with variable and
#write the line in template file.
MACRO(OMPI_DEF_CACHE_VAR NAME VALUE CACHE_TYPE DESC IS_STR FORCE_DEF)

  SET(${NAME} ${VALUE} CACHE ${CACHE_TYPE} "${DESC}")

  APPEND_CONFIG_FILE(${NAME} ${NAME} "" ${DESC} ${IS_STR} ${FORCE_DEF} 1)

ENDMACRO(OMPI_DEF_CACHE_VAR NAME VALUE OPT DESC IS_STR FORCE_DEF)


#add an configure option and
#write the line in template file.
MACRO(OMPI_DEF_OPT NAME DESC DEFAULT_VAL)

  UNSET(APPEND_STRING)

  OPTION(${NAME} "${DESC}" ${DEFAULT_VAL})

  IF(${${NAME}} STREQUAL "OFF")
    SET(${NAME}_VAL 0)
  ELSE(${${NAME}} STREQUAL "OFF")
    SET(${NAME}_VAL 1)
  ENDIF(${${NAME}} STREQUAL "OFF")

  APPEND_CONFIG_FILE(${NAME} ${NAME}_VAL "" ${DESC} 0 1 1)

ENDMACRO(OMPI_DEF_OPT NAME DESC DEFAULT_VAL)
