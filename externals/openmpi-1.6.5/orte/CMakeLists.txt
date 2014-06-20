# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


PROJECT (ORTE)


# Recuresive search sub directories excluding mca, event, include and tools. 
# Add sources in different source groups.
INCLUDE(list_subdirs)
CHECK_SUBDIRS("${PROJECT_SOURCE_DIR}" ORTE_SUBDIRS)

SET(ORTE_EXCLUDE_SUBDIRS mca test tools)

FOREACH(ORTE_SUBDIR ${ORTE_SUBDIRS})

  LIST(FIND ORTE_EXCLUDE_SUBDIRS ${ORTE_SUBDIR} ORTE_EXCLUDE_SUBDIR)

  IF(${ORTE_EXCLUDE_SUBDIR} EQUAL -1)
      
      FILE(GLOB_RECURSE ORTE_${ORTE_SUBDIR}_FILES 
        "${ORTE_SUBDIR}/*.h" "${ORTE_SUBDIR}/*.c" "${ORTE_SUBDIR}/*.cc" "${ORTE_SUBDIR}/*.cpp")
                  
      SET (ORTE_SOURCE_FILES
        ${ORTE_SOURCE_FILES}
        ${ORTE_${ORTE_SUBDIR}_FILES}
      )
      
      SOURCE_GROUP("${ORTE_SUBDIR}" FILES ${ORTE_${ORTE_SUBDIR}_FILES})
      
  ENDIF(${ORTE_EXCLUDE_SUBDIR} EQUAL -1)

ENDFOREACH(ORTE_SUBDIR ${ORTE_SUBDIRS})


#only generate if it's not a tarball
IF(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/hostfile/hostfile_lex.c)
  ADD_FLEX_FILE(ORTE_SOURCE_FILES ./util/hostfile/hostfile_lex.l "orte_util_hostfile_"
    "${PROJECT_BINARY_DIR}/util/hostfile/")
ENDIF(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/hostfile/hostfile_lex.c)


INCLUDE (check_mca_subdirs)
SET (ORTE_SOURCE_FILES ${ORTE_SOURCE_FILES} ${MCA_FILES})

ADD_LIBRARY (libopen-rte ${ORTE_SOURCE_FILES})

ADD_DEPENDENCIES(libopen-rte libopen-pal)

IF(WINDOWS_MINGW)
  SET(CMAKE_CXX_STANDARD_LIBRARIES "${CMAKE_CXX_STANDARD_LIRARIES} -lpthread")
  SET(CMAKE_C_STANDARD_LIBRARIES "${CMAKE_CXX_STANDARD_LIRARIES} -lpthread")
ENDIF(WINDOWS_MINGW)

# Set compile flags for this target
IF (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libopen-rte PROPERTIES COMPILE_DEFINITIONS "_USRDLL;ORTE_EXPORTS;OPAL_IMPORTS" 
                                    COMPILE_FLAGS "${OMPI_CXX_LAN_FLAG}" PREFIX "" IMPORT_PREFIX "") 
  TARGET_LINK_LIBRARIES(libopen-rte libopen-pal Ws2_32.lib shlwapi.lib) 
ELSE (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libopen-rte PROPERTIES COMPILE_DEFINITIONS "${OMPI_C_DEF_PRE}_LIB"
                                    COMPILE_FLAGS "${OMPI_CXX_LAN_FLAG}")
ENDIF(BUILD_SHARED_LIBS)

# generate orte_config.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/orte/include/orte_config.h.in  ${OpenMPI_BINARY_DIR}/orte/include/orte_config.h)

# generate version.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/orte/include/orte/version.h.in  ${OpenMPI_BINARY_DIR}/orte/include/orte/version.h)

ADD_SUBDIRECTORY(tools)

# Install libraries and shared files.
INSTALL(TARGETS libopen-rte
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
INSTALL(FILES orted/help-orted.txt runtime/help-orte-runtime.txt
  util/hostfile/help-hostfile.txt util/dash_host/help-dash-host.txt
  # we dont build mca/odls/default, but we need the help file anyway.
  mca/odls/default/help-odls-default.txt
  DESTINATION share/openmpi)

IF (OMPI_DEBUG_BUILD AND WINDOWS_VS)
  INSTALL(FILES ${OpenMPI_BINARY_DIR}/Debug/libopen-rte${CMAKE_DEBUG_POSTFIX}.pdb
    DESTINATION bin)
ENDIF (OMPI_DEBUG_BUILD AND WINDOWS_VS)

INSTALL(FILES ${OpenMPI_SOURCE_DIR}/orte/etc/openmpi-default-hostfile
  DESTINATION etc)

