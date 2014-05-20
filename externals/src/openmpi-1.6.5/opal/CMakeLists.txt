# Copyright (c) 2007-2010 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


PROJECT (OPAL)



# Add include sub directory to gernerate opal_config.h
ADD_SUBDIRECTORY (include)

# These two files need to be compiled twice, one time with preprocessor CHECKSUM.
# Make one copy of them with another file name and add them into the project.
CONFIGURE_FILE(datatype/opal_datatype_pack.c ${PROJECT_BINARY_DIR}/datatype/opal_datatype_pack_checksum.c [COPYONLY])
CONFIGURE_FILE(datatype/opal_datatype_unpack.c ${PROJECT_BINARY_DIR}/datatype/opal_datatype_unpack_checksum.c [COPYONLY])

SET(DATATYPE_CHECKSUM_FILES ${PROJECT_BINARY_DIR}/datatype/opal_datatype_pack_checksum.c
                            ${PROJECT_BINARY_DIR}/datatype/opal_datatype_unpack_checksum.c )

SET_SOURCE_FILES_PROPERTIES(${DATATYPE_CHECKSUM_FILES}
                            PROPERTIES COMPILE_DEFINITIONS CHECKSUM)
SET(OPAL_SOURCE_FILES ${OPAL_SOURCE_FILES} ${DATATYPE_CHECKSUM_FILES})
SOURCE_GROUP(datatype FILES  ${DATATYPE_CHECKSUM_FILES})

IF(OPAL_WANT_LIBLTDL)
  INCLUDE(find_libltdl)
ENDIF(OPAL_WANT_LIBLTDL)

# Add source files

# Windows event files
INCLUDE (event/CMakeLists.txt)
FOREACH (FILE ${EVENT_FILES})
  SET (OPAL_EVENT_FILES ${OPAL_EVENT_FILES} ${PROJECT_SOURCE_DIR}/event/${FILE})
ENDFOREACH (FILE)
SOURCE_GROUP(event FILES ${OPAL_EVENT_FILES})

# Include headers.
FILE(GLOB OPAL_INCLUDE_FILES "include/*.h" "include/opal/*.h" "include/opal/sys/win32/*.h")
SOURCE_GROUP(include FILES ${OPAL_INCLUDE_FILES})

SET (OPAL_SOURCE_FILES
  ${OPAL_SOURCE_FILES}
  ${OPAL_EVENT_FILES}
  ${OPAL_INCLUDE_FILES}
)

# Recuresive search sub directories excluding mca, event, include and tools. 
# Add sources in different source groups.
INCLUDE(list_subdirs)
CHECK_SUBDIRS("${PROJECT_SOURCE_DIR}" OPAL_SUBDIRS)

SET(OPAL_EXCLUDE_SUBDIRS event include libltdl mca tools)

FOREACH(OPAL_SUBDIR ${OPAL_SUBDIRS})

  LIST(FIND OPAL_EXCLUDE_SUBDIRS ${OPAL_SUBDIR} OPAL_EXCLUDE_SUBDIR)

  IF(${OPAL_EXCLUDE_SUBDIR} EQUAL -1)
      
    FILE(GLOB_RECURSE OPAL_${OPAL_SUBDIR}_FILES 
      "${OPAL_SUBDIR}/*.h" "${OPAL_SUBDIR}/*.c" "${OPAL_SUBDIR}/*.cc" "${OPAL_SUBDIR}/*.cpp")
      
      SET (OPAL_SOURCE_FILES
        ${OPAL_SOURCE_FILES}
        ${OPAL_${OPAL_SUBDIR}_FILES}
      )
      
      SOURCE_GROUP("${OPAL_SUBDIR}" FILES ${OPAL_${OPAL_SUBDIR}_FILES})
      
  ENDIF(${OPAL_EXCLUDE_SUBDIR} EQUAL -1)

ENDFOREACH(OPAL_SUBDIR ${OPAL_SUBDIRS})

#only generate if it's not a tarball
IF(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/keyval/keyval_lex.c)
  ADD_FLEX_FILE(OPAL_SOURCE_FILES ./util/keyval/keyval_lex.l
    opal_util_keyval_yy "${PROJECT_BINARY_DIR}/util/keyval/")
ENDIF(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/keyval/keyval_lex.c)

IF(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/show_help_lex.c)
  ADD_FLEX_FILE(OPAL_SOURCE_FILES ./util/show_help_lex.l
    opal_show_help_yy "${PROJECT_BINARY_DIR}/util/")
ENDIF(NOT EXISTS ${PROJECT_SOURCE_DIR}/util/show_help_lex.c)


# Add source files from mca subdirectories.
INCLUDE (check_mca_subdirs)
SET (OPAL_SOURCE_FILES ${OPAL_SOURCE_FILES} ${MCA_FILES})


ADD_LIBRARY (libopen-pal ${OPAL_SOURCE_FILES})

IF(WINDOWS_MINGW)
  SET(CMAKE_CXX_STANDARD_LIBRARIES "${CMAKE_CXX_STANDARD_LIRARIES} -lpthread")
  SET(CMAKE_C_STANDARD_LIBRARIES "${CMAKE_CXX_STANDARD_LIRARIES} -lpthread")
ENDIF(WINDOWS_MINGW)

# Set compile flags for this target
IF (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libopen-pal PROPERTIES COMPILE_DEFINITIONS "_USRDLL;OPAL_EXPORTS"
                                    PREFIX "" IMPORT_PREFIX "")
  IF(OPAL_WANT_LIBLTDL AND LIBLTDL_FOUND)
    INCLUDE_DIRECTORIES(${LIBLTDL_INCLUDE_PATH})
    TARGET_LINK_LIBRARIES (libopen-pal Ws2_32.lib shlwapi.lib ${LIBLTDL_LIB})
  ELSE(OPAL_WANT_LIBLTDL AND LIBLTDL_FOUND)
    TARGET_LINK_LIBRARIES (libopen-pal Ws2_32.lib shlwapi.lib)
  ENDIF(OPAL_WANT_LIBLTDL AND LIBLTDL_FOUND)
ELSE (BUILD_SHARED_LIBS)
  SET_TARGET_PROPERTIES(libopen-pal PROPERTIES COMPILE_DEFINITIONS _LIB)
ENDIF(BUILD_SHARED_LIBS)

# Generate install_dirs.h
SET(prefix ${CMAKE_INSTALL_PREFIX})
SET(bindir ${prefix}/bin)
SET(sbindir ${prefix}/sbin)
SET(libexecdir ${prefix}/libexec)
SET(datarootdir ${prefix}/share)
SET(datadir ${prefix}/share)
SET(PACKAGE "openmpi")
SET(sysconfdir ${prefix}/etc)
SET(sharedstatedir ${prefix}/com)
SET(localstatedir ${prefix}/var)
SET(libdir ${prefix}/lib)
SET(includedir ${prefix}/include)
SET(infodir ${datarootdir}/info)
SET(mandir ${datarootdir}/man)
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/opal/mca/installdirs/config/install_dirs.h.in
  ${OpenMPI_BINARY_DIR}/opal/mca/installdirs/config/install_dirs.h)

# generate version.h
CONFIGURE_FILE(${OpenMPI_SOURCE_DIR}/opal/include/opal/version.h.in  ${OpenMPI_BINARY_DIR}/opal/include/opal/version.h)

ADD_SUBDIRECTORY(tools)

# Install libraries, shared files.
INSTALL(TARGETS libopen-pal
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
INSTALL(FILES runtime/help-opal-runtime.txt
    mca/base/help-mca-base.txt
    mca/base/help-mca-param.txt
    DESTINATION share/openmpi)

IF (OMPI_DEBUG_BUILD AND WINDOWS_VS)
  INSTALL(FILES ${OpenMPI_BINARY_DIR}/Debug/libopen-pal${CMAKE_DEBUG_POSTFIX}.pdb
    DESTINATION bin)
ENDIF (OMPI_DEBUG_BUILD AND WINDOWS_VS)
