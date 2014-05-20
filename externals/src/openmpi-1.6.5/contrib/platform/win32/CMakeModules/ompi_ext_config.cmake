# Copyright (c) 2010      High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

FILE(GLOB OMPI_EXT_BASE_FILES ${PROJECT_SOURCE_DIR}/mpiext/*.h ${PROJECT_SOURCE_DIR}/mpiext/*.c)
SOURCE_GROUP(mpiext FILES ${OMPI_EXT_BASE_FILES})

SET(OMPI_EXT_FILES ${OMPI_EXT_FILES} ${OMPI_EXT_BASE_FILES})

CHECK_SUBDIRS("${PROJECT_SOURCE_DIR}/mpiext" OMPI_EXT_SUBDIRS)

FOREACH(OMPI_MPIEXT_COMPONENT ${OMPI_MPIEXT_COMPONENTS})
  LIST(FIND OMPI_EXT_SUBDIRS ${OMPI_MPIEXT_COMPONENT} VALID_COMPONENT)
  
  IF(NOT VALID_COMPONENT EQUAL -1)
    FILE(GLOB ${OMPI_MPIEXT_COMPONENT}_FILES
      ${PROJECT_SOURCE_DIR}/mpiext/${OMPI_MPIEXT_COMPONENT}/*.h ${PROJECT_SOURCE_DIR}/mpiext/${OMPI_MPIEXT_COMPONENT}/*.c)
    SOURCE_GROUP(mpiext\\${OMPI_MPIEXT_COMPONENT} FILES ${${OMPI_MPIEXT_COMPONENT}_FILES})

    FILE(GLOB ${OMPI_MPIEXT_COMPONENT}_C_FILES
      ${PROJECT_SOURCE_DIR}/mpiext/${OMPI_MPIEXT_COMPONENT}/c/*.h ${PROJECT_SOURCE_DIR}/mpiext/${OMPI_MPIEXT_COMPONENT}/c/*.c)
    SOURCE_GROUP(mpiext\\${OMPI_MPIEXT_COMPONENT}\\c FILES ${${OMPI_MPIEXT_COMPONENT}_C_FILES})

    SET(OMPI_EXT_FILES ${OMPI_EXT_FILES} ${${OMPI_MPIEXT_COMPONENT}_FILES} ${${OMPI_MPIEXT_COMPONENT}_C_FILES})
  ENDIF(NOT VALID_COMPONENT EQUAL -1)
ENDFOREACH(OMPI_MPIEXT_COMPONENT ${OMPI_MPIEXT_COMPONENTS})


# write out static-component.h for this mca.
FILE(WRITE "${PROJECT_BINARY_DIR}/mpiext/static-components.h" 
      "/*
 * $HEADER$
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern \"C\" {
#endif



const ompi_mpiext_component_t *ompi_mpiext_components[] = {

  NULL
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
  ")
