# Install script for directory: /Users/meister/Development/cando/externals/src/cmake-2.8.5/Utilities

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "/usr/local")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "")
  ENDIF(BUILD_TYPE)
  MESSAGE(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
ENDIF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)

# Set the component getting installed.
IF(NOT CMAKE_INSTALL_COMPONENT)
  IF(COMPONENT)
    MESSAGE(STATUS "Install component: \"${COMPONENT}\"")
    SET(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  ELSE(COMPONENT)
    SET(CMAKE_INSTALL_COMPONENT)
  ENDIF(COMPONENT)
ENDIF(NOT CMAKE_INSTALL_COMPONENT)

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/man/man1" TYPE FILE FILES
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake.1"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmakecommands.1"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmakecompat.1"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmakeprops.1"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmakepolicies.1"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmakevars.1"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmakemodules.1"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/ctest.1"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cpack.1"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/ccmake.1"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/doc/cmake-2.8" TYPE FILE FILES
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake.html"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-policies.html"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-properties.html"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-variables.html"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-modules.html"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-commands.html"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-compatcommands.html"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/ctest.html"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cpack.html"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/ccmake.html"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake.txt"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake.docbook"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-policies.txt"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-properties.txt"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-variables.txt"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-modules.txt"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-commands.txt"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cmake-compatcommands.txt"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/ctest.txt"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/ctest.docbook"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cpack.txt"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/cpack.docbook"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/ccmake.txt"
    "/Users/meister/Development/cando/externals/src/cmake-2.8.5/Docs/ccmake.docbook"
    )
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  INCLUDE("/Users/meister/Development/cando/externals/src/cmake-2.8.5/Utilities/Doxygen/cmake_install.cmake")
  INCLUDE("/Users/meister/Development/cando/externals/src/cmake-2.8.5/Utilities/KWStyle/cmake_install.cmake")

ENDIF(NOT CMAKE_INSTALL_LOCAL_ONLY)

