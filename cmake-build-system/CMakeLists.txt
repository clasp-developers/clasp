#
#  The start of a CMake build system for clasp by faheem May 24, 2015
#
cmake_minimum_required (VERSION 3.2.1)
project (Claspcl)
set(CMAKE_VERBOSE_MAKEFILE on)
set (Corrmodel_VERSION_MAJOR 0.3)
set (Corrmodel_VERSION_MINOR test3)
find_package(PythonLibs 2.7 EXACT REQUIRED)
find_package(PythonInterp 2.7 EXACT REQUIRED)

set (CLBIND_PATH src/clbind)
set (CORE_PATH src/core)

#set (PUMP_PATHS ${CLBIND_PATH}/clbind_constructor_functoids.pmp ${CLBIND_PATH}/clbind_functoids.pmp ${CLBIND_PATH}/clbind_methoids.pmp ${CLBIND_PATH}/clbind_static_members.pmp ${CLBIND_PATH}/constructors.pmp ${CORE_PATH}/applyToActivationFrame.pmp ${CORE_PATH}/clbind_templates.pmp ${CORE_PATH}/external_wrappers_indirect_methoids.pmp ${CORE_PATH}/wrappers_functoids.pmp ${CORE_PATH}/wrappers_methoids.pmp)

set (PUMP_PATHS ${CLBIND_PATH}/clbind_constructor_functoids.pmp ${CLBIND_PATH}/clbind_functoids.pmp ${CLBIND_PATH}/clbind_methoids.pmp ${CLBIND_PATH}/clbind_static_members.pmp ${CORE_PATH}/applyToActivationFrame.pmp ${CORE_PATH}/clbind_templates.pmp ${CORE_PATH}/external_wrappers_indirect_methoids.pmp ${CORE_PATH}/wrappers_functoids.pmp ${CORE_PATH}/wrappers_methoids.pmp)

set (GENERATED_SUBPATH /include/generate)

set(PUMP_EXECUTABLE ${CMAKE_SOURCE_DIR}/src/common/pump.py)

foreach(i ${PUMP_PATHS})
  get_filename_component(DIRNAME ${i} DIRECTORY)
  get_filename_component(FILENAME ${i} NAME)
  string(REPLACE "." ";" FILENAMELIST ${FILENAME})
  list(GET FILENAMELIST 0 BASENAME)
  set(HEADER ${CMAKE_CURRENT_BINARY_DIR}/${DIRNAME}/${GENERATED_SUBPATH}/${BASENAME}.h)
  set(PUMP_FILE ${CMAKE_SOURCE_DIR}/${i})
  add_custom_command(OUTPUT ${HEADER} COMMAND ${PYTHON_EXECUTABLE} ${PUMP_EXECUTABLE} ${PUMP_FILE} ${HEADER} DEPENDS ${HEADER})
  string(REGEX REPLACE "/" "." HEADER_TARGET ${HEADER}_target)
  add_custom_target(${HEADER_TARGET} ALL DEPENDS ${HEADER})
ENDFOREACH()

#########################################################################

# fix-scraping:
#         for d in src/*/; do cd "$$d"; export PYTHONPATH="$$PWD:$$PYTHONPATH"; python ../../src/common/symbolScraper.py symbols_scraped.inc *.h *.cc *.scrape.inc; cd ../..; done

##########################################################################

#include the subdirectory containing our libs
#add_subdirectory (Hello)
#include_directories(Hello)
#indicate the entry point for the executable

# set (COMMON randutil.cc util.cc gendata_fn.cc model.cc score_fn.cc search_fn.cc pval.cc print.cc)
# set(PIF cpparr_conv_pif.cc cppmap_conv_pif.cc cpppair_conv_pif.cc cppset_conv_pif.cc cpptup_conv_pif.cc cppvec_conv_pif.cc cppunicode_conv_pif.cc util_pif.cc gendata_fn_pif.cc model_pif.cc score_fn_pif.cc search_fn_pif.cc pval_pif.cc main.cc)
# set(LIBS blitz Rmath python2.7 m boost_python)

# set (COMPILE_OPTIONS -ftemplate-depth-100 -fno-strict-aliasing -fno-common -ansi -Wextra -Wall -Werror -Wno-unused-function -Wc++0x-compat -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -std=c++11 -march=native -mtune=native -mfpmath=sse -msse3 -O3)
# add_executable (simann_ex ${COMMON} simann_ex.cc)
# target_link_libraries(simann_ex ${LIBS})
# target_compile_options(simann_ex PUBLIC ${COMPILE_OPTIONS})

# add_library (cpplib MODULE ${COMMON} ${PIF})
# set_target_properties(cpplib PROPERTIES PREFIX "")
# # Indicate which libraries to include during the link process.
# target_link_libraries(cpplib ${LIBS})
# target_compile_options(cpplib PUBLIC ${COMPILE_OPTIONS})
# target_include_directories(cpplib PUBLIC ${Boost_INCLUDE_DIRS} ${PYTHON_INCLUDE_DIRS})

# install (TARGETS cpplib simann_ex
#   LIBRARY DESTINATION lib
#   RUNTIME DESTINATION bin
#   )

# install(PROGRAMS cm-genetable cm-neglogpp cm-neglogpptable cm-sim cm-cdf getfasta init_db cmload DESTINATION bin)

# uninstall target
# configure_file(
#   "${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
#   "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
#   IMMEDIATE @ONLY)

#add_custom_target(uninstall
#      COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)
