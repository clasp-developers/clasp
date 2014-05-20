set(DirInput-RESULT 1)
set(DirInput-STDERR "is a directory")
set(DirOutput-RESULT 0)
set(DirOutput-STDERR "DirOutput test file")
set(Relative-RESULT 0)
set(Relative-STDERR "Relative test file")
set(BadArg-RESULT 1)
set(BadArg-STDERR "called with incorrect number of arguments")

include("/Users/meister/Development/cando/externals/src/cmake-2.8.5/Tests/CMakeTests/CheckCMakeTest.cmake")
check_cmake_test(ConfigureFile
  DirInput
  DirOutput
  Relative
  BadArg
  )
