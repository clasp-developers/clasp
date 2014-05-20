FILE(REMOVE_RECURSE
  "CMakeFiles/documentation"
  "../Docs/ctest.txt"
  "../Docs/cpack.txt"
  "../Docs/ccmake.txt"
  "../Docs/cmake.txt"
)

# Per-language clean rules from dependency scanning.
FOREACH(lang)
  INCLUDE(CMakeFiles/documentation.dir/cmake_clean_${lang}.cmake OPTIONAL)
ENDFOREACH(lang)
