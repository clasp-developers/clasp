#ifndef gcFunctions_H
#define gcFunctions_H

namespace gctools {
  /*! Return true if any debugging flags are set and a description of all debugging flag
settings in string stream */
  bool debugging_configuration(stringstream& ss);

  void gctools__cleanup(bool verbose=false);

void initialize_gc_functions();
};

#endif
