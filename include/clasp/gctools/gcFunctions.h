#ifndef gcFunctions_H
#define gcFunctions_H

namespace gctools {
  /*! Return true if any debugging flags are set and a description of all debugging flag
settings in string stream */
  bool debugging_configuration(bool setFeatures, bool buildReport, stringstream& ss);

  void gctools__cleanup(bool verbose=false);

void initialize_gc_functions();

 Fixnum core__header_kind(core::T_sp obj);
 Fixnum core__header_stamp(core::T_sp obj);
 
};

#endif
