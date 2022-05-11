#ifndef gcFunctions_H
#define gcFunctions_H

namespace gctools {
core::T_sp core__instance_stamp(core::T_sp obj);
};

namespace gctools {
  /*! Return true if any debugging flags are set and a description of all debugging flag
settings in string stream */
bool debugging_configuration(bool setFeatures, bool buildReport, stringstream& ss);

void gctools__garbage_collect();
void gctools__cleanup(bool verbose=false);

void initialize_gc_functions();

core::T_mv gctools__ensure_valid_object(core::T_mv obj);

core::T_mv cl__room(core::T_sp x);


};

#endif
