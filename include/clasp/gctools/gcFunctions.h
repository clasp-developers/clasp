#ifndef gcFunctions_H
#define gcFunctions_H


#ifdef DEBUG_TRACK_UNWINDS
extern std::atomic<size_t> global_unwind_count;
extern std::atomic<size_t> global_ReturnFrom_count;
extern std::atomic<size_t> global_DynamicGo_count;
extern std::atomic<size_t> global_CatchThrow_count;
#endif

namespace gctools {
  /*! Return true if any debugging flags are set and a description of all debugging flag
settings in string stream */
bool debugging_configuration(bool setFeatures, bool buildReport, stringstream& ss);

void gctools__garbage_collect();
void gctools__cleanup(bool verbose=false);

void initialize_gc_functions();

Fixnum core__header_kind(core::T_sp obj);
Fixnum core__header_stamp(core::T_sp obj);


};

extern "C" {
#ifdef DEBUG_FLOW_TRACKER
#define MAX_STACK_SIZE_FLOW_TRACKER 16
extern void initialize_flow_tracker();
extern size_t next_flow_tracker_counter();
extern void flow_tracker_about_to_throw(Fixnum tracker);
extern void flow_tracker_last_throw_backtrace_dump();
extern void flow_tracker_flush();
extern void flow_tracker_close();
extern bool global_flow_tracker_on;
#endif
};

#endif
