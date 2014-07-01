#ifndef main_gc_interface_fwd_H
#define main_gc_interface_fwd_H
extern "C" {
    extern const char* obj_name( gctools::GCKindEnum kind );

    extern void registerLoadTimeValuesRoot(core::LoadTimeValues_O** ptr);

};
#endif
