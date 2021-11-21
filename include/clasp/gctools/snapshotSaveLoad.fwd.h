/*
    File: snapshotSaveLoad.h
*/


#ifndef snapshotSaveLoad_fwd_H //[
#define snapshotSaveLoad_fwd_H


namespace snapshotSaveLoad {


typedef enum { InfoOp, TestOp, SaveOp, LoadOp } FixupOperation_;

struct Fixup;

FixupOperation_ operation(Fixup* fixup);

struct snapshot_save_load_init_s {
  gctools::Header_s*                  _headStart;
  gctools::clasp_ptr_t                _clientStart; // include vtable
  gctools::clasp_ptr_t                _clientEnd; // after client
  snapshot_save_load_init_s(gctools::Header_s* head, gctools::clasp_ptr_t clientStart, gctools::clasp_ptr_t clientEnd) :
    _headStart(head),
    _clientStart(clientStart),
    _clientEnd(clientEnd) {};

  void fill(void* client) {
    memcpy((void*)client,
           (void*)this->_clientStart,
           this->_clientEnd-this->_clientStart);
  }
};
  
};


#endif // snapshotSaveLoad_fwd_H
