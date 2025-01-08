#pragma once

#include <string>

/*
    File: snapshotSaveLoad.h
*/

namespace snapshotSaveLoad {

typedef enum { InfoOp, TestOp, SaveOp, LoadOp } FixupOperation_;

struct Fixup;

FixupOperation_ operation(Fixup* fixup);

//   noStomp - forwarding uses a separate map of pointer -> forwarded-pointer.
//                This is slow and is meant for snapshot_save and to facilitate debugging.
//   stomp   - The forwarding pointer is written into the pointer address.
//                This is fast and is meant for snapshot_load
//   testStomp - maintain a map of pointer -> forwarded-pointer AND write the forwarding
//               pointer into the pointer address and compare the two on every operation.
//               This is to test stomp.
//   undef - this means global_forwardingKind wasn't set yet.
enum class ForwardingEnum { undef, stomp, noStomp, testStomp };

struct SaveLispAndDie {
  std::string _FileName;
  bool _Executable;
  std::string _LibDir;
  bool   _Exit;  // Set to true unless debugging
  ForwardingEnum _ForwardingKind;
  bool           _TestMemory;
  SaveLispAndDie(const std::string& filename, bool executable, const std::string& libDir, bool ep=true, ForwardingEnum fk=ForwardingEnum::noStomp, bool tm=true)
      : _FileName(filename), _Executable(executable), _LibDir(libDir), _Exit(ep), _ForwardingKind(fk), _TestMemory(tm) {};
};

struct snapshot_save_load_init_s {
  gctools::Header_s* _headStart;
  gctools::clasp_ptr_t _clientStart; // include vtable
  gctools::clasp_ptr_t _clientEnd;   // after client
  snapshot_save_load_init_s(gctools::Header_s* head, gctools::clasp_ptr_t clientStart, gctools::clasp_ptr_t clientEnd)
      : _headStart(head), _clientStart(clientStart), _clientEnd(clientEnd){};

  void fill(void* client) { memcpy((void*)client, (void*)this->_clientStart, this->_clientEnd - this->_clientStart); }
};

}; // namespace snapshotSaveLoad
