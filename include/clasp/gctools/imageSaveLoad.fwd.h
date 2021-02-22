/*
    File: imageSaveLoad.h
*/


#ifndef imageSaveLoad_fwd_H //[
#define imageSaveLoad_fwd_H


namespace gctools {

struct image_save_load_init_s {
  gctools::clasp_ptr_t _vtableSegmentStart;
  Header_s::StampWtagMtag        _stamp_wtag_mtag;
  gctools::clasp_ptr_t                _clientStart; // include vtable
  gctools::clasp_ptr_t                _clientEnd; // after client
  image_save_load_init_s(gctools::clasp_ptr_t vtableSegmentStart, Header_s::StampWtagMtag stamp, gctools::clasp_ptr_t clientStart, gctools::clasp_ptr_t clientEnd) :
    _vtableSegmentStart(vtableSegmentStart),
    _stamp_wtag_mtag(stamp),
    _clientStart(clientStart),
    _clientEnd(clientEnd) {};

  void fill(void* client) {
    printf("%s:%d:%s copying source: %p end: %p --> %p\n",
           __FILE__, __LINE__, __FUNCTION__,
           (void*)this->_clientStart,
           (void*)this->_clientEnd,
           (void*)client);
    // Fixup the vtable
    memcpy((void*)client, // skip vtable
           (void*)this->_clientStart,
           this->_clientEnd-this->_clientStart);
    uintptr_t vtableOffset = *(uintptr_t*)client;
    vtableOffset += (uintptr_t)this->_vtableSegmentStart;
    *(uintptr_t*)client = vtableOffset;
  }
  void fill_no_virtual(void* client) {
    printf("%s:%d:%s filling from %p bytes: %lu\n",
           __FILE__, __LINE__, __FUNCTION__,
           (void*)this->_clientStart,
           this->_clientEnd-this->_clientStart);
    memcpy((void*)((char*)client), // skip vtable
           (void*)this->_clientStart,
           this->_clientEnd-this->_clientStart);
  }
};
  
};


#endif // imageSaveLoad_fwd_H
