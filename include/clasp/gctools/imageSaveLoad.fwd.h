/*
    File: imageSaveLoad.h
*/


#ifndef imageSaveLoad_fwd_H //[
#define imageSaveLoad_fwd_H


namespace gctools {

struct image_save_load_init_s {
  Header_s::StampWtagMtag        _stamp_wtag_mtag;
  gctools::clasp_ptr_t                _clientStart; // include vtable
  gctools::clasp_ptr_t                _clientEnd; // after client
  image_save_load_init_s(Header_s::StampWtagMtag stamp, gctools::clasp_ptr_t clientStart, gctools::clasp_ptr_t clientEnd) :
    _stamp_wtag_mtag(stamp),
    _clientStart(clientStart),
    _clientEnd(clientEnd) {};

  void fill(void* client) {
    printf("%s:%d:%s copying source: %p end: %p  size: %lu --> %p\n",
           __FILE__, __LINE__, __FUNCTION__,
           (void*)this->_clientStart,
           (void*)this->_clientEnd,
           (this->_clientEnd - this->_clientStart),
           (void*)client);
#if 1
    memcpy((void*)client,
           (void*)this->_clientStart,
           this->_clientEnd-this->_clientStart);
#endif
  }
};
  
};


#endif // imageSaveLoad_fwd_H
