/*
    File: imageSaveLoad.h
*/


#ifndef imageSaveLoad_fwd_H //[
#define imageSaveLoad_fwd_H


namespace gctools {

struct image_save_load_init_s {
  Header_s*                           _headStart;
  gctools::clasp_ptr_t                _clientStart; // include vtable
  gctools::clasp_ptr_t                _clientEnd; // after client
  image_save_load_init_s(Header_s* head, gctools::clasp_ptr_t clientStart, gctools::clasp_ptr_t clientEnd) :
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


#endif // imageSaveLoad_fwd_H
