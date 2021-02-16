/*
    File: imageSaveLoad.h
*/


#ifndef imageSaveLoad_fwd_H //[
#define imageSaveLoad_fwd_H


namespace gctools {

struct image_save_load_init_s {
  Header_s* _header;
  size_t    _size;
  char*     _object_data_start; // after vtable
  size_t    _object_data_size; 
  image_save_load_init_s(Header_s* header, size_t sz) : _header(header), _size(sz) {};
  image_save_load_init_s() : _header(NULL), _size(0) {};

  void fill(void* object) {
    printf("%s:%d:%s filling from %p bytes: %lu\n",
           __FILE__, __LINE__, __FUNCTION__,
           (void*)this->_object_data_start,
           this->_object_data_size);
    memcpy((void*)((char*)object+sizeof(void*)), // skip vtable
           (void*)this->_object_data_start,
           this->_object_data_size);
  }
  void fill_no_virtual(void* object) {
    printf("%s:%d:%s filling from %p bytes: %lu\n",
           __FILE__, __LINE__, __FUNCTION__,
           (void*)this->_object_data_start,
           this->_object_data_size);
    memcpy((void*)((char*)object), // skip vtable
           (void*)this->_object_data_start,
           this->_object_data_size);
  }
};
  
};


#endif // imageSaveLoad_fwd_H
