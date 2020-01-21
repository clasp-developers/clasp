#ifndef gctools_threadLocalStacks_H
#define gctools_threadLocalStacks_H

#include <clasp/core/clasp_gmpxx.h>
//#include <clasp/core/mpPackage.fwd.h>
namespace core {
  struct BignumExportBuffer {
  BignumExportBuffer() : buffer(NULL), bufferSize(0){};
    ~BignumExportBuffer() {
      if (this->buffer)
        free(this->buffer);
    };
    unsigned int *buffer = NULL;
    size_t bufferSize = 0;
    unsigned int *getOrAllocate(const mpz_class &bignum, int nail);
  };
};




namespace core {
#pragma GCC visibility push(default)
  class DynamicBindingStack {
  public:
   mutable gctools::Vec0<T_sp>           _ThreadLocalBindings;
  public:
    size_t new_binding_index() const;
    void release_binding_index(size_t index) const;
    uint32_t ensure_binding_index(const Symbol_O*) const;
    void expandThreadLocalBindings(size_t index);
    // Access
    T_sp thread_local_value(const Symbol_O*) const;
    void set_thread_local_value(T_sp, const Symbol_O*);
    bool thread_local_boundp(const Symbol_O*) const;
  private:
    T_sp* thread_local_reference(const uint32_t) const;
  };
#pragma GCC visibility pop

}; // namespace core

#endif
