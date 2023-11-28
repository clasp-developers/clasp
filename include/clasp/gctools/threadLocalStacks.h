#pragma once

#include <clasp/core/clasp_gmpxx.h>
// #include <clasp/core/mpPackage.fwd.h>
namespace core {
struct BignumExportBuffer {
  BignumExportBuffer() : buffer(NULL), bufferSize(0){};
  ~BignumExportBuffer() {
    if (this->buffer)
      free(this->buffer);
  };
  unsigned int* buffer = NULL;
  size_t bufferSize = 0;
  unsigned int* getOrAllocate(const mpz_class& bignum, int nail);
};

#pragma GCC visibility push(default)
class DynamicBindingStack {
public:
  DynamicBindingStack()
      : _ThreadLocalBindings(true) // don't allocate GC memory ctor
        {};                        //
public:
  mutable gctools::Vec0<T_sp> _ThreadLocalBindings;

public:
  size_t new_binding_index() const;
  void release_binding_index(size_t index) const;
  // Access
  T_sp thread_local_value(uint32_t index) const;
  void set_thread_local_value(T_sp, uint32_t);
  bool thread_local_boundp(uint32_t index) const;

public:
  T_sp* thread_local_reference(const uint32_t) const;
};
#pragma GCC visibility pop

}; // namespace core
