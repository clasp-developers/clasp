#include <clasp/core/foundation.h>
#include <clasp/gctools/hardErrors.h>

[[noreturn]] void throw_hard_error(const std::string& msg) {
  printf("%s\n", msg.c_str());
  throw(HardError(msg));
}

[[noreturn]] void throw_hard_error_implement_me(const char* funcname, const char* filename, size_t lineno ) throw(HardError) {
  throw_hard_error((BF("Implement me: %s  %s  %u") % funcname % filename % lineno ).str());
}

[[noreturn]] void throw_hard_error_implement_me_message(const char* funcname, const char* filename, size_t lineno, const std::string& msg ) throw(HardError) {
  throw_hard_error((BF("Implement me: %s  %s  %u - %s") % funcname % filename % lineno % msg ).str());
}

[[noreturn]] void throw_hard_error_failed_assertion(const char* assertion) {
  throw_hard_error((BF("Failed assertion: %s") % assertion).str());
}

[[noreturn]] void throw_hard_error_not_applicable_method(const char* method) {
  throw_hard_error((BF("Not applicable method: %s") % method).str());
}

[[noreturn]] void throw_hard_error_subclass_must_implement(const std::string& className, const std::string& method) {
  throw_hard_error((BF("Subclass %s must implement method %s") % className % method ).str());
}

[[noreturn]] void throw_hard_error_cannot_cast_tagged_pointer(const char* name, size_t kind) {
  throw_hard_error((BF("Cannot cast tagged_pointer from %s/%zu to some other type (check with debugger)") % name % kind).str());
}

[[noreturn]] void throw_hard_error_cast_failed(const char* type, const char* from) {
  throw_hard_error((BF("TaggedCast<Type*,From*> failed due to an illegal cast To* = %s  From* = %s") % type % from).str());
};


[[noreturn]] void throw_hard_error_bad_client(void* ptr) {
  printf("%s:%d Bad client pointer %p\n", __FILE__, __LINE__, ptr );
  abort();
}

[[noreturn]] void throw_hard_error_size_stack_damaged(size_t totalSize, size_t calcSize) {
  throw_hard_error((BF("The side-stack is damaged _TotalSize = %u  calcSize = %u") % totalSize % calcSize ).str());
}

[[noreturn]] void throw_hard_error_mps_bad_result(int result) {
  throw_hard_error((BF("MPS returned a bad result -> %d") % result).str());
}
[[noreturn]] void throw_hard_error_bad_layout_command(int cmd) {
  throw_hard_error((BF("The Layout_code table contained an illegal command -> %d") % cmd).str());
}
[[noreturn]] void throw_hard_error_mps_bad_result(const char* method) {
  throw_hard_error((BF("Method is not applicable %s") % method).str());
}


HardError::HardError(const string &msg) {
  this->_Message = msg;
}

HardError::HardError(const char* file, const char* func, int lineno, const char* msg) {
  stringstream ss;
  ss << file << "/" << func << ":" << lineno << " " << msg;
  this->_Message = ss.str();
}

string HardError::message() {
  return this->_Message;
}



  
  
