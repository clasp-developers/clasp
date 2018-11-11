#include <clasp/core/foundation.h>
#include <clasp/gctools/hardErrors.h>

[[noreturn]] void throw_hard_error(const std::string& msg) {
  printf("%s\n", msg.c_str());
  abort();
}

[[noreturn]] void throw_hard_error_implement_me(const char* funcname, const char* filename, size_t lineno ) throw(HardError) {
  printf("%s:%d:%s %s %s %lu\n", __FILE__, __LINE__, __FUNCTION__, funcname, filename, lineno);
  abort();
}

[[noreturn]] void throw_hard_error_implement_me_message(const char* funcname, const char* filename, size_t lineno, const std::string& msg ) throw(HardError) {
  printf("%s:%d:%s %s %s %zu %s\n", __FILE__, __LINE__, __FUNCTION__, funcname, filename, lineno, msg.c_str());
  abort();
}

[[noreturn]] void throw_hard_error_failed_assertion(const char* assertion) {
  printf("%s:%d:%s %s\n", __FILE__, __LINE__, __FUNCTION__, assertion);
  abort();
}

[[noreturn]] void throw_hard_error_not_applicable_method(const char* method) {
  printf("%s:%d:%s %s\n", __FILE__, __LINE__, __FUNCTION__, method);
  abort();
}

[[noreturn]] void throw_hard_error_subclass_must_implement(const std::string& className, const std::string& method) { 
  printf("%s:%d:%s className: %s method: %s\n", __FILE__, __LINE__, __FUNCTION__, className.c_str(), method.c_str());
  abort();
}

[[noreturn]] void throw_hard_error_cannot_cast_tagged_pointer(const char* name, size_t kind) {
  printf("%s:%d:%s name: %s kind: %lu\n", __FILE__, __LINE__, __FUNCTION__, name, kind);
  abort();
}

[[noreturn]] void throw_hard_error_cast_failed(const char* type, const char* from) {
  printf("%s:%d:%s type: %s from: %s\n", __FILE__, __LINE__, __FUNCTION__, type, from);
  abort();
};


[[noreturn]] void throw_hard_error_bad_client(void* ptr) {
  printf("%s:%d Bad client pointer %p\n", __FILE__, __LINE__, ptr );
  abort();
}

[[noreturn]] void throw_hard_error_size_stack_damaged(size_t totalSize, size_t calcSize) {
  printf("%s:%d:%s totalSize: %lu calcSize: %lu\n", __FILE__, __LINE__, __FUNCTION__, totalSize, calcSize );
  abort();
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



  
  
