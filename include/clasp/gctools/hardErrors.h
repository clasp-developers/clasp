/*
    File: hardErrors.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */

#ifndef gc_hardErrors_H
#define gc_hardErrors_H

void dbg_hook(const char *errorString);

namespace core {
  [[noreturn]]void errorFormatted(const char *errorString);
  [[noreturn]]void errorFormatted(const string &msg);
};

class HardError {
 private:
  string _Message;
 public:
    //HardError(const char *sourceFile, const char *functionName, uint lineNumber, const boost::format &fmt);
  HardError(const std::string& msg);
  HardError(const char* file, const char* func, int lineno, const char* msg);
  string message();
};

[[noreturn]] void throw_hard_error(const std::string& msg);
[[noreturn]] void throw_hard_error_not_applicable_method(const char* msg);
[[noreturn]] void throw_hard_error_bad_client(void* ptr);
[[noreturn]] void throw_hard_error_bad_layout_command(int cmd);
[[noreturn]] void throw_hard_error_side_stack_damaged(size_t totalSize, size_t calcSize);
[[noreturn]] void throw_hard_error_mps_bad_result(int result);
[[noreturn]] void throw_hard_error_failed_assertion(const char* assertion);
[[noreturn]] void throw_hard_error_cast_failed(const char* type, const char* from);
[[noreturn]] void throw_hard_error_cannot_cast_tagged_pointer(const char* name, size_t kind);
[[noreturn]] void throw_hard_error_implement_me(const char* funcname, const char* filename, size_t lineno) throw(HardError);
[[noreturn]] void throw_hard_error_implement_me_message(const char* funcname, const char* filename, size_t lineno, const string& msg) throw(HardError);
[[noreturn]] void throw_hard_error_subclass_must_implement(const std::string& className, const std::string& method);

#define HARD_IMPLEMENT_ME() throw_hard_error_implement_me(__FUNCTION__, __FILE__, __LINE__);
#define HARD_IMPLEMENT_MEF(msg) throw_hard_error_implement_me_message(__FUNCTION__, __FILE__, __LINE__, msg);
#define SUBCLASS_MUST_IMPLEMENT() throw_hard_error_subclass_must_implement(lisp_classNameAsString(core::instance_class(this->asSmartPtr())),__FUNCTION__);
#define SUBIMP() SUBCLASS_MUST_IMPLEMENT();

#if 1 
#define THROW_HARD_ERROR(fmt)                                        \
  {                                                                  \
    std::string str = (fmt).str(); \
    dbg_hook(str.c_str());                                   \
    ::core::errorFormatted(str);                                     \
  }
#define HARD_UNREACHABLE() throw_hard_error("Unreachable");
#define HARD_SUBCLASS_MUST_IMPLEMENT() throw_hard_error("Subclass must implement");
#ifdef DEBUG_ASSERT
#define GCTOOLS_ASSERT(x)                                       \
  {                                                             \
    if (!(x))                                                   \
      throw_hard_error_failed_assertion( #x ); \
  };
#define GCTOOLS_ASSERTF(x, msg) \
  {                             \
    if (!(x))                   \
      throw_hard_error_failed_assertion( #x " " msg); \
  };
#else
#define GCTOOLS_ASSERT(x)
#define GCTOOLS_ASSERTF(x, f)
#endif

#endif
#endif // gc_hardErrors_H
