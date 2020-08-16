/*
    File: foundation.h
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


#ifndef FOUNDATION_H //[
#define FOUNDATION_H

#include <string>
#include <boost/format.hpp>
#include <boost/filesystem.hpp>



namespace boost_filesystem = boost::filesystem;

#define BF boost::format



//
// Trap failed BOOST_ASSERT invocations
// so that they call my code and throw a proper exception
//
#define BOOST_ENABLE_ASSERT_HANDLER 1

#include <clasp/core/core.h>

namespace core {
  [[noreturn]]void lisp_throwLispError(const std::string &str);
  [[noreturn]]void lisp_throwLispError(const boost::format &fmt);
  [[noreturn]]void errorFormatted(boost::format fmt);
  void lisp_write(const boost::format &fmt, T_sp stream);
  [[noreturn]]void lisp_error_simple(const char *functionName, const char *fileName, int lineNumber, const boost::format &fmt);
  void lisp_debugLogWrite(const char *fileName, const char *funcName, uint lineNumber, uint column, const boost::format &message, uint debugFlags = DEBUG_CPP_FUNCTION);
};

#define TRY_BOOST_FORMAT_STRING(__fmt, __fmt_str)                                                   \
  string __fmt_str;                                                                                 \
  try {                                                                                             \
    __fmt_str = __fmt.str();                                                                        \
  } catch (const std::exception &exc) {                                                             \
    lisp_throwLispError(BF("Error - could not get string from boost::format --> %s") % exc.what()); \
  }


#endif //]
