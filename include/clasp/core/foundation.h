#pragma once
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

// ------------------------------------------------------------
// ------------------------------------------------------------
// ------------------------------------------------------------
// ------------------------------------------------------------
// ------------------------------------------------------------
// ------------------------------------------------------------

#include <string>
#include <filesystem>

// We are using the fmt formatting library as an intermediate to std::format when it becomes available

#include <fmt/format.h>
#include <fmt/ostream.h>

//
// Trap failed BOOST_ASSERT invocations
// so that they call my code and throw a proper exception
//
#define BOOST_ENABLE_ASSERT_HANDLER 1

#include <clasp/core/core.h>

namespace core {
[[noreturn]] void lisp_throwLispError(const std::string& str);
[[noreturn]] void lisp_error_simple(const char* functionName, const char* fileName, int lineNumber, const string& fmt);
[[noreturn]] void lisp_error_simple(const char* functionName, const char* fileName, int lineNumber, const std::string& str);
void lisp_debugLogWrite(const char* fileName, const char* funcName, uint lineNumber, uint column, const std::string& message,
                        uint debugFlags = DEBUG_CPP_FUNCTION);
}; // namespace core

template <typename Char> struct fmt::formatter<core::T_sp, Char> : fmt::formatter<fmt::basic_string_view<Char>> {
  template <typename FormatContext> auto format(const core::T_sp& o, FormatContext& ctx) const -> typename FormatContext::iterator {
    return fmt::formatter<fmt::basic_string_view<Char>>::format(_rep_(o), ctx);
  }
};

template <typename Char> struct fmt::formatter<core::Symbol_sp, Char> : fmt::formatter<fmt::basic_string_view<Char>> {
  template <typename FormatContext>
  auto format(const core::Symbol_sp& o, FormatContext& ctx) const -> typename FormatContext::iterator {
    return fmt::formatter<fmt::basic_string_view<Char>>::format(_rep_(o), ctx);
  }
};

template <> struct fmt::formatter<gctools::GCStampEnum> : fmt::formatter<int> {
  template <typename FormatContext>
  auto format(const gctools::GCStampEnum& o, FormatContext& ctx) const -> typename FormatContext::iterator {
    return fmt::formatter<int>::format((int)o, ctx);
  }
};
