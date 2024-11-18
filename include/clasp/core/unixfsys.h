#pragma once
/*
    File: unixfsys.h
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
#include <csignal>
#include <sys/select.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/pathname.fwd.h>

#if !defined(_TARGET_OS_DARWIN) && !defined(_TARGET_OS_FREEBSD)
#if !defined(FD_COPY)
#define FD_COPY(dest, src) memcpy((dest), (src), sizeof *(dest))
#endif
#endif

namespace core {

Integer_sp clasp_file_len(int f);
int clasp_backup_open(const char* filename, int option, int mode);

Pathname_sp cl__truename(T_sp filespec);
T_sp cl__probe_file(T_sp filespec);
Symbol_sp core__file_kind(T_sp filename, bool follow_links = true);
T_mv af_renameFile(T_sp oldn, T_sp newn, T_sp if_exists = kw::_sym_supersede);
T_sp cl__delete_file(T_sp filespec);
T_mv cl__rename_file(T_sp oldn, T_sp newn, T_sp if_exists);
String_sp clasp_strerror(int e);
bool clasp_has_file_position(int filedescriptor);

T_sp core__mkstemp(String_sp thetemplate);
void core__chmod(T_sp file, T_sp mode);

}; // namespace core

namespace ext {
core::Str8Ns_sp ext__getcwd();
};
