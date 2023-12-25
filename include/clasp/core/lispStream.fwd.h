#pragma once
/*
    File: lispStream.fwd.h
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
namespace core {
FORWARD(Stream);
FORWARD(AnsiStream);
FORWARD(BroadcastStream);
FORWARD(ConcatenatedStream);
FORWARD(EchoStream);
FORWARD(StringStream);
FORWARD(StringInputStream);
FORWARD(StringOutputStream);
FORWARD(SynonymStream);
FORWARD(TwoWayStream);
FORWARD(FileStream);
FORWARD(PosixFileStream);
FORWARD(CFileStream);
#ifdef ECL_WINSOCK
FORWARD(WinsockStream);
#endif
#ifdef CLASP_MS_WINDOWS_HOST
FORWARD(ConsoleStream);
#endif

void clasp_write_string(const string& str, T_sp strm);

#define STDOUT_BFORMAT(x) core::clasp_write_string((x).str(), ::cl::_sym_STARstandard_outputSTAR->symbolValue())

#define CPP_SOURCE() (fmt::format("{}:{}:{}", __FILE__, __LINE__, __FUNCTION__))

void lisp_write(const std::string& s);
void lisp_write(const std::string& s, T_sp stream);

} // namespace core
