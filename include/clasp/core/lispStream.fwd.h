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
#ifndef lispStream_fwd_H
#define lispStream_fwd_H
namespace core {
FORWARD(Stream);
FORWARD(AnsiStream);
FORWARD(StringStream);
FORWARD(StringInputStream);
FORWARD(StringOutputStream);
FORWARD(SynonymStream);
FORWARD(TwoWayStream);
FORWARD(BroadcastStream);
FORWARD(EchoStream);
FORWARD(FileStream);
FORWARD(IOFileStream);
FORWARD(IOStreamStream);
FORWARD(ConcatenatedStream);

void clasp_write_string(const string &str, T_sp strm);
inline void clasp_write_format(boost::format const &f, T_sp strm) { clasp_write_string(f.str(), strm); }

#define STDOUT_BFORMAT(x) core::clasp_write_string((x).str(), cl::_sym_STARstandard_outputSTAR->symbolValue())
}
#endif
