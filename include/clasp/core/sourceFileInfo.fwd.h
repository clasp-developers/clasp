/*
    File: sourceFileInfo.fwd.h
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
#ifndef _core__source_file_info_fwd_H_
#define _core__source_file_info_fwd_H_

namespace core {
FORWARD(FileScope);
FORWARD(SourcePosInfo);

uint clasp_sourcePosInfo_fileHandle(SourcePosInfo_sp info);
size_t clasp_sourcePosInfo_filepos(SourcePosInfo_sp info);
uint clasp_sourcePosInfo_lineno(SourcePosInfo_sp info);
uint clasp_sourcePosInfo_column(SourcePosInfo_sp info);
};

#endif /* _core__source_file_info_fwd_H_ */
