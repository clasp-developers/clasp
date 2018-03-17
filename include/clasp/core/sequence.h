/*
    File: sequence.h
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
#ifndef _core_Sequence_H
#define _core_Sequence_H

#include <clasp/core/object.h>
#include <clasp/core/sequence.fwd.h>
#include <clasp/core/corePackage.fwd.h>

namespace core {
  size_t cl__length(T_sp arg);

  T_sp cl__reverse(T_sp obj);
  T_sp cl__nreverse(T_sp obj);

  T_sp cl__elt(T_sp sequence, size_t index);
  T_sp core_setf_elt(T_sp sequence, size_t index, T_sp value);

  T_sp cl__subseq(T_sp sequence, size_t start, T_sp end);
  T_sp core_setf_subseq(T_sp sequence, size_t start, T_sp end, T_sp newSubseq);

  T_sp cl__copy_seq(T_sp seq);


};
#endif /* _core_Sequence_H */
