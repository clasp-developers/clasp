/*
    File: clasp_ffi_package.h
*/

/*
Copyright (c) 2016, Christian E. Schafmeister

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
#if !defined( __CLASP_CORE_FFI_PACKAGE_H__ )
#define __CLASP_CORE_FFI_PACKAGE_H__ __FILE__" $Id$"

#include <clasp/core/object.h>
#include <clasp/core/clasp_ffi_package.fwd.h>

#if defined( __cplusplus )

namespace clasp_ffi {

  void initialize_clasp_ffi_package( void );

};

#endif // __cplusplus
#endif // __CLASP_CORE_FFI_PACKAGE_H__
