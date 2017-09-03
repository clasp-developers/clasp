/*
    File: genericFunction.h
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
#ifndef _core_genericFunction_H_
#define _core_genericFunction_H_

#include <clasp/core/object.h>

namespace core {

// Arguments are passed in the multiple_values structure

  LCC_RETURN not_funcallable_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs);

  LCC_RETURN generic_function_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs);

  LCC_RETURN empty_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs);

 // See accessor.h for optimized_slot_reader_dispatch and optimized_slot_writer_dispatch
 
#if 0
  LCC_RETURN slot_reader_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs);

  LCC_RETURN slot_writer_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs);
#endif

  LCC_RETURN user_function_dispatch(gctools::Tagged tgf, gctools::Tagged tvargs);

  extern "C" LCC_RETURN apply_method0(T_O* func_tagged, T_O* arg0_tagged, T_O* arg1_tagged, T_O* rest_args_tagged);

};
#endif /* _core_genericFunction_H_ */
