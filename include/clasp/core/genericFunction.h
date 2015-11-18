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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>

namespace core {

// Arguments are passed in the multiple_values structure

LCC_RETURN notFuncallableDispatch(Instance_sp gf, VaList_sp vargs);

LCC_RETURN generic_function_dispatch(Instance_sp gf, VaList_sp vargs);

LCC_RETURN slotReaderDispatch(Instance_sp gf, VaList_sp vargs);

LCC_RETURN slotWriterDispatch(Instance_sp gf, VaList_sp vargs);

LCC_RETURN userFunctionDispatch(Instance_sp gf, VaList_sp vargs);

void initialize_genericFunction();
};
#endif /* _core_genericFunction_H_ */
