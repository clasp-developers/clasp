/*
    File: multipleValues.cc
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
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/array.h>
#include <clasp/core/wrappers.h>
namespace core {

const int MultipleValues::MultipleValuesLimit;

void MultipleValues::initialize(){};

void dump_values_pos(T_sp v, const char* name, int n)
{
  if (n>4) {
    if (_sym_STARdebug_valuesSTAR &&
        _sym_STARdebug_valuesSTAR->boundP() &&
        _sym_STARdebug_valuesSTAR->symbolValue().notnilp()) {
      if (strcmp(name,"v0")==0) {
        printf("--------------------- values dump ---------------\n");
      }
      printf("%s:%d  VALUES[%s] -> %s\n",  __FILE__, __LINE__, name, _rep_(v).c_str() );
#if 0
      if (strcmp(name,"v1")==0 && v==kw::_sym_internal) {
        printf("%s:%d  Trapped  VALUES[1] -> :INTERNAL\n",  __FILE__, __LINE__ );
      }
#endif
    }
  }
}

}; /* core */

core::T_mv ValuesFromCons(core::List_sp vals) {
  size_t len = cl__length(vals);
  if (len == 0) {
    return core::T_mv(_Nil<core::T_O>(), 0);
  }
  core::MultipleValues &me = (core::lisp_multipleValues());
  int i = 1;
  SUPPRESS_GC();
  me.setSize(0);
  for (auto cur : (core::List_sp)oCdr(vals)) {
    if (i >= core::MultipleValues::MultipleValuesLimit) {
      SIMPLE_ERROR(BF("Overflow when returning multiple values - only %d are supported and you tried to return %d values") % core::MultipleValues::MultipleValuesLimit % cl__length(vals));
    }
    core::T_sp obj = oCar(cur);
    me.valueSet(i, obj);
    ++i;
  }
  me.setSize(i);
  ENABLE_GC();
  core::T_mv mv = gctools::multiple_values<core::T_O>(oCar(vals), i);
  return mv;
}
