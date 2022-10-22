/*
    File: environment.cc
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
//
// This is an optimization where a static_cast is used
// for the environment rather than a dynamic cast.
// beach found that a lot of time was spent in dynamic_cast if
// this is not set
#define USE_STATIC_CAST_FOR_ENVIRONMENT 1

//#define DEBUG_LEVEL_FULL

#include <string.h>

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
//#i n c l u d e "stringSet.h"
#include <clasp/core/symbolTable.h>
#include <clasp/core/environment.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/sequence.h>
#include <clasp/core/array.h>
#include <clasp/core/primitives.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/wrappers.h>

#define MAX_CONS_CHARS 1024

namespace core {

#define ENV_DEBUG_ON() (_sym_STARenvironment_debugSTAR->symbolValue().notnilp())

  SYMBOL_SC_(CorePkg, environmentActivationFrame);
  SYMBOL_SC_(CorePkg, currentVisibleEnvironment);
  SYMBOL_SC_(CorePkg, runtimeEnvironment);
  SYMBOL_SC_(CorePkg, environmentList);
  SYMBOL_SC_(CorePkg, environmentTypeList);

}; // namespace core
