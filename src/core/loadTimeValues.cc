/*
    File: loadTimeValues.cc
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
#include <clasp/core/array.h>
#include <clasp/core/predicates.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/wrappers.h>

namespace core {

// ----------------------------------------------------------------------
//

SYMBOL_SC_(CorePkg, loadTimeValuesIds);
SYMBOL_SC_(CorePkg, loadTimeValueArray);
SYMBOL_SC_(CorePkg, lookupLoadTimeValue);
//SYMBOL_SC_(CorePkg, lookupLoadTimeSymbol);
SYMBOL_EXPORT_SC_(CorePkg, setRunTimeValuesVector);


}; /* core */
