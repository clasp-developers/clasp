#pragma once
/*
    File: lambdaListHandler.h
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

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/ql.h>
#include <clasp/core/arguments.h>
#include <clasp/core/lambdaListHandler.fwd.h>
#include <clasp/core/symbol.h>

namespace core {

typedef enum { required, optional, dot_rest, va_rest, rest, keyword, allowOtherKeys, aux } ArgumentMode;

/*! A LambdaListHandler converts lambda-lists of all different types into
      an object that binds arguments to symbols in an environment.
      There is a subclass for every possible type of lambda-list namely:
      ordinary
      generic-function
      specialized
      macro
      destructuring
      boa
      defsetf
      deftype
      define-modify-macro
      define-method-combination
      See CLHS 3.4
    */

}; // namespace core

namespace core {

bool parse_lambda_list(List_sp, T_sp, gctools::Vec0<RequiredArgument>&, gctools::Vec0<OptionalArgument>&, RestArgument&, T_sp&,
                       gctools::Vec0<KeywordArgument>&, T_sp&, gctools::Vec0<AuxArgument>&);

List_sp lexical_variable_names(gctools::Vec0<RequiredArgument>& reqs, gctools::Vec0<OptionalArgument>& optionals,
                               RestArgument& restarg, gctools::Vec0<KeywordArgument>& keys, gctools::Vec0<AuxArgument>& auxs);

T_sp lambda_list_for_name(T_sp);
List_sp core__canonicalize_declarations(List_sp declares);
}; // namespace core

namespace core {
T_mv process_single_dispatch_lambda_list(List_sp lambda_list, bool allow_first_argument_default_dispatcher = false);
List_sp process_macro_lambda_list(List_sp lambda_list);

}; // namespace core
