/*
    File: argumentBinding.cc
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

/*! Define the following macros to configure this code for using ActivationFrames 
  or var-args...
  Name				va_list
  PASS_FUNCTION_REQUIRED 	bind_required_var_args
  PASS_FUNCTION_OPTIONAL 	bind_optional_var_args
  PASS_FUNCTION_REST 		bind_rest_var_args
  PASS_FUNCTION_KEYWORD 	bind_keyword_var_args
  PASS_ARGS  			'int n_args, va_list ap'	'ActivationFrame_sp args'
  PASS_ARGS_NUM 		'n_args'				'cl__length(args)'
  PASS_NEXT_ARG(arg_idx) 		'gctools::smart_ptr<T_O>(va_arg(ap,TAGGED_PTR))' 	'args->entry(arg_idx)'
*/

int PASS_FUNCTION_REQUIRED(core::T_sp closure,
                           gctools::Vec0<RequiredArgument> const &reqs,
                           PASS_ARGS,
                           int arg_idx,
                           ScopeManager &scope) {
  // Fill required arguments
  LOG(BF("There are %d required arguments") % reqs.size());
  size_t length_args(PASS_ARGS_NUM);
  size_t reqs_size(reqs.size());
  if (length_args < reqs_size) {
    throwTooFewArgumentsError(closure,length_args, reqs_size);
  }
  if (reqs.size()>0) {
    _BLOCK_TRACE("Assigning required arguments");
    for (gctools::Vec0<RequiredArgument>::const_iterator it = reqs.begin(); it != reqs.end(); ++it) {
      T_sp value = PASS_NEXT_ARG(arg_idx);
      LOG(BF("Binding value[%s] to target[%s]") % _rep_(value) % it->asString());
      scope.new_binding(*it, value);
      ++arg_idx;
    }
  }
  return arg_idx;
}

/*! Fill an activation frame with optional arguments.
  The optional arguments required are in (optionals).
  The optional arguments passed are in (args) starting at (arg_idx).
  If default values are evaluated, they are evaluated in the lambda-list-handlers (closed_env).
*/
int PASS_FUNCTION_OPTIONAL(core::T_sp closure,
                           gctools::Vec0<OptionalArgument> const &optionals,
                           PASS_ARGS,
                           int arg_idx,
                           ScopeManager &scope) {
  int num_args(PASS_ARGS_NUM);
  // Fill required arguments
  LOG(BF("There are %d optional arguments") % optionals.size());
  gctools::Vec0<OptionalArgument>::const_iterator it = optionals.begin();
  {
    _BLOCK_TRACE("Assigning given optional arguments");
    for (; it != optionals.end(); it++) {
      LOG(BF("Checking if it->_Target.nilp() = %d") % it->_ArgTarget.nilp());
      if (arg_idx == num_args) {
        LOG(BF("We ran out of optional arguments - switching to filling with defaults"));
        break;
      }
      T_sp value = PASS_NEXT_ARG(arg_idx);
      LOG(BF("Binding value[%s] to target[%s]") % _rep_(value) % it->asString());
      scope.new_binding(*it, value);
      if (it->_Sensor.isDefined()) {
        scope.new_binding(it->_Sensor, _lisp->_true());
      }
      ++arg_idx;
    }
  }
  {
    _BLOCK_TRACE("Assigning missing optional arguments with default values");
    for (; it != optionals.end(); it++) {
      T_sp init_form = it->_Default;
      LOG(BF("Init form: %s") % _rep_(init_form));
      T_sp value = evaluate_lambda_list_form(init_form, scope.lexenv());
      LOG(BF("Binding value[%s] to target[%s]") % _rep_(value) % it->asString());
      scope.new_binding(*it, value);
      if (it->_Sensor.isDefined()) {
        scope.new_binding(it->_Sensor, _lisp->_false());
      }
    }
  }
  return arg_idx;
}

void PASS_FUNCTION_REST(core::T_sp closure,
                        RestArgument const &restarg,
                        PASS_ARGS,
                        int arg_idx,
                        ScopeManager &scope) {
  if (restarg.VaRest) {
    scope.valist().set_from_other_Vaslist(&*arglist); // _change_nargs(&*arglist, n_args - arg_idx);
    scope.va_rest_binding(restarg);
  } else if (arg_idx == PASS_ARGS_NUM) {
    scope.new_binding(restarg, _Nil<T_O>());
  } else {
    T_sp rest = Cons_O::create(PASS_NEXT_ARG(arg_idx), _Nil<T_O>());
    T_sp cur = rest;
    for (int i(arg_idx+1), iEnd(PASS_ARGS_NUM); i < iEnd; ++i) {
      T_sp one = Cons_O::create(PASS_NEXT_ARG(i), _Nil<T_O>());
      CONS_CDR(cur) = one;
      cur = one;
    }
    scope.new_binding(restarg, rest);
  }
}

#if 0
void PASS_FUNCTION_VA_REST(RestArgument const &va_restarg,
                           PASS_ARGS,
                           int arg_idx,
                           ScopeManager &scope) {
  Cons_O::CdrType_sp rest = _Nil<Cons_O::CdrType_O>();
  Cons_O::CdrType_sp *curP = &rest;
  scope.valist().set(&*arglist, n_args - arg_idx);
  scope.va_rest_binding(va_restarg);
}
#endif

int PASS_FUNCTION_KEYWORD(T_sp closure,
                          gctools::Vec0<KeywordArgument> const &keyed_args,
                          T_sp allow_other_keys,
                          PASS_ARGS,
                          int arg_idx,
                          ScopeManager &scope) {
  int num_args(PASS_ARGS_NUM);
  int num_keyed_arguments = keyed_args.size();
  bool passed_allow_other_keys = false;
  bool *sawkeys = (bool *)(__builtin_alloca(sizeof(bool) * num_keyed_arguments));
  //  bool sawkeys[num_keyed_arguments];// CALL_ARGUMENTS_LIMIT];
  memset(sawkeys, 0, num_keyed_arguments);
  LOG(BF(":allow-other-keywords --> %d") % passed_allow_other_keys);
  T_sp first_illegal_keyword(_Nil<T_O>());
  {
    _BLOCK_TRACEF(BF("Copy passed keyword values to environment"));
    for (int i(arg_idx), iEnd(num_args); i < iEnd; i += 2) {
      T_sp keyword = T_sp(PASS_NEXT_ARG(arg_idx));
      arg_idx++;
      T_sp value = PASS_NEXT_ARG(arg_idx);
      arg_idx++;
      if (keyword != kw::_sym_allow_other_keys) {
        LOG(BF("Binding passed keyword[%s] value[%s]") % _rep_(keyword) % _rep_(value));
        gctools::Vec0<KeywordArgument>::iterator fi;
        int ik(0);
        for (fi = keyed_args.begin(); fi != keyed_args.end(); fi++) {
          if (fi->_Keyword == keyword) {
            if (!scope.lexicalElementBoundP(*fi)) {
              scope.new_binding(*fi, value);
            }
            if (fi->_Sensor.isDefined())
              scope.new_binding(fi->_Sensor, _lisp->_true());
            sawkeys[ik] = 1;
            break;
          }
          ik++;
        }
        if (fi == keyed_args.end() && first_illegal_keyword.nilp())
          first_illegal_keyword = keyword;
      } else {
        passed_allow_other_keys = value.isTrue();
      }
    }
  }
  if (first_illegal_keyword.notnilp() && !passed_allow_other_keys && allow_other_keys.nilp()) {
    throwUnrecognizedKeywordArgumentError(closure,first_illegal_keyword);
    //	UNRECOGNIZED_KEYWORD_ARGUMENTS_ERROR(first_illegal_keyword);
  }
  // Now fill in the default values for the missing parameters
  {
    _BLOCK_TRACEF(BF("Add missing keyword default init-form values to ActivationFrame"));
    gctools::Vec0<KeywordArgument>::iterator fi;
    int ik(0);
    for (fi = keyed_args.begin(); fi != keyed_args.end(); fi++) {
      LOG(BF("Checking if keyword[%s] needs default value") % _rep_(fi->_Keyword));
      // If the value hasn't been filled in the ActivationFrame then fill it with a default value
      if (sawkeys[ik] == 0) {
        T_sp expr = fi->_Default;
        T_sp value = evaluate_lambda_list_form(expr, scope.lexenv());
        scope.new_binding(*fi, value);
        if (fi->_Sensor.isDefined()) {
          scope.new_binding(fi->_Sensor, _lisp->_false());
        }
      }
      ++ik;
    }
  }
  return arg_idx;
}
