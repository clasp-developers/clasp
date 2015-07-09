/*
    File: llenv.cc
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
int bind_required_arguments(vector<RequiredArgument> const &reqs,
                            ActivationFrame_sp args,
                            int arg_idx,
                            DynamicScopeManager &scope) {
  _G();
  // Fill required arguments
  LOG(BF("There are %d required arguments") % reqs.size());
  size_t length_args(cl_length(args));
  size_t reqs_size(reqs.size());
  if (length_args < reqs_size) {
    TOO_FEW_ARGUMENTS_ERROR();
  }
  {
    _BLOCK_TRACE("Assigning required arguments");
    LOG(BF("There are %d required arguments") % reqs.size());
    for (vector<RequiredArgument>::const_iterator it = reqs.begin(); it != reqs.end(); it++) {
      T_sp value = args->entry(arg_idx);
      LOG(BF("Binding value[%s] to target[%s]") % value->__repr__() % it->asString());
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
int bind_optional_arguments(vector<OptionalArgument> const &optionals,
                            ActivationFrame_sp args,
                            int arg_idx,
                            DynamicScopeManager &scope) {
  _G();
  int num_args = cl_length(args);
  // Fill required arguments
  LOG(BF("There are %d optional arguments") % optionals.size());
  vector<OptionalArgument>::const_iterator it = optionals.begin();
  {
    _BLOCK_TRACE("Assigning given optional arguments");
    for (; it != optionals.end(); it++) {
      LOG(BF("Checking if it->_Target.nilp() = %d") % it->_ArgTarget.nilp());
      if (arg_idx == num_args) {
        LOG(BF("We ran out of optional arguments - switching to filling with defaults"));
        break;
      }
      T_sp value = args->entry(arg_idx);
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
      LOG(BF("Init form: %s") % init_form->__repr__());
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

int bind_rest_var(RestArgument const &restarg,
                  ActivationFrame_sp args,
                  int arg_idx,
                  DynamicScopeManager &scope) {
  _G();
  if (restarg.isDefined()) {
#if VARARGS
    Cons_O::CdrType_sp rest(_Nil<Cons_O::CdrType_O>());
    Cons_O::CdrType_sp *cur = &rest;
    for (int i(arg_idx); i < n_args; ++i) {
      T_sp obj = gctools::smart_ptr<T_O>(va_arg(ap, TAGGED_PTR));
      Cons_sp one = Cons_O::create(obj);
      *cur = one;
      cur = one->cdrPtr();
    }
#else
    Cons_O::CdrType_sp rest = args->asCons(arg_idx);
#endif
    scope.new_binding(restarg, rest);
  }
  return arg_idx;
}

int bind_keyword_arguments(vector<KeywordArgument> const &keyed_args,
                           T_sp allow_other_keys,
                           ActivationFrame_sp args,
                           int arg_idx,
                           DynamicScopeManager &scope) {
  _G();
  int num_keyed_arguments = keyed_args.size();
  if (num_keyed_arguments == 0)
    return arg_idx;
  bool passed_allow_other_keys = false;
  bool sawkeys[CALL_ARGUMENTS_LIMIT];
  memset(sawkeys, 0, num_keyed_arguments);
  LOG(BF(":allow-other-keywords --> %d") % passed_allow_other_keys);
  T_sp first_illegal_keyword(_Nil<T_O>());
  {
    _BLOCK_TRACEF(BF("Copy passed keyword values to environment"));
    for (int i(arg_idx), iEnd(cl_length(args)); i < iEnd; i += 2) {
      Symbol_sp keyword = args->entry(i).as<Symbol_O>();
      T_sp value = args->entry(i + 1);
      if (keyword != kw::_sym_allow_other_keys) {
        LOG(BF("Binding passed keyword[%s] value[%s]") % _rep_(keyword) % _rep_(value));
        vector<KeywordArgument>::const_iterator fi;
        int ik(0);
        for (fi = keyed_args.begin(); fi != keyed_args.end(); fi++) {
          if (fi->_Keyword == keyword) {
            if (!scope.lexicalElementBoundP(*fi))
              scope.new_binding(*fi, value);
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
    UNRECOGNIZED_KEYWORD_ARGUMENTS_ERROR(first_illegal_keyword);
  }
  // Now fill in the default values for the missing parameters
  {
    _BLOCK_TRACEF(BF("Add missing keyword default init-form values to ActivationFrame"));
    vector<KeywordArgument>::const_iterator fi;
    int ik(0);
    for (fi = keyed_args.begin(); fi != keyed_args.end(); fi++) {
      LOG(BF("Checking if keyword[%s] needs default value") % fi->_Keyword->__repr__());
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

void bind_aux(vector<AuxArgument> const &auxs, DynamicScopeManager &scope) {
  _G();
  int num_auxs = auxs.size();
  if (num_auxs == 0)
    return;
  LOG(BF("There are %d aux variables") % auxs.size());
  vector<AuxArgument>::const_iterator ci;
  {
    _BLOCK_TRACE("Assigning aux variables");
    for (ci = auxs.begin(); ci != auxs.end(); ci++) {
      T_sp expr = ci->_Expression;
      if (expr.notnilp()) {
        T_sp value = evaluate_lambda_list_form(expr, scope.lexenv());
        scope.new_binding(*ci, value);
      } else {
        scope.new_binding(*ci, _Nil<T_O>());
      }
    }
  }
}
