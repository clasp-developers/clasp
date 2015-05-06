/*
    File: processLambdaList.cc
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

string argument_mode_as_string(ArgumentMode mode) {
  switch (mode) {
  case required:
    return "required";
  case optional:
    return "optional";
  case rest:
    return "rest";
  case keyword:
    return "keyword";
  case aux:
    return "aux";
  case allowOtherKeys:
    return "allow-other-keys";
  case complete:
    return "complete";
  }
  return "-unknownAddArgumentMode-";
}

bool switch_add_argument_mode(Symbol_sp symbol, ArgumentMode &mode, Lisp_sp lisp) {
  _G();
  ASSERTP(!symbol->isKeywordSymbol(), "Do not use keyword symbols when specifying arguments");
  LOG(BF("It is a symbol: %s %X") % symbol->__repr__() % symbol.get());
  bool isAmpSymbol = (symbol == _lisp->getAmpKeySymbol()               // &key
                      || symbol == _lisp->getAmpAuxSymbol()            // &aux
                      || symbol == _lisp->getAmpEnvironment()          // &environment
                      || symbol == _lisp->getAmpAllowOtherKeysSymbol() // &allow-other-keys
                      || symbol == _lisp->getAmpRestSymbol()           // &rest
                      || symbol == _lisp->getAmpBodySymbol()           // &body
                      || symbol == _lisp->getAmpAuxSymbol()            // &aux
                      || symbol == _lisp->getAmpOptionalSymbol()       // &optional
                      );
  if (isAmpSymbol) {
    LOG(BF("It is an amp symbol"));
    switch (mode) {
    case required:
      LOG(BF("Was in required mode"));
      if (symbol == _lisp->getAmpOptionalSymbol()) {
        LOG(BF("Argument handling mode switched to (optional)"));
        mode = optional;
        return true;
      } else if (symbol == _lisp->getAmpRestSymbol() || symbol == _lisp->getAmpBodySymbol()) {
        LOG(BF("Argument handling mode switched to (rest)"));
        mode = rest;
        return true;
      } else if (symbol == _lisp->getAmpKeySymbol()) {
        LOG(BF("Argument handling mode switched to (keyword)"));
        mode = keyword;
        return true;
      } else if (symbol == _lisp->getAmpAuxSymbol()) {
        LOG(BF("Argument handling mode switched to (aux)"));
        mode = aux;
        return true;
      }
      LOG(BF("Did not recognize symbol(%s)") % symbol->fullName());
      SIMPLE_ERROR(BF("Did not recognize symbol(%s)") % symbol->fullName());
      break;
    case optional:
      LOG(BF("Was in optional mode"));
      if (symbol == _lisp->getAmpOptionalSymbol()) {
        SIMPLE_ERROR(BF("Duplicate optional argument"));
      }
      if (symbol == _lisp->getAmpRestSymbol()) {
        LOG(BF("Argument handling mode switched to (rest)"));
        mode = rest;
        return true;
      }
      if (symbol == _lisp->getAmpKeySymbol()) {
        LOG(BF("Argument handling mode switched to (keyword)"));
        mode = keyword;
        return true;
      }
      if (symbol == _lisp->getAmpAuxSymbol()) {
        LOG(BF("Argument handling mode switched to (aux)"));
        mode = aux;
        return true;
      }
      LOG(BF("Did not recognize symbol(%s)") % symbol->fullName());
      SIMPLE_ERROR(BF("Did not recognize symbol(%s)") % symbol->fullName());
      break;
    case rest:
      LOG(BF("Was in rest mode"));
      if (symbol == _lisp->getAmpOptionalSymbol()) {
        SIMPLE_ERROR(BF("&optional argument specified after &rest is illegal"));
      }
      if (symbol == _lisp->getAmpRestSymbol()) {
        SIMPLE_ERROR(BF("Duplicate &rest argument"));
      }
      if (symbol == _lisp->getAmpKeySymbol()) {
        LOG(BF("Argument handling mode switched to (keyword)"));
        mode = keyword;
        return true;
      }
      if (symbol == _lisp->getAmpAuxSymbol()) {
        LOG(BF("Argument handling mode switched to (aux)"));
        mode = aux;
        return true;
      }
      LOG(BF("Did not recognize symbol(%s)") % symbol->fullName());
      SIMPLE_ERROR(BF("Did not recognize symbol(%s)") % symbol->fullName());
      break;
    case keyword:
      LOG(BF("Was in keyword mode"));
      if (symbol == _lisp->getAmpOptionalSymbol()) {
        SIMPLE_ERROR(BF("&optional argument specified after &key is illegal"));
      }
      if (symbol == _lisp->getAmpRestSymbol()) {
        SIMPLE_ERROR(BF("&rest argument after &key is illegal - put &key after &rest"));
      }
      if (symbol == _lisp->getAmpKeySymbol()) {
        SIMPLE_ERROR(BF("Duplicate &key argument"));
      }
      if (symbol == _lisp->getAmpAllowOtherKeysSymbol()) {
        mode = allowOtherKeys;
      }
      if (symbol == _lisp->getAmpAuxSymbol()) {
        LOG(BF("Argument handling mode switched to (aux)"));
        mode = aux;
        return true;
      }
      LOG(BF("Did not recognize symbol(%s)") % symbol->fullName());
      SIMPLE_ERROR(BF("Did not recognize symbol(%s)") % symbol->fullName());
      break;
    case allowOtherKeys:
      LOG(BF("Was in allowOtherKeys mode"));
      if (symbol == _lisp->getAmpOptionalSymbol()) {
        SIMPLE_ERROR(BF("&optional argument specified after &allow-other-keys is illegal"));
      }
      if (symbol == _lisp->getAmpRestSymbol()) {
        SIMPLE_ERROR(BF("&rest argument after &allow-other-keys is illegal - put &key after &rest"));
      }
      if (symbol == _lisp->getAmpKeySymbol()) {
        SIMPLE_ERROR(BF("&key argument after &allow-other-keys is illegal"));
      }
      if (symbol == _lisp->getAmpAllowOtherKeysSymbol()) {
        SIMPLE_ERROR(BF("duplicate &allow-other-keys is illegal"));
      }
      LOG(BF("Did not recognize symbol(%s)") % symbol->fullName());
      SIMPLE_ERROR(BF("Did not recognize symbol(%s)") % symbol->fullName());
      break;
    case aux:
      LOG(BF("Was in aux mode"));
      if (symbol == _lisp->getAmpOptionalSymbol()) {
        SIMPLE_ERROR(BF("&optional argument specified after &aux is illegal - &aux must be last"));
      }
      if (symbol == _lisp->getAmpRestSymbol()) {
        SIMPLE_ERROR(BF("&rest argument after &aux is illegal - &aux must be last"));
      }
      if (symbol == _lisp->getAmpKeySymbol()) {
        SIMPLE_ERROR(BF("&key argument after &aux - &aux must be last"));
      }
      if (symbol == _lisp->getAmpAllowOtherKeysSymbol()) {
        mode = allowOtherKeys;
      }
      if (symbol == _lisp->getAmpAuxSymbol()) {
        SIMPLE_ERROR(BF("&Duplicate &aux symbol"));
      }
      LOG(BF("Did not recognize symbol(%s)") % symbol->fullName());
      SIMPLE_ERROR(BF("Did not recognize symbol(%s)") % symbol->fullName());
      break;
    case complete:
      SIMPLE_ERROR(BF("Argument handling was attempted when the LambdaListHandler setup was complete"));
      break;
    };
  } else {
    LOG(BF("It is not an amp symbol"));
  }
  return false;
}

void extract_lambda_list(Cons_sp arguments,
                         Symbol_sp context,
                         Cons_sp &reqs,
                         Cons_sp &optionals,
                         Symbol_sp &restvar,
                         T_sp &key_flag,
                         Cons_sp &keys,
                         T_sp &allow_other_keys,
                         Cons_sp &auxs,
                         Lisp_sp lisp) {
  _G();
  LOG(BF("Argument handling mode starts in (required) - interpreting: %s") % arguments->__repr__());
  ArgumentMode add_argument_mode = required;
  restvar = _Nil<Symbol_O>();
  int num_requireds = 0;
  ql::list qlrequireds(_lisp);
  int num_optionals = 0;
  ql::list qloptionals(_lisp);
  int num_keys = 0;
  ql::list qlkeys(_lisp);
  allow_other_keys = _lisp->_false();
  int num_auxs = 0;
  ql::list qlauxs(_lisp);
  for (Cons_sp cur = arguments; cur.notnilp(); cur = cCdr(cur)) {
    LOG(BF("Handing argument: %s") % cur->ocar()->__repr__());
    T_sp oarg = cur->ocar();
    if (oarg.isA<Symbol_O>()) {
      Symbol_sp sym = oarg.as<Symbol_O>();
      if (switch_add_argument_mode(sym, add_argument_mode, _lisp))
        continue;
    }
    //
    // Now put the argument symbol where it belongs
    //
    switch (add_argument_mode) {
    case required: {
      qlrequireds << oarg;
      ++num_requireds;
      break;
    }
    case optional: {
      Symbol_sp sarg = _Nil<Symbol_O>();
      T_sp defaultValue = _Nil<T_O>();
      Symbol_sp supplied = _Nil<Symbol_O>();
      if (oarg->symbolP()) {
        sarg = oarg.as<Symbol_O>();
        LOG(BF("Optional argument was a Symbol_O[%s]") % sarg->__repr__());
      } else if (oarg->consP()) {
        Cons_sp carg = oarg.as_or_nil<Cons_O>();
        LOG(BF("Optional argument is a Cons: %s") % carg->__repr__());
        sarg = carg->ocar().as<Symbol_O>();
        if (carg->cdr().notnilp()) {
          defaultValue = carg->ocadr();
          if (carg->cddr().notnilp()) {
            supplied = carg->ocaddr().as<Symbol_O>();
          }
        }
        LOG(BF("Optional argument was a Cons_O[%s] with parts - symbol[%s] default[%s] supplied[%s]") % carg->__repr__() % sarg->__repr__() % defaultValue->__repr__() % supplied->__repr__());
      }
      LOG(BF("Saving _OptionalArgument(%s) default(%s) supplied(%s)") % sarg->__repr__() % defaultValue->__repr__() % supplied->__repr__());
      ++num_optionals;
      qloptionals << sarg << defaultValue << supplied;
      break;
    }
    case rest: {
      Symbol_sp sarg = oarg.as<Symbol_O>();
      LOG(BF("Saving _Rest argument: %s") % sarg->__repr__());
      if (restvar.notnilp()) {
        SIMPLE_ERROR(BF("Only one name is allowed after &rest"));
      }
      restvar = sarg;
      break;
    }
    case keyword: {
      Symbol_sp keySymbol = _Nil<Symbol_O>();
      Symbol_sp localSymbol = _Nil<Symbol_O>();
      T_sp defaultValue = _Nil<T_O>();
      Symbol_sp sensorSymbol = _Nil<Symbol_O>();
      if (oarg->symbolP()) {
        localSymbol = oarg.as<Symbol_O>();
        keySymbol = localSymbol->asKeywordSymbol();
      } else if (oarg->consP()) {
        Cons_sp carg = oarg.as_or_nil<Cons_O>();
        T_sp head = carg->ocar();
        if (head->symbolP()) {
          localSymbol = head.as<Symbol_O>();
          ASSERTP(!localSymbol->isKeywordSymbol(), "Do not use keyword symbols when specifying arguments");
          keySymbol = localSymbol->asKeywordSymbol();
        } else if (head->consP()) {
          Cons_sp namePart = head.as_or_nil<Cons_O>();
          keySymbol = namePart->ocar().as<Symbol_O>(); // This is the keyword name
          ASSERTP(keySymbol->isKeywordSymbol(), "with key arguments of the form ((:x y) ...) the first argument must be a symbol");
          localSymbol = namePart->ocadr().as<Symbol_O>(); // this is the symbol to rename it to
          ASSERTP(!localSymbol->isKeywordSymbol(), "Do not use keyword symbols when specifying local argument names");
        }
        //
        // Is there a default value?
        //
        if (carg->cdr().notnilp()) {
          defaultValue = carg->ocadr();
          if (carg->cddr().notnilp()) {
            sensorSymbol = carg->ocaddr().as<Symbol_O>();
            ASSERTP(!sensorSymbol->isKeywordSymbol(), "Do not use keyword symbols when specifying local argument names");
          }
        }
      } else {
        SIMPLE_ERROR(BF("key arguments must be symbol or cons"));
      }
      LOG(BF("Saving keyword(%s) local(%s) default(%s) sensor(%s)") % keySymbol->__repr__() % localSymbol->__repr__() % defaultValue->__repr__() % sensorSymbol->__repr__());
      ++num_keys;
      qlkeys << keySymbol << localSymbol << defaultValue << sensorSymbol;
      break;
    }
    case allowOtherKeys:
      allow_other_keys = _lisp->_true();
      break;
    case aux: {
      Symbol_sp localSymbol = _Nil<Symbol_O>();
      T_sp expression = _Nil<T_O>();
      if (oarg.isA<Symbol_O>()) {
        localSymbol = oarg.as<Symbol_O>();
      } else if (oarg->consP()) {
        Cons_sp carg = oarg.as_or_nil<Cons_O>();
        localSymbol = carg->ocar().as<Symbol_O>();
        //
        // Is there an expression
        //
        if (carg->cdr().notnilp())
          expression = carg->ocadr();
      } else {
        SIMPLE_ERROR(BF("&aux variables must be specified by a symbol or a cons of a symbol and an expression"));
      }
      ++num_auxs;
      qlauxs << localSymbol << expression;
      break;
    }
    case complete:
      SIMPLE_ERROR(BF("You are adding arguments when argument processing is complete"));
      break;
    }
  }
  reqs = qlrequireds.all();
  reqs->setCar(Fixnum_O::create(num_requireds));
  optionals = qloptionals.all();
  optionals->setCar(Fixnum_O::create(num_optionals));
  key_flag = _lisp->_boolean(num_keys > 0);
  keys = qlkeys.all();
  keys->setCar(Fixnum_O::create(num_keys));
  auxs = qlauxs.all();
  auxs->setCar(Fixnum_O::create(num_auxs));
}

/*!
 * (si::process-lambda-list lambda-list context)
 *
 * Parses different types of lambda lists. CONTEXT may be MACRO,
 * FTYPE, FUNCTION, METHOD or DESTRUCTURING-BIND, and determines the
 * valid sytax. The output is made of several values:
 *
 * Return results in MultipleValues object
 * VALUES(0) = (N req1 ... )			; required values
 * VALUES(1) = (N opt1 init1 flag1 ... )	; optional values
 * VALUES(2) = rest-var				; rest-variable, if any
 * VALUES(3) = key-flag				; T if &key was supplied
 * VALUES(4) = (N key1 var1 init1 flag1 ... )	; keyword arguments
 * VALUES(5) = allow-other-keys			; flag &allow-other-keys
 * VALUES(6) = (N aux1 init1 ... )		; auxiliary variables
 *
 * 1°) The prefix "N" is an integer value denoting the number of
 * variables which are declared within this section of the lambda
 * list.
 *
 * 2°) The INIT* arguments are lisp forms which are evaluated when
 * no value is provided.
 *
 * 3°) The FLAG* arguments is the name of a variable which holds a
 * boolean value in case an optional or keyword argument was
 * provided. If it is NIL, no such variable exists.
 */

T_mv process_lambda_list(Cons_sp arguments, Symbol_sp context, Lisp_sp lisp) {
  _G();
  Cons_sp reqs;
  Cons_sp optionals;
  Symbol_sp restvar;
  T_sp key_flag;
  Cons_sp keys;
  T_sp allow_other_keys;
  Cons_sp auxs;
  process_lambda_list(arguments, context, reqs, optional, restvar, key_flag, keys, allow_other_keys, auxs, _lisp);
  return (Values(reqs, optional, restvar, key_flag, keys, allow_other_keys, auxs));
}
