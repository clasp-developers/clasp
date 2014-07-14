#define DEBUG_LEVEL_FULL
//#include "core/foundation.h"
#include "core/common.h"
#include "core/corePackage.h"
#include "evaluator.h"
#include "iterator.h"
#include "metaClass.h"
#include "core/array.h"
#include "symbolTable.h"
#include "hashTable.h"
#include "specialForm.h"
//#i n c l u d e "setfExpander.h"
#include "environment.h"
#include "executables.h"
#include "builtInClass.h"
#include "lambdaListHandler.h"
#include "vectorObjects.h"
#include "standardClass.h"
#include "standardObject.h"
#include "predicates.h"
#include "lisp.h"
#include "backquote.h"
#include "sysprop.h"
#include "conditions.h"
#include "multipleValues.h"
#include "primitives.h"
//#include "debugger.h"
#include "str.h"
#include "wrappers.h"

namespace core
{



    Cons_sp separateTopLevelForms(Cons_sp accumulated, T_sp possibleForms)
    {
        if ( Cons_sp cpf = possibleForms.asOrNull<Cons_O>() ) {
            if ( oCar(cpf).as<Symbol_O>() == cl::_sym_progn ) {
                for ( Cons_sp cur = cCdr(cpf); cur.notnilp(); cur=cCdr(cur) ) {
                    accumulated = separateTopLevelForms(accumulated,oCar(cur));
                }
                return accumulated;
            }
        }
        accumulated = Cons_O::create(possibleForms,accumulated);
        return accumulated;
    }

#define ARGS_af_evalWithEnv "(form &optional env stepping compiler-env-p (execute t))"
#define DECL_af_evalWithEnv ""
#define DOCS_af_evalWithEnv "evalWithEnv"
    T_mv af_evalWithEnv(T_sp form, Environment_sp env, bool stepping, bool compiler_env_p, bool execute)
    {_G();
	TopLevelIHF stackFrame(_lisp->invocationHistoryStack(),form);
	T_mv result;
	// If we want to compile the form then do this
	stackFrame.setActivationFrame(Environment_O::nilCheck_getActivationFrame(env));
        Cons_sp topLevelForms = separateTopLevelForms(_Nil<Cons_O>(),form)->nreverse().as<Cons_O>();
        for ( Cons_sp curtlf = topLevelForms; curtlf.notnilp(); curtlf=cCdr(curtlf) ) {
            T_sp thunk = eval::funcall(_sym_STARimplicit_compile_hookSTAR->symbolValue(),oCar(curtlf),env);
            LOG(BF("After compile thunk[%s]") % _rep_(thunk) );
            try {_BLOCK_TRACEF(BF("-eval/print stage-"));
                ValueFrame_sp vf = ValueFrame_O::create(0,_Nil<ActivationFrame_O>());
                result = eval::applyToActivationFrame(thunk,vf);
                LOG(BF("---result[%s]") % _rep_(result) );
            }
            catch (Condition& err)
            {
                _lisp->print(BF("%s:%d - A Condition was caught in readEvalPrint - _lisp shouldn't happen - exiting") % __FILE__ % __LINE__ );
                exit(1);
            }
            catch (HardError& err )
            {
                _lisp->print(BF("HardError - should never happen! Catch and convert to Condition below"));
                IMPLEMENT_ME();
//		    _lisp->enterDebugger();
            }
        }
	ASSERTNOTNULL(result);
	return result;
    };



    
    
#define ARGS_af_interpreter_lookup_variable "(symbol env)"
#define DECL_af_interpreter_lookup_variable ""
#define DOCS_af_interpreter_lookup_variable "environment_lookup_variable"
    T_mv af_interpreter_lookup_variable(Symbol_sp sym, Environment_sp env)
    {_G();
	if ( env.notnilp() )
	{
	    T_mv result;
	    bool foundp;
	    {MULTIPLE_VALUES_CONTEXT();
		result = env->variable_lookup(sym);
		foundp = result.valueGet(1).as<T_O>().notnilp();
	    }
	    if (foundp)
	    {
		return(Values(result));
	    } else if ( result.notnilp() )
	    {
		// It's a special variable
		return(Values(sym->symbolValue()));
	    }
	}
	if ( sym->specialP() || sym->boundP() )
	{
	    return(Values(sym->symbolValue()));
	}
	SIMPLE_ERROR(BF("Could not find variable %s in lexical/global environment") % _rep_(sym));
    };


#define ARGS_af_interpreter_lookup_function "(symbol env)"
#define DECL_af_interpreter_lookup_function ""
#define DOCS_af_interpreter_lookup_function "environment_lookup_function return the function or UNBOUND"
    Function_sp af_interpreter_lookup_function(Symbol_sp name, Environment_sp env)
    {_G();
	if ( env.notnilp() )
	{
	    Function_sp fn = env->function_lookup(name);
	    if ( fn.notnilp() )
	    {
		return(Values(fn));
	    }
	}
	Function_sp fn = name->symbolFunction();
	
	return fn;
    };



#define ARGS_af_interpreter_lookup_setf_function "(symbol env)"
#define DECL_af_interpreter_lookup_setf_function ""
#define DOCS_af_interpreter_lookup_setf_function "environment_lookup_setf_function"
    Function_mv af_interpreter_lookup_setf_function(Cons_sp setf_name, Environment_sp env)
    {_G();
	Symbol_sp name = oCadr(setf_name).as<Symbol_O>();
	if ( env.notnilp() )
	{
	    Function_sp fn = env->function_lookup(setf_name);
	    if ( fn.notnilp() )
	    {
		return(Values(fn));
	    }
	}
	Function_sp fn = _lisp->get_setfDefinition(name);
	if ( fn.notnilp() )
	{
	    return(Values(fn));
	}
	return(Values(_Nil<Function_O>()));
    };



#define ARGS_af_interpreter_lookup_symbol_macro "(symbol env)"
#define DECL_af_interpreter_lookup_symbol_macro ""
#define DOCS_af_interpreter_lookup_symbol_macro "environment_lookup_symbol_macro_definition"
    Function_sp af_interpreter_lookup_symbol_macro(Symbol_sp sym, Environment_sp env)
    {_G();
	if ( sym.nilp() ) return _Nil<Function_O>();
	if ( env.notnilp() )
	{
	    int depth=0;
	    int level=0;
	    bool shadowed = false;
	    Function_sp macro;
	    bool found = env->findSymbolMacro(sym,depth,level,shadowed,macro);
	    if ( found ) return macro;
	}
	SYMBOL_SC_(CorePkg,symbolMacro);
	Function_sp fn = _Nil<Function_O>();
	T_mv result = af_get_sysprop(sym,core::_sym_symbolMacro);
	if ( result.valueGet(1).as<T_O>().notnilp() )
	{
	    fn = result.as<Function_O>();
	}
	return fn;
    };



#define ARGS_af_interpreter_lookup_macro "(symbol env)"
#define DECL_af_interpreter_lookup_macro ""
#define DOCS_af_interpreter_lookup_macro "environment_lookup_macro_definition"
    Function_sp af_interpreter_lookup_macro(Symbol_sp sym, Environment_sp env)
    {_G();
	if ( sym.nilp() ) return _Nil<Function_O>();
	if ( env.notnilp() )
	{
	    int depth=0;
	    int level=0;
	    Function_sp macro;
	    bool found = env->findMacro(sym,depth,level,macro);
	    if ( found ) return macro;
	}
	Function_sp fn = sym->symbolFunction();
	if ( fn.pointerp() && fn->macroP() ) return fn;
	return _Nil<Function_O>();
    };









    namespace interpret
    {
	T_mv interpreter_cond(Cons_sp args, Environment_sp environment)
	{_G();
	    for ( Cons_sp cur = args; cur.notnilp(); cur = cCdr(cur) )
	    {
		T_sp cond;
		Cons_sp condProgn;
		{
		    condProgn = oCar(cur).as_or_nil<Cons_O>();
		    cond = eval::evaluate(oCar(condProgn),environment);
		}
		if ( cond.isTrue() )
		{
		    Cons_sp code = cCdr(condProgn);
		    if ( code.notnilp() )
		    {
			return eval::sp_progn(code,environment);
		    }
		    return(Values(cond));
		}
	    }
	    return Values(_Nil<T_O>());
	}

	SYMBOL_EXPORT_SC_(ClPkg,case);
	T_mv interpreter_case(Cons_sp args, Environment_sp environment)
	{_G();
	    T_sp keyform = oCar(args);
	    Cons_sp clauses = cCdr(args);

	    T_sp test_key = eval::evaluate(keyform,environment);
	    LOG(BF("Evaluated test_key = %s\n") % _rep_(test_key) );
	    
	    for ( Cons_sp cur = clauses; cur.notnilp(); cur = cCdr(cur) )
	    {
		T_sp oclause = oCar(cur);
		if ( af_consP(oclause) )
		{
		    Cons_sp clause = oclause.as_or_nil<Cons_O>();
		    T_sp keys = oCar(clause);
		    Cons_sp forms = cCdr(clause);
		    SYMBOL_EXPORT_SC_(ClPkg,otherwise);
		    if ( keys == cl::_sym_otherwise || keys == _lisp->_true() )
		    {
			if ( cCdr(cur).notnilp() )
			{
			    SIMPLE_ERROR(BF("otherwise-clause must be the last clause of case - it is not"));
			}
			return eval::sp_progn(forms,environment);
		    } else if ( af_atom(keys) )
		    {
			if ( af_eql(keys,test_key) )
			{
			    return eval::sp_progn(forms,environment);
			}
		    } else if ( af_consP(keys) )
		    {
			for (Cons_sp kcur = keys.as_or_nil<Cons_O>(); kcur.notnilp(); kcur=cCdr(kcur) )
			{
			    if ( af_eql(oCar(kcur),test_key) )
			    {
				return eval::sp_progn(forms,environment);
			    }
			}
		    }
		} else
		{
		    SIMPLE_ERROR(BF("Bad case clause: %s") % _rep_(oclause) );
		}
	    }
	    return(Values(_Nil<T_O>()));
	}


	void setq_symbol_value(Symbol_sp symbol, T_sp value, Environment_sp environment)
	{
	    Environment_sp localEnv(environment);
	    if ( symbol->specialP() || (environment.notnilp() && environment->lexicalSpecialP(symbol) ))
	    {
		symbol->setf_symbolValue(value);
	    } else {
		bool updated = environment->updateValue(symbol,value);
		if ( !updated )
		{
		    symbol->setf_symbolValue(value);
		}
	    }
	}


	SYMBOL_EXPORT_SC_(ClPkg,multipleValueSetq);
	T_mv interpreter_multipleValueSetq(Cons_sp args, Environment_sp environment)
	{_G();
	    Cons_sp cur = oCar(args).as_or_nil<Cons_O>();
	    T_sp form = oCadr(args);
	    VectorObjects_sp values(VectorObjects_O::create());
	    T_mv result = eval::evaluate(form,environment);
	    multipleValuesSaveToVector(result,values);
	    Cons_sp skipFirst = Cons_O::create(_Nil<T_O>(),_Nil<Cons_O>());
	    Cons_sp add = skipFirst;
	    // Assemble a Cons for sp_setq
	    size_t valuesLength = af_length(values);
	    for ( int i=0 ; cur.notnilp(); cur=cCdr(cur), ++i )
	    {
		Symbol_sp symbol = oCar(cur).as<Symbol_O>();
		T_sp value = i<valuesLength ? values->operator[](i) : _Nil<T_O>();
		Cons_sp one = Cons_O::create(symbol,_Nil<Cons_O>());
		add->setCdr(one);
		add = one;
		Cons_sp quotedValue = Cons_O::createList(cl::_sym_quote,value);
		Cons_sp two = Cons_O::create(quotedValue,_Nil<Cons_O>());
		add->setCdr(two);
		add = two;
	    }
	    eval::sp_setq(cCdr(skipFirst),environment);
	    return(Values(values->operator[](0)));
	}



	SYMBOL_EXPORT_SC_(ClPkg,prog1);
	T_mv interpreter_prog1(Cons_sp args, Environment_sp environment)
	{_G();
	    T_sp firstForm = oCar(args);
	    Cons_sp forms = cCdr(args);
	    T_sp result = eval::evaluate(firstForm,environment);
	    eval::sp_progn(forms,environment);
	    return(Values(result));
	}




    };

    namespace eval
    {
	void extract_declares_docstring_code_specials(Cons_sp inputBody, Cons_sp& declares, bool expectDocString, Str_sp& documentation, Cons_sp& code, Cons_sp& specials )
	{_G();
#if 1
	    Cons_sp body = inputBody;
	    documentation = _Nil<Str_O>();
	    declares = _Nil<Cons_O>();
	    specials = _Nil<Cons_O>();
	    for (; body.notnilp(); body=cCdr(body) )
	    {
		if ( !af_listp(body) )
		{
		    SIMPLE_ERROR(BF("Bad input to processDeclares: %s") % _rep_(inputBody));
		}
		T_sp form = oCar(body);
		if ( expectDocString && af_stringP(form) && cCdr(body).notnilp() )
		{
		    if ( documentation.notnilp() ) break;
		    documentation = form.as<Str_O>();
		    continue;
		}
		if ( af_atom(form) || oCar(form) != cl::_sym_declare )
		{
		    break;
		}
		Cons_sp cform = form.as_or_nil<Cons_O>();
		for ( cform = cCdr(form.as_or_nil<Cons_O>()); cform.notnilp(); )
		{
		    Cons_sp sentence = oCar(cform).as_or_nil<Cons_O>();
		    cform = cCdr(cform);
		    declares = Cons_O::create(sentence,declares);
		    T_sp sentenceHead = oCar(sentence);
		    sentence = cCdr(sentence);
		    if ( sentenceHead == cl::_sym_special )
		    {
			while (sentence.notnilp())
			{
			    T_sp v = oCar(sentence);
			    sentence = cCdr(sentence);
			    if ( !af_symbolp(v) )
			    {
				SIMPLE_ERROR(BF("Illegal object[%s] in declare special") % v);
			    }
			    specials = Cons_O::create(v,specials);
			}
		    }
		}
	    }
	    code = body;
	    declares = af_nreverse(declares).as_or_nil<Cons_O>();
#else // This is my old code - it doesn't separate out specials
	    Cons_sp cur = inputBody;
	    documentation = _Nil<Str_O>();
	    specials = _Nil<Cons_O>();
	    ql::list ldeclares(_lisp);
	    while (1)
	    {
		T_sp o = cur->ocar();
		if (o->stringP())
		{
		    if ( !expectDocString )
		    {
			SIMPLE_ERROR(BF("Unexpected docstring in: %s") % inputBody->__repr__() );
		    } else if (documentation.notnilp() )
		    {
			SIMPLE_ERROR(BF("Duplicate documentation in: %s") % inputBody->__repr__() );
		    } else
		    {
			documentation = o.as<Str_O>();
		    }
		} else if ( o->consP() && o.as_or_nil<Cons_O>()->ocar() == cl::_sym_declare )
		{
		    ldeclares << o;
		} else break;
		cur = cCdr(cur);
	    }
	    declares = ldeclares.cons();
	    code = cur;
	    IMPLEMENT_MEF(BF("Extract specials from declares"));
#endif

	}



	
	
#define ARGS_af_extractDeclaresDocstringCode "(body &key (expect-docstring t))"
#define DECL_af_extractDeclaresDocstringCode ""
#define DOCS_af_extractDeclaresDocstringCode "extractDeclaresDocstringCode"
	T_mv af_extractDeclaresDocstringCode(Cons_sp body, T_sp expectDocStr)
	{_G();
	    IMPLEMENT_MEF(BF("Switch to process-declarations"));
	    Cons_sp declares;
	    Str_sp docstr;
	    Cons_sp code;
	    Cons_sp specials;
	    extract_declares_docstring_code_specials(body,declares,expectDocStr,docstr,code,specials);
	    return(Values(declares,docstr,code,specials));
	};





	void extract_declares_code(Cons_sp args, Cons_sp& declares, Cons_sp& code )
	{_G();
	    Str_sp dummy_docstring;
	    Cons_sp specials;
	    IMPLEMENT_MEF(BF("Check who is using this and why they aren't calling extract_declares_docstring_code_specials directly"));
	    extract_declares_docstring_code_specials(args,declares,false,dummy_docstring,code,specials);
	}


	void parse_lambda_body(Cons_sp body, Cons_sp& declares, Str_sp& docstring, Cons_sp& code, Lisp_sp lisp)
	{_G();
	    LOG(BF("Parsing lambda body: %s") % body->__repr__() );
	    Cons_sp specials;
	    extract_declares_docstring_code_specials(body, declares, true, docstring, code, specials);
	}



/*
  __BEGIN_DOC(candoScript.specialForm.block,block)
  \scriptCmdRet{block}{command1 command2 ...}{lastObject}

  Evaluates each command and returns the value \scriptArg{lastObject} from evaluating the last command. This is what you use to write blocks of code.
  __END_DOC
*/
	T_mv sp_progn(Cons_sp args, Environment_sp environment)
	{_G();
	    Environment_sp localEnv(environment);
	    return eval::evaluateListReturnLast(args,environment);
	}



	T_mv sp_loadTimeValue(Cons_sp args, Environment_sp environment)
	{_G();
	    T_sp form = oCar(args);
	    return eval::evaluate(form,_Nil<Environment_O>());
	}

	T_mv sp_progv(Cons_sp args, Environment_sp environment)
	{_G();
	    Cons_sp symbols = eval::evaluate(oCar(args),environment).as_or_nil<Cons_O>();
	    Cons_sp values = eval::evaluate(oCadr(args),environment).as_or_nil<Cons_O>();
	    DynamicScopeManager manager;
	    for( ; symbols.notnilp(); symbols=cCdr(symbols), values=cCdr(values) )
	    {
		Symbol_sp symbol = oCar(symbols).as<Symbol_O>();
		T_sp value = oCar(values);
		manager.pushSpecialVariableAndSet(symbol,value);
	    }
	    Cons_sp forms = cCddr(args);
	    return sp_progn(forms,environment);
	}

	T_mv sp_dbg_i32(Cons_sp args, Environment_sp env)
	{_G();
	    Fixnum_sp num = oCar(args).as<Fixnum_O>();
	    printf( "+++DBG-I32[%d]\n", num->get());
	    return(Values(_Nil<T_O>()));
	}

	T_mv sp_evalWhen(Cons_sp args, Environment_sp environment)
	{_G();
	    SYMBOL_SC_(KeywordPkg,execute);
	    SYMBOL_SC_(KeywordPkg,load_toplevel);
	    Cons_sp situations = oCar(args).as_or_nil<Cons_O>();
	    Cons_sp body = cCdr(args);
	    bool execute = false;
	    if ( af_member(kw::_sym_execute,situations,_Nil<T_O>(),_Nil<T_O>(),_Nil<T_O>()).isTrue() )
	    {
		execute = true;
	    }
	    if ( execute )
	    {
		return sp_progn(body,environment);
	    }
	    return(Values(_Nil<T_O>()));
	}




	T_mv sp_the(Cons_sp args, Environment_sp env)
	{_G();
	    T_mv val = eval::evaluate(oCadr(args),env);
	    return(val);
	}





	T_mv sp_specialVar(Cons_sp args, Environment_sp env)
	{_G();
	    Symbol_sp sym = oCar(args).as<Symbol_O>();
	    return(Values(sym->symbolValue()));
	}


	T_mv sp_lexicalVar(Cons_sp args, Environment_sp env)
	{_G();
	    int depth = oCadr(args).as<Fixnum_O>()->get();
	    int index = oCddr(args).as<Fixnum_O>()->get();
	    return(Values(env->lookupValue(depth,index)));
	}



	T_mv sp_locally( Cons_sp args, Environment_sp env)
	{_G();
	    Cons_sp declares;
	    Str_sp docstring;
	    Cons_sp code;
	    Cons_sp specials;
	    extract_declares_docstring_code_specials(args,declares,false,docstring,code,specials);
	    ValueEnvironment_sp le = ValueEnvironment_O::createForLocallySpecialEntries(specials,env);
	    // ignore everything else for now
	    return eval::sp_progn(code,le);
	}



#define DOCS_af_eval "eval"
#define LOCK_af_eval 1
#define ARGS_af_eval "(form)"
#define DECL_af_eval ""
	T_mv af_eval(T_sp form)
	{_G();
	    return eval::evaluate(form,_Nil<Environment_O>());
	};




#define when_load_p(s) ((s) & FLAG_LOAD)
#define when_compile_p(s) ((s) & FLAG_COMPILE)
#define when_execute_p(s) ((s) & FLAG_EXECUTE)

	
#define DOCS_sp_eval_when "eval_when"
#define LOCK_sp_eval_when 1
#define ARGS_sp_eval_when "(situation &rest body)"
#define DECL_sp_eval_when ""
	T_mv sp_eval_when( Cons_sp args, Environment_sp env)
	{_G();
	    Cons_sp situation_list = oCar(args).as_or_nil<Cons_O>();
	    Cons_sp body = cCdr(args);
	    uint situation = 0;
	    for ( Cons_sp cursit = situation_list; cursit.notnilp(); cursit=cCdr(cursit) )
	    {
		
		Symbol_sp s = oCar(cursit).as<Symbol_O>();
		if ( s == kw::_sym_compile_toplevel ) situation |= FLAG_COMPILE;
		else if ( s == cl::_sym_compile ) situation |= FLAG_COMPILE;
		else if ( s == kw::_sym_load_toplevel ) situation |= FLAG_LOAD;
		else if ( s == cl::_sym_load ) situation |= FLAG_LOAD;
		else if ( s == kw::_sym_execute ) situation |= FLAG_EXECUTE;
		else if ( s == cl::_sym_eval ) situation |= FLAG_EXECUTE;
		else
		{
		    SIMPLE_ERROR(BF("Illegal situation[%s] for eval-when - only :compile-toplevel, :load-toplevel, :execute, compile, load or eval allowed") % _rep_(s) );
		}
	    }
	    uint mode = _lisp->mode();
	    if (mode == FLAG_EXECUTE) {
                if (!when_execute_p(situation)) body = _Nil<Cons_O>();
#if 0
	    } else if (c_env->lexical_level) {
                if (!when_execute_p(situation))
		    body = _Nil<Cons_O>();
#endif
	    } else if (mode == FLAG_LOAD) {
		if ( !when_load_p(situation) )
		{
		    body = _Nil<Cons_O>();
		}
#if 0
                if (when_compile_p(situation)) {
		    env->c_env->mode = FLAG_COMPILE;
		    execute_each_form(env, body);
		    env->c_env->mode = FLAG_LOAD;
		    if (!when_load_p(situation))
			body = _Nil<Cons_O>();
                } else
		    if (when_load_p(situation)) {
			env->c_env->mode = FLAG_ONLY_LOAD;
			flags = compile_toplevel_body(env, body, flags);
			env->c_env->mode = FLAG_LOAD;
			return flags;
		    } else {
			body = _Nil<Cons_O>();
		    }
#endif
	    } else if (mode == FLAG_ONLY_LOAD) {
                if (!when_load_p(situation))
		    body = _Nil<Cons_O>();
	    } else { /* FLAG_COMPILE */
		SIMPLE_ERROR(BF("I don't have a compiler yet"));
#if 0
                if (when_execute_p(situation) || when_compile_p(situation)) {
		    execute_each_form(env, body);
                }
		body = _Nil<Cons_O>();
#endif
	    }
	    return eval::sp_progn(body,env);
//	    return eval::evaluateListReturnLast(body,env,_lisp);
#if 0
	    return compile_toplevel_body(env, body, flags);
#endif
		
	};



	
#define DOCS_sp_step "step is implemented as a special"
#define ARGS_sp_step "(form)"
#define DECL_sp_step ""
	T_mv sp_step( Cons_sp args, Environment_sp env)
	{_G();
	    IMPLEMENT_ME();
	};






#define DOCS_sp_tagbody "tagbody special form - see CLHS"
	T_mv sp_tagbody( Cons_sp args, Environment_sp env)
	{_G();
	    TagbodyEnvironment_sp tagbodyEnv = TagbodyEnvironment_O::make(env);
	    //
	    // Find all the tags and tell the TagbodyEnvironment where they are in the list of forms.
	    //
	    for ( Cons_sp cur = args; cur.notnilp(); cur = cCdr(cur) )
	    {
	        T_sp tagOrForm = oCar(cur);
		if ( af_symbolp(tagOrForm) )
		{
		    Symbol_sp tag = tagOrForm.as<Symbol_O>();
		    // The tag is associated with its position in list of forms
		    tagbodyEnv->addTag(tag,cur);
		}
	    }
	    LOG(BF("sp_tagbody has extended the environment to: %s") % tagbodyEnv->__repr__() );
            T_sp tagbodyId = Environment_O::nilCheck_getActivationFrame(tagbodyEnv).as<TagbodyFrame_O>();
            int frame = _lisp->exceptionStack().push(TagbodyFrame,tagbodyId);
            // Start to evaluate the tagbody
            Cons_sp ip = args;
            while (ip.notnilp())
            {
                T_sp tagOrForm = oCar(ip);
                if ( af_consP(tagOrForm) )
                {
                    try
                    {
                        eval::evaluate(tagOrForm,tagbodyEnv);
                    }
                    catch (LexicalGo& go)
                    {
                        if ( go.getFrame() != frame ) {throw go;}
                        int index = go.index();
                        ip = tagbodyEnv->codePos(index);
                    }
                    catch (DynamicGo& dgo)
                    {
                        if ( dgo.getFrame() != frame ) {throw dgo;}
                        int index = dgo.index();
                        ip = tagbodyEnv->codePos(index);
                    }
                }
                ip = cCdr(ip);
            }
            LOG(BF("Leaving sp_tagbody"));
            _lisp->exceptionStack().unwind(frame);
	    return Values0<T_O>();
	};



	
#define DOCS_sp_go "go special form - see CLHS"
	T_mv sp_go( Cons_sp args, Environment_sp env)
	{_G();
	    Symbol_sp tag = oCar(args).as<Symbol_O>();
	    Environment_sp tagbodyEnvironment(_Nil<Environment_O>());
            int depth;
	    int index;
	    bool foundTag = env->findTag(tag,depth,index);
	    if ( !foundTag )
	    {
		SIMPLE_ERROR(BF("Could not find tag[%s] in the lexical environment: %s") 
                             % _rep_(tag) % _rep_(env) );
	    }
	    T_sp tagbodyId = Environment_O::nilCheck_getActivationFrame(env)->lookupTagbodyId(depth,index).as<TagbodyFrame_O>();
            int frame = _lisp->exceptionStack().findKey(TagbodyFrame,tagbodyId);
            if ( frame < 0 ) {
                SIMPLE_ERROR(BF("Could not find tagbody frame for tag %s") % _rep_(tag) );
            }
	    DynamicGo go(frame,index);
	    throw go;
	}



	
	
#define ARGS_af_classifyLetVariablesAndDeclares "(variables declared-specials)"
#define DECL_af_classifyLetVariablesAndDeclares ""
#define DOCS_af_classifyLetVariablesAndDeclares "classifyLetVariablesAndDeclares - return (values classified-variables num-lexicals) - For each variable name in variables and declared-specials classify each as special-var, lexical-var or declared-special using the declared-specials list"

	Cons_mv af_classifyLetVariablesAndDeclares(Cons_sp variables, Cons_sp declaredSpecials)
	{_G();
	    SymbolSet_sp specialsSet = SymbolSet_O::make(declaredSpecials);
	    SymbolSet_sp specialInVariables(SymbolSet_O::create());
	    HashTable_sp indices = af_make_hash_table(cl::_sym_eq,Fixnum_O::create(8),
                                                      DoubleFloat_O::create(1.5),
                                                      DoubleFloat_O::create(1.0));
	    ql::list classified(_lisp);
	    size_t indicesSize = 0;
	    for ( Cons_sp cur = variables; cur.notnilp(); cur=cCdr(cur) )
	    {
		Symbol_sp sym = oCar(cur).as<Symbol_O>();
		if ( specialsSet->contains(sym) )
		{
		    classified << Cons_O::create(ext::_sym_specialVar,sym);
		    specialInVariables->insert(sym);
		} else if ( sym->specialP() )
		{
		    classified << Cons_O::create(ext::_sym_specialVar,sym);
		    specialInVariables->insert(sym);
		} else
		{
		    int idx;
		    T_sp fi = indices->gethash(sym,_Unbound<T_O>());
		    if ( !fi.unboundp() ) {
			idx = fi.as<Fixnum_O>()->get();
		    } else {
			idx = indicesSize;
			indices->hash_table_setf_gethash(sym,Fixnum_O::create(idx));
			++indicesSize;
		    }
		    classified << Cons_O::create(ext::_sym_lexicalVar,
						 Cons_O::create(sym,Fixnum_O::create(idx)));
		}
	    }
            specialsSet->map( [&classified,&specialInVariables] (Symbol_sp s) {
                    if ( !specialInVariables->contains(s) ) {
                        classified << Cons_O::create(core::_sym_declaredSpecial,s);
                    }
                } );
	    return Values(classified.cons(),Fixnum_O::create((int)indicesSize));
	}


	/*! If evaluateInNewEnvironment is false then it behaves like let and if true it should behave like let* */
	T_mv let_letSTAR( Cons_sp args, Environment_sp parentEnvironment, bool evaluateInNewEnvironment=false)
	{_G();
	    Cons_sp assignments = oCar(args).as_or_nil<Cons_O>();
	    Cons_mv pairOfLists = af_separatePairList(assignments);
	    Cons_sp variables = pairOfLists;
	    Cons_sp expressions = pairOfLists.valueGet(1).as_or_nil<Cons_O>();
	    Cons_sp body = cCdr(args);
//    LOG(BF("Extended the environment - result -->\n%s") % newEnvironment->__repr__() );
//    LOG(BF("Evaluating code in this new lexical environment: %s") % body->__repr__() );
	    Cons_sp declares;
	    Str_sp docstring;
	    Cons_sp code;
	    Cons_sp declaredSpecials;
	    extract_declares_docstring_code_specials(body,declares,false,docstring,code,declaredSpecials);
	    LOG(BF("Assignment part=%s") % assignments->__repr__() );
	    Cons_mv classifiedAndCount = af_classifyLetVariablesAndDeclares(variables,declaredSpecials);
	    Cons_sp classified = classifiedAndCount;
	    int numberOfLexicalVariables = classifiedAndCount.valueGet(1).as<Fixnum_O>()->get();
	    ValueEnvironment_sp newEnvironment =
		ValueEnvironment_O::createForNumberOfEntries(numberOfLexicalVariables,parentEnvironment);
	    ValueEnvironmentDynamicScopeManager scope(newEnvironment);


	    // Set up the debugging info - it's empty to begin with
	    ValueFrame_sp valueFrame = newEnvironment->getActivationFrame().as<ValueFrame_O>();
	    VectorObjects_sp debuggingInfo = VectorObjects_O::create(_Nil<T_O>(),
								     af_length(valueFrame),_Nil<T_O>());
	    valueFrame->attachDebuggingInfo(debuggingInfo);


	    // Figure out which environment to evaluate in
	    Cons_sp curExp = expressions;
	    Environment_sp evaluateEnvironment;
	    if ( evaluateInNewEnvironment )
	    {
		evaluateEnvironment = newEnvironment;
	    } else
	    {
		evaluateEnvironment = parentEnvironment;
	    }

	    int debugInfoIndex = 0;
	    for ( Cons_sp curClassified = classified; curClassified.notnilp(); curClassified=cCdr(curClassified) )
	    {
		Cons_sp classified = oCar(curClassified).as_or_nil<Cons_O>();
		Symbol_sp shead = oCar(classified).as<Symbol_O>();
		if ( shead == ext::_sym_specialVar || shead == ext::_sym_lexicalVar )
		{
		    T_sp expr = oCar(curExp);
		    T_sp result = eval::evaluate(expr,evaluateEnvironment);
		    scope.new_variable(classified,result);
		    curExp = cCdr(curExp);
		} else if ( shead == _sym_declaredSpecial )
		{
		    scope.new_special(classified);
		}
		if ( shead == ext::_sym_lexicalVar )
		{
		    debuggingInfo->setf_elt(debugInfoIndex,oCadr(classified));
		    debugInfoIndex++;
		}
	    }
	    return eval::sp_progn(code,newEnvironment);
	}

/*
  __BEGIN_DOC(candoScript.specialForm.let,let)
  \scriptCmd{let}{assignments code}

  Assign lexical variables and then evaluate code in that context.
  __END_DOC
*/


/*
  __BEGIN_DOC(candoScript.specialForm.let,let)
  \scriptCmd{let\*}{assignments code}

  Assign lexical variables and then evaluate code in that context.
  __END_DOC
*/
	T_mv sp_let( Cons_sp args, Environment_sp parentEnvironment)
	{_G();
	    return let_letSTAR(args,parentEnvironment,false);
	}


	T_mv sp_letSTAR( Cons_sp args, Environment_sp parentEnvironment)
	{_G();
	    return let_letSTAR(args,parentEnvironment,true);
	}






/*
  __BEGIN_DOC(candoScript.specialForm.if,if)
  \scriptCmd{if}{condition thenCode elseCode}\par
  \scriptCmd{if}{condition thenCode }

  If/then/else control statement.
  __END_DOC
*/
	T_mv sp_if(Cons_sp args, Environment_sp environment)
	{_G();
	    T_sp res;
	    {
		res = eval::evaluate(oCar(args),environment);
	    }
	    if ( cCdddr(args).notnilp() )
	    {
		SIMPLE_ERROR(BF("Illegal if has too many expressions: %s") % _rep_(args) );
	    }
	    if ( res.isTrue() )
	    {
		return eval::evaluate(oCadr(args),environment);
	    } else
	    { if ( cCdr(cCdr(args)).notnilp() )
		{
		    return eval::evaluate(oCaddr(args),environment);
		}
	    }
	    return(Values(_Nil<T_O>()));
	}







/*
  __BEGIN_DOC(candoScript.specialForm.cond,cond)
  \scriptCmd{cond}{[ [cond1 code1 ...] [cond2 code2 ... ] ...] }\par

  Works just like lisp "cond" control structure. Evaluates each condition and for the first one that evaluates as true its associated block is evaluated.
  __END_DOC
*/
#if 1
	T_mv sp_cond(Cons_sp args, Environment_sp environment)
	{_G();
	    for ( Cons_sp cur = args; cur.notnilp(); cur = cCdr(cur) )
	    {
		T_sp cond;
		Cons_sp condProgn;
		{
		    condProgn = oCar(cur).as_or_nil<Cons_O>();
		    cond = eval::evaluate(oCar(condProgn),environment);
		}
		if ( cond.isTrue() )
		{
		    Cons_sp code = cCdr(condProgn);
		    if ( code.notnilp() )
		    {
			return eval::sp_progn(code,environment);
		    }
		    return(Values(cond));
		}
	    }
	    return(Values(_Nil<T_O>()));
	}
#endif



	T_mv sp_block( Cons_sp args, Environment_sp environment)
	{_G();
	    Symbol_sp blockSymbol = oCar(args).as<Symbol_O>();
	    BlockEnvironment_sp newEnvironment = BlockEnvironment_O::make(blockSymbol,environment);
            int frame = _lisp->exceptionStack().push(BlockFrame,blockSymbol);
	    LOG(BF("sp_block has extended the environment to: %s") % newEnvironment->__repr__() );
	    T_mv result;
	    try {
		result = eval::sp_progn(cCdr(args),newEnvironment);
	    } catch (ReturnFrom& returnFrom) {
		LOG(BF("Caught ReturnFrom with returnFrom.getBlockDepth() ==> %d") % returnFrom.getBlockDepth() );
		if ( returnFrom.getFrame() != frame ) // Symbol() != newEnvironment->getBlockSymbol() )
		{
		    throw returnFrom;
		}
		result = gctools::multiple_values<T_O>::createFromValues();  // returnFrom.getReturnedObject();
	    }
	    LOG(BF("Leaving sp_block"));
            _lisp->exceptionStack().unwind(frame);
	    return result;
	}


	T_mv sp_returnFrom( Cons_sp args, Environment_sp environment)
	{_G();
	    Symbol_sp blockSymbol = oCar(args).as<Symbol_O>();
            int frame = _lisp->exceptionStack().findKey(BlockFrame,blockSymbol);
            if ( frame < 0 ) {
		SIMPLE_ERROR(BF("Could not find block named %s in lexical environment: %s") % _rep_(blockSymbol) % _rep_(environment) );
	    }
	    T_mv result = Values(_Nil<T_O>());
	    if ( cCdr(args).notnilp() )
	    {
		result = eval::evaluate(oCadr(args),environment);
	    }
            result.saveToMultipleValue0();
            ReturnFrom returnFrom(frame);
            throw returnFrom;
	}

#if 1 // new way using RAII
	T_mv sp_unwindProtect( Cons_sp args, Environment_sp environment)
	{_G();
            MultipleValues* mv = lisp_multipleValues();
            gctools::Vec0<T_sp>  save;
            struct UnwindProtectDone {};
	    try {
                // Evaluate the protected form
		T_mv result = eval::evaluate(oCar(args),environment);
                result.saveToMultipleValue0();
                throw(UnwindProtectDone());
            } catch (UnwindProtectDone& dummy) {
                // Save the return values
                mv->saveToVec0(save);
                // Evaluate the unwind forms -- This is wrong - it shouldn't be protected here
		eval::sp_progn(cCdr(args),environment);
            } catch (...) {
                mv->saveToVec0(save);
                eval::sp_progn(cCdr(args),environment);
                mv->loadFromVec0(save);
                throw;
            }
	    return gctools::multiple_values<T_O>::createFromVec0(save);
	}

#else // old
  #if 0
        // use gctools::Vec0
	T_mv sp_unwindProtect( Cons_sp args, Environment_sp environment)
	{_G();
            T_mv result = Values(_Nil<T_O>());
            MultipleValues* mv = lisp_multipleValues();
            gctools::Vec0<T_sp> save;
            TRY()
	    {
                // Evaluate the protected form
		result = eval::evaluate(oCar(args),environment);
                // Save the return values
                mv->saveToVec0(save);
                // Evaluate the unwind forms --
                // THIS IS REALLY, REALLY WRONG - it shouldn't be protected here
		eval::sp_progn(cCdr(args),environment);
	    } catch (...)
	      {
                  mv->saveToVec0(save);
		  eval::sp_progn(cCdr(args),environment);
		  throw;
	      }
	    return gctools::multiple_values<T_O>::createFromVec0(save);
	}
  #else
        // original
	T_mv sp_unwindProtect( Cons_sp args, Environment_sp environment)
	{_G();
	    T_mv result = Values(_Nil<T_O>());
	    VectorObjects_sp save(VectorObjects_O::create());
	    TRY()
	    {
                // Evaluate the protected form
		result = eval::evaluate(oCar(args),environment);
                // Save the return values
		multipleValuesSaveToVector(result,save);
                // Evaluate the unwind forms --
                // THIS IS REALLY, REALLY WRONG - it shouldn't be protected here
		eval::sp_progn(cCdr(args),environment);
	    } catch (...)
	      {
		  eval::sp_progn(cCdr(args),environment);
		  throw;
	      }
	    return multipleValuesLoadFromVector(save);
	}
  #endif
#endif



	T_mv sp_catch( Cons_sp args, Environment_sp environment)
	{_G();
	    T_sp mytag = eval::evaluate(oCar(args),environment);
            int frame = _lisp->exceptionStack().push(CatchFrame,mytag);
	    T_mv result;
	    try {
		result = eval::sp_progn(cCdr(args),environment);
	    } catch (CatchThrow& catchThrow) {
                if ( catchThrow.getFrame() != frame )
                {
                    throw catchThrow;
                }
                result = gctools::multiple_values<T_O>::createFromValues();
            }
            _lisp->exceptionStack().unwind(frame);
	    return result;
	}






	T_mv sp_throw( Cons_sp args, Environment_sp environment)
	{_G();
	    T_sp throwTag = eval::evaluate(oCar(args),environment);
	    T_mv result = Values(_Nil<T_O>());
            int frame = _lisp->exceptionStack().findKey(CatchFrame,throwTag);
            if ( frame < 0 ) {
                CONTROL_ERROR();
            }
	    if ( cCdr(args).notnilp() )
	    {
		result = eval::evaluate(oCadr(args),environment);
	    }
            // The first return value needs to be saved in MultipleValues
            result.saveToMultipleValue0();
            // I should search for the Catch frame for throwTag and
            // invoke an error if it doesn't exist
	    throw CatchThrow(frame);
	}



	T_mv sp_multipleValueProg1(Cons_sp args, Environment_sp environment)
	{_G();
	    VectorObjects_sp save(VectorObjects_O::create());
	    T_mv val0 = eval::evaluate(oCar(args), environment);
	    multipleValuesSaveToVector(val0,save);
	    eval::evaluateListReturnLast(cCdr(args),environment);
	    return multipleValuesLoadFromVector(save);
	}





	T_mv sp_multipleValueCall(Cons_sp args, Environment_sp env)
	{_G();
	    Function_sp func;
	    func = eval::evaluate(oCar(args),env).as<Function_O>();
	    Cons_sp forms = cCdr(args);
	    ql::list resultList(_lisp);
	    Cons_sp results = _Nil<Cons_O>();
	    for ( Cons_sp forms=cCdr(args); forms.notnilp(); forms=cCdr(forms) )
	    {
		T_sp oneForm = oCar(forms);
		T_mv retval = eval::evaluate(oneForm,env);
		resultList << retval;
		for ( int i(1); i<retval.number_of_values(); i++ )
		{
		    resultList << retval.valueGet(i);
		}
	    }
	    ValueFrame_sp vf = ValueFrame_O::create(resultList.cons(),Environment_O::nilCheck_getActivationFrame(env));
	    T_mv result = eval::applyToActivationFrame(func,vf);
	    return(result);
	}



#define ARGS_af_processDeclarations "(body &optional (expectDocString t))"
#define DECL_af_processDeclarations ""
#define DOCS_af_processDeclarations "Handle special declarations and remove declarations from body. Return MultipleValues: declarations body documentation specials"
	T_mv af_processDeclarations(Cons_sp inputBody, T_sp expectDocString)
	{_G();
	    bool b_expect_doc = expectDocString.isTrue();
	    Cons_sp declares;
	    Str_sp docstring;
	    Cons_sp code;
	    Cons_sp specials;
	    extract_declares_docstring_code_specials(inputBody, declares,
						     b_expect_doc, docstring, code, specials);
	    return(Values(declares,code,docstring,specials));
	};


	/*! Parse a lambda expression of the form ([declare*] ["docstring"] body...) */
	Function_sp lambda(T_sp name, bool wrap_block, T_sp lambda_list, Cons_sp body, Environment_sp env)
	{_G();
	    Cons_sp declares;
	    Str_sp docstring;
	    Cons_sp form;
	    parse_lambda_body(body,declares,docstring,form,_lisp);
	    LOG(BF("lambda is closing over environment\n%s") % env->__repr__() );
	    LambdaListHandler_sp llh;
	    if ( lambda_list.nilp() )
	    {
		llh = lisp_function_lambda_list_handler(_lisp,_Nil<Cons_O>(),declares);
	    } else if ( af_consP(lambda_list) )
	    {
		llh = lisp_function_lambda_list_handler(_lisp,lambda_list.as_or_nil<Cons_O>(),declares);
		LOG(BF("Passed lambdaList: %s" ) % lambda_list->__repr__() );
	    } else if ( af_lambda_list_handler_p(lambda_list) )
	    {
		llh = lambda_list.as<LambdaListHandler_O>();
	    } else
	    {
		SIMPLE_ERROR(BF("Illegal object for lambda-list you can "
				      "only pass a Cons or LambdaListHandler"));
	    }
	    Cons_sp code(form);
	    if ( wrap_block )
	    {
		code = Cons_O::create(Cons_O::create(cl::_sym_block,
						     Cons_O::create(
							 af_functionBlockName(name),
							 code)));
                if ( _lisp->sourceDatabase().notnilp() ) {
                    _lisp->sourceDatabase()->duplicateSourceInfo(body,code);
                }
	    }
	    Function_sp proc = Interpreted_O::create(name,llh,declares,docstring,code,env,kw::_sym_function);
	    return proc;
	}






/*
  __BEGIN_DOC(candoScript.specialForm.function,function)
  \scriptCmd{function}{object}

  Returns function associated with the name.
  (name) is either a symbol or a lambda or lambda-block expression.
  (lambda (args...) body...) or (lambda-block name (args...) body...)
  __END_DOC
*/
	T_mv sp_function(Cons_sp args, Environment_sp environment)
	{_G();
	    ASSERTP(cCdr(args).nilp(),"You can provide only one argument - a symbol that has a function bound to it or a lambda");
	    T_sp arg = oCar(args);
	    if ( af_symbolp(arg) )
	    {
		Symbol_sp fnSymbol = oCar(args).as<Symbol_O>();
		LOG(BF("In sp_function - looking up for for[%s]")
		    % fnSymbol->__repr__() );
		T_sp fn = af_interpreter_lookup_function(fnSymbol,environment);
		if (!fn.pointerp())
		{
		    SIMPLE_ERROR(BF("Could not find function %s args: %s") % _rep_(fnSymbol) % _rep_(args) );
		}
		LOG(BF("     Found form: %s") % fn->__repr__() );
		return(Values(fn));
	    } else if ( af_consP(arg) )
	    {
		Cons_sp consArg = arg.as_or_nil<Cons_O>();
		T_sp head = oCar(consArg);
		if ( head == cl::_sym_setf )
		{
		    T_sp fn = af_interpreter_lookup_setf_function(consArg,environment);
		    if ( fn.nilp() )
		    {
			SIMPLE_ERROR(BF("Could not find function %s args: %s") % _rep_(consArg) % _rep_(args) );
		    }
		    return(Values(fn));
		} else if ( head == cl::_sym_lambda || head == ext::_sym_lambda_block)
		{
		    Symbol_sp name;
		    Cons_sp lambdaList;
		    Cons_sp body;
		    bool wrapBlock = false;
		    if ( head == cl::_sym_lambda )
		    {
			name = _sym_anonymous;
			lambdaList = oCadr(consArg).as_or_nil<Cons_O>();
			body = cCddr(consArg);
			wrapBlock = false;
		    } else // head==cl::_sym_lambda_block
		    {
			name = af_functionBlockName(oCadr(consArg));
			lambdaList = oCaddr(consArg).as_or_nil<Cons_O>();
			body = cCdddr(consArg);
			wrapBlock = true;
		    }
//		    HALT(BF("Check name/lambdaList/body and if ok remove me"));
		    // Create an anonymous function and close it over the current environment
		    Function_sp lambdaFunction = lambda(name,
							wrapBlock,
							lambdaList,
							body,
							environment);
		    return(Values(lambdaFunction));
		}
	    }
	    SIMPLE_ERROR(BF("Illegal argument[%s] for function") % _rep_(arg) );
	}












#if 0
#define DOCS_sp_lambda_block "Like lambda but the first argument is a symbol that defines the name of the lambda"
	T_mv sp_lambda_block( Cons_sp args, Environment_sp env)
	{_G();
	    ASSERTNOTNULL(args);
	    Symbol_sp name = args->ocar().as<Symbol_O>();
	    return lambda(name,true,args->ocadr(),args->cddr(),env);
	}
#endif

#if 0
#define DOCS_sp_lambda_with_handler "Like lambda but the first argument is a symbol that defines the name of the lambda and the second argument is a lambda-list-handler rather than a lambda-list"
	T_mv sp_lambda_with_handler( Cons_sp args, Environment_sp env)
	{_G();
	    ASSERTNOTNULL(args);
	    Symbol_sp name = args->ocar().as<Symbol_O>();
	    LambdaListHandler_sp llh = eval::evaluate(args->ocadr(),env).as<LambdaListHandler_O>();
	    return lambda(name,false,llh,args->cddr(),env,_lisp);
	}
#endif






/*
  __BEGIN_DOC(candoScript.specialForm.quote,quote)
  \scriptCmdRet{quote}{object}{unevaluatedObject}

  Returns the \scriptArg{object} without evaluating it.
  __END_DOC
*/
	T_mv sp_quote(Cons_sp args, Environment_sp environment)
	{_G();
	    ASSERTF(af_length(args)==1,BF("Only one argument allowed for QUOTE"));
	    return(Values(oCar(args)));
	}


	



/*
  __BEGIN_DOC(candoScript.general.let,let)
  \scriptCmd{let}{symbol object}\par
  \scriptInfix{symbol}{=}{object}

  Evaluate the arguments and put it into the local variable \scriptArg{symbol}.
  __END_DOC
*/
	T_mv sp_setq(Cons_sp args, Environment_sp environment)
	{_G();
	    ASSERTP(cCdr(args).notnilp(),"You must provide at least 2 arguments");
	    Cons_sp pairs = args;
	    T_sp result = _Nil<T_O>();
	    while (pairs.notnilp())
	    {
		T_sp target = oCar(pairs);
		if ( Symbol_sp symbol = target.asOrNull<Symbol_O>() )
		{
		    if ( cCdr(pairs).nilp() )
		    {
			SIMPLE_ERROR(BF("Missing value for setq of target[%s] - body of setq: %s")
					   % _rep_(target) % _rep_(args) );
		    }
		    T_sp expr = oCadr(pairs);
		    T_sp texpr = af_macroexpand(symbol,environment);
		    if ( texpr != symbol )
		    {
			// The target symbol was a symbol-macro so we
			// switch from SETQ to a SETF to define it
			eval::evaluate(Cons_O::createList(cl::_sym_setf,texpr,expr),environment);
		    } else
		    {
			result = eval::evaluate(expr,environment);
			interpret::setq_symbol_value(symbol,result,environment);
		    }
		    pairs = cCddr(pairs);
		} else
		{
		    SIMPLE_ERROR(BF("Illegal target[%s] for setq - body of setq: %s") % _rep_(target) % _rep_(args) );
		}
	    }
	    return(Values(result) );
	}






	
		



	T_mv sp_flet(Cons_sp args, Environment_sp environment)
	{_G();
	    // TODO: handle trace
	    T_sp functionName;
	    Cons_sp functions = oCar(args).as_or_nil<Cons_O>();
	    FunctionValueEnvironment_sp newEnvironment = FunctionValueEnvironment_O::createForEntries(af_length(functions),environment);
	    Cons_sp body = cCdr(args);
	    Cons_sp cur = functions;
	    LOG(BF("functions part=%s") % functions->__repr__() );
	    Str_sp docString = _Nil<Str_O>();
	    while ( cur.notnilp() )
	    {
		Cons_sp oneDef = cCar(cur);
		functionName = oCar(oneDef);
		Function_sp func = lambda(functionName,true,oCadr(oneDef),cCddr(oneDef),environment);
		newEnvironment->bind_function(functionName,func);
		cur = cCdr(cur);
	    }
	    Cons_sp declares;
	    Cons_sp code;
	    Str_sp docstring;
	    Cons_sp specials;
	    extract_declares_docstring_code_specials(body,declares,false,docstring,code,specials);
	    return eval::sp_progn(code,newEnvironment);
	}


/*
  __BEGIN_DOC(candoScript.macros.labels,labels)
  \scriptCmd{labels}{(function bindings) code...}

  Define functions recursively in new lexical environments.
  __END_DOC
*/
	T_mv sp_labels(Cons_sp args, Environment_sp environment)
	{_G();
	    // TODO: handle trace
	    T_sp name;
	    Cons_sp functions = oCar(args).as_or_nil<Cons_O>();
	    Cons_sp body = cCdr(args);
	    Cons_sp cur = functions;
	    LOG(BF("functions part=%s") % functions->__repr__() );
	    Str_sp docString = _Nil<Str_O>();
	    FunctionValueEnvironment_sp newEnvironment = FunctionValueEnvironment_O::createForEntries(af_length(functions),environment);
	    while ( cur.notnilp() )
	    {
		Cons_sp oneDef = cCar(cur);
		name = oCar(oneDef);
		Function_sp func = lambda(name,true,oCadr(oneDef),cCddr(oneDef),newEnvironment);
		LOG(BF("func = %s") % func->__repr__() );
		newEnvironment->bind_function(name,func);
		cur = cCdr(cur);
	    }
	    Cons_sp declares;
	    Cons_sp code;
	    Str_sp docstring;
	    Cons_sp specials;
	    extract_declares_docstring_code_specials(body,declares,false,docstring,code,specials);
	    return eval::sp_progn(code,newEnvironment);
	}


/*
  __BEGIN_DOC(candoScript.macros.macroLet,macroLet)
  \scriptCmd{macroLet}{(function bindings) code...}

  Define macros recursively in new lexical environments.
  __END_DOC
*/
	T_mv sp_macrolet(Cons_sp args, Environment_sp env)
	{_G();
	    // TODO: handle trace
	    Cons_sp macros = oCar(args).as_or_nil<Cons_O>();
	    MacroletEnvironment_sp newEnv(MacroletEnvironment_O::make(env));
	    Cons_sp body = cCdr(args).as_or_nil<Cons_O>();
	    Cons_sp cur = macros;
	    LOG(BF("macros part=%s") % macros->__repr__() );
	    Str_sp docString = _Nil<Str_O>();
	    while ( cur.notnilp() )
	    {
		Cons_sp oneDef = oCar(cur).as_or_nil<Cons_O>();
		Symbol_sp name = oCar(oneDef).as<Symbol_O>();
		T_sp olambdaList = oCadr(oneDef);
		Cons_sp inner_body = cCdr(cCdr(oneDef)).as_or_nil<Cons_O>();
		Cons_sp outer_func_cons = eval::funcall(comp::_sym_parse_macro,name,olambdaList,inner_body).as_or_nil<Cons_O>();
		Cons_sp outer_ll = oCaddr(outer_func_cons).as_or_nil<Cons_O>();
		Cons_sp outer_body = cCdddr(outer_func_cons);
		Cons_sp declares;
		Str_sp docstring;
		Cons_sp code;
		parse_lambda_body(outer_body,declares,docstring,code,_lisp);
		LambdaListHandler_sp outer_llh = LambdaListHandler_O::create(outer_ll,declares,cl::_sym_function);
		Function_sp outer_func = Interpreted_O::create(name,outer_llh,
							       declares,docstring,
							       code,newEnv,
							       kw::_sym_macro );
		LOG(BF("func = %s") % outer_func_cons->__repr__() );
		newEnv->addMacro(name,outer_func);
//		newEnv->bind_function(name,outer_func);
		cur = cCdr(cur);
	    }
	    Cons_sp declares;
	    Cons_sp code;
	    Str_sp docstring;
	    Cons_sp specials;
	    extract_declares_docstring_code_specials(body,declares,false,docstring,code,specials);
	    return eval::sp_progn(code,newEnv);
	}




	T_mv sp_symbolMacrolet(Cons_sp args, Environment_sp env)
	{_G();
	    Cons_sp macros = oCar(args).as_or_nil<Cons_O>();
	    SymbolMacroletEnvironment_sp newEnv(SymbolMacroletEnvironment_O::make(env));
	    Cons_sp body = cCdr(args).as_or_nil<Cons_O>();
	    Cons_sp cur = macros;
	    LOG(BF("macros part=%s") % macros->__repr__() );
	    Str_sp docString = _Nil<Str_O>();
	    SYMBOL_SC_(CorePkg,whole);
	    SYMBOL_SC_(CorePkg,env);
	    Cons_sp outer_ll = Cons_O::createList(_sym_whole, _sym_env);
	    SYMBOL_EXPORT_SC_(ClPkg,ignore);
	    Cons_sp declares = Cons_O::createList(cl::_sym_declare,Cons_O::createList(cl::_sym_ignore,_sym_whole,_sym_env));
	    while ( cur.notnilp() )
	    {
		Cons_sp oneDef = oCar(cur).as_or_nil<Cons_O>();
		Symbol_sp name = oCar(oneDef).as<Symbol_O>();
		Cons_sp expansion = Cons_O::create(Cons_O::createList(cl::_sym_quote,oCadr(oneDef)),_Nil<Cons_O>());
		LambdaListHandler_sp outer_llh = LambdaListHandler_O::create(outer_ll,
									     oCadr(declares).as_or_nil<Cons_O>(),
									     cl::_sym_function);
		Function_sp outer_func = Interpreted_O::create(_Nil<T_O>(),
							       outer_llh,
							       declares,
							       _Nil<Str_O>(),
							       expansion,
							       newEnv,
							       kw::_sym_macro );
		newEnv->addSymbolMacro(name,outer_func);
		cur = cCdr(cur);
	    }
	    return eval::sp_locally(body,newEnv);
	}







	T_mv handleConditionInEvaluate(Environment_sp environment)
	{_G();
	    T_mv result;
	    try
	    {
		throw;
	    }
	    catch (Condition& cond)
	    {
		THROW_HARD_ERROR(BF("Figure out what should happen here"));
#if 0
		try
		{
		    THROW(_lisp->error(cond.conditionObject()/*,environment */));
		}
		catch (DebuggerSaysContinue& debuggerSaysResume)
		{
		    T_mv resumeResult = debuggerSaysResume.returnObject();
		    LOG(BF("Execution will resume with return value[%s]") % resumeResult->__repr__() );
		    return resumeResult;
		}
		catch (HardError& err)
		{
		    // Convert the HardError into a Condition
		    SIMPLE_ERROR(BF("HARD_ERROR: %s") % err.message() );
		}
//		catch (...) { throw;}
		;
#endif
	    }
	    catch (HardError& err)
	    {
		// Convert the HardError into a Condition
		SIMPLE_ERROR(BF("HARD_ERROR: %s") % err.message() );
	    }
#if 0
	    catch (DebuggerSaysContinue& debuggerSaysResume)
	    {
		T_mv resumeResult = debuggerSaysResume.returnObject();
		LOG(BF("Execution will resume with return value[%s]") % resumeResult->__repr__() );
		return resumeResult;
	    }
#endif
	    catch (const std::exception &exc)
	    {
		SIMPLE_ERROR(BF("std::exception--> %s") % exc.what() );
	    }
	    SIMPLE_ERROR(BF("Failed to handle exception"));
	}

	    








	Function_sp lookupFunction(T_sp functionDesignator, Environment_sp env)
	{_G();
	    ASSERTF(functionDesignator,BF("In apply, the head function designator is UNDEFINED"));
	    if ( Function_sp exec = functionDesignator.asOrNull<Function_O>() ) return exec;
	    Symbol_sp shead = functionDesignator.as<Symbol_O>();
	    Function_sp exec = af_interpreter_lookup_function(shead,env);
	    return exec;
	}



	T_mv applyToActivationFrame(T_sp head,ActivationFrame_sp args )
	{_G();
	    Function_sp fn = lookupFunction(head,args);
	    if ( !fn.pointerp() )
	    {
		if ( head == cl::_sym_findClass )
		{
		    // When booting, cl::_sym_findClass may be apply'd but not
		    // defined yet
		    return(af_findClass(args->entry(0).as<Symbol_O>(),true,_Nil<Environment_O>()));
		}
		SIMPLE_ERROR(BF("Could not find function %s args: %s") % _rep_(head) % _rep_(args));
	    }
	    return fn->INVOKE(args->length(),args->argArray()); // return applyFunctionToActivationFrame(fn,args);
	}




/*!
 * This method:
 * 1) evaluates the arguments
 * 2) Looks up the method using the methodCall and the first argument
 * 3) evaluates the method
 * Can return MultipleValues
 */



	int _evaluateVerbosity = 0;
	int _evaluateDepth = 0;



	
	
#define ARGS_af_evaluateDepth "()"
#define DECL_af_evaluateDepth ""
#define DOCS_af_evaluateDepth "evaluateDepth"
	int af_evaluateDepth()
	{_G();
	    return _evaluateDepth;
	};

	
	
#define ARGS_af_evaluateVerbosity "(arg)"
#define DECL_af_evaluateVerbosity ""
#define DOCS_af_evaluateVerbosity "evaluateVerbosity"
	void af_evaluateVerbosity(Fixnum_sp level)
	{_G();
	    _evaluateVerbosity = level->get();
	};

	struct EvaluateDepthUpdater
	{
	    EvaluateDepthUpdater()
	    {
		++_evaluateDepth;
	    }
	    ~EvaluateDepthUpdater()
	    {
		--_evaluateDepth;
	    }
	};



	T_mv evaluate_atom(T_sp exp, Environment_sp environment)
	{
	    T_mv result;
	    LOG(BF("Evaluating atom: %s")% exp->__repr__());
	    if ( exp.nilp() ) return Values(_Nil<T_O>());
            else if ( Symbol_sp sym = exp.asOrNull<Symbol_O>() )
	    {_BLOCK_TRACEF(BF("Evaluating symbol: %s")% exp->__repr__() );
		if ( sym->isKeywordSymbol() ) return Values(sym);
		if ( af_interpreter_lookup_symbol_macro(sym,environment).notnilp() )
		{
		    T_sp texpr;
		    {MULTIPLE_VALUES_CONTEXT();
			texpr = af_macroexpand(sym,environment);
		    }
		    try {result = eval::evaluate(texpr,environment);}
		    catch (...) {result = handleConditionInEvaluate(environment);};
		    return(result);
		}
		try {result = af_interpreter_lookup_variable(sym,environment);}
		catch (...) {result = handleConditionInEvaluate(environment);}
		return(result);
	    }
	    LOG(BF(" Its the self returning object: %s")% exp->__repr__() );
	    return(Values(exp));
	}


	T_mv evaluate_lambdaHead( Cons_sp headCons, Cons_sp form, Environment_sp environment )
	{
	    T_mv result;
	    if (oCar(headCons) == cl::_sym_lambda)
	    {
		ASSERTF(oCar(headCons)==cl::_sym_lambda,BF("Illegal head %s - must be a LAMBDA expression") % _rep_(headCons) );
		//
		// The head is a cons with a non-symbol for a head, evaluate it
		//
		{
		    if ( _lisp->isSingleStepOn() )
		    {
			IMPLEMENT_ME();
#if 0
			LispDebugger::step();
#endif
		    }
		    ValueFrame_sp evaluatedArgs(ValueFrame_O::create(af_length(cCdr(form)),_Nil<ActivationFrame_O>()));
		    evaluateIntoActivationFrame(evaluatedArgs,cCdr(form),environment);
		    try { result = eval::applyToActivationFrame(headCons,evaluatedArgs);}
		    catch (...) { result = handleConditionInEvaluate(environment);};
		}
	    } else
	    {
		SIMPLE_ERROR(BF("Illegal form: %s") % _rep_(form) );
	    }
	    return(result);
	}


	T_mv evaluate_specialForm( SpecialForm_sp specialForm, Cons_sp form, Environment_sp environment )
	{
//		    LOG(BF("Evaluating specialForm non-atom: %s")% specialForm->__repr__() );
	    T_mv result;
	    try { result = specialForm->evaluate(cCdr(form),environment);}
	    catch (...) { result = handleConditionInEvaluate(environment); }
	    ASSERTNOTNULL(result);
	    return(result);
	}


	T_mv evaluate_cond(Cons_sp form, Environment_sp environment )
	{_G();
	    T_mv result;
	    try { result = interpret::interpreter_cond(cCdr(form),environment);}
	    catch (...) { result = handleConditionInEvaluate(environment); }
	    ASSERTNOTNULL(result);
	    return(result);
	}
	
	T_mv evaluate_case(Cons_sp form, Environment_sp environment )
	{_G();	
	    T_mv result;
	    try { result = interpret::interpreter_case(cCdr(form),environment);}
	    catch (...) { result = handleConditionInEvaluate(environment); }
	    ASSERTNOTNULL(result);
	    return(result);
	}


	T_mv evaluate_multipleValueSetq(Cons_sp form, Environment_sp environment )
	{_G();
	    T_mv result;
	    SYMBOL_EXPORT_SC_(ClPkg,multipleValueSetq);
	    try { result = interpret::interpreter_multipleValueSetq(cCdr(form),environment);}
	    catch (...) { result = handleConditionInEvaluate(environment); }
	    ASSERTNOTNULL(result);
	    return(result);
	}


	T_mv evaluate_prog1( Cons_sp form, Environment_sp environment )
	{_G();
	    T_mv result;
	    SYMBOL_EXPORT_SC_(ClPkg,prog1);
	    try { result = interpret::interpreter_prog1(cCdr(form),environment);}
	    catch (...) { result = handleConditionInEvaluate(environment); }
	    ASSERTNOTNULL(result);
	    return(result);
	}


        T_mv t1Evaluate(T_sp exp, Environment_sp environment);

	T_mv t1Progn(T_sp args, Environment_sp environment)
	{_G();
            T_mv result(_Nil<T_O>());
	    Environment_sp localEnv(environment);
            for ( Cons_sp cur=args; cur.notnilp(); cur=cCdr(cur) ) {
                result = t1Evaluate(oCar(cur),localEnv);
            }
            return result;
	}

	T_mv t1EvalWhen(T_sp args, Environment_sp environment)
	{_G();
	    Cons_sp situations = oCar(args).as_or_nil<Cons_O>();
	    Cons_sp body = cCdr(args);
	    bool execute = af_member(kw::_sym_execute,situations,_Nil<T_O>(),_Nil<T_O>(),_Nil<T_O>()).isTrue();
	    if ( execute ) return t1Progn(body,environment);
	    return(Values(_Nil<T_O>()));
	}

	T_mv t1Locally(Cons_sp args, Environment_sp env)
	{_G();
	    Cons_sp declares;
	    Str_sp docstring;
	    Cons_sp code;
	    Cons_sp specials;
	    extract_declares_docstring_code_specials(args,declares,false,docstring,code,specials);
	    ValueEnvironment_sp le = ValueEnvironment_O::createForLocallySpecialEntries(specials,env);
	    // ignore everything else for now
	    return eval::t1Progn(code,le);
	}

	T_mv t1Macrolet(Cons_sp args, Environment_sp env)
	{_G();
	    // TODO: handle trace
	    Cons_sp macros = oCar(args).as_or_nil<Cons_O>();
	    MacroletEnvironment_sp newEnv(MacroletEnvironment_O::make(env));
	    Cons_sp body = cCdr(args).as_or_nil<Cons_O>();
	    Cons_sp cur = macros;
	    LOG(BF("macros part=%s") % macros->__repr__() );
	    Str_sp docString = _Nil<Str_O>();
	    while ( cur.notnilp() )
	    {
		Cons_sp oneDef = oCar(cur).as_or_nil<Cons_O>();
		Symbol_sp name = oCar(oneDef).as<Symbol_O>();
		T_sp olambdaList = oCadr(oneDef);
		Cons_sp inner_body = cCdr(cCdr(oneDef)).as_or_nil<Cons_O>();
		Cons_sp outer_func_cons = eval::funcall(comp::_sym_parse_macro,name,olambdaList,inner_body).as_or_nil<Cons_O>();
		Cons_sp outer_ll = oCaddr(outer_func_cons).as_or_nil<Cons_O>();
		Cons_sp outer_body = cCdddr(outer_func_cons);
		Cons_sp declares;
		Str_sp docstring;
		Cons_sp code;
		parse_lambda_body(outer_body,declares,docstring,code,_lisp);
		LambdaListHandler_sp outer_llh = LambdaListHandler_O::create(outer_ll,declares,cl::_sym_function);
		Function_sp outer_func = Interpreted_O::create(name,outer_llh,
							       declares,docstring,
							       code,newEnv,
							       kw::_sym_macro );
		LOG(BF("func = %s") % outer_func_cons->__repr__() );
		newEnv->addMacro(name,outer_func);
//		newEnv->bind_function(name,outer_func);
		cur = cCdr(cur);
	    }
	    Cons_sp declares;
	    Cons_sp code;
	    Str_sp docstring;
	    Cons_sp specials;
	    extract_declares_docstring_code_specials(body,declares,false,docstring,code,specials);
	    return t1Progn(code,newEnv);
	}

	T_mv t1SymbolMacrolet(Cons_sp args, Environment_sp env)
	{_G();
	    Cons_sp macros = oCar(args).as_or_nil<Cons_O>();
	    SymbolMacroletEnvironment_sp newEnv(SymbolMacroletEnvironment_O::make(env));
	    Cons_sp body = cCdr(args).as_or_nil<Cons_O>();
	    Cons_sp cur = macros;
	    LOG(BF("macros part=%s") % macros->__repr__() );
	    Str_sp docString = _Nil<Str_O>();
	    SYMBOL_SC_(CorePkg,whole);
	    SYMBOL_SC_(CorePkg,env);
	    Cons_sp outer_ll = Cons_O::createList(_sym_whole, _sym_env);
	    SYMBOL_EXPORT_SC_(ClPkg,ignore);
	    Cons_sp declares = Cons_O::createList(cl::_sym_declare,Cons_O::createList(cl::_sym_ignore,_sym_whole,_sym_env));
	    while ( cur.notnilp() )
	    {
		Cons_sp oneDef = oCar(cur).as_or_nil<Cons_O>();
		Symbol_sp name = oCar(oneDef).as<Symbol_O>();
		Cons_sp expansion = Cons_O::create(Cons_O::createList(cl::_sym_quote,oCadr(oneDef)),_Nil<Cons_O>());
		LambdaListHandler_sp outer_llh = LambdaListHandler_O::create(outer_ll,
									     oCadr(declares).as_or_nil<Cons_O>(),
									     cl::_sym_function);
		Function_sp outer_func = Interpreted_O::create(_Nil<T_O>(),
							       outer_llh,
							       declares,
							       _Nil<Str_O>(),
							       expansion,
							       newEnv,
							       kw::_sym_macro );
		newEnv->addSymbolMacro(name,outer_func);
		cur = cCdr(cur);
	    }
	    return t1Locally(body,newEnv);
	}

        T_mv t1Evaluate(T_sp exp, Environment_sp environment)
        {
            if ( af_consP(exp) ) {
                T_sp head = oCar(exp);
                if ( head == cl::_sym_progn ) {
                    return t1Progn(oCdr(exp),environment);
                } else if ( head == cl::_sym_eval_when ) {
                    return t1EvalWhen(oCdr(exp),environment);
                } else if ( head == cl::_sym_locally ) {
                    return t1Locally(oCdr(exp),environment);
                } else if ( head == cl::_sym_macrolet ) {
                    return t1Macrolet(oCdr(exp),environment);
                } else if ( head == cl::_sym_symbol_macrolet ) {
                    return t1SymbolMacrolet(oCdr(exp),environment);
                }
            }
            if ( _sym_STARdebugEvalSTAR && _sym_STARdebugEvalSTAR->symbolValue().notnilp() ) {
                printf("%s:%d About to evalWithEnv: %s\n", __FILE__, __LINE__, _rep_(exp).c_str());
            }
            return eval::funcall(_sym_evalWithEnv,exp,environment);
        }

    
#define ARGS_af_topLevelEvalWithEnv "(form &optional env stepping compiler-env-p (execute t))"
#define DECL_af_topLevelEvalWithEnv ""
#define DOCS_af_topLevelEvalWithEnv "topLevelEvalWithEnv"
    T_mv af_topLevelEvalWithEnv(T_sp form, Environment_sp env, bool stepping,       bool compiler_env_p,      bool execute)
    {_G();
        return t1Evaluate(form,env);
    }


    

	T_mv evaluate(T_sp exp, Environment_sp environment)
	{_G();
	    Environment_sp localEnvironment = environment;
	    T_mv result;
	    af_stackMonitor();
	    EvaluateDepthUpdater evaluateDepthUpdater;
	    if ( _evaluateVerbosity>0 )
	    {
		string ts = _rep_(exp);
		printf("core::eval::evaluate depth[%5d] -> %s\n", _evaluateDepth, ts.c_str());
	    }
	    if ( exp.nilp() ) 
	    {
//		LOG(BF("Expression is nil - returning nil"));
 		return Values(exp);
	    }
	    if ( af_atom(exp) ) return evaluate_atom(exp,environment);
	    //
	    // If it reached here then exp is a cons
	    //
//	    LOG(BF("Evaluating cons[%s]") % exp->__repr__() );
//	    printf("    Evaluating: %s\n", _rep_(exp).c_str() );
//	    printf("    In env: %s\n", _rep_(environment).c_str() );
	    Cons_sp form = exp.asOrNull<Cons_O>();
	    ASSERTNOTNULL(form);
	    T_sp head = oCar(form);
	    if ( Symbol_sp headSym = head.asOrNull<Symbol_O>() )
	    {
//		LOG(BF("Head[%s] is a Symbol") % headSym->__repr__() );
		_lisp->invocationHistoryStack().setExpressionForTop(form);
		_lisp->invocationHistoryStack().setActivationFrameForTop(Environment_O::nilCheck_getActivationFrame(environment));
		if ( _lisp->isSingleStepOn() )
		{
		    IMPLEMENT_ME();
#if 0
		    LispDebugger::step();
#endif
		}
		SpecialForm_sp specialForm = _lisp->specialFormOrNil(headSym);
		if ( !specialForm.nilp() ) return evaluate_specialForm( specialForm, form, environment );

		if ( headSym == cl::_sym_cond ) return evaluate_cond( form, environment );
		else if (headSym == cl::_sym_case) return evaluate_case( form, environment );
		else if (headSym == cl::_sym_multipleValueSetq ) return evaluate_multipleValueSetq( form, environment );
		else if (headSym == cl::_sym_prog1 ) return evaluate_prog1( form, environment );

		Function_sp headFunc = af_interpreter_lookup_macro(headSym,environment);
		if ( headFunc.notnilp() )
		{
		    /* Macro expansion should be done immediately after the reader - 
		       - done here the macros are expanded again and again and again
		    */
		    T_sp expanded = _Nil<T_O>();
		    try {
			expanded = af_macroexpand(form,environment);
			if ( _evaluateVerbosity>0 )
			{
			    string es = _rep_(expanded);
			    printf("core::eval::evaluate expression is macro - expanded --> %s\n", es.c_str());
			}
		    }
		    catch (Condition& cond)
		    {
			THROW_HARD_ERROR(BF("Figure out what to do from here"));
//			_lisp->error(cond.conditionObject()/*,environment*/);
		    };// catch (...) {throw;};
//		    LOG(BF("Expanded macro to: %s") % expanded->__repr__() );
//		    LOG(BF("Evaluating macro in environment: %s") % environment->__repr__() );
		    try { result = eval::evaluate(expanded,environment);} // ->object(); }
		    catch (...) {result = handleConditionInEvaluate(environment);}
		    if ( !result ) goto NULL_RESULT;
		    return(result);
		}
		Environment_sp localEnv(environment);
		headFunc = af_interpreter_lookup_function(headSym,environment);
		if ( !headFunc.pointerp() )
		{
		    SIMPLE_ERROR(BF("Could not find form(%s) in the lexical/dynamic environment")
				       % _rep_(headSym) );
		}

		//
		// It is a form and its head is a symbol,
		// evaluate the arguments and apply the function bound to the head to them
		//
//		LOG(BF("Symbol[%s] is a normal form - evaluating arguments") % head->__repr__() );
		if ( af_functionP(headFunc) )
		{
		    ValueFrame_sp evaluatedArgs(ValueFrame_O::create(af_length(cCdr(form)),
								     _Nil<ActivationFrame_O>()));
		    evaluateIntoActivationFrame(evaluatedArgs,cCdr(form),environment);
		    try { result = headFunc->INVOKE(evaluatedArgs->length(),evaluatedArgs->argArray()); } // result = eval::applyFunctionToActivationFrame(headFunc,evaluatedArgs);}
		    catch (...) {result = handleConditionInEvaluate(environment);};
		    if ( !result ) goto NULL_RESULT;
		    return(result);
		}else
		{
		    SIMPLE_ERROR(BF("Could not find form(%s) in the lexical/dynamic environment")
				       % _rep_(headSym) );
		}
	    }
	    {
		Cons_sp headCons = head.asOrNull<Cons_O>();
		ASSERTF(headCons,BF("Illegal head %s - must be a LAMBDA expression") % _rep_(head) );
		return evaluate_lambdaHead( headCons, form, environment );
	    }
	NULL_RESULT:
	    SIMPLE_ERROR(BF("result was NULL"));
	}



	void evaluateIntoActivationFrame(ActivationFrame_sp af,
					 Cons_sp args, Environment_sp environment )
	{_G();
	    if ( args.nilp() )
	    {
		LOG(BF("Arguments before evaluateList: Nil ---> returning Nil"));
		return;
	    }
	    LOG(BF("Arguments before evaluateList: %s")%_rep_(args) );
	    {_BLOCK_TRACE("Evaluating...");
		int idx = 0;
		// Iterate through each car in exp and
		// evaluate it (handling Nil objects and results)
		// and string the results into a linked list
		for ( Cons_sp p=args; p.notnilp(); p=cCdr(p) )
		{
		    T_sp inObj = oCar(p);
		    T_sp result = eval::evaluate(inObj,environment);
		    ASSERTNOTNULL(result);
		    LOG(BF("After evaluation result = %s")% _rep_(result) );
		    af->set_entry(idx,result);
		    ++idx;
		}
	    }
	    LOG(BF("Arguments after evaluateList: %s") % _rep_(af) );
	}




	Cons_sp evaluateList(Cons_sp args, Environment_sp environment)
	{_G();
	    Cons_sp firstCons = Cons_O::create(_Nil<T_O>());
	    Cons_sp curCons = firstCons;
	    if ( args.nilp() )
	    {
		LOG(BF("Arguments before evaluateList: Nil ---> returning Nil"));
		return _Nil<Cons_O>();
	    }
	    LOG(BF("Arguments before evaluateList: %s")%args->__repr__() );
	    {_BLOCK_TRACE("Evaluating...");
		// Iterate through each car in exp and
		// evaluate it (handling Nil objects and results)
		// and string the results into a linked list
		for ( Cons_sp p=args; p.notnilp(); p=cCdr(p) )
		{
		    T_sp inObj = oCar(p);
		    T_sp result = eval::evaluate(inObj,environment);
		    ASSERTNOTNULL(result);
		    LOG(BF("After evaluation result = %s @ %X")% result->__repr__() % (void*)(result.get()) );
		    Cons_sp outCons = Cons_O::create(result);
		    curCons->setCdr(outCons);
		    curCons = outCons;
		}
	    }
#ifdef DEBUG_ON
	    Cons_sp tempCons = firstCons->cdr();
	    while ( tempCons.notnilp() )
	    {
		T_sp zobj = oCar(tempCons);
		LOG(BF("Argument after evaluateList in order: %s @ %X")% zobj->__repr__() % (void*)(zobj.get()) );
		tempCons = tempCons->cdr();
	    }
#endif
	    LOG(BF("Arguments after evaluateList: %s")%_rep_(oCdr(firstCons)));
	    return cCdr(firstCons);
	}

	T_mv evaluateListReturnLast(Cons_sp args, Environment_sp environment)
	{_G();
	    T_sp inObj;
	    T_mv outObj;
	    outObj = Values(_Nil<T_O>());
	    {_BLOCK_TRACE("Evaluating...");
		// Iterate through each car in exp and
		// evaluate it (handling Nil objects and results)
		// and string the results into a linked list
		//
		//
		for ( Cons_sp p=args; p.notnilp(); p=cCdr(p) )
		{
		    inObj = oCar(p);
		    LOG(BF("Pushing code onto the backTrace: <%s>")%p->__repr__() );
		    { 
			TRY()
			{
			    outObj = eval::evaluate(inObj,environment);       // used to use newEnvironment
			}
			catch (Condition& err)
			{
			    THROW_HARD_ERROR(BF("Figure out what to do here"));
#if 0
			    TRY()
			    {
				_lisp->error(err.conditionObject() /*,environment */);
			    }
			    catch (DebuggerSaysContinue& dc)
			    {
				outObj = dc.returnObject();
			    }
#endif
			}
		    }
		}
	    }
	    return outObj;
	}



	void defineSpecialOperatorsAndMacros(Package_sp pkg)
	{_G();
	    SYMBOL_EXPORT_SC_(ClPkg,block);
	    SYMBOL_EXPORT_SC_(ClPkg,quote);
	    SYMBOL_EXPORT_SC_(ClPkg,progn);
	    SYMBOL_EXPORT_SC_(ClPkg,throw);
	    _lisp->defineSpecialOperator(ExtPkg,"special-var", &sp_specialVar);
	    _lisp->defineSpecialOperator(ExtPkg,"lexical-var", &sp_lexicalVar);
	    _lisp->defineSpecialOperator(ClPkg,"block", &sp_block);
	    _lisp->defineSpecialOperator(ClPkg,"catch",&sp_catch);
	    _lisp->defineSpecialOperator(ClPkg,"eval-when",&sp_eval_when);
//	    _lisp->defineSpecialOperator(ExtPkg,"dbg-i32",&sp_dbg_i32);
	    _lisp->defineSpecialOperator(ClPkg,"flet", &sp_flet);
	    _lisp->defineSpecialOperator(ClPkg,"function",&sp_function);
	    _lisp->defineSpecialOperator(ClPkg,"the",&sp_the);
	    // SBCL defined truly-the as a special operator
	    _lisp->defineSpecialOperator(ExtPkg,"truly-the",&sp_the);
	    _lisp->defineSpecialOperator(ClPkg,"go",&sp_go);
	    _lisp->defineSpecialOperator(ClPkg,"if",&sp_if);
	    _lisp->defineSpecialOperator(ClPkg,"labels", &sp_labels);
	    _lisp->defineSpecialOperator(ClPkg,"let", &sp_let);
	    _lisp->defineSpecialOperator(ClPkg,"let*", &sp_letSTAR);
	    _lisp->defineSpecialOperator(ClPkg,"locally",&sp_locally);
	    _lisp->defineSpecialOperator(ClPkg,"macrolet", &sp_macrolet);
	    _lisp->defineSpecialOperator(ClPkg,"multipleValueProg1",&sp_multipleValueProg1);
	    _lisp->defineSpecialOperator(ClPkg,"multipleValueCall",&sp_multipleValueCall);
	    _lisp->defineSpecialOperator(ClPkg,"progn",&sp_progn);
	    _lisp->defineSpecialOperator(ClPkg,"progv",&sp_progv);
	    _lisp->defineSpecialOperator(ClPkg,"quote",&sp_quote);
	    _lisp->defineSpecialOperator(ClPkg,"return-from",&sp_returnFrom);
	    _lisp->defineSpecialOperator(ClPkg,"setq",&sp_setq);
	    _lisp->defineSpecialOperator(ClPkg,"tagbody",&sp_tagbody);
	    _lisp->defineSpecialOperator(ClPkg,"throw",&sp_throw);
	    _lisp->defineSpecialOperator(ClPkg,"unwind-protect",&sp_unwindProtect);
	    _lisp->defineSpecialOperator(ClPkg,"symbol-macrolet",&sp_symbolMacrolet);
	    _lisp->defineSpecialOperator(ClPkg,"load-time-value",&sp_loadTimeValue);
// missing special operator load-time-value
// missing progv



// These need to be converted to macros
//	    _lisp->defineSpecialOperator(ExtPkg,"step",&sp_step);

	    SYMBOL_SC_(CorePkg,processDeclarations);
	    Defun(processDeclarations);
	    SYMBOL_EXPORT_SC_(ClPkg,eval);	
	    Defun(eval);
	    SYMBOL_SC_(CorePkg,extractDeclaresDocstringCode);
	    Defun(extractDeclaresDocstringCode);
	    SYMBOL_SC_(CorePkg,evaluateVerbosity);
	    Defun(evaluateVerbosity);
	    SYMBOL_EXPORT_SC_(CorePkg,evalWithEnv);
	    Defun(evalWithEnv);
	    SYMBOL_EXPORT_SC_(CorePkg,topLevelEvalWithEnv);
	    Defun(topLevelEvalWithEnv);
	    SYMBOL_SC_(CorePkg,evaluateDepth);
	    Defun(evaluateDepth);	
	    SYMBOL_SC_(CorePkg,classifyLetVariablesAndDeclares);
	    Defun(classifyLetVariablesAndDeclares);
	};

    };

};


