namespace closette
{

#define ARGS_fn_canconicalize_defclass_options "(options)"
    DECLARE_SID(options)
    T_sp fn_canonicalize_defclass_options(Executable_sp exec, Cons_sp args, Environment_sp env, Lisp_sp lisp)
    {_G();
	eval::apply(SID_fn_mapappend,
		    ( ql::list(_lisp) ,
		      _lisp->function(_lisp->symbol(SID_fn_canonicalize_defclass_option) ,
				      options ).cons() ),
		    _lisp);
    };



#define ARGS_fn_canonicalize_defclass_option "(option)"
    DECLARE_SID(option)
    T_sp fn_canonicalize_defclass_option(Executable_sp exec, Cons_sp args, Environment_sp env, Lisp_sp lisp)
    {_G();
	eval::apply(_lisp->symbol(_sym_case),
		    (ql::list(_lisp) ,
		     env->lookup(_sym_option)->as<Cons_O>()->ocar(),	// case of what
		     (ql::list(_lisp) ,
		      _lisp->symbol(_kw_metaclass) ,
		      
		      
		     
    };

};
