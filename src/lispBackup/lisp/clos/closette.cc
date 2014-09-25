/*
    File: closette.cc
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
