/*
    File: bformat.cc
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
#define	DEBUG_LEVEL_FULL

#include "core/common.h"
#include "core/environment.h"
#include "core/str.h"
#include "core/symbolTable.h"
#include "core/designators.h"
#include "core/evaluator.h"
#include "core/lispStream.h"
#include "bformat.h"
#include "bignum.h"
#include "core/wrappers.h"
namespace core
{







/*! Boost-format interface - works like CL:format but uses boost format strings
 */
#define ARGS_af_bformat "(destination control &rest args)"
#define DECL_af_bformat ""
#define DOCS_af_bformat "Like CL format but uses C/boost format strings"    
    T_sp af_bformat(T_sp destination, const string& control, Cons_sp args )
    {_G();
	T_sp output;
        if ( destination.nilp() ) {
            output = clasp_make_string_output_stream();
        } else {
            output = coerce::outputStreamDesignator(destination);
        }
	boost::format fmter(control);
	string fmter_str;
	TRY()
	{
	    for( Cons_sp farg=args; farg.notnilp(); farg = cCdr(farg) )
	    {
		T_sp fobj = oCar(farg);
		if ( !fobj )
		{
		    fmter % "!!!!UNDEFINED-BFORMAT-ARGUMENT!!!!!";
		} else if ( af_fixnumP(fobj) )
		{
		    Fixnum_sp fint = safe_downcast<Fixnum_O>(fobj);
		    fmter % fint->get();
		} else if ( af_bignumP(fobj) )
		{
		    Bignum_sp flli = safe_downcast<Bignum_O>(fobj);
		    stringstream ss;
		    ss << flli->as_mpz();
		    fmter % ss.str();
		} else if ( af_strP(fobj) )
		{
		    Str_sp ftext = safe_downcast<Str_O>(fobj);
		    fmter % ftext->get();
		} else if ( af_doubleFloatP(fobj) )
		{
		    DoubleFloat_sp freal= safe_downcast<DoubleFloat_O>(fobj);
		    fmter % freal->get();
		} else
		{
		    fmter % _rep_(fobj);
		}
	    }
	    fmter_str = fmter.str();
	} catch ( boost::io::bad_format_string& err )
	  {
	      SIMPLE_ERROR(BF("bformat command error: bad format string" ));
	  } catch ( boost::io::too_few_args& err )
	    {
		SIMPLE_ERROR(BF("bformat command error: too few args" ));
	    } catch ( boost::io::too_many_args& err )
	      {
		  SIMPLE_ERROR(BF("bformat command error: too many args" ));
	      } catch ( boost::io::out_of_range& err )
		{
		    SIMPLE_ERROR(BF("bformat command error: out of range" ));
		} catch (...)
		  {
		      SIMPLE_ERROR(BF("Unknown bformat command error"));
		  }
        clasp_write_string(fmter_str,output);
        if ( destination.nilp() ) {
            return cl_get_output_stream_string(output);
        }
        return _Nil<T_O>();
    }







    
    
#define ARGS_af_format "(destination control &rest args)"
#define DECL_af_format ""
#define DOCS_af_format "Subset of CL format - this does the job until the real format is installed"
    T_sp af_format(T_sp destination, T_sp control, Cons_sp args)
    {_G();
	stringstream tf;
	if ( af_functionP(control) )
	{
	    SIMPLE_ERROR(BF("Add support for functions as FORMAT controls"));
	}
	if ( !af_stringP(control) )
	{
	    SIMPLE_ERROR(BF("FORMAT control must be a string or a function - you gave: %s") % _rep_(control) );
	}
	string ts = control.as<Str_O>()->get();
	const char* cur = ts.c_str();
	while ( *cur )
	{
	    if ( *cur == '~' )
	    {
		++cur;
		switch (*cur)
		{
		case 'c':
		    tf << "%c";
		    break;
		case 's':
		case 'S':
		    tf << "%s";
		    break;
		case 'd':
		    tf << "%d";
		    break;
		case 'a':
		case 'A':
		    tf << "%s";
		    break;
		case '%':
		    tf << std::endl;
		    break;
		default:
		    SIMPLE_ERROR(BF("Add support for control[%s]") % cur);
		}
		++cur;
	    } else if ( *cur == '%' ) {
                ++cur;
                tf << "%%";
            } else
	    {
		tf << *cur;
		++cur;
	    }
	}
	return af_bformat(destination,tf.str(),args);
    };



    void initialize_bformat(Lisp_sp lisp)
    {_G();
	SYMBOL_SC_(CorePkg,bformat);
	Defun(bformat);
	SYMBOL_EXPORT_SC_(ClPkg,format);
	Defun(format);
    }





}; /* (>>>namespace<<<) */
