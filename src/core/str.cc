/*
    File: str.cc
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

//#include "clasp_gmpxx.h"
#include <ctype.h>
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/designators.h>
#include <clasp/core/character.h>
#include <clasp/core/bignum.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/str.h>
#include <clasp/core/print.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/wrappers.h>

namespace core
{

    string str_get(Str_sp str) { return str.as<Str_O>()->get(); };
    string str_get(T_sp str) {  if (str.nilp()) {SIMPLE_ERROR(BF("Could not convert nil to Str"));}; return str.as<Str_O>()->get(); };
    T_sp str_create(const string& str) { return Str_O::create(str);};
    T_sp str_create(const char* str) { return Str_O::create(str);};


    typedef enum { iinit, iwhite, inum, itrailspace, ijunk,idone } IntegerFSMState;

    /*! Digits are 0-9 or a-z/A-Z.
      If digit >= radix then return -1.
     */
    int fsmIntegerDigit(char c,int radix)
    {
	int idigit = -1;
	if ( isdigit(c) )
	{
	    idigit = c-'0';
	} else if ( isalpha(c) )
	{
	    idigit = -1;
	    if ( c>='A' && c<='Z') idigit = c-'A'+10;
	    else if ( c>='a' && c<= 'z' ) idigit = c-'a'+10;
	}
	if ( idigit < 0 ) return idigit;
	if ( idigit >= radix ) return -1;
	return idigit;
    }

    int fsmInteger(mpz_class& result, int& numDigits, bool& sawJunk, const string& str, int istart, int iend, bool junkAllowed, int radix)
    {
	IntegerFSMState state = iinit;
	int sign = 1;
	result = 0;
	numDigits = 0;
	int cur = istart;
	while (1)
	{
	    char c = str[cur];
	    switch (state)
	    {
	    case iinit:
	    case iwhite:
	    {
		if ( isspace(c) )
		{
		    state = iwhite;
		    break;
		} else if (c=='-')
		{
		    state = inum;
		    sign = -1;
		    break;
		} else if (c=='+')
		{
		    state = inum;
		    break;
		} else if (isalnum(c))
		{
		    int idigit = fsmIntegerDigit(c,radix);
		    if ( idigit < 0 || idigit >= radix )
		    {
			state = ijunk;
			break;
		    }
		    result = result * radix + idigit;
		    ++numDigits;
		    state = inum;
		    break;
		}
		state = ijunk;
		break;
	    }
	    case inum:
	    {
		if ( isspace(c) )
		{
		    if ( junkAllowed )
		    {
			state = itrailspace;
			break;
		    }
		    state = idone;
		    break;
		} else if (isalnum(c))
		{
		    int idigit = fsmIntegerDigit(c,radix);
		    if ( idigit < 0 || idigit >= radix )
		    {
			state = ijunk;
			break;
		    }
		    result = result * radix + idigit;
		    ++numDigits;
		    state = inum;
		    break;
		}
		state = ijunk;
		break;
	    }
	    case itrailspace:
	    {
		if (junkAllowed)
		{
		    break;
		}
	    }
	    case ijunk:
		break;
	    case idone:
		break;
	    }
	    if ( state == idone ) break;
	    if ( state == ijunk ) break;
	    ++cur;
	    if ( cur >= iend ) break;
	}
	sawJunk = (state == ijunk);
	if ( sign < 0 )
	{
	    mpz_class nresult;
	    mpz_neg(nresult.get_mpz_t(),result.get_mpz_t());
	    mpz_swap(nresult.get_mpz_t(),result.get_mpz_t());
	}
	return cur;
    };


#define ARGS_cl_schar "(str index)"
#define DECL_cl_schar ""
#define DOCS_cl_schar "CLHS schar"
    claspChar cl_schar(Str_sp str, int idx)
    {
	if ( idx >= 0  && idx < str->length() ) {
	    return str->schar(idx);
	}
	SIMPLE_ERROR(BF("index %d out of range (0,%d)") % idx % str->length());
    };

#define ARGS_core_charSet "(str index c)"
#define DECL_core_charSet ""
#define DOCS_core_charSet "CLHS schar"
    claspChar core_charSet(Str_sp str, int idx, claspChar c)
    {
	if ( idx >= 0  && idx < str->length() ) {
	    str->scharSet(idx,c);
	    return c;
	}
	SIMPLE_ERROR(BF("index %d out of range (0,%d)") % idx % str->length());
    };

#define ARGS_core_scharSet "(str index c)"
#define DECL_core_scharSet ""
#define DOCS_core_scharSet "CLHS schar"
    claspChar core_scharSet(Str_sp str, int idx, claspChar c)
    {
	if ( idx >= 0  && idx < str->length() ) {
	    str->scharSet(idx,c);
	    return c;
	}
	SIMPLE_ERROR(BF("index %d out of range (0,%d)") % idx % str->length());
    };





#define ARGS_af_searchString "(str1 start1 end1 str2 start2 end2)"
#define DECL_af_searchString ""
#define DOCS_af_searchString "searchString"
    T_sp af_searchString(Str_sp str1, Fixnum_sp start1, T_sp end1, Str_sp str2, Fixnum_sp start2, T_sp end2)
    {_G();
        string s1 = str1->get().substr(start1->get(),end1.nilp()?str1->get().size() : end1.as<Fixnum_O>()->get());
        string s2 = str2->get().substr(start2->get(),end2.nilp()?str2->get().size() : end2.as<Fixnum_O>()->get());
//        printf("%s:%d Searching for \"%s\" in \"%s\"\n", __FILE__, __LINE__, s1.c_str(), s2.c_str());
        size_t pos = s2.find(s1);
        if ( pos == string::npos ) {
            return _Nil<T_O>();
        }
        return Fixnum_O::create(static_cast<int>(pos+start2->get()));
    };




#define ARGS_af_parseInteger "(string &key (start 0) end (radix 10) junk-allowed)"
#define DECL_af_parseInteger ""
#define DOCS_af_parseInteger "parseInteger"
    T_mv af_parseInteger(Str_sp str, uint start, T_sp end, uint radix, T_sp junkAllowed)
    {_G();
	int istart = MAX(0,start);
	int iend = cl_length(str);
	if ( end.notnilp() )
	{
	    iend = MIN(iend,end.as<Fixnum_O>()->get());
	}
	mpz_class result;
	bool sawJunk = false;
	int numDigits = 0;
	int cur = fsmInteger(result,numDigits,sawJunk,str->get(),istart,iend,junkAllowed.isTrue(),radix);
	if ( junkAllowed.notnilp() || (cur >= iend) || !sawJunk )
	{
	    // normal exit
	    if ( numDigits > 0 )
	    {
		return(Values(Integer_O::create(result),Fixnum_O::create(cur)));
	    } else
	    {
		return(Values(_Nil<T_O>(),Fixnum_O::create(cur)));
	    }
	}
	PARSE_ERROR(Str_O::create("Could not parse integer from ~S"), Cons_O::create(str));
	UNREACHABLE();
    };


    Str_sp Str_O::create(const boost::format& nm)
    {
	GC_ALLOCATE(Str_O,v);
	v->set(nm.str());
	return v;
    };



    Str_sp Str_O::create(const string& nm)
    {
        GC_ALLOCATE(Str_O,v );
//	printf("%s:%d Str_O::create(const string& nm) @ %p nm = %s\n", __FILE__, __LINE__, v.raw_(), nm.c_str() );
	v->set(nm);
	return v;
    };


    Str_sp Str_O::create(const char* nm)
    {
        GC_ALLOCATE(Str_O,v );
//	printf("%s:%d Str_O::create(const char* nm) @ %p nm = %s\n", __FILE__, __LINE__, v.raw_(), nm );
	v->setFromChars(nm);
	return v;
    };

    Str_sp Str_O::create(const char* nm, int numChars)
    {
        GC_ALLOCATE(Str_O,v );
//	printf("%s:%d Str_O::create(const char* nm) @ %p nm = %s\n", __FILE__, __LINE__, v.raw_(), nm );
	v->setFromChars(nm,numChars);
	return v;
    };



    Bignum Str_O::stringToBignum(const char* str)
    {_G();
	Bignum bn = 0;
	for ( const unsigned char* cp = (const unsigned char*)str; *cp; ++cp )
	{
	    bn = (bn<<7) | ((*cp) & 0x7f);
	}
#if 0
	stringstream ss;
	ss << bn;
	LOG(BF("stringToBignum string[%s] bignum[%s]") % str % ss.str() );
#endif
	return bn;
    }




    T_sp Str_O::elementType() const {
	return cl::_sym_BaseChar_O;
    }



#define ARGS_af_make_string "(size &key initial-element (element-type 'character))"
#define DECL_af_make_string ""
#define DOCS_af_make_string "See CLHS: make_string"
    T_mv af_make_string(Fixnum_sp size, T_sp initial_element, T_sp element_type )
    {_G();
	stringstream ss;
	char ch(' ');
	if ( initial_element.notnilp() ) ch = initial_element.as<Character_O>()->asChar();
	int isize = size->get();
	for ( int i=0; i<isize; i++ )
	{
	    ss << ch;
	}
	Str_sp ns = Str_O::create(ss.str());
	return(Values(ns));
    };



#define ARGS_af_base_string_concatenate "(&rest args)"
#define DECL_af_base_string_concatenate ""
#define DOCS_af_base_string_concatenate "base_string_concatenate"
    T_sp af_base_string_concatenate(List_sp args)
    {_G();
	stringstream ss;
	for ( auto cur : args ) {
	    Str_sp str = coerce::stringDesignator(oCar(cur));
	    ss << str->get();
	}
	return Str_O::create(ss.str());
    };





    inline void setup_string_op_arguments(T_sp string1_desig, T_sp string2_desig,
					  Str_sp& string1, Str_sp& string2,
					  Fixnum_sp start1, T_sp end1,
					  Fixnum_sp start2, T_sp end2,
					  int& istart1, int& iend1, int& istart2, int& iend2 )
    {_G();
	string1 = coerce::stringDesignator(string1_desig);
	string2 = coerce::stringDesignator(string2_desig);
	istart1 = MAX(start1->get(),0);
	iend1 = MIN(end1.nilp() ? cl_length(string1) : end1.as<Fixnum_O>()->get(),cl_length(string1));
	istart2 = MAX(start2->get(),0);
	iend2 = MIN(end2.nilp() ? cl_length(string2) : end2.as<Fixnum_O>()->get(),cl_length(string2));
    }


#define DOCS_af_string_EQ_ "string_EQ_"
#define LOCK_af_string_EQ_ 1
#define ARGS_af_string_EQ_ "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_af_string_EQ_ ""
    T_mv af_string_EQ_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2 )
    {_G();
	int istart1, iend1, istart2, iend2;
	Str_sp string1;
	Str_sp string2;
	setup_string_op_arguments(strdes1,strdes2,string1,string2,start1,end1,start2,end2,istart1,iend1,istart2,iend2);
	return(Values(string1->string_EQ_(string2,istart1,iend1,istart2,iend2)));
    }

#define DOCS_af_string_NE_ "string_NE_"
#define LOCK_af_string_NE_ 1
#define ARGS_af_string_NE_ "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_af_string_NE_ ""
    T_mv af_string_NE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2 )
    {_G();
	int istart1, iend1, istart2, iend2;
	Str_sp string1;
	Str_sp string2;
	setup_string_op_arguments(strdes1,strdes2,string1,string2,start1,end1,start2,end2,istart1,iend1,istart2,iend2);
	return(Values(string1->string_NE_(string2,istart1,iend1,istart2,iend2)));
    }

#define DOCS_af_string_LT_ "string_LT_"
#define LOCK_af_string_LT_ 1
#define ARGS_af_string_LT_ "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_af_string_LT_ ""
    T_mv af_string_LT_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2 )
    {_G();
	int istart1, iend1, istart2, iend2;
	Str_sp string1;
	Str_sp string2;
	setup_string_op_arguments(strdes1,strdes2,string1,string2,start1,end1,start2,end2,istart1,iend1,istart2,iend2);
	return(Values(string1->string_LT_(string2,istart1,iend1,istart2,iend2)));
    }

#define DOCS_af_string_GT_ "string_GT_"
#define LOCK_af_string_GT_ 1
#define ARGS_af_string_GT_ "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_af_string_GT_ ""
    T_mv af_string_GT_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2 )
    {_G();
	int istart1, iend1, istart2, iend2;
	Str_sp string1;
	Str_sp string2;
	setup_string_op_arguments(strdes1,strdes2,string1,string2,start1,end1,start2,end2,istart1,iend1,istart2,iend2);
	return(Values(string1->string_GT_(string2,istart1,iend1,istart2,iend2)));
    }

#define DOCS_af_string_LE_ "string_LE_"
#define LOCK_af_string_LE_ 1
#define ARGS_af_string_LE_ "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_af_string_LE_ ""
    T_mv af_string_LE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2 )
    {_G();
	int istart1, iend1, istart2, iend2;
	Str_sp string1;
	Str_sp string2;
	setup_string_op_arguments(strdes1,strdes2,string1,string2,start1,end1,start2,end2,istart1,iend1,istart2,iend2);
	return(Values(string1->string_LE_(string2,istart1,iend1,istart2,iend2)));
    }

#define DOCS_af_string_GE_ "string_GE_"
#define LOCK_af_string_GE_ 1
#define ARGS_af_string_GE_ "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_af_string_GE_ ""
    T_mv af_string_GE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2 )
    {_G();
	int istart1, iend1, istart2, iend2;
	Str_sp string1;
	Str_sp string2;
	setup_string_op_arguments(strdes1,strdes2,string1,string2,start1,end1,start2,end2,istart1,iend1,istart2,iend2);
	return(Values(string1->string_GE_(string2,istart1,iend1,istart2,iend2)));
    }




#define DOCS_af_string_equal "string_equal"
#define LOCK_af_string_equal 1
#define ARGS_af_string_equal "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_af_string_equal ""
    T_sp af_string_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2 )
    {_G();
	int istart1, iend1, istart2, iend2;
	Str_sp string1;
	Str_sp string2;
	setup_string_op_arguments(strdes1,strdes2,string1,string2,start1,end1,start2,end2,istart1,iend1,istart2,iend2);
	return(string1->string_equal(string2,istart1,iend1,istart2,iend2));
    }

#define DOCS_af_string_not_equal "string_not_equal"
#define LOCK_af_string_not_equal 1
#define ARGS_af_string_not_equal "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_af_string_not_equal ""
    T_mv af_string_not_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2 )
    {_G();
	int istart1, iend1, istart2, iend2;
	Str_sp string1;
	Str_sp string2;
	setup_string_op_arguments(strdes1,strdes2,string1,string2,start1,end1,start2,end2,istart1,iend1,istart2,iend2);
	return(Values(string1->string_not_equal(string2,istart1,iend1,istart2,iend2)));
    }
#define DOCS_af_string_lessp "string_lessp"
#define LOCK_af_string_lessp 1
#define ARGS_af_string_lessp "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_af_string_lessp ""
    T_mv af_string_lessp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2 )
    {_G();
	int istart1, iend1, istart2, iend2;
	Str_sp string1;
	Str_sp string2;
	setup_string_op_arguments(strdes1,strdes2,string1,string2,start1,end1,start2,end2,istart1,iend1,istart2,iend2);
	return(Values(string1->string_lessp(string2,istart1,iend1,istart2,iend2)));
    }
#define DOCS_af_string_greaterp "string_greaterp"
#define LOCK_af_string_greaterp 1
#define ARGS_af_string_greaterp "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_af_string_greaterp ""
    T_mv af_string_greaterp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2 )
    {_G();
	int istart1, iend1, istart2, iend2;
	Str_sp string1;
	Str_sp string2;
	setup_string_op_arguments(strdes1,strdes2,string1,string2,start1,end1,start2,end2,istart1,iend1,istart2,iend2);
	return(Values(string1->string_greaterp(string2,istart1,iend1,istart2,iend2)));
    }
#define DOCS_af_string_not_greaterp "string_not_greaterp"
#define LOCK_af_string_not_greaterp 1
#define ARGS_af_string_not_greaterp "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_af_string_not_greaterp ""
    T_mv af_string_not_greaterp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2 )
    {_G();
	int istart1, iend1, istart2, iend2;
	Str_sp string1;
	Str_sp string2;
	setup_string_op_arguments(strdes1,strdes2,string1,string2,start1,end1,start2,end2,istart1,iend1,istart2,iend2);
	return(Values(string1->string_not_greaterp(string2,istart1,iend1,istart2,iend2)));
    }
#define DOCS_af_string_not_lessp "string_not_lessp"
#define LOCK_af_string_not_lessp 1
#define ARGS_af_string_not_lessp "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_af_string_not_lessp ""
    T_mv af_string_not_lessp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, T_sp end1, Fixnum_sp start2, T_sp end2 )
    {_G();
	int istart1, iend1, istart2, iend2;
	Str_sp string1;
	Str_sp string2;
	setup_string_op_arguments(strdes1,strdes2,string1,string2,start1,end1,start2,end2,istart1,iend1,istart2,iend2);
	return(Values(string1->string_not_lessp(string2,istart1,iend1,istart2,iend2)));
    }



    EXPOSE_CLASS(core,Str_O);

	void Str_O::exposeCando(Lisp_sp lisp)
	{_G();
	    class_<Str_O>()
//	.def("valueAsStr", &Str_O::valueAsString )
//	.def("setFromStr", &Str_O::setFromString )
		.def("core:asInt", &Str_O::asInt)
		.def("core:parse-real",&Str_O::asReal)
		.def("core:asReal", &Str_O::asReal)
		.def("core:asSymbol", &Str_O::asSymbol)
		.def("core:asKeywordSymbol", &Str_O::asKeywordSymbol)
//		.def("core:set", &Str_O::set)
		.def("core:left", &Str_O::left)
		.def("core:string-find", &Str_O::find)
		.def("core:right", &Str_O::right)
		.def("core:substr", &Str_O::substr)
//		.def("get", &Str_O::get)
		.def("core:size", &Str_O::size)
		.def("core:countOccurances", &Str_O::countOccurances)
		.def("core:split", &Str_O::split)
		.def("core:splitAtWhiteSpace", &Str_O::splitAtWhiteSpace)
		.def("core:schar",&Str_O::schar)
                .def("core:scharSet",&Str_O::scharSet)
//		.def_raw("format", &Str_O::prim_format)
//		.def_raw("%", &Str_O::prim_format)
//		.def_raw("%%", &Str_O::prim_formatCons)
		;

	    SYMBOL_SC_(CorePkg,base_string_concatenate);
	    Defun(searchString);
	    Defun(base_string_concatenate);
	    Defun(string_EQ_);
	    Defun(string_NE_);
	    Defun(string_LT_);
	    Defun(string_GT_);
	    Defun(string_LE_);
	    Defun(string_GE_);

	    Defun(string_equal);
	    Defun(string_not_equal);
	    Defun(string_lessp);
	    Defun(string_greaterp);
	    Defun(string_not_greaterp);
	    Defun(string_not_lessp);
	    ClDefun(schar);
	    CoreDefun(scharSet);
	    CoreDefun(charSet);
	    

	    SYMBOL_EXPORT_SC_(ClPkg,string_EQ_);
	    SYMBOL_EXPORT_SC_(ClPkg,string_NE_);
	    SYMBOL_EXPORT_SC_(ClPkg,string_LT_);
	    SYMBOL_EXPORT_SC_(ClPkg,string_GT_);
	    SYMBOL_EXPORT_SC_(ClPkg,string_LE_);
	    SYMBOL_EXPORT_SC_(ClPkg,string_GE_);

	    SYMBOL_EXPORT_SC_(ClPkg,string_equal);
	    SYMBOL_EXPORT_SC_(ClPkg,string_not_equal);
	    SYMBOL_EXPORT_SC_(ClPkg,string_lessp);
	    SYMBOL_EXPORT_SC_(ClPkg,string_greaterp);
	    SYMBOL_EXPORT_SC_(ClPkg,string_not_greaterp);
	    SYMBOL_EXPORT_SC_(ClPkg,string_not_lessp);

	    SYMBOL_EXPORT_SC_(ClPkg,parseInteger);
	    Defun(parseInteger);

	    SYMBOL_EXPORT_SC_(ClPkg,make_string);
	    Defun(make_string);

	}


    void Str_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Str,"","",_lisp)
		.def("valueAsStr", &Str_O::valueAsString )
		.def("setFromStr", &Str_O::setFromString )
//		.def("set", &Str_O::set)
//		.def("get", &Str_O::get)
		;
#endif
	}



    Str_sp Str_O::create(char initial_element, int dimension, T_sp seq )
    {_G();
        GC_ALLOCATE(Str_O,str );
	str->_Contents = string(dimension, initial_element);
	if ( seq.notnilp() ) str->fillInitialContents(seq);
	return str;
    }


#if defined(OLD_SERIALIZE)
    void Str_O::serialize(serialize::SNode node)
    {_G();
	if ( node->saving() )
	{
	    // Do nothing
	} else
	{
	    IMPLEMENT_ME();
	}
    }
#endif


#if defined(XML_ARCHIVE)
    void	Str_O::archiveBase(ArchiveP node)
    {
	this->Base::archiveBase(node);
	node->archiveString("contents",this->_Contents);
    }
#endif // defined(XML_ARCHIVE)

    void Str_O::sxhash(HashGenerator& hg) const
    {_OF();
	Bignum bn = Str_O::stringToBignum(this->get().c_str());
	hg.addPart(bn);
    }


    Fixnum_sp	Str_O::asInt() const
    {
	Fixnum_sp i;
	i = Fixnum_O::create(atoi(this->get().c_str()));
	return i;
    }


    Rational_sp Str_O::parseInteger()
    {
	return Integer_O::create(this->get());
    }




    DoubleFloat_sp	Str_O::asReal() const
    {
	DoubleFloat_sp n;
	n = DoubleFloat_O::create(atof(this->get().c_str()));
	return n;
    }



    T_sp Str_O::elt(int index) const
    {_OF();
	ASSERTF(index>=0 && index<this->size(),BF("Index %d out of range for elt of string size: %d") % index % this->size() );
	char c = this->_Contents[index];
	Character_sp ch = Character_O::create(c);
	return ch;
    }

    T_sp Str_O::setf_elt(int index, T_sp val)
    {_OF();
	ASSERTF(index>=0 && index<this->size(),BF("Index out of range for string: %d") % index );
	char ch = val.as<Character_O>()->asChar();
	this->_Contents[index] = ch;
	return val;
    }

    T_sp Str_O::asetUnsafe(int index, T_sp val)
    {_OF();
	char ch = val.as<Character_O>()->asChar();
	this->_Contents[index] = ch;
	return val;
    }




    T_sp Str_O::aref(List_sp indices) const
    {_OF();
	ASSERTF(cl_length(indices)==1,BF("Illegal index for string: %s") % _rep_(indices) );
	int index = oCar(indices).as<Fixnum_O>()->get();
	return this->elt(index);
    }

    T_sp Str_O::setf_aref(List_sp indices_val)
    {_OF();
	ASSERTF(cl_length(indices_val)==2,BF("Illegal index/val for setf_aref of string: %s") % _rep_(indices_val) );
	int index = oCar(indices_val).as<Fixnum_O>()->get();
	return this->setf_elt(index,oCadr(indices_val));
    }



    uint Str_O::countOccurances(const string& chars)
    {_OF();
	ASSERT_eq(chars.size(),1 );
	uint c=0;
	string::size_type found = this->_Contents.find_first_of(chars);
	while ( found < this->size() && found != string::npos )
	{
	    c++;
	    found = this->_Contents.find_first_of(chars,found+1);
	}
	return c;
    }


    string Str_O::left(int num) const
    {
	string res = this->get().substr(0,num);
	return res;
    }

    string Str_O::right(int num) const
    {
	string res = this->get().substr(this->size()-num,num);
	return res;
    }



    T_sp Str_O::find(const string& substring, int start)
    {
	size_t res = this->get().find(substring,start);
	if ( res != string::npos ) return Integer_O::create((uint)res);
	return _Nil<T_O>();
    }

    string Str_O::substr(int start, int num) const
    {
	string res = this->get().substr(start,num);
	return res;
    }


    Symbol_sp Str_O::asSymbol() const
    {
	Symbol_sp sym = _lisp->intern(this->get());
	return sym;
    }

    Symbol_sp Str_O::asKeywordSymbol() const
    {
	Symbol_sp sym = _lisp->internKeyword(this->get());
	return sym;
    }




    bool Str_O::operator<(T_sp obj) const
    {
	if ( af_strP(obj) )
	{
	    Str_sp t = obj.as<Str_O>();
	    return this->get() < t->get();
	}
	return this->Base::operator<(obj);
    }

    bool Str_O::operator<=(T_sp obj) const
    {
	if ( af_strP(obj) )
	{
	    Str_sp t = obj.as<Str_O>();
	    return this->get() <= t->get();
	}
	return this->Base::operator<=(obj);
    }

    List_sp	Str_O::splitAtWhiteSpace()
    {
	vector<string> parts = core::split(this->get()," \n\t");
	Cons_sp first = Cons_O::create(_Nil<T_O>(),_Nil<T_O>());
	Cons_sp cur = first;
	for ( vector<string>::iterator it=parts.begin(); it!=parts.end(); it++ )
	{
	    Cons_sp one = Cons_O::create(Str_O::create(*it),_Nil<T_O>());
	    cur->setCdr(one);
	    cur=one;
	}
	return oCdr(first);
    }

    List_sp	Str_O::split(const string& chars)
    {
	TESTING();
	vector<string> parts = core::split(this->get(),chars);
	List_sp result(_Nil<T_O>());
	for ( vector<string>::reverse_iterator it = parts.rend(); it != parts.rbegin(); ++it ) {
	    Str_sp sone = translate::to_object<const string&>::convert(*it);
	    result = Cons_O::create(sone,result);
	}
	return result;
#if 0
	return Cons_O::createFromRangeObjectify< vector<string>::iterator, string >(parts.begin(),parts.end());
#endif
    }

    bool	Str_O::eql(T_sp obj) const
    {
	if ( Str_sp t = obj.asOrNull<Str_O>() ) {
	    return this->get() == t->get();
	}
	return this->Base::eql(obj);
    }


    bool Str_O::equal(T_sp obj) const
    {
	if (af_strP(obj) )
	{
	    return this->eql(obj);
	}
	return false;
    }

    bool Str_O::equalp(T_sp obj) const
    {
        if ( obj.nilp() ) return false;
        else if (Str_sp s2 = obj.asOrNull<Str_O>() ) {
            if ( this->length() != s2->length() ) return false;
	    return this->string_equal(s2,0,this->length(),0,this->length()).isTrue();
	}
	return false;
    }


    bool Str_O::operator>(T_sp obj) const
    {
	IMPLEMENT_ME();
#if 0
	if ( obj->strP() )
	{
	    Str_sp t = obj.as<Str_O>();
	    return this->get() > t->get();
	}
	return this->Base::operator>(obj);
#endif
    }

    bool Str_O::operator>=(T_sp obj) const
    {
	IMPLEMENT_ME();
#if 0
	if ( Str_sp t = obj.asOrNull<Str_O>() )
	{
	    int largest = MAX(this->size(),t->size());
	    return strncmp(this->_Contents,t->_Contents,largest) >= 0;
	}
	return this->Base::operator>=(obj);
#endif
    }



    string Str_O::__repr__() const
    {
	stringstream ss;
	ss << '"';
	const char* cur=this->_Contents.c_str(); // this->_Contents is a std::string
	while (*cur)
	{
	    if ( *cur == '"')
	    {
		ss << '\\' << '"';
	    } else if ( *cur == '\n' )
	    {
		ss << '\\' << 'n';
	    } else
	    {
		ss << *cur;
	    }
	    ++cur;
	}
	ss << '"';
	return ss.str();
    }



    /*! bounding index designator range from 0 to the end of each string */
    T_sp Str_O::string_EQ_(Str_sp string2, int start1, int end1, int start2, int end2) const
    {_OF();
	const char* cp1 = this->_Contents.c_str()+start1;
	const char* cp2 = string2->_Contents.c_str()+start2;
	int num1 = end1-start1;
	int num2 = end2-start2;
	while (1)
	{
	    if ( num1 == 0 ) goto END_STRING1;
	    if ( num2 == 0 ) goto END_STRING2;
	    if ( (*cp1 != *cp2) ) goto RETURN_FALSE;
	    --num1;--num2;++cp1;++cp2;
	}
    END_STRING1:
	    if ( num2 == 0 ) goto RETURN_TRUE;
	    goto RETURN_FALSE;
    END_STRING2:
    RETURN_FALSE:
	    return _Nil<T_O>();
    RETURN_TRUE:
	    return _lisp->_true();
    }


    /*! bounding index designator range from 0 to the end of each string */
    T_sp Str_O::string_NE_(Str_sp string2, int start1, int end1, int start2, int end2) const
    {_OF();
	const char* cp1 = this->_Contents.c_str()+start1;
	const char* cp2 = string2->_Contents.c_str()+start2;
	int num1 = end1-start1;
	int num2 = end2-start2;
	while (1)
	{
	    if ( num1 == 0 ) goto END_STRING1;
	    if ( num2 == 0 ) goto END_STRING2;
	    if ( (*cp1 != *cp2) ) goto RETURN_TRUE;
	    --num1;--num2;++cp1;++cp2;
	}
    END_STRING1:
	    if ( num2 != 0 ) goto RETURN_TRUE;
	    goto RETURN_FALSE;
    END_STRING2: // Did not hit end of string 1 at this point
    RETURN_TRUE: // strings are not equal
	    return _lisp->_true();
    RETURN_FALSE:
	    return _Nil<T_O>();
    }


    /*! bounding index designator range from 0 to the end of each string */
    T_sp Str_O::string_LT_(Str_sp string2, int start1, int end1, int start2, int end2) const
    {_OF();
	const char* cp1 = this->_Contents.c_str()+start1;
	const char* cp2 = string2->_Contents.c_str()+start2;
	int num1 = end1-start1;
	int num2 = end2-start2;
	while (1)
	{
	    if ( num1 == 0 ) goto END_STRING1;
	    if ( num2 == 0 ) goto END_STRING2;
	    if ( (*cp1 != *cp2) )
	    {
		if ( *cp1 < *cp2 ) goto RETURN_TRUE;
		goto RETURN_FALSE;
	    }
	    --num1;--num2;++cp1;++cp2;
	}
    END_STRING1:
	    if ( num2 == 0 ) goto RETURN_FALSE;
    RETURN_TRUE:
	    return Fixnum_O::create((int)(cp1-this->_Contents.c_str()));
    END_STRING2:
    RETURN_FALSE:
	    return _Nil<T_O>();
    }



    /*! bounding index designator range from 0 to the end of each string */
    T_sp Str_O::string_GT_(Str_sp string2, int start1, int end1, int start2, int end2) const
    {_OF();
	const char* cp1 = this->_Contents.c_str()+start1;
	const char* cp2 = string2->_Contents.c_str()+start2;
	int num1 = end1-start1;
	int num2 = end2-start2;
	while (1)
	{
	    if ( num1 == 0 ) goto END_STRING1;
	    if ( num2 == 0 ) goto END_STRING2;
	    if ( (*cp1 != *cp2) )
	    {
		if ( *cp1 > *cp2 ) goto RETURN_TRUE;
		goto RETURN_FALSE;
	    }
	    --num1;--num2;++cp1;++cp2;
	}
    END_STRING1:
    RETURN_FALSE:
	return _Nil<T_O>();
    END_STRING2:
    RETURN_TRUE:
	return Fixnum_O::create((int)(cp1-this->_Contents.c_str()));
    }





    /*! bounding index designator range from 0 to the end of each string */
    T_sp Str_O::string_LE_(Str_sp string2, int start1, int end1, int start2, int end2) const
    {_OF();
	const char* cp1 = this->_Contents.c_str()+start1;
	const char* cp2 = string2->_Contents.c_str()+start2;
	int num1 = end1-start1;
	int num2 = end2-start2;
	while (1)
	{
	    if ( num1 == 0 ) goto END_STRING1;
	    if ( num2 == 0 ) goto END_STRING2;
	    if ( (*cp1 != *cp2) )
	    {
		if ( *cp1 < *cp2 ) goto RETURN_TRUE;
		goto RETURN_FALSE;
	    }
	    --num1;--num2;++cp1;++cp2;
	}
    END_STRING1:
	    if ( num2 == 0 ) goto RETURN_TRUE;
	    goto RETURN_FALSE;
    END_STRING2:
    RETURN_FALSE:
	    return _Nil<T_O>();
    RETURN_TRUE:
	    return Fixnum_O::create((int)(cp1-this->_Contents.c_str()));
    }



    /*! bounding index designator range from 0 to the end of each string */
    T_sp Str_O::string_GE_(Str_sp string2, int start1, int end1, int start2, int end2) const
    {_OF();
	const char* cp1 = this->_Contents.c_str()+start1;
	const char* cp2 = string2->_Contents.c_str()+start2;
	int num1 = end1-start1;
	int num2 = end2-start2;
	while (1)
	{
	    if ( num1 == 0 ) goto END_STRING1;
	    if ( num2 == 0 ) goto END_STRING2;
	    if ( (*cp1 != *cp2) )
	    {
		if ( *cp1 > *cp2 ) goto RETURN_TRUE;
		goto RETURN_FALSE;
	    }
	    --num1;--num2;++cp1;++cp2;
	}
    END_STRING1:
	if ( num2 == 0 ) goto RETURN_TRUE;
	goto RETURN_FALSE;
    END_STRING2:
    RETURN_FALSE:
	return _Nil<T_O>();
    RETURN_TRUE:
	return Fixnum_O::create((int)(cp1-this->_Contents.c_str()));
    }








    /*! bounding index designator range from 0 to the end of each string */
    T_sp Str_O::string_equal(Str_sp string2, int start1, int end1, int start2, int end2) const
    {_OF();
	const char* cp1 = this->_Contents.c_str()+start1;
	const char* cp2 = string2->_Contents.c_str()+start2;
	int num1 = end1-start1;
	int num2 = end2-start2;
	while (1)
	{
	    if ( num1 == 0 ) goto END_STRING1;
	    if ( num2 == 0 ) goto END_STRING2;
	    if ( (toupper(*cp1) != toupper(*cp2)) ) goto RETURN_FALSE;
	    --num1;--num2;++cp1;++cp2;
	}
    END_STRING1:
	    if ( num2 == 0 ) goto RETURN_TRUE;
	    goto RETURN_FALSE;
    END_STRING2:
    RETURN_FALSE:
	    return _Nil<T_O>();
    RETURN_TRUE:
	    return _lisp->_true();
    }



    /*! bounding index designator range from 0 to the end of each string */
    T_sp Str_O::string_not_equal(Str_sp string2, int start1, int end1, int start2, int end2) const
    {_OF();
	const char* cp1 = this->_Contents.c_str()+start1;
	const char* cp2 = string2->_Contents.c_str()+start2;
	int num1 = end1-start1;
	int num2 = end2-start2;
	while (1)
	{
	    if ( num1 == 0 ) goto END_STRING1;
	    if ( num2 == 0 ) goto END_STRING2;
	    if ( (toupper(*cp1)!= toupper(*cp2)) ) goto RETURN_TRUE;
	    --num1;--num2;++cp1;++cp2;
	}
    END_STRING1:
	    if ( num2 != 0 ) goto RETURN_TRUE;
	    goto RETURN_FALSE;
    END_STRING2: // Did not hit end of string 1 at this point
    RETURN_TRUE: // strings are not equal
	    return _lisp->_true();
    RETURN_FALSE:
	    return _Nil<T_O>();
    }


    /*! bounding index designator range from 0 to the end of each string */
    T_sp Str_O::string_lessp(Str_sp string2, int start1, int end1, int start2, int end2) const
    {_OF();
	const char* cp1 = this->_Contents.c_str()+start1;
	const char* cp2 = string2->_Contents.c_str()+start2;
	int num1 = end1-start1;
	int num2 = end2-start2;
	while (1)
	{
	    if ( num1 == 0 ) goto END_STRING1;
	    if ( num2 == 0 ) goto END_STRING2;
	    char ucp1 = toupper(*cp1);
	    char ucp2 = toupper(*cp2);
	    if ( ucp1 != ucp2 )
	    {
		if ( ucp1 < ucp2 ) goto RETURN_TRUE;
		goto RETURN_FALSE;
	    }
	    --num1;--num2;++cp1;++cp2;
	}
    END_STRING1:
	    if ( num2 == 0 ) goto RETURN_FALSE;
    RETURN_TRUE:
	    return Fixnum_O::create((int)(cp1-this->_Contents.c_str()));
    END_STRING2:
    RETURN_FALSE:
	    return _Nil<T_O>();
    }



    /*! bounding index designator range from 0 to the end of each string */
    T_sp Str_O::string_greaterp(Str_sp string2, int start1, int end1, int start2, int end2) const
    {_OF();
	const char* cp1 = this->_Contents.c_str()+start1;
	const char* cp2 = string2->_Contents.c_str()+start2;
	int num1 = end1-start1;
	int num2 = end2-start2;
	while (1)
	{
	    if ( num1 == 0 ) goto END_STRING1;
	    if ( num2 == 0 ) goto END_STRING2;
	    char ucp1 = toupper(*cp1);
	    char ucp2 = toupper(*cp2);
	    if ( (ucp1 != ucp2) )
	    {
		if ( ucp1 > ucp2 ) goto RETURN_TRUE;
		goto RETURN_FALSE;
	    }
	    --num1;--num2;++cp1;++cp2;
	}
    END_STRING1:
    RETURN_FALSE:
	return _Nil<T_O>();
    END_STRING2:
    RETURN_TRUE:
	return Fixnum_O::create((int)(cp1-this->_Contents.c_str()));
    }





    /*! bounding index designator range from 0 to the end of each string */
    T_sp Str_O::string_not_greaterp(Str_sp string2, int start1, int end1, int start2, int end2) const
    {_OF();
	const char* cp1 = this->_Contents.c_str()+start1;
	const char* cp2 = string2->_Contents.c_str()+start2;
	int num1 = end1-start1;
	int num2 = end2-start2;
	while (1)
	{
	    if ( num1 == 0 ) goto END_STRING1;
	    if ( num2 == 0 ) goto END_STRING2;
	    char ucp1 = toupper(*cp1);
	    char ucp2 = toupper(*cp2);
	    if ( (ucp1 != ucp2) )
	    {
		if ( ucp1 < ucp2 ) goto RETURN_TRUE;
		goto RETURN_FALSE;
	    }
	    --num1;--num2;++cp1;++cp2;
	}
    END_STRING1:
	    if ( num2 == 0 ) goto RETURN_TRUE;
	    goto RETURN_FALSE;
    END_STRING2:
    RETURN_FALSE:
	    return _Nil<T_O>();
    RETURN_TRUE:
	    return Fixnum_O::create((int)(cp1-this->_Contents.c_str()));
    }



    /*! bounding index designator range from 0 to the end of each string */
    T_sp Str_O::string_not_lessp(Str_sp string2, int start1, int end1, int start2, int end2) const
    {_OF();
	const char* cp1 = this->_Contents.c_str()+start1;
	const char* cp2 = string2->_Contents.c_str()+start2;
	int num1 = end1-start1;
	int num2 = end2-start2;
	while (1)
	{
	    if ( num1 == 0 ) goto END_STRING1;
	    if ( num2 == 0 ) goto END_STRING2;
	    char ucp1 = toupper(*cp1);
	    char ucp2 = toupper(*cp2);
	    if ( (ucp1 != ucp2) )
	    {
		if ( ucp1 > ucp2 ) goto RETURN_TRUE;
		goto RETURN_FALSE;
	    }
	    --num1;--num2;++cp1;++cp2;
	}
    END_STRING1:
	if ( num2 == 0 ) goto RETURN_TRUE;
	goto RETURN_FALSE;
    END_STRING2:
    RETURN_FALSE:
	return _Nil<T_O>();
    RETURN_TRUE:
	return Fixnum_O::create((int)(cp1-this->_Contents.c_str()));
    }


    Str_O::~Str_O()
    {
    }



    T_sp Str_O::subseq(int start, T_sp end) const
    {_G();
	if ( start < 0 )
	{
	    SIMPLE_ERROR(BF("Illegal start %d for subseq") % start);
	}
	int ilen = 0;
	if ( end.nilp() )
	{
	    ilen = this->get().size()-start;
	} else
	{
	    ilen = end.as<Fixnum_O>()->get()-start;
	}
	Str_sp news = Str_O::create(this->get().substr(start,ilen));
	return news;
    }

    T_sp Str_O::setf_subseq(int start, T_sp end, T_sp new_subseq)
    {_G();
	IMPLEMENT_ME();
    }


    claspChar Str_O::schar(int index) const
    {
	if ( index >= 0 && index < this->size() ) {
	    return this->_Contents[index];
	}
	SIMPLE_ERROR(BF("Illegal index for schar %d must be in (integer 0 %d)") % index % this->size());
    }

    claspChar Str_O::scharSet(int index, claspChar c)
    {
	if ( index >= 0 && index < this->size() ) {
	    this->_Contents[index] = c;
	    return c;
	}
	SIMPLE_ERROR(BF("Illegal index for schar %d must be in (integer 0 %d)") % index % this->size());
    }

    void Str_O::fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end )
    {_G();
	char celement = element.as<Character_O>()->asChar();
	uint istart = start->get();
	uint last = this->size();
	uint iend = last;
	if ( end.notnilp() ) iend = end.as<Fixnum_O>()->get();
	ASSERTF(iend>=istart,BF("Illegal fill range istart=%d iend=%d") % istart % iend );
	ASSERTF(iend<=last,BF("Illegal value for end[%d] - must be between istart[%d] and less than %d") % iend % istart % last );
	ASSERTF(istart>=0 <= iend,BF("Illegal value for start[%d] - must be between 0 and %d") % istart % iend );
	for ( uint i = istart; i<iend; i++ )
	{
	    this->_Contents[i] = celement;
	}
    }


    void* Str_O::addressOfBuffer() const
    {
	return (void*)(this->_Contents.c_str());
    }



    void Str_O::fillInitialContents(T_sp seq)
    {
	if ( List_sp ls = seq.asOrNull<Cons_O>() ) {
	    if ( cl_length(seq) != this->size() ) goto ERROR;
	    size_t i = 0;
	    for ( auto cur : ls ) {
		this->_Contents[i] = oCar(cur).as<Character_O>()->asChar();
		++i;
	    }
	} else if ( Str_sp ss = seq.asOrNull<Str_O>() ) {
	    if ( ss->length() != this->size() ) goto ERROR;
	    for ( size_t i=0; i<this->size(); ++i ) {
		this->_Contents[i] = (*ss)[i];
	    }
	} else if ( Vector_sp vs = seq.asOrNull<Vector_O>() ) {
	    if ( vs->length() != this->size() ) goto ERROR;
	    for ( size_t i=0; i<this->size(); ++i ) {
		this->_Contents[i] = vs->elt(i).as<Character_O>()->asChar();
	    }
	} else {
	    SIMPLE_ERROR(BF("Illegal :INITIAL-CONTENTS"));
	}
	return;
    ERROR:
	SIMPLE_ERROR(BF("There are %d elements in the :INITIAL-CONTENTS, but the %s length is %d") % cl_length(seq) % _rep_(seq->__class()->className()) % this->size() );
    }



    void Str_O::__write__(T_sp stream) const
    {
	int ndx;
	if ( !clasp_print_escape() && !clasp_print_readably() )
	{
	    for (ndx = 0;  ndx < this->size(); ndx++ )
	    {
		clasp_write_char(this->_Contents[ndx],stream);
	    }
        } else {
	    clasp_write_char('"',stream);
	    for (ndx = 0;  ndx < this->size(); ndx++ )
	    {
		char c = this->_Contents[ndx];
		if (c == '"' || c == '\\') clasp_write_char('\\',stream);
		clasp_write_char(c,stream);
	    }
	    clasp_write_char('"',stream);
        }
    }



}; /* core */
