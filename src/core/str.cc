#define	DEBUG_LEVEL_FULL

#include "clasp_gmpxx.h"
#include <ctype.h>
#include "core/common.h"
#include "core/environment.h"
#include "symbolTable.h"
#include "core/designators.h"
#include "core/character.h"
#include "bignum.h"
#include "multipleValues.h"
#include "str.h"
#include "print.h"
#include "lispStream.h"
#include "hashTable.h"
#include "core/wrappers.h"

namespace core
{

    string str_get(Str_sp str) { if (str.nilp()) {SIMPLE_ERROR(BF("Could not convert nil to Str"));}; return str->get(); };
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
	
		


    
    
#define ARGS_af_searchString "(str1 start1 end1 str2 start2 end2)"
#define DECL_af_searchString ""
#define DOCS_af_searchString "searchString"
    Fixnum_sp af_searchString(Str_sp str1, Fixnum_sp start1, Fixnum_sp end1, Str_sp str2, Fixnum_sp start2, Fixnum_sp end2)
    {_G();
        string s1 = str1->get().substr(start1->get(),end1.nilp()?str1->get().size() : end1->get());
        string s2 = str2->get().substr(start2->get(),end2.nilp()?str2->get().size() : end2->get());
//        printf("%s:%d Searching for \"%s\" in \"%s\"\n", __FILE__, __LINE__, s1.c_str(), s2.c_str());
        size_t pos = s2.find(s1);
        if ( pos == string::npos ) {
            return _Nil<Fixnum_O>();
        }
        return Fixnum_O::create(static_cast<int>(pos+start2->get()));
    };

    
    
    
#define ARGS_af_parseInteger "(string &key (start 0) end (radix 10) junk-allowed)"
#define DECL_af_parseInteger ""
#define DOCS_af_parseInteger "parseInteger"
    T_mv af_parseInteger(Str_sp str, uint start, T_sp end, uint radix, T_sp junkAllowed)
    {_G();
	int istart = MAX(0,start);
	int iend = af_length(str);
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
	GC_RESERVE(Str_O,v);
	v->set(nm.str());
	return v;
    };



    Str_sp Str_O::create(const string& nm)
    {
	GC_RESERVE_BEGIN(Str_O,v ){
	    GC_RESERVE_GET(Str_O,v );
	} GC_RESERVE_END(Str_O,v );
//	printf("%s:%d Str_O::create(const string& nm) @ %p nm = %s\n", __FILE__, __LINE__, v.px_ref(), nm.c_str() );
	v->set(nm);
	return v;
    };


    Str_sp Str_O::create(const char* nm)
    {
	GC_RESERVE_BEGIN(Str_O,v ){
	    GC_RESERVE_GET(Str_O,v );
	} GC_RESERVE_END(Str_O,v );
//	printf("%s:%d Str_O::create(const char* nm) @ %p nm = %s\n", __FILE__, __LINE__, v.px_ref(), nm );
	v->setFromChars(nm);
	return v;
    };

    Str_sp Str_O::create(const char* nm, int numChars)
    {
	GC_RESERVE_BEGIN(Str_O,v ){
	    GC_RESERVE_GET(Str_O,v );
	} GC_RESERVE_END(Str_O,v );
//	printf("%s:%d Str_O::create(const char* nm) @ %p nm = %s\n", __FILE__, __LINE__, v.px_ref(), nm );
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
    T_mv af_make_string(Fixnum_sp size, Character_sp initial_element, T_sp element_type )
    {_G();
	stringstream ss;
	char ch(' ');
	if ( initial_element.notnilp() ) ch = initial_element->asChar();
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
    T_mv af_base_string_concatenate(Cons_sp args)
    {_G();
	stringstream ss;
	for ( Cons_sp cur = args; cur.notnilp(); cur = cCdr(cur) )
	{
	    Str_sp str = coerce::stringDesignator(oCar(cur));
	    ss << str->get();
	}
	return(Values(Str_O::create(ss.str())));
    };





    inline void setup_string_op_arguments(T_sp string1_desig, T_sp string2_desig,
					  Str_sp& string1, Str_sp& string2,
					  Fixnum_sp start1, Fixnum_sp end1,
					  Fixnum_sp start2, Fixnum_sp end2,
					  int& istart1, int& iend1, int& istart2, int& iend2 )
    {_G();
	string1 = coerce::stringDesignator(string1_desig);
	string2 = coerce::stringDesignator(string2_desig);
	istart1 = MAX(start1->get(),0);
	iend1 = MIN(end1.nilp() ? af_length(string1) : end1->get(),af_length(string1));
	istart2 = MAX(start2->get(),0);
	iend2 = MIN(end2.nilp() ? af_length(string2) : end2->get(),af_length(string2));
    }


#define DOCS_af_string_EQ_ "string_EQ_"
#define LOCK_af_string_EQ_ 1
#define ARGS_af_string_EQ_ "(strdes1 strdes2 &key (start1 0) end1 (start2 0) end2)"
#define DECL_af_string_EQ_ ""    
    T_mv af_string_EQ_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, Fixnum_sp end1, Fixnum_sp start2, Fixnum_sp end2 )
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
    T_mv af_string_NE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, Fixnum_sp end1, Fixnum_sp start2, Fixnum_sp end2 )
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
    T_mv af_string_LT_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, Fixnum_sp end1, Fixnum_sp start2, Fixnum_sp end2 )
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
    T_mv af_string_GT_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, Fixnum_sp end1, Fixnum_sp start2, Fixnum_sp end2 )
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
    T_mv af_string_LE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, Fixnum_sp end1, Fixnum_sp start2, Fixnum_sp end2 )
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
    T_mv af_string_GE_(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, Fixnum_sp end1, Fixnum_sp start2, Fixnum_sp end2 )
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
    T_sp af_string_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, Fixnum_sp end1, Fixnum_sp start2, Fixnum_sp end2 )
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
    T_mv af_string_not_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, Fixnum_sp end1, Fixnum_sp start2, Fixnum_sp end2 )
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
    T_mv af_string_lessp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, Fixnum_sp end1, Fixnum_sp start2, Fixnum_sp end2 )
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
    T_mv af_string_greaterp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, Fixnum_sp end1, Fixnum_sp start2, Fixnum_sp end2 )
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
    T_mv af_string_not_greaterp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, Fixnum_sp end1, Fixnum_sp start2, Fixnum_sp end2 )
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
    T_mv af_string_not_lessp(T_sp strdes1, T_sp strdes2, Fixnum_sp start1, Fixnum_sp end1, Fixnum_sp start2, Fixnum_sp end2 )
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
		.def("asInt", &Str_O::asInt)
		.def("parse-real",&Str_O::asReal)
		.def("asReal", &Str_O::asReal)
		.def("asSymbol", &Str_O::asSymbol)
		.def("asKeywordSymbol", &Str_O::asKeywordSymbol)
		.def("set", &Str_O::set)
		.def("left", &Str_O::left)
		.def("find", &Str_O::find)
		.def("right", &Str_O::right)
		.def("substr", &Str_O::substr)
//		.def("get", &Str_O::get)
		.def("size", &Str_O::size)
		.def("countOccurances", &Str_O::countOccurances)
		.def("split", &Str_O::split)
		.def("concat", &Str_O::concat)
		.def("splitAtWhiteSpace", &Str_O::splitAtWhiteSpace)
		.def("schar",&Str_O::schar)
                .def("scharSet",&Str_O::scharSet)
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
		.def("set", &Str_O::set)
//		.def("get", &Str_O::get)
		;
#endif
	}



    Str_sp Str_O::create(char initial_element, int dimension, Sequence_sp seq )
    {_G();
	GC_RESERVE_BEGIN(Str_O,str ){
	    GC_RESERVE_GET(Str_O,str );
	} GC_RESERVE_END(Str_O,str );
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
	ASSERTF(index>=0 && index<this->size(),BF("Index out of range for string: %d") % index );
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




    T_sp Str_O::aref(Cons_sp indices) const
    {_OF();
	ASSERTF(af_length(indices)==1,BF("Illegal index for string: %s") % _rep_(indices) );
	int index = oCar(indices).as<Fixnum_O>()->get();
	return this->elt(index);
    }

    T_sp Str_O::setf_aref(Cons_sp indices_val)
    {_OF();
	ASSERTF(af_length(indices_val)==2,BF("Illegal index/val for setf_aref of string: %s") % _rep_(indices_val) );
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



    Rational_sp Str_O::find(const string& substring, int start)
    {
	size_t res = this->get().find(substring,start);
	if ( res != string::npos ) return Integer_O::create((uint)res);
	return _Nil<Fixnum_O>();
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
	    Str_sp t = safe_downcast<Str_O>(obj);
	    return this->get() < t->get();
	}
	return this->Base::operator<(obj);
    }

    bool Str_O::operator<=(T_sp obj) const
    {
	if ( af_strP(obj) )
	{
	    Str_sp t = safe_downcast<Str_O>(obj);
	    return this->get() <= t->get();
	}
	return this->Base::operator<=(obj);
    }

    Cons_sp	Str_O::splitAtWhiteSpace()
    {
	vector<string> parts = core::split(this->get()," \n\t");
	Cons_sp first = Cons_O::create(_Nil<T_O>(),_Nil<Cons_O>());
	Cons_sp cur = first;
	for ( vector<string>::iterator it=parts.begin(); it!=parts.end(); it++ )
	{
	    Cons_sp one = Cons_O::create(Str_O::create(*it),_Nil<Cons_O>());
	    cur->setCdr(one);
	    cur=one;
	}
	return cCdr(first);
    }

    Cons_sp	Str_O::split(const string& chars)
    {
	vector<string> parts = core::split(this->get(),chars);
	return Cons_O::createFromRangeObjectify< vector<string>::iterator, string >(parts.begin(),parts.end());
    }

    bool	Str_O::eql(T_sp obj) const
    {
	if ( af_strP(obj) )
	{
	    Str_sp t = safe_downcast<Str_O>(obj);
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
        if (Str_sp s2 = obj.asOrNull<Str_O>() ) {
            if ( this->length() != s2->length() ) return false;
	    return this->string_equal(s2,0,this->length(),0,this->length());
	}
	return false;
    }


    bool Str_O::operator>(T_sp obj) const
    {
	IMPLEMENT_ME();
#if 0
	if ( obj->strP() )
	{
	    Str_sp t = safe_downcast<Str_O>(obj);
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



/*
  __BEGIN_DOC( candoScript.general.format, format)
  \scriptCmdRet{format}{Text::format args ...}{string}\par
  \scriptMethodRet{Text::format}{\%}{args ...}{string}

  Generates formatted output using the boost "format" library.  
  It generates formatted output similar to the C-printf function.
  The result is returned as a string.
  __END_DOC
*/
#if 0
    T_sp	Str_O::prim_format(Function_sp e, Cons_sp args, Environment_sp environ, Lisp_sp lisp )
    {_G();
	if ( af_length(args)<1 )
	    SIMPLE_ERROR(BF("You must provide at least the format string"));
	Str_sp fmt = args->listref<Str_O>(0);
	Str_sp result;
	TRY()
	{
	    boost::format fmter(fmt->get());
	    stringstream sout;
	    for( Cons_sp farg=args->cdr(); farg.notnilp(); farg = farg->cdr() )
	    {
		T_sp fobj = farg->ocar();
		if ( fobj->fixnumP() )
		{
		    Fixnum_sp fint = safe_downcast<Fixnum_O>(fobj);
		    fmter % fint->get();
		} else if ( fobj->bignumP() )
		{
		    stringstream ss;
		    ss << fobj.as<Bignum_O>()->as_mpz();
		    fmter % ss.str();
		} else if ( fobj->strP() )
		{
		    Str_sp ftext = safe_downcast<Str_O>(fobj);
		    fmter % ftext->get();
		} else if ( fobj->doubleFloatP() )
		{
		    DoubleFloat_sp freal= safe_downcast<DoubleFloat_O>(fobj);
		    fmter % freal->get();
		} else
		{
		    fmter % fobj->__str__();
		}
	    }
	    TRY_BOOST_FORMAT_STRING(fmter,fmter_str);
	    result = Str_O::create(fmter_str);
	} catch ( boost::io::bad_format_string& err )
	  {
	      SIMPLE_ERROR(BF("format/% command error: bad format string" ));
	  } catch ( boost::io::too_few_args& err )
	    {
		SIMPLE_ERROR(BF("format/% command error: too few args" ));
	    } catch ( boost::io::too_many_args& err )
	      {
		  SIMPLE_ERROR(BF("format/% command error: too many args" ));
	      } catch ( boost::io::out_of_range& err )
		{
		    SIMPLE_ERROR(BF("format/% command error: out of range" ));
		}
	return result;
    }


/*
  __BEGIN_DOC( candoScript.general.formatCons, formatCons)
  \scriptCmdRet{format}{Text::formatCons Cons::args}{string}\par
  \scriptMethodRet{Text::formatCons}{\%}{Cons::argsstring}

  Generates formatted output using the boost "format" library.  
  Arguments are passed as a Cons.
  It generates formatted output similar to the C-printf function.
  The result is returned as a string.
  __END_DOC
*/
    T_sp	Str_O::prim_formatCons(Function_sp e, Cons_sp args, Environment_sp environ, Lisp_sp lisp )
    {_G();
	if ( af_length(args)!=2 )
	{
	    SIMPLE_ERROR(BF("You must provide the format string and arguments as a cons"));
	}
	Str_sp fmt = args->listref<Str_O>(0);
	Cons_sp cargs = args->listref<Cons_O>(1);
	Str_sp result;
	TRY()
	{
	    boost::format fmter(fmt->get());
	    stringstream sout;
	    for( Cons_sp farg=cargs; farg.notnilp(); farg = farg->cdr() )
	    {
		T_sp fobj = farg->ocar().as<Number_O>();
		if ( fobj->fixnumP!() )
		{
		    Fixnum_sp fint = safe_downcast<Fixnum_O>(fobj);
		    fmter % fint->get();
		} else if ( fobj->bignumP() )
		{
		    stringstream ss;
		    ss << fobj.as<Bignum_O>()->as_mpz();
		    fmter % ss.str();
		} else if ( fobj->strP() )
		{
		    Str_sp ftext = safe_downcast<Str_O>(fobj);
		    fmter % ftext->get();
		} else if ( fobj->doubleFloatP() )
		{
		    DoubleFloat_sp freal= safe_downcast<DoubleFloat_O>(fobj);
		    fmter % freal->get();
		} else
		{
		    fmter % fobj->__str__();
		}
	    }
	    TRY_BOOST_FORMAT_STRING(fmter,fmter_str);
	    result = Str_O::create(fmter_str);
	} catch ( boost::io::bad_format_string& err )
	  {
	      SIMPLE_ERROR(BF("format/% command error: bad format string" ));
	  } catch ( boost::io::too_few_args& err )
	    {
		SIMPLE_ERROR(BF("format/% command error: too few args" ));
	    } catch ( boost::io::too_many_args& err )
	      {
		  SIMPLE_ERROR(BF("format/% command error: too many args" ));
	      } catch ( boost::io::out_of_range& err )
		{
		    SIMPLE_ERROR(BF("format/% command error: out of range" ));
		}
	return result;
    }
#endif

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
	    return BRCL_NIL;
    RETURN_TRUE:
	    return BRCL_T;
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
	    return BRCL_T;
    RETURN_FALSE:
	    return BRCL_NIL;
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
	    return BRCL_NIL;
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
	return BRCL_NIL;
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
	    return BRCL_NIL;
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
	return BRCL_NIL;
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
	    return BRCL_NIL;
    RETURN_TRUE:
	    return BRCL_T;
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
	    return BRCL_T;
    RETURN_FALSE:
	    return BRCL_NIL;
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
	    return BRCL_NIL;
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
	return BRCL_NIL;
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
	    return BRCL_NIL;
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
	return BRCL_NIL;
    RETURN_TRUE:
	return Fixnum_O::create((int)(cp1-this->_Contents.c_str()));
    }
	    

    Str_O::~Str_O()
    {
    }



    Sequence_sp Str_O::subseq(int start, T_sp end) const
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

    Sequence_sp Str_O::setf_subseq(int start, T_sp end, Sequence_sp new_subseq)
    {_G();
	IMPLEMENT_ME();
    }


    brclChar Str_O::schar(int index) const
    {
	ASSERTF(index >= 0 && index < this->size(),BF("schar index out of bounds[%d] - must be less than %d") % index % this->size() );
	return this->_Contents[index];
    }

    brclChar Str_O::scharSet(int index, brclChar c) 
    {
	ASSERTF(index >= 0 && index < this->size(),BF("schar index out of bounds[%d] - must be less than %d") % index % this->size() );
        this->_Contents[index] = c;
	return this->_Contents[index];
    }

    void Str_O::fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end )
    {_G();
	char celement = element.as<Character_O>()->asChar();
	uint istart = start->get();
	uint last = this->size();
	uint iend = last-1;
	if ( end.notnilp() ) iend = end.as<Fixnum_O>()->get();
	ASSERTF(iend>=istart,BF("Illegal fill range istart=%d iend=%d") % istart % iend );
	ASSERTF(iend<last,BF("Illegal value for end[%d] - must be between istart[%d] and less than %d") % iend % istart % last );
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



    void Str_O::fillInitialContents(Sequence_sp seq)
    {
	if ( Cons_sp ls = seq.asOrNull<Cons_O>() )
	{
	    if ( af_length(seq) != this->dimension() ) goto ERROR;
	    size_t i = 0;
	    for (Cons_sp cur=ls; cur.notnilp(); cur=cCdr(cur))
	    {
		this->_Contents[i] = oCar(cur).as<Character_O>()->asChar();
		++i;
	    }
	} else if ( Str_sp ss = seq.asOrNull<Str_O>() ) {
	    if ( ss->length() != this->dimension() ) goto ERROR;
	    for ( size_t i=0; i<this->dimension(); ++i ) {
		this->_Contents[i] = ss->_contents()[i];
	    }
	} else if ( Vector_sp vs = seq.asOrNull<Vector_O>() ) {
	    if ( vs->length() != this->dimension() ) goto ERROR;
	    for ( size_t i=0; i<this->dimension(); ++i ) {
		this->_Contents[i] = vs->elt(i).as<Character_O>()->asChar();
	    }
	} else {
	    SIMPLE_ERROR(BF("Illegal :INITIAL-CONTENTS"));
	}
	return;
    ERROR:
	SIMPLE_ERROR(BF("There are %d elements in the :INITIAL-CONTENTS, but the %s length is %d") % af_length(seq) % _rep_(seq->__class()->className()) % this->dimension() );
    }



    void Str_O::__write__(Stream_sp stream) const
    {
	int ndx;
	if ( !brcl_print_escape() && !brcl_print_readably() )
	{
	    for (ndx = 0;  ndx < this->size(); ndx++ )
	    {
		stream->writeChar(this->_Contents[ndx]);
	    }
        } else {
	    stream->writeChar('"');
	    for (ndx = 0;  ndx < this->size(); ndx++ )
	    {
		char c = this->_Contents[ndx];
		if (c == '"' || c == '\\') stream->writeChar('\\');
		stream->writeChar(c);
	    }
	    stream->writeChar('"');
        }
    }



}; /* core */
