

%{
#define DEBUG_LEVEL_FULL
#define	YYERROR_VERBOSE


#include<iostream>
#include<string>
#include<vector>
#include <istream>

#include "foundation.h"
#include "object.h"
//#include "macro.h"
#include "aggregate.h"
#include "molecule.h"
#include "residue.h"
#include "atom.h"
#include "stringList.h"
#include "hold.h"
#include "specialForm.h"
#include "values.h"
#include "cons.h"

#include "lisp.h"
#include "symbol.h"
#include "intrinsics.h"
#include "keyedObject.h"

//#include	"lex.lisp.h"

#include "lispLexContext.h"

using namespace std;
using namespace mbb;
typedef struct	{
    int	_LineNumber;
    int	_Column;
} LispParsePos;

RPLisp			globalLisp;
LispParsePos		lispParsePos;
RPCons			globalProgramList;
string			globalFileName;
stringstream		lispErrorStream;

istream*	lispStream;		
// This defines the stream that we are reading from


#define	LEXDEBUG	1

#ifdef	LEXDEBUG
#define	LEXPRINT(x) {LOG(BF("Token: %s") % (x));}
#define	LEXDPRINT(x) { LOG(BF("%s")%(x));}
#else
#define	LEXPRINT(x)
#define	LEXDPRINT(x)
#endif


void	lisp_lex_debug(int l, const RPLisp& lisp);
//int	lisplex();
extern void	lisp_lex_flush(istream* instream, RPLisp lisp);
void	lisperror(const string& str);

%}



//  ------------------------------------------------------------------------------------------
//  ------------------------------------------------------------------------------------------
//  ------------------------------------------------------------------------------------------
//
//
//  options
//
//
//

%define api.pure










//
// Tokens
//
%token  lEol
%token	lIndent
%token	lDedent
%token	lOpenBrace		// "{"
%token	lCloseBrace		// "}"
%token	lOpenInfixForm		// "["
%token	lCloseInfixForm		// "]"
%token	lOpenPrefixForm		// "("
%token	lClosePrefixForm	// ")"
%token	lQuoteChar		// "'"
%token	lSharpQuote		// "#'"
%token	lBackQuote		// "`"
%token	lComma			// ","
%token	lCommaAt		// ",@"
%token	lTrue
%token	lFalse
%token	lNil
%token	<cval>lSymbol	//
%token	<cval>lKeyedName // [a-zA-Z][a-zA-Z0-9_]*: KeyName
%token	<ival>lInteger		//
%token	<llval>lLongLongInteger		//
%token	<dval>lDouble		//
%token	<cval>lString

%union yyunion {
		char*					eval;
		char					cval[256];
		int 					ival;
		mbb::LongLongInt			llval;
		bool					bval;
		double					dval;
		mbb::Hold<mbb::O_Cons>*			cons;
	}

%type<cons>		input
%type<cons>		codeBlock
%type<cons>		list
%type<cons>		bracketedStatement	
%type<cons>		unbracketedStatement	
%type<cons>		form	
%type<cons>		unknownTypeForm	
%type<cons>		quotedBracketedList
%type<cons>		backQuotedBracketedList
%type<cons>		quotedSymbol
%type<cons>		sharpQuotedSymbol
%type<cons>		commaSymbol
%type<cons>		commaAtSymbol
%type<cons>		entry
//%type<cons>		keyValuePair
%type<cons>	atom


%destructor	{ if ($$!=NULL) delete ($$);} 	codeBlock
%destructor	{ if ($$!=NULL) delete ($$);}	form
%destructor	{ if ($$!=NULL) delete ($$);}	unknownTypeForm
%destructor	{ if ($$!=NULL) delete ($$);} 	bracketedStatement	
%destructor	{ if ($$!=NULL) delete ($$);} 	unbracketedStatement	
%destructor	{ if ($$!=NULL) delete ($$);} 	quotedSymbol
%destructor	{ if ($$!=NULL) delete ($$);} 	sharpQuotedSymbol
%destructor	{ if ($$!=NULL) delete ($$);} 	commaSymbol
%destructor	{ if ($$!=NULL) delete ($$);} 	commaAtSymbol
%destructor	{ if ($$!=NULL) delete ($$);} 	quotedBracketedList
%destructor	{ if ($$!=NULL) delete ($$);} 	backQuotedBracketedList
%destructor	{ if ($$!=NULL) delete ($$);} 	entry
//%destructor	{ if ($$!=NULL) delete ($$);} 	keyValuePair
%destructor	{ if ($$!=NULL) delete ($$);} 	atom



%%


input:  codeBlock	
	{ _BLOCK_TRACE("input: codeBlock");
	    lisp_LOG(globalLisp,BF("codeBlock = %s") % $1->_obj->__repr__());
	    globalProgramList = $1->_obj;
	}
| error  
	{ _BLOCK_TRACE("input: error ");
	    lisp_THROW(globalLisp,O_LispError(globalLisp,"Compile error in file: "+globalFileName));
	}
| /* empty */
	{ _BLOCK_TRACE("Empty");
	    RPObject block= O_Object::nil(globalLisp);
	    lisp_ASSERTP(globalLisp,block->notNil(), "block intrinsic is nil");
	    globalProgramList = O_Cons::nil(globalLisp);
	};

codeBlock:	form
	{ _BLOCK_TRACE("parse: codeBlock>form");
	    lisp_LOG(globalLisp,BF("form= %s") % $1->_obj->__repr__() );
	    $$ = $1;
	}
| form codeBlock
	{_BLOCK_TRACE("parse: codeBlock> form codeBlock");
	    lisp_LOG(globalLisp,BF("form = %s" ) %  $1->_obj->__repr__() );
	    lisp_LOG(globalLisp,BF("codeBlock= %s") % $2->_obj->__repr__() );
	    $$ = $1;
	    $$->_obj->setCdr($2->_obj);
	    lisp_LOG(globalLisp,BF("$$->_obj = %s") % $$->_obj->__repr__() );
	}
;


form: unknownTypeForm
	{_BLOCK_TRACE("parse: form -> unknownTypeForm");
		// 
		// I think this is where we will recognize and expand macros
		//
	    $$ = $1;
	    lisp_LOG(globalLisp,BF("$$->_obj = %s")% $$->_obj->__repr__() );
	}
;


unknownTypeForm:  unbracketedStatement
	{_BLOCK_TRACE("parse: unknownTypeForm ->unbracketedStatement" );
	    $$ = $1;
	    lisp_LOG(globalLisp,BF("$$->_obj = %s") % $$->_obj->__repr__() );
	}
| bracketedStatement lEol
	{_BLOCK_TRACE("parse: bracketedStatement");
	    $$ = $1;
	    lisp_LOG(globalLisp,BF( "$$->_obj = %s") % $$->_obj->__repr__() );
	}
| bracketedStatement lEol lIndent codeBlock lDedent
	{_BLOCK_TRACE("parse: bracketedStatement lIndent codeBlock lDedent");
	    RPCons head = $1->_obj;
	    RPCons tail = $4->_obj;
	    lisp_LOG(globalLisp,BF("head = %s") % (head->__repr__().c_str() ) ); // vp0(( "head = %s", head->__repr__().c_str() ));
	    lisp_LOG(globalLisp,BF("tail = %s") % (tail->__repr__().c_str() ) ); // vp0(( "tail = %s", tail->__repr__().c_str() ));
	    head->append(tail);
	    lisp_LOG(globalLisp,BF("combined= %s") % (head->__repr__().c_str() ) ); // vp0(( "combined= %s", head->__repr__().c_str() ));
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(head,O_Cons::nil(globalLisp),
	    			lispParsePos._LineNumber,
				lispParsePos._Column,
				globalFileName, globalLisp
	    ));
	    lisp_LOG(globalLisp,BF("$$->_obj = %s") % ($$->_obj->__repr__().c_str() ) ); // vp0(( "$$->_obj = %s", $$->_obj->__repr__().c_str() ));
	}
| unbracketedStatement lIndent codeBlock lDedent
	{_BLOCK_TRACE("parse: | unbracketedStatement lIndent codeBlock lDedent " );
	    RPCons head = $1->_obj->car<O_Cons>();
	    RPCons tail = $3->_obj;
	    lisp_LOG(globalLisp,BF("head = %s") % (head->__repr__().c_str() ) ); // vp0(( "head = %s", head->__repr__().c_str() ));
	    lisp_LOG(globalLisp,BF("tail = %s") % (tail->__repr__().c_str() ) ); // vp0(( "tail = %s", tail->__repr__().c_str() ));
	    head->append(tail);
	    lisp_LOG(globalLisp,BF("combined= %s") % (head->__repr__().c_str() ) ); // vp0(( "combined= %s", head->__repr__().c_str() ));
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(head,O_Cons::nil(globalLisp),
	    			lispParsePos._LineNumber,
				lispParsePos._Column,
				globalFileName, globalLisp
	    ));
	    lisp_LOG(globalLisp,BF("$$->_obj = %s") % ($$->_obj->__repr__().c_str() ) ); // vp0(( "$$->_obj = %s", $$->_obj->__repr__().c_str() ));
	}
;


unbracketedStatement:  atom list lEol
	{_BLOCK_TRACE("parse: atom list eol" );
	    RPCons head = $1->_obj;
	    RPCons tail = $2->_obj;
	    lisp_LOG(globalLisp,BF("head = %s") % (head->__repr__().c_str() ) ); // vp0(( "head = %s", head->__repr__().c_str() ));
	    lisp_LOG(globalLisp,BF("tail = %s") % (tail->__repr__().c_str() ) ); // vp0(( "tail = %s", tail->__repr__().c_str() ));
	    head->append(tail);
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(head,O_Cons::nil(globalLisp),
	    			lispParsePos._LineNumber,
				lispParsePos._Column,
				globalFileName, globalLisp
	    ));
	    lisp_LOG(globalLisp,BF("$$->_obj = %s") % ($$->_obj->__repr__().c_str() ) ); // vp0(( "$$->_obj = %s", $$->_obj->__repr__().c_str() ));
	}
| atom lEol
	{_BLOCK_TRACE("parse: atom eol" );
	    RPCons head = $1->_obj;
	    lisp_LOG(globalLisp,BF("head = %s") % (head->__repr__().c_str() ) ); // vp0(( "head = %s", head->__repr__().c_str() ));
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(head,O_Cons::nil(globalLisp),
	    			lispParsePos._LineNumber,
				lispParsePos._Column,
				globalFileName, globalLisp
	    ));
	    lisp_LOG(globalLisp,BF("$$->_obj = %s") % ($$->_obj->__repr__().c_str() ) ); // vp0(( "$$->_obj = %s", $$->_obj->__repr__().c_str() ));
	}
;



bracketedStatement:	lOpenPrefixForm list lClosePrefixForm
	{_BLOCK_TRACE("parse: list");
	    $$ = new Hold<O_Cons>(O_ParsingCons::create($2->_obj,O_Cons::nil(globalLisp),
	    			lispParsePos._LineNumber,
				lispParsePos._Column,
				globalFileName, globalLisp
	    ));
	    lisp_LOG(globalLisp,BF("$$->_obj = %s") % ($$->_obj->__repr__().c_str() ) ); // vp0(( "$$->_obj = %s", $$->_obj->__repr__().c_str() ));
	}
| lOpenPrefixForm lClosePrefixForm
	{_BLOCK_TRACE("parse: []");
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(O_Object::nil(globalLisp),O_Cons::nil(globalLisp),
	    			lispParsePos._LineNumber,
				lispParsePos._Column,
				globalFileName, globalLisp
	    ));
	}
| lOpenInfixForm entry entry list lCloseInfixForm
	{_BLOCK_TRACE("parse: infix list > 2 elements");
	    lisp_LOG(globalLisp,BF("$3->_obj = |%s|") % ($3->_obj->ocar()->__repr__().c_str() ) ); // vp0(("$3->_obj = |%s|", $3->_obj->ocar()->__repr__().c_str() ));
	    lisp_LOG(globalLisp,BF("$2->_obj = |%s|") % ($2->_obj->ocar()->__repr__().c_str() ) ); // vp0(("$2->_obj = |%s|", $2->_obj->ocar()->__repr__().c_str() ));
	    lisp_LOG(globalLisp,BF("$4->_obj = |%s|") % ($4->_obj->__repr__().c_str() ) ); // vp0(("$4->_obj = |%s|", $4->_obj->__repr__().c_str() ));
	    RPCons body = O_ParsingCons::create($2->_obj->ocar(),$4->_obj,
	    			lispParsePos._LineNumber,
				lispParsePos._Column,
				globalFileName, globalLisp
	    );
	    RPCons result = O_ParsingCons::create($3->_obj->ocar(),body,
	    			lispParsePos._LineNumber,
				lispParsePos._Column,
				globalFileName, globalLisp
	    );
	    lisp_LOG(globalLisp,BF("put it together = %s") % (result->__repr__().c_str() ) ); // vp0(("put it together = %s", result->__repr__().c_str() ));
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(result,O_Cons::nil(globalLisp),
	    			lispParsePos._LineNumber,
				lispParsePos._Column,
				globalFileName, globalLisp
	    ));
	}
| lOpenInfixForm entry entry lCloseInfixForm
	{_BLOCK_TRACE("parse: infix list > 2 elements");
	    lisp_LOG(globalLisp,BF("$3->_obj = |%s|") % ($3->_obj->ocar()->__repr__().c_str() ) ); // vp0(("$3->_obj = |%s|", $3->_obj->ocar()->__repr__().c_str() ));
	    lisp_LOG(globalLisp,BF("$2->_obj = |%s|") % ($2->_obj->ocar()->__repr__().c_str() ) ); // vp0(("$2->_obj = |%s|", $2->_obj->ocar()->__repr__().c_str() ));
	    RPCons receiver = O_ParsingCons::create($2->_obj->ocar(),
	    			O_Cons::nil(globalLisp),
	    			lispParsePos._LineNumber,
				lispParsePos._Column,
				globalFileName, globalLisp
		);
	    RPCons result = O_ParsingCons::create($3->_obj->ocar(),receiver,
	    			lispParsePos._LineNumber,
				lispParsePos._Column,
				globalFileName, globalLisp
		);
	    lisp_LOG(globalLisp,BF("put it together = %s") % (result->__repr__().c_str() ) ); // vp0(("put it together = %s", result->__repr__().c_str() ));
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(result,O_Cons::nil(globalLisp),
	    			lispParsePos._LineNumber,
				lispParsePos._Column,
				globalFileName, globalLisp
	    ));
	}
| lOpenInfixForm entry lCloseInfixForm
	{_BLOCK_TRACE("parse: infix list > 1 elements");
	    lisp_LOG(globalLisp,BF("$2->_obj = |%s|") % ($2->_obj->ocar()->__repr__().c_str() ) ); // vp0(("$2->_obj = |%s|", $2->_obj->ocar()->__repr__().c_str() ));
	    $$ = new Hold<O_Cons>(O_ParsingCons::create($2->_obj->ocar(),O_Cons::nil(globalLisp),
	    			lispParsePos._LineNumber,
				lispParsePos._Column,
				globalFileName, globalLisp
	    ));
	}
;





list:	entry
	{ _BLOCK_TRACE("parse: entryList> entry");
	    $$ = $1;
	}
| entry list
	{ _BLOCK_TRACE("parse: entryList> entry list");
	    $$ = $1;
	    $$->_obj->setCdr($2->_obj);
	}
;






entry:	atom
	{ _BLOCK_TRACE("parse: entry>atom");
	    $$ = $1;
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
| bracketedStatement
	{ _BLOCK_TRACE("parse: entry>bracketedStatement");
	    $$ = $1;
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
 | quotedSymbol
	{ _BLOCK_TRACE("parse: entry>quotedSymbol");
	    $$ = $1;
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
 | sharpQuotedSymbol
	{ _BLOCK_TRACE("parse: entry>sharpQuotedSymbol");
	    $$ = $1;
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
 | quotedBracketedList
	{ _BLOCK_TRACE("parse: entry>quotedBracketedList");
	    $$ = $1;
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
 | backQuotedBracketedList
	{ _BLOCK_TRACE("parse: entry>backQuotedBracketedList");
	    $$ = $1;
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
 | commaSymbol
	{ _BLOCK_TRACE("parse: entry>commaSymbol");
	    $$ = $1;
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
 | commaAtSymbol
	{ _BLOCK_TRACE("parse: entry>commaAtSymbol");
	    $$ = $1;
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
;


//
//keyValuePair :	lKeyedName entry
//	{ _BLOCK_TRACE("parse: keyValue pair");
//	    $$ = new Hold<O_Cons>(O_ParsingCons::create(O_KeyedObject::create(globalLisp,$1,$2->_obj->ocar()),
//	    		O_Cons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
//	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|", $$->_obj->__repr__().c_str() ));
//	}
//;

	
quotedSymbol: lQuoteChar lSymbol
	{ _BLOCK_TRACE("parse: symbol");
	    RPParsingCons qq = O_ParsingCons::createList(globalLisp->getSpecialForm_quote(),globalLisp->intern($2),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp);
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(qq,O_ParsingCons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
;

sharpQuotedSymbol: lSharpQuote lSymbol
	{ _BLOCK_TRACE("parse: #'symbol");
	    RPParsingCons qq = O_ParsingCons::createList(globalLisp->getSpecialForm_function(),globalLisp->intern($2),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp);
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(qq,O_ParsingCons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
;

commaSymbol: lComma lSymbol
	{ _BLOCK_TRACE("parse: comma symbol");
	    RPParsingCons qq = O_ParsingCons::createList(globalLisp->getSpecialForm_comma(),globalLisp->intern($2),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp);
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(qq,O_ParsingCons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
;
commaAtSymbol: lCommaAt lSymbol
	{ _BLOCK_TRACE("parse: commaAt symbol");
	    RPParsingCons qq = O_ParsingCons::createList(globalLisp->getSpecialForm_commaAt(),globalLisp->intern($2),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp);
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(qq,O_ParsingCons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
;


quotedBracketedList: lQuoteChar bracketedStatement
	{ _BLOCK_TRACE("parse: symbol");
	    RPParsingCons qq = O_ParsingCons::createList(globalLisp->getSpecialForm_quote(),$2->_obj->ocar(),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp);
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(qq,O_ParsingCons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
;

backQuotedBracketedList: lBackQuote bracketedStatement
	{ _BLOCK_TRACE("parse: symbol");
	    RPParsingCons qq = O_ParsingCons::createList(globalLisp->getSpecialForm_backQuote(),$2->_obj->ocar(),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp);
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(qq,O_ParsingCons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
;



atom:	lSymbol
	{ _BLOCK_TRACE("parse: symbol");
	    RPObject symbolOrSpecialForm = globalLisp->specialFormOrNil($1);
	    if ( symbolOrSpecialForm->isNil() )
	    {
		symbolOrSpecialForm = globalLisp->intern($1);
	    }
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(symbolOrSpecialForm,
	    		O_Cons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
| lInteger
	{ _BLOCK_TRACE("parse: integer");
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(O_Int::create($1,globalLisp),
	    		O_Cons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
| lLongLongInteger
	{ _BLOCK_TRACE("parse: LongLongInt");
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(O_LongLongInt::create($1,globalLisp),
	    		O_Cons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
| lDouble
	{ _BLOCK_TRACE("parse: double");
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(O_Real::create($1,globalLisp),
	    		O_Cons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
| lString
	{ _BLOCK_TRACE("parse: string");
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(globalLisp->create<O_String>($1),
	    		O_Cons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
| lTrue 
	{ _BLOCK_TRACE("parse: tee is symbolTrue");
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(globalLisp->_true(),
	    			O_Cons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
| lFalse
	{ _BLOCK_TRACE("parse: eff is nil");
	    $$ = new Hold<O_Cons>(O_ParsingCons::create(globalLisp->_false(),
	    			O_Cons::nil(globalLisp),lispParsePos._LineNumber,lispParsePos._Column,globalFileName,globalLisp));
	    lisp_LOG(globalLisp,BF("Parsed|%s|") % ($$->_obj->__repr__().c_str() ) ); // vp0(("Parsed|%s|",$$->_obj->__repr__().c_str() ));
	}
;

	   


%%

//
//
//	Epilogue
//
//

char	lispGetChar()
{
char		c;
    c = lispStream->get();
    lisp_LOG(globalLisp,BF("lispGetChar char=(%d/\"%c\")") % c % c );
    if ( c == '\n' ) 
    {
	lispParsePos._LineNumber++;
	lispParsePos._Column=1;
    } else lispParsePos._Column++;
    if ( c < 0 ) c = 0;
    return c;
}


void	lispUnGetChar()
{
    lispStream->unget();
}

void	lisperror(const string&  str) 
{
    lispErrorStream <<"Error, file: " << globalFileName;
    lispErrorStream <<"       line: " << lispParsePos._LineNumber;
    lispErrorStream << "    column: " << lispParsePos._Column << endl;
    lispErrorStream << str;
    string err = lispErrorStream.str();
    lisp_THROW(globalLisp,O_LispError(globalLisp,err.c_str()));
}


namespace mbb {

#if 0
void lispScannerDebug(istream& sin)
{_F(globalLisp);
int		status=0;
    
    lispStream = &sin;
    lisp_lex_flush();
    lisp_lex_debug(2,globalLisp);
    lispParsePos._LineNumber = 1;
    lispParsePos._Column = 1;
    lispErrorStream.str("Script: ");
    lisp_LOG(globalLisp,BF("Starting scanner") ); // vp0(("Starting scanner"));
    while ( lisplex() );
    lisp_lex_debug(0,globalLisp);
    if ( status == 1 )
    {
	lisp_THROW(globalLisp,O_LispError(globalLisp,lispErrorStream.str()));
	globalProgramList = O_Cons::nil(globalLisp);
    }
}
#endif



/*! lispCompile is where everything happens.
 * If you want to make this reentrant you need to set up the extra
 * data that the parser and lexer will need here rather than
 * setting up global variables.
 */
RPCons lispCompile(istream& sin, RPLisp lisp, const string& fileName)
{_F(lisp);
int		status;
    LispLexContext lexContext;
    lisp_lex_flush(sin,&context,lisp);
    lispStream = &sin;
    globalLisp  = lisp;
    lispParsePos._LineNumber = 1;
    lispParsePos._Column = 1;
    globalFileName = fileName;
    globalProgramList = O_Cons::nil(globalLisp);
    lispErrorStream.str("Script: ");
    lisp_LOG(globalLisp,BF("Entering lispparse") ); // vp0(("Entering lispparse"));
    status = lispparse();
    if ( status == 1 )
    {
	THROW(O_LispError(globalLisp,lispErrorStream.str()));
	globalProgramList = O_Cons::nil(globalLisp);
    }
    lisp_LOG(globalLisp,BF("Returned status=%d") % (status) ); // vp0(("Returned status=%d",status));
    return globalProgramList;
}


string	getLispError()
{
    return lispErrorStream.str();
}

};    

