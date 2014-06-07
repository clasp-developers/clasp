#define	DEBUG_LEVEL_FULL

#include <string>
#include "core/common.h"
#include "core/corePackage.h"
#include "core/sourceFileInfo.h"
#include "character.h"
#include "numbers.h"
#include "symbolTable.h"
#include "lispDefinitions.h"
#include "lispStream.h"
#include "str.h"
#include "specialForm.h"
#include "cons.h"
//#include "lisp_ParserExtern.h"
#include "reader.h"
#include "wrappers.h"

namespace core 
{





    EXPOSE_CLASS(core,Reader_O);
    void Reader_O::exposeCando(Lisp_sp lisp)
    {
	class_<Reader_O>()
	    ;
    }

    void Reader_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Reader,"","",_lisp)
	    ;
#endif
    }



    Reader_sp Reader_O::create(Stream_sp sin, Lisp_sp lisp)
    {	
	Reader_sp reader = lisp->create<Reader_O>();
	reader->_Input = sin;
       	return reader;
    }


    Reader_O::Reader_O() : Reader_O::Base() {}

    void Reader_O::initialize()
    {_OF();
	this->Base::initialize();
	this->_Input = _Nil<Stream_O>();
    }

    Reader_O::~Reader_O()
    {
    }

    string Reader_O::fileName()
    {
	return af_sourceFileInfo(this->_Input)->fileName();
    }

    SYMBOL_EXPORT_SC_(ClPkg,STARread_suppressSTAR);
    bool Reader_O::suppressRead() const
    {_G();
	return cl::_sym_STARread_suppressSTAR->symbolValue().isTrue();
    }


    T_sp Reader_O::internSymbol(const string& chars)
    {_G();
	Symbol_sp sym = _lisp->intern(chars);
	return sym;
    }


    /*! Figure out if the string in chars is a LongLongInt, Fixnum, Real or Symbol 
      and return the appropriate object
    */
    T_sp Reader_O::parseString(const string& chars, bool sawEscape )
    {_OF();
	LOG(BF("parseString[%s]")%chars);
	double d;
	int li;
//	LongLongInt lli;
	/* Try to interpret as booleans */
	if ( chars == "true" || chars == "t" )
	{
	    if (this->suppressRead()) return _Nil<T_O>();
	    LOG(BF("true"));
	    ASSERTNOTNULL(_lisp->_true());
	    return _lisp->_true();
	}
	if ( chars == "nil") // used to be false
	{
	    if (this->suppressRead()) return _Nil<T_O>();
	    LOG(BF("false"));
	    return _Nil<T_O>();
	}
	/* Try to interpret as numbers */
	if ( sawEscape )
	{
	    if (this->suppressRead()) return _Nil<T_O>();
	    LOG(BF("saw escape - symbol"));
	    T_sp sym = this->internSymbol(chars);
	    return sym;
	}
	const char* beginChar = chars.c_str();
	char* lastValid = (char*)beginChar;
	const char* endChar = beginChar+chars.size();
//	char lastChar = chars[chars.size()-1];
	int sawExponent = 0;
	int sawPeriod = 0;
	bool possibleNumber = false;
	if ( *beginChar == '+' || *beginChar == '-' )
	{
	    if ( isdigit(*(beginChar+1))) possibleNumber = true;
	} else if ( isdigit(*beginChar) || *beginChar == '.' ) possibleNumber = true;
	for ( const char* c=beginChar; *c; c++ )
	{
	    if ( *c == '.' ) 
	    {
		sawPeriod++;
	    }
	    if ( *c == 'e' || *c == 'E' ) 
	    {
		sawExponent++;
	    }
	}
	if ( possibleNumber )
	{
	    if ((sawExponent>0) || (sawPeriod>0))
	    {
		LOG(BF("sawExponent or sawPeriod true"));
		d = strtod(beginChar,&lastValid);
		LOG(BF("double = %lf") % d );
		if ( lastValid == endChar )
		{
		    if ( this->suppressRead() ) return _Nil<T_O>();
		    LOG(BF("Returning double"));
		    DoubleFloat_sp od = DoubleFloat_O::create(d);
		    return od;
		}
		goto INTERPRET_SYMBOL;
	    }
#if 0
	    if ( lastChar == 'l' || lastChar == 'L')
	    {
		LOG(BF("last character is l/L"));
		lli = strtoll(beginChar,&lastValid,10);
		if ( lastValid == endChar-1 )
		{
		    if ( this->suppressRead() ) return _Nil<T_O>();
		    LOG(BF("Returning LongLongInt"));
		    LongLongInt_sp oll = LongLongInt_O::create(lli,_lisp);
		    return oll;
		}
		goto INTERPRET_SYMBOL;
	    }
#endif
	    li = strtol(beginChar,&lastValid,10);
	    LOG(BF("beginChar(%X) endChar(%X) lastValid(%X)") % beginChar % endChar % lastValid );
	    if ( lastValid == endChar )
	    {
		if ( this->suppressRead() ) return _Nil<T_O>();
		LOG(BF("Returning Fixnum"));
		Fixnum_sp oi = Fixnum_O::create(li);
		return oi;
	    }
	    goto INTERPRET_SYMBOL;
	}
    INTERPRET_SYMBOL:
	if ( this->suppressRead() ) return _Nil<T_O>();
//	transform(feature.begin(),feature.end(),feature.begin(),::toUpper);
	LOG(BF("About to internSymbol[%s]") % chars );
	string charsUpper = lispify_symbol_name(chars);
	T_sp sym = this->internSymbol(charsUpper);
	return sym;
    }


    /*! Handle read suppression with RAII */
    struct ReadSuppress
    {
	bool	_SavedReadSuppress;
	ReadSuppress()
	{_G();
	    Symbol_sp rs = cl::_sym_STARread_suppressSTAR;
	    this->_SavedReadSuppress = rs->symbolValue().isTrue();
	    rs->setf_symbolValue(_lisp->_true());
	}
	virtual ~ReadSuppress()
	{_G();
	    cl::_sym_STARread_suppressSTAR->setf_symbolValue(_lisp->_boolean(this->_SavedReadSuppress));
	}
    };


    bool Reader_O::isFeatureRecognized(string const& feature) const
    {_OF();
	if ( feature == "cando" ) return true;
	// Add compiler feature once we have a compiler
	return false;
    }


    T_sp Reader_O::featureRead(bool readIfRecognized, bool& readExpression )
    {_OF();
	this->skipWhiteSpace();
	char c = this->nextChar();
	string feature = this->readString(c);
	T_sp value = _Nil<T_O>();
	transform(feature.begin(),feature.end(),feature.begin(),::tolower);
	if ( this->isFeatureRecognized(feature) == readIfRecognized )
	{
	    value = this->primitive_read(true,_Nil<T_O>(),true);
	    readExpression = true;
	} else
	{
	    ReadSuppress suppresion;
	    this->primitive_read(true,_Nil<T_O>(),true);
	    readExpression = false;
	    value = _Nil<T_O>();
	}
	return value;
    }



    T_sp Reader_O::primitive_read(bool eofErrorP, T_sp eofValue, bool recursiveP)
    {_OF();
	bool sawEscape;
	string chars;
	T_sp result = _Nil<T_O>();
	while (1)
	{
	    int lastChar = this->skipWhiteSpace();
	    if ( lastChar == EOF )
	    {
		if ( eofErrorP )
		{
		    END_OF_FILE_ERROR(this->_Input);
		}
		/* eof is returned as nil */
		result = eofValue;
		goto RETURN;
	    }
	    Token tok = this->nextToken(chars,sawEscape);
	    switch (tok)
	    {
	    case openParen:
	    {
		Cons_sp list = this->readDelimitedList(')',true);
		result = list;
		goto RETURN;
	    }
	    case singleQuote:
	    {
		T_sp quotedObject = this->primitive_read(true,_Nil<T_O>(),true);
		if (this->suppressRead())
		{
		    result = _Nil<T_O>();
		    goto RETURN;
		}
//		SourceLocation sourceLoc = this->_Input->sourceLocation();
		result = Cons_O::createList(cl::_sym_quote, quotedObject );
                if ( _lisp->sourceDatabase().notnilp() ) {
                    _lisp->sourceDatabase()->registerSourceInfoFromStream(result,this->_Input);
                }
	    goto RETURN;
	    }
	    case quotedString:
	    {
		if (this->suppressRead())
		{
		    result = _Nil<T_O>();
		    goto RETURN;
		}
		T_sp str = Str_O::create(chars);
		result = str;
		goto RETURN;
	    }
	    case sharpQuote:
	    {
		T_sp quotedObject = this->primitive_read(true,_Nil<T_O>(),true);
		if (this->suppressRead())
		{
		    result = _Nil<T_O>();
		    goto RETURN;
		}
//		SourceLocation sl = this->_Input->sourceLocation();
		result = Cons_O::createList(cl::_sym_function, quotedObject);
                if ( _lisp->sourceDatabase().notnilp() ) {
                    _lisp->sourceDatabase()->registerSourceInfo(result,
							   this->_Input->sourceFileInfo(),
							   this->_Input->lineNumber(),
							   this->_Input->column(),
							   this->_Input->tell());
                }
		goto RETURN;
	    }
	    case sharpMinus:
	    {
		LOG(BF("Got sharpMinus"));
		bool readExpression = false;
		T_sp value = this->featureRead(false,readExpression);
		if ( readExpression )
		{
		    result = value;
		    goto RETURN;
		}
		break;
	    }
	    case sharpPlus:
	    {
		bool readExpression = false;
		T_sp value = this->featureRead(true,readExpression);
		if (readExpression)
		{
		    result = value;
		    goto RETURN;
		}
		break;
	    }
	    case sharpVerticalBar:
		this->skipToMatchingVerticalBarSharp();
		break;
	    case sharpBackslash:
	    {
		stringstream ss;
		int iread = 0;
		Character_sp ch;
		ss << this->_Input->get();
		iread++;
		bool done = false;
		while ( !done )
		{
		    unsigned char c = this->_Input->peek_char();
		    if ( isalpha(c) )
		    {
			ss << this->_Input->get();
			iread++;
		    }
		    else done = true;
		}
		if ( iread == 1 )
		{
		    if (this->suppressRead())
		    {
			result = _Nil<T_O>();
			goto RETURN;
		    }
		    result = Character_O::create(ss.str()[0]);
		    goto RETURN;
		} else
		{
		    if (this->suppressRead())
		    {
			result = _Nil<T_O>();
			goto RETURN;
		    }
		    ch = Character_O::create_from_name(ss.str());
		}
	    }

	    case backQuote:
	    {
		T_sp quotedObject = this->primitive_read(true,_Nil<T_O>(),true);
		if (this->suppressRead())
		{
		    result = _Nil<T_O>();
		    goto RETURN;
		}
		result = Cons_O::createList(_sym_backquote, quotedObject);
                if ( _lisp->sourceDatabase().notnilp() ) {
                    _lisp->sourceDatabase()->registerSourceInfoFromStream(result,this->_Input);
                }
		goto RETURN;
	    }
// This is like a backQuote but two arguments must follow.
// The first element is a template Cons or SourceCodeCons and the second object is the object being backQuoted.
// The doubleBackQuote special creates an object of the same class as the template
// and if the template is a SourceCodeCons then copies the lineNumber/fileName info from it into the
// new SourceCodeCons
//  - this lets us keep track of where the source code was read that was backQuoted so that if we find
// errors in it we can track it back to the original source rather than the place where the backQuote was
// defined.
// Usage:
// (defvar *a* '(pp z))
// (defvar *b* `` *a* ( ,(car *a*) ,@(cdr *a*)))
// --> *b* is a SourceCodeCons whose lineNumber/fileName points to where *a* was defined.
//

	    case doubleBackQuote:
	    {
		T_sp templateObject = this->primitive_read(true,_Nil<T_O>(),true);
		T_sp quotedObject = this->primitive_read(true,_Nil<T_O>(),true);
		if (this->suppressRead())
		{
		    result = _Nil<T_O>();
		    goto RETURN;
		}
		result = Cons_O::createList(_sym_double_backquote, templateObject);
                if ( _lisp->sourceDatabase().notnilp() ) {
                    _lisp->sourceDatabase()->registerSourceInfoFromStream(result,this->_Input);
                }
		goto RETURN;
	    }
	    case comma:
	    {
		T_sp quotedObject = this->primitive_read(true,_Nil<T_O>(),true);
		if (this->suppressRead())
		{
		    result = _Nil<T_O>();
		    goto RETURN;
		}
		result = Cons_O::createList(_sym_unquote,quotedObject);
                if ( _lisp->sourceDatabase().notnilp() ) {
                    _lisp->sourceDatabase()->registerSourceInfoFromStream(result,this->_Input);
                }
		goto RETURN;
	    }
	    case commaAt:
	    {
		T_sp quotedObject = this->primitive_read(true,_Nil<T_O>(),true);
		if (this->suppressRead())
		{
		    result = _Nil<T_O>();
		    goto RETURN;
		}
		result = Cons_O::createList(_sym_unquote_splice,quotedObject);
                if ( _lisp->sourceDatabase().notnilp() ) {
                    _lisp->sourceDatabase()->registerSourceInfoFromStream(result,this->_Input);
                }
		goto RETURN;
	    }
	    case symbol:
	    {
		result = this->parseString(chars,sawEscape);
		goto RETURN;
	    }
	    case closeParen:
	    {
		SIMPLE_ERROR(BF("Hit unexpected close parenthesis %s") % this->posAsString() );
	    }
	    default:
	    {
		SIMPLE_ERROR(BF("Hit unexpected token %s") % this->posAsString() );
		/*! Return nil for eof */
		result = _Nil<T_O>();
		goto RETURN;
	    }
	    }
	}

    RETURN:
	if ( result.nilp() ) return _Nil<T_O>();
	return result;
    }


    int Reader_O::peekChar()
    {
	return this->_Input->peek_char();
    }

    int Reader_O::nextChar()
    {_OF(); 
	char c;
	c = this->_Input->get();
	return c;
    }


    void Reader_O::skipToEol()
    {
	while (1)
	{
	    int c = this->nextChar();
	    if ( c == EOF ) break;
	    if ( c == '\n') return;
	}
    }


    void Reader_O::skipToMatchingVerticalBarSharp()
    {
	int commentCount = 1;
	while ( 1 ) {
	    int c = this->nextChar();
	    if ( c == EOF ) break;
	    if ( c == '#' && this->_Input->peek_char() == '|' )
	    {
		this->nextChar();
		commentCount++;
	    } else if ( c == '|' && this->_Input->peek_char() == '#' )
	    {
		this->nextChar();
		commentCount--;
	    }
	    if ( commentCount == 0 ) return;
	}
    }

    int Reader_O::skipWhiteSpace()
    {_OF();
	while (1) {
	    int c = this->_Input->peek_char();
	    if ( c == EOF ) return c;
	    LOG(BF("Peek[%c/%d]") % c % (int)c);
//	    if ( c == ' ' || c == '\n' || c == '\t' || c == '' || c == '\r' )
	    if ( c <= ' ')
	    {
		LOG(BF("Skipping white-space[%s]") % (int)c);
		this->nextChar();
	    } else if ( c == ';' )
	    {
		this->skipToEol();
	    } else return c;
	}
    }

    bool Reader_O::inSet(char c, const char* chrs)
    {
	while ( *chrs )
	{
	    if ( *chrs == c ) return true;
	    chrs++;
	}
	return false;
    }

    bool Reader_O::isWhiteSpace(char c)
    {
	return this->inSet(c," \t\n\r");
    }

    bool Reader_O::isTerminal(char c)
    {
	return this->inSet(c,"()\";,`'");
    }


    string Reader_O::readString(char firstChar)
    {_OF();
	stringstream ss;
	ss << firstChar;
	while (1)
	{
	    int ic = this->_Input->peek_char();
	    LOG(BF("Peeked token dec[%d] char[%c]") % ic % (char)ic );
	    if ( ic == -1 ) 
	    {
		LOG(BF("We hit the end - break"));
		break;
	    }
	    if ( this->isWhiteSpace(ic) || this->isTerminal(ic) ) break;
	    char cc = this->nextChar();
	    LOG(BF("Token dec[%d] char[%c]") % ((int)(cc)) % cc );
	    ss << cc;
	}
	LOG(BF("Read unquoted string[%s]") % ss.str() );
	return ss.str();
    }
    
    string Reader_O::readDoubleQuoteString()
    {_OF();
	stringstream acc;
	uint startLineNumber = this->_Input->lineNumber();
	while(1)
	{
	    int c = this->peekChar();
	    if ( c == '"' )
	    {
		this->nextChar();
		break;
	    }
	    c = this->nextChar();
	    if ( c == EOF )
	    {	
		SIMPLE_ERROR(BF("Unterminated string %s - it started on line %d") % this->posAsString() % startLineNumber );
	    }
	    // Convert back quoted control or special characters
	    if ( c == '\\' )
	    {
		c = this->nextChar();
		if ( c == 'n' )
		{
		    acc << std::endl;
		    continue;
		} else if ( c >='a' && c <= 'z' )
		{
		    c = c - 'a' +1;
		}
	    }
	    acc << c;
	}
	LOG(BF("Read double quoted string[%s]") % acc.str());
	return acc.str();
    }

    string Reader_O::posAsString()
    {
	stringstream ss;
	ss << this->_Input->lineNumber() << ":" << this->_Input->column() << " " << af_sourceFileInfo(this->_Input)->fileName();
	return ss.str();
    }


    Token Reader_O::nextToken(string& chars, bool& sawEscape)
    {_OF();
	sawEscape = false;
	this->skipWhiteSpace();
	char c = this->nextChar();
	switch (c)
	{
	case '(': 
	    LOG(BF("Token[%c] - openParen") % c );
	    return openParen;
	case ')': 
	    LOG(BF("Token[)] - closeParen") );
	    return closeParen;
	case '\'': 
	    LOG(BF("Token['] - singleQuote"));
	    return singleQuote;
	case '"': 
	    LOG(BF("Token[\"] - doubleQuote"));
	    chars = this->readDoubleQuoteString();
	    return quotedString;
	case '#':
	{
	    LOG(BF("Token[#] - sharp"));
	    char p = this->peekChar();
	    if ( p == '\'' ) 
	    {	
		LOG(BF("  completed [#'] - sharpQuote"));
		p = this->nextChar();
		return sharpQuote;
	    } else if (p=='v' || p=='V')
	    {
		LOG(BF("   completed [#v] - sharpV"));
		p = this->nextChar();
		return sharpV;
	    } else if (p=='|')
	    {
		LOG(BF("  sharp-vertical-bar"));
		p = this->nextChar();
		return sharpVerticalBar;
	    } else if (p== '-')
	    {
		LOG(BF("  sharp-minus"));
		p = this->nextChar();
		return sharpMinus;
	    } else if (p== '+')
	    {
		LOG(BF("  sharp-plus"));
		p = this->nextChar();
		return sharpPlus;
	    } else if (p=='\\')
	    {
		LOG(BF("   completed #\\ sharpBackslash"));
		p = this->nextChar();
		return sharpBackslash;
	    }
	    SIMPLE_ERROR(BF("Illegal sharp sequence (#%c)") % p );
	    break;
	}
	case '`':
	{
	    char cp = this->peekChar();
	    if ( cp == '`' )
	    {
		LOG(BF("Token[``] - doubleBackQuote"));
		cp = this->nextChar();
		return doubleBackQuote;
	    }
	    LOG(BF("Token[`] - backQuote"));
	    return backQuote;
	}
	case ',':
	{
	    char cp = this->peekChar();
	    if ( cp == '@' )
	    {
		LOG(BF("Token[,@] - commaAt"));
		cp = this->nextChar();
		return commaAt;
	    }
	    LOG(BF("Token[,] - comma"));
	    return comma;
	}
	}
	chars = this->readString(c);
	return symbol;
    }


    Cons_sp Reader_O::readDelimitedList(char endChar, bool recursiveP)
    {_OF();
	Cons_sp first = Cons_O::create(_Nil<T_O>(), _Nil<Cons_O>());
	Cons_sp cur = first;
	while (1)
	{
	    this->skipWhiteSpace();
	    char c = this->peekChar();
	    if ( c == endChar )
	    {
		this->nextChar();
		break;
	    }
	    uint lineNumber = this->_Input->lineNumber();
	    uint column = this->_Input->column();
	    uint filePos = this->_Input->tell();
	    T_sp element = this->primitive_read(true,_Unbound<T_O>(),true);
	    ASSERTNOTNULL(element);
	    if ( element.unboundp() )
	    {
		END_OF_FILE_ERROR(this->_Input);
	    }
	    Cons_sp one = Cons_O::create(element, _Nil<Cons_O>());
            SourceManager_sp sm = _lisp->sourceDatabase();
            if ( sm.notnilp() ) {
                sm->registerSourceInfo(one,af_sourceFileInfo(this->_Input),
				      lineNumber,column,filePos);
            }
	    cur->setCdr(one);
	    cur = one;
	}
	LOG(BF("readList: %s") % _rep_(oCdr(first)));
	return cCdr(first);
    }


		







};




