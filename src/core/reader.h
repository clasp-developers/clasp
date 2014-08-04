#ifndef	reader_H //[
#define reader_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "intStackQueue.h"

namespace core {






#define MAX_STR_CONST 2048

struct lisp_SourceParam
{
    /* The scanner extra pointer will point back to this lisp_SParserParam structure
     */
    void* scanner; // was yyscan_t

    Lisp_sp _lisp;

    // The resulting s-expression code is stored here
    Cons_sp		expression;

    // name of the file that we are compiling
    string			fileName;
    stringstream		lispErrorStream;


    // ------------------------------------------------------------
    // ------------------------------------------------------------
    // ------------------------------------------------------------
    // ------------------------------------------------------------
    // Lexer stuff
    //

    char string_buf[MAX_STR_CONST];
    char *string_buf_ptr;

    // Terminal symbols are now pulled from a queue
    // before they are handed to the parser to handle
    // multiple DEDENTs
    IntQueue	lispScannerQueue;

    // The previous indent positions are stored on a stack
    IntStack	lispIndentStack;

    // Whenever we start processing a string we save the
    // current start condition so that we can return to it
    int	_stringSaveState;
    int _stringSaveStateDepth;

    // indent mode is turned off inside of 
    // parentheses and brackets
    int	bracketDepth;

    // Current line indent position
    int	currentLineIndent;

    // We start in indent mode whenever the scanner
    // is first called
    bool	switchToIndentStartCondition;

    bool	specialsDefined;
    map<string,int>	specials;

    //
    // Debug the lex scanner
    //
    bool	debugScannerLevel;
    std::istream*	lispStream;
    int colnum;
    int eofToEol;

    void setStringSaveState(int ss)
    {_OF();
	this->_stringSaveStateDepth++;
	if ( this->_stringSaveStateDepth != 1 )
	{
	    SIMPLE_ERROR(BF("Illegal stringSaveStateDepth(%d) should only ever be (1)") % this->_stringSaveStateDepth);
	}
	this->_stringSaveState = ss;
    }
    int getStringSaveState()
    {
	if ( this->_stringSaveStateDepth != 1 )
	{
	    SIMPLE_ERROR(BF("Illegal stringSaveStateDepth(%d) should only ever be (1)") % this->_stringSaveStateDepth);
	}
	this->_stringSaveStateDepth--;
	return this->_stringSaveState;
    };

    Lisp_sp lisp() { return this->_lisp;};


    void startup();

    void shutdown();



};


#define YYLEX_PARAM	((lisp_SourceParam*)data)->scanner


typedef enum { 
    openParen, 	// (
    closeParen,  // )
    singleQuote, // '
    quotedString,	// "
    sharpQuote, // #'
    sharpV, // #v or #V
    sharpMinus, // #- for options
    sharpPlus, // #+ for 
    sharpVerticalBar, // #| for comments
    sharpBackslash, // #v or #V
    backQuote, 	// `
    doubleBackQuote, // ``
    comma, 	// ,
    commaAt, 	// ,@
    symbol,
    illegal } Token;


SMART(Reader);
class Reader_O : public T_O
{
    LISP_BASE1(T_O);
    LISP_CLASS(core,CorePkg,Reader_O,"Reader");
public: // ctors-dtors
    explicit Reader_O();
    virtual ~Reader_O();
private: // ---- instance variables ----
    Stream_sp	_Input;
private:
    void prepareToRead();
//    static void lisp_initGlobals(Lisp_sp lisp);
public: // virtual functions inherited from Object
    void	initialize();
public:
    /*! Create a reader to read from an object indicated by the inputStreamDesignator (see CLHS) */
    static Reader_sp create( Stream_sp sin);

    /*! Return true if c is in chrSet */
    bool inSet(char c, const char* chrSet);

    string posAsString();

    int peekChar();
    int nextChar();
    int skipWhiteSpace();
    void skipToEol();
    void skipToMatchingVerticalBarSharp();
    bool isWhiteSpace(char c);
    bool isTerminal(char c);
    string readString(char firstChar);
    string readDoubleQuoteString();

    Token nextToken(string& chars, bool& sawEscape);

    void appendString(const string& code);

    bool isFeatureRecognized(string const& feature) const;

    /*! Return true if we are suppressing read */
    bool suppressRead() const;

    /*! Handle sharpMinus and sharpPlus
      If sharpMinus set readIfRecognized to false otherwise true
      If the expression was read then set recognized to true and return the object read.
      If the expression was not read then set recognized to false and return nil.
    */
    T_sp featureRead(bool readIfRecognized, bool& recognized);

    /*! Return the next token */
    Token nextToken(string& chars);

    string fileName();

    T_sp internSymbol(const string& chars);
    T_sp parseString(const string& chars, bool sawEscape );


    Cons_sp readDelimitedList( char c, bool recursiveP = false);
    T_sp primitive_read(bool eofErrorP, T_sp eofValue, bool recursiveP=false);

};













};
TRANSLATE(core::Reader_O);
#endif //]
