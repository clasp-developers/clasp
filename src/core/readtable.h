#ifndef	_core_ReadTable_H
#define _core_ReadTable_H

#include "core/foundation.h"
#include "core/character.fwd.h"
#include "core/hashTable.fwd.h"


namespace core
{


    FORWARD(ReadTable);
    class ReadTable_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,ClPkg,ReadTable_O,"readtable");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(ReadTable_O);
    public:
	void initialize();

    private: // instance variables here
	Symbol_sp	_Case;
	/*! _Syntax is a HashTable and each value is a plist */
	HashTable_sp	_Syntax; 
    public: // static functions here
	static ReadTable_sp create_standard_readtable(Lisp_sp lisp);
	/*! Create a basic syntax table that describes the syntax of
	  the charactes: tab, newline, linefeed, page, return, space
	  and '\\'(single-escape) and '|'(multiple-escape)
	  macro characters need to be added to this to create a standard readtable
	*/
	static HashTable_sp create_standard_syntax_table(Lisp_sp lisp);


    public: // instance member functions here

	string __repr__() const;

	Symbol_sp setf_readtable_case(Symbol_sp newCase);

	/*! syntax-type returns the syntax type of a character */
	Symbol_sp syntax_type(Character_sp ch) const;

	/*! Define a macro character */
	T_sp set_macro_character(Character_sp ch, T_sp funcDesig, T_sp non_terminating );

	/*! CLHS get-macro-character */
	T_mv get_macro_character(Character_sp ch );

	/*! Define a dispatch macro character */
	T_sp make_dispatch_macro_character(Character_sp ch,
					   T_sp non_terminating_p );


	Function_sp get_dispatch_macro_character(Character_sp disp_char, Character_sp sub_chDar );
	T_sp set_dispatch_macro_character(Character_sp ch,
					  Character_sp second,
					  T_sp funcDesig );


	Character_sp convert_case(Character_sp c);
	Function_sp lookup_reader_macro(Character_sp c);
    };

}; /* core */

TRANSLATE(core::ReadTable_O);

#endif /* _core_ReadTable_H */


