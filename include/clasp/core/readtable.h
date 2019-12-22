/*
    File: readtable.h
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
#ifndef _core_Readtable_H
#define _core_Readtable_H

#include <clasp/core/character.fwd.h>
#include <clasp/core/hashTable.fwd.h>

namespace core {

enum clasp_readtable_case {
  clasp_case_upcase,
  clasp_case_downcase,
  clasp_case_invert,
  clasp_case_preserve
};

FORWARD(Readtable);
class Readtable_O : public General_O {
  LISP_CLASS(core, ClPkg, Readtable_O, "readtable",General_O);
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(Readtable_O);

public:
  void initialize();

GCPRIVATE: // instance variables here
  Symbol_sp Case_;
  HashTable_sp SyntaxTypes_;
  HashTable_sp MacroCharacters_;
  HashTable_sp DispatchMacroCharacters_;

public: // static functions here
  static Readtable_sp create_standard_readtable();
  /*! Create a basic syntax table that describes the syntax of
	  the charactes: tab, newline, linefeed, page, return, space
	  and '\\'(single-escape) and '|'(multiple-escape)
	  macro characters need to be added to this to create a standard readtable
	*/
  static HashTable_sp create_standard_syntax_table();

public: // instance member functions here
  Readtable_sp copyReadtable_(gc::Nilable<Readtable_sp> dest);

  string __repr__() const;

  T_sp set_syntax_type_(Character_sp ch, T_sp syntaxType);
  Symbol_sp setf_readtable_case_(Symbol_sp newCase);
  clasp_readtable_case getReadtableCaseAsEnum_();
  Symbol_sp getReadtableCase_() const { return this->Case_; };

  /*! syntax-type returns the syntax type of a character */
  Symbol_sp syntax_type_(Character_sp ch) const;

  /*! Define a macro character */
  T_sp set_macro_character_(Character_sp ch, T_sp funcDesig, T_sp non_terminating);

  /*! CLHS get-macro-character */
  T_mv get_macro_character_(Character_sp ch);

  /*! Define a dispatch macro character */
  T_sp make_dispatch_macro_character_(Character_sp ch,
                                     T_sp non_terminating_p);

  T_sp get_dispatch_macro_character_(Character_sp disp_char, Character_sp sub_chDar);
  T_sp set_dispatch_macro_character_(Character_sp ch,
                                    Character_sp second,
                                    T_sp funcDesig);

  Character_sp convert_case_(Character_sp c);
};
T_sp cl__copy_readtable(T_sp fromReadTable, T_sp toReadTable);
T_sp cl__readtable_case(T_sp);
T_sp core__readtable_case_set(T_sp mode, T_sp readTable);
T_sp core__syntax_type(T_sp, Character_sp);
T_sp core__setf_syntax_type(T_sp syntax_type, T_sp readtable, Character_sp chr);
T_sp core__reader_list_allow_consing_dot(T_sp sin, Character_sp ch); 
T_sp core__reader_double_quote_string(T_sp sin, Character_sp ch); 
T_sp core__reader_quote(T_sp sin, Character_sp ch);
T_mv cl__get_macro_character(Character_sp chr, T_sp readtable);


}; /* core */


#endif /* _core_Readtable_H */
