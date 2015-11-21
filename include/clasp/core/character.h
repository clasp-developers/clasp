/*
    File: character.h
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
#ifndef _core_character_H
#define _core_character_H

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/str.fwd.h>
#include <clasp/core/character.fwd.h>
namespace core {
claspChar clasp_as_char(Character_sp c);
Character_sp clasp_make_standard_character(claspCharacter c);
inline claspCharacter unbox_character(Character_sp c) {
  return c.unsafe_character();
};

Str_sp cl_char_name(Character_sp och);

int clasp_string_case(Str_sp s);
Fixnum clasp_digitp(int ch, int basis);

bool af_standard_char_p(Character_sp ch);

class Character_dummy_O : public T_O {
  LISP_BASE1(T_O);
  LISP_VIRTUAL_CLASS(core, ClPkg, Character_dummy_O, "character");
};
};

#if 0
namespace core {
    SMART(StandardChar);
    c l a s s StandardChar_O : public BaseChar_O
    {
	LISP_BASE1(BaseChar_O);
	L I S P_CLASS(core,ClPkg,StandardChar_O,"standard-char");
    public:
#if defined(OLD_SERIALIZE)
	void serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    private:
	unsigned char	_Value;
    public:
	static StandardChar_sp create(claspChar nm);
	/*! Create a character from a name like TAB, NEWLINE, LINEFEED, PAGE, RETURN, SPACE */
	static StandardChar_sp create_from_name(string const& name);

    public:
	claspChar asChar() const { return this->_Value;};
	claspChar get() const { return this->_Value;};
	void sxhash_(HashGenerator& hg) const;
	virtual T_sp deepCopy() const;
	string __repr__() const;
	void set(int val) { this->_Value = val; };
	void increment(int i) { this->_Value += i; };
	int operator+(int y) const { return this->_Value+y;};
	LongLongInt operator+(LongLongInt y) const { return ((LongLongInt)(this->_Value))+y;};
	double operator+(double y) const { return ((double)(this->_Value))+y;};

//	uint asUInt() const;
//	T_sp sub(Function_sp e, List_sp args, Environment_sp environ, Lisp_sp lisp);

	int operator-(int y) const { return this->_Value-y;};
	LongLongInt operator-(LongLongInt y) const { return ((LongLongInt)(this->_Value))-y;};

	virtual	bool	eqn(T_sp obj) const;
	virtual	bool	eql_(T_sp obj) const;
	virtual	bool	operator<(T_sp obj) const;
	virtual	bool	operator<=(T_sp obj) const;
	virtual	bool	operator>(T_sp obj) const;
	virtual	bool	operator>=(T_sp obj) const;

    public:
	virtual	string	valueAsString() const;
	virtual	void	setFromString( const string& strVal );
	int	toInt() const { return (int)this->_Value; };
	virtual Character_sp char_upcase() const;
	virtual Character_sp char_downcase() const;
	virtual bool upper_case_p() const;
	virtual bool lower_case_p() const;
	virtual bool both_case_p() const;
	virtual bool alpha_char_p() const;
	virtual bool alphanumericp() const;
	virtual bool graphicCharP() const;

	DEFAULT_CTOR_DTOR(StandardChar_O);
    };
};
template<> struct gctools::GCInfo<core::StandardChar_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static GCInfo_policy constexpr Policy = normal;
};
#endif

#if 0
namespace core {
    FORWARD(ExtendedChar);
    c l a s s ExtendedChar_O : public Character_O
    {
	LISP_BASE1(Character_O);
	L I S P_CLASS(core,ClPkg,ExtendedChar_O,"extended-char");
#if defined(XML_ARCHIVE)
	DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(ExtendedChar_O);
    public:
	void initialize();
    private: // instance variables here
    public: // Functions here
    };
};
template<> struct gctools::GCInfo<core::ExtendedChar_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static GCInfo_policy constexpr Policy = normal;
};
#endif

namespace core {
inline short clasp_digit_char(Fixnum w, Fixnum r) {
  if (r < 2 || r > 36 || w < 0 || w >= r)
    return (-1);
  if (w < 10)
    return (w + '0');
  else
    return (w - 10 + 'A');
}

}; /* core */

namespace translate {
template <>
struct from_object<claspChar, std::true_type> {
  typedef claspChar DeclareType;
  DeclareType _v;
  from_object(T_P o) {
    if (core::Character_sp ch = o.asOrNull<core::Character_O>()) {
      this->_v = clasp_as_char(ch);
      return;
    }
    SIMPLE_ERROR(BF("Could not convert %s to CHARACTER") % _rep_(o));
  }
};

template <>
struct to_object<char> {
  typedef uint GivenType;
  static core::T_sp convert(GivenType v) {
    _G();
    return core::clasp_make_character(v);
  }
};
};
TRANSLATE(core::Character_O);

namespace core {
claspChar clasp_charCode(T_sp elt); // like ecl_char_code

inline bool clasp_invalid_character_p(int c) {
  return (c <= 32) || (c == 127);
}

inline Character_sp clasp_char_upcase(claspCharacter code) {
  unsigned char uc = toupper(code);
  return clasp_make_character(uc);
}

inline Character_sp clasp_char_downcase(claspCharacter code) {
  unsigned char uc = tolower(code);
  return clasp_make_character(uc);
}

inline Character_sp clasp_char_upcase(Character_sp code) {
  unsigned char uc = toupper(clasp_as_char(code));
  return clasp_make_character(uc);
}

inline Character_sp clasp_char_downcase(Character_sp code) {
  unsigned char uc = tolower(clasp_as_char(code));
  return clasp_make_character(uc);
}

inline bool clasp_alphanumericp(claspCharacter i) {
  return isalnum(i);
}

inline claspChar clasp_as_char(Character_sp c) {
  return c.unsafe_character();
}

inline claspCharacter clasp_as_character(Character_sp c) {
  return c.unsafe_character();
}

inline Character_sp clasp_make_character(claspCharacter c) {
  return gc::make_tagged_character(c);
}

inline Character_sp clasp_make_standard_character(claspCharacter c) {
  return gc::make_tagged_character(c);
}

inline claspCharacter clasp_char_code(Character_sp c) {
  return unbox_character(c);
}

Character_sp clasp_character_create_from_name(string const &name);
};

#endif
