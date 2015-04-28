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
#ifndef	_core_character_H
#define _core_character_H

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/str.fwd.h>
#include <clasp/core/character.fwd.h>
namespace core
{


    Str_sp cl_char_name(Character_sp och);

    int clasp_string_case(Str_sp s);
    Fixnum clasp_digitp( int ch, int basis );

    bool af_standard_char_p(Character_sp ch);

    FORWARD(Character);
    class Character_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_VIRTUAL_CLASS(core,ClPkg,Character_O,"character");
#if defined(XML_ARCHIVE)
	DECLARE_ARCHIVE();
#endif // defined(XML_ARCHIVE)
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(Character_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit Character_O(core::Class_sp const& mc) : _O(mc), Base(mc) {};
//    virtual ~Character_O() {};
    public:
	void initialize();
    public:
        static Character_sp create(gctools::Fixnum c);
	static Character_sp create(claspChar c) { return create((gctools::Fixnum)c);};
	static Character_sp create(uint c) { return create((gctools::Fixnum)c);};
	static Character_sp create(int c) { return create((gctools::Fixnum)c);};
	/*! Create a character from a name like TAB, NEWLINE, LINEFEED, PAGE, RETURN, SPACE */
	static Character_sp create_from_name(string const& name);

	/*! Create a character from a variety of objects Integer, Character etc */
	static Character_sp create(T_sp val);
    private: // instance variables here


    public: // Functions here
	virtual claspChar asChar() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual claspChar get() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual Character_sp char_upcase() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual Character_sp char_downcase() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual bool upper_case_p() const {SUBIMP();};
	virtual bool lower_case_p() const {SUBIMP();};
	virtual bool both_case_p() const {SUBIMP();};
	virtual bool alpha_char_p() const {SUBIMP();};
	virtual bool alphanumericp() const {SUBIMP();};
	virtual bool graphicCharP() const {SUBIMP();};
	virtual bool equal(T_sp other) const;
	virtual bool equalp(T_sp other) const;
	virtual int toInt() const {SUBIMP();};
	int charCode() const { return this->toInt(); };
	void __write__(T_sp sout) const; // Look in write_ugly.cc
    };
};
template<> struct gctools::GCInfo<core::Character_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = true;
};

namespace core {



    FORWARD(BaseChar);
    class BaseChar_O : public Character_O
    {
	LISP_BASE1(Character_O);
	LISP_CLASS(core,ClPkg,BaseChar_O,"base-char");
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(BaseChar_O);
    public:
	void initialize();

    private: // instance variables here


    public: // Functions here
    };
};
template<> struct gctools::GCInfo<core::BaseChar_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = true;
};


namespace core {





    SMART(StandardChar);
    class StandardChar_O : public BaseChar_O
    {
	LISP_BASE1(BaseChar_O);
	LISP_CLASS(core,ClPkg,StandardChar_O,"standard-char");
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
	void sxhash(HashGenerator& hg) const;
	virtual T_sp deepCopy() const;
	string __repr__() const;
	void set(int val) { this->_Value = val; };
	void increment(int i) { this->_Value += i; };
	int operator+(int y) const { return this->_Value+y;};
	LongLongInt operator+(LongLongInt y) const { return ((LongLongInt)(this->_Value))+y;};
	double operator+(double y) const { return ((double)(this->_Value))+y;};

//	uint asUInt() const;
//	T_sp sub(Function_sp e, Cons_sp args, Environment_sp environ, Lisp_sp lisp);

	int operator-(int y) const { return this->_Value-y;};
	LongLongInt operator-(LongLongInt y) const { return ((LongLongInt)(this->_Value))-y;};

	virtual	bool	eqn(T_sp obj) const;
	virtual	bool	eql(T_sp obj) const;
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
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = true;
};

namespace core {




    FORWARD(ExtendedChar);
    class ExtendedChar_O : public Character_O
    {
	LISP_BASE1(Character_O);
	LISP_CLASS(core,ClPkg,ExtendedChar_O,"extended-char");
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
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = true;
};

namespace core {




    inline short brcl_digit_char(Fixnum w, Fixnum r)
    {
	if (r < 2 || r > 36 || w < 0 || w >= r)
	    return(-1);
	if (w < 10)
	    return(w + '0');
	else
	    return(w - 10 + 'A');
    }




}; /* core */



namespace translate
{
    template <>
    struct	from_object<claspChar,std::true_type>
    {
	typedef	claspChar	DeclareType;
	DeclareType _v;
	from_object(T_P o)
	{
	    if ( core::Character_sp ch = o.asOrNull<core::Character_O>() )
	    {
		this->_v = ch->asChar();
		return;
	    }
	    SIMPLE_ERROR(BF("Could not convert %s to CHARACTER") % _rep_(o));
	}
    };



    template <>
	struct	to_object<char>
    {
	typedef	uint		GivenType;
	static core::T_sp convert(GivenType v)
	{_G();
	    return core::StandardChar_O::create(v);
	}
    };
};




TRANSLATE(core::Character_O);


namespace core {
    claspChar clasp_charCode(T_sp elt); // like ecl_char_code
    bool clasp_invalid_character_p(int c);

    claspCharacter clasp_char_upcase(claspCharacter code);

    claspCharacter clasp_char_downcase(claspCharacter code);
    bool clasp_alphanumericp(claspCharacter i);



};

#endif
