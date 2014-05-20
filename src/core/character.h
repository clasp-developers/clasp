#ifndef	_core_character_H
#define _core_character_H

#include "core/foundation.h"
#include "core/object.h"
#include "core/str.fwd.h"
#include "core/character.fwd.h"
namespace core
{


    Str_sp cl_char_name(Character_sp och);

    int brcl_string_case(Str_sp s);

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
	static Character_sp create(brclChar c);
	static Character_sp create(int c);
	/*! Create a character from a name like TAB, NEWLINE, LINEFEED, PAGE, RETURN, SPACE */
	static Character_sp create_from_name(string const& name);

	/*! Create a character from a variety of objects Integer, Character etc */
	static Character_sp create(T_sp val);
    private: // instance variables here


    public: // Functions here
	virtual brclChar asChar() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual brclChar get() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
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
	void __write__(Stream_sp sout) const; // Look in write_ugly.cc
    };




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
	static StandardChar_sp create(brclChar nm);
	/*! Create a character from a name like TAB, NEWLINE, LINEFEED, PAGE, RETURN, SPACE */
	static StandardChar_sp create_from_name(string const& name);

    public:
	brclChar asChar() const { return this->_Value;};
	brclChar get() const { return this->_Value;};
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




    inline short brcl_digit_char(Fixnum w, Fixnum r)
    {
	if (r < 2 || r > 36 || w < 0 || w >= r)
	    return(-1);
	if (w < 10)
	    return(w + '0');
	else
	    return(w - 10 + 'A');
    }




    struct CharacterInfo : public gctools::HeapRoot {
        map<string,Character_sp>	gNamesToCharacters;
        gctools::Vec0<Str_sp>		gCharacterNames;
        const char* repr() const { return "CharacterInfo";};
        DECLARE_onHeapScanGCRoots();
	CharacterInfo(); 
    };

    extern CharacterInfo* global_CharacterInfo;


}; /* core */



namespace translate
{
    template <>
    struct	from_object<brclChar,std::true_type>
    {
	typedef	brclChar	DeclareType;
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
    brclChar brcl_charCode(T_sp elt); // like ecl_char_code
};

#endif
