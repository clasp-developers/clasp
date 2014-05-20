#ifndef _core_str_H
#define _core_str_H

#include "core/foundation.h"
#include "core/object.h"
#include "core/lispString.h"



namespace core
{

    FORWARD(Str);
    class Str_O : public String_O
    {
	LISP_BASE1(String_O);
	LISP_CLASS(core,ClPkg,Str_O,"base-string");
    public:
#if defined(OLD_SERIALIZE)
	void	serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    protected:
	string	_Contents;
    public:
	static Str_sp create(const boost::format& fmt);
	static Str_sp create(const string& nm);
	static Str_sp create(const char* nm);
	static Str_sp create(const char* nm, int numChars);
	static Str_sp create(brclChar initial_element, int dimension, Sequence_sp initialContents);
	static Str_sp create(Str_sp orig);
    public:
	static Bignum stringToBignum(const char* str);
    public:
	virtual bool adjustableArrayP() const { return false;}
    public:
	virtual T_sp asetUnsafe(int j, T_sp val);
	string __str__() { return this->_Contents; };
	string __repr__() const;
	uint countOccurances(const string& chars);
	Cons_sp splitAtWhiteSpace();
	Cons_sp split(const string& splitChars);
	virtual void set(const string& v) { this->_Contents = v; };
	virtual void setFromChars(const char* v) { this->_Contents = v; };
	virtual void setFromChars(const char* v,int num) { string temp(v,num); this->_Contents.swap(temp);};
	const char* c_str() const { return this->_Contents.c_str(); };
	string& _contents() { return this->_Contents; };
	string const& _contents() const { return this->_Contents; };
	virtual string get() const { return this->_Contents; };
	Fixnum_sp asInt() const;
	Rational_sp parseInteger();
	DoubleFloat_sp asReal() const;
	Symbol_sp asSymbol() const;
	Symbol_sp asKeywordSymbol() const;
	string left(int num) const;
	string right(int num) const;
	string concat(Str_sp other) const { return this->_Contents + other->_contents();};
	string substr(int start, int num ) const;
	void sxhash(HashGenerator& hg) const;

	brclChar schar(int index) const;
        brclChar scharSet(int index, brclChar c);

	/*! Return the index of where substring is found 
	  or nil
	*/
	Rational_sp find(const string& substring, int start);
    public:
	//! dim ignore fill pointers - don't overload
	virtual uint dimension() const { return this->_Contents.size();};
	virtual uint	size() const { return this->_Contents.size(); };
	uint length() const { return this->size(); };
	T_sp prim_format(Function_sp e, Cons_sp args, Environment_sp environ, Lisp_sp lisp );
	T_sp prim_formatCons(Function_sp e, Cons_sp args, Environment_sp environ, Lisp_sp lisp );
	virtual T_sp elementType() const;
	virtual	string	valueAsString() const { return this->_Contents;};
	virtual	void	setFromString(const string& strVal) { this->set(strVal);};
	virtual bool	equal(T_sp obj) const;
        virtual bool    equalp(T_sp obj) const;
	virtual	bool	eql(T_sp obj) const;
	virtual	bool	operator<(T_sp obj) const;
	virtual	bool	operator<=(T_sp obj) const;
	virtual	bool	operator>(T_sp obj) const;
	virtual	bool	operator>=(T_sp obj) const;


	virtual T_sp string_EQ_(Str_sp string2, int start1, int end1, int start2, int end2 ) const;
	virtual T_sp string_NE_(Str_sp string2, int start1, int end1, int start2, int end2 ) const;
	virtual T_sp string_LT_(Str_sp string2, int start1, int end1, int start2, int end2 ) const;
	virtual T_sp string_GT_(Str_sp string2, int start1, int end1, int start2, int end2 ) const;
	virtual T_sp string_LE_(Str_sp string2, int start1, int end1, int start2, int end2 ) const;
	virtual T_sp string_GE_(Str_sp string2, int start1, int end1, int start2, int end2 ) const;

	virtual T_sp string_equal(Str_sp string2, int start1, int end1, int start2, int end2 ) const;
	virtual T_sp string_not_equal(Str_sp string2, int start1, int end1, int start2, int end2 ) const;
	virtual T_sp string_lessp(Str_sp string2, int start1, int end1, int start2, int end2 ) const;
	virtual T_sp string_greaterp(Str_sp string2, int start1, int end1, int start2, int end2 ) const;
	virtual T_sp string_not_greaterp(Str_sp string2, int start1, int end1, int start2, int end2 ) const;
	virtual T_sp string_not_lessp(Str_sp string2, int start1, int end1, int start2, int end2 ) const;



	/*! Return the value at the indices */
	virtual T_sp aref(Cons_sp indices) const;
	/*! Return the value at the indices */
	virtual T_sp setf_aref(Cons_sp indices_val);

	virtual void __write__(Stream_sp strm) const;

	virtual T_sp elt(int index) const;
	virtual T_sp setf_elt(int index, T_sp value);

        virtual T_sp svref(int index) const { return elt(index); };
        virtual T_sp setf_svref(int index, T_sp value) { return this->setf_elt(index,value); };
                


    virtual Sequence_sp subseq(int start, T_sp end) const;
    virtual Sequence_sp setf_subseq(int start, T_sp end, Sequence_sp new_subseq);


	virtual void fillArrayWithElt(T_sp element, Fixnum_sp start, T_sp end);


	virtual void* addressOfBuffer() const;
	virtual size_t elementSizeInBytes() const { return sizeof(brclChar);};

	virtual void fillInitialContents(Sequence_sp initialContents);

    public:
	explicit Str_O() : T_O(), Base() {};
	virtual ~Str_O();
    };



};



TRANSLATE(core::Str_O);



namespace core {
    T_mv af_parseInteger(Str_sp str, uint start=0, T_sp end=_Nil<T_O>(), uint radix=10, T_sp junkAllowed=_Nil<T_O>());
    T_sp af_string_equal(T_sp strdes1, T_sp strdes2, Fixnum_sp start1=Fixnum_O::create(0), Fixnum_sp end1=_Nil<Fixnum_O>(), Fixnum_sp start2=Fixnum_O::create(0), Fixnum_sp end2=_Nil<Fixnum_O>() );


};

#endif
