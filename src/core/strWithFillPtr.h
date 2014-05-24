#ifndef _core_strWithFillPtr_H
#define _core_strWithFillPtr_H

#include "core/foundation.h"
#include "core/object.h"
#include "core/str.h"
#include "core/strWithFillPtr.fwd.h"



namespace core
{

    class StrWithFillPtr_O : public Str_O
    {
	LISP_BASE1(Str_O);
	LISP_CLASS(core,ClPkg,StrWithFillPtr_O,"base-string-with-fill-ptr");
    protected:
	int	_FillPointer;
	bool	_Adjustable;
    public:
	static StrWithFillPtr_sp create(const string& nm)
	{
            GC_ALLOCATE(StrWithFillPtr_O,v );
            v->set(nm);
	    return v;
	};
	static StrWithFillPtr_sp create(const char* nm)
	{
            GC_ALLOCATE(StrWithFillPtr_O,v );
            v->setFromChars(nm);
	    return v;
	};
	static StrWithFillPtr_sp create(char initial_element, int dimension, int fill_ptr, bool adjustable,Sequence_sp initialContents=_Nil<Sequence_O>());
	/*! Create a buffer BUFFER_STRING_LEN size, fill_ptr=0, adjustable=true */
	static StrWithFillPtr_sp createBufferString(size_t bufferSize=BUFFER_STRING_SIZE) {
	    return StrWithFillPtr_O::create('\0',bufferSize,0,true,_Nil<Sequence_O>());
	};
    public:
	virtual bool adjustableArrayP() const { return this->_Adjustable;}
	virtual void set(const string& v) { this->_Contents = v; this->_FillPointer = v.size();};
	virtual void setFromChars(const char* v) { this->_Contents = v; this->_FillPointer = this->_Contents.size();}
	virtual string get() const { return this->_Contents.substr(0,this->_FillPointer);};
	virtual uint size() const { return this->_FillPointer;};

	void incrementFillPointer(int offset);
	void setFillPointer(size_t fp);

	void adjustSize(int adjustment);
	void setSize(int sz);

	/*! Make sure there is enough space from the fill-pointer out */
	void ensureSpaceAfterFillPointer(size_t size);
	/*! Return the address of where the fill_ptr points to */
	char* addressOfFillPtr();
	/*! Adjust the fill pointer */
	void incrementFillPtr(size_t size);
	

	virtual Fixnum_sp vectorPush(T_sp newElement);
	virtual Fixnum_sp vectorPushExtend(T_sp newElement, int extension=0);

	int pushCharExtend(brclChar c, int extension = 0);

	/*! Push the contents of string designator (str) from (start) to (end) */
	void pushSubString(T_sp str, size_t start, size_t end );

	/*! Push the entire contents of the string in (str) */
	void pushString(T_sp str);

	/*! Push the entire contents of the string in (str) */
	void pushString(const char* str);


    string __repr__() const;
    public:
	explicit StrWithFillPtr_O() : T_O(), Base(), _FillPointer(0), _Adjustable(false) {};
	virtual ~StrWithFillPtr_O() {};
    };

    inline void brcl_string_push_extend(StrWithFillPtr_sp str, Fixnum c) {
	str->pushCharExtend(c);
    }
	
    


};



TRANSLATE(core::StrWithFillPtr_O);



#endif
