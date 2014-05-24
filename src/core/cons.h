#ifndef	_core_Cons_H //[
#define _core_Cons_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "cons.fwd.h"
#include "sourceFileInfo.fwd.h"
#include "lispList.h"

namespace cl {
    extern core::Symbol_sp _sym_typeError;
    extern core::Symbol_sp _sym_Cons_O;
};

namespace kw {
    extern core::Symbol_sp _sym_datum;
    extern core::Symbol_sp _sym_expectedType;
};

namespace core
{

    SMART(ObjectDictionary);
    SMART(Environment);




    T_sp oCar(T_sp obj);
    T_sp oCdr(T_sp obj);
    Cons_sp cCar(T_sp obj);
    Cons_sp cCdr(T_sp obj);
    Cons_sp cCddr(T_sp obj);
    Cons_sp cCdddr(T_sp obj);
    Cons_sp cCddddr(T_sp obj);
    T_sp oCaar(T_sp o);
    T_sp oCadr(T_sp o);
    T_sp oCdar(T_sp o);
    T_sp oCddr(T_sp o);
    T_sp oCaaar(T_sp o);
    T_sp oCaadr(T_sp o);
    T_sp oCadar(T_sp o);
    T_sp oCaddr(T_sp o);
    T_sp oCdaar(T_sp o);
    T_sp oCdadr(T_sp o);
    T_sp oCddar(T_sp o);
    T_sp oCdddr(T_sp o);
    T_sp oCaaaar(T_sp o);
    T_sp oCaadar(T_sp o);
    T_sp oCadaar(T_sp o);
    T_sp oCaddar(T_sp o);
    T_sp oCdaaar(T_sp o);
    T_sp oCdadar(T_sp o);
    T_sp oCddaar(T_sp o);
    T_sp oCdddar(T_sp o);
    T_sp oCaaadr(T_sp o);
    T_sp oCaaddr(T_sp o);
    T_sp oCadadr(T_sp o);
    T_sp oCadddr(T_sp o);
    T_sp oCdaadr(T_sp o);
    T_sp oCdaddr(T_sp o);
    T_sp oCddadr(T_sp o);
    T_sp oCddddr(T_sp o);
    T_sp oFirst(T_sp o);
    T_sp oSecond(T_sp o);
    T_sp oThird(T_sp o);
    T_sp oFourth(T_sp o);
    T_sp oFifth(T_sp o);
    T_sp oSixth(T_sp o);
    T_sp oSeventh(T_sp o);
    T_sp oEighth(T_sp o);
    T_sp oNinth(T_sp o);
    T_sp oTenth(T_sp o);

#define	CONS_CAR(x) oCar(x)
#define	CONS_CDR(x) oCdr(x)
#define	CAR(x) oCar(x)
#define	CDR(x) oCdr(x)
};

namespace core {
    SMART(Cons);
};

template<> struct gctools::GCAllocatorInfo<core::Cons_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};
    

namespace core {
    
    class Cons_O : public List_O
    {
	LISP_BASE1(List_O);
	LISP_CLASS(core,ClPkg,Cons_O,"Cons");
#if defined(OLD_SERIALIZE)
	DECLARE_SERIALIZE();
#endif // defined(OLD_SERIALIZE)
	friend class SourceCodeCons_O;
	friend T_sp oCar(T_sp o);
	friend T_sp oCdr(T_sp o);
	friend Cons_sp cCar(T_sp o);
	friend Cons_sp cCdr(T_sp o);
	friend Cons_sp cCddr(T_sp o);
	friend Cons_sp cCdddr(T_sp o);
	friend Cons_sp cCddddr(T_sp o);
    public:
	void	archiveBase(ArchiveP node);
    public:
	typedef T_O	CarType_O;
	typedef T_O	CdrType_O;
	typedef T_sp 	CarType_sp;
	typedef T_sp 	CdrType_sp;
    protected:
	CarType_sp	_Car;
	CdrType_sp	_Cdr;
	/*! Keep track of the length of the cons along the cdr chain
	 * every time the cdr is set add 1 to the length of the cons
	 * being added and store that here.  This will keep a running
	 * tab of how many cons elements are in the list.
	 */
//	uint		_CdrLength;	// Keep track of the length of the cons
    public:
	static Cons_sp createFrom_va_list(va_list& va_args);
	static Cons_sp createList(T_sp o1);
	static Cons_sp createList(T_sp o1, T_sp o2);
	static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3);
	static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4 );
	static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5);
	static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6 );
	static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, T_sp o7 );
	static Cons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, T_sp o7, T_sp o8 );
	static Cons_sp create(T_sp car, T_sp cdr)
	{
//            Cons_sp ll = gctools::GCObjectAllocator<Cons_O>::allocate();
            GC_ALLOCATE(Cons_O,ll);
            ll->setCar(car);
            ll->setOCdr(cdr);
	    return ll;
	};
#if 0
	static Cons_sp create(T_sp car, T_sp cdr, Lisp_sp lisp)
	{
	    return Cons_O::create(car,cdr);
	}

	static Cons_sp create(T_sp obj, Lisp_sp lisp)
	{
	    return Cons_O::create(obj,_Nil<T_O>(),lisp);
	}
#endif
	static Cons_sp create(T_sp obj)
	{
	    return Cons_O::create(obj,_Nil<T_O>());
	}

	template <typename iter>
	    static Cons_sp createFromRange(iter begin, iter end)
	{
	    Cons_sp first = Cons_O::create(_Nil<T_O>(),_Nil<T_O>());
	    Cons_sp cur = first;
	    for ( iter it=begin; it!=end; it++ )
	    {
		Cons_sp one = Cons_O::create(*it,_Nil<T_O>(),_lisp);
		cur->setCdr(one);
		cur = one;
	    }
	    return cCdr(first);
	}
	template <typename iter,typename baseType>
	    static Cons_sp createFromRangeObjectify(iter begin, iter end)
	{_G();
	    Cons_sp first = Cons_O::create(_Nil<T_O>(),_Nil<T_O>());
	    Cons_sp cur = first;
	    for ( iter it=begin; it!=end; it++ )
	    {
		Cons_sp one = Cons_O::create(translate::to_object<baseType>::convert(*it),_Nil<Cons_O>());
		cur->setCdr(one);
		cur = one;
	    }
	    return cCdr(first);
	}
	static Cons_sp createFromCommandLineArguments(int argc, char**argv);
	static Cons_sp createFromVectorStringsCommandLineArguments(const vector<string>& strings , Lisp_sp e);
    public:
	static void appendInto(T_sp head, mem::StackRootedPointerToSmartPtr<T_O>& tail, T_sp l);
	static List_sp append(List_sp x, List_sp y);
    public:
	/*! Recursively hash the car and cdr parts - until the HashGenerator fills up */
	void sxhash(HashGenerator& hg) const;

	/*! Depth first search to find a Cons with ParsePos information */
	virtual Cons_sp walkToFindParsePos() const;



	inline Cons_sp rplaca(T_sp o) { this->_Car = o; return this->sharedThis<Cons_O>();};
	inline Cons_sp rplacd(T_sp o) { this->_Cdr = o; return this->sharedThis<Cons_O>();};

	virtual T_sp onth(int idx) const;
	virtual T_sp onthcdr(int idx) const;


	T_sp elt(int index) const;
	T_sp setf_elt(int index, T_sp value);


	CdrType_sp* cdrPtr() { return &(this->_Cdr);};

	/* TODO:
	   Remove the following member functions and replace them
	   with the real functions  oCar, oCdr, cCdr etc */
	   
	T_sp ocar() const    { return this->_Car;};
	T_sp ocadr() const { return this->cdr()->ocar(); };
	T_sp ocaddr() const { return this->cdr()->cdr()->ocar(); };
	Cons_sp cdr() const 	{ return this->_Cdr.as<Cons_O>();};
	Cons_sp cddr() const 	{ return oCdr(this->_Cdr).as<Cons_O>();};
	Cons_sp cdddr() const 	{ return oCddr(this->_Cdr).as<Cons_O>();};
	Cons_sp cddddr() const 	{ return oCdddr(this->_Cdr).as<Cons_O>();};
#if 0
	Cons_sp ccar() const { return cCar(this->const_sharedThis<Cons_O>());};
	T_sp ocar() const { return oCar(this->const_sharedThis<Cons_O>());};
	T_sp ocdr() const { return oCdr(this->const_sharedThis<Cons_O>());};
	T_sp ocaar() const {return oCaar(this->const_sharedThis<Cons_O>());};
	T_sp ocadr() const {return oCadr(this->const_sharedThis<Cons_O>());};
	T_sp ocdar() const {return oCdar(this->const_sharedThis<Cons_O>());};
	T_sp ocddr() const {return oCddr(this->const_sharedThis<Cons_O>());};
	T_sp ocaaar() const {return oCaaar(this->const_sharedThis<Cons_O>());};
	T_sp ocaadr() const {return oCaadr(this->const_sharedThis<Cons_O>());};
	T_sp ocadar() const {return oCadar(this->const_sharedThis<Cons_O>());};
	T_sp ocaddr() const {return oCaddr(this->const_sharedThis<Cons_O>());};
	T_sp ocdaar() const {return oCdaar(this->const_sharedThis<Cons_O>());};
	T_sp ocdadr() const {return oCdadr(this->const_sharedThis<Cons_O>());};
	T_sp ocddar() const {return oCddar(this->const_sharedThis<Cons_O>());};
	T_sp ocdddr() const {return oCdddr(this->const_sharedThis<Cons_O>());};
	T_sp ocaaaar() const {return oCaaaar(this->const_sharedThis<Cons_O>());};
	T_sp ocaadar() const {return oCaadar(this->const_sharedThis<Cons_O>());};
	T_sp ocadaar() const {return oCadaar(this->const_sharedThis<Cons_O>());};
	T_sp ocaddar() const {return oCaddar(this->const_sharedThis<Cons_O>());};
	T_sp ocdaaar() const {return oCdaaar(this->const_sharedThis<Cons_O>());};
	T_sp ocdadar() const {return oCdadar(this->const_sharedThis<Cons_O>());};
	T_sp ocddaar() const {return oCddaar(this->const_sharedThis<Cons_O>());};
	T_sp ocdddar() const {return oCdddar(this->const_sharedThis<Cons_O>());};
	T_sp ocaaadr() const {return oCaaadr(this->const_sharedThis<Cons_O>());};
	T_sp ocaaddr() const {return oCaaddr(this->const_sharedThis<Cons_O>());};
	T_sp ocadadr() const {return oCadadr(this->const_sharedThis<Cons_O>());};
	T_sp ocadddr() const {return oCadddr(this->const_sharedThis<Cons_O>());};
	T_sp ocdaadr() const {return oCdaadr(this->const_sharedThis<Cons_O>());};
	T_sp ocdaddr() const {return oCdaddr(this->const_sharedThis<Cons_O>());};
	T_sp ocddadr() const {return oCddadr(this->const_sharedThis<Cons_O>());};
	T_sp ocddddr() const {return oCddddr(this->const_sharedThis<Cons_O>());};


	T_sp ofirst() const {return oFirst(this->const_sharedThis<Cons_O>());};
	T_sp osecond() const {return oSecond(this->const_sharedThis<Cons_O>());};
	T_sp othird() const {return oThird(this->const_sharedThis<Cons_O>());};
	T_sp ofourth() const {return oFourth(this->const_sharedThis<Cons_O>());};
	T_sp ofifth() const {return oFifth(this->const_sharedThis<Cons_O>());};
	T_sp osixth() const {return oSixth(this->const_sharedThis<Cons_O>());};
	T_sp oseventh() const {return oSeventh(this->const_sharedThis<Cons_O>());};
	T_sp oeighth() const {return oEighth(this->const_sharedThis<Cons_O>());};
	T_sp oninth() const {return oNinth(this->const_sharedThis<Cons_O>());};
	T_sp otenth() const {return oTenth(this->const_sharedThis<Cons_O>());};
#endif

	/*! Set the data for this element */
	void	setCar(T_sp o ) { ANN(o);this->_Car = o; };

	T_sp setf_car(T_sp o) {ANN(o);this->_Car=o;return o;};


	/*! Get the data for the first element */
	template <class o_class>
	mem::smart_ptr<o_class> car() { ASSERTNOTNULL(this->_Car);return this->_Car.as<o_class>();};

	virtual bool equal(T_sp obj) const;
        virtual bool equalp(T_sp obj) const;

	/*! For CompiledBody this will return the Functoid* to invoke */
	virtual Functoid* functoid() const { return NULL;};


	T_sp setf_nth(int index, T_sp val);

	/*! Return a Cons that has all the same elements
	 * in the same order but with nil objects removed.
	 */
	Cons_sp filterOutNil();

	/*! Return a new list by combinding the given list of elements to our list
	 */
	Cons_sp extend(Cons_sp rest);

	/*! Return the reversed list */
	Sequence_sp reverse();

	/*! Return the reversed list */
	Sequence_sp nreverse();

	virtual List_sp revappend(T_sp tail);
	virtual List_sp nreconc(T_sp tail);

	/*! Set the next pointer for this element */
	void	setCdr(Cons_sp o);

	/*! Set the cdr for this cons */
	void	setOCdr(T_sp o);

	T_sp setf_cdr(T_sp o) { this->setOCdr(o); return o;};
#if 0
	uint	cdrLength() const
	{
	    return this->_CdrLength;
	};
#endif
	/*! Return the last cons (not the last element) of list.
	  If we are nil then return nil */	
	virtual T_sp last(int idx=1) const;


	/*! Like Common Lisp copy-list */
	virtual Cons_sp copyList() const;

	/*! Like Common Lisp copy-list */
	virtual Cons_sp copyListCar() const;

	/*! Like Common Lisp copy-tree */
	virtual T_sp copyTree() const;

	/*! Return a new Cons with a tree copy of the current car*/
	virtual Cons_sp copyTreeCar() const;

	/*! Return the number of elements in the list*/
	uint	length() const;

	/*! Return an arbitrary member of the list or an empty member*/
	template <class o_class>
	mem::smart_ptr<o_class> listref(int i) { return this->olistref(i).as<o_class>();};

	/*! Return an arbitrary member of the list or an empty member*/
	T_sp olistref(int index);

	/*! Return an arbitrary member of the list or an empty member - used only for passing arguments from Lisp to C++*/
	T_sp olistrefArgument(int index);

	/*! Lookup the association given a key 
	 * (associations are two element lists or KeyedObjects)
	 */
	T_sp olookupKeyObject(Symbol_sp key);

	T_sp olookupKeyObjectDefault(Symbol_sp key, T_sp dflt);

	Cons_sp memberEq(T_sp item) const;
	Cons_sp memberEql(T_sp item) const;

	Cons_sp member1(T_sp item, T_sp key, T_sp test, T_sp testNot) const;
	Cons_sp member(T_sp item, T_sp key, T_sp test, T_sp testNot) const;
	Cons_sp assoc(T_sp item, T_sp key, T_sp test, T_sp testNot) const;


	/*! Return true if every CAR of other matches the cooresponding CAR of this pointer */
	bool exactlyMatches(Cons_sp other) const;

	/*!If the Cons doesn't contained KeyedObjects
	 * return nil.
	 * If it does and they are all at the end of the list
	 * then set the cdr of the last non-keyed entry to nil
	 * and return the Cons of the first KeyedObject.
	 * This will separate the list into two lists.
	 * If they aren't all at the end of the list then throw
	 * an exception
	 */
//	void splitListIntoNonKeyedAndKeyedObjects(Cons_sp& nonKeyed, Cons_sp& keyed);
//	void splitListIntoNonKeyedListAndDictionary(Cons_sp& nonKeyed, ObjectDictionary_sp& keyed,bool& allArgsIn__args);


	template <class string>
	    T_sp olookup(Symbol_sp key)
	{
	    return this->olookupKeyObject(key);
	}
	template <class oreturnType, class string>
	mem::smart_ptr<oreturnType> lookup(Symbol_sp s)
	{
	    T_sp oret = this->olookupKeyObject(s);
	    return oret.as<oreturnType>();
	}

	string __repr__() const;
	void __write__(Stream_sp stream) const;


	virtual bool hasParsePos() const { return false;};
	virtual void getParsePos(int& ln, int& col) const {_OF(); ln=0; col=0; };
	virtual int getParsePosLineNumber() const {_OF(); return 0;};
	virtual int getParsePosColumn() const {return 0;};
	virtual SourceFileInfo_sp sourceFileInfo() const;
	int lineNumber() const { return this->getParsePosLineNumber();};
	int column() const { return this->getParsePosColumn();};

	T_sp product(Cons_sp list);
	T_sp max(Cons_sp list);
	T_sp min(Cons_sp list);
	T_sp booleanOr(Cons_sp list);
	T_sp booleanAnd(Cons_sp list);

	/*!Set the owner of every car in the list
	 */
//	void setOwnerOfAllEntries(T_sp obj);


	virtual Sequence_sp subseq(int start, T_sp end) const;
	virtual Sequence_sp setf_subseq(int start, T_sp end, Sequence_sp new_subseq) {_G(); IMPLEMENT_ME();};



	/*! Return the value associated with the property of the plist - implements CL getf */
	T_sp getf(T_sp key, T_sp defValue) const;

	explicit Cons_O();
	virtual ~Cons_O();

    };


//
// The LispParserPos structure is used to keep track of the parse position
// while parsing text
//

    typedef struct	{
	int	first_line;
	int first_column;
	int last_line;
	int last_column;
    } LispParserPos;


inline T_sp oCar(T_sp obj) {
	if (obj.nilp()) return _Nil<T_O>();
	if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
	    return cobj->_Car;
	}
	TYPE_ERROR(obj,cl::_sym_Cons_O);
    }
    inline T_sp oCdr(T_sp obj) {
	if (obj.nilp()) return _Nil<T_O>();
	if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
	    return cobj->_Cdr;
	}
	TYPE_ERROR(obj,cl::_sym_Cons_O);
    }
    inline Cons_sp cCar(T_sp obj) {
	if (obj.nilp()) return _Nil<Cons_O>();
	if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
	    return cobj->_Car.as_or_nil<Cons_O>();
	}
	TYPE_ERROR(obj,cl::_sym_Cons_O);
    }
    inline Cons_sp cCdr(T_sp obj) {
	if (obj.nilp()) return _Nil<Cons_O>();
	if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
	    return cobj->_Cdr.as_or_nil<Cons_O>();
	}
	TYPE_ERROR(obj,cl::_sym_Cons_O);
    }

    inline Cons_sp cCddr(T_sp obj) {
	if (obj.nilp()) return _Nil<Cons_O>();
	if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
	    return cCdr(cobj->_Cdr).as_or_nil<Cons_O>();
	}
	TYPE_ERROR(obj,cl::_sym_Cons_O);
    }

    inline Cons_sp cCdddr(T_sp obj) {
	if (obj.nilp()) return _Nil<Cons_O>();
	if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
	    return cCddr(cobj->_Cdr).as_or_nil<Cons_O>();
	}
	TYPE_ERROR(obj,cl::_sym_Cons_O);
    }

    inline Cons_sp cCddddr(T_sp obj) {
	if (obj.nilp()) return _Nil<Cons_O>();
	if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
	    return cCdddr(cobj->_Cdr).as_or_nil<Cons_O>();
	}
	TYPE_ERROR(obj,cl::_sym_Cons_O);
    }


inline T_sp oCaar(T_sp o) { return oCar(cCar(o));};
inline T_sp oCadr(T_sp o) { return oCar(cCdr(o));};
inline T_sp oCdar(T_sp o) { return oCdr(cCar(o));};
inline T_sp oCddr(T_sp o) { return oCdr(cCdr(o));};
inline T_sp oCaaar(T_sp o)  { return oCar(cCar(cCar(o)));};
inline T_sp oCaadr(T_sp o)  { return oCar(cCar(cCdr(o)));};
inline T_sp oCadar(T_sp o)  { return oCar(cCdr(cCar(o)));};
inline T_sp oCaddr(T_sp o)  { return oCar(cCdr(cCdr(o)));};
inline T_sp oCdaar(T_sp o)  { return oCdr(cCar(cCar(o)));};
inline T_sp oCdadr(T_sp o)  { return oCdr(cCar(cCdr(o)));};
inline T_sp oCddar(T_sp o)  { return oCdr(cCdr(cCar(o)));};
inline T_sp oCdddr(T_sp o)  { return oCdr(cCdr(cCdr(o)));};
inline T_sp oCaaaar(T_sp o) { return oCar(cCar(cCar(o)));};
inline T_sp oCaadar(T_sp o) { return oCar(cCar(cCdr(cCar(o))));};
inline T_sp oCadaar(T_sp o) { return oCar(cCdr(cCar(cCar(o))));};
inline T_sp oCaddar(T_sp o) { return oCar(cCdr(cCdr(cCar(o))));};
inline T_sp oCdaaar(T_sp o) { return oCdr(cCar(cCar(cCar(o))));};
inline T_sp oCdadar(T_sp o) { return oCdr(cCar(cCdr(cCar(o))));};
inline T_sp oCddaar(T_sp o) { return oCdr(cCdr(cCar(cCar(o))));};
inline T_sp oCdddar(T_sp o) { return oCdr(cCdr(cCdr(cCar(o))));};
inline T_sp oCaaadr(T_sp o) { return oCar(cCar(cCar(cCar(o))));};
inline T_sp oCaaddr(T_sp o) { return oCar(cCar(cCdr(cCdr(o))));};
inline T_sp oCadadr(T_sp o) { return oCar(cCdr(cCar(cCdr(o))));};
inline T_sp oCadddr(T_sp o) { return oCar(cCdr(cCdr(cCdr(o))));};
inline T_sp oCdaadr(T_sp o) { return oCdr(cCar(cCar(cCdr(o))));};
inline T_sp oCdaddr(T_sp o) { return oCdr(cCar(cCdr(cCdr(o))));};
inline T_sp oCddadr(T_sp o) { return oCdr(cCdr(cCar(cCdr(o))));};
inline T_sp oCddddr(T_sp o) { return oCdr(cCdr(cCdr(cCdr(o))));};
inline T_sp oFirst(T_sp o)  { return oCar(o); };
inline T_sp oSecond(T_sp o) { return oCar(cCdr(o));};
inline T_sp oThird(T_sp o)  { return oCar(cCdr(cCdr(o)));};
inline T_sp oFourth(T_sp o) { return oCar(cCdr(cCdr(cCdr(o))));};
inline T_sp oFifth(T_sp o)  { return oCar(cCdr(cCdr(cCdr(cCdr(o)))));};
inline T_sp oSixth(T_sp o)  { return oCar(cCdr(cCdr(cCdr(cCdr(cCdr(o))))));};
inline T_sp oSeventh(T_sp o){ return oCar(cCdr(cCdr(cCdr(cCdr(cCdr(cCdr(o)))))));};
inline T_sp oEighth(T_sp o) { return oCar(cCdr(cCdr(cCdr(cCdr(cCdr(cCdr(cCdr(o))))))));};
inline T_sp oNinth(T_sp o)  { return oCar(cCdr(cCdr(cCdr(cCdr(cCdr(cCdr(cCdr(cCdr(o)))))))));};
inline T_sp oTenth(T_sp o)  { return oCar(cCdr(cCdr(cCdr(cCdr(cCdr(cCdr(cCdr(cCdr(cCdr(o))))))))));};






#if 0
// Constructor function

/*! SourceCodeCons is a Cons that results from parsing code
 * It keeps track of the file, line number and column where
 * this Cons was parsed from for error reporting.
 */
    SMART(SourceCodeCons);
    c l a s s SourceCodeCons_O : public Cons_O
    {

	L I S P _BASE1(Cons_O);
	L I S P _CLASS(core,CorePkg,SourceCodeCons_O,"SourceCodeCons");
#if defined(OLD_SERIALIZE)
	DECLARE_SERIALIZE();
#endif // defined(OLD_SERIALIZE)
    public:
	void initialize();
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    private:
	SourceFileInfo_sp 	_SourceFileInfo;
	int		_ParsePosLineNumber;
	int		_ParsePosColumn;
    public:
	static SourceCodeCons_sp createList(T_sp o1, T_sp o2, uint ln, uint col, SourceFileInfo_sp fileName);
	static SourceCodeCons_sp createList(T_sp o1, T_sp o2, T_sp o3, uint ln, uint col, SourceFileInfo_sp fileName);
	static SourceCodeCons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, uint ln, uint col, SourceFileInfo_sp fileName);
	static SourceCodeCons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, uint ln, uint col, SourceFileInfo_sp fileName );
	static SourceCodeCons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, uint ln, uint col, SourceFileInfo_sp fileName );
	static SourceCodeCons_sp create(T_sp car, T_sp cdr,int lineNumber,int column,SourceFileInfo_sp fileName, Lisp_sp e );
	static SourceCodeCons_sp create(int lineNumber,int column,SourceFileInfo_sp fileName, Lisp_sp e );

	static SourceCodeCons_sp createList(T_sp o1, T_sp o2, const LispParserPos& pos, SourceFileInfo_sp fileName);
	static SourceCodeCons_sp createList(T_sp o1, T_sp o2, T_sp o3, const LispParserPos& pos, SourceFileInfo_sp fileName);
	static SourceCodeCons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, const LispParserPos& pos, SourceFileInfo_sp filename );
	static SourceCodeCons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, const LispParserPos& pos, SourceFileInfo_sp fileName );
	static SourceCodeCons_sp createList(T_sp o1, T_sp o2, T_sp o3, T_sp o4, T_sp o5, T_sp o6, const LispParserPos& pos, SourceFileInfo_sp fileName );
	static SourceCodeCons_sp create(T_sp car, Cons_sp cdr, const LispParserPos& pos, SourceFileInfo_sp fileName, Lisp_sp lisp );

	static SourceCodeCons_sp create(T_sp car, Lisp_sp lisp);

	static SourceCodeCons_sp createWithDuplicateSourceCodeInfo(T_sp car, Cons_sp cdr, Cons_sp parsed, Lisp_sp e);
	static SourceCodeCons_sp createWithDuplicateSourceCodeInfo(T_sp car, Cons_sp parsed, Lisp_sp e);
	static SourceCodeCons_sp createWithDuplicateSourceCodeInfo(Cons_sp parsed, Lisp_sp lisp);
    public:
//	virtual bool equal(T_sp obj) const;


	virtual bool hasParsePos() const { return this->_ParsePosLineNumber!=0;};
	virtual void getParsePos(int& lineNumber, int& column) const
	{
	    lineNumber = this->_ParsePosLineNumber;
	    column = this->_ParsePosColumn;
	}

	virtual SourceFileInfo_sp sourceFileInfo() const;

	/*! Depth first search to find a Cons with ParsePos information */
	virtual Cons_sp walkToFindParsePos() const;

	void duplicateSourceCodeInfo(Cons_sp other);

	int getParsePosLineNumber() const { return this->_ParsePosLineNumber; };
	int getParsePosColumn() const { return this->_ParsePosColumn;};
	string __repr__() const;

	/*! Preserve source info of each cons node */
	virtual Cons_sp copyList() const;


	/*! Like Common Lisp copy-list */
	virtual Cons_sp copyListCar() const;

	/*! Return a new Cons with a tree copy of the current car*/
	virtual Cons_sp copyTreeCar() const;



    public:
	explicit SourceCodeCons_O();
	virtual ~SourceCodeCons_O();

    };
#endif

};


TRANSLATE(core::Cons_O);
//TRANSLATE(core::SourceCodeCons_O);



namespace core
{
    /*! Create a Cons or a SourceCodeCons that that copies the source code location of locCons */
    Cons_sp Cons_create(T_sp car,Cons_sp locCons);

    /*! Create a Cons or a SourceCodeCons that that copies the source code location of locCons */
    Cons_sp Cons_create(T_sp car, T_sp cdr, Cons_sp locCons);

    Cons_sp Cons_create_loc(T_sp car, const char* fileName, int line);

    Cons_sp Cons_create_loc(T_sp car, T_sp cdr, const char* fileName, int line);

#if 0
/*! Create SourceCodeCons with location information tied to where the macro is defined */
#define HERE_scCONS_CREATE(car) SourceCodeCons_O::create(car,_Nil<Cons_O>(),__LINE__,0,SourceFileInfo_O::getOrCreate(__FILE__),_lisp)
#define HERE_scCONS_CREATE2(car,cdr) SourceCodeCons_O::create(car,cdr,__LINE__,0,SourceFileInfo_O::getOrCreate(__FILE__),_lisp)
#define HERE_scCONS_CREATE_LIST2(c1,c2) SourceCodeCons_O::createList(c1,c2,__LINE__,0,SourceFileInfo_O::getOrCreate(__FILE__))
#define HERE_scCONS_CREATE_LIST3(c1,c2,c3) SourceCodeCons_O::createList(c1,c2,c3,__LINE__,0,SourceFileInfo_O::getOrCreate(__FILE__))
#define HERE_scCONS_CREATE_LIST4(c1,c2,c3,c4) SourceCodeCons_O::createList(c1,c2,c3,c4,__LINE__,0,SourceFileInfo_O::getOrCreate(__FILE__))
#else

#define HERE_scCONS_CREATE(car) Cons_O::create(car,_Nil<Cons_O>())
#define HERE_scCONS_CREATE2(car,cdr) Cons_O::create(car,cdr)
#define HERE_scCONS_CREATE_LIST2(c1,c2) Cons_O::createList(c1,c2)
#define HERE_scCONS_CREATE_LIST3(c1,c2,c3) Cons_O::createList(c1,c2,c3)
#define HERE_scCONS_CREATE_LIST4(c1,c2,c3,c4) Cons_O::createList(c1,c2,c3,c4)

#endif
};


namespace core
{
    /*! Store the compiled body of a function referenced by a Functoid* */
    class CompiledBody_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,CorePkg,CompiledBody_O,"compiled-body");
    public: // ctor/dtor for classes with shared virtual base
	explicit CompiledBody_O();
	virtual ~CompiledBody_O();
    public:
	static CompiledBody_sp create(Functoid* functoid, T_sp compiledFunctions);
    public:
	void initialize();
    private: // instance variables here
	Functoid*	_Functoid;
	/*! When a Lisp function is compiled, more than one LLVM function may be generated because of internal lambda etc.
	  All of these functions are gathered together and stored within _CompiledFuncs for debugging */
	T_sp 		_CompiledFuncs;
    public: // Functions here
	Functoid*	functoid() const { return this->_Functoid;};
	T_sp compiledFuncs() const;
	void setCompiledFuncs(T_sp compiledFuncs);
	string __repr__() const;



    }; // CompiledBody class


    template <typename T>
    Cons_sp asCons(const gctools::Vec0<T>& vec) {
        Cons_sp res(_Nil<Cons_O>());
        for ( int i(vec.size()-1); i>=0; --i ) {
            res = Cons_O::create(vec[i],res);
        }
        return res;
    }


    template <typename T>
    void fillVec0FromCons(gctools::Vec0<T>& vec, Cons_sp list)
    {
        vec.clear();
        for ( Cons_sp cur=list; cur.notnilp(); cur=cCdr(cur) ) {
            vec.push_back(oCar(cur));
        }
    }
    
}; // core namespace
TRANSLATE(core::CompiledBody_O);




#endif //]


