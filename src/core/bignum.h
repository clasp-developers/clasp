#ifndef _core_bignum_H_
#define _core_bignum_H_


#include "clasp_gmpxx.h"
#include "core/foundation.h"
#include "core/object.h"
#include "core/numbers.h"
#include "core/bignum.fwd.h"







namespace core
{
    class Bignum_O : public Integer_O
    {
	LISP_BASE1(Integer_O);
	LISP_CLASS(core,ClPkg,Bignum_O,"Bignum");
//	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(Bignum_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit Bignum_O(core::Class_sp const& mc) : T_O(mc), Integer(mc) {};
//    virtual ~Bignum_O() {};
    public:
//	void initialize();
	
    private: // instance variables here
	Bignum 	_value;
	
    public: // Functions here
	static Bignum_sp make(const string& value_in_string);
	static Bignum_sp create(int i)
	{_G();
	    GC_RESERVE_BEGIN(Bignum_O,b){
		GC_RESERVE_GET(Bignum_O,b);
	    } GC_RESERVE_END(Bignum_O,b);
            b->_value=i;
	    return b;
	};
	static Bignum_sp create(mpz_class v)
	{_G();
	    GC_RESERVE_BEGIN(Bignum_O,b){
		GC_RESERVE_GET(Bignum_O,b);
	    } GC_RESERVE_END(Bignum_O,b);
            b->_value=v;
	    return b;
	};
    public:
	NumberType number_type() const { return number_Bignum;};


	mpz_class& ref() { return this->_value;};

	string __repr__() const;

	/*! Return true if the number fits in a signed int */
	bool fits_sint_p();

	virtual void increment() {++this->_value;};
	virtual void decrement() {--this->_value;};
	virtual Number_sp copy() const;
	string description() const { stringstream ss; ss << this->_value;return ss.str();};
	void set(int val) { this->_value = val; };
	Bignum get() const;
	Bignum get_or_if_nil_default(Bignum default_value) const;
	Number_sp abs() const;
	void increment(int i) { this->_value += i; };
	int sign() const { return this->_value > 0 ? 1 : (this->_value < 0 ? -1 : 0 ); };


	virtual bool zerop() const {return((this->_value == 0));}
	virtual bool plusp() const {return((this->_value > 0));}
	virtual bool minusp() const {return((this->_value < 0));}

	virtual Number_sp oneMinus() const {
	    return Integer_O::create(this->_value-1);
	}
	virtual Number_sp onePlus() const {
	    return Integer_O::create(this->_value+1);
	}

	virtual int bit_length() const;

	/*! Return the value shifted by BITS bits.
	  If BITS < 0 shift right, if BITS >0 shift left. */
	Integer_sp shift(int bits) const;

	virtual	bool	eqn(T_sp obj) const;
	virtual	bool	eql(T_sp obj) const;


//	Fixnum_sp	asInt() { return this->sharedThis<Fixnum_O>();};
    public:
	virtual	string	valueAsString() const { stringstream ss; ss<<this->_value;return ss.str();};
	virtual	void	setFromString( const string& strVal );
	
	virtual int as_int() const;
	virtual uint64_t as_uint64() const;
	string as_uint64_string() const;
	virtual Bignum as_mpz() const;
	virtual LongLongInt as_LongLongInt() const;
	virtual float as_float() const;
	virtual double as_double() const;
	virtual LongFloat as_long_float() const;

	void sxhash(HashGenerator& hg) const;

	virtual bool evenp() const { return (mpz_get_ui(this->_value.get_mpz_t())&1)==0;};
	virtual bool oddp() const { return (mpz_get_ui(this->_value.get_mpz_t())&1)!=0;};
    


	Number_sp log1() const;

    }; // Bignum class
    
}; // core namespace
TRANSLATE(core::Bignum_O);





namespace translate
{
    template <>
    struct	from_object<const Bignum&,std::true_type>
    {
	typedef	Bignum DeclareType;
	DeclareType _v;
	from_object(T_P o)
	{_G();
	    if ( core::Bignum_sp bn = o.asOrNull<core::Bignum_O>() )
	    {
		_v = bn->ref();;
		return;
	    }
	    SIMPLE_ERROR(BF("Handle conversions of %s to Bignum") % _rep_(o));
	}
    };


};




namespace core
{


    Integer_mv big_ceiling(Bignum_sp a, Bignum_sp b);
    Integer_mv big_floor(Bignum_sp a, Bignum_sp b);

    inline Integer_sp _brcl_big_register_normalize(Bignum_sp x) {
	return Integer_O::create(x->get());
    }


    inline Integer_sp _brcl_big_floor(Bignum_sp a, Bignum_sp b, Real_sp* rP){
	Integer_mv res_mv = big_floor(a,b);
	*rP = res_mv.valueGet(1).as<Real_O>();
	return res_mv;
    };

    inline Integer_sp _brcl_big_ceiling(Bignum_sp a, Bignum_sp b, Real_sp* rP) {
	Integer_mv res_mv = big_ceiling(a,b);
	*rP = res_mv.valueGet(1).as<Real_O>();
	return res_mv;
    }

    inline double _brcl_big_to_double(Bignum_sp a) {
	return a->as_double();
    }


    Integer_sp _brcl_fix_divided_by_big(const Fixnum& x, const Bignum& y);
    Integer_sp _brcl_big_divided_by_fix(const Bignum& x, const Fixnum& y);
    Integer_sp _brcl_big_divided_by_big(const Bignum& x, const Bignum& y);

    Integer_sp _brcl_big_gcd(Bignum_sp x, Bignum_sp y);


#define BRCL_BIGNUM_SIZE(x) ((x)->_mp_size)
#define BRCL_BIGNUM_ABS_SIZE(x) \
	(BRCL_BIGNUM_SIZE(x)<0? -BRCL_BIGNUM_SIZE(x) : BRCL_BIGNUM_SIZE(x))

};










#endif /* _bignum_H_ */
