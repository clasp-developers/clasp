#ifndef	_core_numbers_H //[
#define	_core_numbers_H

#include "clasp_gmpxx.h"
#include <math.h>
#include <limits.h>
#include <boost/archive/tmpdir.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/utility.hpp>
#include <boost/serialization/list.hpp>
#include <boost/serialization/assume_abstract.hpp>



#include "foundation.h"
#include "object.h"
#include "numbers.fwd.h"
#include "numerics.h"


#define BRCL_PI_D 3.14159265358979323846264338327950288
#define BRCL_PI_L 3.14159265358979323846264338327950288l
#define BRCL_PI2_D 1.57079632679489661923132169163975144
#define BRCL_PI2_L 1.57079632679489661923132169163975144l


namespace core 
{


    typedef double LongFloat;


    typedef enum { number_Fixnum=0,
		   number_Bignum=1,
		   number_Ratio=2,
		   number_ShortFloat=3,
		   number_SingleFloat=4,
		   number_DoubleFloat=5,
		   number_LongFloat=6,
		   number_Complex=7,
		   number_NUM=8 } NumberType;


    Number_sp contagen_add(Number_sp na, Number_sp nb);
    Number_sp contagen_sub(Number_sp na, Number_sp nb);
    Number_sp contagen_mul(Number_sp na, Number_sp nb);
    Number_sp contagen_div(Number_sp na, Number_sp nb);
    int	      basic_compare(Number_sp na, Number_sp nb);




    SMART(Number);
    class Number_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,ClPkg,Number_O,"number");
    public:
	static Number_sp create(double val);
	static Number_sp create(int val);
	static Number_sp create(uint val);
//	static Number_sp create(size_t val);
    public:
	virtual NumberType number_type() const {SUBIMP();};
	int number_type_int() const { return (int)(this->number_type());};
	virtual	string	valueAsString() const;
	virtual Number_sp copy() const { _OF(); SUBCLASS_MUST_IMPLEMENT();}
	virtual T_sp deepCopy() const { return this->copy();};
	virtual T_sp shallowCopy() const { return this->copy();};
	virtual Number_sp signum() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual Number_sp reciprocal() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual Number_sp abs() const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual T_sp floor(Number_sp divisor) const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual T_sp ffloor(Number_sp divisor) const {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual bool equal(T_sp obj) const;


	virtual Number_sp log1() const { SUBIMP();};
	virtual Number_sp log1p() const;

	virtual Number_sp sqrt() const {SUBIMP();};

	/*! Add one to the number */
	virtual Number_sp onePlus() const {SUBIMP();};
	/*! Subtrace one from the number */
	virtual Number_sp oneMinus() const {SUBIMP();};

	// math routines shared by all numbers
	virtual bool zerop() const {SUBIMP();}
	virtual Number_sp negate() const { SUBIMP(); };


	virtual Number_sp exp() const { SUBIMP(); };


	virtual	bool operator<(T_sp obj) const;
	virtual	bool operator<=(T_sp obj) const;
	virtual	bool operator>(T_sp obj) const;
	virtual	bool operator>=(T_sp obj) const;

	virtual int as_int() const { SUBIMP();}
        virtual uint as_uint() const {SUBIMP();}
	virtual Bignum as_mpz() const { SUBIMP();}
	virtual LongLongInt as_LongLongInt() const {SUBIMP();};
	virtual float as_float() const {SUBIMP();};
	virtual double as_double() const {SUBIMP();}
	virtual LongFloat as_long_float() const {SUBIMP();};


	virtual Number_sp conjugate() const {SUBIMP();};
	virtual Number_sp sin() const {SUBIMP();};
	virtual Number_sp cos() const {SUBIMP();};
	virtual Number_sp tan() const {SUBIMP();};
	virtual Number_sp sinh() const {SUBIMP();};
	virtual Number_sp cosh() const {SUBIMP();};
	virtual Number_sp tanh() const {SUBIMP();};




	virtual void sxhash(HashGenerator& hg) const {SUBIMP();};
	DEFAULT_CTOR_DTOR(Number_O);
    };



    SMART(Real);
    class Real_O : public Number_O
    {
	LISP_BASE1(Number_O);
	LISP_CLASS(core,ClPkg,Real_O,"real");

    public:
	virtual double as_double() const {SUBIMP();};

	// functions shared by all Real
	virtual bool plusp() const {SUBIMP();};
	virtual bool minusp() const {SUBIMP();};

	virtual Number_sp conjugate() const;

	DEFAULT_CTOR_DTOR(Real_O);
    };


    SMART(Rational);
    class Rational_O : public Real_O
    {
	LISP_BASE1(Real_O);
	LISP_CLASS(core,ClPkg,Rational_O,"rational");

    public:
	static Rational_sp create(mpz_class const& num, mpz_class const& denom);
	static Rational_sp create(Integer_sp num, Integer_sp denom);
    public:

	int as_int() const {SUBIMP();};

	virtual Number_sp log1() const;
	virtual Number_sp log1p() const;

	virtual Number_sp sqrt() const;
	virtual Number_sp exp() const;

	virtual Number_sp sin() const;
	virtual Number_sp cos() const;
	virtual Number_sp tan() const;
	virtual Number_sp sinh() const;
	virtual Number_sp cosh() const;
	virtual Number_sp tanh() const;

	DEFAULT_CTOR_DTOR(Rational_O);
    };



    SMART(Integer);
    class Integer_O : public Rational_O
    {
	LISP_BASE1(Rational_O);
	LISP_CLASS(core,ClPkg,Integer_O,"integer");
    public:
	/*! Return a Cons (integer low high) */
	static T_sp makeIntegerType(int low, int high);
	static Integer_sp create(const mpz_class& v);
	static Integer_sp create(LongLongInt v);
	static Integer_sp create(int v);
	static Integer_sp create(const string& v) {mpz_class zv(v); return create(zv);};
	static Integer_sp create(const char* v) {mpz_class zv(v); return create(zv);};
	static Integer_sp create(uint v);
	static Integer_sp create(uint64_t v);
	static Integer_sp create(float f);
	static Integer_sp create(double f);
	static Integer_sp createLongFloat(LongFloat f);
    public:


	virtual bool evenp() const { SUBIMP(); };
	virtual bool oddp() const { SUBIMP(); };

	virtual int bit_length() const {SUBIMP();};

	/*! Return the value shifted by BITS bits.
	  If BITS < 0 shift right, if BITS >0 shift left. */
	virtual Integer_sp shift(int bits) const {SUBIMP();};

	virtual uint64_t as_uint64() const {SUBIMP();};
	virtual void __write__(Stream_sp strm) const;
	DEFAULT_CTOR_DTOR(Integer_O);
    };

};


template <> struct gctools::GCInfo<core::Fixnum_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = true;
};

namespace core {
    SMART(Fixnum);
    class Fixnum_O : public Integer_O
    {
	LISP_BASE1(Integer_O);
	LISP_CLASS(core,ClPkg,Fixnum_O,"fixnum");

    public:
	friend class boost::serialization::access;
#if 0
	template<class Archive>
	    void serialize(Archive &ar, const unsigned int version)
	{
	    ar & this->_Value;
	}
#endif
    public:
#if defined(OLD_SERIALIZE)
	void	serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    private:
	typedef	int	Fixnum_type;
	Fixnum_type	_Value;
    public:
	static Fixnum_sp create(int nm);
	static Fixnum_sp create(LongLongInt nm);
	static Fixnum_sp create(uint nm);
    public:
	static int number_of_bits();
    public:
	NumberType number_type() const { return number_Fixnum;};

	int& ref() { return this->_Value;};
	virtual Number_sp copy() const;
	string __repr__() const;
	void set(int val) { this->_Value = val; };
	int get() const { return this->_Value; };
	Number_sp abs() const { return Fixnum_O::create(::abs(this->_Value)); };
	Number_sp signum() const;


	// math routines shared by all numbers
	virtual bool zerop() const { return this->_Value == 0; };
	virtual Number_sp negate() const { return Fixnum_O::create(-this->_Value);};

	// Shared by real
	virtual bool plusp() const { return this->_Value > 0; };
	virtual bool minusp() const { return this->_Value < 0; };



	virtual bool evenp() const { return !(this->_Value&1); };
	virtual bool oddp() const { return (this->_Value&1);};
    
	virtual	bool	eqn(T_sp obj) const;
	virtual	bool	eql(T_sp obj) const;

	virtual Number_sp onePlus() const
	{
	    if ( this->_Value == MOST_POSITIVE_FIXNUM ) {
		Bignum bn(this->_Value);
		bn = bn + 1;
		return Integer_O::create(bn);
	    } else {
		return Fixnum_O::create(this->_Value+1);
	    }
	};

	virtual Number_sp oneMinus() const
	{
	    if ( this->_Value == MOST_NEGATIVE_FIXNUM ) {
		Bignum bn(this->_Value);
		bn = bn - 1;
		return Integer_O::create(bn);
	    } else {
		return Fixnum_O::create(this->_Value-1);
	    }
	};

    public:
	virtual	string	valueAsString() const { stringstream ss; ss<<this->_Value;return ss.str();};
	virtual	void	setFromString( const string& strVal ) { this->_Value = atoi(strVal.c_str());};

	int bit_length() const;
	/*! Return the value shifted by BITS bits.
	  If BITS < 0 shift right, if BITS >0 shift left. */
	Integer_sp shift(int bits) const;

	string asChar() const;
	virtual int as_int() const;
	virtual uint64_t as_uint64() const;
	virtual Bignum as_mpz() const;
	virtual LongLongInt as_LongLongInt() const;
	virtual float as_float() const;
	virtual double as_double() const;
	virtual LongFloat as_long_float() const;

	void sxhash(HashGenerator& hg) const;

	DEFAULT_CTOR_DTOR(Fixnum_O);
    };
};

namespace core {

    SMART(Float);
    class Float_O : public Real_O
    {
	LISP_BASE1(Real_O);
	LISP_CLASS(core,ClPkg,Float_O,"float");
    public:

	virtual Integer_sp castToInteger() const {SUBIMP();};

	DEFAULT_CTOR_DTOR(Float_O);
    };









    SMART(ShortFloat);
    class ShortFloat_O : public Float_O
    {
	LISP_BASE1(Float_O);
	LISP_CLASS(core,ClPkg,ShortFloat_O,"ShortFloat");
    public:
#if defined(OLD_SERIALIZE)
	void	serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    private:
	float	_Value;
    public:
	static ShortFloat_sp create(float nm)
	{_G();
	    GC_ALLOCATE(ShortFloat_O,sf);
            sf->_Value = nm;
	    return sf;
	};
    public:
	NumberType number_type() const { return number_ShortFloat;};
	float get() const {return this->_Value;};
	void sxhash(HashGenerator& hg) const;
	virtual Number_sp copy() const;
	Number_sp signum() const;
	string __repr__() const;
	Number_sp abs() const;
	bool isnan() const {return this->_Value != this->_Value;}; // NaN is supposed to be the only value that != itself!!!!
    public:
	virtual	string	valueAsString() const;
	virtual	void	setFromString( const string& strVal );
	virtual	bool	eqn(T_sp obj) const;
	virtual	bool	eql(T_sp obj) const;
	virtual Number_sp reciprocal() const;


	// math routines shared by all numbers
	virtual bool zerop() const { return this->_Value == 0.0; };
	virtual Number_sp negate() const { return ShortFloat_O::create(-this->_Value);};

	virtual Number_sp onePlus() const { return ShortFloat_O::create(this->_Value+1.0);};
	virtual Number_sp oneMinus() const { return ShortFloat_O::create(this->_Value-1.0);};

	// shared by real
	virtual bool plusp() const { return this->_Value > 0.0; };
	virtual bool minusp() const { return this->_Value < 0.0; };




	virtual float as_float() const;
	virtual double as_double() const;
	virtual LongFloat as_long_float() const;

	Integer_sp castToInteger() const;
    
	DEFAULT_CTOR_DTOR(ShortFloat_O);
    };





    SMART(SingleFloat);
    class SingleFloat_O : public Float_O
    {
	LISP_BASE1(Float_O);
	LISP_CLASS(core,ClPkg,SingleFloat_O,"SingleFloat");
    public:
#if defined(OLD_SERIALIZE)
	void	serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    private:
	float	_Value;
    public:
	static SingleFloat_sp create(float nm)
	{_G();
            GC_ALLOCATE(SingleFloat_O,sf);
            sf->_Value = nm;
	    return sf;
	};
    public:
	NumberType number_type() const { return number_SingleFloat;};
	void sxhash(HashGenerator& hg) const;
	float get() const { return this->_Value;};
	string __repr__() const;
	virtual Number_sp copy() const;
	Number_sp signum() const;
	Number_sp abs() const;
	bool isnan() const {return this->_Value != this->_Value;}; // NaN is supposed to be the only value that != itself!!!!
    public:
	virtual	string	valueAsString() const;
	virtual	void	setFromString( const string& strVal );
	virtual	bool	eqn(T_sp obj) const;
	virtual	bool	eql(T_sp obj) const;
	virtual Number_sp reciprocal() const;

	// math routines shared by all numbers
	virtual bool zerop() const { return this->_Value == 0.0; };
	virtual Number_sp negate() const { return SingleFloat_O::create(-this->_Value);};

	// shared by real
	virtual bool plusp() const { return this->_Value > 0.0; };
	virtual bool minusp() const { return this->_Value < 0.0; };

	virtual Number_sp log1() const;
	virtual Number_sp log1p() const;
	virtual Number_sp sqrt() const;

	virtual Number_sp onePlus() const { return create(this->_Value+1.0);};
	virtual Number_sp oneMinus() const { return create(this->_Value-1.0);};

	virtual Number_sp exp() const;

	virtual Number_sp sin() const;
	virtual Number_sp cos() const;
	virtual Number_sp tan() const;
	virtual Number_sp sinh() const;
	virtual Number_sp cosh() const;
	virtual Number_sp tanh() const;



	virtual float as_float() const;
	virtual double as_double() const;
	virtual LongFloat as_long_float() const;

	Integer_sp castToInteger() const;
    
	DEFAULT_CTOR_DTOR(SingleFloat_O);
    };









    SMART(DoubleFloat);
    class DoubleFloat_O : public Float_O
    {
	LISP_BASE1(Float_O);
	LISP_CLASS(core,ClPkg,DoubleFloat_O,"double-float");
    public:
#if defined(OLD_SERIALIZE)
	void	serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    private:
	double	_Value;
    public:
	static DoubleFloat_sp create(double nm)
	{_G();
            GC_ALLOCATE(DoubleFloat_O,v );
            v->set(nm);
	    return v;
	};
    public:
	NumberType number_type() const { return number_DoubleFloat;};
	void sxhash(HashGenerator& hg) const;
	virtual Number_sp copy() const;
	string __repr__() const;
	void set(double val) { this->_Value = val; };
	double get() const { return this->_Value; };
	Number_sp signum() const;
	Number_sp abs() const { return DoubleFloat_O::create(fabs(this->_Value));};
	bool isnan() const {return this->_Value != this->_Value;}; // NaN is supposed to be the only value that != itself!!!!
    public:
	virtual	string	valueAsString() const;
	virtual	void	setFromString( const string& strVal );
	virtual	bool	eqn(T_sp obj) const;
	virtual	bool	eql(T_sp obj) const;

	// math routines shared by all numbers
	bool zerop() const { return this->_Value == 0.0; };
	virtual Number_sp negate() const { return DoubleFloat_O::create(-this->_Value);};

	// Shared by real
	bool plusp() const { return this->_Value > 0.0; };
	bool minusp() const { return this->_Value < 0.0; };

	virtual Number_sp sqrt() const;

	virtual Number_sp onePlus() const { return create(this->_Value+1.0);};
	virtual Number_sp oneMinus() const { return create(this->_Value-1.0);};


	virtual Number_sp log1() const;
	virtual Number_sp log1p() const;

    
	virtual float as_float() const;
	virtual double as_double() const;
	virtual LongFloat as_long_float() const;

	Integer_sp castToInteger() const;

	virtual Number_sp exp() const;

	virtual Number_sp sin() const;
	virtual Number_sp cos() const;
	virtual Number_sp tan() const;
	virtual Number_sp sinh() const;
	virtual Number_sp cosh() const;
	virtual Number_sp tanh() const;

	DEFAULT_CTOR_DTOR(DoubleFloat_O);
    };






    SMART(LongFloat);
    class LongFloat_O : public Float_O
    {
	LISP_BASE1(Float_O);
	LISP_CLASS(core,ClPkg,LongFloat_O,"LongFloat");
    public:
#if defined(OLD_SERIALIZE)
	void	serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    private:
	LongFloat	_Value;
    public:
	static LongFloat_sp create(double nm)
	{_G();
            GC_ALLOCATE(LongFloat_O,v );
            v->_Value = nm;
	    return v;
	};
    public:
	NumberType number_type() const { return number_LongFloat;};
	void sxhash(HashGenerator& hg) const;
	double get() const { return this->_Value;};
	LongFloat& ref() { return this->_Value;};
	string __repr__() const;
	virtual Number_sp copy() const;
	Number_sp signum() const;
	Number_sp abs() const;
	bool isnan() const {return this->_Value != this->_Value;}; // NaN is supposed to be the only value that != itself!!!!
    public:
	virtual	string	valueAsString() const;
	virtual	void	setFromString( const string& strVal );
	virtual	bool	eqn(T_sp obj) const;
	virtual	bool	eql(T_sp obj) const;


	// math routines shared by all numbers
	bool zerop() const { return this->_Value == 0.0; };
	virtual Number_sp negate() const { return LongFloat_O::create(-this->_Value);};

	// shared by real
	bool plusp() const { return this->_Value > 0.0; };
	bool minusp() const { return this->_Value < 0.0; };

	virtual Number_sp reciprocal() const;

	virtual Number_sp sqrt() const;



	virtual Number_sp onePlus() const { return create(this->_Value+1.0);};
	virtual Number_sp oneMinus() const { return create(this->_Value-1.0);};



	virtual float as_float() const;
	virtual double as_double() const;
	virtual LongFloat as_long_float() const;

	Integer_sp castToInteger() const;

#ifdef BRCL_LONG_FLOAT
	virtual Number_sp log1() const;
	virtual Number_sp log1p() const;

	virtual Number_sp exp() const;
	virtual Number_sp sin() const;
	virtual Number_sp cos() const;
	virtual Number_sp tan() const;
	virtual Number_sp sinh() const;
	virtual Number_sp cosh() const;
	virtual Number_sp tanh() const;
#endif
    
	DEFAULT_CTOR_DTOR(LongFloat_O);
    };










    SMART(Complex);
    class Complex_O : public Number_O
    {
	LISP_BASE1(Number_O);
	LISP_CLASS(core,ClPkg,Complex_O,"complex");
    public:
#if defined(OLD_SERIALIZE)
	void	serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    private:
	Real_sp	_real;
	Real_sp	_imaginary;
    public:
	static Complex_sp create(double r, double i)
	{_G();
            GC_ALLOCATE(Complex_O,v );
	    v->_real = DoubleFloat_O::create(r);
	    v->_imaginary = DoubleFloat_O::create(i);
	    return v;
	};
	static Complex_sp create(Real_sp r, Real_sp i)
	{_G();
            GC_ALLOCATE(Complex_O,v);
            v->_real = r;
            v->_imaginary = i;
	    return v;
	}


    public:
	NumberType number_type() const { return number_Complex;};

	Real_sp real() const { return this->_real;};
	Real_sp imaginary() const { return this->_imaginary;};




	void sxhash(HashGenerator& hg) const;
	virtual Number_sp copy() const;
	string __repr__() const;
	Number_sp signum() const;
	Number_sp abs() const;
	bool isnan() const;
    public:
	virtual	string	valueAsString() const;
	virtual	void	setFromString( const string& str);
	virtual	bool	eqn(T_sp obj) const;
	virtual	bool	eql(T_sp obj) const;


	// math routines shared by all numbers
	bool zerop() const { return (this->_real->zerop() && this->_imaginary->zerop());};

	virtual Number_sp negate() const { return Complex_O::create(this->_real->negate().as<Real_O>(),
								    this->_imaginary->negate().as<Real_O>());};

	virtual Number_sp log1() const;
	virtual Number_sp log1p() const;

	virtual Number_sp onePlus() const { return create(this->_real->onePlus().as<Real_O>(),
							  this->_imaginary);};
	virtual Number_sp oneMinus() const { return create(this->_real->oneMinus().as<Real_O>(),
							   this->_imaginary);};

	Number_sp sqrt() const;


	virtual Number_sp exp() const;

	virtual Number_sp sin() const;
	virtual Number_sp cos() const;
	virtual Number_sp tan() const;
	virtual Number_sp sinh() const;
	virtual Number_sp cosh() const;
	virtual Number_sp tanh() const;

	virtual Number_sp conjugate() const;

	DEFAULT_CTOR_DTOR(Complex_O);
    };





    SMART(Ratio);
    class Ratio_O : public Rational_O
    {
	LISP_BASE1(Rational_O);
	LISP_CLASS(core,ClPkg,Ratio_O,"ratio");
    public:
#if defined(OLD_SERIALIZE)
	void	serialize(serialize::SNode node);
#endif
#if defined(XML_ARCHIVE)
	void	archiveBase(ArchiveP node);
#endif // defined(XML_ARCHIVE)
    private:
	Integer_sp 	_numerator;
	Integer_sp 	_denomenator;
    public:
	static Ratio_sp create(Integer_sp num, Integer_sp denom)
	{_G();
            GC_ALLOCATE(Ratio_O,v );
	    if ( denom->as_mpz() < 0 )
	    {
		v->_numerator = num->negate().as<Integer_O>();
		v->_denomenator = denom->negate().as<Integer_O>();
	    } else
	    {
		v->_numerator = num;
		v->_denomenator = denom;
	    }
	    return v;
	};
	static Ratio_sp create(mpz_class const& num, mpz_class const& denom)
	{_G();
            GC_ALLOCATE(Ratio_O,r);
	    r->_numerator = Integer_O::create(num);
	    r->_denomenator = Integer_O::create(denom);
	    return r;
	}
	static Ratio_sp create(const char* str)
	{_G();
            GC_ALLOCATE(Ratio_O,r);
	    r->setFromString(str);
	    return r;
	}

    public:
	NumberType number_type() const { return number_Ratio;};

	Integer_sp numerator() const { return this->_numerator;};
	Integer_sp denomenator() const { return this->_denomenator;};
	Integer_sp num() const { return this->_numerator;};
	Integer_sp den() const { return this->_denomenator;};
	mpz_class numerator_as_mpz() const;
	mpz_class denomenator_as_mpz() const;

	void sxhash(HashGenerator& hg) const;
	virtual Number_sp copy() const;
	string __repr__() const;
	Number_sp signum() const;
	Number_sp abs() const;
	bool isnan() const;
    public:
	virtual	string	valueAsString() const;
	virtual	void	setFromString( const string& str);
	virtual	bool	eqn(T_sp obj) const;
	virtual	bool	eql(T_sp obj) const;

	Number_sp onePlus() const { return create(contagen_add(this->_numerator,this->_denomenator).as<Integer_O>(),this->_denomenator);};
	Number_sp oneMinus() const { return create(contagen_sub(this->_numerator,this->_denomenator).as<Integer_O>(),this->_denomenator);};

	virtual float as_float() const;
	virtual double as_double() const;
	virtual LongFloat as_long_float() const;
    
	// functions shared by all Real

	bool plusp() const {
	    return this->_numerator->plusp();
	}

    bool minusp() const {
	return this->_numerator->minusp();
    }
    
	DEFAULT_CTOR_DTOR(Ratio_O);
    };







    void brcl_deliver_fpe(int status);

    inline Number_sp brcl_plus(Number_sp na, Number_sp nb)    { return contagen_add(na,nb);};
    inline Number_sp brcl_minus(Number_sp na, Number_sp nb)    { return contagen_sub(na,nb);};
    inline Number_sp brcl_times(Number_sp na, Number_sp nb)  { return contagen_mul(na,nb);};
    inline Number_sp brcl_divide(Number_sp na, Number_sp nb) { return contagen_div(na,nb);};

    inline int brcl_number_compare(Number_sp x, Number_sp y) { return basic_compare(x,y);};

    Number_sp brcl_atan2(Number_sp x, Number_sp y);

    inline Number_sp brcl_sqrt(Number_sp z) {
	return z->sqrt();
    }

    inline Number_sp brcl_log1(Number_sp x) {
	return x->log1();
    }

    inline Number_sp brcl_log1p(Number_sp x)
    {
	return x->log1p();
    };
       

    inline bool brcl_zerop(Number_sp n)
    {
	return n->zerop();
    }

    inline Number_sp brcl_negate(Number_sp n)
    {
	return n->negate();
    }

    inline Number_sp brcl_one_plus(Number_sp x)
    {
	return x->onePlus();
    }

    inline Number_sp brcl_one_minus(Number_sp x)
    {
	return x->oneMinus();
    }

    inline bool brcl_plusp(Real_sp n)
    {
	return n->plusp();
    }

    inline bool brcl_minusp(Real_sp n)
    {
	return n->minusp();
    }

    inline bool brcl_evenp(Integer_sp n)
    {
	return n->evenp();
    }

    inline bool brcl_oddp(Integer_sp n)
    {
	return n->oddp();
    };


    Number_sp cl_expt(Number_sp x, Number_sp y);




    inline int brcl_integer_length(Integer_sp x)
    {
	return x->bit_length();
    }

    Integer_sp brcl_ash(Integer_sp x, int bits);

    inline float brcl_to_float(Number_sp x) { return x->as_float(); };
    inline double brcl_to_double(Number_sp x) { return x->as_double(); };
    inline LongFloat brcl_to_long_float(Number_sp x) { return x->as_long_float(); };
    inline LongFloat brcl_to_long_double(Number_sp x) { return x->as_long_float(); };

    inline Fixnum brcl_fixnum(Number_sp x) {
	return x.as<Fixnum_O>()->get();
    }

    inline float brcl_single_float(Number_sp x) {
	return x.as<SingleFloat_O>()->get();
    }

    inline double brcl_double_float(Number_sp x) {
	return x.as<DoubleFloat_O>()->get();
    }

    inline LongFloat brcl_long_float(Number_sp x) {
	return x.as<LongFloat_O>()->get();
    }

    inline Ratio_sp brcl_make_ratio(Integer_sp num, Integer_sp denom)
    {
	return Ratio_O::create(num,denom);
    }

    inline Fixnum_sp brcl_make_fixnum(int i)
    {
	return Fixnum_O::create(i);
    }

    inline Integer_sp _brcl_float_to_integer(float d)
    {
	return Integer_O::create(d);
    }

    inline Integer_sp _brcl_double_to_integer(double d)
    {
	return Integer_O::create(d);
    }

    inline Integer_sp _brcl_long_float_to_integer(LongFloat d)
    {
	return Integer_O::create(d);
    }

    inline Integer_sp _brcl_long_double_to_integer(LongFloat d)
    {
	return Integer_O::create(d);
    }

    inline SingleFloat_sp brcl_make_single_float(float d)
    {
	return SingleFloat_O::create(d);
    }

    inline DoubleFloat_sp brcl_make_double_float(double d)
    {
	return DoubleFloat_O::create(d);
    }

#ifdef BRCL_LONG_FLOAT
    inline LongFloat_sp brcl_make_long_float(LongFloat d)
    {
	return LongFloat_O::create(d);
    }
#endif


#define brcl_return2(ENV,n1,n2) return Values(n1,n2);
#define BRCL_REAL_TYPE_P(y) (y.isA<Real_O>())
    inline NumberType brcl_t_of(Number_sp n) {
	return n->number_type();
    }

#define BRCL_FIXNUMP(n) (n.isA<Fixnum_O>())

    /*! In num_co.cc */
    Real_mv brcl_floor1(Real_sp x);
    Real_mv brcl_floor2(Real_sp x,Real_sp y);
    Real_mv brcl_ceiling1(Real_sp x);
    Real_mv brcl_ceiling2(Real_sp x,Real_sp y);

    Real_mv brcl_truncate1(Real_sp x);
    Real_mv brcl_truncate2(Real_sp x,Real_sp y);

    Real_mv brcl_round1(Real_sp x);
    Real_mv brcl_round2(Real_sp x,Real_sp y);

    Real_sp brcl_max2(Real_sp x, Real_sp y);
    Real_sp brcl_min2(Real_sp x, Real_sp y);

#define brcl_lowereq(x,y) (brcl_number_compare((x),(y)) <= 0)
#define brcl_greatereq(x,y) (brcl_number_compare((x),(y)) >= 0)
#define brcl_lower(x,y) (brcl_number_compare((x),(y)) < 0)
#define brcl_greater(x,y) (brcl_number_compare((x),(y)) > 0)





    /*! Initialize all math functions here */
    void initialize_numbers();


}; // namespace core


TRANSLATE(core::Number_O); 	// superclass T_O
TRANSLATE(core::Real_O);	// superclass Number_O
TRANSLATE(core::Rational_O);	// superclass Real_O
TRANSLATE(core::Integer_O);	// superclass Rational_O
TRANSLATE(core::Fixnum_O);	// superclass Integer_O
#if 0
TRANSLATE(core::SignedByte_O);	// superclass Integer_O
TRANSLATE(core::UnsignedByte_O);// superclass SignedByte_O
TRANSLATE(core::Bit_O);		// superclass UnsignedByte_O
#endif
TRANSLATE(core::Float_O);	// superclass Real_O
TRANSLATE(core::ShortFloat_O);
TRANSLATE(core::SingleFloat_O);
TRANSLATE(core::DoubleFloat_O);// superclass DoubleFloat_O
TRANSLATE(core::LongFloat_O);
TRANSLATE(core::Ratio_O);	// superclass Rational_O
TRANSLATE(core::Complex_O);	// superclass Number_O






#endif //]
