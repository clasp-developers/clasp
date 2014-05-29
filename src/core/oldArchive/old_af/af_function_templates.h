#ifndef af_functionptrt_H
#define af_functionptrt_H



// comment: general_case returns(RT) arity(10)
template < typename RT ,typename P1=void,typename P2=void,typename P3=void,typename P4=void,typename P5=void,typename P6=void,typename P7=void,typename P8=void,typename P9=void,typename P10=void>
class ActivationFrameFunctionPtrT  {
   public:
      enum { NumParams = 10 };
      typedef RT (*Type)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 10 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (10) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(4+0));
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(5+0));
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(6+0));
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(7+0));
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(8+0));
        typename translate::from_object<P10 >::DeclareType a10 = translate::from_object<P10>::convert(af->element(9+0));
        RT retval = fn(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
        return translate::to_object<RT>::convert(retval);
   }
};



// comment: partially_specialized returns(RT) arity(9)
template < typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
class ActivationFrameFunctionPtrT <RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,void> {
   public:
      enum { NumParams = 9 };
      typedef RT (*Type)(P1,P2,P3,P4,P5,P6,P7,P8,P9)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 9 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (9) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(4+0));
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(5+0));
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(6+0));
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(7+0));
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(8+0));
        RT retval = fn(a1,a2,a3,a4,a5,a6,a7,a8,a9);
        return translate::to_object<RT>::convert(retval);
   }
};



// comment: partially_specialized returns(RT) arity(8)
template < typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
class ActivationFrameFunctionPtrT <RT,P1,P2,P3,P4,P5,P6,P7,P8,void,void> {
   public:
      enum { NumParams = 8 };
      typedef RT (*Type)(P1,P2,P3,P4,P5,P6,P7,P8)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 8 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (8) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(4+0));
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(5+0));
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(6+0));
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(7+0));
        RT retval = fn(a1,a2,a3,a4,a5,a6,a7,a8);
        return translate::to_object<RT>::convert(retval);
   }
};



// comment: partially_specialized returns(RT) arity(7)
template < typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
class ActivationFrameFunctionPtrT <RT,P1,P2,P3,P4,P5,P6,P7,void,void,void> {
   public:
      enum { NumParams = 7 };
      typedef RT (*Type)(P1,P2,P3,P4,P5,P6,P7)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 7 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (7) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(4+0));
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(5+0));
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(6+0));
        RT retval = fn(a1,a2,a3,a4,a5,a6,a7);
        return translate::to_object<RT>::convert(retval);
   }
};



// comment: partially_specialized returns(RT) arity(6)
template < typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
class ActivationFrameFunctionPtrT <RT,P1,P2,P3,P4,P5,P6,void,void,void,void> {
   public:
      enum { NumParams = 6 };
      typedef RT (*Type)(P1,P2,P3,P4,P5,P6)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 6 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (6) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(4+0));
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(5+0));
        RT retval = fn(a1,a2,a3,a4,a5,a6);
        return translate::to_object<RT>::convert(retval);
   }
};



// comment: partially_specialized returns(RT) arity(5)
template < typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5>
class ActivationFrameFunctionPtrT <RT,P1,P2,P3,P4,P5,void,void,void,void,void> {
   public:
      enum { NumParams = 5 };
      typedef RT (*Type)(P1,P2,P3,P4,P5)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 5 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (5) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(4+0));
        RT retval = fn(a1,a2,a3,a4,a5);
        return translate::to_object<RT>::convert(retval);
   }
};



// comment: partially_specialized returns(RT) arity(4)
template < typename RT ,typename P1,typename P2,typename P3,typename P4>
class ActivationFrameFunctionPtrT <RT,P1,P2,P3,P4,void,void,void,void,void,void> {
   public:
      enum { NumParams = 4 };
      typedef RT (*Type)(P1,P2,P3,P4)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 4 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (4) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        RT retval = fn(a1,a2,a3,a4);
        return translate::to_object<RT>::convert(retval);
   }
};



// comment: partially_specialized returns(RT) arity(3)
template < typename RT ,typename P1,typename P2,typename P3>
class ActivationFrameFunctionPtrT <RT,P1,P2,P3,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 3 };
      typedef RT (*Type)(P1,P2,P3)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 3 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (3) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        RT retval = fn(a1,a2,a3);
        return translate::to_object<RT>::convert(retval);
   }
};



// comment: partially_specialized returns(RT) arity(2)
template < typename RT ,typename P1,typename P2>
class ActivationFrameFunctionPtrT <RT,P1,P2,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 2 };
      typedef RT (*Type)(P1,P2)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 2 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (2) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        RT retval = fn(a1,a2);
        return translate::to_object<RT>::convert(retval);
   }
};



// comment: partially_specialized returns(RT) arity(1)
template < typename RT ,typename P1>
class ActivationFrameFunctionPtrT <RT,P1,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 1 };
      typedef RT (*Type)(P1)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 1 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (1) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        RT retval = fn(a1);
        return translate::to_object<RT>::convert(retval);
   }
};



// comment: partially_specialized returns(RT) arity(0)
template < typename RT >
class ActivationFrameFunctionPtrT <RT,void,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 0 };
      typedef RT (*Type)()  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 0 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (0) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        RT retval = fn();
        return translate::to_object<RT>::convert(retval);
   }
};



// comment: partially_specialized returns(void) arity(10)
template < typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9,typename P10>
class ActivationFrameFunctionPtrT <void,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10> {
   public:
      enum { NumParams = 10 };
      typedef void (*Type)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 10 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (10) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(4+0));
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(5+0));
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(6+0));
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(7+0));
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(8+0));
        typename translate::from_object<P10 >::DeclareType a10 = translate::from_object<P10>::convert(af->element(9+0));
        fn(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
        return T_O::_nil;
   }
};



// comment: partially_specialized returns(void) arity(9)
template < typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
class ActivationFrameFunctionPtrT <void,P1,P2,P3,P4,P5,P6,P7,P8,P9,void> {
   public:
      enum { NumParams = 9 };
      typedef void (*Type)(P1,P2,P3,P4,P5,P6,P7,P8,P9)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 9 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (9) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(4+0));
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(5+0));
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(6+0));
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(7+0));
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(8+0));
        fn(a1,a2,a3,a4,a5,a6,a7,a8,a9);
        return T_O::_nil;
   }
};



// comment: partially_specialized returns(void) arity(8)
template < typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
class ActivationFrameFunctionPtrT <void,P1,P2,P3,P4,P5,P6,P7,P8,void,void> {
   public:
      enum { NumParams = 8 };
      typedef void (*Type)(P1,P2,P3,P4,P5,P6,P7,P8)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 8 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (8) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(4+0));
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(5+0));
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(6+0));
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(7+0));
        fn(a1,a2,a3,a4,a5,a6,a7,a8);
        return T_O::_nil;
   }
};



// comment: partially_specialized returns(void) arity(7)
template < typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
class ActivationFrameFunctionPtrT <void,P1,P2,P3,P4,P5,P6,P7,void,void,void> {
   public:
      enum { NumParams = 7 };
      typedef void (*Type)(P1,P2,P3,P4,P5,P6,P7)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 7 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (7) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(4+0));
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(5+0));
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(6+0));
        fn(a1,a2,a3,a4,a5,a6,a7);
        return T_O::_nil;
   }
};



// comment: partially_specialized returns(void) arity(6)
template < typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
class ActivationFrameFunctionPtrT <void,P1,P2,P3,P4,P5,P6,void,void,void,void> {
   public:
      enum { NumParams = 6 };
      typedef void (*Type)(P1,P2,P3,P4,P5,P6)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 6 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (6) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(4+0));
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(5+0));
        fn(a1,a2,a3,a4,a5,a6);
        return T_O::_nil;
   }
};



// comment: partially_specialized returns(void) arity(5)
template < typename P1,typename P2,typename P3,typename P4,typename P5>
class ActivationFrameFunctionPtrT <void,P1,P2,P3,P4,P5,void,void,void,void,void> {
   public:
      enum { NumParams = 5 };
      typedef void (*Type)(P1,P2,P3,P4,P5)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 5 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (5) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(4+0));
        fn(a1,a2,a3,a4,a5);
        return T_O::_nil;
   }
};



// comment: partially_specialized returns(void) arity(4)
template < typename P1,typename P2,typename P3,typename P4>
class ActivationFrameFunctionPtrT <void,P1,P2,P3,P4,void,void,void,void,void,void> {
   public:
      enum { NumParams = 4 };
      typedef void (*Type)(P1,P2,P3,P4)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 4 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (4) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(3+0));
        fn(a1,a2,a3,a4);
        return T_O::_nil;
   }
};



// comment: partially_specialized returns(void) arity(3)
template < typename P1,typename P2,typename P3>
class ActivationFrameFunctionPtrT <void,P1,P2,P3,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 3 };
      typedef void (*Type)(P1,P2,P3)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 3 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (3) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(2+0));
        fn(a1,a2,a3);
        return T_O::_nil;
   }
};



// comment: partially_specialized returns(void) arity(2)
template < typename P1,typename P2>
class ActivationFrameFunctionPtrT <void,P1,P2,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 2 };
      typedef void (*Type)(P1,P2)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 2 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (2) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(1+0));
        fn(a1,a2);
        return T_O::_nil;
   }
};



// comment: partially_specialized returns(void) arity(1)
template < typename P1>
class ActivationFrameFunctionPtrT <void,P1,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 1 };
      typedef void (*Type)(P1)  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 1 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (1) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(0+0));
        fn(a1);
        return T_O::_nil;
   }
};



// comment: partially_specialized returns(void) arity(0)
template < >
class ActivationFrameFunctionPtrT <void,void,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 0 };
      typedef void (*Type)()  ;
      static T_sp activate( Type fn, const_ActivationFrame_spREF af)
    {_G();
	if ( af->length() != 0 )
	{
	    stringstream ss;
	    ss << "Function expected "<< (0) << " argument(s) but was passed " << af->length() << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        fn();
        return T_O::_nil;
   }
};



// Wrapper for ActivationFrameFunctionPtrT
template<typename RT,typename P1=void,typename P2=void,typename P3=void,typename P4=void,typename P5=void,typename P6=void,typename P7=void,typename P8=void,typename P9=void,typename P10=void >
class ActivationFrameFunctionWrapPtr : public Closure {
private:
    typedef typename ActivationFrameFunctionPtrT<RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>::Type FuncPtr;
    ActivationFrameFunctionPtrT<RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>		invoker;
    FuncPtr 				fptr;
public:
    string describe() const { return "ActivationFrameFunctionWrapPtr";}
    enum { NumParams = ActivationFrameFunctionPtrT<RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>::NumParams };
    typedef RT      ReturnT;
    typedef P1      Param1T;
    typedef P2      Param2T;
    typedef P3      Param3T;
    typedef P4      Param4T;
    typedef P5      Param5T;
    typedef P6      Param6T;
    typedef P7      Param7T;
    typedef P8      Param8T;
    typedef P9      Param9T;
    typedef P10      Param10T;
// constructor
    ActivationFrameFunctionWrapPtr(const string& name, FuncPtr ptr) : Closure(name), fptr(ptr) {}
    DISABLE_NEW();
    T_sp activate(const_ActivationFrame_spREF af)
    {_G();
        return this->invoker.activate(this->fptr,af);
    };
};



template <typename RT>
    void af_def(const string& packageName, const string& name, RT (*fp)() , const string& arguments="", const string& declares="", const string& docstring="", int locked=1 )
    {_G();
        Functoid* f = new ActivationFrameFunctionWrapPtr<RT>(packageName+"::"+name,fp);
        lisp_defun_lispify_name(_lisp,packageName,name,f,arguments,declares,docstring,locked,true,0);
    }



template <typename RT,typename P1>
    void af_def(const string& packageName, const string& name, RT (*fp)(P1) , const string& arguments="", const string& declares="", const string& docstring="", int locked=1 )
    {_G();
        Functoid* f = new ActivationFrameFunctionWrapPtr<RT,P1>(packageName+"::"+name,fp);
        lisp_defun_lispify_name(_lisp,packageName,name,f,arguments,declares,docstring,locked,true,1);
    }



template <typename RT,typename P1,typename P2>
    void af_def(const string& packageName, const string& name, RT (*fp)(P1,P2) , const string& arguments="", const string& declares="", const string& docstring="", int locked=1 )
    {_G();
        Functoid* f = new ActivationFrameFunctionWrapPtr<RT,P1,P2>(packageName+"::"+name,fp);
        lisp_defun_lispify_name(_lisp,packageName,name,f,arguments,declares,docstring,locked,true,2);
    }



template <typename RT,typename P1,typename P2,typename P3>
    void af_def(const string& packageName, const string& name, RT (*fp)(P1,P2,P3) , const string& arguments="", const string& declares="", const string& docstring="", int locked=1 )
    {_G();
        Functoid* f = new ActivationFrameFunctionWrapPtr<RT,P1,P2,P3>(packageName+"::"+name,fp);
        lisp_defun_lispify_name(_lisp,packageName,name,f,arguments,declares,docstring,locked,true,3);
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4>
    void af_def(const string& packageName, const string& name, RT (*fp)(P1,P2,P3,P4) , const string& arguments="", const string& declares="", const string& docstring="", int locked=1 )
    {_G();
        Functoid* f = new ActivationFrameFunctionWrapPtr<RT,P1,P2,P3,P4>(packageName+"::"+name,fp);
        lisp_defun_lispify_name(_lisp,packageName,name,f,arguments,declares,docstring,locked,true,4);
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5>
    void af_def(const string& packageName, const string& name, RT (*fp)(P1,P2,P3,P4,P5) , const string& arguments="", const string& declares="", const string& docstring="", int locked=1 )
    {_G();
        Functoid* f = new ActivationFrameFunctionWrapPtr<RT,P1,P2,P3,P4,P5>(packageName+"::"+name,fp);
        lisp_defun_lispify_name(_lisp,packageName,name,f,arguments,declares,docstring,locked,true,5);
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
    void af_def(const string& packageName, const string& name, RT (*fp)(P1,P2,P3,P4,P5,P6) , const string& arguments="", const string& declares="", const string& docstring="", int locked=1 )
    {_G();
        Functoid* f = new ActivationFrameFunctionWrapPtr<RT,P1,P2,P3,P4,P5,P6>(packageName+"::"+name,fp);
        lisp_defun_lispify_name(_lisp,packageName,name,f,arguments,declares,docstring,locked,true,6);
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
    void af_def(const string& packageName, const string& name, RT (*fp)(P1,P2,P3,P4,P5,P6,P7) , const string& arguments="", const string& declares="", const string& docstring="", int locked=1 )
    {_G();
        Functoid* f = new ActivationFrameFunctionWrapPtr<RT,P1,P2,P3,P4,P5,P6,P7>(packageName+"::"+name,fp);
        lisp_defun_lispify_name(_lisp,packageName,name,f,arguments,declares,docstring,locked,true,7);
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
    void af_def(const string& packageName, const string& name, RT (*fp)(P1,P2,P3,P4,P5,P6,P7,P8) , const string& arguments="", const string& declares="", const string& docstring="", int locked=1 )
    {_G();
        Functoid* f = new ActivationFrameFunctionWrapPtr<RT,P1,P2,P3,P4,P5,P6,P7,P8>(packageName+"::"+name,fp);
        lisp_defun_lispify_name(_lisp,packageName,name,f,arguments,declares,docstring,locked,true,8);
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
    void af_def(const string& packageName, const string& name, RT (*fp)(P1,P2,P3,P4,P5,P6,P7,P8,P9) , const string& arguments="", const string& declares="", const string& docstring="", int locked=1 )
    {_G();
        Functoid* f = new ActivationFrameFunctionWrapPtr<RT,P1,P2,P3,P4,P5,P6,P7,P8,P9>(packageName+"::"+name,fp);
        lisp_defun_lispify_name(_lisp,packageName,name,f,arguments,declares,docstring,locked,true,9);
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9,typename P10>
    void af_def(const string& packageName, const string& name, RT (*fp)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) , const string& arguments="", const string& declares="", const string& docstring="", int locked=1 )
    {_G();
        Functoid* f = new ActivationFrameFunctionWrapPtr<RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>(packageName+"::"+name,fp);
        lisp_defun_lispify_name(_lisp,packageName,name,f,arguments,declares,docstring,locked,true,10);
    }
#endif // af_functionptrt_H
