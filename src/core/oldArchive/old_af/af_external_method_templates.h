/*
    File: af_external_method_templates.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
#ifdef af_external_methodptrt_H



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: general_case returns(RT) arity(10)
template < typename OT, typename RT ,typename P1=void,typename P2=void,typename P3=void,typename P4=void,typename P5=void,typename P6=void,typename P7=void,typename P8=void,typename P9=void,typename P10=void>
class ActivationFrameExternalObjectMethodPtrT  {
   public:
      enum { NumParams = 10 };
      typedef RT (OT::*Type)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 11 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 10 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P10 >::DeclareType a10 = translate::from_object<P10>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(10)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9,typename P10>
class ActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10> {
   public:
      enum { NumParams = 10 };
      typedef void (OT::*Type)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 11 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 10 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P10 >::DeclareType a10 = translate::from_object<P10>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: general_case returns(RT) arity(10)
template < typename OT, typename RT ,typename P1=void,typename P2=void,typename P3=void,typename P4=void,typename P5=void,typename P6=void,typename P7=void,typename P8=void,typename P9=void,typename P10=void>
class constActivationFrameExternalObjectMethodPtrT  {
   public:
      enum { NumParams = 10 };
      typedef RT (OT::*constType)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 11 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 10 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P10 >::DeclareType a10 = translate::from_object<P10>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(10)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9,typename P10>
class constActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10> {
   public:
      enum { NumParams = 10 };
      typedef void (OT::*constType)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 11 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 10 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P10 >::DeclareType a10 = translate::from_object<P10>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: general_case returns(RT) arity(10)
template < typename OT, typename RT ,typename P1=void,typename P2=void,typename P3=void,typename P4=void,typename P5=void,typename P6=void,typename P7=void,typename P8=void,typename P9=void,typename P10=void>
class ActivationFrameExternalIndirectMethodPtrT  {
   public:
      enum { NumParams = 10 };
      typedef RT (OT::WrappedClass::*Type)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 11 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 10 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P10 >::DeclareType a10 = translate::from_object<P10>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(10)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9,typename P10>
class ActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10> {
   public:
      enum { NumParams = 10 };
      typedef void (OT::WrappedClass::*Type)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 11 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 10 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P10 >::DeclareType a10 = translate::from_object<P10>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: general_case returns(RT) arity(10)
template < typename OT, typename RT ,typename P1=void,typename P2=void,typename P3=void,typename P4=void,typename P5=void,typename P6=void,typename P7=void,typename P8=void,typename P9=void,typename P10=void>
class constActivationFrameExternalIndirectMethodPtrT  {
   public:
      enum { NumParams = 10 };
      typedef RT (OT::WrappedClass::*constType)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 11 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 10 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P10 >::DeclareType a10 = translate::from_object<P10>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(10)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9,typename P10>
class constActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10> {
   public:
      enum { NumParams = 10 };
      typedef void (OT::WrappedClass::*constType)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 11 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 10 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P10 >::DeclareType a10 = translate::from_object<P10>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(9)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
class ActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,void> {
   public:
      enum { NumParams = 9 };
      typedef RT (OT::*Type)(P1,P2,P3,P4,P5,P6,P7,P8,P9)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 10 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 9 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(9)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
class ActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,P8,P9,void> {
   public:
      enum { NumParams = 9 };
      typedef void (OT::*Type)(P1,P2,P3,P4,P5,P6,P7,P8,P9)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 10 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 9 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(9)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
class constActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,void> {
   public:
      enum { NumParams = 9 };
      typedef RT (OT::*constType)(P1,P2,P3,P4,P5,P6,P7,P8,P9) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 10 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 9 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(9)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
class constActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,P8,P9,void> {
   public:
      enum { NumParams = 9 };
      typedef void (OT::*constType)(P1,P2,P3,P4,P5,P6,P7,P8,P9) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 10 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 9 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(9)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
class ActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,void> {
   public:
      enum { NumParams = 9 };
      typedef RT (OT::WrappedClass::*Type)(P1,P2,P3,P4,P5,P6,P7,P8,P9)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 10 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 9 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(9)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
class ActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,P8,P9,void> {
   public:
      enum { NumParams = 9 };
      typedef void (OT::WrappedClass::*Type)(P1,P2,P3,P4,P5,P6,P7,P8,P9)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 10 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 9 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(9)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
class constActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,void> {
   public:
      enum { NumParams = 9 };
      typedef RT (OT::WrappedClass::*constType)(P1,P2,P3,P4,P5,P6,P7,P8,P9) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 10 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 9 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(9)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
class constActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,P8,P9,void> {
   public:
      enum { NumParams = 9 };
      typedef void (OT::WrappedClass::*constType)(P1,P2,P3,P4,P5,P6,P7,P8,P9) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 10 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 9 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P9 >::DeclareType a9 = translate::from_object<P9>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8,a9);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(8)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
class ActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,void,void> {
   public:
      enum { NumParams = 8 };
      typedef RT (OT::*Type)(P1,P2,P3,P4,P5,P6,P7,P8)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 9 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 8 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(8)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
class ActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,P8,void,void> {
   public:
      enum { NumParams = 8 };
      typedef void (OT::*Type)(P1,P2,P3,P4,P5,P6,P7,P8)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 9 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 8 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(8)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
class constActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,void,void> {
   public:
      enum { NumParams = 8 };
      typedef RT (OT::*constType)(P1,P2,P3,P4,P5,P6,P7,P8) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 9 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 8 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(8)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
class constActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,P8,void,void> {
   public:
      enum { NumParams = 8 };
      typedef void (OT::*constType)(P1,P2,P3,P4,P5,P6,P7,P8) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 9 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 8 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(8)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
class ActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,void,void> {
   public:
      enum { NumParams = 8 };
      typedef RT (OT::WrappedClass::*Type)(P1,P2,P3,P4,P5,P6,P7,P8)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 9 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 8 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(8)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
class ActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,P8,void,void> {
   public:
      enum { NumParams = 8 };
      typedef void (OT::WrappedClass::*Type)(P1,P2,P3,P4,P5,P6,P7,P8)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 9 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 8 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(8)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
class constActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,void,void> {
   public:
      enum { NumParams = 8 };
      typedef RT (OT::WrappedClass::*constType)(P1,P2,P3,P4,P5,P6,P7,P8) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 9 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 8 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(8)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
class constActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,P8,void,void> {
   public:
      enum { NumParams = 8 };
      typedef void (OT::WrappedClass::*constType)(P1,P2,P3,P4,P5,P6,P7,P8) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 9 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 8 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P8 >::DeclareType a8 = translate::from_object<P8>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7,a8);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(7)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
class ActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,P7,void,void,void> {
   public:
      enum { NumParams = 7 };
      typedef RT (OT::*Type)(P1,P2,P3,P4,P5,P6,P7)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 8 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 7 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(7)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
class ActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,void,void,void> {
   public:
      enum { NumParams = 7 };
      typedef void (OT::*Type)(P1,P2,P3,P4,P5,P6,P7)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 8 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 7 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(7)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
class constActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,P7,void,void,void> {
   public:
      enum { NumParams = 7 };
      typedef RT (OT::*constType)(P1,P2,P3,P4,P5,P6,P7) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 8 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 7 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(7)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
class constActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,void,void,void> {
   public:
      enum { NumParams = 7 };
      typedef void (OT::*constType)(P1,P2,P3,P4,P5,P6,P7) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 8 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 7 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(7)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
class ActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,P7,void,void,void> {
   public:
      enum { NumParams = 7 };
      typedef RT (OT::WrappedClass::*Type)(P1,P2,P3,P4,P5,P6,P7)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 8 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 7 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(7)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
class ActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,void,void,void> {
   public:
      enum { NumParams = 7 };
      typedef void (OT::WrappedClass::*Type)(P1,P2,P3,P4,P5,P6,P7)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 8 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 7 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(7)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
class constActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,P7,void,void,void> {
   public:
      enum { NumParams = 7 };
      typedef RT (OT::WrappedClass::*constType)(P1,P2,P3,P4,P5,P6,P7) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 8 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 7 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6,a7);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(7)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
class constActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,P7,void,void,void> {
   public:
      enum { NumParams = 7 };
      typedef void (OT::WrappedClass::*constType)(P1,P2,P3,P4,P5,P6,P7) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 8 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 7 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P7 >::DeclareType a7 = translate::from_object<P7>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6,a7);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(6)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
class ActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,void,void,void,void> {
   public:
      enum { NumParams = 6 };
      typedef RT (OT::*Type)(P1,P2,P3,P4,P5,P6)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 7 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 6 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(6)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
class ActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,void,void,void,void> {
   public:
      enum { NumParams = 6 };
      typedef void (OT::*Type)(P1,P2,P3,P4,P5,P6)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 7 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 6 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(6)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
class constActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,void,void,void,void> {
   public:
      enum { NumParams = 6 };
      typedef RT (OT::*constType)(P1,P2,P3,P4,P5,P6) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 7 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 6 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(6)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
class constActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,void,void,void,void> {
   public:
      enum { NumParams = 6 };
      typedef void (OT::*constType)(P1,P2,P3,P4,P5,P6) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 7 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 6 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(6)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
class ActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,void,void,void,void> {
   public:
      enum { NumParams = 6 };
      typedef RT (OT::WrappedClass::*Type)(P1,P2,P3,P4,P5,P6)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 7 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 6 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(6)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
class ActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,void,void,void,void> {
   public:
      enum { NumParams = 6 };
      typedef void (OT::WrappedClass::*Type)(P1,P2,P3,P4,P5,P6)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 7 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 6 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(6)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
class constActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,P6,void,void,void,void> {
   public:
      enum { NumParams = 6 };
      typedef RT (OT::WrappedClass::*constType)(P1,P2,P3,P4,P5,P6) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 7 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 6 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5,a6);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(6)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
class constActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,P5,P6,void,void,void,void> {
   public:
      enum { NumParams = 6 };
      typedef void (OT::WrappedClass::*constType)(P1,P2,P3,P4,P5,P6) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 7 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 6 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P6 >::DeclareType a6 = translate::from_object<P6>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5,a6);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(5)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5>
class ActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,void,void,void,void,void> {
   public:
      enum { NumParams = 5 };
      typedef RT (OT::*Type)(P1,P2,P3,P4,P5)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 6 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 5 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(5)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5>
class ActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,P5,void,void,void,void,void> {
   public:
      enum { NumParams = 5 };
      typedef void (OT::*Type)(P1,P2,P3,P4,P5)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 6 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 5 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(5)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5>
class constActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,void,void,void,void,void> {
   public:
      enum { NumParams = 5 };
      typedef RT (OT::*constType)(P1,P2,P3,P4,P5) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 6 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 5 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(5)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5>
class constActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,P5,void,void,void,void,void> {
   public:
      enum { NumParams = 5 };
      typedef void (OT::*constType)(P1,P2,P3,P4,P5) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 6 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 5 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(5)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5>
class ActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,void,void,void,void,void> {
   public:
      enum { NumParams = 5 };
      typedef RT (OT::WrappedClass::*Type)(P1,P2,P3,P4,P5)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 6 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 5 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(5)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5>
class ActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,P5,void,void,void,void,void> {
   public:
      enum { NumParams = 5 };
      typedef void (OT::WrappedClass::*Type)(P1,P2,P3,P4,P5)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 6 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 5 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(5)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4,typename P5>
class constActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,P4,P5,void,void,void,void,void> {
   public:
      enum { NumParams = 5 };
      typedef RT (OT::WrappedClass::*constType)(P1,P2,P3,P4,P5) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 6 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 5 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4,a5);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(5)
template < typename OT,typename P1,typename P2,typename P3,typename P4,typename P5>
class constActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,P5,void,void,void,void,void> {
   public:
      enum { NumParams = 5 };
      typedef void (OT::WrappedClass::*constType)(P1,P2,P3,P4,P5) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 6 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 5 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P5 >::DeclareType a5 = translate::from_object<P5>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4,a5);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(4)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4>
class ActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,P4,void,void,void,void,void,void> {
   public:
      enum { NumParams = 4 };
      typedef RT (OT::*Type)(P1,P2,P3,P4)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 5 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 4 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(4)
template < typename OT,typename P1,typename P2,typename P3,typename P4>
class ActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,void,void,void,void,void,void> {
   public:
      enum { NumParams = 4 };
      typedef void (OT::*Type)(P1,P2,P3,P4)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 5 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 4 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(4)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4>
class constActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,P4,void,void,void,void,void,void> {
   public:
      enum { NumParams = 4 };
      typedef RT (OT::*constType)(P1,P2,P3,P4) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 5 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 4 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(4)
template < typename OT,typename P1,typename P2,typename P3,typename P4>
class constActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,P4,void,void,void,void,void,void> {
   public:
      enum { NumParams = 4 };
      typedef void (OT::*constType)(P1,P2,P3,P4) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 5 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 4 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(4)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4>
class ActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,P4,void,void,void,void,void,void> {
   public:
      enum { NumParams = 4 };
      typedef RT (OT::WrappedClass::*Type)(P1,P2,P3,P4)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 5 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 4 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(4)
template < typename OT,typename P1,typename P2,typename P3,typename P4>
class ActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,void,void,void,void,void,void> {
   public:
      enum { NumParams = 4 };
      typedef void (OT::WrappedClass::*Type)(P1,P2,P3,P4)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 5 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 4 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(4)
template < typename OT, typename RT ,typename P1,typename P2,typename P3,typename P4>
class constActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,P4,void,void,void,void,void,void> {
   public:
      enum { NumParams = 4 };
      typedef RT (OT::WrappedClass::*constType)(P1,P2,P3,P4) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 5 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 4 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3,a4);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(4)
template < typename OT,typename P1,typename P2,typename P3,typename P4>
class constActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,P4,void,void,void,void,void,void> {
   public:
      enum { NumParams = 4 };
      typedef void (OT::WrappedClass::*constType)(P1,P2,P3,P4) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 5 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 4 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P4 >::DeclareType a4 = translate::from_object<P4>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3,a4);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(3)
template < typename OT, typename RT ,typename P1,typename P2,typename P3>
class ActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 3 };
      typedef RT (OT::*Type)(P1,P2,P3)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 4 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 3 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(3)
template < typename OT,typename P1,typename P2,typename P3>
class ActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 3 };
      typedef void (OT::*Type)(P1,P2,P3)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 4 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 3 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(3)
template < typename OT, typename RT ,typename P1,typename P2,typename P3>
class constActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,P3,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 3 };
      typedef RT (OT::*constType)(P1,P2,P3) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 4 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 3 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(3)
template < typename OT,typename P1,typename P2,typename P3>
class constActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,P3,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 3 };
      typedef void (OT::*constType)(P1,P2,P3) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 4 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 3 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(3)
template < typename OT, typename RT ,typename P1,typename P2,typename P3>
class ActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 3 };
      typedef RT (OT::WrappedClass::*Type)(P1,P2,P3)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 4 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 3 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(3)
template < typename OT,typename P1,typename P2,typename P3>
class ActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 3 };
      typedef void (OT::WrappedClass::*Type)(P1,P2,P3)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 4 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 3 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(3)
template < typename OT, typename RT ,typename P1,typename P2,typename P3>
class constActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,P3,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 3 };
      typedef RT (OT::WrappedClass::*constType)(P1,P2,P3) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 4 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 3 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2,a3);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(3)
template < typename OT,typename P1,typename P2,typename P3>
class constActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,P3,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 3 };
      typedef void (OT::WrappedClass::*constType)(P1,P2,P3) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 4 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 3 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P3 >::DeclareType a3 = translate::from_object<P3>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2,a3);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(2)
template < typename OT, typename RT ,typename P1,typename P2>
class ActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 2 };
      typedef RT (OT::*Type)(P1,P2)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 3 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 2 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(2)
template < typename OT,typename P1,typename P2>
class ActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 2 };
      typedef void (OT::*Type)(P1,P2)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 3 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 2 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(2)
template < typename OT, typename RT ,typename P1,typename P2>
class constActivationFrameExternalObjectMethodPtrT <OT,RT,P1,P2,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 2 };
      typedef RT (OT::*constType)(P1,P2) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 3 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 2 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(2)
template < typename OT,typename P1,typename P2>
class constActivationFrameExternalObjectMethodPtrT <OT,void,P1,P2,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 2 };
      typedef void (OT::*constType)(P1,P2) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 3 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 2 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(2)
template < typename OT, typename RT ,typename P1,typename P2>
class ActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 2 };
      typedef RT (OT::WrappedClass::*Type)(P1,P2)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 3 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 2 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(2)
template < typename OT,typename P1,typename P2>
class ActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 2 };
      typedef void (OT::WrappedClass::*Type)(P1,P2)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 3 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 2 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(2)
template < typename OT, typename RT ,typename P1,typename P2>
class constActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,P2,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 2 };
      typedef RT (OT::WrappedClass::*constType)(P1,P2) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 3 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 2 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1,a2);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(2)
template < typename OT,typename P1,typename P2>
class constActivationFrameExternalIndirectMethodPtrT <OT,void,P1,P2,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 2 };
      typedef void (OT::WrappedClass::*constType)(P1,P2) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 3 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 2 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        ++idx; // advance to next arg
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P2 >::DeclareType a2 = translate::from_object<P2>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1,a2);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(1)
template < typename OT, typename RT ,typename P1>
class ActivationFrameExternalObjectMethodPtrT <OT,RT,P1,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 1 };
      typedef RT (OT::*Type)(P1)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 2 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 1 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(1)
template < typename OT,typename P1>
class ActivationFrameExternalObjectMethodPtrT <OT,void,P1,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 1 };
      typedef void (OT::*Type)(P1)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 2 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 1 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(1)
template < typename OT, typename RT ,typename P1>
class constActivationFrameExternalObjectMethodPtrT <OT,RT,P1,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 1 };
      typedef RT (OT::*constType)(P1) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 2 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 1 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(1)
template < typename OT,typename P1>
class constActivationFrameExternalObjectMethodPtrT <OT,void,P1,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 1 };
      typedef void (OT::*constType)(P1) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 2 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 1 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(1)
template < typename OT, typename RT ,typename P1>
class ActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 1 };
      typedef RT (OT::WrappedClass::*Type)(P1)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 2 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 1 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(1)
template < typename OT,typename P1>
class ActivationFrameExternalIndirectMethodPtrT <OT,void,P1,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 1 };
      typedef void (OT::WrappedClass::*Type)(P1)  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 2 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 1 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(1)
template < typename OT, typename RT ,typename P1>
class constActivationFrameExternalIndirectMethodPtrT <OT,RT,P1,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 1 };
      typedef RT (OT::WrappedClass::*constType)(P1) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 2 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 1 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)(a1);
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(1)
template < typename OT,typename P1>
class constActivationFrameExternalIndirectMethodPtrT <OT,void,P1,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 1 };
      typedef void (OT::WrappedClass::*constType)(P1) const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 2 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 1 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        int idx = 0;
        if ( idx == singleDispatchArgumentIndex ) ++idx; // skip the single dispatch argument index 
        typename translate::from_object<P1 >::DeclareType a1 = translate::from_object<P1>::convert(af->element(idx));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)(a1);
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(0)
template < typename OT, typename RT >
class ActivationFrameExternalObjectMethodPtrT <OT,RT,void,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 0 };
      typedef RT (OT::*Type)()  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 1 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 0 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)();
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(0)
template < typename OT>
class ActivationFrameExternalObjectMethodPtrT <OT,void,void,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 0 };
      typedef void (OT::*Type)()  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 1 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 0 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)();
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(0)
template < typename OT, typename RT >
class constActivationFrameExternalObjectMethodPtrT <OT,RT,void,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 0 };
      typedef RT (OT::*constType)() const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 1 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 0 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)();
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(0)
template < typename OT>
class constActivationFrameExternalObjectMethodPtrT <OT,void,void,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 0 };
      typedef void (OT::*constType)() const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 1 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 0 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        OT* receiver = ot.get();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)();
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(0)
template < typename OT, typename RT >
class ActivationFrameExternalIndirectMethodPtrT <OT,RT,void,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 0 };
      typedef RT (OT::WrappedClass::*Type)()  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 1 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 0 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)();
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(0)
template < typename OT>
class ActivationFrameExternalIndirectMethodPtrT <OT,void,void,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 0 };
      typedef void (OT::WrappedClass::*Type)()  ;
      static T_sp activate( Type method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 1 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 0 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)();
        return T_O::_nil;
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(RT) arity(0)
template < typename OT, typename RT >
class constActivationFrameExternalIndirectMethodPtrT <OT,RT,void,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 0 };
      typedef RT (OT::WrappedClass::*constType)() const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 1 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 0 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
        RT retval = (receiver->*method)();
        return translate::to_object<RT>::convert(retval);
   }
};



// method invoker, dispatch on OT (0th entry in ActivationFrame) - comment: partially_specialized returns(void) arity(0)
template < typename OT>
class constActivationFrameExternalIndirectMethodPtrT <OT,void,void,void,void,void,void,void,void,void,void,void> {
   public:
      enum { NumParams = 0 };
      typedef void (OT::WrappedClass::*constType)() const ;
      static T_sp activate( constType method, const_ActivationFrame_spREF af,int singleDispatchArgumentIndex )
    {_G();
	if ( af->length() != 1 )
	{
           // note: number of ActivationFrame entries passed is one more than the 
           // number of arguments for the method - one entry is for the receiver object
	    stringstream ss;
	    ss << "Method expected "<< 0 << " argument(s) bu was passed " << (af->length()-1) << " argument(s).";
	    THROW(LispError_O::create(ss.str(),_lisp));
	}
        typename translate::from_object<OT >::DeclareType ot = translate::from_object<OT>::convert(af->element(singleDispatchArgumentIndex));
        OT* otget = ot.get();
        typename OT::PointerToWrappedType receiver = otget->wrappedPtr();
        ASSERTF(receiver!=NULL,BF("The receiver pointer is NULL and this should never happen"));
(receiver->*method)();
        return T_O::_nil;
   }
};



// Wrapper for ActivationFrameExternalObjectMethodPtrT
template<typename OT, typename RT,typename P1=void,typename P2=void,typename P3=void,typename P4=void,typename P5=void,typename P6=void,typename P7=void,typename P8=void,typename P9=void,typename P10=void >
class ActivationFrameExternalObjectMethodWrapPtr : public SingleDispatchMethoid {
private:
    typedef typename ActivationFrameExternalObjectMethodPtrT<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>::Type MethPtr;
    ActivationFrameExternalObjectMethodPtrT<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>		invoker;
    MethPtr 				mptr;
public:
    string describe() const { return "ActivationFrameExternalObjectMethodWrapPtr";}
    enum { NumParams = ActivationFrameExternalObjectMethodPtrT<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>::NumParams };
    typedef OT      ReceiverT;
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
    ActivationFrameExternalObjectMethodWrapPtr(const string& name,MethPtr ptr) : SingleDispatchMethoid(name), mptr(ptr) {}

    T_sp activate(const_ActivationFrame_spREF af)
    {_G();
        return this->invoker.activate(this->mptr,af,this->_SingleDispatchArgumentIndex);
    };
};



// Wrapper for constActivationFrameExternalObjectMethodPtrT
template<typename OT, typename RT,typename P1=void,typename P2=void,typename P3=void,typename P4=void,typename P5=void,typename P6=void,typename P7=void,typename P8=void,typename P9=void,typename P10=void >
class constActivationFrameExternalObjectMethodWrapPtr : public SingleDispatchMethoid {
private:
    typedef typename constActivationFrameExternalObjectMethodPtrT<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>::constType constMethPtr;
    constActivationFrameExternalObjectMethodPtrT<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>		invoker;
    constMethPtr 				mptr;
public:
    string describe() const { return "ActivationFrameExternalObjectMethodWrapPtr";}
    enum { NumParams = constActivationFrameExternalObjectMethodPtrT<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>::NumParams };
    typedef OT      ReceiverT;
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
    constActivationFrameExternalObjectMethodWrapPtr(const string& name,constMethPtr ptr) : SingleDispatchMethoid(name), mptr(ptr) {}

    T_sp activate(const_ActivationFrame_spREF af)
    {_G();
        return this->invoker.activate(this->mptr,af,this->_SingleDispatchArgumentIndex);
    };
};



// Wrapper for ActivationFrameExternalIndirectMethodPtrT
template<typename OT, typename RT,typename P1=void,typename P2=void,typename P3=void,typename P4=void,typename P5=void,typename P6=void,typename P7=void,typename P8=void,typename P9=void,typename P10=void >
class ActivationFrameExternalIndirectMethodWrapPtr : public SingleDispatchMethoid {
private:
    typedef typename ActivationFrameExternalIndirectMethodPtrT<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>::Type MethPtr;
    ActivationFrameExternalIndirectMethodPtrT<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>		invoker;
    MethPtr 				mptr;
public:
    string describe() const { return "ActivationFrameExternalIndirectMethodWrapPtr";}
    enum { NumParams = ActivationFrameExternalIndirectMethodPtrT<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>::NumParams };
    typedef OT      ReceiverT;
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
    ActivationFrameExternalIndirectMethodWrapPtr(const string& name,MethPtr ptr) : SingleDispatchMethoid(name), mptr(ptr) {}

    T_sp activate(const_ActivationFrame_spREF af)
    {_G();
        return this->invoker.activate(this->mptr,af,this->_SingleDispatchArgumentIndex);
    };
};



// Wrapper for constActivationFrameExternalIndirectMethodPtrT
template<typename OT, typename RT,typename P1=void,typename P2=void,typename P3=void,typename P4=void,typename P5=void,typename P6=void,typename P7=void,typename P8=void,typename P9=void,typename P10=void >
class constActivationFrameExternalIndirectMethodWrapPtr : public SingleDispatchMethoid {
private:
    typedef typename constActivationFrameExternalIndirectMethodPtrT<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>::constType constMethPtr;
    constActivationFrameExternalIndirectMethodPtrT<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>		invoker;
    constMethPtr 				mptr;
public:
    string describe() const { return "ActivationFrameExternalIndirectMethodWrapPtr";}
    enum { NumParams = constActivationFrameExternalIndirectMethodPtrT<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>::NumParams };
    typedef OT      ReceiverT;
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
    constActivationFrameExternalIndirectMethodWrapPtr(const string& name,constMethPtr ptr) : SingleDispatchMethoid(name), mptr(ptr) {}

    T_sp activate(const_ActivationFrame_spREF af)
    {_G();
        return this->invoker.activate(this->mptr,af,this->_SingleDispatchArgumentIndex);
    };
};
#endif // af_external_methodptrt_H
#ifdef af_external_method_def_H



template <typename RT>
    externalClass_& def( const string& name, RT (OT::*mp)()  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalObjectMethodWrapPtr<OT,RT>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,1);
        return *this;
    }



template <typename RT,typename P1>
    externalClass_& def( const string& name, RT (OT::*mp)(P1)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,2);
        return *this;
    }



template <typename RT,typename P1,typename P2>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,3);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,4);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,5);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4,P5)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,6);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4,P5,P6)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,7);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4,P5,P6,P7)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,8);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4,P5,P6,P7,P8)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,9);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4,P5,P6,P7,P8,P9)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,10);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9,typename P10>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,11);
        return *this;
    }



template <typename RT>
    externalClass_& def( const string& name, RT (OT::*mp)() const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalObjectMethodWrapPtr<OT,RT>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,1);
        return *this;
    }



template <typename RT,typename P1>
    externalClass_& def( const string& name, RT (OT::*mp)(P1) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,2);
        return *this;
    }



template <typename RT,typename P1,typename P2>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,3);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,4);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,5);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4,P5) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,6);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4,P5,P6) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,7);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4,P5,P6,P7) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,8);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4,P5,P6,P7,P8) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,9);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4,P5,P6,P7,P8,P9) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,10);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9,typename P10>
    externalClass_& def( const string& name, RT (OT::*mp)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalObjectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,11);
        return *this;
    }



template <typename RT>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)()  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalIndirectMethodWrapPtr<OT,RT>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,1);
        return *this;
    }



template <typename RT,typename P1>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,2);
        return *this;
    }



template <typename RT,typename P1,typename P2>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,3);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,4);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,5);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4,P5)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,6);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4,P5,P6)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,7);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4,P5,P6,P7)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,8);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4,P5,P6,P7,P8)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,9);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4,P5,P6,P7,P8,P9)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,10);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9,typename P10>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10)  , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new ActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,11);
        return *this;
    }



template <typename RT>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)() const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalIndirectMethodWrapPtr<OT,RT>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,1);
        return *this;
    }



template <typename RT,typename P1>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,2);
        return *this;
    }



template <typename RT,typename P1,typename P2>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,3);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,4);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,5);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4,P5) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,6);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4,P5,P6) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,7);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4,P5,P6,P7) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,8);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4,P5,P6,P7,P8) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,9);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4,P5,P6,P7,P8,P9) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,10);
        return *this;
    }



template <typename RT,typename P1,typename P2,typename P3,typename P4,typename P5,typename P6,typename P7,typename P8,typename P9,typename P10>
    externalClass_& def( const string& name, RT (OT::WrappedClass::*mp)(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) const , const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
    {_G();
        SingleDispatchMethoid* m = new constActivationFrameExternalIndirectMethodWrapPtr<OT,RT,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10>(name,mp);
        lisp_defineSingleDispatchMethod(this->_Lisp,name,this->_ClassSymbol,m,lambda_list,declares,docstring,autoExport,11);
        return *this;
    }
#endif // af_external_method_def_H
