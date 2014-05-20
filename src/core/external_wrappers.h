#ifndef	external_wrappers_H
#define external_wrappers_H

#include "lispDefinitions.h"


#include "wrappers.h"
//#include "clbind.h"


namespace core 
{

//    using namespace clbind::policies;

#include "external_policies.h"
    using namespace policies;



    template<typename Policies, typename OT, typename Method>
    class IndirectVariadicMethoid : public Functoid {};

#include "external_wrappers_indirect_methoids.h"

// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
// ----------------------------------------
    extern Symbol_sp _sym_STARallCxxClassesSTAR;


    template < typename OT>
    class externalClass_
    {
    private:
	Symbol_sp	_ClassSymbol;
    public:
        void setup_class(const string& makerName)
        {_G();
            if ( IS_SYMBOL_UNDEFINED(OT::static_classSymbol()) )
            {
                SIMPLE_ERROR(BF("Attempting to add methods for "
                                "class that isn't defined yet"));
            }

            this->_ClassSymbol = OT::static_classSymbol();
            reg::lisp_registerClassSymbol<OT>(this->_ClassSymbol);
            /*! Accumulate all of the classes in reverse order of how they were initialized
              in the core::*all-cxx-classes* variable */
            if ( _sym_STARallCxxClassesSTAR->symbolValueUnsafe() ) {
                _sym_STARallCxxClassesSTAR->setf_symbolValue(Cons_O::create(OT::static_classSymbol(),_sym_STARallCxxClassesSTAR->symbolValue()));
            }

            // 
            // If the class isn't in the class table then add it
            //
            if ( lisp_boot_findClassBySymbolOrNil(OT::static_classSymbol()).nilp())
            {
                LOG(BF("Adding class(%s) to environment")% OT::static_className() );
                lisp_addClass(OT::static_classSymbol(),
                              OT::static_allocator,
                              OT::Bases::baseClass1Id(),
                              OT::Bases::baseClass2Id() );
            }
            if (makerName!="")
            {
                // use make-<className>
                DEPRECIATED();
                af_def(OT::static_packageName(),makerName,&new_LispObject<OT>);
            }
        }


        //
        //
        // ctor
        //
        //

        externalClass_()
        {_G();
            this->setup_class("");
        }

        externalClass_(const string& makerName)
        {_G();
            this->setup_class(makerName);
        }



        template <typename RT,class... ARGS>
        externalClass_& def( string const& name, RT (OT::*mp)(ARGS...), string const& lambda_list="", const string& declares="", const string& docstring="",bool autoExport=true)
        {_G();
            Functoid* m = new VariadicMethoid<0,RT(OT::*)(ARGS...)>(name,mp);
            lisp_defineSingleDispatchMethod(name,this->_ClassSymbol,m,0,lambda_list,declares,docstring,autoExport,sizeof...(ARGS)+1);
            return *this;
        }

        template <typename RT,class... ARGS>
        externalClass_& def( string const& name, RT (OT::*mp)(ARGS...) const,
                             string const& lambda_list="", const string& declares="", const string& docstring="",bool autoExport=true)
        {_G();
            Functoid* m = new VariadicMethoid<0,RT(OT::*)(ARGS...) const>(name,mp);
            lisp_defineSingleDispatchMethod(name,this->_ClassSymbol,m,0,lambda_list,declares,docstring,autoExport,sizeof...(ARGS)+1);
            return *this;
        }

        template <typename RT,class... ARGS>
        externalClass_& def( const string& name, RT (OT::ExternalType::*mp)(ARGS...),
                             const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
        {_G();
            Functoid* m = new IndirectVariadicMethoid<policies_<>,OT,RT(OT::ExternalType::*)(ARGS...)>(name,mp);
            lisp_defineSingleDispatchMethod(name,this->_ClassSymbol,m,0,lambda_list,declares,docstring,autoExport,sizeof...(ARGS)+1);
            return *this;
        }


        template <typename RT,class... ARGS>
        externalClass_& def( const string& name, RT (OT::ExternalType::*mp)(ARGS...) const,
                             const string& lambda_list="", const string& declares="", const string& docstring="", bool autoExport=true )
        {_G();
            Functoid* m = new IndirectVariadicMethoid<policies_<>,OT,RT(OT::ExternalType::*)(ARGS...) const>(name,mp);
            lisp_defineSingleDispatchMethod(name,this->_ClassSymbol,m,0,lambda_list,declares,docstring,autoExport,sizeof...(ARGS)+1);
            return *this;
        }


    };





};

#endif // external_wrappers_h
