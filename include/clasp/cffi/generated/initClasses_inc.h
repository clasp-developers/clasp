// start
// define cpp macros: SET_SYMBOL, CREATE_CLASS, SET_CLASS, DEFINE_BASE_CLASSES, DEFINE_CLASS_NAMES, EXPOSE_TO_CANDO 
// define cpp macro: ALL_STAGES to get the effect of defining all of the macros above
// define cpp macro: EXPOSE_PYTHON to expose python
// Associating namespace(cffi) with package(CffiPkg)
 // class Pointer_O : public core::T_O {
// Associating namespace(cffi) with package(CffiPkg)
#ifdef HEADER_INCLUDES
#include "include/cffi.h"
#endif // HEADER_INCLUDES
#undef HEADER_INCLUDES
#if defined(SET_SYMBOL) || defined(ALL_STAGES)
// requires LOOKUP_SYMBOL(pkg,symbolName) be defined
cffi::Pointer_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(cffi::Pointer_O::static_packageName(),cffi::Pointer_O::static_className()));
#endif // SET_SYMBOL
#undef SET_SYMBOL
#if defined(CREATE_CLASS) || defined(ALL_STAGES)

    LOG(BF("Creating class[classcffi__Pointer_Oval]"));
    core::BuiltInClass_sp classcffi__Pointer_Oval = core::BuiltInClass_O::createUncollectable();
    classcffi__Pointer_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classcffi__Pointer_Oval,_lisp,cffi::Pointer_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<cffi::Pointer_O>::id,cffi::Pointer_O::static_classSymbol());
    cffi::Pointer_O::___staticClass = classcffi__Pointer_Oval;
#ifdef USE_MPS
    cffi::Pointer_O::static_Kind = gctools::GCKind<cffi::Pointer_O>::Kind;
#endif
    core::af_setf_findClass(classcffi__Pointer_Oval,cffi::Pointer_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<cffi::Pointer_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<cffi::Pointer_O>>::allocateClass();
        cffi::Pointer_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% cffi::Pointer_O::static_className() % (void*)(cffi::Pointer_O::static_allocator) );
    classcffi__Pointer_Oval->setCreator(cffi::Pointer_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % cffi::Pointer_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classcffi__Pointer_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */
#endif // CREATE_CLASS
#undef CREATE_CLASS
#ifdef DUMP_INFO_CLASS // {
// Depends on nothing

    LOG(BF("---    dump_info   --- className: cffi::Pointer_O @ %X") % classcffi__Pointer_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % cffi::Pointer_O::static_className() % cffi::Pointer_O::static_classSymbol() );
#endif // } DUMP_INFO_CLASS
#undef DUMP_INFO_CLASS
#if defined(DEFINE_BASE_CLASSES) || defined(ALL_STAGES) // {
// Depends on nothing
classcffi__Pointer_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
#endif // } DEFINE_BASE_CLASSES
#undef DEFINE_BASE_CLASSES
#if defined(DEFINE_CLASS_NAMES) || defined(ALL_STAGES) // {
 core::T_sp _curPkg = _lisp->findPackage(CurrentPkg,true);
// Depends on nothing

    classcffi__Pointer_Oval->__setupStage3NameAndCalculateClassPrecedenceList(cffi::Pointer_O::static_classSymbol());
#endif // } DEFINE_CLASS_NAMES
#undef DEFINE_CLASS_NAMES
#if defined(EXPOSE_TO_CANDO) || defined(ALL_STAGES)
#ifdef Use_CffiPkg
#ifdef EXTERN_REGISTER
extern void Register_cffi__Pointer_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_cffi__Pointer_O");
    cffi::Register_cffi__Pointer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_CffiPkg
#endif // EXPOSE_TO_CANDO
#undef EXPOSE_TO_CANDO
#ifdef EXPOSE_TO_PYTHON
#ifdef Use_CffiPkg
extern void Call_exposePython_cffi__Pointer_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: cffi__Pointer_O");
	Call_exposePython_cffi__Pointer_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_CffiPkg
#endif // EXPOSE_TO_PYTHON
#undef EXPOSE_TO_PYTHON
#if defined(EXPAND_CLASS_MACROS)
_CLASS_MACRO(cffi::Pointer_O)
#endif // EXPAND_CLASS_MACROS
#undef ALL_STAGES
// ---------------- after class initializers
#ifdef EXPOSE_TO_PYTHON
#endif // EXPOSE_TO_PYTHON
