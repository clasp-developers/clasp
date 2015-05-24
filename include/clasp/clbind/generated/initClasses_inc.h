// start
// define cpp macros: SET_SYMBOL, CREATE_CLASS, SET_CLASS, DEFINE_BASE_CLASSES, DEFINE_CLASS_NAMES, EXPOSE_TO_CANDO 
// define cpp macro: ALL_STAGES to get the effect of defining all of the macros above
// define cpp macro: EXPOSE_PYTHON to expose python
// Associating namespace(clbind) with package(ClbindPkg)
 // class ClassRegistry_O : public core::T_O {
 // class ClassRep_O : public core::BuiltInClass_O {
// Associating namespace(clbind) with package(ClbindPkg)
#ifdef HEADER_INCLUDES
#include "include/class_registry.h"
#include "include/class_rep.h"
#endif // HEADER_INCLUDES
#undef HEADER_INCLUDES
#if defined(SET_SYMBOL) || defined(ALL_STAGES)
// requires LOOKUP_SYMBOL(pkg,symbolName) be defined
clbind::ClassRegistry_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(clbind::ClassRegistry_O::static_packageName(),clbind::ClassRegistry_O::static_className()));
clbind::ClassRep_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(clbind::ClassRep_O::static_packageName(),clbind::ClassRep_O::static_className()));
#endif // SET_SYMBOL
#undef SET_SYMBOL
#if defined(CREATE_CLASS) || defined(ALL_STAGES)

    LOG(BF("Creating class[classclbind__ClassRegistry_Oval]"));
    core::BuiltInClass_sp classclbind__ClassRegistry_Oval = core::BuiltInClass_O::createUncollectable();
    classclbind__ClassRegistry_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classclbind__ClassRegistry_Oval,_lisp,clbind::ClassRegistry_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<clbind::ClassRegistry_O>::id,clbind::ClassRegistry_O::static_classSymbol());
    clbind::ClassRegistry_O::___staticClass = classclbind__ClassRegistry_Oval;
#ifdef USE_MPS
    clbind::ClassRegistry_O::static_Kind = gctools::GCKind<clbind::ClassRegistry_O>::Kind;
#endif
    core::af_setf_findClass(classclbind__ClassRegistry_Oval,clbind::ClassRegistry_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<clbind::ClassRegistry_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<clbind::ClassRegistry_O>>::allocateClass();
        clbind::ClassRegistry_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% clbind::ClassRegistry_O::static_className() % (void*)(clbind::ClassRegistry_O::static_allocator) );
    classclbind__ClassRegistry_Oval->setCreator(clbind::ClassRegistry_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % clbind::ClassRegistry_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classclbind__ClassRegistry_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */

    LOG(BF("Creating class[classclbind__ClassRep_Oval]"));
    StandardClass_sp classclbind__ClassRep_Oval = StandardClass_O::createUncollectable();
    classclbind__ClassRep_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classclbind__ClassRep_Oval,_lisp,clbind::ClassRep_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<clbind::ClassRep_O>::id,clbind::ClassRep_O::static_classSymbol());
    clbind::ClassRep_O::___staticClass = classclbind__ClassRep_Oval;
#ifdef USE_MPS
    clbind::ClassRep_O::static_Kind = gctools::GCKind<clbind::ClassRep_O>::Kind;
#endif
    core::af_setf_findClass(classclbind__ClassRep_Oval,clbind::ClassRep_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<clbind::ClassRep_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<clbind::ClassRep_O>>::allocateClass();
        clbind::ClassRep_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% clbind::ClassRep_O::static_className() % (void*)(clbind::ClassRep_O::static_allocator) );
    classclbind__ClassRep_Oval->setCreator(clbind::ClassRep_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % clbind::ClassRep_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classclbind__ClassRep_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */
#endif // CREATE_CLASS
#undef CREATE_CLASS
#ifdef DUMP_INFO_CLASS // {
// Depends on nothing

    LOG(BF("---    dump_info   --- className: clbind::ClassRegistry_O @ %X") % classclbind__ClassRegistry_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % clbind::ClassRegistry_O::static_className() % clbind::ClassRegistry_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: clbind::ClassRep_O @ %X") % classclbind__ClassRep_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % clbind::ClassRep_O::static_className() % clbind::ClassRep_O::static_classSymbol() );
#endif // } DUMP_INFO_CLASS
#undef DUMP_INFO_CLASS
#if defined(DEFINE_BASE_CLASSES) || defined(ALL_STAGES) // {
// Depends on nothing
classclbind__ClassRegistry_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
classclbind__ClassRep_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::BuiltInClass_O::static_classSymbol());
#endif // } DEFINE_BASE_CLASSES
#undef DEFINE_BASE_CLASSES
#if defined(DEFINE_CLASS_NAMES) || defined(ALL_STAGES) // {
// Depends on nothing

    classclbind__ClassRegistry_Oval->__setupStage3NameAndCalculateClassPrecedenceList(clbind::ClassRegistry_O::static_classSymbol());

    classclbind__ClassRep_Oval->__setupStage3NameAndCalculateClassPrecedenceList(clbind::ClassRep_O::static_classSymbol());
#endif // } DEFINE_CLASS_NAMES
#undef DEFINE_CLASS_NAMES
#if defined(EXPOSE_TO_CANDO) || defined(ALL_STAGES)
#ifdef Use_ClbindPkg
#ifdef EXTERN_REGISTER
extern void Register_clbind__ClassRegistry_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_clbind__ClassRegistry_O");
    clbind::Register_clbind__ClassRegistry_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClbindPkg
#ifdef Use_ClbindPkg
#ifdef EXTERN_REGISTER
extern void Register_clbind__ClassRep_O(core::Lisp_sp); // base(s): set(['core::BuiltInClass_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_clbind__ClassRep_O");
    clbind::Register_clbind__ClassRep_O(_lisp); // base(s): set(['core::BuiltInClass_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_ClbindPkg
#endif // EXPOSE_TO_CANDO
#undef EXPOSE_TO_CANDO
#ifdef EXPOSE_TO_PYTHON
#ifdef Use_ClbindPkg
extern void Call_exposePython_clbind__ClassRegistry_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: clbind__ClassRegistry_O");
	Call_exposePython_clbind__ClassRegistry_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_ClbindPkg
#ifdef Use_ClbindPkg
extern void Call_exposePython_clbind__ClassRep_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: clbind__ClassRep_O");
	Call_exposePython_clbind__ClassRep_O(_lisp); // base(s): set(['core::BuiltInClass_O'])
}
#endif // ifdef Use_ClbindPkg
#endif // EXPOSE_TO_PYTHON
#undef EXPOSE_TO_PYTHON
#if defined(EXPAND_CLASS_MACROS)
_CLASS_MACRO(clbind::ClassRegistry_O)
_CLASS_MACRO(clbind::ClassRep_O)
#endif // EXPAND_CLASS_MACROS
#undef ALL_STAGES
// ---------------- after class initializers
#ifdef EXPOSE_TO_PYTHON
#endif // EXPOSE_TO_PYTHON
