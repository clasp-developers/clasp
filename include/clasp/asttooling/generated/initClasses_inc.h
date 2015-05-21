// start
// define cpp macros: SET_SYMBOL, CREATE_CLASS, SET_CLASS, DEFINE_BASE_CLASSES, DEFINE_CLASS_NAMES, EXPOSE_TO_CANDO 
// define cpp macro: ALL_STAGES to get the effect of defining all of the macros above
// define cpp macro: EXPOSE_PYTHON to expose python
// Associating namespace(asttooling) with package(AstToolingPkg)
 //class AstVisitor_O : public core::T_O, public clang::RecursiveASTVisitor<AstVisitor_O> {
// Associating namespace(asttooling) with package(AstToolingPkg)
#ifdef HEADER_INCLUDES
#include "include/astVisitor.h"
#endif // HEADER_INCLUDES
#undef HEADER_INCLUDES
#if defined(SET_SYMBOL) || defined(ALL_STAGES)
// requires LOOKUP_SYMBOL(pkg,symbolName) be defined
asttooling::AstVisitor_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(asttooling::AstVisitor_O::static_packageName(),asttooling::AstVisitor_O::static_className()));
#endif // SET_SYMBOL
#undef SET_SYMBOL
#if defined(CREATE_CLASS) || defined(ALL_STAGES)

    LOG(BF("Creating class[classasttooling__AstVisitor_Oval]"));
    core::BuiltInClass_sp classasttooling__AstVisitor_Oval = core::BuiltInClass_O::createUncollectable();
    classasttooling__AstVisitor_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classasttooling__AstVisitor_Oval,_lisp,asttooling::AstVisitor_O::static_classSymbol());
    reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<asttooling::AstVisitor_O>::id,asttooling::AstVisitor_O::static_classSymbol());
    asttooling::AstVisitor_O::___staticClass = classasttooling__AstVisitor_Oval;
#ifdef USE_MPS
    asttooling::AstVisitor_O::static_Kind = gctools::GCKind<asttooling::AstVisitor_O>::Kind;
#endif
    core::af_setf_findClass(classasttooling__AstVisitor_Oval,asttooling::AstVisitor_O::static_classSymbol(),true,_Nil<core::Environment_O>());
    {
        core::LispObjectCreator<asttooling::AstVisitor_O>* cb = gctools::ClassAllocator<core::LispObjectCreator<asttooling::AstVisitor_O>>::allocateClass();
        asttooling::AstVisitor_O::___set_static_creator(cb);
    }
    LOG(BF("Set static_allocator for class(%s) to %X")% asttooling::AstVisitor_O::static_className() % (void*)(asttooling::AstVisitor_O::static_allocator) );
    classasttooling__AstVisitor_Oval->setCreator(asttooling::AstVisitor_O::static_creator);
    {
        LOG(BF("Created nil for class[%s]") % asttooling::AstVisitor_O::static_className() );
    }
    /* ----- the class and its nil are now defined and so is classasttooling__AstVisitor_Oval::___staticClass but the class _Slots and _Signature_ClassSlots are undefined - set them both to _Nil<T_O>() in stage3   ----- */
#endif // CREATE_CLASS
#undef CREATE_CLASS
#ifdef DUMP_INFO_CLASS // {
// Depends on nothing

    LOG(BF("---    dump_info   --- className: asttooling::AstVisitor_O @ %X") % classasttooling__AstVisitor_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % asttooling::AstVisitor_O::static_className() % asttooling::AstVisitor_O::static_classSymbol() );
#endif // } DUMP_INFO_CLASS
#undef DUMP_INFO_CLASS
#if defined(DEFINE_BASE_CLASSES) || defined(ALL_STAGES) // {
// Depends on nothing
classasttooling__AstVisitor_Oval->addInstanceBaseClassDoNotCalculateClassPrecedenceList(core::T_O::static_classSymbol());
#endif // } DEFINE_BASE_CLASSES
#undef DEFINE_BASE_CLASSES
#if defined(DEFINE_CLASS_NAMES) || defined(ALL_STAGES) // {
 core::T_sp _curPkg = _lisp->findPackage(CurrentPkg,true);
// Depends on nothing

    classasttooling__AstVisitor_Oval->__setupStage3NameAndCalculateClassPrecedenceList(asttooling::AstVisitor_O::static_classSymbol());
#endif // } DEFINE_CLASS_NAMES
#undef DEFINE_CLASS_NAMES
#if defined(EXPOSE_TO_CANDO) || defined(ALL_STAGES)
#ifdef Use_AstToolingPkg
#ifdef EXTERN_REGISTER
extern void Register_asttooling__AstVisitor_O(core::Lisp_sp); // base(s): set(['core::T_O'])
#endif // EXTERN_REGISTER
#ifdef INVOKE_REGISTER
{_BLOCK_TRACE("initializing Register_asttooling__AstVisitor_O");
    asttooling::Register_asttooling__AstVisitor_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // INVOKE_REGISTER
#endif // ifdef Use_AstToolingPkg
#endif // EXPOSE_TO_CANDO
#undef EXPOSE_TO_CANDO
#ifdef EXPOSE_TO_PYTHON
#ifdef Use_AstToolingPkg
extern void Call_exposePython_asttooling__AstVisitor_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: asttooling__AstVisitor_O");
	Call_exposePython_asttooling__AstVisitor_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_AstToolingPkg
#endif // EXPOSE_TO_PYTHON
#undef EXPOSE_TO_PYTHON
#if defined(EXPAND_CLASS_MACROS)
_CLASS_MACRO(asttooling::AstVisitor_O)
#endif // EXPAND_CLASS_MACROS
#undef ALL_STAGES
// ---------------- after class initializers
#ifdef EXPOSE_TO_PYTHON
#endif // EXPOSE_TO_PYTHON
