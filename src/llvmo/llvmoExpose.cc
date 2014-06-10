#define DEBUG_LEVEL_FULL

#include <llvm/Support/system_error.h>
#include <llvm/ADT/OwningPtr.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/CodeGen/LinkAllCodegenComponents.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include "llvm/Support/raw_ostream.h"
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Transforms/Instrumentation.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Pass.h>
#include <llvm/PassManager.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Verifier.h>
#include "llvm/IR/AssemblyAnnotationWriter.h" // will be llvm/IR
//#include <llvm/IR/PrintModulePass.h> // will be llvm/IR  was llvm/Assembly

#include "core/common.h"
#include "core/cons.h"
#include "core/evaluator.h"
#include "core/symbolTable.h"
#include "core/package.h"
#include "core/stringList.h"
#include "core/environment.h"
#include "core/hashTableEqual.h"
#include "core/builtInClass.h"
#include "core/lambdaListHandler.h"
#include "core/multipleValues.h"
#include "core/environment.h"
#include "core/sourceFileInfo.h"
#include "core/loadTimeValues.h"
#include "core/bignum.h"
#include "core/pointer.h"
#include "core/str.h"
#include "core/vectorObjectsWithFillPtr.h"
#include <clbind/clbind.h>
#include "llvmoExpose.h"
#include "insertPoint.h"
#include "debugLoc.h"
#include "core/external_wrappers.h"
#include "core/wrappers.h"





namespace llvmo
{

    
    
#define ARGS_af_llvm_value_p "(arg)"
#define DECL_af_llvm_value_p ""
#define DOCS_af_llvm_value_p "llvm_value_p"
    bool af_llvm_value_p(core::T_sp o)
    {_G();
	if ( o.nilp() ) return false;
	if ( Value_sp v = o.asOrNull<Value_O>() )
	{
	    return true;
	}
	return false;
    };

    LLVMContext_sp LLVMContext_O::get_global_context() 
    {_G();
        GC_ALLOCATE(LLVMContext_O,context );
	context->_ptr = &(llvm::getGlobalContext());
	return context;
    }
    ;
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,LLVMContext_O);

void LLVMContext_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<LLVMContext_O>()
;
af_def(LlvmoPkg,"get-global-context",&LLVMContext_O::get_global_context);

};

	   void LLVMContext_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo




namespace llvmo
{



    
    
#define ARGS_Linker_O_make "(module)"
#define DECL_Linker_O_make ""
#define DOCS_Linker_O_make "Linker_O_make"
    Linker_sp Linker_O::make(Module_sp module)
    {_G();
        GC_ALLOCATE(Linker_O,self );
        self->_ptr = new llvm::Linker(module->wrappedPtr());
	return self;
    };


    
    
#define ARGS_af_linkInModule "(linker module)"
#define DECL_af_linkInModule ""
#define DOCS_af_linkInModule "linkInModule"
    core::T_mv af_linkInModule(Linker_sp linker, Module_sp module)
    {_G();
        if (linker.nilp() ) {
            SIMPLE_ERROR(BF("Linker was nil"));
        }
        if (module.nilp() ) {
            SIMPLE_ERROR(BF("Module must be something other than nil"));
        }
        std::string errorMsg;
        bool res = linker->wrappedPtr()->linkInModule(module->wrappedPtr(),&errorMsg);
        return Values(_lisp->_boolean(res),core::Str_O::create(errorMsg));
    };

    EXPOSE_CLASS(llvmo,Linker_O);

    void Linker_O::exposeCando(core::Lisp_sp lisp)
    {_G();
        core::externalClass_<Linker_O>()
            .def("getModule",&llvm::Linker::getModule)
            ;
        Defun_maker(LlvmoPkg,Linker);
        Defun(linkInModule);
    };

    void Linker_O::exposePython(core::Lisp_sp lisp)
    {_G();
        IMPLEMENT_ME();
    };
}; // llvmo



namespace llvmo
{
EXPOSE_CLASS(llvmo,Pass_O);

void Pass_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<Pass_O>()
;
};

	   void Pass_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,FunctionPass_O);

void FunctionPass_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<FunctionPass_O>()
;
};

	   void FunctionPass_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,ModulePass_O);

void ModulePass_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<ModulePass_O>()
;
};

	   void ModulePass_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,ImmutablePass_O);

void ImmutablePass_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<ImmutablePass_O>()
;
};

	   void ImmutablePass_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,PassManagerBase_O);

void PassManagerBase_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<PassManagerBase_O>()
        
;
};

	   void PassManagerBase_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
Value_sp Value_O::create(llvm::Value* ptr) {_G(); return core::RP_Create_wrapped<Value_O,llvm::Value*>(ptr);}
;
}


namespace llvmo
{
    EXPOSE_CLASS(llvmo,Value_O);

    void Value_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<Value_O>()
	    .def("dump", &llvm::Value::dump)
	    .def("getName", &llvm::Value::getName)
	    .def("setName", &llvm::Value::setName)
	    .def("getType", &llvm::Value::getType)
	    ;
    };

    void Value_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,User_O);

void User_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<User_O>()
;
};

	   void User_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo





namespace llvmo
{




#define ARGS_af_writeIrToFile "(module path)"
#define DECL_af_writeIrToFile ""
#define DOCS_af_writeIrToFile "writeIrToFile"
    void af_writeIrToFile(Module_sp module, core::Str_sp path)
    {_G();
	string ErrorInfo;
	string pathName = path->get();
	llvm::raw_fd_ostream OS(pathName.c_str(), ErrorInfo, ::llvm::sys::fs::OpenFlags::F_None);
	if ( !ErrorInfo.empty())
	{
	    SIMPLE_ERROR(BF("Could not write bitcode to %s - problem: %s") % pathName % ErrorInfo );
	}
	llvm::AssemblyAnnotationWriter* aaw = new llvm::AssemblyAnnotationWriter();
	module->wrappedPtr()->print(OS,aaw);
	delete aaw;
    }





    
    
#define ARGS_af_verifyModule "(module action)"
#define DECL_af_verifyModule ""
#define DOCS_af_verifyModule "verifyModule returns (values result errorinfo)"
    core::T_sp af_verifyModule(Module_sp module, core::Symbol_sp action)
    {_G();
	string errorInfo;
        llvm::raw_string_ostream ei(errorInfo);
	llvm::Module* m = module->wrappedPtr();
	bool result = llvm::verifyModule(*m,&ei);
	return( Values(_lisp->_boolean(result),core::Str_O::create(errorInfo)) );
    };



    
    
#define ARGS_af_verifyFunction "(function)"
#define DECL_af_verifyFunction ""
#define DOCS_af_verifyFunction "verifyFunction"
    core::T_mv af_verifyFunction(Function_sp function)
    {_G();
	llvm::Function* f = function->wrappedPtr();
	string errorInfo;
        llvm::raw_string_ostream ei(errorInfo);
	bool result = llvm::verifyFunction(*f,&ei);
	return Values( _lisp->_boolean(result), core::Str_O::create(errorInfo));
    };

    
    
#define ARGS_af_writeBitcodeToFile "(module pathname)"
#define DECL_af_writeBitcodeToFile ""
#define DOCS_af_writeBitcodeToFile "writeBitcodeToFile"
    void af_writeBitcodeToFile(Module_sp module, core::Str_sp pathname)
    {_G();
	string pn = pathname->get();
	string ErrorInfo;
	llvm::raw_fd_ostream OS(pn.c_str(),ErrorInfo, ::llvm::sys::fs::OpenFlags::F_None);
	if ( !ErrorInfo.empty())
	{
	    SIMPLE_ERROR(BF("Could not write bitcode to file[%s] - error: %s") % pn % ErrorInfo );
	}
	llvm::WriteBitcodeToFile(module->wrappedPtr(),OS);
    };


    
    
#define ARGS_af_parseBitcodeFile "(filename context)"
#define DECL_af_parseBitcodeFile ""
#define DOCS_af_parseBitcodeFile "parseBitcodeFile"
    Module_sp af_parseBitcodeFile(core::Str_sp filename, LLVMContext_sp context )
    {_G();
	string error;
	std::unique_ptr<llvm::MemoryBuffer> membuf;
	llvm::MemoryBuffer::getFile(filename->get(),membuf);
	llvm::ErrorOr<llvm::Module *>eom = llvm::parseBitcodeFile(membuf.get(),
                                                            *(context->wrappedPtr()));
        
	if ( eom.getError() )
	{
	    SIMPLE_ERROR(BF("Could not load bitcode for file %s - error: %s") % filename->get() % error);
	}

#if 0
	if ( engine->hasNamedModule(filename->get()))
	{
	    engine->removeNamedModule(filename->get());
	    LOG(BF("Removed existing module: %s") % filename->get());
	}
	Module_sp omodule = core::RP_Create_wrapped<Module_O,llvm::Module*>(m);
	engine->addNamedModule(filename->get(),omodule);
	LOG(BF("Added module: %s") % filename->get());
#else
	Module_sp omodule = core::RP_Create_wrapped<Module_O,llvm::Module*>(eom.get());
#endif
	return omodule;
    };


    
    
#define ARGS_af_valuep "(arg)"
#define DECL_af_valuep ""
#define DOCS_af_valuep "Return true if the arg is derived from llvm::Value"
    bool af_valuep(core::T_sp arg)
    {_G();
	return arg.isA<Value_O>();
    };

#if 0
	// Jan 31, 2013 - the Attribute/Argument api is changing fast and I'm not using it right now
	// and I don't want to mess with this code until it settles down
    Attribute_sp Attribute_O::get(LLVMContext_sp context, core::Cons_sp attribute_symbols)
    {_G();
	llvm::AttrBuilder attrBuilder;
	core::SymbolToEnumConverter_sp converter = _sym_AttributeEnum->symbolValue().as<core::SymbolToEnumConverter_O>();
	for ( core::Cons_sp cur=attribute_symbols;cur->notNil(); cur=cur->cdr() )
	{
	    core::Symbol_sp sym = cur->ocar().as<core::Symbol_O>();
	    llvm::Attribute::AttrKind e = converter->enumForSymbol<llvm::Attribute::AttrKind>(sym);
	    attrBuilder.addAttribute(e);
	}
	llvm::Attribute at = llvm::Attribute::get(*(context->wrappedPtr()),attrBuilder);
	return translate::to_object<llvm::Attribute>::convert(at).as<Attribute_O>();
    }
#endif
    
    EXPOSE_CLASS(llvmo,Attribute_O);
    
    void Attribute_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<Attribute_O>()
	    ;
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeNone);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeZExt);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeSExt);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeNoReturn);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeInReg);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeStructRet);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeNoUnwind);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeNoAlias);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeByVal);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeNest);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeReadNone);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeReadOnly);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeNoInline);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeAlwaysInline);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeOptimizeForSize);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeStackProtect);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeStackProtectReq);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeAlignment);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeNoCapture);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeNoRedZone);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeNoImplicitFloat);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeNaked);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeInlineHint);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeStackAlignment);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeReturnsTwice);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeUWTable);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeNonLazyBind);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeAddressSafety);
	SYMBOL_EXPORT_SC_(LlvmoPkg,AttributeEnum);
	core::enum_<llvm::Attribute::AttrKind>(_sym_AttributeEnum,"Attribute")
	    .value(_sym_AttributeNone,llvm::Attribute::None)
	    .value(_sym_AttributeZExt,llvm::Attribute::ZExt)
	    .value(_sym_AttributeSExt,llvm::Attribute::SExt)
	    .value(_sym_AttributeNoReturn,llvm::Attribute::NoReturn)
	    .value(_sym_AttributeInReg,llvm::Attribute::InReg)
	    .value(_sym_AttributeStructRet,llvm::Attribute::StructRet)
	    .value(_sym_AttributeNoUnwind,llvm::Attribute::NoUnwind)
	    .value(_sym_AttributeNoAlias,llvm::Attribute::NoAlias)
	    .value(_sym_AttributeByVal,llvm::Attribute::ByVal)
	    .value(_sym_AttributeNest,llvm::Attribute::Nest)
	    .value(_sym_AttributeReadNone,llvm::Attribute::ReadNone)
	    .value(_sym_AttributeReadOnly,llvm::Attribute::ReadOnly)
	    .value(_sym_AttributeNoInline,llvm::Attribute::NoInline)
	    .value(_sym_AttributeAlwaysInline,llvm::Attribute::AlwaysInline)
	    .value(_sym_AttributeOptimizeForSize,llvm::Attribute::OptimizeForSize)
	    .value(_sym_AttributeStackProtect,llvm::Attribute::StackProtect)
	    .value(_sym_AttributeStackProtectReq,llvm::Attribute::StackProtectReq)
	    .value(_sym_AttributeAlignment,llvm::Attribute::Alignment)
	    .value(_sym_AttributeNoCapture,llvm::Attribute::NoCapture)
	    .value(_sym_AttributeNoRedZone,llvm::Attribute::NoRedZone)
	    .value(_sym_AttributeNoImplicitFloat,llvm::Attribute::NoImplicitFloat)
	    .value(_sym_AttributeNaked,llvm::Attribute::Naked)
	    .value(_sym_AttributeInlineHint,llvm::Attribute::InlineHint)
	    .value(_sym_AttributeStackAlignment,llvm::Attribute::StackAlignment)
	    .value(_sym_AttributeReturnsTwice,llvm::Attribute::ReturnsTwice)
	    .value(_sym_AttributeUWTable,llvm::Attribute::UWTable)
	    .value(_sym_AttributeNonLazyBind,llvm::Attribute::NonLazyBind)
//	    .value(_sym_AttributeAddressSafety,llvm::Attribute::AddressSafety)
	    ;
	SYMBOL_EXPORT_SC_(LlvmoPkg,attributesGet);
#if 0
	// Jan 31, 2013 - the Attribute/Argument api is changing fast and I'm not using it right now
	// and I don't want to mess with this code until it settles down
	core::af_def(LlvmoPkg,"attributeGet",&Attribute_O::get);
#endif
    }
    
    void Attribute_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(packageName,Attribute,"","",_lisp)
	    ;
#endif
    }




    
    
#define ARGS_af_makeStringGlobal "(module svalue)"
#define DECL_af_makeStringGlobal ""
#define DOCS_af_makeStringGlobal "makeStringGlobal"
    Value_sp af_makeStringGlobal(Module_sp module, core::Str_sp svalue)
    {_G();
	llvm::Module &M = *(module->wrappedPtr());
	llvm::Constant *StrConstant = llvm::ConstantDataArray::getString(M.getContext(),svalue->get());
	llvm::GlobalVariable *GV = new llvm::GlobalVariable(M,StrConstant->getType(),
						      true,llvm::GlobalValue::InternalLinkage,
						      StrConstant);
	GV->setName(":::str");
	GV->setUnnamedAddr(true);
	return translate::to_object<llvm::Value*>::convert(GV).as<Value_O>();
    }

#if 0
	llvm::Module* mod = module->wrappedPtr();
	using namespace llvm;
	ArrayType* ArrayTy_0 = ArrayType::get(IntegerType::get(mod->getContext(), 8), svalue->length()+1 );
	GlobalVariable* gvar_array__str = new GlobalVariable(/*Module=*/*mod, 
							     /*Type=*/ArrayTy_0,
							     /*isConstant=*/true,
							     /*Linkage=*/GlobalValue::InternalLinkage,
							     /*Initializer=*/0, // has initializer, specified below
							     /*Name=*/".str");
	gvar_array__str->setAlignment(1);
// Constant Definitions
	const string& str = svalue->get();
	ArrayRef<uint8_t> ar_str((uint8_t*)&(str.c_str()[0]),str.size()+1);
	Constant* const_array_4 = ConstantArray::get(ArrayTy_0,ar_str);
//	Constant* const_array_4 = ConstantArray::get(ArrayTy_0, ArrayRef<string>(svalue->get()));
// Global Variable Definitions
	gvar_array__str->setInitializer(const_array_4);
	return gvar_array__str;
    }
#endif





    // Define Value_O::__repr__ which is prototyped in llvmoExpose.lisp
    string Value_O::__repr__() const
    {_G();
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " ";
	string str;
	llvm::raw_string_ostream ro(str);
	if ( this->wrappedPtr() == 0 )
	{
	    ss << "wrappedPtr()==0";
	} else
	{
	    try
	    {
		this->wrappedPtr()->print(ro);
		ss << ro.str();
	    } catch (...)
	    {
		ss << "COULD_NOT__REPR__LLVM::Value";
	    }
	}
	ss <<">";
	return ss.str();
    }

    bool Value_O::valid() const
    {_G();
	return this->wrappedPtr() != NULL;
    }




    
    
#define ARGS_af_valid "(value)"
#define DECL_af_valid ""
#define DOCS_af_valid "Return true if this is a valid LLMV value, meaning its pointer is not NULL"
    bool af_valid(Value_sp value)
    {_G();
	// nil is a valid 
	if ( value.nilp() ) return true;
	return value->valid();
    }

};




namespace llvmo
{

    void convert_sequence_types_to_vector(core::Sequence_sp elements, vector<llvm::Type*>& velements)
    {_G();
	core::Sequence_sp save_elements = elements;
	if ( af_consP(elements) )
	{
	    for ( core::Cons_sp cur = elements.as<core::Cons_O>(); cur.notnilp(); cur = cCdr(cur) )
	    {
		velements.push_back(oCar(cur).as<Type_O>()->wrappedPtr());
	    }
	} else if ( af_vectorP(elements) )
	{
	    core::Vector_sp vec = elements.as<core::Vector_O>();
	    for ( int i=0; i< vec->length(); i++ )
	    {
		core::T_sp element = vec->elt(i);
		Type_sp ty = element.as<Type_O>();
		velements.push_back(ty->wrappedPtr());
	    }
	} else
	{
	    SIMPLE_ERROR(BF("Could not convert %s into vector<llvm::Type*>") % _rep_(elements) );
	}
    }


}


namespace llvmo
{
#define ARGS_Module_O_make "(module-name context)"
#define DECL_Module_O_make ""
#define DOCS_Module_O_make ""
    Module_sp Module_O::make(llvm::StringRef module_name, LLVMContext_sp context)
    {_G();
        GC_ALLOCATE(Module_O,self );
	ASSERT(&(llvm::getGlobalContext()) == context->wrappedPtr());
	self->_ptr = new llvm::Module(module_name,*(context->wrappedPtr()));
	return self;
    };




    
    
#define ARGS_af_module_get_function_list "(module)"
#define DECL_af_module_get_function_list ""
#define DOCS_af_module_get_function_list "module_get_function_list"
    core::Cons_sp af_module_get_function_list(Module_sp module)
    {_G();
	ql::list fl(_lisp);
	llvm::Module::FunctionListType& functionList = module->wrappedPtr()->getFunctionList();
	for ( llvm::Module::FunctionListType::const_iterator it=functionList.begin();
	      it!=functionList.end(); it++ )
	{
	    Function_sp wrapped_func = translate::to_object<const llvm::Function&>::convert(*it).as<Function_O>();
	    fl << wrapped_func;
	}
	return fl.cons();
    };

};




namespace llvmo
{
    EXPOSE_CLASS(llvmo,Module_O);

    void Module_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	using namespace llvm;
	GlobalVariable* (llvm::Module::*getGlobalVariable_nc)(StringRef,bool) = &llvm::Module::getGlobalVariable;
	GlobalVariable* (Module::*getNamedGlobal_nc)(StringRef) = &Module::getNamedGlobal;
	void (Module::*addModuleFlag_nc)(MDNode *Node) = &Module::addModuleFlag;
	core::externalClass_<Module_O>()
	    .def("dump",  &llvm::Module::dump)
	    .def("addModuleFlag",addModuleFlag_nc)
	    .def("getModuleIdentifier",&llvm::Module::getModuleIdentifier)
	    .def("getFunction", &llvmo::Module_O::getFunction)
	    .def("getGlobalVariable", getGlobalVariable_nc)
	    .def("getNamedGlobal", getNamedGlobal_nc)
	    .def("getOrInsertGlobal", &llvm::Module::getOrInsertGlobal)
	    .def("moduleValid",&Module_O::valid)
	    .def("getGlobalList",&Module_O::getGlobalList)
	    .def("getOrCreateUniquedStringGlobalVariable",&Module_O::getOrCreateUniquedStringGlobalVariable)
	    .def("dump_namedMDList",&Module_O::dump_namedMDList)
	    .def("moduleDelete",&Module_O::moduleDelete)
	    .def("setTargetTriple",&llvm::Module::setTargetTriple)
	    ;
	core::af_def(LlvmoPkg,"make-Module",&Module_O::make,ARGS_Module_O_make,DECL_Module_O_make,DOCS_Module_O_make);
	SYMBOL_EXPORT_SC_(LlvmoPkg,verifyModule);
	Defun(verifyModule);

	SYMBOL_EXPORT_SC_(LlvmoPkg,module_get_function_list);
	Defun(module_get_function_list);
    };

    void Module_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };


    llvm::Function* Module_O::getFunction( core::Str_sp dispatchName )
    {_G();
	llvm::Module* module = this->wrappedPtr();
	string funcName = dispatchName->get();
	llvm::Function* func = module->getFunction(funcName);
	return func;
    }


    bool Module_O::valid() const
    {_G();
	return this->wrappedPtr() != NULL;
    }

    void Module_O::moduleDelete()
    {
	ASSERT(this->wrappedPtr()!=NULL);
	delete this->wrappedPtr();
	this->set_wrapped(NULL);
    }


    void Module_O::dump_namedMDList() const
    {_G();
	llvm::Module* M = this->wrappedPtr();
	for ( llvm::Module::const_named_metadata_iterator it=M->named_metadata_begin();
	      it!=M->named_metadata_end(); it++ )
	{
	    (*it).dump();
	}
    }

    void Module_O::initialize() {
        this->Base::initialize();
        this->_UniqueGlobalVariableStrings = core::HashTableEqual_O::create_default();
    }


    GlobalVariable_sp Module_O::getOrCreateUniquedStringGlobalVariable( const string& value, const string& name )
    {
        core::Str_sp nameKey = core::Str_O::create(name);
        core::Cons_sp it = this->_UniqueGlobalVariableStrings->gethash(nameKey,_Nil<core::Cons_O>()).as<core::Cons_O>();
//	map<string,GlobalVariableStringHolder>::iterator it = this->_UniqueGlobalVariableStrings.find(name);
	llvm::GlobalVariable* GV;
	if ( it.nilp() )
	{
	    llvm::Module* M = this->wrappedPtr();
	    llvm::LLVMContext& context = M->getContext();
	    llvm::Constant *StrConstant = llvm::ConstantDataArray::getString(context, value);
	    GV = new llvm::GlobalVariable(*M, StrConstant->getType(),
					  true, llvm::GlobalValue::InternalLinkage,
					  StrConstant);
	    GV->setName(name);
	    GV->setUnnamedAddr(true);
//	    GlobalVariableStringHolder holder;
            core::Str_sp first = core::Str_O::create(value);
            GlobalVariable_sp second = core::RP_Create_wrapped<GlobalVariable_O,llvm::GlobalVariable*>(GV);
            core::Cons_sp pair = core::Cons_O::create(first,second);
            this->_UniqueGlobalVariableStrings->setf_gethash(nameKey,pair);
//	    holder._String = value;
//	    holder._LlvmValue = core::RP_Create_wrapped<GlobalVariable_O,llvm::GlobalVariable*>(GV);
//	    this->_UniqueGlobalVariableStrings[name] = holder;
//	    return holder._LlvmValue;
            return second;
	}
	if ( oCar(it).as<core::Str_O>()->get() != value ) // as<Str_Oit->second._String != value )
	{
	    SIMPLE_ERROR(BF("You tried to getOrCreateUniquedStringGlobalVariable with name[%s] and value[%s] - there was already a StringGlobalVariable with that name but it has a different value!!!! value[%s]") % name % value % oCar(it).as<core::Str_O>()->get() ); // it->second._String );
	}
	return oCdr(it).as<GlobalVariable_O>(); // it->second._LlvmValue;
    }



    core::Cons_sp Module_O::getGlobalList() const
    {_G();
	ql::list globals(_lisp);
	llvm::Module* m = this->wrappedPtr();
	for ( llvm::Module::global_iterator it = m->global_begin(); it!=m->global_end(); it++ )
	{
	    globals << core::RP_Create_wrapped<GlobalVariable_O,llvm::GlobalVariable*>(&(*it));
	}
	return globals.cons();
    }
}; // llvmo


namespace llvmo
{

    void ExecutionEngine_O::initialize()
    {
        this->_DependentModules = core::HashTableEqual_O::create_default();
    }

    string ExecutionEngine_O::__repr__() const
    {_G();
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " " << this->_ptr << " > ";
	return ss.str();
    }


    core::Cons_sp ExecutionEngine_O::dependentModuleNames() const
    {_G();
	ql::list l;
        this->_DependentModules->mapHash( [&l] (core::T_sp key, core::T_sp val) {
                l << key;
            } );
	return l.cons();
    }


    void ExecutionEngine_O::addNamedModule(const string& name, Module_sp module)
    {_G();
        core::Str_sp key = core::Str_O::create(name);
        if ( this->_DependentModules->contains(key) ) {
//	if ( this->_DependentModules.count(name) != 0 )
	    SIMPLE_ERROR(BF("A module named %s is already in this ExecutionEngine - remove it first before adding another") % name );
	}
        this->_DependentModules->setf_gethash(key,module);
//	this->_DependentModules[name] = module;
	this->wrappedPtr()->addModule(module->wrappedPtr());
    }



    bool ExecutionEngine_O::hasNamedModule(const string& name)
    {_G();
	if ( this->_DependentModules->contains(core::Str_O::create(name)) ) return true;
	return false;
    }

    void ExecutionEngine_O::removeNamedModule(const string& name)
    {_G();
        core::Str_sp key = core::Str_O::create(name);
        core::T_mv mi = this->_DependentModules->gethash(key);
//	core::StringMap<Module_O>::iterator mi = this->_DependentModules.find(name);
	if ( mi.valueGet(1).nilp() ) // == this->_DependentModules.end() )
	{
	    SIMPLE_ERROR(BF("Could not find named module %s") % name );
	}
        Module_sp mod = mi.as<Module_O>();
	this->wrappedPtr()->clearGlobalMappingsFromModule(mod->wrappedPtr());
	this->wrappedPtr()->removeModule(mod->wrappedPtr());
	this->_DependentModules->remhash(key);
    }




    core::Function_sp ExecutionEngine_O::getCompiledFunction(core::Symbol_sp sym, Function_sp fn, core::ActivationFrame_sp activationFrameEnvironment, core::Symbol_sp functionKind)
    {_G();
	// Stuff to support MCJIT
	llvm::ExecutionEngine* engine = this->wrappedPtr();
	engine->finalizeObject();
	ASSERTF(fn.notnilp(),BF("The Function must never be nil"));
	void* p = engine->getPointerToFunction(fn->wrappedPtr());
	if (!p) {
	    SIMPLE_ERROR(BF("Could not get a pointer to the function: %s") % _rep_(sym));
	}
	LLVMFunctoid::fptr_type lisp_funcPtr = (LLVMFunctoid::fptr_type)(p);
	string functoidName = sym.nilp() ? "" : sym->fullName();
	LLVMFunctoid* functoid = gctools::ClassAllocator<LLVMFunctoid>::allocateClass(functoidName,lisp_funcPtr);
	core::CompiledBody_sp compiledBody = core::CompiledBody_O::create(functoid,fn);
	core::CompiledFunction_sp func = core::CompiledFunction_O::makeCompiledFunction( sym,
											 compiledBody,
											 activationFrameEnvironment,
											 _Nil<core::SourceFileInfo_O>(), 0, 0,
											 functionKind );
	return func;
    }


    void ExecutionEngine_O::runFunction(Function_sp func, core::Str_sp fileName ) //, core::Cons_sp args )
    {_G();
        if ( func.nilp() ) {
            SIMPLE_ERROR(BF("Function is nil"));
        }
	vector<llvm::GenericValue> argValues;
//	argValues.push_back(llvm::GenericValue((void*)(&result)));
//	argValues.push_back(llvm::GenericValue((void*)(&vf)));
	ASSERTF(this->wrappedPtr()!=NULL,BF("You asked to runFunction but the pointer to the function is NULL"));
	llvm::ExecutionEngine* engine = this->wrappedPtr();
	llvm::Function* fn = func->wrappedPtr();
	/* Force compilation like lli */
	engine->finalizeObject();
	/* Make sure a pointer for the function is available */
	void* fnptr = engine->getPointerToFunction(fn);
	if ( !fnptr ) {
	  SIMPLE_ERROR(BF("Could not get a pointer to the function: %s") % fn->getName().data() );
	}
	
	/* Run the function */
//	printf( "%s:%d - Calling startup function in: %s - Figure out what to do here - I need to start using the unix backtrace and dwarf debugging information rather than setting up my own backtrace info in the IHF", __FILE__, __LINE__, fileName->c_str() );
	core::TopLevelIHF frame(_lisp->invocationHistoryStack(),_Nil<T_O>());
	engine->runFunction(fn,argValues);
//	return result;
    }



    void ExecutionEngine_O::addGlobalMapping(GlobalValue_sp value, core::Pointer_sp ptr)
    {_G();
	this->wrappedPtr()->addGlobalMapping(value->wrappedPtr(),ptr->ptr());
    }

#if 0
    void ExecutionEngine_O::addGlobalMappingForLoadTimeValueVector(GlobalValue_sp value,const string& name)
    {
	static core::LoadTimeValues_sp* ltvP;
	core::LoadTimeValues_sp& vo = _lisp->getOrCreateLoadTimeValues(name,0,0);
	ltvP = &vo;
	void* currentPtr = this->wrappedPtr()->getPointerToGlobalIfAvailable(value->wrappedPtr());
	if ( currentPtr!=NULL )
	{
	    // Remove existing mapping
	    this->wrappedPtr()->updateGlobalMapping(value->wrappedPtr(),NULL);
	}
	this->wrappedPtr()->addGlobalMapping(value->wrappedPtr(),((void*)(&ltvP)));
    }
#endif







    EXPOSE_CLASS(llvmo,ExecutionEngine_O);




    void ExecutionEngine_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<ExecutionEngine_O>()
	    .def("clearAllGlobalMappings",&llvm::ExecutionEngine::clearAllGlobalMappings)
	    .def("addGlobalMapping",&ExecutionEngine_O::addGlobalMapping)
//	    .def("addGlobalMappingForLoadTimeValueVector",&ExecutionEngine_O::addGlobalMappingForLoadTimeValueVector)
	    .def("getCompiledFunction",&ExecutionEngine_O::getCompiledFunction)
	    .def("getDataLayout", &llvm::ExecutionEngine::getDataLayout)
	    .def("hasNamedModule", &ExecutionEngine_O::hasNamedModule)
	    .def("dependentModuleNames", &ExecutionEngine_O::dependentModuleNames)
	    .def("runFunction",&ExecutionEngine_O::runFunction)
//	    .def("getPointerToFunction", &llvm::ExecutionEngine::getPointerToFunction)
	    ;
	
    };

    void ExecutionEngine_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo











namespace llvmo
{



    DataLayout_sp DataLayout_O::copy() const
    {_G();
        GC_ALLOCATE(DataLayout_O,cp );
	cp->_ptr = new llvm::DataLayout(*(this->wrappedPtr()));
	return cp;
    };






EXPOSE_CLASS(llvmo,DataLayout_O);

void DataLayout_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<DataLayout_O>()
	.def("DataLayoutCopy",&DataLayout_O::copy)
	.def("DataLayout-getTypeAllocSize",&DataLayout_O::ExternalType::getTypeAllocSize)
	;

};

	   void DataLayout_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}



namespace llvmo
{
    EXPOSE_CLASS(llvmo,DataLayoutPass_O);



#define ARGS_DataLayoutPass_O_make "(module)"
#define DECL_DataLayoutPass_O_make ""
#define DOCS_DataLayoutPass_O_make ""
    DataLayoutPass_sp DataLayoutPass_O::make(llvm::DataLayout const & dl)
    {_G();
        GC_ALLOCATE(DataLayoutPass_O,self );
	self->_ptr = new llvm::DataLayoutPass(dl);
	return self;
    };


    void DataLayoutPass_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<DataLayoutPass_O>()
	    ;
        core::af_def(LlvmoPkg,"makeDataLayoutPass",&DataLayoutPass_O::make,ARGS_DataLayoutPass_O_make,DECL_DataLayoutPass_O_make,DOCS_DataLayoutPass_O_make);
    };

    void DataLayoutPass_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo




#if 0 // TargetData was depreciated
namespace llvmo
{
    EXPOSE_CLASS(llvmo,TargetData_O);



#define ARGS_TargetData_O_copy "(module)"
#define DECL_TargetData_O_copy ""
#define DOCS_TargetData_O_copy ""
    TargetData_sp TargetData_O::copy(llvm::TargetData const & orig)
    {_G();
        GC_ALLOCATE(TargetData_O,self );
	self->_ptr = new llvm::TargetData(orig);
	return self;
    };


    void TargetData_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<TargetData_O>()
	    ;
        core::af_def(LlvmoPkg,"target-data-copy",&TargetData_O::copy,ARGS_TargetData_O_copy,DECL_TargetData_O_copy,DOCS_TargetData_O_copy);

    };

    void TargetData_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}

#endif






namespace llvmo
{
#define ARGS_FunctionPassManager_O_make "(module)"
#define DECL_FunctionPassManager_O_make ""
#define DOCS_FunctionPassManager_O_make ""
    FunctionPassManager_sp FunctionPassManager_O::make(llvm::Module* module)
    {_G();
        GC_ALLOCATE(FunctionPassManager_O,self );
	self->_ptr = new llvm::FunctionPassManager(module);
	return self;
    };



    EXPOSE_CLASS(llvmo,FunctionPassManager_O);

    void FunctionPassManager_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<FunctionPassManager_O>()
	    .def("function-pass-manager-add",&llvm::FunctionPassManager::add)
	    .def("doInitialization",&llvm::FunctionPassManager::doInitialization)
	    .def("function-pass-manager-run",&llvm::FunctionPassManager::run)
	    ;
        core::af_def(LlvmoPkg,"makeFunctionPassManager",&FunctionPassManager_O::make,ARGS_FunctionPassManager_O_make,DECL_FunctionPassManager_O_make,DOCS_FunctionPassManager_O_make);
    };

    void FunctionPassManager_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}





namespace llvmo
{
#define ARGS_PassManager_O_make "()"
#define DECL_PassManager_O_make ""
#define DOCS_PassManager_O_make ""
    PassManager_sp PassManager_O::make()
    {_G();
        GC_ALLOCATE(PassManager_O,self );
	self->_ptr = new llvm::PassManager();
	return self;
    };



    EXPOSE_CLASS(llvmo,PassManager_O);

    void PassManager_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<PassManager_O>()
	    .def("pass-manager-add",&llvm::PassManager::add)
	    .def("pass-manager-run",&llvm::PassManager::run)
	    ;
        core::af_def(LlvmoPkg,"makePassManager",&PassManager_O::make,ARGS_PassManager_O_make,DECL_PassManager_O_make,DOCS_PassManager_O_make);
    };

    void PassManager_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}



namespace llvmo
{
#define ARGS_EngineBuilder_O_make "(module)"
#define DECL_EngineBuilder_O_make ""
#define DOCS_EngineBuilder_O_make ""
    EngineBuilder_sp EngineBuilder_O::make(llvm::Module* module)
    {_G();
        GC_ALLOCATE(EngineBuilder_O,self );
	self->_ptr = new llvm::EngineBuilder(module);
	self->_ptr->setErrorStr(&(self->_ErrorStr));
	return self;
    };

    void EngineBuilder_O::setEngineKind(core::Symbol_sp kind)
    {_G();
	SYMBOL_EXPORT_SC_(LlvmoPkg,interpreter);
	SYMBOL_EXPORT_SC_(LlvmoPkg,jit);
	if ( kind == _sym_interpreter )
	{
	    this->wrappedPtr()->setEngineKind(llvm::EngineKind::Interpreter);
	} else if ( kind == _sym_jit )
	{
	    this->wrappedPtr()->setEngineKind(llvm::EngineKind::JIT);
	} else
	{
	    stringstream ss;
	    ss << "Engine kind can only be ";
	    ss << _sym_interpreter->fullName() << " or ";
	    ss << _sym_jit->fullName() << " - you gave: " << kind->fullName();
	    SIMPLE_ERROR(BF("%s") % ss.str());
	}
    }


    void EngineBuilder_O::setUseMCJIT(bool use_mcjit)
    {_G();
	this->wrappedPtr()->setUseMCJIT(use_mcjit);
#if 0
	if ( use_mcjit )
	{
	    // Setup to use MCJIT
	    llvm::EngineBuilder* builder = this->wrappedPtr();
	    builder->setRelocationModel(llvm::Reloc::Default);
	    builder->setOptLevel(llvm::CodeGenOpt::Default);
	    llvm::JITMemoryManager* mm = new llvm::SectionMemoryManager();
	    this->wrappedPtr()->setJITMemoryManager(mm);
	}
#endif
    }

	

    void EngineBuilder_O::setTargetOptions(core::Cons_sp optionsPlist)
    {_G();
	SYMBOL_EXPORT_SC_(LlvmoPkg,JITEmitDebugInfo);
	SYMBOL_EXPORT_SC_(LlvmoPkg,JITEmitDebugInfoToDisk);
	llvm::TargetOptions targetOptions;
	for ( core::Cons_sp cur=optionsPlist; cur.notnilp(); cur=cCddr(cur) )
	{
	    core::Symbol_sp kw = oCar(cur).as<core::Symbol_O>();
	    core::T_sp val = oCadr(cur);
	    unsigned ival = 0;
	    if ( val.isTrue() ) ival = 1;
	    if ( kw == _sym_JITEmitDebugInfo )
	    {
		targetOptions.JITEmitDebugInfo = ival;
	    } else if ( kw == _sym_JITEmitDebugInfoToDisk )
	    {
		targetOptions.JITEmitDebugInfoToDisk = ival;
	    } else
	    {
		SIMPLE_ERROR(BF("Unrecognized TargetOption keyword[%s]") % _rep_(kw) );
	    }
	}
	this->wrappedPtr()->setTargetOptions(targetOptions);
    }




    EXPOSE_CLASS(llvmo,EngineBuilder_O);

    void EngineBuilder_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	
//	llvm::ExecutionEngine* (llvm::EngineBuilder::*create)() = &llvm::EngineBuilder::create;
//	llvm::ExecutionEngine* (llvm::EngineBuilder::*create_targetMachine)(llvm::TargetMachine*) = &llvm::EngineBuilder::create;

	core::externalClass_<EngineBuilder_O>()
	    .def("create",&EngineBuilder_O::createExecutionEngine)
	    .def("error_string",&EngineBuilder_O::error_string)
	    .def("setEngineKind",&EngineBuilder_O::setEngineKind)
	    .def("setTargetOptions",&EngineBuilder_O::setTargetOptions)
	    .def("setUseMCJIT",&EngineBuilder_O::setUseMCJIT)
//	    .def("getPointerToFunction",&llvm::EngineBuilder::getPointerToFunction)
//	    .def("createTargetMachine",createTargetMachine)
	    ;
        core::af_def(LlvmoPkg,"make-EngineBuilder",&EngineBuilder_O::make,ARGS_EngineBuilder_O_make,DECL_EngineBuilder_O_make,DOCS_EngineBuilder_O_make);
    };

    void EngineBuilder_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };



    ExecutionEngine_sp EngineBuilder_O::createExecutionEngine()
    {_G();
	llvm::ExecutionEngine* ee = this->wrappedPtr()->create();
	ExecutionEngine_sp eeo = core::RP_Create_wrapped<ExecutionEngine_O,llvm::ExecutionEngine*>(ee);
	return eeo;
    }

}; // llvmo







namespace llvmo
{
#define ARGS_PassManagerBuilder_O_make "()"
#define DECL_PassManagerBuilder_O_make ""
#define DOCS_PassManagerBuilder_O_make ""
    PassManagerBuilder_sp PassManagerBuilder_O::make()
    {_G();
        GC_ALLOCATE(PassManagerBuilder_O,self );
	self->_ptr = new llvm::PassManagerBuilder();
	return self;
    };






    EXPOSE_CLASS(llvmo,PassManagerBuilder_O);

    void PassManagerBuilder_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	
	core::externalClass_<PassManagerBuilder_O>()
	    .def("populateModulePassManager",&llvm::PassManagerBuilder::populateModulePassManager)
	    .def("populateLTOPassManager",&llvm::PassManagerBuilder::populateLTOPassManager)
	    ;
        core::af_def(LlvmoPkg,"make-PassManagerBuilder",&PassManagerBuilder_O::make,ARGS_PassManagerBuilder_O_make,DECL_PassManagerBuilder_O_make,DOCS_PassManagerBuilder_O_make);
    };

    void PassManagerBuilder_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };



}; // llvmo







namespace llvmo
{
Constant_sp Constant_O::create(llvm::Constant* ptr) {_G(); return core::RP_Create_wrapped<Constant_O,llvm::Constant*>(ptr);}
;
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,Constant_O);

void Constant_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<Constant_O>()
;
};

	   void Constant_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo






namespace llvmo
{

    EXPOSE_CLASS(llvmo,ConstantDataSequential_O);

    void ConstantDataSequential_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<ConstantDataSequential_O>()
	    ;
    };

    void ConstantDataSequential_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}




namespace llvmo
{
#define ARGS_ConstantDataArray_O_getUInt32 "(type values)"
#define DECL_ConstantDataArray_O_getUInt32 ""
#define DOCS_ConstantDataArray_O_getUInt32 "Docs for ConstantDataArray get"
    Constant_sp ConstantDataArray_O::getUInt32(LLVMContext_sp context, core::T_sp ovalues)
    {_G();
	Constant_sp ca = ConstantDataArray_O::create();
	vector<uint32_t> vector_IdxList;
	core::Vector_sp vvalues;
	if ( af_consP(ovalues) )
	{
	    for ( core::Cons_sp cur=ovalues.as<core::Cons_O>(); cur.notnilp(); cur=cCdr(cur) )
	    {
		vector_IdxList.push_back(oCar(cur).as<core::Fixnum_O>()->get());
	    }
	} else if ( (vvalues = ovalues.asOrNull<core::Vector_O>()) )
	{
	    for ( int i=0; i<vvalues->length(); i++ )
	    {
		vector_IdxList.push_back(vvalues->svref(i).as<core::Fixnum_O>()->get());
	    }
	}
	llvm::ArrayRef<uint32_t> array_ref_vector_IdxList(vector_IdxList);
	llvm::Constant* llvm_ca = llvm::ConstantDataArray::get(*(context->wrappedPtr()),array_ref_vector_IdxList);
	ca->set_wrapped(llvm_ca);
	return ca;
    }


    EXPOSE_CLASS(llvmo,ConstantDataArray_O);

    void ConstantDataArray_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<ConstantDataArray_O>()
	    ;
	core::af_def(LlvmoPkg,"constant-data-array-get-uint32",&ConstantDataArray_O::getUInt32,ARGS_ConstantDataArray_O_getUInt32,DECL_ConstantDataArray_O_getUInt32,DOCS_ConstantDataArray_O_getUInt32);

    };

    void ConstantDataArray_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}














namespace llvmo
{
#define ARGS_ConstantArray_O_get "(type values)"
#define DECL_ConstantArray_O_get ""
#define DOCS_ConstantArray_O_get "Docs for ConstantArray get"
    Constant_sp ConstantArray_O::get(ArrayType_sp type, core::Cons_sp values)
    {_G();
	Constant_sp ca = ConstantArray_O::create();
	vector<llvm::Constant*> vector_IdxList;
	for ( core::Cons_sp cur=values; cur.notnilp(); cur=cCdr(cur) )
	{
	    vector_IdxList.push_back(oCar(cur).as<Constant_O>()->wrappedPtr());
	}
	llvm::ArrayRef<llvm::Constant*> array_ref_vector_IdxList(vector_IdxList);
	llvm::Constant* llvm_ca = llvm::ConstantArray::get(type->wrapped(),array_ref_vector_IdxList);
	ca->set_wrapped(llvm_ca);
	return ca;
    }


    EXPOSE_CLASS(llvmo,ConstantArray_O);

    void ConstantArray_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<ConstantArray_O>()
	    ;
	core::af_def(LlvmoPkg,"constant-array-get",&ConstantArray_O::get,ARGS_ConstantArray_O_get,DECL_ConstantArray_O_get,DOCS_ConstantArray_O_get);

    };

    void ConstantArray_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}








namespace llvmo
{
#define ARGS_BlockAddress_O_get "(function basic-block)"
#define DECL_BlockAddress_O_get ""
#define DOCS_BlockAddress_O_get "Docs for BlockAddress get"
    BlockAddress_sp BlockAddress_O::get(Function_sp func, BasicBlock_sp bb)
    {_G();
	BlockAddress_sp basp = BlockAddress_O::create();
	llvm::BlockAddress* ba = llvm::BlockAddress::get(func->wrappedPtr(),bb->wrappedPtr());
	basp->set_wrapped(ba);
	return basp;
    }


    EXPOSE_CLASS(llvmo,BlockAddress_O);

    void BlockAddress_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<BlockAddress_O>()
	    ;
	core::af_def(LlvmoPkg,"block-address-get",&BlockAddress_O::get,ARGS_BlockAddress_O_get,DECL_BlockAddress_O_get,DOCS_BlockAddress_O_get);

    };

    void BlockAddress_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}











namespace llvmo
{


    Constant_sp ConstantExpr_O::getInBoundsGetElementPtr(Constant_sp constant, core::Cons_sp idxList )
    {_G();
        GC_ALLOCATE(Constant_O,res );
	vector<llvm::Constant*> vector_IdxList;
	for ( core::Cons_sp cur=idxList; cur.notnilp(); cur=cCdr(cur) )
	{
	    vector_IdxList.push_back(oCar(cur).as<Constant_O>()->wrappedPtr());
	}
	llvm::ArrayRef<llvm::Constant*> array_ref_vector_IdxList(vector_IdxList);
	llvm::Constant* llvm_res = llvm::ConstantExpr::getInBoundsGetElementPtr(constant->wrappedPtr(),array_ref_vector_IdxList);
	res->set_wrapped(llvm_res);
	return res;
    }



    EXPOSE_CLASS(llvmo,ConstantExpr_O);

    void ConstantExpr_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<ConstantExpr_O>()
	    ;
	core::af_def(LlvmoPkg,"constant-expr-get-in-bounds-get-element-ptr",&ConstantExpr_O::getInBoundsGetElementPtr);
    };

    void ConstantExpr_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}















namespace llvmo
{
    EXPOSE_CLASS(llvmo,GlobalValue_O);

    void GlobalValue_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<GlobalValue_O>()
	    ;
    };

    void GlobalValue_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{


#define ARGS_GlobalVariable_O_make "(module type is-constant linkage initializer name)"
#define DECL_GlobalVariable_O_make ""
#define DOCS_GlobalVariable_O_make "make GlobalVariable args: module type is-constant linkage initializer name"
    GlobalVariable_sp GlobalVariable_O::make(Module_sp mod, Type_sp type, bool isConstant, core::Symbol_sp linkage, Constant_sp initializer, core::Str_sp name)
    {_G();
        GC_ALLOCATE(GlobalVariable_O,me );
	translate::from_object<llvm::GlobalValue::LinkageTypes> llinkage(linkage);
	llvm::Constant* llvm_initializer = NULL;
	if ( initializer.notnilp() ) {
	    llvm_initializer = initializer->wrappedPtr();
	}
	llvm::GlobalVariable* gv = new llvm::GlobalVariable(*(mod->wrappedPtr()),type->wrappedPtr(),isConstant,llinkage._v,llvm_initializer,name->get());
	me->set_wrapped(gv);
//	me->set_ptrIsOwned(true); // GlobalVariables made this way are responsible for freeing their pointers - I hope this isn't a disaster
	return me;
    };



    EXPOSE_CLASS(llvmo,GlobalVariable_O);

    void GlobalVariable_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<GlobalVariable_O>()
	    ;
	Defun_maker(LlvmoPkg,GlobalVariable);
    };

    void GlobalVariable_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo








namespace llvmo
{

    void Instruction_O::setMetadata(core::Str_sp kind, MDNode_sp mdnode)
    {_G();
	this->wrappedPtr()->setMetadata(kind->get(),mdnode->wrappedPtr());
    }


    EXPOSE_CLASS(llvmo,Instruction_O);

    void Instruction_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<Instruction_O>()
	    .def("setMetadata",&Instruction_O::setMetadata)
	    .def("terminatorInstP",&Instruction_O::terminatorInstP)
	    ;
    };

    void Instruction_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };


    bool Instruction_O::terminatorInstP() const
    {_G();
	return llvm::TerminatorInst::classof(this->wrappedPtr());
    }

}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,StoreInst_O);

void StoreInst_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<StoreInst_O>()
;
};

	   void StoreInst_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,FenceInst_O);

void FenceInst_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<FenceInst_O>()
;
};

	   void FenceInst_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,AtomicCmpXchgInst_O);

void AtomicCmpXchgInst_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<AtomicCmpXchgInst_O>()
;
};

	   void AtomicCmpXchgInst_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,AtomicRMWInst_O);

void AtomicRMWInst_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<AtomicRMWInst_O>()
;
};

	   void AtomicRMWInst_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,PHINode_O);

void PHINode_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<PHINode_O>()
	.def("addIncoming",&llvm::PHINode::addIncoming)
;
};

	   void PHINode_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,CallInst_O);

void CallInst_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<CallInst_O>()
;
};

	   void CallInst_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
    EXPOSE_CLASS(llvmo,LandingPadInst_O);

    void LandingPadInst_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<LandingPadInst_O>()
	    .def("setCleanup", &llvm::LandingPadInst::setCleanup)
	    .def("isCleanup",&llvm::LandingPadInst::isCleanup)
	    .def("addClause",&llvm::LandingPadInst::addClause)
	    ;
    };

    void LandingPadInst_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,UnaryInstruction_O);

void UnaryInstruction_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<UnaryInstruction_O>()
;
};

	   void UnaryInstruction_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
    EXPOSE_CLASS(llvmo,AllocaInst_O);

    void AllocaInst_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<AllocaInst_O>()
	    .def("setAlignment",&AllocaInst_O::ExternalType::setAlignment)
	    ;
    };

    void AllocaInst_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,VAArgInst_O);

void VAArgInst_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<VAArgInst_O>()
;
};

	   void VAArgInst_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,LoadInst_O);

void LoadInst_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<LoadInst_O>()
;
};

	   void LoadInst_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,TerminatorInst_O);

void TerminatorInst_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<TerminatorInst_O>()
;
};

	   void TerminatorInst_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,BranchInst_O);

void BranchInst_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<BranchInst_O>()
;
};

	   void BranchInst_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
    EXPOSE_CLASS(llvmo,SwitchInst_O);

    void SwitchInst_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<SwitchInst_O>()
	    .def("addCase",&SwitchInst_O::addCase)
	    ;
    };

    void SwitchInst_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };


    void SwitchInst_O::addCase(ConstantInt_sp onVal, BasicBlock_sp dest)
    {
	this->wrappedPtr()->addCase(onVal->wrappedPtr(),dest->wrappedPtr());
    }

}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
    EXPOSE_CLASS(llvmo,IndirectBrInst_O);

    void IndirectBrInst_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<IndirectBrInst_O>()
	    .def("addDestination",&llvm::IndirectBrInst::addDestination)
	    ;
    };

    void IndirectBrInst_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,InvokeInst_O);

void InvokeInst_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<InvokeInst_O>()
;
};

	   void InvokeInst_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
EXPOSE_CLASS(llvmo,ResumeInst_O);

void ResumeInst_O::exposeCando(core::Lisp_sp lisp)
{_G();
    core::externalClass_<ResumeInst_O>()
;
};

	   void ResumeInst_O::exposePython(core::Lisp_sp lisp)
	   {_G();
	       IMPLEMENT_ME();
           };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
    EXPOSE_CLASS(llvmo,UnreachableInst_O);

    void UnreachableInst_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<UnreachableInst_O>()
	    ;
    };

    void UnreachableInst_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}


namespace llvmo
{
    EXPOSE_CLASS(llvmo,ReturnInst_O);

    void ReturnInst_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<ReturnInst_O>()
	    ;
    };

    void ReturnInst_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo





namespace llvmo
{
    ConstantFP_sp ConstantFP_O::create(llvm::ConstantFP* ptr) {_G(); return core::RP_Create_wrapped<ConstantFP_O,llvm::ConstantFP*>(ptr);}
    ;
}


namespace llvmo
{
    EXPOSE_CLASS(llvmo,ConstantFP_O);

    void ConstantFP_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<ConstantFP_O>()
	    ;
	llvm::ConstantFP* (*fx1)(llvm::LLVMContext&, const llvm::APFloat&) = &llvm::ConstantFP::get;
	core::af_def(LlvmoPkg,"constantFpGet",fx1);
	llvm::Constant* (*fx2)(llvm::Type*,double) = &llvm::ConstantFP::get;
	core::af_def(LlvmoPkg,"constantFpGetTypeDouble",fx2);
	llvm::Constant* (*fx3)(llvm::Type*,llvm::StringRef) = &llvm::ConstantFP::get;
	core::af_def(LlvmoPkg,"constantFpGetTypeStringref",fx3);
    };

    void ConstantFP_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };


    string ConstantFP_O::__repr__() const
    {_G();
	stringstream ss;
	llvm::APFloat const& val = this->wrappedPtr()->getValueAPF();
	llvm::SmallVector<char,100> svistr;
	val.toString(svistr);
	std::string str(svistr.data(), svistr.size());
	ss << "#<" << this->_instanceClass()->classNameAsString() << " " << str << ">";
	return ss.str();
    }

}; // llvmo
namespace llvmo
{
}








namespace llvmo
{
    ConstantInt_sp ConstantInt_O::create(llvm::ConstantInt* ptr) {_G(); return core::RP_Create_wrapped<ConstantInt_O,llvm::ConstantInt*>(ptr);}
    ;


    EXPOSE_CLASS(llvmo,ConstantInt_O);

    void ConstantInt_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<ConstantInt_O>()
	    ;
	llvm::ConstantInt* (*fx1)(llvm::LLVMContext&, const llvm::APInt&) = &llvm::ConstantInt::get;
	core::af_def(LlvmoPkg,"constantIntGet",fx1);
    };

    void ConstantInt_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };


    string ConstantInt_O::__repr__() const
    {_G();
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " " << this->wrappedPtr()->getValue().toString(10,true) << ">";
	return ss.str();
    }
}; // llvmo




namespace llvmo
{
    UndefValue_sp UndefValue_O::create(llvm::UndefValue* ptr) {_G(); return core::RP_Create_wrapped<UndefValue_O,llvm::UndefValue*>(ptr);}
    ;


    EXPOSE_CLASS(llvmo,UndefValue_O);

    void UndefValue_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<UndefValue_O>()
	    ;
	core::af_def(LlvmoPkg,"UndefValueGet",llvm::UndefValue::get);
    };

    void UndefValue_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };


    string UndefValue_O::__repr__() const
    {_G();
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() <<  ">";
	return ss.str();
    }
}; // llvmo







namespace llvmo
{
    ConstantPointerNull_sp ConstantPointerNull_O::create(llvm::ConstantPointerNull* ptr) {_G(); return core::RP_Create_wrapped<ConstantPointerNull_O,llvm::ConstantPointerNull*>(ptr);}
    ;


    EXPOSE_CLASS(llvmo,ConstantPointerNull_O);

    void ConstantPointerNull_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<ConstantPointerNull_O>()
	    ;
	core::af_def(LlvmoPkg,"ConstantPointerNullGet",llvm::ConstantPointerNull::get);
    };

    void ConstantPointerNull_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };


    string ConstantPointerNull_O::__repr__() const
    {_G();
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() <<  ">";
	return ss.str();
    }
}; // llvmo














namespace llvmo
{
}















namespace llvmo
{

#define ARGS_APFloat_O_makeAPFloatFloat "(value)"
#define DECL_APFloat_O_makeAPFloatFloat ""
#define DOCS_APFloat_O_makeAPFloatFloat ""
    APFloat_sp APFloat_O::makeAPFloatFloat(core::SingleFloat_sp value)
    {_G();
        GC_ALLOCATE(APFloat_O,self );
	self->_value = llvm::APFloat(value->get());
	return self;
    };


#define ARGS_APFloat_O_makeAPFloatDouble "(value)"
#define DECL_APFloat_O_makeAPFloatDouble ""
#define DOCS_APFloat_O_makeAPFloatDouble ""
    APFloat_sp APFloat_O::makeAPFloatDouble(core::DoubleFloat_sp value)
    {_G();
        GC_ALLOCATE(APFloat_O,self );
	self->_value = llvm::APFloat(value->get());
	return self;
    };





}


namespace llvmo
{
    EXPOSE_CLASS(llvmo,APFloat_O);

    void APFloat_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<APFloat_O>()
	    ;
        core::af_def(LlvmoPkg,"makeAPFloatFloat",&APFloat_O::makeAPFloatFloat,ARGS_APFloat_O_makeAPFloatFloat,DECL_APFloat_O_makeAPFloatFloat,DOCS_APFloat_O_makeAPFloatFloat);
        core::af_def(LlvmoPkg,"makeAPFloatDouble",&APFloat_O::makeAPFloatDouble,ARGS_APFloat_O_makeAPFloatDouble,DECL_APFloat_O_makeAPFloatDouble,DOCS_APFloat_O_makeAPFloatDouble);
    };

    void APFloat_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo


namespace llvmo
{

    APInt_sp APInt_O::create(llvm::APInt api)
    {
        GC_ALLOCATE(APInt_O,self );
        self->_value = api;
        return self;
    }
        


#define ARGS_APInt_O_makeAPInt "(value)"
#define DECL_APInt_O_makeAPInt ""
#define DOCS_APInt_O_makeAPInt ""
    APInt_sp APInt_O::makeAPInt(core::Integer_sp value)
    {_G();
        GC_ALLOCATE(APInt_O,self );
	if (af_fixnumP(value) )
	{
	    core::Fixnum_sp fixnum_value = value.as<core::Fixnum_O>();
	    self->_value = llvm::APInt(core::Fixnum_O::number_of_bits(),fixnum_value->as_int(),true);
	} else
	{
	    // It's a bignum so lets convert the bignum to a string and put it into an APInt
	    char* asString = NULL;
	    core::Bignum_sp bignum_value = value.as<core::Bignum_O>();
	    mpz_class& mpz_val = bignum_value->ref();
	    int mpz_size_in_bits = mpz_sizeinbase(mpz_val.get_mpz_t(),2);
	    asString = ::mpz_get_str(NULL,10,mpz_val.get_mpz_t());
	    self->_value = llvm::APInt(mpz_size_in_bits,llvm::StringRef(asString,strlen(asString)),10);
	    free(asString);
	}
	return self;
    }
}



namespace llvmo
{
#define ARGS_APInt_O_makeAPInt1 "(value)"
#define DECL_APInt_O_makeAPInt1 ""
#define DOCS_APInt_O_makeAPInt1 ""
    APInt_sp APInt_O::makeAPInt1(core::T_sp value)
    {_G();
        GC_ALLOCATE(APInt_O,self );
	if (af_fixnumP(value) )
	{
	    core::Fixnum_sp fixnum_value = value.as<core::Fixnum_O>();
	    self->_value = llvm::APInt(1,fixnum_value->as_int()&1,false);
	} else
	{
	    if ( value.isTrue() )
	    {
		self->_value = llvm::APInt(1,1,false);
	    } else
	    {
		self->_value = llvm::APInt(1,0,false);
	    }
	}
	return self;
    }


#define ARGS_APInt_O_makeAPIntWidth "(value bitswide signed)"
#define DECL_APInt_O_makeAPIntWidth ""
#define DOCS_APInt_O_makeAPIntWidth ""
    APInt_sp APInt_O::makeAPIntWidth(core::Integer_sp value, uint width, bool sign)
    {_G();
        GC_ALLOCATE(APInt_O,self );
	llvm::APInt    apint;
	int	       numbits;
	if (af_fixnumP(value) )
	{
	    core::Fixnum_sp fixnum_value = value.as<core::Fixnum_O>();
	    if ( !sign && fixnum_value->get() < 0 ) 
	    {
		SIMPLE_ERROR(BF("You tried to create an unsigned APInt32 with the negative value: %d") % fixnum_value->get() );
	    }
	    apint = llvm::APInt(width,fixnum_value->as_int(),sign);
	    numbits = core::Fixnum_O::number_of_bits();
	} else
	{
	    // It's a bignum so lets convert the bignum to a string and put it into an APInt
	    char* asString = NULL;
	    core::Bignum_sp bignum_value = value.as<core::Bignum_O>();
	    mpz_class& mpz_val = bignum_value->ref();
	    int mpz_size_in_bits = mpz_sizeinbase(mpz_val.get_mpz_t(),2);
	    asString = ::mpz_get_str(NULL,10,mpz_val.get_mpz_t());
	    apint = llvm::APInt(width,llvm::StringRef(asString,strlen(asString)),10);
	    free(asString);
	    numbits = mpz_size_in_bits;
	    if ( numbits > width )
	    {
		string numstr = asString;
		SIMPLE_ERROR(BF("You tried to create an unsigned I%d with a value[%s] that requires %d bits to represent") % width % numstr % mpz_size_in_bits );
	    }
	}
	if ( numbits < width )
	{
	    apint = apint.zext(width);
	}
	self->_value = apint;
	return self;
    }



#define ARGS_APInt_O_makeAPInt32 "(value)"
#define DECL_APInt_O_makeAPInt32 ""
#define DOCS_APInt_O_makeAPInt32 ""
    APInt_sp APInt_O::makeAPInt32(core::Integer_sp value)
    {_G();
	return APInt_O::makeAPIntWidth(value,32,true);
    }


#define ARGS_APInt_O_makeAPInt64 "(value)"
#define DECL_APInt_O_makeAPInt64 ""
#define DOCS_APInt_O_makeAPInt64 ""
    APInt_sp APInt_O::makeAPInt64(core::Integer_sp value)
    {_G();
	return APInt_O::makeAPIntWidth(value,64,true);
    }



}






namespace llvmo
{
    EXPOSE_CLASS(llvmo,APInt_O);

    void APInt_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<APInt_O>()
            .def("toString",&APInt_O::toString)
	    ;
	SYMBOL_EXPORT_SC_(LlvmoPkg,makeAPInt1);
	SYMBOL_EXPORT_SC_(LlvmoPkg,makeAPInt);
	SYMBOL_EXPORT_SC_(LlvmoPkg,makeAPWidth);
	SYMBOL_EXPORT_SC_(LlvmoPkg,makeAP32);
	SYMBOL_EXPORT_SC_(LlvmoPkg,makeAP64);
        core::af_def(LlvmoPkg,"makeAPInt1",&APInt_O::makeAPInt1,ARGS_APInt_O_makeAPInt1,DECL_APInt_O_makeAPInt1,DOCS_APInt_O_makeAPInt1);
        core::af_def(LlvmoPkg,"makeAPInt",&APInt_O::makeAPInt,ARGS_APInt_O_makeAPInt,DECL_APInt_O_makeAPInt,DOCS_APInt_O_makeAPInt);
        core::af_def(LlvmoPkg,"makeAPIntWidth",&APInt_O::makeAPIntWidth,ARGS_APInt_O_makeAPIntWidth,DECL_APInt_O_makeAPIntWidth,DOCS_APInt_O_makeAPIntWidth);
        core::af_def(LlvmoPkg,"makeAPInt32",&APInt_O::makeAPInt32,ARGS_APInt_O_makeAPInt32,DECL_APInt_O_makeAPInt32,DOCS_APInt_O_makeAPInt32);
        core::af_def(LlvmoPkg,"makeAPInt64",&APInt_O::makeAPInt64,ARGS_APInt_O_makeAPInt64,DECL_APInt_O_makeAPInt64,DOCS_APInt_O_makeAPInt64);
    };

    void APInt_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };


    string APInt_O::toString(int radix, bool isigned) const
    {
        return this->_value.toString(radix,isigned);
    }

    string APInt_O::__repr__() const
    {_G();
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " ";
	ss << this->_value.toString(10,true);
	ss << ">";
	return ss.str();
    }
}; // llvmo











namespace llvmo
{
    EXPOSE_CLASS(llvmo,IRBuilderBase_O);

    void IRBuilderBase_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	void (llvm::IRBuilderBase::*SetInsertPoint_1)(llvm::BasicBlock*)  = &llvm::IRBuilderBase::SetInsertPoint;
	void (llvm::IRBuilderBase::*SetInsertPoint_2)(llvm::Instruction*)  = &llvm::IRBuilderBase::SetInsertPoint;
//	void (llvm::IRBuilderBase::*SetInsertPoint_3)(llvm::Use&)  = &llvm::IRBuilderBase::SetInsertPoint;

	core::externalClass_<IRBuilderBase_O>()
	    .def("SetInsertPointBasicBlock", SetInsertPoint_1)
	    .def("SetInsertPointInstruction", SetInsertPoint_2)
	    .def("GetInsertBlock", &IRBuilderBase_O::ExternalType::GetInsertBlock)
	    .def("restoreIP",&IRBuilderBase_O::restoreIP)
	    .def("saveIP",&IRBuilderBase_O::saveIP)
	    .def("SetCurrentDebugLocation",&IRBuilderBase_O::SetCurrentDebugLocation)
	    ;
    };

    void IRBuilderBase_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };




    void IRBuilderBase_O::restoreIP(InsertPoint_sp insertPoint)
    {_G();
	this->wrappedPtr()->restoreIP(insertPoint->insertPoint());
    }

    InsertPoint_sp IRBuilderBase_O::saveIP()
    {_G();
	llvm::IRBuilderBase::InsertPoint ip = this->wrappedPtr()->saveIP();
	InsertPoint_sp oip = InsertPoint_O::create(ip);
	return oip;
    }

    void IRBuilderBase_O::SetCurrentDebugLocation(DebugLoc_sp loc)
    {_G();
//	llvm::DebugLoc dlold = this->wrappedPtr()->getCurrentDebugLocation();
//	printf("                       old DebugLocation: %d\n", dlold.getLine() );
	llvm::DebugLoc& dl = loc->debugLoc();
//	printf("%s:%d IRBuilderBase_O::SetCurrentDebugLoc changing to line %d\n", __FILE__, __LINE__, dl.getLine() );
	this->wrappedPtr()->SetCurrentDebugLocation(dl);
//	llvm::DebugLoc dlnew = this->wrappedPtr()->getCurrentDebugLocation();
//	printf("                       new DebugLocation: %d\n", dlnew.getLine() );
    }

}; // llvmo



namespace llvmo
{
#define ARGS_IRBuilder_O_make "(context)"
#define DECL_IRBuilder_O_make ""
#define DOCS_IRBuilder_O_make ""
    IRBuilder_sp IRBuilder_O::make(LLVMContext_sp context)
    {_G();
        GC_ALLOCATE(IRBuilder_O,self );
	ASSERT(&(llvm::getGlobalContext()) == context->wrappedPtr());
	self->set_wrapped(new llvm::IRBuilder<>(*(context->wrappedPtr())));
	return self;
    };





    llvm::InvokeInst* IRBuilder_O::CreateInvoke(llvm::Value* Callee, llvm::BasicBlock *NormalDest, llvm::BasicBlock *UnwindDest, core::Cons_sp Args, const llvm::Twine &Name)
    {
	vector<llvm::Value*> vector_Args;
	for ( core::Cons_sp cur=Args; cur.notnilp(); cur=cCdr(cur) )
	{
	    Value_sp val = oCar(cur).as<Value_O>();
	    if ( val.nilp() )
	    {
		vector_Args.push_back(NULL);
	    } else
	    {
		vector_Args.push_back(val->wrappedPtr());
	    }
	}
	llvm::ArrayRef<llvm::Value*> array_ref_vector_Args(vector_Args);
	return this->wrappedPtr()->CreateInvoke(Callee,NormalDest,UnwindDest,array_ref_vector_Args,Name);
    }



    llvm::Value* IRBuilder_O::CreateInBoundsGEP(llvm::Value* Ptr, core::Cons_sp IdxList, const llvm::Twine &Name)
    {
	vector<llvm::Value*> vector_IdxList;
	for ( core::Cons_sp cur=IdxList; cur.notnilp(); cur=cCdr(cur) )
	{
	    vector_IdxList.push_back(oCar(cur).as<Value_O>()->wrappedPtr());
	}
	llvm::ArrayRef<llvm::Value*> array_ref_vector_IdxList(vector_IdxList);
	return this->wrappedPtr()->CreateInBoundsGEP(Ptr,array_ref_vector_IdxList,Name);
    }



    llvm::Value* IRBuilder_O::CreateExtractValue(llvm::Value* Ptr, core::Cons_sp IdxList, const llvm::Twine& Name)
    {
	vector<unsigned int> vector_IdxList;
	for ( core::Cons_sp cur=IdxList; cur.notnilp(); cur=cCdr(cur) )
	{
	    vector_IdxList.push_back(oCar(cur).as<core::Fixnum_O>()->get());
	}
	llvm::ArrayRef<unsigned int> array_ref_vector_IdxList(vector_IdxList);
	return this->wrappedPtr()->CreateExtractValue(Ptr,array_ref_vector_IdxList,Name);
    }

    llvm::Value* IRBuilder_O::CreateInsertValue(llvm::Value* Agg, llvm::Value* Val, core::Cons_sp IdxList, const llvm::Twine& Name)
    {
	vector<unsigned int> vector_IdxList;
	for ( core::Cons_sp cur=IdxList; cur.notnilp(); cur=cCdr(cur) )
	{
	    vector_IdxList.push_back(oCar(cur).as<core::Fixnum_O>()->get());
	}
	llvm::ArrayRef<unsigned int> array_ref_vector_IdxList(vector_IdxList);
	return this->wrappedPtr()->CreateInsertValue(Agg,Val,array_ref_vector_IdxList,Name);
    }


    string IRBuilder_O::__repr__() const
    {
	IRBuilder_O* irbuilder = const_cast<IRBuilder_O*>(this);
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " ";
	llvm::BasicBlock* bb = irbuilder->wrappedPtr()->GetInsertBlock();
	if ( bb )
	{
	    ss << " :insert-block-name " << bb->getName().data();
	    llvm::Function* func = bb->getParent();
	    if ( func )
	    {
		ss << " :function " << func->getName().data();
	    } else
	    {
		ss << " :function UNDEFINED-FUNCTION! ";
	    }
	} else
	{ 
	    ss << " :insert-block-name UNDEFINED-BASIC_BLOCK! ";
	}
	ss << " >";
	return ss.str();
    }



    EXPOSE_CLASS(llvmo,IRBuilder_O);

    void IRBuilder_O::exposeCando(core::Lisp_sp lisp)
    {_G();
//	void (llvm::IRBuilder::*SetInsertPoint_1)(llvm::BasicBlock*)  = &llvm::IRBuilder::SetInsertPoint;
//	void (llvm::IRBuilder::*SetInsertPoint_2)(llvm::Instruction*)  = &llvm::IRBuilder::SetInsertPoint;
//	void (llvm::IRBuilder::*SetInsertPoint_3)(llvm::Use&)  = &llvm::IRBuilder::SetInsertPoint;

	core::externalClass_<IRBuilder_O> irbuilder;
	irbuilder
	    .def("CreateRet",&IRBuilder_O::ExternalType::CreateRet)
	    .def("CreateRetVoid",&IRBuilder_O::ExternalType::CreateRetVoid)
	    .def("CreateBr",&IRBuilder_O::ExternalType::CreateBr)
	    .def("CreateCondBr",&IRBuilder_O::ExternalType::CreateCondBr)
	    .def("CreateSwitch",&IRBuilder_O::ExternalType::CreateSwitch)
	    .def("CreateIndirectBr",&IRBuilder_O::ExternalType::CreateIndirectBr)
//	    .def("CreateInvoke3",&IRBuilder_O::ExternalType::CreateInvoke3)
	    .def("CreateInvoke",&IRBuilder_O::CreateInvoke)
	    .def("CreateResume",&IRBuilder_O::ExternalType::CreateResume)
	    .def("CreateUnreachable",&IRBuilder_O::ExternalType::CreateUnreachable)
	    .def("CreateAdd",&IRBuilder_O::ExternalType::CreateAdd)
	    .def("CreateNSWAdd",&IRBuilder_O::ExternalType::CreateNSWAdd)
	    .def("CreateNUWAdd",&IRBuilder_O::ExternalType::CreateNUWAdd)
	    .def("CreateFAdd",&IRBuilder_O::ExternalType::CreateFAdd)
	    .def("CreateSub",&IRBuilder_O::ExternalType::CreateSub)
	    .def("CreateNSWSub",&IRBuilder_O::ExternalType::CreateNSWSub)
	    .def("CreateNUWSub",&IRBuilder_O::ExternalType::CreateNUWSub)
	    .def("CreateFSub",&IRBuilder_O::ExternalType::CreateFSub)
	    .def("CreateMul",&IRBuilder_O::ExternalType::CreateMul)
	    .def("CreateNSWMul",&IRBuilder_O::ExternalType::CreateNSWMul)
	    .def("CreateNUWMul",&IRBuilder_O::ExternalType::CreateNUWMul)
	    .def("CreateFMul",&IRBuilder_O::ExternalType::CreateFMul)
	    .def("CreateUDiv",&IRBuilder_O::ExternalType::CreateUDiv)
	    .def("CreateExactUDiv",&IRBuilder_O::ExternalType::CreateExactUDiv)
	    .def("CreateSDiv",&IRBuilder_O::ExternalType::CreateSDiv)
	    .def("CreateExactSDiv",&IRBuilder_O::ExternalType::CreateExactSDiv)
	    .def("CreateFDiv",&IRBuilder_O::ExternalType::CreateFDiv)
	    .def("CreateURem",&IRBuilder_O::ExternalType::CreateURem)
	    .def("CreateSRem",&IRBuilder_O::ExternalType::CreateSRem)
	    .def("CreateFRem",&IRBuilder_O::ExternalType::CreateFRem)
	    ;

#define	AVOID_OVERLOAD(irb,ret,fn,suffix,args) \
	{											\
	    ret (IRBuilder_O::ExternalType::*fn) args = &IRBuilder_O::ExternalType::fn; 		\
	    irb.def(#fn #suffix,fn);								\
	}

	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateShl,_value_value,(llvm::Value*,llvm::Value*,const llvm::Twine&,bool,bool));
	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateShl,_value_apint,(llvm::Value*,llvm::APInt const&,const llvm::Twine&,bool,bool));
	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateShl,_value_uint64,(llvm::Value*,uint64_t,const llvm::Twine&,bool,bool));

//	llvm::Value* (IRBuilder_O::ExternalType::&CreateShl_value_value)(llvm::Value*,llvm::Value*,const llvm::Twine&,bool,bool) = IRBuilder_O::ExternalType::CreateShl;
//	llvm::Value* (IRBuilder_O::ExternalType::&CreateShl_value_apint)(llvm::Value*,const llvm::APInt&,const llvm::Twine&,bool,bool) = IRBuilder_O::ExternalType::CreateShl;
//	llvm::Value* (IRBuilder_O::ExternalType::&CreateShl_value_uint64)(llvm::Value*,uint64_t,const llvm::Twine&,bool,bool) = IRBuilder_O::ExternalType::CreateShl;
	
	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateLShr,_value_value,(llvm::Value*,llvm::Value*,const llvm::Twine&,bool));
	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateLShr,_value_apint,(llvm::Value*,llvm::APInt const&,const llvm::Twine&,bool));
	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateLShr,_value_uint64,(llvm::Value*,uint64_t,const llvm::Twine&,bool));

	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateAShr,_value_value,(llvm::Value*,llvm::Value*,const llvm::Twine&,bool));
	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateAShr,_value_apint,(llvm::Value*,llvm::APInt const&,const llvm::Twine&,bool));
	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateAShr,_value_uint64,(llvm::Value*,uint64_t,const llvm::Twine&,bool));

	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateAnd,_value_value,(llvm::Value*,llvm::Value*,const llvm::Twine&));
	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateAnd,_value_apint,(llvm::Value*,llvm::APInt const&,const llvm::Twine&));
	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateAnd,_value_uint64,(llvm::Value*,uint64_t,const llvm::Twine&));

	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateOr,_value_value,(llvm::Value*,llvm::Value*,const llvm::Twine&));
	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateOr,_value_apint,(llvm::Value*,llvm::APInt const&,const llvm::Twine&));
	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateOr,_value_uint64,(llvm::Value*,uint64_t,const llvm::Twine&));

	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateXor,_value_value,(llvm::Value*,llvm::Value*,const llvm::Twine&));
	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateXor,_value_apint,(llvm::Value*,llvm::APInt const&,const llvm::Twine&));
	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateXor,_value_uint64,(llvm::Value*,uint64_t,const llvm::Twine&));
	irbuilder
	    .def("CreateNeg",&IRBuilder_O::ExternalType::CreateNeg)
	    .def("CreateNSWNeg",&IRBuilder_O::ExternalType::CreateNSWNeg)
	    .def("CreateNUWNeg",&IRBuilder_O::ExternalType::CreateNUWNeg)
	    .def("CreateFNeg",&IRBuilder_O::ExternalType::CreateFNeg)
	    .def("CreateNot",&IRBuilder_O::ExternalType::CreateNot)
	    .def("CreateAlloca",&IRBuilder_O::ExternalType::CreateAlloca)
	    ;

//	AVOID_OVERLOAD(irbuilder,llvm::LoadInst*,CreateLoad,_value_string,(llvm::Value*,const char*));
	AVOID_OVERLOAD(irbuilder,llvm::LoadInst*,CreateLoad,_value_twine,(llvm::Value*,const llvm::Twine&));
	AVOID_OVERLOAD(irbuilder,llvm::LoadInst*,CreateLoad,_value_bool_twine,(llvm::Value*,bool, const llvm::Twine&));
//	    .def("CreateLoad",&IRBuilder_O::ExternalType::CreateLoad)
//	    .def("CreateLoad",&IRBuilder_O::ExternalType::CreateLoad)
//	    .def("CreateLoad",&IRBuilder_O::ExternalType::CreateLoad)
	irbuilder
	    .def("CreateStore",&IRBuilder_O::ExternalType::CreateStore)
	    .def("CreateFence",&IRBuilder_O::ExternalType::CreateFence)
	    .def("CreateAtomicCmpXchg",&IRBuilder_O::ExternalType::CreateAtomicCmpXchg)
	    .def("CreateAtomicRMW",&IRBuilder_O::ExternalType::CreateAtomicRMW)
	    .def("CreateConstGEP1-32",&IRBuilder_O::ExternalType::CreateConstGEP1_32)
	    .def("CreateConstInBoundsGEP1-32",&IRBuilder_O::ExternalType::CreateConstInBoundsGEP1_32)
	    .def("CreateConstGEP2-32",&IRBuilder_O::ExternalType::CreateConstGEP2_32)
	    .def("CreateConstInBoundsGEP2-32",&IRBuilder_O::ExternalType::CreateConstInBoundsGEP2_32)
	    .def("CreateConstGEP1-64",&IRBuilder_O::ExternalType::CreateConstGEP1_64)
	    .def("CreateConstInBoundsGEP1-64",&IRBuilder_O::ExternalType::CreateConstInBoundsGEP1_64)
	    .def("CreateConstGEP2-64",&IRBuilder_O::ExternalType::CreateConstGEP2_64)
	    .def("CreateConstInBoundsGEP2-64",&IRBuilder_O::ExternalType::CreateConstInBoundsGEP2_64)
	    .def("CreateStructGEP",&IRBuilder_O::ExternalType::CreateStructGEP)
	    .def("CreateGlobalStringPtr",&IRBuilder_O::ExternalType::CreateGlobalStringPtr)
	    .def("CreateTrunc",&IRBuilder_O::ExternalType::CreateTrunc)
	    .def("CreateZExt",&IRBuilder_O::ExternalType::CreateZExt)
	    .def("CreateSExt",&IRBuilder_O::ExternalType::CreateSExt)
	    .def("CreateFPToUI",&IRBuilder_O::ExternalType::CreateFPToUI)
	    .def("CreateFPToSI",&IRBuilder_O::ExternalType::CreateFPToSI)
	    .def("CreateUIToFP",&IRBuilder_O::ExternalType::CreateUIToFP)
	    .def("CreateSIToFP",&IRBuilder_O::ExternalType::CreateSIToFP)
	    .def("CreateFPTrunc",&IRBuilder_O::ExternalType::CreateFPTrunc)
	    .def("CreateFPExt",&IRBuilder_O::ExternalType::CreateFPExt)
	    .def("CreatePtrToInt",&IRBuilder_O::ExternalType::CreatePtrToInt)
	    .def("CreateIntToPtr",&IRBuilder_O::ExternalType::CreateIntToPtr)
	    .def("CreateBitCast",&IRBuilder_O::ExternalType::CreateBitCast)
	    .def("CreateZExtOrBitCast",&IRBuilder_O::ExternalType::CreateZExtOrBitCast)
	    .def("CreateSExtOrBitCast",&IRBuilder_O::ExternalType::CreateSExtOrBitCast)
	    .def("CreateTruncOrBitCast",&IRBuilder_O::ExternalType::CreateTruncOrBitCast)
	    .def("CreateCast",&IRBuilder_O::ExternalType::CreateCast)
	    .def("CreatePointerCast",&IRBuilder_O::ExternalType::CreatePointerCast)
	    ;

#if 0
	irbuilder
	    .def("CreateIntCast",&IRBuilder_O::ExternalType::CreateIntCast)
	    ;
#endif
	irbuilder
	    .def("CreateFPCast",&IRBuilder_O::ExternalType::CreateFPCast)
	    .def("CreateICmpEQ",&IRBuilder_O::ExternalType::CreateICmpEQ)
	    .def("CreateICmpNE",&IRBuilder_O::ExternalType::CreateICmpNE)
	    .def("CreateICmpUGT",&IRBuilder_O::ExternalType::CreateICmpUGT)
	    .def("CreateICmpUGE",&IRBuilder_O::ExternalType::CreateICmpUGE)
	    .def("CreateICmpULT",&IRBuilder_O::ExternalType::CreateICmpULT)
	    .def("CreateICmpULE",&IRBuilder_O::ExternalType::CreateICmpULE)
	    .def("CreateICmpSGT",&IRBuilder_O::ExternalType::CreateICmpSGT)
	    .def("CreateICmpSGE",&IRBuilder_O::ExternalType::CreateICmpSGE)
	    .def("CreateICmpSLT",&IRBuilder_O::ExternalType::CreateICmpSLT)
	    .def("CreateICmpSLE",&IRBuilder_O::ExternalType::CreateICmpSLE)
	    .def("CreateFCmpOEQ",&IRBuilder_O::ExternalType::CreateFCmpOEQ)
	    .def("CreateFCmpOGT",&IRBuilder_O::ExternalType::CreateFCmpOGT)
	    .def("CreateFCmpOGE",&IRBuilder_O::ExternalType::CreateFCmpOGE)
	    .def("CreateFCmpOLT",&IRBuilder_O::ExternalType::CreateFCmpOLT)
	    .def("CreateFCmpOLE",&IRBuilder_O::ExternalType::CreateFCmpOLE)
	    .def("CreateFCmpONE",&IRBuilder_O::ExternalType::CreateFCmpONE)
	    .def("CreateFCmpORD",&IRBuilder_O::ExternalType::CreateFCmpORD)
	    .def("CreateFCmpUNO",&IRBuilder_O::ExternalType::CreateFCmpUNO)
	    .def("CreateFCmpUEQ",&IRBuilder_O::ExternalType::CreateFCmpUEQ)
	    .def("CreateFCmpUGT",&IRBuilder_O::ExternalType::CreateFCmpUGT)
	    .def("CreateFCmpUGE",&IRBuilder_O::ExternalType::CreateFCmpUGE)
	    .def("CreateFCmpULT",&IRBuilder_O::ExternalType::CreateFCmpULT)
	    .def("CreateFCmpULE",&IRBuilder_O::ExternalType::CreateFCmpULE)
	    .def("CreateFCmpUNE",&IRBuilder_O::ExternalType::CreateFCmpUNE)
	    .def("CreateICmp",&IRBuilder_O::ExternalType::CreateICmp)
	    .def("CreateFCmp",&IRBuilder_O::ExternalType::CreateFCmp)
	    .def("CreatePHI",&IRBuilder_O::ExternalType::CreatePHI)
	    ;


	AVOID_OVERLOAD(irbuilder,llvm::CallInst*,CreateCall,0,(llvm::Value*,const llvm::Twine&));
	AVOID_OVERLOAD(irbuilder,llvm::CallInst*,CreateCall,1,(llvm::Value*,llvm::Value*,const llvm::Twine&));
	// Add the one for variable numbers of arguments

	irbuilder
	    .def("CreateCall2",&IRBuilder_O::ExternalType::CreateCall2)
	    .def("CreateCall3",&IRBuilder_O::ExternalType::CreateCall3)
	    .def("CreateCall4",&IRBuilder_O::ExternalType::CreateCall4)
	    .def("CreateCall5",&IRBuilder_O::ExternalType::CreateCall5)
	    ;

	irbuilder
	    .def("CreateSelect",&IRBuilder_O::ExternalType::CreateSelect)
	    .def("CreateVAArg",&IRBuilder_O::ExternalType::CreateVAArg)
	    .def("CreateExtractElement",&IRBuilder_O::ExternalType::CreateExtractElement)
	    .def("CreateInsertElement",&IRBuilder_O::ExternalType::CreateInsertElement)
	    .def("CreateShuffleVector",&IRBuilder_O::ExternalType::CreateShuffleVector)
	    .def("CreateLandingPad",&IRBuilder_O::ExternalType::CreateLandingPad)
	    .def("CreateIsNull",&IRBuilder_O::ExternalType::CreateIsNull)
	    .def("CreateIsNotNull",&IRBuilder_O::ExternalType::CreateIsNotNull)
	    .def("CreatePtrDiff",&IRBuilder_O::ExternalType::CreatePtrDiff)
	    ;

	irbuilder
	    .def("CreateBinOp",&IRBuilder_O::ExternalType::CreateBinOp)
	    .def("CreateInBoundsGEP",&IRBuilder_O::CreateInBoundsGEP)
	    .def("CreateExtractValue",&IRBuilder_O::CreateExtractValue)
	    .def("CreateInsertValue",&IRBuilder_O::CreateInsertValue)
//	    .def("CreateCall",&IRBuilder_O::ExternalType::CreateCall)
	    ;

	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateGEP,0,(llvm::Value*,llvm::Value*,const llvm::Twine&));
//	AVOID_OVERLOAD(irbuilder,llvm::Value*,CreateGEP,Array,(llvm::Value*,llvm::ArrayRef<llvm::Value*>,const llvm::Twine&));

	// Problem instructions that will have to be handled with IRBuilder_O methods
#if 0
	irbuilder
	    .def("CreateAggregateRet",&IRBuilder_O::ExternalType::CreateAggregateRet)
	    ;

#endif

	core::af_def(LlvmoPkg,"make-irbuilder",&IRBuilder_O::make,ARGS_IRBuilder_O_make,DECL_IRBuilder_O_make,DOCS_IRBuilder_O_make);	

    };

    void IRBuilder_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo





namespace llvmo
{
    EXPOSE_CLASS(llvmo,Argument_O);

    void Argument_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<Argument_O>()
//	    .def("addAttr",&llvm::Argument::addAttr)
//	    .def("removeAttr",&llvm::Argument::removeAttr)
	    .def("hasStructRetAttr",&llvm::Argument::hasStructRetAttr)
	    .def("hasNoAliasAttr",&llvm::Argument::hasNoAliasAttr)
	    .def("hasNestAttr",&llvm::Argument::hasNestAttr)
	    .def("hasByValAttr",&llvm::Argument::hasByValAttr)
	    ;
    };

    void Argument_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };

}; // llvmo





namespace llvmo
{

    MDNode_sp MDNode_O::get(LLVMContext_sp context, core::Cons_sp values)
    {_G();
	vector<llvm::Value*> valvec;
	for ( core::Cons_sp cur=values;cur.notnilp(); cur=cCdr(cur) )
	{
	    llvm::Value* val = oCar(cur).as<Value_O>()->wrappedPtr();
	    valvec.push_back(val);
	}
	llvm::MDNode* mdnode = llvm::MDNode::get(*context->wrappedPtr(),valvec);
	MDNode_sp omd = core::RP_Create_wrapped<llvmo::MDNode_O,llvm::MDNode*>(mdnode);
	return omd;
    }



    EXPOSE_CLASS(llvmo,MDNode_O);

    void MDNode_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<MDNode_O>()
	    ;
	SYMBOL_EXPORT_SC_(LlvmoPkg,mdnodeGet);
	core::af_def(LlvmoPkg,"mdnodeGet",&MDNode_O::get);
    };

    void MDNode_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}




namespace llvmo
{

    MDString_sp MDString_O::get(LLVMContext_sp context, core::Str_sp str)
    {_G();
	llvm::MDString* mdstr = llvm::MDString::get(*context->wrappedPtr(),str->get());
	MDString_sp omd = core::RP_Create_wrapped<llvmo::MDString_O,llvm::MDString*>(mdstr);
	return omd;
    }



    EXPOSE_CLASS(llvmo,MDString_O);

    void MDString_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<MDString_O>()
	    ;
	SYMBOL_EXPORT_SC_(LlvmoPkg,mdnodeGet);
	core::af_def(LlvmoPkg,"mdstringGet",&MDString_O::get);
    };

    void MDString_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo
namespace llvmo
{
}







namespace llvmo
{



    EXPOSE_CLASS(llvmo,NamedMDNode_O);

    void NamedMDNode_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<NamedMDNode_O>()
	    .def("addOperand",&NamedMDNode_O::addOperand)
	    ;
    };

    void NamedMDNode_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };
}; // llvmo






namespace llvmo
{

    
    
#define ARGS_af_FunctionCreate "(ty linkage n m)"
#define DECL_af_FunctionCreate ""
#define DOCS_af_FunctionCreate "FunctionCreate - wrapps llvm::Function::Create"
    Function_sp af_FunctionCreate(FunctionType_sp tysp, llvm::GlobalValue::LinkageTypes linkage, core::Str_sp nsp, Module_sp modulesp )
    {_G();
	translate::from_object<llvm::FunctionType*> ty(tysp);
	translate::from_object<llvm::Module*> m(modulesp);
//        printf("%s:%d FunctionCreate %s with linkage %d\n", __FILE__, __LINE__, nsp->get().c_str(), linkage);
	llvm::Function* func = llvm::Function::Create(ty._v,linkage,nsp->get(),m._v);
	Function_sp funcsp = translate::to_object<llvm::Function*>::convert(func).as<Function_O>();
	return funcsp;
    };

    core::Cons_sp Function_O::getArgumentList()
    {_G();
	ql::list l(_lisp);
	llvm::Function::ArgumentListType& args = this->wrappedPtr()->getArgumentList();
	return translate::to_object<llvm::Function::ArgumentListType&>::convert(args);
    }


    void Function_O::appendBasicBlock(BasicBlock_sp basicBlock)
    {
	this->wrappedPtr()->getBasicBlockList().push_back(basicBlock->wrappedPtr());
    }
    


    EXPOSE_CLASS(llvmo,Function_O);

    void Function_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<Function_O>()
	    .def("getArgumentList", &Function_O::getArgumentList)
	    .def("eraseFromParent", &llvm::Function::eraseFromParent)
	    .def("empty", &llvm::Function::empty)
	    .def("arg_size", &llvm::Function::arg_size)
	    .def("setDoesNotThrow", &llvm::Function::setDoesNotThrow)
	    .def("doesNotThrow",&llvm::Function::doesNotThrow)
	    .def("setDoesNotReturn", &llvm::Function::setDoesNotReturn)
	    .def("doesNotReturn",&llvm::Function::doesNotReturn)
	    .def("appendBasicBlock",&Function_O::appendBasicBlock)
	    .def("setLiterals",&Function_O::setLiterals)
	    .def("literals",&Function_O::literals)
	    ;
//	core::af_def(LlvmoPkg,"functionCreate",&llvm::Function::Create);
	core::af_def(LlvmoPkg,"functionCreate",&af_FunctionCreate);
    };

    void Function_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };


    void Function_O::setLiterals(core::LoadTimeValues_sp ltv)
    {_G();
	this->_RunTimeValues = ltv;
    }

    core::LoadTimeValues_sp Function_O::literals() const
    {
	return this->_RunTimeValues;
    }

}; // llvmo










namespace llvmo
{
    EXPOSE_CLASS(llvmo,BasicBlock_O);

    void BasicBlock_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	typedef llvm::Function* (llvm::BasicBlock::*getParent_type)();
	getParent_type getParent_notConst = &llvm::BasicBlock::getParent;
	core::externalClass_<BasicBlock_O>()
	    .def("getParent",getParent_notConst)
	    .def("BasicBlockBack",&BasicBlock_O::back)
	    .def("BasicBlockEmpty",&BasicBlock_O::empty)
	    ;
	core::af_def(LlvmoPkg,"BasicBlock-Create",&llvm::BasicBlock::Create,"(context &optional (name \"\") parent basic_block)","","");
    };

    void BasicBlock_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };

    bool BasicBlock_O::empty()
    {_G();
	return this->wrappedPtr()->empty();
    }

    Instruction_sp BasicBlock_O::back()
    {_G();
	llvm::Instruction& inst = this->wrappedPtr()->back();
	return core::RP_Create_wrapped<Instruction_O,llvm::Instruction*>(&inst);
    }

}; // llvmo




namespace llvmo
{
    EXPOSE_CLASS(llvmo,Type_O);

    bool Type_O::equal(core::T_sp obj) const
    {
	if ( Type_sp t = obj.asOrNull<Type_O>() )
	{
	    return t->_ptr == this->_ptr;
	}
	return false;
    }


    string Type_O::__repr__() const
    {_G();
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " ";
	string str;
	llvm::raw_string_ostream ro(str);
	this->wrappedPtr()->print(ro);
	ss << ro.str() << ">";
	return ss.str();
    }


#define	ARGS_PointerType_O_getPointerTo "((self type) &optional (addressSpace 0))"
#define DECL_PointerType_O_getPointerTo ""
#define DOCS_PointerType_O_getPointerTo "Return a PointerType to the llvm Type"
    PointerType_sp Type_O::getPointerTo(int addressSpace)
    {_G();
	llvm::PointerType* ptrType = this->wrappedPtr()->getPointerTo();
	return translate::to_object<llvm::PointerType*>::convert(ptrType);
    }



    void Type_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<Type_O>()
	    .def("type-get-pointer-to",&Type_O::getPointerTo,ARGS_PointerType_O_getPointerTo,DECL_PointerType_O_getPointerTo,DOCS_PointerType_O_getPointerTo)
	    ;
	core::af_def(LlvmoPkg,"type-get-float-ty",&llvm::Type::getFloatTy);
	core::af_def(LlvmoPkg,"type-get-double-ty",&llvm::Type::getDoubleTy);
	core::af_def(LlvmoPkg,"type-get-void-ty",&llvm::Type::getVoidTy);
	core::af_def(LlvmoPkg,"type-get-int1-ty",&llvm::Type::getInt1Ty);
	core::af_def(LlvmoPkg,"type-get-int8-ty",&llvm::Type::getInt8Ty);
	core::af_def(LlvmoPkg,"type-get-int32-ty",&llvm::Type::getInt32Ty);
	core::af_def(LlvmoPkg,"type-get-int64-ty",&llvm::Type::getInt64Ty);
	core::af_def(LlvmoPkg,"type-get-int8-ptr-ty",&llvm::Type::getInt8PtrTy);
	core::af_def(LlvmoPkg,"type-get-int32-ptr-ty",&llvm::Type::getInt32PtrTy);
	core::af_def(LlvmoPkg,"type-get-int64-ptr-ty",&llvm::Type::getInt64PtrTy);
    };

    void Type_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };

}; // llvmo











namespace llvmo
{
    EXPOSE_CLASS(llvmo,FunctionType_O);




#define ARGS_FunctionType_O_get "(result &optional params is_var_arg)"
#define DECL_FunctionType_O_get ""
#define DOCS_FunctionType_O_get "Docs for FunctionType get"
    core::T_sp FunctionType_O::get(core::T_sp result_type, core::Sequence_sp params, core::T_sp is_var_arg)
    {_G();
	translate::from_object<llvm::Type*> r(result_type);
	bool iva = is_var_arg.isTrue();
	llvm::FunctionType* result = NULL;
	if ( params.nilp() )
	{
	    result = llvm::FunctionType::get(r._v,iva);
	} else
	{
	    vector<llvm::Type*> vparams;
	    convert_sequence_types_to_vector(params,vparams);
	    llvm::ArrayRef<llvm::Type*> p(vparams);
	    result = llvm::FunctionType::get(r._v,p,iva);
	}
	return translate::to_object<llvm::FunctionType*>::convert(result);
    };








    void FunctionType_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<FunctionType_O>()
	    ;
	core::af_def(LlvmoPkg,"function-type-get",&FunctionType_O::get,ARGS_FunctionType_O_get,DECL_FunctionType_O_get,DOCS_FunctionType_O_get);
    };

    void FunctionType_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };



};






namespace llvmo
{
    EXPOSE_CLASS(llvmo,IntegerType_O);


    void IntegerType_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<IntegerType_O>()
	    ;
    };

    void IntegerType_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };



};



namespace llvmo
{
    EXPOSE_CLASS(llvmo,CompositeType_O);


    void CompositeType_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<CompositeType_O>()
	    ;
    };

    void CompositeType_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };



};




namespace llvmo
{
    EXPOSE_CLASS(llvmo,StructType_O);



    
    

#define ARGS_StructType_O_make "(context &key elements name is-packed)"
#define DECL_StructType_O_make ""
#define DOCS_StructType_O_make "make StructType args: context &key name elements is-packed"
    StructType_sp StructType_O::make(LLVMContext_sp context, core::Sequence_sp elements, core::Str_sp name, core::T_sp isPacked )
    {_G();
	llvm::StructType* result = NULL;
	translate::from_object<llvm::StringRef> srname(name);
	if ( elements.notnilp() )
	{
	    vector<llvm::Type*> velements;
	    convert_sequence_types_to_vector(elements,velements);
	    llvm::ArrayRef<llvm::Type*> p(velements);
	    result = llvm::StructType::create(*(context->wrappedPtr()),p,srname._v,isPacked.isTrue());
	} else
	{
	    result = llvm::StructType::create(*(context->wrappedPtr()),srname._v);
	}
	return translate::to_object<llvm::StructType*>::convert(result);
    }



    StructType_sp StructType_O::get(LLVMContext_sp context, core::Sequence_sp elements, bool isPacked )
    {_G();
	llvm::StructType* result = NULL;
	if ( elements.notnilp() )
	{
	    vector<llvm::Type*> velements;
	    convert_sequence_types_to_vector(elements,velements);
	    llvm::ArrayRef<llvm::Type*> p(velements);
	    result = llvm::StructType::get(*(context->wrappedPtr()),p,isPacked);
	} else
	{
	    result = llvm::StructType::get(*(context->wrappedPtr()),isPacked);
	}
	return translate::to_object<llvm::StructType*>::convert(result);
    }



    void StructType_O::setBody( core::Sequence_sp elements, core::T_sp isPacked )
    {_G();
	llvm::StructType* st = this->wrapped();
	if ( elements.notnilp() )
	{
	    vector<llvm::Type*> velements;
	    convert_sequence_types_to_vector(elements,velements);
	    llvm::ArrayRef<llvm::Type*> p(velements);
	    st->setBody(p,isPacked.isTrue());
	}
    }




    void StructType_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<StructType_O>()
	    .def("setBody",&StructType_O::setBody)
	    ;

	core::af_def(LlvmoPkg,"struct-type-create",&StructType_O::make,ARGS_StructType_O_make, DECL_StructType_O_make, DOCS_StructType_O_make );
	core::af_def(LlvmoPkg,"struct-type-get",&StructType_O::get);
    };

    void StructType_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };





};



namespace llvmo
{
    EXPOSE_CLASS(llvmo,SequentialType_O);


    void SequentialType_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<SequentialType_O>()
	    ;
    };

    void SequentialType_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };



};




namespace llvmo
{

#define ARGS_ArrayType_O_get "(element-type num-elements)"
#define DECL_ArrayType_O_get ""
#define DOCS_ArrayType_O_get "Docs for ArrayType get"
    ArrayType_sp ArrayType_O::get(Type_sp elementType, uint64_t numElements)
    {_G();
	ArrayType_sp at = ArrayType_O::create();
	llvm::ArrayType* llvm_at = llvm::ArrayType::get(elementType->wrappedPtr(),numElements);
	at->set_wrapped(llvm_at);
	return at;
    }

    EXPOSE_CLASS(llvmo,ArrayType_O);


    void ArrayType_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<ArrayType_O>()
	    ;
	core::af_def(LlvmoPkg,"array-type-get",&ArrayType_O::get,ARGS_ArrayType_O_get,DECL_ArrayType_O_get,DOCS_ArrayType_O_get);

    };

    void ArrayType_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };



};






namespace llvmo
{
    EXPOSE_CLASS(llvmo,PointerType_O);


    void PointerType_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<PointerType_O>()
	    ;
    };

    void PointerType_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };



};






namespace llvmo
{
    EXPOSE_CLASS(llvmo,VectorType_O);


    void VectorType_O::exposeCando(core::Lisp_sp lisp)
    {_G();
	core::externalClass_<VectorType_O>()
	    ;
    };

    void VectorType_O::exposePython(core::Lisp_sp lisp)
    {_G();
	IMPLEMENT_ME();
    };



};































namespace llvmo
{










#if 0    
    
#define ARGS_af_createDebugIRPass "(filename-postfix)"
#define DECL_af_createDebugIRPass ""
#define DOCS_af_createDebugIRPass "createDebugIRPass"
    llvm::ModulePass* af_createDebugIRPass(core::Str_sp filenamePostfix)
    {_G();
	return llvm::createDebugIRPass(filenamePostfix->get());
    };


#endif










void initialize_llvmo_expose()
{_G();
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();


    SYMBOL_SC_(LlvmoPkg,STARglobal_value_linkage_typesSTAR);
    SYMBOL_EXPORT_SC_(LlvmoPkg,ExternalLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,AvailableExternallyLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,LinkOnceAnyLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,LinkOnceODRLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,LinkOnceODRAutoHideLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,WeakAnyLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,WeakODRLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,AppendingLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,InternalLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,PrivateLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,LinkerPrivateLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,LinkerPrivateWeakLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,DLLImportLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,DLLExportLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,ExternalWeakLinkage);
    SYMBOL_EXPORT_SC_(LlvmoPkg,CommonLinkage);
    core::enum_<llvm::GlobalValue::LinkageTypes>(_sym_STARglobal_value_linkage_typesSTAR,"llvm::GlobalValue::LinkageTypes")
	.value(_sym_ExternalLinkage,llvm::GlobalValue::ExternalLinkage)
	.value(_sym_AvailableExternallyLinkage,llvm::GlobalValue::AvailableExternallyLinkage)
	.value(_sym_LinkOnceAnyLinkage,llvm::GlobalValue::LinkOnceAnyLinkage)
	.value(_sym_LinkOnceODRLinkage,llvm::GlobalValue::LinkOnceODRLinkage)
//	.value(_sym_LinkOnceODRAutoHideLinkage,llvm::GlobalValue::LinkOnceODRAutoHideLinkage)
	.value(_sym_WeakAnyLinkage,llvm::GlobalValue::WeakAnyLinkage)
	.value(_sym_WeakODRLinkage,llvm::GlobalValue::WeakODRLinkage)
	.value(_sym_AppendingLinkage,llvm::GlobalValue::AppendingLinkage)
	.value(_sym_InternalLinkage,llvm::GlobalValue::InternalLinkage)
	.value(_sym_PrivateLinkage,llvm::GlobalValue::PrivateLinkage)
//	.value(_sym_LinkerPrivateLinkage,llvm::GlobalValue::LinkerPrivateLinkage)
//	.value(_sym_LinkerPrivateWeakLinkage,llvm::GlobalValue::LinkerPrivateWeakLinkage)
//	.value(_sym_DLLImportLinkage,llvm::GlobalValue::DLLImportLinkage)
//	.value(_sym_DLLExportLinkage,llvm::GlobalValue::DLLExportLinkage)
	.value(_sym_ExternalWeakLinkage,llvm::GlobalValue::ExternalWeakLinkage)
	.value(_sym_CommonLinkage,llvm::GlobalValue::CommonLinkage)
	;
    SYMBOL_EXPORT_SC_(LlvmoPkg,verifyFunction);
    Defun(verifyFunction);

//
// Compiler optimization passes
//
//    core::af_def(LlvmoPkg,"createDebugIRPass",&llvmo::af_createDebugIRPass);
    core::af_def(LlvmoPkg,"createAliasAnalysisCounterPass",&llvm::createAliasAnalysisCounterPass);
    core::af_def(LlvmoPkg,"createAAEvalPass",&llvm::createAAEvalPass);
    core::af_def(LlvmoPkg,"createScalarEvolutionAliasAnalysisPass",&llvm::createScalarEvolutionAliasAnalysisPass);
//    core::af_def(LlvmoPkg,"createProfileLoaderPass",&llvm::createProfileLoaderPass);
//    core::af_def(LlvmoPkg,"createNoProfileInfoPass",&llvm::createNoProfileInfoPass);
//    core::af_def(LlvmoPkg,"createProfileEstimatorPass",&llvm::createProfileEstimatorPass);
//    core::af_def(LlvmoPkg,"createProfileVerifierPass",&llvm::createProfileVerifierPass);
//    core::af_def(LlvmoPkg,"createPathProfileLoaderPass",&llvm::createPathProfileLoaderPass);
//    core::af_def(LlvmoPkg,"createNoPathProfileInfoPass",&llvm::createNoPathProfileInfoPass);
//    core::af_def(LlvmoPkg,"createPathProfileVerifierPass",&llvm::createPathProfileVerifierPass);
    core::af_def(LlvmoPkg,"createLazyValueInfoPass",&llvm::createLazyValueInfoPass);
    core::af_def(LlvmoPkg,"createInstCountPass",&llvm::createInstCountPass);
//    core::af_def(LlvmoPkg,"createDbgInfoPrinterPass",&llvm::createDbgInfoPrinterPass);
    core::af_def(LlvmoPkg,"createRegionInfoPass",&llvm::createRegionInfoPass);
    core::af_def(LlvmoPkg,"createModuleDebugInfoPrinterPass",&llvm::createModuleDebugInfoPrinterPass);
    core::af_def(LlvmoPkg,"createMemDepPrinter",&llvm::createMemDepPrinter);
//    core::af_def(LlvmoPkg,"createInstructionCombiningPass",&llvm::createInstructionCombiningPass);
//    core::af_def(LlvmoPkg,"createReassociatePass",&llvm::createReassociatePass);
//    core::af_def(LlvmoPkg,"createPostDomTree",&llvm::createPostDomTree);
    core::af_def(LlvmoPkg,"InitializeNativeTarget",&llvm::InitializeNativeTarget);


    core::af_def(LlvmoPkg,"createAggressiveDCEPass",&llvm::createAggressiveDCEPass);
    core::af_def(LlvmoPkg,"createCFGSimplificationPass",&llvm::createCFGSimplificationPass);
    core::af_def(LlvmoPkg,"createDeadStoreEliminationPass",&llvm::createDeadStoreEliminationPass);
    core::af_def(LlvmoPkg,"createGVNPass",&llvm::createGVNPass);
    core::af_def(LlvmoPkg,"createIndVarSimplifyPass",&llvm::createIndVarSimplifyPass);
    core::af_def(LlvmoPkg,"createInstructionCombiningPass",&llvm::createInstructionCombiningPass);
    core::af_def(LlvmoPkg,"createJumpThreadingPass",&llvm::createJumpThreadingPass);
    core::af_def(LlvmoPkg,"createLICMPass",&llvm::createLICMPass);
    core::af_def(LlvmoPkg,"createLoopDeletionPass",&llvm::createLoopDeletionPass);
    core::af_def(LlvmoPkg,"createLoopIdiomPass",&llvm::createLoopIdiomPass);
    core::af_def(LlvmoPkg,"createLoopRotatePass",&llvm::createLoopRotatePass);
    core::af_def(LlvmoPkg,"createLoopUnrollPass",&llvm::createLoopUnrollPass);
    core::af_def(LlvmoPkg,"createLoopUnswitchPass",&llvm::createLoopUnswitchPass);
    core::af_def(LlvmoPkg,"createMemCpyOptPass",&llvm::createMemCpyOptPass);
    core::af_def(LlvmoPkg,"createPromoteMemoryToRegisterPass",&llvm::createPromoteMemoryToRegisterPass);
    core::af_def(LlvmoPkg,"createReassociatePass",&llvm::createReassociatePass);
    core::af_def(LlvmoPkg,"createSCCPPass",&llvm::createSCCPPass);
    core::af_def(LlvmoPkg,"createScalarReplAggregatesPass",&llvm::createScalarReplAggregatesPass);
//    core::af_def(LlvmoPkg,"createScalarReplAggregatesPassSSA",&llvm::createScalarReplAggregatesPassSSA);
//    core::af_def(LlvmoPkg,"createScalarReplAggregatesPassWithThreshold",&llvm::createScalarReplAggregatesPassWithThreshold);
//    core::af_def(LlvmoPkg,"createSimplifyLibCallsPass",&llvm::createSimplifyLibCallsPass);
    core::af_def(LlvmoPkg,"createTailCallEliminationPass",&llvm::createTailCallEliminationPass);
    core::af_def(LlvmoPkg,"createConstantPropagationPass",&llvm::createConstantPropagationPass);
//    core::af_def(LlvmoPkg,"createDemoteMemoryToRegisterPass",&llvm::createDemoteMemoryToRegisterPass);
    core::af_def(LlvmoPkg,"createVerifierPass",&llvm::createVerifierPass);
    core::af_def(LlvmoPkg,"createCorrelatedValuePropagationPass",&llvm::createCorrelatedValuePropagationPass);
    core::af_def(LlvmoPkg,"createEarlyCSEPass",&llvm::createEarlyCSEPass);
    core::af_def(LlvmoPkg,"createLowerExpectIntrinsicPass",&llvm::createLowerExpectIntrinsicPass);
    core::af_def(LlvmoPkg,"createTypeBasedAliasAnalysisPass",&llvm::createTypeBasedAliasAnalysisPass);
    core::af_def(LlvmoPkg,"createBasicAliasAnalysisPass",&llvm::createBasicAliasAnalysisPass);








    SYMBOL_EXPORT_SC_(LlvmoPkg,STARatomic_orderingSTAR);
    SYMBOL_EXPORT_SC_(LlvmoPkg,NotAtomic);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Unordered);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Monotonic);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Acquire);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Release);
    SYMBOL_EXPORT_SC_(LlvmoPkg,AquireRelease);
    SYMBOL_EXPORT_SC_(LlvmoPkg,SequentiallyConsistent);
    core::enum_<llvm::AtomicOrdering>(_sym_STARatomic_orderingSTAR,"llvm::AtomicOrdering")
	.value(_sym_NotAtomic,llvm::NotAtomic)
	.value(_sym_Unordered,llvm::Unordered)
	.value(_sym_Monotonic,llvm::Monotonic)
	.value(_sym_Acquire,llvm::Acquire)
	.value(_sym_Release,llvm::Release)
//	.value(_sym_AquireRelease,llvm::AtomicOrdering::AquireRelease)
	.value(_sym_SequentiallyConsistent,llvm::SequentiallyConsistent)
	;

    SYMBOL_EXPORT_SC_(LlvmoPkg,STARsynchronization_scopeSTAR);
    SYMBOL_EXPORT_SC_(LlvmoPkg,SingleThread);
    SYMBOL_EXPORT_SC_(LlvmoPkg,CrossThread);
    core::enum_<llvm::SynchronizationScope>(_sym_STARsynchronization_scopeSTAR,"llvm::SynchronizationScope")
	.value(_sym_SingleThread,llvm::SingleThread)
	.value(_sym_CrossThread,llvm::CrossThread)
	;

    SYMBOL_EXPORT_SC_(LlvmoPkg,STARAtomicRMWInstBinOpSTAR);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Xchg);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Add);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Sub);
    SYMBOL_EXPORT_SC_(LlvmoPkg,And);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Nand);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Or);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Xor);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Max);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Min);
    SYMBOL_EXPORT_SC_(LlvmoPkg,UMax);
    SYMBOL_EXPORT_SC_(LlvmoPkg,UMin);
    core::enum_<llvm::AtomicRMWInst::BinOp>(_sym_STARAtomicRMWInstBinOpSTAR,"llvm::AtomicRMWInst::BinOp")
	.value(_sym_Xchg,llvm::AtomicRMWInst::Xchg)
	.value(_sym_Add,llvm::AtomicRMWInst::Add)
	.value(_sym_Sub,llvm::AtomicRMWInst::Sub)
	.value(_sym_And,llvm::AtomicRMWInst::And)
	.value(_sym_Nand,llvm::AtomicRMWInst::Nand)
	.value(_sym_Or,llvm::AtomicRMWInst::Or)
	.value(_sym_Xor,llvm::AtomicRMWInst::Xor)
	.value(_sym_Max,llvm::AtomicRMWInst::Max)
	.value(_sym_Min,llvm::AtomicRMWInst::Min)
	.value(_sym_UMax,llvm::AtomicRMWInst::UMax)
	.value(_sym_UMin,llvm::AtomicRMWInst::UMin)
	;


    SYMBOL_EXPORT_SC_(LlvmoPkg,Add);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FAdd);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Sub);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FSub);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Mul);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FMul);
    SYMBOL_EXPORT_SC_(LlvmoPkg,UDiv);
    SYMBOL_EXPORT_SC_(LlvmoPkg,SDiv);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FDiv);
    SYMBOL_EXPORT_SC_(LlvmoPkg,URem);
    SYMBOL_EXPORT_SC_(LlvmoPkg,SRem);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FRem);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Shl);
    SYMBOL_EXPORT_SC_(LlvmoPkg,LShr);
    SYMBOL_EXPORT_SC_(LlvmoPkg,AShr);
    SYMBOL_EXPORT_SC_(LlvmoPkg,And);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Or);
    SYMBOL_EXPORT_SC_(LlvmoPkg,Xor);
    SYMBOL_EXPORT_SC_(LlvmoPkg,STARBinaryOpsSTAR);
    core::enum_<llvm::Instruction::BinaryOps>(_sym_STARBinaryOpsSTAR,"llvm::Instruction::BinaryOps")
	.value(_sym_Add  , llvm::Instruction::Add  )
	.value(_sym_FAdd , llvm::Instruction::FAdd )
	.value(_sym_Sub  , llvm::Instruction::Sub  )
	.value(_sym_FSub , llvm::Instruction::FSub )
	.value(_sym_Mul  , llvm::Instruction::Mul  )
	.value(_sym_FMul , llvm::Instruction::FMul )
	.value(_sym_UDiv , llvm::Instruction::UDiv )
	.value(_sym_SDiv , llvm::Instruction::SDiv )
	.value(_sym_FDiv , llvm::Instruction::FDiv )
	.value(_sym_URem , llvm::Instruction::URem )
	.value(_sym_SRem , llvm::Instruction::SRem )
	.value(_sym_FRem , llvm::Instruction::FRem )
	.value(_sym_Shl  , llvm::Instruction::Shl  )
	.value(_sym_LShr , llvm::Instruction::LShr )
	.value(_sym_AShr , llvm::Instruction::AShr )
	.value(_sym_And  , llvm::Instruction::And  )
	.value(_sym_Or   , llvm::Instruction::Or   )
	.value(_sym_Xor  , llvm::Instruction::Xor  )
	;


    SYMBOL_EXPORT_SC_(LlvmoPkg,Trunc);
    SYMBOL_EXPORT_SC_(LlvmoPkg,ZExt);
    SYMBOL_EXPORT_SC_(LlvmoPkg,SExt);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FPToUI);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FPToSI);
    SYMBOL_EXPORT_SC_(LlvmoPkg,UIToFP);
    SYMBOL_EXPORT_SC_(LlvmoPkg,SIToFP);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FPTrunc);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FPExt);
    SYMBOL_EXPORT_SC_(LlvmoPkg,PtrToInt);
    SYMBOL_EXPORT_SC_(LlvmoPkg,IntToPtr);
    SYMBOL_EXPORT_SC_(LlvmoPkg,BitCast);
    SYMBOL_EXPORT_SC_(LlvmoPkg,STARInstructionCastOpsSTAR);
    core::enum_<llvm::Instruction::CastOps>(_sym_STARInstructionCastOpsSTAR,"llvm::Instruction::CastOps")
	.value(_sym_Trunc   , llvm::Instruction::Trunc   )
	.value(_sym_ZExt    , llvm::Instruction::ZExt    )
	.value(_sym_SExt    , llvm::Instruction::SExt    )
	.value(_sym_FPToUI  , llvm::Instruction::FPToUI  )
	.value(_sym_FPToSI  , llvm::Instruction::FPToSI  )
	.value(_sym_UIToFP  , llvm::Instruction::UIToFP  )
	.value(_sym_SIToFP  , llvm::Instruction::SIToFP  )
	.value(_sym_FPTrunc , llvm::Instruction::FPTrunc )
	.value(_sym_FPExt   , llvm::Instruction::FPExt   )
	.value(_sym_PtrToInt, llvm::Instruction::PtrToInt)
	.value(_sym_IntToPtr, llvm::Instruction::IntToPtr)
	.value(_sym_BitCast , llvm::Instruction::BitCast )
	;







    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_FALSE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_OEQ);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_OGT);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_OGE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_OLT);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_OLE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_ONE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_ORD);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_UNO);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_UEQ);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_UGT);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_UGE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_ULT);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_ULE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_UNE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_TRUE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FIRST_FCMP_PREDICATE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_PREDICATE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,FCMP_PREDICATE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,ICMP_EQ);
    SYMBOL_EXPORT_SC_(LlvmoPkg,ICMP_NE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,ICMP_UGT);
    SYMBOL_EXPORT_SC_(LlvmoPkg,ICMP_UGE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,ICMP_ULT);
    SYMBOL_EXPORT_SC_(LlvmoPkg,ICMP_ULE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,ICMP_SGT);
    SYMBOL_EXPORT_SC_(LlvmoPkg,ICMP_SGE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,ICMP_SLT);
    SYMBOL_EXPORT_SC_(LlvmoPkg,ICMP_SLE);
    SYMBOL_EXPORT_SC_(LlvmoPkg,STARCmpInstPredicateSTAR);
    core::enum_<llvm::CmpInst::Predicate>(_sym_STARCmpInstPredicateSTAR,"llvm::CmpInst::Predicate")
	.value(_sym_FCMP_FALSE,llvm::CmpInst::FCMP_FALSE)
	.value(_sym_FCMP_OEQ,llvm::CmpInst::FCMP_OEQ)
	.value(_sym_FCMP_OGT,llvm::CmpInst::FCMP_OGT)
	.value(_sym_FCMP_OGE,llvm::CmpInst::FCMP_OGE)
	.value(_sym_FCMP_OLT,llvm::CmpInst::FCMP_OLT)
	.value(_sym_FCMP_OLE,llvm::CmpInst::FCMP_OLE)
	.value(_sym_FCMP_ONE,llvm::CmpInst::FCMP_ONE)
	.value(_sym_FCMP_ORD,llvm::CmpInst::FCMP_ORD)
	.value(_sym_FCMP_UNO,llvm::CmpInst::FCMP_UNO)
	.value(_sym_FCMP_UEQ,llvm::CmpInst::FCMP_UEQ)
	.value(_sym_FCMP_UGT,llvm::CmpInst::FCMP_UGT)
	.value(_sym_FCMP_UGE,llvm::CmpInst::FCMP_UGE)
	.value(_sym_FCMP_ULT,llvm::CmpInst::FCMP_ULT)
	.value(_sym_FCMP_ULE,llvm::CmpInst::FCMP_ULE)
	.value(_sym_FCMP_UNE,llvm::CmpInst::FCMP_UNE)
	.value(_sym_FCMP_TRUE,llvm::CmpInst::FCMP_TRUE)
	.value(_sym_ICMP_EQ,llvm::CmpInst::ICMP_EQ)
	.value(_sym_ICMP_NE,llvm::CmpInst::ICMP_NE)
	.value(_sym_ICMP_UGT,llvm::CmpInst::ICMP_UGT)
	.value(_sym_ICMP_UGE,llvm::CmpInst::ICMP_UGE)
	.value(_sym_ICMP_ULT,llvm::CmpInst::ICMP_ULT)
	.value(_sym_ICMP_ULE,llvm::CmpInst::ICMP_ULE)
	.value(_sym_ICMP_SGT,llvm::CmpInst::ICMP_SGT)
	.value(_sym_ICMP_SGE,llvm::CmpInst::ICMP_SGE)
	.value(_sym_ICMP_SLT,llvm::CmpInst::ICMP_SLT)
	.value(_sym_ICMP_SLE,llvm::CmpInst::ICMP_SLE)
    ;
    SYMBOL_EXPORT_SC_(LlvmoPkg,valid);
    Defun(valid);

    SYMBOL_EXPORT_SC_(LlvmoPkg,makeStringGlobal);
    Defun(makeStringGlobal);

    SYMBOL_EXPORT_SC_(LlvmoPkg,valuep);
    Defun(valuep);

    SYMBOL_EXPORT_SC_(LlvmoPkg,parseBitcodeFile);
    Defun(parseBitcodeFile);

    SYMBOL_EXPORT_SC_(LlvmoPkg,writeBitcodeToFile);
    Defun(writeBitcodeToFile);

    SYMBOL_EXPORT_SC_(LlvmoPkg,writeIrToFile);
    Defun(writeIrToFile);

    SYMBOL_EXPORT_SC_(LlvmoPkg,llvm_value_p);
    Defun(llvm_value_p);
}


}; // llvmo

