#ifndef	debugInfoExpose_H //[
#define debugInfoExpose_H

#include "core/common.h"
#include "core/symbolToEnumConverter.h"
#include "core/str.h"
#include "core/ql.h"
//#include "llvm/DataLayout.h"

#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/PassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include <llvm/IR/Constants.h>
//#include "llvm/Support/IRBuilder.h"

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "core/foundation.h"
#include "core/object.h"
#include "core/metaClass.fwd.h"
#include "core/externalObject.h"
#include "core/lispVector.h"
#include "debugInfoExpose.fwd.h"
#include "core/loadTimeValues.fwd.h"
#include "core/vectorObjectsWithFillPtr.fwd.h"
#include "insertPoint.fwd.h"
#include "debugLoc.fwd.h"
#include "llvmo/llvmoPackage.h"
#include "llvmo/llvmoExpose.h"



namespace llvmo
{
    FORWARD(DebugInfo);
    class DebugInfo_O : public core::T_O
    {
	LISP_BASE1(core::T_O);
	LISP_CLASS(llvmo,LlvmoPkg,DebugInfo_O,"DebugInfo");
    public:
	virtual operator llvm::DIDescriptor*() {SUBIMP();};
	virtual operator llvm::DIType*() {SUBIMP();};
	virtual ~DebugInfo_O() {};
    }; // DebugInfo_O
}; // llvmo
TRANSLATE(llvmo::DebugInfo_O);




namespace llvmo
{
    FORWARD(DIDescriptor);
    class DIDescriptor_O : public DebugInfo_O
			 , public llvm::DIDescriptor
    {
	LISP_BASE1(DebugInfo_O);
	LISP_CLASS(llvmo,LlvmoPkg,DIDescriptor_O,"DIDescriptor");
    public:
	typedef llvm::DIDescriptor OtherType;
    public:
 	virtual operator llvm::DIDescriptor* () { return this;};
//	virtual llvm::DIDescriptor* operator ->() const { return (llvm::DIDescriptor*)(this);};
	DIDescriptor_O(const OtherType& val) : llvm::DIDescriptor(val) {};
	DIDescriptor_O() {};
	virtual ~DIDescriptor_O() {};
    }; // DIDescriptor_O
}; // llvmo
TRANSLATE(llvmo::DIDescriptor_O);

namespace translate
{
    template <>
    struct to_object<llvm::DIDescriptor>
    {
        static core::T_sp convert(const llvm::DIDescriptor& val)
        {_G();
	    GC_RESERVE_BEGIN(llvmo::DIDescriptor_O,obj) {
		GC_RESERVE_GET_VARIADIC(llvmo::DIDescriptor_O,obj,val);
	    } GC_RESERVE_END(llvmo::DIDescriptor_O,obj);
	    printf("to_object<llvm::DIDescriptor>\n");
	    return((obj));
	};
    };
    template <>
    struct from_object<llvm::DIDescriptor,std::true_type>
    {
	typedef llvm::DIDescriptor& DeclareType;
        DeclareType _v;
	from_object(T_P object) : _v(*(object.as<llvmo::DebugInfo_O>()->operator llvm::DIDescriptor*())) {};
    };
};



namespace llvmo
{
    FORWARD(DIScope);
    class DIScope_O : public DebugInfo_O
		    , public llvm::DIScope
    {
	LISP_BASE1(DebugInfo_O)
	LISP_CLASS(llvmo,LlvmoPkg,DIScope_O,"discope");
    private:
	typedef llvm::DIScope OtherType;
    public:
	virtual operator llvm::DIDescriptor* () { return this;};
	DIScope_O(const OtherType& val) : llvm::DIScope(val) {};
	DIScope_O() {};
	virtual ~DIScope_O() {}

    }; // DIScope_O
}; // llvmo
TRANSLATE(llvmo::DIScope_O);
/* from_object translators */
/* to_object translators */

namespace translate
{
    template <>
    struct to_object<llvm::DIScope>
    {
        static core::T_sp convert(const llvm::DIScope& val)
        {_G();
	    GC_RESERVE_BEGIN(llvmo::DIScope_O,obj) {
		GC_RESERVE_GET_VARIADIC(llvmo::DIScope_O,obj,val);
	    } GC_RESERVE_END(llvmo::DIScope_O,obj);
	    return((obj));
	};
    };
    template <>
    struct from_object<llvm::DIScope,std::true_type>
    {
	typedef llvm::DIScope& DeclareType;
	DeclareType _v;
	from_object(T_P object) : _v(*object.as<llvmo::DIScope_O>()) {};
    };
};






















namespace llvmo
{
    FORWARD(DIArray);
    class DIArray_O : public DebugInfo_O
		    , public llvm::DIArray
    {
	LISP_BASE1(DebugInfo_O);
	LISP_CLASS(llvmo,LlvmoPkg,DIArray_O,"diarray");
	typedef llvm::DIArray OtherType;
    public:
	DIArray_O(const OtherType& val) : llvm::DIArray(val) {};
	DIArray_O() : Base() {};
	virtual ~DIArray_O() {}

    }; // DIArray_O
}; // llvmo
TRANSLATE(llvmo::DIArray_O);

namespace translate
{
    template <>
    struct to_object<llvm::DIArray>
    {
        static core::T_sp convert(const llvm::DIArray& val)
        {_G();
	    GC_RESERVE_BEGIN(llvmo::DIArray_O,obj) {
		GC_RESERVE_GET_VARIADIC(llvmo::DIArray_O,obj,val);
	    } GC_RESERVE_END(llvmo::DIArray_O,obj);
	    return((obj));
	};
    };
    template <>
    struct from_object<llvm::DIArray,std::true_type>
    {
	typedef llvm::DIArray& DeclareType;
	DeclareType _v;
	from_object(T_P object) : _v(*(object.as<llvmo::DIArray_O>())) {};
    };
};















namespace llvmo
{
    FORWARD(DIFile);
    class DIFile_O : public DebugInfo_O
		   , public llvm::DIFile
    {
	LISP_BASE1(DebugInfo_O);
	LISP_CLASS(llvmo,LlvmoPkg,DIFile_O,"difile");
	typedef llvm::DIFile OtherType;
    private:
    public:
	virtual operator llvm::DIDescriptor* () { return this;};
	DIFile_O() : Base() {};
	DIFile_O(const llvm::DIFile& val) : llvm::DIFile(val) {};
	virtual ~DIFile_O() {}

    }; // DIFile_O
}; // llvmo
TRANSLATE(llvmo::DIFile_O);
/* from_object translators */
/* to_object translators */

namespace translate
{
    template <>
    struct to_object<llvm::DIFile>
    {
        static core::T_sp convert(const llvm::DIFile& val)
        {_G();
	    GC_RESERVE_BEGIN(llvmo::DIFile_O,obj) {
		GC_RESERVE_GET_VARIADIC(llvmo::DIFile_O,obj,val);
	    } GC_RESERVE_END(llvmo::DIFile_O,obj);
	    return (obj);
	};
    };
    template <>
    struct from_object<llvm::DIFile,std::true_type>
    {
	typedef llvm::DIFile& DeclareType;
	DeclareType _v;
	from_object(T_P object) : _v(*object.as<llvmo::DIFile_O>()) {};
    };
};





namespace llvmo
{
    FORWARD(DISubprogram);
    class DISubprogram_O : public DebugInfo_O
			 , public llvm::DISubprogram
    {
	LISP_BASE1(DebugInfo_O);
	LISP_CLASS(llvmo,LlvmoPkg,DISubprogram_O,"DISubprogram");
	typedef llvm::DISubprogram OtherType;
    private:
    public:
 	virtual operator llvm::DIDescriptor* () { return this;};
	DISubprogram_O() : Base() {};
	DISubprogram_O(const OtherType& val) : OtherType(val) {};
	virtual ~DISubprogram_O() {}

    }; // DISubprogram_O
}; // llvmo
TRANSLATE(llvmo::DISubprogram_O);
/* from_object translators */
/* to_object translators */

namespace translate
{
    template <>
    struct to_object<llvm::DISubprogram>
    {
        static core::T_sp convert(const llvm::DISubprogram& val)
        {_G();
	    GC_RESERVE_BEGIN(llvmo::DISubprogram_O,obj) {
		GC_RESERVE_GET_VARIADIC(llvmo::DISubprogram_O,obj,val);
	    } GC_RESERVE_END(llvmo::DISubprogram_O,obj);
	    return (obj);
	};
    };
    template <>
    struct from_object<llvm::DISubprogram,std::true_type>
    {
	typedef llvm::DISubprogram& DeclareType;
	static DeclareType convert(core::T_sp object)
	{
	    ASSERT(object.pointerp());
	    llvm::DIDescriptor* didescriptor = object.as<llvmo::DebugInfo_O>()->operator llvm::DIDescriptor* ();
	    if ( didescriptor->isSubprogram() )
	    {
		return *static_cast<llvm::DISubprogram*>(didescriptor);
	    }
	    SIMPLE_ERROR(BF("Cannot convert %s to llvm::DISubprogram") % _rep_(object) );
	}
    };
};





namespace llvmo
{
    FORWARD(DIType);
    class DIType_O : public DebugInfo_O
		   , public llvm::DIType
    {
	LISP_BASE1(DebugInfo_O);
	LISP_CLASS(llvmo,LlvmoPkg,DIType_O,"DIType");
	typedef llvm::DIType OtherType;
    private:
    public:
 	virtual operator llvm::DIDescriptor* () { return this;};
 	virtual operator llvm::DIType* () { return this;};
	DIType_O() : Base() {};
	DIType_O(const OtherType& val) : OtherType(val) {};
	virtual ~DIType_O() {}

    }; // DIType_O
}; // llvmo
TRANSLATE(llvmo::DIType_O);

namespace translate
{
    template <>
    struct to_object<llvm::DIType>
    {
        static core::T_sp convert(const llvm::DIType& val)
        {_G();
	    GC_RESERVE_BEGIN(llvmo::DIType_O,obj) {
		GC_RESERVE_GET_VARIADIC(llvmo::DIType_O,obj,val);
	    } GC_RESERVE_END(llvmo::DIType_O,obj);
	    return (obj);
	};
    };
    template <>
    struct from_object<llvm::DIType,std::true_type>
    {
	typedef llvm::DIType& DeclareType;
	DeclareType _v;
	from_object(T_P object) : _v(*(object.as<llvmo::DebugInfo_O>()->operator llvm::DIType*())) {};
    };
};






namespace llvmo
{
    FORWARD(DIDerivedType);
    class DIDerivedType_O : public DebugInfo_O
			  , public llvm::DIDerivedType
    {
	LISP_BASE1(DebugInfo_O);
	LISP_CLASS(llvmo,LlvmoPkg,DIDerivedType_O,"DIDerivedType");
	typedef llvm::DIDerivedType OtherType;
    private:
    public:
 	virtual operator llvm::DIDescriptor* () { return this;};
 	virtual operator llvm::DIType* () { return this;};
	DIDerivedType_O() : Base() {};
	DIDerivedType_O(const OtherType& val) : OtherType(val) {};
	virtual ~DIDerivedType_O() {}

    }; // DIDerivedType_O
}; // llvmo
TRANSLATE(llvmo::DIDerivedType_O);
/* from_object translators */
/* to_object translators */

namespace translate
{
    template <>
    struct to_object<llvm::DIDerivedType>
    {
        static core::T_sp convert(const llvm::DIDerivedType& val)
        {_G();
	    GC_RESERVE_BEGIN(llvmo::DIDerivedType_O,obj) {
		GC_RESERVE_GET_VARIADIC(llvmo::DIDerivedType_O,obj,val);
	    } GC_RESERVE_END(llvmo::DIDerivedType_O,obj);
	    return (obj);
	};
    };
    template <>
    struct from_object<llvm::DIDerivedType,std::true_type>
    {
	typedef llvm::DIDerivedType& DeclareType;
	static DeclareType convert(core::T_sp object)
	{
	    ASSERT(object.pointerp());
	    return *(object.as<llvmo::DIDerivedType_O>());
	}
    };
};





namespace llvmo
{
    FORWARD(DIBasicType);
    class DIBasicType_O : public DebugInfo_O
			, public llvm::DIBasicType
    {
	LISP_BASE1(DebugInfo_O);
	LISP_CLASS(llvmo,LlvmoPkg,DIBasicType_O,"DIBasicType");
	typedef llvm::DIBasicType OtherType;
    private:
    public:
 	virtual operator llvm::DIDescriptor* () { return this;};
 	virtual operator llvm::DIType* () { return this;};
	DIBasicType_O() : Base() {};
	DIBasicType_O(const OtherType& val) : OtherType(val) {};
	virtual ~DIBasicType_O() {}

    }; // DIBasicType_O
}; // llvmo
TRANSLATE(llvmo::DIBasicType_O);
/* from_object translators */
/* to_object translators */

namespace translate
{
    template <>
    struct to_object<llvm::DIBasicType>
    {
        static core::T_sp convert(const llvm::DIBasicType& val)
        {_G();
	    GC_RESERVE_BEGIN(llvmo::DIBasicType_O,obj) {
		GC_RESERVE_GET_VARIADIC(llvmo::DIBasicType_O,obj,val);
	    } GC_RESERVE_END(llvmo::DIBasicType_O,obj);
	    return (obj);
	};
    };
    template <>
    struct from_object<llvm::DIBasicType,std::true_type>
    {
	typedef llvm::DIBasicType& DeclareType;
	static DeclareType convert(core::T_sp object)
	{
	    ASSERT(object.pointerp());
	    return *(object.as<llvmo::DIBasicType_O>());
	}
    };
};










namespace llvmo
{
    FORWARD(DICompositeType);
    class DICompositeType_O : public DebugInfo_O
			    , public llvm::DICompositeType
    {
	LISP_BASE1(DebugInfo_O);
	LISP_CLASS(llvmo,LlvmoPkg,DICompositeType_O,"DICompositeType");
	typedef llvm::DICompositeType OtherType;
    private:
    public:
 	virtual operator llvm::DIDescriptor* () { return this;};
 	virtual operator llvm::DIType* () { return this;};
	DICompositeType_O() : Base() {};
	DICompositeType_O(const OtherType& val) : OtherType(val) {};
	virtual ~DICompositeType_O() {}

    }; // DICompositeType_O
}; // llvmo
TRANSLATE(llvmo::DICompositeType_O);
/* from_object translators */
/* to_object translators */

namespace translate
{
    template <>
    struct to_object<llvm::DICompositeType>
    {
        static core::T_sp convert(const llvm::DICompositeType& val)
        {_G();
	    GC_RESERVE_BEGIN(llvmo::DICompositeType_O,obj) {
		GC_RESERVE_GET_VARIADIC(llvmo::DICompositeType_O,obj,val);
	    } GC_RESERVE_END(llvmo::DICompositeType_O,obj);
	    return (obj);
	};
    };
    template <>
    struct from_object<llvm::DICompositeType,std::true_type>
    {
	typedef llvm::DICompositeType& DeclareType;
	DeclareType _v;
	from_object(T_P object) : _v(*(object.as<llvmo::DICompositeType_O>())) {};
    };
};








namespace llvmo
{
    FORWARD(DILexicalBlock);
    class DILexicalBlock_O : public DebugInfo_O
			   , public llvm::DILexicalBlock
    {
	LISP_BASE1(DebugInfo_O);
	LISP_CLASS(llvmo,LlvmoPkg,DILexicalBlock_O,"DILexicalBlock");
	typedef llvm::DILexicalBlock OtherType;
    private:
    public:
 	virtual operator llvm::DIDescriptor* () { return this;};
	DILexicalBlock_O() : Base() {};
	DILexicalBlock_O(const OtherType& val) : OtherType(val) {};
	virtual ~DILexicalBlock_O() {}

    }; // DILexicalBlock_O
}; // llvmo
TRANSLATE(llvmo::DILexicalBlock_O);
/* from_object translators */
/* to_object translators */
namespace translate
{
    template <>
    struct to_object<llvm::DILexicalBlock>
    {
        static core::T_sp convert(const llvm::DILexicalBlock& val)
        {_G();
	    GC_RESERVE_BEGIN(llvmo::DILexicalBlock_O,obj) {
		GC_RESERVE_GET_VARIADIC(llvmo::DILexicalBlock_O,obj,val);
	    } GC_RESERVE_END(llvmo::DILexicalBlock_O,obj);
	    return (obj);
	};
    };
    template <>
    struct from_object<llvm::DILexicalBlock,std::true_type>
    {
	typedef llvm::DILexicalBlock& DeclareType;
	static DeclareType convert(core::T_sp object)
	{
	    ASSERT(object.pointerp());
	    return *(object.as<llvmo::DILexicalBlock_O>());
	}
    };
};






namespace llvmo
{
    FORWARD(DICompileUnit);
    class DICompileUnit_O : public DebugInfo_O
			  , public llvm::DICompileUnit
    {
	LISP_BASE1(DebugInfo_O);
	LISP_CLASS(llvmo,LlvmoPkg,DICompileUnit_O,"DICompileUnit");
	typedef llvm::DICompileUnit OtherType;
    private:
    public:
 	virtual operator llvm::DIDescriptor* () { return this;};
	DICompileUnit_O() : Base() {};
	DICompileUnit_O(const OtherType& val) : OtherType(val) {};
	virtual ~DICompileUnit_O() {}

    }; // DICompileUnit_O
}; // llvmo
TRANSLATE(llvmo::DICompileUnit_O);
/* from_object translators */
/* to_object translators */
namespace translate
{
    template <>
    struct to_object<llvm::DICompileUnit>
    {
        static core::T_sp convert(const llvm::DICompileUnit& val)
        {_G();
	    GC_RESERVE_BEGIN(llvmo::DICompileUnit_O,obj) {
		GC_RESERVE_GET_VARIADIC(llvmo::DICompileUnit_O,obj,val);
	    } GC_RESERVE_END(llvmo::DICompileUnit_O,obj);
	    return (obj);
	};
    };
    template <>
    struct from_object<llvm::DICompileUnit,std::true_type>
    {
	typedef llvm::DICompileUnit& DeclareType;
	static DeclareType convert(core::T_sp object)
	{
	    ASSERT(object.pointerp());
	    return *(object.as<llvmo::DICompileUnit_O>());
	}
    };
};








namespace llvmo
{
    FORWARD(DIBuilder);
    class DIBuilder_O : public core::ExternalObject_O
    {
	LISP_EXTERNAL_CLASS(llvmo,LlvmoPkg,llvm::DIBuilder,DIBuilder_O,"DIBuilder",core::ExternalObject_O);
	typedef llvm::DIBuilder ExternalType;
	typedef llvm::DIBuilder* PointerToExternalType;

    protected:
	PointerToExternalType _ptr;
    public:
	virtual void* externalObject() const
	{
	    return this->_ptr;
	};
	PointerToExternalType wrappedPtr() const
	{
	    return this->_ptr;
	}

    public:
	static DIBuilder_sp make(Module_sp context);

    public:
	void set_wrapped(PointerToExternalType ptr)
	{
	    if (this->_ptr != NULL ) delete this->_ptr; 
	    this->_ptr = ptr;
	};

	DIArray_sp getOrCreateArray(core::Cons_sp elements);

	DIBuilder_O() : Base(), _ptr(NULL)  {};
	virtual ~DIBuilder_O() {if (_ptr != NULL ) { delete _ptr; _ptr = NULL;};}

    }; // DIBuilder_O
}; // llvmo
TRANSLATE(llvmo::DIBuilder_O);
/* from_object translators */

namespace translate
{
    template <>
    struct from_object<llvm::DIBuilder&,std::true_type>
    {
        typedef llvm::DIBuilder& DeclareType;
	DeclareType _v;
	from_object(T_P object) : _v(*object.as<llvmo::DIBuilder_O>()->wrappedPtr()) {};
    };
};
;
/* to_object translators */

#if 0
namespace translate
{
    template <>
    struct from_object<llvm::ArrayRef<llvm::Value*,std::true_type> >
    {
        typedef llvm::ArrayRef<llvm::Value*> DeclareType;
        static DeclareType convert(core::T_sp object)
	{_G();
	    if ( core::Cons_sp cur = object.asOrNull<core::Cons_O>() )
	    {
//		printf("%s:%d About to convert Cons into ArrayRef<llvm::Value*>\n", __FILE__, __LINE__);
//		printf("     cons --> %s\n", cur->__repr__().c_str() );
		vector<llvm::Value*> vector_values;
		for ( ; cur->notNil(); cur=cur->cdr() )
		{
		    if ( llvmo::Value_sp val = cur->ocar().asOrNull<llvmo::Value_O>() )
		    {
//			printf("      push_back val->wrappedPtr() --> %p\n", val->wrappedPtr());
			vector_values.push_back(val->wrappedPtr());
		    } else if ( llvmo::DebugInfo_sp di = cur->ocar().asOrNull<llvmo::DebugInfo_O>() )
		    {
//			printf("      getting DIDescriptor*\n");
			llvm::DIDescriptor* didescriptor = di->operator llvm::DIDescriptor*();
//			printf("      convert DIDescrptor* to MDNode* --> %p\n", didescriptor );
			llvm::MDNode* mdnode_didescriptor = *didescriptor;
//			printf("      push_back mdnode_didescriptor --> %p\n", mdnode_didescriptor );
			vector_values.push_back(mdnode_didescriptor);
		    } else
		    {
			SIMPLE_ERROR(BF("Handle conversion of %s to llvm::Value*") % cur->ocar()->__repr__() );
		    }
		}
		llvm::ArrayRef<llvm::Value*> array(vector_values);
		return array;
	    }
	    SIMPLE_ERROR(BF("Cannot convert object %s to llvm::ArrayRef<llvm::Value*>") % object->__repr__() );
	}
    };
};
#endif

;



#endif // debugInfo expose
