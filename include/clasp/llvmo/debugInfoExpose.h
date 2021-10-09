/*
    File: debugInfoExpose.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
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
#ifndef debugInfoExpose_H //[
#define debugInfoExpose_H

#include <clasp/core/common.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/array.h>
#include <clasp/core/debugger.h>
#include <clasp/core/ql.h>
//#include "llvm/DataLayout.h"

#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
//#include "llvm/ExecutionEngine/JIT.h"
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Constants.h>
//#include "llvm/Support/IRBuilder.h"
#include <llvm/DebugInfo/DIContext.h>

// These macros are all defined in /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX11.1.sdk/usr/include/mach-o/loader.h
// And then AGAIN in enums in /opt/llvm13/bin/../include/llvm/BinaryFormat/MachO.h
// We undef them here so they don't collide
#ifdef _TARGET_OS_DARWIN
#undef MH_MAGIC
#undef MH_CIGAM
#undef MH_MAGIC_64
#undef MH_CIGAM_64
#undef MH_EXECUTE
#undef MH_FVMLIB
#undef MH_CORE
#undef MH_PRELOAD
#undef MH_DYLIB
#undef MH_DYLINKER
#undef MH_BUNDLE
#undef MH_DSYM
#undef MH_DYLIB_STUB
#undef MH_NOUNDEFS
#undef MH_INCRLINK
#undef MH_OBJECT 
#undef MH_KEXT_BUNDLE 
#undef MH_DYLDLINK 
#undef MH_BINDATLOAD 
#undef MH_PREBOUND 
#undef MH_SPLIT_SEGS 
#undef MH_LAZY_INIT 
#undef MH_TWOLEVEL 
#undef MH_FORCE_FLAT 
#undef MH_NOMULTIDEFS 
#undef MH_NOFIXPREBINDING 
#undef MH_PREBINDABLE 
#undef MH_ALLMODSBOUND 
#undef MH_SUBSECTIONS_VIA_SYMBOLS 
#undef MH_CANONICAL 
#undef MH_WEAK_DEFINES 
#undef MH_BINDS_TO_WEAK 
#undef MH_ALLOW_STACK_EXECUTION 
#undef MH_ROOT_SAFE 
#undef MH_SETUID_SAFE 
#undef MH_NO_REEXPORTED_DYLIBS 
#undef MH_PIE 
#undef MH_DEAD_STRIPPABLE_DYLIB 
#undef MH_HAS_TLV_DESCRIPTORS 
#undef MH_NO_HEAP_EXECUTION 
#undef MH_APP_EXTENSION_SAFE 
#undef MH_NLIST_OUTOFSYNC_WITH_DYLDINFO 
#undef MH_SIM_SUPPORT 
#undef MH_DYLIB_IN_CACHE 
#undef LC_REQ_DYLD
#undef LC_SEGMENT
#undef LC_SYMTAB
#undef LC_SYMSEG 
#undef LC_THREAD 
#undef LC_UNIXTHREAD 
#undef LC_LOADFVMLIB 
#undef LC_IDFVMLIB 
#undef LC_IDENT 
#undef LC_FVMFILE 
#undef LC_PREPAGE 
#undef LC_DYSYMTAB 
#undef LC_LOAD_DYLIB 
#undef LC_ID_DYLIB 
#undef LC_LOAD_DYLINKER 
#undef LC_ID_DYLINKER 
#undef LC_PREBOUND_DYLIB 
#undef LC_ROUTINES 
#undef LC_SUB_FRAMEWORK 
#undef LC_SUB_UMBRELLA 
#undef LC_SUB_CLIENT 
#undef LC_SUB_LIBRARY 
#undef LC_TWOLEVEL_HINTS 
#undef LC_PREBIND_CKSUM 
#undef LC_LOAD_WEAK_DYLIB 
#undef LC_SEGMENT_64 
#undef LC_ROUTINES_64 
#undef LC_UUID 
#undef LC_RPATH       
#undef LC_CODE_SIGNATURE 
#undef LC_SEGMENT_SPLIT_INFO 
#undef LC_REEXPORT_DYLIB  
#undef LC_LAZY_LOAD_DYLIB 
#undef LC_ENCRYPTION_INFO 
#undef LC_DYLD_INFO 
#undef LC_DYLD_INFO_ONLY  
#undef LC_LOAD_UPWARD_DYLIB  
#undef LC_VERSION_MIN_MACOSX 
#undef LC_VERSION_MIN_IPHONEOS 
#undef LC_FUNCTION_STARTS 
#undef LC_DYLD_ENVIRONMENT 
#undef LC_MAIN 
#undef LC_DATA_IN_CODE 
#undef LC_SOURCE_VERSION 
#undef LC_DYLIB_CODE_SIGN_DRS 
#undef LC_ENCRYPTION_INFO_64 
#undef LC_LINKER_OPTION 
#undef LC_LINKER_OPTIMIZATION_HINT 
#undef LC_VERSION_MIN_TVOS 
#undef LC_VERSION_MIN_WATCHOS 
#undef LC_NOTE 
#undef LC_BUILD_VERSION 
#undef SG_HIGHVM       
#undef SG_FVMLIB       
#undef SG_NORELOC      
#undef SG_PROTECTED_VERSION_1  
#undef S_REGULAR               
#undef S_ZEROFILL              
#undef S_CSTRING_LITERALS      
#undef S_4BYTE_LITERALS        
#undef S_8BYTE_LITERALS        
#undef S_LITERAL_POINTERS      
#undef S_NON_LAZY_SYMBOL_POINTERS      
#undef S_LAZY_SYMBOL_POINTERS          
#undef S_SYMBOL_STUBS                  
#undef S_MOD_INIT_FUNC_POINTERS        
#undef S_MOD_TERM_FUNC_POINTERS        
#undef S_COALESCED                     
#undef S_GB_ZEROFILL                   
#undef S_INTERPOSING                   
#undef S_16BYTE_LITERALS               
#undef S_DTRACE_DOF                    
#undef S_LAZY_DYLIB_SYMBOL_POINTERS    
#undef S_THREAD_LOCAL_REGULAR                   
#undef S_THREAD_LOCAL_ZEROFILL                  
#undef S_THREAD_LOCAL_VARIABLES                 
#undef S_THREAD_LOCAL_VARIABLE_POINTERS         
#undef S_THREAD_LOCAL_INIT_FUNCTION_POINTERS    
#undef S_ATTR_PURE_INSTRUCTIONS 
#undef S_ATTR_NO_TOC            
#undef S_ATTR_STRIP_STATIC_SYMS 
#undef S_ATTR_NO_DEAD_STRIP     
#undef S_ATTR_LIVE_SUPPORT      
#undef S_ATTR_SELF_MODIFYING_CODE 
#undef S_ATTR_DEBUG             
#undef S_ATTR_SOME_INSTRUCTIONS
#undef SECTION_TYPE             
#undef SECTION_ATTRIBUTES       
#undef SECTION_ATTRIBUTES_USR   
#undef SECTION_ATTRIBUTES_SYS   
#undef S_ATTR_EXT_RELOC         
#undef S_ATTR_LOC_RELOC         
#undef INDIRECT_SYMBOL_LOCAL   
#undef INDIRECT_SYMBOL_ABS     
#undef DICE_KIND_DATA              
#undef DICE_KIND_JUMP_TABLE8       
#undef DICE_KIND_JUMP_TABLE16      
#undef DICE_KIND_JUMP_TABLE32      
#undef DICE_KIND_ABS_JUMP_TABLE32  
#undef REBASE_TYPE_POINTER                                     
#undef REBASE_TYPE_TEXT_ABSOLUTE32                             
#undef REBASE_TYPE_TEXT_PCREL32                                
#undef REBASE_OPCODE_MASK                                      
#undef REBASE_IMMEDIATE_MASK                                   
#undef REBASE_OPCODE_DONE
#undef REBASE_OPCODE_SET_TYPE_IMM                              
#undef REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB               
#undef REBASE_OPCODE_ADD_ADDR_ULEB                             
#undef REBASE_OPCODE_ADD_ADDR_IMM_SCALED                       
#undef REBASE_OPCODE_DO_REBASE_IMM_TIMES                       
#undef REBASE_OPCODE_DO_REBASE_ULEB_TIMES                      
#undef REBASE_OPCODE_DO_REBASE_ADD_ADDR_ULEB                   
#undef REBASE_OPCODE_DO_REBASE_ULEB_TIMES_SKIPPING_ULEB        
#undef BIND_TYPE_POINTER                                       
#undef BIND_TYPE_TEXT_ABSOLUTE32                               
#undef BIND_TYPE_TEXT_PCREL32                                  
#undef BIND_SPECIAL_DYLIB_SELF                                  
#undef BIND_SPECIAL_DYLIB_MAIN_EXECUTABLE                      
#undef BIND_SPECIAL_DYLIB_FLAT_LOOKUP                          
#undef BIND_SYMBOL_FLAGS_WEAK_IMPORT                           
#undef BIND_SYMBOL_FLAGS_NON_WEAK_DEFINITION                   
#undef BIND_OPCODE_MASK                                        
#undef BIND_IMMEDIATE_MASK                                     
#undef BIND_OPCODE_DONE                                        
#undef BIND_OPCODE_SET_DYLIB_ORDINAL_IMM			
#undef BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB			
#undef BIND_OPCODE_SET_DYLIB_SPECIAL_IMM			
#undef BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM		
#undef BIND_OPCODE_SET_TYPE_IMM				
#undef BIND_OPCODE_SET_ADDEND_SLEB				
#undef BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB			
#undef BIND_OPCODE_ADD_ADDR_ULEB				
#undef BIND_OPCODE_DO_BIND					
#undef BIND_OPCODE_DO_BIND_ADD_ADDR_ULEB			
#undef BIND_OPCODE_DO_BIND_ADD_ADDR_IMM_SCALED			
#undef BIND_OPCODE_DO_BIND_ULEB_TIMES_SKIPPING_ULEB		
#undef	BIND_OPCODE_THREADED					
#undef	BIND_SUBOPCODE_THREADED_SET_BIND_ORDINAL_TABLE_SIZE_ULEB 
#undef	BIND_SUBOPCODE_THREADED_APPLY				 
#undef EXPORT_SYMBOL_FLAGS_KIND_MASK				
#undef EXPORT_SYMBOL_FLAGS_KIND_REGULAR			
#undef EXPORT_SYMBOL_FLAGS_KIND_THREAD_LOCAL			
#undef EXPORT_SYMBOL_FLAGS_KIND_ABSOLUTE			
#undef EXPORT_SYMBOL_FLAGS_WEAK_DEFINITION			
#undef EXPORT_SYMBOL_FLAGS_REEXPORT				
#undef EXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER			
#undef N_STAB  
#undef N_PEXT  
#undef N_TYPE  
#undef N_EXT   
#undef N_UNDF  
#undef N_ABS   
#undef N_SECT  
#undef N_PBUD  
#undef N_INDR  
#undef NO_SECT         
#undef MAX_SECT        
#undef REFERENCE_TYPE                          
#undef REFERENCE_FLAG_UNDEFINED_NON_LAZY               
#undef REFERENCE_FLAG_UNDEFINED_LAZY                   
#undef REFERENCE_FLAG_DEFINED                          
#undef REFERENCE_FLAG_PRIVATE_DEFINED                  
#undef REFERENCE_FLAG_PRIVATE_UNDEFINED_NON_LAZY       
#undef REFERENCE_FLAG_PRIVATE_UNDEFINED_LAZY           
#undef N_ARM_THUMB_DEF
#undef REFERENCED_DYNAMICALLY  
#undef N_NO_DEAD_STRIP 
#undef N_WEAK_REF      
#undef N_WEAK_DEF      
#undef N_SYMBOL_RESOLVER  
#undef N_ALT_ENTRY 
#undef N_COLD_FUNC 
#undef SELF_LIBRARY_ORDINAL 
#undef MAX_LIBRARY_ORDINAL 
#undef DYNAMIC_LOOKUP_ORDINAL 
#undef EXECUTABLE_ORDINAL 
#undef VM_PROT_READ    
#undef VM_PROT_WRITE   
#undef VM_PROT_EXECUTE 
#undef PLATFORM_MACOS 
#undef PLATFORM_IOS 
#undef PLATFORM_TVOS 
#undef PLATFORM_WATCHOS 
#undef PLATFORM_BRIDGEOS 
#undef PLATFORM_MACCATALYST 
#undef PLATFORM_IOSSIMULATOR 
#undef PLATFORM_TVOSSIMULATOR 
#undef PLATFORM_WATCHOSSIMULATOR 
#undef PLATFORM_DRIVERKIT 
#undef TOOL_CLANG 
#undef TOOL_SWIFT 
#undef TOOL_LD 
#undef GET_LIBRARY_ORDINAL
#undef CPU_TYPE_ANY            
#undef CPU_TYPE_X86            
#undef CPU_TYPE_I386           
#undef CPU_TYPE_X86            
#undef CPU_TYPE_X86_64         
#undef CPU_TYPE_MC98000        
#undef CPU_TYPE_ARM            
#undef CPU_TYPE_ARM64          
#undef CPU_TYPE_ARM64_32       
#undef CPU_TYPE_SPARC          
#undef CPU_TYPE_POWERPC                
#undef CPU_TYPE_POWERPC64              
#undef CPU_SUBTYPE_MASK        
#undef CPU_SUBTYPE_LIB64       
#undef CPU_SUBTYPE_MULTIPLE            
#undef CPU_SUBTYPE_I386_ALL                    
#undef CPU_SUBTYPE_386                                 
#undef CPU_SUBTYPE_486                                 
#undef CPU_SUBTYPE_486SX                               
#undef CPU_SUBTYPE_586                                 
#undef CPU_SUBTYPE_PENT        
#undef CPU_SUBTYPE_PENTPRO     
#undef CPU_SUBTYPE_PENTII_M3   
#undef CPU_SUBTYPE_PENTII_M5   
#undef CPU_SUBTYPE_CELERON                             
#undef CPU_SUBTYPE_CELERON_MOBILE              
#undef CPU_SUBTYPE_PENTIUM_3                   
#undef CPU_SUBTYPE_PENTIUM_3_M                 
#undef CPU_SUBTYPE_PENTIUM_3_XEON
#undef SET_LIBRARY_ORDINAL
#undef GET_COMM_ALIGN
#undef SET_COMM_ALIGN
#undef CPU_ARCH_MASK           
#undef CPU_ARCH_ABI64          
#undef CPU_ARCH_ABI64_32       
#undef CPU_SUBTYPE_PENTIUM_M                   
#undef CPU_SUBTYPE_PENTIUM_4                   
#undef CPU_SUBTYPE_PENTIUM_4_M                 
#undef CPU_SUBTYPE_ITANIUM                             
#undef CPU_SUBTYPE_ITANIUM_2                   
#undef CPU_SUBTYPE_XEON                                
#undef CPU_SUBTYPE_XEON_MP                             
#undef CPU_SUBTYPE_X86_ALL             
#undef CPU_SUBTYPE_X86_64_ALL          
#undef CPU_SUBTYPE_X86_ARCH1           
#undef CPU_SUBTYPE_X86_64_H            
#undef CPU_SUBTYPE_INTEL
#undef CPU_SUBTYPE_INTEL_FAMILY
#undef CPU_SUBTYPE_INTEL_MODEL
#undef CPU_SUBTYPE_INTEL_MAX
#undef CPU_SUBTYPE_INTEL_ALL
#undef CPU_SUBTYPE_FAMILY_ALL
#undef CPU_SUBTYPE_MODEL_ALL
#undef CPU_SUBTYPE_ARM_V6
#undef CPU_SUBTYPE_ARM_V5TEJ
#undef CPU_SUBTYPE_INTEL_FAMILY_MAX    
#undef CPU_SUBTYPE_INTEL_MODEL_ALL     
#undef CPU_SUBTYPE_ARM_ALL             
#undef CPU_SUBTYPE_ARM_V4T             
#undef CPU_SUBTYPE_ARM_XSCALE          
#undef CPU_SUBTYPE_ARM_V7              
#undef CPU_SUBTYPE_ARM_V7S             
#undef CPU_SUBTYPE_ARM_V7K             
#undef CPU_SUBTYPE_ARM_V6M             
#undef CPU_SUBTYPE_ARM_V7M             
#undef CPU_SUBTYPE_ARM_V7EM            
#undef CPU_SUBTYPE_ARM64_ALL           
#undef CPU_SUBTYPE_ARM64_V8            
#undef CPU_SUBTYPE_ARM64E              
#undef CPU_SUBTYPE_ARM64_32_V8 
#undef CPU_SUBTYPE_SPARC_ALL           
#undef CPU_SUBTYPE_POWERPC_ALL         
#undef CPU_SUBTYPE_POWERPC_601         
#undef CPU_SUBTYPE_POWERPC_602
#undef CPU_SUBTYPE_POWERPC_603         
#undef CPU_SUBTYPE_POWERPC_603e        
#undef CPU_SUBTYPE_POWERPC_603ev       
#undef CPU_SUBTYPE_POWERPC_604         
#undef CPU_SUBTYPE_POWERPC_604e        
#undef CPU_SUBTYPE_POWERPC_620         
#undef CPU_SUBTYPE_POWERPC_750         
#undef CPU_SUBTYPE_POWERPC_7400        
#undef CPU_SUBTYPE_POWERPC_7450        
#undef CPU_SUBTYPE_POWERPC_970         
#undef CPU_SUBTYPE_MC98601     
#undef x86_THREAD_STATE32              
#undef x86_FLOAT_STATE32               
#undef x86_EXCEPTION_STATE32           
#undef x86_THREAD_STATE64              
#undef x86_FLOAT_STATE64               
#undef x86_EXCEPTION_STATE64           
#undef x86_THREAD_STATE                
#undef x86_FLOAT_STATE                 
#undef x86_EXCEPTION_STATE             
#undef x86_DEBUG_STATE32               
#undef x86_DEBUG_STATE64               
#undef x86_DEBUG_STATE                 
#undef x86_THREAD_STATE_COUNT        
#undef x86_THREAD_STATE32_COUNT        
#undef x86_THREAD_STATE64_COUNT        
#undef x86_EXCEPTION_STATE_COUNT        
#undef x86_EXCEPTION_STATE32_COUNT        
#undef x86_EXCEPTION_STATE64_COUNT        
#undef x86_FLOAT_STATE_COUNT        
#undef x86_FLOAT_STATE32_COUNT        
#undef x86_FLOAT_STATE64_COUNT        
#endif





#include <llvm/DebugInfo/DWARF/DWARFContext.h>

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/metaClass.fwd.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/array.h>
#include <clasp/llvmo/debugInfoExpose.fwd.h>
#include <clasp/core/loadTimeValues.fwd.h>
#include <clasp/llvmo/insertPoint.fwd.h>
#include <clasp/llvmo/debugLoc.fwd.h>
#include <clasp/llvmo/llvmoPackage.h>
#include <clasp/llvmo/llvmoExpose.h>

namespace llvmo {
FORWARD(DILocation);
class DILocation_O : public MDNode_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DILocation, DILocation_O, "DILocation", MDNode_O );
  typedef llvm::DILocation ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  static DILocation_sp make(llvm::LLVMContext&, unsigned int, unsigned int, DINode_sp, core::T_sp);
  virtual operator llvm::DILocation *() { return reinterpret_cast<llvm::DILocation*>(this->_ptr); };
  virtual operator llvm::MDNode *() { return reinterpret_cast<llvm::MDNode*>(this->_ptr); };
  virtual operator llvm::Metadata *() { return reinterpret_cast<llvm::Metadata*>(this->_ptr); }
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
 
  //	virtual llvm::DILocation* operator ->() const { return (llvm::DILocation*)(this);};
 DILocation_O() : Base() {};
  virtual ~DILocation_O(){};
}; // DILocation_O
}; // llvmo
TRANSLATE(llvmo::DILocation_O);

namespace translate {
  template <>
    struct from_object<llvm::DILocation*,std::true_type> {
    typedef llvm::DILocation* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DILocation_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DILocation*> {
    static core::T_sp convert(const llvm::DILocation* ptr) {
      return (core::RP_Create_wrapped<llvmo::DILocation_O, llvm::DILocation*>(const_cast<llvm::DILocation*>(ptr)));
    };
  };
};


namespace llvmo {
FORWARD(DINode);
class DINode_O : public MDNode_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DINode, DINode_O, "DINode", MDNode_O );
  typedef llvm::DINode ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  virtual operator llvm::MDNode *() { return reinterpret_cast<llvm::MDNode*>(this->_ptr); };
  virtual operator llvm::Metadata *() { return reinterpret_cast<llvm::Metadata*>(this->_ptr); }
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
 
  //	virtual llvm::DINode* operator ->() const { return (llvm::DINode*)(this);};
 DINode_O() : Base() {};
  virtual ~DINode_O(){};
}; // DINode_O
}; // llvmo
TRANSLATE(llvmo::DINode_O);

namespace translate {
  template <>
    struct from_object<llvm::DINode*,std::true_type> {
    typedef llvm::DINode* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DINode_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DINode*> {
    static core::T_sp convert(const llvm::DINode* ptr) {
      return (core::RP_Create_wrapped<llvmo::DINode_O, llvm::DINode*>(const_cast<llvm::DINode*>(ptr)));
    };
  };
};


namespace llvmo {
FORWARD(DIExpression);
class DIExpression_O : public MDNode_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIExpression, DIExpression_O, "DIExpression", MDNode_O );
  typedef llvm::DIExpression ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DIExpression *() { return reinterpret_cast<llvm::DIExpression*>(this->_ptr); };
  virtual operator llvm::MDNode *() { return reinterpret_cast<llvm::MDNode*>(this->_ptr); };
  virtual operator llvm::Metadata *() { return reinterpret_cast<llvm::Metadata*>(this->_ptr); }
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
 
  //	virtual llvm::DIExpression* operator ->() const { return (llvm::DIExpression*)(this);};
 DIExpression_O() : Base() {};
  virtual ~DIExpression_O(){};
}; // DIExpression_O
}; // llvmo
TRANSLATE(llvmo::DIExpression_O);

namespace translate {
  template <>
    struct from_object<llvm::DIExpression*,std::true_type> {
    typedef llvm::DIExpression* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIExpression_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DIExpression*> {
    static core::T_sp convert(const llvm::DIExpression* ptr) {
      return (core::RP_Create_wrapped<llvmo::DIExpression_O, llvm::DIExpression*>(const_cast<llvm::DIExpression*>(ptr)));
    };
  };
};


namespace llvmo {
FORWARD(DIScope);
class DIScope_O : public DINode_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIScope, DIScope_O, "discope", DINode_O);
  typedef llvm::DIScope ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIScope_O(){};
  virtual ~DIScope_O() {}
}; // DIScope_O
}; // llvmo
TRANSLATE(llvmo::DIScope_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DIScope*,std::true_type> {
    typedef llvm::DIScope* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIScope_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DIScope*> {
    static core::T_sp convert(const llvm::DIScope* ptr) {
      return (core::RP_Create_wrapped<llvmo::DIScope_O, llvm::DIScope*>(const_cast<llvm::DIScope*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DINodeArray);
 class DINodeArray_O : public core::CxxObject_O {
   LISP_CLASS(llvmo, LlvmoPkg, DINodeArray_O, "DINodeArray",core::CxxObject_O);
 private:
   dont_expose<llvm::DINodeArray> _Val;
public:
  llvm::DINodeArray& get() { return this->_Val._value;};
 DINodeArray_O(const llvm::DINodeArray& v) : _Val(v) {};
  DINodeArray_O() : Base(){};
  virtual ~DINodeArray_O() {}

}; // DINodeArray_O
}; // llvmo
TRANSLATE(llvmo::DINodeArray_O);

namespace translate {
template <>
struct to_object<llvm::DINodeArray> {
  static core::T_sp convert(const llvm::DINodeArray &val) {
    auto  obj = gctools::GC<llvmo::DINodeArray_O>::allocate( val);
    return ((obj));
  };
};
template <>
struct from_object<llvm::DINodeArray, std::true_type> {
  typedef llvm::DINodeArray &DeclareType;
  DeclareType _v;
 from_object(T_P object) : _v(gc::As<llvmo::DINodeArray_sp>(object)->get()) {};
};
};


namespace llvmo {
FORWARD(DITypeRefArray);
 class DITypeRefArray_O : public core::CxxObject_O {
   LISP_CLASS(llvmo, LlvmoPkg, DITypeRefArray_O, "DITypeRefArray", core::CxxObject_O);
 private:
   dont_expose<llvm::DITypeRefArray> _Val;
public:
  llvm::DITypeRefArray& get() { return this->_Val._value;};
 DITypeRefArray_O(const llvm::DITypeRefArray &val) : _Val(val) {};
 DITypeRefArray_O() : Base(), _Val((llvm::DITypeRefArray)NULL) {};
  virtual ~DITypeRefArray_O() {}
}; // DITypeRefArray_O
}; // llvmo
TRANSLATE(llvmo::DITypeRefArray_O);

namespace translate {
template <>
struct to_object<llvm::DITypeRefArray> {
  static core::T_sp convert(const llvm::DITypeRefArray &val) {
    auto  obj = gctools::GC<llvmo::DITypeRefArray_O>::allocate( val);
    return ((obj));
  };
};
template <>
struct from_object<llvm::DITypeRefArray, std::true_type> {
  typedef llvm::DITypeRefArray &DeclareType;
  DeclareType _v;
 from_object(core::T_sp object) : _v(gc::As<llvmo::DITypeRefArray_sp>(object)->get()){};
};
};



namespace llvmo {
FORWARD(DIFile);
class DIFile_O : public DIScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIFile, DIFile_O, "difile", DIScope_O);
  typedef llvm::DIFile ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  std::string __repr__() const;
  std::string getPath() const;
  DIFile_O(){};
  virtual ~DIFile_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DIFile_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DIFile*,std::true_type> {
    typedef llvm::DIFile* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIFile_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DIFile*> {
    static core::T_sp convert(const llvm::DIFile* ptr) {
      return (core::RP_Create_wrapped<llvmo::DIFile_O, llvm::DIFile*>(const_cast<llvm::DIFile*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DILocalScope);
class DILocalScope_O : public DIScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DILocalScope, DILocalScope_O, "dilocal-scope", DIScope_O);
  typedef llvm::DILocalScope ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DILocalScope_O(){};
  virtual ~DILocalScope_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DILocalScope_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DILocalScope*,std::true_type> {
    typedef llvm::DILocalScope* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DILocalScope_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DILocalScope*> {
    static core::T_sp convert(const llvm::DILocalScope* ptr) {
      return (core::RP_Create_wrapped<llvmo::DILocalScope_O, llvm::DILocalScope*>(const_cast<llvm::DILocalScope*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DISubprogram);
class DISubprogram_O : public DILocalScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DISubprogram, DISubprogram_O, "disubprogram", DILocalScope_O);
  typedef llvm::DISubprogram ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  std::string __repr__() const;
  std::string getSubprogram() const;
  DISubprogram_O(){};
  virtual ~DISubprogram_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DISubprogram_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DISubprogram*,std::true_type> {
    typedef llvm::DISubprogram* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DISubprogram_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DISubprogram*> {
    static core::T_sp convert(const llvm::DISubprogram* ptr) {
      return (core::RP_Create_wrapped<llvmo::DISubprogram_O, llvm::DISubprogram*>(const_cast<llvm::DISubprogram*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DIType);
class DIType_O : public DIScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIType, DIType_O, "ditype", DIScope_O);
  typedef llvm::DIType ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIType_O(){};
  virtual ~DIType_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DIType_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DIType*,std::true_type> {
    typedef llvm::DIType* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIType_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DIType*> {
    static core::T_sp convert(const llvm::DIType* ptr) {
      return (core::RP_Create_wrapped<llvmo::DIType_O, llvm::DIType*>(const_cast<llvm::DIType*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DIBasicType);
class DIBasicType_O : public DIType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIBasicType, DIBasicType_O, "DIBasicType", DIType_O);
  typedef llvm::DIBasicType ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIBasicType_O(){};
  virtual ~DIBasicType_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DIBasicType_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DIBasicType*,std::true_type> {
    typedef llvm::DIBasicType* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIBasicType_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DIBasicType*> {
    static core::T_sp convert(const llvm::DIBasicType* ptr) {
      return (core::RP_Create_wrapped<llvmo::DIBasicType_O, llvm::DIBasicType*>(const_cast<llvm::DIBasicType*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DIDerivedType);
class DIDerivedType_O : public DIType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIDerivedType, DIDerivedType_O, "DIDerivedType", DIType_O);
  typedef llvm::DIDerivedType ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIDerivedType_O(){};
  virtual ~DIDerivedType_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DIDerivedType_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DIDerivedType*,std::true_type> {
    typedef llvm::DIDerivedType* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIDerivedType_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DIDerivedType*> {
    static core::T_sp convert(const llvm::DIDerivedType* ptr) {
      return (core::RP_Create_wrapped<llvmo::DIDerivedType_O, llvm::DIDerivedType*>(const_cast<llvm::DIDerivedType*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DICompositeType);
class DICompositeType_O : public DIType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DICompositeType, DICompositeType_O, "DICompositeType", DIType_O);
  typedef llvm::DICompositeType ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DICompositeType_O(){};
  virtual ~DICompositeType_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DICompositeType_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DICompositeType*,std::true_type> {
    typedef llvm::DICompositeType* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DICompositeType_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DICompositeType*> {
    static core::T_sp convert(const llvm::DICompositeType* ptr) {
      return (core::RP_Create_wrapped<llvmo::DICompositeType_O, llvm::DICompositeType*>(const_cast<llvm::DICompositeType*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DISubroutineType);
class DISubroutineType_O : public DIType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DISubroutineType, DISubroutineType_O, "DISubroutineType", DIType_O);
  typedef llvm::DISubroutineType ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DISubroutineType_O(){};
  virtual ~DISubroutineType_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DISubroutineType_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DISubroutineType*,std::true_type> {
    typedef llvm::DISubroutineType* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DISubroutineType_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DISubroutineType*> {
    static core::T_sp convert(const llvm::DISubroutineType* ptr) {
      return (core::RP_Create_wrapped<llvmo::DISubroutineType_O, llvm::DISubroutineType*>(const_cast<llvm::DISubroutineType*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DILexicalBlockBase);
class DILexicalBlockBase_O : public DILocalScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DILexicalBlockBase, DILexicalBlockBase_O, "DILexicalBlockBase", DILocalScope_O);
  typedef llvm::DILexicalBlockBase ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DILexicalBlockBase_O(){};
  virtual ~DILexicalBlockBase_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DILexicalBlockBase_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DILexicalBlockBase*,std::true_type> {
    typedef llvm::DILexicalBlockBase* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DILexicalBlockBase_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DILexicalBlockBase*> {
    static core::T_sp convert(const llvm::DILexicalBlockBase* ptr) {
      return (core::RP_Create_wrapped<llvmo::DILexicalBlockBase_O, llvm::DILexicalBlockBase*>(const_cast<llvm::DILexicalBlockBase*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DILexicalBlock);
class DILexicalBlock_O : public DILexicalBlockBase_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DILexicalBlock, DILexicalBlock_O, "DILexicalBlock", DILexicalBlockBase_O);
  typedef llvm::DILexicalBlock ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DILexicalBlock_O(){};
  virtual ~DILexicalBlock_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DILexicalBlock_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DILexicalBlock*,std::true_type> {
    typedef llvm::DILexicalBlock* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DILexicalBlock_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DILexicalBlock*> {
    static core::T_sp convert(const llvm::DILexicalBlock* ptr) {
      return (core::RP_Create_wrapped<llvmo::DILexicalBlock_O, llvm::DILexicalBlock*>(const_cast<llvm::DILexicalBlock*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DICompileUnit);
class DICompileUnit_O : public DIScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DICompileUnit, DICompileUnit_O, "DICompileUnit", DIScope_O);
  typedef llvm::DICompileUnit ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DICompileUnit_O(){};
  virtual ~DICompileUnit_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DICompileUnit_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DICompileUnit*,std::true_type> {
    typedef llvm::DICompileUnit* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DICompileUnit_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DICompileUnit*> {
    static core::T_sp convert(const llvm::DICompileUnit* ptr) {
      return (core::RP_Create_wrapped<llvmo::DICompileUnit_O, llvm::DICompileUnit*>(const_cast<llvm::DICompileUnit*>(ptr)));
    };
  };
};

template <>
struct gctools::GCInfo<llvmo::DIBuilder_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};


namespace llvmo {
FORWARD(DIBuilder);
class DIBuilder_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIBuilder, DIBuilder_O, "DIBuilder", core::ExternalObject_O);
  typedef llvm::DIBuilder ExternalType;
  typedef llvm::DIBuilder *PointerToExternalType;
protected:
  PointerToExternalType _ptr;
public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }
public:
  static DIBuilder_sp make(Module_sp context);
public:
  void set_wrapped(PointerToExternalType ptr) {
    delete this->_ptr;
    this->_ptr = ptr;
  };
  DINodeArray_sp getOrCreateArray(core::List_sp elements);
  DITypeRefArray_sp getOrCreateTypeArray(core::List_sp elements);
  DIBuilder_O() : Base(), _ptr(NULL){};
  virtual ~DIBuilder_O() {
    if (_ptr != NULL) {
      auto ptr = this->_ptr;
//      printf("%s:%d:%s registering dtor\n", __FILE__, __LINE__, __FUNCTION__ );
      core::thread_local_register_cleanup([ptr] (void) {
#ifdef DEBUG_DTORS
                                            printf("%s:%d:%s dtor %p\n", __FILE__, __LINE__, __FUNCTION__, ptr);
#endif
                                            delete ptr;
                                          });
      _ptr = NULL;
    };
  }

}; // DIBuilder_O
}; // llvmo
TRANSLATE(llvmo::DIBuilder_O);
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::DIBuilder &, std::true_type> {
  typedef llvm::DIBuilder &DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*gc::As<llvmo::DIBuilder_sp>(object)->wrappedPtr()){};
  ~from_object() {/*non trivial*/};
};
};



namespace llvmo {
FORWARD(DIVariable);
class DIVariable_O : public DINode_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIVariable, DIVariable_O, "DIVariable", DINode_O);
  typedef llvm::DIVariable ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIVariable_O(){};
  virtual ~DIVariable_O() {}
}; // DIVariable_O
}; // llvmo
TRANSLATE(llvmo::DIVariable_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DIVariable*,std::true_type> {
    typedef llvm::DIVariable* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIVariable_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DIVariable*> {
    static core::T_sp convert(const llvm::DIVariable* ptr) {
      return (core::RP_Create_wrapped<llvmo::DIVariable_O, llvm::DIVariable*>(const_cast<llvm::DIVariable*>(ptr)));
    };
  };
};


namespace llvmo {
FORWARD(DILocalVariable);
class DILocalVariable_O : public DIVariable_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DILocalVariable, DILocalVariable_O, "DILocalVariable", DIVariable_O);
  typedef llvm::DILocalVariable ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  std::string __repr__() const;
  std::string getVariableName() const;
  DILocalVariable_O(){};
  virtual ~DILocalVariable_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DILocalVariable_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DILocalVariable*,std::true_type> {
    typedef llvm::DILocalVariable* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DILocalVariable_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DILocalVariable*> {
    static core::T_sp convert(const llvm::DILocalVariable* ptr) {
      return (core::RP_Create_wrapped<llvmo::DILocalVariable_O, llvm::DILocalVariable*>(const_cast<llvm::DILocalVariable*>(ptr)));
    };
  };
};






namespace translate {
template <>
struct from_object<llvm::Optional<llvm::DIFile::ChecksumInfo<llvm::StringRef>>, std::true_type> {
  typedef llvm::Optional<llvm::DIFile::ChecksumInfo<llvm::StringRef>> DeclareType;
  DeclareType _v;
  std::string _Storage;
  from_object(core::T_sp object) {
//    printf("%s:%d:%s object = %s\n", __FILE__, __LINE__, __FUNCTION__, core::_rep_(object).c_str());
    core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_CSKEnum->symbolValue());
    if (object.nilp()) {
      DeclareType none;
      this->_v = none;
//      printf("%s:%d:%s ChecksumInfo RESET  this->_v -> %d\n", __FILE__, __LINE__, __FUNCTION__, this->_v.hasValue() );
    } else if (gc::IsA<core::Symbol_sp>(object)) {
      core::Symbol_sp sobject = gc::As<core::Symbol_sp>(object);
      this->_Storage = sobject->symbolNameAsString();
      llvm::DIFile::ChecksumKind kind = converter->enumForSymbol<llvm::DIFile::ChecksumKind>(sobject);
      for ( int p=0; p<this->_Storage.size(); p++ ) {
        if (this->_Storage[p] == '-') this->_Storage[p] = '_';
      }
      llvm::DIFile::ChecksumInfo<llvm::StringRef> checksum(kind,this->_Storage);
      this->_v = checksum;
//      printf("%s:%d:%s ChecksumInfo kind = %d  str = %s \n", __FILE__, __LINE__, __FUNCTION__, kind, this->_Storage.c_str() );
    } else {
      SIMPLE_ERROR_SPRINTF("You must pass a valid Checksum like :CSK_MD5");
    }
  }
  from_object(const from_object& orig) = delete;
  from_object(from_object&& orig) : _Storage(std::move(orig._Storage)), _v(orig._v) {
    if (this->_v.hasValue()) {
//      printf("%s:%d:%s from_object move ctor\n", __FILE__, __LINE__, __FUNCTION__ );
      llvm::DIFile::ChecksumInfo<llvm::StringRef> checksum(this->_v->Kind,this->_Storage);
      this->_v = checksum;
    } else {
//      printf("%s:%d:%s from_object move ctor NIL\n", __FILE__, __LINE__, __FUNCTION__ );
    }
  }
};
};

namespace translate {
template <>
struct from_object<llvm::Optional<llvm::StringRef>, std::true_type> {
  typedef llvm::Optional<llvm::StringRef> DeclareType;
  std::string _Storage;
  DeclareType _v;
  from_object(core::T_sp object) {
    if (core::String_sp so = object.asOrNull<core::String_O>()) {
      this->_Storage = gc::As<core::String_sp>(so)->get_std_string();
      this->_v = this->_Storage;
    } else {
      SIMPLE_ERROR_SPRINTF("You must pass a String");
    }
  }
  from_object(const from_object& orig) = delete;
  from_object(from_object&& orig) : _Storage(std::move(orig._Storage)), _v(_Storage) {}
};
};

// DIContext_O
namespace llvmo {
FORWARD(DIContext);
class DIContext_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIContext, DIContext_O, "DIContext", core::ExternalObject_O);
  typedef llvm::DIContext ExternalType;
  typedef llvm::DIContext *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void *externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIContext_O() : Base(), _ptr(NULL){};
  ~DIContext_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }
}; // DIContext_O class def
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::DIContext *, std::true_type> {
  typedef llvm::DIContext *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::DIContext_sp>(object)->wrappedPtr()){};
};

};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::DIContext *> {
  static core::T_sp convert(llvm::DIContext *ptr) {
    return core::RP_Create_wrapped<llvmo::DIContext_O, llvm::DIContext *>(ptr);
  }
};
}; // namespace llvmo - DIContext_O done

// DWARFContext_O
namespace llvmo {
FORWARD(DWARFContext);
class DWARFContext_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DWARFContext, DWARFContext_O, "DWARFContext", core::ExternalObject_O);
  typedef llvm::DWARFContext ExternalType;
  typedef llvm::DWARFContext *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void *externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
 public:
  static DWARFContext_sp createDWARFContext(ObjectFile_sp);
  DWARFContext_O() : Base(), _ptr(NULL){};
  ~DWARFContext_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }
  size_t getNumCompileUnits() const;
}; // DWARFContext_O class def
// FIXME: move?
const char* getFunctionNameForAddress(DWARFContext_sp, SectionedAddress_sp);
core::T_mv getLineInfoForAddress_( DWARFContext_sp, SectionedAddress_sp, bool verbose );
llvm::Expected<std::vector<llvm::DWARFAddressRange>> getAddressRangesForAddressInner(DWARFContext_sp, SectionedAddress_sp);

}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::DWARFContext *, std::true_type> {
  typedef llvm::DWARFContext *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::DWARFContext_sp>(object)->wrappedPtr()){};
};

};

/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::DWARFContext *> {
  static core::T_sp convert(llvm::DWARFContext *ptr) {
    return core::RP_Create_wrapped<llvmo::DWARFContext_O, llvm::DWARFContext *>(ptr);
  }
};
}; // namespace llvmo - DWARFContext_O done




namespace llvmo {
FORWARD(DWARFUnit);
class DWARFUnit_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DWARFUnit, DWARFUnit_O, "DWARFUnit", core::ExternalObject_O);
  typedef llvm::DWARFUnit ExternalType;
  typedef llvm::DWARFUnit *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void *externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
 public:
  DWARFUnit_O() : Base(), _ptr(NULL){};
  ~DWARFUnit_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }
  
}; // DWARFUnit_O class def

}; // llvmo



namespace translate {
template <>
struct from_object<llvm::DWARFUnit *, std::true_type> {
  typedef llvm::DWARFUnit *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::DWARFUnit_sp>(object)->wrappedPtr()){};
};

};

/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::DWARFUnit *> {
  static core::T_sp convert(llvm::DWARFUnit *ptr) {
    return core::RP_Create_wrapped<llvmo::DWARFUnit_O, llvm::DWARFUnit *>(ptr);
  }
};
}; // namespace llvmo - DWARFUnit_O done




// LineTable_O
namespace llvmo {
FORWARD(LineTable);
class LineTable_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DWARFDebugLine::LineTable, LineTable_O, "LineTable", core::ExternalObject_O);
  typedef const llvm::DWARFDebugLine::LineTable ExternalType;
  typedef const llvm::DWARFDebugLine::LineTable *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void *externalObject() const { return (void*)const_cast<llvm::DWARFDebugLine::LineTable*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
 public:
  LineTable_O() : Base(), _ptr(NULL){};
  ~LineTable_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }
  size_t size() const;
  core::List_sp element(size_t index) const;
  
}; // LineTable_O class def
// FIXME: move?

}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<const llvm::DWARFDebugLine::LineTable *, std::true_type> {
  typedef const llvm::DWARFDebugLine::LineTable *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::LineTable_sp>(object)->wrappedPtr()){};
};

};

/* to_object translators */

namespace translate {
template <>
struct to_object<const llvm::DWARFDebugLine::LineTable *> {
  static core::T_sp convert(const llvm::DWARFDebugLine::LineTable *ptr) {
    return core::RP_Create_wrapped<llvmo::LineTable_O, const llvm::DWARFDebugLine::LineTable *>(ptr);
  }
};
}; // namespace llvmo - LineTable_O done


// ------------------------------------------------------------
//
// Translators for other types
//

ENUM_FROM_OBJECT_TRANSLATOR(llvm::DIFile::ChecksumKind,llvmo::_sym_CSKEnum);
ENUM_FROM_OBJECT_TRANSLATOR(llvm::DICompileUnit::DebugNameTableKind,llvmo::_sym_DNTKEnum);



#endif // debugInfo expose
