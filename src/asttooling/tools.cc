#define DEBUG_LEVEL_FULL


#include "clang/Lex/Lexer.h"
#include "clang/AST/ASTContext.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/JSONCompilationDatabase.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Rewrite/Core/Rewriter.h"

#include "core/common.h"
#include "core/cons.h"
#include "core/commonLispUserPackage.h"
#include "core/evaluator.h"
#include "core/symbolTable.h"
#include "core/package.h"
#include "core/stringList.h"
#include "core/fileSystem.h"
#include "core/environment.h"
#include "core/builtInClass.h"
#include "core/lambdaListHandler.h"
#include "core/multipleValues.h"
#include "core/environment.h"
#include "core/loadTimeValues.h"
#include "core/hashTable.h"
#include "core/bignum.h"
#include "core/pointer.h"
#include "core/pathname.h"
#include "core/str.h"
#include "core/bformat.h"
#include "core/commonLispPackage.h"
#include "core/vectorObjectsWithFillPtr.h"
#include "symbolTable.h"
#include "asttoolingPackage.h"
#include "core/wrappers.h"

using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace clang;
using namespace llvm;
using namespace core;

#define DUMP_LOCATION(bfmsg) cerr <<"!!!!!!!"<< (bfmsg).str() << __FUNCTION__ << " " << __FILE__ << ":" << __LINE__ << std::endl;

namespace asttooling
{

    Str_sp className(const CXXRecordDecl* classDecl)
    {
	stringstream ss;
	ss << "class " << classDecl->getQualifiedNameAsString();
	Str_sp str = Str_O::create(ss.str());
	return str;
    }


    T_sp classifyTemplateSpecializationType(QualType qualty, const TemplateSpecializationType* tsty);

    bool isDerivedFromGCObject(QualType qualty)
    {
	IMPLEMENT_ME();
    }
	

    T_sp classifyTemplateArgs(const TemplateSpecializationType* tsty)
    {
	STATIC_ROOT_FRAME_BEGIN(StaticFrame) {
	    StaticFrame() { this->attachToGCRoot(); };
	    Symbol_sp sym_make_gcobject_derived = lispify_intern("make-gcobject-derived",CommonLispUserPkg);
	    Symbol_sp sym_make_unclassified_ctype = lispify_intern("make-unclassified-ctype",CommonLispUserPkg);
	    Symbol_sp sym_make_gc_template_argument = lispify_intern("make-gc-template-argument",CommonLispUserPkg);
	    Symbol_sp kw_index = lispify_intern("index",KeywordPkg);
	    Symbol_sp kw_ctype = lispify_intern("ctype",KeywordPkg);
	    Symbol_sp kw_description = lispify_intern("description",KeywordPkg);
	    GC_SCANNER() {
		GC_SCANNER_BEGIN() {
		    SMART_PTR_FIX(sym_make_gcobject_derived);
		    SMART_PTR_FIX(sym_make_unclassified_ctype);
		    SMART_PTR_FIX(sym_make_gc_template_argument);
		    SMART_PTR_FIX(kw_index);
		    SMART_PTR_FIX(kw_ctype);
		    SMART_PTR_FIX(kw_description);
		} GC_SCANNER_END();
		return GC_RES_OK;
	    };
	} STATIC_ROOT_FRAME_END(StaticFrame,static_frame);
	Cons_sp args = _Nil<Cons_O>();
	for ( int i=tsty->getNumArgs()-1;i>=0; --i )
	{
	    TemplateArgument arg = tsty->getArg(i);
	    QualType qtarg = arg.getAsType();
	    T_sp classified(_Nil<T_O>());
	    if ( const TemplateSpecializationType* tsty = qtarg->getAs<TemplateSpecializationType>() )
	    {
		classified = classifyTemplateSpecializationType(qtarg,tsty);
		if ( classified.nilp() )
		{
		    if ( isDerivedFromGCObject(qtarg) )
		    {
			classified = eval::funcall(static_frame->sym_make_gcobject_derived,
						   static_frame->kw_description, Str_O::create(qtarg.getAsString()));
		    }
		}
	    }
	    if ( classified.nilp() ) {
		classified = eval::funcall(static_frame->sym_make_unclassified_ctype,
					   static_frame->kw_description, Str_O::create(qtarg.getAsString()));
	    }
	    ASSERTF(classified && !classified.unboundp(), BF("classified must be defined!!!"));
	    T_sp oneArg = eval::funcall(static_frame->sym_make_gc_template_argument,
					static_frame->kw_index,Fixnum_O::create(i),
					static_frame->kw_ctype, classified );
	    args = Cons_O::create(oneArg,args,_lisp);
	}
	return args;
    }

    bool inheritsFromGCHolder(const CXXRecordDecl* decl)
    {
	if ( !decl->hasDefinition() ) return false;
	for ( clang::CXXRecordDecl::base_class_const_iterator it=decl->bases_begin(), itEnd=decl->bases_end();
	      it<itEnd; ++it )
	{
	    QualType qty = it->getType();
	    if ( !qty.isNull() ) {
		const CXXRecordDecl* baseDecl = qty->getAsCXXRecordDecl();
		if ( baseDecl ) {
		    string baseName = qty->getAsCXXRecordDecl()->getNameAsString();
		    if ( baseName == "GCHolder" )
		    {
			return true;
		    }
		}
	    }
	}
	return false;
    };

    T_sp classifyTemplateSpecializationType(QualType qualty, const TemplateSpecializationType* tsty)
    {
	STATIC_ROOT_FRAME_BEGIN(StaticFrame) {
	    StaticFrame() { this->attachToGCRoot(); };
	    Symbol_sp sym_make_smart_ptr_ctype = lispify_intern("make-smart-ptr-ctype",CommonLispUserPkg);
	    Symbol_sp sym_makeWeakSmartPtrCtype = lispify_intern("makeWeakSmartPtrCtype",CommonLispUserPkg);
	    Symbol_sp kw_specializer = lispify_intern("specializer",KeywordPkg);
	    Symbol_sp sym_make_gcholder = lispify_intern("make-gcholder",CommonLispUserPkg);
	    Symbol_sp kw_name = lispify_intern("name",KeywordPkg);
	    Symbol_sp kw_arguments = lispify_intern("arguments",KeywordPkg);
	    Symbol_sp sym_make_stl_container = lispify_intern("make-stl-container",CommonLispUserPkg);
	    Symbol_sp sym_make_unclassified_ctype = lispify_intern("make-unclassified-ctype",CommonLispUserPkg);
	    Symbol_sp kw_description = lispify_intern("description",KeywordPkg);
	    GC_SCANNER() {
		GC_SCANNER_BEGIN() {
		    SMART_PTR_FIX(sym_make_smart_ptr_ctype);
		    SMART_PTR_FIX(sym_makeWeakSmartPtrCtype);
		    SMART_PTR_FIX(kw_specializer);
		    SMART_PTR_FIX(sym_make_gcholder);
		    SMART_PTR_FIX(kw_name);
		    SMART_PTR_FIX(kw_arguments);
		    SMART_PTR_FIX(sym_make_stl_container);
		    SMART_PTR_FIX(sym_make_unclassified_ctype);
		    SMART_PTR_FIX(kw_description);
		} GC_SCANNER_END();
		return GC_RES_OK;
	    };
	} STATIC_ROOT_FRAME_END(StaticFrame,static_frame);

	if (tsty)
	{
	    const CXXRecordDecl* decl = tsty->getAsCXXRecordDecl();
	    if ( decl == NULL )
	    {
		return _Nil<Cons_O>();
	    }
	    if ( decl->getName().str() == "smart_ptr" )
	    {
		ASSERTF(tsty->getNumArgs()==1,BF("%s requires 1 arg")%decl->getName().str());
		TemplateArgument arg = tsty->getArg(0);
		QualType qtarg = arg.getAsType();
		SYMBOL_EXPORT_SC_(AstToolingPkg,smartPtr);
		return eval::funcall(static_frame->sym_make_smart_ptr_ctype,
				     static_frame->kw_specializer,Str_O::create(qtarg.getAsString()));
	    } else if ( decl->getName().str() == "weak_smart_ptr" )
	    {
		ASSERTF(tsty->getNumArgs()==1,BF("%s requires 1 arg")%decl->getName().str());
		TemplateArgument arg = tsty->getArg(0);
		QualType qtarg = arg.getAsType();
		return eval::funcall(static_frame->sym_makeWeakSmartPtrCtype,
				     static_frame->kw_specializer,Str_O::create(qtarg.getAsString()));
	    } else if ( inheritsFromGCHolder(decl) )
	    {
		SYMBOL_EXPORT_SC_(CLUserPkg,holder);
		ASSERTF(tsty->getNumArgs() == 1, BF("%s requires 1 arg") % decl->getName().str() );
		stringstream ss;
		ss << "GCHOLDER_" << decl->getName().str();
		return eval::funcall(static_frame->sym_make_gcholder,
				     static_frame->kw_name, lisp_upcase_intern(ss.str(),CLUserPkg),
				     static_frame->kw_arguments, classifyTemplateArgs(tsty));
	    } else if ( decl->getName().str() == "vector"
			|| decl->getName().str() == "set"
			|| decl->getName().str() == "map"
			|| decl->getName().str() == "queue"
			|| decl->getName().str() == "stack"
			|| decl->getName().str() == "multimap" )
	    {
		SYMBOL_EXPORT_SC_(CLUserPkg,StlStack);
		return eval::funcall(static_frame->sym_make_stl_container,
				     static_frame->kw_name, lisp_upcase_intern("STL"+decl->getName().str(),CLUserPkg),
				     static_frame->kw_arguments, classifyTemplateArgs(tsty));
	    } else {
		SYMBOL_EXPORT_SC_(CLUserPkg,notYetClassifiedTemplateSpecializationType);
		return eval::funcall(static_frame->sym_make_unclassified_ctype,
				     static_frame->kw_description, Str_O::create(qualty.getAsString()));
	    }
	}
	return _Nil<Cons_O>();
    }

    T_sp typeIsOrContainsSmartPtr(QualType qualty, set<const Type*>& seenTypes);
    

    bool recursiveCheckIfRecordContainsSmartPtr(const RecordType* recty,set<const Type*>& seenTypes)
    {
	if (seenTypes.count(recty)>0) return false;
	seenTypes.insert(recty);
	const RecordDecl* recdecl = recty->getDecl();
	for ( RecordDecl::field_iterator fi = recdecl->field_begin(); fi!=recdecl->field_end(); fi++ )
	{
	    QualType qtfield = (*fi)->getType();
	    if ( typeIsOrContainsSmartPtr(qtfield,seenTypes).isTrue() ) return true;
	}
	return false;
    }

    T_sp typeIsOrContainsSmartPtr(QualType qualty, set<const Type*>& seenTypes)
    {
	STATIC_ROOT_FRAME_BEGIN(StaticFrame) {
	    StaticFrame() { this->attachToGCRoot(); };
	    Symbol_sp sym_make_record_ctype = lispify_intern("make-record-ctype",CommonLispUserPkg);
	    Symbol_sp sym_make_pointer_to_record_ctype = lispify_intern("make-pointer-to-record-ctype",CommonLispUserPkg);
	    Symbol_sp kw_description = lispify_intern("description",KeywordPkg);
	    GC_SCANNER() {
		GC_SCANNER_BEGIN() {
		    SMART_PTR_FIX(sym_make_record_ctype);
		    SMART_PTR_FIX(sym_make_pointer_to_record_ctype);
		    SMART_PTR_FIX(kw_description);
		} GC_SCANNER_END();
		return GC_RES_OK;
	    };
	} STATIC_ROOT_FRAME_END(StaticFrame,static_frame);
	if ( const TemplateSpecializationType* tsty = qualty->getAs<TemplateSpecializationType>() )
	{
	    T_sp smartPtr = classifyTemplateSpecializationType(qualty,tsty);
	    if ( smartPtr.notnilp() )
	    {
		return smartPtr;
	    }
	    return _Nil<T_O>();
	} else if ( const RecordType* recty = qualty->getAs<RecordType>() )
	{
	    bool containsSmartPtr = recursiveCheckIfRecordContainsSmartPtr(recty,seenTypes);
	    if ( !containsSmartPtr ) return _Nil<T_O>();
	    return eval::funcall(static_frame->sym_make_record_ctype,
				 static_frame->kw_description,Str_O::create(qualty.getAsString()));
	} else if ( const PointerType* ptrty = qualty->getAs<PointerType>())
	{
	    QualType qtpointee = ptrty->getPointeeType();
	    if ( const RecordType* recty = qtpointee->getAs<RecordType>() )
	    {
		bool containsSmartPtr = recursiveCheckIfRecordContainsSmartPtr(recty,seenTypes);
		if ( !containsSmartPtr ) return _Nil<T_O>();
		return eval::funcall(static_frame->sym_make_pointer_to_record_ctype,
				     static_frame->kw_description,Str_O::create(qualty.getAsString()));
	    }
	}
	return _Nil<T_O>();
    }
	
	    
};



namespace asttooling
{

    struct InstanceVarAccumulator
    {
	HashTable_sp 	_Result;
	InstanceVarAccumulator()
	{
	    this->_Result = HashTable_O::create(::cl::_sym_equal->symbolFunction());
	}
	void add(Str_sp key, T_sp val)
	{
	    Cons_sp curval = this->_Result->gethash(key,_Nil<Cons_O>()).as<Cons_O>();
	    curval = Cons_O::create(val,curval);
	    this->_Result->hash_table_setf_gethash(key,curval);
	}
	HashTable_sp result()
	{
	    return this->_Result;
	};
    };

    class InstanceVarCallback : public MatchFinder::MatchCallback
    {
    private:
	InstanceVarAccumulator 	_Result;
    public:
	virtual void run(const MatchFinder::MatchResult &Result)
	{
	STATIC_ROOT_FRAME_BEGIN(StaticFrame) {
	    StaticFrame() { this->attachToGCRoot(); };
	    Symbol_sp sym_make_instance_variable = lispify_intern("make-instance-variable",CommonLispUserPkg);
	    Symbol_sp kw_field_name = lispify_intern("field-name",KeywordPkg);
	    Symbol_sp kw_class_name = lispify_intern("class-name",KeywordPkg);
	    Symbol_sp kw_ctype = lispify_intern("ctype",KeywordPkg);
	    Symbol_sp sym_make_rest_argument = lispify_intern("make-rest-argument",CLUserPkg);
	    Symbol_sp sym_make_unclassified_ctype = lispify_intern("make-unclassified-ctype",CLUserPkg);
	    Symbol_sp kw_description = lispify_intern("description",KeywordPkg);
	    GC_SCANNER() {
		GC_SCANNER_BEGIN() {
		    SMART_PTR_FIX(sym_make_instance_variable);
		    SMART_PTR_FIX(kw_field_name);
		    SMART_PTR_FIX(kw_class_name);
		    SMART_PTR_FIX(kw_ctype);
		    SMART_PTR_FIX(sym_make_rest_argument);
		    SMART_PTR_FIX(sym_make_unclassified_ctype);
		    SMART_PTR_FIX(kw_description);
		} GC_SCANNER_END();
		return GC_RES_OK;
	    };
	} STATIC_ROOT_FRAME_END(StaticFrame,static_frame);

	    const FieldDecl *fieldDecl = Result.Nodes.getNodeAs<clang::FieldDecl>("field");
	    const CXXRecordDecl *classDecl = Result.Nodes.getNodeAs<clang::CXXRecordDecl>("class");
	    QualType qualty = fieldDecl->getType();
            printf("%s:%d Looking at: %s\n", __FILE__, __LINE__, qualty.getAsString().c_str());
	    T_sp classified(_Nil<T_O>());
	    if ( const TemplateSpecializationType* tsty = qualty->getAs<TemplateSpecializationType>() )
	    {
                printf("   It's a TemplateSpecializationType\n");
		classified = classifyTemplateSpecializationType(qualty,tsty);
	    } else if ( qualty.getAsString() == "struct core::RestArgument" )
	    {
		SYMBOL_EXPORT_SC_(CLUserPkg,restArgument);
		classified = eval::funcall(static_frame->sym_make_rest_argument,
					   static_frame->kw_description, Str_O::create(qualty.getAsString()));
                printf("   It's a RestArgument\n");
	    } else
	    {
		SYMBOL_EXPORT_SC_(CLUserPkg,notYetClassified);
                printf("   It's unclassified\n");
		classified = eval::funcall(static_frame->sym_make_unclassified_ctype,
					   static_frame->kw_description, Str_O::create(qualty.getAsString()));
	    }
	    this->_Result.add(className(classDecl),eval::funcall(static_frame->sym_make_instance_variable,
								 static_frame->kw_field_name, Str_O::create(fieldDecl->getName().str()),
								 static_frame->kw_class_name, className(classDecl),
								 static_frame->kw_ctype, classified ));
	}
	
	HashTable_sp results()
	{
	    return this->_Result.result();
	}
    };
    DeclarationMatcher InstanceVarMatcher = fieldDecl(hasType(type().bind("type")),hasParent(recordDecl(isDerivedFrom("GCObject")).bind("class"))).bind("field");



    struct InheritanceAccumulator
    {
	HashTable_sp 	_Result;
	InheritanceAccumulator()
	{
	    this->_Result = HashTable_O::create(::cl::_sym_equal->symbolFunction());
	}
	void add(Str_sp key, T_sp val)
	{
	    this->_Result->hash_table_setf_gethash(key,val);
	}
	HashTable_sp result()
	{
	    return this->_Result;
	};
    };



    class InheritanceCallback : public MatchFinder::MatchCallback
    {
    private:
	InheritanceAccumulator 	_Result;
    public:
    public:
	virtual void run(const MatchFinder::MatchResult &Result)
	{
	    Symbol_sp sym_makeInheritanceRelationship;
	    Symbol_sp kw_myclass, kw_bases, kw_vbases;
	    if ( !sym_makeInheritanceRelationship )
	    {
		sym_makeInheritanceRelationship = _lisp->intern("MAKE-INHERITANCE-RELATIONSHIP",CommonLispUserPkg);
		kw_myclass = _lisp->internKeyword("MYCLASS");
		kw_bases = _lisp->internKeyword("BASES");
		kw_vbases = _lisp->internKeyword("VBASES");
	    }
	    
	    const CXXRecordDecl *classDecl = Result.Nodes.getNodeAs<clang::CXXRecordDecl>("class");
	    SYMBOL_EXPORT_SC_(AstToolingPkg,inheritance);
	    Cons_sp bases = _Nil<Cons_O>();
	    for ( CXXRecordDecl::base_class_const_iterator bci=classDecl->bases_begin(); bci!=classDecl->bases_end(); bci++ )
	    {
		QualType qtbase = bci->getType();
		bases = Cons_O::create(Str_O::create(qtbase.getAsString()),bases,_lisp);
	    }
	    Cons_sp vbases = _Nil<Cons_O>();
	    for ( CXXRecordDecl::base_class_const_iterator vbci=classDecl->vbases_begin(); vbci!=classDecl->vbases_end(); vbci++ )
	    {
		QualType qtvbase = vbci->getType();
		vbases = Cons_O::create(Str_O::create(qtvbase.getAsString()),vbases,_lisp);
	    }
	    SYMBOL_EXPORT_SC_(CommonLispUserPkg,makeInheritance);
	    SYMBOL_EXPORT_SC_(KeywordPkg,myclass);
	    SYMBOL_EXPORT_SC_(KeywordPkg,bases);
	    SYMBOL_EXPORT_SC_(KeywordPkg,vbases);
	    this->_Result.add(className(classDecl),
			      eval::funcall(sym_makeInheritanceRelationship,
					    kw_myclass,className(classDecl),
					    kw_bases,bases,
					    kw_vbases,vbases));
	}
	HashTable_sp results()
	{
	    return this->_Result.result();
	}
    };
    DeclarationMatcher InheritanceMatcher = recordDecl(isDerivedFrom("GCObject")).bind("class");
};



namespace asttooling
{
    struct GlobalAccumulator
    {
	HashTable_sp 	_Result;
	GlobalAccumulator()
	{
	    this->_Result = HashTable_O::create(::cl::_sym_equal->symbolFunction());
	}
	void add(Str_sp key, T_sp val )
	{
	    this->_Result->hash_table_setf_gethash(key,val);
	}
	HashTable_sp result()
	{
	    return this->_Result;
	};
    };



    class GlobalCallback : public MatchFinder::MatchCallback
    {
    private:
	GlobalAccumulator 	_Result;
    public:
    public:
	virtual void run(const MatchFinder::MatchResult &Result)
	{
	    const VarDecl *varDecl = Result.Nodes.getNodeAs<clang::VarDecl>("var");
	    if ( varDecl->hasGlobalStorage() )
	    {
#if 0
		if (varDecl->getQualifiedNameAsString() == "_lisp")
		{
		    dbg_hook("caught _lisp");
		}
#endif
		set<const Type*> seenTypes;
		T_sp containsSmartPtr = typeIsOrContainsSmartPtr(varDecl->getType(),seenTypes);
		if ( containsSmartPtr.isTrue() )
		{
		    clang::ASTContext& context = varDecl->getASTContext();
		    clang::SourceManager& sourceManager = context.getSourceManager();
		    const clang::LangOptions& lopt = context.getLangOpts();
		    clang::SourceLocation b(varDecl->getLocStart()), _e(varDecl->getLocEnd());
		    clang::SourceLocation e(clang::Lexer::getLocForEndOfToken(_e,0,sourceManager,lopt));
		    this->_Result.add(Str_O::create(varDecl->getQualifiedNameAsString()),Cons_O::createList(Str_O::create(b.printToString(sourceManager)), Str_O::create(e.printToString(sourceManager)),containsSmartPtr));
		}
	    }
	}
	HashTable_sp results()
	{
	    return this->_Result.result();
	}
    };
    DeclarationMatcher GlobalMatcher = varDecl().bind("var");

};


namespace asttooling
{
    struct LocalAccumulator
    {
	HashTable_sp 	_Result;
	LocalAccumulator()
	{
	    this->_Result = HashTable_O::create(::cl::_sym_equal->symbolFunction());
	}
	void add(Cons_sp key, T_sp val )
	{
	    this->_Result->hash_table_setf_gethash(key,val);
	}
	HashTable_sp result()
	{
	    return this->_Result;
	};
    };



    class LocalCallback : public MatchFinder::MatchCallback
    {
    private:
	LocalAccumulator 	_Parameters;
	LocalAccumulator	_Locals;
    public:
    public:
	virtual void run(const MatchFinder::MatchResult &Result)
	{
	    const VarDecl *varDecl = Result.Nodes.getNodeAs<clang::VarDecl>("var");
	    if ( !varDecl->hasGlobalStorage() )
	    {
		set<const Type*> seenTypes;
		T_sp containsSmartPtr = typeIsOrContainsSmartPtr(varDecl->getType(),seenTypes);
		if ( containsSmartPtr.isTrue() )
		{
		    clang::ASTContext& context = varDecl->getASTContext();
		    clang::SourceManager& sourceManager = context.getSourceManager();
		    const clang::LangOptions& lopt = context.getLangOpts();
		    clang::SourceLocation b(varDecl->getLocStart()), _e(varDecl->getLocEnd());
		    clang::SourceLocation e(clang::Lexer::getLocForEndOfToken(_e,0,sourceManager,lopt));
		    // Key is a cons of the variable name and the source position of the declaration
		    // The local variable name is not going to be unique
		    if ( varDecl->isLocalVarDecl() )
		    {
			this->_Locals.add(Cons_O::createList(Str_O::create(varDecl->getQualifiedNameAsString()),Str_O::create(b.printToString(sourceManager))),
					  Cons_O::createList(Str_O::create(b.printToString(sourceManager)), Str_O::create(e.printToString(sourceManager)),containsSmartPtr));
		    } else
		    {
			this->_Parameters.add(Cons_O::createList(Str_O::create(varDecl->getQualifiedNameAsString()),Str_O::create(b.printToString(sourceManager))),
					      Cons_O::createList(Str_O::create(b.printToString(sourceManager)), Str_O::create(e.printToString(sourceManager)),containsSmartPtr));
		    }
		}
	    }
	}
	HashTable_sp parameters()
	{
	    return this->_Parameters.result();
	}
	HashTable_sp locals()
	{
	    return this->_Locals.result();
	}
    };
    DeclarationMatcher LocalMatcher = varDecl().bind("var");
};



namespace asttooling
{

    /*! Gather all of the filenames into the vector<string> */
    void gatherFileNames(vector<string>& fileNames, CompilationDatabase* db, Cons_sp files,
			 int istart, int iend)
    {
	int idx = 0;
	Cons_sp fileNamesCons = _Nil<Cons_O>();
	if ( files.nilp() )
	{
	    vector<string> allFileNames = db->getAllFiles();
	    iend = MIN(iend,(int)allFileNames.size());
	    if ( istart < allFileNames.size()) {
		for ( idx=istart; idx<iend; ++idx)
		{
		    fileNames.push_back(allFileNames[idx]);
		}
	    }
	} else
	{
	    for (Cons_sp cur = files; cur.notnilp(); cur=cCdr(cur) )
	    {
		string fn = oCar(cur).as<Str_O>()->get();
		if ( istart <= idx && idx < iend )  {
		    fileNames.push_back(fn);
		}
	    }
	}
	eval::funcall(_sym_bformat,_lisp->_true(),Str_O::create("ASTSearch on files: \n"));
	idx = istart;
	for ( vector<string>::iterator it=fileNames.begin(); it!=fileNames.end(); it++ )
	{
	    eval::funcall(_sym_bformat,_lisp->_true(),Str_O::create("  #%d  %s\n"), Fixnum_O::create(idx),Str_O::create(*it));
	    ++idx;
	}
    }

	
#define ARGS_af_astSearch "(search &optional (database \"./compile_commands.json\") files &key (start 0) end)"
#define DECL_af_astSearch ""
#define DOCS_af_astSearch "SEARCH is one of Asttooling:garbage-collector-scanner Asttooling:global-variables Asttooling:local-variables"
    T_mv af_astSearch(Cons_sp searchCons, core::T_sp databasePathnameDesig, core::Cons_sp files, Fixnum_sp start, Fixnum_sp end)
    {_G();
	Pathname_sp pn = af_pathname(databasePathnameDesig);
	Str_sp strpn = af_coerceToFilename(pn);
	string dbname = strpn->get();
	string errors;
	CompilationDatabase* db = JSONCompilationDatabase::loadFromFile(dbname,errors);
	if (db == NULL)
	{
	    SIMPLE_ERROR(BF("Could not load file %s - %s") % _rep_(pn) % errors );
	}
	vector<string> fileNames;
	int istart = start->get();
	int iend = end.nilp() ? 99999999 : end->get();
	gatherFileNames(fileNames,db,files,istart,iend);
	ClangTool Tool(*db,fileNames);
	Cons_sp results = _Nil<Cons_O>();
	MatchFinder Finder;
	bool setupSearch = false;
	InheritanceCallback callbackInheritance;
	InstanceVarCallback callbackInstanceVar;
	GlobalCallback callbackGlobal;
	LocalCallback callbackLocal;

	/* Set up the search */
	for ( Cons_sp cur = searchCons; cur.notnilp(); cur=cCdr(cur) )
	{
	    Symbol_sp search = oCar(cur).as<Symbol_O>();
	    if ( search == _sym_garbageCollectorScanner )
	    {
		SYMBOL_EXPORT_SC_(AstToolingPkg,garbageCollectorScanner);
		Finder.addMatcher(InstanceVarMatcher,&callbackInstanceVar);
		Finder.addMatcher(InheritanceMatcher,&callbackInheritance);
		setupSearch = true;
	    } else if ( search == _sym_globalVariables )
	    {
		SYMBOL_EXPORT_SC_(AstToolingPkg,globalVariables);
		Finder.addMatcher(GlobalMatcher,&callbackGlobal);
		setupSearch = true;
	    } else if ( search == _sym_localVariables )
	    {
		SYMBOL_EXPORT_SC_(AstToolingPkg,localVariables);
		Finder.addMatcher(LocalMatcher,&callbackLocal);
		setupSearch = true;
	    }
	}
	if ( !setupSearch ) SIMPLE_ERROR(BF("Unknown search type %s") % _rep_(searchCons) );
	eval::funcall(::cl::_sym_format,_lisp->_true(),Str_O::create("Starting search for: ~a~%"), searchCons);
	int iresult = Tool.run(newFrontendActionFactory(&Finder));
	/* Set up the results */
	for ( Cons_sp cur = searchCons; cur.notnilp(); cur=cCdr(cur) )
	{
	    Symbol_sp search = oCar(cur).as<Symbol_O>();
	    if ( search == _sym_garbageCollectorScanner )
	    {
		results = Cons_O::create(Cons_O::create(_sym_garbageCollectorScanner,Cons_O::createList(callbackInheritance.results(),callbackInstanceVar.results()),_lisp),
					 results,_lisp);
	    } else if ( search == _sym_globalVariables )
	    {
		results = Cons_O::create(Cons_O::create(_sym_globalVariables,
							callbackGlobal.results(),_lisp),
					 results,_lisp);
	    } else if ( search == _sym_localVariables )
	    {
		results = Cons_O::create(Cons_O::create(_sym_localVariables,
							callbackLocal.locals(),_lisp),
					 results,_lisp);
		SYMBOL_EXPORT_SC_(AstToolingPkg,parameters);
		results = Cons_O::create(Cons_O::create(_sym_parameters,
							callbackLocal.parameters(),_lisp),
					 results,_lisp);
	    }
	}
	return Values(results,Fixnum_O::create(iresult));
    };

    class RewriteReprCallback : public MatchFinder::MatchCallback
    {
    private:
	tooling::Replacements* 	_Replacements;
    public:
	RewriteReprCallback(tooling::Replacements* replacements) : _Replacements(replacements) {};
	virtual void run(const MatchFinder::MatchResult &result)
	{
	    const CXXMemberCallExpr* call = result.Nodes.getNodeAs<clang::CXXMemberCallExpr>("call");
	    const Expr* expr = call->getImplicitObjectArgument();
	    if ( isa<CXXThisExpr>(expr) )
	    {
		return;
	    }
	    clang::ASTContext* context = result.Context;
	    clang::SourceManager* sourceManager = result.SourceManager;
	    const clang::LangOptions& lopt = context->getLangOpts();
	    clang::SourceLocation b = sourceManager->getSpellingLoc((expr->getLocStart()));
	    clang::SourceLocation _e = sourceManager->getSpellingLoc((expr->getLocEnd()));
	    clang::SourceLocation e(clang::Lexer::getLocForEndOfToken(_e,0,*sourceManager,lopt));
	    CharSourceRange range = CharSourceRange::getCharRange(b,e);
	    std::string source = clang::Lexer::getSourceText(range,*sourceManager,lopt);
	    printf(" at:%s to:%s   match: \"%s\"\n", b.printToString(*sourceManager).c_str(), e.printToString(*sourceManager).c_str(), source.c_str() );
	    if ( source != "" )
	    {
		stringstream ss;
		ss << "_rep_(" << source << ")";
		Replacement replace(*result.SourceManager,call,ss.str());
		std::string repText = replace.getReplacementText();
		printf("       replace with: \"%s\"\n", repText.c_str() );
		this->_Replacements->insert(replace);
		printf("       Number of replacements: %ld\n", this->_Replacements->size());
	    }
	}
    };
    StatementMatcher matcherRewriteRepr = memberCallExpr(argumentCountIs(0),callee(methodDecl(hasName("__repr__")))).bind("call");


#define ARGS_af_astRewriteRepr "(&optional (database \"./compile_commands.json\") files &key save (start 0) end)"
#define DECL_af_astRewriteRepr ""
#define DOCS_af_astRewriteRepr "astRewriteRepr"
    T_mv af_astRewriteRepr(core::Str_sp database, core::Cons_sp files, bool save, Fixnum_sp start, Fixnum_sp end)
    {_G();
 	string dbname = database->get();
	string errors;
	CompilationDatabase* db = JSONCompilationDatabase::loadFromFile(dbname,errors);
	if (db == NULL)
	{
	    SIMPLE_ERROR(BF("Could not load file %s - %s") % database->get() % errors );
	}
	vector<string> fileNames;
	int istart = start->get();
	int iend = end.nilp() ? 9999999 : end->get();
	gatherFileNames(fileNames,db,files,istart,iend);
	RefactoringTool tool(*db,fileNames);
	MatchFinder finder;
	RewriteReprCallback callbackRewriteRepr(&tool.getReplacements());
	finder.addMatcher(matcherRewriteRepr,&callbackRewriteRepr);
	int iresult;
	if ( save )
	{
	    printf("Running tool and saving results\n");
	    iresult = tool.runAndSave(newFrontendActionFactory(&finder));
	} else
	{
	    iresult = tool.run(newFrontendActionFactory(&finder));
	}
	// Extract the results here
	printf("Done iresult=%d  total replacements=%ld\n", iresult, tool.getReplacements().size());
	return Values(Fixnum_O::create(iresult));
    };



    class RewriteNullaryMemberFunctionsCallback : public MatchFinder::MatchCallback
    {
    private:
	HashTable_sp 		_Conversions;
	tooling::Replacements* 	_Replacements;
    public:
	RewriteNullaryMemberFunctionsCallback(HashTable_sp conversions,
					      tooling::Replacements* replacements)
	    : _Conversions(conversions), _Replacements(replacements) {};
	virtual void run(const MatchFinder::MatchResult &result)
	{
	    const CXXMemberCallExpr* call = result.Nodes.getNodeAs<clang::CXXMemberCallExpr>("call");
	    const Expr* expr = call->getImplicitObjectArgument();
	    clang::ASTContext* context = result.Context;
	    clang::SourceManager* sourceManager = result.SourceManager;
	    const clang::LangOptions& lopt = context->getLangOpts();
	    clang::SourceLocation b = sourceManager->getSpellingLoc((expr->getLocStart()));
	    clang::SourceLocation _e = sourceManager->getSpellingLoc((expr->getLocEnd()));
	    clang::SourceLocation e(clang::Lexer::getLocForEndOfToken(_e,0,*sourceManager,lopt));
	    CharSourceRange range = CharSourceRange::getCharRange(b,e);
	    const CXXMethodDecl* methodDecl = call->getMethodDecl();
	    if ( !methodDecl )
	    {
                std::cerr << "Could not process match at " << b.printToString(*sourceManager) << std::endl;
		call->dump();
		return;
	    }
	    Str_sp memberName = Str_O::create(methodDecl->getNameAsString());
	    Str_sp transform = this->_Conversions->gethash(memberName,_Unbound<Str_O>()).as<Str_O>();
	    if ( transform.unboundp() ) return;
	    std::string source = clang::Lexer::getSourceText(range,*sourceManager,lopt);
            std::cerr << (BF("Match at:%s to:%s   member function[%s]  expression[%s]")
		     % b.printToString(*sourceManager)
		     % e.printToString(*sourceManager)
		     % memberName->get()
                          % source ) << std::endl;
	    if ( source != "" )
	    {
		stringstream ss;
		ss << transform->get() << "(" << source << ")";
		Replacement replace(*result.SourceManager,call,ss.str());
		std::string repText = replace.getReplacementText();
		this->_Replacements->insert(replace);
		std::cerr << "        my replacement text: [" << ss.str() << "]" << std::endl;
		std::cerr << "        replace.getReplacementText(): \"" << repText << "\"" << " accumulated replacements " << this->_Replacements->size() << std::endl << std::endl;
	    }
	}
    };


#define ARGS_af_astRewriteNullaryMemberFunctions "(conversions &optional (database \"./compile_commands.json\") files &key save (start 0) end)"
#define DECL_af_astRewriteNullaryMemberFunctions ""
#define DOCS_af_astRewriteNullaryMemberFunctions "astRewriteNullaryMemberFunctions"
    T_mv af_astRewriteNullaryMemberFunctions(HashTable_sp conversions, core::Str_sp database, core::Cons_sp files, bool save, Fixnum_sp start, Fixnum_sp end)
    {_G();
 	string dbname = database->get();
	string errors;
	CompilationDatabase* db = JSONCompilationDatabase::loadFromFile(dbname,errors);
	if (db == NULL)
	{
	    SIMPLE_ERROR(BF("Could not load file %s - %s") % database->get() % errors );
	}
	vector<string> fileNames;
	int istart = start->get();
	int iend = end.nilp() ? 9999999 : end->get();
	gatherFileNames(fileNames,db,files,istart,iend);
	RefactoringTool tool(*db,fileNames);
	MatchFinder finder;
	RewriteNullaryMemberFunctionsCallback
	    callbackRewriteNullaryMemberFunctions(conversions,
						  &tool.getReplacements());
	finder.addMatcher(memberCallExpr(argumentCountIs(0)).bind("call"),
			  &callbackRewriteNullaryMemberFunctions);
	std::cerr << "Matcher completed search" << std::endl;
	int iresult;
	if ( save )
	{
	    printf("Running tool and saving results\n");
	    iresult = tool.runAndSave(newFrontendActionFactory(&finder));
	} else
	{
	    iresult = tool.run(newFrontendActionFactory(&finder));
	}
	// Extract the results here
	printf("Done iresult=%d  total replacements=%ld\n", iresult, tool.getReplacements().size());
	return Values(Fixnum_O::create(iresult));
    };



    class RewriteFunctionNamesCallback : public MatchFinder::MatchCallback
    {
    private:
	HashTable_sp 		_Conversions;
	tooling::Replacements* 	_Replacements;
    public:
	RewriteFunctionNamesCallback(HashTable_sp conversions,
					      tooling::Replacements* replacements)
	    : _Conversions(conversions), _Replacements(replacements) {};
	virtual void run(const MatchFinder::MatchResult &result)
	{
	    const CallExpr* call = result.Nodes.getNodeAs<clang::CallExpr>("call");
	    const Expr* expr = call->getCallee();
	    clang::ASTContext* context = result.Context;
	    clang::SourceManager* sourceManager = result.SourceManager;
	    const clang::LangOptions& lopt = context->getLangOpts();
	    clang::SourceLocation b = sourceManager->getSpellingLoc((expr->getLocStart()));
	    clang::SourceLocation _e = sourceManager->getSpellingLoc((expr->getLocEnd()));
	    clang::SourceLocation e(clang::Lexer::getLocForEndOfToken(_e,0,*sourceManager,lopt));
	    CharSourceRange range = CharSourceRange::getCharRange(b,e);
	    const FunctionDecl* funcDecl = call->getDirectCallee();
	    if ( !funcDecl )
	    {
		std::cerr << "Could not process match at " << b.printToString(*sourceManager) << std::endl;
		call->dump();
		return;
	    }
	    Str_sp memberName = Str_O::create(funcDecl->getNameAsString());
	    Str_sp transform = this->_Conversions->gethash(memberName,_Unbound<Str_O>()).as<Str_O>();
	    if ( transform.unboundp() ) return;
	    std::string source = clang::Lexer::getSourceText(range,*sourceManager,lopt);
	    std::cerr << (BF("Match at:%s to:%s   member function[%s]  expression[%s]")
		     % b.printToString(*sourceManager)
		     % e.printToString(*sourceManager)
		     % memberName->get()
		     % source ) << std::endl;
	    if ( source != "" )
	    {
		stringstream ss;
		ss << transform->get();
		Replacement replace(*result.SourceManager,expr,ss.str());
		std::string repText = replace.getReplacementText();
		this->_Replacements->insert(replace);
		std::cerr << "        my replacement text: [" << ss.str() << "]" << std::endl;
		std::cerr << "        replace.getReplacementText(): \"" << repText << "\"" << " accumulated replacements " << this->_Replacements->size() << std::endl << std::endl;
	    }
	}
    };


#define ARGS_af_astRewriteFunctionNames "(conversions &optional (database \"./compile_commands.json\") files &key save (start 0) end)"
#define DECL_af_astRewriteFunctionNames ""
#define DOCS_af_astRewriteFunctionNames "astRewriteFunctionNames"
    T_mv af_astRewriteFunctionNames(HashTable_sp conversions, core::Str_sp database, core::Cons_sp files, bool save, Fixnum_sp start, Fixnum_sp end)
    {_G();
 	string dbname = database->get();
	string errors;
	CompilationDatabase* db = JSONCompilationDatabase::loadFromFile(dbname,errors);
	if (db == NULL)
	{
	    SIMPLE_ERROR(BF("Could not load file %s - %s") % database->get() % errors );
	}
	vector<string> fileNames;
	int istart = start->get();
	int iend = end.nilp() ? 9999999 : end->get();
	gatherFileNames(fileNames,db,files,istart,iend);
	RefactoringTool tool(*db,fileNames);
	MatchFinder finder;
	RewriteFunctionNamesCallback
	    callbackRewriteFunctionNames(conversions,
						  &tool.getReplacements());
	finder.addMatcher(callExpr().bind("call"),
			  &callbackRewriteFunctionNames);
	std::cerr << "Matcher completed search" << std::endl;
	int iresult;
	if ( save )
	{
	    printf("Running tool and saving results\n");
	    iresult = tool.runAndSave(newFrontendActionFactory(&finder));
	} else
	{
	    iresult = tool.run(newFrontendActionFactory(&finder));
	}
	// Extract the results here
	printf("Done iresult=%d  total replacements=%ld\n", iresult, tool.getReplacements().size());
	return Values(Fixnum_O::create(iresult));
    };



    
    
    
#define ARGS_af_testStructureCreation "()"
#define DECL_af_testStructureCreation ""
#define DOCS_af_testStructureCreation "testStructureCreation"
    T_sp af_testStructureCreation()
    {_G();
	STATIC_ROOT_FRAME_BEGIN(StaticFrame) {
	    Symbol_sp sym_make_smart_ptr_ctype;
	    Symbol_sp sym_makeWeakSmartPtrCtype;
	    Symbol_sp kw_specializer;
	    StaticFrame() {
		sym_make_smart_ptr_ctype = lispify_intern("make-smart-ptr-ctype",CommonLispUserPkg);
		sym_makeWeakSmartPtrCtype = lispify_intern("makeWeakSmartPtrCtype",CommonLispUserPkg);
		kw_specializer = lispify_intern("specializer",KeywordPkg);
		this->attachToGCRoot();
	    };
	    GC_SCANNER() {
		GC_SCANNER_BEGIN() {
		    SMART_PTR_FIX(sym_make_smart_ptr_ctype);
		    SMART_PTR_FIX(sym_makeWeakSmartPtrCtype);
		    SMART_PTR_FIX(kw_specializer);
		} GC_SCANNER_END();
		return GC_RES_OK;
	    };
	} STATIC_ROOT_FRAME_END(StaticFrame,static_frame);
	return eval::funcall(static_frame->sym_make_smart_ptr_ctype,
			     static_frame->kw_specializer,Str_O::create("dummy specializer"));
    };



#if 0
    class ASTVisitor : public RecursiveASTVisitor<ASTVisitor> {
    public:
	explicit BrclASTVisitor(ASTContext *Context) : Context(Context) {};
	bool VisitCXXRecordDecl(CXXRecordDecl *val) {
	    if ( _sym_VisitCXXRecordDecl->symbolValue().isTrue() ) {
		return eval::funcall(_sym_VisitCXXRecordDecl->symbolValue(),translate::to_object(val)).isTrue();
	    }
	    return true;
	}

    private:
	ASTContext *Context;
    };



    class BrclASTClassConsumer : public clang::ASTConsumer {
    public:
	explicit BrclASTClassConsumer(ASTContext *Context)
	    : Visitor(Context) {}

	virtual void HandleTranslationUnit(clang::ASTContext &Context) {
	    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
	}
    private:
	BrclASTClassVisitor Visitor;
    };

    class BrclASTClassAction : public clang::ASTFrontendAction {
    public:
	virtual clang::ASTConsumer *CreateASTConsumer(
	    clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
	    return new BrclASTClassConsumer(&Compiler.getASTContext());
	}
    };
#endif







    void initialize_tools()
    {
	SYMBOL_EXPORT_SC_(AstToolingPkg,astSearch);
	Defun(astSearch);
	SYMBOL_EXPORT_SC_(AstToolingPkg,astRewriteRepr);
	Defun(astRewriteRepr);
	SYMBOL_EXPORT_SC_(AstToolingPkg,astRewriteNullaryMemberFunctions);
	Defun(astRewriteNullaryMemberFunctions);
	SYMBOL_EXPORT_SC_(AstToolingPkg,astRewriteFunctionNames);
	Defun(astRewriteFunctionNames);
	SYMBOL_EXPORT_SC_(AstToolingPkg,testStructureCreation);
	Defun(testStructureCreation);

    };

}

