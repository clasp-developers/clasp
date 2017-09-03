/*
    File: clc-env.h
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
#ifndef _core_clc_env_H_
#define _core_clc_env_H_

#include <clasp/core/object.h>
#include <clasp/core/clcenv.fwd.h>


namespace clcenv {

  
  FORWARD(Entry);
  class Entry_O : public core::CxxObject_O {
    LISP_CLASS(clcenv,ClcenvPkg,Entry_O,"Entry",core::CxxObject_O);
  public:
    core::T_sp _Next;
  public:
    virtual core::T_sp ignore() const { SUBIMP(); };
    explicit Entry_O() : _Next(_Nil<core::T_O>()) {};
    explicit Entry_O(core::T_sp next) : _Next(next) {};
    virtual ~Entry_O(){};
  };

    /*! A dummy class that represents the global environment.
It's something to dispatch on */
  FORWARD(GlobalEnvironment);
  class GlobalEnvironment_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,GlobalEnvironment_O,"GlobalEnvironment",Entry_O);
  public:
    core::T_sp speed() const;
    core::T_sp debug() const;
    core::T_sp space() const;
    core::T_sp compilation_speed() const;
    core::T_sp safety() const;
    explicit GlobalEnvironment_O() {};
    virtual ~GlobalEnvironment_O(){};
  };

  FORWARD(LexicalVariable);
  class LexicalVariable_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,LexicalVariable_O,"LexicalVariable",Entry_O);
  public:
    core::T_sp _Name;
    core::T_sp _Identity;
  public:
    /*METHODS*/
    CL_LISPIFY_NAME("CLCENV:INFO-IDENTITY");
    CL_DEFMETHOD     virtual core::T_sp identity() const { return this->_Identity; };
  public:
  LexicalVariable_O() : _Name(_Unbound<core::T_O>()), _Identity(_Unbound<core::T_O>()) {};
  LexicalVariable_O(core::T_sp env, core::T_sp name, core::T_sp identity) :
    Entry_O(env)
      , _Name(name)
      , _Identity(identity)
    {};
    virtual ~LexicalVariable_O() {};
  };

  FORWARD(SpecialVariable);
  class SpecialVariable_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,SpecialVariable_O,"SpecialVariable",Entry_O);
  public:
    core::T_sp _Name;
  public:
    /*METHODS*/
  public:
  SpecialVariable_O() : _Name(_Unbound<core::T_O>())
    {};
  SpecialVariable_O(core::T_sp env, core::T_sp name) :
    Entry_O(env)
      , _Name(name)
    {};
    virtual ~SpecialVariable_O() {};
  };

  FORWARD(SymbolMacro);
  class SymbolMacro_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,SymbolMacro_O,"SymbolMacro",Entry_O);
  public:
    core::T_sp _Name;
    core::T_sp _Expansion;
  public:
    /*METHODS*/
  public:
  explicit SymbolMacro_O() : _Name(_Unbound<core::T_O>())
      ,_Expansion(_Unbound<core::T_O>())
    {};
  explicit SymbolMacro_O(core::T_sp env, core::T_sp name, core::T_sp expansion) :
    Entry_O(env)
      , _Name(name)
      ,_Expansion(expansion)
    {};
    virtual ~SymbolMacro_O() {};
  };

  /*! Add this to the environment to whenever a FUNCTION special operator
is encountered */
  FORWARD(Closure);
  class Closure_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,Closure_O,"Closure",Entry_O);
  public:
//    core::T_sp _Name;
//    core::T_sp _Identity;
  public:
    /*METHODS*/
  public:
    explicit Closure_O()
//      : _Name(_Unbound<core::T_O>())
//      ,_Identity(_Unbound<core::T_O>())
    {};
    explicit Closure_O(core::T_sp env) //, core::T_sp name, core::T_sp identity) :
      : Entry_O(env)
//      , _Name(name)
//      , _Identity(identity)
    {};
    virtual ~Closure_O() {};
  };

    FORWARD(Function);
  class Function_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,Function_O,"Function",Entry_O);
  public:
    core::T_sp _Name;
    core::T_sp _Identity;
  public:
    /*METHODS*/
    CL_LISPIFY_NAME("CLCENV:INFO-IDENTITY");
    CL_DEFMETHOD     virtual core::T_sp identity() const { return this->_Identity; };
  public:
    explicit Function_O() : _Name(_Unbound<core::T_O>())
      ,_Identity(_Unbound<core::T_O>())
    {};
    explicit Function_O(core::T_sp env, core::T_sp name, core::T_sp identity) :
    Entry_O(env)
      , _Name(name)
      , _Identity(identity)
    {};
    virtual ~Function_O() {};
  };

  FORWARD(Macro);
  class Macro_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,Macro_O,"Macro",Entry_O);
  public:
    core::T_sp _Name;
    core::T_sp _Expander;
  public:
    /*METHODS*/
  public:
  explicit Macro_O() : _Name(_Unbound<core::T_O>())
      ,_Expander(_Unbound<core::T_O>())
    {};
    explicit Macro_O(core::T_sp env, core::T_sp name, core::T_sp expander) : Entry_O(env)
      , _Name(name)
      ,_Expander(expander)
    {};
    virtual ~Macro_O() {};
  };

  FORWARD(Block);
  class Block_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,Block_O,"Block",Entry_O);
  public:
    core::T_sp _Name;
    core::T_sp _Identity;
  public:
    /*METHODS*/
    CL_LISPIFY_NAME("CLCENV:INFO-IDENTITY");
    CL_DEFMETHOD     virtual core::T_sp identity() const { return this->_Identity; };
  public:
  explicit Block_O() : _Name(_Unbound<core::T_O>())
      ,_Identity(_Unbound<core::T_O>())
    {};
    explicit Block_O(core::T_sp env, core::T_sp name, core::T_sp identity ) : Entry_O(env)
      , _Name(name)
      , _Identity(identity)
    {};
    virtual ~Block_O() {};
  };

  FORWARD(Tag);
  class Tag_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,Tag_O,"Tag",Entry_O);
  public:
    core::T_sp _Name;
    core::T_sp _Identity;
  public:
    /*METHODS*/
    CL_LISPIFY_NAME("CLCENV:INFO-IDENTITY");
    CL_DEFMETHOD     virtual core::T_sp identity() const { return this->_Identity; };
  public:
  Tag_O() : _Name(_Unbound<core::T_O>())
      ,_Identity(_Unbound<core::T_O>())
    {};
    explicit Tag_O(core::T_sp env, core::T_sp name, core::T_sp identity ) : Entry_O(env)
      , _Name(name)
      , _Identity(identity)
    {};
    virtual ~Tag_O() {};
  };

  FORWARD(VariableType);
  class VariableType_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,VariableType_O,"VariableType",Entry_O);
  public:
    core::T_sp _Name;
    core::T_sp _Type;
  public:
    /*METHODS*/
  public:
  VariableType_O() : _Name(_Unbound<core::T_O>())
      ,_Type(_Unbound<core::T_O>())
    {};
    explicit VariableType_O(core::T_sp env, core::T_sp name, core::T_sp type ) : Entry_O(env)
      , _Name(name)
      , _Type(type)
    {};
    virtual ~VariableType_O() {};
  };

  FORWARD(FunctionType);
  class FunctionType_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,FunctionType_O,"FunctionType",Entry_O);
  public:
    core::T_sp _Name;
    core::T_sp _Type;
  public:
    /*METHODS*/
  public:
  FunctionType_O() : _Name(_Unbound<core::T_O>())
      ,_Type(_Unbound<core::T_O>())
    {};
    explicit FunctionType_O(core::T_sp env, core::T_sp name, core::T_sp type ) : Entry_O(env)
      , _Name(name)
      , _Type(type)
    {};
    virtual ~FunctionType_O() {};
  };

  FORWARD(LambdaName);
  class LambdaName_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,LambdaName_O,"LambdaName",Entry_O);
  public:
    core::T_sp _Name;
  public:
    /*METHODS*/
  public:
  LambdaName_O() : _Name(cl::_sym_lambda)
    {};
    explicit LambdaName_O(core::T_sp env, core::T_sp name ) : Entry_O(env)
      , _Name(name)
    {};
    virtual ~LambdaName_O() {};
  };

  FORWARD(VariableIgnore);
  class VariableIgnore_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,VariableIgnore_O,"VariableIgnore",Entry_O);
  public:
    core::T_sp _Name;
    core::T_sp _Ignore;
  public:
    /*METHODS*/
    virtual core::T_sp ignore() const override { return this->_Ignore; };
  public:
  VariableIgnore_O() : _Name(_Unbound<core::T_O>())
      ,_Ignore(_Unbound<core::T_O>())
    {};
    explicit VariableIgnore_O(core::T_sp env, core::T_sp name, core::T_sp ignore ) : Entry_O(env)
      , _Name(name)
      , _Ignore(ignore)
    {};
    virtual ~VariableIgnore_O() {};
  };

  FORWARD(FunctionIgnore);
  class FunctionIgnore_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,FunctionIgnore_O,"FunctionIgnore",Entry_O);
  public:
    core::T_sp _Name;
    core::T_sp _Ignore;
  public:
    /*METHODS*/
    virtual core::T_sp ignore() const override { return this->_Ignore; };
  public:
  FunctionIgnore_O() : _Name(_Unbound<core::T_O>())
      ,_Ignore(_Unbound<core::T_O>())
    {};
    explicit FunctionIgnore_O(core::T_sp env, core::T_sp name, core::T_sp ignore ) : Entry_O(env)
      , _Name(name)
      , _Ignore(ignore)
    {};
    virtual ~FunctionIgnore_O() {};
  };

  FORWARD(VariableDynamicExtent);
  class VariableDynamicExtent_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,VariableDynamicExtent_O,"VariableDynamicExtent",Entry_O);
  public:
    core::T_sp _Name;
  public:
    /*METHODS*/
  public:
  VariableDynamicExtent_O() : _Name(_Unbound<core::T_O>())
    {};
    explicit VariableDynamicExtent_O(core::T_sp env, core::T_sp name ) : Entry_O(env)
      , _Name(name)
    {};
    virtual ~VariableDynamicExtent_O() {};
  };
  
  FORWARD(FunctionDynamicExtent);
  class FunctionDynamicExtent_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,FunctionDynamicExtent_O,"FunctionDynamicExtent",Entry_O);
  public:
    core::T_sp _Name;
  public:
    /*METHODS*/
  public:
  FunctionDynamicExtent_O() : _Name(_Unbound<core::T_O>())
    {};
    explicit FunctionDynamicExtent_O(core::T_sp env, core::T_sp name ) : Entry_O(env)
      , _Name(name)
    {};
    virtual ~FunctionDynamicExtent_O() {};
  };

  FORWARD(Optimize);
  class Optimize_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,Optimize_O,"Optimize",Entry_O);
  public:
    core::T_sp _Quality;
    core::T_sp _Value;
  public:
    /*METHODS*/
  public:
  Optimize_O() : _Quality(_Unbound<core::T_O>())
      , _Value(_Unbound<core::T_O>())
    {};
    explicit Optimize_O(core::T_sp env, core::T_sp quality, core::T_sp value ) : Entry_O(env)
      , _Quality(quality)
      , _Value(value)
    {};
    virtual ~Optimize_O() {};
  };

  FORWARD(Inline);
  class Inline_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,Inline_O,"Inline",Entry_O);
  public:
    core::T_sp _Name;
    core::T_sp _Inline;
  public:
    /*METHODS*/
  public:
  Inline_O() : _Name(_Unbound<core::T_O>())
      , _Inline(_Unbound<core::T_O>())
    {};
    explicit Inline_O(core::T_sp env, core::T_sp name, core::T_sp inlineit ) : Entry_O(env)
      , _Name(name)
      , _Inline(inlineit)
    {};
    virtual ~Inline_O() {};
  };

};



namespace clcenv {

  FORWARD(Info);
  class Info_O : public core::CxxObject_O {
    LISP_CLASS(clcenv,ClcenvPkg,Info_O,"Info",core::CxxObject_O);
  public:
    CL_LISPIFY_NAME("CLCENV:INFO-NAME");
    CL_DEFMETHOD virtual core::T_sp name() const { SUBIMP(); };
    CL_LISPIFY_NAME("CLCENV:INFO-TYPE");
    CL_DEFMETHOD virtual core::T_sp type() const { SUBIMP(); };
    CL_LISPIFY_NAME("CLCENV:INFO-INLINE");
    CL_DEFMETHOD virtual core::T_sp inline_() const { SUBIMP(); };
    CL_LISPIFY_NAME("CLCENV:INFO-IDENTITY");
    CL_DEFMETHOD virtual core::T_sp identity() const { SUBIMP(); };
    Info_O() {};
    virtual ~Info_O() {};
//    virtual clc::Ast_sp convert_form(ARGS_form_env_rest);
  };

  FORWARD(VariableInfo);
  class VariableInfo_O : public Info_O {
    LISP_CLASS(clcenv,ClcenvPkg,VariableInfo_O,"VariableInfo",Info_O);
  public:
  public:
    /*METHODS*/
  public:
    VariableInfo_O(){};
    virtual ~VariableInfo_O() {};
  };

  FORWARD(FunctionInfo);
  class FunctionInfo_O : public Info_O {
    LISP_CLASS(clcenv,ClcenvPkg,FunctionInfo_O,"FunctionInfo",Info_O);
  public:
  public:
    /*METHODS*/
  public:
    FunctionInfo_O(){};
    virtual ~FunctionInfo_O() {};
  };

  FORWARD(LexicalVariableInfo);
  class LexicalVariableInfo_O : public VariableInfo_O {
    LISP_CLASS(clcenv,ClcenvPkg,LexicalVariableInfo_O,"LexicalVariableInfo",VariableInfo_O);
  public:
    core::T_sp _Name;
    core::T_sp _Identity;
    core::T_sp _Type;
    core::T_sp _Ignore;
    core::T_sp _DynamicExtent;
  public:
    /*METHODS*/
    core::T_sp name() const override { return this->_Name; };
    CL_LISPIFY_NAME("CLCENV:INFO-IDENTITY");
    CL_DEFMETHOD     virtual core::T_sp identity() const { return this->_Identity; };
  public:
  LexicalVariableInfo_O() :
    _Name(_Unbound<core::T_O>())
      , _Identity(_Unbound<core::T_O>())
      , _Type(_lisp->_true())
      , _Ignore(_Nil<core::T_O>())
      , _DynamicExtent(_Nil<core::T_O>())
    {};
    virtual ~LexicalVariableInfo_O() {};
  };

    

  FORWARD(SpecialVariableInfo);
  class SpecialVariableInfo_O : public VariableInfo_O {
    LISP_CLASS(clcenv,ClcenvPkg,SpecialVariableInfo_O,"SpecialVariableInfo",VariableInfo_O);
  public:
    core::T_sp _Name;
    core::T_sp _Type;
    core::T_sp _Ignore;
    core::T_sp _GlobalP;
  public:
    /*METHODS*/
    core::T_sp name() const override { return this->_Name; };
  public:
  SpecialVariableInfo_O() :
    _Name(_Unbound<core::T_O>())
      , _Type(_lisp->_true())
      , _Ignore(_Nil<core::T_O>())
      , _GlobalP(_Nil<core::T_O>())
    {};
    virtual ~SpecialVariableInfo_O() {};
  };

  FORWARD(ConstantVariableInfo);
  class ConstantVariableInfo_O : public VariableInfo_O {
    LISP_CLASS(clcenv,ClcenvPkg,ConstantVariableInfo_O,"ConstantVariableInfo",VariableInfo_O);
  public:
    core::T_sp _Name;
    core::T_sp _Value;
  public:
    /*METHODS*/
    core::T_sp name() const override { return this->_Name; };
  public:
  ConstantVariableInfo_O() :
    _Name(_Unbound<core::T_O>())
      , _Value(_Unbound<core::T_O>())
    {};
    virtual ~ConstantVariableInfo_O() {};
  };

  FORWARD(SymbolMacroInfo);
  class SymbolMacroInfo_O : public VariableInfo_O {
    LISP_CLASS(clcenv,ClcenvPkg,SymbolMacroInfo_O,"SymbolMacroInfo",VariableInfo_O);
  public:
    core::T_sp _Name;
    core::T_sp _Type;
    core::T_sp _Expansion;
  public:
    /*METHODS*/
    core::T_sp name() const override { return this->_Name; };
  public:
  SymbolMacroInfo_O() :
    _Name(_Unbound<core::T_O>())
      , _Type(_lisp->_true())
      , _Expansion(_Unbound<core::T_O>())
    {};
    virtual ~SymbolMacroInfo_O() {};
  };

  FORWARD(LocalFunctionInfo);
  class LocalFunctionInfo_O : public FunctionInfo_O {
    LISP_CLASS(clcenv,ClcenvPkg,LocalFunctionInfo_O,"LocalFunctionInfo",FunctionInfo_O);
  public:
    core::T_sp _Name;
    core::T_sp _Identity;
    core::T_sp _Type;
    core::T_sp _Inline;
    core::T_sp _Ignore;
    core::T_sp _Ast;
    core::T_sp _DynamicExtent;
  public:
    /*METHODS*/
    core::T_sp name() const override { return this->_Name; };
    core::T_sp inline_() const override { return this->_Inline; };
    CL_LISPIFY_NAME("CLCENV:INFO-IDENTITY");
    CL_DEFMETHOD     virtual core::T_sp identity() const { return this->_Identity; };
  public:
  LocalFunctionInfo_O() :
    _Name(_Unbound<core::T_O>())
      , _Identity(_Unbound<core::T_O>())
      , _Type(_lisp->_true())
      , _Inline(_Nil<core::T_O>())
      , _Ignore(_Nil<core::T_O>())
      , _Ast(_Nil<core::T_O>())
      , _DynamicExtent(_Nil<core::T_O>())
    {};
    virtual ~LocalFunctionInfo_O() {};
  };

  FORWARD(GlobalFunctionInfo);
  class GlobalFunctionInfo_O : public FunctionInfo_O {
    LISP_CLASS(clcenv,ClcenvPkg,GlobalFunctionInfo_O,"GlobalFunctionInfo",FunctionInfo_O);
  public:
    core::T_sp _Name;
    core::T_sp _Type;
    core::T_sp _Inline;
    core::T_sp _CompilerMacro;
    core::T_sp _Ignore;
    core::T_sp _Ast;
    core::T_sp _DynamicExtent;
  public:
    /*METHODS*/
    core::T_sp name() const override { return this->_Name; };
    core::T_sp inline_() const override { return this->_Inline; };
  public:
  GlobalFunctionInfo_O() :
    _Name(_Unbound<core::T_O>())
      , _Type(_lisp->_true())
      , _Inline(_Nil<core::T_O>())
      , _CompilerMacro(_Nil<core::T_O>())
      , _Ignore(_Nil<core::T_O>())
      , _Ast(_Nil<core::T_O>())
      , _DynamicExtent(_Nil<core::T_O>())
    {};
    virtual ~GlobalFunctionInfo_O() {};
  };

  FORWARD(LambdaNameInfo);
  class LambdaNameInfo_O : public FunctionInfo_O {
    LISP_CLASS(clcenv,ClcenvPkg,LambdaNameInfo_O,"LambdaNameInfo",FunctionInfo_O);
  public:
    core::T_sp _Name;
  public:
    /*METHODS*/
    core::T_sp name() const override { return this->_Name; };
  public:
  LambdaNameInfo_O() : _Name(_Unbound<core::T_O>()) {};
    virtual ~LambdaNameInfo_O() {};
  };

  FORWARD(LocalMacroInfo);
  class LocalMacroInfo_O : public FunctionInfo_O {
    LISP_CLASS(clcenv,ClcenvPkg,LocalMacroInfo_O,"LocalMacroInfo",FunctionInfo_O);
  public:
    core::T_sp _Name;
    core::T_sp _Expander;
  public:
    /*METHODS*/
    core::T_sp name() const override { return this->_Name; };
  public:
  LocalMacroInfo_O() :
    _Name(_Unbound<core::T_O>())
      , _Expander(_Unbound<core::T_O>())
    {};
    virtual ~LocalMacroInfo_O() {};
  };

  FORWARD(GlobalMacroInfo);
  class GlobalMacroInfo_O : public FunctionInfo_O {
    LISP_CLASS(clcenv,ClcenvPkg,GlobalMacroInfo_O,"GlobalMacroInfo",FunctionInfo_O);
  public:
    core::T_sp _Name;
    core::T_sp _Expander;
    core::T_sp _CompilerMacro;
  public:
    /*METHODS*/
    core::T_sp name() const override { return this->_Name; };
  public:
  GlobalMacroInfo_O() :
    _Name(_Unbound<core::T_O>())
      , _Expander(_Unbound<core::T_O>())
      , _CompilerMacro(_Nil<core::T_O>())
    {};
    virtual ~GlobalMacroInfo_O() {};
  };

  FORWARD(SpecialOperatorInfo);
  class SpecialOperatorInfo_O : public FunctionInfo_O {
    LISP_CLASS(clcenv,ClcenvPkg,SpecialOperatorInfo_O,"SpecialOperatorInfo",FunctionInfo_O);
  public:
    core::T_sp _Name;
  public:
    /*METHODS*/
    core::T_sp name() const override { return this->_Name; };
  public:
  SpecialOperatorInfo_O() :
    _Name(_Unbound<core::T_O>())
    {};
    virtual ~SpecialOperatorInfo_O() {};
//    clc::Ast_sp convert_form(ARGS_form_env_rest);
  };

  FORWARD(BlockInfo);
  class BlockInfo_O : public Info_O {
    LISP_CLASS(clcenv,ClcenvPkg,BlockInfo_O,"BlockInfo",Info_O);
  public:
    core::T_sp _Name;
    core::T_sp _Identity;
  public:
    /*METHODS*/
    core::T_sp name() const override { return this->_Name; };
    CL_LISPIFY_NAME("CLCENV:INFO-IDENTITY");
    CL_DEFMETHOD     virtual core::T_sp identity() const { return this->_Identity; };
  public:
  BlockInfo_O() :
    _Name(_Unbound<core::T_O>())
      , _Identity(_Unbound<core::T_O>())
    {};
    virtual ~BlockInfo_O() {};
  };

  FORWARD(OptimizeInfo);
  class OptimizeInfo_O : public Info_O {
    LISP_CLASS(clcenv,ClcenvPkg,OptimizeInfo_O,"OptimizeInfo",Info_O);
  public:
    core::T_sp _Speed;
    core::T_sp _Debug;
    core::T_sp _Space;
    core::T_sp _Safety;
    core::T_sp _CompilationSpeed;
  public:
    /*METHODS*/
  public:
  OptimizeInfo_O() :
    _Speed(core::clasp_make_fixnum(3))
      , _Debug(core::clasp_make_fixnum(3))
      , _Space(core::clasp_make_fixnum(3))
      , _Safety(core::clasp_make_fixnum(3))
      , _CompilationSpeed(core::clasp_make_fixnum(3))
    {};
    virtual ~OptimizeInfo_O() {};
  };




};

namespace clcenv {
CL_LAMBDA(environment symbol &optional (identity (cl:gensym)));
inline CL_DEFUN Entry_sp add_lexical_variable(core::T_sp env, core::T_sp symbol, core::T_sp identity )
{
  GC_ALLOCATE_VARIADIC(LexicalVariable_O, entry, env, symbol, identity );
  return entry;
}

inline CL_DEFUN Entry_sp add_special_variable(core::T_sp env, core::T_sp symbol)
{
  GC_ALLOCATE_VARIADIC(SpecialVariable_O, entry, env, symbol );
  return entry;
}

inline CL_DEFUN Entry_sp add_local_symbol_macro(core::T_sp env, core::T_sp symbol, core::T_sp expansion )
{
  GC_ALLOCATE_VARIADIC(SymbolMacro_O, entry, env, symbol, expansion );
  return entry;
}

 inline CL_DEFUN Entry_sp add_closure(core::T_sp env)
{
  GC_ALLOCATE_VARIADIC(Closure_O, entry, env);
  return entry;
}

CL_LAMBDA(environment symbol &optional (identity (cl:gensym)));
inline CL_DEFUN Entry_sp add_local_function(core::T_sp env, core::T_sp symbol, core::T_sp identity )
{
  GC_ALLOCATE_VARIADIC(Function_O, entry, env, symbol, identity );
  return entry;
}

inline CL_DEFUN Entry_sp add_local_macro(core::T_sp env, core::T_sp name, core::T_sp expander )
{
  GC_ALLOCATE_VARIADIC(Macro_O, entry, env, name, expander );
  return entry;
}

CL_LAMBDA(environment symbol &optional (identity (cl:gensym)));
inline CL_DEFUN Block_sp add_block(core::T_sp env, core::T_sp symbol, core::T_sp identity )
{
  GC_ALLOCATE_VARIADIC(Block_O, entry, env, symbol, identity );
  return entry;
}

CL_LAMBDA(environment symbol &optional (identity (cl:gensym)));
inline CL_DEFUN Entry_sp add_tag(core::T_sp env, core::T_sp symbol, core::T_sp identity )
{
  GC_ALLOCATE_VARIADIC(Tag_O, entry, env, symbol, identity );
  return entry;
}

inline CL_DEFUN Entry_sp add_variable_type(core::T_sp env, core::T_sp symbol, core::T_sp type )
{
  GC_ALLOCATE_VARIADIC(VariableType_O, entry, env, symbol, type );
  return entry;
}

 inline CL_DEFUN Entry_sp add_function_type(core::T_sp env, core::T_sp symbol, core::T_sp type )
{
  GC_ALLOCATE_VARIADIC(FunctionType_O, entry, env, symbol, type );
  return entry;
}

inline CL_DEFUN Entry_sp add_function_ignore(core::T_sp env, core::T_sp name, core::T_sp ignore )
{
  GC_ALLOCATE_VARIADIC(FunctionIgnore_O, entry, env, name, ignore );
  return entry;
}

 inline CL_DEFUN Entry_sp add_variable_ignore(core::T_sp env, core::T_sp name, core::T_sp ignore )
{
  GC_ALLOCATE_VARIADIC(VariableIgnore_O, entry, env, name, ignore );
  return entry;
}


inline CL_DEFUN Entry_sp add_variable_dynamic_extent(core::T_sp env, core::T_sp symbol)
{
  GC_ALLOCATE_VARIADIC(VariableDynamicExtent_O, entry, env, symbol );
  return entry;
}


inline CL_DEFUN Entry_sp add_function_dynamic_extent(core::T_sp env, core::T_sp symbol)
{
  GC_ALLOCATE_VARIADIC(FunctionDynamicExtent_O, entry, env, symbol );
  return entry;
}

 inline CL_DEFUN Entry_sp add_lambda_name(core::T_sp env, core::T_sp symbol)
{
  GC_ALLOCATE_VARIADIC(LambdaName_O, entry, env, symbol );
  return entry;
}


inline CL_DEFUN Entry_sp add_optimize(core::T_sp env, core::T_sp quality, core::T_sp value )
{
  GC_ALLOCATE_VARIADIC(Optimize_O, entry, env, quality, value );
  return entry;
}

inline CL_DEFUN Entry_sp add_inline(core::T_sp env, core::T_sp name, core::T_sp inlineit )
{
  GC_ALLOCATE_VARIADIC(Inline_O, entry, env, name, inlineit );
  return entry;
}


};

namespace clcenv {
 
  core::T_sp function_info(core::T_sp environment, core::T_sp form);
  core::T_sp variable_info(core::T_sp environment, core::T_sp form);
  core::T_sp global_environment(core::T_sp env);
};

namespace clcenv {
  core::T_sp global_environment(core::T_sp environment);


  core::T_mv clcenv__variable_is_special_p(core::T_sp variable, core::List_sp declarations, Entry_sp env );

  Entry_sp clcenv__augment_environment_with_variable(core::T_sp variable, core::List_sp declarations, Entry_sp env, Entry_sp orig_env );
  Entry_sp clcenv__augment_environment_with_declarations(Entry_sp environment, core::List_sp canonicalized_dspecs);
};
#endif

