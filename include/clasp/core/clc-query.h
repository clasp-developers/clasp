/*
    File: clc-query.h
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
#ifndef _core_clc_query_H_
#define _core_clc_query_H_

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/clc-env.fwd.h>

namespace clcenv {

  FORWARD(Info);
  class Info_O : public core::T_O {
    LISP_CLASS(clcenv,ClcenvPkg,Info_O,"Info",core::T_O);
  public:
    virtual core::T_sp name() const { SUBIMP(); };
    virtual core::T_sp type() const { SUBIMP(); };
    virtual core::T_sp inline_() const { SUBIMP(); };
    Info_O() {};
    virtual ~Info_O() {};
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
#endif

