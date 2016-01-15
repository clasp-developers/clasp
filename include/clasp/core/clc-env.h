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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/clc-env.fwd.h>

namespace clcenv {

  /*! A dummy class that represents the global environment.
It's something to dispatch on */
  FORWARD(GlobalEnvironment);
  class GlobalEnvironment_O : public core::T_O {
    LISP_CLASS(clcenv,ClcenvPkg,GlobalEnvironment_O,"GlobalEnvironment",core::T_O);
  public:
    core::T_sp speed() const;
    core::T_sp debug() const;
    core::T_sp space() const;
    core::T_sp compilation_speed() const;
    core::T_sp safety() const;
    explicit GlobalEnvironment_O() {};
    virtual ~GlobalEnvironment_O(){};
  };
  
  FORWARD(Entry);
  class Entry_O : public core::T_O {
    LISP_CLASS(clcenv,ClcenvPkg,Entry_O,"Entry",core::T_O);
  public:
    core::T_sp _Next;
  public:
    virtual core::T_sp ignore() const { SUBIMP(); };
    explicit Entry_O() : _Next(_Nil<core::T_O>()) {};
    explicit Entry_O(core::T_sp next) : _Next(next) {};
    virtual ~Entry_O(){};
  };

  FORWARD(LexicalVariable);
  class LexicalVariable_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,LexicalVariable_O,"LexicalVariable",Entry_O);
  public:
    core::T_sp _Name;
    core::T_sp _Identity;
  public:
    /*METHODS*/
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

  FORWARD(Function);
  class Function_O : public Entry_O {
    LISP_CLASS(clcenv,ClcenvPkg,Function_O,"Function",Entry_O);
  public:
    core::T_sp _Name;
    core::T_sp _Identity;
  public:
    /*METHODS*/
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
#endif

