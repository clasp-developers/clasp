/*
    File: serialize.h
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
#ifndef _core_serialize_H //[
#define _core_serialize_H

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/core/str.fwd.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/serialize.fwd.h>

namespace core {

class SNode_O;
FORWARD(SNode);

SNode_sp getOrCreateSNodeForObjectIncRefCount(T_sp val);

class SNode_O : public T_O {
  LISP_BASE1(T_O);
  LISP_VIRTUAL_CLASS(core, CorePkg, SNode_O, "SNode");

public: // Simple default ctor/dtor
        //	DEFAULT_CTOR_DTOR(SNode_O);
private:
  int _RefCount;

public:
  static SNode_sp create(T_sp kind, List_sp plist, Vector_sp data);
  static SNode_sp createBranchSNode(Symbol_sp kind);
  static SNode_sp makeAppropriateSNode(T_sp val, HashTable_sp objToSNodeMap);

public:
  virtual T_sp createObject(HashTable_sp ht) { return _Nil<T_O>(); }; // Only BranchSNode does anything

public:
  virtual void archiveBase(core::ArchiveP node) { SIMPLE_ERROR(BF("This should never be called - perhaps you meant to call addAttributeSNode above???")); };

public: // info
  virtual bool leafSNodeP() { return false; };
  bool saving() const { return !this->loading(); };
  bool loading() const;
  virtual T_sp object() const { SUBIMP(); };
  virtual List_sp keys() const { SUBIMP(); };
  /*! Make the appropriate kind of SNode for the type of value */
public:
  void incRefCount() { this->_RefCount++; };
  int refCount() const { return this->_RefCount; };

public: // loading
  /*! Get the raw SNode for the attribute - this is used when POD's are read from archives */
  virtual SNode_sp getAttributeSNode(Symbol_sp name, SNode_sp defValue) const { SUBIMP(); };
  virtual SNode_sp getAttributeSNodeOrError(Symbol_sp name) const { SUBIMP(); };
  virtual T_sp getAttribute(Symbol_sp name, T_sp defaultValue) const { SUBIMP(); };
  inline T_sp getAttributeOrError(Symbol_sp name) {
    T_sp val = this->getAttribute(name, _Unbound<T_O>());
    if (val.unboundp()) {
      SIMPLE_ERROR(BF("Could not find attribute %s in %s") % _rep_(val) % _rep_(this->asSmartPtr()));
    }
    return val;
  }
  virtual Symbol_sp getKind() const { SUBIMP(); };
  virtual void loadVectorSNodes(gctools::Vec0<T_sp> &vec) const { SUBIMP(); };
  virtual List_sp getAttributes() const { SUBIMP(); };
  virtual void addAttributeSNode(Symbol_sp name, SNode_sp node) { SUBIMP(); };
  virtual void addAttribute(Symbol_sp name, T_sp val) { SUBIMP(); };
  virtual T_sp getUniqueId() const { return _Nil<T_O>(); };
  virtual SNode_sp childWithUniqueId(Symbol_sp uid) const { return _Nil<SNode_O>(); };
  virtual Vector_sp getVectorSNodes() const { SUBIMP(); };
  virtual SNode_sp &operator[](size_t i) { SUBIMP(); };
  int vectorSize() const { return this->getVectorSNodes()->length(); }
  virtual void loadVector(gctools::Vec0<T_sp> &vec) { SUBIMP(); };

  virtual void mapVectorSNodes(std::function<void(SNode_sp)> const &fn) { SUBIMP(); };
  virtual void mapVector(std::function<void(T_sp)> const &fn) { SUBIMP(); };

  void needsFinalization() const;

public: // saving
  virtual void setKind(Symbol_sp kind) { SUBIMP(); };
  virtual void setVectorSNodesUnsafe(Vector_sp vec) { SUBIMP(); };
  virtual void setAttributesUnsafe(List_sp plist) { SUBIMP(); };

  virtual void saveVector(gctools::Vec0<T_sp> const &vec) { SUBIMP(); };
  virtual void pushVectorSNode(SNode_sp obj) { SUBIMP(); };
  virtual void pushVector(T_sp obj) { SUBIMP(); };

public: // bidirectional
  template <class T>
  void attribute(const string &name, gctools::smart_ptr<T> &val) {
    Symbol_sp sym = _lisp->internKeyword(name);
    this->attribute<T>(sym, val);
  }

  template <class T>
  void attribute(Symbol_sp name, gctools::smart_ptr<T> &val) {
    if (this->loading()) {
      val = gc::As<gc::smart_ptr<T>>(this->getAttributeOrError(name));
    } else {
      this->addAttribute(name, val);
    }
  }

#if 0
	template <class T>
	void attributeWeakPointer(Symbol_sp name, gctools::weak_smart_ptr<T>& val)
	{
	    if (this->loading()) {
		T_sp o = this->getAttribute(name,_Unbound<T_O>());
		if ( o.unboundp() ) {
		    val = _Nil<T>();
		} else {
		    val = o.as<T>();
		}
	    } else {
		T_sp o = val.lock();
		if ( o.pointerp() ) {
		    this->addAttribute(name,o);
		}
	    }
	}
#endif
  template <class T>
  void attribute(Symbol_sp name, T &val) {
    if (this->loading()) {
      T_sp tval = this->getAttributeOrError(name);
      val = translate::from_object<T>(tval)._v;
    } else {
      T_sp tval = translate::to_object<T>::convert(val);
      this->addAttribute(name, tval);
    }
  }

  template <class T>
  void attributeIfDefined(Symbol_sp name, bool &defined, T &val) {
    if (this->loading()) {
      T_sp tval = this->getAttribute(name, _Unbound<T_O>());
      if (!tval.unboundp()) {
        val = translate::from_object<T>(tval)._v;
        defined = true;
      } else {
        defined = false;
      }
    } else {
      if (defined) {
        T_sp tval = translate::to_object<T>::convert(val);
        this->addAttribute(name, tval);
      }
    }
  }

  template <class T>
  void attribute(string const &name, T &val) {
    this->attribute(_lisp->intern(name), val);
  }

#if 0
	void attribute(Symbol_sp name, int& val)
	{
	    if (this->loading()) {
		T_sp tval = this->getAttributeOrError(name);
		val = tval.as<Integer_O>()->as_int();
	    } else {
		T_sp tval = Integer_O::create(val);
		this->addAttribute(name,tval);
	    }
	}

	void attribute(Symbol_sp name, double& val)
	{
	    if (this->loading()) {
		T_sp tval = this->getAttributeOrError(name);
		val = tval.as<Number_O>()->as_double();
	    } else {
		T_sp tval = DoubleFloat_O::create(val);
		this->addAttribute(name,tval);
	    }
	}

#endif

  template <typename T>
  void attributeIfNotNil(Symbol_sp name, gctools::smart_ptr<T> &val) {
    if (this->loading()) {
      T_sp oval = this->getAttribute(name, _Unbound<T_O>());
      if (oval.unboundp()) {
        val = _Nil<T>();
      } else {
        val = gc::As<T>(oval);
      }
    } else {
      if (val.notnilp()) {
        this->addAttribute(name, val);
      }
    }
  };

  template <typename T>
  void attributeIfNotNil(string const &name, T &val) {
    Symbol_sp kw = _lisp->internKeyword(name);
    this->attributeIfNotNil(kw, val);
  };

  template <typename T>
  void attributeIfNotDefault(Symbol_sp name, T &val, T const &defaultVal) {
    if (this->loading()) {
      T_sp oval = this->getAttribute(name, _Unbound<T_O>());
      if (oval.unboundp()) {
        val = defaultVal;
      } else {
        val = translate::from_object<T>(oval)._v;
      }
    } else {
      if (val != defaultVal) { // ->equal(defaultVal) ) {
        T_sp oval = translate::to_object<T>::convert(val);
        this->addAttribute(name, oval);
      }
    }
  };

  template <typename T>
  void attributeIfNotDefault(string const &name, T &val, T const &defaultVal) {
    Symbol_sp kw = _lisp->internKeyword(name);
    this->attributeIfNotDefault(kw, val, defaultVal);
  };

  template <typename SymbolEnumType>
  void attributeSymbolEnumHiddenConverter(Symbol_sp name, SymbolEnumType &val, Symbol_sp converterName) {
    SymbolToEnumConverter_sp converter = gc::As<SymbolToEnumConverter_sp>(converterName->symbolValue());
    if (this->loading()) {
      T_sp tval = this->getAttributeOrError(name);
      val = converter->enumForSymbol<SymbolEnumType>(gc::As<Symbol_sp>(tval));
    } else {
      converter->throwIfUnrecognizedEnum<SymbolEnumType>(val);
      Symbol_sp enumSym(converter->symbolForEnum<SymbolEnumType>(val));
      this->addAttribute(name, enumSym);
    }
  }

  template <typename SymbolEnumType>
  void attributeSymbolEnumHiddenConverter(string const &name, SymbolEnumType &val, Symbol_sp converterName) {
    Symbol_sp kw = _lisp->internKeyword(name);
    this->attributeSymbolEnumHiddenConverter(kw, val, converterName);
  }

  template <typename SymbolEnumType>
  void attributeSymbolEnumHiddenConverterIfNotDefault(Symbol_sp name, SymbolEnumType &val, Symbol_sp converterName, SymbolEnumType defVal) {
    SymbolToEnumConverter_sp converter = gc::As<SymbolToEnumConverter_sp>(converterName->symbolValue());
    if (this->loading()) {
      T_sp tval = this->getAttribute(name, _Unbound<T_O>());
      if (tval.unboundp()) {
        val = defVal;
      } else {
        val = converter->enumForSymbol<SymbolEnumType>(gc::As<Symbol_sp>(tval));
      }
    } else {
      if (val != defVal) {
        converter->throwIfUnrecognizedEnum<SymbolEnumType>(val);
        Symbol_sp enumSym(converter->symbolForEnum<SymbolEnumType>(val));
        this->addAttribute(name, enumSym);
      }
    }
  }

  template <typename SymbolEnumType>
  void attributeSymbolEnumHiddenConverterIfNotDefault(string const &name, SymbolEnumType &val, Symbol_sp converterName, SymbolEnumType defVal) {
    Symbol_sp kw = _lisp->internKeyword(name);
    this->attributeSymbolEnumHiddenConverterIfNotDefault(kw, val, converterName, defVal);
  }

  /*! Archive any POD class.  All it neds is a archive method
	 * void archive(ArchiveP node);
	 */
  template <class SimpleClass>
  void attributePOD(Symbol_sp name, Symbol_sp nodeName, SimpleClass &plainObject) {
    _G();
    if (this->loading()) {
      _BLOCK_TRACE("Loading");
      SNode_sp plainNode = this->getAttributeSNode(name, _Unbound<SNode_O>());
      if (plainNode.unboundp()) {
        SIMPLE_ERROR(BF("Could not find node %s") % _rep_(name));
      }
      if (plainNode->getKind() != nodeName) {
        SIMPLE_ERROR(BF("Expecting nodeName(%s) got(%s)") % _rep_(nodeName) % _rep_(plainNode->getKind()));
      }
      plainObject.archive(plainNode);
    } else {
      _BLOCK_TRACE("Saving");
      SNode_sp plainNode = createBranchSNode(nodeName);
      this->addAttributeSNode(name, plainNode);
      plainObject.archive(plainNode);
    }
  }

  template <class SimpleClass>
  void attributePOD(string const &name, string const &nodeName, SimpleClass &plainObject) {
    _G();
    this->attributePOD(_lisp->internKeyword(name), _lisp->internKeyword(nodeName), plainObject);
  }

  /*! Archive any POD class.  All it neds is a archive method
	 * void archive(ArchiveP node);
	 */
  template <class SimpleClass>
  void attributePODIfDefined(Symbol_sp name, Symbol_sp nodeName, bool &defined, SimpleClass &plainObject) {
    _G();
    if (this->loading()) {
      _BLOCK_TRACE("Loading");
      SNode_sp plainNode = this->getAttributeSNode(name, _Unbound<SNode_O>());
      if (!plainNode.unboundp()) {
        plainObject.archive(plainNode);
        defined = true;
      }
    } else {
      _BLOCK_TRACE("Saving");
      if (defined) {
        SNode_sp plainNode = createBranchSNode(nodeName);
        this->addAttributeSNode(name, plainNode);
        plainObject.archive(plainNode);
      }
    }
  }

  template <class SimpleClass>
  void attributePODIfDefined(string const &name, string const &nodeName, bool defined, SimpleClass &plainObject) {
    _G();
    this->attributePODIfDefined(_lisp->internKeyword(name), _lisp->internKeyword(nodeName), defined, plainObject);
  }

  /*! Serialize an enumerated type
   */
  template <typename EnumType>
  void attributeEnum(Symbol_sp name, EnumType &val) {
    _OF();
    if (this->saving()) {
      core::T_sp valo = translate::to_object<EnumType>::convert(val);
      this->addAttribute(name, valo);
    } else {
      core::T_sp valo = this->getAttributeOrError(name);
      val = translate::from_object<EnumType>(valo)._v;
    }
  }

  template <typename EnumType>
  void attributeEnumIfNotDefault(core::Symbol_sp name, EnumType &val, EnumType defVal) {
    _OF();
    if (this->saving()) {
      if (val != defVal) {
        T_sp sobj = translate::to_object<EnumType>::convert(val);
        this->addAttribute(name, sobj);
      }
    } else {
      core::T_sp o = this->getAttribute(name, _Unbound<core::T_O>());
      if (!o.unboundp()) {
        val = translate::from_object<EnumType>(o)._v;
      } else {
        val = defVal;
      }
    }
  }

  template <typename pType>
  void archiveMapKeyStringValuePOD(Symbol_sp uid, map<string, pType> &v) {
    _G();
#if 0
    Symbol_sp kw_sip = KW("SIP"); // kind of child node
    Symbol_sp kw_str = KW("STR");
    Symbol_sp kw_int = KW("INT");
#endif
    ArchiveP listNode;
    if (this->saving()) {
      _BLOCK_TRACE("Saving");
      typename map<string, pType>::iterator oi;
      for (oi = v.begin(); oi != v.end(); oi++) {
        List_sp pair = Cons_O::create(str_create(oi->first), Integer_O::create(oi->second));
        this->pushVector(pair);
      }
    } else {
      _BLOCK_TRACE("Loading");
      v.clear();
      this->mapVector([&v](T_sp pair) {
			string key = str_get(oCar(pair));
			Integer_sp ival = gc::As<Integer_sp>(oCdr(pair));
			v[key] = clasp_to_int(ival);
      });
    }
  }

  inline void archiveObject(Symbol_sp name, T_sp object) {
    this->attribute(name, object);
  }

  inline void archiveObject(string const &name, T_sp object) {
    this->attribute(_lisp->internKeyword(name), object);
  }

  void vector(gctools::Vec0<T_sp> &vec) {
    if (this->loading()) {
      this->loadVector(vec);
    } else {
      this->saveVector(vec);
    }
  };

  // utility

  explicit SNode_O() : T_O(), _RefCount(0){};
  virtual ~SNode_O(){};
};

class LeafSNode_O : public SNode_O {
  LISP_BASE1(SNode_O);
  LISP_VIRTUAL_CLASS(core, CorePkg, LeafSNode_O, "LeafSNode");
GCPROTECTED:
  T_sp _Object;

public:
  static LeafSNode_sp create(T_sp kind);

private:
  void noAttributes(const char *fn) const { SIMPLE_ERROR(BF("lead-snode does not have attributes - cannot respond to %s") % fn); };

public: // ctor/dtor for classes with shared virtual base
  virtual bool leafSNodeP() { return true; };
  Symbol_sp getKind() const {
    this->noAttributes(__FUNCTION__);
    UNREACHABLE();
  };
  virtual void addAttribute(Symbol_sp name, T_sp val) { this->noAttributes(__FUNCTION__); };
  List_sp getAttributes() const {
    this->noAttributes(__FUNCTION__);
    UNREACHABLE();
  };
  virtual T_sp getAttribute(Symbol_sp name, T_sp defaultValue) const {
    this->noAttributes(__FUNCTION__);
    UNREACHABLE();
  };
  T_sp object() const { return this->_Object; };
  List_sp keys() const { return _Nil<T_O>(); };

  string __repr__() const;
  void setKind(Symbol_sp kind) { SIMPLE_ERROR(BF("leaf-snode does not have kind")); };
  void setAttributesUnsafe(List_sp plist) { SIMPLE_ERROR(BF("leaf-snode does not have attributes")); };
  explicit LeafSNode_O() : _Object(_Nil<T_O>()){};
  virtual ~LeafSNode_O(){};
};

class BranchSNode_O : public SNode_O {
  LISP_BASE1(SNode_O);
  LISP_VIRTUAL_CLASS(core, CorePkg, BranchSNode_O, "BranchSNode");
  friend class LoadArchive_O;
  friend class SNode_O;
  friend class SexpSaveArchive_O;
GCPRIVATE:
  Symbol_sp _Kind;
  /*! PLIST of keyword symbols to SNode_sp */
  List_sp _SNodePList;
  /*! Vector of SNode_sp(s) */
  gc::Nilable<Vector_sp> _VectorSNodes;

public: // Simple default ctor/dtor
  BranchSNode_O() : _Kind(_Nil<Symbol_O>()), _SNodePList(_Nil<T_O>()), _VectorSNodes(_Nil<T_O>()){};
  virtual ~BranchSNode_O(){};

public:
  static BranchSNode_sp create(Symbol_sp kind, List_sp plist, Vector_sp data);
  static BranchSNode_sp create();

public: // ctor/dtor for classes with shared virtual base
  virtual Symbol_sp getKind() const { return this->_Kind; };
  virtual List_sp getAttributes() const { return this->_SNodePList; };
  SNode_sp getAttributeSNodeOrError(Symbol_sp name) const;
  SNode_sp getAttributeSNode(Symbol_sp name, SNode_sp defValue) const;
  virtual T_sp getAttribute(Symbol_sp name, T_sp defaultValue) const;
  virtual void addAttribute(Symbol_sp name, T_sp val);
  virtual void setVectorSNodesUnsafe(Vector_sp vec) { this->_VectorSNodes = gc::As<Vector_sp>(vec); };
  virtual Vector_sp getVectorSNodes() const {
    ASSERT(!this->_VectorSNodes.unboundp());
    return this->_VectorSNodes;
  };
  virtual T_sp object() const;
  List_sp keys() const;

  virtual T_sp getUniqueId() const;
  virtual SNode_sp childWithUniqueId(Symbol_sp uid) const;

  virtual void loadVector(gctools::Vec0<T_sp> &vec);
  virtual void mapVector(std::function<void(T_sp)> const &fn);

public:
  virtual void setKind(Symbol_sp kind) { this->_Kind = kind; };
  virtual void setAttributesUnsafe(List_sp plist) { this->_SNodePList = plist; };
  virtual void saveVector(gctools::Vec0<T_sp> const &vec);
  virtual void pushVectorSNode(SNode_sp node);
  virtual void pushVector(T_sp obj);
  virtual void addAttributeSNode(Symbol_sp name, SNode_sp node);

public:
  virtual T_sp createObject(HashTable_sp snodeToObject);

  string __repr__() const;

  //    explicit SNode_O(core::Class_sp const& mc) : _O(mc), Base(mc) {};
  //    virtual ~SNode_O() {};
};

/*! Virtual class
 */
SMART(Archive);
class Archive_O : public T_O {
  LISP_BASE1(T_O);
  LISP_CLASS(core, CorePkg, Archive_O, "Archive");
GCPROTECTED:
  int _Version;
  gc::Nilable<BranchSNode_sp> _TopNode;
  int _NextUniqueId;

public:
  static Archive_sp currentArchive();
  static LoadArchive_sp currentLoadArchive();
  static SaveArchive_sp currentSaveArchive();

public:
  uint nextUniqueId() { return (this->_NextUniqueId++); };

  virtual bool isSaveArchive() { return false; };
  bool loading() { return !this->isSaveArchive(); };

  virtual void addNodeToFinalize(SNode_sp node){};

  //	T_sp getTopNode() const { return this->_TopBranchSNode;};

  string __repr__() const;

  explicit Archive_O();
  virtual ~Archive_O(){};
};

SMART(LoadArchive);
class LoadArchive_O : public Archive_O {
  LISP_BASE1(Archive_O);
  LISP_CLASS(core, CorePkg, LoadArchive_O, "LoadArchive");

public:
  void initialize();

public:
  void addNodeToFinalize(SNode_sp node);
GCPRIVATE:
  HashTable_sp _ObjectForSNode;
  HashTable_sp _NodesToFinalize; // UnorderedSet<SNode_O> _NodesToFinalize;
public:
  T_sp loadObjectDirectly(SNode_sp node);
  void needsFinalization(SNode_sp node);

protected:
  virtual void createContents();
  virtual void finalizeObjects();

public:
  virtual void parseFromObject(T_sp object) { SUBIMP(); };
  virtual void parseFromStream(T_sp streamDesignator) { SUBIMP(); };

  virtual bool contains(Symbol_sp sym);
  virtual T_sp get(Symbol_sp sym);
  virtual List_sp getContents();

  List_sp keys() const;

  LoadArchive_O() : _ObjectForSNode(_Nil<HashTable_O>()), _NodesToFinalize(_Nil<HashTable_O>()){};
};
};

template <>
struct gctools::GCInfo<core::LoadArchive_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
SMART(SaveArchive);
class SaveArchive_O : public Archive_O {
  LISP_BASE1(Archive_O);
  LISP_CLASS(core, CorePkg, SaveArchive_O, "SaveArchive");

public:
  void initialize();
GCPRIVATE:
  HashTable_sp _SNodeForObject;

public:
  /*! Return the SNode for the object */
  SNode_sp getOrCreateSNodeForObjectIncRefCount(T_sp val);

public:
  bool isSaveArchive() { return true; };
  virtual void put(Symbol_sp name, T_sp obj);
  SaveArchive_O() : _SNodeForObject(_Nil<HashTable_O>()){};
  virtual ~SaveArchive_O(){};
};
};
template <>
struct gctools::GCInfo<core::SaveArchive_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

#endif
