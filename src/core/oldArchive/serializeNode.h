/*
    File: serializeNode.h
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
#ifndef	_core_serializeNode_H //[
#define _core_serializeNode_H



#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"



namespace core
{

    class SNode_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_VIRTUAL_CLASS(core,ClPkg,SNode_O,"SNode");
    private:
	T_sp 		_Kind;
	Cons_sp 	_PList;
	Vector_sp	_Data;
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(SNode_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit SNode_O(core::Class_sp const& mc) : _O(mc), Base(mc) {};
//    virtual ~SNode_O() {};

    };





/*! Virtual class
 */
    SMART(Archive);
    class Archive_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(CorePkg,Archive_O,"Archive");
    public:
	void initialize();
    public:
	static const string UniqueIdTag; //  = "_uid";
	static const string CoreBuiltInClassAttribute; //  = "_coreClass";
    protected:
	int		_Version;
	SNode	_TopNode;
	int		_NextUniqueId;
    public:
	uint nextUniqueId() { return (this->_NextUniqueId++);};

	virtual bool isSaveArchive() { return false; };
	bool loading() { return !this->isSaveArchive(); };

	virtual void addNodeToFinalize(SNode_sp node) {};

	SNode_sp	getTopNode() const { return this->_TopNode;};

	explicit Archive_O();
	virtual ~Archive_O();
    };






    SMART(LoadArchive);
    class LoadArchive_O : public Archive_O
    {
	LISP_BASE1(Archive_O);
	LISP_CLASS(CorePkg,LoadArchive_O,"LoadArchive");
    public:
	void initialize();
    public:
	void addNodeToFinalize(SNode_sp node);

    private:
	Vector0<SNode_O>	_NodesToFinalize;

    protected:

	virtual void createContents();
	virtual void finalizeObjects();


    public:
	virtual void parseFromObject( Cons_sp object ) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual void parseFromStream( T_sp streamDesignator ) {_OF(); SUBCLASS_MUST_IMPLEMENT();};

	virtual bool contains(const string& uid);
	virtual T_sp get(const string& uid);
	virtual T_sp getContents();

	DEFAULT_CTOR_DTOR(LoadArchive_O);
    };





    SMART(SaveArchive);
    class SaveArchive_O : public Archive_O
    {
	LISP_BASE1(Archive_O);
	LISP_CLASS(CorePkg,SaveArchive_O,"SaveArchive");
    public:
	void	initialize();
    public:

    public:
	bool	isSaveArchive() { return true; };
	virtual void put(const string& uid, T_sp obj);


	virtual	void	write(Stream_sp outStream) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual	void	saveAs(const string& fileName) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual	void	saveToSteeam(ostream& sout ) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual	void	saveAsToPath(Path_sp path ) {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual	string	asString() {_OF(); SUBCLASS_MUST_IMPLEMENT();};
	virtual	string	asStringSubstituteRootNodeNameAndAttributes(const string& nodeName, const string& rawAttributes) {_OF(); SUBCLASS_MUST_IMPLEMENT();};


	DEFAULT_CTOR_DTOR(SaveArchive_O);
    };













};
TRANSLATE(core::SNode_O);
TRANSLATE(core::Archive_O);
TRANSLATE(core::LoadArchive_O);
TRANSLATE(core::SaveArchive_O);
