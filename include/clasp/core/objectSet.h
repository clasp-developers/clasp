/*
    File: objectSet.h
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
       
       

#ifndef	ObjectSet_H //[
#define	ObjectSet_H

#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/hashTableEq.h>


namespace core {


class ObjectSetCartesianProductWrapper
{
public:
    virtual T_sp operator()(T_sp obj1, T_sp obj2) const = 0;
};


SMART(ObjectSet);




class ObjectSet_O : public T_O
{
    LISP_BASE1(T_O);
    LISP_CLASS(core,CorePkg,ObjectSet_O,"ObjectSet");
    DECLARE_INIT();
    void initialize();
public:
#if defined(XML_ARCHIVE)
	void	archive(ArchiveP node);
#endif // defined(XML_ARCHIVE)

GCPRIVATE:
    HashTableEq_sp _Set;

public:
    bool contains(T_sp obj) { return this->_Set->contains(obj); };
    void	insert( T_sp obj) { this->_Set->setf_gethash(obj,_lisp->_true());};
    void	remove( T_sp obj) { this->_Set->remhash(obj); };
    int	size() { return this->_Set->hashTableSize();};

	Cons_sp asCons();

	void	addObjectsInCons(Cons_sp objs);

	void addObjects(ObjectSet_sp b);



		// Set theory operations

		//! A setUnion B = (x:x E A || x E B)
	ObjectSet_sp	setUnion(ObjectSet_sp b);

		//! A intersection B = (x:x E A && x E B)
	ObjectSet_sp	intersection(ObjectSet_sp b);

		//! A-B = (x: x E A && not x E B )
	ObjectSet_sp	relativeComplement(ObjectSet_sp b);

	ObjectSet_sp	removeAll(ObjectSet_sp b) { return this->relativeComplement(b); };

	/*! AxB = ("x,y": x E A ; y E B )
	  Return x,y as ObjectPair
	*/
	ObjectSet_sp	cartesianProduct(ObjectSet_sp b);

	/*! AxB = ("x,y": x E A ; y E B )
	  Return x,y wrapped by a callback functor
	*/
	ObjectSet_sp	cartesianProductWrapped(ObjectSet_sp b,const ObjectSetCartesianProductWrapper& wrapper);

	string asString() const;

        void map(std::function<void(T_sp)> const& fn);
        void map(std::function<void(T_sp)> const& fn) const;


public:
	DEFAULT_CTOR_DTOR(ObjectSet_O);
};


};

TRANSLATE(core::ObjectSet_O);
#endif //]
