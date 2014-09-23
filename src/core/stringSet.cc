/*
    File: stringSet.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See file 'clasp/Copyright' for full details.
 
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
#define	DEBUG_LEVEL_FULL


//
// (C) 2004 Christian E. Schafmeister
//


#include "common.h"
#include "object.h"
#include "lisp.h"
#include "stringSet.h"
#include "symbolTable.h"
#include "multipleValues.h"
#include "stringList.h"
#include "str.h"
#include "serialize.h"
#include "cons.h"
#include "numbers.h"
#include "wrappers.h"



namespace core {


/*
  __BEGIN_DOC(classes.StringSet.!class.StringSet)
  \requiredKeyed{entries:}{Cons::listOfStrings}

  Create a StringSet containing the strings in \sa{listOfStrings}.
  __END_DOC
*/


    StringSet_sp StringSet_O::insertConsStrings(Cons_sp vals)
    {
	for ( Cons_sp p=vals; p.notnilp(); p=cCdr(p))
	{
	    Str_sp t = oCar(p).as<Str_O>();
	    this->insert(t->get());
	}
	return this->sharedThis<StringSet_O>();
    }

    void	StringSet_O::insertVectorStrings(VectorStrings s )
    {
	VectorStrings::iterator	i;
	for ( i=s.begin(); i!= s.end(); i++ ) {
	    this->insert(*i);
	}
    }



    void	StringSet_O::insertStringSet(StringSet_sp s)
    {
	set<string>::iterator	si;
	for ( si=s->strs.begin(); si!= s->strs.end(); si++ ) {
	    this->insert(*si);
	}
    }

    void	StringSet_O::insertStringList(StringList_sp s)
    {
	StringList_O::iterator	si;
	for ( si=s->begin(); si!= s->end(); si++ ) {
	    this->insert(*si);
	}
    }


    StringSet_sp	StringSet_O::copy()
    {_G();
	StringSet_O::iterator	it;
	GC_COPY(StringSet_O,snew,*this);
	snew->clear();
	for ( it=this->begin(); it!=this->end(); it++ ) snew->insert(*it);
	return snew;
    }

    string	StringSet_O::__repr__() const
    {
	stringstream 		ss;
	set<string>::iterator	si;
	string			nm;
	ss.str("");
	for ( si = this->strs.begin(); si!=this->strs.end(); si++ ) {
	    if ( si!=this->strs.begin() ) {
		ss << " ";
	    }
	    ss << *si;
	}
	return ss.str();
    }
    string	StringSet_O::asString()
    {
	stringstream 		ss;
	set<string>::iterator	si;
	string			nm;
	ss.str("");
	for ( si = this->strs.begin(); si!=this->strs.end(); si++ ) {
	    if ( si!=this->strs.begin() ) {
		ss << " ";
	    }
	    ss << *si;
	}
	return ss.str();
    }


    void	StringSet_O::archiveBase(ArchiveP node)
    {
	if ( node->loading() ) {
	    Vector_sp vec = node->getVectorSNodes();
	    this->strs.clear();
	    for ( int i(0),iEnd(vec->length()); i<iEnd; ++i ) {
		LeafSNode_sp ln = vec->elt(i).as<LeafSNode_O>();
		this->insert(ln->object().as<Str_O>()->get());
	    }
	} else {
	    VectorObjects_sp vec = VectorObjects_O::create(_Nil<T_O>(),this->strs.size(),core::_sym_LeafSNode_O);
	    int i(0);
	    set<string>::iterator	si;
	    for ( si = this->strs.begin(); si!=this->strs.end(); si++ ) {
		vec->setf_elt(i,LeafSNode_O::create(Str_O::create(*si)));
		++i;
	    }
	    node->setVectorSNodesUnsafe(vec);
	}
    }







    bool	StringSet_O::contains(const string& s)
    {_G();
	bool	ye;
	ye = this->strs.count(s);
	return ye;
    }


    bool	StringSet_O::containsSubset(StringSet_sp sub )
    {
	StringSet_O::iterator	si;
	for ( si=sub->begin(); si!=sub->end(); si++ ) {
	    if ( !this->contains(*si) ) {
		return false;
	    }
	}
	return true;
    }



    void	StringSet_O::clear()
    {_OF();
	LOG(BF("StringSet::clear size=%d") % (this->strs.size() ) );
	if ( this->strs.size() == 0 ) return;
	this->strs.clear();
    }

/*! Check if the StringSet contains the same strings as this
 */
    bool	StringSet_O::equal(T_sp obj) const
    {
	if ( this->eq(obj) ) return true;
	if ( obj.isA<StringSet_O>() )
	{
	    set<string>::iterator	si;
	    StringSet_sp ss = obj.as<StringSet_O>();
	    if ( this->strs.size() != ss->strs.size() ) return false;
	    for (si=this->strs.begin();si!=this->strs.end();si++)
	    {
		if ( !ss->contains(*si) ) 
		{
		    return false;
		}
	    }
	    return true;
	}
	return false;
    }


    void	StringSet_O::remove(const string& s)
    {_OF();
#ifdef 	DEBUG_ON
	if ( this->strs.count(s)!=1 ) {
	    SIMPLE_ERROR(BF("The string: %s was not found in StringSet") % s );
	}
#endif
	this->strs.erase(s);
    }




    void StringSet_O::initialize()
    {
	this->Base::initialize();
	this->rest = false;
    }

    StringSet_O::StringSet_O(const StringSet_O& ss) : T_O(ss)
    {
	this->rest = ss.rest;
	this->strs = ss.strs;
    }



    void	StringSet_O::setFromString( const string& s )
    {_G();
	VectorStrings	words;
	tokenize(s,words,"\t\n ");
	this->clear();
	this->insertVectorStrings(words);
    }




    StringSet_sp	StringSet_O::setUnion(StringSet_sp b)
    {_G();
	StringSet_sp		nset;
	set<string>::iterator	si;
	nset = StringSet_O::create();
	for (si=this->strs.begin();si!=this->strs.end();si++){
	    nset->insert(*si);
	}
	for (si=b->strs.begin();si!=b->strs.end();si++){
	    nset->insert(*si);
	}
	return nset;
    }


    StringSet_sp	StringSet_O::intersection(StringSet_sp b)
    {_G();
	StringSet_sp		nset;
	set<string>::iterator	si;
	nset = StringSet_O::create();
	for (si=this->strs.begin();si!=this->strs.end();si++){
	    LOG(BF("Looking for(%s)") % (*si).c_str() );
	    if ( b->contains(*si) ) {
		LOG(BF("Found it!!!") );
		nset->insert(*si);
	    } else {
		LOG(BF("Not found") );
	    }
	}
	return nset;
    }


    StringSet_sp	StringSet_O::relativeComplement(StringSet_sp b)
    {
	StringSet_sp		nset;
	set<string>::iterator	si;
	nset = StringSet_O::create();
	for (si=this->strs.begin();si!=this->strs.end();si++){
	    if ( !b->contains(*si) ) {
		nset->insert(*si);
	    }
	}
	return nset;
    }


/*! Return a new set that takes every element of (this) in combination
  with every element in b separated by a comma
*/
    StringSet_sp	StringSet_O::cartesianProductInsert(string ins, StringSet_sp b)
    {_G();
	StringSet_sp		nset;
	set<string>::iterator	si,bi;
	stringstream		sstr;
	nset = StringSet_O::create();
	for (si=this->strs.begin();si!=this->strs.end();si++){
	    for (bi=b->strs.begin();bi!=b->strs.end();bi++){
		sstr.str("");
		sstr << (*si) << ins << (*bi);
		nset->insert(sstr.str());
	    }
	}
	return nset;
    }

    Cons_sp	StringSet_O::asCons() const
    {_G();
	Cons_sp cur = _Nil<Cons_O>();
	set<string>::iterator	si;
	for (si=this->strs.begin();si!=this->strs.end();si++){
	    Str_sp s = Str_O::create(*si);
	    cur = Cons_O::create(s,cur);
	}
	return cur;
    }

    Vector_sp StringSet_O::asVector() const
    {_G();
	Vector_sp vec = af_make_vector(cl::_sym_Str_O,
				       this->strs.size() /* dim */,
				       true /* adjustable */,
				       brcl_make_fixnum(0) /* fill pointer */ );
	int i=0;
	set<string>::iterator	si;
	for ( si=this->strs.begin();si!=this->strs.end();si++) {
	    Str_sp s = Str_O::create(*si);
	    vec->setf_elt(i,s);
	    ++i;
	}
	return vec;
    }



    void StringSet_O::exposeCando(Lisp_sp lisp)
    {
	class_<StringSet_O>()
	    .def("size", &StringSet_O::size)
	    .def("insertConsStrings", &StringSet_O::insertConsStrings)
	    .def("insertStringSet", &StringSet_O::insertStringSet)
	    .def("insert",&StringSet_O::insert)
	    .def("contains",&StringSet_O::contains)
	    .def("containsSubset",&StringSet_O::containsSubset)
//	    .def("remove",&StringSet_O::remove)
	    .def("clear",&StringSet_O::clear)
	    .def("asString",&StringSet_O::asString)
	    .def("setFromString",&StringSet_O::setFromString)
	    .def("StringSetUnion",&StringSet_O::setUnion)
	    .def("StringSetIntersection",&StringSet_O::intersection)
	    .def("relativeComplement",&StringSet_O::relativeComplement)
	    .def("removeAll",&StringSet_O::removeAll)
	    .def("cartesianProduct",&StringSet_O::cartesianProduct)
	    .def("cartesianProductInsert",&StringSet_O::cartesianProductInsert)
	    .def("asCons",&StringSet_O::asCons)
	    ;
    }

    void StringSet_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,StringSet,INIT_ARGS_StringSet_O,DOCS_StringSet_O,_lisp)
	    .def("insertStringSet", &StringSet_O::insertStringSet)
	    .def("size", &StringSet_O::size)
	    .def("insert",&StringSet_O::insert)
	    .def("equals",&StringSet_O::equal)
	    .def("contains",&StringSet_O::contains)
	    .def("containsSubset",&StringSet_O::containsSubset)
//	    .def("remove",&StringSet_O::remove)
	    .def("clear",&StringSet_O::clear)
	    .def("asString",&StringSet_O::asString)
	    .def("setFromString",&StringSet_O::setFromString)
	    .def("StringSetUnion",&StringSet_O::setUnion)
	    .def("StringSetIntersection",&StringSet_O::intersection)
	    .def("relativeComplement",&StringSet_O::relativeComplement)
	    .def("cartesianProduct",&StringSet_O::cartesianProduct)
	    .def("cartesianProductInsert",&StringSet_O::cartesianProductInsert)
	    .add_property("iterate",
			  boost::python::range(&StringSet_O::begin,
					       &StringSet_O::end))
	    ;
#endif
    }





#if 0
#ifdef	USEBOOSTPYTHON
    void	StringSet_O::python_setFromList(boost::python::list res) {
//string		i;
//boost::python::str		lval;
//VectorStrings	vs;
//VectorStrings::iterator vi;
	int			ll;
	string			x;

	ll = boost::python::extract<int>(res.attr("__len__")());
	for ( int i=0; i<ll; i++ ) {
	    x = boost::python::extract<string>(res[i]);
//	printf( "Extracted element: %d = %s\n", i, x.c_str() );
	    this->strs.insert(x);
	}
    }



    boost::python::list StringSet_O::python_asList() {
	string		i;
	int		id;
	boost::python::list	res;
	boost::python::str		lval;
	VectorStrings	vs;
	set<string>::iterator vi;

	for ( vi=this->strs.begin(); vi!=this->strs.end(); vi++ ) {
	    i = *vi;
	    LOG(BF("Appending %s to list") % (i.c_str() ) );
	    lval = boost::python::str(i);
	    res.append(lval);
	}
	return res;
    }
#endif
#endif




    StringSet_O::~StringSet_O()
    {
// nothing
    }
    EXPOSE_CLASS(core,StringSet_O);

};
