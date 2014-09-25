/*
    File: stringList.cc
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
       
#define	DEBUG_LEVEL_NONE

//
// (C) 2004 Christian E. Schafmeister
//

#include "common.h"
#include "lisp.h"
#include "object.h"
#include "stringList.h"
#include "stringSet.h"
#include "str.h"
#include "wrappers.h"




namespace core 
{


    EXPOSE_CLASS(core,StringList_O);

    void StringList_O::exposeCando(Lisp_sp lisp)
    {
	class_<StringList_O>()
	    .def("string-list-append",&StringList_O::append)
	    .def("asString",&StringList_O::asString)
	    ;
    }

    void StringList_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,StringList,"","",_lisp)
	    .def("string_list_append",&StringList_O::append)
	    .def("asString",&StringList_O::asString)
	    ;
#endif
    }


    void	StringList_O::setFromString(const string& ss)
    {_G();
	VectorStrings	parts;
	tokenize(ss,parts," \n\t");
	this->setFromVectorStrings(parts);
    }


    string	StringList_O::asString()
    {_G();
	stringstream 		ss;
	StringList_O::iterator	si;
	string			nm;
	ss.str("");
	for ( si = this->_Contents.begin(); si!=this->_Contents.end(); si++ ) {
	    if ( si!=this->_Contents.begin() ) {
		ss << " ";
	    }
	    ss << *si;
	}
	return ss.str();
    }


#if defined(XML_ARCHIVE)
    void	StringList_O::archiveBase(ArchiveP node)
    {
	if ( node->loading() ) {
	    VectorStrings		v_Contents;
	    node->getDataAsVectorOfStrings(v_Contents);
	    this->_Contents.clear();
	    this->setFromVectorStrings(v_Contents);
	} else {
	    node->setCharacters(this->asString());
	}
    }
#endif // defined(XML_ARCHIVE)



    void	StringList_O::clear()
    {
	LOG(BF("StringList::clear size=%d") % (this->_Contents.size() ) );
	if ( this->_Contents.size() == 0 ) return;
	this->_Contents.clear();
    }


    void	StringList_O::prepend(const string& str )
    {_G();
	this->_Contents.insert(this->_Contents.begin(),str);
    }


    void	StringList_O::append(const string& str )
    {_G();
	this->_Contents.push_back(str);
    }


    void	StringList_O::appendStringList(StringList_sp s)
    {_G();
	StringList_O::iterator	ic;
	for ( ic=s->begin(); ic!=s->end(); ic++ )
	{
	    this->_Contents.push_back(*ic);
	}
    }

    void	StringList_O::appendConsOfStrings(Cons_sp s)
    {_G();
	for ( Cons_sp cur=s; cur.notnilp(); cur=cCdr(cur) )
	{
	    this->_Contents.push_back(oCar(cur).as<Str_O>()->get());
	}
    }




    void	StringList_O::setFromVectorStrings(VectorStrings vs)
    {_G();
	VectorStrings::iterator	it;
	this->clear();
	for ( it=vs.begin(); it!=vs.end(); it++ )
	{
	    this->append(*it);
	}
    }











    bool	StringList_O::contains(const string& nm)
    {_G();
	StringList_O::iterator	it;
	for ( it=this->begin(); it!=this->end(); it++ )
	{
	    if ( (*it)==nm ) return true;
	}
	return false;
    }


    int	StringList_O::indexOf(const string& nm)
    {_G();
	StringList_O::iterator	it;
	int			idx;
	LOG(BF("Looking for string(%s)") % nm.c_str()  );
	for ( it=this->begin(),idx=0; it!=this->end(); it++,idx++ )
	{
	    LOG(BF("   Looking at StringList entry(%s)") % (*it).c_str()  );
	    if ( (*it)==nm )
	    {
		LOG(BF("Found Match!!!") );
		return idx;
	    }
	}
	SIMPLE_ERROR(BF("StringList does not contain: "+nm));
    }



};
