       
#define	DEBUG_LEVEL_NONE

//
// (C) 2004 Christian E. Schafmeister
//

#include "common.h"
#include "lisp.h"
#include "object.h"
#include "metaClass.h"
#include "str.h"
#include "symbolList.h"
#include "symbolSet.h"
#include "wrappers.h"

#define GCINFO_KIND_GCVECTOR_gctools__GCVector_moveable_class_mem__smart_ptr_class_core__Symbol_O__
#include GC_INTERFACE_HEADER
#undef GCINFO_KIND_GCVECTOR_gctools__GCVector_moveable_class_mem__smart_ptr_class_core__Symbol_O__



namespace core
{


    EXPOSE_CLASS(core,SymbolList_O);

    void SymbolList_O::exposeCando(Lisp_sp lisp)
    {
	class_<SymbolList_O>()
	    ;
    }

    void SymbolList_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,SymbolList,"","",_lisp)
	    ;
#endif
    }



    string	SymbolList_O::asString()
    {_G();
	stringstream 		ss;
	SymbolList_O::iterator	si;
	string			nm;
	ss.str("");
	for ( si = this->_Contents.begin(); si!=this->_Contents.end(); si++ )
	{
	    if ( si!=this->_Contents.begin() )
	    {
		ss << " ";
	    }
	    ss << (*si)->formattedName(true);
	}
	return ss.str();
    }


#if defined(XML_ARCHIVE)
    void	SymbolList_O::archiveBase(ArchiveP node)
    {
	if ( node->loading() )
	{
	    VectorStrings		v_Contents;
	    node->getDataAsVectorOfStrings(v_Contents);
	    this->_Contents.clear();
	    this->setFromVectorStrings(v_Contents);
	} else
	{
	    node->setCharacters(this->asString());
	}
    }
#endif // defined(XML_ARCHIVE)



    void	SymbolList_O::clear()
    {
	LOG(BF("SymbolList::clear size=%d") % (this->_Contents.size() ) );
	if ( this->_Contents.size() == 0 ) return;
	this->_Contents.clear();
    }


    void	SymbolList_O::prepend(Symbol_sp str)
    {_G();
	this->_Contents.insert(this->_Contents.begin(),str);
    }


    void	SymbolList_O::append(Symbol_sp str )
    {_G();
	this->_Contents.push_back(str);
    }


    void	SymbolList_O::appendSymbolList(SymbolList_sp s)
    {_G();
        s->map( [this] (Symbol_sp s) {
                this->_Contents.push_back(s);
            });
    }

    void	SymbolList_O::appendConsOfStrings(Cons_sp s)
    {_G();
        
	for ( Cons_sp cur=s; cur.notnilp(); cur=cCdr(cur) )
	{
	    Symbol_sp sym = _lisp->intern(oCar(cur).as<Str_O>()->get());
	    this->_Contents.push_back(sym);
	}
    }




    void	SymbolList_O::setFromVectorStrings(VectorStrings vs)
    {_G();
	VectorStrings::iterator	it;
	this->clear();
	for ( it=vs.begin(); it!=vs.end(); it++ )
	{
	    Symbol_sp sym = _lisp->intern(*it);
	    this->append(sym);
	}
    }

    bool	SymbolList_O::contains(Symbol_sp nm)
    {_G();
	for ( auto it=this->_Contents.begin(); it!=this->_Contents.end(); it++ )
	{
	    if ( (*it)==nm ) return true;
	}
	return false;
    }


    int	SymbolList_O::indexOf(Symbol_sp nm)
    {_G();
	int			idx;
	LOG(BF("Looking for string(%s)") % nm->__repr__()  );
	for ( auto it=this->_Contents.begin(); it!=this->_Contents.end(); it++ )
	{
	    LOG(BF("   Looking at SymbolList entry(%s)") % (*it)->__repr__()  );
	    if ( (*it)==nm )
	    {
		LOG(BF("Found Match!!!") );
                return it - this->_Contents.begin();
	    }
	}
	SIMPLE_ERROR(BF("SymbolList does not contain: %s")%nm);
    }


    void SymbolList_O::map(std::function<void(Symbol_sp)> const& fn)
    {
        for ( auto it=this->_Contents.begin(); it!=this->_Contents.end(); ++it ) {
            Symbol_sp key = *it;
            fn(key);
        }
    }


};
