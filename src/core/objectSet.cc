
#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "objectSet.h"
#include "environment.h"
#include "multipleValues.h"
#include "wrappers.h"



namespace core {


#if 0
    T_sp ObjectSet_O::__init__(Function_sp exec, Cons_sp args, Environment_sp bargs, Lisp_sp lisp)
    {_G();
	T_sp ov = translate::from_object<T_sp>::convert(bargs->lookup(Pkg(),"values")->object());
	if ( ov.notnilp() )
	{
	    if ( ov->consP() )
	    {
		Cons_sp co = ov.as_or_nil<Cons_O>();
		this->addObjectsInCons(co);
	    } else
	    {
		SIMPLE_ERROR(BF("Illegal argument for ObjectSet init"));
	    }
	}
	return _Nil<T_O>();
    }
#endif


    void ObjectSet_O::initialize()
    {
        this->Base::initialize();
        this->_Set = HashTableEq_O::create_default();
    }


void	ObjectSet_O::addObjects(ObjectSet_sp other)
{_G();
    other->map( [this] (T_sp obj) {this->insert(obj); });
}



    Cons_sp ObjectSet_O::asCons()
    {
	Cons_sp res = _Nil<Cons_O>();
        this->map( [&res] (T_sp o) {
	    res = Cons_O::create(o,res);
            } );
	return res;
    }


    ObjectSet_sp ObjectSet_O::setUnion(ObjectSet_sp other)
    {
	ObjectSet_sp os = ObjectSet_O::create();
	os->addObjects(this->sharedThis<ObjectSet_O>());
	os->addObjects(other);
	return os;
    }


ObjectSet_sp	ObjectSet_O::intersection(ObjectSet_sp b)
{_G();
    ObjectSet_sp		nset;
    nset = ObjectSet_O::create();
    
    this->map( [&b,&nset] (T_sp o) {
            if ( b->contains(o) ) {
                LOG(BF("Found it!!!") );
                nset->insert(o);
            } else {
                LOG(BF("Not found") );
            } 
        } );
    return nset;
}



ObjectSet_sp	ObjectSet_O::relativeComplement(ObjectSet_sp b)
{
    ObjectSet_sp		nset;
    nset = ObjectSet_O::create();
    this->map( [&b,&nset] (T_sp o) {
	if ( !b->contains(o) ) {
	    nset->insert(o);
	}
        } );
    return nset;
}








    string ObjectSet_O::asString() const
    {
	stringstream ss;
        this->map( [&ss] (T_sp si) {
                ss << _rep_(si) << " ";
            } );
	return ss.str();
    }


/*! Return a new set that takes every element of (this) in combination
	with every element in b separated by a comma
*/
ObjectSet_sp	ObjectSet_O::cartesianProduct(ObjectSet_sp b)
{_G();
ObjectSet_sp		nset;
stringstream		sstr;
    nset = ObjectSet_O::create();
    this->map([&b,&nset,this] (T_sp si) {
            this->map( [&si,&nset] (T_sp bi) {
                    Cons_sp op = _lisp->create<Cons_O>(si,bi);
                    nset->insert(op);
                });
	} );
    return nset;
}



/*! Return a new set that takes every element of (this) in combination
	with every element in b separated by a comma
*/
    ObjectSet_sp	ObjectSet_O::cartesianProductWrapped(ObjectSet_sp b,const ObjectSetCartesianProductWrapper& wrapper)
{_G();
ObjectSet_sp		nset;
stringstream		sstr;
    nset = ObjectSet_O::create();

    this->map([&b,&nset,this,&wrapper] (T_sp si) {
            this->map( [&si,&nset,&wrapper] (T_sp bi) {
                    T_sp op = wrapper(si,bi);
                    nset->insert(op);
                });
	} );
    return nset;
}





    void ObjectSet_O::map(std::function<void(T_sp)> const& fn)
    {
        this->_Set->mapHash([&fn] (T_sp key, T_sp val) {
                fn(key);
            });
    }

    void ObjectSet_O::map(std::function<void(T_sp)> const& fn) const
    {
        this->_Set->mapHash([&fn] (T_sp key, T_sp val) {
                fn(key);
            });
    }



void ObjectSet_O::addObjectsInCons(Cons_sp c)
{
    while ( c.notnilp() )
    {
	this->insert(oCar(c));
	c = cCdr(c);
    }
}

#if defined(XML_ARCHIVE)
void	ObjectSet_O::archive(ArchiveP node)
{_OF();
    stringstream suid;
    if ( node->saving() )
    { _BLOCK_TRACE("Saving");
	if ( this->_Set.size() != 0 )
	{
	    int i = 0;
	    stringstream suid;
	    T_sp	obj;
	    set<mem::smart_ptr<T_O> >::iterator oi;
	    for ( oi=this->_Set.begin(); oi!=this->_Set.end(); i++,oi++ )
	    {
	    	obj = (*oi);
		suid.str("");
		suid << i;
		node->archiveObject(suid.str(),obj);
	    }
	}
    } else
    { _BLOCK_TRACE("Loading");
	VectorNodes::iterator	ci;
	T_sp object;
	this->_Set.clear();
	for ( ci=node->begin_Children(); ci!=node->end_Children(); ci++ )
	{
	    object = node->getArchive()->loadObjectDirectly((*ci));
	    ASSERTNOTNULL(object);
	    this->_Set.insert(object);
	}
    }
}
#endif // defined(XML_ARCHIVE)


    void ObjectSet_O::exposeCando(Lisp_sp lisp)
{
    class_<ObjectSet_O>()
	.def("insert",&core::ObjectSet_O::insert)
	.def("add",&core::ObjectSet_O::insert)
	.def("addObjectsInCons",&core::ObjectSet_O::addObjectsInCons)
	.def("addObjects",&core::ObjectSet_O::addObjects)
	.def("size",&core::ObjectSet_O::size)
	.def("remove",&core::ObjectSet_O::remove)
	.def("asCons",&ObjectSet_O::asCons)
	.def("relativeComplement",&ObjectSet_O::relativeComplement)
	.def("union",&ObjectSet_O::setUnion)
	.def("intersection",&ObjectSet_O::intersection)
	.def("contains",&ObjectSet_O::contains)
	;
}

    void ObjectSet_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON //[
	PYTHON_CLASS(CorePkg,ObjectSet,"","",_lisp)
	.add_property("iterate",
		      boost::python::range(&core::ObjectSet_O::begin, &core::ObjectSet_O::end))
	.def("insert",&core::ObjectSet_O::insert)
	.def("add",&core::ObjectSet_O::insert)
	.def("add",&core::ObjectSet_O::insert)
	.def("setUnion",&core::ObjectSet_O::setUnion)
	.def("size",&core::ObjectSet_O::size)
	.def("remove",&core::ObjectSet_O::remove)
	;
#endif
}
    EXPOSE_CLASS(core, ObjectSet_O );
};
