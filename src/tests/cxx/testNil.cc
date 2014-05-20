
#include	<stdio.h>
#include	<string>
#include	<boost/smart_ptr.hpp>
#include	<boost/weak_ptr.hpp>
#include	<boost/enable_shared_from_this.hpp>
#include	<vector>

using namespace std;



class Object;
typedef boost::shared_ptr<Object>	RPObject;




class Object {
private:
public:
   static RPObject nil();
public:
   virtual bool isNil() const { return this==this->nil().get(); };
public:
    static RPObject create() 
	{
	    RPObject res = RPObject(new Object());
	    return res;
	}
public:
    static std::string Name() { return "Object";};
    virtual string __str__() { return "-object-";};
    Object() {};
    virtual ~Object() {};
};


RPObject	Nil;

class Word;
typedef boost::shared_ptr<Word>	RPWord;
class	Word : public Object {
public:
   static RPWord nil();
   virtual bool isNil() const { return this==this->nil().get(); };
private:
	string	_Word;
public:
    static RPWord create(const string& str) 
	{
	    RPWord res = RPWord(new Word());
	    res->set(str);
	    return res;
	}
public:
	void set(const string& w) { this->_Word = w; };
	string get() { return this->_Word;};
    virtual string __str__() { return this->get(); };
	Word() {};
	virtual ~Word() {};
};



class Cons;
typedef	boost::shared_ptr<Cons>	RPCons;

class Cons : public Object {
public:
   static RPCons nil();
   virtual bool isNil() const { return this==this->nil().get(); };
private:
	RPObject	_Car;
	RPObject	_Cdr;
public:
   static RPCons create(RPObject car, RPObject cdr)
	{
	    RPCons cc = RPCons(new Cons());;
	    cc->setCar(car);
	    cc->setCdr(cdr);
	    return cc;
	};
public:

    void	setCar(RPObject o) { this->_Car = o; };
    void	setCdr(RPObject o) { this->_Cdr = o; };

    RPObject	car() { return this->_Car; };
    RPCons	cdr() { return boost::shared_polymorphic_downcast<Cons>(this->_Cdr); };
  
   
    void print()
    {
	printf("-----> %s\n", this->car()->__str__().c_str() );
	for ( RPCons p=this->cdr(); p!=Nil; p = p->cdr()  )
	{
	    printf( "-----> %s\n", p->car()->__str__().c_str() );
	}
	printf("\n");
    }

    Cons() {};
    virtual ~Cons() {};
};

struct	null_deleter {
    void operator()(void const* ) const {
    }
};

RPObject Object::nil()
{
static RPObject _nil = RPObject(new Object(),null_deleter());
    return _nil;
}

RPWord Word::nil()
{
static RPWord _nil = RPWord(new Word(),null_deleter());
    return _nil;
}


RPCons Cons::nil()
{
static RPCons _nil = RPCons(new Cons(),null_deleter());
    return _nil;
}


#if 1
    template <class T>
    bool operator==(boost::shared_ptr<Object> const& a, boost::shared_ptr<T> const& b )
    {
        printf(" * * * reached operator==\n");
	if ( a->isNil() && b->isNil() ) return true;
	if ( a->isNil() ) return false;
	if ( b->isNil() ) return false;
	return (a.get()==b.get());
    };


    template <class T>
    bool operator==(boost::shared_ptr<Cons> const& a, boost::shared_ptr<T> const& b )
    {
        printf(" * * * reached operator==\n");
	if ( a->isNil() && b->isNil() ) return true;
	if ( a->isNil() ) return false;
	if ( b->isNil() ) return false;
	return (a.get()==b.get());
    };
#endif

void	trial(const char* message, bool comp )
{
   printf("%30s --> %d\n", message, comp );
}

int main()
{
    printf("Try static nil approach\n");

#if 0
    printf("Make co and wo two real objects and compare them");
    RPCons co = Cons::create();
    RPWord wo = Word::create();
    trial("co==wo", (co == wo) );
#endif

    RPCons co = Cons::nil();
    RPWord wo = Word::nil();
    printf("I want a way to make every one of the following tests TRUE\n");
    printf("----- Set co = Cons::nil\n");
    trial("co==Cons::nil()", (co == Cons::nil()) );
    RPObject o = Object::nil();
    printf("----- o set to Object::nil()\n");
//    trial("o.get()", (o.get() == Object::nil() ));
    trial("o == Object::nil()", (o == Object::nil()) );
    trial("o == Cons::nil()", (o == Cons::nil()) );
    trial("co == Object::nil()", (co == Object::nil()) );
    trial("o == Word::nil()", (o == Word::nil()) );
    trial("o==co", (o==co) );
    trial("o==o", (o==o) );
    trial("o->isNil()", o->isNil() );
    trial("co->isNil()", co->isNil() );
    o = co;
    printf("----- Set o = co\n");
    trial("o == Object::nil()", (o == Object::nil()) );
    trial("o == Cons::nil()", (o == Cons::nil()) );
    trial("co == Object::nil()", (co == Object::nil()) );
    trial("o==co", (o==co) );
    trial("o->isNil()", o->isNil() );
    trial("co->isNil()", co->isNil() );

}

