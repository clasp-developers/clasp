
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



    bool operator==(boost::shared_ptr<Object> const& a, boost::shared_ptr<Object> const& b )
    {
        printf(" * * * reached operator==\n");
	if ( a->isNil() && b->isNil() ) return true;
	if ( a->isNil() ) return false;
	if ( b->isNil() ) return false;
	return (a.get()==b.get());
    };

    bool operator==(boost::shared_ptr<Object> const& a, boost::shared_ptr<Cons> const& b )
    {
        printf(" * * * reached operator==\n");
	if ( a->isNil() && b->isNil() ) return true;
	if ( a->isNil() ) return false;
	if ( b->isNil() ) return false;
	return (a.get()==b.get());
    };

    bool operator==(boost::shared_ptr<Cons> const& a, boost::shared_ptr<Object> const& b )
    {
        printf(" * * * reached operator==\n");
	if ( a->isNil() && b->isNil() ) return true;
	if ( a->isNil() ) return false;
	if ( b->isNil() ) return false;
	return (a.get()==b.get());
    };


void	trial(const char* message, bool comp )
{
   printf("%30s --> %d\n", message, comp );
}

int main()
{
    printf("Try static nil approach\n");

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

#if 0
    printf("Try undefined pointer nil approach\n");
    Nil.reset();
    printf("Nil.use_count() = %d\n", Nil.use_count() );
    RPObject o = Nil;
    RPObject o2 = Object::create();


    printf("o is Nil\n");
    printf("o2 is obj\n");
    printf("o==o2   ->%d\n", (o==o2) );
    printf("o==Nil  ->%d\n", (o==Nil) );
    printf("o.use_count=%d\n", o.use_count() );
    printf("o2.use_count=%d\n", o2.use_count() );

    RPCons co = RPCons(new Cons());
    printf("co is obj\n");
    printf("co==Nil  ->%d\n", (co==Nil) );

    RPCons c2 = RPCons(new Cons());
    printf("before c2 reset\n");
    printf("c2==Nil  ->%d\n", (c2==Nil) );
    printf("c2.use_count=%d\n", c2.use_count() );
    c2.reset();
    printf("after c2 reset\n");
    printf("c2==Nil  ->%d\n", (c2==Nil) );
    printf("c2.use_count=%d\n", c2.use_count() );

    co = Cons::create(Word::create("test"),Nil);
    co = Cons::create(Word::create("a "), co );
    co = Cons::create(Word::create("is "), co );
    co = Cons::create(Word::create("this "), co );
    co->print();
#endif
}

