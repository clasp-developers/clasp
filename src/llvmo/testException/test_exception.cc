#include <stdio.h>
#include <stdlib.h>


    struct err
    {
	int msg;
    };



    class XX
    {
    public:
	XX() {};
	virtual ~XX() {printf("Exiting\n");};
    };

    class YY
    {
    public:
	YY() {};
	virtual ~YY() {printf("Exiting\n");};
    };


extern "C"
{

    void z()
    { 
	printf(" in z\n");
	int xx = 1;
	throw xx;
    };

    void a()
    {
	XX xx;
	printf("in a\n");
	z();
    }

    void b()
    {
	printf("In b\n");
    }


    void c()
    {
	printf("In c\n");
    }



    void proto_unwind_protect()
    {
	XX xx;
	try
	{
	    a();
	} catch (...)
	{
	    c();
	}
   
    }
    
    void proto_cleanup()
    {
	XX xx;
	a();
    }
};

int main(int argc, char* argv[])
{
    printf("Do nothing\n");
}
