#include <stdio.h>

class TestClass
{
public:
    virtual ~TestClass() {};
};

class MyException
{
public:
    virtual ~MyException() {};
};


int main(int argc, char* argv[])
{
    TestClass a;
    try
    {
	{
	    TestClass b;
	}
	throw MyException();
    } catch (MyException& exp)
    {
	printf("Caught MyException\n");
    }
    printf("Done\n");
}
