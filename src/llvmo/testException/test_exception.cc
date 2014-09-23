/*
    File: test_exception.cc
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
