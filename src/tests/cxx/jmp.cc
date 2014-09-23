/*
    File: jmp.cc
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
#include <setjmp.h>
#include <string>


using namespace std;

jmp_buf env;

struct Cleanup {
    string s;
    Cleanup(string x) : s(x) {
        printf("Initialized Cleanup(%s)\n", x.c_str());
    };
    ~Cleanup() {
        printf("In Cleanup for %s\n", this->s.c_str());
    }
};




void foo()
{
    printf("Entered foo\n");
    Cleanup foox("foo");
    longjmp(env,1);
}

int main(int argc, char* argv[])
{
    int val = setjmp(env);
    if (val) {
        printf("longjmp returned %d\n", val);
    } else {
        Cleanup mainx("main");
        foo();
    }
}
