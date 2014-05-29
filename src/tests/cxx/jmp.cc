
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

