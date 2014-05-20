#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <functional>


class MyException {
    int i;
};




int main(int argc, char* argv[])
{
    try {
        throw MyException(1);
    } catch (MyException& e)
    {
        printf("Caught MyException\n");
    }
}
