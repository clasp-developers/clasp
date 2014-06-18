#include <my_class.hpp>

int main( int, char* [] )
{
    my_class test_object( "qwerty" );

    return test_object.is_valid() ? EXIT_SUCCESS : EXIT_FAILURE;
}
