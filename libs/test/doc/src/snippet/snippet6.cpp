template<typename TestType>
void
specific_type_test( TestType* = 0 )
{
    MyComponent<TestType> c;
    ... // here we perform actual testing
}

void my_component_test()
{
    specific_type_test( (int*)0 );
    specific_type_test( (float*)0 );
    specific_type_test( (UDT*)0 );
    ... 
}
