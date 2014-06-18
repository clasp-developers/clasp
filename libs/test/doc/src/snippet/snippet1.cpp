void single_test( int i )
{
    BOOST_CHECK( /* test assertion */ );
}

void combined_test()
{
    int params[] = { 1, 2, 3, 4, 5 };

    std::for_each( params, params+5, &single_test );
}
