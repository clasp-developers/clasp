
#include <iostream>
#include <boost/numeric/ublas/banded.hpp>
#include <boost/numeric/ublas/io.hpp>
#include <boost/numeric/ublas/operation.hpp>
#include <iomanip>

#include "utils.hpp"

using namespace boost::numeric::ublas;
 

int expected_index( int index, column_major tag ) {
   // this is the data shown on http://www.netlib.org/lapack/lug/node124.html
   // read column-by-column, aka column_major
   int mapping[] = { 0, 11, 21, 31, 12, 22, 32, 42, 23, 33, 43, 53, 34, 44, 54, 0, 45, 55, 0, 0 };
   return mapping[ index ];
}

int expected_index( int index, row_major tag ) {
   // this is the data shown on http://www.netlib.org/lapack/lug/node124.html
   // read row-by-row, aka row_major
   int mapping[] = { 0, 0, 11, 12, 0, 21, 22, 23, 31, 32, 33, 34, 42, 43, 44, 45, 53, 54, 55, 0 };
   return mapping[ index ];
}

template< typename Orientation >
bool test_band_storage() {
        
    int m = 5;
    int n = 5;
    int kl = 2;
    int ku = 1;
    int band_storage_size = n * (1+kl+ku);
    
    banded_matrix< int, Orientation > test_matrix( m, n, kl, ku );
    test_matrix.clear();
    
    test_matrix( 0, 0 ) = 11;
    test_matrix( 0, 1 ) = 12;
    test_matrix( 1, 0 ) = 21;
    test_matrix( 1, 1 ) = 22;
    test_matrix( 1, 2 ) = 23;
    test_matrix( 2, 0 ) = 31;
    test_matrix( 2, 1 ) = 32;
    test_matrix( 2, 2 ) = 33;
    test_matrix( 2, 3 ) = 34;
    test_matrix( 3, 1 ) = 42;
    test_matrix( 3, 2 ) = 43;
    test_matrix( 3, 3 ) = 44;
    test_matrix( 3, 4 ) = 45;
    test_matrix( 4, 2 ) = 53;
    test_matrix( 4, 3 ) = 54;
    test_matrix( 4, 4 ) = 55;
        
    BOOST_UBLAS_TEST_TRACE( "Full matrix" );
    BOOST_UBLAS_TEST_TRACE( std::setw( 3 ) << test_matrix );
    
    BOOST_UBLAS_TEST_TRACE( "data() of matrix" );
    for ( int i = 0; i < band_storage_size; ++i ) {
        std::cerr << test_matrix.data()[ i ] << " ";
    }
    std::cerr << std::endl;
   
    BOOST_UBLAS_TEST_TRACE( "Expected data() of matrix" );
    for ( int i = 0; i < band_storage_size; ++i ) {
        std::cerr << expected_index( i, Orientation() ) << " ";
    }
    std::cerr << std::endl;
    
    size_t mismatch = 0;

    for ( int i = 0; i < band_storage_size; ++i ) {
      if ( test_matrix.data()[ i ] != expected_index( i, Orientation() ) ) {
        ++mismatch;
      }
    }

    return 0 == mismatch;
}

BOOST_UBLAS_TEST_DEF( banded_matrix_column_major )
{
	BOOST_UBLAS_TEST_TRACE( "Test case: storage layout banded_matrix < column_major >" );

    BOOST_UBLAS_TEST_CHECK( test_band_storage< column_major >() );
}

BOOST_UBLAS_TEST_DEF( banded_matrix_row_major )
{
	BOOST_UBLAS_TEST_TRACE( "Test case: storage layout banded_matrix < row_major >" );

    BOOST_UBLAS_TEST_CHECK( test_band_storage< row_major >() );
}

int main()
{

	BOOST_UBLAS_TEST_SUITE( "Test storage layout of banded matrix type" );

	BOOST_UBLAS_TEST_TRACE( "Example data taken from http://www.netlib.org/lapack/lug/node124.html" );

	BOOST_UBLAS_TEST_BEGIN();

    BOOST_UBLAS_TEST_DO( banded_matrix_column_major );
    
    BOOST_UBLAS_TEST_DO( banded_matrix_row_major );

	BOOST_UBLAS_TEST_END();

    return EXIT_SUCCESS;
}


