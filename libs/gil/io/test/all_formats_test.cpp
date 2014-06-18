/*
    Copyright 2013 Christian Henning
    Use, modification and distribution are subject to the Boost Software License,
    Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt).
*/

// Test will include all format's headers and load and write some images.
// This test is more of a compilation test.


#include <boost/gil/extension/io/png_all.hpp>
#include <boost/gil/extension/io/bmp_all.hpp>
#include <boost/gil/extension/io/jpeg_all.hpp>
#include <boost/gil/extension/io/pnm_all.hpp>
#include <boost/gil/extension/io/targa_all.hpp>
#include <boost/gil/extension/io/tiff_all.hpp>

#include <boost/test/unit_test.hpp>

#include "paths.hpp"

using namespace std;
using namespace boost::gil;


BOOST_AUTO_TEST_SUITE( gil_io_tests )

BOOST_AUTO_TEST_CASE( non_bit_aligned_image_test )
{
#ifdef BOOST_GIL_IO_TEST_ALLOW_READING_IMAGES
    {
        rgb8_image_t img;
        read_image( bmp_filename, img, bmp_tag() );
#ifdef BOOST_GIL_IO_TEST_ALLOW_WRITING_IMAGES
        write_view( bmp_out + "all_formats_test.bmp", view( img ), bmp_tag() );
#endif // BOOST_GIL_IO_TEST_ALLOW_WRITING_IMAGES
    }

    {
        rgb8_image_t img;
        read_image( jpeg_filename, img, jpeg_tag() );
#ifdef BOOST_GIL_IO_TEST_ALLOW_WRITING_IMAGES
        write_view( jpeg_out + "all_formats_test.jpg", view( img ), jpeg_tag() );
#endif // BOOST_GIL_IO_TEST_ALLOW_WRITING_IMAGES
    }

    {
        rgba8_image_t img;
        read_image( png_filename, img, png_tag() );
#ifdef BOOST_GIL_IO_TEST_ALLOW_WRITING_IMAGES
        write_view( png_out + "all_formats_test.png", view( img ), png_tag() );
#endif // BOOST_GIL_IO_TEST_ALLOW_WRITING_IMAGES
    }

    {
        rgb8_image_t img;
        read_image( pnm_filename, img, pnm_tag() );
#ifdef BOOST_GIL_IO_TEST_ALLOW_WRITING_IMAGES
        write_view( pnm_out + "all_formats_test.pnm", view( img ), pnm_tag() );
#endif // BOOST_GIL_IO_TEST_ALLOW_WRITING_IMAGES
    }

    {
        rgb8_image_t img;
        read_image( targa_filename, img, targa_tag() );
#ifdef BOOST_GIL_IO_TEST_ALLOW_WRITING_IMAGES
        write_view( targa_out + "all_formats_test.tga", view( img ), targa_tag() );
#endif // BOOST_GIL_IO_TEST_ALLOW_WRITING_IMAGES
    }

    {
        rgba8_image_t img;
        read_image( tiff_filename, img, tiff_tag() );
#ifdef BOOST_GIL_IO_TEST_ALLOW_WRITING_IMAGES
        write_view( tiff_out + "all_formats_test.tif", view( img ), tiff_tag() );
#endif // BOOST_GIL_IO_TEST_ALLOW_WRITING_IMAGES
    }
#endif // BOOST_GIL_IO_TEST_ALLOW_READING_IMAGES
}

BOOST_AUTO_TEST_SUITE_END()
