/*
    Copyright 2013 Christian Henning
    Use, modification and distribution are subject to the Boost Software License,
    Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt).
*/

/// \brief Unit test for indexed_image type.

#include <boost/test/unit_test.hpp>

#include <boost/gil/gil_all.hpp>
#include <boost/gil/extension/toolbox/image_types/indexed_image.hpp>


using namespace std;
using namespace boost;
using namespace gil;


BOOST_AUTO_TEST_SUITE( toolbox_tests )

BOOST_AUTO_TEST_CASE( index_image_test )
{
    {
        indexed_image< uint8_t, rgb8_pixel_t > img( 640, 480 );
        fill_pixels( view( img ), rgb8_pixel_t( 255, 0, 0 ));

        rgb8_pixel_t p = *view( img ).xy_at( 10, 10 );
    }

    {
        typedef indexed_image< gray8_pixel_t, rgb8_pixel_t > image_t;

        image_t img( 640, 480, 256 );

        
#if __cplusplus >= 201103L
        generate_pixels( img.get_indices_view()
                       , [] () -> uint8_t
                        {
                            static uint8_t i = 0;
                            i = ( i == 256 ) ? 0 : ++i;

                            return gray8_pixel_t( i );
                        }
                       );


        generate_pixels( img.get_palette_view()
                       , [] () ->rgb8_pixel_t
                        {
                            static uint8_t i = 0;
                            i = ( i == 256 ) ? 0 : ++i;

                            return rgb8_pixel_t( i, i, i );
                        }
                       );
#else

        image_t::indices_view_t indices = img.get_indices_view();

        for( image_t::indices_view_t::iterator it = indices.begin(); it != indices.end(); ++it )
        {
            static uint8_t i = 0; 
            i = ( i == 256 ) ? 0 : ++i;

            *it = gray8_pixel_t( i );
        }

        image_t::palette_view_t colors = img.get_palette_view();
        for( image_t::palette_view_t::iterator it = colors.begin(); it != colors.end(); ++it )
        {
            static uint8_t i = 0; 
            i = ( i == 256 ) ? 0 : ++i;

            *it = rgb8_pixel_t( i, i, i );
        }
#endif

        gray8_pixel_t index = *img.get_indices_view().xy_at( 10   , 1 );
        rgb8_pixel_t  color = *img.get_palette_view().xy_at( index, 0 );

        rgb8_pixel_t p = *view( img ).xy_at( 10, 1 );
    }

    {
        typedef indexed_image< gray8_pixel_t, rgb8_pixel_t > image_t;
        image_t img( 640, 480, 256 );

#if __cplusplus >= 201103L
        generate_pixels( img.get_indices_view()
                       , [] () -> uint8_t
                       {
                            static uint8_t i = 0;
                            i = ( i == 256 ) ? 0 : ++i;

                            return i;
                       }
                       );


        generate_pixels( img.get_palette_view()
                       , [] () ->rgb8_pixel_t
                       {
                          static uint8_t i = 0;
                          i = ( i == 256 ) ? 0 : ++i;

                          return rgb8_pixel_t( i, i, i );
                       }
                       );
#else
        image_t::indices_view_t indices = img.get_indices_view();
        for( image_t::indices_view_t::iterator it = indices.begin(); it != indices.end(); ++it )
        {
            static uint8_t i = 0; 
            i = ( i == 256 ) ? 0 : ++i;

            *it = gray8_pixel_t( i );
        }

        image_t::palette_view_t colors = img.get_palette_view();
        for( image_t::palette_view_t::iterator it = colors.begin(); it != colors.end(); ++it )
        {
            static uint8_t i = 0; 
            i = ( i == 256 ) ? 0 : ++i;

            *it = rgb8_pixel_t( i, i, i );
        }
#endif

        uint8_t      index = *img.get_indices_view().xy_at( 10   , 1 );
        rgb8_pixel_t color = *img.get_palette_view().xy_at( index, 0 );

        rgb8_pixel_t p = *view( img ).xy_at( 10, 1 );
    }

    {
        typedef indexed_image< uint8_t, rgb8_pixel_t > image_t;
        image_t img( 640, 480, 256 );

        for( image_t::y_coord_t y = 0; y < view( img ).height(); ++y )
        {
            image_t::view_t::x_iterator it = view( img ).row_begin( y );

            for( image_t::x_coord_t x = 0; x < view( img ).width(); ++x )
            {
                rgb8_pixel_t p = *it;
                it++;
            }
        }
    }
}

BOOST_AUTO_TEST_SUITE_END()
