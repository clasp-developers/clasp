/*
    Copyright 2007-2008 Andreas Pokorny, Christian Henning
    Use, modification and distribution are subject to the Boost Software License,
    Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
    http://www.boost.org/LICENSE_1_0.txt).
*/

/*************************************************************************************************/

#ifndef BOOST_GIL_EXTENSION_IO_DETAIL_TIFF_IO_DEVICE_HPP
#define BOOST_GIL_EXTENSION_IO_DETAIL_TIFF_IO_DEVICE_HPP

////////////////////////////////////////////////////////////////////////////////////////
/// \file
/// \brief
/// \author Andreas Pokorny, Christian Henning \n
///
/// \date   2007-2008 \n
///
////////////////////////////////////////////////////////////////////////////////////////

// taken from jpegxx - https://bitbucket.org/edd/jpegxx/src/ea2492a1a4a6/src/ijg_headers.hpp
#ifndef BOOST_GIL_EXTENSION_IO_TIFF_C_LIB_COMPILED_AS_CPLUSPLUS
    extern "C" {
#endif

#include <tiff.h>
#include <tiffio.h>

#ifndef BOOST_GIL_EXTENSION_IO_TIFF_C_LIB_COMPILED_AS_CPLUSPLUS
    }
#endif

#include <tiffio.hxx>

#include <boost/shared_ptr.hpp>
#include <boost/utility/enable_if.hpp>

#include <boost/gil/extension/io/detail/base.hpp>
#include <boost/gil/extension/io/detail/io_device.hpp>
#include <boost/gil/extension/io/formats/tiff/log.hpp>

namespace boost { namespace gil { namespace detail {

template< typename Log >
class tiff_device_base
{
public:

    tiff_device_base()
    {}

    tiff_device_base( TIFF* tiff_file )
    : _tiff_file( tiff_file
                , TIFFClose )
    {}


    template <typename Property>
    bool get_property( typename Property::type& value  )
    {
        if( TIFFGetFieldDefaulted( _tiff_file.get()
                                 , Property::tag
                                 , &value ) == 1 )
        {
            return true;
        }

        return false;
    }
   

    template <typename Property>
    inline
    bool set_property( const typename Property::type& value )
    {
       if( TIFFSetField( _tiff_file.get()
                       , Property::tag
                       , value  ) == 1 )
       {
          return true;
       }

       return false;
    }

    // TIFFIsByteSwapped returns a non-zero value if the image data was in a different 
    // byte-order than the host machine. Zero is returned if the TIFF file and local 
    // host byte-orders are the same. Note that TIFFReadTile(), TIFFReadStrip() and TIFFReadScanline() 
    // functions already normally perform byte swapping to local host order if needed.
    bool are_bytes_swapped()
    {
        return ( TIFFIsByteSwapped( _tiff_file.get() )) ? true : false;
    }

    bool is_tiled() const
    {
        return ( TIFFIsTiled( _tiff_file.get() )) ? true : false;
    }

    unsigned int get_default_strip_size()
    {
        return TIFFDefaultStripSize( _tiff_file.get()
                                   , 0 );
    }

    std::size_t get_scanline_size()
    {
      return TIFFScanlineSize( _tiff_file.get() );
    }

    std::size_t get_tile_size()
    {
      return TIFFTileSize( _tiff_file.get() );
    }


    int get_field_defaulted( uint16_t*& red
                           , uint16_t*& green
                           , uint16_t*& blue
                           )
    {
        return TIFFGetFieldDefaulted( _tiff_file.get()
                                    , TIFFTAG_COLORMAP
                                    , &red
                                    , &green
                                    , &blue
                                    );
    }

    template< typename Buffer >
    void read_scanline( Buffer&        buffer
                      , std::ptrdiff_t row
                      , tsample_t      plane
                      )
    {
        io_error_if( TIFFReadScanline( _tiff_file.get()
                                     , reinterpret_cast< tdata_t >( &buffer.front() )
                                     , (uint32) row
                                     , plane           ) == -1
                   , "Read error."
                   );
    }

    void read_scanline( byte_t*        buffer
                      , std::ptrdiff_t row
                      , tsample_t      plane
                      )
    {
        io_error_if( TIFFReadScanline( _tiff_file.get()
                                     , reinterpret_cast< tdata_t >( buffer )
                                     , (uint32) row
                                     , plane           ) == -1
                   , "Read error."
                   );
    }

    template< typename Buffer >
    void read_tile( Buffer&        buffer
                  , std::ptrdiff_t x
                  , std::ptrdiff_t y
                  , std::ptrdiff_t z
                  , tsample_t      plane
                  )
    {
        if( TIFFReadTile( _tiff_file.get()
                        , reinterpret_cast< tdata_t >( &buffer.front() )
                        , (uint32) x
                        , (uint32) y
                        , (uint32) z
                        , plane
                        ) == -1 )
        {
            std::ostringstream oss;
            oss << "Read tile error (" << x << "," << y << "," << z << "," << plane << ").";
            io_error(oss.str().c_str());
        }
    }

    template< typename Buffer >
    void write_scaline( Buffer&     buffer
                      , uint32      row
                      , tsample_t   plane
                      )
    {
       io_error_if( TIFFWriteScanline( _tiff_file.get()
                                     , &buffer.front()
                                     , row
                                     , plane 
                                     ) == -1
                   , "Write error"
                   );
    }

    void write_scaline( byte_t*     buffer
                      , uint32      row
                      , tsample_t   plane
                      )
    {
       io_error_if( TIFFWriteScanline( _tiff_file.get()
                                     , buffer
                                     , row
                                     , plane 
                                     ) == -1
                   , "Write error"
                   );
    }

    template< typename Buffer >
    void write_tile( Buffer&     buffer
                   , uint32      x
                   , uint32      y
                   , uint32      z
                   , tsample_t   plane
                   )
    {
       if( TIFFWriteTile( _tiff_file.get()
                        , &buffer.front()
                        , x
                        , y
                        , z
                        , plane
                        ) == -1 )
           {
               std::ostringstream oss;
               oss << "Write tile error (" << x << "," << y << "," << z << "," << plane << ").";
               io_error(oss.str().c_str());
           }
    }

    void set_directory( tdir_t directory )
    {
        io_error_if( TIFFSetDirectory( _tiff_file.get()
                                     , directory
                                     ) != 1
                   , "Failing to set directory"
                   );
    }

    // return false if the given tile width or height is not TIFF compliant (multiple of 16) or larger than image size, true otherwise
    bool check_tile_size( tiff_tile_width::type&  width
                        , tiff_tile_length::type& height

                        )
    {
        bool result = true;
        uint32 tw = static_cast< uint32 >( width  );
        uint32 th = static_cast< uint32 >( height );

        TIFFDefaultTileSize( _tiff_file.get()
                           , &tw
                           , &th
                           );

        if(width==0 || width%16!=0)
        {
            width = tw;
            result = false;
        }
        if(height==0 || height%16!=0)
        {
            height = th;
            result = false;
        }
        return result;
    }

protected:

   typedef shared_ptr<TIFF> tiff_file_t;
   tiff_file_t _tiff_file;

    Log _log;
};

/*!
 *
 * file_stream_device specialization for tiff images, which are based on TIFF*.
 */
template<>
class file_stream_device< tiff_tag > : public tiff_device_base< tiff_no_log >
{
public:

    struct read_tag {};
    struct write_tag {};

    file_stream_device( std::string const& file_name, read_tag )
    {
        TIFF* tiff;

        io_error_if( ( tiff = TIFFOpen( file_name.c_str(), "r" )) == NULL
                   , "file_stream_device: failed to open file" );

        _tiff_file = tiff_file_t( tiff, TIFFClose );
    }

    file_stream_device( std::string const& file_name, write_tag )
    {
        TIFF* tiff;

        io_error_if( ( tiff = TIFFOpen( file_name.c_str(), "w" )) == NULL
                   , "file_stream_device: failed to open file" );

        _tiff_file = tiff_file_t( tiff, TIFFClose );
    }

    file_stream_device( TIFF* tiff_file )
    : tiff_device_base( tiff_file )
    {}
};

/*!
 *
 * ostream_device specialization for tiff images.
 */
template<>
class ostream_device< tiff_tag > : public tiff_device_base< tiff_no_log >
{
public:
    ostream_device( std::ostream & out )
    : _out( out )
    {
        TIFF* tiff;

        io_error_if( ( tiff = TIFFStreamOpen( ""
                                            , &_out
                                            )
                      ) == NULL
                   , "ostream_device: failed to stream"
                   );

        _tiff_file = tiff_file_t( tiff, TIFFClose );
    }

private:
    ostream_device& operator=( const ostream_device& ) { return *this; }

private:

    std::ostream& _out;
};

/*!
 *
 * ostream_device specialization for tiff images.
 */
template<>
class istream_device< tiff_tag > : public tiff_device_base< tiff_no_log >
{
public:
    istream_device( std::istream & in )
    : _in( in )
    {
        TIFF* tiff;

        io_error_if( ( tiff = TIFFStreamOpen( ""
                                            , &_in
                                            )
                     ) == NULL
                   , "istream_device: failed to stream"
                   );

        _tiff_file = tiff_file_t( tiff, TIFFClose );
    }

private:
    istream_device& operator=( const istream_device& ) { return *this; }

private:

    std::istream& _in;
};

/*
template< typename T, typename D >
struct is_adaptable_input_device< tiff_tag, T, D > : mpl::false_{};
*/

template< typename FormatTag >
struct is_adaptable_input_device< FormatTag
                                , TIFF*
                                , void
                                >
    : mpl::true_
{
    typedef file_stream_device< FormatTag > device_type;
};

template< typename FormatTag >
struct is_adaptable_output_device< FormatTag
                                 , TIFF*
                                 , void
                                 >
    : mpl::true_
{
    typedef file_stream_device< FormatTag > device_type;
};


template < typename Channel > struct sample_format : public mpl::int_<SAMPLEFORMAT_UINT> {};
template<> struct sample_format<bits8>   : public mpl::int_<SAMPLEFORMAT_UINT> {};
template<> struct sample_format<bits16>  : public mpl::int_<SAMPLEFORMAT_UINT> {};
template<> struct sample_format<bits32>  : public mpl::int_<SAMPLEFORMAT_UINT> {};
template<> struct sample_format<bits32f> : public mpl::int_<SAMPLEFORMAT_IEEEFP> {};
template<> struct sample_format<double>  : public mpl::int_<SAMPLEFORMAT_IEEEFP> {};
template<> struct sample_format<bits8s>  : public mpl::int_<SAMPLEFORMAT_INT> {};
template<> struct sample_format<bits16s> : public mpl::int_<SAMPLEFORMAT_INT> {};
template<> struct sample_format<bits32s> : public mpl::int_<SAMPLEFORMAT_INT> {};

template <typename Channel> struct photometric_interpretation {};
template<> struct photometric_interpretation< gray_t > : public mpl::int_< PHOTOMETRIC_MINISBLACK > {};
template<> struct photometric_interpretation< rgb_t  > : public mpl::int_< PHOTOMETRIC_RGB        > {};
template<> struct photometric_interpretation< rgba_t > : public mpl::int_< PHOTOMETRIC_RGB        > {};
template<> struct photometric_interpretation< cmyk_t > : public mpl::int_< PHOTOMETRIC_SEPARATED  > {};


} // namespace detail
} // namespace gil
} // namespace boost

#endif // BOOST_GIL_EXTENSION_IO_DETAIL_TIFF_IO_DEVICE_HPP
