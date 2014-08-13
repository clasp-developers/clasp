#ifndef	lispStream_H //[
#define lispStream_H

#define MERGE_FDSTREAM

#include <fstream>
#include <iostream>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "numerics.h"
#include "character.h"
#include "lispString.fwd.h"
#include "pathname.fwd.h"
#include "lispVector.fwd.h"
#include "sourceFileInfo.fwd.h"
#include "strWithFillPtr.fwd.h"
#include "intStackQueue.h"

#define OPEN_R	"rb"
#define OPEN_W	"wb"
#define OPEN_RW	"r+b"
#define OPEN_A	"ab"
#define OPEN_RA	"a+b"

namespace core
{

    enum StreamMode {		/*  stream mode  */
	clasp_smm_input,		/*  input  */
	clasp_smm_input_file,		/*  input  */
	clasp_smm_output,		/*  output  */
	clasp_smm_output_file,	/*  output  */
	clasp_smm_io,			/*  input-output  */
	clasp_smm_io_file,		/*  input-output  */
	clasp_smm_synonym,		/*  synonym  */
	clasp_smm_broadcast,		/*  broadcast  */
	clasp_smm_concatenated,	/*  concatenated  */
	clasp_smm_two_way,		/*  two way  */
	clasp_smm_echo,		/*  echo  */
	clasp_smm_string_input,	/*  string input  */
	clasp_smm_string_output,	/*  string output  */
	clasp_smm_probe,		/*  probe (only used in open_stream())  */
#if defined(ECL_WSOCK)
	clasp_smm_input_wsock,	/*  input socket (Win32) */
	clasp_smm_output_wsock,	/*  output socket (Win32) */
	clasp_smm_io_wsock,		/*  input/output socket (Win32) */
#endif
#if defined(ECL_MS_WINDOWS_HOST)
	clasp_smm_io_wcon,		/*  windows console (Win32) */
#endif
	clasp_smm_sequence_input,	/*  sequence input  */
	clasp_smm_sequence_output	/*  sequence output  */
    };

    typedef enum {
	CLASP_STREAM_BINARY = 0,
	CLASP_STREAM_FORMAT = 0xF,
#ifndef ECL_UNICODE
	CLASP_STREAM_DEFAULT_FORMAT = 1,
#else
	CLASP_STREAM_DEFAULT_FORMAT = 2,
	CLASP_STREAM_ISO_8859_1 = 1,
	CLASP_STREAM_LATIN_1 = 1,
	CLASP_STREAM_UTF_8 = 2,
	CLASP_STREAM_UCS_2 = 3,
	CLASP_STREAM_UCS_2LE = 5 + 128,
	CLASP_STREAM_UCS_2BE = 5,
	CLASP_STREAM_UCS_4 = 6,
	CLASP_STREAM_UCS_4LE = 7 + 128,
	CLASP_STREAM_UCS_4BE = 7,
	CLASP_STREAM_USER_FORMAT = 8,
	CLASP_STREAM_US_ASCII = 10,
#endif
	CLASP_STREAM_CR = 16,
	CLASP_STREAM_LF = 32,
	CLASP_STREAM_SIGNED_BYTES = 64,
	CLASP_STREAM_LITTLE_ENDIAN = 128,
	CLASP_STREAM_C_STREAM = 256,
	CLASP_STREAM_MIGHT_SEEK = 512,
	CLASP_STREAM_CLOSE_COMPONENTS = 1024
    } StreamFlagsEnum;


}
namespace core
{
    cl_index clasp_read_byte8(T_sp stream, unsigned char *c, cl_index n);
    cl_index clasp_write_byte8(T_sp stream, unsigned char *c, cl_index n);

    void clasp_force_output(T_sp strm);

    T_sp clasp_read_byte(T_sp strm);
    void clasp_write_byte(T_sp c, T_sp strm);

    claspCharacter clasp_read_char(T_sp strm);
    void clasp_unread_char(claspCharacter c, T_sp strm);
    claspCharacter clasp_write_char(claspCharacter c, T_sp strm);
    claspCharacter clasp_peek_char(T_sp strm);
    int clasp_listen_stream(T_sp strm);

    void clasp_clear_input(T_sp strm);
    void clasp_clear_output(T_sp strm);
    void clasp_force_output(T_sp strm);

    void clasp_finish_output(T_sp strm);
    int clasp_file_column(T_sp strm);
    int clasp_input_lineno(T_sp strm);
    int clasp_input_column(T_sp strm);
    SourceFileInfo_sp clasp_input_source_file_info(T_sp strm);
    Pathname_sp clasp_input_pathname(T_sp strm);
    /*! Return the filename of the stream if possible, error if errorp=true and no name can be determined */
    Str_sp clasp_filename(T_sp strm, bool errorp=false);
    
    T_sp cl_get_output_stream_string(T_sp strm);

    T_sp clasp_file_length(T_sp strm);
    T_sp clasp_file_position(T_sp strm);
    T_sp clasp_file_position_set(T_sp strm, T_sp pos);
    bool clasp_input_stream_p(T_sp strm);
    bool clasp_output_stream_p(T_sp strm);
    T_sp clasp_stream_element_type(T_sp strm);
    int clasp_interactive_stream_p(T_sp strm);
    T_sp clasp_off_t_to_integer(clasp_off_t offset);
    clasp_off_t clasp_integer_to_off_t(T_sp i);


    T_sp cl_stream_element_type(T_sp strm);
    T_sp cl_stream_external_format(T_sp strm);

    T_sp clasp_make_stream_from_FILE(T_sp fname
                                     , FILE* f
                                     , enum StreamMode smm
                                     , cl_fixnum byte_size=8
                                     , int flags= CLASP_STREAM_DEFAULT_FORMAT
                                     , T_sp external_format=_Nil<T_O>() );

    T_sp clasp_make_stream_from_fd(T_sp fname
                                   , int fd
                                   , enum StreamMode smm
                                   , cl_fixnum byte_size=8
                                   , int flags= CLASP_STREAM_DEFAULT_FORMAT
                                   , T_sp external_format=_Nil<T_O>() );

    T_sp cl_make_synonym_stream(T_sp sym);
    T_sp cl_make_two_way_stream(T_sp in, T_sp out);

    T_sp cl_make_string_input_stream(Str_sp strng, Fixnum_sp istart, Fixnum_sp iend);
    T_sp clasp_make_string_output_stream(cl_index line_length=128, bool extended=false);


    T_sp cl_close(T_sp strm, T_sp abort=_Nil<T_O>() );
};

namespace core
{

#define CLASP_LISTEN_NO_CHAR	0
#define CLASP_LISTEN_AVAILABLE	1
#define CLASP_LISTEN_EOF		-1

    typedef int (*cl_eformat_decoder)(T_sp stream);
    typedef int (*cl_eformat_encoder)(T_sp stream, unsigned char *buffer, int c);
    typedef cl_index (*cl_eformat_read_byte8)(T_sp object, unsigned char *buffer, cl_index n);


    struct FileOps {
        cl_index (*write_byte8)(T_sp strm, unsigned char *c, cl_index n);
        cl_index (*read_byte8)(T_sp strm, unsigned char *c, cl_index n);

        void (*write_byte)(T_sp c, T_sp strm);
        T_sp (*read_byte)(T_sp strm);

        int (*read_char)(T_sp strm);
        int (*write_char)(T_sp strm, int c);
        void (*unread_char)(T_sp strm, int c);
        int (*peek_char)(T_sp strm);

        cl_index (*read_vector)(T_sp strm, T_sp data, cl_index start, cl_index end);
        cl_index (*write_vector)(T_sp strm, T_sp data, cl_index start, cl_index end);

        int (*listen)(T_sp strm);
        void (*clear_input)(T_sp strm);
        void (*clear_output)(T_sp strm);
        void (*finish_output)(T_sp strm);
        void (*force_output)(T_sp strm);

        int (*input_p)(T_sp strm);
        int (*output_p)(T_sp strm);
        int (*interactive_p)(T_sp strm);
        T_sp (*element_type)(T_sp strm);

        T_sp (*length)(T_sp strm);
        T_sp (*get_position)(T_sp strm);
        T_sp (*set_position)(T_sp strm, T_sp pos);
        int (*column)(T_sp strm);

        T_sp (*close)(T_sp strm);
    };





// Define types of streams
// See ecl object.h:600

#define	C_STREAM	1


    typedef enum { brcl_stream_mode_input, brcl_stream_mode_output, brcl_stream_mode_io } BrclStreamModeEnum;

    class StreamCursor
    {
    private:
	/*! Tell that _LineNumber/_Column mean something */
	bool		_CursorIsValid;
	/*! Keep track of line number and column - if not valid return 0 */
	LongLongInt	_LineNumber;
	/*! Keep track of column - if not valid return 0 */
	uint		_Column;
    public:
	StreamCursor(): _CursorIsValid(true), _LineNumber(1), _Column(0) {};
    public:
	void advanceLineNumber(int num=1)
	{
	    this->_LineNumber += num;
	    this->_Column = 0;
	}
	void advanceColumn(int num=1)
	{
	    this->_Column++;
	}
	void invalidate() { this->_CursorIsValid = false;};
	void advanceForChar(char c)
	{
	    if (c == '\n' || c == '\r')
		this->advanceLineNumber();
	    else
		this->advanceColumn();
	}
	void backup() {
	    this->_Column--;
	}
    public:
	LongLongInt lineNumber() const { return this->_LineNumber;};
	uint column() const { return this->_Column;};
	bool atStartOfLine() const { return this->_Column==0;};
	bool isValid() const { return this->_CursorIsValid;};
    };


    /*! Used to store one unput char */
    class UnputChar
    {
    private:
	bool		_HasUnputChar;
	int		_UnputChar;
    public:
	UnputChar() : _HasUnputChar(false) {};
	void clear() { this->_HasUnputChar = false;};
	LongLongInt tellg() { return this->_HasUnputChar ? -1 : 0; };
	void putback(claspChar c) { this->_UnputChar = c; this->_HasUnputChar = true;};
	int get_char() { this->_HasUnputChar = false; return this->_UnputChar;};
	int peek_char() { return this->_UnputChar;};
	bool has_unput_char() { return this->_HasUnputChar;};
	void updateCursor(StreamCursor& c) { c.backup();};
    };



    // Line counting stuff for input streams
#define CURSOR_HANDLING_FUNCTIONS()                                     \
    virtual uint lineNumber() const {return this->_Cursor.lineNumber();}; \
    virtual uint column() const {return this->_Cursor.column();};       \
    virtual void invalidateCursor() {this->_Cursor.invalidate();}       \
    virtual void advanceLineNumber(int num=1) {this->_Cursor.advanceLineNumber(num);}; \
    virtual void advanceColumn(int num=1) {this->_Cursor.advanceColumn(num);}; \

// last


};






namespace core
{






    SMART(Stream);
    class Stream_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,ClPkg,Stream_O,"stream");
	DECLARE_INIT_GLOBALS();
    public:
        FileOps         ops;
        int             _Closed;
        StreamMode      _Mode;
        char*           _Buffer;
        Symbol_sp       _Format;
        int             _ByteSize;
        int             _Flags; // bitmap of flags
        Cons_sp         _ByteStack; // For unget in input streams
        cl_eformat_encoder _Encoder;
        cl_eformat_decoder _Decoder;
        Fixnum          _LastCode[2];
        claspCharacter _EofChar;
        int             _LastOp;
        int             _LastChar;
        T_sp            _ExternalFormat;
        int             _OutputColumn;
    public:
        Stream_O() : _Closed(0)
                   , _Buffer(NULL)
                   , _Format(_Nil<Symbol_O>())
                   , _ByteSize(8)
                   , _Flags(0)
                   , _ByteStack(_Nil<Cons_O>())
                   , _Encoder(NULL)
                   , _Decoder(NULL)
                   , _LastCode{EOF,EOF}
            , _EofChar(EOF)
            , _ExternalFormat(_Nil<T_O>())
            , _OutputColumn(0)
        {};
        virtual ~Stream_O();
    public:
        Str_sp filename() const;
        int lineno() const;
        int column() const;
    };

};
template<> struct gctools::GCInfo<core::Stream_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = true;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};




namespace core
{
    SMART(Stream);
    class AnsiStream_O : public Stream_O
    {
	LISP_BASE1(Stream_O);
	LISP_CLASS(core,ExtPkg,AnsiStream_O,"AnsiStream");
	DECLARE_INIT_GLOBALS();
    public:
	DEFAULT_CTOR_DTOR(AnsiStream_O);
    };

};





namespace core
{
    class FileStream_O : public AnsiStream_O
    {
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,FileStream_O,"file-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(FileStream_O);
	
    public: // Functions here
    }; // FileStream class

    class IOFileStream_O : public FileStream_O
    {
        friend int& IOFileStreamDescriptor(T_sp);
        friend Str_sp& IOFileStreamFilename(T_sp);
        friend T_sp& IOFileStreamEltType(T_sp);
	LISP_BASE1(FileStream_O);
	LISP_CLASS(core,CorePkg,IOFileStream_O,"iofile-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(IOFileStream_O);
	
    private: // instance variables here
        Str_sp  _Filename;
        T_sp    _ElementType;
	int     _FileDescriptor;
    public: // Functions here
        static T_sp makeInput(const string& name, int fd ) {
            return clasp_make_stream_from_fd(str_create(name)
                                             ,fd
                                             ,clasp_smm_input_file
                                             ,8
                                             ,CLASP_STREAM_DEFAULT_FORMAT
                                             ,_Nil<T_O>());
        }
        static T_sp makeOutput(const string& name, int fd ) {
            return clasp_make_stream_from_fd(str_create(name)
                                             ,fd
                                             ,clasp_smm_output_file
                                             ,8
                                             ,CLASP_STREAM_DEFAULT_FORMAT
                                             ,_Nil<T_O>());
        }
        static T_sp makeIO(const string& name, int fd ) {
            return clasp_make_stream_from_fd(str_create(name)
                                             ,fd
                                             ,clasp_smm_io_file
                                             ,8
                                             ,CLASP_STREAM_DEFAULT_FORMAT
                                             ,_Nil<T_O>());
        }
        static T_sp make(const string& name
                         , int fd
                         , enum StreamMode smm
                         , T_sp elementType
                         , T_sp externalFormat );
public:
        Str_sp filename() const { return this->_Filename; };
    }; 
};
template<> struct gctools::GCInfo<core::IOFileStream_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = true;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};

namespace core {    
    class IOStreamStream_O : public FileStream_O
    {
        friend T_sp& IOStreamStreamEltType(T_sp strm);
        friend Str_sp& IOStreamStreamFilename(T_sp strm);
        friend FILE*& IOStreamStreamFile(T_sp strm);
	LISP_BASE1(FileStream_O);
	LISP_CLASS(core,CorePkg,IOStreamStream_O,"iostream-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(IOStreamStream_O);
	
    private: // instance variables here
        Str_sp  _Filename;
        T_sp    _ElementType;
	FILE*   _File;
    public: // Functions here
        static T_sp makeInput(const string& name, FILE* f ) {
            return clasp_make_stream_from_FILE(str_create(name)
                                             ,f
                                             ,clasp_smm_input
                                             ,8
                                             ,CLASP_STREAM_DEFAULT_FORMAT
                                             ,_Nil<T_O>());
        }
        static T_sp makeOutput(const string& name, FILE* f ) {
            return clasp_make_stream_from_FILE(str_create(name)
                                             ,f
                                             ,clasp_smm_output
                                             ,8
                                             ,CLASP_STREAM_DEFAULT_FORMAT
                                             ,_Nil<T_O>());
        };
        static T_sp makeIO(const string& name, FILE* f ) {
            return clasp_make_stream_from_FILE(str_create(name)
                                             ,f
                                             ,clasp_smm_io
                                             ,8
                                             ,CLASP_STREAM_DEFAULT_FORMAT
                                             ,_Nil<T_O>());
        };
    public:
        Str_sp filename() const { return this->_Filename; };
    }; 
}; // core namespace
template<> struct gctools::GCInfo<core::IOStreamStream_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = true;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};
    






namespace core
{
    class StringStream_O : public AnsiStream_O
    {
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,StringStream_O,"string-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(StringStream_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit StringStream_O(core::Class_sp const& mc) : T_O(mc),AnsiStream(mc) {};
//    virtual ~StringStream_O() {};
	
    public: // Functions here
    }; // StringStream class
    

    class StringOutputStream_O : public StringStream_O
    {
        friend StrWithFillPtr_sp& StringOutputStreamOutputString(T_sp);
	LISP_BASE1(StringStream_O);
	LISP_CLASS(core,CorePkg,StringOutputStream_O,"string-output-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(StringOutputStream_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit StringStream_O(core::Class_sp const& mc) : T_O(mc),AnsiStream(mc) {};
//    virtual ~StringStream_O() {};
    private: // instance variables here
        StrWithFillPtr_sp       _Contents;
    public: // Functions here
    }; // StringStream class
};
template<> struct gctools::GCInfo<core::StringOutputStream_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = true;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};

namespace core {
    class StringInputStream_O : public StringStream_O
    {
        friend cl_fixnum& StringInputStreamInputPosition(T_sp strm);
        friend cl_fixnum& StringInputStreamInputLimit(T_sp strm);
        friend Str_sp& StringInputStreamInputString(T_sp strm);
	LISP_BASE1(StringStream_O);
	LISP_CLASS(core,CorePkg,StringInputStream_O,"string-input-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(StringInputStream_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit StringStream_O(core::Class_sp const& mc) : T_O(mc),AnsiStream(mc) {};
//    virtual ~StringStream_O() {};
    private: // instance variables here
        Str_sp       _Contents;
        cl_fixnum       _InputPosition;
        cl_fixnum       _InputLimit;
    public: // Functions here
        static T_sp make(const string& str);
    }; // StringStream class
};
template<> struct gctools::GCInfo<core::StringInputStream_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = true;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};
    


namespace core
{
    class SynonymStream_O : public AnsiStream_O
    {
        friend Symbol_sp& SynonymStreamSymbol(T_sp strm);
        friend T_sp& SynonymStreamStream(T_sp);
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,SynonymStream_O,"synonym-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	SynonymStream_O() : _SynonymSymbol(_Nil<Symbol_O>()), _Stream(_Nil<T_O>()) {};
        virtual ~SynonymStream_O() {};

    protected: // instance variables here
	Symbol_sp 	_SynonymSymbol;
        T_sp            _Stream;
    public:
	static SynonymStream_sp make(Symbol_sp symbol) {
            return cl_make_synonym_stream(symbol);
        }
    public: // Functions here
        Str_sp filename() const { return clasp_filename(this->_Stream);};

    }; // SynonymStream class
    
}; // core namespace
template<> struct gctools::GCInfo<core::SynonymStream_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = true;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};









namespace core
{
    class TwoWayStream_O : public AnsiStream_O
    {
        friend T_sp& TwoWayStreamInput(T_sp);
        friend T_sp& TwoWayStreamOutput(T_sp);
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,TwoWayStream_O,"two-way-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	TwoWayStream_O() : _In(_Nil<T_O>()), _Out(_Nil<T_O>()) {};
	virtual ~TwoWayStream_O() {};
    protected: // instance variables here
	T_sp 	_In;
	T_sp 	_Out;
    public:
        static T_sp make(T_sp in, T_sp out) {
            return cl_make_two_way_stream(in,out);
        };
    }; // TwoWayStream class
    
}; // core namespace
template<> struct gctools::GCInfo<core::TwoWayStream_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = true;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};


namespace core
{
    FORWARD(BroadcastStream);
    class BroadcastStream_O : public AnsiStream_O
    {
        friend T_sp& BroadcastStreamList(T_sp strm);
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,BroadcastStream_O,"BroadcastStream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(BroadcastStream_O);
	
    private: // instance variables here
        T_sp         _Streams;
    public: // Functions here
    }; // BroadcastStream class
    
}; // core namespace
template<> struct gctools::GCInfo<core::BroadcastStream_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = true;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};





namespace core
{
    class ConcatenatedStream_O : public AnsiStream_O
    {
        friend T_sp& ConcatenatedStreamList(T_sp strm);
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,ConcatenatedStream_O,"ConcatenatedStream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(ConcatenatedStream_O);
    private: // instance variables here
        T_sp    _List;
    public: // Functions here
    }; // ConcatenatedStream class
    
}; // core namespace
template<> struct gctools::GCInfo<core::ConcatenatedStream_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = true;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};




namespace core
{
    class EchoStream_O : public AnsiStream_O
    {
        friend T_sp& EchoStreamInput(T_sp);
        friend T_sp& EchoStreamOutput(T_sp);
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,EchoStream_O,"EchoStream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(EchoStream_O);
	
    private: // instance variables here
        T_sp _In;
        T_sp _Out;
	
    public: // Functions here
    }; // EchoStream class

};
template<> struct gctools::GCInfo<core::EchoStream_O> {
    static bool constexpr NeedsInitialization = false;
    static bool constexpr NeedsFinalization = true;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};


namespace core {

    T_sp cl_peekChar(T_sp peek_type, T_sp strm, T_sp eof_errorp, T_sp eof_value, T_sp recursivep);
    T_sp cl_readChar(T_sp ostrm, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p);



    Str_sp cl_writeString(Str_sp str, T_sp stream, int start, Fixnum_sp end);

    T_sp cl_writeSequence(T_sp seq, T_sp stream, Fixnum_sp start, Fixnum_sp end);


    bool cl_streamp(T_sp strm);

    Str_sp clasp_writeString(Str_sp str, T_sp stream, int istart=0, Fixnum_sp end=_Nil<Fixnum_O>());

//    int af_streamLinenumber(T_sp strm);
//    int af_streamColumn(T_sp strm);


    void clasp_terpri(T_sp strm);
    void clasp_write_characters(const char* buf, int sz, T_sp strm);
    void clasp_write_string(const string& str, T_sp strm);
    void clasp_writeln_string(const string& str, T_sp strm);

    void initialize_lispStream();

    T_sp cl_open(T_sp filename,
                 T_sp direction,
                 T_sp element_type, 
                 T_sp if_exists, T_sp iesp,
                 T_sp if_does_not_exist, T_sp idnesp,
                 T_sp external_format,
                 T_sp cstream );

};






#endif //]
