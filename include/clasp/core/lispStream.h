/*
    File: lispStream.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
#ifndef lispStream_H //[
#define lispStream_H

#define MERGE_FDSTREAM

#include <fstream>
#include <iostream>
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunneeded-internal-declaration"
//#pragma clang diagnostic ignored "-Wunused-local-typedef"
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#pragma GCC diagnostic pop
#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/numerics.h>
#include <clasp/core/character.h>
#include <clasp/core/lispString.fwd.h>
#include <clasp/core/pathname.fwd.h>
#include <clasp/core/lispVector.fwd.h>
#include <clasp/core/sourceFileInfo.fwd.h>
#include <clasp/core/strWithFillPtr.fwd.h>
#include <clasp/core/intStackQueue.h>

#define OPEN_R "rb"
#define OPEN_W "wb"
#define OPEN_RW "r+b"
#define OPEN_A "ab"
#define OPEN_RA "a+b"

namespace core {

enum StreamMode {                          /*  stream mode  */
                  clasp_smm_input,         /*  input  */
                  clasp_smm_input_file,    /*  input  */
                  clasp_smm_output,        /*  output  */
                  clasp_smm_output_file,   /*  output  */
                  clasp_smm_io,            /*  input-output  */
                  clasp_smm_io_file,       /*  input-output  */
                  clasp_smm_synonym,       /*  synonym  */
                  clasp_smm_broadcast,     /*  broadcast  */
                  clasp_smm_concatenated,  /*  concatenated  */
                  clasp_smm_two_way,       /*  two way  */
                  clasp_smm_echo,          /*  echo  */
                  clasp_smm_string_input,  /*  string input  */
                  clasp_smm_string_output, /*  string output  */
                  clasp_smm_probe,         /*  probe (only used in open_stream())  */
#if defined(ECL_WSOCK)
                  clasp_smm_input_wsock,  /*  input socket (Win32) */
                  clasp_smm_output_wsock, /*  output socket (Win32) */
                  clasp_smm_io_wsock,     /*  input/output socket (Win32) */
#endif
#if defined(ECL_MS_WINDOWS_HOST)
                  clasp_smm_io_wcon, /*  windows console (Win32) */
#endif
                  clasp_smm_sequence_input, /*  sequence input  */
                  clasp_smm_sequence_output /*  sequence output  */
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
namespace core {
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
size_t clasp_input_filePos(T_sp strm);
int clasp_input_lineno(T_sp strm);
int clasp_input_column(T_sp strm);
SourcePosInfo_sp core_inputStreamSourcePosInfo(T_sp strm);
SourceFileInfo_sp clasp_input_source_file_info(T_sp strm);
Pathname_sp clasp_input_pathname(T_sp strm);
/*! Return the filename of the stream if possible, error if errorp=true and no name can be determined */
T_sp clasp_filename(T_sp strm, bool errorp = false);

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

T_sp clasp_make_stream_from_FILE(T_sp fname, FILE *f, enum StreamMode smm, gctools::Fixnum byte_size = 8, int flags = CLASP_STREAM_DEFAULT_FORMAT, T_sp external_format = _Nil<T_O>());

T_sp clasp_make_stream_from_fd(T_sp fname, int fd, enum StreamMode smm, gctools::Fixnum byte_size = 8, int flags = CLASP_STREAM_DEFAULT_FORMAT, T_sp external_format = _Nil<T_O>());

T_sp cl_make_synonym_stream(T_sp sym);
T_sp cl_make_two_way_stream(T_sp in, T_sp out);

T_sp cl_make_string_input_stream(Str_sp strng, Fixnum_sp istart, T_sp iend);
T_sp clasp_make_string_output_stream(cl_index line_length = 128, bool extended = false);
T_sp cl_get_output_stream_string(T_sp strm);

T_sp cl_close(T_sp strm, T_sp abort = _Nil<T_O>());
};

namespace core {

#define CLASP_LISTEN_NO_CHAR 0
#define CLASP_LISTEN_AVAILABLE 1
#define CLASP_LISTEN_EOF -1

typedef claspCharacter (*cl_eformat_decoder)(T_sp stream);
typedef int (*cl_eformat_encoder)(T_sp stream, unsigned char *buffer, claspCharacter c);
typedef cl_index (*cl_eformat_read_byte8)(T_sp object, unsigned char *buffer, cl_index n);

struct FileOps {
  cl_index (*write_byte8)(T_sp strm, unsigned char *c, cl_index n);
  cl_index (*read_byte8)(T_sp strm, unsigned char *c, cl_index n);

  void (*write_byte)(T_sp c, T_sp strm);
  T_sp (*read_byte)(T_sp strm);

  claspCharacter (*read_char)(T_sp strm);
  claspCharacter (*write_char)(T_sp strm, claspCharacter c);
  void (*unread_char)(T_sp strm, claspCharacter c);
  claspCharacter (*peek_char)(T_sp strm);

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

#define C_STREAM 1

typedef enum { clasp_stream_mode_input,
               clasp_stream_mode_output,
               clasp_stream_mode_io } ClaspStreamModeEnum;

class StreamCursor {
public:
  /*! Tell that _LineNumber/_Column mean something */
  bool _CursorIsValid;
  /*! Keep track of line number and column - if not valid return 0 */
  LongLongInt _LineNumber;
  /*! Keep track of column - if not valid return 0 */
  uint _Column;
  LongLongInt _PrevLineNumber;
  /*! Keep track of column - if not valid return 0 */
  uint _PrevColumn;

public:
  StreamCursor() : _CursorIsValid(true), _LineNumber(1), _Column(0){};

public:
  void advanceLineNumber(T_sp strm, claspCharacter c, int num = 1);
  void advanceColumn(T_sp strm, claspCharacter c, int num = 1);
  void invalidate() { this->_CursorIsValid = false; };
  void advanceForChar(T_sp strm, char c, char previous) {
    if ((c == '\n' || c == '\r') && previous != '\r')
      this->advanceLineNumber(strm, c);
    else
      this->advanceColumn(strm, c);
  }
  void backup(T_sp strm, claspCharacter c);

public:
  LongLongInt lineNumber() const { return this->_LineNumber; };
  uint column() const { return this->_Column; };
  bool atStartOfLine() const { return this->_Column == 0; };
  bool isValid() const { return this->_CursorIsValid; };
};
};

namespace core {

SMART(Stream);
class Stream_O : public T_O {
  LISP_BASE1(T_O);
  LISP_CLASS(core, ClPkg, Stream_O, "stream");
  DECLARE_INIT_GLOBALS();

public:
  FileOps ops;
  int _Closed;
  StreamMode _Mode;
  char *_Buffer;
  T_sp _Format;
  int _ByteSize;
  int _Flags;         // bitmap of flags
  List_sp _ByteStack; // For unget in input streams
  cl_eformat_encoder _Encoder;
  cl_eformat_decoder _Decoder;
  Fixnum _LastCode[2];
  claspCharacter _EofChar;
  int _LastOp;
  int _LastChar;
  T_sp _ExternalFormat;
  int _OutputColumn;
  StreamCursor _InputCursor;

public:
  Stream_O() : _Closed(0), _Buffer(NULL), _Format(_Nil<Symbol_O>()), _ByteSize(8), _Flags(0), _ByteStack(_Nil<T_O>()), _Encoder(NULL), _Decoder(NULL), _LastCode{EOF, EOF}, _EofChar(EOF), _ExternalFormat(_Nil<T_O>()), _OutputColumn(0){};
  virtual ~Stream_O(); // nontrivial

public:
  virtual T_sp filename() const;
  virtual int lineno() const;
  virtual int column() const;
};
};
template <>
struct gctools::GCInfo<core::Stream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
SMART(Stream);
class AnsiStream_O : public Stream_O {
  LISP_BASE1(Stream_O);
  LISP_CLASS(core, ExtPkg, AnsiStream_O, "AnsiStream");
  DECLARE_INIT_GLOBALS();

public:
  DEFAULT_CTOR_DTOR(AnsiStream_O);
};
};

namespace core {
class FileStream_O : public AnsiStream_O {
  friend T_sp &FileStreamFilename(T_sp);
  friend T_sp &FileStreamEltType(T_sp);
  LISP_BASE1(AnsiStream_O);
  LISP_CLASS(core, ClPkg, FileStream_O, "file-stream");
  DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  FileStream_O(){};
GCPROTECTED:
  T_sp _Filename;
  T_sp _ElementType;

public: // Functions here
  virtual string __repr__() const;
  T_sp filename() const { return this->_Filename; };
}; // FileStream class

class IOFileStream_O : public FileStream_O {
  friend int &IOFileStreamDescriptor(T_sp);
  LISP_BASE1(FileStream_O);
  LISP_CLASS(core, CorePkg, IOFileStream_O, "iofile-stream");
  DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  IOFileStream_O(){};

private: // instance variables here
  int _FileDescriptor;

public: // Functions here
  static T_sp makeInput(const string &name, int fd) {
    return clasp_make_stream_from_fd(str_create(name), fd, clasp_smm_input_file, 8, CLASP_STREAM_DEFAULT_FORMAT, _Nil<T_O>());
  }
  static T_sp makeOutput(const string &name, int fd) {
    return clasp_make_stream_from_fd(str_create(name), fd, clasp_smm_output_file, 8, CLASP_STREAM_DEFAULT_FORMAT, _Nil<T_O>());
  }
  static T_sp makeIO(const string &name, int fd) {
    return clasp_make_stream_from_fd(str_create(name), fd, clasp_smm_io_file, 8, CLASP_STREAM_DEFAULT_FORMAT, _Nil<T_O>());
  }
  static T_sp make(const string &name, int fd, enum StreamMode smm, T_sp elementType, T_sp externalFormat);

public:
  int fileDescriptor() const { return this->_FileDescriptor; };
};
};
template <>
struct gctools::GCInfo<core::IOFileStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
class IOStreamStream_O : public FileStream_O {
  friend FILE *&IOStreamStreamFile(T_sp strm);
  LISP_BASE1(FileStream_O);
  LISP_CLASS(core, CorePkg, IOStreamStream_O, "iostream-stream");
  DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  IOStreamStream_O(){};

private: // instance variables here
  FILE *_File;

public: // Functions here
  static T_sp makeInput(const string &name, FILE *f) {
    return clasp_make_stream_from_FILE(str_create(name), f, clasp_smm_input, 8, CLASP_STREAM_DEFAULT_FORMAT, _Nil<T_O>());
  }
  static T_sp makeOutput(const string &name, FILE *f) {
    return clasp_make_stream_from_FILE(str_create(name), f, clasp_smm_output, 8, CLASP_STREAM_DEFAULT_FORMAT, _Nil<T_O>());
  };
  static T_sp makeIO(const string &name, FILE *f) {
    return clasp_make_stream_from_FILE(str_create(name), f, clasp_smm_io, 8, CLASP_STREAM_DEFAULT_FORMAT, _Nil<T_O>());
  };

public:
  FILE *file() const { return this->_File; };
};
}; // core namespace
template <>
struct gctools::GCInfo<core::IOStreamStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
class StringStream_O : public AnsiStream_O {
  LISP_BASE1(AnsiStream_O);
  LISP_CLASS(core, ClPkg, StringStream_O, "string-stream");
  DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(StringStream_O);

public: // ctor/dtor for classes with shared virtual base
        //    explicit StringStream_O(core::Class_sp const& mc) : T_O(mc),AnsiStream(mc) {};
        //    virtual ~StringStream_O() {};

public: // Functions here
};      // StringStream class

class StringOutputStream_O : public StringStream_O {
  friend StrWithFillPtr_sp &StringOutputStreamOutputString(T_sp);
  LISP_BASE1(StringStream_O);
  LISP_CLASS(core, CorePkg, StringOutputStream_O, "string-output-stream");
  DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(StringOutputStream_O);

public: // ctor/dtor for classes with shared virtual base
        //    explicit StringStream_O(core::Class_sp const& mc) : T_O(mc),AnsiStream(mc) {};
        //    virtual ~StringStream_O() {};
public: // instance variables here
  StrWithFillPtr_sp _Contents;

public: // Functions here
  void fill(const string &data);
  StrWithFillPtr_sp getAndReset();
}; // StringStream class
};
template <>
struct gctools::GCInfo<core::StringOutputStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
class StringInputStream_O : public StringStream_O {
  friend gctools::Fixnum &StringInputStreamInputPosition(T_sp strm);
  friend gctools::Fixnum &StringInputStreamInputLimit(T_sp strm);
  friend Str_sp &StringInputStreamInputString(T_sp strm);
  LISP_BASE1(StringStream_O);
  LISP_CLASS(core, CorePkg, StringInputStream_O, "string-input-stream");
  DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(StringInputStream_O);

public:    // ctor/dtor for classes with shared virtual base
           //    explicit StringStream_O(core::Class_sp const& mc) : T_O(mc),AnsiStream(mc) {};
           //    virtual ~StringStream_O() {};
GCPRIVATE: // instance variables here
  Str_sp _Contents;
  gctools::Fixnum _InputPosition;
  gctools::Fixnum _InputLimit;

public: // Functions here
  static T_sp make(const string &str);
}; // StringStream class
};
template <>
struct gctools::GCInfo<core::StringInputStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
class SynonymStream_O : public AnsiStream_O {
  friend Symbol_sp &SynonymStreamSymbol(T_sp strm);
  friend T_sp SynonymStreamStream(T_sp);
  LISP_BASE1(AnsiStream_O);
  LISP_CLASS(core, ClPkg, SynonymStream_O, "synonym-stream");
  DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  SynonymStream_O() : _SynonymSymbol(_Nil<Symbol_O>()){};
  virtual ~SynonymStream_O(){};

GCPROTECTED: // instance variables here
  Symbol_sp _SynonymSymbol;

public:
  static SynonymStream_sp make(Symbol_sp symbol) {
    return cl_make_synonym_stream(symbol);
  }

public: // Functions here
  T_sp filename() const;

}; // SynonymStream class

}; // core namespace
template <>
struct gctools::GCInfo<core::SynonymStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
class TwoWayStream_O : public AnsiStream_O {
  friend T_sp &TwoWayStreamInput(T_sp);
  friend T_sp &TwoWayStreamOutput(T_sp);
  LISP_BASE1(AnsiStream_O);
  LISP_CLASS(core, ClPkg, TwoWayStream_O, "two-way-stream");
  DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  TwoWayStream_O() : _In(_Nil<T_O>()), _Out(_Nil<T_O>()){};
  virtual ~TwoWayStream_O(){};
GCPROTECTED: // instance variables here
  T_sp _In;
  T_sp _Out;

public:
  static T_sp make(T_sp in, T_sp out) {
    return cl_make_two_way_stream(in, out);
  };
}; // TwoWayStream class

}; // core namespace
template <>
struct gctools::GCInfo<core::TwoWayStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
FORWARD(BroadcastStream);
class BroadcastStream_O : public AnsiStream_O {
  friend T_sp &BroadcastStreamList(T_sp strm);
  LISP_BASE1(AnsiStream_O);
  LISP_CLASS(core, ClPkg, BroadcastStream_O, "BroadcastStream");
  DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(BroadcastStream_O);

GCPRIVATE: // instance variables here
  T_sp _Streams;

public: // Functions here
};      // BroadcastStream class

}; // core namespace
template <>
struct gctools::GCInfo<core::BroadcastStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
class ConcatenatedStream_O : public AnsiStream_O {
  friend T_sp &ConcatenatedStreamList(T_sp strm);
  LISP_BASE1(AnsiStream_O);
  LISP_CLASS(core, ClPkg, ConcatenatedStream_O, "ConcatenatedStream");
  DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(ConcatenatedStream_O);
GCPRIVATE: // instance variables here
  T_sp _List;

public: // Functions here
};      // ConcatenatedStream class

}; // core namespace
template <>
struct gctools::GCInfo<core::ConcatenatedStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {
class EchoStream_O : public AnsiStream_O {
  friend T_sp &EchoStreamInput(T_sp);
  friend T_sp &EchoStreamOutput(T_sp);
  LISP_BASE1(AnsiStream_O);
  LISP_CLASS(core, ClPkg, EchoStream_O, "EchoStream");
  DECLARE_INIT();
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(EchoStream_O);

GCPRIVATE: // instance variables here
  T_sp _In;
  T_sp _Out;

public: // Functions here
};      // EchoStream class
};
template <>
struct gctools::GCInfo<core::EchoStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

T_sp cl_peekChar(T_sp peek_type, T_sp strm, T_sp eof_errorp, T_sp eof_value, T_sp recursivep);
T_sp cl_readChar(T_sp ostrm, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p);

Str_sp cl_writeString(Str_sp str, T_sp stream, int start, T_sp end);

T_sp cl_write_sequence(T_sp seq, T_sp stream, Fixnum_sp start, T_sp end);

bool cl_streamp(T_sp strm);

Str_sp clasp_writeString(Str_sp str, T_sp stream, int istart = 0, T_sp end = _Nil<T_O>());

//    int af_streamLinenumber(T_sp strm);
//    int af_streamColumn(T_sp strm);

void clasp_terpri(T_sp strm);
void clasp_write_characters(const char *buf, int sz, T_sp strm);
void clasp_write_string(const string &str, T_sp strm);
void clasp_writeln_string(const string &str, T_sp strm);
void writestr_stream(const char *str, T_sp strm);
void clasp_write_addr(T_sp x, T_sp strm);
claspCharacter clasp_write_char(claspCharacter c, T_sp strm);

void initialize_lispStream();

T_sp cl_open(T_sp filename,
             T_sp direction,
             T_sp element_type,
             T_sp if_exists, bool iesp,
             T_sp if_does_not_exist, bool idnesp,
             T_sp external_format,
             T_sp cstream);
T_mv cl_read_line(T_sp sin, T_sp eof_error_p = cl::_sym_T_O, T_sp eof_value = _Nil<T_O>(), T_sp recursive_p = _Nil<T_O>());

T_sp clasp_openRead(T_sp pathDesig);
T_sp clasp_openWrite(T_sp pathDesig);
};

#endif //]
