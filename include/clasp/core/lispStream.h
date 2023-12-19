#pragma once
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

#define MERGE_FDSTREAM

#include <fstream>
#include <iostream>
#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/numerics.h>
#include <clasp/core/character.h>
#include <clasp/core/array.fwd.h>
#include <clasp/core/pathname.fwd.h>
#include <clasp/core/sourceFileInfo.fwd.h>
#include <clasp/core/intStackQueue.h>

#define OPEN_R "rb"
#define OPEN_W "wb"
#define OPEN_RW "r+b"
#define OPEN_A "ab"
#define OPEN_RA "a+b"

namespace core {

#ifdef CLASP_MS_WINDOWS_HOST
#define clasp_mode_t int
#else
#define clasp_mode_t mode_t
#endif

void cannot_close(T_sp stream);
void not_a_file_stream(T_sp fn);
void not_an_input_stream(T_sp fn);
void not_an_output_stream(T_sp fn);
void not_a_character_stream(T_sp s);
void not_a_binary_stream(T_sp s);
void unread_error(T_sp strm);
void unread_twice(T_sp strm);
void io_error(T_sp strm);
#ifdef CLASP_UNICODE
cl_index encoding_error(T_sp strm, unsigned char* buffer, claspCharacter c);
claspCharacter decoding_error(T_sp strm, unsigned char** buffer, int length, unsigned char* buffer_end);
#endif
void wrong_file_handler(T_sp strm) NO_RETURN;
#if defined(ECL_WSOCK)
void wsock_error(const char* err_msg, T_sp strm) NO_RETURN;
#endif

int safe_open(const char* filename, int flags, clasp_mode_t mode);

enum StreamMode {
  stream_mode_input = 0b0001,  //  input
  stream_mode_output = 0b0010, //  output
  stream_mode_io = 0b0011,     //  input-output
  stream_mode_probe = 0b0100   //  probe (only used in open_stream())
};

typedef enum {
  CLASP_STREAM_BINARY = 0,
  CLASP_STREAM_FORMAT = 0xF,
#ifndef CLASP_UNICODE
  CLASP_STREAM_DEFAULT_FORMAT = 1,
#else
  CLASP_STREAM_DEFAULT_FORMAT = 2,
  CLASP_STREAM_ISO_8859_1 = 1,
  CLASP_STREAM_LATIN_1 = 1,
  CLASP_STREAM_UTF_8 = 2,
  CLASP_STREAM_UCS_2 = 4,
  CLASP_STREAM_UCS_2LE = 5 + 128,
  CLASP_STREAM_UCS_2BE = 5,
  CLASP_STREAM_UCS_4 = 6,
  CLASP_STREAM_UCS_4LE = 7 + 128,
  CLASP_STREAM_UCS_4BE = 7,
  CLASP_STREAM_USER_FORMAT = 8,
  CLASP_STREAM_USER_MULTISTATE_FORMAT = 9,
  CLASP_STREAM_US_ASCII = 10,
#endif
  CLASP_STREAM_CR = 16,
  CLASP_STREAM_LF = 32,
  CLASP_STREAM_CRLF = CLASP_STREAM_CR | CLASP_STREAM_LF,
  CLASP_STREAM_SIGNED_BYTES = 64,
  CLASP_STREAM_LITTLE_ENDIAN = 128,
  CLASP_STREAM_C_STREAM = 256,
  CLASP_STREAM_MIGHT_SEEK = 512,
  CLASP_STREAM_CLOSE_COMPONENTS = 1024
} StreamFlagsEnum;

typedef enum {
  listen_result_no_char = 0,
  listen_result_available = 1,
  listen_result_eof = -1,
  listen_result_unknown = -3
} ListenResult;

size_t clasp_input_filePos(T_sp strm);
int clasp_input_lineno(T_sp strm);
int clasp_input_column(T_sp strm);
SourcePosInfo_sp core__input_stream_source_pos_info(T_sp, FileScope_sp, size_t, size_t);
SourcePosInfo_sp clasp_simple_input_stream_source_pos_info(T_sp);
FileScope_sp clasp_input_source_file_info(T_sp strm);
Pathname_sp clasp_input_pathname(T_sp strm);

T_sp cl__unread_char(Character_sp ch, T_sp dstrm);
T_sp cl__get_output_stream_string(T_sp strm);

T_sp clasp_off_t_to_integer(clasp_off_t offset);
clasp_off_t clasp_integer_to_off_t(T_sp i);

T_sp cl__file_length(T_sp stream);
T_sp cl__file_position(T_sp stream, T_sp position = nil<core::T_O>());

T_sp cl__stream_element_type(T_sp strm);
T_sp cl__stream_external_format(T_sp strm);

T_sp cl__make_synonym_stream(T_sp sym);
T_sp cl__make_two_way_stream(T_sp in, T_sp out);

T_sp cl__make_string_input_stream(String_sp strng, cl_index istart, T_sp iend);
#define STRING_OUTPUT_STREAM_DEFAULT_SIZE 128
T_sp clasp_make_string_output_stream(cl_index line_length = STRING_OUTPUT_STREAM_DEFAULT_SIZE, bool extended = false);
T_sp cl__make_string_output_stream(Symbol_sp elementType);
T_sp cl__get_output_stream_string(T_sp strm);

T_sp cl__close(T_sp strm, T_sp abort = nil<T_O>());

cl_index stream_write_byte8(T_sp stream, unsigned char* c, cl_index n);
cl_index stream_read_byte8(T_sp stream, unsigned char* c, cl_index n);

void stream_write_byte(T_sp stream, T_sp c);
T_sp stream_read_byte(T_sp stream);

claspCharacter stream_read_char(T_sp stream);
claspCharacter stream_write_char(T_sp stream, claspCharacter c);
void stream_unread_char(T_sp stream, claspCharacter c);
claspCharacter stream_peek_char(T_sp stream);

cl_index stream_read_vector(T_sp stream, T_sp data, cl_index start, cl_index end);
cl_index stream_write_vector(T_sp stream, T_sp data, cl_index start, cl_index end);

ListenResult stream_listen(T_sp stream);
void stream_clear_input(T_sp stream);
void stream_clear_output(T_sp stream);
void stream_finish_output(T_sp stream);
void stream_force_output(T_sp stream);

bool stream_p(T_sp stream);
bool stream_open_p(T_sp stream);
bool stream_input_p(T_sp stream);
bool stream_output_p(T_sp stream);
bool stream_interactive_p(T_sp stream);
T_sp stream_element_type(T_sp stream);
T_sp stream_set_element_type(T_sp stream, T_sp type);
T_sp stream_external_format(T_sp stream);
T_sp stream_set_external_format(T_sp stream, T_sp format);

T_sp stream_length(T_sp stream);
T_sp stream_position(T_sp stream);
T_sp stream_set_position(T_sp stream, T_sp pos);
T_sp stream_string_length(T_sp stream, T_sp string);
int stream_column(T_sp stream);
int stream_set_column(T_sp stream, int column);

int stream_input_handle(T_sp stream);
int stream_output_handle(T_sp stream);

T_sp stream_close(T_sp stream, T_sp abort);

T_sp stream_pathname(T_sp stream);
T_sp stream_truename(T_sp stream);

// Define types of streams
// See ecl object.h:600

#define C_STREAM 1

typedef enum { clasp_stream_mode_input, clasp_stream_mode_output, clasp_stream_mode_io } ClaspStreamModeEnum;

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
}; // namespace core

template <> struct gctools::GCInfo<core::Stream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

template <> struct fmt::formatter<core::StreamMode> : fmt::formatter<int> {
  template <typename FormatContext>
  auto format(const core::StreamMode& o, FormatContext& ctx) const -> typename FormatContext::iterator {
    return fmt::formatter<int>::format((int)o, ctx);
  }
};

namespace core {

SMART(Stream);
class Stream_O : public General_O {
  LISP_CLASS(core, ClPkg, Stream_O, "stream", General_O);

public:
  DEFAULT_CTOR_DTOR(Stream_O);
};

SMART(Stream);
class AnsiStream_O : public Stream_O {
  LISP_CLASS(core, ExtPkg, AnsiStream_O, "ansi-stream", Stream_O);

public:
  bool _Open;
  char* _Buffer;
  T_sp _Format;
  int _ByteSize;
  int _Flags;         // bitmap of flags
  List_sp _ByteStack; // For unget in input streams
  T_sp _FormatTable;
  Fixnum _LastCode[2];
  claspCharacter _EofChar;
  int _LastOp;
  int _LastChar;
  T_sp _ExternalFormat;
  int _OutputColumn;
  StreamCursor _InputCursor;

public:
  AnsiStream_O()
      : _Open(true), _Buffer(NULL), _Format(nil<Symbol_O>()), _ByteSize(8), _Flags(0), _ByteStack(nil<T_O>()),
        _FormatTable(nil<T_O>()), _LastCode{EOF, EOF}, _EofChar(EOF), _ExternalFormat(nil<T_O>()), _OutputColumn(0){};
  virtual ~AnsiStream_O(); // nontrivial

  cl_index consume_byte_stack(unsigned char* c, cl_index n);
  int restartable_io_error(const char* s);
  void update_column(claspCharacter c);

  virtual cl_index write_byte8(unsigned char* c, cl_index n);
  virtual cl_index read_byte8(unsigned char* c, cl_index n);

  virtual void write_byte(T_sp c);
  virtual T_sp read_byte();

  virtual claspCharacter read_char();
  virtual claspCharacter write_char(claspCharacter c);
  virtual void unread_char(claspCharacter c);
  virtual claspCharacter peek_char();

  virtual cl_index read_vector(T_sp data, cl_index start, cl_index end);
  virtual cl_index write_vector(T_sp data, cl_index start, cl_index end);

  virtual ListenResult listen();
  virtual void clear_input();
  virtual void clear_output();
  virtual void finish_output();
  virtual void force_output();

  virtual bool open_p() const;
  virtual bool input_p() const;
  virtual bool output_p() const;
  virtual bool interactive_p() const;
  virtual T_sp element_type() const;
  virtual T_sp set_element_type(T_sp type);
  virtual T_sp external_format() const;
  virtual T_sp set_external_format(T_sp format);

  virtual T_sp length();
  virtual T_sp position();
  virtual T_sp set_position(T_sp pos);
  virtual T_sp string_length(T_sp string);
  virtual int column() const;
  virtual int set_column(int column);

  virtual int input_handle();
  virtual int output_handle();

  virtual T_sp close(T_sp abort);

  virtual T_sp pathname() const;
  virtual T_sp truename() const;
  virtual int lineno() const;

  inline void check_open() {
    if (!_Open)
      CLOSED_STREAM_ERROR(asSmartPtr());
  }

  inline void check_input() {
    if (!input_p())
      not_an_input_stream(asSmartPtr());
    check_open();
  }

  inline void check_output() {
    if (!output_p())
      not_an_output_stream(asSmartPtr());
    check_open();
  }
};

class FileStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, FileStream_O, "file-stream", AnsiStream_O);

public:
  StreamMode _mode;
  T_sp _Filename;
  T_sp _TempFilename;
  bool _Created = false;
  T_sp _ElementType;

public: // Functions here
  FileStream_O(){};

  virtual string __repr__() const override;
  virtual bool has_file_position() const;
  ListenResult _fd_listen(int fd);
  void close_cleanup(T_sp abort);
  cl_index compute_char_size(claspCharacter c);

  claspCharacter decode_passthrough(unsigned char** buffer, unsigned char* buffer_end);
  int encode_passthrough(unsigned char* buffer, claspCharacter c);

  claspCharacter decode_ascii(unsigned char** buffer, unsigned char* buffer_end);
  int encode_ascii(unsigned char* buffer, claspCharacter c);

  claspCharacter decode_ucs_4be(unsigned char** buffer, unsigned char* buffer_end);
  int encode_ucs_4be(unsigned char* buffer, claspCharacter c);

  claspCharacter decode_ucs_4le(unsigned char** buffer, unsigned char* buffer_end);
  int encode_ucs_4le(unsigned char* buffer, claspCharacter c);

  claspCharacter decode_ucs_4(unsigned char** buffer, unsigned char* buffer_end);
  int encode_ucs_4(unsigned char* buffer, claspCharacter c);

  claspCharacter decode_ucs_2be(unsigned char** buffer, unsigned char* buffer_end);
  int encode_ucs_2be(unsigned char* buffer, claspCharacter c);

  claspCharacter decode_ucs_2le(unsigned char** buffer, unsigned char* buffer_end);
  int encode_ucs_2le(unsigned char* buffer, claspCharacter c);

  claspCharacter decode_ucs_2(unsigned char** buffer, unsigned char* buffer_end);
  int encode_ucs_2(unsigned char* buffer, claspCharacter c);

  claspCharacter decode_user(unsigned char** buffer, unsigned char* buffer_end);
  int encode_user(unsigned char* buffer, claspCharacter c);

  claspCharacter decode_user_multistate(unsigned char** buffer, unsigned char* buffer_end);
  int encode_user_multistate(unsigned char* buffer, claspCharacter c);

  claspCharacter decode_utf_8(unsigned char** buffer, unsigned char* buffer_end);
  int encode_utf_8(unsigned char* buffer, claspCharacter c);

  claspCharacter decode(unsigned char** buffer, unsigned char* buffer_end);
  int encode(unsigned char* buffer, claspCharacter c);
  claspCharacter decode_char_from_buffer(unsigned char* buffer, unsigned char** buffer_pos, unsigned char** buffer_end,
                                         bool seekable, cl_index min_needed_bytes);

  T_sp read_byte_short();
  T_sp read_byte_long();
  T_sp read_byte_le();
  T_sp read_byte_signed8();
  T_sp read_byte_unsigned8();

  void write_byte_short(T_sp c);
  void write_byte_long(T_sp c);
  void write_byte_le(T_sp c);
  void write_byte_signed8(T_sp c);
  void write_byte_unsigned8(T_sp c);

  bool input_p() const;
  bool output_p() const;

  T_sp read_byte();
  void write_byte(T_sp c);
  claspCharacter read_char_no_cursor();

  claspCharacter write_char(claspCharacter c);
  claspCharacter read_char();
  void unread_char(claspCharacter c);

  cl_index read_vector(T_sp data, cl_index start, cl_index n);
  cl_index write_vector(T_sp data, cl_index start, cl_index n);

  T_sp element_type() const;
  T_sp set_element_type(T_sp type);
  T_sp set_external_format(T_sp format);

  T_sp string_length(T_sp string);

  T_sp pathname() const;
  T_sp truename() const;
}; // FileStream class

}; // namespace core

template <> struct gctools::GCInfo<core::IOFileStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

class IOFileStream_O : public FileStream_O {
  LISP_CLASS(core, CorePkg, IOFileStream_O, "iofile-stream", FileStream_O);

public:
  int _FileDescriptor;

public:
  IOFileStream_O(){};

  static T_sp make(T_sp fname, int fd, StreamMode smm, gctools::Fixnum byte_size = 8, int flags = CLASP_STREAM_DEFAULT_FORMAT,
                   T_sp external_format = nil<T_O>(), T_sp tempName = nil<T_O>(), bool created = false);

  int fileDescriptor() const { return this->_FileDescriptor; };
  virtual bool has_file_position() const override;

  cl_index read_byte8(unsigned char* c, cl_index n);
  cl_index write_byte8(unsigned char* c, cl_index n);

  ListenResult listen();
  void clear_input();
  void clear_output();
  void force_output();
  void finish_output();

  bool interactive_p() const;

  T_sp length();
  T_sp position();
  T_sp set_position(T_sp pos);

  int input_handle();
  int output_handle();

  T_sp close(T_sp abort);
};

#ifdef ECL_WSOCK

class WinsockStream_O : public FileStream_O {
  LISP_CLASS(core, CorePkg, WinsockStream_O, "winsock-stream", FileStream_O);

public:
  SOCKET _socket;

public:
  WinsockStream_O(){};

  static T_sp make(T_sp fname, SOCKET socket, StreamMode smm, gctools::Fixnum byte_size = 8,
                   int flags = CLASP_STREAM_DEFAULT_FORMAT, T_sp external_format = nil<T_O>());

  cl_index read_byte8(unsigned char* c, cl_index n);
  cl_index write_byte8(unsigned char* c, cl_index n);
  ListenResult listen();
  void clear_input();
  T_sp close(T_sp abort);
};

#endif

#ifdef CLASP_MS_WINDOWS_HOST

class ConsoleStream_O : public FileStream_O {
  LISP_CLASS(core, CorePkg, ConsoleStream_O, "console-stream", FileStream_O);

  HANDLE _handle;

public:
  ConsoleStream_O(){};

  static T_sp make(T_sp fname, HANDLE handle, StreamMode smm, gctools::Fixnum byte_size = 8,
                   int flags = CLASP_STREAM_DEFAULT_FORMAT, T_sp external_format = nil<T_O>());

  bool interactive_p() const;
  cl_index read_byte8(unsigned char* c, cl_index n);
  cl_index write_byte8(unsigned char* c, cl_index n);
  ListenResult listen();
  void clear_input();
  void force_output();
};

#endif

}; // namespace core

template <> struct gctools::GCInfo<core::IOStreamStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

class IOStreamStream_O : public FileStream_O {
  LISP_CLASS(core, CorePkg, IOStreamStream_O, "iostream-stream", FileStream_O);

public:
  FILE* _File;

public:
  IOStreamStream_O(){};

  void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup);

  static T_sp make(T_sp fname, FILE* f, StreamMode smm, gctools::Fixnum byte_size = 8, int flags = CLASP_STREAM_DEFAULT_FORMAT,
                   T_sp external_format = nil<T_O>(), T_sp tempName = nil<T_O>(), bool created = false);

  static T_sp make(T_sp fname, int fd, StreamMode smm, gctools::Fixnum byte_size = 8, int flags = CLASP_STREAM_DEFAULT_FORMAT,
                   T_sp external_format = nil<T_O>(), T_sp tempName = nil<T_O>(), bool created = false);

  FILE* file() const { return this->_File; };

  cl_index read_byte8(unsigned char* c, cl_index n);
  cl_index write_byte8(unsigned char* c, cl_index n);

  ListenResult _file_listen();

  ListenResult listen();
  void clear_input();
  void clear_output();
  void force_output();
  void finish_output();

  bool interactive_p() const;

  T_sp length();
  T_sp position();
  T_sp set_position(T_sp pos);

  int input_handle();
  int output_handle();

  T_sp close(T_sp abort);
};

class StringStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, StringStream_O, "string-stream", AnsiStream_O);

public:
  DEFAULT_CTOR_DTOR(StringStream_O);
};

}; // namespace core

template <> struct gctools::GCInfo<core::StringOutputStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

class StringOutputStream_O : public StringStream_O {
  LISP_CLASS(core, CorePkg, StringOutputStream_O, "string-output-stream", StringStream_O);

public:
  String_sp _Contents;

public:
  DEFAULT_CTOR_DTOR(StringOutputStream_O);

  void fill(const string& data);
  void clear();
  String_sp getAndReset();

  claspCharacter write_char(claspCharacter c);

  void clear_output();
  void finish_output();
  void force_output();

  bool output_p() const;
  T_sp element_type() const;

  T_sp position();
  T_sp set_position(T_sp pos);
};

}; // namespace core

template <> struct gctools::GCInfo<core::StringInputStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

class StringInputStream_O : public StringStream_O {
  LISP_CLASS(core, CorePkg, StringInputStream_O, "string-input-stream", StringStream_O);

public:
  String_sp _Contents;
  gctools::Fixnum _InputPosition;
  gctools::Fixnum _InputLimit;

public:
  DEFAULT_CTOR_DTOR(StringInputStream_O);

  static T_sp make(const string& str);
  string peer(size_t len);
  string peerFrom(size_t start, size_t len);

  claspCharacter read_char();
  void unread_char(claspCharacter c);
  claspCharacter peek_char();
  ListenResult listen();
  void clear_input();

  bool input_p() const;
  T_sp element_type() const;

  T_sp position();
  T_sp set_position(T_sp pos);
}; // StringStream class

}; // namespace core

template <> struct gctools::GCInfo<core::SynonymStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

class SynonymStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, SynonymStream_O, "synonym-stream", AnsiStream_O);

public: // Simple default ctor/dtor
  SynonymStream_O() : _SynonymSymbol(nil<Symbol_O>()){};

public: // instance variables here
  Symbol_sp _SynonymSymbol;

public:
  static SynonymStream_sp make(Symbol_sp symbol) { return gc::As<SynonymStream_sp>(cl__make_synonym_stream(symbol)); }

public: // Functions here
  virtual string __repr__() const override;

  T_sp stream() const { return _SynonymSymbol->symbolValue(); }

  cl_index read_byte8(unsigned char* c, cl_index n);
  cl_index write_byte8(unsigned char* c, cl_index n);
  T_sp read_byte();
  void write_byte(T_sp c);
  claspCharacter read_char();
  claspCharacter write_char(claspCharacter c);
  void unread_char(claspCharacter c);
  claspCharacter peek_char();
  cl_index read_vector(T_sp data, cl_index start, cl_index n);
  cl_index write_vector(T_sp data, cl_index start, cl_index n);

  ListenResult listen();
  void clear_input();
  void clear_output();
  void force_output();
  void finish_output();

  bool input_p() const;
  bool output_p() const;
  bool interactive_p() const;
  T_sp element_type() const;
  T_sp external_format() const;
  T_sp set_external_format(T_sp format);

  T_sp length();
  T_sp position();
  T_sp set_position(T_sp pos);
  int column() const;
  int set_column(int column);

  int input_handle();
  int output_handle();

  T_sp pathname() const;
  T_sp truename() const;
}; // SynonymStream class

}; // namespace core

template <> struct gctools::GCInfo<core::TwoWayStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

class TwoWayStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, TwoWayStream_O, "two-way-stream", AnsiStream_O);

public: // Simple default ctor/dtor
  TwoWayStream_O() : _In(nil<T_O>()), _Out(nil<T_O>()){};

public: // instance variables here
  T_sp _In;
  T_sp _Out;

public:
  static T_sp make(T_sp in, T_sp out) { return cl__make_two_way_stream(in, out); };

  cl_index read_byte8(unsigned char* c, cl_index n);
  cl_index write_byte8(unsigned char* c, cl_index n);
  T_sp read_byte();
  void write_byte(T_sp c);
  claspCharacter read_char();
  claspCharacter write_char(claspCharacter c);
  void unread_char(claspCharacter c);
  claspCharacter peek_char();
  cl_index read_vector(T_sp data, cl_index start, cl_index n);
  cl_index write_vector(T_sp data, cl_index start, cl_index n);

  ListenResult listen();
  void clear_input();
  void clear_output();
  void force_output();
  void finish_output();

  bool input_p() const;
  bool output_p() const;
  bool interactive_p() const;
  T_sp element_type() const;

  T_sp position();
  int column() const;
  int set_column(int column);

  int input_handle();
  int output_handle();

  T_sp close(T_sp abort);
}; // TwoWayStream class

}; // namespace core

template <> struct gctools::GCInfo<core::BroadcastStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

FORWARD(BroadcastStream);
class BroadcastStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, BroadcastStream_O, "BroadcastStream", AnsiStream_O);

public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(BroadcastStream_O);

public: // instance variables here
  T_sp _Streams;

public: // Functions here
  cl_index write_byte8(unsigned char* c, cl_index n);
  void write_byte(T_sp c);
  claspCharacter write_char(claspCharacter c);

  void clear_output();
  void force_output();
  void finish_output();

  bool output_p() const;
  T_sp element_type() const;
  T_sp external_format() const;

  T_sp length();
  T_sp position();
  T_sp set_position(T_sp pos);
  T_sp string_length(T_sp string);
  int column() const;
  int set_column(int column);
  T_sp close(T_sp abort);
}; // BroadcastStream class

}; // namespace core

template <> struct gctools::GCInfo<core::ConcatenatedStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

class ConcatenatedStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, ConcatenatedStream_O, "ConcatenatedStream", AnsiStream_O);

public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(ConcatenatedStream_O);

public: // instance variables here
  T_sp _List;

public: // Functions here
  cl_index read_byte8(unsigned char* c, cl_index n);
  T_sp read_byte();
  claspCharacter read_char();
  void unread_char(claspCharacter c);
  ListenResult listen();
  void clear_input();

  bool input_p() const;
  T_sp element_type() const;

  T_sp position();
  T_sp close(T_sp abort);
}; // ConcatenatedStream class

}; // namespace core

template <> struct gctools::GCInfo<core::EchoStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

class EchoStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, EchoStream_O, "EchoStream", AnsiStream_O);

public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(EchoStream_O);

public: // instance variables here
  T_sp _In;
  T_sp _Out;

public: // Functions here
  cl_index read_byte8(unsigned char* c, cl_index n);
  cl_index write_byte8(unsigned char* c, cl_index n);
  T_sp read_byte();
  void write_byte(T_sp c);
  claspCharacter read_char();
  claspCharacter write_char(claspCharacter c);
  void unread_char(claspCharacter c);
  claspCharacter peek_char();

  ListenResult listen();
  void clear_input();
  void clear_output();
  void force_output();
  void finish_output();

  bool input_p() const;
  bool output_p() const;
  T_sp element_type() const;

  T_sp position();
  int column() const;
  int set_column(int column);

  int input_handle();
  int output_handle();

  T_sp close(T_sp abort);
}; // EchoStream class

T_sp cl__peek_char(T_sp peek_type, T_sp strm, T_sp eof_errorp, T_sp eof_value, T_sp recursivep);
T_sp cl__read_char(T_sp ostrm, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p);

T_sp cl__write_sequence(T_sp seq, T_sp stream, Fixnum_sp start, T_sp end);
T_sp cl__read_sequence(T_sp sequence, T_sp stream, T_sp start, T_sp oend);

bool cl__streamp(T_sp strm);

String_sp clasp_writeString(String_sp str, T_sp stream, int istart = 0, T_sp end = nil<T_O>());

//    int core__stream_linenumber(T_sp strm);
//    int core__stream_column(T_sp strm);

void clasp_terpri(T_sp strm = cl::_sym_STARstandard_outputSTAR->symbolValue());
void clasp_write_characters(const char* buf, int sz, T_sp strm);
void clasp_write_string(const string& str, T_sp strm = cl::_sym_STARstandard_outputSTAR->symbolValue());
void clasp_writeln_string(const string& str, T_sp strm = cl::_sym_STARstandard_outputSTAR->symbolValue());
void writestr_stream(const char* str, T_sp strm = cl::_sym_STARstandard_outputSTAR->symbolValue());
void core__write_addr(T_sp x, T_sp strm);
claspCharacter clasp_write_char(claspCharacter c, T_sp strm);

T_sp cl__open(T_sp filename, T_sp direction = kw::_sym_input, T_sp element_type = cl::_sym_base_char,
              T_sp if_exists = nil<core::T_O>(), bool iesp = false, T_sp if_does_not_exist = nil<core::T_O>(), bool idnesp = false,
              T_sp external_format = kw::_sym_default, T_sp cstream = lisp_true());
T_mv cl__read_line(T_sp sin, T_sp eof_error_p = cl::_sym_T_O, T_sp eof_value = nil<T_O>(), T_sp recursive_p = nil<T_O>());

T_sp clasp_openRead(T_sp pathDesig);
T_sp clasp_openWrite(T_sp pathDesig);

void denseReadTo8Bit(T_sp stream, size_t charCount, unsigned char* buffer);

}; // namespace core
