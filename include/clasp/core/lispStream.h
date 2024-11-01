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
#include <clasp/core/symbol.h>
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

// GCInfo structures

template <> struct gctools::GCInfo<core::Stream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

template <> struct gctools::GCInfo<core::BroadcastStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

template <> struct gctools::GCInfo<core::ConcatenatedStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

template <> struct gctools::GCInfo<core::EchoStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

template <> struct gctools::GCInfo<core::StringInputStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

template <> struct gctools::GCInfo<core::StringOutputStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

template <> struct gctools::GCInfo<core::SynonymStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

template <> struct gctools::GCInfo<core::TwoWayStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

template <> struct gctools::GCInfo<core::PosixFileStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

template <> struct gctools::GCInfo<core::CFileStream_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

#ifdef CLASP_MS_WINDOWS_HOST
#define clasp_mode_t int
#else
#define clasp_mode_t mode_t
#endif

enum class StreamDirection : uint8_t {
  input = 0b0001,  //  input
  output = 0b0010, //  output
  io = 0b0011,     //  input-output
  probe = 0b0100   //  probe (only used in open_stream())
};

inline bool operator&(StreamDirection x, StreamDirection y) {
  return static_cast<bool>(static_cast<uint8_t>(x) & static_cast<uint8_t>(y));
};

inline bool has_direction(StreamDirection is, StreamDirection want) {
  return (static_cast<uint8_t>(is) & static_cast<uint8_t>(want)) == static_cast<uint8_t>(want);
};

enum class StreamIfExists : uint8_t {
  nil = 0,
  error = 1,
  new_version = 2,
  rename = 3,
  rename_and_delete = 4,
  overwrite = 5,
  append = 6,
  supersede = 7
};

enum class StreamIfDoesNotExist : uint8_t { nil = 0, error = 1, create = 2 };

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

typedef enum : claspCharacter {
  listen_result_available = 1,
  listen_result_eof = EOF,
  listen_result_no_char = EOF - 2,
  listen_result_unknown = EOF - 3
} ListenResult;

// Stream interface functions

T_sp stream_open(T_sp fn, StreamDirection direction, StreamIfExists if_exists, StreamIfDoesNotExist if_does_not_exist,
                 gctools::Fixnum byte_size, int flags, T_sp external_format);
T_sp stream_close(T_sp stream, T_sp abort);

// Low level byte functions

cl_index stream_read_byte8(T_sp stream, unsigned char* c, cl_index n);
cl_index stream_write_byte8(T_sp stream, unsigned char* c, cl_index n);

// Binary stream functions

T_sp stream_read_byte(T_sp stream);
void stream_write_byte(T_sp stream, T_sp c);

// Character input functions

claspCharacter stream_read_char(T_sp stream);
void stream_unread_char(T_sp stream, claspCharacter c);
claspCharacter stream_read_char_no_hang(T_sp stream);
claspCharacter stream_peek_char(T_sp stream);
ListenResult stream_listen(T_sp stream);
T_mv stream_read_line(T_sp stream);
void stream_clear_input(T_sp stream);

// Character output functions

claspCharacter stream_write_char(T_sp stream, claspCharacter c);
bool stream_advance_to_column(T_sp stream, T_sp column);
void stream_write_string(T_sp stream, String_sp data, cl_index start, cl_index end);
void stream_terpri(T_sp stream);
bool stream_fresh_line(T_sp stream);
void stream_clear_output(T_sp stream);
void stream_finish_output(T_sp stream);
void stream_force_output(T_sp stream);

// Sequence functions

cl_index stream_read_sequence(T_sp stream, T_sp data, cl_index start, cl_index end);
void stream_write_sequence(T_sp stream, T_sp data, cl_index start, cl_index end);

// Predicates

bool stream_p(T_sp stream);
bool stream_open_p(T_sp stream);
bool stream_input_p(T_sp stream);
bool stream_output_p(T_sp stream);
bool stream_interactive_p(T_sp stream);

// Stream element type and external format

T_sp stream_element_type(T_sp stream);
T_sp stream_set_element_type(T_sp stream, T_sp type);
T_sp stream_external_format(T_sp stream);
T_sp stream_set_external_format(T_sp stream, T_sp format);

// Stream length and position functions

T_sp stream_length(T_sp stream);
T_sp stream_position(T_sp stream);
T_sp stream_set_position(T_sp stream, T_sp pos);
T_sp stream_string_length(T_sp stream, T_sp string);

// Stream column and line functions

T_sp stream_output_column(T_sp stream);
bool stream_start_line_p(T_sp stream);
T_sp stream_line(T_sp stream);
void stream_update_output_cursor(T_sp stream, claspCharacter c);
void stream_restore_output_cursor(T_sp stream);

uint stream_output_column_as_uint(T_sp stream);
uint stream_output_line_as_uint(T_sp stream);

// Stream input tracking functions

T_sp stream_input_column(T_sp stream);
T_sp stream_input_line(T_sp stream);
void stream_update_input_cursor(T_sp stream, claspCharacter c);
void stream_restore_input_cursor(T_sp stream);

uint stream_input_column_as_uint(T_sp stream);
uint stream_input_line_as_uint(T_sp stream);

// Stream pathname functions

T_sp stream_pathname(T_sp stream);
T_sp stream_truename(T_sp stream);

// Stream file descriptor functions

int stream_file_descriptor(T_sp stream, StreamDirection direction);

// CL interface function

T_sp cl__close(T_sp stream, T_sp abort = nil<T_O>());
T_sp cl__make_string_input_stream(String_sp strng, cl_index istart, T_sp iend);
#define STRING_OUTPUT_STREAM_DEFAULT_SIZE 128
StringOutputStream_sp clasp_make_string_output_stream(cl_index line_length = STRING_OUTPUT_STREAM_DEFAULT_SIZE,
                                                      bool extended = false);
StringOutputStream_sp cl__make_string_output_stream(Symbol_sp elementType);

void cl__terpri(T_sp outputStreamDesig = nil<T_O>());

T_sp cl__peek_char(T_sp peek_type, T_sp strm, T_sp eof_errorp, T_sp eof_value, T_sp recursivep);
T_sp cl__read_char(T_sp ostrm, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p);

T_sp cl__write_sequence(T_sp seq, T_sp stream, Fixnum_sp start, T_sp end);
T_sp cl__read_sequence(T_sp sequence, T_sp stream, T_sp start, T_sp oend);

bool cl__streamp(T_sp strm);

String_sp cl__write_string(String_sp str, T_sp stream, int istart = 0, T_sp end = nil<T_O>());

T_sp cl__open(T_sp filename, StreamDirection direction = StreamDirection::input, T_sp element_type = cl::_sym_base_char,
              StreamIfExists if_exists = StreamIfExists::nil, bool iesp = false,
              StreamIfDoesNotExist if_does_not_exist = StreamIfDoesNotExist::nil, bool idnesp = false,
              T_sp external_format = kw::_sym_default, T_sp cstream = lisp_true());
T_mv cl__read_line(T_sp sin, T_sp eof_error_p = cl::_sym_T_O, T_sp eof_value = nil<T_O>(), T_sp recursive_p = nil<T_O>());

// Clasp Stream Utility Functions

size_t clasp_input_filePos(T_sp strm);
int clasp_input_lineno(T_sp strm);
int clasp_input_column(T_sp strm);
SourcePosInfo_sp core__input_stream_source_pos_info(T_sp, FileScope_sp, size_t, size_t);
SourcePosInfo_sp clasp_simple_input_stream_source_pos_info(T_sp);
FileScope_sp clasp_input_source_file_info(T_sp strm);
Pathname_sp clasp_input_pathname(T_sp strm);

T_sp clasp_off_t_to_integer(clasp_off_t offset);
clasp_off_t clasp_integer_to_off_t(T_sp i);

void clasp_write_characters(const char* buf, int sz, T_sp strm);
void clasp_write_string(const string& str, T_sp strm = cl::_sym_STARstandard_outputSTAR->symbolValue());
void clasp_write_string(const char* str, T_sp strm = cl::_sym_STARstandard_outputSTAR->symbolValue());
void clasp_writeln_string(const string& str, T_sp strm = cl::_sym_STARstandard_outputSTAR->symbolValue());
void clasp_writeln_string(const char* str, T_sp strm = cl::_sym_STARstandard_outputSTAR->symbolValue());
T_sp cl__finish_output(T_sp strm = cl::_sym_STARstandard_outputSTAR->symbolValue());
void core__write_addr(T_sp x, T_sp strm);

// Error functions

void cannot_close(T_sp stream);
void not_a_file_stream(T_sp fn);
void not_an_input_stream(T_sp fn);
void not_an_output_stream(T_sp fn);
void not_a_character_stream(T_sp s);
void not_a_binary_stream(T_sp s);
void unread_error(T_sp strm);
void unread_twice(T_sp strm);
#ifdef CLASP_UNICODE
cl_index encoding_error(T_sp strm, unsigned char* buffer, claspCharacter c);
claspCharacter decoding_error(T_sp strm, unsigned char** buffer, int length, unsigned char* buffer_end);
#endif
void wrong_file_handler(T_sp strm) NO_RETURN;
#if defined(ECL_WSOCK)
void wsock_error(const char* err_msg, T_sp strm) NO_RETURN;
#endif

inline void check_stream(T_sp stream) {
  if (!stream_p(stream))
    TYPE_ERROR(stream, cl::_sym_Stream_O);
}

inline void check_input_stream(T_sp stream) {
  if (!stream_input_p(stream))
    not_an_input_stream(stream);
}

inline void check_output_stream(T_sp stream) {
  if (!stream_output_p(stream))
    not_an_output_stream(stream);
}

// Define types of streams

#define C_STREAM 1

class StreamCursor {
public:
  std::pair<uint, uint> _previous, _current;

  StreamCursor() : _previous(0, 1), _current(0, 1) {}

  void save() { _previous = _current; }
  void restore() { _current = _previous; }
  void reset() { _previous = _current = std::make_pair(0, 1); }

  uint column() const { return _current.first; }
  uint& column() { return _current.first; }
  uint line() const { return _current.second; }
  uint& line() { return _current.second; }

  claspCharacter update(claspCharacter c);
  bool start_line_p() const { return this->column() == 0; };
};

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
  bool _open;
  int _flags; // bitmap of flags
  StreamCursor _input_cursor, _output_cursor;

public:
  AnsiStream_O() : _open(true){};
  virtual ~AnsiStream_O(); // nontrivial

  [[noreturn]] void io_error(const char* s);

  virtual T_sp close(T_sp abort);

  virtual cl_index read_byte8(unsigned char* c, cl_index n);
  virtual cl_index write_byte8(unsigned char* c, cl_index n);

  virtual T_sp read_byte();
  virtual void write_byte(T_sp c);

  virtual claspCharacter read_char();
  virtual void unread_char(claspCharacter c);
  virtual claspCharacter read_char_no_hang();
  virtual claspCharacter peek_char();
  virtual ListenResult listen();
  virtual T_mv read_line();
  virtual void clear_input();

  virtual claspCharacter write_char(claspCharacter c);
  virtual bool advance_to_column(T_sp column);
  virtual void write_string(String_sp data, cl_index start, cl_index end);
  virtual void terpri();
  virtual bool fresh_line();
  virtual void clear_output();
  virtual void finish_output();
  virtual void force_output();

  virtual cl_index read_sequence(T_sp data, cl_index start, cl_index end);
  virtual void write_sequence(T_sp data, cl_index start, cl_index end);

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

  virtual T_sp output_column() const;
  virtual bool start_line_p() const;
  virtual T_sp output_line() const;
  virtual void update_output_cursor(claspCharacter c);
  virtual void restore_output_cursor();

  virtual T_sp input_column() const;
  virtual T_sp input_line() const;
  virtual void update_input_cursor(claspCharacter c);
  virtual void restore_input_cursor();

  virtual T_sp pathname() const;
  virtual T_sp truename() const;

  virtual int file_descriptor(StreamDirection direction) const;

  inline void check_open() {
    if (!_open)
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

FORWARD(BroadcastStream);
class BroadcastStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, BroadcastStream_O, "BroadcastStream", AnsiStream_O);

public: // instance variables here
  T_sp _streams;

public:
  DEFAULT_CTOR_DTOR(BroadcastStream_O);

  static BroadcastStream_sp make(List_sp streams);
  static T_sp streams(T_sp broadcast_stream);

  inline T_sp last_stream() const { return _streams.as<Cons_O>()->last().as_unsafe<Cons_O>()->car(); }

  T_sp close(T_sp abort) override;

  cl_index write_byte8(unsigned char* c, cl_index n) override;

  void write_byte(T_sp c) override;

  claspCharacter write_char(claspCharacter c) override;
  bool advance_to_column(T_sp column) override;
  void write_string(String_sp data, cl_index start, cl_index end) override;
  void terpri() override;
  bool fresh_line() override;
  void clear_output() override;
  void force_output() override;
  void finish_output() override;

  void write_sequence(T_sp data, cl_index start, cl_index end);

  bool output_p() const override;

  T_sp element_type() const override;
  T_sp set_element_type(T_sp type) override;
  T_sp external_format() const override;
  T_sp set_external_format(T_sp format) override;

  T_sp length() override;
  T_sp position() override;
  T_sp set_position(T_sp pos) override;
  T_sp string_length(T_sp string) override;

  T_sp output_column() const override;
  bool start_line_p() const override;
  T_sp output_line() const override;
}; // BroadcastStream class

class ConcatenatedStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, ConcatenatedStream_O, "ConcatenatedStream", AnsiStream_O);

public: // instance variables here
  T_sp _streams;

public:
  DEFAULT_CTOR_DTOR(ConcatenatedStream_O);

  static ConcatenatedStream_sp make(List_sp input_streams);
  static T_sp streams(T_sp concatenated_stream);

  T_sp close(T_sp abort) override;

  cl_index read_byte8(unsigned char* c, cl_index n) override;

  T_sp read_byte() override;

  claspCharacter read_char() override;
  void unread_char(claspCharacter c) override;
  ListenResult listen() override;
  void clear_input() override;

  bool input_p() const override;

  T_sp element_type() const override;
  T_sp set_element_type(T_sp type) override;
  T_sp external_format() const override;
  T_sp set_external_format(T_sp format) override;

  T_sp position() override;

  T_sp input_column() const override;
  T_sp input_line() const override;
}; // ConcatenatedStream class

class EchoStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, EchoStream_O, "EchoStream", AnsiStream_O);

protected:
  int _last_char;
  T_sp _input_stream;
  T_sp _output_stream;

public:
  EchoStream_O() : _last_char(EOF){};

  static EchoStream_sp make(T_sp input_stream, T_sp output_stream);
  static T_sp input_stream(T_sp echo_stream);
  static T_sp output_stream(T_sp echo_stream);

  T_sp input_stream() const { return _input_stream; }
  T_sp output_stream() const { return _output_stream; }

  T_sp close(T_sp abort) override;

  cl_index read_byte8(unsigned char* c, cl_index n) override;
  cl_index write_byte8(unsigned char* c, cl_index n) override;

  T_sp read_byte() override;
  void write_byte(T_sp c) override;

  claspCharacter read_char() override;
  void unread_char(claspCharacter c) override;
  claspCharacter peek_char() override;
  ListenResult listen() override;
  void clear_input() override;

  claspCharacter write_char(claspCharacter c) override;
  bool advance_to_column(T_sp column) override;
  void write_string(String_sp data, cl_index start, cl_index end) override;
  void terpri() override;
  bool fresh_line() override;
  void clear_output() override;
  void force_output() override;
  void finish_output() override;

  void write_sequence(T_sp data, cl_index start, cl_index end) override;

  bool input_p() const override;
  bool output_p() const override;

  T_sp element_type() const override;
  T_sp set_element_type(T_sp type) override;
  T_sp external_format() const override;
  T_sp set_external_format(T_sp format) override;

  T_sp position() override;

  T_sp output_column() const override;
  bool start_line_p() const override;
  T_sp output_line() const override;

  T_sp input_column() const override;
  T_sp input_line() const override;

  int file_descriptor(StreamDirection direction) const override;
}; // EchoStream class

class StringStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, StringStream_O, "string-stream", AnsiStream_O);

public:
  String_sp _contents;

  DEFAULT_CTOR_DTOR(StringStream_O);

  T_sp element_type() const override;
  T_sp external_format() const override;
};

class StringInputStream_O : public StringStream_O {
  LISP_CLASS(core, CorePkg, StringInputStream_O, "string-input-stream", StringStream_O);

public:
  gctools::Fixnum _input_position;
  gctools::Fixnum _input_limit;

public:
  DEFAULT_CTOR_DTOR(StringInputStream_O);

  static T_sp make(const string& str);
  static StringInputStream_sp make(String_sp string, cl_index istart, cl_index iend);

  string peer(size_t len);
  string peerFrom(size_t start, size_t len);

  claspCharacter read_char() override;
  void unread_char(claspCharacter c) override;
  claspCharacter peek_char() override;
  ListenResult listen() override;
  T_mv read_line() override;
  void clear_input() override;

  bool input_p() const override;

  T_sp position() override;
  T_sp set_position(T_sp pos) override;
}; // StringStream class

class StringOutputStream_O : public StringStream_O {
  LISP_CLASS(core, CorePkg, StringOutputStream_O, "string-output-stream", StringStream_O);

public:
  DEFAULT_CTOR_DTOR(StringOutputStream_O);

  static String_sp get_string(T_sp string_output_stream);
  static String_sp get_string_shrink(T_sp string_output_stream);

  void clear();
  String_sp get_string();

  claspCharacter write_char(claspCharacter c) override;
  void clear_output() override;
  void finish_output() override;
  void force_output() override;

  bool output_p() const override;

  T_sp position() override;
  T_sp set_position(T_sp pos) override;
};

class SynonymStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, SynonymStream_O, "synonym-stream", AnsiStream_O);

protected:
  Symbol_sp _symbol;

public:
  SynonymStream_O() : _symbol(nil<Symbol_O>()){};

  static SynonymStream_sp make(T_sp symbol);
  static Symbol_sp symbol(T_sp synonym_stream);

  virtual string __repr__() const override;

  T_sp stream() const { return _symbol->symbolValue(); }

  cl_index read_byte8(unsigned char* c, cl_index n) override;
  cl_index write_byte8(unsigned char* c, cl_index n) override;

  T_sp read_byte() override;
  void write_byte(T_sp c) override;

  claspCharacter read_char() override;
  void unread_char(claspCharacter c) override;
  claspCharacter read_char_no_hang() override;
  claspCharacter peek_char() override;
  ListenResult listen() override;
  T_mv read_line() override;
  void clear_input() override;

  claspCharacter write_char(claspCharacter c) override;
  bool advance_to_column(T_sp column) override;
  void write_string(String_sp data, cl_index start, cl_index end) override;
  void terpri() override;
  bool fresh_line() override;
  void clear_output() override;
  void force_output() override;
  void finish_output() override;

  cl_index read_sequence(T_sp data, cl_index start, cl_index n) override;
  void write_sequence(T_sp data, cl_index start, cl_index n) override;

  bool input_p() const override;
  bool output_p() const override;
  bool interactive_p() const override;

  T_sp element_type() const override;
  T_sp set_element_type(T_sp type) override;
  T_sp external_format() const override;
  T_sp set_external_format(T_sp format) override;

  T_sp length() override;
  T_sp position() override;
  T_sp set_position(T_sp pos) override;
  T_sp string_length(T_sp string) override;

  T_sp output_column() const override;
  bool start_line_p() const override;
  T_sp output_line() const override;
  void update_output_cursor(claspCharacter c) override;
  void restore_output_cursor() override;

  T_sp input_column() const override;
  T_sp input_line() const override;
  void update_input_cursor(claspCharacter c) override;
  void restore_input_cursor() override;

  T_sp pathname() const override;
  T_sp truename() const override;

  int file_descriptor(StreamDirection direction) const override;
}; // SynonymStream class

class TwoWayStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, TwoWayStream_O, "two-way-stream", AnsiStream_O);

public: // instance variables here
  T_sp _input_stream;
  T_sp _output_stream;
  bool _echo;

public:
  TwoWayStream_O() : _input_stream(nil<T_O>()), _output_stream(nil<T_O>()), _echo(false){};

  static TwoWayStream_sp make(T_sp input_stream, T_sp output_stream);
  static T_sp input_stream(T_sp two_way_stream);
  static T_sp output_stream(T_sp two_way_stream);

  T_sp input_stream() const { return _input_stream; }
  T_sp output_stream() const { return _output_stream; }
  bool echo_p() const { return _echo; }
  bool& echo_p() { return _echo; }

  T_sp close(T_sp abort);

  cl_index read_byte8(unsigned char* c, cl_index n) override;
  cl_index write_byte8(unsigned char* c, cl_index n) override;

  T_sp read_byte() override;
  void write_byte(T_sp c) override;

  claspCharacter read_char() override;
  void unread_char(claspCharacter c) override;
  claspCharacter read_char_no_hang() override;
  claspCharacter peek_char() override;
  ListenResult listen() override;
  T_mv read_line() override;
  void clear_input() override;

  claspCharacter write_char(claspCharacter c) override;
  bool advance_to_column(T_sp column) override;
  void write_string(String_sp data, cl_index start, cl_index end) override;
  void terpri() override;
  bool fresh_line() override;
  void clear_output() override;
  void force_output() override;
  void finish_output() override;

  cl_index read_sequence(T_sp data, cl_index start, cl_index n) override;
  void write_sequence(T_sp data, cl_index start, cl_index n) override;

  bool input_p() const override;
  bool output_p() const override;
  bool interactive_p() const override;

  T_sp element_type() const override;
  T_sp set_element_type(T_sp type) override;
  T_sp external_format() const override;
  T_sp set_external_format(T_sp format) override;

  T_sp position() override;

  T_sp output_column() const override;
  bool start_line_p() const override;
  T_sp output_line() const override;
  void update_output_cursor(claspCharacter c) override;
  void restore_output_cursor() override;

  T_sp input_column() const override;
  T_sp input_line() const override;
  void update_input_cursor(claspCharacter c) override;
  void restore_input_cursor() override;

  int file_descriptor(StreamDirection direction) const override;
}; // TwoWayStream class

class FileStream_O : public AnsiStream_O {
  LISP_CLASS(core, ClPkg, FileStream_O, "file-stream", AnsiStream_O);

public:
  StreamDirection _direction;
  int _byte_size;
  List_sp _byte_stack; // For unget in input streams
  T_sp _format_table;
  Fixnum _last_code[2];
  claspCharacter _eof_char;
  int _last_char;
  int _last_op;
  T_sp _external_format;
  T_sp _filename;
  T_sp _temp_filename;
  bool _created = false;
  T_sp _format;
  T_sp _element_type;

public: // Functions here
  FileStream_O()
      : _byte_size(8), _byte_stack(nil<T_O>()), _format_table(nil<T_O>()), _last_code{EOF, EOF}, _eof_char(EOF),
        _external_format(nil<T_O>()), _format(nil<Symbol_O>()){};

  virtual string __repr__() const override;
  virtual void __write__(T_sp stream) const override;
  virtual bool has_file_position() const;
  cl_index consume_byte_stack(unsigned char* c, cl_index n);
  ListenResult _fd_listen(int fd);
  void close_cleanup(T_sp abort);
  cl_index compute_char_size(claspCharacter c);
  void parse_external_format(T_sp format);

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

  bool input_p() const override;
  bool output_p() const override;

  T_sp read_byte() override;
  void write_byte(T_sp c) override;

  claspCharacter read_char_no_cursor();
  claspCharacter read_char() override;
  void unread_char(claspCharacter c) override;

  claspCharacter write_char(claspCharacter c) override;

  cl_index read_sequence(T_sp data, cl_index start, cl_index n) override;
  void write_sequence(T_sp data, cl_index start, cl_index n) override;

  T_sp element_type() const override;
  T_sp set_element_type(T_sp type) override;
  T_sp external_format() const override;
  T_sp set_external_format(T_sp format) override;

  T_sp string_length(T_sp string) override;

  T_sp pathname() const override;
  T_sp truename() const override;
}; // FileStream class

class PosixFileStream_O : public FileStream_O {
  LISP_CLASS(core, CorePkg, PosixFileStream_O, "posix-file-stream", FileStream_O);

public:
  int _file_descriptor;

public:
  PosixFileStream_O(){};

  static PosixFileStream_sp make(T_sp fname, int fd, StreamDirection smm, gctools::Fixnum byte_size = 8,
                                 int flags = CLASP_STREAM_DEFAULT_FORMAT, T_sp external_format = nil<T_O>(),
                                 T_sp tempName = nil<T_O>(), bool created = false);

  virtual bool has_file_position() const override;

  T_sp close(T_sp abort) override;

  cl_index read_byte8(unsigned char* c, cl_index n) override;
  cl_index write_byte8(unsigned char* c, cl_index n) override;

  ListenResult listen() override;
  void clear_input() override;

  void clear_output() override;
  void force_output() override;
  void finish_output() override;

  bool interactive_p() const override;

  T_sp length() override;
  T_sp position() override;
  T_sp set_position(T_sp pos) override;

  int file_descriptor(StreamDirection direction) const override;
};

class CFileStream_O : public FileStream_O {
  LISP_CLASS(core, CorePkg, CFileStream_O, "c-file-stream", FileStream_O);

public:
  FILE* _file;
  char* _buffer;

public:
  CFileStream_O() : _buffer(NULL){};

  void fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup);

  static CFileStream_sp make(T_sp fname, FILE* f, StreamDirection smm, gctools::Fixnum byte_size = 8,
                             int flags = CLASP_STREAM_DEFAULT_FORMAT, T_sp external_format = nil<T_O>(), T_sp tempName = nil<T_O>(),
                             bool created = false);

  static CFileStream_sp make(T_sp fname, int fd, StreamDirection smm, gctools::Fixnum byte_size = 8,
                             int flags = CLASP_STREAM_DEFAULT_FORMAT, T_sp external_format = nil<T_O>(), T_sp tempName = nil<T_O>(),
                             bool created = false);

  FILE* file() const { return this->_file; };

  T_sp close(T_sp abort) override;

  cl_index read_byte8(unsigned char* c, cl_index n) override;
  cl_index write_byte8(unsigned char* c, cl_index n) override;

  ListenResult _file_listen();

  ListenResult listen() override;
  void clear_input() override;

  void clear_output() override;
  void force_output() override;
  void finish_output() override;

  bool interactive_p() const override;

  T_sp length() override;
  T_sp position() override;
  T_sp set_position(T_sp pos) override;

  int file_descriptor(StreamDirection direction) const override;

  void set_buffering_mode(T_sp mode);
};

#ifdef ECL_WSOCK

class WinsockStream_O : public FileStream_O {
  LISP_CLASS(core, CorePkg, WinsockStream_O, "winsock-stream", FileStream_O);

public:
  SOCKET _socket;

public:
  WinsockStream_O(){};

  static T_sp make(T_sp fname, SOCKET socket, StreamDirection smm, gctools::Fixnum byte_size = 8,
                   int flags = CLASP_STREAM_DEFAULT_FORMAT, T_sp external_format = nil<T_O>());

  T_sp close(T_sp abort) override;

  cl_index read_byte8(unsigned char* c, cl_index n) override;
  cl_index write_byte8(unsigned char* c, cl_index n) override;

  ListenResult listen() override;
  void clear_input() override;
};

#endif

#ifdef CLASP_MS_WINDOWS_HOST

class ConsoleStream_O : public FileStream_O {
  LISP_CLASS(core, CorePkg, ConsoleStream_O, "console-stream", FileStream_O);

  HANDLE _handle;

public:
  ConsoleStream_O(){};

  static T_sp make(T_sp fname, HANDLE handle, StreamDirection smm, gctools::Fixnum byte_size = 8,
                   int flags = CLASP_STREAM_DEFAULT_FORMAT, T_sp external_format = nil<T_O>());

  cl_index read_byte8(unsigned char* c, cl_index n) override;
  cl_index write_byte8(unsigned char* c, cl_index n) override;

  ListenResult listen() override;
  void clear_input() override;

  bool interactive_p() const override;

  void force_output() override;
};

#endif

void denseReadTo8Bit(T_sp stream, size_t charCount, unsigned char* buffer);

}; // namespace core

namespace translate {

template <> struct from_object<core::StreamDirection> {
  typedef core::StreamDirection DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o == kw::_sym_input) {
      this->_v = core::StreamDirection::input;
      return;
    }
    if (o == kw::_sym_output) {
      this->_v = core::StreamDirection::output;
      return;
    }
    if (o == kw::_sym_io) {
      this->_v = core::StreamDirection::io;
      return;
    }
    if (o == kw::_sym_probe) {
      this->_v = core::StreamDirection::probe;
      return;
    }
    core::T_sp type = core::Cons_O::createList(cl::_sym_member, kw::_sym_input, kw::_sym_output, kw::_sym_io, kw::_sym_probe);
    TYPE_ERROR(o, type);
  }
};

template <> struct to_object<core::StreamDirection> {
  typedef core::StreamDirection DeclareType;
  static core::T_sp convert(DeclareType v) {
    switch (v) {
    case core::StreamDirection::input:
      return kw::_sym_input;
    case core::StreamDirection::output:
      return kw::_sym_output;
    case core::StreamDirection::io:
      return kw::_sym_io;
    case core::StreamDirection::probe:
      return kw::_sym_probe;
    }
  }
};

template <> struct from_object<core::StreamIfExists> {
  typedef core::StreamIfExists DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      this->_v = core::StreamIfExists::nil;
      return;
    }
    if (o == kw::_sym_error) {
      this->_v = core::StreamIfExists::error;
      return;
    }
    if (o == kw::_sym_new_version) {
      this->_v = core::StreamIfExists::new_version;
      return;
    }
    if (o == kw::_sym_rename) {
      this->_v = core::StreamIfExists::rename;
      return;
    }
    if (o == kw::_sym_rename_and_delete) {
      this->_v = core::StreamIfExists::rename_and_delete;
      return;
    }
    if (o == kw::_sym_overwrite) {
      this->_v = core::StreamIfExists::overwrite;
      return;
    }
    if (o == kw::_sym_append) {
      this->_v = core::StreamIfExists::append;
      return;
    }
    if (o == kw::_sym_supersede) {
      this->_v = core::StreamIfExists::supersede;
      return;
    }
    core::T_sp type = core::Cons_O::createList(cl::_sym_member, cl::_sym_nil, kw::_sym_error, kw::_sym_new_version, kw::_sym_rename,
                                               kw::_sym_rename_and_delete, kw::_sym_overwrite, kw::_sym_append, kw::_sym_supersede);
    TYPE_ERROR(o, type);
  }
};

template <> struct from_object<core::StreamIfDoesNotExist> {
  typedef core::StreamIfDoesNotExist DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      this->_v = core::StreamIfDoesNotExist::nil;
      return;
    }
    if (o == kw::_sym_error) {
      this->_v = core::StreamIfDoesNotExist::error;
      return;
    }
    if (o == kw::_sym_create) {
      this->_v = core::StreamIfDoesNotExist::create;
      return;
    }
    core::T_sp type = core::Cons_O::createList(cl::_sym_member, cl::_sym_nil, kw::_sym_error, kw::_sym_create);
    TYPE_ERROR(o, type);
  }
};

} // namespace translate

template <> struct fmt::formatter<core::StreamDirection> : fmt::formatter<int> {
  template <typename FormatContext>
  auto format(const core::StreamDirection& o, FormatContext& ctx) const -> typename FormatContext::iterator {
    return fmt::formatter<int>::format((int)o, ctx);
  }
};
