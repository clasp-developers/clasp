/*
    File: lispStream.cc
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
//#define DEBUG_CURSOR 1

/*
  Originally from ECL file.d -- File interface.

    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.

    Heavily modified by Christian Schafmeister 2014
*/

//#define DEBUG_LEVEL_FULL
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/readtable.h>
#include <clasp/core/lispDefinitions.h>
#include <clasp/core/instance.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/pathname.h>
#include <clasp/core/primitives.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispList.h>
#include <clasp/core/array.h>
#include <clasp/core/designators.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/lispReader.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/wrappers.h>
namespace core {
FileOps &StreamOps(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->ops;
}

int &StreamByteSize(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_ByteSize;
};

int &StreamFlags(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_Flags;
}

StreamMode &StreamMode(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_Mode;
}

int &StreamLastOp(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_LastOp;
}

char *&StreamBuffer(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_Buffer;
}

T_sp &StreamFormat(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_Format;
}

T_sp &StreamExternalFormat(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_ExternalFormat;
}

int &StreamClosed(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_Closed;
}

bool AnsiStreamP(T_sp strm) {
  if (Stream_sp s = strm.asOrNull<Stream_O>()) {
    (void)s;
    return true;
  }
  return false;
}
bool AnsiStreamTypeP(T_sp strm, int mode) {
  if (Stream_sp s = strm.asOrNull<Stream_O>()) {
    if (StreamMode(s) == mode) {
      return true;
    }
  }
  return false;
}

bool FileStreamP(T_sp strm) {
  return AnsiStreamP(strm) && (StreamMode(strm) < clasp_smm_synonym);
}

int &StreamLastChar(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_LastChar;
}

List_sp &StreamByteStack(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_ByteStack;
}

StreamCursor &StreamInputCursor(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_InputCursor;
}

Fixnum &StreamLastCode(T_sp strm, int index) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_LastCode[index];
}

cl_eformat_decoder &StreamDecoder(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_Decoder;
}

cl_eformat_encoder &StreamEncoder(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_Encoder;
}

claspCharacter &StreamEofChar(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_EofChar;
}

int &StreamOutputColumn(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  return stream->_OutputColumn;
}

String_sp &StringOutputStreamOutputString(T_sp strm) {
  StringOutputStream_sp sout = gc::As<StringOutputStream_sp>(strm);
  return sout->_Contents;
}


Fixnum StringFillp(String_sp s) {
  ASSERT(core__non_simple_stringp(s));
  if (!s->arrayHasFillPointerP()) {
    SIMPLE_ERROR(BF("The vector does not have a fill pointer"));
  }
  return s->fillPointer();
}

void SetStringFillp(String_sp s, Fixnum fp) {
  ASSERT(core__non_simple_stringp(s));
  if (!s->arrayHasFillPointerP()) {
    SIMPLE_ERROR(BF("The vector does not have a fill pointer"));
  }
  s->fillPointerSet(fp);
}


CL_DEFUN StringOutputStream_sp core__thread_local_write_to_string_output_stream()
{
  return my_thread->_WriteToStringOutputStream;
}

CL_DEFUN String_sp core__get_thread_local_write_to_string_output_stream_string(StringOutputStream_sp my_stream)
{
  // This is like get-string-output-stream-string but it checks the size of the
  // buffer string and if it is too large it knocks it down to 128 characters
  String_sp buffer = StringOutputStreamOutputString(my_stream);
  String_sp result = gc::As_unsafe<String_sp>(cl__copy_seq(buffer));
  if (buffer->length()>1024) {
    StringOutputStreamOutputString(my_stream) = Str8Ns_O::createBufferString(128);
  } else {
    SetStringFillp(buffer,core::make_fixnum(0));
  }
  return result;
}

gctools::Fixnum &StringInputStreamInputPosition(T_sp strm) {
  StringInputStream_sp ss = gc::As<StringInputStream_sp>(strm);
  return ss->_InputPosition;
}

gctools::Fixnum &StringInputStreamInputLimit(T_sp strm) {
  StringInputStream_sp ss = gc::As<StringInputStream_sp>(strm);
  return ss->_InputLimit;
}

String_sp &StringInputStreamInputString(T_sp strm) {
  StringInputStream_sp ss = gc::As<StringInputStream_sp>(strm);
  return ss->_Contents;
}

T_sp &TwoWayStreamInput(T_sp strm) {
  TwoWayStream_sp s = gc::As<TwoWayStream_sp>(strm);
  return s->_In;
}

T_sp &TwoWayStreamOutput(T_sp strm) {
  TwoWayStream_sp s = gc::As<TwoWayStream_sp>(strm);
  return s->_Out;
}

T_sp &BroadcastStreamList(T_sp strm) {
  BroadcastStream_sp s = gc::As<BroadcastStream_sp>(strm);
  return s->_Streams;
}

T_sp &EchoStreamInput(T_sp strm) {
  EchoStream_sp e = gc::As<EchoStream_sp>(strm);
  return e->_In;
}

T_sp &EchoStreamOutput(T_sp strm) {
  EchoStream_sp e = gc::As<EchoStream_sp>(strm);
  return e->_Out;
}

T_sp &ConcatenatedStreamList(T_sp strm) {
  ConcatenatedStream_sp c = gc::As<ConcatenatedStream_sp>(strm);
  return c->_List;
}

Symbol_sp &SynonymStreamSymbol(T_sp strm) {
  SynonymStream_sp ss = gc::As<SynonymStream_sp>(strm);
  return ss->_SynonymSymbol;
}

T_sp SynonymStreamStream(T_sp strm) {
  Symbol_sp sym = SynonymStreamSymbol(strm);
  return sym->symbolValue();
}

T_sp &FileStreamFilename(T_sp strm) {
  FileStream_sp fds = gc::As<FileStream_sp>(strm);
  return fds->_Filename;
}

T_sp &FileStreamEltType(T_sp strm) {
  FileStream_sp iofs = gc::As<FileStream_sp>(strm);
  return iofs->_ElementType;
}

int &IOFileStreamDescriptor(T_sp strm) {
  IOFileStream_sp fds = gc::As<IOFileStream_sp>(strm);
  return fds->_FileDescriptor;
}

FILE *&IOStreamStreamFile(T_sp strm) {
  IOStreamStream_sp io = gc::As<IOStreamStream_sp>(strm);
  return io->_File;
}
};

namespace core {
void StreamCursor::advanceLineNumber(T_sp strm, claspCharacter c, int num) {
  this->_PrevLineNumber = this->_LineNumber;
  this->_PrevColumn = this->_Column;
  this->_LineNumber += num;
  this->_Column = 0;
#ifdef DEBUG_CURSOR
  if (core::_sym_STARdebugMonitorSTAR->symbolValue().notnilp()) {
    printf("%s:%d stream=%s advanceLineNumber=%c/%d  ln/col=%lld/%d\n", __FILE__, __LINE__, clasp_filename(strm, false)->get().c_str(), c, c, this->_LineNumber, this->_Column);
  }
#endif
}
void StreamCursor::advanceColumn(T_sp strm, claspCharacter c, int num) {
  this->_PrevLineNumber = this->_LineNumber;
  this->_PrevColumn = this->_Column;
  this->_Column++;
#ifdef DEBUG_CURSOR
  if (core::_sym_STARdebugMonitorSTAR->symbolValue().notnilp()) {
    printf("%s:%d stream=%s advanceColumn=%c/%d  ln/col=%lld/%d\n", __FILE__, __LINE__, clasp_filename(strm, false)->get().c_str(), c, c, this->_LineNumber, this->_Column);
  }
#endif
}
void StreamCursor::backup(T_sp strm, claspCharacter c) {
  this->_LineNumber = this->_PrevLineNumber;
  this->_Column = this->_PrevColumn;
#ifdef DEBUG_CURSOR
  if (core::_sym_STARdebugMonitorSTAR->symbolValue().notnilp()) {
    printf("%s:%d stream=%s backup=%c/%d ln/col=%lld/%d\n", __FILE__, __LINE__, clasp_filename(strm, false)->get().c_str(), c, c, this->_LineNumber, this->_Column);
  }
#endif
}
};

namespace core {

/* Maximum number of bytes required to encode a character.
 * This currently corresponds to (4 + 2) for the ISO-2022-JP-* encodings
 * with 4 being the charset prefix, 2 for the character.
 */
#define ENCODING_BUFFER_MAX_SIZE 6

const FileOps &duplicate_dispatch_table(const FileOps &ops);
const FileOps &stream_dispatch_table(T_sp strm);

static int flisten(T_sp, FILE *);
static int file_listen(T_sp, int);

//    static T_sp alloc_stream();

static void cannot_close(T_sp stream) NO_RETURN;
static void file_libc_error(T_sp error_type, T_sp stream, const char *msg, int narg, ...) NO_RETURN;
static T_sp not_a_file_stream(T_sp fn) NO_RETURN;
static void not_an_input_stream(T_sp fn) NO_RETURN;
static void not_an_output_stream(T_sp fn) NO_RETURN;
static void not_a_character_stream(T_sp s) NO_RETURN;
static void not_a_binary_stream(T_sp s) NO_RETURN;
static int restartable_io_error(T_sp strm, const char *s);
static void unread_error(T_sp strm);
static void unread_twice(T_sp strm);
static void io_error(T_sp strm) NO_RETURN;
#ifdef CLASP_UNICODE
cl_index ext__encoding_error(T_sp strm, unsigned char *buffer, claspCharacter c);
claspCharacter ext__decoding_error(T_sp strm, unsigned char *buffer, int length);
#endif
static void wrong_file_handler(T_sp strm) NO_RETURN;
#if defined(ECL_WSOCK)
static void wsock_error(const char *err_msg, T_sp strm) NO_RETURN;
#endif

/**********************************************************************
 * NOT IMPLEMENTED or NOT APPLICABLE OPERATIONS
 */

static cl_index
not_output_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  not_an_output_stream(strm);
  return 0;
}

static cl_index
not_input_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  not_an_input_stream(strm);
  return 0;
}

static cl_index
not_binary_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  not_a_binary_stream(strm);
  return 0;
}

static void
not_output_write_byte(T_sp c, T_sp strm) {
  not_an_output_stream(strm);
}

static T_sp
not_input_read_byte(T_sp strm) {
  not_an_input_stream(strm);
  UNREACHABLE(); // return OBJNULL;
}

static void
not_binary_write_byte(T_sp c, T_sp strm) {
  not_a_binary_stream(strm);
}

static T_sp
not_binary_read_byte(T_sp strm) {
  not_a_binary_stream(strm);
  UNREACHABLE(); // return OBJNULL;
}

static claspCharacter
not_input_read_char(T_sp strm) {
  not_an_input_stream(strm);
  return EOF;
}

static claspCharacter
not_output_write_char(T_sp strm, claspCharacter c) {
  not_an_output_stream(strm);
  return c;
}

static void
not_input_unread_char(T_sp strm, claspCharacter c) {
  not_an_input_stream(strm);
}

static int
not_input_listen(T_sp strm) {
  not_an_input_stream(strm);
  return -1;
}

static claspCharacter
not_character_read_char(T_sp strm) {
  not_a_character_stream(strm);
  return -1;
}

static claspCharacter
not_character_write_char(T_sp strm, claspCharacter c) {
  not_a_character_stream(strm);
  return c;
}

static void
not_input_clear_input(T_sp strm) {
  not_an_input_stream(strm);
  return;
}

static void
not_output_clear_output(T_sp strm) {
  not_an_output_stream(strm);
}

static void
not_output_force_output(T_sp strm) {
  not_an_output_stream(strm);
}

static void
not_output_finish_output(T_sp strm) {
  not_an_output_stream(strm);
}

#if defined(ECL_WSOCK)
static T_sp
not_implemented_get_position(T_sp strm) {
  FEerror("file-position not implemented for stream ~S", 1, strm.raw_());
  return _Nil<T_O>();
}

static T_sp
not_implemented_set_position(T_sp strm, T_sp pos) {
  FEerror("file-position not implemented for stream ~S", 1, strm.raw_());
  return _Nil<T_O>();
}
#endif

/**********************************************************************
 * CLOSED STREAM OPS
 */

static cl_index
closed_stream_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  CLOSED_STREAM_ERROR(strm);
  return 0;
}

static cl_index
closed_stream_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  CLOSED_STREAM_ERROR(strm);
  return 0;
}

static claspCharacter
closed_stream_read_char(T_sp strm) {
  CLOSED_STREAM_ERROR(strm);
  return 0;
}

static claspCharacter
closed_stream_write_char(T_sp strm, claspCharacter c) {
  CLOSED_STREAM_ERROR(strm);
  return c;
}

static void
closed_stream_unread_char(T_sp strm, claspCharacter c) {
  CLOSED_STREAM_ERROR(strm);
}

static int
closed_stream_listen(T_sp strm) {
  CLOSED_STREAM_ERROR(strm);
  return 0;
}

static void
closed_stream_clear_input(T_sp strm) {
  CLOSED_STREAM_ERROR(strm);
}

#define closed_stream_clear_output closed_stream_clear_input
#define closed_stream_force_output closed_stream_clear_input
#define closed_stream_finish_output closed_stream_clear_input

static T_sp
closed_stream_length(T_sp strm) {
  CLOSED_STREAM_ERROR(strm);
}

#define closed_stream_get_position closed_stream_length

static T_sp
closed_stream_set_position(T_sp strm, T_sp position) {
  CLOSED_STREAM_ERROR(strm);
}

/**********************************************************************
 * GENERIC OPERATIONS
 *
 * Versions of the methods which are defined in terms of others
 */
/*
 * Byte operations based on octet operators.
 */

static T_sp
generic_read_byte_unsigned8(T_sp strm) {
  unsigned char c;
  if (StreamOps(strm).read_byte8(strm, &c, 1) < 1) {
    return _Nil<T_O>();
  }
  return make_fixnum(c);
}

static void
generic_write_byte_unsigned8(T_sp byte, T_sp strm) {
  unsigned char c = clasp_toUint8(byte);
  StreamOps(strm).write_byte8(strm, &c, 1);
}

static T_sp
generic_read_byte_signed8(T_sp strm) {
  signed char c;
  if (StreamOps(strm).read_byte8(strm, (unsigned char *)&c, 1) < 1)
    return _Nil<T_O>();
  return make_fixnum(c);
}

static void
generic_write_byte_signed8(T_sp byte, T_sp strm) {
  signed char c = clasp_toSignedInt8(byte);
  StreamOps(strm).write_byte8(strm, (unsigned char *)&c, 1);
}

static T_sp
generic_read_byte_le(T_sp strm) {
  cl_index (*read_byte8)(T_sp, unsigned char *, cl_index);
  unsigned char c;
  cl_index nb, bs;
  T_sp output = make_fixnum(0);
  read_byte8 = StreamOps(strm).read_byte8;
  bs = StreamByteSize(strm);
  for (nb = 0; bs >= 8; bs -= 8, nb += 8) {
    Fixnum_sp aux;
    if (read_byte8(strm, &c, 1) < 1)
      return _Nil<T_O>();
    if (bs <= 8 && (StreamFlags(strm) & CLASP_STREAM_SIGNED_BYTES))
      aux = make_fixnum((signed char)c);
    else
      aux = make_fixnum((unsigned char)c);
    output = cl__logior(Cons_O::createList(output, clasp_ash(aux, nb)));
  }
  return output;
}

static void
generic_write_byte_le(T_sp c, T_sp strm) {
  cl_index (*write_byte8)(T_sp strm, unsigned char *c, cl_index n);
  cl_index bs;
  write_byte8 = StreamOps(strm).write_byte8;
  bs = StreamByteSize(strm);
  do {
    T_sp b = cl__logand(Cons_O::createList(c, make_fixnum(0xFF)));
    ASSERT(b.fixnump());
    unsigned char aux = (unsigned char)(unbox_fixnum(b));
    if (write_byte8(strm, &aux, 1) < 1)
      break;
    c = clasp_ash(gc::As<Integer_sp>(c), -8);
    bs -= 8;
  } while (bs);
}

static T_sp
generic_read_byte(T_sp strm) {
  cl_index (*read_byte8)(T_sp, unsigned char *, cl_index);
  unsigned char c;
  T_sp output;
  cl_index bs;
  read_byte8 = StreamOps(strm).read_byte8;
  bs = StreamByteSize(strm);
  for (; bs >= 8; bs -= 8) {
    if (read_byte8(strm, &c, 1) < 1)
      return _Nil<T_O>();
    if (output.notnilp()) {
      output = cl__logior(Cons_O::createList(make_fixnum(c),
                                             clasp_ash(gc::As<Integer_sp>(output), 8)));
    } else if (StreamFlags(strm) & CLASP_STREAM_SIGNED_BYTES) {
      output = make_fixnum((signed char)c);
    } else {
      output = make_fixnum((unsigned char)c);
    }
  }
  return output;
}

static void
generic_write_byte(T_sp c, T_sp strm) {
  cl_index (*write_byte8)(T_sp strm, unsigned char *c, cl_index n);
  cl_index bs;
  write_byte8 = StreamOps(strm).write_byte8;
  bs = StreamByteSize(strm);
  do {
    unsigned char aux;
    T_sp b;
    bs -= 8;
    b = cl__logand(Cons_O::createList(make_fixnum(0xFF), bs ? gc::As<T_sp>(clasp_ash(gc::As<Integer_sp>(c), -bs)) : c));
    aux = (unsigned char)b.unsafe_fixnum();
    if (write_byte8(strm, &aux, 1) < 1)
      break;
  } while (bs);
}

static claspCharacter
generic_peek_char(T_sp strm) {
  claspCharacter out = clasp_read_char(strm);
  if (out != EOF)
    clasp_unread_char(out, strm);
  return out;
}

static void
generic_void(T_sp strm) {
}

static int
generic_always_true(T_sp strm) {
  return 1;
}

static int
generic_always_false(T_sp strm) {
  return 0;
}

static T_sp
generic_always_nil(T_sp strm) {
  return _Nil<T_O>();
}

static int
generic_column(T_sp strm) {
  return 0;
}

static T_sp
generic_set_position(T_sp strm, T_sp pos) {
  return _Nil<T_O>();
}

static T_sp
generic_close(T_sp strm) {
  struct FileOps &ops = StreamOps(strm);
  if (clasp_input_stream_p(strm)) {
    ops.read_byte8 = closed_stream_read_byte8;
    ops.read_char = closed_stream_read_char;
    ops.unread_char = closed_stream_unread_char;
    ops.listen = closed_stream_listen;
    ops.clear_input = closed_stream_clear_input;
  }
  if (clasp_output_stream_p(strm)) {
    ops.write_byte8 = closed_stream_write_byte8;
    ops.write_char = closed_stream_write_char;
    ops.clear_output = closed_stream_clear_output;
    ops.force_output = closed_stream_force_output;
    ops.finish_output = closed_stream_finish_output;
  }
  ops.get_position = closed_stream_get_position;
  ops.set_position = closed_stream_set_position;
  ops.length = closed_stream_length;
  ops.close = generic_close;
  StreamClosed(strm) = 1;
  return _lisp->_true();
}

static cl_index
generic_write_vector(T_sp strm, T_sp data, cl_index start, cl_index end) {
  if (start >= end)
    return start;
  const FileOps &ops = stream_dispatch_table(strm);
  Vector_sp vec = gc::As<Vector_sp>(data);
  T_sp elementType = vec->arrayElementType();
  if (elementType == cl::_sym_base_char ||
#ifdef CLASP_UNICODE
      elementType == cl::_sym_character ||
#endif
      (elementType == cl::_sym_T && cl__characterp(vec->rowMajorAref(0)))) {
    claspCharacter (*write_char)(T_sp, claspCharacter) = ops.write_char;
    for (; start < end; start++) {
      write_char(strm, clasp_as_claspCharacter(gc::As<Character_sp>((vec->rowMajorAref(start)))));
    }
  } else {
    void (*write_byte)(T_sp, T_sp) = ops.write_byte;
    for (; start < end; start++) {
      write_byte(vec->rowMajorAref(start), strm);
    }
  }

  clasp_elttype elttype = clasp_array_elttype(data);
  if (elttype == clasp_aet_bc ||
#ifdef CLASP_UNICODE
      elttype == clasp_aet_ch ||
#endif
      (elttype == clasp_aet_object && cl__elt(data, 0).characterp())) {
    claspCharacter (*write_char)(T_sp, claspCharacter) = ops.write_char;
    for (; start < end; start++) {
      write_char(strm, clasp_as_claspCharacter(gc::As<Character_sp>(cl__elt(data, start))));
    }
  } else {
    void (*write_byte)(T_sp, T_sp) = ops.write_byte;
    for (; start < end; start++) {
      write_byte(cl__elt(data, start), strm);
    }
  }
  return start;
}

static cl_index
generic_read_vector(T_sp strm, T_sp data, cl_index start, cl_index end) {
  if (start >= end)
    return start;
  Vector_sp vec = gc::As<Vector_sp>(data);
  const FileOps &ops = stream_dispatch_table(strm);
  T_sp expected_type = clasp_stream_element_type(strm);
  if (expected_type == cl::_sym_base_char || expected_type == cl::_sym_character) {
    claspCharacter (*read_char)(T_sp) = ops.read_char;
    for (; start < end; start++) {
      claspCharacter c = read_char(strm);
      if (c == EOF) break;
      vec->rowMajorAset(start, clasp_make_character(c));
    }
  } else {
    T_sp (*read_byte)(T_sp) = ops.read_byte;
    for (; start < end; start++) {
      T_sp x = read_byte(strm);
      if (x.nilp())
        break;
      vec->rowMajorAset(start, x);
    }
  }
  return start;
}

/**********************************************************************
 * CHARACTER AND EXTERNAL FORMAT SUPPORT
 */

static void
eformat_unread_char(T_sp strm, claspCharacter c) {
  unlikely_if(c != StreamLastChar(strm)) {
    unread_twice(strm);
  }
  {
    unsigned char buffer[2 * ENCODING_BUFFER_MAX_SIZE];
    int ndx = 0;
    T_sp l = StreamByteStack(strm);
    gctools::Fixnum i = StreamLastCode(strm, 0);
    if (i != EOF) {
      ndx += StreamEncoder(strm)(strm, buffer, i);
    }
    i = StreamLastCode(strm, 1);
    if (i != EOF) {
      ndx += StreamEncoder(strm)(strm, buffer + ndx, i);
    }
    while (ndx != 0) {
      l = Cons_O::create(make_fixnum(buffer[--ndx]), l);
    }
    StreamByteStack(strm) = gc::As<Cons_sp>(l);
    StreamLastChar(strm) = EOF;
    StreamInputCursor(strm).backup(strm, c);
  }
}

static claspCharacter
eformat_read_char_no_cursor(T_sp strm) {
  claspCharacter c = StreamDecoder(strm)(strm);
  unlikely_if(c == StreamEofChar(strm)) return EOF;
  if (c != EOF) {
    StreamLastChar(strm) = c;
    StreamLastCode(strm, 0) = c;
    StreamLastCode(strm, 1) = EOF;
  }
  return c;
}

static claspCharacter
eformat_read_char(T_sp strm) {
  claspCharacter c = StreamDecoder(strm)(strm);
  unlikely_if(c == StreamEofChar(strm)) return EOF;
  if (c != EOF) {
    StreamLastChar(strm) = c;
    StreamLastCode(strm, 0) = c;
    StreamLastCode(strm, 1) = EOF;
  }
  StreamInputCursor(strm).advanceForChar(strm, c, StreamLastChar(strm));
  return c;
}

static claspCharacter
eformat_write_char(T_sp strm, claspCharacter c) {
  unsigned char buffer[ENCODING_BUFFER_MAX_SIZE];
  claspCharacter nbytes;
  nbytes = StreamEncoder(strm)(strm, buffer, c);
  StreamOps(strm).write_byte8(strm, buffer, nbytes);
  if (c == '\n')
    StreamOutputColumn(strm) = 0;
  else if (c == '\t')
    StreamOutputColumn(strm) = (StreamOutputColumn(strm) & ~((cl_index)07)) + 8;
  else
    StreamOutputColumn(strm)++;
  fflush(stdout);
  return c;
}

static claspCharacter
eformat_read_char_cr(T_sp strm) {
  claspCharacter c = eformat_read_char_no_cursor(strm);
  if (c == CLASP_CHAR_CODE_RETURN) {
    c = CLASP_CHAR_CODE_NEWLINE;
    StreamLastChar(strm) = c;
    StreamInputCursor(strm).advanceLineNumber(strm, c);
  } else {
    StreamInputCursor(strm).advanceColumn(strm, c);
  }
  return c;
}

static claspCharacter
eformat_read_char_crlf(T_sp strm) {
  claspCharacter c = eformat_read_char_no_cursor(strm);
  if (c == CLASP_CHAR_CODE_RETURN) {
    c = eformat_read_char_no_cursor(strm);
    if (c == CLASP_CHAR_CODE_LINEFEED) {
      StreamLastCode(strm, 0) = CLASP_CHAR_CODE_RETURN;
      StreamLastCode(strm, 1) = c;
      c = CLASP_CHAR_CODE_NEWLINE;
    } else {
      eformat_unread_char(strm, c);
      c = CLASP_CHAR_CODE_RETURN;
      StreamLastCode(strm, 0) = c;
      StreamLastCode(strm, 1) = EOF;
    }
    StreamLastChar(strm) = c;
    StreamInputCursor(strm).advanceLineNumber(strm, c);
  } else {
    StreamInputCursor(strm).advanceColumn(strm, c);
  }
  return c;
}

static claspCharacter
eformat_write_char_cr(T_sp strm, claspCharacter c) {
  if (c == CLASP_CHAR_CODE_NEWLINE) {
    eformat_write_char(strm, CLASP_CHAR_CODE_RETURN);
    StreamOutputColumn(strm) = 0;
    return c;
  }
  return eformat_write_char(strm, c);
}

static claspCharacter
eformat_write_char_crlf(T_sp strm, claspCharacter c) {
  if (c == CLASP_CHAR_CODE_NEWLINE) {
    eformat_write_char(strm, CLASP_CHAR_CODE_RETURN);
    eformat_write_char(strm, CLASP_CHAR_CODE_LINEFEED);
    StreamOutputColumn(strm) = 0;
    return c;
  }
  return eformat_write_char(strm, c);
}

/*
 * If we use Unicode, this is LATIN-1, ISO-8859-1, that is the 256
 * lowest codes of Unicode. Otherwise, we simply assume the file and
 * the strings use the same format.
 */

static claspCharacter
passthrough_decoder(T_sp stream) {
  unsigned char aux;
  if (clasp_read_byte8(stream, &aux, 1) < 1)
    return EOF;
  else
    return aux;
}

static int
passthrough_encoder(T_sp stream, unsigned char *buffer, claspCharacter c) {
#ifdef CLASP_UNICODE
  unlikely_if(c > 0xFF) {
    return ext__encoding_error(stream, buffer, c);
  }
#endif
  buffer[0] = c;
  return 1;
}

#ifdef CLASP_UNICODE
/*
 * US ASCII, that is the 128 (0-127) lowest codes of Unicode
 */

static claspCharacter
ascii_decoder(T_sp stream) {
  unsigned char aux;
  if (clasp_read_byte8(stream, &aux, 1) < 1) {
    return EOF;
  } else if (aux > 127) {
    return ext__decoding_error(stream, &aux, 1);
  } else {
    return aux;
  }
}

static int
ascii_encoder(T_sp stream, unsigned char *buffer, claspCharacter c) {
  unlikely_if(c > 127) {
    return ext__encoding_error(stream, buffer, c);
  }
  buffer[0] = c;
  return 1;
}

/*
 * UCS-4 BIG ENDIAN
 */

static claspCharacter
ucs_4be_decoder(T_sp stream) {
  unsigned char buffer[4];
  if (clasp_read_byte8(stream, buffer, 4) < 4) {
    return EOF;
  } else {
    return buffer[3] + (buffer[2] << 8) + (buffer[1] << 16) + (buffer[0] << 24);
  }
}

static int
ucs_4be_encoder(T_sp stream, unsigned char *buffer, claspCharacter c) {
  buffer[3] = c & 0xFF;
  c >>= 8;
  buffer[2] = c & 0xFF;
  c >>= 8;
  buffer[1] = c & 0xFF;
  c >>= 8;
  buffer[0] = c;
  return 4;
}

/*
 * UCS-4 LITTLE ENDIAN
 */

static claspCharacter
ucs_4le_decoder(T_sp stream) {
  unsigned char buffer[4];
  if (clasp_read_byte8(stream, buffer, 4) < 4) {
    return EOF;
  } else {
    return buffer[0] + (buffer[1] << 8) + (buffer[2] << 16) + (buffer[3] << 24);
  }
}

static int
ucs_4le_encoder(T_sp stream, unsigned char *buffer, claspCharacter c) {
  buffer[0] = c & 0xFF;
  c >>= 8;
  buffer[1] = c & 0xFF;
  c >>= 8;
  buffer[2] = c & 0xFF;
  c >>= 8;
  buffer[3] = c;
  return 4;
}

/*
 * UCS-4 BOM ENDIAN
 */

static claspCharacter
ucs_4_decoder(T_sp strm) {
  Stream_sp stream = gc::As<Stream_sp>(strm);
  gctools::Fixnum c = ucs_4be_decoder(stream);
  if (c == 0xFEFF) {
    StreamDecoder(stream) = ucs_4be_decoder;
    stream->_Encoder = ucs_4be_encoder;
    return ucs_4be_decoder(stream);
  } else if (c == 0xFFFE0000) {
    StreamDecoder(stream) = ucs_4le_decoder;
    stream->_Encoder = ucs_4le_encoder;
    return ucs_4le_decoder(stream);
  } else {
    StreamDecoder(stream) = ucs_4be_decoder;
    stream->_Encoder = ucs_4be_encoder;
    return c;
  }
}

static int
ucs_4_encoder(T_sp tstream, unsigned char *buffer, claspCharacter c) {
  Stream_sp stream = gc::As<Stream_sp>(tstream);
  StreamDecoder(stream) = ucs_4be_decoder;
  stream->_Encoder = ucs_4be_encoder;
  buffer[0] = 0xFF;
  buffer[1] = 0xFE;
  buffer[2] = buffer[3] = 0;
  return 4 + ucs_4be_encoder(stream, buffer + 4, c);
}

/*
 * UTF-16 BIG ENDIAN
 */

static claspCharacter
ucs_2be_decoder(T_sp stream) {
  unsigned char buffer[2] = {0, 0};
  if (clasp_read_byte8(stream, buffer, 2) < 2) {
    return EOF;
  } else {
    claspCharacter c = ((claspCharacter)buffer[0] << 8) | buffer[1];
    if ((buffer[0] & 0xFC) == 0xD8) {
      if (clasp_read_byte8(stream, buffer, 2) < 2) {
        return EOF;
      } else {
        claspCharacter aux = ((claspCharacter)buffer[0] << 8) | buffer[1];
        if ((buffer[0] & 0xF8) != 0xDC) {
          return ext__decoding_error(stream, buffer, 1);
        }
        return ((c & 0x3FFF) << 10) + (aux & 0x3FFF) + 0x10000;
      }
    }
    return c;
  }
}

static int
ucs_2be_encoder(T_sp stream, unsigned char *buffer, claspCharacter c) {
  if (c >= 0x10000) {
    c -= 0x10000;
    ucs_2be_encoder(stream, buffer, (c >> 10) | 0xD800);
    ucs_2be_encoder(stream, buffer + 2, (c & 0x3FFF) | 0xDC00);
    return 4;
  } else {
    buffer[1] = c & 0xFF;
    c >>= 8;
    buffer[0] = c;
    return 2;
  }
}

/*
 * UTF-16 LITTLE ENDIAN
 */

static claspCharacter
ucs_2le_decoder(T_sp stream) {
  unsigned char buffer[2];
  if (clasp_read_byte8(stream, buffer, 2) < 2) {
    return EOF;
  } else {
    claspCharacter c = ((claspCharacter)buffer[1] << 8) | buffer[0];
    if ((buffer[1] & 0xFC) == 0xD8) {
      if (clasp_read_byte8(stream, buffer, 2) < 2) {
        return EOF;
      } else {
        claspCharacter aux = ((claspCharacter)buffer[1] << 8) | buffer[0];
        if ((buffer[1] & 0xF8) != 0xDC) {
          return ext__decoding_error(stream, buffer, 2);
        }
        return ((c & 0x3FFF) << 10) + (aux & 0x3FFF) + 0x10000;
      }
    }
    return c;
  }
}

static int
ucs_2le_encoder(T_sp stream, unsigned char *buffer, claspCharacter c) {
  if (c >= 0x10000) {
    c -= 0x10000;
    ucs_2le_encoder(stream, buffer, (c >> 10) | 0xD8000);
    ucs_2le_encoder(stream, buffer + 2, (c & 0x3FFF) | 0xD800);
    return 4;
  } else {
    buffer[0] = c & 0xFF;
    c >>= 8;
    buffer[1] = c & 0xFF;
    return 2;
  }
}

/*
 * UTF-16 BOM ENDIAN
 */

static claspCharacter
ucs_2_decoder(T_sp tstream) {
  Stream_sp stream = gc::As<Stream_sp>(tstream);
  claspCharacter c = ucs_2be_decoder(stream);
  if (c == 0xFEFF) {
    StreamDecoder(stream) = ucs_2be_decoder;
    stream->_Encoder = ucs_2be_encoder;
    return ucs_2be_decoder(stream);
  } else if (c == 0xFFFE) {
    StreamDecoder(stream) = ucs_2le_decoder;
    stream->_Encoder = ucs_2le_encoder;
    return ucs_2le_decoder(stream);
  } else {
    StreamDecoder(stream) = ucs_2be_decoder;
    stream->_Encoder = ucs_2be_encoder;
    return c;
  }
}

static int
ucs_2_encoder(T_sp tstream, unsigned char *buffer, claspCharacter c) {
  Stream_sp stream = gc::As<Stream_sp>(tstream);
  StreamDecoder(stream) = ucs_2be_decoder;
  stream->_Encoder = ucs_2be_encoder;
  buffer[0] = 0xFF;
  buffer[1] = 0xFE;
  return 2 + ucs_2be_encoder(stream, buffer + 2, c);
}

/*
 * USER DEFINED ENCODINGS. SIMPLE CASE.
 */

static claspCharacter
user_decoder(T_sp tstream) {
  Stream_sp stream = gc::As<Stream_sp>(tstream);
  T_sp table = stream->_FormatTable;
  T_sp character;
  unsigned char buffer[2];
  if (clasp_read_byte8(stream, buffer, 1) < 1) {
    return EOF;
  }
  character = clasp_gethash_safe(clasp_make_fixnum(buffer[0]), table, _Nil<T_O>());
  unlikely_if(character.nilp()) {
    return ext__decoding_error(stream, buffer, 1);
  }
  if (character == _lisp->_true()) {
    if (clasp_read_byte8(stream, buffer + 1, 1) < 1) {
      return EOF;
    } else {
      gctools::Fixnum byte = (buffer[0] << 8) + buffer[1];
      character = clasp_gethash_safe(clasp_make_fixnum(byte), table, _Nil<T_O>());
      unlikely_if(character.nilp()) {
        return ext__decoding_error(stream, buffer, 2);
      }
    }
  }
  return character.unsafe_character();
}

static int
user_encoder(T_sp tstream, unsigned char *buffer, claspCharacter c) {
  Stream_sp stream = gc::As<Stream_sp>(tstream);
  T_sp byte = clasp_gethash_safe(clasp_make_character(c), stream->_FormatTable, _Nil<T_O>());
  if (byte.nilp()) {
    return ext__encoding_error(stream, buffer, c);
  } else {
    gctools::Fixnum code = byte.unsafe_fixnum();
    if (code > 0xFF) {
      buffer[1] = code & 0xFF;
      code >>= 8;
      buffer[0] = code;
      return 2;
    } else {
      buffer[0] = code;
      return 1;
    }
  }
}

/*
 * USER DEFINED ENCODINGS. SIMPLE CASE.
 */

static claspCharacter
user_multistate_decoder(T_sp tstream) {
  Stream_sp stream = gc::As<Stream_sp>(tstream);
  T_sp table_list = stream->_FormatTable;
  T_sp table = oCar(table_list);
  T_sp character;
  gctools::Fixnum i, j;
  unsigned char buffer[ENCODING_BUFFER_MAX_SIZE];
  for (i = j = 0; i < ENCODING_BUFFER_MAX_SIZE; i++) {
    if (clasp_read_byte8(stream, buffer + i, 1) < 1) {
      return EOF;
    }
    j = (j << 8) | buffer[i];
    character = clasp_gethash_safe(clasp_make_fixnum(j), table, _Nil<T_O>());
    if (character.characterp()) {
      return character.unsafe_character();
    }
    unlikely_if(character.nilp()) {
      return ext__decoding_error(stream, buffer, i);
    }
    if (character == _lisp->_true()) {
      /* Need more characters */
      continue;
    }
    if (character.consp()) {
      /* Changed the state. */
      stream->_FormatTable = table_list = character;
      table = oCar(table_list);
      i = j = 0;
      continue;
    }
    break;
  }
  FEerror("Internal error in decoder table.", 0);
  UNREACHABLE();
}

static int
user_multistate_encoder(T_sp tstream, unsigned char *buffer, claspCharacter c) {
  Stream_sp stream = gc::As<Stream_sp>(tstream);
  T_sp table_list = stream->_FormatTable;
  T_sp p = table_list;
  do {
    T_sp table = oCar(p);
    T_sp byte = clasp_gethash_safe(clasp_make_character(c), table, _Nil<T_O>());
    if (!byte.nilp()) {
      gctools::Fixnum code = byte.unsafe_fixnum();
      claspCharacter n = 0;
      if (p != table_list) {
        /* Must output a escape sequence */
        T_sp x = clasp_gethash_safe(_lisp->_true(), table, _Nil<T_O>());
        while (!x.nilp()) {
          buffer[0] = (oCar(x)).unsafe_fixnum();
          buffer++;
          x = oCdr(x);
          n++;
        }
        stream->_FormatTable = p;
      }
      if (code > 0xFF) {
        buffer[1] = code & 0xFF;
        code >>= 8;
        buffer[0] = code;
        return n + 2;
      } else {
        buffer[0] = code;
        return n + 1;
      }
    }
    p = oCdr(p);
  } while (p != table_list);
  /* Exhausted all lists */
  return ext__encoding_error(stream, buffer, c);
}

/*
 * UTF-8
 */

static claspCharacter
utf_8_decoder(T_sp stream) {
  /* In understanding this code:
	 * 0x8 = 1000, 0xC = 1100, 0xE = 1110, 0xF = 1111
	 * 0x1 = 0001, 0x3 = 0011, 0x7 = 0111, 0xF = 1111
	 */
  claspCharacter cum = 0;
  unsigned char buffer[5];
  int nbytes, i;
  if (clasp_read_byte8(stream, buffer, 1) < 1)
    return EOF;
  if ((buffer[0] & 0x80) == 0) {
    return buffer[0];
  }
  unlikely_if((buffer[0] & 0x40) == 0) return ext__decoding_error(stream, buffer, 1);
  if ((buffer[0] & 0x20) == 0) {
    cum = buffer[0] & 0x1F;
    nbytes = 1;
  } else if ((buffer[0] & 0x10) == 0) {
    cum = buffer[0] & 0x0F;
    nbytes = 2;
  } else if ((buffer[0] & 0x08) == 0) {
    cum = buffer[0] & 0x07;
    nbytes = 3;
  } else {
    return ext__decoding_error(stream, buffer, 1);
  }
  if (clasp_read_byte8(stream, buffer + 1, nbytes) < nbytes)
    return EOF;
  for (i = 1; i <= nbytes; i++) {
    unsigned char c = buffer[i];
    /*printf(": %04x :", c);*/
    unlikely_if((c & 0xC0) != 0x80) return ext__decoding_error(stream, buffer, nbytes + 1);
    cum = (cum << 6) | (c & 0x3F);
    unlikely_if(cum == 0) return ext__decoding_error(stream, buffer, nbytes + 1);
  }
  if (cum >= 0xd800) {
    unlikely_if(cum <= 0xdfff) return ext__decoding_error(stream, buffer, nbytes + 1);
    unlikely_if(cum >= 0xFFFE && cum <= 0xFFFF) return ext__decoding_error(stream, buffer, nbytes + 1);
  }
  /*printf("; %04x ;", cum);*/
  return cum;
}

static int
utf_8_encoder(T_sp stream, unsigned char *buffer, claspCharacter c) {
  int nbytes;
  if (c < 0) {
    nbytes = 0;
  } else if (c <= 0x7F) {
    buffer[0] = c;
    nbytes = 1;
  } else if (c <= 0x7ff) {
    buffer[1] = (c & 0x3f) | 0x80;
    c >>= 6;
    buffer[0] = c | 0xC0;
    /*printf("\n; %04x ;: %04x :: %04x :\n", c_orig, buffer[0], buffer[1]);*/
    nbytes = 2;
  } else if (c <= 0xFFFF) {
    buffer[2] = (c & 0x3f) | 0x80;
    c >>= 6;
    buffer[1] = (c & 0x3f) | 0x80;
    c >>= 6;
    buffer[0] = c | 0xE0;
    nbytes = 3;
  } else if (c <= 0x1FFFFFL) {
    buffer[3] = (c & 0x3f) | 0x80;
    c >>= 6;
    buffer[2] = (c & 0x3f) | 0x80;
    c >>= 6;
    buffer[1] = (c & 0x3f) | 0x80;
    c >>= 6;
    buffer[0] = c | 0xF0;
    nbytes = 4;
  }
  return nbytes;
}
#endif

/********************************************************************************
 * CLOS STREAMS
 */

static cl_index
clos_stream_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  cl_index i;
  for (i = 0; i < n; i++) {
    T_sp byte = eval::funcall(gray::_sym_stream_read_byte, strm);
    if (!core__fixnump(byte))
      break;
    c[i] = (byte).unsafe_fixnum();
  }
  return i;
}

static cl_index
clos_stream_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  cl_index i;
  for (i = 0; i < n; i++) {
    T_sp byte = eval::funcall(gray::_sym_stream_write_byte, strm,
                              make_fixnum(c[i]));
    if (!core__fixnump(byte))
      break;
  }
  return i;
}

static T_sp
clos_stream_read_byte(T_sp strm) {
  T_sp b = eval::funcall(gray::_sym_stream_read_byte, strm);
  if (b == kw::_sym_eof)
    b = _Nil<T_O>();
  return b;
}

static void
clos_stream_write_byte(T_sp c, T_sp strm) {
  eval::funcall(gray::_sym_stream_write_byte, strm, c);
}

static claspCharacter
clos_stream_read_char(T_sp strm) {
  T_sp output = eval::funcall(gray::_sym_stream_read_char, strm);
  gctools::Fixnum value;
  if (cl__characterp(output))
    value = output.unsafe_character();
  else if (core__fixnump(output))
    value = (output).unsafe_fixnum();
  else if (output == _Nil<T_O>() || output == kw::_sym_eof)
    return EOF;
  else
    value = -1;
  unlikely_if(value < 0 || value > CHAR_CODE_LIMIT)
      FEerror("Unknown character ~A", 1, output.raw_());
  return value;
}

static claspCharacter
clos_stream_write_char(T_sp strm, claspCharacter c) {
  eval::funcall(gray::_sym_stream_write_char, strm, clasp_make_character(c));
  return c;
}

static void
clos_stream_unread_char(T_sp strm, claspCharacter c) {
  eval::funcall(gray::_sym_stream_unread_char, strm, clasp_make_character(c));
}

static claspCharacter
clos_stream_peek_char(T_sp strm) {
  T_sp out = eval::funcall(gray::_sym_stream_peek_char, strm);
  if (out == kw::_sym_eof)
    return EOF;
  return clasp_as_claspCharacter(gc::As<Character_sp>(out));
}

static int
clos_stream_listen(T_sp strm) {
  return !(T_sp(eval::funcall(gray::_sym_stream_listen, strm))).nilp();
}

static void
clos_stream_clear_input(T_sp strm) {
  eval::funcall(gray::_sym_stream_clear_input, strm);
}

static void
clos_stream_clear_output(T_sp strm) {
  eval::funcall(gray::_sym_stream_clear_output, strm);
  return;
}

static void
clos_stream_force_output(T_sp strm) {
  eval::funcall(gray::_sym_stream_force_output, strm);
}

static void
clos_stream_finish_output(T_sp strm) {
  eval::funcall(gray::_sym_stream_finish_output, strm);
}

static int
clos_stream_input_p(T_sp strm) {
  return !T_sp(eval::funcall(gray::_sym_input_stream_p, strm)).nilp();
}

static int
clos_stream_output_p(T_sp strm) {
  return !T_sp(eval::funcall(gray::_sym_output_stream_p, strm)).nilp();
}

static int
clos_stream_interactive_p(T_sp strm) {
  return !T_sp(eval::funcall(gray::_sym_stream_interactive_p, strm)).nilp();
}

static T_sp
clos_stream_element_type(T_sp strm) {
  return eval::funcall(gray::_sym_stream_element_type, strm);
}

#define clos_stream_length not_a_file_stream

static T_sp
clos_stream_get_position(T_sp strm) {
  return eval::funcall(gray::_sym_stream_file_position, strm);
}

static T_sp
clos_stream_set_position(T_sp strm, T_sp pos) {
  return eval::funcall(gray::_sym_stream_file_position, strm, pos);
}

static int
clos_stream_column(T_sp strm) {
  T_sp col = eval::funcall(gray::_sym_stream_line_column, strm);
  /* FIXME! The Gray streams specifies NIL is a valid
	 * value but means "unknown". Should we make it
	 * zero? */
  return col.nilp() ? 0 : clasp_toSize(col);
}

static T_sp
clos_stream_close(T_sp strm) {
  return eval::funcall(gray::_sym_close, strm);
}

const FileOps clos_stream_ops = {
    clos_stream_write_byte8,
    clos_stream_read_byte8,

    clos_stream_write_byte,
    clos_stream_read_byte,

    clos_stream_read_char,
    clos_stream_write_char,
    clos_stream_unread_char,
    clos_stream_peek_char,

    generic_read_vector,
    generic_write_vector,

    clos_stream_listen,
    clos_stream_clear_input,
    clos_stream_clear_output,
    clos_stream_finish_output,
    clos_stream_force_output,

    clos_stream_input_p,
    clos_stream_output_p,
    clos_stream_interactive_p,
    clos_stream_element_type,

    clos_stream_length,
    clos_stream_get_position,
    clos_stream_set_position,
    clos_stream_column,
    clos_stream_close};

/**********************************************************************
 * STRING OUTPUT STREAMS
 */

static claspCharacter
str_out_write_char(T_sp strm, claspCharacter c) {
  //  StringOutputStream_sp sout = gc::As<StringOutputStream_sp>(strm);
  int column = StreamOutputColumn(strm);
  if (c == '\n')
    StreamOutputColumn(strm) = 0;
  else if (c == '\t')
    StreamOutputColumn(strm) = (column & ~(cl_index)7) + 8;
  else
    StreamOutputColumn(strm)++;
  StringOutputStreamOutputString(strm)->vectorPushExtend(clasp_make_character(c));
  return c;
}

static T_sp
str_out_element_type(T_sp strm) {
  T_sp tstring = StringOutputStreamOutputString(strm);
  ASSERT(cl__stringp(tstring));
  return gc::As_unsafe<String_sp>(tstring)->arrayElementType();
}

T_sp str_out_get_position(T_sp strm) {
  String_sp str = StringOutputStreamOutputString(strm);
  return Integer_O::create((gc::Fixnum)(StringFillp(str)));
}

static T_sp
str_out_set_position(T_sp strm, T_sp pos) {
  String_sp string = StringOutputStreamOutputString(strm);
  Fixnum disp;
  if (pos.nilp()) {
    disp = StringOutputStreamOutputString(strm)->arrayTotalSize();
  } else {
    disp = clasp_toSize(pos);
  }
  if (disp < StringFillp(string)) {
    SetStringFillp(string,disp);
  } else {
    disp -= StringFillp(string);
    while (disp-- > 0)
      clasp_write_char(' ', strm);
  }
  return _lisp->_true();
}

static int
str_out_column(T_sp strm) {
  return StreamOutputColumn(strm);
}

const FileOps str_out_ops = {
    not_output_write_byte8,
    not_binary_read_byte8,

    not_binary_write_byte,
    not_input_read_byte,

    not_input_read_char,
    str_out_write_char,
    not_input_unread_char,
    generic_peek_char,

    generic_read_vector,
    generic_write_vector,

    not_input_listen,
    not_input_clear_input,
    generic_void, /* clear-output */
    generic_void, /* finish-output */
    generic_void, /* force-output */

    generic_always_false, /* input_p */
    generic_always_true,  /* output_p */
    generic_always_false,
    str_out_element_type,

    not_a_file_stream, /* length */
    str_out_get_position,
    str_out_set_position,
    str_out_column,
    generic_close};

CL_LAMBDA(s);
CL_DECLARE();
CL_DOCSTRING("make_string_output_stream_from_string");
CL_DEFUN T_sp core__make_string_output_stream_from_string(T_sp s) {
  T_sp strm = StringOutputStream_O::create();
  bool stringp = cl__stringp(s);
  unlikely_if(!stringp || !gc::As<Array_sp>(s)->arrayHasFillPointerP())
  {
      FEerror("~S is not a string with a fill-pointer.", 1, s.raw_());
  }
  StreamOps(strm) = str_out_ops; // duplicate_dispatch_table(&str_out_ops);
  StreamMode(strm) = clasp_smm_string_output;
  StringOutputStreamOutputString(strm) = gc::As<String_sp>(s);
  StreamOutputColumn(strm) = 0;
#if !defined(CLASP_UNICODE)
  StreamFormat(strm) = kw::_sym_passThrough;
  StreamFlags(strm) = CLASP_STREAM_DEFAULT_FORMAT;
  StreamByteSize(strm) = 8;
#else
  if (cl__simple_string_p(s)) {
    StreamFormat(strm) = kw::_sym_latin_1;
    StreamFlags(strm) = CLASP_STREAM_LATIN_1;
    StreamByteSize(strm) = 8;
  } else {
    StreamFormat(strm) = kw::_sym_ucs_4;
    StreamFlags(strm) = CLASP_STREAM_UCS_4;
    StreamByteSize(strm) = 32;
  }
#endif
  return strm;
}

T_sp clasp_make_string_output_stream(cl_index line_length, bool extended) {
#ifdef CLASP_UNICODE
  T_sp s;
  if (extended) {
    s = StrWNs_O::createBufferString(line_length);
  } else {
    s = Str8Ns_O::createBufferString(line_length);
  }
#else
  T_sp s = Str8Ns_O::createBufferString(line_length); // clasp_alloc_adjustable_base_string(line_length);
#endif
  return core__make_string_output_stream_from_string(s);
}

CL_LAMBDA("&key (element-type 'character)");
CL_DECLARE();
CL_DOCSTRING("makeStringOutputStream");
CL_DEFUN T_sp cl__make_string_output_stream(Symbol_sp elementType) {
  int extended = 0;
  if (elementType == cl::_sym_base_char) {
    (void)0;
  } else if (elementType == cl::_sym_character) {
#ifdef CLASP_UNICODE
    extended = 1;
#endif
  } else if (!T_sp(eval::funcall(cl::_sym_subtypep, elementType, cl::_sym_base_char)).nilp()) {
    (void)0;
  } else if (!T_sp(eval::funcall(cl::_sym_subtypep, elementType, cl::_sym_character)).nilp()) {
#ifdef CLASP_UNICODE
    extended = 1;
#endif
  } else {
    FEerror("In MAKE-STRING-OUTPUT-STREAM, the argument :ELEMENT-TYPE (~A) must be a subtype of character",
            1, elementType.raw_());
  }
  return clasp_make_string_output_stream(128, extended);
}

CL_LAMBDA(strm);
CL_DECLARE();
CL_DOCSTRING("get_output_stream_string");
CL_DEFUN T_sp cl__get_output_stream_string(T_sp strm) {
  T_sp strng;
  unlikely_if(!AnsiStreamTypeP(strm, clasp_smm_string_output))
      af_wrongTypeOnlyArg(__FILE__, __LINE__, cl::_sym_getOutputStreamString, strm, cl::_sym_StringStream_O);
  String_sp buffer = StringOutputStreamOutputString(strm);
  //        printf("%s:%d StringOutputStreamOutputString = %s\n", __FILE__, __LINE__, buffer->get().c_str());
  strng = cl__copy_seq(buffer);
  SetStringFillp(buffer,0);
  return strng;
}

/**********************************************************************
 * STRING INPUT STREAMS
 */

static claspCharacter
str_in_read_char(T_sp strm) {
  gctools::Fixnum curr_pos = StringInputStreamInputPosition(strm);
  claspCharacter c;
  if (curr_pos >= StringInputStreamInputLimit(strm)) {
    c = EOF;
  } else {
    c = clasp_as_claspCharacter(cl__char(StringInputStreamInputString(strm),curr_pos));
    StringInputStreamInputPosition(strm) = curr_pos + 1;
  }
  return c;
}

static void
str_in_unread_char(T_sp strm, claspCharacter c) {
  gctools::Fixnum curr_pos = StringInputStreamInputPosition(strm);
  unlikely_if(c <= 0) {
    unread_error(strm);
  }
  StringInputStreamInputPosition(strm) = curr_pos - 1;
}

static claspCharacter
str_in_peek_char(T_sp strm) {
  cl_index pos = StringInputStreamInputPosition(strm);
  if (pos >= StringInputStreamInputLimit(strm)) {
    return EOF;
  } else {
    return clasp_as_claspCharacter(cl__char(StringInputStreamInputString(strm),pos));
  }
}

static int
str_in_listen(T_sp strm) {
  if (StringInputStreamInputPosition(strm) < StringInputStreamInputLimit(strm))
    return CLASP_LISTEN_AVAILABLE;
  else
    return CLASP_LISTEN_EOF;
}

static T_sp
str_in_element_type(T_sp strm) {
  T_sp tstring = StringInputStreamInputString(strm);
  ASSERT(cl__stringp(tstring));
  return gc::As_unsafe<String_sp>(tstring)->arrayElementType();
}

static T_sp
str_in_get_position(T_sp strm) {
  return Integer_O::create((gc::Fixnum)(StringInputStreamInputPosition(strm)));
}

static T_sp
str_in_set_position(T_sp strm, T_sp pos) {
  gctools::Fixnum disp;
  if (pos.nilp()) {
    disp = StringInputStreamInputLimit(strm);
  } else {
    disp = clasp_toSize(pos);
    if (disp >= StringInputStreamInputLimit(strm)) {
      disp = StringInputStreamInputLimit(strm);
    }
  }
  StringInputStreamInputPosition(strm) = disp;
  return _lisp->_true();
}

const FileOps str_in_ops = {
    not_output_write_byte8,
    not_binary_read_byte8,

    not_output_write_byte,
    not_binary_read_byte,

    str_in_read_char,
    not_output_write_char,
    str_in_unread_char,
    str_in_peek_char,

    generic_read_vector,
    generic_write_vector,

    str_in_listen,
    generic_void, /* clear-input */
    not_output_clear_output,
    not_output_finish_output,
    not_output_force_output,

    generic_always_true,  /* input_p */
    generic_always_false, /* output_p */
    generic_always_false,
    str_in_element_type,

    not_a_file_stream, /* length */
    str_in_get_position,
    str_in_set_position,
    generic_column,
    generic_close};

T_sp clasp_make_string_input_stream(T_sp strng, cl_index istart, cl_index iend) {
  ASSERT(cl__stringp(strng));
  T_sp strm;
  strm = StringInputStream_O::create();
  StreamOps(strm) = duplicate_dispatch_table(str_in_ops);
  StreamMode(strm) = clasp_smm_string_input;
  StringInputStreamInputString(strm) = gc::As<String_sp>(strng);
  StringInputStreamInputPosition(strm) = istart;
  StringInputStreamInputLimit(strm) = iend;
#if !defined(CLASP_UNICODE)
  StreamFormat(strm) = kw::_sym_passThrough;
  StreamFlags(strm) = CLASP_STREAM_DEFAULT_FORMAT;
  StreamByteSize(strm) = 8;
#else
  if ( core__base_string_p(strng) /*cl__simple_string_p(strng) == t_base_string*/) {
    StreamFormat(strm) = kw::_sym_latin_1;
    StreamFlags(strm) = CLASP_STREAM_LATIN_1;
    StreamByteSize(strm) = 8;
  } else {
    StreamFormat(strm) = kw::_sym_ucs_4;
    StreamFlags(strm) = CLASP_STREAM_UCS_4;
    StreamByteSize(strm) = 32;
  }
#endif
  return strm;
}

CL_LAMBDA(strng &optional (istart 0) iend);
CL_DECLARE();
CL_DOCSTRING("make_string_input_stream");
CL_DEFUN T_sp cl__make_string_input_stream(String_sp strng, cl_index istart, T_sp iend) {
  ASSERT(cl__stringp(strng));
  size_t_pair p = sequenceStartEnd(cl::_sym_make_string_input_stream,
                                   strng->length(), istart, iend);
  return clasp_make_string_input_stream(strng, p.start, p.end);
}

/**********************************************************************
     * TWO WAY STREAM
     */

static cl_index
two_way_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  if (strm == _lisp->_Roots._TerminalIO)
    clasp_force_output(TwoWayStreamOutput(_lisp->_Roots._TerminalIO));
  return clasp_read_byte8(TwoWayStreamInput(strm), c, n);
}

static cl_index
two_way_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  return clasp_write_byte8(TwoWayStreamOutput(strm), c, n);
}

static void
two_way_write_byte(T_sp byte, T_sp stream) {
  clasp_write_byte(byte, TwoWayStreamOutput(stream));
}

static T_sp
two_way_read_byte(T_sp stream) {
  return clasp_read_byte(TwoWayStreamInput(stream));
}

static claspCharacter
two_way_read_char(T_sp strm) {
  return clasp_read_char(TwoWayStreamInput(strm));
}

static claspCharacter
two_way_write_char(T_sp strm, claspCharacter c) {
  return clasp_write_char(c, TwoWayStreamOutput(strm));
}

static void
two_way_unread_char(T_sp strm, claspCharacter c) {
  clasp_unread_char(c, TwoWayStreamInput(strm));
}

static claspCharacter
two_way_peek_char(T_sp strm) {
  return clasp_peek_char(TwoWayStreamInput(strm));
}

static cl_index
two_way_read_vector(T_sp strm, T_sp data, cl_index start, cl_index n) {
  strm = TwoWayStreamInput(strm);
  return stream_dispatch_table(strm).read_vector(strm, data, start, n);
}

static cl_index
two_way_write_vector(T_sp strm, T_sp data, cl_index start, cl_index n) {
  strm = TwoWayStreamOutput(strm);
  return stream_dispatch_table(strm).write_vector(strm, data, start, n);
}

static int
two_way_listen(T_sp strm) {
  return clasp_listen_stream(TwoWayStreamInput(strm));
}

static void
two_way_clear_input(T_sp strm) {
  clasp_clear_input(TwoWayStreamInput(strm));
}

static void
two_way_clear_output(T_sp strm) {
  clasp_clear_output(TwoWayStreamOutput(strm));
}

static void
two_way_force_output(T_sp strm) {
  clasp_force_output(TwoWayStreamOutput(strm));
}

static void
two_way_finish_output(T_sp strm) {
  clasp_finish_output(TwoWayStreamOutput(strm));
}

static int
two_way_interactive_p(T_sp strm) {
  return clasp_interactive_stream_p(TwoWayStreamInput(strm));
}

static T_sp
two_way_element_type(T_sp strm) {
  return clasp_stream_element_type(TwoWayStreamInput(strm));
}

static int
two_way_column(T_sp strm) {
  return clasp_file_column(TwoWayStreamOutput(strm));
}

static T_sp
two_way_close(T_sp strm) {
  if (StreamFlags(strm) & CLASP_STREAM_CLOSE_COMPONENTS) {
    eval::funcall(cl::_sym_close, TwoWayStreamInput(strm));
    eval::funcall(cl::_sym_close, TwoWayStreamOutput(strm));
  }
  return generic_close(strm);
}

const FileOps two_way_ops = {
    two_way_write_byte8,
    two_way_read_byte8,

    two_way_write_byte,
    two_way_read_byte,

    two_way_read_char,
    two_way_write_char,
    two_way_unread_char,
    two_way_peek_char,

    two_way_read_vector,
    two_way_write_vector,

    two_way_listen,
    two_way_clear_input,
    two_way_clear_output,
    two_way_finish_output,
    two_way_force_output,

    generic_always_true, /* input_p */
    generic_always_true, /* output_p */
    two_way_interactive_p,
    two_way_element_type,

    not_a_file_stream,  /* length */
    generic_always_nil, /* get_position */
    generic_set_position,
    two_way_column,
    two_way_close};

CL_LAMBDA(istrm ostrm);
CL_DECLARE();
CL_DOCSTRING("make-two-way-stream");
CL_DEFUN T_sp cl__make_two_way_stream(T_sp istrm, T_sp ostrm) {
  T_sp strm;
  if (!clasp_input_stream_p(istrm))
    not_an_input_stream(istrm);
  if (!clasp_output_stream_p(ostrm))
    not_an_output_stream(ostrm);
  strm = TwoWayStream_O::create();
  StreamFormat(strm) = cl__stream_external_format(istrm);
  StreamMode(strm) = clasp_smm_two_way;
  StreamOps(strm) = duplicate_dispatch_table(two_way_ops);
  TwoWayStreamInput(strm) = istrm;
  TwoWayStreamOutput(strm) = ostrm;
  return strm;
}

CL_LAMBDA(strm);
CL_DECLARE();
CL_DOCSTRING("two-way-stream-input-stream");
CL_DEFUN T_sp cl__two_way_stream_input_stream(T_sp strm) {
  unlikely_if(!AnsiStreamTypeP(strm, clasp_smm_two_way))
      ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_two_way_stream_input_stream,
                                strm, cl::_sym_two_way_stream);
  return TwoWayStreamInput(strm);
}

CL_LAMBDA(strm);
CL_DECLARE();
CL_DOCSTRING("two-way-stream-output-stream");
CL_DEFUN T_sp cl__two_way_stream_output_stream(T_sp strm) {
  unlikely_if(!AnsiStreamTypeP(strm, clasp_smm_two_way))
      ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_two_way_stream_output_stream,
                                strm, cl::_sym_two_way_stream);
  return TwoWayStreamOutput(strm);
}

/**********************************************************************
 * BROADCAST STREAM
 */

static cl_index
broadcast_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  T_sp l;
  cl_index out = n;
  for (l = BroadcastStreamList(strm); !l.nilp(); l = oCdr(l)) {
    out = clasp_write_byte8(oCar(l), c, n);
  }
  return out;
}

static claspCharacter
broadcast_write_char(T_sp strm, claspCharacter c) {
  T_sp l;
  for (l = BroadcastStreamList(strm); !l.nilp(); l = oCdr(l)) {
    clasp_write_char(c, oCar(l));
  }
  return c;
}

static void
broadcast_write_byte(T_sp c, T_sp strm) {
  T_sp l;
  for (l = BroadcastStreamList(strm); !l.nilp(); l = oCdr(l)) {
    clasp_write_byte(c, oCar(l));
  }
}

static void
broadcast_clear_output(T_sp strm) {
  T_sp l;
  for (l = BroadcastStreamList(strm); !l.nilp(); l = oCdr(l)) {
    clasp_clear_output(oCar(l));
  }
}

static void
broadcast_force_output(T_sp strm) {
  T_sp l;
  for (l = BroadcastStreamList(strm); !l.nilp(); l = oCdr(l)) {
    clasp_force_output(oCar(l));
  }
}

static void
broadcast_finish_output(T_sp strm) {
  T_sp l;
  for (l = BroadcastStreamList(strm); !l.nilp(); l = oCdr(l)) {
    clasp_finish_output(oCar(l));
  }
}

static T_sp
broadcast_element_type(T_sp strm) {
  T_sp l = BroadcastStreamList(strm);
  if (l.nilp())
    return _lisp->_true();
  return clasp_stream_element_type(oCar(cl__last(l, clasp_make_fixnum(1))));
}

static T_sp
broadcast_length(T_sp strm) {
  T_sp l = BroadcastStreamList(strm);
  if (l.nilp())
    return make_fixnum(0);
  return clasp_file_length(oCar(cl__last(l, clasp_make_fixnum(1))));
}

static T_sp
broadcast_get_position(T_sp strm) {
  T_sp l = BroadcastStreamList(strm);
  if (l.nilp())
    return make_fixnum(0);
  return clasp_file_position(oCar(cl__last(l, clasp_make_fixnum(1))));
}

static T_sp
broadcast_set_position(T_sp strm, T_sp pos) {
  T_sp l = BroadcastStreamList(strm);
  if (l.nilp())
    return _Nil<T_O>();
  return clasp_file_position_set(oCar(l), pos);
}

static int
broadcast_column(T_sp strm) {
  T_sp l = BroadcastStreamList(strm);
  if (l.nilp())
    return 0;
  return clasp_file_column(oCar(l));
}

static T_sp
broadcast_close(T_sp strm) {
  if (StreamFlags(strm) & CLASP_STREAM_CLOSE_COMPONENTS) {
    cl__mapc(cl::_sym_close, gc::As<Cons_sp>(BroadcastStreamList(strm)));
  }
  return generic_close(strm);
}

const FileOps broadcast_ops = {
    broadcast_write_byte8,
    not_input_read_byte8,

    broadcast_write_byte,
    not_input_read_byte,

    not_input_read_char,
    broadcast_write_char,
    not_input_unread_char,
    generic_peek_char,

    generic_read_vector,
    generic_write_vector,

    not_input_listen,
    broadcast_force_output, /* clear_input */ /* FIXME! This is legacy behaviour */
    broadcast_clear_output,
    broadcast_finish_output,
    broadcast_force_output,

    generic_always_false, /* input_p */
    generic_always_true,  /* output_p */
    generic_always_false,
    broadcast_element_type,

    broadcast_length,
    broadcast_get_position,
    broadcast_set_position,
    broadcast_column,
    broadcast_close};

CL_LAMBDA(&rest ap);
CL_DECLARE();
CL_DOCSTRING("makeBroadcastStream");
CL_DEFUN T_sp cl__make_broadcast_stream(List_sp ap) {
  T_sp x, streams;
  // we need to verify that ap are all streams and if so, also output-streams
  // previously (make-broadcast-stream 1 2 3) worked fine
  if (ap.notnilp()) {
    for (T_sp l = ap; !l.nilp(); l = oCdr(l)) {
      T_sp potentialstream = oCar(l);
      if (!clasp_output_stream_p(potentialstream))
        not_an_output_stream(potentialstream);
    }
  }
  streams = ap;
  x = BroadcastStream_O::create();
  StreamFormat(x) = kw::_sym_default;
  StreamOps(x) = duplicate_dispatch_table(broadcast_ops);
  StreamMode(x) = clasp_smm_broadcast;
  // nreverse is needed in ecl, since they freshly cons_up a list in reverse order
  // but not here streams is in the original order
  BroadcastStreamList(x) = streams;
  return x;
}

CL_LAMBDA(strm);
CL_DECLARE();
CL_DOCSTRING("broadcast-stream-streams");
CL_DEFUN
T_sp cl__broadcast_stream_streams(T_sp strm) {
  unlikely_if(!AnsiStreamTypeP(strm, clasp_smm_broadcast))
      ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_broadcast_stream_streams,
                                strm, cl::_sym_BroadcastStream_O);
  return cl__copy_list(BroadcastStreamList(strm));
}

/**********************************************************************
 * ECHO STREAM
 */

static cl_index
echo_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  cl_index out = clasp_read_byte8(EchoStreamInput(strm), c, n);
  return clasp_write_byte8(EchoStreamOutput(strm), c, out);
}

static cl_index
echo_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  return clasp_write_byte8(EchoStreamOutput(strm), c, n);
}

static void
echo_write_byte(T_sp c, T_sp strm) {
  clasp_write_byte(c, EchoStreamOutput(strm));
}

static T_sp
echo_read_byte(T_sp strm) {
  T_sp out = clasp_read_byte(EchoStreamInput(strm));
  if (!out.nilp())
    clasp_write_byte(out, EchoStreamOutput(strm));
  return out;
}

static claspCharacter
echo_read_char(T_sp strm) {
  claspCharacter c = StreamLastCode(strm, 0);
  if (c == EOF) {
    c = clasp_read_char(EchoStreamInput(strm));
    if (c != EOF)
      clasp_write_char(c, EchoStreamOutput(strm));
  } else {
    StreamLastCode(strm, 0) = EOF;
    clasp_read_char(EchoStreamInput(strm));
  }
  return c;
}

static claspCharacter
echo_write_char(T_sp strm, claspCharacter c) {
  return clasp_write_char(c, EchoStreamOutput(strm));
}

static void
echo_unread_char(T_sp strm, claspCharacter c) {
  unlikely_if(StreamLastCode(strm, 0) != EOF) {
    unread_twice(strm);
  }
  StreamLastCode(strm, 0) = c;
  clasp_unread_char(c, EchoStreamInput(strm));
}

static claspCharacter
echo_peek_char(T_sp strm) {
  claspCharacter c = StreamLastCode(strm, 0);
  if (c == EOF) {
    c = clasp_peek_char(EchoStreamInput(strm));
  }
  return c;
}

static int
echo_listen(T_sp strm) {
  return clasp_listen_stream(EchoStreamInput(strm));
}

static void
echo_clear_input(T_sp strm) {
  clasp_clear_input(EchoStreamInput(strm));
}

static void
echo_clear_output(T_sp strm) {
  clasp_clear_output(EchoStreamOutput(strm));
}

static void
echo_force_output(T_sp strm) {
  clasp_force_output(EchoStreamOutput(strm));
}

static void
echo_finish_output(T_sp strm) {
  clasp_finish_output(EchoStreamOutput(strm));
}

static T_sp
echo_element_type(T_sp strm) {
  return clasp_stream_element_type(EchoStreamInput(strm));
}

static int
echo_column(T_sp strm) {
  return clasp_file_column(EchoStreamOutput(strm));
}

static T_sp
echo_close(T_sp strm) {
  if (StreamFlags(strm) & CLASP_STREAM_CLOSE_COMPONENTS) {
    eval::funcall(cl::_sym_close, EchoStreamInput(strm));
    eval::funcall(cl::_sym_close, EchoStreamOutput(strm));
  }
  return generic_close(strm);
}

const FileOps echo_ops = {
    echo_write_byte8,
    echo_read_byte8,

    echo_write_byte,
    echo_read_byte,

    echo_read_char,
    echo_write_char,
    echo_unread_char,
    echo_peek_char,

    generic_read_vector,
    generic_write_vector,

    echo_listen,
    echo_clear_input,
    echo_clear_output,
    echo_finish_output,
    echo_force_output,

    generic_always_true, /* input_p */
    generic_always_true, /* output_p */
    generic_always_false,
    echo_element_type,

    not_a_file_stream,  /* length */
    generic_always_nil, /* get_position */
    generic_set_position,
    echo_column,
    echo_close};

CL_LAMBDA(strm1 strm2);
CL_DECLARE();
CL_DOCSTRING("make-echo-stream");
CL_DEFUN T_sp cl__make_echo_stream(T_sp strm1, T_sp strm2) {
  T_sp strm;
  unlikely_if(!clasp_input_stream_p(strm1))
      not_an_input_stream(strm1);
  unlikely_if(!clasp_output_stream_p(strm2))
      not_an_output_stream(strm2);
  strm = EchoStream_O::create();
  StreamFormat(strm) = cl__stream_external_format(strm1);
  StreamMode(strm) = clasp_smm_echo;
  StreamOps(strm) = duplicate_dispatch_table(echo_ops);
  EchoStreamInput(strm) = strm1;
  EchoStreamOutput(strm) = strm2;
  return strm;
}

CL_LAMBDA(strm);
CL_DECLARE();
CL_DOCSTRING("echo-stream-input-stream");
CL_DEFUN T_sp cl__echo_stream_input_stream(T_sp strm) {
  unlikely_if(!AnsiStreamTypeP(strm, clasp_smm_echo))
      ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_echo_stream_input_stream, strm, cl::_sym_EchoStream_O);
  return EchoStreamInput(strm);
}

CL_LAMBDA(strm);
CL_DECLARE();
CL_DOCSTRING("echo-stream-output-stream");
CL_DEFUN T_sp cl__echo_stream_output_stream(T_sp strm) {
  unlikely_if(!AnsiStreamTypeP(strm, clasp_smm_echo))
      ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_echo_stream_output_stream, strm, cl::_sym_EchoStream_O);
  return EchoStreamOutput(strm);
}

/**********************************************************************
 * CONCATENATED STREAM
 */

static cl_index
concatenated_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  T_sp l = ConcatenatedStreamList(strm);
  cl_index out = 0;
  while (out < n && !l.nilp()) {
    cl_index delta = clasp_read_byte8(oCar(l), c + out, n - out);
    out += delta;
    if (out == n)
      break;
    ConcatenatedStreamList(strm) = l = oCdr(l);
  }
  return out;
}

static T_sp
concatenated_read_byte(T_sp strm) {
  T_sp l = ConcatenatedStreamList(strm);
  T_sp c = _Nil<T_O>();
  while (!l.nilp()) {
    c = clasp_read_byte(oCar(l));
    if (c != _Nil<T_O>())
      break;
    ConcatenatedStreamList(strm) = l = oCdr(l);
  }
  return c;
}

static T_sp
concatenated_element_type(T_sp strm) {
  T_sp l = ConcatenatedStreamList(strm);
  if (l.nilp())
    return _lisp->_true();
  return clasp_stream_element_type(oCar(l));
}

static claspCharacter
concatenated_read_char(T_sp strm) {
  T_sp l = ConcatenatedStreamList(strm);
  claspCharacter c = EOF;
  while (!l.nilp()) {
    c = clasp_read_char(oCar(l));
    if (c != EOF)
      break;
    ConcatenatedStreamList(strm) = l = oCdr(l);
  }
  return c;
}

static void
concatenated_unread_char(T_sp strm, claspCharacter c) {
  T_sp l = ConcatenatedStreamList(strm);
  unlikely_if(l.nilp())
      unread_error(strm);
  clasp_unread_char(c, oCar(l));
}

static int
concatenated_listen(T_sp strm) {
  T_sp l = ConcatenatedStreamList(strm);
  while (!l.nilp()) {
    int f = clasp_listen_stream(oCar(l));
    l = oCdr(l);
    if (f == CLASP_LISTEN_EOF) {
      ConcatenatedStreamList(strm) = l;
    } else {
      return f;
    }
  }
  return CLASP_LISTEN_EOF;
}

static T_sp
concatenated_close(T_sp strm) {
  if (StreamFlags(strm) & CLASP_STREAM_CLOSE_COMPONENTS) {
    cl__mapc(cl::_sym_close, gc::As<Cons_sp>(ConcatenatedStreamList(strm)));
  }
  return generic_close(strm);
}

const FileOps concatenated_ops = {
    not_output_write_byte8,
    concatenated_read_byte8,

    not_output_write_byte,
    concatenated_read_byte,

    concatenated_read_char,
    not_output_write_char,
    concatenated_unread_char,
    generic_peek_char,

    generic_read_vector,
    generic_write_vector,

    concatenated_listen,
    generic_void, /* clear_input */
    not_output_clear_output,
    not_output_finish_output,
    not_output_force_output,

    generic_always_true,  /* input_p */
    generic_always_false, /* output_p */
    generic_always_false,
    // this is wrong, must be specific for concatenated streams
    // should be concatenated_element_type with a proper definition for that
    // ccl does more or less (stream-element-type (concatenated-stream-current-input-stream s))
    concatenated_element_type,
    not_a_file_stream,  /* length */
    generic_always_nil, /* get_position */
    generic_set_position,
    generic_column,
    concatenated_close};

CL_LAMBDA(&rest ap);
CL_DECLARE();
CL_DOCSTRING("makeConcatenatedStream");
CL_DEFUN T_sp cl__make_concatenated_stream(List_sp ap) {
  T_sp x, streams;
  streams = ap;
  x = ConcatenatedStream_O::create();
  if (streams.nilp()) {
    StreamFormat(x) = kw::_sym_passThrough;
  } else {
    StreamFormat(x) = cl__stream_external_format(oCar(streams));
    // here we should test that the effectively we were passed a list of streams that satisfy INPUT-STREAM-P
    // fixes MAKE-CONCATENATED-STREAM.ERROR.1, MAKE-CONCATENATED-STREAM.ERROR.2
    for (T_sp l = streams; !l.nilp(); l = oCdr(l)) {
      T_sp potentialstream = oCar(l);
      //clasp_input_stream_p also verifies if is a stream at all
      if (!clasp_input_stream_p(potentialstream))
        not_an_input_stream(potentialstream);
    }
  }
  StreamMode(x) = clasp_smm_concatenated;
  StreamOps(x) = duplicate_dispatch_table(concatenated_ops);
  // used to be nreverse, but this gives wrong results, since it than reads first from the last stream passed
  // stick with the original list
  // in ecl there is nreverse, since the list of streams is consed up newly, so is in inverse order
  ConcatenatedStreamList(x) = streams;
  return x;
}

CL_LAMBDA(strm);
CL_DECLARE();
CL_DOCSTRING("concatenated-stream-streams");
CL_DEFUN T_sp cl__concatenated_stream_streams(T_sp strm) {
  unlikely_if(!AnsiStreamTypeP(strm, clasp_smm_concatenated))
      ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_concatenated_stream_streams, strm, cl::_sym_ConcatenatedStream_O);
  return cl__copy_list(ConcatenatedStreamList(strm));
}

/**********************************************************************
 * SYNONYM STREAM
 */

static cl_index
synonym_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  return clasp_read_byte8(SynonymStreamStream(strm), c, n);
}

static cl_index
synonym_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  return clasp_write_byte8(SynonymStreamStream(strm), c, n);
}

static void
synonym_write_byte(T_sp c, T_sp strm) {
  clasp_write_byte(c, SynonymStreamStream(strm));
}

static T_sp
synonym_read_byte(T_sp strm) {
  return clasp_read_byte(SynonymStreamStream(strm));
}

static claspCharacter
synonym_read_char(T_sp strm) {
  return clasp_read_char(SynonymStreamStream(strm));
}

static claspCharacter
synonym_write_char(T_sp strm, claspCharacter c) {
  return clasp_write_char(c, SynonymStreamStream(strm));
}

static void
synonym_unread_char(T_sp strm, claspCharacter c) {
  clasp_unread_char(c, SynonymStreamStream(strm));
}

static claspCharacter
synonym_peek_char(T_sp strm) {
  return clasp_peek_char(SynonymStreamStream(strm));
}

static cl_index
synonym_read_vector(T_sp strm, T_sp data, cl_index start, cl_index n) {
  strm = SynonymStreamStream(strm);
  return stream_dispatch_table(strm).read_vector(strm, data, start, n);
}

static cl_index
synonym_write_vector(T_sp strm, T_sp data, cl_index start, cl_index n) {
  strm = SynonymStreamStream(strm);
  return stream_dispatch_table(strm).write_vector(strm, data, start, n);
}

static int
synonym_listen(T_sp strm) {
  return clasp_listen_stream(SynonymStreamStream(strm));
}

static void
synonym_clear_input(T_sp strm) {
  clasp_clear_input(SynonymStreamStream(strm));
}

static void
synonym_clear_output(T_sp strm) {
  clasp_clear_output(SynonymStreamStream(strm));
}

static void
synonym_force_output(T_sp strm) {
  clasp_force_output(SynonymStreamStream(strm));
}

static void
synonym_finish_output(T_sp strm) {
  clasp_finish_output(SynonymStreamStream(strm));
}

static int
synonym_input_p(T_sp strm) {
  return clasp_input_stream_p(SynonymStreamStream(strm));
}

static int
synonym_output_p(T_sp strm) {
  return clasp_output_stream_p(SynonymStreamStream(strm));
}

static int
synonym_interactive_p(T_sp strm) {
  return clasp_interactive_stream_p(SynonymStreamStream(strm));
}

static T_sp
synonym_element_type(T_sp strm) {
  return clasp_stream_element_type(SynonymStreamStream(strm));
}

static T_sp
synonym_length(T_sp strm) {
  return clasp_file_length(SynonymStreamStream(strm));
}

static T_sp
synonym_get_position(T_sp strm) {
  return clasp_file_position(SynonymStreamStream(strm));
}

static T_sp
synonym_set_position(T_sp strm, T_sp pos) {
  return clasp_file_position_set(SynonymStreamStream(strm), pos);
}

static int
synonym_column(T_sp strm) {
  return clasp_file_column(SynonymStreamStream(strm));
}

const FileOps synonym_ops = {
    synonym_write_byte8,
    synonym_read_byte8,

    synonym_write_byte,
    synonym_read_byte,

    synonym_read_char,
    synonym_write_char,
    synonym_unread_char,
    synonym_peek_char,

    synonym_read_vector,
    synonym_write_vector,

    synonym_listen,
    synonym_clear_input,
    synonym_clear_output,
    synonym_finish_output,
    synonym_force_output,

    synonym_input_p,
    synonym_output_p,
    synonym_interactive_p,
    synonym_element_type,

    synonym_length,
    synonym_get_position,
    synonym_set_position,
    synonym_column,
    generic_close};

CL_LAMBDA(strm1);
CL_DECLARE();
CL_DOCSTRING("make-synonym-stream");
CL_DEFUN T_sp cl__make_synonym_stream(T_sp tsym) {
  Symbol_sp sym = gc::As<Symbol_sp>(tsym);
  T_sp x = SynonymStream_O::create();
  StreamOps(x) = duplicate_dispatch_table(synonym_ops);
  StreamMode(x) = clasp_smm_synonym;
  SynonymStreamSymbol(x) = sym;
  return x;
}

CL_LAMBDA(s);
CL_DECLARE();
CL_DOCSTRING("See CLHS synonym-stream-symbol");
CL_DEFUN T_sp cl__synonym_stream_symbol(T_sp strm) {
  unlikely_if(!AnsiStreamTypeP(strm, clasp_smm_synonym))
      ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_synonym_stream_symbol, strm, cl::_sym_SynonymStream_O);
  return SynonymStreamSymbol(strm);
}

/**********************************************************************
 * UNINTERRUPTED OPERATIONS
 */

#ifdef CLASP_MS_WINDOWS_HOST
#define clasp_mode_t int
#else
#define clasp_mode_t mode_t
#endif

static int
safe_open(const char *filename, int flags, clasp_mode_t mode) {
  const cl_env_ptr the_env = clasp_process_env();
  clasp_disable_interrupts_env(the_env);
  int output = open(filename, flags, mode);
  clasp_enable_interrupts_env(the_env);
  return output;
}

static int
safe_close(int f) {
  const cl_env_ptr the_env = clasp_process_env();
  int output;
  clasp_disable_interrupts_env(the_env);
  output = close(f);
  clasp_enable_interrupts_env(the_env);
  return output;
}

static FILE *
safe_fopen(const char *filename, const char *mode) {
  const cl_env_ptr the_env = clasp_process_env();
  FILE *output;
  clasp_disable_interrupts_env(the_env);
  output = fopen(filename, mode);
  clasp_enable_interrupts_env(the_env);
  return output;
}

static FILE *
safe_fdopen(int fildes, const char *mode) {
  const cl_env_ptr the_env = clasp_process_env();
  FILE *output;
  clasp_disable_interrupts_env(the_env);
  output = fdopen(fildes, mode);
  clasp_enable_interrupts_env(the_env);
  return output;
}

static int
safe_fclose(FILE *stream) {
  const cl_env_ptr the_env = clasp_process_env();
  int output;
  clasp_disable_interrupts_env(the_env);
  output = fclose(stream);
  clasp_enable_interrupts_env(the_env);
  return output;
}

/**********************************************************************
 * POSIX FILE STREAM
 */

static cl_index
consume_byte_stack(T_sp strm, unsigned char *c, cl_index n) {
  cl_index out = 0;
  T_sp l;
  while (n) {
    l = StreamByteStack(strm);
    if (l.nilp())
      return out + StreamOps(strm).read_byte8(strm, c, n);
    *(c++) = (oCar(l)).unsafe_fixnum();
    out++;
    n--;
    StreamByteStack(strm) = l = oCdr(l);
  }
  return out;
}

static cl_index
io_file_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  unlikely_if(StreamByteStack(strm).notnilp()) { // != _Nil<T_O>()) {
    return consume_byte_stack(strm, c, n);
  }
  else {
    int f = IOFileStreamDescriptor(strm);
    gctools::Fixnum out = 0;
    clasp_disable_interrupts();
    do {
      out = read(f, c, sizeof(char) * n);
    } while (out < 0 && restartable_io_error(strm, "read"));
    clasp_enable_interrupts();
    return out;
  }
}

static cl_index
output_file_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  int f = IOFileStreamDescriptor(strm);
  gctools::Fixnum out;
  clasp_disable_interrupts();
  do {
    out = write(f, c, sizeof(char) * n);
  } while (out < 0 && restartable_io_error(strm, "write"));
  clasp_enable_interrupts();
  return out;
}

static cl_index
io_file_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  unlikely_if(StreamByteStack(strm).notnilp()) { // != _Nil<T_O>()) {
    /* Try to move to the beginning of the unread characters */
    T_sp aux = clasp_file_position(strm);
    if (!aux.nilp())
      clasp_file_position_set(strm, aux);
    StreamByteStack(strm) = _Nil<T_O>();
  }
  return output_file_write_byte8(strm, c, n);
}

static int
io_file_listen(T_sp strm) {
  if (StreamByteStack(strm).notnilp()) // != _Nil<T_O>())
    return CLASP_LISTEN_AVAILABLE;
  if (StreamFlags(strm) & CLASP_STREAM_MIGHT_SEEK) {
    cl_env_ptr the_env = clasp_process_env();
    int f = IOFileStreamDescriptor(strm);
    clasp_off_t disp, onew;
    clasp_disable_interrupts_env(the_env);
    disp = lseek(f, 0, SEEK_CUR);
    clasp_enable_interrupts_env(the_env);
    if (disp != (clasp_off_t)-1) {
      clasp_disable_interrupts_env(the_env);
      onew = lseek(f, 0, SEEK_END);
      clasp_enable_interrupts_env(the_env);
      lseek(f, disp, SEEK_SET);
      if (onew == disp) {
        return CLASP_LISTEN_NO_CHAR;
      } else if (onew != (clasp_off_t)-1) {
        return CLASP_LISTEN_AVAILABLE;
      }
    }
  }
  return file_listen(strm, IOFileStreamDescriptor(strm));
}

#if defined(CLASP_MS_WINDOWS_HOST)
static int
isaconsole(int i) {
  HANDLE h = (HANDLE)_get_osfhandle(i);
  DWORD mode;
  return !!GetConsoleMode(h, &mode);
}
#define isatty isaconsole
#endif

static void
io_file_clear_input(T_sp strm) {
  int f = IOFileStreamDescriptor(strm);
#if defined(CLASP_MS_WINDOWS_HOST)
  if (isatty(f)) {
    /* Flushes Win32 console */
    if (!FlushConsoleInputBuffer((HANDLE)_get_osfhandle(f)))
      FEwin32_error("FlushConsoleInputBuffer() failed", 0);
    /* Do not stop here: the FILE structure needs also to be flushed */
  }
#endif
  while (file_listen(strm, f) == CLASP_LISTEN_AVAILABLE) {
    claspCharacter c = eformat_read_char(strm);
    if (c == EOF)
      return;
  }
}

#define io_file_clear_output generic_void
#define io_file_force_output generic_void
#define io_file_finish_output io_file_force_output

static int
io_file_interactive_p(T_sp strm) {
  int f = IOFileStreamDescriptor(strm);
  return isatty(f);
}

static T_sp
io_file_element_type(T_sp strm) {
  return FileStreamEltType(strm);
}

static T_sp
io_file_length(T_sp strm) {
  int f = IOFileStreamDescriptor(strm);
  T_sp output = clasp_file_len(f); // NIL or Integer_sp
  if (StreamByteSize(strm) != 8 && output.notnilp()) {
    cl_index bs = StreamByteSize(strm);
    Real_mv output_mv = clasp_floor2(gc::As_unsafe<Integer_sp>(output), make_fixnum(bs / 8));
    // and now lets use the calculated value
    output = output_mv;
    Fixnum_sp fn1 = gc::As<Fixnum_sp>(output_mv.valueGet_(1));
    unlikely_if(unbox_fixnum(fn1) != 0) {
      FEerror("File length is not on byte boundary", 0);
    }
  }
  return output;
}

static T_sp
io_file_get_position(T_sp strm) {
  int f = IOFileStreamDescriptor(strm);
  T_sp output;
  clasp_off_t offset;

  clasp_disable_interrupts();
  offset = lseek(f, 0, SEEK_CUR);
  clasp_enable_interrupts();
  unlikely_if(offset < 0)
      io_error(strm);
  if (sizeof(clasp_off_t) == sizeof(long)) {
    output = Integer_O::create((gctools::Fixnum)offset);
  } else {
    output = clasp_off_t_to_integer(offset);
  }
  {
    /* If there are unread octets, we return the position at which
             * these bytes begin! */
    T_sp l = StreamByteStack(strm);
    while ((l).consp()) {
      output = clasp_one_minus(gc::As<Number_sp>(output));
      l = oCdr(l);
    }
  }
  if (StreamByteSize(strm) != 8) {
    output = clasp_floor2(gc::As<Real_sp>(output), make_fixnum(StreamByteSize(strm) / 8));
  }
  return output;
}

static T_sp
io_file_set_position(T_sp strm, T_sp large_disp) {
  int f = IOFileStreamDescriptor(strm);
  clasp_off_t disp;
  int mode;
  if (large_disp.nilp()) {
    disp = 0;
    mode = SEEK_END;
  } else {
    if (StreamByteSize(strm) != 8) {
      large_disp = clasp_times(gc::As<Number_sp>(large_disp),
                               make_fixnum(StreamByteSize(strm) / 8));
    }
    disp = clasp_integer_to_off_t(large_disp);
    mode = SEEK_SET;
  }
  disp = lseek(f, disp, mode);
  return (disp == (clasp_off_t)-1) ? _Nil<T_O>() : _lisp->_true();
}

static int
io_file_column(T_sp strm) {
  return StreamOutputColumn(strm);
}

static T_sp
io_file_close(T_sp strm) {
  int f = IOFileStreamDescriptor(strm);
  int failed;
  unlikely_if(f == STDOUT_FILENO)
      FEerror("Cannot close the standard output", 0);
  unlikely_if(f == STDIN_FILENO)
      FEerror("Cannot close the standard input", 0);
  failed = safe_close(f);
  unlikely_if(failed < 0)
      cannot_close(strm);
  IOFileStreamDescriptor(strm) = -1;
  return generic_close(strm);
}

static cl_index
io_file_read_vector(T_sp strm, T_sp data, cl_index start, cl_index end) {
  return generic_read_vector(strm, data, start, end);
}

static cl_index
io_file_write_vector(T_sp strm, T_sp data, cl_index start, cl_index end) {
  return generic_write_vector(strm, data, start, end);
}

const FileOps io_file_ops = {
    io_file_write_byte8,
    io_file_read_byte8,

    generic_write_byte,
    generic_read_byte,

    eformat_read_char,
    eformat_write_char,
    eformat_unread_char,
    generic_peek_char,

    io_file_read_vector,
    io_file_write_vector,

    io_file_listen,
    io_file_clear_input,
    io_file_clear_output,
    io_file_finish_output,
    io_file_force_output,

    generic_always_true, /* input_p */
    generic_always_true, /* output_p */
    io_file_interactive_p,
    io_file_element_type,

    io_file_length,
    io_file_get_position,
    io_file_set_position,
    io_file_column,
    io_file_close};

const FileOps output_file_ops = {
    output_file_write_byte8,
    not_input_read_byte8,

    generic_write_byte,
    not_input_read_byte,

    not_input_read_char,
    eformat_write_char,
    not_input_unread_char,
    not_input_read_char,

    generic_read_vector,
    io_file_write_vector,

    not_input_listen,
    not_input_clear_input,
    io_file_clear_output,
    io_file_finish_output,
    io_file_force_output,

    generic_always_false, /* input_p */
    generic_always_true,  /* output_p */
    generic_always_false,
    io_file_element_type,

    io_file_length,
    io_file_get_position,
    io_file_set_position,
    io_file_column,
    io_file_close};

const FileOps input_file_ops = {
    not_output_write_byte8,
    io_file_read_byte8,

    not_output_write_byte,
    generic_read_byte,

    eformat_read_char,
    not_output_write_char,
    eformat_unread_char,
    generic_peek_char,

    io_file_read_vector,
    generic_write_vector,

    io_file_listen,
    io_file_clear_input,
    not_output_clear_output,
    not_output_finish_output,
    not_output_force_output,

    generic_always_true,  /* input_p */
    generic_always_false, /* output_p */
    io_file_interactive_p,
    io_file_element_type,

    io_file_length,
    io_file_get_position,
    io_file_set_position,
    generic_column,
    io_file_close};



SYMBOL_EXPORT_SC_(KeywordPkg,utf_8);
SYMBOL_EXPORT_SC_(KeywordPkg,ucs_2);
SYMBOL_EXPORT_SC_(KeywordPkg,ucs_2be);
SYMBOL_EXPORT_SC_(KeywordPkg,ucs_2le) 
SYMBOL_EXPORT_SC_(KeywordPkg,ucs_4);
SYMBOL_EXPORT_SC_(KeywordPkg,ucs_4be);
SYMBOL_EXPORT_SC_(KeywordPkg,ucs_4le);
SYMBOL_EXPORT_SC_(KeywordPkg,iso_8859_1);
SYMBOL_EXPORT_SC_(KeywordPkg,latin_1);
SYMBOL_EXPORT_SC_(KeywordPkg,us_ascii);
SYMBOL_EXPORT_SC_(ExtPkg,make_encoding);

static int
parse_external_format(T_sp tstream, T_sp format, int flags) {
  Stream_sp stream = gc::As<Stream_sp>(tstream);
  if (format == kw::_sym_default) {
    format = ext::_sym_STARdefault_external_formatSTAR->symbolValue();
  }
  if ((format).consp()) {
    flags = parse_external_format(stream, oCdr(format), flags);
    format = oCar(format);
  }
  if (format == _lisp->_true()) {
#ifdef CLASP_UNICODE
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_UTF_8;
#else
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_DEFAULT_FORMAT;
#endif
  }
  if (format == _Nil<T_O>()) {
    return flags;
  }
  if (format == kw::_sym_cr) {
    return (flags | CLASP_STREAM_CR) & ~CLASP_STREAM_LF;
  }
  if (format == kw::_sym_lf) {
    return (flags | CLASP_STREAM_LF) & ~CLASP_STREAM_CR;
  }
  if (format == kw::_sym_crlf) {
    return flags | (CLASP_STREAM_CR + CLASP_STREAM_LF);
  }
  if (format == kw::_sym_littleEndian) {
    return flags | CLASP_STREAM_LITTLE_ENDIAN;
  }
  if (format == kw::_sym_bigEndian) {
    return flags & ~CLASP_STREAM_LITTLE_ENDIAN;
  }
  if (format == kw::_sym_passThrough) {
#ifdef CLASP_UNICODE
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_LATIN_1;
#else
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_DEFAULT_FORMAT;
#endif
  }
#ifdef CLASP_UNICODE
PARSE_SYMBOLS:
  if (format == kw::_sym_utf_8) {
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_UTF_8;
  }
  if (format == kw::_sym_ucs_2) {
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_UCS_2;
  }
  if (format == kw::_sym_ucs_2be) {
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_UCS_2BE;
  }
  if (format == kw::_sym_ucs_2le) {
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_UCS_2LE;
  }
  if (format == kw::_sym_ucs_4) {
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_UCS_4;
  }
  if (format == kw::_sym_ucs_4be) {
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_UCS_4BE;
  }
  if (format == kw::_sym_ucs_4le) {
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_UCS_4LE;
  }
  if (format == kw::_sym_iso_8859_1) {
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_ISO_8859_1;
  }
  if (format == kw::_sym_latin_1) {
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_LATIN_1;
  }
  if (format == kw::_sym_us_ascii) {
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_US_ASCII;
  }
  if (gc::IsA<HashTable_sp>(format)) {
    stream->_FormatTable = format;
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_USER_FORMAT;
  }
  if (gc::IsA<Symbol_sp>(format)) {
    ASSERT(format.notnilp());
    format = eval::funcall(ext::_sym_make_encoding, format);
    if (gc::IsA<Symbol_sp>(format)) goto PARSE_SYMBOLS;
    stream->_FormatTable = format;
    return (flags & ~CLASP_STREAM_FORMAT) | CLASP_STREAM_USER_FORMAT;
  }
#endif
  FEerror("Unknown or unsupported external format: ~A", 1, format.raw_());
  return CLASP_STREAM_DEFAULT_FORMAT;
}

static void
set_stream_elt_type(T_sp tstream, gctools::Fixnum byte_size, int flags,
                    T_sp external_format) {
  T_sp t;
  Stream_sp stream = gc::As<Stream_sp>(tstream);
  if (byte_size < 0) {
    byte_size = -byte_size;
    flags |= CLASP_STREAM_SIGNED_BYTES;
    t = cl::_sym_SignedByte;
  } else {
    flags &= ~CLASP_STREAM_SIGNED_BYTES;
    t = cl::_sym_UnsignedByte;
  }
  flags = parse_external_format(stream, external_format, flags);
  StreamOps(stream).read_char = eformat_read_char;
  StreamOps(stream).write_char = eformat_write_char;
  switch (flags & CLASP_STREAM_FORMAT) {
  case CLASP_STREAM_BINARY:
    // e.g. (T size) is not a valid type, use (UnsignedByte size)
    // This is better than (T Size), but not necesarily the right type
    // Probably the value of the vriable t was meant, use it now!
    FileStreamEltType(stream) = Cons_O::createList(t, make_fixnum(byte_size));
    StreamFormat(stream) = t;
    StreamOps(stream).read_char = not_character_read_char;
    StreamOps(stream).write_char = not_character_write_char;
    break;
#ifdef CLASP_UNICODE
  /*case ECL_ISO_8859_1:*/
  case CLASP_STREAM_LATIN_1:
    FileStreamEltType(stream) = cl::_sym_base_char;
    byte_size = 8;
    StreamFormat(stream) = kw::_sym_latin_1;
    stream->_Encoder = passthrough_encoder;
    StreamDecoder(stream) = passthrough_decoder;
    break;
  case CLASP_STREAM_UTF_8:
    FileStreamEltType(stream) = cl::_sym_character;
    byte_size = 8;
    StreamFormat(stream) = kw::_sym_utf_8;
    stream->_Encoder = utf_8_encoder;
    StreamDecoder(stream) = utf_8_decoder;
    break;
  case CLASP_STREAM_UCS_2:
    FileStreamEltType(stream) = cl::_sym_character;
    byte_size = 8 * 2;
    StreamFormat(stream) = kw::_sym_ucs_2;
    stream->_Encoder = ucs_2_encoder;
    StreamDecoder(stream) = ucs_2_decoder;
    break;
  case CLASP_STREAM_UCS_2BE:
    FileStreamEltType(stream) = cl::_sym_character;
    byte_size = 8 * 2;
    if (flags & CLASP_STREAM_LITTLE_ENDIAN) {
      StreamFormat(stream) = kw::_sym_ucs_2le;
      stream->_Encoder = ucs_2le_encoder;
      StreamDecoder(stream) = ucs_2le_decoder;
    } else {
      StreamFormat(stream) = kw::_sym_ucs_2be;
      stream->_Encoder = ucs_2be_encoder;
      StreamDecoder(stream) = ucs_2be_decoder;
    }
    break;
  case CLASP_STREAM_UCS_4:
    FileStreamEltType(stream) = cl::_sym_character;
    byte_size = 8 * 4;
    StreamFormat(stream) = kw::_sym_ucs_4be;
    stream->_Encoder = ucs_4_encoder;
    StreamDecoder(stream) = ucs_4_decoder;
    break;
  case CLASP_STREAM_UCS_4BE:
    FileStreamEltType(stream) = cl::_sym_character;
    byte_size = 8 * 4;
    if (flags & CLASP_STREAM_LITTLE_ENDIAN) {
      StreamFormat(stream) = kw::_sym_ucs_4le;
      stream->_Encoder = ucs_4le_encoder;
      StreamDecoder(stream) = ucs_4le_decoder;
    } else {
      StreamFormat(stream) = kw::_sym_ucs_4be;
      stream->_Encoder = ucs_4be_encoder;
      StreamDecoder(stream) = ucs_4be_decoder;
    }
    break;
  case CLASP_STREAM_USER_FORMAT:
    FileStreamEltType(stream) = cl::_sym_character;
    byte_size = 8;
    StreamFormat(stream) = stream->_FormatTable;
    if ((StreamFormat(stream)).consp()) {
      stream->_Encoder = user_multistate_encoder;
      StreamDecoder(stream) = user_multistate_decoder;
    } else {
      stream->_Encoder = user_encoder;
      StreamDecoder(stream) = user_decoder;
    }
    break;
  case CLASP_STREAM_US_ASCII:
    FileStreamEltType(stream) = cl::_sym_base_char;
    byte_size = 8;
    StreamFormat(stream) = kw::_sym_us_ascii;
    StreamEncoder(stream) = ascii_encoder;
    StreamDecoder(stream) = ascii_decoder;
    break;
#else
  case CLASP_STREAM_DEFAULT_FORMAT:
    FileStreamEltType(stream) = cl::_sym_base_char;
    byte_size = 8;
    StreamFormat(stream) = kw::_sym_passThrough;
    StreamEncoder(stream) = passthrough_encoder;
    StreamDecoder(stream) = passthrough_decoder;
    break;
#endif
  default:
    FEerror("Invalid or unsupported external format ~A with code ~D",
            2, external_format.raw_(), make_fixnum(flags).raw_());
  }
  t = kw::_sym_lf;
  if (StreamOps(stream).write_char == eformat_write_char &&
      (flags & CLASP_STREAM_CR)) {
    if (flags & CLASP_STREAM_LF) {
      StreamOps(stream).read_char = eformat_read_char_crlf;
      StreamOps(stream).write_char = eformat_write_char_crlf;
      t = kw::_sym_crlf;
    } else {
      StreamOps(stream).read_char = eformat_read_char_cr;
      StreamOps(stream).write_char = eformat_write_char_cr;
      t = kw::_sym_cr;
    }
  }
  StreamFormat(stream) = Cons_O::createList(StreamFormat(stream), t);
  {
    T_sp (*read_byte)(T_sp);
    void (*write_byte)(T_sp, T_sp);
    byte_size = (byte_size + 7) & (~(gctools::Fixnum)7);
    if (byte_size == 8) {
      if (flags & CLASP_STREAM_SIGNED_BYTES) {
        read_byte = generic_read_byte_signed8;
        write_byte = generic_write_byte_signed8;
      } else {
        read_byte = generic_read_byte_unsigned8;
        write_byte = generic_write_byte_unsigned8;
      }
    } else if (flags & CLASP_STREAM_LITTLE_ENDIAN) {
      read_byte = generic_read_byte_le;
      write_byte = generic_write_byte_le;
    } else {
      read_byte = generic_read_byte;
      write_byte = generic_write_byte;
    }
    if (clasp_input_stream_p(stream)) {
      StreamOps(stream).read_byte = read_byte;
    }
    if (clasp_output_stream_p(stream)) {
      StreamOps(stream).write_byte = write_byte;
    }
  }
  StreamFlags(stream) = flags;
  StreamByteSize(stream) = byte_size;
}

void si_stream_external_format_set(T_sp stream, T_sp format) {
  if (Instance_sp instance = stream.asOrNull<Instance_O>()) {
    (void)instance;
    FEerror("Cannot change external format of stream ~A", 1, stream.raw_());
  }
  switch (StreamMode(stream)) {
  case clasp_smm_input:
  case clasp_smm_input_file:
  case clasp_smm_output:
  case clasp_smm_output_file:
  case clasp_smm_io:
  case clasp_smm_io_file:
#ifdef ECL_WSOCK
  case clasp_smm_input_wsock:
  case clasp_smm_output_wsock:
  case clasp_smm_io_wsock:
  case clasp_smm_io_wcon:
#endif
  {
    T_sp elt_type = clasp_stream_element_type(stream);
    unlikely_if(elt_type != cl::_sym_character &&
                elt_type != cl::_sym_base_char)
        FEerror("Cannot change external format"
                "of binary stream ~A",
                1, stream.raw_());
    set_stream_elt_type(stream, StreamByteSize(stream),
                        StreamFlags(stream), format);
  } break;
  default:
    FEerror("Cannot change external format of stream ~A", 1, stream.raw_());
  }
  return;
}

T_sp clasp_make_file_stream_from_fd(T_sp fname, int fd, enum StreamMode smm,
                                    gctools::Fixnum byte_size, int flags, T_sp external_format) {
  T_sp stream = IOFileStream_O::create();
  switch (smm) {
  case clasp_smm_input:
    smm = clasp_smm_input_file;
  case clasp_smm_input_file:
  case clasp_smm_probe:
    StreamOps(stream) = duplicate_dispatch_table(input_file_ops);
    break;
  case clasp_smm_output:
    smm = clasp_smm_output_file;
  case clasp_smm_output_file:
    StreamOps(stream) = duplicate_dispatch_table(output_file_ops);
    break;
  case clasp_smm_io:
    smm = clasp_smm_io_file;
  case clasp_smm_io_file:
    StreamOps(stream) = duplicate_dispatch_table(io_file_ops);
    break;
  default:
      FEerror("make_stream: wrong mode in clasp_make_file_stream_from_fd smm = ~d", 1, clasp_make_fixnum(smm).raw_());
  }
  StreamMode(stream) = smm;
  StreamClosed(stream) = 0;
  set_stream_elt_type(stream, byte_size, flags, external_format);
  FileStreamFilename(stream) = fname; /* not really used */
  StreamOutputColumn(stream) = 0;
  IOFileStreamDescriptor(stream) = fd;
  StreamLastOp(stream) = 0;
  //	si_set_finalizer(stream, _lisp->_true());
  return stream;
}

/**********************************************************************
 * C STREAMS
 */

static cl_index
input_stream_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  unlikely_if(StreamByteStack(strm).notnilp()) { //  != _Nil<T_O>()) {
    return consume_byte_stack(strm, c, n);
  }
  else {
    FILE *f = IOStreamStreamFile(strm);
    gctools::Fixnum out = 0;
    clasp_disable_interrupts();
    do {
      out = fread(c, sizeof(char), n, f);
    } while (out < n && ferror(f) && restartable_io_error(strm, "fread"));
    clasp_enable_interrupts();
    return out;
  }
}

static cl_index
output_stream_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  cl_index out;
  clasp_disable_interrupts();
  do {
    out = fwrite(c, sizeof(char), n, IOStreamStreamFile(strm));
  } while (out < n && restartable_io_error(strm, "fwrite"));
  clasp_enable_interrupts();
  return out;
}

static cl_index
io_stream_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  /* When using the same stream for input and output operations, we have to
	 * use some file position operation before reading again. Besides this, if
	 * there were unread octets, we have to move to the position at the
	 * begining of them.
	 */
  if (StreamByteStack(strm).notnilp()) { //  != _Nil<T_O>()) {
    T_sp aux = clasp_file_position(strm);
    if (!aux.nilp())
      clasp_file_position_set(strm, aux);
  } else if (StreamLastOp(strm) > 0) {
    clasp_fseeko(IOStreamStreamFile(strm), 0, SEEK_CUR);
  }
  StreamLastOp(strm) = -1;
  return output_stream_write_byte8(strm, c, n);
}

static void io_stream_force_output(T_sp strm);

static cl_index
io_stream_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  /* When using the same stream for input and output operations, we have to
	 * flush the stream before reading.
	 */
  if (StreamLastOp(strm) < 0) {
    io_stream_force_output(strm);
  }
  StreamLastOp(strm) = +1;
  return input_stream_read_byte8(strm, c, n);
}

static int
io_stream_listen(T_sp strm) {
  if (StreamByteStack(strm).notnilp()) // != _Nil<T_O>())
    return CLASP_LISTEN_AVAILABLE;
  return flisten(strm, IOStreamStreamFile(strm));
}

static void
io_stream_clear_input(T_sp strm) {
  FILE *fp = IOStreamStreamFile(strm);
#if defined(CLASP_MS_WINDOWS_HOST)
  int f = fileno(fp);
  if (isatty(f)) {
    /* Flushes Win32 console */
    unlikely_if(!FlushConsoleInputBuffer((HANDLE)_get_osfhandle(f)))
        FEwin32_error("FlushConsoleInputBuffer() failed", 0);
    /* Do not stop here: the FILE structure needs also to be flushed */
  }
#endif
  while (flisten(strm, fp) == CLASP_LISTEN_AVAILABLE) {
    clasp_disable_interrupts();
    getc(fp);
    clasp_enable_interrupts();
  }
}

#define io_stream_clear_output generic_void

static void
io_stream_force_output(T_sp strm) {
  FILE *f = IOStreamStreamFile(strm);
  clasp_disable_interrupts();
  while ((fflush(f) == EOF) && restartable_io_error(strm, "fflush"))
    (void)0;
  clasp_enable_interrupts();
}

#define io_stream_finish_output io_stream_force_output

static int
io_stream_interactive_p(T_sp strm) {
  FILE *f = IOStreamStreamFile(strm);
  return isatty(fileno(f));
}

static T_sp
io_stream_length(T_sp strm) {
  FILE *f = IOStreamStreamFile(strm);
  T_sp output = clasp_file_len(fileno(f)); // NIL or Integer_sp
  if (StreamByteSize(strm) != 8 && output.notnilp()) {
    //            const cl_env_ptr the_env = clasp_process_env();
    cl_index bs = StreamByteSize(strm);
    T_mv output_mv = clasp_floor2(gc::As_unsafe<Integer_sp>(output), make_fixnum(bs / 8));
     // and now lets use the calculated value
    output = output_mv;
    Fixnum_sp ofn1 = gc::As<Fixnum_sp>(output_mv.valueGet_(1));
    Fixnum fn = unbox_fixnum(ofn1);
    unlikely_if(fn != 0) {
      FEerror("File length is not on byte boundary", 0);
    }
  }
  return output;
}

static T_sp
io_stream_get_position(T_sp strm) {
  FILE *f = IOStreamStreamFile(strm);
  T_sp output;
  clasp_off_t offset;

  clasp_disable_interrupts();
  offset = clasp_ftello(f);
  clasp_enable_interrupts();
  if (offset < 0) {
    return make_fixnum(0);
    //io_error(strm);
  }
  if (sizeof(clasp_off_t) == sizeof(long)) {
    output = Integer_O::create((gctools::Fixnum)offset);
  } else {
    output = clasp_off_t_to_integer(offset);
  }
  {
    /* If there are unread octets, we return the position at which
             * these bytes begin! */
    T_sp l = StreamByteStack(strm);
    while ((l).consp()) {
      output = clasp_one_minus(gc::As<Integer_sp>(output));
      l = oCdr(l);
    }
  }
  if (StreamByteSize(strm) != 8) {
    output = clasp_floor2(gc::As<Integer_sp>(output), make_fixnum(StreamByteSize(strm) / 8));
  }
  return output;
}

static T_sp
io_stream_set_position(T_sp strm, T_sp large_disp) {
  FILE *f = IOStreamStreamFile(strm);
  clasp_off_t disp;
  int mode;
  if (large_disp.nilp()) {
    disp = 0;
    mode = SEEK_END;
  } else {
    if (StreamByteSize(strm) != 8) {
      large_disp = clasp_times(gc::As<Integer_sp>(large_disp),
                               make_fixnum(StreamByteSize(strm) / 8));
    }
    disp = clasp_integer_to_off_t(large_disp);
    mode = SEEK_SET;
  }
  clasp_disable_interrupts();
  mode = clasp_fseeko(f, disp, mode);
  clasp_enable_interrupts();
  return mode ? _Nil<T_O>() : _lisp->_true();
}

static int
io_stream_column(T_sp strm) {
  return StreamOutputColumn(strm);
}

static T_sp
io_stream_close(T_sp strm) {
  FILE *f = IOStreamStreamFile(strm);
  int failed;
  unlikely_if(f == stdout)
      FEerror("Cannot close the standard output", 0);
  unlikely_if(f == stdin)
      FEerror("Cannot close the standard input", 0);
  unlikely_if(f == NULL)
      wrong_file_handler(strm);
  if (clasp_output_stream_p(strm)) {
    clasp_force_output(strm);
  }
  failed = safe_fclose(f);
  unlikely_if(failed)
      cannot_close(strm);
  gctools::clasp_dealloc(StreamBuffer(strm));
  StreamBuffer(strm) = NULL;
  IOStreamStreamFile(strm) = NULL;
  return generic_close(strm);
}

/*
 * Specialized sequence operations
 */

#define io_stream_read_vector io_file_read_vector
#define io_stream_write_vector io_file_write_vector

const FileOps io_stream_ops = {
    io_stream_write_byte8,
    io_stream_read_byte8,

    generic_write_byte,
    generic_read_byte,

    eformat_read_char,
    eformat_write_char,
    eformat_unread_char,
    generic_peek_char,

    io_file_read_vector,
    io_file_write_vector,

    io_stream_listen,
    io_stream_clear_input,
    io_stream_clear_output,
    io_stream_finish_output,
    io_stream_force_output,

    generic_always_true, /* input_p */
    generic_always_true, /* output_p */
    io_stream_interactive_p,
    io_file_element_type,

    io_stream_length,
    io_stream_get_position,
    io_stream_set_position,
    io_stream_column,
    io_stream_close};

const FileOps output_stream_ops = {
    output_stream_write_byte8,
    not_input_read_byte8,

    generic_write_byte,
    not_input_read_byte,

    not_input_read_char,
    eformat_write_char,
    not_input_unread_char,
    not_input_read_char,

    generic_read_vector,
    io_file_write_vector,

    not_input_listen,
    generic_void,
    io_stream_clear_output,
    io_stream_finish_output,
    io_stream_force_output,

    generic_always_false, /* input_p */
    generic_always_true,  /* output_p */
    generic_always_false,
    io_file_element_type,

    io_stream_length,
    io_stream_get_position,
    io_stream_set_position,
    io_stream_column,
    io_stream_close};

const FileOps input_stream_ops = {
    not_output_write_byte8,
    input_stream_read_byte8,

    not_output_write_byte,
    generic_read_byte,

    eformat_read_char,
    not_output_write_char,
    eformat_unread_char,
    generic_peek_char,

    io_file_read_vector,
    generic_write_vector,

    io_stream_listen,
    io_stream_clear_input,
    generic_void,
    generic_void,
    generic_void,

    generic_always_true,  /* input_p */
    generic_always_false, /* output_p */
    io_stream_interactive_p,
    io_file_element_type,

    io_stream_length,
    io_stream_get_position,
    io_stream_set_position,
    generic_column,
    io_stream_close};

/**********************************************************************
 * WINSOCK STREAMS
 */

#if defined(ECL_WSOCK)

#define winsock_stream_element_type io_file_element_type

static cl_index
winsock_stream_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  cl_index len = 0;

  unlikely_if(StreamByteStack(strm).notnilp()) { //  != _Nil<T_O>()) {
    return consume_byte_stack(strm, c, n);
  }
  if (n > 0) {
    SOCKET s = (SOCKET)IOFileStreamDescriptor(strm);
    unlikely_if(INVALID_SOCKET == s) {
      wrong_file_handler(strm);
    }
    else {
      clasp_disable_interrupts();
      len = recv(s, c, n, 0);
      unlikely_if(len == SOCKET_ERROR)
          wsock_error("Cannot read bytes from Windows "
                      "socket ~S.~%~A",
                      strm);
      clasp_enable_interrupts();
    }
  }
  return (len > 0) ? len : EOF;
}

static cl_index
winsock_stream_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  cl_index out = 0;
  unsigned char *endp;
  unsigned char *p;
  SOCKET s = (SOCKET)IOFileStreamDescriptor(strm);
  unlikely_if(INVALID_SOCKET == s) {
    wrong_file_handler(strm);
  }
  else {
    clasp_disable_interrupts();
    do {
      cl_index res = send(s, c + out, n, 0);
      unlikely_if(res == SOCKET_ERROR) {
        wsock_error("Cannot write bytes to Windows"
                    " socket ~S.~%~A",
                    strm);
        break; /* stop writing */
      }
      else {
        out += res;
        n -= res;
      }
    } while (n > 0);
    clasp_enable_interrupts();
  }
  return out;
}

static int
winsock_stream_listen(T_sp strm) {
  SOCKET s;
  unlikely_if(StreamByteStack(strm).notnilp()) { // != _Nil<T_O>()) {
    return CLASP_LISTEN_AVAILABLE;
  }
  s = (SOCKET)IOFileStreamDescriptor(strm);
  unlikely_if(INVALID_SOCKET == s) {
    wrong_file_handler(strm);
  }
  {
    struct timeval tv = {0, 0};
    fd_set fds;
    cl_index result;

    FD_ZERO(&fds);
    FD_SET(s, &fds);
    clasp_disable_interrupts();
    result = select(0, &fds, NULL, NULL, &tv);
    unlikely_if(result == SOCKET_ERROR)
        wsock_error("Cannot listen on Windows "
                    "socket ~S.~%~A",
                    strm);
    clasp_enable_interrupts();
    return (result > 0
                ? CLASP_LISTEN_AVAILABLE
                : CLASP_LISTEN_NO_CHAR);
  }
}

static void
winsock_stream_clear_input(T_sp strm) {
  while (winsock_stream_listen(strm) == CLASP_LISTEN_AVAILABLE) {
    eformat_read_char(strm);
  }
}

static T_sp
winsock_stream_close(T_sp strm) {
  SOCKET s = (SOCKET)IOFileStreamDescriptor(strm);
  int failed;
  clasp_disable_interrupts();
  failed = closesocket(s);
  clasp_enable_interrupts();
  unlikely_if(failed < 0)
      cannot_close(strm);
  IOFileStreamDescriptor(strm) = (int)INVALID_SOCKET;
  return generic_close(strm);
}

const FileOps winsock_stream_io_ops = {
    winsock_stream_write_byte8,
    winsock_stream_read_byte8,

    generic_write_byte,
    generic_read_byte,

    eformat_read_char,
    eformat_write_char,
    eformat_unread_char,
    generic_peek_char,

    generic_read_vector,
    generic_write_vector,

    winsock_stream_listen,
    winsock_stream_clear_input,
    generic_void,
    generic_void,
    generic_void,

    generic_always_true, /* input_p */
    generic_always_true, /* output_p */
    generic_always_false,
    winsock_stream_element_type,

    not_a_file_stream,
    not_implemented_get_position,
    not_implemented_set_position,
    generic_column,

    winsock_stream_close};

const FileOps winsock_stream_output_ops = {
    winsock_stream_write_byte8,
    not_input_read_byte8,

    generic_write_byte,
    not_input_read_byte,

    not_input_read_char,
    eformat_write_char,
    not_input_unread_char,
    generic_peek_char,

    generic_read_vector,
    generic_write_vector,

    not_input_listen,
    not_input_clear_input,
    generic_void,
    generic_void,
    generic_void,

    generic_always_false, /* input_p */
    generic_always_true,  /* output_p */
    generic_always_false,
    winsock_stream_element_type,

    not_a_file_stream,
    not_implemented_get_position,
    not_implemented_set_position,
    generic_column,

    winsock_stream_close};

const FileOps winsock_stream_input_ops = {
    not_output_write_byte8,
    winsock_stream_read_byte8,

    not_output_write_byte,
    generic_read_byte,

    eformat_read_char,
    not_output_write_char,
    eformat_unread_char,
    generic_peek_char,

    generic_read_vector,
    generic_write_vector,

    winsock_stream_listen,
    winsock_stream_clear_input,
    not_output_clear_output,
    not_output_finish_output,
    not_output_force_output,

    generic_always_true,  /* input_p */
    generic_always_false, /* output_p */
    generic_always_false,
    winsock_stream_element_type,

    not_a_file_stream,
    not_implemented_get_position,
    not_implemented_set_position,
    generic_column,

    winsock_stream_close};
#endif

/**********************************************************************
 * WINCONSOLE STREAM
 */

#if defined(CLASP_MS_WINDOWS_HOST)

#define wcon_stream_element_type io_file_element_type

static cl_index
wcon_stream_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  unlikely_if(StreamByteStack(strm).notnilp()) { // != _Nil<T_O>()) {
    return consume_byte_stack(strm, c, n);
  }
  else {
    cl_index len = 0;
    cl_env_ptr the_env = clasp_process_env();
    HANDLE h = (HANDLE)IOFileStreamDescriptor(strm);
    DWORD nchars;
    unsigned char aux[4];
    for (len = 0; len < n;) {
      int i, ok;
      clasp_disable_interrupts_env(the_env);
      ok = ReadConsole(h, &aux, 1, &nchars, NULL);
      clasp_enable_interrupts_env(the_env);
      unlikely_if(!ok) {
        FEwin32_error("Cannot read from console", 0);
      }
      for (i = 0; i < nchars; i++) {
        if (len < n) {
          c[len++] = aux[i];
        } else {
          StreamByteStack(strm) =
              clasp_nconc(StreamByteStack(strm),
                          clasp_list1(make_fixnum(aux[i])));
        }
      }
    }
    return (len > 0) ? len : EOF;
  }
}

static cl_index
wcon_stream_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  HANDLE h = (HANDLE)IOFileStreamDescriptor(strm);
  DWORD nchars;
  unlikely_if(!WriteConsole(h, c, n, &nchars, NULL)) {
    FEwin32_error("Cannot write to console.", 0);
  }
  return nchars;
}

static int
wcon_stream_listen(T_sp strm) {
  HANDLE h = (HANDLE)IOFileStreamDescriptor(strm);
  INPUT_RECORD aux;
  DWORD nevents;
  do {
    unlikely_if(!PeekConsoleInput(h, &aux, 1, &nevents))
        FEwin32_error("Cannot read from console.", 0);
    if (nevents == 0)
      return 0;
    if (aux.EventType == KEY_EVENT)
      return 1;
    unlikely_if(!ReadConsoleInput(h, &aux, 1, &nevents))
        FEwin32_error("Cannot read from console.", 0);
  } while (1);
}

static void
wcon_stream_clear_input(T_sp strm) {
  FlushConsoleInputBuffer((HANDLE)IOFileStreamDescriptor(strm));
}

static void
wcon_stream_force_output(T_sp strm) {
  DWORD nchars;
  WriteConsole((HANDLE)IOFileStreamDescriptor(strm), 0, 0, &nchars, NULL);
}

const FileOps wcon_stream_io_ops = {
    wcon_stream_write_byte8,
    wcon_stream_read_byte8,

    generic_write_byte,
    generic_read_byte,

    eformat_read_char,
    eformat_write_char,
    eformat_unread_char,
    generic_peek_char,

    generic_read_vector,
    generic_write_vector,

    wcon_stream_listen,
    wcon_stream_clear_input,
    generic_void,
    wcon_stream_force_output,
    wcon_stream_force_output,

    generic_always_true, /* input_p */
    generic_always_true, /* output_p */
    generic_always_false,
    wcon_stream_element_type,

    not_a_file_stream,
    not_implemented_get_position,
    not_implemented_set_position,
    generic_column,

    generic_close,
};

#define CONTROL_Z 26

static T_sp
maybe_make_windows_console_FILE(T_sp fname, FILE *f, StreamMode smm,
                                gctools::Fixnum byte_size, int flags,
                                T_sp external_format) {
  int desc = fileno(f);
  T_sp output;
  if (isatty(desc)) {
    output = clasp_make_stream_from_FILE(fname,
                                         (void *)_get_osfhandle(desc),
                                         clasp_smm_io_wcon,
                                         byte_size, flags,
                                         external_format);
    StreamEofChar(output) = CONTROL_Z;
  } else {
    output = clasp_make_stream_from_FILE(fname, f, smm, byte_size, flags,
                                         external_format);
  }
  return output;
}

static T_sp
maybe_make_windows_console_fd(T_sp fname, int desc, StreamMode smm,
                              gctools::Fixnum byte_size, int flags,
                              T_sp external_format) {
  T_sp output;
  if (isatty(desc)) {
    output = clasp_make_stream_from_FILE(fname,
                                         (void *)_get_osfhandle(desc),
                                         clasp_smm_io_wcon,
                                         byte_size, flags,
                                         external_format);
    StreamEofChar(output) = CONTROL_Z;
  } else {
    /* Windows changes the newline characters for \r\n
             * even when using read()/write() */
    if (clasp_option_values[ECL_OPT_USE_SETMODE_ON_FILES]) {
      _setmode(desc, _O_BINARY);
    } else {
      external_format = oCdr(external_format);
    }
    output = clasp_make_file_stream_from_fd(fname, desc, smm,
                                            byte_size, flags,
                                            external_format);
  }
  return output;
}
#else
#define maybe_make_windows_console_FILE clasp_make_stream_from_FILE
#define maybe_make_windows_console_fd clasp_make_file_stream_from_fd
#endif

CL_LAMBDA(stream mode);
CL_DECLARE();
CL_DOCSTRING("set-buffering-mode");
CL_DEFUN 
T_sp core__set_buffering_mode(T_sp stream, T_sp buffer_mode_symbol) {
  enum StreamMode mode = StreamMode(stream);
  int buffer_mode;

  unlikely_if(!AnsiStreamP(stream)) {
    FEerror("Cannot set buffer of ~A", 1, stream.raw_());
  }

  if (buffer_mode_symbol == kw::_sym_none || buffer_mode_symbol.nilp())
    buffer_mode = _IONBF;
  else if (buffer_mode_symbol == kw::_sym_line || buffer_mode_symbol == kw::_sym_line_buffered)
    buffer_mode = _IOLBF;
  else if (buffer_mode_symbol == kw::_sym_full || buffer_mode_symbol == kw::_sym_fully_buffered)
    buffer_mode = _IOFBF;
  else
    FEerror("Not a valid buffering mode: ~A", 1, buffer_mode_symbol.raw_());

  if (mode == clasp_smm_output || mode == clasp_smm_io || mode == clasp_smm_input) {
    FILE *fp = IOStreamStreamFile(stream);

    if (buffer_mode != _IONBF) {
      cl_index buffer_size = BUFSIZ;
      char *new_buffer = gctools::clasp_alloc_atomic(buffer_size);
      StreamBuffer(stream) = new_buffer;
      setvbuf(fp, new_buffer, buffer_mode, buffer_size);
    } else
      setvbuf(fp, NULL, _IONBF, 0);
  }
  return stream;
}


T_sp clasp_make_stream_from_FILE(T_sp fname, FILE *f, enum StreamMode smm,
                                 gctools::Fixnum byte_size, int flags, T_sp external_format) {
  T_sp stream;
  stream = IOStreamStream_O::create();
  StreamMode(stream) = smm;
  StreamClosed(stream) = 0;
  switch (smm) {
  case clasp_smm_io:
    StreamOps(stream) = duplicate_dispatch_table(io_stream_ops);
    break;
  case clasp_smm_probe:
  case clasp_smm_input:
    StreamOps(stream) = duplicate_dispatch_table(input_stream_ops);
    break;
  case clasp_smm_output:
    StreamOps(stream) = duplicate_dispatch_table(output_stream_ops);
    break;
#if defined(ECL_WSOCK)
  case clasp_smm_input_wsock:
    StreamOps(stream) = duplicate_dispatch_table(winsock_stream_input_ops);
    break;
  case clasp_smm_output_wsock:
    StreamOps(stream) = duplicate_dispatch_table(winsock_stream_output_ops);
    break;
  case clasp_smm_io_wsock:
    StreamOps(stream) = duplicate_dispatch_table(winsock_stream_io_ops);
    break;
  case clasp_smm_io_wcon:
    StreamOps(stream) = duplicate_dispatch_table(wcon_stream_io_ops);
    break;
#endif
  default:
    FEerror("Not a valid mode ~D for clasp_make_stream_from_FILE", 1, make_fixnum(smm).raw_());
  }
  set_stream_elt_type(stream, byte_size, flags, external_format);
  FileStreamFilename(stream) = fname; /* not really used */
  StreamOutputColumn(stream) = 0;
  IOStreamStreamFile(stream) = reinterpret_cast<FILE *>(f);
  StreamLastOp(stream) = 0;
  return stream;
}

T_sp clasp_make_stream_from_fd(T_sp fname, int fd, enum StreamMode smm,
                               gctools::Fixnum byte_size, int flags, T_sp external_format) {
  const char *mode; /* file open mode */
  FILE *fp;         /* file pointer */
  switch (smm) {
  case clasp_smm_input:
    mode = OPEN_R;
    break;
  case clasp_smm_output:
    mode = OPEN_W;
    break;
  case clasp_smm_io:
    mode = OPEN_RW;
    break;
#if defined(ECL_WSOCK)
  case clasp_smm_input_wsock:
  case clasp_smm_output_wsock:
  case clasp_smm_io_wsock:
  case clasp_smm_io_wcon:
    break;
#endif
  default:
    mode = OPEN_R; // dummy
    FEerror("make_stream: wrong mode in clasp_make_stream_from_fd smm = ~d", 1, clasp_make_fixnum(smm).raw_());
  }
#if defined(ECL_WSOCK)
  if (smm == clasp_smm_input_wsock || smm == clasp_smm_output_wsock || smm == clasp_smm_io_wsock || smm == clasp_smm_io_wcon)
    fp = (FILE *)fd;
  else
    fp = safe_fdopen(fd, mode);
#else
  fp = safe_fdopen(fd, mode);
#endif
  if (fp == NULL) {
    FElibc_error("Unable to create stream for file descriptor ~D",
                 1, Integer_O::create((gc::Fixnum)fd).raw_());
  }
  return clasp_make_stream_from_FILE(fname, fp, smm, byte_size, flags,
                                     external_format);
}

int clasp_stream_to_handle(T_sp s, bool output) {
BEGIN:
  if (UNLIKELY(!AnsiStreamP(s)))
    return -1;
  switch (StreamMode(s)) {
  case clasp_smm_input:
    if (output)
      return -1;
    return fileno(IOStreamStreamFile(s));
  case clasp_smm_input_file:
    if (output)
      return -1;
    return IOFileStreamDescriptor(s);
  case clasp_smm_output:
    if (!output)
      return -1;
    return fileno(IOStreamStreamFile(s));
  case clasp_smm_output_file:
    if (!output)
      return -1;
    return IOFileStreamDescriptor(s);
  case clasp_smm_io:
    return fileno(IOStreamStreamFile(s));
  case clasp_smm_io_file:
    return IOFileStreamDescriptor(s);
  case clasp_smm_synonym:
    s = SynonymStreamStream(s);
    goto BEGIN;
  case clasp_smm_two_way:
    s = output ? TwoWayStreamOutput(s) : TwoWayStreamInput(s);
    goto BEGIN;
#if defined(ECL_WSOCK)
  case clasp_smm_input_wsock:
  case clasp_smm_output_wsock:
  case clasp_smm_io_wsock:
#endif
#if defined(CLASP_MS_WINDOWS_HOST)
  case clasp_smm_io_wcon:
#endif
    return -1;
  default:
    clasp_internal_error("illegal stream mode");
  }
  return -1;
}

CL_LAMBDA(s);
CL_DECLARE();
CL_DOCSTRING("core-file-stream-fd");
CL_DEFUN T_sp core__file_stream_fd(T_sp s) {
  T_sp ret;
  unlikely_if(!AnsiStreamP(s))
      FEerror("file_stream_fd: not a stream", 0);

  switch (StreamMode(s)) {
  case clasp_smm_input:
  case clasp_smm_output:
  case clasp_smm_io:
    ret = make_fixnum(fileno(IOStreamStreamFile(s)));
    break;
  case clasp_smm_input_file:
  case clasp_smm_output_file:
  case clasp_smm_io_file:
    ret = make_fixnum(IOFileStreamDescriptor(s));
    break;
  default:
    clasp_internal_error("not a file stream");
  }
  return ret;
}

       /**********************************************************************
     * MEDIUM LEVEL INTERFACE
     */

const FileOps &
duplicate_dispatch_table(const FileOps &ops) {
  return ops;
}

SYMBOL_EXPORT_SC_(CorePkg, dispatchTable);

const FileOps &
stream_dispatch_table(T_sp strm) {
  if (Instance_sp instance = strm.asOrNull<Instance_O>()) {
    (void)instance;
    return clos_stream_ops;
  }
  if (!AnsiStreamP(strm))
    ERROR_WRONG_TYPE_ONLY_ARG(core::_sym_dispatchTable, strm, cl::_sym_Stream_O);
  return StreamOps(strm);
}

cl_index
clasp_read_byte8(T_sp strm, unsigned char *c, cl_index n) {
  return stream_dispatch_table(strm).read_byte8(strm, c, n);
}

cl_index
clasp_write_byte8(T_sp strm, unsigned char *c, cl_index n) {
  return stream_dispatch_table(strm).write_byte8(strm, c, n);
}

claspCharacter
clasp_read_char(T_sp strm) {
  return stream_dispatch_table(strm).read_char(strm);
}

claspCharacter
clasp_read_char_noeof(T_sp strm) {
  claspCharacter c = clasp_read_char(strm);
  if (c == EOF)
    ERROR_END_OF_FILE(strm);
  return c;
}

T_sp clasp_read_byte(T_sp strm) {
  return stream_dispatch_table(strm).read_byte(strm);
}

void clasp_write_byte(T_sp c, T_sp strm) {
  stream_dispatch_table(strm).write_byte(c, strm);
}

claspCharacter
clasp_write_char(claspCharacter c, T_sp strm) {
  return stream_dispatch_table(strm).write_char(strm, c);
}

void clasp_unread_char(claspCharacter c, T_sp strm) {
  stream_dispatch_table(strm).unread_char(strm, c);
}

int clasp_listen_stream(T_sp strm) {
  return stream_dispatch_table(strm).listen(strm);
}

void clasp_clear_input(T_sp strm) {
  stream_dispatch_table(strm).clear_input(strm);
}

void clasp_clear_output(T_sp strm) {
  stream_dispatch_table(strm).clear_output(strm);
}

void clasp_force_output(T_sp strm) {
  stream_dispatch_table(strm).force_output(strm);
}

void clasp_finish_output(T_sp strm) {
  stream_dispatch_table(strm).finish_output(strm);
}

void clasp_finish_output_t() {
  clasp_finish_output(cl::_sym_STARstandard_outputSTAR->symbolValue());
}

int clasp_file_column(T_sp strm) {
  return stream_dispatch_table(strm).column(strm);
}

T_sp clasp_file_length(T_sp strm) {
  return stream_dispatch_table(strm).length(strm);
}

T_sp clasp_file_position(T_sp strm) {
  //        printf("%s:%d strm = %s\n", __FILE__, __LINE__, _rep_(strm).c_str());
  return stream_dispatch_table(strm).get_position(strm);
}

T_sp clasp_file_position_set(T_sp strm, T_sp pos) {
  return stream_dispatch_table(strm).set_position(strm, pos);
}

bool clasp_input_stream_p(T_sp strm) {
  return stream_dispatch_table(strm).input_p(strm);
}

bool clasp_output_stream_p(T_sp strm) {
  return stream_dispatch_table(strm).output_p(strm);
}

T_sp clasp_stream_element_type(T_sp strm) {
  return stream_dispatch_table(strm).element_type(strm);
}

int clasp_interactive_stream_p(T_sp strm) {
  return stream_dispatch_table(strm).interactive_p(strm);
}

/*
 * clasp_read_char(s) tries to read a character from the stream S. It outputs
 * either the code of the character read, or EOF. Whe compiled with
 * CLOS-STREAMS and S is an instance object, STREAM-READ-CHAR is invoked
 * to retrieve the character. Then STREAM-READ-CHAR should either
 * output the character, or NIL, indicating EOF.
 *
 * INV: clasp_read_char(strm) checks the type of STRM.
 */
claspCharacter
clasp_peek_char(T_sp strm) {
  return stream_dispatch_table(strm).peek_char(strm);
}

/*******************************tl***************************************
 * SEQUENCES I/O
 */

void writestr_stream(const char *s, T_sp strm) {
  while (*s != '\0')
    clasp_write_char(*s++, strm);
}

void write_bf_stream(const boost::format& fmt, T_sp strm)
{
  clasp_write_string(fmt.str(),strm);
}

void clasp_write_addr(T_sp x, T_sp strm) {
  stringstream ss;
  ss << (void *)x.raw_();
  writestr_stream(ss.str().c_str(), strm);
}

static cl_index
compute_char_size(T_sp stream, claspCharacter c) {
  // TODO  Make this work with full characters
  unsigned char buffer[5];
  int l = 0;
  if (c == CLASP_CHAR_CODE_NEWLINE) {
    int flags = StreamFlags(stream);
    if (flags & CLASP_STREAM_CR) {
      l += StreamEncoder(stream)(stream, buffer, CLASP_CHAR_CODE_RETURN);
      if (flags & CLASP_STREAM_LF)
        l += StreamEncoder(stream)(stream, buffer,
                                   CLASP_CHAR_CODE_LINEFEED);
    } else {
      l += StreamEncoder(stream)(stream, buffer, CLASP_CHAR_CODE_LINEFEED);
    }
  } else {
    l += StreamEncoder(stream)(stream, buffer, c);
  }
  return l;
}

CL_LAMBDA(stream string);
CL_DECLARE();
CL_DOCSTRING("file-string-length");
CL_DEFUN T_sp cl__file_string_length(T_sp stream, T_sp tstring) {
  gctools::Fixnum l = 0;
/* This is a stupid requirement from the spec. Why returning 1???
	 * Why not simply leaving the value unspecified, as with other
	 * streams one cannot write to???
	 */
BEGIN:
  if (Instance_sp instance = stream.asOrNull<Instance_O>()) {
    (void)instance;
    return _Nil<T_O>();
  }
  unlikely_if(!AnsiStreamP(stream)) {
    ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_file_string_length, stream, cl::_sym_Stream_O);
  }
  if (StreamMode(stream) == clasp_smm_broadcast) {
    stream = BroadcastStreamList(stream);
    if (stream.nilp()) {
      return make_fixnum(1);
    } else {
      // stream is now a list of streams, but need a stream object
      // http://www.lispworks.com/documentation/HyperSpec/Body/t_broadc.htm#broadcast-stream
      stream = oCar(cl__last(stream, clasp_make_fixnum(1)));
      goto BEGIN;
    }
  }
  unlikely_if(!FileStreamP(stream)) {
    not_a_file_stream(stream);
  }
  if (cl__characterp(tstring)) {
    l = compute_char_size(stream, tstring.unsafe_character());
  } else if (cl__stringp(tstring)) {
    Fixnum iEnd;
    String_sp sb = tstring.asOrNull<String_O>();
    if (sb && (sb->arrayHasFillPointerP()))
      iEnd = StringFillp(sb);
    else
      iEnd = cl__length(sb);        
    for (int i(0); i < iEnd; ++i) {
      l += compute_char_size(stream, cl__char(sb, i).unsafe_character());
    }
  } else {
    ERROR_WRONG_TYPE_NTH_ARG(cl::_sym_file_string_length, 2, tstring, cl::_sym_string);
  }
  return make_fixnum(l);
}

CL_LAMBDA(seq stream start end);
CL_DECLARE();
CL_DOCSTRING("do_write_sequence");
CL_DEFUN T_sp core__do_write_sequence(T_sp seq, T_sp stream, T_sp s, T_sp e) {
  gctools::Fixnum start, limit, end(0);

  /* Since we have called clasp_length(), we know that SEQ is a valid
	   sequence. Therefore, we only need to check the type of the
	   object, and seq == _Nil<T_O>() i.f.f. t = t_symbol */
  limit = cl__length(seq);
  if (!core__fixnump(s)) {
    ERROR_WRONG_TYPE_KEY_ARG(cl::_sym_write_sequence, kw::_sym_start, s,
                             Integer_O::makeIntegerType(0, limit - 1));
  }
  start = (s).unsafe_fixnum();
  if ((start < 0) || (start > limit)) {
    ERROR_WRONG_TYPE_KEY_ARG(cl::_sym_write_sequence, kw::_sym_start, s,
                             Integer_O::makeIntegerType(0, limit - 1));
  }
  if (e.nilp()) {
    end = limit;
  } else if (!e.fixnump()) { //!core__fixnump(e)) {
    ERROR_WRONG_TYPE_KEY_ARG(cl::_sym_write_sequence, kw::_sym_end, e,
                             Integer_O::makeIntegerType(0, limit));
  } else
    end = (e).unsafe_fixnum();
  if ((end < 0) || (end > limit)) {
    ERROR_WRONG_TYPE_KEY_ARG(cl::_sym_write_sequence, kw::_sym_end, e,
                             Integer_O::makeIntegerType(0, limit));
  }
  if (start < end) {
    const FileOps &ops = stream_dispatch_table(stream);
    if (cl__listp(seq)) {
      T_sp elt_type = cl__stream_element_type(stream);
      bool ischar = (elt_type == cl::_sym_base_char) || (elt_type == cl::_sym_character);
      T_sp s = cl__nthcdr(clasp_make_integer(start), seq);
      T_sp orig = s;
      for (; s.notnilp(); s = oCdr(s)) {
        if (!cl__listp(s)) {
          TYPE_ERROR_PROPER_LIST(orig);
        }
        if (start < end) {
          T_sp elt = oCar(s);
          if (ischar)
            ops.write_char(stream, clasp_as_claspCharacter(gc::As<Character_sp>(elt)));
          else
            ops.write_byte(elt, stream);
          start++;
        } else {
          return seq;
        }
      };
    } else {
      ops.write_vector(stream, seq, start, end);
    }
  }
  return seq;
}

T_sp si_do_read_sequence(T_sp seq, T_sp stream, T_sp s, T_sp e) {
  gctools::Fixnum start, limit, end(0);
  /* Since we have called clasp_length(), we know that SEQ is a valid
	   sequence. Therefore, we only need to check the type of the
	   object, and seq == _Nil<T_O>() i.f.f. t = t_symbol */
  limit = cl__length(seq);
  if (!core__fixnump(s)) {
    ERROR_WRONG_TYPE_KEY_ARG(cl::_sym_read_sequence, kw::_sym_start, s,
                             Integer_O::makeIntegerType(0, limit - 1));
  }
  start = (s).unsafe_fixnum();
  if ((start < 0) || (start > limit)) {
    ERROR_WRONG_TYPE_KEY_ARG(cl::_sym_read_sequence, kw::_sym_start, s,
                             Integer_O::makeIntegerType(0, limit - 1));
  }
  if (e.nilp()) {
    end = limit;
  } else if (!e.fixnump()) {
    ERROR_WRONG_TYPE_KEY_ARG(cl::_sym_read_sequence, kw::_sym_end, e,
                             Integer_O::makeIntegerType(0, limit));
  } else {
    end = (e).unsafe_fixnum();
  }
  if ((end < 0) || (end > limit)) {
    ERROR_WRONG_TYPE_KEY_ARG(cl::_sym_read_sequence, kw::_sym_end, e,
                             Integer_O::makeIntegerType(0, limit));
  }
  if (end < start) {
    ERROR_WRONG_TYPE_KEY_ARG(cl::_sym_read_sequence, kw::_sym_end, e,
                             Integer_O::makeIntegerType(start, limit));
  }
  if (start < end) {
    const FileOps &ops = stream_dispatch_table(stream);
    if (cl__listp(seq)) {
      T_sp elt_type = cl__stream_element_type(stream);
      bool ischar = (elt_type == cl::_sym_base_char) || (elt_type == cl::_sym_character);
      seq = cl__nthcdr(clasp_make_integer(start), seq);
      T_sp orig = seq;
      for (; seq.notnilp(); seq = oCdr(seq)) {
        if (start >= end) {
          return make_fixnum(start);
        } else {
          T_sp c;
          if (ischar) {
            int i = ops.read_char(stream);
            if (i < 0) {
              return make_fixnum(start);
            }
            c = clasp_make_character(i);
          } else {
            c = ops.read_byte(stream);
            if (c.nilp()) {
              return make_fixnum(start);
            }
          }
          gc::As<Cons_sp>(seq)->rplaca(c);
          start++;
        }
      };
    } else {
      start = ops.read_vector(stream, seq, start, end);
    }
  }
  return make_fixnum(start);
}

CL_LAMBDA(sequence stream &key (start 0) end);
CL_DECLARE();
CL_DOCSTRING("readSequence");
CL_DEFUN T_sp cl__read_sequence(T_sp sequence, T_sp stream, T_sp start, T_sp oend) {
  stream = coerce::inputStreamDesignator(stream);
  if (!AnsiStreamP(stream)) {
    return eval::funcall(gray::_sym_stream_read_sequence, stream, sequence, start, oend);
  }
  return si_do_read_sequence(sequence, stream, start, oend);
}

CL_LAMBDA(sequence stream start end);
CL_DECLARE();
CL_DOCSTRING("readSequence");
CL_DEFUN T_sp core__do_read_sequence(T_sp sequence, T_sp stream, T_sp start, T_sp oend) {
  stream = coerce::inputStreamDesignator(stream);
  return si_do_read_sequence(sequence, stream, start, oend);
}
/**********************************************************************
 * LISP LEVEL INTERFACE
 */

T_sp si_file_column(T_sp strm) {
  return make_fixnum(clasp_file_column(strm));
}

CL_LAMBDA(strm);
CL_DECLARE();
CL_DOCSTRING("file_length");
CL_DEFUN T_sp cl__file_length(T_sp strm) {
  return clasp_file_length(strm);
}

CL_LAMBDA(file-stream &optional position);
CL_DECLARE();
CL_DOCSTRING("filePosition");
CL_DEFUN T_sp cl__file_position(T_sp stream, T_sp position) {
  T_sp output;
  if (position.nilp()) {
    output = clasp_file_position(stream);
  } else {
    if (position == kw::_sym_start) {
      position = make_fixnum(0);
    } else if (position == kw::_sym_end) {
      position = _Nil<T_O>();
    }
    output = clasp_file_position_set(stream, position);
  }
  return output;
}

CL_LAMBDA(strm);
CL_DEFUN T_sp core__input_stream_pSTAR(T_sp strm) {
  ASSERT(strm);
  return (clasp_input_stream_p(strm) ? _lisp->_true() : _Nil<T_O>());
}

CL_LAMBDA(strm);
CL_DEFUN T_sp cl__input_stream_p(T_sp strm) {
  return core__input_stream_pSTAR(strm);
}

CL_LAMBDA(arg);
CL_DEFUN T_sp core__output_stream_pSTAR(T_sp strm) {
  ASSERT(strm);
  return (clasp_output_stream_p(strm) ? _lisp->_true() : _Nil<T_O>());
}

CL_LAMBDA(arg);
CL_DEFUN T_sp cl__output_stream_p(T_sp strm) {
  return core__output_stream_pSTAR(strm);
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("interactive_stream_p");
CL_DEFUN T_sp cl__interactive_stream_p(T_sp strm) {
  ASSERT(strm);
  return (stream_dispatch_table(strm).interactive_p(strm) ? _lisp->_true() : _Nil<T_O>());
}


CL_DEFUN T_sp core__open_stream_pSTAR(T_sp strm) {
  /* ANSI and Cltl2 specify that open-stream-p should work
	   on closed streams, and that a stream is only closed
	   when #'close has been applied on it */
  if (Instance_sp instance = strm.asOrNull<Instance_O>()) {
    return eval::funcall(gray::_sym_open_stream_p, instance);
  }
  unlikely_if(!AnsiStreamP(strm))
      ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_open_stream_p, strm, cl::_sym_Stream_O);
  return (StreamClosed(strm) ? _Nil<T_O>() : _lisp->_true());
}

CL_DEFUN T_sp cl__open_stream_p(T_sp strm) {
  /* ANSI and Cltl2 specify that open-stream-p should work
	   on closed streams, and that a stream is only closed
	   when #'close has been applied on it */
  return core__open_stream_pSTAR(strm);
}

CL_DEFUN T_sp core__stream_element_typeSTAR(T_sp strm) {
  return clasp_stream_element_type(strm);
}

CL_DEFUN T_sp cl__stream_element_type(T_sp strm) {
  return core__stream_element_typeSTAR(strm);
}

CL_DEFUN T_sp cl__stream_external_format(T_sp strm) {
  T_sp output;
AGAIN:
  if (Instance_sp instance = strm.asOrNull<Instance_O>()) {
    (void)instance;
    output = kw::_sym_default;
    return output;
  } else if (Stream_sp s = strm.asOrNull<Stream_O>()) {
    if (StreamMode(s) == clasp_smm_synonym) {
      strm = SynonymStreamStream(strm);
      goto AGAIN;
    }
  } else {
    ERROR_WRONG_TYPE_ONLY_ARG(cl::_sym_stream_external_format, strm, cl::_sym_Stream_O);
  }
  output = StreamFormat(strm);
  return output;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("streamp");
CL_DEFUN bool cl__streamp(T_sp strm) {
  if (Instance_sp instance = strm.asOrNull<Instance_O>()) {
    return T_sp(eval::funcall(gray::_sym_streamp, instance)).isTrue();
  }
  return AnsiStreamP(strm);
}

/**********************************************************************
 * OTHER TOOLS
 */

T_sp si_copy_stream(T_sp in, T_sp out) {
  claspCharacter c;
  for (c = clasp_read_char(in); c != EOF; c = clasp_read_char(in)) {
    clasp_write_char(c, out);
  }
  clasp_force_output(out);
  return _lisp->_true();
}

/**********************************************************************
 * FILE OPENING AND CLOSING
 */

gctools::Fixnum
clasp_normalize_stream_element_type(T_sp element_type) {
  gctools::Fixnum sign = 0;
  cl_index size;
  if (element_type == cl::_sym_SignedByte || element_type == ext::_sym_integer8) {
    return -8;
  } else if (element_type == cl::_sym_UnsignedByte || element_type == ext::_sym_byte8) {
    return 8;
  } else if (element_type == kw::_sym_default) {
    return 0;
  } else if (element_type == cl::_sym_base_char || element_type == cl::_sym_character) {
    return 0;
  } else if (T_sp(eval::funcall(cl::_sym_subtypep, element_type, cl::_sym_character)).notnilp()) {
    return 0;
  } else if (T_sp(eval::funcall(cl::_sym_subtypep, element_type, cl::_sym_UnsignedByte)).notnilp()) {
    sign = +1;
  } else if (T_sp(eval::funcall(cl::_sym_subtypep, element_type, cl::_sym_SignedByte)).notnilp()) {
    sign = -1;
  } else {
    FEerror("Not a valid stream element type: ~A", 1, element_type.raw_());
  }
  if ((element_type).consp()) {
    if (oCar(element_type) == cl::_sym_UnsignedByte)
      return clasp_toSize(oCadr(element_type));
    if (oCar(element_type) == cl::_sym_SignedByte)
      return -clasp_toSize(oCadr(element_type));
  }
  for (size = 8; 1; size++) {
    T_sp type;
    type = Cons_O::createList(sign > 0 ? cl::_sym_UnsignedByte : cl::_sym_SignedByte,
                              make_fixnum(size));
    if (T_sp(eval::funcall(cl::_sym_subtypep, element_type, type)).notnilp()) {
      return size * sign;
    }
  }
  FEerror("Not a valid stream element type: ~A", 1, element_type.raw_());
}

static void
FEinvalid_option(T_sp option, T_sp value) {
  FEerror("Invalid value op option ~A: ~A", 2, option.raw_(), value.raw_());
}

T_sp clasp_open_stream(T_sp fn, enum StreamMode smm, T_sp if_exists,
                       T_sp if_does_not_exist, gctools::Fixnum byte_size,
                       int flags, T_sp external_format) {
  T_sp output;
  int f;
#if defined(CLASP_MS_WINDOWS_HOST)
  clasp_mode_t mode = _S_IREAD | _S_IWRITE;
#else
  clasp_mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
#endif
  if (fn.nilp()) SIMPLE_ERROR(BF("In %s the filename is NIL") % __FUNCTION__);
  String_sp filename = core__coerce_to_filename(fn);
  string fname = filename->get_std_string();
  bool appending = 0;
  ASSERT(filename);
  bool exists = core__file_kind(filename, true).notnilp();
  if (smm == clasp_smm_input || smm == clasp_smm_probe) {
    if (!exists) {
      if (if_does_not_exist == kw::_sym_error) {
        FEcannot_open(fn);
      } else if (if_does_not_exist == kw::_sym_create) {
        f = safe_open(fname.c_str(), O_WRONLY | O_CREAT, mode);
        unlikely_if(f < 0) FEcannot_open(fn);
        safe_close(f);
      } else if (if_does_not_exist.nilp()) {
        return _Nil<T_O>();
      } else {
        FEinvalid_option(kw::_sym_if_does_not_exist,
                         if_does_not_exist);
      }
    }
    f = safe_open(fname.c_str(), O_RDONLY, mode);
    unlikely_if(f < 0) FEcannot_open(fn);
  } else if (smm == clasp_smm_output || smm == clasp_smm_io) {
    int base = (smm == clasp_smm_output) ? O_WRONLY : O_RDWR;
    if (if_exists == kw::_sym_new_version &&
        if_does_not_exist == kw::_sym_create) {
      exists = 0;
      if_does_not_exist = kw::_sym_create;
    }
    if (exists) {
      if (if_exists == kw::_sym_error) {
        FEcannot_open(fn);
      } else if (if_exists == kw::_sym_rename) {
        f = clasp_backup_open(fname.c_str(), base | O_CREAT, mode);
        unlikely_if(f < 0) FEcannot_open(fn);
      } else if (if_exists == kw::_sym_rename_and_delete ||
                 if_exists == kw::_sym_new_version ||
                 if_exists == kw::_sym_supersede) {
        f = safe_open(fname.c_str(), base | O_TRUNC, mode);
        unlikely_if(f < 0) FEcannot_open(fn);
      } else if (if_exists == kw::_sym_overwrite || if_exists == kw::_sym_append) {
        f = safe_open(fname.c_str(), base, mode);
        unlikely_if(f < 0) FEcannot_open(fn);
        appending = (if_exists == kw::_sym_append);
      } else if (if_exists.nilp()) {
        return _Nil<T_O>();
      } else {
        FEinvalid_option(kw::_sym_if_exists, if_exists);
      }
    } else {
      if (if_does_not_exist == kw::_sym_error) {
        FEcannot_open(fn);
      } else if (if_does_not_exist == kw::_sym_create) {
        f = safe_open(fname.c_str(), base | O_CREAT | O_TRUNC, mode);
        unlikely_if(f < 0) FEcannot_open(fn);
      } else if (if_does_not_exist.nilp()) {
        return _Nil<T_O>();
      } else {
        FEinvalid_option(kw::_sym_if_does_not_exist,
                         if_does_not_exist);
      }
    }
  } else {
    FEerror("Illegal stream mode ~S", 1, make_fixnum(smm).raw_());
  }
  if (flags & CLASP_STREAM_C_STREAM) {
    FILE *fp = NULL;
    safe_close(f);
    /* We do not use fdopen() because Windows seems to
             * have problems with the resulting streams. Furthermore, even for
             * output we open with w+ because we do not want to
             * overwrite the file. */
    switch (smm) {
    case clasp_smm_probe:
    case clasp_smm_input:
      fp = safe_fopen(fname.c_str(), OPEN_R);
      break;
    case clasp_smm_output:
    case clasp_smm_io:
      fp = safe_fopen(fname.c_str(), OPEN_RW);
      break;
    default:
      ; /* never reached */
      SIMPLE_ERROR(BF("Illegal smm mode: %d for CLASP_STREAM_C_STREAM") % smm);
      UNREACHABLE();
    }
    output = clasp_make_stream_from_FILE(fn, fp, smm, byte_size, flags,
                                         external_format);
    core__set_buffering_mode(output, byte_size ? kw::_sym_full : kw::_sym_line);
  } else {
    output = clasp_make_file_stream_from_fd(fn, f, smm, byte_size, flags,
                                            external_format);
  }
  if (smm == clasp_smm_probe) {
    eval::funcall(cl::_sym_close, output);
  } else {
    StreamFlags(output) |= CLASP_STREAM_MIGHT_SEEK;
    //            si_set_finalizer(output, _lisp->_true());
    /* Set file pointer to the correct position */
    if (appending) {
      clasp_file_position_set(output, _Nil<T_O>());
    } else {
      clasp_file_position_set(output, make_fixnum(0));
    }
  }
  return output;
}

CL_LAMBDA("filename &key (direction :input) (element-type 'base-char) (if-exists nil iesp) (if-does-not-exist nil idnesp) (external-format :default) (cstream T)");
CL_DECLARE();
CL_DOCSTRING("open");
CL_DEFUN T_sp cl__open(T_sp filename,
             T_sp direction,
             T_sp element_type,
             T_sp if_exists, bool iesp,
             T_sp if_does_not_exist, bool idnesp,
             T_sp external_format,
             T_sp cstream) {
  if (filename.nilp()) {
    TYPE_ERROR(filename,Cons_O::createList(cl::_sym_or,cl::_sym_string,cl::_sym_Pathname_O,cl::_sym_Stream_O));
  }
  T_sp strm;
  enum StreamMode smm;
  int flags = 0;
  gctools::Fixnum byte_size;
  /* INV: clasp_open_stream() checks types */
  if (direction == kw::_sym_input) {
    smm = clasp_smm_input;
    if (!idnesp)
      if_does_not_exist = kw::_sym_error;
  } else if (direction == kw::_sym_output) {
    smm = clasp_smm_output;
    if (!iesp)
      if_exists = kw::_sym_new_version;
    if (!idnesp) {
      if (if_exists == kw::_sym_overwrite ||
          if_exists == kw::_sym_append)
        if_does_not_exist = kw::_sym_error;
      else
        if_does_not_exist = kw::_sym_create;
    }
  } else if (direction == kw::_sym_io) {
    smm = clasp_smm_io;
    if (!iesp)
      if_exists = kw::_sym_new_version;
    if (!idnesp) {
      if (if_exists == kw::_sym_overwrite ||
          if_exists == kw::_sym_append)
        if_does_not_exist = kw::_sym_error;
      else
        if_does_not_exist = kw::_sym_create;
    }
  } else if (direction == kw::_sym_probe) {
    smm = clasp_smm_probe;
    if (!idnesp)
      if_does_not_exist = _Nil<T_O>();
  } else {
    FEerror("~S is an illegal DIRECTION for OPEN.",
            1, direction.raw_());
    UNREACHABLE();
  }
  byte_size = clasp_normalize_stream_element_type(element_type);
  if (byte_size != 0) {
    external_format = _Nil<T_O>();
  }
  if (!cstream.nilp()) {
    flags |= CLASP_STREAM_C_STREAM;
  }
  strm = clasp_open_stream(filename, smm, if_exists, if_does_not_exist,
                           byte_size, flags, external_format);
  return strm;
}


CL_LAMBDA(strm &key abort);
CL_DECLARE();
CL_DOCSTRING("Identical to cl:close but this won't be redefined by gray streams and will be available to call after cl:close is redefined by gray::redefine-cl-functions.");
CL_DEFUN T_sp core__closeSTAR(T_sp strm, T_sp abort) {
  return stream_dispatch_table(strm).close(strm);
}

CL_LAMBDA(strm &key abort);
CL_DECLARE();
CL_DOCSTRING("close");
CL_DEFUN T_sp cl__close(T_sp strm, T_sp abort) {
  return core__closeSTAR(strm,abort);
}

/**********************************************************************
     * BACKEND
     */

static int
file_listen(T_sp stream, int fileno) {
#if !defined(CLASP_MS_WINDOWS_HOST)
#if defined(HAVE_SELECT)
  fd_set fds;
  int retv;
  struct timeval tv = {0, 0};
  /*
         * Note that the following code is fragile. If the file is closed (/dev/null)
         * then select() may return 1 (at least on OS X), so that we return a flag
         * saying characters are available but will find none to read. See also the
         * code in cl_clear_input().
         */
  FD_ZERO(&fds);
  FD_SET(fileno, &fds);
  retv = select(fileno + 1, &fds, NULL, NULL, &tv);
  if (UNLIKELY(retv < 0))
    file_libc_error(core::_sym_simpleStreamError, stream, "Error while listening to stream.", 0);
  else if (retv > 0)
    return CLASP_LISTEN_AVAILABLE;
  else
    return CLASP_LISTEN_NO_CHAR;
#elif defined(FIONREAD)
  {
    long c = 0;
    ioctl(fileno, FIONREAD, &c);
    return (c > 0) ? CLASP_LISTEN_AVAILABLE : CLASP_LISTEN_NO_CHAR;
  }
#endif /* FIONREAD */
#else
  HANDLE hnd = (HANDLE)_get_osfhandle(fileno);
  switch (GetFileType(hnd)) {
  case FILE_TYPE_CHAR: {
    DWORD dw, dw_read, cm;
    if (GetNumberOfConsoleInputEvents(hnd, &dw)) {
      unlikely_if(!GetConsoleMode(hnd, &cm))
          FEwin32_error("GetConsoleMode() failed", 0);
      if (dw > 0) {
        PINPUT_RECORD recs = (PINPUT_RECORD)GC_malloc(sizeof(INPUT_RECORD) * dw);
        int i;
        unlikely_if(!PeekConsoleInput(hnd, recs, dw, &dw_read))
            FEwin32_error("PeekConsoleInput failed()", 0);
        if (dw_read > 0) {
          if (cm & ENABLE_LINE_INPUT) {
            for (i = 0; i < dw_read; i++)
              if (recs[i].EventType == KEY_EVENT &&
                  recs[i].Event.KeyEvent.bKeyDown &&
                  recs[i].Event.KeyEvent.uChar.AsciiChar == 13)
                return CLASP_LISTEN_AVAILABLE;
          } else {
            for (i = 0; i < dw_read; i++)
              if (recs[i].EventType == KEY_EVENT &&
                  recs[i].Event.KeyEvent.bKeyDown &&
                  recs[i].Event.KeyEvent.uChar.AsciiChar != 0)
                return CLASP_LISTEN_AVAILABLE;
          }
        }
      }
      return CLASP_LISTEN_NO_CHAR;
    } else
      FEwin32_error("GetNumberOfConsoleInputEvents() failed", 0);
    break;
  }
  case FILE_TYPE_DISK:
    /* use regular file code below */
    break;
  case FILE_TYPE_PIPE: {
    DWORD dw;
    if (PeekNamedPipe(hnd, NULL, 0, NULL, &dw, NULL))
      return (dw > 0 ? CLASP_LISTEN_AVAILABLE : CLASP_LISTEN_NO_CHAR);
    else if (GetLastError() == ERROR_BROKEN_PIPE)
      return CLASP_LISTEN_EOF;
    else
      FEwin32_error("PeekNamedPipe() failed", 0);
    break;
  }
  default:
    FEerror("Unsupported Windows file type: ~A", 1, make_fixnum(GetFileType(hnd)).raw_());
    break;
  }
#endif
  return -3;
}

static int
flisten(T_sp stream, FILE *fp) {
  ASSERT(stream.notnilp());
  int aux;
  if (feof(fp))
    return CLASP_LISTEN_EOF;
#ifdef FILE_CNT
  if (FILE_CNT(fp) > 0)
    return CLASP_LISTEN_AVAILABLE;
#endif
  aux = file_listen(stream, fileno(fp));
  if (aux != -3)
    return aux;
  /* This code is portable, and implements the expected behavior for regular files.
	    It will fail on noninteractive streams. */
  {
    /* regular file */
    clasp_off_t old_pos = clasp_ftello(fp), end_pos;
    unlikely_if(old_pos < 0) {
      //                printf("%s:%d ftello error old_pos = %ld error = %s\n", __FILE__, __LINE__, old_pos, strerror(errno));
      file_libc_error(core::_sym_simpleFileError, stream,
                      "Unable to check file position in SEEK_END", 0);
    }
    unlikely_if(clasp_fseeko(fp, 0, SEEK_END) != 0) {
      //                printf("%s:%d Seek error fp=%p error = %s\n", __FILE__, __LINE__, fp, strerror(errno));
      file_libc_error(core::_sym_simpleFileError, stream,
                      "Unable to check file position in SEEK_END", 0);
    }
    end_pos = clasp_ftello(fp);
    unlikely_if(clasp_fseeko(fp, old_pos, SEEK_SET) != 0)
        file_libc_error(core::_sym_simpleFileError, stream,
                        "Unable to check file position in SEEK_SET", 0);
    return (end_pos > old_pos ? CLASP_LISTEN_AVAILABLE : CLASP_LISTEN_EOF);
  }
  return !CLASP_LISTEN_AVAILABLE;
}

T_sp clasp_off_t_to_integer(clasp_off_t offset) {
  T_sp output;
  if (sizeof(clasp_off_t) == sizeof(gctools::Fixnum)) {
    output = Integer_O::create((gctools::Fixnum)offset);
  } else if (offset <= MOST_POSITIVE_FIXNUM) {
    output = make_fixnum((gctools::Fixnum)offset);
  } else {
    IMPLEMENT_MEF("Handle breaking converting clasp_off_t to Bignum");
  }
  return output;
}

clasp_off_t
clasp_integer_to_off_t(T_sp offset) {
  clasp_off_t output = 0;
  if (sizeof(clasp_off_t) == sizeof(gctools::Fixnum)) {
    output = fixint(offset);
  } else if (core__fixnump(offset)) {
    output = fixint(offset);
  } else if (core__bignump(offset)) {
    IMPLEMENT_MEF("Implement convert Bignum to clasp_off_t");
  } else {
    //	ERR:
    FEerror("Not a valid file offset: ~S", 1, offset.raw_());
  }
  return output;
}

/**********************************************************************
 * ERROR MESSAGES
 */

static T_sp
not_a_file_stream(T_sp strm) {
  cl__error(cl::_sym_simpleTypeError,
           Cons_O::createList(kw::_sym_format_control,
                              SimpleBaseString_O::make("~A is not an file stream"),
                              kw::_sym_format_arguments, Cons_O::createList(strm),
                              kw::_sym_expected_type, cl::_sym_FileStream_O,
                              kw::_sym_datum, strm));
  UNREACHABLE();
}

static void
not_an_input_stream(T_sp strm) {
  cl__error(cl::_sym_simpleTypeError, Cons_O::createList(kw::_sym_format_control,
                                                        SimpleBaseString_O::make("~A is not an input stream"),
                                                        kw::_sym_format_arguments, Cons_O::createList(strm),
                                                        kw::_sym_expected_type,
                                                        Cons_O::createList(cl::_sym_satisfies, cl::_sym_input_stream_p),
                                                        kw::_sym_datum, strm));
}

static void
not_an_output_stream(T_sp strm) {
  cl__error(cl::_sym_simpleTypeError, Cons_O::createList(kw::_sym_format_control,
                                                        SimpleBaseString_O::make("~A is not an output stream"),
                                                        kw::_sym_format_arguments, Cons_O::createList(strm),
                                                        kw::_sym_expected_type, Cons_O::createList(cl::_sym_satisfies, cl::_sym_output_stream_p),
                                                        kw::_sym_datum, strm));
}

static void
not_a_character_stream(T_sp s) {
  cl__error(cl::_sym_simpleTypeError, Cons_O::createList(kw::_sym_format_control,
                                                        SimpleBaseString_O::make("~A is not a character stream"),
                                                        kw::_sym_format_arguments, Cons_O::createList(s),
                                                        kw::_sym_expected_type, cl::_sym_character,
                                                        kw::_sym_datum, cl__stream_element_type(s)));
}

static void
not_a_binary_stream(T_sp s) {
  cl__error(cl::_sym_simpleTypeError, Cons_O::createList(kw::_sym_format_control,
                                                        SimpleBaseString_O::make("~A is not a binary stream"),
                                                        kw::_sym_format_arguments, Cons_O::createList(s),
                                                        kw::_sym_expected_type, cl::_sym_Integer_O,
                                                        kw::_sym_datum, cl__stream_element_type(s)));
}

static void
cannot_close(T_sp stream) {
  file_libc_error(core::_sym_simpleFileError, stream, "Stream cannot be closed", 0);
}

static void
file_libc_error(T_sp error_type, T_sp stream,
                const char *msg, int narg, ...) {
  clasp_va_list args;
  T_sp error = SimpleBaseString_O::make(std::string(strerror(errno)));
  clasp_va_start(args, narg);
  T_sp rest = clasp_grab_rest_args(args, narg);
  clasp_va_end(args);

  eval::funcall(core::_sym_signalSimpleError,
                error_type, _Nil<T_O>(),
                SimpleBaseString_O::make("~?~%C library explanation: ~A."),
                Cons_O::createList(SimpleBaseString_O::make(std::string(msg)), rest,
                                   error));
}

static void
unread_error(T_sp s) {
  CEerror(_lisp->_true(), "Error when using UNREAD-CHAR on stream ~D", 1, s.raw_());
}

static void
unread_twice(T_sp s) {
  CEerror(_lisp->_true(), "Used UNREAD-CHAR twice on stream ~D", 1, s.raw_());
}

static void
maybe_clearerr(T_sp strm) {
  int t = StreamMode(strm);
  if (t == clasp_smm_io || t == clasp_smm_output || t == clasp_smm_input) {
    FILE *f = IOStreamStreamFile(strm);
    if (f != NULL)
      clearerr(f);
  }
}

static int
restartable_io_error(T_sp strm, const char *s) {
  cl_env_ptr the_env = clasp_process_env();
  volatile int old_errno = errno;
  /* clasp_disable_interrupts(); ** done by caller */
  maybe_clearerr(strm);
  clasp_enable_interrupts_env(the_env);
  if (old_errno == EINTR) {
    return 1;
  } else {
    String_sp temp = SimpleBaseString_O::make(std::string(s, strlen(s)));
    file_libc_error(core::_sym_simpleStreamError, strm,
                    "C operation (~A) signaled an error.",
                    1, temp.raw_());
    return 0;
  }
}

static void
io_error(T_sp strm) {
  cl_env_ptr the_env = clasp_process_env();
  /* clasp_disable_interrupts(); ** done by caller */
  maybe_clearerr(strm);
  clasp_enable_interrupts_env(the_env);
  file_libc_error(core::_sym_simpleStreamError, strm,
                  "Read or write operation signaled an error", 0);
}

static void
wrong_file_handler(T_sp strm) {
  FEerror("Internal error: stream ~S has no valid C file handler.", 1, strm.raw_());
}

#ifdef CLASP_UNICODE
SYMBOL_EXPORT_SC_(ExtPkg,encoding_error);
CL_DEFUN cl_index ext__encoding_error(T_sp stream, unsigned char *buffer, claspCharacter c) {
  T_sp code = eval::funcall(ext::_sym_encoding_error, stream,
                            StreamExternalFormat(stream),
                            Integer_O::create((Fixnum)c));
  if (code.nilp()) {
    /* Output nothing */
    return 0;
  } else {
    /* Try with supplied character */
    return StreamEncoder(stream)(stream, buffer, clasp_as_claspCharacter(gc::As<Character_sp>(code)));
  }
}

SYMBOL_EXPORT_SC_(ExtPkg,decoding_error);
CL_DEFUN claspCharacter ext__decoding_error(T_sp stream, unsigned char *buffer, int length) {
  T_sp octets = _Nil<T_O>(), code;
  while (length > 0) {
    octets = Cons_O::create(make_fixnum(buffer[--length]), octets);
  }
  code = eval::funcall(ext::_sym_decoding_error, stream,
                       StreamExternalFormat(stream),
                       octets);
  if (code.nilp()) {
    /* Go for next character */
    return StreamDecoder(stream)(stream);
  } else {
    /* Return supplied character */
    return clasp_as_claspCharacter(gc::As<Character_sp>(code));
  }
}
#endif

#if defined(ECL_WSOCK)
static void
wsock_error(const char *err_msg, T_sp strm) {
  char *msg;
  T_sp msg_obj;
  /* clasp_disable_interrupts(); ** done by caller */
  {
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER,
                  0, WSAGetLastError(), 0, (void *)&msg, 0, NULL);
    msg_obj = make_base_string_copy(msg);
    LocalFree(msg);
  }
  clasp_enable_interrupts();
  FEerror(err_msg, 2, strm.raw_(), msg_obj.raw_());
}
#endif

CL_LAMBDA(stream);
CL_DECLARE();
CL_DOCSTRING("streamLinenumber");
CL_DEFUN int core__stream_linenumber(T_sp tstream) {
  return clasp_input_lineno(tstream);
};

CL_LAMBDA(stream);
CL_DECLARE();
CL_DOCSTRING("streamColumn");
CL_DEFUN int core__stream_column(T_sp tstream) {
  return clasp_input_column(tstream);
};
};

namespace core {
void clasp_write_characters(const char *buf, int sz, T_sp strm) {
  for (int i(0); i < sz; ++i) {
    clasp_write_char(buf[i], strm);
  }
}

void clasp_write_string(const string &str, T_sp strm) {
  clasp_write_characters(str.c_str(), str.size(), strm);
}

void clasp_writeln_string(const string &str, T_sp strm) {
  clasp_write_string(str, strm);
  clasp_terpri(strm);
  clasp_finish_output(strm);
}

T_sp clasp_filename(T_sp strm, bool errorp) {
  T_sp fn = _Nil<T_O>();
  if (AnsiStreamP(strm)) {
    Stream_sp ss = gc::As<Stream_sp>(strm);
    fn = ss->filename();
  }
  if (fn.nilp()) {
    if (errorp) {
      SIMPLE_ERROR(BF("The stream %s does not have a filename") % _rep_(strm));
    } else {
      return SimpleBaseString_O::make("-no-name-");
    }
  }
  return fn;
}

size_t clasp_input_filePos(T_sp strm) {
  T_sp position = clasp_file_position(strm);
  if (position == kw::_sym_start)
    return 0;
  else if (position == kw::_sym_end) {
    SIMPLE_ERROR(BF("Handle clasp_input_filePos getting :end"));
  } else if (position.nilp()) {
    return 0;
    //	    SIMPLE_ERROR(BF("Stream does not have file position"));
  } else if (position.fixnump()) { // Fixnum_sp ii = position.asOrNull<Fixnum_O>() ) {
    return unbox_fixnum(gc::As<Fixnum_sp>(position));
  }
  return 0;
}

int clasp_input_lineno(T_sp strm) {
  if (Stream_sp sin = strm.asOrNull<Stream_O>()) {
    StreamCursor &ic = StreamInputCursor(sin);
    return ic._LineNumber;
  }
  return 0;
}

int clasp_input_column(T_sp strm) {
  if (Stream_sp sin = strm.asOrNull<Stream_O>()) {
    StreamCursor &ic = StreamInputCursor(sin);
    return ic._Column;
  }
  return 0;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("sourcePosInfo");
CL_DEFUN SourcePosInfo_sp core__input_stream_source_pos_info(T_sp strm) {
  strm = coerce::inputStreamDesignator(strm);
  FileScope_sp sfi = clasp_input_source_file_info(strm);
  size_t filePos = clasp_input_filePos(strm);
  uint lineno, column;
  lineno = clasp_input_lineno(strm);
  column = clasp_input_column(strm);
  SourcePosInfo_sp spi = SourcePosInfo_O::create(sfi->fileHandle(), filePos, lineno, column);
  return spi;
}

FileScope_sp clasp_input_source_file_info(T_sp strm) {
  T_sp filename = clasp_filename(strm);
  FileScope_sp sfi = gc::As<FileScope_sp>(core__file_scope(filename));
  return sfi;
}
};

namespace core {














};

namespace core {

Stream_O::~Stream_O() {
  gctools::clasp_dealloc(this->_Buffer);
  this->_Buffer = NULL;
};

IOStreamStream_O::~IOStreamStream_O() {
    stream_dispatch_table(this->asSmartPtr()).close(this->asSmartPtr());
}

IOFileStream_O::~IOFileStream_O() {
  stream_dispatch_table(this->asSmartPtr()).close(this->asSmartPtr());
}


T_sp Stream_O::filename() const {
  return _Nil<T_O>();
};

int Stream_O::lineno() const {
  return 0;
};

int Stream_O::column() const {
  return 0;
};

T_sp SynonymStream_O::filename() const {
  T_sp strm = SynonymStreamStream(this->asSmartPtr());
  return clasp_filename(strm);
};
};

namespace core {


void StringOutputStream_O::fill(const string &data) {
  StringPushStringCharStar(this->_Contents,data.c_str());
}

/*! Get the contents and reset them */
String_sp StringOutputStream_O::getAndReset() {
  String_sp contents = this->_Contents;
  StringOutputStreamOutputString(this->asSmartPtr()) = Str8Ns_O::createBufferString(128);
  StreamOutputColumn(this->asSmartPtr()) = 0;
  return contents;
};
};

namespace core {

string FileStream_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString();
  ss << " " << _rep_(this->filename());
  if (!this->_Closed) {
    ss <<  " file-pos ";
    ss << _rep_(clasp_file_position(this->asSmartPtr()));
  }
  ss << ">";
  return ss.str();
}

string SynonymStream_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " ";
  ss << _rep_(this->_SynonymSymbol) << ">";
  return ss.str();
}

T_sp StringInputStream_O::make(const string &str) {
  String_sp s = str_create(str);
  return cl__make_string_input_stream(s, make_fixnum(0), _Nil<T_O>());
}

string StringInputStream_O::peer(size_t len)
{
  size_t remaining = this->_InputLimit-this->_InputPosition;
  len = MIN(len,remaining);
  stringstream ss;
  for (size_t i=0; i<len; ++i ) {
    ss << (char)(this->_Contents->rowMajorAref(this->_InputPosition+i).unsafe_character()&0x7F);
  }
  return ss.str();
}

T_sp IOFileStream_O::make(const string &name, int fd, enum StreamMode smm, T_sp elementType, T_sp externalFormat) {
  String_sp sname = str_create(name);
  T_sp stream = clasp_make_file_stream_from_fd(sname, fd, smm, 8, CLASP_STREAM_DEFAULT_FORMAT, externalFormat);
  FileStreamEltType(stream) = elementType;
  return stream;
}

CL_LAMBDA(strm &optional (eof-error-p t) eof-value);
CL_DECLARE();
CL_DOCSTRING("readByte");
CL_DEFUN T_sp cl__read_byte(T_sp strm, T_sp eof_error_p, T_sp eof_value) {
  // Should signal an error of type type-error if stream is not a stream.
  // Should signal an error of type error if stream is not a binary input stream.
  if (strm.nilp())
    TYPE_ERROR(strm, cl::_sym_Stream_O);
   if (!AnsiStreamP(strm))
     return eval::funcall(gray::_sym_stream_read_byte, strm);
  // as a side effect verifies that strm is really a stream.
  T_sp elt_type = clasp_stream_element_type(strm);
  if (elt_type == cl::_sym_character || elt_type == cl::_sym_base_char)
    SIMPLE_ERROR_BF("Not a binary stream");
  T_sp c = clasp_read_byte(strm);
  if (c.nilp()) {
    LOG(BF("Hit eof"));
    if (!eof_error_p.isTrue()) {
      LOG(BF("Returning eof_value[%s]") % _rep_(eof_value));
      return eof_value;
    }
    ERROR_END_OF_FILE(strm);
  }
  LOG(BF("Read and returning char[%s]") % c);
  return c;
}

CL_LAMBDA(&optional peek-type strm (eof-errorp t) eof-value recursivep);
CL_DECLARE();
CL_DOCSTRING("peekChar");
CL_DEFUN T_sp cl__peek_char(T_sp peek_type, T_sp strm, T_sp eof_errorp, T_sp eof_value, T_sp recursive_p) {
  strm = coerce::inputStreamDesignator(strm);
  if (!clasp_input_stream_p(strm))
    SIMPLE_ERROR(BF("Not input-stream"));
  if (peek_type.nilp()) {
    int c = clasp_peek_char(strm);
    if (c == EOF)
      goto HANDLE_EOF;
    return clasp_make_character(clasp_peek_char(strm));
  }
  if (cl__characterp(peek_type)) {
    claspCharacter looking_for = clasp_as_claspCharacter(gc::As<Character_sp>(peek_type));
    while (1) {
      int c = clasp_peek_char(strm);
      if (c == EOF) goto HANDLE_EOF;
      if (c == looking_for) return clasp_make_character(c);
      clasp_read_char(strm);
    }
  }
  // Now peek_type is true - this means skip whitespace until the first non-whitespace character
  if (peek_type != _lisp->_true()) {
    SIMPLE_ERROR(BF("Illegal first argument for PEEK-CHAR %s") % _rep_(peek_type));
  } else {
    T_sp readtable = _lisp->getCurrentReadTable();
    while (1) {
      int c = clasp_peek_char(strm);
      if (c == EOF)
        goto HANDLE_EOF;
      Character_sp charc = clasp_make_character(c);
      if (core__syntax_type(readtable, charc) != kw::_sym_whitespace)
        return charc;
      clasp_read_char(strm);
    }
  }
HANDLE_EOF:
  if (eof_errorp.isTrue())
    ERROR_END_OF_FILE(strm);
  return eof_value;
}

CL_LAMBDA(&optional strm (eof-error-p t) eof-value recursive-p);
CL_DECLARE();
CL_DOCSTRING("readChar");
CL_DEFUN T_sp cl__read_char(T_sp strm, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p) {
  strm = coerce::inputStreamDesignator(strm);
  int c = clasp_read_char(strm);
  if (c == EOF) {
    LOG(BF("Hit eof"));
    if (!eof_error_p.isTrue()) {
      LOG(BF("Returning eof_value[%s]") % _rep_(eof_value));
      return eof_value;
    }
    ERROR_END_OF_FILE(strm);
  }
  LOG(BF("Read and returning char[%s]") % c);
  return clasp_make_character(c);
}

CL_LAMBDA(&optional strm (eof-error-p t) eof-value recursive-p);
CL_DECLARE();
CL_DOCSTRING("readCharNoHang");
CL_DEFUN T_sp cl__read_char_no_hang(T_sp strm, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p) {
  strm = coerce::inputStreamDesignator(strm);
  if (!AnsiStreamP(strm)) {
    T_sp output = eval::funcall(gray::_sym_stream_read_char_no_hang, strm);
    if (output == kw::_sym_eof) {
      goto END_OF_FILE;
    }
    return output;
  }
  {
    int f = clasp_listen_stream(strm);
    if (f == CLASP_LISTEN_AVAILABLE) {
      int c = clasp_read_char(strm);
      if (c != EOF)
        return clasp_make_standard_character(c);
    } else if (f == CLASP_LISTEN_NO_CHAR) {
      return _Nil<T_O>();
    }
  }
END_OF_FILE:
  if (eof_error_p.nilp())
    return eof_value;
  ERROR_END_OF_FILE(strm);
}

CL_LAMBDA(content &optional (eof-error-p t) eof-value &key (start 0) end preserve-whitespace);
CL_DECLARE();
CL_DOCSTRING("read_from_string");
CL_DEFUN T_mv cl__read_from_string(String_sp content, T_sp eof_error_p, T_sp eof_value, Fixnum_sp start, T_sp end, T_sp preserve_whitespace) {
  ASSERT(cl__stringp(content));
  bool eofErrorP = eof_error_p.isTrue();
  int istart = clasp_to_int(start);
  int iend;
  if (end.nilp())
    iend = content->get().size();
  else
    iend = clasp_to_int(gc::As<Fixnum_sp>(end));
  StringInputStream_sp sin = gc::As_unsafe<StringInputStream_sp>(StringInputStream_O::make(content->get().substr(istart, iend - istart)));
  if (iend - istart == 0) {
    if (eofErrorP) {
      ERROR_END_OF_FILE(sin);
    } else {
      return (Values(eof_value, _lisp->_true()));
    }
  }
  LOG(BF("Seeking to position: %d") % start);
  LOG(BF("Character at position[%d] is[%c/%d]") % sin->tell() % (char)sin->peek_char() % (int)sin->peek_char());
  T_sp res;
  if ( preserve_whitespace.isTrue() ) {
     res = cl__read_preserving_whitespace(sin, _Nil<T_O>(), _Unbound<T_O>(), _Nil<T_O>());
  } else {
     res = cl__read(sin, _Nil<T_O>(), _Unbound<T_O>(), _Nil<T_O>());
  }
  if (res.unboundp()) {
    if (eofErrorP) {
      ERROR_END_OF_FILE(sin);
    } else {
      return (Values(eof_value, _lisp->_true()));
    }
  }
  return (Values(res, clasp_file_position(sin)));
}

CL_LAMBDA(&optional input-stream (eof-error-p t) eof-value recursive-p);
CL_DECLARE();
CL_DOCSTRING("See clhs");
CL_DEFUN T_mv cl__read_line(T_sp sin, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p) {
  // TODO Handle encodings from sin - currently only Str8Ns is supported
  bool eofErrorP = eof_error_p.isTrue();
  sin = coerce::inputStreamDesignator(sin);
  if (!AnsiStreamP(sin)) {
    T_mv results = eval::funcall(gray::_sym_stream_read_line, sin);
    if (results.second().isTrue()) {
      if (eof_error_p.notnilp()) {
        ERROR_END_OF_FILE(sin);
      } else {
        return Values(eof_value,_lisp->_true());
      }
    }
  }
  //    bool recursiveP = translate::from_object<bool>::convert(env->lookup(_sym_recursive_p));
  bool small = true;
  Str8Ns_sp sbuf_small = Str8Ns_O::createBufferString();
  StrWNs_sp sbuf_wide;
  while (1) {
    T_sp tch = cl__read_char(sin, _Nil<T_O>(), _Nil<T_O>(), recursive_p);
    if (tch.nilp()) {
      if (eofErrorP) {
        // If something has been read in this line, no error and it must be returned
        if (small) {
          if (sbuf_small->length()>0)
            return Values(sbuf_small, _lisp->_true());
          else ERROR_END_OF_FILE(sin);
        }
        else {
          if (sbuf_wide->length()>0)
            return Values(sbuf_wide, _lisp->_true());
          else ERROR_END_OF_FILE(sin);
        }
      } else {
        // tch == nil but no eof
        if (small) {
          if (sbuf_small->length()>0)
            return Values(sbuf_small, _Nil<T_O>());
          else return Values(eof_value, _lisp->_true());
        }
        else {
          if (sbuf_wide->length()>0)
            return Values(sbuf_wide, _Nil<T_O>());
          else return Values(eof_value, _lisp->_true());
        }
      }
    } else {
      claspCharacter cc = (gc::As<Character_sp>(tch)).unsafe_character();
      if (small && (!clasp_base_char_p(cc))) {
        // first wide char read
        small = false;
        sbuf_wide = StrWNs_O::createBufferString();
        sbuf_wide->unsafe_setf_subseq(0,sbuf_small->length(),sbuf_small->asSmartPtr());
       }
      if (cc == '\n') {
        break;
      } else if (cc == '\r') {
        if (clasp_peek_char(sin) == '\n') {
          clasp_read_char(sin);
        }
        break;
      }
      if (small)
        sbuf_small->vectorPushExtend_claspChar(cc);
      else sbuf_wide->vectorPushExtend_claspCharacter(cc);
    }
  }
  LOG(BF("Read line result -->[%s]") % sbuf.str());
   if (small)
     return Values(sbuf_small, _Nil<T_O>());
   else return Values(sbuf_wide, _Nil<T_O>());
}

void clasp_terpri(T_sp s) {
  s = coerce::outputStreamDesignator(s);
  if (!AnsiStreamP(s)) {
    eval::funcall(gray::_sym_stream_terpri, s);
    return;
  }
  clasp_write_char('\n', s);
  clasp_force_output(s);
}

CL_LAMBDA(&optional output-stream);
CL_DECLARE();
CL_DOCSTRING("Send a newline to the output stream");
CL_DEFUN T_sp cl__terpri(T_sp outputStreamDesig) {
  // outputStreamDesign in clasp_terpri
  clasp_terpri(outputStreamDesig);
  return _Nil<T_O>();
};

bool clasp_freshLine(T_sp s) {
  s = coerce::outputStreamDesignator(s);
  if (!AnsiStreamP(s)) {
    return T_sp(eval::funcall(gray::_sym_stream_fresh_line, s)).isTrue();
  }
  if (clasp_file_column(s) != 0) {
    clasp_write_char('\n', s);
    clasp_force_output(s);
    return true;
  }
  return false;
}

CL_LAMBDA(&optional outputStream);
CL_DECLARE();
CL_DOCSTRING("freshLine");
CL_DEFUN bool cl__fresh_line(T_sp outputStreamDesig) {
  // outputStreamDesignator in clasp_freshLine
  return clasp_freshLine(outputStreamDesig);
};

String_sp clasp_writeString(String_sp str, T_sp stream, int istart, T_sp end) {
  ASSERT(cl__stringp(str));
  stream = coerce::outputStreamDesignator(stream);
  if (!AnsiStreamP(stream)) {
    Fixnum_sp fnstart = make_fixnum(istart);
    eval::funcall(gray::_sym_stream_write_string, stream, str, fnstart, end);
    return str;
  }
  int iend = cl__length(str);
  if (end.notnilp()) {
    iend = MIN(iend, unbox_fixnum(gc::As<Fixnum_sp>(end)));
  }
  int ilen = iend - istart;
  clasp_write_characters(&(str->get_std_string().c_str()[istart]), ilen, stream);
  return str;
}

CL_LAMBDA(string &optional (output-stream cl:*standard-output*) &key (start 0) end);
CL_DECLARE();
CL_DOCSTRING("writeString");
CL_DEFUN T_sp cl__write_string(T_sp tstr, T_sp tstream, int start, T_sp end) {
  T_sp stream = coerce::outputStreamDesignator(tstream);
  if ( cl__stringp(tstr)) {
    String_sp str = gc::As_unsafe<String_sp>(tstr);
    clasp_writeString(str, stream, start, end);
  } else if ( Vector_sp vstr = tstr.asOrNull<Vector_O>() ) {
    if ( clasp_is_character_type(vstr->arrayElementType()) ) {
      size_t send;
      if (end.nilp()) {
        send = vstr->arrayTotalSize();
      } else if (end.fixnump()) {
        send = MIN(end.unsafe_fixnum(),vstr->arrayTotalSize());
      } else {
        TYPE_ERROR(end,cl::_sym_integer);
      }
      for (size_t i=start; i<send; ++i ) {
        clasp_write_char(gctools::As<Character_sp>(vstr->rowMajorAref(i)).unsafe_character(),stream);
      }
    } else {
      TYPE_ERROR(tstr,cl::_sym_string);
    }
  }
  return tstr;
};

CL_LAMBDA(string &optional output-stream &key (start 0) end);
CL_DECLARE();
CL_DOCSTRING("writeLine");
CL_DEFUN String_sp cl__write_line(String_sp str, T_sp stream, Fixnum_sp start, T_sp end) {
  stream = coerce::outputStreamDesignator(stream);
  clasp_writeString(str, stream, unbox_fixnum(start), end);
  clasp_terpri(stream);
  return str;
};

CL_LAMBDA(byte output-stream);
CL_DECLARE();
CL_DOCSTRING("writeByte");
CL_DEFUN Integer_sp cl__write_byte(Integer_sp byte, T_sp stream) {
  if (stream.nilp())
    TYPE_ERROR(stream, cl::_sym_Stream_O);
  // clhs in 21.2 says stream---a binary output stream, not mentioning a stream designator
  clasp_write_byte(byte, stream);
  return (byte);
};

CL_LAMBDA(string &optional output-stream);
CL_DECLARE();
CL_DOCSTRING("writeChar");
CL_DEFUN Character_sp cl__write_char(Character_sp chr, T_sp stream) {
  stream = coerce::outputStreamDesignator(stream);
  clasp_write_char(clasp_as_claspCharacter(chr), stream);
  return chr;
};

CL_LAMBDA(&optional dstrm);
CL_DECLARE();
CL_DOCSTRING("clearInput");
CL_DEFUN T_sp cl__clear_input(T_sp dstrm) {
  dstrm = coerce::inputStreamDesignator(dstrm);
  clasp_clear_input(dstrm);
  return _Nil<T_O>();
}

CL_LAMBDA(&optional dstrm);
CL_DECLARE();
CL_DOCSTRING("clearOutput");
CL_DEFUN T_sp cl__clear_output(T_sp dstrm) {
  dstrm = coerce::outputStreamDesignator(dstrm);
  clasp_clear_output(dstrm);
  return _Nil<T_O>();
}

CL_LAMBDA(&optional dstrm);
CL_DECLARE();
CL_DOCSTRING("listen");
CL_DEFUN bool cl__listen(T_sp strm) {
  strm = coerce::inputStreamDesignator(strm);
  int result = clasp_listen_stream(strm);
  if (result == CLASP_LISTEN_EOF)
    return 0;
  else return result;
}

CL_LAMBDA(&optional strm);
CL_DECLARE();
CL_DOCSTRING("force_output");
CL_DEFUN T_sp cl__force_output(T_sp ostrm) {
  ostrm = coerce::outputStreamDesignator(ostrm);
  clasp_force_output(ostrm);
  return _Nil<T_O>();
};

CL_LAMBDA(&optional strm);
CL_DECLARE();
CL_DOCSTRING("finish_output");
CL_DEFUN T_sp cl__finish_output(T_sp ostrm) {
  ostrm = coerce::outputStreamDesignator(ostrm);
  clasp_finish_output(ostrm);
  return _Nil<T_O>();
};

CL_LAMBDA(char &optional strm);
CL_DECLARE();
CL_DOCSTRING("unread_char");
CL_DEFUN T_sp cl__unread_char(Character_sp ch, T_sp dstrm) {
  dstrm = coerce::inputStreamDesignator(dstrm);
  clasp_unread_char(clasp_as_claspCharacter(ch), dstrm);
  return _Nil<T_O>();
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("column");
CL_DEFUN T_sp core__file_column(T_sp strm) {
  strm = coerce::outputStreamDesignator(strm);
  return make_fixnum(clasp_file_column(strm));
};

/*! Translated from ecl::si_do_write_sequence */
CL_LAMBDA(seq stream &key (start 0) end);
CL_DECLARE();
CL_DOCSTRING("writeSequence");
CL_DEFUN T_sp cl__write_sequence(T_sp seq, T_sp stream, Fixnum_sp fstart, T_sp tend) {
  stream = coerce::outputStreamDesignator(stream);
  if (!AnsiStreamP(stream)) {
    return eval::funcall(gray::_sym_stream_write_sequence, stream, seq, fstart, tend);
  }
  int limit = cl__length(seq);
  unlikely_if(!core__fixnump(fstart) ||
              (unbox_fixnum(fstart) < 0) ||
              (unbox_fixnum(fstart) > limit)) {
    ERROR_WRONG_TYPE_KEY_ARG(cl::_sym_write_sequence, kw::_sym_start, fstart,
                             Integer_O::makeIntegerType(0, limit - 1));
  }
  int start = unbox_fixnum(fstart);
  int end;
  if (tend.notnilp()) {
    end = unbox_fixnum(gc::As<Fixnum_sp>(tend));
    unlikely_if(!core__fixnump(tend) ||
                (end < 0) ||
                (end > limit)) {
      ERROR_WRONG_TYPE_KEY_ARG(cl::_sym_write_sequence, kw::_sym_end, tend,
                               Integer_O::makeIntegerType(0, limit - 1));
    }
  }
  else end = limit;
  if (end == start)
    return seq;
  else
    if  (end < start) {
    // I don't believe that we can silently return seq, sbcl throws an error
    ERROR_WRONG_TYPE_KEY_ARG(cl::_sym_write_sequence, kw::_sym_end, tend,
                               Integer_O::makeIntegerType(start, limit - 1));
  }
  const FileOps &ops = stream_dispatch_table(stream);
  if (cl__listp(seq)) {
    T_sp elt_type = cl__stream_element_type(stream);
    bool ischar = (elt_type == cl::_sym_base_char) || (elt_type == cl::_sym_character);
    T_sp s = cl__nthcdr(clasp_make_integer(start), seq);
    for (;; s = cons_cdr(s)) {
      if (start < end) {
        T_sp elt = oCar(s);
        if (ischar)
          clasp_write_char(clasp_as_claspCharacter(gc::As<Character_sp>(elt)), stream);
        else
          clasp_write_byte(gc::As<Integer_sp>(elt), stream);
        start++;
      } else {
        goto OUTPUT;
      }
    }
  } else {
    ops.write_vector(stream, gc::As<Vector_sp>(seq), start, end);
  }
OUTPUT:
  return seq;
}

T_sp clasp_openRead(T_sp sin) {
  String_sp filename = gc::As<String_sp>(cl__namestring(sin));
  enum StreamMode smm = clasp_smm_input;
  T_sp if_exists = _Nil<T_O>();
  T_sp if_does_not_exist = _Nil<T_O>();
  gctools::Fixnum byte_size = 8;
  int flags = CLASP_STREAM_DEFAULT_FORMAT;
  T_sp external_format = _Nil<T_O>();
  if (filename.nilp()) {
    SIMPLE_ERROR(BF("%s was called with NIL as the argument") % __FUNCTION__);
  }
  T_sp strm = clasp_open_stream(filename, smm, if_exists, if_does_not_exist,
                                byte_size, flags, external_format);
  return strm;
}

T_sp clasp_openWrite(T_sp path) {
  T_sp stream = eval::funcall(cl::_sym_open,path,kw::_sym_direction,kw::_sym_output,kw::_sym_if_exists, kw::_sym_supersede);
  return stream;
}

SYMBOL_EXPORT_SC_(ClPkg,open);
SYMBOL_EXPORT_SC_(KeywordPkg,direction);
SYMBOL_EXPORT_SC_(KeywordPkg,output);
SYMBOL_EXPORT_SC_(KeywordPkg,input);
  SYMBOL_EXPORT_SC_(ClPkg, filePosition);
  SYMBOL_EXPORT_SC_(ClPkg, readSequence);
  SYMBOL_EXPORT_SC_(CorePkg, doReadSequence);
  SYMBOL_EXPORT_SC_(ClPkg, read_from_string);
  SYMBOL_EXPORT_SC_(ClPkg, read_line);
  SYMBOL_EXPORT_SC_(ClPkg, terpri);
  SYMBOL_EXPORT_SC_(ClPkg, freshLine);
  SYMBOL_EXPORT_SC_(ClPkg, writeString);
  SYMBOL_EXPORT_SC_(ClPkg, writeLine);
  SYMBOL_EXPORT_SC_(ClPkg, writeChar);
  SYMBOL_EXPORT_SC_(ClPkg, clearInput);
  SYMBOL_EXPORT_SC_(ClPkg, clearOutput);
  SYMBOL_EXPORT_SC_(ClPkg, readByte);
  SYMBOL_EXPORT_SC_(ClPkg, peekChar);
  SYMBOL_EXPORT_SC_(ClPkg, readChar);
  SYMBOL_EXPORT_SC_(ClPkg, readCharNoHang);
  SYMBOL_EXPORT_SC_(ClPkg, force_output);
  SYMBOL_EXPORT_SC_(ClPkg, finish_output);
  SYMBOL_EXPORT_SC_(ClPkg, listen);
  SYMBOL_EXPORT_SC_(ClPkg, unread_char);
  SYMBOL_EXPORT_SC_(CorePkg, fileColumn);
  SYMBOL_EXPORT_SC_(CorePkg, makeStringOutputStreamFromString);
  SYMBOL_EXPORT_SC_(ClPkg, makeStringOutputStream);
  SYMBOL_EXPORT_SC_(CorePkg, do_write_sequence);
  SYMBOL_EXPORT_SC_(ClPkg, writeByte);
  SYMBOL_EXPORT_SC_(ClPkg, input_stream_p);
  SYMBOL_EXPORT_SC_(ClPkg, output_stream_p);
  SYMBOL_EXPORT_SC_(ClPkg, interactive_stream_p);
  SYMBOL_EXPORT_SC_(ClPkg, streamp);
  SYMBOL_EXPORT_SC_(ClPkg, close);
  SYMBOL_EXPORT_SC_(ClPkg, get_output_stream_string);
  SYMBOL_EXPORT_SC_(CorePkg, streamLinenumber);
  SYMBOL_EXPORT_SC_(CorePkg, streamColumn);
  SYMBOL_EXPORT_SC_(ClPkg, synonymStreamSymbol);

CL_DOCSTRING("Use read to read characters if they are available - return (values num-read errno-or-nil)");
CL_DEFUN T_mv core__read_fd(int filedes, SimpleBaseString_sp buffer) {
  char c;
  size_t buffer_length = cl__length(buffer);
  unsigned char* buffer_data = &(*buffer)[0];
  while (1) {
    int num = read(filedes,buffer_data,buffer_length);
    if (!(num<0 && errno==EINTR)) {
      if (num<0) {
        return Values(make_fixnum(num),make_fixnum(errno));
      }
      return Values(make_fixnum(num),_Nil<T_O>());
    }
  }
};


CL_DOCSTRING("Set filedescriptor to nonblocking");
CL_DEFUN void core__fcntl_non_blocking(int filedes) {
  int flags = fcntl(filedes,F_GETFL,0);
  fcntl(filedes,F_SETFL, flags | O_NONBLOCK);
};

CL_DOCSTRING("Close the file descriptor");
CL_DEFUN void core__close_fd(int filedes) {
  close(filedes);
};

SYMBOL_EXPORT_SC_(KeywordPkg,seek_set);
SYMBOL_EXPORT_SC_(KeywordPkg,seek_cur);
SYMBOL_EXPORT_SC_(KeywordPkg,seek_end);
CL_DEFUN int64_t core__lseek(int fd, int64_t offset, Symbol_sp whence)
{
  int iwhence;
  if (whence==kw::_sym_seek_set) {
    iwhence = SEEK_SET;
  } else if (whence == kw::_sym_seek_cur) {
    iwhence = SEEK_CUR;
  } else if (whence == kw::_sym_seek_end) {
    iwhence = SEEK_END;
  } else {
    SIMPLE_ERROR(BF("whence must be one of :seek-set, :seek-cur, :seek-end - it was %s") % _rep_(whence));
  }
  size_t off = lseek(fd,offset,iwhence);
  return off;
};

};
