/*
    File: eclStream.cc
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
#define DEBUG_LEVEL_FULL
#include <stdio.h>
#include "core/common.h"
#include "fileSystem.h"
#include "lispStream.h"
#include "str.h"
#include "sourceFileInfo.h"
#include "symbolTable.h"
#include "corePackage.h"
#include "readtable.h"
#include "lispDefinitions.h"
#include "primitives.h"
#include "multipleValues.h"
#include "strWithFillPtr.h"
#include "designators.h"
#include "reader.h"
#include "lispReader.h"
#include "fileSystem.h"
#include "wrappers.h"

namespace core {

#define ARGS_af_writeChar "(string &optional output-stream)"
#define DECL_af_writeChar ""
#define DOCS_af_writeChar "writeChar"
Character_mv af_writeChar(Character_sp chr, T_sp outputStream) {
  _G();
  Stream_sp stream = coerce::outputStreamDesignator(outputStream);
  stream->writeChar(chr->asChar());
  return (Values(chr));
};

/*
  BRCL stream functions need to be gray stream aware:

  Done:

  read-byte8
  clear-input 
  finish-output
  force-output
  peek-char
  read-char
  listen 
  unread-char
  column 
  write-byte8
  ---------   Still to be implemented:   -------
  read-byte
  write-byte
  write-char 
  input-p
  output-p
  interactive-p
  element-type
  get-position
  set-position
  close
  open-stream-p
  streamp
*/

/********************************************************************************
 * CLOS STREAMS
 */

#ifdef CLOS_STREAMS

static int
clos_stream_read_byte8(T_sp strm, unsigned char *buf, int num) {
  _G();
  int i;
  for (i = 0; i < num; i++) {
    T_sp byte = eval::funcall(gray::_sym_stream_read_byte, strm);
    if (!af_fixnumP(byte))
      break;
    buf[i] = byte.as<Fixnum_O>()->get();
  }
  return i;
};

static int
clos_stream_write_byte8(T_sp strm, const unsigned char *c, int n) {
  _G();
  int i;
  for (i = 0; i < n; i++) {
    T_sp byte = eval::funcall(gray::_sym_stream_write_byte, strm, make_fixnum(c[i]));
    if (!af_fixnumP(byte))
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

static int
clos_stream_read_char(T_sp strm) {
  T_sp output = eval::funcall(gray::_sym_stream_read_char, strm);
  int value;
  if (af_characterP(output))
    value = output.as<Character_O>()->charCode(); // ECL_CHAR_CODE(output);
  else if (af_fixnumP(output))
    value = output.as<Fixnum_O>()->get();
  else if (output.nilp() || output == kw::_sym_eof)
    return EOF;
  else
    value = -1;
  unlikely_if(value < 0 || value > CHAR_CODE_LIMIT) {
    // FEerror("Unknown character ~A", 1, output);
    SIMPLE_ERROR(BF("Unknown character %s") % output);
  }
  return value;
}

static brclChar
clos_stream_write_char(T_sp strm, brclChar c) {
  eval::funcall(gray::_sym_stream_write_char, strm, Character_O::create(c)); // ECL_CODE_CHAR(c));
  return c;
}

static void
clos_stream_unread_char(T_sp strm, brclChar c) {
  eval::funcall(gray::_sym_stream_unread_char, strm, Character_O::create(c)); // ECL_CODE_CHAR(c));
}

static int
clos_stream_peek_char(T_sp strm) {
  T_sp out = eval::funcall(gray::_sym_stream_peek_char, strm);
  if (out == kw::_sym_eof)
    return EOF;
  Character_sp charOut = out.as<Character_O>();
  return charOut->charCode();
}

static int
clos_stream_listen(T_sp strm) {
  return !(eval::funcall(gray::_sym_stream_listen, strm)).nilp();
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
  return !(eval::funcall(gray::_sym_input_stream_p, strm)).nilp();
}

static int
clos_stream_output_p(T_sp strm) {
  return !(eval::funcall(gray::_sym_output_stream_p, strm)).nilp();
}

static int
clos_stream_interactive_p(T_sp strm) {
  return !(eval::funcall(gray::_sym_stream_interactive_p, strm)).nilp();
}

static T_sp
clos_stream_element_type(T_sp strm) {
  return eval::funcall(gray::_sym_stream_element_type, strm);
}

static T_sp
clos_stream_length(T_sp strm) {
  TYPE_ERROR(strm, cl::_sym_fileStream);
}

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
  Fixnum_sp fncol = col.as<Fixnum_O>();
  return (col.nilp()) ? 0 : fncol->get();
}

static T_sp
clos_stream_close(T_sp strm) {
  return eval::funcall(gray::_sym_close, strm);
}

#endif // CLOS_STREAMS

/*---------------------------------------------------
  ---------------------------------------------------
  ---------------------------------------------------
  
  BRCL stream routines - these dispatch to Gray streams
  or regular streams depending on the stream type.

*/

void brcl_write_byte8(T_sp strm, const unsigned char *c, int n) {
  _G();
#if CLOS_STREAMS
  if (strm->instancep()) {
    clos_stream_write_byte8(strm, c, n);
    return;
  }
#endif
  Stream_sp ostrm = strm.as<Stream_O>();
  ostrm->writeBytes(c, n);
}

void brcl_unread_char(T_sp strm, brclChar ch) {
  _G();
#if CLOS_STREAMS
  if (strm->instancep()) {
    return clos_stream_unread_char(strm, ch);
  }
#endif
  Stream_sp ostrm = strm.as<Stream_O>();
  ostrm->unread_char(ch);
};

void brcl_finish_output(T_sp strm) {
  _G();
#if CLOS_STREAMS
  if (strm->instancep()) {
    return clos_stream_finish_output(strm);
  }
#endif
  Stream_sp ostrm = strm.as<Stream_O>();
  return ostrm->finishOutput();
};

void brcl_force_output(T_sp strm) {
  _G();
#if CLOS_STREAMS
  if (strm->instancep()) {
    return clos_stream_force_output(strm);
  }
#endif
  Stream_sp ostrm = strm.as<Stream_O>();
  return ostrm->forceOutput();
};

int brcl_column(T_sp strm) {
  _G();
#if CLOS_STREAMS
  if (strm->instancep()) {
    return clos_stream_column(strm);
  }
#endif
  Stream_sp ostrm = strm.as<Stream_O>();
  return ostrm->column();
};

int brcl_peek_char(T_sp strm) {
  _G();
#if CLOS_STREAMS
  if (strm->instancep()) {
    return clos_stream_peek_char(strm);
  }
#endif
  Stream_sp ostrm = strm.as<Stream_O>();
  return ostrm->peek_char();
};

int brcl_read_char(T_sp strm) {
  _G();
#if CLOS_STREAMS
  if (strm->instancep()) {
    return clos_stream_read_char(strm);
  }
#endif
  Stream_sp ostrm = strm.as<Stream_O>();
  return ostrm->get();
};

bool brcl_listen(T_sp strm) {
  _G();
#if CLOS_STREAMS
  if (strm->instancep()) {
    return clos_stream_listen(strm);
  }
#endif
  Stream_sp ostrm = strm.as<Stream_O>();
  return ostrm->listen() != 0;
};

void brcl_clearInput(T_sp strm) {
  _G();
#if CLOS_STREAMS
  if (strm->instancep()) {
    clos_stream_clear_input(strm);
    return;
  }
#endif
  Stream_sp ostrm = strm.as<Stream_O>();
  ostrm->clearInput();
}

#define ARGS_af_readByte8 "(stream buffer size)"
#define DECL_af_readByte8 ""
#define DOCS_af_readByte8 "readByte8"
int af_readByte8(T_sp strm, unsigned char *c, int n) {
  _G();
#if CLOS_STREAMS
  if (strm->instancep()) {
    return clos_stream_read_byte8(strm, c, n);
  }
#endif
  Stream_sp ostrm = strm.as<Stream_O>();
  return ostrm->read(c, n);
}

#define ARGS_af_writeByte8 "(stream buffer size)"
#define DECL_af_writeByte8 ""
#define DOCS_af_writeByte8 "writeByte8"
int af_writeByte8(T_sp strm, unsigned char *c, int n) {
  _G();
#if CLOS_STREAMS
  if (strm->instancep()) {
    return clos_stream_write_byte8(strm, c, n);
  }
#endif
  Stream_sp ostrm = strm.as<Stream_O>();
  return ostrm->writeBytes(c, n);
}

#define ARGS_af_fileColumn "(arg)"
#define DECL_af_fileColumn ""
#define DOCS_af_fileColumn "column"
T_sp af_fileColumn(T_sp ostrm) {
  _G();
  Stream_sp strm = coerce::outputStreamDesignator(ostrm);
  return make_fixnum(strm->outputColumn());
};

#define ARGS_af_unread_char "(char &optional strm)"
#define DECL_af_unread_char ""
#define DOCS_af_unread_char "unread_char"
void af_unread_char(Character_sp ch, T_sp dstrm) {
  _G();
  Stream_sp strm = coerce::inputStreamDesignator(dstrm);
  brcl_unread_char(strm, ch->asChar());
};

#define ARGS_af_peekChar "(&optional peek_type strm (eof_errorp t) eof_value recursivep)"
#define DECL_af_peekChar ""
#define DOCS_af_peekChar "peekChar"
T_sp af_peekChar(T_sp peek_type, T_sp ostrm, T_sp eof_errorp, T_sp eof_value, T_sp recursive_p) {
  _G();
  Stream_sp strm = coerce::inputStreamDesignator(ostrm);
  if (!strm->inputStreamP())
    SIMPLE_ERROR(BF("Not input-stream"));
  if (peek_type.nilp())
    return Character_O::create(brcl_peek_char(strm));
  if (af_characterP(peek_type)) {
    int looking_for = peek_type.as<Character_O>()->charCode();
    while (1) {
      int c = brcl_peek_char(strm);
      if (c == looking_for) {
        return Character_O::create(c);
      }
      brcl_read_char(strm);
    }
  }
  // Now peek_type is true - this means skip whitespace until the first non-whitespace character
  ASSERT(peek_type == _lisp->_true());
  ReadTable_sp readtable = cl::_sym_STARreadtableSTAR->symbolValue().as<ReadTable_O>();
  while (1) {
    int c = brcl_peek_char(strm);
    Character_sp charc = Character_O::create(c);
    if (readtable->syntax_type(charc) != kw::_sym_whitespace_character) {
      return charc;
    }
    c = brcl_read_char(strm);
    if (c == EOF) {
      if (eof_errorp.isTrue()) {
        SIMPLE_ERROR(BF("Hit end of stream in peek"));
      }
      return eof_value;
    }
  }
}

#define ARGS_af_readChar "(&optional strm eof_error_p eof_value recursive_p)"
#define DECL_af_readChar ""
#define DOCS_af_readChar "readChar"
T_sp af_readChar(T_sp ostrm, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p) {
  _G();
  Stream_sp strm = coerce::inputStreamDesignator(ostrm);
  if (!strm->inputStreamP())
    SIMPLE_ERROR(BF("Not input-stream"));
  int c = strm->get();
  if (c == EOF) {
    LOG(BF("Hit eof"));
    if (!eof_error_p.isTrue()) {
      LOG(BF("Returning eof_value[%s]") % _rep_(eof_value));
      return eof_value;
    }
    END_OF_FILE_ERROR(strm);
  }
  LOG(BF("Read and returning char[%s]") % c);
  return StandardChar_O::create(c);
}

#define ARGS_af_clearInput "(&optional dstrm)"
#define DECL_af_clearInput ""
#define DOCS_af_clearInput "clearInput"
void af_clearInput(T_sp dstrm) {
  _G();
  Stream_sp strm = coerce::inputStreamDesignator(dstrm);
  brcl_clearInput(strm);
}

#define ARGS_af_listen "(&optional dstrm)"
#define DECL_af_listen ""
#define DOCS_af_listen "listen"
bool af_listen(T_sp dstrm) {
  _G();
  Stream_sp strm = coerce::inputStreamDesignator(dstrm);
  return brcl_listen(strm);
}

#define ARGS_af_force_output "(&optional strm)"
#define DECL_af_force_output ""
#define DOCS_af_force_output "force_output"
void af_force_output(T_sp ostrm) {
  _G();
  T_sp strm = coerce::outputStreamDesignator(ostrm);
  brcl_force_output(strm);
};

#define ARGS_af_finish_output "(&optional strm)"
#define DECL_af_finish_output ""
#define DOCS_af_finish_output "finish_output"
void af_finish_output(T_sp ostrm) {
  _G();
  T_sp strm = coerce::outputStreamDesignator(ostrm);
  brcl_finish_output(strm);
};

#define ARGS_af_writeString "(string &optional output-stream &key (start 0) end)"
#define DECL_af_writeString ""
#define DOCS_af_writeString "writeString"
String_mv af_writeString(Str_sp str, T_sp outputStream, Fixnum_sp start, T_sp end) {
  _G();
  Stream_sp stream = coerce::outputStreamDesignator(outputStream);
  int istart = start->get();
  int iend = af_length(str);
  if (end.notnilp()) {
    iend = MIN(iend, end.as<Fixnum_O>()->get());
  }
  string const &sstr = str->_contents();
  for (int i = istart; i < iend; i++)
    stream->writeChar(sstr[i]);
  return (Values(str));
};

#define ARGS_af_writeLine "(string &optional output-stream &key (start 0) end)"
#define DECL_af_writeLine ""
#define DOCS_af_writeLine "writeLine"
String_mv af_writeLine(Str_sp str, T_sp outputStream, Fixnum_sp start, T_sp end) {
  _G();
  Stream_sp stream = coerce::outputStreamDesignator(outputStream);
  int istart = start->get();
  int iend = str->size();
  if (end.notnilp()) {
    iend = MIN(iend, end.as<Fixnum_O>()->get());
  }
  string const &sstr = str->_contents();
  for (int i = istart; i < iend; i++)
    stream->writeChar(sstr[i]);
  stream->writeln("");
  return (Values(str));
};

#define ARGS_af_terpri "(&optional (output-stream ext:+process-standard-output+))"
#define DECL_af_terpri ""
#define DOCS_af_terpri "Send a newline to the output stream"
void af_terpri(T_sp outputStreamDesig) {
  _G();
  Stream_sp outputStream = coerce::outputStreamDesignator(outputStreamDesig);
  outputStream->writeln("");
};

#define ARGS_af_freshLine "(&optional (outputStream ext:+process-standard-output+))"
#define DECL_af_freshLine ""
#define DOCS_af_freshLine "freshLine"
bool af_freshLine(T_sp outputStreamDesig) {
  _G();
  Stream_sp outputStream = coerce::outputStreamDesignator(outputStreamDesig);
  if (!outputStream->atStartOfLine()) {
    outputStream->writeln("");
    return true;
  }
  return false;
};

#define ARGS_af_read_from_string "(content &optional eof-error-p eof-value &key (start 0) (end -1) preserve-whitespace)"
#define DECL_af_read_from_string ""
#define DOCS_af_read_from_string "read_from_string"
T_mv af_read_from_string(Str_sp content, T_sp eof_error_p, T_sp eof_value, Fixnum_sp start, Fixnum_sp end, T_sp preserve_whitespace) {
  _G();
  bool eofErrorP = eof_error_p.isTrue();
  int istart = start->as_int();
  int iend;
  if (end.nilp())
    iend = content->get().size();
  else
    iend = end->as_int();
  bool preserveWhitespace = preserve_whitespace.isTrue();
  if (preserveWhitespace) {
    printf("Implement preserve-whitespace\n");
    IMPLEMENT_ME(); // handle this
  }
  StringInputStream_sp sin = StringInputStream_O::create(content->get().substr(istart, iend - istart));
  if (iend - istart == 0) {
    if (eofErrorP) {
      END_OF_FILE_ERROR(sin);
    } else {
      return (Values(eof_value, _lisp->_true()));
    }
  }
  LOG(BF("Seeking to position: %d") % start);
  LOG(BF("Character at position[%d] is[%c/%d]") % sin->tell() % (char)sin->peek_char() % (int)sin->peek_char());
#if 0
	Reader_sp reader = Reader_O::create(sin,_lisp);
	T_sp res = reader->read(false,_lisp->_eof());
#endif
  T_sp res = read_lisp_object(sin, false, _Unbound<T_O>(), false, _lisp);
  if (res.unboundp()) {
    if (eofErrorP) {
      END_OF_FILE_ERROR(sin);
    } else {
      return (Values(eof_value, _lisp->_true()));
    }
  }
  return (Values(res, make_fixnum(sin->tell())));
}

#define DOCS_af_read_line "See clhs"
#define LOCK_af_read_line 1
#define ARGS_af_read_line "(&optional input-stream (eof-error-p t) eof-value recursive-p)"
#define DECL_af_read_line ""
T_mv af_read_line(T_sp input_stream, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p) {
  _G();
  Stream_sp sin = coerce::inputStreamDesignator(input_stream);
  bool eofErrorP = eof_error_p.isTrue();
  //    bool recursiveP = translate::from_object<bool>::convert(env->lookup(_sym_recursive_p));
  if (sin.nilp()) {
    SIMPLE_ERROR(BF("Currently I can't handle stdin as input - create a *standard-input* object"));
  }
  stringstream sbuf;
  while (1) {
    Character_sp ch = af_readChar(sin, _Nil<T_O>(), _Nil<T_O>(), recursive_p).as<Character_O>();
    if (ch.nilp()) {
      if (eofErrorP) {
        END_OF_FILE_ERROR(sin);
      } else {
        return (Values(Str_O::create(sbuf.str()), _lisp->_true()));
      }
    } else {
      char cc = ch->get();
      if (cc == '\n') {
        break;
      } else if (cc == '\r') {
        if (sin->peek_char() == '\n') {
          sin->get();
        }
        break;
      }
      sbuf << cc;
    }
  }
  LOG(BF("Read line result -->[%s] hitEof=%d") % sbuf.str() % hitEof);
  return (Values(Str_O::create(sbuf.str()), _Nil<T_O>()));
}

#define ARGS_Stream_O_close "((stream stream) &key abort)"
#define DECL_Stream_O_close ""
#define DOCS_Stream_O_close "See CLHS: close"
void Stream_O::close(bool abort) {
  _G();
  SUBCLASS_MUST_IMPLEMENT();
}

int Stream_O::read(unsigned char *buffer, int num) {
  int numAvailable = this->listen();
  if (numAvailable < num)
    num = numAvailable;
  while (num) {
    char c = this->get();
    *buffer = c;
    ++buffer;
    --num;
  }
  return num;
}

void Stream_O::writeBytes(const unsigned char *c, int n) {
  for (const unsigned char *cp = c; n > 0; --n) {
    this->writeChar(static_cast<char>(*cp));
    ++cp;
  }
}

Fixnum_sp Stream_O::writeVector(Vector_sp vec, Fixnum_sp fnstart, Fixnum_sp fnend) {
  size_t start = fnstart->get();
  size_t end = fnend.nilp() ? vec->length() : fnend->get();
  if (start >= end) {
    return fnstart;
  }
  T_sp elementType = vec->elementType();
  if (elementType == cl::_sym_Character_O || elementType == cl::_sym_BaseChar_O

#ifdef BRCL_UNICODE
// elementType == cl::_sym_UnicodeCharacter???
#endif
                                                 /* TODO: Handle specialized arrays */
      ) {
    for (; start < end; start++) {
      this->writeChar(vec->elt(start).as<Character_O>()->asChar());
    }
  } else {
    this->writeByte(vec->elt(start));
  }
  return make_fixnum(static_cast<int>(start));
};

void Stream_O::writeStr(const string &str) {
  for (const char *cp = str.c_str(); *cp; ++cp) {
    this->writeChar(static_cast<char>(*cp));
  }
}

void Stream_O::clearInput() {
  if (this->inputStreamP()) {
    SIMPLE_ERROR(BF("Add support for clearInput for %s") % _rep_(this));
  }
  SIMPLE_ERROR(BF("clear-input not supported on output-stream"));
}

EXPOSE_CLASS(core, Stream_O);

void Stream_O::exposeCando(Lisp_sp lisp) {
  _G();
  class_<Stream_O>(no_init)
      .def("close", &Stream_O::close, ARGS_Stream_O_close, DECL_Stream_O_close, DOCS_Stream_O_close)
      .def("stream-lineNumber", &Stream_O::lineNumber)
      .def("stream-column", &Stream_O::column);
}

void Stream_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, Stream, "", "", _lisp);
#endif
}

EXPOSE_CLASS(core, AnsiStream_O);

void AnsiStream_O::lisp_initGlobals(Lisp_sp lisp) {
  _G();
}

int AnsiStream_O::get() {
  _G();
  if (!this->inputStreamP())
    SIMPLE_ERROR(BF("Not input-stream"));
  // get a raw character
  int c = this->_get();
  if (c == EOF)
    return EOF;
  if (c == '\n') {
    this->advanceLineNumber();
  } else if (c == '\r') {
    if (this->peek_char() == '\n') {
      c = this->_get();
    }
    this->advanceLineNumber();
  } else {
    this->advanceColumn();
  }
  return c;
}

void AnsiStream_O::unread_char(brclChar cc) {
  _OF();
  this->putback(cc);
}

void AnsiStream_O::exposeCando(Lisp_sp lisp) {
  _G();
  class_<AnsiStream_O>(no_init);
}

void AnsiStream_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, Stream, "", "", _lisp);
#endif
}

// ------------------------------------------------------------

EXPOSE_CLASS(core, FileStream_O);

void FileStream_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<FileStream_O>();
}

void FileStream_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, FileStream, "", "", _lisp);
#endif
}

EXPOSE_CLASS(core, FileInStream_O);
void FileInStream_O::initialize() {
  this->Base::initialize();
}

#define ARGS_FileInStream_O_make "(self fileName)"
#define DECL_FileInStream_O_make ""
#define DOCS_FileInStream_O_make ""
FileInStream_sp FileInStream_O::make(T_sp fileName) {
  _G();
  GC_RESERVE_BEGIN(FileInStream_O, fin) {
    GC_RESERVE_GET(FileInStream_O, fin);
  }
  GC_RESERVE_END(fin);
  fin->_SourceFileInfo = SourceFileInfo_O::getOrCreate(coerce::pathDesignator(fileName));
  fin->_InStream.open(fin->_SourceFileInfo->fileName().c_str());
  return fin;
}

void FileInStream_O::exposeCando(Lisp_sp lisp) {
  class_<FileInStream_O>();
  Defun_maker(CorePkg, FileInStream);
  //	af_def(CurrentPkg,"make-FileInStream",&FileInStream_O::make);
}

void FileInStream_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, FileInStream, "", "", _lisp);
#endif
}

FileInStream_sp FileInStream_O::create(Path_sp path, Lisp_sp lisp) {
  _G();
  GC_RESERVE_BEGIN(FileInStream_O, fin) {
    GC_RESERVE_GET(FileInStream_O, fin);
  }
  GC_RESERVE_END(fin);
  fin->_SourceFileInfo = SourceFileInfo_O::getOrCreate(path->asString());
  LOG(BF("Opening file[%s]") % fin->_SourceFileInfo->fileName());
  fin->_InStream.open(fin->_SourceFileInfo->pathName().c_str());
  if (!fin->_InStream.good()) {
    FILE_ERROR(fin->_SourceFileInfo);
  }
  return fin;
}

string FileInStream_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString() << " :sourceFileInfo " << this->sourceFileInfo()->fileName() << " :line-number " << this->lineNumber() << " >";
  return ss.str();
}

void FileInStream_O::clearInput() {
  this->_Unput.clear();
  this->_InStream.ignore(std::numeric_limits<std::streamsize>::max(), EOF);
}

#if 0
    void FileInStream_O::readLine(string& buf, bool& hitEof )
    {
	stringstream sstore;
    }
#endif
string FileInStream_O::readEntireFile() {
  this->_Unput.clear();
  this->_InStream.seekg(0, ios_base::end);
  LongLongInt fileSize = this->_InStream.tellg();
  this->_InStream.seekg(0, ios_base::beg);
  char *buffer = (char *)malloc(fileSize + 1);
  for (LongLongInt i = 0; i < fileSize; i++) {
    buffer[i] = this->get(); // read file and keep track of line numbers
  }
  buffer[fileSize] = '\0';
  string fileContents = buffer;
  return fileContents;
}

LongLongInt FileInStream_O::tell() {
  return (LongLongInt)(this->_InStream.tellg()) + this->_Unput.tellg();
}

void FileInStream_O::seek(LongLongInt pos) {
  this->_Unput.clear();
  this->_InStream.seekg(pos, ios_base::beg);
  this->_Cursor.invalidate();
}

LongLongInt FileInStream_O::fileSize() {
  streampos pos = this->_InStream.tellg();
  this->_InStream.seekg(0, ios_base::end);
  LongLongInt fileSize = this->_InStream.tellg();
  this->_InStream.seekg(pos, ios_base::beg);
  return fileSize;
}

void FileInStream_O::close(bool abort) {
  _OF();
  if (abort) {
    _lisp->print(BF("-----Close for %s with abort=true called - I don't currently support abort=true") % this->_instanceClass()->classNameAsString());
  }
  this->_InStream.close();
}

#if 0
    int FileInStream_O::read(unsigned char* buf, int size)
    {
	this->_InStream.read((char*)buf,size);
	this->_Cursor.invalidate();
	return this->_InStream.gcount();
    }
#endif

LongLongInt FileInStream_O::gcount() {
  return this->_InStream.gcount();
}

void FileInStream_O::putback(char c) {
  this->_Unput.updateCursor(this->_Cursor);
  this->_Unput.putback(c);
}

int FileInStream_O::peek_char() {
  return this->_Unput.has_unput_char() ? this->_Unput.peek_char() : this->_InStream.peek();
}

int FileInStream_O::_get() {
  return this->_Unput.has_unput_char() ? this->_Unput.get_char() : this->_InStream.get();
}

#if 0
    void FileInStream_O::readLine(string& sbuf, bool& hitEof)
    {_OF();
	getline(this->_InStream,sbuf);
	this->_Cursor.advanceLineNumber();
	hitEof = this->_InStream.eof();
    }
#endif

bool FileInStream_O::good() const {
  return this->_InStream.good();
}

bool FileInStream_O::eof() const {
  return this->_InStream.eof();
}

int FileInStream_O::listen() {
  streampos current = this->_InStream.tellg();
  this->_InStream.seekg(0, ios_base::end);
  int bytes_left = this->_InStream.tellg() - current + (this->_Unput.has_unput_char() ? 1 : 0);
  this->_InStream.seekg(current, ios_base::beg);
  return bytes_left;
}

EXPOSE_CLASS(core, FileOutStream_O);

void FileOutStream_O::exposeCando(Lisp_sp lisp) {
  class_<FileOutStream_O>();
}

void FileOutStream_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, FileOutStream, "", "", _lisp);
#endif //]
}

FileOutStream_sp FileOutStream_O::create(Path_sp currentPath, ios_base::openmode mode, Lisp_sp lisp) {
  _G();
  GC_RESERVE_BEGIN(FileOutStream_O, fout) {
    GC_RESERVE_GET(FileOutStream_O, fout);
  }
  GC_RESERVE_END(fout);
  fout->_ActiveFilePath = currentPath;
  fout->_RenameToOriginalOnClose = false;
  fout->_OriginalFilePath = _Nil<Path_O>();
  fout->_Stream.open(currentPath->asString().c_str(), mode);
  if ((mode & ios_base::ate) != 0) {
    fout->_Stream.seekp(0, ios_base::end);
#ifdef DEBUG_ON
    LongLongInt pos = fout->_Stream.tellp();
    LOG(BF("Seek to end of file puts me at: %u") % pos);
#endif
  }
  return fout;
}

FileOutStream_sp FileOutStream_O::createTemporary(Path_sp temporaryPath, Path_sp originalPath, ios_base::openmode mode, Lisp_sp lisp) {
  GC_RESERVE_BEGIN(FileOutStream_O, fout) {
    GC_RESERVE_GET(FileOutStream_O, fout);
  }
  GC_RESERVE_END(fout);
  fout->_ActiveFilePath = temporaryPath;
  fout->_OriginalFilePath = originalPath;
  fout->_RenameToOriginalOnClose = true;
  fout->_Stream.open(temporaryPath->asString().c_str(), mode);
  return fout;
}

T_sp FileOutStream_O::__init__(Function_sp exec, Cons_sp args, Environment_sp environ, Lisp_sp lisp) {
  IMPLEMENT_ME();
#if 0
	this->_ActiveFilePath = translate::from_object<string>::convert(environ->lookup(CorePkg,"fileName"));
	this->_Stream.open(this->_FileName.c_str(),ios_base::app|ios_base::out|ios_base::ate);
    	// your stuff here
#endif
  return _Nil<T_O>();
}

void FileOutStream_O::initialize() {
  this->Base::initialize();
}

uint FileOutStream_O::outputColumn() const {
  return this->_OutputCursor.column();
}

void FileOutStream_O::writeChar(char c) {
  this->_Stream << c;
  if (c == '\n' || c == '\r')
    this->_OutputCursor.advanceLineNumber();
  else
    this->_OutputCursor.advanceColumn();
}

void FileOutStream_O::writeln(const string &s) {
  this->_Stream << s << endl;
  this->_OutputCursor.advanceLineNumber();
}

bool FileOutStream_O::atStartOfLine() const {
  return this->_OutputCursor.atStartOfLine();
}

void FileOutStream_O::close(bool abort) {
  _OF();
  this->_Stream.close();
  if (!abort) {
    // We are supposed to rename the ActiveFilePath to the OriginalFilePath
    if (this->_RenameToOriginalOnClose) {
      safeRename(this->_ActiveFilePath, this->_OriginalFilePath);
    }
  }
}

EXPOSE_CLASS(core, FileInCompressedStream_O);

void FileInCompressedStream_O::initialize() {
  this->Base::initialize();
}

void FileInCompressedStream_O::exposeCando(Lisp_sp lisp) {
  class_<FileInCompressedStream_O>();
}

void FileInCompressedStream_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, FileInCompressedStream, "", "", _lisp);
#endif
}

FileInCompressedStream_sp FileInCompressedStream_O::createGzip(Path_sp path, Lisp_sp lisp) {
  GC_RESERVE_BEGIN(FileInCompressedStream_O, fin) {
    GC_RESERVE_GET(FileInCompressedStream_O, fin);
  }
  GC_RESERVE_END(fin);
  fin->_SourceFileInfo = SourceFileInfo_O::getOrCreate(path->asString());
  fin->_RawStream.open(path->asString().c_str(), ios_base::in | ios_base::binary);
  fin->_In.push(boost::iostreams::gzip_decompressor());
  fin->_In.push(fin->_RawStream);
  return fin;
}

T_sp FileInCompressedStream_O::__init__(Function_sp exec, Cons_sp args, Environment_sp environ, Lisp_sp lisp) {
  IMPLEMENT_ME();
  return _Nil<T_O>();
}

void FileInCompressedStream_O::clearInput() {
  this->_Unput.clear();
  this->_RawStream.ignore(std::numeric_limits<std::streamsize>::max(), EOF);
}

LongLongInt FileInCompressedStream_O::fileSize() {
  streampos pos = this->_RawStream.tellg();
  this->_RawStream.seekg(0, ios_base::end);
  LongLongInt fileSize = this->_RawStream.tellg();
  this->_RawStream.seekg(pos, ios_base::beg);
  return fileSize;
}

#if 0
    int FileInCompressedStream_O::read(unsigned char* buf, int size)
    {
	this->_In.read((char*)buf,size);
	this->_Cursor.invalidate();
	return this->_In.gcount();
    }
#endif

int FileInCompressedStream_O::_get() {
  return this->_Unput.has_unput_char() ? this->_Unput.get_char() : this->_In.get();
}
void FileInCompressedStream_O::putback(char c) {
  this->_Unput.updateCursor(this->_Cursor);
  this->_Unput.putback(c);
}

int FileInCompressedStream_O::peek_char() {
  return this->_Unput.has_unput_char() ? this->_Unput.peek_char() : this->_In.peek();
}

LongLongInt FileInCompressedStream_O::gcount() {
  return this->_In.gcount();
}

#if 0
    void FileInCompressedStream_O::readLine(string& sbuf, bool& hitEof)
    {_OF();
	getline(this->_In,sbuf);
	this->advanceLineNumber();
	hitEof = this->_In.eof();
    }
#endif

bool FileInCompressedStream_O::good() const {
  return this->_In.good();
}

bool FileInCompressedStream_O::eof() const {
  return this->_In.eof();
}

int FileInCompressedStream_O::listen() {
  streampos current = this->_In.tellg();
  this->_In.seekg(0, ios_base::end);
  int bytes_left = this->_In.tellg() - current;
  this->_In.seekg(current, ios_base::beg);
  return bytes_left + (this->_Unput.has_unput_char() ? 1 : 0);
}

void FileInCompressedStream_O::close(bool abort) {
  _OF();
  if (abort) {
    _lisp->print(BF("-----Close for %s with abort=true called - I don't currently support abort=true") % this->_instanceClass()->classNameAsString());
  }
  this->_In.pop();
  this->_In.pop();
}

EXPOSE_CLASS(core, FileOutCompressedStream_O);

void FileOutCompressedStream_O::initialize() {
  this->Base::initialize();
}

void FileOutCompressedStream_O::exposeCando(Lisp_sp lisp) {
  class_<FileOutCompressedStream_O>();
}

void FileOutCompressedStream_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, FileOutCompressedStream, "", "", _lisp);
#endif
}

FileOutCompressedStream_sp FileOutCompressedStream_O::createGzip(Path_sp path, Lisp_sp lisp) {
  GC_RESERVE_BEGIN(FileOutCompressedStream_O, fout) {
    GC_RESERVE_GET(FileOutCompressedStream_O, fout);
  }
  GC_RESERVE_END(fout);
  fout->_ActiveFilePath = path;
  fout->_RenameActiveToOriginalOnClose = false;
  fout->_OriginalFilePath = _Nil<Path_O>();
  fout->_RawStream.open(path->asString().c_str(), ios_base::out | ios_base::binary);
  fout->_Out.push(boost::iostreams::gzip_compressor());
  fout->_Out.push(fout->_RawStream);
  return fout;
}

FileOutCompressedStream_sp FileOutCompressedStream_O::createGzipTemporary(Path_sp activePath, Path_sp originalPath, Lisp_sp lisp) {
  GC_RESERVE_BEGIN(FileOutCompressedStream_O, fout) {
    GC_RESERVE_GET(FileOutCompressedStream_O, fout);
  }
  GC_RESERVE_END(fout);
  fout->_ActiveFilePath = activePath;
  fout->_RenameActiveToOriginalOnClose = true;
  fout->_OriginalFilePath = originalPath;
  fout->_RawStream.open(activePath->asString().c_str(), ios_base::out | ios_base::binary);
  fout->_Out.push(boost::iostreams::gzip_compressor());
  fout->_Out.push(fout->_RawStream);
  return fout;
}

T_sp FileOutCompressedStream_O::__init__(Function_sp exec, Cons_sp args, Environment_sp environ, Lisp_sp lisp) {
  IMPLEMENT_ME();
  return _Nil<T_O>();
}

void FileOutCompressedStream_O::flush() {
  _OF();
  this->_Out.flush();
}

uint FileOutCompressedStream_O::column() const {
  return this->_Cursor.column();
}

void FileOutCompressedStream_O::writeChar(char c) {
  _OF();
  this->_Out << c;
  if (c == '\n' || c == '\r')
    this->_Cursor.advanceLineNumber();
  else
    this->_Cursor.advanceColumn();
}

void FileOutCompressedStream_O::writeln(string const &sbuf) {
  this->_Out << sbuf << endl;
  this->_Cursor.advanceLineNumber();
}

bool FileOutCompressedStream_O::atStartOfLine() const {
  return this->_Cursor.atStartOfLine();
}

void FileOutCompressedStream_O::close(bool abort) {
  _OF();
  this->_Out.pop();
  this->_Out.pop();
  if (!abort) {
    if (this->_RenameActiveToOriginalOnClose) {
      safeRename(this->_ActiveFilePath, this->_OriginalFilePath);
    }
  }
}

EXPOSE_CLASS(core, StringStream_O);

void StringStream_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<StringStream_O>();
}

void StringStream_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, StringStream, "", "", _lisp);
#endif
}

EXPOSE_CLASS(core, StringInputStream_O);

#define ARGS_StringInputStream_O_make "(string &optional (start 0) (end nil))"
#define DECL_StringInputStream_O_make ""
#define DOCS_StringInputStream_O_make ""
StringInputStream_sp StringInputStream_O::make(Str_sp string, Fixnum_sp start, T_sp end) {
  _G();
  int iend = af_length(string);
  if (end.notnilp())
    iend = MIN(end.as<Fixnum_O>()->get(), iend);
  int istart = start->get();
  StringInputStream_sp sin = StringInputStream_O::create(string->substr(istart, iend - istart));
  return sin;
}

void StringInputStream_O::initialize() {
  this->Base::initialize();
}

void StringInputStream_O::exposeCando(Lisp_sp lisp) {
  class_<StringInputStream_O>();
  Defun_maker(ClPkg, StringInputStream);
}

void StringInputStream_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, StringInputStream, "", "", _lisp);
#endif
}

int StringInputStream_O::listen() {
  return (this->_Stream.rdbuf()->in_avail() + (this->_Unput.has_unput_char() ? 1 : 0));
}

void StringInputStream_O::clearInput() {
  this->_Unput.clear();
  this->_Stream.ignore(std::numeric_limits<std::streamsize>::max(), EOF);
}

StringInputStream_sp StringInputStream_O::create(string const &contents) {
  GC_RESERVE_BEGIN(StringInputStream_O, fin) {
    GC_RESERVE_GET(StringInputStream_O, fin);
  }
  GC_RESERVE_END(fin);
  fin->_Stream.str(contents);
  return fin;
}

SourceFileInfo_sp StringInputStream_O::sourceFileInfo() const {
  return SourceFileInfo_O::getOrCreate("-StringInputStream-");
}

string StringInputStream_O::readEntireFile() {
  _OF();
  this->_Cursor.invalidate();
  return this->_Stream.str();
}

LongLongInt StringInputStream_O::tell() {
  _OF();
  return this->_Stream.tellg();
}

void StringInputStream_O::seek(LongLongInt pos) {
  _OF();
  this->_Stream.seekg(pos, ios_base::beg);
}

LongLongInt StringInputStream_O::fileSize() {
  streampos pos = this->_Stream.tellg();
  this->_Stream.seekg(0, ios_base::end);
  LongLongInt fileSize = this->_Stream.tellg();
  this->_Stream.seekg(pos, ios_base::beg);
  return fileSize;
}

void StringInputStream_O::close(bool abort) {
  _OF();
}

#if 0
    int StringInputStream_O::read(unsigned char* buf, int size)
    {_OF();
	this->_Stream.read((char*)buf,size);
	this->_Cursor.invalidate();
	return this->_Stream.gcount();
    }
#endif
LongLongInt StringInputStream_O::gcount() {
  return this->_Stream.gcount();
}

#if 0
    void StringInputStream_O::readLine(string& sbuf, bool& hitEof)
    {_OF();
	getline(this->_Stream,sbuf);
	this->advanceLineNumber();
	hitEof = this->_Stream.eof();
    }
#endif

bool StringInputStream_O::good() const {
  return this->_Stream.good();
}

bool StringInputStream_O::eof() const {
  return this->_Stream.eof();
}

int StringInputStream_O::_get() {
  _OF();
  int c = this->_Unput.has_unput_char() ? this->_Unput.get_char() : this->_Stream.get();
  LOG(BF("Got char[%c/%d]") % c % c);
  if (_sym_STARdebugReaderSTAR->symbolValue().isTrue()) {
    printf("%s:%d  StringInputStream_O::_get returning %d[%c]\n", __FILE__, __LINE__, c, c);
  }
  return c;
}

void StringInputStream_O::putback(char c) {
  _G();
  LOG(BF("StringInputStream_O::putback[%c]") % c);
  if (_sym_STARdebugReaderSTAR->symbolValue().isTrue()) {
    printf("%s:%d  StringInputStream_O::putback returning %d[%c]\n", __FILE__, __LINE__, c, c);
  }
  this->_Unput.updateCursor(this->_Cursor);
  this->_Unput.putback(c);
  char pc = this->peek_char();
  if (pc != c) {
    SIMPLE_ERROR(BF("putback char[%c] does not match peek_char[%c]") % c % pc);
  }
}

int StringInputStream_O::peek_char() {
  _OF();
  int c = this->_Unput.has_unput_char() ? this->_Unput.peek_char() : this->_Stream.peek();
  LOG(BF("Peek char[%c/%d]") % c % c);
  if (_sym_STARdebugReaderSTAR->symbolValue().isTrue()) {
    printf("%s:%d  StringInputStream_O::peek_char returning %d[%c]\n", __FILE__, __LINE__, c, c);
  }
  return c;
}

StringOutStream_sp StringOutStream_O::make() {
  GC_RESERVE_BEGIN(StringOutStream_O, ss) {
    GC_RESERVE_GET(StringOutStream_O, ss);
  }
  GC_RESERVE_END(ss);
  return ss;
}

StringOutStream_sp StringOutStream_O::create(StrWithFillPtr_sp str) {
  GC_RESERVE_BEGIN(StringOutStream_O, ss) {
    GC_RESERVE_GET(StringOutStream_O, ss);
  }
  GC_RESERVE_END(ss);
  ss->_String = str;
  return ss;
}

EXPOSE_CLASS(core, StringOutStream_O);

void StringOutStream_O::exposeCando(Lisp_sp lisp) {
  class_<StringOutStream_O>();
}

void StringOutStream_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, StringOutStream, "", "", _lisp);
#endif //]
}

T_sp StringOutStream_O::__init__(Function_sp exec, Cons_sp args, Environment_sp environ, Lisp_sp lisp) {
  // your stuff here
  return _Nil<T_O>();
}

void StringOutStream_O::initialize() {
  this->Base::initialize();
  this->_String = StrWithFillPtr_O::create(' ', 256, 0, true);
}

void StringOutStream_O::flush() {
  // this->_Stream.flush();
}

void StringOutStream_O::clear() {
  this->_String->setFillPointer(0);
}

uint StringOutStream_O::outputColumn() const {
  return this->_OutputCursor.column();
}

LongLongInt StringOutStream_O::tell() { return this->_String->length(); };

void StringOutStream_O::writeChar(char c) {
  this->_String->pushCharExtend(c);
  this->_OutputCursor.advanceForChar(c);
}

void StringOutStream_O::writeln(const string &s) {
  this->writeStr(s);
  this->writeStr("\n");
  this->_OutputCursor.advanceLineNumber();
}

bool StringOutStream_O::atStartOfLine() const {
  return this->_OutputCursor.atStartOfLine();
}

string StringOutStream_O::str() {
  return this->_String->get();
}

void StringOutStream_O::close(bool abort) {
  // do nothing
}

// ------------------------------------------------------------------

void FDStream_O::close(bool abort) {
  _OF();
  if (!this->_Closeable) {
    SIMPLE_ERROR(BF("You tried to close a file that is not closeable"));
  }
  if (this->_FileDescriptor != NULL) {
    SIMPLE_ERROR(BF("Could not close file - it was already closed"));
  }
  if (abort) {
    _lisp->print(BF("-----Close for %s with abort=true called - I don't currently support abort=true") % this->_instanceClass()->classNameAsString());
  }
  this->throw_if_no_file_descriptor();
  fclose(this->_FileDescriptor);
  this->_FileDescriptor = NULL;
}

EXPOSE_CLASS(core, FDStream_O);

void FDStream_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<FDStream_O>();
}

void FDStream_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, FDStream, "", "", _lisp);
#endif
}

FDStream_O::FDStream_O() : Base(), _FileDescriptor(NULL), _Closeable(true){};

FDStream_O::~FDStream_O() {
  if (this->_FileDescriptor != NULL) {
    close(this->_FileDescriptor);
    this->_FileDescriptor = NULL;
  }
}

void FDStream_O::throw_if_no_file_descriptor() const {
  _G();
  if (this->_FileDescriptor == NULL) {
    SIMPLE_ERROR(BF("The file is not open!!"));
  }
}

EXPOSE_CLASS(core, FDInStream_O);

#define ARGS_FDInStream_O_make "(file_desig)"
#define DECL_FDInStream_O_make ""
#define DOCS_FDInStream_O_make ""
FDInStream_sp FDInStream_O::make(T_sp file_descriptor) {
  _G();
  Path_sp path = coerce::pathDesignator(file_descriptor);
  GC_RESERVE_BEGIN(FDInStream_O, fin) {
    GC_RESERVE_GET(FDInStream_O, fin);
  }
  GC_RESERVE_END(fin);
  fin->_SourceFileInfo = SourceFileInfo_O::getOrCreate(path->asString());
  fin->_FileDescriptor = fopen(path->asString().c_str(), "r");
  return fin;
}

FDInStream_sp FDInStream_O::create(FILE *fid, const string &name, bool closeable) {
  _G();
  GC_RESERVE_BEGIN(FDInStream_O, fin) {
    GC_RESERVE_GET(FDInStream_O, fin);
  }
  GC_RESERVE_END(fin);
  fin->_FileDescriptor = fid;
  fin->_SourceFileInfo = SourceFileInfo_O::getOrCreate(name);
  fin->_Closeable = closeable;
  return fin;
}

void FDInStream_O::exposeCando(Lisp_sp lisp) {
  class_<FDInStream_O>();
  SYMBOL_SC_(CorePkg, make_fd_in_stream);
  af_def(CurrentPkg, "make-fd-in-stream", &FDInStream_O::make, ARGS_FDInStream_O_make, DECL_FDInStream_O_make, DOCS_FDInStream_O_make);
}

void FDInStream_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, FDInStream, "", "", _lisp);
#endif
}

void FDInStream_O::clearInput() {
  int lst;
  while ((lst = this->listen())) {
    if (lst <= 0)
      break;
    int c = this->_get();
    if (c == EOF)
      return;
  }
}

string FDInStream_O::readEntireFile() {
  _G();
  this->throw_if_no_file_descriptor();
  int res = fseek(this->_FileDescriptor, SEEK_END, 0);
  if (res < 0) {
    SIMPLE_ERROR(BF("Could not fseek to end of file: %s") % this->_SourceFileInfo->fileName());
  }
  res = fseek(this->_FileDescriptor, SEEK_SET, 0);
  if (res < 0) {
    SIMPLE_ERROR(BF("Problem with fseek"));
  }
  LongLongInt fileSize = ftell(this->_FileDescriptor);
  char *buffer = (char *)malloc(fileSize + 1);
  for (LongLongInt i = 0; i < fileSize; i++) {
    buffer[i] = this->get(); // read file and keep track of line numbers
  }
  buffer[fileSize] = '\0';
  string fileContents = buffer;
  return fileContents;
}

LongLongInt FDInStream_O::tell() {
  this->throw_if_no_file_descriptor();
  return ftell(this->_FileDescriptor);
}

void FDInStream_O::seek(LongLongInt pos) {
  _G();
  this->throw_if_no_file_descriptor();
  int res = fseek(this->_FileDescriptor, SEEK_SET, pos);
  if (res < 0)
    SIMPLE_ERROR(BF("Problem with fseek"));
  this->_Cursor.invalidate();
}

LongLongInt FDInStream_O::fileSize() {
  _G();
  this->throw_if_no_file_descriptor();
  LongLongInt pos = ftell(this->_FileDescriptor);
  int res = fseek(this->_FileDescriptor, SEEK_END, 0);
  if (res < 0)
    SIMPLE_ERROR(BF("Problem with fseek"));
  LongLongInt fileSize = ftell(this->_FileDescriptor);
  res = fseek(this->_FileDescriptor, SEEK_SET, pos);
  if (res < 0)
    SIMPLE_ERROR(BF("Problem with fseek"));
  return fileSize;
}

#if 0
    int FDInStream_O::read(unsigned char* buf, int size)
    {
	this->throw_if_no_file_descriptor();
	this->_gcount = fread(buf,1,size,this->_FileDescriptor);
	this->_Cursor.invalidate();
	return this->_gcount;
    }
#endif

LongLongInt FDInStream_O::gcount() {
  return this->_gcount;
}

int FDInStream_O::_get() {
  this->throw_if_no_file_descriptor();
  int ch = this->_Unput.has_unput_char() ? this->_Unput.get_char() : fgetc(this->_FileDescriptor);
  return ch;
}

void FDInStream_O::putback(char c) {
  this->throw_if_no_file_descriptor();
  this->_Unput.updateCursor(this->_Cursor);
  this->_Unput.putback(c);
}

int FDInStream_O::peek_char() {
  this->throw_if_no_file_descriptor();
  char c;
  if (this->_Unput.has_unput_char()) {
    c = this->_Unput.peek_char();
  } else {
    c = fgetc(this->_FileDescriptor);
    ungetc(c, this->_FileDescriptor);
  }
  return c;
}

#if 0
    void FDInStream_O::readLine(string& sbuf, bool& hitEof)
    {_OF();
	this->throw_if_no_file_descriptor();
	char* buf = NULL;
	size_t n=0;
	getline(&buf,&n,this->_FileDescriptor);
	if ( buf )
	{
	    this->_gcount = n;
	    sbuf = buf;
	    free(buf);
	} else
	{
	    sbuf = "";
	    this->_gcount = 0;
	}
	hitEof = feof(this->_FileDescriptor);
    }
#endif

bool FDInStream_O::good() const {
  if (this->_FileDescriptor == NULL)
    return true;
  return !(ferror(this->_FileDescriptor));
}

bool FDInStream_O::eof() const {
  if (this->_FileDescriptor == NULL)
    return true;
  return feof(this->_FileDescriptor);
}

int FDInStream_O::listen() {
  int current = ftell(this->_FileDescriptor);
  fseek(this->_FileDescriptor, 0, SEEK_SET);
  int bytes_left = ftell(this->_FileDescriptor) - current;
  fseek(this->_FileDescriptor, current, SEEK_SET); /* back to where we started */
  return (bytes_left + (this->_Unput.has_unput_char() ? 1 : 0));
}

string convert_ios_base_openmode_to_fopen_flags(ios_base::openmode mode) {
  _G();
  stringstream ss;
  if ((mode & ios_base::in) && (mode & ios_base::out)) {
    return "r+";
  } else if ((mode & ios_base::in)) {
    return "r";
  } else if ((mode & ios_base::out) && (mode & ios_base::app)) {
    return "a";
  } else if (mode & ios_base::out) {
    return "w";
  }
  SIMPLE_ERROR(BF("Could not convert ios_base openmode flags: %d") % mode);
}

FDOutStream_sp FDOutStream_O::create(FILE *fod, const string &name, bool closeable) {
  _G();
  GC_RESERVE_BEGIN(FDOutStream_O, fout) {
    GC_RESERVE_GET(FDOutStream_O, fout);
  }
  GC_RESERVE_END(fout);
  fout->_FileDescriptor = fod;
  fout->_Closeable = closeable;
  return fout;
}

FDOutStream_sp FDOutStream_O::create(Path_sp currentPath, ios_base::openmode mode, Lisp_sp lisp) {
  _G();
  GC_RESERVE_BEGIN(FDOutStream_O, fout) {
    GC_RESERVE_GET(FDOutStream_O, fout);
  }
  GC_RESERVE_END(fout);
  fout->_ActiveFilePath = currentPath;
  fout->_RenameToOriginalOnClose = false;
  fout->_OriginalFilePath = _Nil<Path_O>();
  fout->do_open(currentPath, mode);
  return fout;
}

void FDOutStream_O::do_open(Path_sp currentPath, ios_base::openmode mode) {
  _OF();
  this->_FileDescriptor = fopen(currentPath->asString().c_str(),
                                convert_ios_base_openmode_to_fopen_flags(mode).c_str());
  if (this->_FileDescriptor == NULL) {
    SIMPLE_ERROR(BF("Could not open file: %s") % currentPath->asString());
  }
  if ((mode & ios_base::ate) != 0) {
    IMPLEMENT_ME();
    int res = fseek(this->_FileDescriptor, SEEK_END, 0);
    if (res < 0)
      SIMPLE_ERROR(BF("Problem with fseek"));
#ifdef DEBUG_ON
    LongLongInt pos = ftell(this->_FileDescriptor);
    LOG(BF("Seek to end of file puts me at: %u") % pos);
#endif
  }
}

FDOutStream_sp FDOutStream_O::createTemporary(Path_sp temporaryPath, Path_sp originalPath, ios_base::openmode mode, Lisp_sp lisp) {
  GC_RESERVE_BEGIN(FDOutStream_O, fout) {
    GC_RESERVE_GET(FDOutStream_O, fout);
  }
  GC_RESERVE_END(fout);
  fout->_ActiveFilePath = temporaryPath;
  fout->_OriginalFilePath = originalPath;
  fout->_RenameToOriginalOnClose = true;
  fout->do_open(temporaryPath, mode);
  return fout;
}

#define ARGS_FDOutStream_O_make "(file_desig)"
#define DECL_FDOutStream_O_make ""
#define DOCS_FDOutStream_O_make ""
FDOutStream_sp FDOutStream_O::make(T_sp file_desig) {
  _G();
  Path_sp activeFilePath = coerce::pathDesignator(file_desig);
  FDOutStream_sp fout = FDOutStream_O::create(activeFilePath, ios_base::out, _lisp);
  return fout;
}

void FDOutStream_O::flush() {
  fflush(this->_FileDescriptor);
}

uint FDOutStream_O::outputColumn() const {
  return this->_OutputCursor.column();
}

void FDOutStream_O::writeChar(char c) {
  this->throw_if_no_file_descriptor();
  fputc(c, this->_FileDescriptor);
  this->_OutputCursor.advanceForChar(c);
}

void FDOutStream_O::writeln(const string &s) {
  this->throw_if_no_file_descriptor();
  fwrite(s.c_str(), 1, s.size(), this->_FileDescriptor);
  const char *eoln = "\n";
  fwrite(eoln, 1, strlen(eoln), this->_FileDescriptor);
  this->_OutputCursor.advanceLineNumber();
}

bool FDOutStream_O::atStartOfLine() const {
  return this->_OutputCursor.atStartOfLine();
}

void FDOutStream_O::close(bool abort) {
  _OF();
  this->throw_if_no_file_descriptor();
  if (!this->_Closeable) {
    SIMPLE_ERROR(BF("You tried to close a file that is not closeable"));
  }
  if (this->_FileDescriptor != NULL) {
    fclose(this->_FileDescriptor);
    this->_FileDescriptor = NULL;
  }
  if (!abort) {
    // We are supposed to rename the ActiveFilePath to the OriginalFilePath
    if (this->_RenameToOriginalOnClose) {
      safeRename(this->_ActiveFilePath, this->_OriginalFilePath);
    }
  }
}

EXPOSE_CLASS(core, FDOutStream_O);

void FDOutStream_O::exposeCando(Lisp_sp lisp) {
  class_<FDOutStream_O>();
  af_def(CurrentPkg, "make-fd-out-stream", &FDOutStream_O::make, ARGS_FDOutStream_O_make, DECL_FDOutStream_O_make, DOCS_FDOutStream_O_make);
}

void FDOutStream_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, FDOutStream, "", "", _lisp);
#endif //]
}

EXPOSE_CLASS(core, FDIOStream_O);

void FDIOStream_O::exposeCando(Lisp_sp lisp) {
  class_<FDIOStream_O>();
  //	af_def(CurrentPkg,"make-fd-io-stream",&FDIOStream_O::make,ARGS_FDIOStream_O_make,DECL_FDIOStream_O_make,DOCS_FDIOStream_O_make);
}

void FDIOStream_O::exposePython(Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON //[
  PYTHON_CLASS(CorePkg, FDIOStream, "", "", _lisp);
#endif //]
}

// ------------------------------------------------------------
// ------------------------------------------------------------
// ------------------------------------------------------------
// ------------------------------------------------------------
//
// synonym-stream
//
//

EXPOSE_CLASS(core, SynonymStream_O);

#define ARGS_SynonymStream_O_make "(symbol)"
#define DECL_SynonymStream_O_make ""
#define DOCS_SynonymStream_O_make ""
SynonymStream_sp SynonymStream_O::make(Symbol_sp symbol) {
  _G();
  Stream_sp stream = symbol->symbolValue().as<Stream_O>();
  GC_RESERVE_BEGIN(SynonymStream_O, str) {
    GC_RESERVE_GET(SynonymStream_O, str);
  }
  GC_RESERVE_END(str);
  str->_SynonymSymbol = symbol;
  return str;
}

void SynonymStream_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<SynonymStream_O>(no_init);
  af_def(CurrentPkg, "make-synonym-stream", &SynonymStream_O::make, ARGS_SynonymStream_O_make, DECL_SynonymStream_O_make, DOCS_SynonymStream_O_make);
}

void SynonymStream_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, SynonymStream, "", "", _lisp);
#endif
}

SynonymStream_O::SynonymStream_O() : Base(), _SynonymSymbol(_Nil<Symbol_O>()){};

SynonymStream_O::~SynonymStream_O() {
}

string SynonymStream_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString() << " :symbol " << _rep_(this->_SynonymSymbol) << ">";
  return ss.str();
}

void SynonymStream_O::flush() {
  this->stream()->flush();
}

Stream_sp SynonymStream_O::stream() {
  Stream_sp stream = this->_SynonymSymbol->symbolValue().as<Stream_O>();
  return stream;
}

Stream_sp SynonymStream_O::stream() const {
  Stream_sp stream = this->_SynonymSymbol->symbolValue().as<Stream_O>();
  return stream;
}

void SynonymStream_O::clearInput() {
  this->stream()->clearInput();
}

int SynonymStream_O::listen() {
  return this->stream()->listen();
}

void SynonymStream_O::close(bool abort) {
  _OF();
  this->stream()->close(abort);
}

bool SynonymStream_O::inputStreamP() const {
  _G();
  return this->stream()->inputStreamP();
}

bool SynonymStream_O::outputStreamP() const {
  _G();
  return this->stream()->outputStreamP();
}

string SynonymStream_O::readEntireFile() {
  _G();
  return this->stream()->readEntireFile();
}

LongLongInt SynonymStream_O::tell() {
  return this->stream()->tell();
}

void SynonymStream_O::seek(LongLongInt pos) {
  _G();
  this->stream()->seek(pos);
}

LongLongInt SynonymStream_O::fileSize() {
  _G();
  return this->stream()->fileSize();
}

#if 0
    int SynonymStream_O::read(unsigned char* buf, int size)
    {
	return this->stream()->read(buf,size);
    }
#endif

LongLongInt SynonymStream_O::gcount() {
  return this->stream()->gcount();
}

SourceFileInfo_sp SynonymStream_O::sourceFileInfo() const {
  return af_sourceFileInfo(this->stream());
}

int SynonymStream_O::_get() {
  return this->stream()->_get();
}

void SynonymStream_O::putback(char c) {
  this->stream()->putback(c);
}

int SynonymStream_O::peek_char() {
  return this->stream()->peek_char();
}

#if 0
    void SynonymStream_O::readLine(string& sbuf, bool& hitEof)
    {
	return this->stream()->readLine(sbuf,hitEof);
    }
#endif

bool SynonymStream_O::good() const {
  return this->stream()->good();
}

bool SynonymStream_O::eof() const {
  return this->stream()->eof();
}

uint SynonymStream_O::lineNumber() const {
  return this->stream()->lineNumber();
}

uint SynonymStream_O::column() const {
  return this->stream()->column();
}

void SynonymStream_O::invalidateCursor() {
  this->stream()->invalidateCursor();
}

void SynonymStream_O::advanceLineNumber(int num) {
  this->stream()->advanceLineNumber(num);
}

void SynonymStream_O::advanceColumn(int num) {
  this->stream()->advanceColumn(num);
}

void SynonymStream_O::writeChar(char c) {
  this->stream()->writeChar(c);
}

void SynonymStream_O::writeln(const string &s) {
  this->stream()->writeln(s);
}

bool SynonymStream_O::atStartOfLine() const {
  return this->stream()->atStartOfLine();
}

// ------------------------------------------------------------

TwoWayStream_O::TwoWayStream_O() : Base(), _in_stream(_Nil<Stream_O>()), _out_stream(Stream_O::_nil){};

TwoWayStream_O::~TwoWayStream_O() {
  if (this->_in_stream.notnilp())
    this->_in_stream->close();
  if (this->_out_stream.notnilp())
    this->_out_stream->close();
}

void TwoWayStream_O::clearInput() {
  this->in_stream()->clearInput();
}

int TwoWayStream_O::listen() {
  return this->in_stream()->listen();
}

string TwoWayStream_O::__repr__() const {
  stringstream ss;
  ss << "#< " << this->_instanceClass()->classNameAsString() << " :in-stream " << _rep_(this->_in_stream) << " :out-stream " << _rep_(this->_out_stream) << ">";
  return ss.str();
}

Stream_sp TwoWayStream_O::in_stream() {
  _G();
  if (this->_in_stream.notnilp())
    return this->_in_stream;
  SIMPLE_ERROR(BF("two-way-stream in-stream is nil!"));
}

Stream_sp TwoWayStream_O::in_stream() const {
  _G();
  if (this->_in_stream.notnilp())
    return this->_in_stream;
  SIMPLE_ERROR(BF("two-way-stream in-stream is nil!"));
}

Stream_sp TwoWayStream_O::out_stream() {
  _G();
  if (this->_out_stream.notnilp())
    return this->_out_stream;
  SIMPLE_ERROR(BF("two-way-stream out-stream is nil!"));
}

Stream_sp TwoWayStream_O::out_stream() const {
  _G();
  if (this->_out_stream.notnilp())
    return this->_out_stream;
  SIMPLE_ERROR(BF("two-way-stream out-stream is nil!"));
}

void TwoWayStream_O::flush() {
  if (this->_out_stream.notnilp())
    this->_out_stream->flush();
}

void TwoWayStream_O::close(bool abort) {
  _G();
  if (this->_in_stream.notnilp())
    this->_in_stream->close(abort);
  if (this->_out_stream.notnilp())
    this->_out_stream->close(abort);
};

#define ARGS_TwoWayStream_O_make "(in-stream out-stream)"
#define DECL_TwoWayStream_O_make ""
#define DOCS_TwoWayStream_O_make ""
TwoWayStream_sp TwoWayStream_O::make(Stream_sp in_stream, Stream_sp out_stream) {
  _G();
  GC_RESERVE_BEGIN(TwoWayStream_O, f) {
    GC_RESERVE_GET(TwoWayStream_O, f);
  }
  GC_RESERVE_END(f);
  f->_in_stream = in_stream;
  f->_out_stream = out_stream;
  return f;
}

string TwoWayStream_O::readEntireFile() {
  _G();
  return this->in_stream()->readEntireFile();
}

LongLongInt TwoWayStream_O::tell() {
  return this->in_stream()->tell();
}

void TwoWayStream_O::seek(LongLongInt pos) {
  _G();
  this->in_stream()->seek(pos);
}

LongLongInt TwoWayStream_O::fileSize() {
  _G();
  return this->in_stream()->fileSize();
}

#if 0
    int TwoWayStream_O::read(unsigned char* buf, int size)
    {
	return this->in_stream()->read(buf,size);
    }
#endif

LongLongInt TwoWayStream_O::gcount() {
  return this->in_stream()->gcount();
}

int TwoWayStream_O::_get() {
  return this->in_stream()->_get();
}

void TwoWayStream_O::putback(char c) {
  this->in_stream()->putback(c);
}

int TwoWayStream_O::peek_char() {
  return this->in_stream()->peek_char();
}

#if 0
    void TwoWayStream_O::readLine(string& sbuf, bool& hitEof)
    {
	return this->in_stream()->readLine(sbuf,hitEof);
    }
#endif

bool TwoWayStream_O::good() const {
  return this->in_stream()->good();
}

bool TwoWayStream_O::eof() const {
  return this->in_stream()->eof();
}

void TwoWayStream_O::writeChar(char c) {
  this->out_stream()->writeChar(c);
}

void TwoWayStream_O::writeln(const string &s) {
  this->out_stream()->writeln(s);
}

bool TwoWayStream_O::atStartOfLine() const {
  return this->out_stream()->atStartOfLine();
}

EXPOSE_CLASS(core, TwoWayStream_O);

void TwoWayStream_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<TwoWayStream_O>(no_init)
      .def("two-way-stream-input-stream", &TwoWayStream_O::input_stream)
      .def("two-way-stream-output-stream", &TwoWayStream_O::output_stream);
  af_def(CurrentPkg, "make-two-way-stream", &TwoWayStream_O::make, ARGS_TwoWayStream_O_make, DECL_TwoWayStream_O_make, DOCS_TwoWayStream_O_make);
}

void TwoWayStream_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, TwoWayStream, "", "", _lisp);
#endif
}

// ------------------------------------------------------------

EXPOSE_CLASS(core, BroadcastStream_O);

void BroadcastStream_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<BroadcastStream_O>(no_init);
}

void BroadcastStream_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, BroadcastStream, "", "", _lisp);
#endif
}

// ------------------------------------------------------------

EXPOSE_CLASS(core, ConcatenatedStream_O);

void ConcatenatedStream_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<ConcatenatedStream_O>(no_init);
}

void ConcatenatedStream_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, ConcatenatedStream, "", "", _lisp);
#endif
}

int ConcatenatedStream_O::listen() {
  IMPLEMENT_ME();
}

// ------------------------------------------------------------

EXPOSE_CLASS(core, EchoStream_O);

void EchoStream_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<EchoStream_O>(no_init);
}

void EchoStream_O::exposePython(core::Lisp_sp lisp) {
  _G();
#ifdef USEBOOSTPYTHON
  PYTHON_CLASS(CorePkg, EchoStream, "", "", _lisp);
#endif
}

int EchoStream_O::listen() {
  IMPLEMENT_ME();
}

#define ARGS_af_makeStringOutputStreamFromString "(str)"
#define DECL_af_makeStringOutputStreamFromString ""
#define DOCS_af_makeStringOutputStreamFromString "makeStringOutputStreamFromString"
T_sp af_makeStringOutputStreamFromString(StrWithFillPtr_sp str) {
  _G();
  StringOutStream_sp ss = StringOutStream_O::create(str);
  return ss;
};

/*! Translated from ecl::si_do_write_sequence */
#define ARGS_af_writeSequence "(seq stream &key (start 0) end)"
#define DECL_af_writeSequence ""
#define DOCS_af_writeSequence "writeSequence"
Sequence_sp af_writeSequence(Sequence_sp seq, Stream_sp stream, Fixnum_sp fstart, Fixnum_sp tend) {
  _G();
  int limit = af_length(seq);
  unlikely_if(!af_fixnumP(fstart) ||
              (fstart->get() < 0) ||
              (fstart->get() > limit)) {
    WRONG_TYPE_KEY_ARG(kw::_sym_start, fstart,
                       Integer_O::makeIntegerType(0, limit - 1));
  }
  int start = fstart->get();
  int end = limit;
  if (tend.notnilp()) {
    unlikely_if(!af_fixnumP(tend) ||
                (end < 0) ||
                (end > limit)) {
      WRONG_TYPE_KEY_ARG(kw::_sym_end, tend,
                         Integer_O::makeIntegerType(0, limit - 1));
    }
    end = tend->get();
  }
  if (end <= start) {
    goto OUTPUT;
  }
  if (af_listp(seq)) {
    T_sp elt_type = stream->streamElementType();
    bool ischar = (elt_type == cl::_sym_BaseChar_O) || (elt_type == cl::_sym_Character_O);
    T_sp s = af_nthcdr(start, seq);
    for (;; s = CONS_CDR(s)) {
      if (start < end) {
        T_sp elt = CONS_CAR(s);
        if (ischar)
          stream->writeChar(elt.as<Character_O>()->charCode());
        else
          stream->writeByte(elt);
        start++;
      } else {
        goto OUTPUT;
      }
    }
  } else {
    stream->writeVector(seq.as<Vector_O>(), fstart, tend);
  }
OUTPUT:
  return seq;
}

void initialize_lispStream() {
  SYMBOL_EXPORT_SC_(ClPkg, read_from_string);
  Defun(read_from_string);
  SYMBOL_EXPORT_SC_(ClPkg, read_line);
  Defun(read_line);
  SYMBOL_EXPORT_SC_(ClPkg, terpri);
  Defun(terpri);
  SYMBOL_EXPORT_SC_(ClPkg, freshLine);
  Defun(freshLine);
  SYMBOL_EXPORT_SC_(ClPkg, writeString);
  Defun(writeString);
  SYMBOL_EXPORT_SC_(ClPkg, writeLine);
  Defun(writeLine);
  SYMBOL_EXPORT_SC_(ClPkg, writeChar);
  Defun(writeChar);
  SYMBOL_EXPORT_SC_(ClPkg, clearInput);
  Defun(clearInput);
  SYMBOL_EXPORT_SC_(ClPkg, peekChar);
  Defun(peekChar);
  SYMBOL_EXPORT_SC_(ClPkg, readChar);
  Defun(readChar);
  SYMBOL_EXPORT_SC_(ClPkg, force_output);
  Defun(force_output);
  SYMBOL_EXPORT_SC_(ClPkg, finish_output);
  Defun(finish_output);
  SYMBOL_EXPORT_SC_(ClPkg, listen);
  Defun(listen);
  SYMBOL_EXPORT_SC_(ClPkg, unread_char);
  Defun(unread_char);
  SYMBOL_EXPORT_SC_(CorePkg, fileColumn);
  Defun(fileColumn);
  SYMBOL_EXPORT_SC_(CorePkg, makeStringOutputStreamFromString);
  Defun(makeStringOutputStreamFromString);
}
};
