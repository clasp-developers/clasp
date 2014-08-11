#define	DEBUG_LEVEL_FULL
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
#include "pathname.h"
#include "primitives.h"
#include "multipleValues.h"
#include "evaluator.h"
#include "strWithFillPtr.h"
#include "designators.h"
#include "unixfsys.h"
#include "reader.h"
#include "lispReader.h"
#include "fileSystem.h"
#include "wrappers.h"

namespace core
{
    FileOps& StreamOps(T_sp strm)
    {
        Stream_sp stream = strm.as<Stream_O>();
        return stream->ops;
    }

    int StreamByteSize(T_sp strm)
    {
        return 8;
    };
};


namespace core
{

/* Maximum number of bytes required to encode a character.
 * This currently corresponds to (4 + 2) for the ISO-2022-JP-* encodings
 * with 4 being the charset prefix, 2 for the character.
 */
#define ENCODING_BUFFER_MAX_SIZE 6

    static cl_index ecl_read_byte8(T_sp stream, unsigned char *c, cl_index n);
    static cl_index ecl_write_byte8(T_sp stream, unsigned char *c, cl_index n);

    struct ecl_file_ops *duplicate_dispatch_table(const struct ecl_file_ops *ops);
    const struct ecl_file_ops *stream_dispatch_table(T_sp strm);

    static int flisten(T_sp, FILE *);
    static int file_listen(T_sp, int);

    static T_sp alloc_stream();

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
#ifdef ECL_UNICODE
    static cl_index encoding_error(T_sp strm, unsigned char *buffer, claspCharacter c);
    static claspCharacter decoding_error(T_sp strm, unsigned char *buffer, int length);
#endif
    static void wrong_file_handler(T_sp strm) NO_RETURN;
#if defined(ECL_WSOCK)
    static void wsock_error( const char *err_msg, T_sp strm ) NO_RETURN;
#endif


/**********************************************************************
 * NOT IMPLEMENTED or NOT APPLICABLE OPERATIONS
 */

    static cl_index
    not_output_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	not_an_output_stream(strm);
	return 0;
    }

    static cl_index
    not_input_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	not_an_input_stream(strm);
	return 0;
    }

    static cl_index
    not_binary_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	not_a_binary_stream(strm);
	return 0;
    }

    static void
    not_output_write_byte(T_sp c, T_sp strm)
    {
	not_an_output_stream(strm);
    }

    static T_sp
    not_input_read_byte(T_sp strm)
    {
	not_an_input_stream(strm);
	UNREACHABLE(); // return OBJNULL;
    }

    static void
    not_binary_write_byte(T_sp c, T_sp strm)
    {
	not_a_binary_stream(strm);
    }

    static T_sp
    not_binary_read_byte(T_sp strm)
    {
	not_a_binary_stream(strm);
	UNREACHABLE(); // return OBJNULL;
    }

    static claspCharacter
    not_input_read_char(T_sp strm)
    {
	not_an_input_stream(strm);
	return -1;
    }

    static claspCharacter
    not_output_write_char(T_sp strm, claspCharacter c)
    {
	not_an_output_stream(strm);
	return c;
    }

    static void
    not_input_unread_char(T_sp strm, claspCharacter c)
    {
	not_an_input_stream(strm);
    }

    static int
    not_input_listen(T_sp strm)
    {
	not_an_input_stream(strm);
	return -1;
    }

    static claspCharacter
    not_character_read_char(T_sp strm)
    {
	not_a_character_stream(strm);
	return -1;
    }

    static claspCharacter
    not_character_write_char(T_sp strm, claspCharacter c)
    {
	not_a_character_stream(strm);
	return c;
    }

    static void
    not_input_clear_input(T_sp strm)
    {
	not_an_input_stream(strm);
	return;
    }

    static void
    not_output_clear_output(T_sp strm)
    {
	not_an_output_stream(strm);
    }

    static void
    not_output_force_output(T_sp strm)
    {
	not_an_output_stream(strm);
    }

    static void
    not_output_finish_output(T_sp strm)
    {
	not_an_output_stream(strm);
    }

#if defined(ECL_WSOCK)
    static T_sp
    not_implemented_get_position(T_sp strm)
    {
	FEerror("file-position not implemented for stream ~S", 1, strm.asTPtr());
	return ECL_NIL;
    }

    static T_sp
    not_implemented_set_position(T_sp strm, T_sp pos)
    {
	FEerror("file-position not implemented for stream ~S", 1, strm.asTPtr());
	return ECL_NIL;
    }
#endif

/**********************************************************************
 * CLOSED STREAM OPS
 */

    static cl_index
    closed_stream_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	CLOSED_STREAM_ERROR(strm);
	return 0;
    }

    static cl_index
    closed_stream_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	CLOSED_STREAM_ERROR(strm);
	return 0;
    }

    static claspCharacter
    closed_stream_read_char(T_sp strm)
    {
	CLOSED_STREAM_ERROR(strm);
	return 0;
    }

    static claspCharacter
    closed_stream_write_char(T_sp strm, claspCharacter c)
    {
	CLOSED_STREAM_ERROR(strm);
	return c;
    }

    static void
    closed_stream_unread_char(T_sp strm, claspCharacter c)
    {
	CLOSED_STREAM_ERROR(strm);
    }

    static int
    closed_stream_listen(T_sp strm)
    {
	CLOSED_STREAM_ERROR(strm);
	return 0;
    }

    static void
    closed_stream_clear_input(T_sp strm)
    {
	CLOSED_STREAM_ERROR(strm);
    }

#define closed_stream_clear_output closed_stream_clear_input
#define closed_stream_force_output closed_stream_clear_input
#define closed_stream_finish_output closed_stream_clear_input

    static T_sp
    closed_stream_length(T_sp strm)
    {
	CLOSED_STREAM_ERROR(strm);
    }

#define closed_stream_get_position closed_stream_length

    static T_sp
    closed_stream_set_position(T_sp strm, T_sp position)
    {
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
    generic_read_byte_unsigned8(T_sp strm)
    {
	unsigned char c;
	if (StreamOps(strm).read_byte8(strm, &c, 1) < 1) {
            return _Nil<T_O>();
	}
	return Fixnum_O::create(c);
    }

    static void
    generic_write_byte_unsigned8(T_sp byte, T_sp strm)
    {
	unsigned char c = ecl_to_uint8_t(byte);
	StreamOps(strm).write_byte8(strm, &c, 1);
    }

    static T_sp
    generic_read_byte_signed8(T_sp strm)
    {
	signed char c;
	if (StreamOps(strm).read_byte8(strm, (unsigned char *)&c, 1) < 1)
            return _Nil<T_O>();
	return Fixnum_O::create(c);
    }

    static void
    generic_write_byte_signed8(T_sp byte, T_sp strm)
    {
	signed char c = fixint(byte);
	StreamOps(strm).write_byte8(strm, (unsigned char *)&c, 1);
    }

    static T_sp
    generic_read_byte_le(T_sp strm)
    {
	cl_index (*read_byte8)(T_sp, unsigned char *, cl_index);
	unsigned char c;
	cl_index nb, bs;
	T_sp output = Fixnum_O::create(0);
	read_byte8 = StreamOps(strm).read_byte8;
	bs = StreamByteSize(strm);
	for (nb = 0; bs >= 8; bs -= 8, nb += 8) {
            T_sp aux;
            if (read_byte8(strm, &c, 1) < 1)
                return _Nil<T_O>();
            if (bs <= 8 && (strm->stream.flags & ECL_STREAM_SIGNED_BYTES))
                aux = Fixnum_O::create((signed char)c);
            else
                aux = Fixnum_O::create((unsigned char)c);
            output = cl_logior(Cons_O::createList( output, cl_ash(aux, Fixnum_O::create(nb))));
	}
	return output;
    }

    static void
    generic_write_byte_le(T_sp c, T_sp strm)
    {
	cl_index (*write_byte8)(T_sp strm, unsigned char *c, cl_index n);
	cl_index bs;
	write_byte8 = StreamOps(strm).write_byte8;
	bs = StreamByteSize(strm);
	do {
            T_sp b = cl_logand(Cons_O::createList( c, Fixnum_O::create(0xFF)));
            unsigned char aux = (unsigned char)(b.as<Fixnum_O>()->get());
            if (write_byte8(strm, &aux, 1) < 1)
                break;
            c = cl_ash(c, Fixnum_O::create(-8));
            bs -= 8;
	} while (bs);
    }

    static T_sp
    generic_read_byte(T_sp strm)
    {
	cl_index (*read_byte8)(T_sp, unsigned char *, cl_index);
	unsigned char c;
	T_sp output = NULL;
	cl_index bs;
	read_byte8 = StreamOps(strm).read_byte8;
	bs = StreamByteSize(strm);
	for (; bs >= 8; bs -= 8) {
            if (read_byte8(strm, &c, 1) < 1)
                return _Nil<T_O>();
            if (output) {
                output = cl_logior(Cons_O::createList(Fixnum_O::create(c),
                                                      cl_ash(output, Fixnum_O::create(8))));
            } else if (strm->stream.flags & ECL_STREAM_SIGNED_BYTES) {
                output = Fixnum_O::create((signed char)c);
            } else {
                output = Fixnum_O::create((unsigned char)c);
            }
	}
	return output;
    }

    static void
    generic_write_byte(T_sp c, T_sp strm)
    {
	cl_index (*write_byte8)(T_sp strm, unsigned char *c, cl_index n);
	cl_index bs;
	write_byte8 = StreamOps(strm).write_byte8;
	bs = StreamByteSize(strm);
	do {
            unsigned char aux;
            T_sp b;
            bs -= 8;
            b = cl_logand(Cons_O::createList(Fixnum_O::create(0xFF), bs? cl_ash(c, Fixnum_O::create(-bs)) : c));
            aux = (unsigned char)ecl_fixnum(b);
            if (write_byte8(strm, &aux, 1) < 1)
                break;
	} while (bs);
    }

    static claspCharacter
    generic_peek_char(T_sp strm)
    {
	claspCharacter out = ecl_read_char(strm);
	if (out != EOF) ecl_unread_char(out, strm);
	return out;
    }

    static void
    generic_void(T_sp strm)
    {
    }

    static int
    generic_always_true(T_sp strm)
    {
	return 1;
    }

    static int
    generic_always_false(T_sp strm)
    {
	return 0;
    }

    static T_sp
    generic_always_nil(T_sp strm)
    {
	return _Nil<T_O>();
    }

    static int
    generic_column(T_sp strm)
    {
	return 0;
    }

    static T_sp
    generic_set_position(T_sp strm, T_sp pos)
    {
	return _Nil<T_O>();
    }

    static T_sp
    generic_close(T_sp strm)
    {
	struct ecl_file_ops *ops = strm->stream.ops;
	if (ecl_input_stream_p(strm)) {
            ops->read_byte8 = closed_stream_read_byte8;
            ops->read_char = closed_stream_read_char;
            ops->unread_char = closed_stream_unread_char;
            ops->listen = closed_stream_listen;
            ops->clear_input = closed_stream_clear_input;
	}
	if (ecl_output_stream_p(strm)) {
            ops->write_byte8 = closed_stream_write_byte8;
            ops->write_char = closed_stream_write_char;
            ops->clear_output = closed_stream_clear_output;
            ops->force_output = closed_stream_force_output;
            ops->finish_output = closed_stream_finish_output;
	}
	ops->get_position = closed_stream_get_position;
	ops->set_position = closed_stream_set_position;
	ops->length = closed_stream_length;
	ops->close = generic_close;
	strm->stream.closed = 1;
	return ECL_T;
    }

    static cl_index
    generic_write_vector(T_sp strm, T_sp data, cl_index start, cl_index end)
    {
        cl_elttype elttype;
	const struct ecl_file_ops *ops;
	if (start >= end)
            return start;
	ops = stream_dispatch_table(strm);
        elttype = ecl_array_elttype(data);
	if (elttype == ecl_aet_bc ||
#ifdef ECL_UNICODE
	    elttype == ecl_aet_ch ||
#endif
	    (elttype == ecl_aet_object && CLASPCHARACTERP(ecl_elt(data, 0)))) {
            claspCharacter (*write_char)(T_sp, claspCharacter) = ops->write_char;			
            for (; start < end; start++) {
                write_char(strm, ecl_char_code(ecl_elt(data, start)));
            }
	} else {
            void (*write_byte)(T_sp, T_sp) = ops->write_byte;
            for (; start < end; start++) {
                write_byte(ecl_elt(data, start), strm);
            }
	}
	return start;
    }

    static cl_index
    generic_read_vector(T_sp strm, T_sp data, cl_index start, cl_index end)
    {
	const struct ecl_file_ops *ops;
	T_sp expected_type;
	if (start >= end)
            return start;
	expected_type = ecl_stream_element_type(strm);
	ops = stream_dispatch_table(strm);
	if (expected_type == @'base-char' || expected_type == @'character') {
            claspCharacter (*read_char)(T_sp) = ops->read_char;			
            for (; start < end; start++) {
                cl_fixnum c = read_char(strm);
                if (c == EOF) break;
                ecl_elt_set(data, start, ECL_CODE_CHAR(c));
            }
	} else {
            T_sp (*read_byte)(T_sp) = ops->read_byte;
            for (; start < end; start++) {
                T_sp x = read_byte(strm);
                if (Null(x)) break;
                ecl_elt_set(data, start, x);
            }
	}
	return start;
    }




/**********************************************************************
 * CHARACTER AND EXTERNAL FORMAT SUPPORT
 */

    static void
    eformat_unread_char(T_sp strm, claspCharacter c)
    {
	unlikely_if (c != strm->stream.last_char) {
            unread_twice(strm);
	}
	{
            unsigned char buffer[2*ENCODING_BUFFER_MAX_SIZE];
            int ndx = 0;
            T_sp l = strm->stream.byte_stack;
            cl_fixnum i = strm->stream.last_code[0];
            if (i != EOF) {
                ndx += strm->stream.encoder(strm, buffer, i);
            }
            i = strm->stream.last_code[1];
            if (i != EOF) {
                ndx += strm->stream.encoder(strm, buffer+ndx, i);
            }
            while (ndx != 0) {
                l = CONS(ecl_make_fixnum(buffer[--ndx]), l);
            }
            strm->stream.byte_stack = l;
            strm->stream.last_char = EOF;
	}
    }

    static claspCharacter
    eformat_read_char(T_sp strm)
    {
	claspCharacter c = strm->stream.decoder(strm);
	unlikely_if (c == strm->stream.eof_char)
            return EOF;
	if (c != EOF) {
            strm->stream.last_char = c;
            strm->stream.last_code[0] = c;
            strm->stream.last_code[1] = EOF;
	}
	return c;
    }

    static claspCharacter
    eformat_write_char(T_sp strm, claspCharacter c)
    {
	unsigned char buffer[ENCODING_BUFFER_MAX_SIZE];
	claspCharacter nbytes;
        nbytes = strm->stream.encoder(strm, buffer, c);
	strm->stream.ops->write_byte8(strm, buffer, nbytes);
	if (c == '\n')
            strm->stream.column = 0;
	else if (c == '\t')
            strm->stream.column = (strm->stream.column & ~((cl_index)07)) + 8;
	else
            strm->stream.column++;
	fflush(stdout);
	return c;
    }

    static claspCharacter
    eformat_read_char_cr(T_sp strm)
    {
	claspCharacter c = eformat_read_char(strm);
	if (c == ECL_CHAR_CODE_RETURN) {
            c = ECL_CHAR_CODE_NEWLINE;
            strm->stream.last_char = c;
	}
	return c;
    }

    static claspCharacter
    eformat_write_char_cr(T_sp strm, claspCharacter c)
    {
	if (c == ECL_CHAR_CODE_NEWLINE) {
            eformat_write_char(strm, ECL_CHAR_CODE_RETURN);
            strm->stream.column = 0;
            return c;
	}
	return eformat_write_char(strm, c);
    }

    static claspCharacter
    eformat_read_char_crlf(T_sp strm)
    {
	claspCharacter c = eformat_read_char(strm);
	if (c == ECL_CHAR_CODE_RETURN) {
            c = eformat_read_char(strm);
            if (c == ECL_CHAR_CODE_LINEFEED) {
                strm->stream.last_code[0] = ECL_CHAR_CODE_RETURN;
                strm->stream.last_code[1] = c;
                c = ECL_CHAR_CODE_NEWLINE;
            } else {
                eformat_unread_char(strm, c);
                c = ECL_CHAR_CODE_RETURN;
                strm->stream.last_code[0] = c;
                strm->stream.last_code[1] = EOF;
            }
            strm->stream.last_char = c;
	}
	return c;
    }

    static claspCharacter
    eformat_write_char_crlf(T_sp strm, claspCharacter c)
    {
	if (c == ECL_CHAR_CODE_NEWLINE) {
            eformat_write_char(strm, ECL_CHAR_CODE_RETURN);
            eformat_write_char(strm, ECL_CHAR_CODE_LINEFEED);
            strm->stream.column = 0;
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
    passthrough_decoder(T_sp stream)
    {
	unsigned char aux;
	if (ecl_read_byte8(stream, &aux, 1) < 1)
            return EOF;
	else
            return aux;
    }

    static int
    passthrough_encoder(T_sp stream, unsigned char *buffer, claspCharacter c)
    {
#ifdef ECL_UNICODE
	unlikely_if (c > 0xFF) {
            return encoding_error(stream, buffer, c);
	}
#endif
	buffer[0] = c;
	return 1;
    }

#ifdef ECL_UNICODE
/*
 * US ASCII, that is the 128 (0-127) lowest codes of Unicode
 */

    static claspCharacter
    ascii_decoder(T_sp stream)
    {
	unsigned char aux;
	if (ecl_read_byte8(stream, &aux, 1) < 1) {
            return EOF;
	} else if (aux > 127) {
            return decoding_error(stream, &aux, 1);
	} else {
            return aux;
	}
    }

    static int
    ascii_encoder(T_sp stream, unsigned char *buffer, claspCharacter c)
    {
	unlikely_if (c > 127) {
            return encoding_error(stream, buffer, c);
	}
	buffer[0] = c;
	return 1;
    }

/*
 * UCS-4 BIG ENDIAN
 */

    static claspCharacter
    ucs_4be_decoder(T_sp stream)
    {
	unsigned char buffer[4];
	if (ecl_read_byte8(stream, buffer, 4) < 4) {
            return EOF;
	} else {
            return buffer[3]+(buffer[2]<<8)+(buffer[1]<<16)+(buffer[0]<<24);
	}
    }

    static int
    ucs_4be_encoder(T_sp stream, unsigned char *buffer, claspCharacter c)
    {
	buffer[3] = c & 0xFF; c >>= 8;
	buffer[2] = c & 0xFF; c >>= 8;
	buffer[1] = c & 0xFF; c >>= 8;
	buffer[0] = c;
	return 4;
    }

/*
 * UCS-4 LITTLE ENDIAN
 */

    static claspCharacter
    ucs_4le_decoder(T_sp stream)
    {
	unsigned char buffer[4];
	if (ecl_read_byte8(stream, buffer, 4) < 4) {
            return EOF;
	} else {
            return buffer[0]+(buffer[1]<<8)+(buffer[2]<<16)+(buffer[3]<<24);
	}
    }

    static int
    ucs_4le_encoder(T_sp stream, unsigned char *buffer, claspCharacter c)
    {
	buffer[0] = c & 0xFF; c >>= 8;
	buffer[1] = c & 0xFF; c >>= 8;
	buffer[2] = c & 0xFF; c >>= 8;
	buffer[3] = c;
	return 4;
    }

/*
 * UCS-4 BOM ENDIAN
 */

    static claspCharacter
    ucs_4_decoder(T_sp stream)
    {
	cl_fixnum c = ucs_4be_decoder(stream);
	if (c == 0xFEFF) {
            stream->stream.decoder = ucs_4be_decoder;
            stream->stream.encoder = ucs_4be_encoder;
            return ucs_4be_decoder(stream);
	} else if (c == 0xFFFE0000) {
            stream->stream.decoder = ucs_4le_decoder;
            stream->stream.encoder = ucs_4le_encoder;
            return ucs_4le_decoder(stream);
	} else {
            stream->stream.decoder = ucs_4be_decoder;
            stream->stream.encoder = ucs_4be_encoder;
            return c;
	}
    }

    static int
    ucs_4_encoder(T_sp stream, unsigned char *buffer, claspCharacter c)
    {
	stream->stream.decoder = ucs_4be_decoder;
	stream->stream.encoder = ucs_4be_encoder;
	buffer[0] = 0xFF;
	buffer[1] = 0xFE;
	buffer[2] = buffer[3] = 0;
	return 4 + ucs_4be_encoder(stream, buffer+4, c);
    }


/*
 * UTF-16 BIG ENDIAN
 */

    static claspCharacter
    ucs_2be_decoder(T_sp stream)
    {
	unsigned char buffer[2] = {0,0};
	if (ecl_read_byte8(stream, buffer, 2) < 2) {
            return EOF;
	} else {
            claspCharacter c = ((claspCharacter)buffer[0] << 8) | buffer[1];
            if ((buffer[0] & 0xFC) == 0xD8) {
                if (ecl_read_byte8(stream, buffer, 2) < 2) {
                    return EOF;
                } else {
                    claspCharacter aux = ((claspCharacter)buffer[0] << 8) | buffer[1];
                    if ((buffer[0] & 0xF8) != 0xDC) {
                        return decoding_error(stream, buffer, 1);
                    }
                    return ((c & 0x3FFF) << 10) + (aux & 0x3FFF) + 0x10000;
                }
            }
            return c;
	}
    }

    static int
    ucs_2be_encoder(T_sp stream, unsigned char *buffer, claspCharacter c)
    {
	if (c >= 0x10000) {
            c -= 0x10000;
            ucs_2be_encoder(stream, buffer, (c >> 10) | 0xD800);
            ucs_2be_encoder(stream, buffer+2, (c & 0x3FFF) | 0xDC00);
            return 4;
	} else {
            buffer[1] = c & 0xFF; c >>= 8;
            buffer[0] = c;
            return 2;
	}
    }

/*
 * UTF-16 LITTLE ENDIAN
 */

    static claspCharacter
    ucs_2le_decoder(T_sp stream)
    {
	unsigned char buffer[2];
	if (ecl_read_byte8(stream, buffer, 2) < 2) {
            return EOF;
	} else {
            claspCharacter c = ((claspCharacter)buffer[1] << 8) | buffer[0];
            if ((buffer[1] & 0xFC) == 0xD8) {
                if (ecl_read_byte8(stream, buffer, 2) < 2) {
                    return EOF;
                } else {
                    claspCharacter aux = ((claspCharacter)buffer[1] << 8) | buffer[0];
                    if ((buffer[1] & 0xF8) != 0xDC) {
                        return decoding_error(stream, buffer, 2);
                    }
                    return ((c & 0x3FFF) << 10) + (aux & 0x3FFF) + 0x10000;
                }
            }
            return c;
	}
    }

    static int
    ucs_2le_encoder(T_sp stream, unsigned char *buffer, claspCharacter c)
    {
	if (c >= 0x10000) {
            c -= 0x10000;
            ucs_2le_encoder(stream, buffer, (c >> 10) | 0xD8000);
            ucs_2le_encoder(stream, buffer+2, (c & 0x3FFF) | 0xD800);
            return 4;
	} else {
            buffer[0] = c & 0xFF; c >>= 8;
            buffer[1] = c & 0xFF;
            return 2;
	}
    }

/*
 * UTF-16 BOM ENDIAN
 */

    static claspCharacter
    ucs_2_decoder(T_sp stream)
    {
	claspCharacter c = ucs_2be_decoder(stream);
	if (c == 0xFEFF) {
            stream->stream.decoder = ucs_2be_decoder;
            stream->stream.encoder = ucs_2be_encoder;
            return ucs_2be_decoder(stream);
	} else if (c == 0xFFFE) {
            stream->stream.decoder = ucs_2le_decoder;
            stream->stream.encoder = ucs_2le_encoder;
            return ucs_2le_decoder(stream);
	} else {
            stream->stream.decoder = ucs_2be_decoder;
            stream->stream.encoder = ucs_2be_encoder;
            return c;
	}
    }

    static int
    ucs_2_encoder(T_sp stream, unsigned char *buffer, claspCharacter c)
    {
	stream->stream.decoder = ucs_2be_decoder;
	stream->stream.encoder = ucs_2be_encoder;
	buffer[0] = 0xFF;
	buffer[1] = 0xFE;
	return 2 + ucs_2be_encoder(stream, buffer+2, c);
    }

/*
 * USER DEFINED ENCODINGS. SIMPLE CASE.
 */

    static claspCharacter
    user_decoder(T_sp stream)
    {
	T_sp table = stream->stream.format_table;
	T_sp character;
	unsigned char buffer[2];
	if (ecl_read_byte8(stream, buffer, 1) < 1) {
            return EOF;
	}
	character = ecl_gethash_safe(ecl_make_fixnum(buffer[0]), table, ECL_NIL);
	unlikely_if (Null(character)) {
            return decoding_error(stream, buffer, 1);
	}
	if (character == ECL_T) {
            if (ecl_read_byte8(stream, buffer+1, 1) < 1) {
                return EOF;
            } else {
                cl_fixnum byte = (buffer[0]<<8) + buffer[1];
                character = ecl_gethash_safe(ecl_make_fixnum(byte), table, ECL_NIL);
                unlikely_if (Null(character)) {
                    return decoding_error(stream, buffer, 2);
                }
            }
	}
	return ECL_CHAR_CODE(character);
    }

    static int
    user_encoder(T_sp stream, unsigned char *buffer, claspCharacter c)
    {
	T_sp byte = ecl_gethash_safe(ECL_CODE_CHAR(c), stream->stream.format_table, ECL_NIL);
	if (Null(byte)) {
            return encoding_error(stream, buffer, c);
	} else {
            cl_fixnum code = ecl_fixnum(byte);
            if (code > 0xFF) {
                buffer[1] = code & 0xFF; code >>= 8;
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
    user_multistate_decoder(T_sp stream)
    {
	T_sp table_list = stream->stream.format_table;
	T_sp table = ECL_CONS_CAR(table_list);
	T_sp character;
	cl_fixnum i, j;
	unsigned char buffer[ENCODING_BUFFER_MAX_SIZE];
	for (i = j = 0; i < ENCODING_BUFFER_MAX_SIZE; i++) {
            if (ecl_read_byte8(stream, buffer+i, 1) < 1) {
                return EOF;
            }
            j = (j << 8) | buffer[i];
            character = ecl_gethash_safe(ecl_make_fixnum(j), table, ECL_NIL);
            if (CLASPCHARACTERP(character)) {
                return ECL_CHAR_CODE(character);
            }
            unlikely_if (Null(character)) {
                return decoding_error(stream, buffer, i);
            }
            if (character == ECL_T) {
                /* Need more characters */
                continue;
            }
            if (CONSP(character)) {
                /* Changed the state. */
                stream->stream.format_table = table_list = character;
                table = ECL_CONS_CAR(table_list);
                i = j = 0;
                continue;
            }
            break;
	}
	FEerror("Internal error in decoder table.", 0);
    }

    static int
    user_multistate_encoder(T_sp stream, unsigned char *buffer, claspCharacter c)
    {
	T_sp table_list = stream->stream.format_table;
	T_sp p = table_list;
	do {
            T_sp table = ECL_CONS_CAR(p);
            T_sp byte = ecl_gethash_safe(ECL_CODE_CHAR(c), table, ECL_NIL);
            if (!Null(byte)) {
                cl_fixnum code = ecl_fixnum(byte);
                claspCharacter n = 0;
                if (p != table_list) {
                    /* Must output a escape sequence */
                    T_sp x = ecl_gethash_safe(ECL_T, table, ECL_NIL);
                    while (!Null(x)) {
                        buffer[0] = ecl_fixnum(ECL_CONS_CAR(x));
                        buffer++;
                        x = ECL_CONS_CDR(x);
                        n++;
                    }
                    stream->stream.format_table = p;
                }
                if (code > 0xFF) {
                    buffer[1] = code & 0xFF; code >>= 8;
                    buffer[0] = code;
                    return n+2;
                } else {
                    buffer[0] = code;
                    return n+1;
                }
            }
            p = ECL_CONS_CDR(p);
	} while (p != table_list);
	/* Exhausted all lists */
	return encoding_error(stream, buffer, c);
    }

/*
 * UTF-8
 */

    static claspCharacter
    utf_8_decoder(T_sp stream)
    {
	/* In understanding this code:
	 * 0x8 = 1000, 0xC = 1100, 0xE = 1110, 0xF = 1111
	 * 0x1 = 0001, 0x3 = 0011, 0x7 = 0111, 0xF = 1111
	 */
	claspCharacter cum = 0;
	unsigned char buffer[5];
	int nbytes, i;
	if (ecl_read_byte8(stream, buffer, 1) < 1)
            return EOF;
	if ((buffer[0] & 0x80) == 0) {
            return buffer[0];
	}
	unlikely_if ((buffer[0] & 0x40) == 0)
            return decoding_error(stream, buffer, 1);
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
            return decoding_error(stream, buffer, 1);
	}
	if (ecl_read_byte8(stream, buffer+1, nbytes) < nbytes)
            return EOF;
	for (i = 1; i <= nbytes; i++) {
            unsigned char c = buffer[i];
            /*printf(": %04x :", c);*/
            unlikely_if ((c & 0xC0) != 0x80)
                return decoding_error(stream, buffer, nbytes+1);
            cum = (cum << 6) | (c & 0x3F);
            unlikely_if (cum == 0)
                return decoding_error(stream, buffer, nbytes+1);
	}
	if (cum >= 0xd800) {
            unlikely_if (cum <= 0xdfff)
                return decoding_error(stream, buffer, nbytes+1);
            unlikely_if (cum >= 0xFFFE && cum <= 0xFFFF)
                return decoding_error(stream, buffer, nbytes+1);
	}
	/*printf("; %04x ;", cum);*/
	return cum;
    }

    static int
    utf_8_encoder(T_sp stream, unsigned char *buffer, claspCharacter c)
    {
	int nbytes;
	if (c < 0) {
            nbytes = 0;
	} else if (c <= 0x7F) {
            buffer[0] = c;
            nbytes = 1;
	} else if (c <= 0x7ff) {
            buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
            buffer[0] = c | 0xC0;
            /*printf("\n; %04x ;: %04x :: %04x :\n", c_orig, buffer[0], buffer[1]);*/
            nbytes = 2;
	} else if (c <= 0xFFFF) {
            buffer[2] = (c & 0x3f) | 0x80; c >>= 6;
            buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
            buffer[0] = c | 0xE0;
            nbytes = 3;
	} else if (c <= 0x1FFFFFL) {
            buffer[3] = (c & 0x3f) | 0x80; c >>= 6;
            buffer[2] = (c & 0x3f) | 0x80; c >>= 6;
            buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
            buffer[0] = c | 0xF0;
            nbytes = 4;
	}
	return nbytes;
    }
#endif

/********************************************************************************
 * CLOS STREAMS
 */

#ifdef ECL_CLOS_STREAMS
    static cl_index
    clos_stream_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	cl_index i;
	for (i = 0; i < n; i++) {
            T_sp byte = _ecl_funcall2(@'gray::stream-read-byte', strm);
            if (!ECL_FIXNUMP(byte))
                break;
            c[i] = ecl_fixnum(byte);
	}
	return i;
    }

    static cl_index
    clos_stream_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	cl_index i;
	for (i = 0; i < n; i++) {
            T_sp byte = _ecl_funcall3(@'gray::stream-write-byte', strm,
                                      ecl_make_fixnum(c[i]));
            if (!ECL_FIXNUMP(byte))
                break;
	}
	return i;
    }

    static T_sp
    clos_stream_read_byte(T_sp strm)
    {
	T_sp b = _ecl_funcall2(@'gray::stream-read-byte', strm);
        if (b == @':eof') b = ECL_NIL;
        return b;
    }

    static void
    clos_stream_write_byte(T_sp c, T_sp strm)
    {
	_ecl_funcall3(@'gray::stream-write-byte', strm, c);
    }

    static claspCharacter
    clos_stream_read_char(T_sp strm)
    {
	T_sp output = _ecl_funcall2(@'gray::stream-read-char', strm);
        cl_fixnum value;
	if (CLASPCHARACTERP(output))
            value = ECL_CHAR_CODE(output);
        else if (ECL_FIXNUMP(output))
            value = ecl_fixnum(output);
	else if (output == ECL_NIL || output == @':eof')
            return EOF;
        else
            value = -1;
        unlikely_if (value < 0 || value > ECL_CHAR_CODE_LIMIT)
            FEerror("Unknown character ~A", 1, output.asTPtr());
        return value;
    }

    static claspCharacter
    clos_stream_write_char(T_sp strm, claspCharacter c)
    {
	_ecl_funcall3(@'gray::stream-write-char', strm, ECL_CODE_CHAR(c));
	return c;
    }

    static void
    clos_stream_unread_char(T_sp strm, claspCharacter c)
    {
	_ecl_funcall3(@'gray::stream-unread-char', strm, ECL_CODE_CHAR(c));
    }

    static int
    clos_stream_peek_char(T_sp strm)
    {
	T_sp out = _ecl_funcall2(@'gray::stream-peek-char', strm);
	if (out == @':eof') return EOF;
	return ecl_char_code(out);
    }

    static int
    clos_stream_listen(T_sp strm)
    {
	return !Null(_ecl_funcall2(@'gray::stream-listen', strm));
    }

    static void
    clos_stream_clear_input(T_sp strm)
    {
	_ecl_funcall2(@'gray::stream-clear-input', strm);
    }

    static void
    clos_stream_clear_output(T_sp strm)
    {
	_ecl_funcall2(@'gray::stream-clear-output', strm);
	return;
    }

    static void
    clos_stream_force_output(T_sp strm)
    {
	_ecl_funcall2(@'gray::stream-force-output', strm);
    }

    static void
    clos_stream_finish_output(T_sp strm)
    {
	_ecl_funcall2(@'gray::stream-finish-output', strm);
    }

    static int
    clos_stream_input_p(T_sp strm)
    {
	return !Null(_ecl_funcall2(@'gray::input-stream-p', strm));
    }

    static int
    clos_stream_output_p(T_sp strm)
    {
	return !Null(_ecl_funcall2(@'gray::output-stream-p', strm));
    }

    static int
    clos_stream_interactive_p(T_sp strm)
    {
	return !Null(_ecl_funcall2(@'gray::stream-interactive-p', strm));

    }

    static T_sp
    clos_stream_element_type(T_sp strm)
    {
	return _ecl_funcall2(@'gray::stream-element-type', strm);
    }

#define clos_stream_length not_a_file_stream

    static T_sp
    clos_stream_get_position(T_sp strm)
    {
	return _ecl_funcall2(@'gray::stream-file-position', strm);
    }

    static T_sp
    clos_stream_set_position(T_sp strm, T_sp pos)
    {
	return _ecl_funcall3(@'gray::stream-file-position', strm, pos);
    }

    static int
    clos_stream_column(T_sp strm)
    {
	T_sp col = _ecl_funcall2(@'gray::stream-line-column', strm);
	/* FIXME! The Gray streams specifies NIL is a valid
	 * value but means "unknown". Should we make it
	 * zero? */
	return Null(col)? 0 : ecl_to_size(col);
    }

    static T_sp
    clos_stream_close(T_sp strm)
    {
	return _ecl_funcall2(@'gray::close', strm);
    }

    const struct ecl_file_ops clos_stream_ops = {
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
	clos_stream_close
    };
#endif /* ECL_CLOS_STREAMS */

/**********************************************************************
 * STRING OUTPUT STREAMS
 */

    static claspCharacter
    str_out_write_char(T_sp strm, claspCharacter c)
    {
	int column = strm->stream.column;
	if (c == '\n')
            strm->stream.column = 0;
	else if (c == '\t')
            strm->stream.column = (column&~(cl_index)7) + 8;
	else
            strm->stream.column++;
	ecl_string_push_extend(STRING_OUTPUT_STRING(strm), c);
	return c;
    }

    static T_sp
    str_out_element_type(T_sp strm)
    {
	T_sp string = STRING_OUTPUT_STRING(strm);
	if (ECL_BASE_STRING_P(string))
            return @'base-char';
	return @'character';
    }

    static T_sp
    str_out_get_position(T_sp strm)
    {
	return ecl_make_unsigned_integer(STRING_OUTPUT_STRING(strm)->base_string.fillp);
    }

    static T_sp
    str_out_set_position(T_sp strm, T_sp pos)
    {
	T_sp string = STRING_OUTPUT_STRING(strm);
	cl_fixnum disp;
	if (Null(pos)) {
            disp = strm->base_string.dim;
	} else {
            disp = ecl_to_size(pos);
	}
	if (disp < string->base_string.fillp) {
            string->base_string.fillp = disp;
	} else {
            disp -= string->base_string.fillp;
            while (disp-- > 0)
                ecl_write_char(' ', strm);
	}
	return ECL_T;
    }

    static int
    str_out_column(T_sp strm)
    {
	return strm->stream.column;
    }

    const struct ecl_file_ops str_out_ops = {
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
	generic_always_true, /* output_p */
	generic_always_false,
	str_out_element_type,

	not_a_file_stream, /* length */
	str_out_get_position,
	str_out_set_position,
	str_out_column,
	generic_close
    };


    T_sp
    si_make_string_output_stream_from_string(T_sp s)
    {
	T_sp strm = alloc_stream();
	unlikely_if (!ECL_STRINGP(s) || !ECL_ARRAY_HAS_FILL_POINTER_P(s))
            FEerror("~S is not a -string with a fill-pointer.", 1, s.asTPtr());
	strm->stream.ops = duplicate_dispatch_table(&str_out_ops);
	strm->stream.mode = (short)ecl_smm_string_output;
	STRING_OUTPUT_STRING(strm) = s;
	strm->stream.column = 0;
#if !defined(ECL_UNICODE)
	strm->stream.format = @':pass-through';
	strm->stream.flags = ECL_STREAM_DEFAULT_FORMAT;
	strm->stream.byte_size = 8;
#else
	if (ECL_BASE_STRING_P(s)) {
            strm->stream.format = @':latin-1';
            strm->stream.flags = ECL_STREAM_LATIN_1;
            strm->stream.byte_size = 8;
	} else {
            strm->stream.format = @':ucs-4';
            strm->stream.flags = ECL_STREAM_UCS_4;
            strm->stream.byte_size = 32;
	}
#endif
	@(return strm)
            }

    T_sp
    ecl_make_string_output_stream(cl_index line_length, int extended)
    {
#ifdef ECL_UNICODE
	T_sp s = extended?
            ecl_alloc_adjustable_extended_string(line_length) :
            ecl_alloc_adjustable_base_string(line_length);
#else
	T_sp s = ecl_alloc_adjustable_base_string(line_length);
#endif
	return si_make_string_output_stream_from_string(s);
    }

    @(defun make-string-output-stream (&key (element_type @'character'))
      int extended = 0;
      @
      if (element_type == @'base-char') {
          (void)0;
      } else if (element_type == @'character') {
#ifdef ECL_UNICODE
          extended = 1;
#endif
      } else if (!Null(_ecl_funcall3(@'subtypep', element_type, @'base-char'))) {
          (void)0;
      } else if (!Null(_ecl_funcall3(@'subtypep', element_type, @'character'))) {
#ifdef ECL_UNICODE
          extended = 1;
#endif
      } else {
          FEerror("In MAKE-STRING-OUTPUT-STREAM, the argument :ELEMENT-TYPE (~A) must be a subtype of character",
                  1, element_type.asTPtr());
      }
      @(return ecl_make_string_output_stream(128, extended))
      @)

    T_sp
    cl_get_output_stream_string(T_sp strm)
    {
	T_sp strng;
	unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_string_output))
            FEwrong_type_only_arg(@[get-output-stream-string],
                                  strm, @[string-stream]);
	strng = cl_copy_seq(STRING_OUTPUT_STRING(strm));
	STRING_OUTPUT_STRING(strm)->base_string.fillp = 0;
	@(return strng)
            }

/**********************************************************************
 * STRING INPUT STREAMS
 */

    static claspCharacter
    str_in_read_char(T_sp strm)
    {
	cl_fixnum curr_pos = STRING_INPUT_POSITION(strm);
	claspCharacter c;
	if (curr_pos >= STRING_INPUT_LIMIT(strm)) {
            c = EOF;
	} else {
            c = ecl_char(STRING_INPUT_STRING(strm), curr_pos);
            STRING_INPUT_POSITION(strm) = curr_pos+1;
	}
	return c;
    }

    static void
    str_in_unread_char(T_sp strm, claspCharacter c)
    {
	cl_fixnum curr_pos = STRING_INPUT_POSITION(strm);
	unlikely_if (c <= 0) {
            unread_error(strm);
	}
	STRING_INPUT_POSITION(strm) = curr_pos - 1;
    }

    static claspCharacter
    str_in_peek_char(T_sp strm)
    {
	cl_index pos = STRING_INPUT_POSITION(strm);
	if (pos >= STRING_INPUT_LIMIT(strm)) {
            return EOF;
	} else {
            return ecl_char(STRING_INPUT_STRING(strm), pos);
	}
    }

    static int
    str_in_listen(T_sp strm)
    {
	if (STRING_INPUT_POSITION(strm) < STRING_INPUT_LIMIT(strm))
            return ECL_LISTEN_AVAILABLE;
	else
            return ECL_LISTEN_EOF;
    }

    static T_sp
    str_in_element_type(T_sp strm)
    {
	T_sp string = STRING_INPUT_STRING(strm);
	if (ECL_BASE_STRING_P(string))
            return @'base-char';
	return @'character';
    }

    static T_sp
    str_in_get_position(T_sp strm)
    {
	return ecl_make_unsigned_integer(STRING_INPUT_POSITION(strm));
    }

    static T_sp
    str_in_set_position(T_sp strm, T_sp pos)
    {
	cl_fixnum disp;
	if (Null(pos)) {
            disp = STRING_INPUT_LIMIT(strm);
	}  else {
            disp = ecl_to_size(pos);
            if (disp >= STRING_INPUT_LIMIT(strm)) {
                disp = STRING_INPUT_LIMIT(strm);
            }
	}
	STRING_INPUT_POSITION(strm) = disp;
	return ECL_T;
    }

    const struct ecl_file_ops str_in_ops = {
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

	generic_always_true, /* input_p */
	generic_always_false, /* output_p */
	generic_always_false,
	str_in_element_type,

	not_a_file_stream, /* length */
	str_in_get_position,
	str_in_set_position,
	generic_column,
	generic_close
    };

    T_sp
    ecl_make_string_input_stream(T_sp strng, cl_index istart, cl_index iend)
    {
	T_sp strm;

	strm = alloc_stream();
	strm->stream.ops = duplicate_dispatch_table(&str_in_ops);
	strm->stream.mode = (short)ecl_smm_string_input;
	STRING_INPUT_STRING(strm) = strng;
	STRING_INPUT_POSITION(strm) = istart;
	STRING_INPUT_LIMIT(strm) = iend;
#if !defined(ECL_UNICODE)
	strm->stream.format = @':pass-through';
	strm->stream.flags = ECL_STREAM_DEFAULT_FORMAT;
	strm->stream.byte_size = 8;
#else
	if (ECL_BASE_STRING_P(strng) == t_base_string) {
            strm->stream.format = @':latin-1';
            strm->stream.flags = ECL_STREAM_LATIN_1;
            strm->stream.byte_size = 8;
	} else {
            strm->stream.format = @':ucs-4';
            strm->stream.flags = ECL_STREAM_UCS_4;
            strm->stream.byte_size = 32;
	}
#endif
	return strm;
    }

    @(defun make_string_input_stream (strng &o (istart ecl_make_fixnum(0)) iend)
      cl_index_pair p;
      @
      strng = cl_string(strng);
      p = ecl_vector_start_end(@[make-string-input-stream], strng, istart, iend);
      @(return (ecl_make_string_input_stream(strng, p.start, p.end)))
      @)

    /**********************************************************************
     * TWO WAY STREAM
     */

    static cl_index
    two_way_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	if (strm == cl_core.terminal_io)
            ecl_force_output(TWO_WAY_STREAM_OUTPUT(cl_core.terminal_io));
	return ecl_read_byte8(TWO_WAY_STREAM_INPUT(strm), c, n);
    }

    static cl_index
    two_way_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	return ecl_write_byte8(TWO_WAY_STREAM_OUTPUT(strm), c, n);
    }

    static void
    two_way_write_byte(T_sp byte, T_sp stream)
    {
	ecl_write_byte(byte, TWO_WAY_STREAM_OUTPUT(stream));
    }

    static T_sp
    two_way_read_byte(T_sp stream)
    {
	return ecl_read_byte(TWO_WAY_STREAM_INPUT(stream));
    }

    static claspCharacter
    two_way_read_char(T_sp strm)
    {
	return ecl_read_char(TWO_WAY_STREAM_INPUT(strm));
    }

    static claspCharacter
    two_way_write_char(T_sp strm, claspCharacter c)
    {
	return ecl_write_char(c, TWO_WAY_STREAM_OUTPUT(strm));
    }

    static void
    two_way_unread_char(T_sp strm, claspCharacter c)
    {
	ecl_unread_char(c, TWO_WAY_STREAM_INPUT(strm));
    }

    static claspCharacter
    two_way_peek_char(T_sp strm)
    {
	return ecl_peek_char(TWO_WAY_STREAM_INPUT(strm));
    }

    static cl_index
    two_way_read_vector(T_sp strm, T_sp data, cl_index start, cl_index n)
    {
	strm = TWO_WAY_STREAM_INPUT(strm);
	return stream_dispatch_table(strm)->read_vector(strm, data, start, n);
    }

    static cl_index
    two_way_write_vector(T_sp strm, T_sp data, cl_index start, cl_index n)
    {
	strm = TWO_WAY_STREAM_OUTPUT(strm);
	return stream_dispatch_table(strm)->write_vector(strm, data, start, n);
    }

    static int
    two_way_listen(T_sp strm)
    {
	return ecl_listen_stream(TWO_WAY_STREAM_INPUT(strm));
    }

    static void
    two_way_clear_input(T_sp strm)
    {
	ecl_clear_input(TWO_WAY_STREAM_INPUT(strm));
    }

    static void
    two_way_clear_output(T_sp strm)
    {
	ecl_clear_output(TWO_WAY_STREAM_OUTPUT(strm));
    }

    static void
    two_way_force_output(T_sp strm)
    {
	ecl_force_output(TWO_WAY_STREAM_OUTPUT(strm));
    }

    static void
    two_way_finish_output(T_sp strm)
    {
	ecl_finish_output(TWO_WAY_STREAM_OUTPUT(strm));
    }

    static int
    two_way_interactive_p(T_sp strm)
    {
	return ecl_interactive_stream_p(TWO_WAY_STREAM_INPUT(strm));
    }

    static T_sp
    two_way_element_type(T_sp strm)
    {
	return ecl_stream_element_type(TWO_WAY_STREAM_INPUT(strm));
    }

    static int
    two_way_column(T_sp strm)
    {
	return ecl_file_column(TWO_WAY_STREAM_OUTPUT(strm));
    }

    static T_sp
    two_way_close(T_sp strm)
    {
	if (strm->stream.flags & ECL_STREAM_CLOSE_COMPONENTS) {
            cl_close(1, TWO_WAY_STREAM_INPUT(strm));
            cl_close(1, TWO_WAY_STREAM_OUTPUT(strm));
	}
	return generic_close(strm);
    }

    const struct ecl_file_ops two_way_ops = {
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

	not_a_file_stream, /* length */
	generic_always_nil, /* get_position */
	generic_set_position,
	two_way_column,
	two_way_close
    };


    T_sp
    cl_make_two_way_stream(T_sp istrm, T_sp ostrm)
    {
	T_sp strm;
	if (!ecl_input_stream_p(istrm))
            not_an_input_stream(istrm);
	if (!ecl_output_stream_p(ostrm))
            not_an_output_stream(ostrm);
	strm = alloc_stream();
	strm->stream.format = cl_stream_external_format(istrm);
	strm->stream.mode = (short)ecl_smm_two_way;
	strm->stream.ops = duplicate_dispatch_table(&two_way_ops);
	TWO_WAY_STREAM_INPUT(strm) = istrm;
	TWO_WAY_STREAM_OUTPUT(strm) = ostrm;
	@(return strm)
            }

    T_sp
    cl_two_way_stream_input_stream(T_sp strm)
    {
	unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm,ecl_smm_two_way))
            FEwrong_type_only_arg(@[two-way-stream-input-stream],
                                  strm, @[two-way-stream]);
	@(return TWO_WAY_STREAM_INPUT(strm));
    }

    T_sp
    cl_two_way_stream_output_stream(T_sp strm)
    {
	unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_two_way))
            FEwrong_type_only_arg(@[two-way-stream-output-stream],
                                  strm, @[two-way-stream]);
	@(return TWO_WAY_STREAM_OUTPUT(strm))
            }

/**********************************************************************
 * BROADCAST STREAM
 */

    static cl_index
    broadcast_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	T_sp l;
	cl_index out = n;
	for (l = BROADCAST_STREAM_LIST(strm); !Null(l); l = ECL_CONS_CDR(l)) {
            out = ecl_write_byte8(ECL_CONS_CAR(l), c, n);
	}
	return out;
    }

    static claspCharacter
    broadcast_write_char(T_sp strm, claspCharacter c)
    {
	T_sp l;
	for (l = BROADCAST_STREAM_LIST(strm); !Null(l); l = ECL_CONS_CDR(l)) {
            ecl_write_char(c, ECL_CONS_CAR(l));
	}
	return c;
    }

    static void
    broadcast_write_byte(T_sp c, T_sp strm)
    {
	T_sp l;
	for (l = BROADCAST_STREAM_LIST(strm); !Null(l); l = ECL_CONS_CDR(l)) {
            ecl_write_byte(c, ECL_CONS_CAR(l));
	}
    }

    static void
    broadcast_clear_output(T_sp strm)
    {
	T_sp l;
	for (l = BROADCAST_STREAM_LIST(strm); !Null(l); l = ECL_CONS_CDR(l)) {
            ecl_clear_output(ECL_CONS_CAR(l));
	}
    }

    static void
    broadcast_force_output(T_sp strm)
    {
	T_sp l;
	for (l = BROADCAST_STREAM_LIST(strm); !Null(l); l = ECL_CONS_CDR(l)) {
            ecl_force_output(ECL_CONS_CAR(l));
	}
    }

    static void
    broadcast_finish_output(T_sp strm)
    {
	T_sp l;
	for (l = BROADCAST_STREAM_LIST(strm); !Null(l); l = ECL_CONS_CDR(l)) {
            ecl_finish_output(ECL_CONS_CAR(l));
	}
    }

    static T_sp
    broadcast_element_type(T_sp strm)
    {
	T_sp l = BROADCAST_STREAM_LIST(strm);
	if (Null(l))
            return ECL_T;
	return ecl_stream_element_type(ECL_CONS_CAR(l));
    }

    static T_sp
    broadcast_length(T_sp strm)
    {
	T_sp l = BROADCAST_STREAM_LIST(strm);
	if (Null(l))
            return ecl_make_fixnum(0);
	return ecl_file_length(ECL_CONS_CAR(l));
    }

    static T_sp
    broadcast_get_position(T_sp strm)
    {
	T_sp l = BROADCAST_STREAM_LIST(strm);
	if (Null(l))
            return ecl_make_fixnum(0);
	return ecl_file_position(ECL_CONS_CAR(l));
    }

    static T_sp
    broadcast_set_position(T_sp strm, T_sp pos)
    {
	T_sp l = BROADCAST_STREAM_LIST(strm);
	if (Null(l))
            return ECL_NIL;
	return ecl_file_position_set(ECL_CONS_CAR(l), pos);
    }

    static int
    broadcast_column(T_sp strm)
    {
	T_sp l = BROADCAST_STREAM_LIST(strm);
	if (Null(l))
            return 0;
	return ecl_file_column(ECL_CONS_CAR(l));
    }

    static T_sp
    broadcast_close(T_sp strm)
    {
	if (strm->stream.flags & ECL_STREAM_CLOSE_COMPONENTS) {
            cl_mapc(2, @'close', BROADCAST_STREAM_LIST(strm));
	}
	return generic_close(strm);
    }

    const struct ecl_file_ops broadcast_ops = {
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
	generic_always_true, /* output_p */
	generic_always_false,
	broadcast_element_type,

	broadcast_length,
	broadcast_get_position,
	broadcast_set_position,
	broadcast_column,
	broadcast_close
    };

    @(defun make_broadcast_stream (&rest ap)
      T_sp x, streams;
      int i;
      @
      streams = ECL_NIL;
      for (i = 0; i < narg; i++) {
          x = ecl_va_arg(ap);
          unlikely_if (!ecl_output_stream_p(x))
              not_an_output_stream(x);
          streams = CONS(x, streams);
      }
      x = alloc_stream();
      x->stream.format = @':default';
      x->stream.ops = duplicate_dispatch_table(&broadcast_ops);
      x->stream.mode = (short)ecl_smm_broadcast;
      BROADCAST_STREAM_LIST(x) = cl_nreverse(streams);
      @(return x)
      @)

    T_sp
    cl_broadcast_stream_streams(T_sp strm)
    {
	unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_broadcast))
            FEwrong_type_only_arg(@[broadcast-stream-streams],
                                  strm, @[broadcast-stream]);
	return cl_copy_list(BROADCAST_STREAM_LIST(strm));
    }

/**********************************************************************
 * ECHO STREAM
 */

    static cl_index
    echo_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	cl_index out = ecl_read_byte8(ECHO_STREAM_INPUT(strm), c, n);
	return ecl_write_byte8(ECHO_STREAM_OUTPUT(strm), c, out);
    }

    static cl_index
    echo_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	return ecl_write_byte8(ECHO_STREAM_OUTPUT(strm), c, n);
    }

    static void
    echo_write_byte(T_sp c, T_sp strm)
    {
	ecl_write_byte(c, ECHO_STREAM_OUTPUT(strm));
    }

    static T_sp
    echo_read_byte(T_sp strm)
    {
	T_sp out = ecl_read_byte(ECHO_STREAM_INPUT(strm));
	if (!Null(out)) ecl_write_byte(out, ECHO_STREAM_OUTPUT(strm));
	return out;
    }

    static claspCharacter
    echo_read_char(T_sp strm)
    {
	claspCharacter c = strm->stream.last_code[0];
	if (c == EOF) {
            c = ecl_read_char(ECHO_STREAM_INPUT(strm));
            if (c != EOF)
                ecl_write_char(c, ECHO_STREAM_OUTPUT(strm));
	} else {
            strm->stream.last_code[0] = EOF;
            ecl_read_char(ECHO_STREAM_INPUT(strm));
	}
	return c;
    }

    static claspCharacter
    echo_write_char(T_sp strm, claspCharacter c)
    {
	return ecl_write_char(c, ECHO_STREAM_OUTPUT(strm));
    }

    static void
    echo_unread_char(T_sp strm, claspCharacter c)
    {
	unlikely_if (strm->stream.last_code[0] != EOF) {
            unread_twice(strm);
	}
	strm->stream.last_code[0] = c;
	ecl_unread_char(c, ECHO_STREAM_INPUT(strm));
    }

    static claspCharacter
    echo_peek_char(T_sp strm)
    {
	claspCharacter c = strm->stream.last_code[0];
	if (c == EOF) {
            c = ecl_peek_char(ECHO_STREAM_INPUT(strm));
	}
	return c;
    }

    static int
    echo_listen(T_sp strm)
    {
	return ecl_listen_stream(ECHO_STREAM_INPUT(strm));
    }

    static void
    echo_clear_input(T_sp strm)
    {
	ecl_clear_input(ECHO_STREAM_INPUT(strm));
    }

    static void
    echo_clear_output(T_sp strm)
    {
	ecl_clear_output(ECHO_STREAM_OUTPUT(strm));
    }

    static void
    echo_force_output(T_sp strm)
    {
	ecl_force_output(ECHO_STREAM_OUTPUT(strm));
    }

    static void
    echo_finish_output(T_sp strm)
    {
	ecl_finish_output(ECHO_STREAM_OUTPUT(strm));
    }

    static T_sp
    echo_element_type(T_sp strm)
    {
	return ecl_stream_element_type(ECHO_STREAM_INPUT(strm));
    }

    static int
    echo_column(T_sp strm)
    {
	return ecl_file_column(ECHO_STREAM_OUTPUT(strm));
    }

    static T_sp
    echo_close(T_sp strm)
    {
	if (strm->stream.flags & ECL_STREAM_CLOSE_COMPONENTS) {
            cl_close(1, ECHO_STREAM_INPUT(strm));
            cl_close(1, ECHO_STREAM_OUTPUT(strm));
	}
	return generic_close(strm);
    }

    const struct ecl_file_ops echo_ops = {
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

	not_a_file_stream, /* length */
	generic_always_nil, /* get_position */
	generic_set_position,
	echo_column,
	echo_close
    };

    T_sp
    cl_make_echo_stream(T_sp strm1, T_sp strm2)
    {
	T_sp strm;
	unlikely_if (!ecl_input_stream_p(strm1))
            not_an_input_stream(strm1);
	unlikely_if (!ecl_output_stream_p(strm2))
            not_an_output_stream(strm2);
	strm = alloc_stream();
	strm->stream.format = cl_stream_external_format(strm1);
	strm->stream.mode = (short)ecl_smm_echo;
	strm->stream.ops = duplicate_dispatch_table(&echo_ops);
	ECHO_STREAM_INPUT(strm) = strm1;
	ECHO_STREAM_OUTPUT(strm) = strm2;
	@(return strm)
            }

    T_sp
    cl_echo_stream_input_stream(T_sp strm)
    {
	unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_echo))
            FEwrong_type_only_arg(@[echo-stream-input-stream],
                                  strm, @[echo-stream]);
	@(return ECHO_STREAM_INPUT(strm))
            }

    T_sp
    cl_echo_stream_output_stream(T_sp strm)
    {
	unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_echo))
            FEwrong_type_only_arg(@[echo-stream-output-stream],
                                  strm, @[echo-stream]);
	@(return ECHO_STREAM_OUTPUT(strm))
            }

/**********************************************************************
 * CONCATENATED STREAM
 */

    static cl_index
    concatenated_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	T_sp l = CONCATENATED_STREAM_LIST(strm);
	cl_index out = 0;
	while (out < n && !Null(l)) {
            cl_index delta = ecl_read_byte8(ECL_CONS_CAR(l), c + out, n - out);
            out += delta;
            if (out == n) break;
            CONCATENATED_STREAM_LIST(strm) = l = ECL_CONS_CDR(l);
	}
	return out;
    }

    static T_sp
    concatenated_read_byte(T_sp strm)
    {
	T_sp l = CONCATENATED_STREAM_LIST(strm);
	T_sp c = ECL_NIL;
	while (!Null(l)) {
            c = ecl_read_byte(ECL_CONS_CAR(l));
            if (c != ECL_NIL) break;
            CONCATENATED_STREAM_LIST(strm) = l = ECL_CONS_CDR(l);
	}
	return c;
    }

    static claspCharacter
    concatenated_read_char(T_sp strm)
    {
	T_sp l = CONCATENATED_STREAM_LIST(strm);
	claspCharacter c = EOF;
	while (!Null(l)) {
            c = ecl_read_char(ECL_CONS_CAR(l));
            if (c != EOF) break;
            CONCATENATED_STREAM_LIST(strm) = l = ECL_CONS_CDR(l);
	}
	return c;
    }

    static void
    concatenated_unread_char(T_sp strm, claspCharacter c)
    {
	T_sp l = CONCATENATED_STREAM_LIST(strm);
	unlikely_if (Null(l))
            unread_error(strm);
	ecl_unread_char(c, ECL_CONS_CAR(l));
    }

    static int
    concatenated_listen(T_sp strm)
    {
	T_sp l = CONCATENATED_STREAM_LIST(strm);
	while (!Null(l)) {
            int f = ecl_listen_stream(ECL_CONS_CAR(l));
            l = ECL_CONS_CDR(l);
            if (f == ECL_LISTEN_EOF) {
                CONCATENATED_STREAM_LIST(strm) = l;
            } else {
                return f;
            }
	}
	return ECL_LISTEN_EOF;
    }

    static T_sp
    concatenated_close(T_sp strm)
    {
	if (strm->stream.flags & ECL_STREAM_CLOSE_COMPONENTS) {
            cl_mapc(2, @'close', CONCATENATED_STREAM_LIST(strm));
	}
	return generic_close(strm);
    }

    const struct ecl_file_ops concatenated_ops = {
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

	generic_always_true, /* input_p */
	generic_always_false, /* output_p */
	generic_always_false,
	broadcast_element_type,

	not_a_file_stream, /* length */
	generic_always_nil, /* get_position */
	generic_set_position,
	generic_column,
	concatenated_close
    };

    @(defun make_concatenated_stream (&rest ap)
      T_sp x, streams;
      int i;
      @
      streams = ECL_NIL;
      for (i = 0; i < narg; i++) {
          x = ecl_va_arg(ap);
          unlikely_if (!ecl_input_stream_p(x))
              not_an_input_stream(x);
          streams = CONS(x, streams);
      }
      x = alloc_stream();
      if (Null(streams)) {
          x->stream.format = @':pass-through';
      } else {
          x->stream.format = cl_stream_external_format(ECL_CONS_CAR(streams));
      }
      x->stream.mode = (short)ecl_smm_concatenated;
      x->stream.ops = duplicate_dispatch_table(&concatenated_ops);
      CONCATENATED_STREAM_LIST(x) = cl_nreverse(streams);
      @(return x)
      @)

    T_sp
    cl_concatenated_stream_streams(T_sp strm)
    {
	unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_concatenated))
            FEwrong_type_only_arg(@[concatenated-stream-streams],
                                  strm, @[concatenated-stream]);
	return cl_copy_list(CONCATENATED_STREAM_LIST(strm));
    }

/**********************************************************************
 * SYNONYM STREAM
 */

    static cl_index
    synonym_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	return ecl_read_byte8(SYNONYM_STREAM_STREAM(strm), c, n);
    }

    static cl_index
    synonym_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	return ecl_write_byte8(SYNONYM_STREAM_STREAM(strm), c, n);
    }

    static void
    synonym_write_byte(T_sp c, T_sp strm)
    {
	ecl_write_byte(c, SYNONYM_STREAM_STREAM(strm));
    }

    static T_sp
    synonym_read_byte(T_sp strm)
    {
	return ecl_read_byte(SYNONYM_STREAM_STREAM(strm));
    }

    static claspCharacter
    synonym_read_char(T_sp strm)
    {
	return ecl_read_char(SYNONYM_STREAM_STREAM(strm));
    }

    static claspCharacter
    synonym_write_char(T_sp strm, claspCharacter c)
    {
	return ecl_write_char(c, SYNONYM_STREAM_STREAM(strm));
    }

    static void
    synonym_unread_char(T_sp strm, claspCharacter c)
    {
	ecl_unread_char(c, SYNONYM_STREAM_STREAM(strm));
    }

    static claspCharacter
    synonym_peek_char(T_sp strm)
    {
	return ecl_peek_char(SYNONYM_STREAM_STREAM(strm));
    }

    static cl_index
    synonym_read_vector(T_sp strm, T_sp data, cl_index start, cl_index n)
    {
	strm = SYNONYM_STREAM_STREAM(strm);
	return stream_dispatch_table(strm)->read_vector(strm, data, start, n);
    }

    static cl_index
    synonym_write_vector(T_sp strm, T_sp data, cl_index start, cl_index n)
    {
	strm = SYNONYM_STREAM_STREAM(strm);
	return stream_dispatch_table(strm)->write_vector(strm, data, start, n);
    }

    static int
    synonym_listen(T_sp strm)
    {
	return ecl_listen_stream(SYNONYM_STREAM_STREAM(strm));
    }

    static void
    synonym_clear_input(T_sp strm)
    {
	ecl_clear_input(SYNONYM_STREAM_STREAM(strm));
    }

    static void
    synonym_clear_output(T_sp strm)
    {
	ecl_clear_output(SYNONYM_STREAM_STREAM(strm));
    }

    static void
    synonym_force_output(T_sp strm)
    {
	ecl_force_output(SYNONYM_STREAM_STREAM(strm));
    }

    static void
    synonym_finish_output(T_sp strm)
    {
	ecl_finish_output(SYNONYM_STREAM_STREAM(strm));
    }

    static int
    synonym_input_p(T_sp strm)
    {
	return ecl_input_stream_p(SYNONYM_STREAM_STREAM(strm));
    }

    static int
    synonym_output_p(T_sp strm)
    {
	return ecl_output_stream_p(SYNONYM_STREAM_STREAM(strm));
    }

    static int
    synonym_interactive_p(T_sp strm)
    {
	return ecl_interactive_stream_p(SYNONYM_STREAM_STREAM(strm));
    }

    static T_sp
    synonym_element_type(T_sp strm)
    {
	return ecl_stream_element_type(SYNONYM_STREAM_STREAM(strm));
    }

    static T_sp
    synonym_length(T_sp strm)
    {
	return ecl_file_length(SYNONYM_STREAM_STREAM(strm));
    }

    static T_sp
    synonym_get_position(T_sp strm)
    {
	return ecl_file_position(SYNONYM_STREAM_STREAM(strm));
    }

    static T_sp
    synonym_set_position(T_sp strm, T_sp pos)
    {
	return ecl_file_position_set(SYNONYM_STREAM_STREAM(strm), pos);
    }

    static int
    synonym_column(T_sp strm)
    {
	return ecl_file_column(SYNONYM_STREAM_STREAM(strm));
    }

    const struct ecl_file_ops synonym_ops = {
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
	generic_close
    };

    T_sp
    cl_make_synonym_stream(T_sp sym)
    {
	T_sp x;

	sym = ecl_check_cl_type(@'make-synonym-stream',sym,t_symbol);
	x = alloc_stream();
	x->stream.ops = duplicate_dispatch_table(&synonym_ops);
	x->stream.mode = (short)ecl_smm_synonym;
	SYNONYM_STREAM_SYMBOL(x) = sym;
	@(return x)
            }

    T_sp
    cl_synonym_stream_symbol(T_sp strm)
    {
	unlikely_if (!ECL_ANSI_STREAM_TYPE_P(strm, ecl_smm_synonym))
            FEwrong_type_only_arg(@[synonym-stream-symbol],
                                  strm, @[synonym-stream]);
	@(return SYNONYM_STREAM_SYMBOL(strm))
            }

/**********************************************************************
 * UNINTERRUPTED OPERATIONS
 */

#ifdef ECL_MS_WINDOWS_HOST
#define ecl_mode_t int
#else
#define ecl_mode_t mode_t
#endif

    static int
    safe_open(const char *filename, int flags, ecl_mode_t mode)
    {
	const cl_env_ptr the_env = ecl_process_env();
	int output;
	ecl_disable_interrupts_env(the_env);
	output = open(filename, flags, mode);
	ecl_enable_interrupts_env(the_env);
	return output;
    }

    static int
    safe_close(int f)
    {
	const cl_env_ptr the_env = ecl_process_env();
	int output;
	ecl_disable_interrupts_env(the_env);
	output = close(f);
	ecl_enable_interrupts_env(the_env);
	return output;
    }

    static FILE *
    safe_fopen(const char *filename, const char *mode)
    {
	const cl_env_ptr the_env = ecl_process_env();
	FILE *output;
	ecl_disable_interrupts_env(the_env);
	output = fopen(filename, mode);
	ecl_enable_interrupts_env(the_env);
	return output;
    }

    static FILE *
    safe_fdopen(int fildes, const char *mode)
    {
	const cl_env_ptr the_env = ecl_process_env();
	FILE *output;
	ecl_disable_interrupts_env(the_env);
	output = fdopen(fildes, mode);
	ecl_enable_interrupts_env(the_env);
	return output;
    }

    static int
    safe_fclose(FILE *stream)
    {
	const cl_env_ptr the_env = ecl_process_env();
	int output;
	ecl_disable_interrupts_env(the_env);
	output = fclose(stream);
	ecl_enable_interrupts_env(the_env);
	return output;
    }

/**********************************************************************
 * POSIX FILE STREAM
 */

    static cl_index
    consume_byte_stack(T_sp strm, unsigned char *c, cl_index n)
    {
	cl_index out = 0;
	while (n) {
            T_sp l = strm->stream.byte_stack;
            if (l == ECL_NIL)
                return out + strm->stream.ops->read_byte8(strm, c, n);
            *(c++) = ecl_fixnum(ECL_CONS_CAR(l));
            out++;
            n--;
            strm->stream.byte_stack = l = ECL_CONS_CDR(l);
	}
	return out;
    }

    static cl_index
    io_file_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	unlikely_if (strm->stream.byte_stack != ECL_NIL) {
            return consume_byte_stack(strm, c, n);
	} else {
            int f = IO_FILE_DESCRIPTOR(strm);
            cl_fixnum out = 0;
            ecl_disable_interrupts();
            do {
                out = read(f, c, sizeof(char)*n);
            } while (out < 0 && restartable_io_error(strm, "read"));
            ecl_enable_interrupts();
            return out;
	}
    }

    static cl_index
    output_file_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	int f = IO_FILE_DESCRIPTOR(strm);
	cl_fixnum out;
	ecl_disable_interrupts();
	do {
            out = write(f, c, sizeof(char)*n);
	} while (out < 0 && restartable_io_error(strm, "write"));
	ecl_enable_interrupts();
	return out;
    }

    static cl_index
    io_file_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	unlikely_if (strm->stream.byte_stack != ECL_NIL) {
            /* Try to move to the beginning of the unread characters */
            T_sp aux = ecl_file_position(strm);
            if (!Null(aux))
                ecl_file_position_set(strm, aux);
            strm->stream.byte_stack = ECL_NIL;
	}
	return output_file_write_byte8(strm, c, n);
    }

    static int
    io_file_listen(T_sp strm)
    {
	if (strm->stream.byte_stack != ECL_NIL)
            return ECL_LISTEN_AVAILABLE;
	if (strm->stream.flags & ECL_STREAM_MIGHT_SEEK) {
            cl_env_ptr the_env = ecl_process_env();
            int f = IO_FILE_DESCRIPTOR(strm);
            ecl_off_t disp, new;
            ecl_disable_interrupts_env(the_env);
            disp = lseek(f, 0, SEEK_CUR);
            ecl_enable_interrupts_env(the_env);
            if (disp != (ecl_off_t)-1) {
                ecl_disable_interrupts_env(the_env);
                new = lseek(f, 0, SEEK_END);
                ecl_enable_interrupts_env(the_env);
                lseek(f, disp, SEEK_SET);
                if (new == disp) {
                    return ECL_LISTEN_NO_CHAR;
                } else if (new != (ecl_off_t)-1) {
                    return ECL_LISTEN_AVAILABLE;
                }
            }
	}
	return file_listen(strm, IO_FILE_DESCRIPTOR(strm));
    }

#if defined(ECL_MS_WINDOWS_HOST)
    static int
    isaconsole(int i)
    {
	HANDLE h = (HANDLE)_get_osfhandle(i);
	DWORD mode;
	return !!GetConsoleMode(h, &mode);
    }
#define isatty isaconsole
#endif

    static void
    io_file_clear_input(T_sp strm)
    {
	int f = IO_FILE_DESCRIPTOR(strm);
#if defined(ECL_MS_WINDOWS_HOST)
	if (isatty(f)) {
            /* Flushes Win32 console */
            if (!FlushConsoleInputBuffer((HANDLE)_get_osfhandle(f)))
                FEwin32_error("FlushConsoleInputBuffer() failed", 0);
            /* Do not stop here: the FILE structure needs also to be flushed */
	}
#endif
	while (file_listen(strm, f) == ECL_LISTEN_AVAILABLE) {
            claspCharacter c = eformat_read_char(strm);
            if (c == EOF) return;
	}
    }

#define io_file_clear_output generic_void
#define io_file_force_output generic_void
#define io_file_finish_output io_file_force_output

    static int
    io_file_interactive_p(T_sp strm)
    {
	int f = IO_FILE_DESCRIPTOR(strm);
	return isatty(f);
    }

    static T_sp
    io_file_element_type(T_sp strm)
    {
	return IO_FILE_ELT_TYPE(strm);
    }

    static T_sp
    io_file_length(T_sp strm)
    {
	int f = IO_FILE_DESCRIPTOR(strm);
	T_sp output = ecl_file_len(f);
	if (strm->stream.byte_size != 8) {
            const cl_env_ptr the_env = ecl_process_env();
            cl_index bs = strm->stream.byte_size;
            output = ecl_floor2(output, ecl_make_fixnum(bs/8));
            unlikely_if (ecl_nth_value(the_env, 1) != ecl_make_fixnum(0)) {
                FEerror("File length is not on byte boundary", 0);
            }
	}
	return output;
    }

    static T_sp
    io_file_get_position(T_sp strm)
    {
	int f = IO_FILE_DESCRIPTOR(strm);
	T_sp output;
	ecl_off_t offset;

	ecl_disable_interrupts();
	offset = lseek(f, 0, SEEK_CUR);
	ecl_enable_interrupts();
	unlikely_if (offset < 0)
            io_error(strm);
	if (sizeof(ecl_off_t) == sizeof(long)) {
            output = ecl_make_integer(offset);
	} else {
            output = ecl_off_t_to_integer(offset);
	}
	{
            /* If there are unread octets, we return the position at which
             * these bytes begin! */
            T_sp l = strm->stream.byte_stack;
            while (CONSP(l)) {
                output = ecl_one_minus(output);
                l = ECL_CONS_CDR(l);
            }
	}
	if (strm->stream.byte_size != 8) {
            output = ecl_floor2(output, ecl_make_fixnum(strm->stream.byte_size / 8));
	}
	return output;
    }

    static T_sp
    io_file_set_position(T_sp strm, T_sp large_disp)
    {
	int f = IO_FILE_DESCRIPTOR(strm);
	ecl_off_t disp;
	int mode;
	if (Null(large_disp)) {
            disp = 0;
            mode = SEEK_END;
	} else {
            if (strm->stream.byte_size != 8) {
                large_disp = ecl_times(large_disp,
                                       ecl_make_fixnum(strm->stream.byte_size / 8));
            }
            disp = ecl_integer_to_off_t(large_disp);
            mode = SEEK_SET;
	}
	disp = lseek(f, disp, mode);
	return (disp == (ecl_off_t)-1)? ECL_NIL : ECL_T;
    }

    static int
    io_file_column(T_sp strm)
    {
	return strm->stream.column;
    }

    static T_sp
    io_file_close(T_sp strm)
    {
	int f = IO_FILE_DESCRIPTOR(strm);
	int failed;
	unlikely_if (f == STDOUT_FILENO)
            FEerror("Cannot close the standard output", 0);
	unlikely_if (f == STDIN_FILENO)
            FEerror("Cannot close the standard input", 0);
	failed = safe_close(f);
	unlikely_if (failed < 0)
            cannot_close(strm);
	IO_FILE_DESCRIPTOR(strm) = -1;
	return generic_close(strm);
    }

    static cl_index
    io_file_read_vector(T_sp strm, T_sp data, cl_index start, cl_index end)
    {
	cl_elttype t = ecl_array_elttype(data);
	if (start >= end)
            return start;
	if (t == ecl_aet_b8 || t == ecl_aet_i8) {
            if (strm->stream.byte_size == 8) {
                void *aux = data->vector.self.bc + start;
                return start + strm->stream.ops->read_byte8(strm, aux, end-start);
            }
	} else if (t == ecl_aet_fix || t == ecl_aet_index) {
            if (strm->stream.byte_size == sizeof(cl_fixnum)*8) {
                void *aux = data->vector.self.fix + start;
                cl_index bytes = (end - start) * sizeof(cl_fixnum);
                bytes = strm->stream.ops->read_byte8(strm, aux, bytes);
                return start + bytes / sizeof(cl_fixnum);
            }
	}
	return generic_read_vector(strm, data, start, end);
    }

    static cl_index
    io_file_write_vector(T_sp strm, T_sp data, cl_index start, cl_index end)
    {
	cl_elttype t = ecl_array_elttype(data);
	if (start >= end)
            return start;
	if (t == ecl_aet_b8 || t == ecl_aet_i8) {
            if (strm->stream.byte_size == 8) {
                void *aux = data->vector.self.bc + start;
                return strm->stream.ops->write_byte8(strm, aux, end-start);
            }
	} else if (t == ecl_aet_fix || t == ecl_aet_index) {
            if (strm->stream.byte_size == sizeof(cl_fixnum)*8) {
                void *aux = data->vector.self.fix + start;
                cl_index bytes = (end - start) * sizeof(cl_fixnum);
                bytes = strm->stream.ops->write_byte8(strm, aux, bytes);
                return start + bytes / sizeof(cl_fixnum);
            }
	}
	return generic_write_vector(strm, data, start, end);
    }

    const struct ecl_file_ops io_file_ops = {
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
	io_file_close
    };

    const struct ecl_file_ops output_file_ops = {
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
	generic_always_true, /* output_p */
	generic_always_false,
	io_file_element_type,

	io_file_length,
	io_file_get_position,
	io_file_set_position,
	io_file_column,
	io_file_close
    };

    const struct ecl_file_ops input_file_ops = {
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

	generic_always_true, /* input_p */
	generic_always_false, /* output_p */
	io_file_interactive_p,
	io_file_element_type,

	io_file_length,
	io_file_get_position,
	io_file_set_position,
	generic_column,
	io_file_close
    };


    static int
    parse_external_format(T_sp stream, T_sp format, int flags)
    {
        if (format == @':default') {
            format = ecl_symbol_value(@'ext::*default-external-format*');
        }
	if (CONSP(format)) {
            flags = parse_external_format(stream, ECL_CONS_CDR(format), flags);
            format = ECL_CONS_CAR(format);
	}
        if (format == ECL_T) {
#ifdef ECL_UNICODE
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UTF_8;
#else
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_DEFAULT_FORMAT;
#endif
        }
	if (format == ECL_NIL) {
            return flags;
	}
	if (format == @':CR') {
            return (flags | ECL_STREAM_CR) & ~ECL_STREAM_LF;
	}
	if (format == @':LF') {
            return (flags | ECL_STREAM_LF) & ~ECL_STREAM_CR;
	}
	if (format == @':CRLF') {
            return flags | (ECL_STREAM_CR+ECL_STREAM_LF);
	}
	if (format == @':LITTLE-ENDIAN') {
            return flags | ECL_STREAM_LITTLE_ENDIAN;
	}
	if (format == @':BIG-ENDIAN') {
            return flags & ~ECL_STREAM_LITTLE_ENDIAN;
	}
	if (format == @':pass-through') {
#ifdef ECL_UNICODE
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_LATIN_1;
#else
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_DEFAULT_FORMAT;
#endif
	}
#ifdef ECL_UNICODE
    PARSE_SYMBOLS:
	if (format == @':UTF-8') {
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UTF_8; 
	}
	if (format == @':UCS-2') {
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UCS_2;
	}
	if (format == @':UCS-2BE') {
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UCS_2BE;
	}
	if (format == @':UCS-2LE') {
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UCS_2LE;
	}
	if (format == @':UCS-4') {
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UCS_4;
	}
	if (format == @':UCS-4BE') {
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UCS_4BE;
	}
	if (format == @':UCS-4LE') {
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_UCS_4LE;
	}
	if (format == @':ISO-8859-1') {
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_ISO_8859_1;
	}
	if (format == @':LATIN-1') {
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_LATIN_1;
	}
	if (format == @':US-ASCII') {
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_US_ASCII; 
	}
	if (ECL_HASH_TABLE_P(format)) {
            stream->stream.format_table = format;
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_USER_FORMAT;
	}
	if (ECL_SYMBOLP(format)) {
            format = si_make_encoding(format);
            if (ECL_SYMBOLP(format))
                goto PARSE_SYMBOLS;
            stream->stream.format_table = format;
            return (flags & ~ECL_STREAM_FORMAT) | ECL_STREAM_USER_FORMAT;
	}
#endif
	FEerror("Unknown or unsupported external format: ~A", 1, format.asTPtr());
	return ECL_STREAM_DEFAULT_FORMAT;
    }

    static void
    set_stream_elt_type(T_sp stream, cl_fixnum byte_size, int flags,
                        T_sp external_format)
    {
	T_sp t;
	if (byte_size < 0) {
            byte_size = -byte_size;
            flags |= ECL_STREAM_SIGNED_BYTES;
            t = @'signed-byte';
	} else {
            flags &= ~ECL_STREAM_SIGNED_BYTES;
            t = @'unsigned-byte';
	}
	flags = parse_external_format(stream, external_format, flags);
	stream->stream.ops->read_char = eformat_read_char;
	stream->stream.ops->write_char = eformat_write_char;
	switch (flags & ECL_STREAM_FORMAT) {
	case ECL_STREAM_BINARY:
            IO_STREAM_ELT_TYPE(stream) = cl_list(2, t, ecl_make_fixnum(byte_size));
            stream->stream.format = t;
            stream->stream.ops->read_char = not_character_read_char;
            stream->stream.ops->write_char = not_character_write_char;
            break;
#ifdef ECL_UNICODE
            /*case ECL_ISO_8859_1:*/
	case ECL_STREAM_LATIN_1:
            IO_STREAM_ELT_TYPE(stream) = @'base-char';
            byte_size = 8;
            stream->stream.format = @':latin-1';
            stream->stream.encoder = passthrough_encoder;
            stream->stream.decoder = passthrough_decoder;
            break;
	case ECL_STREAM_UTF_8:
            IO_STREAM_ELT_TYPE(stream) = @'character';
            byte_size = 8;
            stream->stream.format = @':utf-8';
            stream->stream.encoder = utf_8_encoder;
            stream->stream.decoder = utf_8_decoder;
            break;
	case ECL_STREAM_UCS_2:
            IO_STREAM_ELT_TYPE(stream) = @'character';
            byte_size = 8*2;
            stream->stream.format = @':ucs-2';
            stream->stream.encoder = ucs_2_encoder;
            stream->stream.decoder = ucs_2_decoder;
            break;
	case ECL_STREAM_UCS_2BE:
            IO_STREAM_ELT_TYPE(stream) = @'character';
            byte_size = 8*2;
            if (flags & ECL_STREAM_LITTLE_ENDIAN) {
                stream->stream.format = @':ucs-2le';
                stream->stream.encoder = ucs_2le_encoder;
                stream->stream.decoder = ucs_2le_decoder;
            } else {
                stream->stream.format = @':ucs-2be';
                stream->stream.encoder = ucs_2be_encoder;
                stream->stream.decoder = ucs_2be_decoder;
            }
            break;
	case ECL_STREAM_UCS_4:
            IO_STREAM_ELT_TYPE(stream) = @'character';
            byte_size = 8*4;
            stream->stream.format = @':ucs-4be';
            stream->stream.encoder = ucs_4_encoder;
            stream->stream.decoder = ucs_4_decoder;
            break;
	case ECL_STREAM_UCS_4BE:
            IO_STREAM_ELT_TYPE(stream) = @'character';
            byte_size = 8*4;
            if (flags & ECL_STREAM_LITTLE_ENDIAN) {
                stream->stream.format = @':ucs-4le';
                stream->stream.encoder = ucs_4le_encoder;
                stream->stream.decoder = ucs_4le_decoder;
            } else {
                stream->stream.format = @':ucs-4be';
                stream->stream.encoder = ucs_4be_encoder;
                stream->stream.decoder = ucs_4be_decoder;
            }
            break;
	case ECL_STREAM_USER_FORMAT:
            IO_STREAM_ELT_TYPE(stream) = @'character';
            byte_size = 8;
            stream->stream.format = stream->stream.format_table;
            if (CONSP(stream->stream.format)) {
                stream->stream.encoder = user_multistate_encoder;
                stream->stream.decoder = user_multistate_decoder;
            } else {
                stream->stream.encoder = user_encoder;
                stream->stream.decoder = user_decoder;
            }
            break;
	case ECL_STREAM_US_ASCII:
            IO_STREAM_ELT_TYPE(stream) = @'base-char';
            byte_size = 8;
            stream->stream.format = @':us-ascii';
            stream->stream.encoder = ascii_encoder;
            stream->stream.decoder = ascii_decoder;
            break;
#else
	case ECL_STREAM_DEFAULT_FORMAT:
            IO_STREAM_ELT_TYPE(stream) = @'base-char';
            byte_size = 8;
            stream->stream.format = @':pass-through';
            stream->stream.encoder = passthrough_encoder;
            stream->stream.decoder = passthrough_decoder;
            break;
#endif
	default:
            FEerror("Invalid or unsupported external format ~A with code ~D",
                    2, external_format, ecl_make_fixnum(flags).asTPtr());
 	}
	t = @':LF';
	if (stream->stream.ops->write_char == eformat_write_char &&
	    (flags & ECL_STREAM_CR)) {
            if (flags & ECL_STREAM_LF) {
                stream->stream.ops->read_char = eformat_read_char_crlf;
                stream->stream.ops->write_char = eformat_write_char_crlf;
                t = @':CRLF';
            } else {
                stream->stream.ops->read_char = eformat_read_char_cr;
                stream->stream.ops->write_char = eformat_write_char_cr;
                t = @':CR';
            }
	}
	stream->stream.format = cl_list(2, stream->stream.format, t);
	{
            T_sp (*read_byte)(T_sp);
            void (*write_byte)(T_sp,T_sp);
            byte_size = (byte_size+7)&(~(cl_fixnum)7);
            if (byte_size == 8) {
                if (flags & ECL_STREAM_SIGNED_BYTES) {
                    read_byte = generic_read_byte_signed8;
                    write_byte = generic_write_byte_signed8;
                } else {
                    read_byte = generic_read_byte_unsigned8;
                    write_byte = generic_write_byte_unsigned8;
                }
            } else if (flags & ECL_STREAM_LITTLE_ENDIAN) {
                read_byte = generic_read_byte_le;
                write_byte = generic_write_byte_le;
            } else {
                read_byte = generic_read_byte;
                write_byte = generic_write_byte;
            }
            if (ecl_input_stream_p(stream)) {
                stream->stream.ops->read_byte = read_byte;
            }
            if (ecl_output_stream_p(stream)) {
                stream->stream.ops->write_byte = write_byte;
            }
	}
	stream->stream.flags = flags;
	stream->stream.byte_size = byte_size;
    }

    T_sp
    si_stream_external_format_set(T_sp stream, T_sp format)
    {
#ifdef ECL_CLOS_STREAMS
        unlikely_if (ECL_INSTANCEP(stream)) {
            FEerror("Cannot change external format of stream ~A", 1, stream.asTPtr());
        }
#endif
        switch (stream->stream.mode) {
        case ecl_smm_input:
        case ecl_smm_input_file:
        case ecl_smm_output:
        case ecl_smm_output_file:
        case ecl_smm_io:
        case ecl_smm_io_file:
#ifdef ECL_WSOCK
        case ecl_smm_input_wsock:
        case ecl_smm_output_wsock:
        case ecl_smm_io_wsock:
	case ecl_smm_io_wcon:
#endif
        {
            T_sp elt_type = ecl_stream_element_type(stream);
            unlikely_if (elt_type != @'character' &&
                         elt_type != @'base-char')
                FEerror("Cannot change external format"
                        "of binary stream ~A", 1, stream.asTPtr());
            set_stream_elt_type(stream, stream->stream.byte_size,
                                stream->stream.flags, format);
        }
        break;
        default:
            FEerror("Cannot change external format of stream ~A", 1, stream.asTPtr());
        }
        @(return)
            }

    T_sp
    ecl_make_file_stream_from_fd(T_sp fname, int fd, enum ecl_smmode smm,
                                 cl_fixnum byte_size, int flags, T_sp external_format)
    {
	T_sp stream = alloc_stream();
	switch(smm) {
	case ecl_smm_input:
            smm = ecl_smm_input_file;
	case ecl_smm_input_file:
	case ecl_smm_probe:
            stream->stream.ops = duplicate_dispatch_table(&input_file_ops);
            break;
	case ecl_smm_output:
            smm = ecl_smm_output_file;
	case ecl_smm_output_file:
            stream->stream.ops = duplicate_dispatch_table(&output_file_ops);
            break;
	case ecl_smm_io:
            smm = ecl_smm_io_file;
	case ecl_smm_io_file:
            stream->stream.ops = duplicate_dispatch_table(&io_file_ops);
            break;
	default:
            FEerror("make_stream: wrong mode", 0);
	}
	stream->stream.mode = (short)smm;
	stream->stream.closed = 0;
	set_stream_elt_type(stream, byte_size, flags, external_format);
	IO_FILE_FILENAME(stream) = fname; /* not really used */
	stream->stream.column = 0;
	IO_FILE_DESCRIPTOR(stream) = fd;
	stream->stream.last_op = 0;
	si_set_finalizer(stream, ECL_T);
	return stream;
    }

/**********************************************************************
 * C STREAMS
 */

    static cl_index
    input_stream_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	unlikely_if (strm->stream.byte_stack != ECL_NIL) {
            return consume_byte_stack(strm, c, n);
	} else {
            FILE *f = IO_STREAM_FILE(strm);
            cl_fixnum out = 0;
            ecl_disable_interrupts();
            do {
                out = fread(c, sizeof(char), n, f);
            } while (out < n && ferror(f) && restartable_io_error(strm, "fread"));
            ecl_enable_interrupts();
            return out;
	}
    }

    static cl_index
    output_stream_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	cl_index out;
	ecl_disable_interrupts();
	do {
            out = fwrite(c, sizeof(char), n, IO_STREAM_FILE(strm));
	} while (out < n && restartable_io_error(strm, "fwrite"));
	ecl_enable_interrupts();
	return out;
    }

    static cl_index
    io_stream_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	/* When using the same stream for input and output operations, we have to
	 * use some file position operation before reading again. Besides this, if
	 * there were unread octets, we have to move to the position at the
	 * begining of them.
	 */
	if (strm->stream.byte_stack != ECL_NIL) {
            T_sp aux = ecl_file_position(strm);
            if (!Null(aux))
                ecl_file_position_set(strm, aux);
	} else if (strm->stream.last_op > 0) {
            ecl_fseeko(IO_STREAM_FILE(strm), 0, SEEK_CUR);
	}
	strm->stream.last_op = -1;
	return output_stream_write_byte8(strm, c, n);
    }

    static void io_stream_force_output(T_sp strm);

    static cl_index
    io_stream_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	/* When using the same stream for input and output operations, we have to
	 * flush the stream before reading.
	 */
	if (strm->stream.last_op < 0) {
            io_stream_force_output(strm);
	}
	strm->stream.last_op = +1;
	return input_stream_read_byte8(strm, c, n);
    }

    static int
    io_stream_listen(T_sp strm)
    {
	if (strm->stream.byte_stack != ECL_NIL)
            return ECL_LISTEN_AVAILABLE;
	return flisten(strm, IO_STREAM_FILE(strm));
    }

    static void
    io_stream_clear_input(T_sp strm)
    {
	FILE *fp = IO_STREAM_FILE(strm);
#if defined(ECL_MS_WINDOWS_HOST)
        int f = fileno(fp);
	if (isatty(f)) {
            /* Flushes Win32 console */
            unlikely_if (!FlushConsoleInputBuffer((HANDLE)_get_osfhandle(f)))
                FEwin32_error("FlushConsoleInputBuffer() failed", 0);
            /* Do not stop here: the FILE structure needs also to be flushed */
	}
#endif
	while (flisten(strm, fp) == ECL_LISTEN_AVAILABLE) {
            ecl_disable_interrupts();
            getc(fp);
            ecl_enable_interrupts();
	}
    }

#define io_stream_clear_output generic_void

    static void
    io_stream_force_output(T_sp strm)
    {
	FILE *f = IO_STREAM_FILE(strm);
	ecl_disable_interrupts();
	while ((fflush(f) == EOF) && restartable_io_error(strm, "fflush"))
            (void)0;
	ecl_enable_interrupts();
    }

#define io_stream_finish_output io_stream_force_output

    static int
    io_stream_interactive_p(T_sp strm)
    {
	FILE *f = IO_STREAM_FILE(strm);
	return isatty(fileno(f));
    }

    static T_sp
    io_stream_length(T_sp strm)
    {
	FILE *f = IO_STREAM_FILE(strm);
	T_sp output = ecl_file_len(fileno(f));
	if (strm->stream.byte_size != 8) {
            const cl_env_ptr the_env = ecl_process_env();
            cl_index bs = strm->stream.byte_size;
            output = ecl_floor2(output, ecl_make_fixnum(bs/8));
            unlikely_if (ecl_nth_value(the_env, 1) != ecl_make_fixnum(0)) {
                FEerror("File length is not on byte boundary", 0);
            }
	}
	return output;
    }

    static T_sp
    io_stream_get_position(T_sp strm)
    {
	FILE *f = IO_STREAM_FILE(strm);
	T_sp output;
	ecl_off_t offset;

	ecl_disable_interrupts();
	offset = ecl_ftello(f);
	ecl_enable_interrupts();
	if (offset < 0)
            io_error(strm);
	if (sizeof(ecl_off_t) == sizeof(long)) {
            output = ecl_make_integer(offset);
	} else {
            output = ecl_off_t_to_integer(offset);
	}
	{
            /* If there are unread octets, we return the position at which
             * these bytes begin! */
            T_sp l = strm->stream.byte_stack;
            while (CONSP(l)) {
                output = ecl_one_minus(output);
                l = ECL_CONS_CDR(l);
            }
	}
	if (strm->stream.byte_size != 8) {
            output = ecl_floor2(output, ecl_make_fixnum(strm->stream.byte_size / 8));
	}
	return output;
    }

    static T_sp
    io_stream_set_position(T_sp strm, T_sp large_disp)
    {
	FILE *f = IO_STREAM_FILE(strm);
	ecl_off_t disp;
	int mode;
	if (Null(large_disp)) {
            disp = 0;
            mode = SEEK_END;
	} else {
            if (strm->stream.byte_size != 8) {
                large_disp = ecl_times(large_disp,
                                       ecl_make_fixnum(strm->stream.byte_size / 8));
            }
            disp = ecl_integer_to_off_t(large_disp);
            mode = SEEK_SET;
	}
	ecl_disable_interrupts();
	mode = ecl_fseeko(f, disp, mode);
	ecl_enable_interrupts();
	return mode? ECL_NIL : ECL_T;
    }

    static int
    io_stream_column(T_sp strm)
    {
	return strm->stream.column;
    }

    static T_sp
    io_stream_close(T_sp strm)
    {
	FILE *f = IO_STREAM_FILE(strm);
	int failed;
	unlikely_if (f == stdout)
            FEerror("Cannot close the standard output", 0);
	unlikely_if (f == stdin)
            FEerror("Cannot close the standard input", 0);
	unlikely_if (f == NULL)
            wrong_file_handler(strm);
	if (ecl_output_stream_p(strm)) {
            ecl_force_output(strm);
	}
	failed = safe_fclose(f);
	unlikely_if (failed)
            cannot_close(strm);
#if !defined(GBC_BOEHM)
	ecl_dealloc(strm->stream.buffer);
	IO_STREAM_FILE(strm) = NULL;
#endif
	return generic_close(strm);
    }

/*
 * Specialized sequence operations
 */

#define io_stream_read_vector io_file_read_vector
#define io_stream_write_vector io_file_write_vector

    const struct ecl_file_ops io_stream_ops = {
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
	io_stream_close
    };

    const struct ecl_file_ops output_stream_ops = {
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
	generic_always_true, /* output_p */
	generic_always_false,
	io_file_element_type,

	io_stream_length,
	io_stream_get_position,
	io_stream_set_position,
	io_stream_column,
	io_stream_close
    };

    const struct ecl_file_ops input_stream_ops = {
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

	generic_always_true, /* input_p */
	generic_always_false, /* output_p */
	io_stream_interactive_p,
	io_file_element_type,

	io_stream_length,
	io_stream_get_position,
	io_stream_set_position,
	generic_column,
	io_stream_close
    };

/**********************************************************************
 * WINSOCK STREAMS  
 */

#if defined(ECL_WSOCK)

#define winsock_stream_element_type io_file_element_type

    static cl_index
    winsock_stream_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	cl_index len = 0;

	unlikely_if (strm->stream.byte_stack != ECL_NIL) {
            return consume_byte_stack(strm, c, n);
	}
	if(n > 0) {
            SOCKET s = (SOCKET)IO_FILE_DESCRIPTOR(strm);
            unlikely_if (INVALID_SOCKET == s) {
                wrong_file_handler(strm);
            } else {
                ecl_disable_interrupts();
                len = recv(s, c, n, 0);
                unlikely_if (len == SOCKET_ERROR)
                    wsock_error("Cannot read bytes from Windows "
                                "socket ~S.~%~A", strm);
                ecl_enable_interrupts();
            }
	}
	return (len > 0) ? len : EOF;
    }

    static cl_index
    winsock_stream_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	cl_index out = 0;
	unsigned char *endp;
	unsigned char *p;
	SOCKET s = (SOCKET)IO_FILE_DESCRIPTOR(strm);
	unlikely_if (INVALID_SOCKET == s) {
            wrong_file_handler(strm);
	} else {
            ecl_disable_interrupts();
            do {
                cl_index res = send(s, c + out, n, 0);
                unlikely_if (res == SOCKET_ERROR) {
                    wsock_error("Cannot write bytes to Windows"
                                " socket ~S.~%~A", strm);
                    break; /* stop writing */
                } else {			
                    out += res;
                    n -= res;
                }
            } while (n > 0);
            ecl_enable_interrupts();
	}
	return out;
    }

    static int
    winsock_stream_listen(T_sp strm) 
    {
	SOCKET s;
	unlikely_if (strm->stream.byte_stack != ECL_NIL) {
            return ECL_LISTEN_AVAILABLE;
	}
	s = (SOCKET)IO_FILE_DESCRIPTOR(strm);
	unlikely_if (INVALID_SOCKET == s) {
            wrong_file_handler(strm);
	}
	{
            struct timeval tv = { 0, 0 };
            fd_set fds;
            cl_index result;
			
            FD_ZERO( &fds );
            FD_SET(s, &fds);
            ecl_disable_interrupts();
            result = select( 0, &fds, NULL, NULL,  &tv );
            unlikely_if (result == SOCKET_ERROR)
                wsock_error("Cannot listen on Windows "
                            "socket ~S.~%~A", strm );
            ecl_enable_interrupts();
            return ( result > 0 
                     ? ECL_LISTEN_AVAILABLE 
                     : ECL_LISTEN_NO_CHAR );
	}
    }

    static void
    winsock_stream_clear_input(T_sp strm)
    {
	while (winsock_stream_listen(strm) == ECL_LISTEN_AVAILABLE) {
            eformat_read_char(strm);
	}
    }

    static T_sp
    winsock_stream_close(T_sp strm)
    {
	SOCKET s = (SOCKET) IO_FILE_DESCRIPTOR(strm);
	int failed;
	ecl_disable_interrupts();
	failed = closesocket(s);
	ecl_enable_interrupts();
	unlikely_if (failed < 0)
            cannot_close(strm);
	IO_FILE_DESCRIPTOR(strm) = (int)INVALID_SOCKET;
	return generic_close(strm);
    }

    const struct ecl_file_ops winsock_stream_io_ops = {
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

	winsock_stream_close
    };

    const struct ecl_file_ops winsock_stream_output_ops = {
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
	generic_always_true, /* output_p */
	generic_always_false,
	winsock_stream_element_type,

	not_a_file_stream,
	not_implemented_get_position,
	not_implemented_set_position,
	generic_column,

	winsock_stream_close
    };

    const struct ecl_file_ops winsock_stream_input_ops = {
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

	generic_always_true, /* input_p */
	generic_always_false, /* output_p */
	generic_always_false,
	winsock_stream_element_type,

	not_a_file_stream,
	not_implemented_get_position,
	not_implemented_set_position,
	generic_column,

	winsock_stream_close
    };
#endif

/**********************************************************************
 * WINCONSOLE STREAM
 */

#if defined(ECL_MS_WINDOWS_HOST)

#define wcon_stream_element_type io_file_element_type

    static cl_index
    wcon_stream_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	unlikely_if (strm->stream.byte_stack != ECL_NIL) {
            return consume_byte_stack(strm, c, n);
	} else {
            cl_index len = 0;
            cl_env_ptr the_env = ecl_process_env();
            HANDLE h = (HANDLE)IO_FILE_DESCRIPTOR(strm);
            DWORD nchars;
            unsigned char aux[4];
            for (len = 0; len < n; ) {
                int i, ok;
                ecl_disable_interrupts_env(the_env);
                ok = ReadConsole(h, &aux, 1, &nchars, NULL);
                ecl_enable_interrupts_env(the_env);
                unlikely_if (!ok) {
                    FEwin32_error("Cannot read from console", 0);
                }
                for (i = 0; i < nchars; i++) {
                    if (len < n) {
                        c[len++] = aux[i];
                    } else {
                        strm->stream.byte_stack =
                            ecl_nconc(strm->stream.byte_stack,
                                      ecl_list1(ecl_make_fixnum(aux[i])));
                    }
                }
            }
            return (len > 0) ? len : EOF;
	}
    }

    static cl_index
    wcon_stream_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	HANDLE h = (HANDLE)IO_FILE_DESCRIPTOR(strm);
	DWORD nchars;
	unlikely_if(!WriteConsole(h, c, n, &nchars, NULL)) {
            FEwin32_error("Cannot write to console.", 0);
	}
	return nchars;
    }

    static int
    wcon_stream_listen(T_sp strm) 
    {
	HANDLE h = (HANDLE)IO_FILE_DESCRIPTOR(strm);
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
    wcon_stream_clear_input(T_sp strm)
    {
	FlushConsoleInputBuffer((HANDLE)IO_FILE_DESCRIPTOR(strm));
    }

    static void
    wcon_stream_force_output(T_sp strm)
    {
	DWORD nchars;
	WriteConsole((HANDLE)IO_FILE_DESCRIPTOR(strm), 0, 0, &nchars, NULL);
    }

    const struct ecl_file_ops wcon_stream_io_ops = {
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
    maybe_make_windows_console_FILE(T_sp fname, FILE *f, enum ecl_smmode smm,
                                    cl_fixnum byte_size, int flags,
                                    T_sp external_format)
    {
	int desc = fileno(f);
	T_sp output;
	if (isatty(desc)) {
            output = ecl_make_stream_from_FILE
                (fname,
                 (void*)_get_osfhandle(desc),
                 ecl_smm_io_wcon,
                 byte_size, flags,
                 external_format);
            output->stream.eof_char = CONTROL_Z;
	} else {
            output = ecl_make_stream_from_FILE
                (fname, f, smm, byte_size, flags,
                 external_format);
	}
	return output;
    }

    static T_sp
    maybe_make_windows_console_fd(T_sp fname, int desc, enum ecl_smmode smm,
                                  cl_fixnum byte_size, int flags,
                                  T_sp external_format)
    {
	T_sp output;
	if (isatty(desc)) {
            output = ecl_make_stream_from_FILE
                (fname,
                 (void*)_get_osfhandle(desc),
                 ecl_smm_io_wcon,
                 byte_size, flags,
                 external_format);
            output->stream.eof_char = CONTROL_Z;
	} else {
            /* Windows changes the newline characters for \r\n
             * even when using read()/write() */
            if (ecl_option_values[ECL_OPT_USE_SETMODE_ON_FILES]) {
                _setmode(desc, _O_BINARY);
            } else {
                external_format = ECL_CONS_CDR(external_format);
            }
            output = ecl_make_file_stream_from_fd
                (fname, desc, smm,
                 byte_size, flags,
                 external_format);
	}
	return output;
    }
#else
#define maybe_make_windows_console_FILE ecl_make_stream_from_FILE
#define maybe_make_windows_console_fd ecl_make_file_stream_from_fd
#endif

    T_sp
    si_set_buffering_mode(T_sp stream, T_sp buffer_mode_symbol)
    {
	enum ecl_smmode mode = stream->stream.mode;
	int buffer_mode;

	unlikely_if (!ECL_ANSI_STREAM_P(stream)) {
            FEerror("Cannot set buffer of ~A", 1, stream.asTPtr());
	}

	if (buffer_mode_symbol == @':none' || Null(buffer_mode_symbol))
            buffer_mode = _IONBF;
	else if (buffer_mode_symbol == @':line' || buffer_mode_symbol == @':line-buffered')
            buffer_mode = _IOLBF;
	else if (buffer_mode_symbol == @':full' || buffer_mode_symbol == @':fully-buffered')
            buffer_mode = _IOFBF;
	else
            FEerror("Not a valid buffering mode: ~A", 1, buffer_mode_symbol.asTPtr());

	if (mode == ecl_smm_output || mode == ecl_smm_io || mode == ecl_smm_input) {
            FILE *fp = IO_STREAM_FILE(stream);

            if (buffer_mode != _IONBF) {
                cl_index buffer_size = BUFSIZ;
                char *new_buffer = ecl_alloc_atomic(buffer_size);
                stream->stream.buffer = new_buffer;
                setvbuf(fp, new_buffer, buffer_mode, buffer_size);
            } else
                setvbuf(fp, NULL, _IONBF, 0);
	}
	@(return stream)
            }

    T_sp
    ecl_make_stream_from_FILE(T_sp fname, void *f, enum ecl_smmode smm,
                              cl_fixnum byte_size, int flags, T_sp external_format)
    {
	T_sp stream;
	stream = alloc_stream();
	stream->stream.mode = (short)smm;
	stream->stream.closed = 0;
	switch (smm) {
	case ecl_smm_io:
            stream->stream.ops = duplicate_dispatch_table(&io_stream_ops);
            break;
	case ecl_smm_probe:
	case ecl_smm_input:
            stream->stream.ops = duplicate_dispatch_table(&input_stream_ops);
            break;
	case ecl_smm_output:
            stream->stream.ops = duplicate_dispatch_table(&output_stream_ops);
            break;
#if defined(ECL_WSOCK)
	case ecl_smm_input_wsock:
            stream->stream.ops = duplicate_dispatch_table(&winsock_stream_input_ops);
            break;
	case ecl_smm_output_wsock:
            stream->stream.ops = duplicate_dispatch_table(&winsock_stream_output_ops);
            break;
	case ecl_smm_io_wsock:
            stream->stream.ops = duplicate_dispatch_table(&winsock_stream_io_ops);
            break;
	case ecl_smm_io_wcon:
            stream->stream.ops = duplicate_dispatch_table(&wcon_stream_io_ops);
            break;
#endif
	default:
            FEerror("Not a valid mode ~D for ecl_make_stream_from_FILE", 1, ecl_make_fixnum(smm).asTPtr());
	}
	set_stream_elt_type(stream, byte_size, flags, external_format);
	IO_STREAM_FILENAME(stream) = fname; /* not really used */
	stream->stream.column = 0;
        IO_STREAM_FILE(stream) = f;
	stream->stream.last_op = 0;
	si_set_finalizer(stream, ECL_T);
	return stream;
    }

    T_sp
    ecl_make_stream_from_fd(T_sp fname, int fd, enum ecl_smmode smm,
                            cl_fixnum byte_size, int flags, T_sp external_format)
    {
	char *mode;			/* file open mode */
	FILE *fp;			/* file pointer */
	switch(smm) {
	case ecl_smm_input:
            mode = OPEN_R;
            break;
	case ecl_smm_output:
            mode = OPEN_W;
            break;
	case ecl_smm_io:
            mode = OPEN_RW;
            break;
#if defined(ECL_WSOCK)
	case ecl_smm_input_wsock:
	case ecl_smm_output_wsock:
	case ecl_smm_io_wsock:
	case ecl_smm_io_wcon:
            break;
#endif
	default:
            FEerror("make_stream: wrong mode", 0);
	}
#if defined(ECL_WSOCK)
	if (smm == ecl_smm_input_wsock || smm == ecl_smm_output_wsock || smm == ecl_smm_io_wsock || smm == ecl_smm_io_wcon)
            fp = (FILE*)fd;
	else
            fp = safe_fdopen(fd, mode);
#else
	fp = safe_fdopen(fd, mode);
#endif
        if (fp == NULL) {
            FElibc_error("Unable to create stream for file descriptor ~D",
                         1, ecl_make_integer(fd));
        }
	return ecl_make_stream_from_FILE(fname, fp, smm, byte_size, flags,
					 external_format);
    }

    int
    ecl_stream_to_handle(T_sp s, bool output)
    {
    BEGIN:
	if (ecl_unlikely(!ECL_ANSI_STREAM_P(s)))
            return -1;
	switch ((enum ecl_smmode)s->stream.mode) {
	case ecl_smm_input:
            if (output) return -1;
            return fileno(IO_STREAM_FILE(s));
	case ecl_smm_input_file:
            if (output) return -1;
            return IO_FILE_DESCRIPTOR(s);
	case ecl_smm_output:
            if (!output) return -1;
            return fileno(IO_STREAM_FILE(s));
	case ecl_smm_output_file:
            if (!output) return -1;
            return IO_FILE_DESCRIPTOR(s);
	case ecl_smm_io:
            return fileno(IO_STREAM_FILE(s));
	case ecl_smm_io_file:
            return IO_FILE_DESCRIPTOR(s);
	case ecl_smm_synonym:
            s = SYNONYM_STREAM_STREAM(s);
            goto BEGIN;
	case ecl_smm_two_way:
            s = output? TWO_WAY_STREAM_OUTPUT(s) : TWO_WAY_STREAM_INPUT(s);
            goto BEGIN;
#if defined(ECL_WSOCK)
	case ecl_smm_input_wsock:
	case ecl_smm_output_wsock:
	case ecl_smm_io_wsock:
#endif
#if defined(ECL_MS_WINDOWS_HOST)
	case ecl_smm_io_wcon:
#endif
            return -1;
	default:
            ecl_internal_error("illegal stream mode");
	}
    }

    T_sp
    si_file_stream_fd(T_sp s)
    {
        T_sp ret;

	unlikely_if (!ECL_ANSI_STREAM_P(s))
            FEerror("file_stream_fd: not a stream", 0);

	switch ((enum ecl_smmode)s->stream.mode) {
	case ecl_smm_input:
	case ecl_smm_output:
	case ecl_smm_io:
            ret = ecl_make_fixnum(fileno(IO_STREAM_FILE(s)));
            break;
	case ecl_smm_input_file:
	case ecl_smm_output_file:
	case ecl_smm_io_file:
            ret = ecl_make_fixnum(IO_FILE_DESCRIPTOR(s));
            break;
	default:
            ecl_internal_error("not a file stream");
	}
        @(return ret);
    }

/**********************************************************************
 * SEQUENCE INPUT STREAMS
 */

    static cl_index
    seq_in_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	cl_fixnum curr_pos = SEQ_INPUT_POSITION(strm);
	cl_fixnum last = SEQ_INPUT_LIMIT(strm);
	cl_fixnum delta = last - curr_pos;
	if (delta > 0) {
            T_sp vector = SEQ_INPUT_VECTOR(strm);
            if (delta > n) delta = n;
            memcpy(c, vector->vector.self.bc + curr_pos, delta);
            SEQ_INPUT_POSITION(strm) += delta;
            return delta;
	}
	return 0;
    }

    static void
    seq_in_unread_char(T_sp strm, claspCharacter c)
    {
	eformat_unread_char(strm, c);
	SEQ_INPUT_POSITION(strm) -= ecl_length(strm->stream.byte_stack);
	strm->stream.byte_stack = ECL_NIL;
    }

    static int
    seq_in_listen(T_sp strm)
    {
	if (SEQ_INPUT_POSITION(strm) < SEQ_INPUT_LIMIT(strm))
            return ECL_LISTEN_AVAILABLE;
	else
            return ECL_LISTEN_EOF;
    }

    static T_sp
    seq_in_get_position(T_sp strm)
    {
	return ecl_make_unsigned_integer(SEQ_INPUT_POSITION(strm));
    }

    static T_sp
    seq_in_set_position(T_sp strm, T_sp pos)
    {
	cl_fixnum disp;
	if (Null(pos)) {
            disp = SEQ_INPUT_LIMIT(strm);
	}  else {
            disp = ecl_to_size(pos);
            if (disp >= SEQ_INPUT_LIMIT(strm)) {
                disp = SEQ_INPUT_LIMIT(strm);
            }
	}
	SEQ_INPUT_POSITION(strm) = disp;
	return ECL_T;
    }

    const struct ecl_file_ops seq_in_ops = {
	not_output_write_byte8,
	seq_in_read_byte8,

	not_output_write_byte,
	generic_read_byte,

	eformat_read_char,
	not_output_write_char,
	seq_in_unread_char,
	generic_peek_char,

	generic_read_vector,
	generic_write_vector,

	seq_in_listen,
	generic_void, /* clear-input */
	not_output_clear_output,
	not_output_finish_output,
	not_output_force_output,

	generic_always_true, /* input_p */
	generic_always_false, /* output_p */
	generic_always_false,
	io_file_element_type,

	not_a_file_stream, /* length */
	seq_in_get_position,
	seq_in_set_position,
	generic_column,
	generic_close
    };

    static T_sp
    make_sequence_input_stream(T_sp vector, cl_index istart, cl_index iend,
                               T_sp external_format)
    {
	T_sp strm;
        cl_elttype type;
        T_sp type_name;
        int byte_size;
        int flags = 0;
        if (!ECL_VECTORP(vector) ||
            ((type = ecl_array_elttype(vector)) < ecl_aet_b8 &&
	     type > ecl_aet_bc) ||
	    ecl_aet_size[type] != 1)
        {
            FEerror("MAKE-SEQUENCE-INPUT-STREAM only accepts vectors whose element has a size of 1 byte.~%~A", 1, vector.asTPtr());
        }
        type_name = ecl_elttype_to_symbol(type);
        byte_size = ecl_normalize_stream_element_type(type_name);
        /* Character streams always get some external format. For binary
         * sequences it has to be explicitly mentioned. */
        strm = alloc_stream();
	strm->stream.ops = duplicate_dispatch_table(&seq_in_ops);
	strm->stream.mode = (short)ecl_smm_sequence_input;
        if (!byte_size) {
#if defined(ECL_UNICODE)
            if (ECL_BASE_STRING_P(vector)) {
                if (Null(external_format))
                    external_format = @':default';
            } else {
                if (Null(external_format)) {
# ifdef WORDS_BIGENDIAN
                    external_format = @':ucs-4be';
# else
                    external_format = @':ucs-4le';
# endif
                }
            }
#else
            if (Null(external_format)) {
                external_format = @':default';
            }
#endif
        }
        set_stream_elt_type(strm, byte_size, flags, external_format);
        /* Override byte size and elt type */
        if (byte_size) strm->stream.byte_size = byte_size;
	SEQ_INPUT_VECTOR(strm) = vector;
	SEQ_INPUT_POSITION(strm) = istart;
	SEQ_INPUT_LIMIT(strm) = iend;
	return strm;
    }

    @(defun ext::make_sequence_input_stream (vector &key
                                             (start ecl_make_fixnum(0))
                                             (end ECL_NIL)
                                             (external_format ECL_NIL))
      cl_index_pair p;
      @
      p = ecl_vector_start_end(@[ext::make-sequence-input-stream],
                               vector, start, end);
      @(return make_sequence_input_stream(vector, p.start, p.end,
                                          external_format))
      @)

    /**********************************************************************
     * SEQUENCE OUTPUT STREAMS
     */

    static cl_index
    seq_out_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
    AGAIN:
        {
            T_sp vector = SEQ_OUTPUT_VECTOR(strm);
            cl_fixnum curr_pos = SEQ_OUTPUT_POSITION(strm);
            cl_fixnum last = vector->vector.dim;
            cl_fixnum delta = last - curr_pos;
            if (delta < n) {
                /* Not enough space, enlarge */
                vector = _ecl_funcall3(@'adjust-array', vector,
				       ecl_ash(ecl_make_fixnum(last), 1));
                SEQ_OUTPUT_VECTOR(strm) = vector;
                goto AGAIN;
            }
            memcpy(vector->vector.self.bc + curr_pos, c, n);
            SEQ_OUTPUT_POSITION(strm) = curr_pos += n;
            if (vector->vector.fillp < curr_pos)
		vector->vector.fillp = curr_pos;
        }
        return n;
    }

    static T_sp
    seq_out_get_position(T_sp strm)
    {
	return ecl_make_unsigned_integer(SEQ_OUTPUT_POSITION(strm));
    }

    static T_sp
    seq_out_set_position(T_sp strm, T_sp pos)
    {
	T_sp vector = SEQ_OUTPUT_VECTOR(strm);
	cl_fixnum disp;
	if (Null(pos)) {
            disp = vector->vector.fillp;
	} else {
            disp = ecl_to_size(pos);
            if (disp >= vector->vector.dim) {
                disp = vector->vector.fillp;
            }
	}
	SEQ_OUTPUT_POSITION(strm) = disp;
	return ECL_T;
    }

    const struct ecl_file_ops seq_out_ops = {
	seq_out_write_byte8,
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
	generic_void, /* clear-output */
	generic_void, /* finish-output */
	generic_void, /* force-output */

	generic_always_false, /* input_p */
	generic_always_true, /* output_p */
	generic_always_false,
	io_file_element_type,

	not_a_file_stream, /* length */
	seq_out_get_position,
	seq_out_set_position,
	generic_column,
	generic_close
    };

    static T_sp
    make_sequence_output_stream(T_sp vector, T_sp external_format)
    {
	T_sp strm;
        cl_elttype type;
        T_sp type_name;
        int byte_size;
        int flags = 0;
        if (!ECL_VECTORP(vector) ||
            ((type = ecl_array_elttype(vector)) < ecl_aet_b8 &&
	     type > ecl_aet_bc) ||
	    ecl_aet_size[type] != 1)
        {
            FEerror("MAKE-SEQUENCE-OUTPUT-STREAM only accepts vectors whose element has a size of 1 byte.~%~A", 1, vector.asTPtr());
        }
        type_name = ecl_elttype_to_symbol(type);
        byte_size = ecl_normalize_stream_element_type(type_name);
        /* Character streams always get some external format. For binary
         * sequences it has to be explicitly mentioned. */
	strm = alloc_stream();
	strm->stream.ops = duplicate_dispatch_table(&seq_out_ops);
	strm->stream.mode = (short)ecl_smm_sequence_output;
        if (!byte_size) {
#if defined(ECL_UNICODE)
            if (ECL_BASE_STRING_P(vector)) {
                if (Null(external_format))
                    external_format = @':default';
            } else {
                if (Null(external_format)) {
# ifdef WORDS_BIGENDIAN
                    external_format = @':ucs-4be';
# else
                    external_format = @':ucs-4le';
# endif
                }
            }
#else
            if (Null(external_format)) {
                external_format = @':default';
            }
#endif
        }
        set_stream_elt_type(strm, byte_size, flags, external_format);
        /* Override byte size and elt type */
        if (byte_size) strm->stream.byte_size = byte_size;
	SEQ_OUTPUT_VECTOR(strm) = vector;
	SEQ_OUTPUT_POSITION(strm) = vector->vector.fillp;
	return strm;
    }

    @(defun ext::make_sequence_output_stream (vector &key (external_format ECL_NIL))
      @
      @(return make_sequence_output_stream(vector, external_format))
      @)

    /**********************************************************************
     * MEDIUM LEVEL INTERFACE
     */

    struct ecl_file_ops *
    duplicate_dispatch_table(const struct ecl_file_ops *ops)
    {
	struct ecl_file_ops *new_ops = ecl_alloc_atomic(sizeof(*ops));
	*new_ops = *ops;
	return new_ops;
    }

    const struct ecl_file_ops *
    stream_dispatch_table(T_sp strm)
    {
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
            return &clos_stream_ops;
	}
#endif
	if (!ECL_ANSI_STREAM_P(strm))
            FEwrong_type_argument(@[stream], strm);
	return (const struct ecl_file_ops *)strm->stream.ops;
    }

    static cl_index
    ecl_read_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	return stream_dispatch_table(strm)->read_byte8(strm, c, n);
    }

    static cl_index
    ecl_write_byte8(T_sp strm, unsigned char *c, cl_index n)
    {
	return stream_dispatch_table(strm)->write_byte8(strm, c, n);
    }

    claspCharacter
    ecl_read_char(T_sp strm)
    {
	return stream_dispatch_table(strm)->read_char(strm);
    }

    claspCharacter
    ecl_read_char_noeof(T_sp strm)
    {
	claspCharacter c = ecl_read_char(strm);
	if (c == EOF)
            FEend_of_file(strm);
	return c;
    }

    T_sp
    ecl_read_byte(T_sp strm)
    {
	return stream_dispatch_table(strm)->read_byte(strm);
    }

    void
    ecl_write_byte(T_sp c, T_sp strm)
    {
	stream_dispatch_table(strm)->write_byte(c, strm);
    }

    claspCharacter
    ecl_write_char(claspCharacter c, T_sp strm)
    {
	return stream_dispatch_table(strm)->write_char(strm, c);
    }

    void
    ecl_unread_char(claspCharacter c, T_sp strm)
    {
	stream_dispatch_table(strm)->unread_char(strm, c);
    }

    int
    ecl_listen_stream(T_sp strm)
    {
	return stream_dispatch_table(strm)->listen(strm);
    }

    void
    ecl_clear_input(T_sp strm)
    {
	stream_dispatch_table(strm)->clear_input(strm);
    }

    void
    ecl_clear_output(T_sp strm)
    {
	stream_dispatch_table(strm)->clear_output(strm);
    }

    void
    ecl_force_output(T_sp strm)
    {
	stream_dispatch_table(strm)->force_output(strm);
    }

    void
    ecl_finish_output(T_sp strm)
    {
	stream_dispatch_table(strm)->finish_output(strm);
    }

    int
    ecl_file_column(T_sp strm)
    {
	return stream_dispatch_table(strm)->column(strm);
    }

    T_sp
    ecl_file_length(T_sp strm)
    {
	return stream_dispatch_table(strm)->length(strm);
    }

    T_sp
    ecl_file_position(T_sp strm)
    {
	return stream_dispatch_table(strm)->get_position(strm);
    }

    T_sp
    ecl_file_position_set(T_sp strm, T_sp pos)
    {
	return stream_dispatch_table(strm)->set_position(strm, pos);
    }

    bool
    ecl_input_stream_p(T_sp strm)
    {
	return stream_dispatch_table(strm)->input_p(strm);
    }

    bool
    ecl_output_stream_p(T_sp strm)
    {
	return stream_dispatch_table(strm)->output_p(strm);
    }

    T_sp
    ecl_stream_element_type(T_sp strm)
    {
	return stream_dispatch_table(strm)->element_type(strm);
    }

    int
    ecl_interactive_stream_p(T_sp strm)
    {
	return stream_dispatch_table(strm)->interactive_p(strm);
    }

/*
 * ecl_read_char(s) tries to read a character from the stream S. It outputs
 * either the code of the character read, or EOF. Whe compiled with
 * CLOS-STREAMS and S is an instance object, STREAM-READ-CHAR is invoked
 * to retrieve the character. Then STREAM-READ-CHAR should either
 * output the character, or NIL, indicating EOF.
 *
 * INV: ecl_read_char(strm) checks the type of STRM.
 */
    claspCharacter
    ecl_peek_char(T_sp strm)
    {
	return stream_dispatch_table(strm)->peek_char(strm);
    }

/*******************************tl***************************************
 * SEQUENCES I/O
 */

    void
    writestr_stream(const char *s, T_sp strm)
    {
	while (*s != '\0')
            ecl_write_char(*s++, strm);
    }

    static cl_index
    compute_char_size(T_sp stream, claspCharacter c)
    {
	unsigned char buffer[5];
	int l = 0;
	if (c == ECL_CHAR_CODE_NEWLINE) {
            int flags = stream->stream.flags;
            if (flags & ECL_STREAM_CR) {
                l += stream->stream.encoder(stream, buffer, ECL_CHAR_CODE_RETURN);
                if (flags & ECL_STREAM_LF)
                    l += stream->stream.encoder(stream, buffer,
                                                ECL_CHAR_CODE_LINEFEED);
            } else {
                l += stream->stream.encoder(stream, buffer, ECL_CHAR_CODE_LINEFEED);
            }
	} else {
            l += stream->stream.encoder(stream, buffer, c);
	}
	return l;
    }

    T_sp
    cl_file_string_length(T_sp stream, T_sp string)
    {
	cl_fixnum l = 0;
	/* This is a stupid requirement from the spec. Why returning 1???
	 * Why not simply leaving the value unspecified, as with other
	 * streams one cannot write to???
	 */
    BEGIN:
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(stream)) {
            @(return ECL_NIL)
                }
#endif
	unlikely_if (!ECL_ANSI_STREAM_P(stream)) {
            FEwrong_type_only_arg(@[file-string-length], stream, @[stream]);
	}
	if (stream->stream.mode == ecl_smm_broadcast) {
            stream = BROADCAST_STREAM_LIST(stream);
            if (Null(stream)) {
                @(return ecl_make_fixnum(1));
            } else {
                goto BEGIN;
            }
	}
	unlikely_if (!ECL_FILE_STREAM_P(stream)) {
            not_a_file_stream(stream);
	}
	switch (ecl_t_of(string)) {
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string: {
            cl_index i;
            for (i = 0; i < string->base_string.fillp; i++) {
                l += compute_char_size(stream, ecl_char(string, i));
            }
            break;
	}
	case t_character:
            l = compute_char_size(stream, ECL_CHAR_CODE(string));
            break;
	default:
            FEwrong_type_nth_arg(@[file-string-length], 2, string, @[string]);
	}
	@(return ecl_make_fixnum(l))
            }

    T_sp
    si_do_write_sequence(T_sp seq, T_sp stream, T_sp s, T_sp e)
    {
	const struct ecl_file_ops *ops;
	cl_fixnum start,limit,end;

	/* Since we have called ecl_length(), we know that SEQ is a valid
	   sequence. Therefore, we only need to check the type of the
	   object, and seq == ECL_NIL i.f.f. t = t_symbol */
	limit = ecl_length(seq);
        if (ecl_unlikely(!ECL_FIXNUMP(s) ||
                         ((start = ecl_fixnum(s)) < 0) ||
                         (start > limit))) {
            FEwrong_type_key_arg(@[write-sequence], @[:start], s,
                                 ecl_make_integer_type(ecl_make_fixnum(0),
                                                       ecl_make_fixnum(limit-1)));
        }
	if (e == ECL_NIL) {
            end = limit;
	} else if (ecl_unlikely(!ECL_FIXNUMP(e) ||
                                ((end = ecl_fixnum(e)) < 0) ||
                                (end > limit))) {
            FEwrong_type_key_arg(@[write-sequence], @[:end], e,
                                 ecl_make_integer_type(ecl_make_fixnum(0),
                                                       ecl_make_fixnum(limit)));
        }
	if (end <= start) {
            goto OUTPUT;
	}
	ops = stream_dispatch_table(stream);
	if (LISTP(seq)) {
            T_sp elt_type = cl_stream_element_type(stream);
            bool ischar = (elt_type == @'base-char') || (elt_type == @'character');
            T_sp s = ecl_nthcdr(start, seq);
            loop_for_in(s) {
                if (start < end) {
                    T_sp elt = CAR(s);
                    if (ischar)
                        ops->write_char(stream, ecl_char_code(elt));
                    else
                        ops->write_byte(elt, stream);
                    start++;
                } else {
                    goto OUTPUT;
                }
            } end_loop_for_in;
	} else {
            ops->write_vector(stream, seq, start, end);
	}
    OUTPUT:
	@(return seq);
    }

    T_sp
    si_do_read_sequence(T_sp seq, T_sp stream, T_sp s, T_sp e)
    {
	const struct ecl_file_ops *ops;
	cl_fixnum start,limit,end;

	/* Since we have called ecl_length(), we know that SEQ is a valid
	   sequence. Therefore, we only need to check the type of the
	   object, and seq == ECL_NIL i.f.f. t = t_symbol */
	limit = ecl_length(seq);
        if (ecl_unlikely(!ECL_FIXNUMP(s) ||
                         ((start = ecl_fixnum(s)) < 0) ||
                         (start > limit))) {
            FEwrong_type_key_arg(@[read-sequence], @[:start], s,
                                 ecl_make_integer_type(ecl_make_fixnum(0),
                                                       ecl_make_fixnum(limit-1)));
        }
	if (e == ECL_NIL) {
            end = limit;
	} else if (ecl_unlikely(!ECL_FIXNUMP(e) ||
                                ((end = ecl_fixnum(e)) < 0) ||
                                (end > limit))) {
            FEwrong_type_key_arg(@[read-sequence], @[:end], e,
                                 ecl_make_integer_type(ecl_make_fixnum(0),
                                                       ecl_make_fixnum(limit)));
        }
	if (end <= start) {
            goto OUTPUT;
	}
	ops = stream_dispatch_table(stream);
	if (LISTP(seq)) {
            T_sp elt_type = cl_stream_element_type(stream);
            bool ischar = (elt_type == @'base-char') || (elt_type == @'character');
            seq = ecl_nthcdr(start, seq);
            loop_for_in(seq) {
                if (start >= end) {
                    goto OUTPUT;
                } else {
                    T_sp c;
                    if (ischar) {
                        int i = ops->read_char(stream);
                        if (i < 0) goto OUTPUT;
                        c = ECL_CODE_CHAR(i);
                    } else {
                        c = ops->read_byte(stream);
                        if (c == ECL_NIL) goto OUTPUT;
                    }
                    ECL_RPLACA(seq, c);
                    start++;
                }
            } end_loop_for_in;
	} else {
            start = ops->read_vector(stream, seq, start, end);
	}
    OUTPUT:
	@(return ecl_make_fixnum(start))
            }

/**********************************************************************
 * LISP LEVEL INTERFACE
 */

    T_sp
    si_file_column(T_sp strm)
    {
	@(return ecl_make_fixnum(ecl_file_column(strm)))
            }

    T_sp
    cl_file_length(T_sp strm)
    {
	@(return ecl_file_length(strm))
            }

    @(defun file-position (file_stream &o position)
      T_sp output;
      @
      if (Null(position)) {
          output = ecl_file_position(file_stream);
      } else {
          if (position == @':start') {
              position = ecl_make_fixnum(0);
          } else if (position == @':end') {
              position = ECL_NIL;
          }
          output = ecl_file_position_set(file_stream, position);
      }
      @(return output)
      @)

    T_sp
    cl_input_stream_p(T_sp strm)
    {
	@(return (ecl_input_stream_p(strm) ? ECL_T : ECL_NIL))
            }

    T_sp
    cl_output_stream_p(T_sp strm)
    {
	@(return (ecl_output_stream_p(strm) ? ECL_T : ECL_NIL))
            }

    T_sp
    cl_interactive_stream_p(T_sp strm)
    {
	@(return (stream_dispatch_table(strm)->interactive_p(strm)? ECL_T : ECL_NIL))
            }

    T_sp
    cl_open_stream_p(T_sp strm)
    {
	/* ANSI and Cltl2 specify that open-stream-p should work
	   on closed streams, and that a stream is only closed
	   when #'close has been applied on it */
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
            return _ecl_funcall2(@'gray::open-stream-p', strm);
	}
#endif
	unlikely_if (!ECL_ANSI_STREAM_P(strm))
            FEwrong_type_only_arg(@'open-stream-p', strm, @'stream');
	@(return (strm->stream.closed ? ECL_NIL : ECL_T))
            }

    T_sp
    cl_stream_element_type(T_sp strm)
    {
	@(return ecl_stream_element_type(strm))
            }

    T_sp
    cl_stream_external_format(T_sp strm)
    {
	T_sp output;
	cl_type t;
    AGAIN:
	t= ecl_t_of(strm);
#ifdef ECL_CLOS_STREAMS
	if (t == t_instance)
            output = @':default';
	else
#endif
            unlikely_if (t != t_stream)
                FEwrong_type_only_arg(@[stream-external-format], strm, @[stream]);
	if (strm->stream.mode == ecl_smm_synonym) {
            strm = SYNONYM_STREAM_STREAM(strm);
            goto AGAIN;
	}
	output = strm->stream.format;
	@(return output)
            }

    T_sp
    cl_streamp(T_sp strm)
    {
#ifdef ECL_CLOS_STREAMS
	if (ECL_INSTANCEP(strm)) {
            return _ecl_funcall2(@'gray::streamp', strm);
	}
#endif
	@(return (ECL_ANSI_STREAM_P(strm) ? ECL_T : ECL_NIL))
            }

/**********************************************************************
 * OTHER TOOLS
 */

    T_sp
    si_copy_stream(T_sp in, T_sp out)
    {
	claspCharacter c;
	for (c = ecl_read_char(in); c != EOF; c = ecl_read_char(in)) {
            ecl_write_char(c, out);
	}
	ecl_force_output(out);
	@(return ECL_T)
            }


/**********************************************************************
 * FILE OPENING AND CLOSING
 */

    cl_fixnum
    ecl_normalize_stream_element_type(T_sp element_type)
    {
	cl_fixnum sign = 0;
	cl_index size;
	if (element_type == @'signed-byte' || element_type == @'ext::integer8') {
            return -8;
	} else if (element_type == @'unsigned-byte' || element_type == @'ext::byte8') {
            return 8;
	} else if (element_type == @':default') {
            return 0;
        } else if (element_type == @'base-char' || element_type == @'character') {
            return 0;
	} else if (_ecl_funcall3(@'subtypep', element_type, @'character') != ECL_NIL) {
            return 0;
	} else if (_ecl_funcall3(@'subtypep', element_type, @'unsigned-byte') != ECL_NIL) {
            sign = +1;
	} else if (_ecl_funcall3(@'subtypep', element_type, @'signed-byte') != ECL_NIL) {
            sign = -1;
	} else {
            FEerror("Not a valid stream element type: ~A", 1, element_type.asTPtr());
	}
	if (CONSP(element_type)) {
            if (CAR(element_type) == @'unsigned-byte')
                return ecl_to_size(cl_cadr(element_type));
            if (CAR(element_type) == @'signed-byte')
                return -ecl_to_size(cl_cadr(element_type));
	}
	for (size = 8; 1; size++) {
            T_sp type;
            type = cl_list(2, sign>0? @'unsigned-byte' : @'signed-byte',
                           ecl_make_fixnum(size));
            if (_ecl_funcall3(@'subtypep', element_type, type) != ECL_NIL) {
                return size * sign;
            }
	}
	FEerror("Not a valid stream element type: ~A", 1, element_type.asTPtr());
    }

    static void
    FEinvalid_option(T_sp option, T_sp value)
    {
	FEerror("Invalid value op option ~A: ~A", 2, option.asTPtr(), value.asTPtr());
    }

    T_sp
    ecl_open_stream(T_sp fn, enum ecl_smmode smm, T_sp if_exists,
                    T_sp if_does_not_exist, cl_fixnum byte_size,
                    int flags, T_sp external_format)
    {
	T_sp output;
	int f;
#if defined(ECL_MS_WINDOWS_HOST)
        ecl_mode_t mode = _S_IREAD | _S_IWRITE;
#else
	ecl_mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
#endif
	T_sp filename = si_coerce_to_filename(fn);
	char *fname = (char*)filename->base_string.self;
	bool appending = 0;
	bool exists = si_file_kind(filename, ECL_T) != ECL_NIL;
	if (smm == ecl_smm_input || smm == ecl_smm_probe) {
            if (!exists) {
                if (if_does_not_exist == @':error') {
                    FEcannot_open(fn);
                } else if (if_does_not_exist == @':create') {
                    f = safe_open(fname, O_WRONLY|O_CREAT, mode);
                    unlikely_if (f < 0) FEcannot_open(fn);
                    safe_close(f);
                } else if (Null(if_does_not_exist)) {
                    return ECL_NIL;
                } else {
                    FEinvalid_option(@':if-does-not-exist',
                                     if_does_not_exist);
                }
            }
            f = safe_open(fname, O_RDONLY, mode);
            unlikely_if (f < 0) FEcannot_open(fn);
	} else if (smm == ecl_smm_output || smm == ecl_smm_io) {
            int base = (smm == ecl_smm_output)? O_WRONLY : O_RDWR;
            if (if_exists == @':new_version' &&
                if_does_not_exist == @':create') {
                exists = 0;
                if_does_not_exist = @':create';
            }
            if (exists) {
                if (if_exists == @':error') {
                    FEcannot_open(fn);
                } else if (if_exists == @':rename') {
                    f = ecl_backup_open(fname, base|O_CREAT, mode);
                    unlikely_if (f < 0) FEcannot_open(fn);
                } else if (if_exists == @':rename_and_delete' ||
                           if_exists == @':new_version' ||
                           if_exists == @':supersede') {
                    f = safe_open(fname, base|O_TRUNC, mode);
                    unlikely_if (f < 0) FEcannot_open(fn);
                } else if (if_exists == @':overwrite' || if_exists == @':append') {
                    f = safe_open(fname, base, mode);
                    unlikely_if (f < 0) FEcannot_open(fn);
                    appending = (if_exists == @':append');
                } else if (Null(if_exists)) {
                    return ECL_NIL;
                } else {
                    FEinvalid_option(@':if-exists', if_exists);
                }
            } else {
                if (if_does_not_exist == @':error') {
                    FEcannot_open(fn);
                } else if (if_does_not_exist == @':create') {
                    f = safe_open(fname, base | O_CREAT | O_TRUNC, mode);
                    unlikely_if (f < 0) FEcannot_open(fn);
                } else if (Null(if_does_not_exist)) {
                    return ECL_NIL;
                } else {
                    FEinvalid_option(@':if-does-not-exist',
                                     if_does_not_exist);
                }
            }
	} else {
            FEerror("Illegal stream mode ~S", 1, ecl_make_fixnum(smm).asTPtr());
	}
	if (flags & ECL_STREAM_C_STREAM) {
            FILE *fp;
            safe_close(f);
            /* We do not use fdopen() because Windows seems to
             * have problems with the resulting streams. Furthermore, even for
             * output we open with w+ because we do not want to
             * overwrite the file. */
            switch (smm) {
            case ecl_smm_probe:
            case ecl_smm_input: fp = safe_fopen(fname, OPEN_R); break;
            case ecl_smm_output:
            case ecl_smm_io: fp = safe_fopen(fname, OPEN_RW); break;
            default:; /* never reached */
            }
            output = ecl_make_stream_from_FILE(fn, fp, smm, byte_size, flags,
                                               external_format);
            si_set_buffering_mode(output, byte_size? @':full' : @':line');
	} else {
            output = ecl_make_file_stream_from_fd(fn, f, smm, byte_size, flags,
                                                  external_format);
	}
	if (smm == ecl_smm_probe) {
            cl_close(1, output);
	} else {
            output->stream.flags |= ECL_STREAM_MIGHT_SEEK;
            si_set_finalizer(output, ECL_T);
            /* Set file pointer to the correct position */
            ecl_file_position_set(output, appending? ECL_NIL : ecl_make_fixnum(0));
	}
	return output;
    }

    @(defun open (filename
                  &key (direction @':input')
                  (element_type @'character')
                  (if_exists ECL_NIL iesp)
                  (if_does_not_exist ECL_NIL idnesp)
                  (external_format @':default')
                  (cstream ECL_T)
                  &aux strm)
      enum ecl_smmode smm;
      int flags = 0;
      cl_fixnum byte_size;
      @
      /* INV: ecl_open_stream() checks types */
      if (direction == @':input') {
          smm = ecl_smm_input;
          if (!idnesp)
              if_does_not_exist = @':error';
      } else if (direction == @':output') {
          smm = ecl_smm_output;
          if (!iesp)
              if_exists = @':new_version';
          if (!idnesp) {
              if (if_exists == @':overwrite' ||
                  if_exists == @':append')
                  if_does_not_exist = @':error';
              else
                  if_does_not_exist = @':create';
          }
      } else if (direction == @':io') {
          smm = ecl_smm_io;
          if (!iesp)
              if_exists = @':new_version';
          if (!idnesp) {
              if (if_exists == @':overwrite' ||
                  if_exists == @':append')
                  if_does_not_exist = @':error';
              else
                  if_does_not_exist = @':create';
          }
      } else if (direction == @':probe') {
          smm = ecl_smm_probe;
          if (!idnesp)
              if_does_not_exist = ECL_NIL;
      } else {
          FEerror("~S is an illegal DIRECTION for OPEN.",
                  1, direction.asTPtr());
      }
      byte_size = ecl_normalize_stream_element_type(element_type);
      if (byte_size != 0) {
          external_format = ECL_NIL;
      }
      if (!Null(cstream)) {
          flags |= ECL_STREAM_C_STREAM;
      }
      strm = ecl_open_stream(filename, smm, if_exists, if_does_not_exist,
                             byte_size, flags, external_format);
      @(return strm)
      @)


    @(defun close (strm &key (abort @'nil'))
      @
      @(return stream_dispatch_table(strm)->close(strm));
      @)

    /**********************************************************************
     * BACKEND
     */

    static int
    file_listen(T_sp stream, int fileno)
    {
#if !defined(ECL_MS_WINDOWS_HOST)
# if defined(HAVE_SELECT)
	fd_set fds;
	int retv;
	struct timeval tv = { 0, 0 };
        /*
         * Note that the following code is fragile. If the file is closed (/dev/null)
         * then select() may return 1 (at least on OS X), so that we return a flag
         * saying characters are available but will find none to read. See also the
         * code in cl_clear_input().
         */
	FD_ZERO(&fds);
	FD_SET(fileno, &fds);
	retv = select(fileno + 1, &fds, NULL, NULL, &tv);
	if (ecl_unlikely(retv < 0))
            file_libc_error(@[stream-error], stream, "Error while listening to stream.", 0);
	else if (retv > 0)
            return ECL_LISTEN_AVAILABLE;
	else
            return ECL_LISTEN_NO_CHAR;
# elif defined(FIONREAD)
	{
            long c = 0;
            ioctl(fileno, FIONREAD, &c);
            return (c > 0)? ECL_LISTEN_AVAILABLE : ECL_LISTEN_NO_CHAR;
	}
# endif /* FIONREAD */
#else
	HANDLE hnd = (HANDLE)_get_osfhandle(fileno);
	switch (GetFileType(hnd)) {
        case FILE_TYPE_CHAR: {
            DWORD dw, dw_read, cm;
            if (GetNumberOfConsoleInputEvents(hnd, &dw)) {
                unlikely_if (!GetConsoleMode(hnd, &cm))
                    FEwin32_error("GetConsoleMode() failed", 0);
                if (dw > 0) {
                    PINPUT_RECORD recs = (PINPUT_RECORD)GC_malloc(sizeof(INPUT_RECORD)*dw);
                    int i;
                    unlikely_if (!PeekConsoleInput(hnd, recs, dw, &dw_read))
                        FEwin32_error("PeekConsoleInput failed()", 0);
                    if (dw_read > 0) {
                        if (cm & ENABLE_LINE_INPUT) {
                            for (i=0; i<dw_read; i++)
                                if (recs[i].EventType == KEY_EVENT &&
                                    recs[i].Event.KeyEvent.bKeyDown &&
                                    recs[i].Event.KeyEvent.uChar.AsciiChar == 13)
                                    return ECL_LISTEN_AVAILABLE;
                        } else {
                            for (i=0; i<dw_read; i++)
                                if (recs[i].EventType == KEY_EVENT &&
                                    recs[i].Event.KeyEvent.bKeyDown &&
                                    recs[i].Event.KeyEvent.uChar.AsciiChar != 0)
                                    return ECL_LISTEN_AVAILABLE;
                        }
                    }
                }
                return ECL_LISTEN_NO_CHAR;
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
                return (dw > 0 ? ECL_LISTEN_AVAILABLE : ECL_LISTEN_NO_CHAR);
            else if (GetLastError() == ERROR_BROKEN_PIPE)
                return ECL_LISTEN_EOF;
            else
                FEwin32_error("PeekNamedPipe() failed", 0);
            break;
        }
        default:
            FEerror("Unsupported Windows file type: ~A", 1, ecl_make_fixnum(GetFileType(hnd)).asTPtr());
            break;
	}
#endif
	return -3;
    }

    static int
    flisten(T_sp stream, FILE *fp)
    {
	int aux;
	if (feof(fp))
            return ECL_LISTEN_EOF;
#ifdef FILE_CNT
	if (FILE_CNT(fp) > 0)
            return ECL_LISTEN_AVAILABLE;
#endif
	aux = file_listen(stream, fileno(fp));
	if (aux != -3)
            return aux;
	/* This code is portable, and implements the expected behavior for regular files.
	   It will fail on noninteractive streams. */
	{
            /* regular file */
            ecl_off_t old_pos = ecl_ftello(fp), end_pos;
            unlikely_if (ecl_fseeko(fp, 0, SEEK_END) != 0)
                file_libc_error(@[file-error], stream,
                                "Unable to check file position", 0);
            end_pos = ecl_ftello(fp);
            unlikely_if (ecl_fseeko(fp, old_pos, SEEK_SET) != 0)
                file_libc_error(@[file-error], stream,
                                "Unable to check file position", 0);
            return (end_pos > old_pos ? ECL_LISTEN_AVAILABLE : ECL_LISTEN_EOF);
	}
	return !ECL_LISTEN_AVAILABLE;
    }

    T_sp
    ecl_off_t_to_integer(ecl_off_t offset)
    {
	T_sp output;
	if (sizeof(ecl_off_t) == sizeof(cl_fixnum)) {
            output = ecl_make_integer(offset);
	} else if (offset <= MOST_POSITIVE_FIXNUM) {
            output = ecl_make_fixnum((cl_fixnum)offset);
	} else {
            T_sp y = _ecl_big_register0();
            if (sizeof(ECL_BIGNUM_LIMBS(y)[0]) == sizeof(cl_index)) {
                ECL_BIGNUM_LIMBS(y)[0] = (cl_index)offset;
                offset >>= FIXNUM_BITS;
                ECL_BIGNUM_LIMBS(y)[1] = offset;
                ECL_BIGNUM_SIZE(y) = offset? 2 : 1;
            } else if (sizeof(ECL_BIGNUM_LIMBS(y)[0]) >= sizeof(ecl_off_t)) {
                ECL_BIGNUM_LIMBS(y)[0] = offset;
                ECL_BIGNUM_SIZE(y) = 1;
            }
            output = _ecl_big_register_normalize(y);
	}
	return output;
    }

    ecl_off_t
    ecl_integer_to_off_t(T_sp offset)
    {
	ecl_off_t output = 0;
	if (sizeof(ecl_off_t) == sizeof(cl_fixnum)) {
            output = fixint(offset);
	} else if (ECL_FIXNUMP(offset)) {
            output = fixint(offset);
	} else if (ECL_BIGNUMP(offset)) {
            if (sizeof(ECL_BIGNUM_LIMBS(offset)[0]) == sizeof(cl_index)) {
                if (ECL_BIGNUM_SIZE(offset) > 2) {
                    goto ERR;
                }
                if (ECL_BIGNUM_SIZE(offset) == 2) {
                    output = ECL_BIGNUM_LIMBS(offset)[1];
                    output <<= FIXNUM_BITS;
                }
                output += ECL_BIGNUM_LIMBS(offset)[0];
            } else if (sizeof(ECL_BIGNUM_LIMBS(offset)[0]) >= sizeof(ecl_off_t)) {
                if (ECL_BIGNUM_SIZE(offset) > 1) {
                    goto ERR;
                }
                output = ECL_BIGNUM_LIMBS(offset)[0];
            }
	} else {
	ERR:	FEerror("Not a valid file offset: ~S", 1, offset.asTPtr());
	}
	return output;
    }

    static T_sp
    alloc_stream()
    {
	T_sp x = ecl_alloc_object(t_stream);
	x->stream.closed = 0;
	x->stream.file.descriptor = -1;
	x->stream.object0 =
            x->stream.object1 = OBJNULL;
	x->stream.int0 = x->stream.int1 = 0;
	x->stream.format = ECL_NIL;
	x->stream.flags = 0;
	x->stream.byte_size = 8;
	x->stream.buffer = NULL;
	x->stream.encoder = NULL;
	x->stream.decoder = NULL;
	x->stream.last_char = EOF;
	x->stream.byte_stack = ECL_NIL;
	x->stream.last_code[0] = x->stream.last_code[1] = EOF;
	x->stream.eof_char = EOF;
	return x;
    }

/**********************************************************************
 * ERROR MESSAGES
 */

    static T_sp
    not_a_file_stream(T_sp strm)
    {
	return cl_error(9, @'simple-type-error', @':format-control',
			make_constant_base_string("~A is not an file stream"),
			@':format-arguments', cl_list(1, strm),
			@':expected-type', @'file-stream',
			@':datum', strm);
    }

    static void
    not_an_input_stream(T_sp strm)
    {
	cl_error(9, @'simple-type-error', @':format-control',
		 make_constant_base_string("~A is not an input stream"),
		 @':format-arguments', cl_list(1, strm),
		 @':expected-type',
		 cl_list(2, @'satisfies', @'input-stream-p'),
		 @':datum', strm);
    }

    static void
    not_an_output_stream(T_sp strm)
    {
	cl_error(9, @'simple-type-error', @':format-control',
		 make_constant_base_string("~A is not an output stream"),
		 @':format-arguments', cl_list(1, strm),
		 @':expected-type', cl_list(2, @'satisfies', @'output-stream-p'),
		 @':datum', strm);
    }

    static void
    not_a_character_stream(T_sp s)
    {
	cl_error(9, @'simple-type-error', @':format-control',
		 make_constant_base_string("~A is not a character stream"),
		 @':format-arguments', cl_list(1, s),
		 @':expected-type', @'character',
		 @':datum', cl_stream_element_type(s));
    }

    static void
    not_a_binary_stream(T_sp s)
    {
	cl_error(9, @'simple-type-error', @':format-control',
		 make_constant_base_string("~A is not a binary stream"),
		 @':format-arguments', cl_list(1, s),
		 @':expected-type', @'integer',
		 @':datum', cl_stream_element_type(s));
    }

    static void
    cannot_close(T_sp stream)
    {
	file_libc_error(@[file-error], stream, "Stream cannot be closed", 0);
    }

    static void
    file_libc_error(T_sp error_type, T_sp stream,
                    const char *msg, int narg, ...)
    {
	ecl_va_list args;
	T_sp rest, error = _ecl_strerror(errno);

	ecl_va_start(args, narg, narg, 0);
	rest = cl_grab_rest_args(args);

	si_signal_simple_error(4, (T_sp)(cl_symbols + ecl_fixnum(error_type)), Cnil,
			       make_constant_base_string("~?~%C library explanation: ~A."),
			       cl_list(3, make_constant_base_string(msg), rest,
				       error));
    }

    static void
    unread_error(T_sp s)
    {
	CEerror(ECL_T, "Error when using UNREAD-CHAR on stream ~D", 1, s);
    }

    static void
    unread_twice(T_sp s)
    {
	CEerror(ECL_T, "Used UNREAD-CHAR twice on stream ~D", 1, s);
    }

    static void
    maybe_clearerr(T_sp strm)
    {
	int t = strm->stream.mode;
	if (t == ecl_smm_io || t == ecl_smm_output || t == ecl_smm_input) {
            FILE *f = IO_STREAM_FILE(strm);
            if (f != NULL) clearerr(f);
	}
    }

    static int
    restartable_io_error(T_sp strm, const char *s)
    {
	cl_env_ptr the_env = ecl_process_env();
	volatile int old_errno = errno;
	/* ecl_disable_interrupts(); ** done by caller */
	maybe_clearerr(strm);
	ecl_enable_interrupts_env(the_env);
	if (old_errno == EINTR) {
            return 1;
	} else {
            file_libc_error(@[stream-error], strm,
                            "C operation (~A) signaled an error.",
                            1, ecl_make_constant_base_string(s, strlen(s)));
            return 0;
	}
    }

    static void
    io_error(T_sp strm)
    {
	cl_env_ptr the_env = ecl_process_env();
	/* ecl_disable_interrupts(); ** done by caller */
	maybe_clearerr(strm);
	ecl_enable_interrupts_env(the_env);
	file_libc_error(@[stream-error], strm,
			"Read or write operation signaled an error", 0);
    }

    static void
    wrong_file_handler(T_sp strm)
    {
	FEerror("Internal error: stream ~S has no valid C file handler.", 1, strm.asTPtr());
    }

#ifdef ECL_UNICODE
    static cl_index
    encoding_error(T_sp stream, unsigned char *buffer, claspCharacter c)
    {
        T_sp code = _ecl_funcall4(@'ext::encoding-error', stream,
                                  cl_stream_external_format(stream),
                                  ecl_make_integer(c));
        if (Null(code)) {
            /* Output nothing */
            return 0;
        } else {
            /* Try with supplied character */
            return stream->stream.encoder(stream, buffer, ecl_char_code(code));
        }
    }

    static claspCharacter
    decoding_error(T_sp stream, unsigned char *buffer, int length)
    {
        T_sp octets = ECL_NIL, code;
        while (length > 0) {
            octets = CONS(ecl_make_fixnum(buffer[--length]), octets);
        }
        code = _ecl_funcall4(@'ext::decoding-error', stream,
                             cl_stream_external_format(stream),
                             octets);
        if (Null(code)) {
            /* Go for next character */
            return stream->stream.decoder(stream);
        } else {
            /* Return supplied character */
            return ecl_char_code(code);
        }
    }
#endif

#if defined(ECL_WSOCK)
    static void
    wsock_error( const char *err_msg, T_sp strm )
    {
	char *msg;
	T_sp msg_obj;
	/* ecl_disable_interrupts(); ** done by caller */
	{
            FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER,
                           0, WSAGetLastError(), 0, ( void* )&msg, 0, NULL );
            msg_obj = make_base_string_copy( msg );
            LocalFree( msg );
	}
	ecl_enable_interrupts();
	FEerror( err_msg, 2, strm.asTPtr(), msg_obj.asTPtr() );
    }
#endif

    void
    init_file(void)
    {
	int flags;
	T_sp standard_input;
	T_sp standard_output;
	T_sp error_output;
	T_sp aux;
	T_sp null_stream;
	T_sp external_format = ECL_NIL;
#if defined(ECL_MS_WINDOWS_HOST)
# ifdef ECL_UNICODE
	external_format = cl_list(2, @':latin-1', @':crlf');
	flags = 0;
# else
	external_format = cl_list(2, @':crlf', @':pass-through');
	flags = ECL_STREAM_DEFAULT_FORMAT;
# endif
#else
	flags = ECL_STREAM_DEFAULT_FORMAT;
#endif

	null_stream = ecl_make_stream_from_FILE(make_constant_base_string("/dev/null"),
						NULL, ecl_smm_io, 8, flags, external_format);
	generic_close(null_stream);
	null_stream = cl_make_two_way_stream(null_stream, cl_make_broadcast_stream(0));
	cl_core.null_stream = null_stream;

        /* We choose C streams by default only when _not_ using threads.
         * The reason is that C streams block on I/O operations. */
#if !defined(ECL_THREADS)
	standard_input = maybe_make_windows_console_FILE(make_constant_base_string("stdin"),
							 stdin, ecl_smm_input, 8, flags, external_format);
	standard_output = maybe_make_windows_console_FILE(make_constant_base_string("stdout"),
							  stdout, ecl_smm_output, 8, flags, external_format);
	error_output = maybe_make_windows_console_FILE(make_constant_base_string("stderr"),
						       stderr, ecl_smm_output, 8, flags, external_format);
#else
	standard_input = maybe_make_windows_console_fd(make_constant_base_string("stdin"),
						       STDIN_FILENO, ecl_smm_input_file, 8, flags,
						       external_format);
	standard_output = maybe_make_windows_console_fd(make_constant_base_string("stdout"),
							STDOUT_FILENO, ecl_smm_output_file, 8, flags,
							external_format);
	error_output = maybe_make_windows_console_fd(make_constant_base_string("stderr"),
						     STDERR_FILENO, ecl_smm_output_file, 8, flags,
						     external_format);
#endif
	cl_core.standard_input = standard_input;
        ECL_SET(@'ext::+process-standard-input+', standard_input);
	ECL_SET(@'*standard-input*', standard_input);
	cl_core.standard_output = standard_output;
        ECL_SET(@'ext::+process-standard-output+', standard_output);
	ECL_SET(@'*standard-output*', standard_output);
	ECL_SET(@'*trace-output*', standard_output);
	cl_core.error_output = error_output;
        ECL_SET(@'ext::+process-error-output+', error_output);
	ECL_SET(@'*error-output*', error_output);

	cl_core.terminal_io = aux 
            = cl_make_two_way_stream(standard_input, standard_output);

	ECL_SET(@'*terminal-io*', aux);
	aux = cl_make_synonym_stream(@'*terminal-io*');
	ECL_SET(@'*query-io*', aux);
	ECL_SET(@'*debug-io*', aux);
    }

};
namespace core
{


    
    

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
  read-byte
  write-byte
  write-char 
  input-p
  output-p
  interactive-p
  ---------   Still to be implemented:   -------
  get-position
  set-position
  close
  streamp
  open-stream-p
  element-type
*/


/********************************************************************************
 * CLOS STREAMS
 */

#ifdef CLOS_STREAMS

    static int
    clos_stream_read_byte8(T_sp strm, unsigned char* buf, int num)
    {_G();
	int i;
	for ( i=0; i<num; i++ )
	{
	    T_sp byte = eval::funcall(gray::_sym_stream_read_byte,strm);
	    if ( !af_fixnumP(byte) ) break;
	    buf[i] = byte.as<Fixnum_O>()->get();
	}
	return i;
    };





    static int
    clos_stream_write_byte8(T_sp strm, const char* c, int n)
    {_G();
	int i;
	for (i = 0; i < n; i++) {
	    T_sp byte = eval::funcall(gray::_sym_stream_write_byte, strm,Fixnum_O::create(c[i]));
	    if (!af_fixnumP(byte)) break;
	}
	return i;
    }


    static Str_sp
    clos_stream_write_string(T_sp stream, Str_sp str, Fixnum_sp start, T_sp end)
    {
        return eval::funcall(gray::_sym_stream_write_string, stream, str,start,end).as<Str_O>();
    }




    static T_sp
    clos_stream_read_byte(T_sp strm)
    {
	T_sp b = eval::funcall(gray::_sym_stream_read_byte, strm);
        if (b == kw::_sym_eof) b = _Nil<T_O>();
        return b;
    }

    static void
    clos_stream_write_byte(T_sp stream, T_sp c)
    {
	eval::funcall(gray::_sym_stream_write_byte, stream, c);
    }

    static Character_sp
    clos_stream_write_char(T_sp strm, Character_sp c)
    {_G();
        Character_sp chret = eval::funcall(gray::_sym_stream_write_char, strm, c ).as<Character_O>();
	return chret;
    }

    static void
    clos_stream_terpri(T_sp strm)
    {
        eval::funcall(gray::_sym_stream_terpri,strm);
    }

    static bool
    clos_stream_fresh_line(T_sp strm)
    {
        return eval::funcall(gray::_sym_stream_fresh_line,strm).isTrue();
    }

    static int
    clos_stream_read_char(T_sp strm)
    {
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
        unlikely_if (value < 0 || value > CHAR_CODE_LIMIT)
	{
	    // FEerror("Unknown character ~A", 1, output);
	    SIMPLE_ERROR(BF("Unknown character %s") % output );
	}
        return value;
    }

#if 0
    static brclChar
    clos_stream_write_char(T_sp strm, brclChar c)
    {
	eval::funcall(gray::_sym_stream_write_char, strm, Character_O::create(c)); // ECL_CODE_CHAR(c));
	return c;
    }
#endif
    static void
    clos_stream_unread_char(T_sp strm, brclChar c)
    {
	eval::funcall(gray::_sym_stream_unread_char, strm, Character_O::create(c)); // ECL_CODE_CHAR(c));
    }

    static int
    clos_stream_peek_char(T_sp strm)
    {
	T_sp out = eval::funcall(gray::_sym_stream_peek_char, strm);
	if (out == kw::_sym_eof) return EOF;
	Character_sp charOut = out.as<Character_O>();
	return charOut->charCode();
    }

    static int
    clos_stream_listen(T_sp strm)
    {
	return !(eval::funcall(gray::_sym_stream_listen, strm)).nilp();
    }

    
    static void
    clos_stream_clear_input(T_sp strm)
    {
	eval::funcall(gray::_sym_stream_clear_input, strm);
    }

    static void
    clos_stream_clear_output(T_sp strm)
    {
	eval::funcall(gray::_sym_stream_clear_output, strm);
	return;
    }

    static void
    clos_stream_force_output(T_sp strm)
    {
	eval::funcall(gray::_sym_stream_force_output, strm);
    }

    static void
    clos_stream_finish_output(T_sp strm)
    {
	eval::funcall(gray::_sym_stream_finish_output, strm);
    }

    static bool
    clos_stream_input_p(T_sp strm)
    {
        IMPLEMENT_MEF(BF("These should be generic functions!!!"));
	return !(eval::funcall(gray::_sym_input_stream_p, strm)).nilp();
    }

    static bool
    clos_stream_output_p(T_sp strm)
    {
        IMPLEMENT_MEF(BF("These should be generic functions!!!"));
	return !(eval::funcall(gray::_sym_output_stream_p, strm)).nilp();
    }

    static bool
    clos_stream_interactive_p(T_sp strm)
    {
	return !(eval::funcall(gray::_sym_stream_interactive_p, strm)).nilp();

    }

    static T_sp
    clos_stream_element_type(T_sp strm)
    {
	return eval::funcall(gray::_sym_stream_element_type, strm);
    }

#if 1
    // There doesn't seem to be a grey stream function for file-length
    static T_sp
    clos_stream_fileLength(T_sp strm)
    {
        SYMBOL_EXPORT_SC_(ClPkg,fileLength);
	TYPE_ERROR(strm,cl::_sym_fileLength);
    }
#endif


    static T_sp
    clos_stream_get_position(T_sp strm)
    {
	return eval::funcall(gray::_sym_stream_file_position, strm);
    }

    static T_sp
    clos_stream_set_position(T_sp strm, T_sp pos)
    {
	return eval::funcall(gray::_sym_stream_file_position, strm, pos);
    }

    static int
    clos_stream_column(T_sp strm)
    {
	T_sp col = eval::funcall(gray::_sym_stream_line_column, strm);
	/* FIXME! The Gray streams specifies NIL is a valid
	 * value but means "unknown". Should we make it
	 * zero? */
	Fixnum_sp fncol = col.as<Fixnum_O>();
	return (col.nilp())? 0 : fncol->get();
    }

    static T_sp
    clos_stream_close(T_sp strm, T_sp abort)
    {
	return eval::funcall(gray::_sym_close, strm, abort);
    }


#endif // CLOS_STREAMS


/*---------------------------------------------------
  ---------------------------------------------------
  ---------------------------------------------------
  
  BRCL stream routines - these dispatch to Gray streams
  or regular streams depending on the stream type.

*/


    Str_sp clasp_writeString(Str_sp str, T_sp stream, int istart, Fixnum_sp end)
    {
#ifdef CLOS_STREAMS
	if ( stream.pointerp() && stream->instancep() )
        {
            Fixnum_sp fnstart(istart);
            return clos_stream_write_string(stream,str,fnstart,end);
        }
#endif
        Stream_sp ostream = coerce::outputStreamDesignator(stream).as<Stream_O>();
	int iend = cl_length(str);
	if ( end.notnilp() )
	{
	    iend = MIN(iend,end.as<Fixnum_O>()->get());
	}
        int ilen = iend-istart;
        ostream->writeBytes(&(str->get().c_str()[istart]),ilen);
        return str;
    }



    int brcl_write_byte8(T_sp strm, const char* c, int n)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_write_byte8(strm,c,n);
	}
#endif
	Stream_sp ostrm = coerce::outputStreamDesignator(strm);
	return ostrm->writeBytes(c,n);
    }

    int brcl_read_byte8(T_sp strm, unsigned char* c, int n)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_read_byte8(strm,c,n);
	}
#endif
	Stream_sp istrm = coerce::inputStreamDesignator(strm);
	return istrm->read(c,n);
    }



    T_sp brcl_read_byte(T_sp strm)
    {
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_read_byte(strm);
	}
#endif
	Stream_sp istrm = coerce::inputStreamDesignator(strm);
        int c = istrm->get();
        if ( c == EOF ) return kw::_sym_eof;
        return Integer_O::create(c);
    }



    void brcl_write_byte(T_sp strm, Integer_sp byte)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    clos_stream_write_byte(strm,byte);
	    return;
	}
#endif
	Stream_sp ostrm = coerce::outputStreamDesignator(strm);
	ostrm->writeByte(byte);
    }


    void clasp_writeChar(T_sp strm, Character_sp chr)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    clos_stream_write_char(strm,chr);
	    return;
	}
#endif
	Stream_sp ostrm = coerce::outputStreamDesignator(strm);
	ostrm->writeChar(chr->asChar());
    }

    void brcl_terpri(T_sp strm)
    {
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    clos_stream_terpri(strm);
	    return;
	}
#endif
	Stream_sp ostrm = coerce::outputStreamDesignator(strm);
	ostrm->writeChar('\n');
    }        




    bool clasp_freshLine(T_sp strm)
    {
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_fresh_line(strm);
	}
#endif
	Stream_sp outputStream = coerce::outputStreamDesignator(strm);
	if ( !outputStream->atStartOfLine() )
	{
	    outputStream->writeln("");
            return true;
	}
        return false;
    }        







    T_sp clasp_streamElementType(T_sp strm)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_element_type(strm);
	}
#endif
        IMPLEMENT_MEF(BF("Implement stream-element-type for normal streams"));
    }








    bool brcl_input_stream_p(T_sp strm)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_input_p(strm);
	}
#endif
	Stream_sp istrm = coerce::inputStreamDesignator(strm);
        return istrm->inputStreamP();
    }


    bool brcl_output_stream_p(T_sp strm)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_output_p(strm);
	}
#endif
	Stream_sp ostrm = coerce::outputStreamDesignator(strm);
        return ostrm->outputStreamP();
    }



    bool brcl_interactive_stream_p(T_sp strm)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_interactive_p(strm);
	}
#endif
        if ( Stream_sp sstrm = strm.asOrNull<Stream_O>() ) {
            return sstrm->interactiveStreamP();
        }
        return false;
    }







    void brcl_unread_char(T_sp strm, brclChar ch)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_unread_char(strm,ch);
	}
#endif
	Stream_sp istrm = coerce::inputStreamDesignator(strm);
	istrm->unread_char(ch);
    };





    void brcl_finish_output(T_sp strm)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_finish_output(strm);
	}
#endif
	Stream_sp ostrm = coerce::outputStreamDesignator(strm);
	return ostrm->finishOutput();
    };

    void clasp_forceOutput(T_sp strm)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_force_output(strm);
	}
#endif
	Stream_sp ostrm = coerce::outputStreamDesignator(strm).as<Stream_O>();
	return ostrm->forceOutput();
    };


    Integer_sp clasp_fileLength(T_sp strm)
    {
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_fileLength(strm);
	}
#endif
        Stream_sp ostrm = strm.as<Stream_O>();
	return ostrm->fileLength();
    };



    int brcl_column(T_sp strm)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_column(strm);
	}
#endif
	Stream_sp ostrm = coerce::outputStreamDesignator(strm);
	return ostrm->column();
    };



    int brcl_peek_char(T_sp strm)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_peek_char(strm);
	}
#endif
	Stream_sp istrm = coerce::inputStreamDesignator(strm);
	return istrm->peek_char();
    };


    int brcl_read_char(T_sp strm)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_read_char(strm);
	}
#endif
	Stream_sp istrm = coerce::inputStreamDesignator(strm);
	return istrm->get();
    };



    bool brcl_listen(T_sp strm)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_listen(strm);
	}
#endif
	Stream_sp istrm = coerce::inputStreamDesignator(strm);
	return istrm->listen()!=0;
    };



    void brcl_clear_input(T_sp strm)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    clos_stream_clear_input(strm);
	    return;
	}
#endif
	Stream_sp istrm = coerce::inputStreamDesignator(strm);
	istrm->clearInput();
    }

    void brcl_clear_output(T_sp strm)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    clos_stream_clear_output(strm);
	    return;
	}
#endif
	Stream_sp ostrm = coerce::outputStreamDesignator(strm);
	ostrm->clearOutput();
    }



T_sp brcl_close(T_sp strm, bool abort)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
            T_sp tabort = _lisp->_boolean(abort);
	    return clos_stream_close(strm,tabort);
	}
#endif
        if ( Stream_sp sstrm = strm.asOrNull<Stream_O>() ) {
            return sstrm->close(abort);
        }
        SIMPLE_ERROR(BF("You cannot close the stream %s") % _rep_(strm));
    };


    T_sp brcl_stream_get_position(T_sp strm)
    {
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_get_position(strm);
	}
#endif
        if ( Stream_sp sstrm = strm.asOrNull<Stream_O>() ) {
            return Integer_O::create(sstrm->tell());
        }
        SIMPLE_ERROR(BF("You cannot get position of the non-stream %s") % _rep_(strm));
    }


    T_sp brcl_stream_set_position(T_sp strm, Integer_sp pos)
    {
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_set_position(strm,pos);
	}
#endif
        if ( Stream_sp sstrm = strm.asOrNull<Stream_O>() ) {
            LongLongInt lli = pos->as_LongLongInt();
            sstrm->seek(lli);
            return _lisp->_true();
        }
        SIMPLE_ERROR(BF("You cannot set position of the non-stream %s") % _rep_(strm));
    }



#define ARGS_cl_filePosition "(stream &optional pos)"
#define DECL_cl_filePosition ""
#define DOCS_cl_filePosition "filePosition"
    T_sp cl_filePosition(T_sp stream, T_sp pos)
    {_G();
        if ( pos.nilp() ) // get-position form
        {
            return brcl_stream_get_position(stream);
        } else { // set-position form
            return brcl_stream_set_position(stream,pos.as<Integer_O>());
        }
    };





#define ARGS_af_writeByte "(byte &optional output-stream)"
#define DECL_af_writeByte ""
#define DOCS_af_writeByte "writeByte"
    Integer_sp af_writeByte(Integer_sp byte, T_sp stream)
    {_G();
        brcl_write_byte(stream,byte);
	return(byte);
    };

#define ARGS_af_writeChar "(string &optional output-stream)"
#define DECL_af_writeChar ""
#define DOCS_af_writeChar "writeChar"
    Character_sp af_writeChar(Character_sp chr, T_sp outputStream)
    {_G();
        clasp_writeChar(outputStream, chr);
        return chr;
    };


    
    
#define ARGS_af_readByte8 "(stream buffer size)"
#define DECL_af_readByte8 ""
#define DOCS_af_readByte8 "readByte8"
    int af_readByte8(T_sp strm, unsigned char* c, int n)
    {_G();
        return brcl_read_byte8(strm,c,n);
    }





    
    
#define ARGS_cl_streamp "(arg)"
#define DECL_cl_streamp ""
#define DOCS_cl_streamp "streamp"
    bool cl_streamp(T_sp obj)
    {_G();
        if ( obj.nilp() ) return false;
#ifdef CLOS_STREAMS
        if ( obj->instancep() ) {
            return eval::funcall(gray::_sym_streamp,obj).isTrue();
        }
#endif
        if (Stream_sp sobj = obj.asOrNull<Stream_O>() ) {
            return true;
        }
        return false;
    };

    


#define ARGS_af_inputStreamP "(stream)"
#define DECL_af_inputStreamP ""
#define DOCS_af_inputStreamP "inputStreamP"
    int af_inputStreamP(T_sp strm)
    {_G();
        return brcl_input_stream_p(strm);
    }

#define ARGS_af_outputStreamP "(stream)"
#define DECL_af_outputStreamP ""
#define DOCS_af_outputStreamP "outputStreamP"
    int af_outputStreamP(T_sp strm)
    {_G();
        return brcl_output_stream_p(strm);
    }


#define ARGS_af_interactiveStreamP "(stream)"
#define DECL_af_interactiveStreamP ""
#define DOCS_af_interactiveStreamP "interactiveStreamP"
    int af_interactiveStreamP(T_sp strm)
    {_G();
        return brcl_interactive_stream_p(strm);
    }




    
    
#define ARGS_af_fileColumn "(arg)"
#define DECL_af_fileColumn ""
#define DOCS_af_fileColumn "column"
    T_sp af_fileColumn(T_sp ostrm)
    {_G();
	Stream_sp strm = coerce::outputStreamDesignator(ostrm);
	return Fixnum_O::create(strm->outputColumn());
    };


    
    
#define ARGS_af_unread_char "(char &optional strm)"
#define DECL_af_unread_char ""
#define DOCS_af_unread_char "unread_char"
    void af_unread_char(Character_sp ch, T_sp dstrm)
    {_G();
        brcl_unread_char(dstrm,ch->asChar());
    };
    

    
    
#define ARGS_af_peekChar "(&optional peek_type strm (eof_errorp t) eof_value recursivep)"
#define DECL_af_peekChar ""
#define DOCS_af_peekChar "peekChar"
    T_sp af_peekChar(T_sp peek_type, T_sp strm, T_sp eof_errorp, T_sp eof_value, T_sp recursive_p)
    {_G();
	if ( !brcl_input_stream_p(strm) ) SIMPLE_ERROR(BF("Not input-stream"));
	if ( peek_type.nilp() ) {
	    int c = brcl_peek_char(strm);
	    if ( c == EOF ) goto HANDLE_EOF;
	    return Character_O::create(brcl_peek_char(strm));
	}
	if ( af_characterP(peek_type) )
	{
	    int looking_for = peek_type.as<Character_O>()->charCode();
	    while (1)
	    {
		int c = brcl_peek_char(strm);
		if ( c == EOF ) goto HANDLE_EOF;
		if ( c == looking_for ) return Character_O::create(c);
		brcl_read_char(strm);
	    }
	}
	// Now peek_type is true - this means skip whitespace until the first non-whitespace character
	if ( peek_type !=_lisp->_true() ) {
	    SIMPLE_ERROR(BF("Illegal first argument for PEEK-CHAR %s") % _rep_(peek_type));
	} else {
	    ReadTable_sp readtable = cl::_sym_STARreadtableSTAR->symbolValue().as<ReadTable_O>();
	    while (1)
	    {
		int c = brcl_peek_char(strm);
		if ( c == EOF ) goto HANDLE_EOF;
		Character_sp charc = Character_O::create(c);
		if ( readtable->syntax_type(charc) != kw::_sym_whitespace_character) return charc;
		brcl_read_char(strm);
	    }
	}
    HANDLE_EOF:
	if ( eof_errorp.isTrue() ) END_OF_FILE_ERROR(strm);
	return eof_value;
    }



    
    
#define ARGS_af_readChar "(&optional strm eof_error_p eof_value recursive_p)"
#define DECL_af_readChar ""
#define DOCS_af_readChar "readChar"
    T_sp af_readChar(T_sp strm, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p)
    {_G();
        int c = brcl_read_char(strm);
	if ( c == EOF )
	{
	    LOG(BF("Hit eof"));
	    if ( !eof_error_p.isTrue() )
	    {
		LOG(BF("Returning eof_value[%s]") % _rep_(eof_value) );
		return eof_value;
	    }
	    END_OF_FILE_ERROR(strm);
	}
	LOG(BF("Read and returning char[%s]") % c );
	return StandardChar_O::create(c);
    }

    
	
	
#define ARGS_af_clearInput "(&optional dstrm)"
#define DECL_af_clearInput ""
#define DOCS_af_clearInput "clearInput"
    void af_clearInput(T_sp dstrm)
    {_G();
	brcl_clear_input(dstrm);
    }


#define ARGS_af_clearOutput "(&optional dstrm)"
#define DECL_af_clearOutput ""
#define DOCS_af_clearOutput "clearOutput"
    void af_clearOutput(T_sp dstrm)
    {_G();
	brcl_clear_output(dstrm);
    }



#define ARGS_af_listen "(&optional dstrm)"
#define DECL_af_listen ""
#define DOCS_af_listen "listen"
    bool af_listen(T_sp strm)
    {_G();
	return brcl_listen(strm);
    }



    
    
#define ARGS_af_force_output "(&optional strm)"
#define DECL_af_force_output ""
#define DOCS_af_force_output "force_output"
    void af_force_output(T_sp ostrm)
    {_G();
	clasp_forceOutput(ostrm);
    };


#define ARGS_af_finish_output "(&optional strm)"
#define DECL_af_finish_output ""
#define DOCS_af_finish_output "finish_output"
    void af_finish_output(T_sp ostrm)
    {_G();
	brcl_finish_output(ostrm);
    };





    
    
#define ARGS_af_writeString "(string &optional output-stream &key (start 0) end)"
#define DECL_af_writeString ""
#define DOCS_af_writeString "writeString"
    Str_sp af_writeString(Str_sp str, T_sp stream, int start, Fixnum_sp end)
    {_G();
        return clasp_writeString(str,stream,start,end);
    };


#define ARGS_af_writeLine "(string &optional output-stream &key (start 0) end)"
#define DECL_af_writeLine ""
#define DOCS_af_writeLine "writeLine"
    String_sp af_writeLine(Str_sp str, T_sp stream, Fixnum_sp start, T_sp end)
    {_G();
        clasp_writeString(str,stream,start->get(),end);
        brcl_terpri(stream);
        return str;
    };



    
#define ARGS_af_terpri "(&optional (output-stream ext:+process-standard-output+))"
#define DECL_af_terpri ""
#define DOCS_af_terpri "Send a newline to the output stream"
    void af_terpri(T_sp outputStreamDesig)
    {_G();
        brcl_terpri(outputStreamDesig);
    };


    
    
#define ARGS_af_freshLine "(&optional (outputStream ext:+process-standard-output+))"
#define DECL_af_freshLine ""
#define DOCS_af_freshLine "freshLine"
    bool af_freshLine(T_sp outputStreamDesig)
    {_G();
        return clasp_freshLine(outputStreamDesig);
    };







#define ARGS_af_read_from_string "(content &optional eof-error-p eof-value &key (start 0) (end -1) preserve-whitespace)"
#define DECL_af_read_from_string ""
#define DOCS_af_read_from_string "read_from_string"
    T_mv af_read_from_string(Str_sp content, T_sp eof_error_p, T_sp eof_value, Fixnum_sp start, Fixnum_sp end, T_sp preserve_whitespace )
    {_G();
	bool eofErrorP = eof_error_p.isTrue();
	int istart = start->as_int();
	int iend;
	if ( end.nilp() ) iend = content->get().size();
	else iend = end->as_int();
	bool preserveWhitespace = preserve_whitespace.isTrue();
	if ( preserveWhitespace )
	{
	    printf("Implement preserve-whitespace\n");
	    IMPLEMENT_ME(); // handle this
	}
	StringInputStream_sp sin = StringInputStream_O::create(content->get().substr(istart,iend-istart));
	if ( iend-istart == 0 )
	{
	    if (eofErrorP )
	    {
		END_OF_FILE_ERROR(sin);
	    } else
	    {
		return(Values(eof_value,_lisp->_true()));
	    }
	}
	LOG(BF("Seeking to position: %d") % start);
	LOG(BF("Character at position[%d] is[%c/%d]") % sin->tell() % (char)sin->peek_char() % (int)sin->peek_char());
#if 0
	Reader_sp reader = Reader_O::create(sin,_lisp);
	T_sp res = reader->read(false,_lisp->_eof());
#endif
	T_sp res = read_lisp_object(sin,false,_Unbound<T_O>(),false);
	if ( res.unboundp() )
	{
	    if (eofErrorP )
	    {
		END_OF_FILE_ERROR(sin);
	    } else
	    {
		return(Values(eof_value,_lisp->_true()));
	    }
	}	
	return(Values(res,Fixnum_O::create(sin->tell())));
    }







#define ARGS_af_read_line "(&optional input-stream (eof-error-p t) eof-value recursive-p)"
#define DECL_af_read_line ""    
#define	DOCS_af_read_line "See clhs"
    T_mv af_read_line(T_sp sin, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p)
    {_G();
	bool eofErrorP = eof_error_p.isTrue();
//    bool recursiveP = translate::from_object<bool>::convert(env->lookup(_sym_recursive_p));
	if ( sin.nilp() ) sin = cl::_sym_STARstandard_inputSTAR->symbolValue();
	stringstream sbuf;
	while (1)
	{
	    Character_sp ch = af_readChar(sin,_Nil<T_O>(),_Nil<T_O>(),recursive_p).as<Character_O>();
	    if ( ch.nilp() ) {
		if ( eofErrorP ) {
		    END_OF_FILE_ERROR(sin);
		} else {
		    return(Values(eof_value/*Str_O::create(sbuf.str())*/,_lisp->_true()));
		}
	    } else {
		char cc = ch->get();
		if ( cc == '\n') {
		    break;
		} else if ( cc == '\r' ) {
		    if ( brcl_peek_char(sin) == '\n') {
			brcl_read_char(sin);
		    }
		    break;
		}
		sbuf << cc;
	    }
	}
	LOG(BF("Read line result -->[%s]") % sbuf.str() );
	return(Values(Str_O::create(sbuf.str()),_Nil<T_O>()));
    }





    
    
#define ARGS_cl_close "(stream &key abort)"
#define DECL_cl_close ""
#define DOCS_cl_close "close"
    T_sp cl_close(T_sp stream, bool abort)
    {_G();
        return brcl_close(stream,abort);
    };





    bool Stream_O::freshLine()
    {
        if ( this->outputStreamP() ) {
            if ( !this->atStartOfLine() )
            {
                this->writeln("");
                return true;
            }
            return false;
        }
        SIMPLE_ERROR(BF("You cannot call freshline on an input stream"));
    }



    T_sp Stream_O::close(bool abort)
    {_G();
	SUBCLASS_MUST_IMPLEMENT();
    }


    Pathname_sp Stream_O::pathname() const {
	return this->sourceFileInfo()->pathname();
    }

    int Stream_O::read(unsigned char* buffer, int num)
    {
	int numAvailable = this->listen();
	if ( numAvailable < num ) num = numAvailable;
	while (num) {
	    char c = this->get();
	    *buffer = c;
	    ++buffer;
	    --num;
	}
	return num;
    }

    int Stream_O::writeBytes(const char* c, int n)
    {
	for (const char* cp = c; n>0; --n)
	{
	    this->writeChar(static_cast<char>(*cp));
	    ++cp;
	}
	return n;
    }

    Fixnum_sp Stream_O::writeVector(Vector_sp vec, Fixnum_sp fnstart, Fixnum_sp fnend)
    {
	size_t start = fnstart->get();
	size_t end = fnend.nilp() ? vec->length() : fnend->get();
	if ( start >= end ) {
	    return fnstart;
	}
	T_sp elementType = vec->elementType();
	if (elementType == cl::_sym_Character_O
	    || elementType == cl::_sym_BaseChar_O
	    
#ifdef BRCL_UNICODE
	    // elementType == cl::_sym_UnicodeCharacter???
#endif
	    /* TODO: Handle specialized arrays */
	    ) {
	    for (; start < end; start++) {
		this->writeChar(vec->elt(start).as<Character_O>()->asChar());
	    }
	} else {
	    this->writeByte(vec->elt(start).as<Integer_O>());
	}
	return Fixnum_O::create(static_cast<int>(start));
    };
	    

	    void Stream_O::writeStr(const string& str)
    {
	for (const char* cp = str.c_str(); *cp; ++cp )
	{
	    this->writeChar(static_cast<char>(*cp));
	}
    }



    void Stream_O::clearInput()
    {
	if ( this->inputStreamP() )
	{
	    SIMPLE_ERROR(BF("Add support for clearInput for %s") % _rep_(this) );
	}
	SIMPLE_ERROR(BF("clear-input not supported on output-stream"));
    }



    EXPOSE_CLASS(core,Stream_O);



    void Stream_O::exposeCando(Lisp_sp lisp)
    {_G();
	class_<Stream_O>()
	    ;
    }

    void Stream_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Stream,"","",_lisp)
	    ;
#endif
    }






    EXPOSE_CLASS(core,AnsiStream_O);



    void AnsiStream_O::lisp_initGlobals(Lisp_sp lisp)
    {_G();
    }




    int AnsiStream_O::get()
    {_G();
	if ( !this->inputStreamP() ) SIMPLE_ERROR(BF("Not input-stream"));
	// get a raw character
	int c = this->_get();
	if ( c == EOF ) return EOF;
	if ( c == '\n' )
	{
	    this->advanceLineNumber();
	} else if ( c == '\r' )
	{
	    if ( this->peek_char() == '\n')
	    {
		c = this->_get();
	    }
	    this->advanceLineNumber();
	} else
	{
	    this->advanceColumn();
	}
	return c;
    }










    void AnsiStream_O::unread_char(brclChar cc)
    {_OF();
	this->putback(cc);
    }





    void AnsiStream_O::exposeCando(Lisp_sp lisp)
    {_G();
	class_<AnsiStream_O>()
	    ;
    }

    void AnsiStream_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,Stream,"","",_lisp)
	    ;
#endif
    }




// ------------------------------------------------------------


    
    EXPOSE_CLASS(core,FileStream_O);
    
    void FileStream_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<FileStream_O>()
	    ;
    }
    
    void FileStream_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,FileStream,"","",_lisp)
	    ;
#endif
    }





    EXPOSE_CLASS(core,FileInStream_O);
    void FileInStream_O::initialize()
    {
	this->Base::initialize();
    }


#define	ARGS_FileInStream_O_make "(self fileName)"
#define DECL_FileInStream_O_make ""
#define DOCS_FileInStream_O_make ""
    FileInStream_sp FileInStream_O::make(T_sp fileName)
    {_G();
        GC_ALLOCATE(FileInStream_O,fin );
	Pathname_sp pn = cl_pathname(fileName);
	fin->_SourceFileInfo = af_sourceFileInfo(pn);
	fin->_InStream.open(fin->_SourceFileInfo->fileName().c_str());
	return fin;
    }



    void FileInStream_O::exposeCando(Lisp_sp lisp)
    {
	class_<FileInStream_O>()
	    ;
	Defun_maker(CorePkg,FileInStream);
//	af_def(CurrentPkg,"make-FileInStream",&FileInStream_O::make);
    }

    void FileInStream_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,FileInStream,"","",_lisp)
	    ;
#endif
    }

    FileInStream_sp FileInStream_O::create(Pathname_sp path)
    {_G();
        GC_ALLOCATE(FileInStream_O,fin );
	Str_sp truename = af_coerceToFilename(af_truename(path));
	fin->_SourceFileInfo = af_sourceFileInfo(truename);
	LOG(BF("Opening file[%s]") % fin->_SourceFileInfo->fileName());
	fin->_InStream.open(truename->c_str());
	if ( !fin->_InStream.good() )
	{
	    FILE_ERROR(fin->_SourceFileInfo);
	}
	return fin;
    }


    string FileInStream_O::__repr__() const
    {
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " :sourceFileInfo " << this->sourceFileInfo()->fileName() << " :line-number " << this->lineNumber() << " >";
	return ss.str();
    }

    void FileInStream_O::clearInput()
    {
	this->_Unput.clear();
	this->_InStream.ignore( std::numeric_limits<std::streamsize>::max(), EOF );
    }


#if 0
    void FileInStream_O::readLine(string& buf, bool& hitEof )
    {
	stringstream sstore;
    }
#endif
    string FileInStream_O::readEntireFile()
    {
	this->_Unput.clear();
	this->_InStream.seekg(0,std::ios_base::end);
	LongLongInt fileSize = this->_InStream.tellg();
	this->_InStream.seekg(0,std::ios_base::beg);
        string fileContents;
        char* buffer = (char*)malloc(fileSize+1);
        try {
            for ( LongLongInt i=0; i<fileSize; i++ )
            {
                buffer[i] = this->get(); // read file and keep track of line numbers
            }
            buffer[fileSize] = '\0';
            fileContents = buffer;
            free(buffer);
        } catch (...) {
            free(buffer);
        }
	return fileContents;
    }

    LongLongInt FileInStream_O::tell()
    {
      return (LongLongInt)(this->_InStream.tellg())+this->_Unput.tellg();
    }

    void FileInStream_O::seek(LongLongInt pos)
    {
	this->_Unput.clear();
	this->_InStream.seekg(pos,std::ios_base::beg);
	this->_Cursor.invalidate();
    }


    LongLongInt FileInStream_O::fileSize()
    {
        std::streampos pos = this->_InStream.tellg();
	this->_InStream.seekg(0,std::ios_base::end);
	LongLongInt fileSize = this->_InStream.tellg();
	this->_InStream.seekg(pos,std::ios_base::beg);
	return fileSize;
    }



    T_sp FileInStream_O::close(bool abort)
    {_OF();
	if ( abort )
	{
//	    _lisp->print(BF("-----Close for %s with abort=true called - I don't currently support abort=true\n") % this->_instanceClass()->classNameAsString() );
	}
	this->_InStream.close();
        return _lisp->_true();
    }


#if 0
    int FileInStream_O::read(unsigned char* buf, int size)
    {
	this->_InStream.read((char*)buf,size);
	this->_Cursor.invalidate();
	return this->_InStream.gcount();
    }
#endif

    LongLongInt FileInStream_O::gcount()
    {
	return this->_InStream.gcount();
    }

    void FileInStream_O::putback(char c)
    {
	this->_Unput.updateCursor(this->_Cursor);
	this->_Unput.putback(c);
    }

    int FileInStream_O::peek_char()
    {
	return this->_Unput.has_unput_char() ? this->_Unput.peek_char() : this->_InStream.peek();
    }

    int FileInStream_O::_get()
    {
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



    bool FileInStream_O::good() const
    {
	return this->_InStream.good();
    }

    bool FileInStream_O::eof() const
    {
	return this->_InStream.eof();
    }

    int FileInStream_O::listen()
    {
        std::streampos current=this->_InStream.tellg();
	this->_InStream.seekg(0,std::ios_base::end);
	int bytes_left= this->_InStream.tellg() - current
	    + (this->_Unput.has_unput_char() ? 1 : 0 );
	this->_InStream.seekg(current,std::ios_base::beg);
	return bytes_left;
    }








    EXPOSE_CLASS(core,FileOutStream_O);

    void FileOutStream_O::exposeCando(Lisp_sp lisp)
    {
	class_<FileOutStream_O>()
	    ;
    }

    void FileOutStream_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON //[
	PYTHON_CLASS(CorePkg,FileOutStream,"","",_lisp)
	    ;
#endif //]
    }


    FileOutStream_sp FileOutStream_O::create(Pathname_sp currentPath, std::ios_base::openmode mode)
    {_G();
        GC_ALLOCATE(FileOutStream_O,fout );
	    fout->_ActivePathname = currentPath;
	    fout->_RenameToOriginalOnClose = false;
	    fout->_OriginalPathname = _Nil<Pathname_O>();
	    Str_sp truename = af_coerceToFilename(af_truename(currentPath));
	    fout->_Stream.open(truename->c_str(),mode);
            if ( (mode & std::ios_base::ate) != 0 )
	{
	    fout->_Stream.seekp(0,std::ios_base::end);
#ifdef DEBUG_ON
	    LongLongInt pos = fout->_Stream.tellp();
	    LOG(BF("Seek to end of file puts me at: %u") % pos);
#endif
	}
	return fout;
    }

    FileOutStream_sp FileOutStream_O::createTemporary(Pathname_sp temporaryPath, Pathname_sp originalPath, std::ios_base::openmode mode)
    {
        GC_ALLOCATE(FileOutStream_O,fout );
	fout->_ActivePathname = temporaryPath;
	fout->_OriginalPathname = originalPath;
	fout->_RenameToOriginalOnClose = true;
	Str_sp truename = af_coerceToFilename(af_truename(temporaryPath));
	fout->_Stream.open(truename->c_str(),mode);
	return fout;
    }



    void	FileOutStream_O::initialize()
    {
	this->Base::initialize();
    }

    uint FileOutStream_O::outputColumn() const
    {
	return this->_OutputCursor.column();
    }



    void FileOutStream_O::writeChar(char c)
    {
	this->_Stream << c;
	if ( c == '\n' || c == '\r' )
	    this->_OutputCursor.advanceLineNumber();
	else
	    this->_OutputCursor.advanceColumn();
    }

    void	FileOutStream_O::writeln(const string& s)
    {
	this->_Stream << s << std::endl;
	this->_OutputCursor.advanceLineNumber();
    }

    bool FileOutStream_O::atStartOfLine() const
    {
	return this->_OutputCursor.atStartOfLine();
    }

    T_sp FileOutStream_O::close(bool abort)
    {_G();
	this->_Stream.close();
	if ( !abort )
	{
	    // We are supposed to rename the ActiveFilePath to the OriginalFilePath
	    if ( this->_RenameToOriginalOnClose )
	    {
		af_renameFile(this->_ActivePathname,this->_OriginalPathname,kw::_sym_supersede);
	    }
	}
        return _lisp->_true();
    }




    EXPOSE_CLASS(core,FileInCompressedStream_O);

    void FileInCompressedStream_O::initialize()
    {
	this->Base::initialize();
    }

    void FileInCompressedStream_O::exposeCando(Lisp_sp lisp)
    {
	class_<FileInCompressedStream_O>()
	    ;
    }

    void FileInCompressedStream_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,FileInCompressedStream,"","",_lisp)
	    ;
#endif
    }

    FileInCompressedStream_sp FileInCompressedStream_O::createGzip(Pathname_sp path)
    {
        GC_ALLOCATE(FileInCompressedStream_O,fin );
	Str_sp truename = af_coerceToFilename(af_truename(path));
	fin->_SourceFileInfo = af_sourceFileInfo(truename);
	fin->_RawStream.open(truename->c_str(),std::ios_base::in | std::ios_base::binary);
	fin->_In.push(boost::iostreams::gzip_decompressor());
	fin->_In.push(fin->_RawStream);
	return fin;
    }

    void FileInCompressedStream_O::clearInput()
    {
	this->_Unput.clear();
	this->_RawStream.ignore ( std::numeric_limits<std::streamsize>::max(), EOF );
    }

    LongLongInt FileInCompressedStream_O::fileSize()
    {
        std::streampos pos = this->_RawStream.tellg();
	this->_RawStream.seekg(0,std::ios_base::end);
	LongLongInt fileSize = this->_RawStream.tellg();
	this->_RawStream.seekg(pos,std::ios_base::beg);
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


    int FileInCompressedStream_O::_get()
    {
	return this->_Unput.has_unput_char() ? this->_Unput.get_char() : this->_In.get();

    }
    void FileInCompressedStream_O::putback(char c)
    {
	this->_Unput.updateCursor(this->_Cursor);
	this->_Unput.putback(c);
    }

    int FileInCompressedStream_O::peek_char()
    {
	return this->_Unput.has_unput_char() ? this->_Unput.peek_char() : this->_In.peek();
    }

    LongLongInt FileInCompressedStream_O::gcount()
    {
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


    bool FileInCompressedStream_O::good() const
    {
	return this->_In.good();
    }

    bool FileInCompressedStream_O::eof() const
    {
	return this->_In.eof();
    }

    int FileInCompressedStream_O::listen()
    {
        std::streampos current = this->_In.tellg();
	this->_In.seekg(0,std::ios_base::end);
	int bytes_left= this->_In.tellg() - current;
	this->_In.seekg(current,std::ios_base::beg);
	return bytes_left + (this->_Unput.has_unput_char() ? 1 : 0 );

    }


    T_sp FileInCompressedStream_O::close(bool abort)
    {_OF();
	if ( abort )
	{
//	    _lisp->print(BF("-----Close for %s with abort=true called - I don't currently support abort=true\n") % this->_instanceClass()->classNameAsString() );
	}
	this->_In.pop();
	this->_In.pop();
        return _lisp->_true();
    }





    EXPOSE_CLASS(core,FileOutCompressedStream_O);

    void FileOutCompressedStream_O::initialize()
    {
	this->Base::initialize();
    }

    void FileOutCompressedStream_O::exposeCando(Lisp_sp lisp)
    {
	class_<FileOutCompressedStream_O>()
	    ;
    }

    void FileOutCompressedStream_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,FileOutCompressedStream,"","",_lisp)
	    ;
#endif
    }

    FileOutCompressedStream_sp FileOutCompressedStream_O::createGzip(Pathname_sp path)
    {
        GC_ALLOCATE(FileOutCompressedStream_O,fout );
	Str_sp truename = af_coerceToFilename(af_truename(path));
	fout->_ActivePathname = path;
	fout->_RenameActiveToOriginalOnClose = false;
	fout->_OriginalPathname = _Nil<Pathname_O>();
	fout->_RawStream.open(truename->get(),std::ios_base::out | std::ios_base::binary);
	fout->_Out.push(boost::iostreams::gzip_compressor());
	fout->_Out.push(fout->_RawStream);
	return fout;
    }


    FileOutCompressedStream_sp FileOutCompressedStream_O::createGzipTemporary(Pathname_sp activePath, Pathname_sp originalPath)
    {
        GC_ALLOCATE(FileOutCompressedStream_O,fout );
	fout->_ActivePathname = activePath;
	fout->_RenameActiveToOriginalOnClose = true;
	fout->_OriginalPathname = originalPath;
	Str_sp truename = af_coerceToFilename(af_truename(activePath));
	fout->_RawStream.open(truename->c_str(),std::ios_base::out | std::ios_base::binary);
	fout->_Out.push(boost::iostreams::gzip_compressor());
	fout->_Out.push(fout->_RawStream);
	return fout;
    }




    void FileOutCompressedStream_O::flush()
    {_OF();
	this->_Out.flush();
    }

    uint FileOutCompressedStream_O::column() const
    {
	return this->_Cursor.column();
    }

    void FileOutCompressedStream_O::writeChar(char c)
    {_OF();
	this->_Out << c;
	if (c == '\n' || c == '\r')
	    this->_Cursor.advanceLineNumber();
	else
	    this->_Cursor.advanceColumn();
    }



    void FileOutCompressedStream_O::writeln(string const& sbuf)
    {
	this->_Out << sbuf << std::endl;
	this->_Cursor.advanceLineNumber();
    }

    bool FileOutCompressedStream_O::atStartOfLine() const
    {
	return this->_Cursor.atStartOfLine();
    }


    T_sp FileOutCompressedStream_O::close(bool abort)
    {_OF();
	this->_Out.pop();
	this->_Out.pop();
	if ( !abort )
	{
	    if ( this->_RenameActiveToOriginalOnClose)
	    {
		af_renameFile(this->_ActivePathname,this->_OriginalPathname);
	    }
	}
        return _lisp->_true();
    }









    
    EXPOSE_CLASS(core,StringStream_O);

    
    void StringStream_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<StringStream_O>()
	    ;
    }
    
    void StringStream_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,StringStream,"","",_lisp)
	    ;
#endif
    }





    
    


    EXPOSE_CLASS(core,StringInputStream_O);



#define	ARGS_StringInputStream_O_make "(string &optional (start 0) (end nil))"
#define DECL_StringInputStream_O_make ""
#define DOCS_StringInputStream_O_make ""
    StringInputStream_sp StringInputStream_O::make(Str_sp string, Fixnum_sp start, T_sp end)
    {_G();
	int iend = cl_length(string);
	if ( end.notnilp() ) iend = MIN(end.as<Fixnum_O>()->get(),iend);
	int istart = start->get();
	StringInputStream_sp sin = StringInputStream_O::create(string->substr(istart,iend-istart));
	return sin;
    }




    void StringInputStream_O::initialize()
    {
	this->Base::initialize();
    }

    void StringInputStream_O::exposeCando(Lisp_sp lisp)
    {
	class_<StringInputStream_O>()
	    ;
	Defun_maker(ClPkg,StringInputStream);
    }

    void StringInputStream_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,StringInputStream,"","",_lisp)
	    ;
#endif
    }


    int StringInputStream_O::listen()
    {
	return ( this->_Stream.rdbuf()->in_avail() + (this->_Unput.has_unput_char() ? 1 : 0 ));
    }


    void StringInputStream_O::clearInput()
    {
	this->_Unput.clear();
	this->_Stream.ignore ( std::numeric_limits<std::streamsize>::max(), EOF );
    }


    StringInputStream_sp StringInputStream_O::create(string const& contents)
    {
        GC_ALLOCATE(StringInputStream_O,fin );
	fin->_Stream.str(contents);
	return fin;
    }

    SourceFileInfo_sp StringInputStream_O::sourceFileInfo() const
    {
	return af_sourceFileInfo(Str_O::create("-StringInputStream-"));
    }


    string StringInputStream_O::readEntireFile()
    {_OF();
	this->_Cursor.invalidate();
	return this->_Stream.str();
    }

    LongLongInt StringInputStream_O::tell()
    {_OF();
	return this->_Stream.tellg();
    }

    void StringInputStream_O::seek(LongLongInt pos)
    {_OF();
	this->_Stream.seekg(pos,std::ios_base::beg);
    }

    LongLongInt StringInputStream_O::fileSize()
    {
        std::streampos pos = this->_Stream.tellg();
	this->_Stream.seekg(0,std::ios_base::end);
	LongLongInt fileSize = this->_Stream.tellg();
	this->_Stream.seekg(pos,std::ios_base::beg);
	return fileSize;
    }


    T_sp StringInputStream_O::close(bool abort)
    {_OF();
        return _lisp->_true();
    }


#if 0
    int StringInputStream_O::read(unsigned char* buf, int size)
    {_OF();
	this->_Stream.read((char*)buf,size);
	this->_Cursor.invalidate();
	return this->_Stream.gcount();
    }
#endif
    LongLongInt StringInputStream_O::gcount()
    {
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


    bool StringInputStream_O::good() const
    {
	return this->_Stream.good();
    }

    bool StringInputStream_O::eof() const
    {
	return this->_Stream.eof();
    }


    int StringInputStream_O::_get()
    {_OF();
	int c = this->_Unput.has_unput_char() ? this->_Unput.get_char() : this->_Stream.get();
	LOG(BF("Got char[%c/%d]") % c % c );
	if ( _sym_STARdebugReaderSTAR->symbolValue().isTrue() ) {
	    printf("%s:%d  StringInputStream_O::_get returning %d[%c]\n", __FILE__,__LINE__,c,c);
	}
	return c;
    }

    void StringInputStream_O::putback(char c)
    {_G();
	LOG(BF("StringInputStream_O::putback[%c]") % c);
	if ( _sym_STARdebugReaderSTAR->symbolValue().isTrue() ) {
	    printf("%s:%d  StringInputStream_O::putback returning %d[%c]\n", __FILE__,__LINE__,c,c);
	}
	this->_Unput.updateCursor(this->_Cursor);
	this->_Unput.putback(c);
	char pc = this->peek_char();
	if ( pc != c ) 
	{
	    SIMPLE_ERROR(BF("putback char[%c] does not match peek_char[%c]") % c % pc );
	}
    }

    int StringInputStream_O::peek_char()
    {_OF();
	int c = this->_Unput.has_unput_char() ? this->_Unput.peek_char() : this->_Stream.peek();
	LOG(BF("Peek char[%c/%d]") % c % c );
	if ( _sym_STARdebugReaderSTAR->symbolValue().isTrue() ) {
	    printf("%s:%d  StringInputStream_O::peek_char returning %d[%c]\n", __FILE__,__LINE__,c,c);
	}
	return c;
    }



    StringOutStream_sp StringOutStream_O::make()
    {
        GC_ALLOCATE(StringOutStream_O,ss );
	return ss;
    }


    StringOutStream_sp StringOutStream_O::create(StrWithFillPtr_sp str)
    {
        GC_ALLOCATE(StringOutStream_O,ss );
	ss->_String = str;
	return ss;
    }



    EXPOSE_CLASS(core,StringOutStream_O);

    void StringOutStream_O::exposeCando(Lisp_sp lisp)
    {
	class_<StringOutStream_O>()
	    ;
    }

    void StringOutStream_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON //[
	PYTHON_CLASS(CorePkg,StringOutStream,"","",_lisp)
	    ;
#endif //]
    }	




    void	StringOutStream_O::initialize()
    {
	this->Base::initialize();
	this->_String = StrWithFillPtr_O::create(' ',256,0,true);
    }

    void StringOutStream_O::flush()
    {
	// this->_Stream.flush();
    }

    void	StringOutStream_O::clear()
    {
	this->_String->setFillPointer(0);
    }

    uint StringOutStream_O::outputColumn() const
    {
	return this->_OutputCursor.column();
    }

    LongLongInt StringOutStream_O::tell()
    { return this->_String->length(); };

    void StringOutStream_O::writeChar(char c)
    {
	this->_String->pushCharExtend(c);
	this->_OutputCursor.advanceForChar(c);
    }	


    void	StringOutStream_O::writeln(const string& s)
    {
	this->writeStr(s);
	this->writeStr("\n");
	this->_OutputCursor.advanceLineNumber();
    }

    bool StringOutStream_O::atStartOfLine() const
    {
	return this->_OutputCursor.atStartOfLine();
    }

    string	StringOutStream_O::str()
    {
	return this->_String->get();
    }


T_sp StringOutStream_O::close(bool abort)
    {
	// do nothing
        return _lisp->_true();
    }
































    
    



// ------------------------------------------------------------------



    T_sp FDStream_O::close(bool abort)
    {_OF();
	if ( !this->_Closeable ) 
	{
	    SIMPLE_ERROR(BF("You tried to close a file that is not closeable"));
	}
	if ( this->_FStream==NULL) return _lisp->_true();;
	if ( abort )
	{
//	    _lisp->print(BF("-----Close for %s with abort=true called - I don't currently support abort=true\n") % this->_instanceClass()->classNameAsString() );
	}
	this->throw_if_no_file_descriptor();
	fclose(this->_FStream);
	this->_FStream = NULL;
        return _lisp->_true();
    }






/* ----------------------------------------------------------------------
   ----------------------------------------------------------------------

   FileDescriptorStream functions

   ----------------------------------------------------------------------
*/


static size_t consume_byte_stack(T_sp strm, unsigned char *c, size_t n)
{
    Stream_sp stream = strm.as<Stream_O>();
    size_t out = 0;
    while (n) {
        Cons_sp& l = stream->_ByteStack;
        if (l.nilp()) {
            return out + stream->ops->read_byte8(strm,c,n);
            *(c++) = oCar(l).as<Fixnum_O>()->get();
            out++;
            n--;
            l = cCdr(l);
	}
	return out;
    }
}

    size_t FDStream_O::out_write_byte8(T_sp strm, unsigned char *c, size_t n)
    {
        FDStream_sp fds = strm.as<FDStream_O>();
        FILE* fout = fds->FILE();
        ssize_t out;
        do {
            out = fwrite(c,sizeof(unsigned char),n,fout);
        } while ( out < 0 && restartable_io_error(fds,"write") );
        return out;
    }


    size_t FDStream_O::in_read_byte8(T_sp strm, unsigned char* c, size_t n)
    {
        FDStream_sp fds = strm.as<FDStream_O>();
        unlikely_if (fds->_ByteStack.notnilp() ) {
            return consume_byte_stack(strm,c,n);
        } else {
            FILE* fin = fds->FILE();
            ssize_t out;
            do {
                out = fread(c,sizeof(unsigned char),n,fin);
            } while ( out < n && ferror(fin) && restartable_io_error(strm,"fread"));
            return out;
        }
    }


            
    




    
    EXPOSE_CLASS(core,FDStream_O);


    FDStream_sp FDStream_O::makeFromFileDescriptor(const string& name,
						   int fileDescriptor, 
						   Symbol_sp direction, /* :input, :output, :io */
						   T_sp elementType,
						   T_sp externalFormat )
    {
	FDStream_sp stream;
	if ( direction == kw::_sym_input ) {
	    FILE* fstream = fdopen(fileDescriptor,"r");
	    stream = FDInStream_O::create(fstream,name,true);
	} else if ( direction == kw::_sym_output ) {
	    FILE* fstream = fdopen(fileDescriptor,"w");
	    stream = FDOutStream_O::create(fstream,name,true);
	} else if ( direction == kw::_sym_io ) {
	    FILE* fstream = fdopen(fileDescriptor,"rw");
	    stream = FDIOStream_O::create(fstream,name,true);
	} else {
	    SIMPLE_ERROR(BF("Illegal direction %s") % _rep_(direction));
	}
	return stream;
    }


    void FDStream_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<FDStream_O>()
	    .def("setBufferingMode",&FDStream_O::setBufferingMode)
	    ;
    }
    
    void FDStream_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,FDStream,"","",_lisp)
	    ;
#endif
    }



    FDStream_O::~FDStream_O()
    {
	if ( this->_FStream != NULL )
	{
	    close(this->_FStream);
	    this->_FStream = NULL;
	}
	if ( this->_Buffer ) {
	    free(this->_Buffer);
	    this->_Buffer = NULL;
	}
    }


FDStream_sp FDStream_O::setBufferingMode(Symbol_sp bufferModeSymbol)
{_G();
    BrclStreamModeEnum mode = this->getStreamMode();
    int buffer_mode;
    
    if (bufferModeSymbol == kw::_sym_none || bufferModeSymbol.nilp())
	buffer_mode = _IONBF;
    else if (bufferModeSymbol == kw::_sym_line || bufferModeSymbol == kw::_sym_line_buffered)
	buffer_mode = _IOLBF;
    else if (bufferModeSymbol == kw::_sym_full || bufferModeSymbol == kw::_sym_fully_buffered)
	buffer_mode = _IOFBF;
    else  {
	SIMPLE_ERROR(BF("Not a valid buffering mode: %s") % _rep_(bufferModeSymbol));
    };
    if (mode == brcl_stream_mode_output || mode == brcl_stream_mode_io || mode == brcl_stream_mode_input) {
	FILE *fp = this->_FStream;
	if (buffer_mode != _IONBF) {
	    size_t buffer_size = BUFSIZ;
	    char *new_buffer = (char*)malloc(buffer_size);
	    this->_Buffer = new_buffer;
	    setvbuf(fp, new_buffer, buffer_mode, buffer_size);
	} else
	    setvbuf(fp, NULL, _IONBF, 0);
        
    }
    return this->sharedThis<FDStream_O>();
};

    void FDStream_O::throw_if_no_file_descriptor() const
    {_G();
	if ( this->_FStream == NULL )
	{
	    SIMPLE_ERROR(BF("The file is not open!!"));
	}
    }






    EXPOSE_CLASS(core,FDInStream_O);


#define	ARGS_FDInStream_O_make "(file_desig)"
#define DECL_FDInStream_O_make ""
#define DOCS_FDInStream_O_make ""
    FDInStream_sp FDInStream_O::make(T_sp file_descriptor)
    {_G();
	Path_sp path = coerce::pathDesignator(file_descriptor);
        GC_ALLOCATE(FDInStream_O,fin );
	fin->_SourceFileInfo = af_sourceFileInfo(Str_O::create(path->asString()));
	fin->_FStream = fopen(path->asString().c_str(),"r");
	return fin;
    }


    FDInStream_sp FDInStream_O::create(Pathname_sp pname)
    {_G();
        GC_ALLOCATE(FDInStream_O,fin );
	string fname = af_coerceToFilename(pname)->get();
	fin->_FStream = fopen(fname.c_str(),"r");
	fin->_SourceFileInfo = af_sourceFileInfo(Str_O::create(fname));
	fin->_Closeable = true;
	return fin;
    }

    FDInStream_sp FDInStream_O::create(FILE* fid,const string& name,bool closeable)
    {_G();
        GC_ALLOCATE(FDInStream_O,fin );
        fin->_FStream = fid;
	fin->_SourceFileInfo = af_sourceFileInfo(Str_O::create(name));
	fin->_Closeable = closeable;
	return fin;
    }


    void FDInStream_O::exposeCando(Lisp_sp lisp)
    {
	class_<FDInStream_O>()
	    ;
	SYMBOL_SC_(CorePkg,make_fd_in_stream);
	af_def(CurrentPkg,"make-fd-in-stream",&FDInStream_O::make,ARGS_FDInStream_O_make,DECL_FDInStream_O_make,DOCS_FDInStream_O_make);
    }

    void FDInStream_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,FDInStream,"","",_lisp)
	    ;
#endif
    }


    void FDInStream_O::clearInput()
    {
	int lst;
	while ((lst = this->listen())) {
	    if ( lst <= 0 ) break;
	    int c = this->_get();
	    if ( c == EOF ) return;
	}
    }


    string FDInStream_O::readEntireFile()
    {_G();
	this->throw_if_no_file_descriptor();
	int res = fseek(this->_FStream,SEEK_END,0);
	if ( res < 0 )
	{
	    SIMPLE_ERROR(BF("Could not fseek to end of file: %s") % this->_SourceFileInfo->fileName() );
	}
	res = fseek(this->_FStream,SEEK_SET,0);
	if ( res < 0 )
	{
	    SIMPLE_ERROR(BF("Problem with fseek"));
	}
	LongLongInt fileSize = ftell(this->_FStream);
	char* buffer = (char*)malloc(fileSize+1);
	for ( LongLongInt i=0; i<fileSize; i++ )
	{
	    buffer[i] = this->get(); // read file and keep track of line numbers
	}
	buffer[fileSize] = '\0';
	string fileContents = buffer;
	return fileContents;
    }

    LongLongInt FDInStream_O::tell()
    {
	this->throw_if_no_file_descriptor();
	return ftell(this->_FStream);
    }

    void FDInStream_O::seek(LongLongInt pos)
    {_G();
	this->throw_if_no_file_descriptor();
	int res = fseek(this->_FStream,SEEK_SET,pos);
	if ( res < 0 ) SIMPLE_ERROR(BF("Problem with fseek"));
	this->_Cursor.invalidate();
    }


    LongLongInt FDInStream_O::fileSize()
    {_G();
	this->throw_if_no_file_descriptor();
	LongLongInt pos = ftell(this->_FStream);
	int res = fseek(this->_FStream,SEEK_END,0);
	if ( res < 0 ) SIMPLE_ERROR(BF("Problem with fseek"));
	LongLongInt fileSize = ftell(this->_FStream);
	res = fseek(this->_FStream,SEEK_SET,pos);
	if ( res < 0 ) SIMPLE_ERROR(BF("Problem with fseek"));
	return fileSize;
    }




#if 0
    int FDInStream_O::read(unsigned char* buf, int size)
    {
	this->throw_if_no_file_descriptor();
	this->_gcount = fread(buf,1,size,this->_FStream);
	this->_Cursor.invalidate();
	return this->_gcount;
    }
#endif

    LongLongInt FDInStream_O::gcount()
    {
	return this->_gcount;
    }

    int FDInStream_O::_get()
    {
	this->throw_if_no_file_descriptor();
	int ch = this->_Unput.has_unput_char() ? this->_Unput.get_char() : fgetc(this->_FStream);
	return ch;
    }

    void FDInStream_O::putback(char c)
    {
	this->throw_if_no_file_descriptor();
	this->_Unput.updateCursor(this->_Cursor);
	this->_Unput.putback(c);
    }

    int FDInStream_O::peek_char()
    {
	this->throw_if_no_file_descriptor();
	char c;
	if (this->_Unput.has_unput_char() ) {
	    c = this->_Unput.peek_char();
	} else
	{
	    c = fgetc(this->_FStream);
	    ungetc(c,this->_FStream);
	}
	return c;
    }

#if 0
    void FDInStream_O::readLine(string& sbuf, bool& hitEof)
    {_OF();
	this->throw_if_no_file_descriptor();
	char* buf = NULL;
	size_t n=0;
	getline(&buf,&n,this->_FStream);
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
	hitEof = feof(this->_FStream);
    }
#endif


    bool FDInStream_O::good() const
    {
	if ( this->_FStream == NULL ) return true;
	return !(ferror(this->_FStream));
    }

    bool FDInStream_O::eof() const
    {
	if ( this->_FStream == NULL ) return true;
	return feof(this->_FStream);
    }

    int FDInStream_O::listen()
    {
	int current=ftell(this->_FStream);
	fseek(this->_FStream,0,SEEK_SET);
	int bytes_left=ftell(this->_FStream) - current;
	fseek(this->_FStream,current,SEEK_SET);  /* back to where we started */
	return ( bytes_left + (this->_Unput.has_unput_char() ? 1 : 0 ));
    }









    string convert_ios_base_openmode_to_fopen_flags(std::ios_base::openmode mode)
    {_G();
	stringstream ss;
	if ( (mode & std::ios_base::in) && (mode &std::ios_base::out))
	{
	    return "r+";
	} else if ((mode&std::ios_base::in))
	{
	    return "r";
	} else if ((mode&std::ios_base::out) && (mode&std::ios_base::app))
	{
	    return "a";
	} else if ( mode & std::ios_base::out )
	{
	    return "w";
	}
	SIMPLE_ERROR(BF("Could not convert std::ios_base openmode flags: %d") % mode );
    }
	


    FDOutStream_sp FDOutStream_O::create(FILE* fod,const string& name,bool closeable)
    {_G();
        GC_ALLOCATE(FDOutStream_O,fout );
	fout->_FStream = fod;
	fout->_Closeable = closeable;
	return fout;
    }
							 

    FDOutStream_sp FDOutStream_O::create(Pathname_sp currentPath, std::ios_base::openmode mode)
    {_G();
        GC_ALLOCATE(FDOutStream_O,fout );
	fout->_ActivePathname = currentPath;
	fout->_RenameToOriginalOnClose = false;
	fout->_OriginalPathname = _Nil<Pathname_O>();
	fout->do_open(currentPath,mode);
	return fout;
    }

    
    void FDOutStream_O::do_open(Pathname_sp currentPathname, std::ios_base::openmode mode)
    {_OF();
	Str_sp truename = af_coerceToFilename(currentPathname); // af_coerceToFilename(currentPathname);
	this->_FStream = fopen(truename->c_str(),
			       convert_ios_base_openmode_to_fopen_flags(mode).c_str());
	if ( this->_FStream == NULL )
	{
	    SIMPLE_ERROR(BF("Could not open file: %s") % truename->get() );
	}
	if ( (mode & std::ios_base::ate) != 0 )
	{
	    IMPLEMENT_ME();
	    int res = fseek(this->_FStream,SEEK_END,0);
	    if ( res<0 ) SIMPLE_ERROR(BF("Problem with fseek"));
#ifdef DEBUG_ON
	    LongLongInt pos = ftell(this->_FStream);
	    LOG(BF("Seek to end of file puts me at: %u") % pos );
#endif
	}
    }

    FDOutStream_sp FDOutStream_O::createTemporary(Pathname_sp temporaryPath, Pathname_sp originalPath, std::ios_base::openmode mode)
    {
        GC_ALLOCATE(FDOutStream_O,fout );
	fout->_ActivePathname = temporaryPath;
	fout->_OriginalPathname = originalPath;
	fout->_RenameToOriginalOnClose = true;
	fout->do_open(temporaryPath,mode);
	return fout;
    }




#define	ARGS_FDOutStream_O_make "(file_desig)"
#define DECL_FDOutStream_O_make ""
#define DOCS_FDOutStream_O_make ""
    FDOutStream_sp FDOutStream_O::make(T_sp file_desig)
    {_G();
	Pathname_sp activePathname = cl_pathname(file_desig);
	FDOutStream_sp fout = FDOutStream_O::create(activePathname,std::ios_base::out);
	return fout;
    }


    void FDOutStream_O::flush()
    {
	fflush(this->_FStream);
    }

    uint FDOutStream_O::outputColumn() const
    {
	return this->_OutputCursor.column();
    }


    void FDOutStream_O::writeChar(char c)
    {
	this->throw_if_no_file_descriptor();
	fputc(c,this->_FStream);
	this->_OutputCursor.advanceForChar(c);
    }

    void FDOutStream_O::writeln(const string& s)
    {
	this->throw_if_no_file_descriptor();
	fwrite(s.c_str(),1,s.size(),this->_FStream);
	const char* eoln = "\n";
	fwrite(eoln,1,strlen(eoln),this->_FStream);
	this->_OutputCursor.advanceLineNumber();
    }

    bool FDOutStream_O::atStartOfLine() const
    {
	return this->_OutputCursor.atStartOfLine();
    }


    T_sp FDOutStream_O::close(bool abort)
    {_OF();
	if ( !this->_Closeable ) 
	{
	    SIMPLE_ERROR(BF("You tried to close a file that is not closeable"));
	}
	if ( this->_FStream!=NULL )
	{
	    fclose(this->_FStream);
	    this->_FStream = NULL;
	}
	if ( abort )
	{
	    // We are supposed to rename the ActivePathname to the OriginalPathname
	    if ( this->_RenameToOriginalOnClose )
	    {
		Pathname_sp backupPathname = af_makePathname(_Nil<T_O>() /* host */,
							     false /* hostp */,
							     _Nil<T_O>() /* device */,
							     false /* devicep */,
							     _Nil<T_O>() /* directory */,
							     false /* directoryp */,
							     _Nil<T_O>() /* name */,
							     false /* namep */,
							     Str_O::create(this->_OriginalPathname->_Type.as<Str_O>()->get()+"Backup") /*type*/,
							     true /* typep */,
							     _Nil<T_O>() /* version */,
							     false /* versionp */,
							     kw::_sym_local /* scase */,
							     this->_OriginalPathname /* defaults */ );
		if ( af_probe_file(backupPathname).notnilp() ) {
		    af_deleteFile(backupPathname);
		}
		af_renameFile(this->_OriginalPathname,backupPathname,kw::_sym_supersede);
		af_renameFile(this->_ActivePathname,this->_OriginalPathname,kw::_sym_supersede);
	    }
	}
        return _lisp->_true();
    }


    EXPOSE_CLASS(core,FDOutStream_O);

    void FDOutStream_O::exposeCando(Lisp_sp lisp)
    {
	class_<FDOutStream_O>()
	    ;
	af_def(CurrentPkg,"make-fd-out-stream",&FDOutStream_O::make,ARGS_FDOutStream_O_make,DECL_FDOutStream_O_make,DOCS_FDOutStream_O_make);
    }

    void FDOutStream_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON //[
	PYTHON_CLASS(CorePkg,FDOutStream,"","",_lisp)
	    ;
#endif //]
    }






    EXPOSE_CLASS(core,FDIOStream_O);


    FDIOStream_sp FDIOStream_O::create(FILE* fid,const string& name,bool closeable)
    {_G();
        GC_ALLOCATE(FDIOStream_O,fio );
	fio->_FStream = fid;
	fio->_SourceFileInfo = af_sourceFileInfo(Str_O::create(name));
	fio->_Closeable = closeable;
	return fio;
    }

    void FDIOStream_O::exposeCando(Lisp_sp lisp)
    {
	class_<FDIOStream_O>()
	    ;
//	af_def(CurrentPkg,"make-fd-io-stream",&FDIOStream_O::make,ARGS_FDIOStream_O_make,DECL_FDIOStream_O_make,DOCS_FDIOStream_O_make);
    }

    void FDIOStream_O::exposePython(Lisp_sp lisp)
    {_G();
#ifdef	USEBOOSTPYTHON //[
	PYTHON_CLASS(CorePkg,FDIOStream,"","",_lisp)
	    ;
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







    
    EXPOSE_CLASS(core,SynonymStream_O);




#define	ARGS_SynonymStream_O_make "(symbol)"
#define DECL_SynonymStream_O_make ""
#define DOCS_SynonymStream_O_make ""
    SynonymStream_sp SynonymStream_O::make(Symbol_sp symbol)
    {_G();
	Stream_sp stream = symbol->symbolValue().as<Stream_O>();
        GC_ALLOCATE(SynonymStream_O,str );
	    str->_SynonymSymbol = symbol;
	return str;
    }


    
    void SynonymStream_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<SynonymStream_O>()
	    ;
	af_def(CurrentPkg,"make-synonym-stream",&SynonymStream_O::make,ARGS_SynonymStream_O_make,DECL_SynonymStream_O_make,DOCS_SynonymStream_O_make);
    }
    
    void SynonymStream_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,SynonymStream,"","",_lisp)
	    ;
#endif
    }



    SynonymStream_O::SynonymStream_O() : Base(), _SynonymSymbol(_Nil<Symbol_O>()) {};

    SynonymStream_O::~SynonymStream_O()
    {
    }


    string SynonymStream_O::__repr__() const
    {
	stringstream ss;
	ss << "#<" << this->_instanceClass()->classNameAsString() << " :symbol " << _rep_(this->_SynonymSymbol) << ">";
	return ss.str();
    }


    void SynonymStream_O::flush()
    {
	this->stream()->flush();
    }

    Stream_sp SynonymStream_O::stream()
    {
	Stream_sp stream = this->_SynonymSymbol->symbolValue().as<Stream_O>();
	return stream;
    }


    Stream_sp SynonymStream_O::stream() const
    {
	Stream_sp stream = this->_SynonymSymbol->symbolValue().as<Stream_O>();
	return stream;
    }

    void SynonymStream_O::clearInput()
    {
	this->stream()->clearInput();
    }

    int SynonymStream_O::listen()
    {
	return this->stream()->listen();
    }


    T_sp SynonymStream_O::close(bool abort)
    {_OF();
	return this->stream()->close(abort);
    }


    bool SynonymStream_O::inputStreamP() const
    {_G();
	return this->stream()->inputStreamP();
    }

    bool SynonymStream_O::outputStreamP() const
    {_G();
	return this->stream()->outputStreamP();
    }



    string SynonymStream_O::readEntireFile()
    {_G();
	return this->stream()->readEntireFile();
    }

    LongLongInt SynonymStream_O::tell()
    {
	return this->stream()->tell();
    }

    void SynonymStream_O::seek(LongLongInt pos)
    {_G();
	this->stream()->seek(pos);
    }


    LongLongInt SynonymStream_O::fileSize()
    {_G();
	return this->stream()->fileSize();
    }


#if 0
    int SynonymStream_O::read(unsigned char* buf, int size)
    {
	return this->stream()->read(buf,size);
    }
#endif


    LongLongInt SynonymStream_O::gcount()
    {
	return this->stream()->gcount();
    }

    SourceFileInfo_sp SynonymStream_O::sourceFileInfo() const
    {
      return this->stream()->sourceFileInfo();
    }

    int SynonymStream_O::_get()
    {
	return this->stream()->_get();
    }

    void SynonymStream_O::putback(char c)
    {
	this->stream()->putback(c);
    }

    int SynonymStream_O::peek_char()
    {
	return this->stream()->peek_char();
    }

#if 0
    void SynonymStream_O::readLine(string& sbuf, bool& hitEof)
    {
	return this->stream()->readLine(sbuf,hitEof);
    }
#endif


    bool SynonymStream_O::good() const
    {
	return this->stream()->good();
    }

    bool SynonymStream_O::eof() const
    {
	return this->stream()->eof();
    }



    uint SynonymStream_O::lineNumber() const
    {
	return this->stream()->lineNumber();
    }

    uint SynonymStream_O::column() const
    {
	return this->stream()->column();
    }

    uint SynonymStream_O::outputColumn() const
    {
	return this->stream()->outputColumn();
    }

    void SynonymStream_O::invalidateCursor()
    {
	this->stream()->invalidateCursor();
    }

    void SynonymStream_O::advanceLineNumber(int num)
    {
	this->stream()->advanceLineNumber(num);
    }

    void SynonymStream_O::advanceColumn(int num)
    {
	this->stream()->advanceColumn(num);
    }







    void SynonymStream_O::writeChar(char c)
    {
	this->stream()->writeChar(c);
    }

    void SynonymStream_O::writeln(const string& s)
    {
	this->stream()->writeln(s);
    }

    bool SynonymStream_O::atStartOfLine() const
    {
	return this->stream()->atStartOfLine();
    }






// ------------------------------------------------------------


    


    TwoWayStream_O::TwoWayStream_O() : Base()
                                     , _in_stream(_Nil<Stream_O>())
                                     , _out_stream(_Nil<Stream_O>())
                                     , interactive(false) {};

    TwoWayStream_O::~TwoWayStream_O()
    {
	if ( this->_in_stream.notnilp() ) this->_in_stream->close();
	if ( this->_out_stream.notnilp() ) this->_out_stream->close();
    }


    void TwoWayStream_O::clearInput()
    {
	this->in_stream()->clearInput();
    }


    int TwoWayStream_O::listen()
    {
	return this->in_stream()->listen();
    }

    string TwoWayStream_O::__repr__() const
    {
	stringstream ss;
	ss << "#< " << this->_instanceClass()->classNameAsString() << " :in-stream " << _rep_(this->_in_stream) << " :out-stream " << _rep_(this->_out_stream) << ">";
	return ss.str();
    }

    Stream_sp TwoWayStream_O::in_stream()
    {_G();
	if ( this->_in_stream.notnilp() ) return this->_in_stream;
	SIMPLE_ERROR(BF("two-way-stream in-stream is nil!"));
    }


    Stream_sp TwoWayStream_O::in_stream() const
    {_G();
	if ( this->_in_stream.notnilp() ) return this->_in_stream;
	SIMPLE_ERROR(BF("two-way-stream in-stream is nil!"));
    }


    Stream_sp TwoWayStream_O::out_stream()
    {_G();
	if ( this->_out_stream.notnilp() ) return this->_out_stream;
	SIMPLE_ERROR(BF("two-way-stream out-stream is nil!"));
    }


    Stream_sp TwoWayStream_O::out_stream() const
    {_G();
	if ( this->_out_stream.notnilp() ) return this->_out_stream;
	SIMPLE_ERROR(BF("two-way-stream out-stream is nil!"));
    }


    void TwoWayStream_O::flush()
    {
	if ( this->_out_stream.notnilp() ) this->_out_stream->flush();
    }


    T_sp TwoWayStream_O::close(bool abort)
    {_G();
	if ( this->_in_stream.notnilp() ) this->_in_stream->close(abort);
	if ( this->_out_stream.notnilp() ) this->_out_stream->close(abort);
        return _lisp->_true();
    };


#define	ARGS_TwoWayStream_O_make "(in-stream out-stream)"
#define DECL_TwoWayStream_O_make ""
#define DOCS_TwoWayStream_O_make ""
    TwoWayStream_sp TwoWayStream_O::make(Stream_sp in_stream, Stream_sp out_stream)
    {_G();
        GC_ALLOCATE(TwoWayStream_O,f );
	    f->_in_stream = in_stream;
	    f->_out_stream = out_stream;
	return f;
    }




    string TwoWayStream_O::readEntireFile()
    {_G();
	return this->in_stream()->readEntireFile();
    }

    LongLongInt TwoWayStream_O::tell()
    {
	return this->in_stream()->tell();
    }

    void TwoWayStream_O::seek(LongLongInt pos)
    {_G();
	this->in_stream()->seek(pos);
    }


    LongLongInt TwoWayStream_O::fileSize()
    {_G();
	return this->in_stream()->fileSize();
    }


#if 0
    int TwoWayStream_O::read(unsigned char* buf, int size)
    {
	return this->in_stream()->read(buf,size);
    }
#endif


    LongLongInt TwoWayStream_O::gcount()
    {
	return this->in_stream()->gcount();
    }

    int TwoWayStream_O::_get()
    {
	return this->in_stream()->_get();
    }

    void TwoWayStream_O::putback(char c)
    {
	this->in_stream()->putback(c);
    }

    int TwoWayStream_O::peek_char()
    {
	return this->in_stream()->peek_char();
    }

#if 0
    void TwoWayStream_O::readLine(string& sbuf, bool& hitEof)
    {
	return this->in_stream()->readLine(sbuf,hitEof);
    }
#endif


    bool TwoWayStream_O::good() const
    {
	return this->in_stream()->good();
    }

    bool TwoWayStream_O::eof() const
    {
	return this->in_stream()->eof();
    }


    void TwoWayStream_O::writeChar(char c)
    {
	this->out_stream()->writeChar(c);
    }

    void TwoWayStream_O::writeln(const string& s)
    {
	this->out_stream()->writeln(s);
    }

    bool TwoWayStream_O::atStartOfLine() const
    {
	return this->out_stream()->atStartOfLine();
    }


    EXPOSE_CLASS(core,TwoWayStream_O);
    
    void TwoWayStream_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<TwoWayStream_O>()
	    .def("two-way-stream-input-stream",&TwoWayStream_O::input_stream)
	    .def("two-way-stream-output-stream",&TwoWayStream_O::output_stream)
	    ;
	af_def(CurrentPkg,"make-two-way-stream",&TwoWayStream_O::make,ARGS_TwoWayStream_O_make,DECL_TwoWayStream_O_make,DOCS_TwoWayStream_O_make);
    }
    
    void TwoWayStream_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,TwoWayStream,"","",_lisp)
	    ;
#endif
    }








// ------------------------------------------------------------



    
    
#define ARGS_cl_makeBroadcastStream "(&rest streams)"
#define DECL_cl_makeBroadcastStream ""
#define DOCS_cl_makeBroadcastStream "makeBroadcastStream"
    BroadcastStream_sp cl_makeBroadcastStream(Cons_sp streams)
    {_G();
        return BroadcastStream_O::create(streams);
    };

    

    EXPOSE_CLASS(core,BroadcastStream_O);
    
    void BroadcastStream_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<BroadcastStream_O>()
	    ;
        SYMBOL_EXPORT_SC_(ClPkg,makeBroadcastStream);
        ClDefun(makeBroadcastStream);
    }
    
    void BroadcastStream_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,BroadcastStream,"","",_lisp)
	    ;
#endif
    }



    BroadcastStream_sp BroadcastStream_O::create(Cons_sp streams)
    {
        GC_ALLOCATE(BroadcastStream_O,fout );
        fout->_Streams = streams;
        return fout;
    }

    T_sp BroadcastStream_O::streamElementType() const {
        T_sp result = _Nil<T_O>();
        for ( Cons_sp cur=this->_Streams; cur.notnilp(); cur=cCdr(cur) ) {
            result = clasp_streamElementType(oCar(cur));
        }
        return result;
    }

    bool BroadcastStream_O::freshLine()
    {
        bool result = false;
        for ( Cons_sp cur=this->_Streams; cur.notnilp(); cur=cCdr(cur) ) {
            result = clasp_freshLine(oCar(cur));
        }
        return result;
    }

    Integer_sp BroadcastStream_O::fileLength()
    {
        Integer_sp result = _Nil<Integer_O>();
        for ( Cons_sp cur=this->_Streams; cur.notnilp(); cur=cCdr(cur) ) {
            result = clasp_fileLength(oCar(cur));
        }
        return result;
    }


    void BroadcastStream_O::writeChar(brclChar c) {
        for ( Cons_sp cur=this->_Streams; cur.notnilp(); cur=cCdr(cur) ) {
            Character_sp cc = Character_O::create(c);
            clasp_writeChar(oCar(cur),cc);
        }
    }
        





// ------------------------------------------------------------


    

    EXPOSE_CLASS(core,ConcatenatedStream_O);
    
    void ConcatenatedStream_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<ConcatenatedStream_O>()
	    ;
    }
    
    void ConcatenatedStream_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,ConcatenatedStream,"","",_lisp)
	    ;
#endif
    }




    int ConcatenatedStream_O::listen()
    {
	IMPLEMENT_ME();
    }






// ------------------------------------------------------------


    

    EXPOSE_CLASS(core,EchoStream_O);
    
    void EchoStream_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<EchoStream_O>()
	    ;
    }
    
    void EchoStream_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,EchoStream,"","",_lisp)
	    ;
#endif
    }





    int EchoStream_O::listen()
    {
	IMPLEMENT_ME();
    }





    
    
#define ARGS_af_makeStringOutputStreamFromString "(str)"
#define DECL_af_makeStringOutputStreamFromString ""
#define DOCS_af_makeStringOutputStreamFromString "makeStringOutputStreamFromString"
    T_sp af_makeStringOutputStreamFromString(StrWithFillPtr_sp str)
    {_G();
	StringOutStream_sp ss = StringOutStream_O::create(str);
	return ss;
    };





    
    /*! Translated from ecl::si_do_write_sequence */
#define ARGS_af_writeSequence "(seq stream &key (start 0) end)"
#define DECL_af_writeSequence ""
#define DOCS_af_writeSequence "writeSequence"
    T_sp af_writeSequence(T_sp seq, Stream_sp stream, Fixnum_sp fstart, Fixnum_sp tend)
    {_G();
	int limit = cl_length(seq);
	unlikely_if( !af_fixnumP(fstart) ||
		     (fstart->get()<0) ||
		     (fstart->get() > limit)) {
	    WRONG_TYPE_KEY_ARG(kw::_sym_start,fstart,
			  Integer_O::makeIntegerType(0,limit-1));
	}
	int start = fstart->get();
	int end = limit;
	if ( tend.notnilp() ) {
	    unlikely_if (!af_fixnumP(tend) ||
			 (end < 0 ) ||
			 (end > limit) ) {
		WRONG_TYPE_KEY_ARG(kw::_sym_end,tend,
				   Integer_O::makeIntegerType(0,limit-1));
	    }
	    end = tend->get();
	}
	if ( end <= start ) {
	    goto OUTPUT;
	}
	if (af_listp(seq)) {
	    T_sp elt_type = stream->streamElementType();
	    bool ischar = (elt_type == cl::_sym_BaseChar_O) || (elt_type == cl::_sym_Character_O);
	    T_sp s = cl_nthcdr(start, seq);
	    for ( ; ; s = CONS_CDR(s) ) {
		if (start < end) {
		    T_sp elt = CONS_CAR(s);
		    if (ischar)
			stream->writeChar(elt.as<Character_O>()->charCode());
		    else
			stream->writeByte(elt.as<Integer_O>());
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




    
    
#define ARGS_af_streamLinenumber "(stream)"
#define DECL_af_streamLinenumber ""
#define DOCS_af_streamLinenumber "streamLinenumber"
    int af_streamLinenumber(T_sp tstream)
    {
        tstream = coerce::inputStreamDesignator(tstream);
        if ( Stream_sp stream = tstream.as<Stream_O>() ) {
            return stream->lineNumber();
        }
	IMPLEMENT_MEF(BF("Implement streamLinenumber for stream object: %s") % _rep_(tstream));
    };
 
#define ARGS_af_streamColumn "(stream)"
#define DECL_af_streamColumn ""
#define DOCS_af_streamColumn "streamColumn"
    int af_streamColumn(T_sp tstream)
    {
        tstream = coerce::inputStreamDesignator(tstream);
        if ( Stream_sp stream = tstream.as<Stream_O>() ) {
            return stream->column();
        }
	IMPLEMENT_MEF(BF("Implement streamColumn for stream object: %s") % _rep_(tstream));
    };







    void initialize_lispStream()
    {
	SYMBOL_EXPORT_SC_(ClPkg,filePosition);
	ClDefun(filePosition);
	SYMBOL_EXPORT_SC_(ClPkg,read_from_string);
	Defun(read_from_string);
	SYMBOL_EXPORT_SC_(ClPkg,read_line);
	Defun(read_line);
	SYMBOL_EXPORT_SC_(ClPkg,terpri);
	Defun(terpri);
	SYMBOL_EXPORT_SC_(ClPkg,freshLine);
	Defun(freshLine);
	SYMBOL_EXPORT_SC_(ClPkg,writeString);
	Defun(writeString);
	SYMBOL_EXPORT_SC_(ClPkg,writeLine);
	Defun(writeLine);
	SYMBOL_EXPORT_SC_(ClPkg,writeChar);
	Defun(writeChar);
	SYMBOL_EXPORT_SC_(ClPkg,clearInput);
	Defun(clearInput);
	SYMBOL_EXPORT_SC_(ClPkg,clearOutput);
	Defun(clearOutput);
	SYMBOL_EXPORT_SC_(ClPkg,peekChar);
	Defun(peekChar);
	SYMBOL_EXPORT_SC_(ClPkg,readChar);
	Defun(readChar);
	SYMBOL_EXPORT_SC_(ClPkg,force_output);
	Defun(force_output);
	SYMBOL_EXPORT_SC_(ClPkg,finish_output);
	Defun(finish_output);
	SYMBOL_EXPORT_SC_(ClPkg,listen);
	Defun(listen);
	SYMBOL_EXPORT_SC_(ClPkg,unread_char);
	Defun(unread_char);
	SYMBOL_EXPORT_SC_(CorePkg,fileColumn);
	Defun(fileColumn);
	SYMBOL_EXPORT_SC_(CorePkg,makeStringOutputStreamFromString);
	Defun(makeStringOutputStreamFromString);
	SYMBOL_EXPORT_SC_(ClPkg,writeSequence);
	Defun(writeSequence);
        SYMBOL_EXPORT_SC_(ClPkg,writeByte);
        Defun(writeByte);
        SYMBOL_EXPORT_SC_(ClPkg,inputStreamP);
        Defun(inputStreamP);
        SYMBOL_EXPORT_SC_(ClPkg,outputStreamP);
        Defun(outputStreamP);
        SYMBOL_EXPORT_SC_(ClPkg,interactiveStreamP);
        Defun(interactiveStreamP);
        SYMBOL_EXPORT_SC_(ClPkg,streamp);
        ClDefun(streamp);
        SYMBOL_EXPORT_SC_(ClPkg,close);
        ClDefun(close);
        SYMBOL_EXPORT_SC_(CorePkg,streamLinenumber);
        Defun(streamLinenumber);
        SYMBOL_EXPORT_SC_(CorePkg,streamColumn);
        Defun(streamColumn);
    }







};




