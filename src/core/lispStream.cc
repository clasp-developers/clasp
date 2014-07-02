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
    clos_stream_write_string(T_sp strm, Str_sp str, Fixnum_sp start, T_sp end)
    {
        return eval::funcall(gray::_sym_stream_write_string,str,start,end).as<Str_O>();
    }




    static T_sp
    clos_stream_read_byte(T_sp strm)
    {
	T_sp b = eval::funcall(gray::_sym_stream_read_byte, strm);
        if (b == kw::_sym_eof) b = _Nil<T_O>();
        return b;
    }

    static void
    clos_stream_write_byte(T_sp c, T_sp strm)
    {
	eval::funcall(gray::_sym_stream_write_byte, strm, c);
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

#if 0
    static T_sp
    clos_stream_length(T_sp strm)
    {
	TYPE_ERROR(strm,cl::_sym_fileStream);
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


    Str_sp brcl_write_string(Str_sp str, T_sp strm, Fixnum_sp start, T_sp end)
    {
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
        {
            return clos_stream_write_string(strm,str,start,end);
        }
#endif
        Stream_sp ostrm = coerce::outputStreamDesignator(strm);
	int istart = start->get();
	int iend = af_length(str);
	if ( end.notnilp() )
	{
	    iend = MIN(iend,end.as<Fixnum_O>()->get());
	}
        int ilen = iend-istart;
        ostrm->writeBytes(&(str->get().c_str()[istart]),ilen);
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


    void brcl_write_char(T_sp strm, Character_sp chr)
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




    bool brcl_fresh_line(T_sp strm)
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







    T_sp brcl_stream_element_type(T_sp strm)
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

    void brcl_force_output(T_sp strm)
    {_G();
#ifdef CLOS_STREAMS
	if ( strm.pointerp() && strm->instancep() )
	{
	    return clos_stream_force_output(strm);
	}
#endif
	Stream_sp ostrm = coerce::outputStreamDesignator(strm);
	return ostrm->forceOutput();
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
        brcl_write_char(outputStream, chr);
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
	brcl_force_output(ostrm);
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
    String_sp af_writeString(Str_sp str, T_sp strm, Fixnum_sp start, T_sp end)
    {_G();
        return brcl_write_string(str,strm,start,end);
    };


#define ARGS_af_writeLine "(string &optional output-stream &key (start 0) end)"
#define DECL_af_writeLine ""
#define DOCS_af_writeLine "writeLine"
    String_sp af_writeLine(Str_sp str, T_sp strm, Fixnum_sp start, T_sp end)
    {_G();
        brcl_write_string(str,strm,start,end);
        brcl_terpri(strm);
        return str;
    };



    
#define ARGS_af_terpri "(&optional (output-stream core::*stdout*))"
#define DECL_af_terpri ""
#define DOCS_af_terpri "Send a newline to the output stream"
    void af_terpri(T_sp outputStreamDesig)
    {_G();
        brcl_terpri(outputStreamDesig);
    };


    
    
#define ARGS_af_freshLine "(&optional (outputStream core:*stdout*))"
#define DECL_af_freshLine ""
#define DOCS_af_freshLine "freshLine"
    bool af_freshLine(T_sp outputStreamDesig)
    {_G();
        return brcl_fresh_line(outputStreamDesig);
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







#define	DOCS_af_read_line "See clhs"
#define LOCK_af_read_line 1
#define ARGS_af_read_line "(&optional input-stream (eof-error-p t) eof-value recursive-p)"
#define DECL_af_read_line ""    
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
	    .def("stream-lineNumber",&Stream_O::lineNumber)
	    .def("stream-column",&Stream_O::column)
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
	Pathname_sp pn = af_pathname(fileName);
	fin->_SourceFileInfo = SourceFileInfo_O::getOrCreate(pn);
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
	fin->_SourceFileInfo = SourceFileInfo_O::getOrCreate(truename->get());
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
	    _lisp->print(BF("-----Close for %s with abort=true called - I don't currently support abort=true\n") % this->_instanceClass()->classNameAsString() );
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



#if 0
    T_sp FileOutStream_O::__init__(Function_sp exec, Cons_sp args, Environment_sp environ)
    {
	IMPLEMENT_ME();
#if 0
	this->_ActiveFilePath = translate::from_object<string>::convert(environ->lookup(CorePkg,"fileName"));
	this->_Stream.open(this->_FileName.c_str(),std::ios_base::app|ios_base::out|std::ios_base::ate);
    	// your stuff here
#endif
	return _Nil<T_O>();
    }
#endif
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
	fin->_SourceFileInfo = SourceFileInfo_O::getOrCreate(truename->get());
	fin->_RawStream.open(truename->c_str(),std::ios_base::in | std::ios_base::binary);
	fin->_In.push(boost::iostreams::gzip_decompressor());
	fin->_In.push(fin->_RawStream);
	return fin;
    }

#if 0
    T_sp FileInCompressedStream_O::__init__(Function_sp exec, Cons_sp args, Environment_sp environ, lisp)
    {
	IMPLEMENT_ME();
	return _Nil<T_O>();
    }
#endif
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
	    _lisp->print(BF("-----Close for %s with abort=true called - I don't currently support abort=true\n") % this->_instanceClass()->classNameAsString() );
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



    T_sp FileOutCompressedStream_O::__init__(Function_sp exec, Cons_sp args, Environment_sp environ, Lisp_sp lisp)
    {
	IMPLEMENT_ME();
	return _Nil<T_O>();
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
	int iend = af_length(string);
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
	return SourceFileInfo_O::getOrCreate("-StringInputStream-");
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



    T_sp StringOutStream_O::__init__(Function_sp exec, Cons_sp args, Environment_sp environ, Lisp_sp lisp)
    {
	// your stuff here
	return _Nil<T_O>();
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
	    _lisp->print(BF("-----Close for %s with abort=true called - I don't currently support abort=true\n") % this->_instanceClass()->classNameAsString() );
	}
	this->throw_if_no_file_descriptor();
	fclose(this->_FStream);
	this->_FStream = NULL;
        return _lisp->_true();
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
	fin->_SourceFileInfo = SourceFileInfo_O::getOrCreate(path->asString());
	fin->_FStream = fopen(path->asString().c_str(),"r");
	return fin;
    }


    FDInStream_sp FDInStream_O::create(Pathname_sp pname)
    {_G();
        GC_ALLOCATE(FDInStream_O,fin );
	string fname = af_coerceToFilename(pname)->get();
	fin->_FStream = fopen(fname.c_str(),"r");
	fin->_SourceFileInfo = SourceFileInfo_O::getOrCreate(fname);
	fin->_Closeable = true;
	return fin;
    }

    FDInStream_sp FDInStream_O::create(FILE* fid,const string& name,bool closeable)
    {_G();
        GC_ALLOCATE(FDInStream_O,fin );
        fin->_FStream = fid;
	fin->_SourceFileInfo = SourceFileInfo_O::getOrCreate(name);
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
	Pathname_sp activePathname = af_pathname(file_desig);
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
	fio->_SourceFileInfo = SourceFileInfo_O::getOrCreate(name);
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


    

    EXPOSE_CLASS(core,BroadcastStream_O);
    
    void BroadcastStream_O::exposeCando(core::Lisp_sp lisp)
    {
	core::class_<BroadcastStream_O>()
	    ;
    }
    
    void BroadcastStream_O::exposePython(core::Lisp_sp lisp)
    {_G();
#ifdef USEBOOSTPYTHON
	PYTHON_CLASS(CorePkg,BroadcastStream,"","",_lisp)
	    ;
#endif
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
    Sequence_sp af_writeSequence(Sequence_sp seq, Stream_sp stream, Fixnum_sp fstart, Fixnum_sp tend)
    {_G();
	int limit = af_length(seq);
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
	    T_sp s = af_nthcdr(start, seq);
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
    }







};




