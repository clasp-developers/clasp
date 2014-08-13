
#include "foundation.h"
#include "object.h"
#include "lisp.h"
#include "symbol.h"
#include "grayPackage.h"
#include "multipleValues.h"
#include "package.h"

namespace gray
{

#pragma GCC visibility push(default)
#define GrayPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) core::Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef GrayPkg_SYMBOLS
#pragma GCC visibility pop


    SYMBOL_SC_(GrayPkg,aSingleGraySymbol);
    SYMBOL_EXPORT_SC_(GrayPkg,open_stream_p);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_read_byte);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_write_byte);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_read_byte);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_write_byte);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_read_char);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_write_char);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_write_string);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_terpri);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_fresh_line);

    SYMBOL_EXPORT_SC_(GrayPkg,stream_unread_char);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_peek_char);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_listen);
    SYMBOL_EXPORT_SC_(GrayPkg,streamClearInput);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_clear_input);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_clear_output);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_force_output);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_finish_output);
    SYMBOL_EXPORT_SC_(GrayPkg,streamp);
    SYMBOL_EXPORT_SC_(GrayPkg,input_stream_p);
    SYMBOL_EXPORT_SC_(GrayPkg,output_stream_p);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_interactive_p);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_element_type);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_file_position);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_file_position);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_line_column);
    SYMBOL_EXPORT_SC_(GrayPkg,stream_advance_to_column);
    SYMBOL_EXPORT_SC_(GrayPkg,close);



    void initialize_grayPackage()
    {
	list<string> lnicknames;
	list<string> luse = { "COMMON-LISP" };
	_lisp->makePackage("GRAY",lnicknames,luse);
	// We don't have to create the GRAY symbols here - it's done in bootStrapCoreSymbolMap
    }


};

