/*
    File: keywordPackage.cc
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

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/metaClass.h>
#include <clasp/core/symbol.h>
#include <clasp/core/keywordPackage.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/package.h>

namespace kw {

SYMBOL_EXPORT_SC_(KeywordPkg, FullDebug);
SYMBOL_EXPORT_SC_(KeywordPkg, LineTablesOnly);
SYMBOL_EXPORT_SC_(KeywordPkg, UnsignedByte);
SYMBOL_EXPORT_SC_(KeywordPkg, _uid);
SYMBOL_EXPORT_SC_(KeywordPkg, absolute);
SYMBOL_EXPORT_SC_(KeywordPkg, allow_other_keys);
SYMBOL_EXPORT_SC_(KeywordPkg, and);
SYMBOL_EXPORT_SC_(KeywordPkg, append);
SYMBOL_EXPORT_SC_(KeywordPkg, array);
SYMBOL_EXPORT_SC_(KeywordPkg, axis); // initarg for conditions
SYMBOL_EXPORT_SC_(KeywordPkg, back);
SYMBOL_EXPORT_SC_(KeywordPkg, bclasp);
SYMBOL_EXPORT_SC_(KeywordPkg, bigEndian);
SYMBOL_EXPORT_SC_(KeywordPkg, bitcode);
SYMBOL_EXPORT_SC_(KeywordPkg, brcl);
SYMBOL_EXPORT_SC_(KeywordPkg, bytecode);
SYMBOL_EXPORT_SC_(KeywordPkg, capitalize);
SYMBOL_EXPORT_SC_(KeywordPkg, cclasp);
SYMBOL_EXPORT_SC_(KeywordPkg, changed);
SYMBOL_EXPORT_SC_(KeywordPkg, clasp_min);
SYMBOL_EXPORT_SC_(KeywordPkg, class);
SYMBOL_EXPORT_SC_(KeywordPkg, code);
SYMBOL_EXPORT_SC_(KeywordPkg, compile_toplevel);
SYMBOL_EXPORT_SC_(KeywordPkg, cr);
SYMBOL_EXPORT_SC_(KeywordPkg, create);
SYMBOL_EXPORT_SC_(KeywordPkg, crlf);
SYMBOL_EXPORT_SC_(KeywordPkg, cst);
SYMBOL_EXPORT_SC_(KeywordPkg, datum);
SYMBOL_EXPORT_SC_(KeywordPkg, debug);
SYMBOL_EXPORT_SC_(KeywordPkg, debugObjectFiles);
SYMBOL_EXPORT_SC_(KeywordPkg, debugStartup);
SYMBOL_EXPORT_SC_(KeywordPkg, debugStartupVerbose);
SYMBOL_EXPORT_SC_(KeywordPkg, default);
SYMBOL_EXPORT_SC_(KeywordPkg, direct_super_classes);
SYMBOL_EXPORT_SC_(KeywordPkg, direction);
SYMBOL_EXPORT_SC_(KeywordPkg, dispatch_function);
SYMBOL_EXPORT_SC_(KeywordPkg, dumpObjectFiles);
SYMBOL_EXPORT_SC_(KeywordPkg, end);
SYMBOL_EXPORT_SC_(KeywordPkg, eof);
SYMBOL_EXPORT_SC_(KeywordPkg, escape);
SYMBOL_EXPORT_SC_(KeywordPkg, eval);
SYMBOL_EXPORT_SC_(KeywordPkg, execute);
SYMBOL_EXPORT_SC_(KeywordPkg, exit_backtrace);
SYMBOL_EXPORT_SC_(KeywordPkg, expected_type);
SYMBOL_EXPORT_SC_(KeywordPkg, external);
SYMBOL_EXPORT_SC_(KeywordPkg, fasl);
SYMBOL_EXPORT_SC_(KeywordPkg, faso);
SYMBOL_EXPORT_SC_(KeywordPkg, fasobc);
SYMBOL_EXPORT_SC_(KeywordPkg, fasoll);
SYMBOL_EXPORT_SC_(KeywordPkg, fill_pointer);
SYMBOL_EXPORT_SC_(KeywordPkg, format_arguments);
SYMBOL_EXPORT_SC_(KeywordPkg, format_control);
SYMBOL_EXPORT_SC_(KeywordPkg, full);
SYMBOL_EXPORT_SC_(KeywordPkg, fully_buffered);
SYMBOL_EXPORT_SC_(KeywordPkg, function);
SYMBOL_EXPORT_SC_(KeywordPkg, handler);
SYMBOL_EXPORT_SC_(KeywordPkg, if_does_not_exist);
SYMBOL_EXPORT_SC_(KeywordPkg, if_exists);
SYMBOL_EXPORT_SC_(KeywordPkg, inherited);
SYMBOL_EXPORT_SC_(KeywordPkg, input);
SYMBOL_EXPORT_SC_(KeywordPkg, input_output);
SYMBOL_EXPORT_SC_(KeywordPkg, instance);
SYMBOL_EXPORT_SC_(KeywordPkg, internal);
SYMBOL_EXPORT_SC_(KeywordPkg, io);
SYMBOL_EXPORT_SC_(KeywordPkg, iso_8859_1);
SYMBOL_EXPORT_SC_(KeywordPkg, junkAllowed);
SYMBOL_EXPORT_SC_(KeywordPkg, key_and_value); // hash table weakness
SYMBOL_EXPORT_SC_(KeywordPkg, key_or_value);
SYMBOL_EXPORT_SC_(KeywordPkg, latin_1);
SYMBOL_EXPORT_SC_(KeywordPkg, lf);
SYMBOL_EXPORT_SC_(KeywordPkg, line);
SYMBOL_EXPORT_SC_(KeywordPkg, line_buffered);
SYMBOL_EXPORT_SC_(KeywordPkg, linkage);
SYMBOL_EXPORT_SC_(KeywordPkg, littleEndian);
SYMBOL_EXPORT_SC_(KeywordPkg, load);
SYMBOL_EXPORT_SC_(KeywordPkg, load_toplevel);
SYMBOL_EXPORT_SC_(KeywordPkg, macro);
SYMBOL_EXPORT_SC_(KeywordPkg, message);
SYMBOL_EXPORT_SC_(KeywordPkg, name);
SYMBOL_EXPORT_SC_(KeywordPkg, new_version);
SYMBOL_EXPORT_SC_(KeywordPkg, none);
SYMBOL_EXPORT_SC_(KeywordPkg, not );
SYMBOL_EXPORT_SC_(KeywordPkg, object);
SYMBOL_EXPORT_SC_(KeywordPkg, operands);
SYMBOL_EXPORT_SC_(KeywordPkg, operation);
SYMBOL_EXPORT_SC_(KeywordPkg, or);
SYMBOL_EXPORT_SC_(KeywordPkg, output);
SYMBOL_EXPORT_SC_(KeywordPkg, overwrite);
SYMBOL_EXPORT_SC_(KeywordPkg, package);
SYMBOL_EXPORT_SC_(KeywordPkg, passThrough);
SYMBOL_EXPORT_SC_(KeywordPkg, pathname);
SYMBOL_EXPORT_SC_(KeywordPkg, pause_pid);
SYMBOL_EXPORT_SC_(KeywordPkg, print);
SYMBOL_EXPORT_SC_(KeywordPkg, probe);
SYMBOL_EXPORT_SC_(KeywordPkg, process);
SYMBOL_EXPORT_SC_(KeywordPkg, read_only);
SYMBOL_EXPORT_SC_(KeywordPkg, relative);
SYMBOL_EXPORT_SC_(KeywordPkg, rename);
SYMBOL_EXPORT_SC_(KeywordPkg, rename_and_delete);
SYMBOL_EXPORT_SC_(KeywordPkg, rtld_default);
SYMBOL_EXPORT_SC_(KeywordPkg, rtld_next);
SYMBOL_EXPORT_SC_(KeywordPkg, script);
SYMBOL_EXPORT_SC_(KeywordPkg, seek_cur);
SYMBOL_EXPORT_SC_(KeywordPkg, seek_end);
SYMBOL_EXPORT_SC_(KeywordPkg, seek_set);
SYMBOL_EXPORT_SC_(KeywordPkg, start);
SYMBOL_EXPORT_SC_(KeywordPkg, stream);
SYMBOL_EXPORT_SC_(KeywordPkg, tag);
SYMBOL_EXPORT_SC_(KeywordPkg, test);
SYMBOL_EXPORT_SC_(KeywordPkg, type_error);
SYMBOL_EXPORT_SC_(KeywordPkg, ucs_2);
SYMBOL_EXPORT_SC_(KeywordPkg, ucs_2be);
SYMBOL_EXPORT_SC_(KeywordPkg, ucs_2le)
SYMBOL_EXPORT_SC_(KeywordPkg, ucs_4);
SYMBOL_EXPORT_SC_(KeywordPkg, ucs_4be);
SYMBOL_EXPORT_SC_(KeywordPkg, ucs_4le);
SYMBOL_EXPORT_SC_(KeywordPkg, us_ascii);
SYMBOL_EXPORT_SC_(KeywordPkg, use_boehm);
SYMBOL_EXPORT_SC_(KeywordPkg, use_boehmdc);
SYMBOL_EXPORT_SC_(KeywordPkg, use_mps);
SYMBOL_EXPORT_SC_(KeywordPkg, utf_8);
SYMBOL_EXPORT_SC_(KeywordPkg, verbose);

}; // namespace kw
