#ifndef core_unixfsys_H
#define core_unixfsys_H

#include "foundation.h"
#include "symbolTable.h"
#include "pathname.fwd.h"

namespace core
{



    extern Str_sp af_currentDir();
    Pathname_sp af_truename(T_sp filespec);
    Pathname_sp af_probe_file(T_sp filespec);
    Symbol_sp af_file_kind(Pathname_sp filename, bool follow_links = true );
    T_mv af_renameFile(T_sp oldn, T_sp newn, T_sp if_exists = kw::_sym_supersede);
    T_sp af_deleteFile(T_sp filespec);

    void initialize_unixfsys();



};
#endif
