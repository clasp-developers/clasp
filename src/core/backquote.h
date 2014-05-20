#ifndef _core_backquote_H_
#define _core_backquote_H_

#include "core/foundation.h"
#include "core/object.h"

namespace core
{


    extern T_mv af_backquote_completely_process(T_sp ox);
    extern T_sp af_backquote_process(T_sp ox);
    extern T_sp af_backquote_bracket(T_sp ox);
    extern T_sp af_backquote_simplify_args(T_sp x);
    extern T_sp af_backquote_attach_append(T_sp op, T_sp item, T_sp result );
    extern Cons_sp af_backquote_attach_conses(T_sp items, T_sp result );


    extern T_sp af_backquote_simplify(T_sp x);
    extern T_sp af_backquote_remove_tokens(T_sp x);




    void initialize_backquote(Lisp_sp lisp);

    
    
};
#endif /* _backquote_H_ */
