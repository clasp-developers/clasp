/*
 * Copyright 1993-2002 Christopher Seiwald and Perforce Software, Inc.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

#ifndef JAM_BUILTINS_H
# define JAM_BUILTINS_H

# include "frames.h"

/*
 * builtins.h - compile parsed jam statements
 */

void load_builtins();
void init_set();
void init_path();
void init_regex();
void init_property_set();
void init_sequence();
void init_order();

LIST *builtin_calc( PARSE *parse, FRAME *args );
LIST *builtin_depends( PARSE *parse, FRAME *args );
LIST *builtin_rebuilds( PARSE *parse, FRAME *args );
LIST *builtin_echo( PARSE *parse, FRAME *args );
LIST *builtin_exit( PARSE *parse, FRAME *args );
LIST *builtin_flags( PARSE *parse, FRAME *args );
LIST *builtin_glob( PARSE *parse, FRAME *args );
LIST *builtin_glob_recursive( PARSE   *parse, FRAME *frame );
LIST *builtin_subst( PARSE  *parse, FRAME *args );
LIST *builtin_match( PARSE *parse, FRAME *args );
LIST *builtin_split_by_characters( PARSE *parse, FRAME *args );
LIST *builtin_hdrmacro( PARSE *parse, FRAME *args );
LIST *builtin_rulenames( PARSE *parse, FRAME *args );
LIST *builtin_varnames( PARSE *parse, FRAME *args );
LIST *builtin_delete_module( PARSE *parse, FRAME *args );
LIST *builtin_import( PARSE *parse, FRAME *args );
LIST *builtin_export( PARSE *parse, FRAME *args );
LIST *builtin_caller_module( PARSE *parse, FRAME *args );
LIST *builtin_backtrace( PARSE *parse, FRAME *args );
LIST *builtin_pwd( PARSE *parse, FRAME *args );
LIST *builtin_update( PARSE *parse, FRAME *args );
LIST *builtin_update_now( PARSE *parse, FRAME *args );
LIST *builtin_search_for_target( PARSE *parse, FRAME *args );
LIST *builtin_import_module( PARSE *parse, FRAME *args );
LIST *builtin_imported_modules( PARSE *parse, FRAME *frame );
LIST *builtin_instance( PARSE *parse, FRAME *frame );
LIST *builtin_sort( PARSE *parse, FRAME *frame );
LIST *builtin_normalize_path( PARSE *parse, FRAME *frame );
LIST *builtin_native_rule( PARSE *parse, FRAME *frame );
LIST *builtin_has_native_rule( PARSE *parse, FRAME *frame );
LIST *builtin_user_module( PARSE *parse, FRAME *frame );
LIST *builtin_nearest_user_location( PARSE *parse, FRAME *frame );
LIST *builtin_check_if_file( PARSE *parse, FRAME *frame );
LIST *builtin_python_import_rule( PARSE *parse, FRAME *frame );
LIST *builtin_shell( PARSE *parse, FRAME *frame );
LIST *builtin_md5( PARSE *parse, FRAME *frame );
LIST *builtin_file_open( PARSE *parse, FRAME *frame );
LIST *builtin_pad( PARSE *parse, FRAME *frame );
LIST *builtin_precious( PARSE *parse, FRAME *frame );

void backtrace( FRAME *frame );

#endif
