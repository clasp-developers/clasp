/*
 *  Copyright 2001-2004 David Abrahams.
 *  Distributed under the Boost Software License, Version 1.0.
 *  (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)
 */
#ifndef MODULES_DWA10182001_H
# define MODULES_DWA10182001_H

#include "lists.h"

struct module_t
{
    char* name;
    struct hash* rules;
    struct hash* variables;
    struct hash* imported_modules;
    struct module_t* class_module;
    struct hash* native_rules;
    int user_module;
};

typedef struct module_t module_t ; /* MSVC debugger gets confused unless this is provided */

module_t* bindmodule( char* name );
module_t* root_module();
void enter_module( module_t* );
void exit_module( module_t* );
void delete_module( module_t* );

void import_module(LIST* module_names, module_t* target_module);
LIST* imported_modules(module_t* module);

struct hash* demand_rules( module_t* );


#endif

