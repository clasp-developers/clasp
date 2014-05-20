/* Copyright David Abrahams 2003. Distributed under the Boost */
/* Software License, Version 1.0. (See accompanying */
/* file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt) */

#ifndef NATIVE_H_VP_2003_12_09
#define NATIVE_H_VP_2003_12_09

#include "rules.h"

struct native_rule_t
{
    char* name;
    argument_list* arguments;
    PARSE* procedure;
    /* Version of the interface that the native rule provides.
       It's possible that we want to change the set parameter
       for existing native rule. In that case, version number
       should be incremented so that Boost.Build can check for
       version it relies on.

       Versions are numbered from 1.
    */
    int version;
};

/* MSVC debugger gets confused unless this is provided */
typedef struct native_rule_t native_rule_t ;

void declare_native_rule(char* module, char* rule, char** args,
                         LIST*(*f)(PARSE*, FRAME*), int version);



#endif
