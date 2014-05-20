/*
 * Copyright 1993, 1995 Christopher Seiwald.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*
 * hdrmacro.h - parses header files for #define MACRO  <filename> or
 *              #define MACRO  "filename" definitions
 */

void   macro_headers( TARGET *t );

char*  macro_header_get( const char*  macro_name );
