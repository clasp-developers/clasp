/****************************************************************************
**  SCALASCA    http://www.scalasca.org/                                   **
**  KOJAK       http://www.fz-juelich.de/jsc/kojak/                        **
*****************************************************************************
**  Copyright (c) 1998-2008                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYING in the package base directory for details         **
****************************************************************************/

#ifndef HANDLER_H
#define HANDLER_H

#include "opari.h"
#include "ompragma.h"

typedef void (* phandler_t)(OMPragma*, ostream&);

void init_handler(const char* infile, const char* rcfile,
		  Language l, bool genLineStmts);

void finalize_handler(const char* rcdir,
                      const char* incfile, const char* tabfile);

void generateTableFile(const char* rcdir,
                       const char* rcfile, const char* tabfile);

phandler_t find_handler(const string& pragma);

void extra_handler(int lineno, ostream& os);

bool set_disabled(const string& constructs);

bool instrument_locks();

bool genLineStmts();

void print_regstack_top();

extern bool do_transform;

#endif
