/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_libwrapgen.h"

#include <sstream>

#include <stdio.h>
#include <stdlib.h>

#include "ctool/ctool.h"

#define CPPFLAGS_FIX "-D\"__attribute__(x)=\" -D__extension__="


//////////////////// class Parser ////////////////////

// public methods
//

ParserC::ParserC() : m_statementNum( 0 )
{
  // Empty
}

ParserC::~ParserC()
{
  // Empty
}

bool
ParserC::parse()
{
  VPrint( 1, " Parsing input header file%s\n", 
          Params.g_input_headers.size() > 1 ? "s" : "" );
  
  bool error = false;

  // create instance of CTool class Project
  Project* prj = new Project();

  // prepare preprocessor command
  //
  std::stringstream cpp_cmd;
  cpp_cmd << Params.g_cpp_cmd << " " << Params.g_cpp_flags
          << " "CPPFLAGS_FIX" %s > %s";

  std::string cpp_file;

  // process input header files
  //
  for( uint32_t i = 0; i < Params.g_input_headers.size(); i++ )
  {
    std::string file = Params.g_input_headers[i];

    VPrint( 2, "  %s\n", file.c_str() );
    
    // create file name for preprocessed header file
    //
    size_t si;
    cpp_file = file;
    si = cpp_file.rfind('/');
    if( si != std::string::npos )
      cpp_file = cpp_file.substr( si+1 );
    cpp_file = Params.output_dir + "/" + cpp_file;
    si = cpp_file.rfind('.');
    if( si != std::string::npos )
      cpp_file = cpp_file.substr( 0, si ) + ".cppized" + cpp_file.substr( si );
    else
      cpp_file += ".cppized";

    // parse header file
    //
    if( !prj->parse(
           file.c_str(), Params.g_use_cpp,
           (Params.g_cpp_dir.length() > 0) ? Params.g_cpp_dir.c_str() : 0,
           Params.g_keep_cpp_file, cpp_file.c_str(),
           cpp_cmd.str().c_str(), 0 ) )
    {
      std::cerr << ExeName << ": Could not parse "
                << file << std::endl;
      error = true;
      break;
    }
  }

  // remove preprocessed header file, if an error occurred
  //
  if( error )
  {
    if( !Params.g_keep_cpp_file )
      remove( cpp_file.c_str() );
  }
  // otherwise, process CTool units
  //
  else
  {
    TransUnitVector::iterator unit_it;

    // count statements to process
    //
    for( unit_it = prj->units.begin();
         unit_it != prj->units.end(); ++unit_it )
    {
      Statement* curr = (*unit_it)->head;
      while( curr )
      {
        m_statementNum++;
        curr = curr->next;
      }
    }

    VPrint( 1, " Processing function declaration statements\n" );
    
    // process statements
    //
    for( unit_it = prj->units.begin();
         unit_it != prj->units.end(); ++unit_it )
    {
      (*unit_it)->findStemnt( (void (*)(Statement*))&ParserC::statementCallback );
    }
    
    // finish progress, if enabled
    //
    if( Params.verbose_level > 0 )
      printf( " %7.2f %%  done\n", 100.0 );
  }

  delete prj;

  return !error;
}


// private methods
//

void
ParserC::statementCallback( void* st )
{
  // show progress, if enabled
  //
  if( Params.verbose_level )
  {
    static uint32_t stemnt_num = theGenerator->m_parser->m_statementNum;
    static uint32_t stemnt_count = 0;
    
    if( stemnt_count < stemnt_num ) stemnt_count++;
    
    printf( " %7.2f %%\r",
            (100.0 * (double)stemnt_count) / (double)stemnt_num );
    fflush( stdout );
  }

  Statement* ct_st = static_cast<Statement*>( st );

  std::stringstream tmp;
  int i;

  // return immediately, if statement isn't a declaration
  if( !ct_st->isDeclaration() ) return;

  DeclStemnt* ct_decl_stemnt = static_cast<DeclStemnt*>( ct_st );
  DeclVector::const_iterator ct_decl_it;

  // iterate over declarations of this statement
  //
  for( ct_decl_it = ct_decl_stemnt->decls.begin();
       ct_decl_it != ct_decl_stemnt->decls.end(); ++ct_decl_it )
  {
    Decl* ct_decl = *ct_decl_it;

    // continue, if it's not a function declaration
    if( !ct_decl->form->isFunction() ) continue;

    FunctionType* ct_func = static_cast<FunctionType*>( ct_decl->form );

    // collect information about this function
    //
    GeneratorC::FuncS func;

    func.name = ct_decl->name->name;               // name
    func.loc.file = ct_decl_stemnt->location.file; // location (file)
    func.loc.line = ct_decl_stemnt->location.line; // location (line)

    // return type
    tmp.str("");
    ct_func->subType->printType( tmp, NULL, true, 0 );
    func.rettype = tmp.str();

    // return type is 'void' ?
    func.noret =
      (ct_func->subType->isBaseType() &&
       static_cast<BaseType*>(ct_func->subType)->typemask == BT_Void );

    // iterate over arguments of this function
    //
    for( i = 0; i < ct_func->nArgs; i++)
    {
      Decl* ct_arg = ct_func->args[i];

      // variable number of arguments '...' ?
      //
      if( ct_arg->form->isBaseType() &&
          static_cast<BaseType*>(ct_arg->form)->typemask == BT_Ellipsis )
      {
        // functions with variable number of arguments are not supported yet
        break;
      }
      // argument type is 'void' ?
      //
      else if( ct_arg->form->isBaseType() &&
               static_cast<BaseType*>(ct_arg->form)->typemask == BT_Void )
      {
        // incompatible argument type
        if( i > 0 ) break;
      }
      // regular argument
      //
      else
      {
        GeneratorC::FuncS::ArgS func_arg;

        // argument name
        //
        if( ct_arg->name )
        {
          func_arg.name = ct_arg->name->name;
        }
        else
        {
          tmp.str("");
          tmp << "_arg_" << i;
          func_arg.name = tmp.str();
        }

        // argument type
        // 'int * arg[]' -> 'int *[]'
        tmp.str("");
        ct_arg->form->printType( tmp, NULL, true, 0 );
        func_arg.type = tmp.str();

        // argument type - base
        // 'int * arg[]' -> 'int'
        tmp.str("");
        ct_arg->form->printBase( tmp, 0 );
        func_arg.type_base = tmp.str();

        // argument type - before
        // 'int * arg[]' -> '*'
        tmp.str("");
        ct_arg->form->printBefore( tmp, NULL, 0 );
        func_arg.type_before = tmp.str();

        // argument type - after
        // 'int * arg[]' -> '[]'
        tmp.str("");
        ct_arg->form->printAfter( tmp );
        func_arg.type_after = tmp.str();

        // add argument
        func.args.push_back( func_arg );
      }
    }

    // ignore function, if whose arguments could not be processed
    if( i < ct_func->nArgs )
      continue;

    // write wrapper function
    theGenerator->writeFunction( func );
  }
}
