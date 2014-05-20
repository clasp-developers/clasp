// Symbol table
#include "clangSys_scrape_flag.h"
// SYMBOL_TABLE_ENTRY ClangSysToolingPkg    0 dummySymbol                    DUMMY-SYMBOL                   export ; cName=_sym_dummySymbol lispName=DUMMY-SYMBOL
// SYMBOL_TABLE_ENTRY  ClangSysPkg    1 Stmt_O                         STMT                           private ; cName=_sym_Stmt_O lispName=STMT
// SYMBOL_TABLE_ENTRY  ClangSysPkg    2 ASTFrontendAction_O            AST-FRONTEND-ACTION            export ; cName=_sym_ASTFrontendAction_O lispName=AST-FRONTEND-ACTION
// SYMBOL_TABLE_ENTRY ClangSysToolingPkg    3 ClangTool_O                    CLANG-TOOL                     private ; cName=_sym_ClangTool_O lispName=CLANG-TOOL
// SYMBOL_TABLE_ENTRY  ClangSysPkg    4 VisitStmt                      VISIT-STMT                     export ; cName=_sym_VisitStmt lispName=VISIT-STMT
// SYMBOL_TABLE_ENTRY ClangSysToolingPkg    5 CompilationDatabase_O          COMPILATION-DATABASE           private ; cName=_sym_CompilationDatabase_O lispName=COMPILATION-DATABASE
// SYMBOL_TABLE_ENTRY  ClangSysPkg    6 RecursiveASTVisitor_O          RECURSIVE-AST-VISITOR          export ; cName=_sym_RecursiveASTVisitor_O lispName=RECURSIVE-AST-VISITOR
// SYMBOL_TABLE_ENTRY  ClangSysPkg    7 ASTConsumer_O                  AST-CONSUMER                   export ; cName=_sym_ASTConsumer_O lispName=AST-CONSUMER
// SYMBOL_TABLE_ENTRY ClangSysToolingPkg    8 JSONCompilationDatabase_O      JSON-COMPILATION-DATABASE      private ; cName=_sym_JSONCompilationDatabase_O lispName=JSON-COMPILATION-DATABASE
#ifdef ClangSysToolingPkg_SYMBOLS
DO_SYMBOL(_sym_dummySymbol,0,ClangSysToolingPkg,"DUMMY-SYMBOL",true);
DO_SYMBOL(_sym_ClangTool_O,3,ClangSysToolingPkg,"CLANG-TOOL",false);
DO_SYMBOL(_sym_CompilationDatabase_O,5,ClangSysToolingPkg,"COMPILATION-DATABASE",false);
DO_SYMBOL(_sym_JSONCompilationDatabase_O,8,ClangSysToolingPkg,"JSON-COMPILATION-DATABASE",false);
#endif
#ifdef ClangSysPkg_SYMBOLS
DO_SYMBOL(_sym_Stmt_O,1,ClangSysPkg,"STMT",false);
DO_SYMBOL(_sym_ASTFrontendAction_O,2,ClangSysPkg,"AST-FRONTEND-ACTION",true);
DO_SYMBOL(_sym_VisitStmt,4,ClangSysPkg,"VISIT-STMT",true);
DO_SYMBOL(_sym_RecursiveASTVisitor_O,6,ClangSysPkg,"RECURSIVE-AST-VISITOR",true);
DO_SYMBOL(_sym_ASTConsumer_O,7,ClangSysPkg,"AST-CONSUMER",true);
#endif
