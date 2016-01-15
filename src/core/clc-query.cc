#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/arguments.h>
#include <clasp/core/primitives.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/predicates.h>
#include <clasp/core/clc.h>
#include <clasp/core/clc-ast.h>
#include <clasp/core/clc-env-info.h>
#include <clasp/core/clc-query.h>
#include <clasp/core/wrappers.h>


namespace clcenv {

EXPOSE_CLASS(clcenv,Info_O);
EXPOSE_CLASS(clcenv,FunctionInfo_O);
EXPOSE_CLASS(clcenv,VariableInfo_O);
EXPOSE_CLASS(clcenv,LexicalVariableInfo_O);
EXPOSE_CLASS(clcenv,SpecialVariableInfo_O);
EXPOSE_CLASS(clcenv,ConstantVariableInfo_O);
EXPOSE_CLASS(clcenv,SymbolMacroInfo_O);
EXPOSE_CLASS(clcenv,LocalFunctionInfo_O);
EXPOSE_CLASS(clcenv,GlobalFunctionInfo_O);
EXPOSE_CLASS(clcenv,LocalMacroInfo_O);
EXPOSE_CLASS(clcenv,GlobalMacroInfo_O);
EXPOSE_CLASS(clcenv,SpecialOperatorInfo_O);
EXPOSE_CLASS(clcenv,BlockInfo_O);
EXPOSE_CLASS(clcenv,OptimizeInfo_O);

};
