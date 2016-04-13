

#define CL_DEFUN(name,ll,decl,docstring)  CL_DEFUNxxxBEGIN name ll decl docstring CL_DEFUNxxxEND

CL_DEFUN(core_foo,R"ll((a b))ll","decl()decl",R"docs(This is\n\
over multiple\n\
lines)docs")
