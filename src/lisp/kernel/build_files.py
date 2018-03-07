import cleavir

def aclasp(wrappers):
    result = [
        "src/lisp/kernel/tag/start",
        "src/lisp/kernel/lsp/prologue"]
    # Don't use wrappers for now - the direct CALL to THROW-FUNCTION needs to use INVOKE and the landing-pad
    # otherwise the exception handling will break because cleanup forms in the wrapper won't be evaluated
    # This is a problem if cc_push_InvocationHistoryFrame/cc_pull_InvocationHistoryFrame are used
    if (wrappers):
        result = result + [
            "src/lisp/kernel/lsp/direct-calls",
            "generated/cl-wrappers"
        ]
    result = result + [  
        "src/lisp/kernel/tag/min-start",
        "src/lisp/kernel/init",
        "src/lisp/kernel/tag/after-init",
        "src/lisp/kernel/cmp/runtime-info",
        "src/lisp/kernel/cmp/jit-setup",
        "src/lisp/kernel/clsymbols",
        "src/lisp/kernel/lsp/packages",
        "src/lisp/kernel/lsp/foundation",
        "src/lisp/kernel/lsp/export",
        "src/lisp/kernel/lsp/defmacro",
        "src/lisp/kernel/lsp/helpfile",
        "src/lisp/kernel/lsp/evalmacros",
        "src/lisp/kernel/lsp/claspmacros",
        "src/lisp/kernel/lsp/source-transformations",
        "src/lisp/kernel/lsp/arraylib",
        "src/lisp/kernel/lsp/setf",
        "src/lisp/kernel/lsp/listlib",
        "src/lisp/kernel/lsp/mislib",
        "src/lisp/kernel/lsp/defstruct",
        "src/lisp/kernel/lsp/predlib",
        "src/lisp/kernel/lsp/cdr-5",
        "src/lisp/kernel/lsp/seq",
        "src/lisp/kernel/lsp/cmuutil",
        "src/lisp/kernel/lsp/seqmacros",
        "src/lisp/kernel/lsp/seqlib",
        "src/lisp/kernel/lsp/iolib",
        "src/lisp/kernel/lsp/sharpmacros",
        "src/lisp/kernel/lsp/trace",
        "src/lisp/kernel/lsp/backtrace",
        "src/lisp/kernel/cmp/cmpexports",
        "src/lisp/kernel/cmp/cmpsetup",
        "src/lisp/kernel/cmp/cmpglobals",
        "src/lisp/kernel/cmp/cmputil",
        "src/lisp/kernel/cmp/cmpintrinsics",
        "src/lisp/kernel/cmp/primitives",
        "src/lisp/kernel/cmp/cmpir",
        "src/lisp/kernel/cmp/cmpeh",
        "src/lisp/kernel/cmp/debuginfo",
        "src/lisp/kernel/cmp/codegen-vars",
        "src/lisp/kernel/cmp/arguments",
        "src/lisp/kernel/cmp/cmplambda",
        "src/lisp/kernel/cmp/cmprunall",
        "src/lisp/kernel/cmp/cmpliteral",
        "src/lisp/kernel/cmp/typeq",
        "src/lisp/kernel/cmp/cmpfastgf",
        "src/lisp/kernel/cmp/codegen-special-form",
        "src/lisp/kernel/cmp/codegen",
        "src/lisp/kernel/cmp/compile",
        "src/lisp/kernel/cmp/codegen-toplevel",
        "src/lisp/kernel/cmp/compile-file",
        "src/lisp/kernel/cmp/disassemble",
        "src/lisp/kernel/cmp/external-clang",
        "src/lisp/kernel/cmp/cmpname",
        "src/lisp/kernel/cmp/cmpbundle",
        "src/lisp/kernel/cmp/cmprepl",
        "src/lisp/kernel/tag/min-pre-epilogue",
        "src/lisp/kernel/lsp/epilogue-aclasp",
        "src/lisp/kernel/tag/min-end"]
    return result

def bclasp(wrappers):
    files = aclasp(wrappers) + [
        "src/lisp/kernel/tag/bclasp-start",
        "src/lisp/kernel/cmp/cmpwalk",
        "src/lisp/kernel/lsp/assert",
        "src/lisp/kernel/lsp/numlib",
        "src/lisp/kernel/lsp/describe",
        "src/lisp/kernel/lsp/module",
        "src/lisp/kernel/lsp/loop2",
        "src/lisp/kernel/cmp/opt", # need loop
        "src/lisp/kernel/cmp/opt-type",
        "src/lisp/kernel/cmp/opt-sequence",
        "src/lisp/kernel/cmp/opt-cons",
        "src/lisp/kernel/lsp/shiftf-rotatef",
        "src/lisp/kernel/lsp/assorted",
        "src/lisp/kernel/lsp/packlib",
        "src/lisp/kernel/lsp/defpackage",
        "src/lisp/kernel/lsp/format",
        "src/lisp/kernel/lsp/mp",
        "src/lisp/kernel/clos/package",
        "src/lisp/kernel/clos/hierarchy",
        "src/lisp/kernel/clos/cpl",
        "src/lisp/kernel/clos/std-slot-value",
        "src/lisp/kernel/clos/slot",
        "src/lisp/kernel/clos/boot",
        "src/lisp/kernel/clos/kernel",
        "src/lisp/kernel/clos/closfastgf",
        "src/lisp/kernel/clos/satiation",
        "src/lisp/kernel/clos/method",
        "src/lisp/kernel/clos/combin",
        "src/lisp/kernel/clos/std-accessors",
        "src/lisp/kernel/clos/defclass",
        "src/lisp/kernel/clos/slotvalue",
        "src/lisp/kernel/clos/standard",
        "src/lisp/kernel/clos/builtin",
        "src/lisp/kernel/clos/change",
        "src/lisp/kernel/clos/stdmethod",
        "src/lisp/kernel/clos/generic",
        "src/lisp/kernel/clos/fixup",
        "src/lisp/kernel/clos/extraclasses",
        "src/lisp/kernel/lsp/source-location",
        "src/lisp/kernel/lsp/defvirtual",
        "src/lisp/kernel/clos/conditions",
        "src/lisp/kernel/clos/print",
        "src/lisp/kernel/clos/streams",
        "src/lisp/kernel/lsp/pprint",
        "src/lisp/kernel/clos/inspect",
        "src/lisp/kernel/lsp/fli",
        "src/lisp/modules/sockets/sockets",
        "src/lisp/kernel/lsp/top",
        "src/lisp/kernel/cmp/export-to-cleavir",
        "src/lisp/kernel/tag/pre-epilogue-bclasp",
        "src/lisp/kernel/lsp/epilogue-bclasp",
        "src/lisp/kernel/tag/bclasp"
    ]
    return files

def cclasp(wrappers):
    return bclasp(wrappers) + cleavir.cleavir_parts + [
        "src/lisp/kernel/tag/pre-epilogue-cclasp",
        "src/lisp/kernel/lsp/epilogue-cclasp",
        "src/lisp/kernel/tag/cclasp" ]

def dump_names(l):
    for x in l:
        print("%s" % x),
    print("")
if __name__ == '__main__':
    print("Running from command line")
    print("aclasp: ")
    dump_names(aclasp(False))
    print("bclasp: ")
    dump_names(bclasp(False))
    print("cclasp: ")
    dump_names(cclasp(False))
