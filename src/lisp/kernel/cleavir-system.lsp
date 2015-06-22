
(DEFPARAMETER *CLEAVIR-SYSTEM*
  '(:INIT #P"/kernel/init" :CLEAVIR-INJECTION #P"/kernel/cmp/jit-setup"
    #P"/kernel/clsymbols" :START #P"/kernel/lsp/packages"
    #P"/kernel/lsp/foundation" #P"/kernel/lsp/export" #P"/kernel/lsp/defmacro"
    :DEFMACRO #P"/kernel/lsp/helpfile" #P"/kernel/lsp/evalmacros"
    #P"/kernel/lsp/claspmacros" :MACROS #P"/kernel/lsp/testing"
    #P"/kernel/lsp/makearray" #P"/kernel/lsp/arraylib" #P"/kernel/lsp/setf"
    #P"/kernel/lsp/listlib" #P"/kernel/lsp/mislib" #P"/kernel/lsp/defstruct"
    #P"/kernel/lsp/predlib" #P"/kernel/lsp/seq" #P"/kernel/lsp/cmuutil"
    #P"/kernel/lsp/seqmacros" #P"/kernel/lsp/iolib" #P"/kernel/lsp/profiling"
    :TINY :PRE-CMP #P"/kernel/cmp/packages" #P"/kernel/cmp/cmpsetup"
    #P"/kernel/cmp/cmpglobals" #P"/kernel/cmp/cmptables" #P"/kernel/cmp/cmpvar"
    #P"/kernel/cmp/cmputil" #P"/kernel/cmp/cmpintrinsics" #P"/kernel/cmp/cmpir"
    #P"/kernel/cmp/cmpeh" #P"/kernel/cmp/debuginfo"
    #P"/kernel/cmp/lambdalistva" #P"/kernel/cmp/cmpvars"
    #P"/kernel/cmp/cmpquote" #P"/kernel/cmp/cmpobj" #P"/kernel/cmp/compiler"
    #P"/kernel/cmp/compilefile" #P"/kernel/cmp/cmpbundle"
    #P"/kernel/cmp/cmpwalk" :CMP :STAGE1 #P"/kernel/cmp/cmprepl" :CMPREPL
    #P"/kernel/lsp/logging" #P"/kernel/lsp/seqlib" #P"/kernel/lsp/trace"
    :WAS-PRE-CMP #P"/kernel/lsp/sharpmacros" #P"/kernel/lsp/assert"
    #P"/kernel/lsp/numlib" #P"/kernel/lsp/describe" #P"/kernel/lsp/module"
    #P"/kernel/lsp/loop2" #P"/kernel/lsp/shiftf-rotatef"
    #P"/kernel/lsp/assorted" #P"/kernel/lsp/packlib" #P"/kernel/lsp/defpackage"
    #P"/kernel/lsp/format" :MIN #P"/kernel/clos/package"
    #P"/kernel/clos/hierarchy" #P"/kernel/clos/cpl"
    #P"/kernel/clos/std-slot-value" #P"/kernel/clos/slot" #P"/kernel/clos/boot"
    #P"/kernel/clos/kernel" #P"/kernel/clos/method" #P"/kernel/clos/combin"
    #P"/kernel/clos/std-accessors" #P"/kernel/clos/defclass"
    #P"/kernel/clos/slotvalue" #P"/kernel/clos/standard"
    #P"/kernel/clos/builtin" #P"/kernel/clos/change" #P"/kernel/clos/stdmethod"
    #P"/kernel/clos/generic" :GENERIC #P"/kernel/clos/fixup"
    #P"/kernel/clos/extraclasses" #P"/kernel/lsp/defvirtual" :STAGE3
    #P"/kernel/clos/conditions" #P"/kernel/clos/print" #P"/kernel/clos/streams"
    #P"/kernel/lsp/pprint" #P"/kernel/clos/inspect" :CLOS #P"/kernel/lsp/ffi"
    #P"/sockets/sockets" :FRONT #P"/kernel/lsp/top" :ALL :BCLASP
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Input-output/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Input-output/io|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Meter/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Meter/meter|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/general-purpose-asts|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/fixnum-related-asts|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/float-related-asts|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/cons-related-asts|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/standard-object-related-asts|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/array-related-asts|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/graphviz-drawing|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/map-ast|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/AST-transformations/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/AST-transformations/clone|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/AST-transformations/replace|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/AST-transformations/hoist-load-time-value|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Primop/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Internationalization/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Internationalization/locale|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Internationalization/date|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Internationalization/language|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Internationalization/language-english|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Internationalization/language-francais|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Internationalization/condition|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Internationalization/init|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Code-utilities/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Code-utilities/conditions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Code-utilities/condition-reporters-english|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Code-utilities/argcount|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Code-utilities/form|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Code-utilities/list-structure|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Code-utilities/declarations|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Code-utilities/lambda-lists|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Code-utilities/destructuring|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Environment/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Environment/conditions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Environment/condition-reporters-english|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Environment/query|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Environment/augmentation-functions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Environment/default-augmentation-classes|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Environment/default-info-methods|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Environment/eval|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Generate-AST/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Generate-AST/conditions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Generate-AST/condition-reporters-english|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Generate-AST/source-tracking|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Generate-AST/check-special-form-syntax|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Generate-AST/environment-query|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Generate-AST/utilities|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Generate-AST/minimal-compilation|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Generate-AST/generate-ast|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Generate-AST/convert-form|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Generate-AST/convert-special|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Generate-AST/convert-primop|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Generate-AST/ast-from-file|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/datum|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/instruction|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/graph-modifications|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/instruction-mixin-classes|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/graphviz-drawing|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/map-instructions-arbitrary-order|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/set-predecessors|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/map-instructions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/data|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/box-unbox-mixins|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/side-effect-mixins|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/general-purpose-instructions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/fixnum-related-instructions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/integer-related-instructions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/float-related-instructions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/cons-related-instructions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/standard-object-related-instructions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/array-related-instructions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/multiple-value-related-instructions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/environment-related-instructions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/graphviz-drawing|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/context|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/compile-general-purpose-asts|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/compile-fixnum-related-asts|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/compile-float-related-asts|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/compile-cons-related-asts|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/compile-standard-object-related-asts|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/compile-array-related-asts|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/HIR-transformations/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/HIR-transformations/traverse|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/HIR-transformations/inline-calls|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/HIR-transformations/static-few-assignments|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/HIR-transformations/type-inference|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/HIR-transformations/eliminate-typeq|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/HIR-transformations/simplify-box-unbox|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/HIR-transformations/segregate-lexicals|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/HIR-transformations/eliminate-superfluous-temporaries|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/HIR-transformations/hir-transformations|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/MIR/general|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/MIR/graphviz-drawing|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR-to-MIR/general|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Utilities/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Utilities/utilities|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Basic-blocks/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Cleavir/Basic-blocks/basic-blocks|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Types/Additional/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Types/Additional/types|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Conditions/Additional/packages|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Conditions/Additional/conditions|
    |/Users/meister/Development/clasp/src/lisp/kernel/contrib/sicl/Code/Conditions/Additional/condition-reporters-en|
    |kernel/cleavir/packages| |kernel/cleavir/system| |kernel/cleavir/ast|
    |kernel/cleavir/convert-form| |kernel/cleavir/convert-special|
    |kernel/cleavir/hir| |kernel/cleavir/introduce-invoke|
    |kernel/cleavir/setup| |kernel/cleavir/ast-to-hir| |kernel/cleavir/mir|
    |kernel/cleavir/hir-to-mir| |kernel/cleavir/ir|
    |kernel/cleavir/gml-drawing| |kernel/cleavir/landing-pad|
    |kernel/cleavir/arguments| |kernel/cleavir/translate| :CLEAVIR-CLASP
    #P"/kernel/cleavir/inline" #P"/kernel/cleavir/auto-compile" :AUTO-COMPILE
    :CCLASP)) 