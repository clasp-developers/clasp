
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
    |kernel/contrib/sicl/Code/Cleavir/Input-output/packages|
    |kernel/contrib/sicl/Code/Cleavir/Input-output/io|
    |kernel/contrib/sicl/Code/Cleavir/Meter/packages|
    |kernel/contrib/sicl/Code/Cleavir/Meter/meter|
    |kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/packages|
    |kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/general-purpose-asts|
    |kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/fixnum-related-asts|
    |kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/float-related-asts|
    |kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/cons-related-asts|
    |kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/standard-object-related-asts|
    |kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/array-related-asts|
    |kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/graphviz-drawing|
    |kernel/contrib/sicl/Code/Cleavir/Abstract-syntax-tree/map-ast|
    |kernel/contrib/sicl/Code/Cleavir/AST-transformations/packages|
    |kernel/contrib/sicl/Code/Cleavir/AST-transformations/clone|
    |kernel/contrib/sicl/Code/Cleavir/AST-transformations/replace|
    |kernel/contrib/sicl/Code/Cleavir/AST-transformations/hoist-load-time-value|
    |kernel/contrib/sicl/Code/Cleavir/Primop/packages|
    |kernel/contrib/sicl/Code/Cleavir/Internationalization/packages|
    |kernel/contrib/sicl/Code/Cleavir/Internationalization/locale|
    |kernel/contrib/sicl/Code/Cleavir/Internationalization/date|
    |kernel/contrib/sicl/Code/Cleavir/Internationalization/language|
    |kernel/contrib/sicl/Code/Cleavir/Internationalization/language-english|
    |kernel/contrib/sicl/Code/Cleavir/Internationalization/language-francais|
    |kernel/contrib/sicl/Code/Cleavir/Internationalization/condition|
    |kernel/contrib/sicl/Code/Cleavir/Internationalization/init|
    |kernel/contrib/sicl/Code/Cleavir/Code-utilities/packages|
    |kernel/contrib/sicl/Code/Cleavir/Code-utilities/conditions|
    |kernel/contrib/sicl/Code/Cleavir/Code-utilities/condition-reporters-english|
    |kernel/contrib/sicl/Code/Cleavir/Code-utilities/argcount|
    |kernel/contrib/sicl/Code/Cleavir/Code-utilities/form|
    |kernel/contrib/sicl/Code/Cleavir/Code-utilities/list-structure|
    |kernel/contrib/sicl/Code/Cleavir/Code-utilities/declarations|
    |kernel/contrib/sicl/Code/Cleavir/Code-utilities/lambda-lists|
    |kernel/contrib/sicl/Code/Cleavir/Code-utilities/destructuring|
    |kernel/contrib/sicl/Code/Cleavir/Environment/packages|
    |kernel/contrib/sicl/Code/Cleavir/Environment/conditions|
    |kernel/contrib/sicl/Code/Cleavir/Environment/condition-reporters-english|
    |kernel/contrib/sicl/Code/Cleavir/Environment/query|
    |kernel/contrib/sicl/Code/Cleavir/Environment/augmentation-functions|
    |kernel/contrib/sicl/Code/Cleavir/Environment/default-augmentation-classes|
    |kernel/contrib/sicl/Code/Cleavir/Environment/default-info-methods|
    |kernel/contrib/sicl/Code/Cleavir/Environment/eval|
    |kernel/contrib/sicl/Code/Cleavir/Generate-AST/packages|
    |kernel/contrib/sicl/Code/Cleavir/Generate-AST/conditions|
    |kernel/contrib/sicl/Code/Cleavir/Generate-AST/condition-reporters-english|
    |kernel/contrib/sicl/Code/Cleavir/Generate-AST/source-tracking|
    |kernel/contrib/sicl/Code/Cleavir/Generate-AST/check-special-form-syntax|
    |kernel/contrib/sicl/Code/Cleavir/Generate-AST/environment-query|
    |kernel/contrib/sicl/Code/Cleavir/Generate-AST/utilities|
    |kernel/contrib/sicl/Code/Cleavir/Generate-AST/minimal-compilation|
    |kernel/contrib/sicl/Code/Cleavir/Generate-AST/generate-ast|
    |kernel/contrib/sicl/Code/Cleavir/Generate-AST/convert-form|
    |kernel/contrib/sicl/Code/Cleavir/Generate-AST/convert-special|
    |kernel/contrib/sicl/Code/Cleavir/Generate-AST/convert-primop|
    |kernel/contrib/sicl/Code/Cleavir/Generate-AST/ast-from-file|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/packages|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/datum|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/instruction|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/graph-modifications|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/instruction-mixin-classes|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/graphviz-drawing|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/map-instructions-arbitrary-order|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/set-predecessors|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/map-instructions|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/data|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/box-unbox-mixins|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/side-effect-mixins|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/general-purpose-instructions|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/fixnum-related-instructions|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/integer-related-instructions|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/float-related-instructions|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/cons-related-instructions|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/standard-object-related-instructions|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/array-related-instructions|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/multiple-value-related-instructions|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/environment-related-instructions|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR/graphviz-drawing|
    |kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/packages|
    |kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/context|
    |kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/compile-general-purpose-asts|
    |kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/compile-fixnum-related-asts|
    |kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/compile-float-related-asts|
    |kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/compile-cons-related-asts|
    |kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/compile-standard-object-related-asts|
    |kernel/contrib/sicl/Code/Cleavir/AST-to-HIR/compile-array-related-asts|
    |kernel/contrib/sicl/Code/Cleavir/HIR-transformations/packages|
    |kernel/contrib/sicl/Code/Cleavir/HIR-transformations/traverse|
    |kernel/contrib/sicl/Code/Cleavir/HIR-transformations/inline-calls|
    |kernel/contrib/sicl/Code/Cleavir/HIR-transformations/static-few-assignments|
    |kernel/contrib/sicl/Code/Cleavir/HIR-transformations/type-inference|
    |kernel/contrib/sicl/Code/Cleavir/HIR-transformations/eliminate-typeq|
    |kernel/contrib/sicl/Code/Cleavir/HIR-transformations/simplify-box-unbox|
    |kernel/contrib/sicl/Code/Cleavir/HIR-transformations/segregate-lexicals|
    |kernel/contrib/sicl/Code/Cleavir/HIR-transformations/eliminate-superfluous-temporaries|
    |kernel/contrib/sicl/Code/Cleavir/HIR-transformations/hir-transformations|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/MIR/general|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/MIR/graphviz-drawing|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR-to-MIR/general|
    |kernel/contrib/sicl/Code/Cleavir/Utilities/packages|
    |kernel/contrib/sicl/Code/Cleavir/Utilities/utilities|
    |kernel/contrib/sicl/Code/Cleavir/Basic-blocks/packages|
    |kernel/contrib/sicl/Code/Cleavir/Basic-blocks/basic-blocks|
    |kernel/contrib/sicl/Code/Types/Additional/packages|
    |kernel/contrib/sicl/Code/Types/Additional/types|
    |kernel/contrib/sicl/Code/Conditions/Additional/packages|
    |kernel/contrib/sicl/Code/Conditions/Additional/conditions|
    |kernel/contrib/sicl/Code/Conditions/Additional/condition-reporters-en|
    |kernel/cleavir/packages| |kernel/cleavir/system| |kernel/cleavir/ast|
    |kernel/cleavir/convert-form| |kernel/cleavir/convert-special|
    |kernel/cleavir/hir| |kernel/cleavir/introduce-invoke|
    |kernel/cleavir/setup| |kernel/cleavir/ast-to-hir| |kernel/cleavir/mir|
    |kernel/cleavir/hir-to-mir| |kernel/cleavir/ir|
    |kernel/cleavir/gml-drawing| |kernel/cleavir/landing-pad|
    |kernel/cleavir/arguments| |kernel/cleavir/translate| :CLEAVIR-CLASP
    #P"/kernel/cleavir/inline" #P"/kernel/cleavir/auto-compile" :AUTO-COMPILE
    :CCLASP)) 