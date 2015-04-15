
(DEFPARAMETER *CLEAVIR-SYSTEM*
  '(:INIT CORE::KERNEL/INIT CORE::KERNEL/CMP/JIT-SETUP CORE::KERNEL/CLSYMBOLS
    :START CORE::KERNEL/LSP/FOUNDATION CORE::KERNEL/LSP/EXPORT
    CORE::KERNEL/LSP/DEFMACRO :DEFMACRO CORE::KERNEL/LSP/HELPFILE
    CORE::KERNEL/LSP/EVALMACROS CORE::KERNEL/LSP/CLASPMACROS :MACROS
    CORE::KERNEL/LSP/TESTING CORE::KERNEL/LSP/MAKEARRAY
    CORE::KERNEL/LSP/ARRAYLIB CORE::KERNEL/LSP/SETF CORE::KERNEL/LSP/LISTLIB
    CORE::KERNEL/LSP/MISLIB CORE::KERNEL/LSP/DEFSTRUCT CORE::KERNEL/LSP/PREDLIB
    CORE::KERNEL/LSP/SEQ CORE::KERNEL/LSP/CMUUTIL CORE::KERNEL/LSP/SEQMACROS
    CORE::KERNEL/LSP/IOLIB CORE::KERNEL/LSP/PROFILING :TINY :PRE-CMP
    CORE::KERNEL/CMP/PACKAGES CORE::KERNEL/CMP/CMPSETUP
    CORE::KERNEL/CMP/CMPENV-FUN CORE::KERNEL/CMP/CMPENV-PROCLAIM
    CORE::KERNEL/CMP/CMPGLOBALS CORE::KERNEL/CMP/CMPTABLES
    CORE::KERNEL/CMP/CMPVAR CORE::KERNEL/CMP/CMPUTIL
    CORE::KERNEL/CMP/CMPINTRINSICS CORE::KERNEL/CMP/CMPIR
    CORE::KERNEL/CMP/CMPEH CORE::KERNEL/CMP/DEBUGINFO
    CORE::KERNEL/CMP/LAMBDALISTVA CORE::KERNEL/CMP/CMPVARS
    CORE::KERNEL/CMP/CMPQUOTE CORE::KERNEL/CMP/CMPOBJ CORE::KERNEL/CMP/COMPILER
    CORE::KERNEL/CMP/COMPILEFILE CORE::KERNEL/CMP/CMPBUNDLE
    CORE::KERNEL/CMP/CMPWALK :CMP :STAGE1 :CMPREPL CORE::KERNEL/LSP/LOGGING
    CORE::KERNEL/LSP/SEQLIB CORE::KERNEL/LSP/TRACE :WAS-PRE-CMP
    CORE::KERNEL/LSP/SHARPMACROS CORE::KERNEL/LSP/ASSERT
    CORE::KERNEL/LSP/NUMLIB CORE::KERNEL/LSP/DESCRIBE CORE::KERNEL/LSP/MODULE
    CORE::KERNEL/LSP/LOOP2 CORE::KERNEL/LSP/ASSORTED CORE::KERNEL/LSP/PACKLIB
    CORE::KERNEL/LSP/DEFPACKAGE CORE::KERNEL/LSP/FORMAT :MIN
    CORE::KERNEL/CLOS/PACKAGE CORE::KERNEL/CLOS/HIERARCHY CORE::KERNEL/CLOS/CPL
    CORE::KERNEL/CLOS/STD-SLOT-VALUE CORE::KERNEL/CLOS/SLOT
    CORE::KERNEL/CLOS/BOOT CORE::KERNEL/CLOS/KERNEL CORE::KERNEL/CLOS/METHOD
    CORE::KERNEL/CLOS/COMBIN CORE::KERNEL/CLOS/STD-ACCESSORS
    CORE::KERNEL/CLOS/DEFCLASS CORE::KERNEL/CLOS/SLOTVALUE
    CORE::KERNEL/CLOS/STANDARD CORE::KERNEL/CLOS/BUILTIN
    CORE::KERNEL/CLOS/CHANGE CORE::KERNEL/CLOS/STDMETHOD
    CORE::KERNEL/CLOS/GENERIC :GENERIC CORE::KERNEL/CLOS/FIXUP
    CORE::KERNEL/CLOS/EXTRACLASSES CORE::KERNEL/LSP/DEFVIRTUAL :STAGE3
    CORE::KERNEL/CLOS/CONDITIONS CORE::KERNEL/CLOS/PRINT
    CORE::KERNEL/CLOS/STREAMS CORE::KERNEL/LSP/PPRINT CORE::KERNEL/CLOS/INSPECT
    :CLOS CORE::KERNEL/LSP/FFI :FRONT CORE::KERNEL/LSP/TOP :ALL
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
    |kernel/contrib/sicl/Code/Cleavir/HIR-transformations/hir-transformations|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/MIR/general|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/MIR/graphviz-drawing|
    |kernel/contrib/sicl/Code/Cleavir/Intermediate-representation/HIR-to-MIR/general|
    |kernel/contrib/sicl/Code/Cleavir/Utilities/packages|
    |kernel/contrib/sicl/Code/Cleavir/Utilities/utilities|
    |kernel/contrib/sicl/Code/Cleavir/Basic-blocks/packages|
    |kernel/contrib/sicl/Code/Cleavir/Basic-blocks/basic-blocks|
    :temp
    |kernel/cleavir/packages| |kernel/cleavir/system| |kernel/cleavir/ast|
    |kernel/cleavir/convert-form| |kernel/cleavir/convert-special|
    |kernel/cleavir/hir| |kernel/cleavir/introduce-invoke|
    |kernel/cleavir/setup| |kernel/cleavir/ast-to-hir| |kernel/cleavir/mir|
    |kernel/cleavir/hir-to-mir| |kernel/cleavir/landing-pad|
    |kernel/cleavir/ir| |kernel/cleavir/arguments| |kernel/cleavir/gml-drawing|
    |kernel/cleavir/translate|
    :pre-auto-compile
    CORE::KERNEL/CLEAVIR/AUTO-COMPILE
    :CLEAVIR-CLASP)) 
