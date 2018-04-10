;; Run sbcl with sbcl --dynamic-space-size 4096

;;; Make sure you run slime from clasp/src/main


(progn
  (defparameter *clasp-home* #P"/Users/meister/Development/clasp/")
  (asdf:initialize-source-registry `(:source-registry (:directory ,(merge-pathnames "src/scraper/" *clasp-home*)) :ignore-inherited-configuration))
  (load (merge-pathnames "src/scraper/dependencies/bundle.lisp" *clasp-home*))
  (asdf:load-system :clasp-scraper)
  (swank:set-default-directory (merge-pathnames "build/boehm/" *clasp-home*))
  (in-package :cscrape))

(progn
  (sb-ext:gc :full t)
  (sb-sprof:with-profiling ()
    (time (cscrape:generate-sif-files '("/usr/lib/llvm-5.0/bin/clang++" "-E" "-DSCRAPING" "-I./" "-I/usr/lib/llvm-5.0/include" "-fno-omit-frame-pointer"
                                        "-mno-omit-leaf-frame-pointer" "-std=c++11" "-flto=thin" "-Wno-macro-redefined" "-Wno-deprecated-register"
                                        "-Wno-expansion-to-defined" "-Wno-return-type-c-linkage" "-Wno-invalid-offsetof" "-Wno-#pragma-messages"
                                        "-Wno-inconsistent-missing-override" "-O3" "-g" "-I." "-I../.." "-I../../src/main" "-I../../include"
                                        "-Igenerated" "-I/usr/lib/llvm-5.0/include" "-I/usr/include")
                                      "/tmp/llvmoExpose.cc" "/tmp/llvmoExpose.sif"))))

(apropos "parse-lambda-list")


(parse-lambda-list-from-signature "APFloat_sp APFloat_O::makeAPFloatFloat(core::SingleFloat_sp value)")

(separate-type-pointer "&llvm::IRBuilderBase::SetInsertPoint")
(separate-type-pointer "(void (llvm::IRBuilderBase::*)(llvm::BasicBlock *))&llvm::IRBuilderBase::SetInsertPoint")



(in-package :cscrape)


(apropos "generate-headers-from-all-sifs"

(cscrape::process-all-sif-files "/Users/meister/Development/cando/"
                                "/Users/meister/Development/cando/build/boehm/" 
                                '("/Users/meister/Development/cando/build/boehmdc/src/gctools/gc_interface.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/gctools/boehmGarbageCollection.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/gctools/mpsGarbageCollection.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/gctools/source_info.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/gctools/gc_boot.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/gctools/gcFunctions.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/gctools/gctoolsPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/gctools/globals.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/gctools/gcStack.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/gctools/telemetry.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/gctools/gcalloc.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/gctools/gcweak.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/gctools/memoryManagement.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/dummy.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/clcenv.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/nativeVector.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/environment.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/activationFrame.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/evaluator.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/functor.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/creator.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/sharpEqualWrapper.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/stacks.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/weakKeyMapping.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/weakHashTable.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/weakPointer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/compiler.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/genericFunction.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/accessor.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/instance.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/cache.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/float_to_string.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/primitives.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/random.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/cxxObject.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/cxxClass.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/record.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/debugger.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/smallMap.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/smallMultimap.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/hashTable.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/hashTableEq.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/hashTableEql.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/hashTableEqual.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/hashTableEqualp.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/numbers.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/numerics.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/num_arith.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/numberToString.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/num_co.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/load.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/bignum.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/write_object.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/write_array.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/print.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/sourceFileInfo.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/symbolToEnumConverter.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/core_globals.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/externalObject.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/myReadLine.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/specialForm.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/unixfsys.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/lispList.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/multiStringBuffer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/candoOpenMp.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/foundation.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/lambdaListHandler.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/lispStream.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/bits.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/write_symbol.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/corePackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/lisp.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/profiler.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/lispDefinitions.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/bundle.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/profile.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/specializer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/write_ugly.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/userData.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/wrappedPointer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/serialize.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/sexpLoadArchive.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/sexpSaveArchive.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/readtable.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/float_to_digits.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/pathname.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/commandLineOptions.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/exceptions.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/commonLispUserPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/metaClass.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/multipleValues.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/testing.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/predicates.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/write_list.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/package.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/commonLispPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/allClSymbols.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/keywordPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/extensionPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/array.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/lispMath.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/grayPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/closPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/cleavirPrimopsPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/cleavirEnvPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/compPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/bootStrapCoreSymbolMap.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/cons.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/symbol.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/builtInClass.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/standardClass.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/conditions.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/object.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/stdClass.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/metaobject.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/arguments.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/pointer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/funcallableStandardClass.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/standardObject.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/iterator.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/sysprop.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/bformat.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/backquote.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/documentation.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/lispReader.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/singleDispatchGenericFunction.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/singleDispatchMethod.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/structureObject.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/structureClass.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/null.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/forwardReferencedClass.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/character.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/designators.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/sequence.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/loadTimeValues.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/reader.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/lightProfiler.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/fileSystem.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/intArray.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/posixTime.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/clasp_ffi_package.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/core/fli.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/clbind/adapter.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/clbind/class_rep.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/clbind/open.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/clbind/class_registry.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/clbind/link_compatibility.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/clbind/scope.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/clbind/inheritance.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/clbind/clbind.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/clbind/clbindPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/clbind/class.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/clbind/derivable_class.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/serveEvent/serveEvent.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/serveEvent/serveEventPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/sockets/sockets.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/sockets/socketsPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/llvmo/debugInfoExpose.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/llvmo/debugLoc.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/llvmo/llvmoDwarf.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/llvmo/link_intrinsics.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/llvmo/intrinsics.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/llvmo/insertPoint.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/llvmo/irtests.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/llvmo/llvmoExpose.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/llvmo/llvmoPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/llvmo/clbindLlvmExpose.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/asttooling/astVisitor.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/asttooling/astExpose.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/asttooling/clangTooling.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/asttooling/asttoolingPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/asttooling/clangCompiler.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/src/main/main.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/adapt/indexedObjectBag.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/adapt/adaptPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/adapt/quickDom.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/adapt/mySaxInterface.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/adapt/objectSet.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/adapt/iterateCons.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/adapt/stringList.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/adapt/stringSet.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/adapt/symbolList.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/adapt/symbolSet.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/geom/candoBasePackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/geom/ovector3.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/geom/ovector2.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/geom/color.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/geom/matrix.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/geom/vector2.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/geom/vector3.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/geom/vector4.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/geom/omatrix.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/geom/boundingBox.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/geom/coordinateArray.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/units/dimension.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/units/unit.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/units/namedUnit.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/units/quantity.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/units/unitsPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/chemInfo.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/ffTypesDb.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/ffStretchDb.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/ffVdwDb.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/ffAngleDb.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/ffBaseDb.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/ffItorDb.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/ffPtorDb.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/ffNonbondDb.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/chemPackage.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/matter.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/aggregate.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/molecule.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/residue.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/atom.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/bond.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/atomIdMap.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/atomId.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/elements.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/restraint.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/minimizer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/angle.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/properTorsion.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/improperTorsion.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/topology.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/candoDatabase.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/constitution.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/monomerCoordinates.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/frameRecognizer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/rotamer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/atomReference.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/entity.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/pdbMonomer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/constitutionAtoms.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/stereoisomerAtoms.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/loop.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/trajectory.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/indirectAtomCoordinateReference.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/energyComponent.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/energyStretch.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/energyAngle.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/energyDihedral.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/energyNonbond.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/energyChiralRestraint.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/energyAnchorRestraint.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/energyImproperRestraint.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/energyFixedNonbond.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/energyAtomTable.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/energyFunction.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/complexRestraints.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/searchStatistics.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/plug.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/specificContext.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/calculatePosition.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/virtualAtom.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/coordSys.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/coupling.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/mbbCoreTools.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/nVector.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/oligomer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/spanningLoop.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/spline.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/superposeEngine.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/virtualSphere.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/moe.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/mol2.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/numericalFunction.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/forceField.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/linearAlgebra.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/minimizerLog.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/iterateRestraints.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/pdb.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/ringFinder.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/iterateMatter.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/macroModel.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/typeAssignmentRules.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/alias.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/superposableConformationCollection.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/zMatrix.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/structureComparer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/chimera.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/command.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/monomerPack.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/statusTracker.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/atomIndexer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/structureList.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/twister.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/largeSquareMatrix.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/randomGenerators.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/stereochemistry.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/entityNameSet.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/monomerContext.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/representedEntityNameSet.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/readAmberParameters.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/cipPrioritizer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/chemdraw.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/candoScript.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/monomer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/conformationExplorer.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/conformationCollection.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/msmarts_Parser.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/chem/gaff_Parser.sif" 
                                  "/Users/meister/Development/cando/build/boehmdc/extensions/cando/src/main/extension.sif"))






(defparameter *s* "class Test_O : T_O { stuff }")
(defparameter *b* (make-instance 'buffer-stream :buffer *s*
                                 :buffer-pathname #P"foo"
                                 :buffer-stream (make-string-input-stream *s*)))
(next-recognition-element *b*)
(process-all-recognition-elements *b*)
(file-position (buffer-stream *b*))
*recognition-elements*
(defparameter *sif-files*
  (list
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/adapt/adaptPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/adapt/indexedObjectBag.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/adapt/iterateCons.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/adapt/mySaxInterface.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/adapt/objectSet.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/adapt/quickDom.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/adapt/stringList.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/adapt/stringSet.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/adapt/symbolList.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/adapt/symbolSet.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/aggregate.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/alias.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/angle.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/atom.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/atomId.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/atomIdMap.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/atomIndexer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/atomReference.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/bond.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/calculatePosition.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/candoDatabase.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/candoScript.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/chemdraw.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/chemInfo.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/chemPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/chimera.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/cipPrioritizer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/command.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/complexRestraints.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/conformationCollection.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/conformationExplorer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/constitution.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/constitutionAtoms.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/coordSys.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/coupling.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/elements.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/energyAnchorRestraint.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/energyAngle.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/energyAtomTable.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/energyChiralRestraint.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/energyComponent.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/energyDihedral.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/energyFixedNonbond.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/energyFunction.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/energyImproperRestraint.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/energyNonbond.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/energyStretch.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/entity.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/entityNameSet.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/ffAngleDb.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/ffBaseDb.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/ffItorDb.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/ffNonbondDb.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/ffPtorDb.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/ffStretchDb.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/ffTypesDb.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/ffVdwDb.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/forceField.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/frameRecognizer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/gaff_Parser.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/improperTorsion.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/indirectAtomCoordinateReference.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/iterateMatter.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/iterateRestraints.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/largeSquareMatrix.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/linearAlgebra.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/loop.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/macroModel.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/matter.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/mbbCoreTools.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/minimizer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/minimizerLog.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/moe.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/mol2.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/molecule.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/monomer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/monomerContext.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/monomerCoordinates.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/monomerPack.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/msmarts_Parser.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/numericalFunction.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/nVector.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/oligomer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/pdb.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/pdbMonomer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/plug.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/properTorsion.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/randomGenerators.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/readAmberParameters.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/representedEntityNameSet.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/residue.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/restraint.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/ringFinder.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/rotamer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/searchStatistics.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/spanningLoop.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/specificContext.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/spline.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/statusTracker.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/stereochemistry.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/stereoisomerAtoms.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/structureComparer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/structureList.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/superposableConformationCollection.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/superposeEngine.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/topology.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/trajectory.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/twister.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/typeAssignmentRules.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/virtualAtom.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/virtualSphere.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/chem/zMatrix.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/geom/boundingBox.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/geom/candoBasePackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/geom/color.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/geom/coordinateArray.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/geom/matrix.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/geom/omatrix.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/geom/ovector2.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/geom/ovector3.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/geom/vector2.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/geom/vector3.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/geom/vector4.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/units/dimension.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/units/namedUnit.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/units/quantity.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/units/unit.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/plugins/cando/src/units/unitsPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/asttooling/astExpose.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/asttooling/asttoolingPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/asttooling/astVisitor.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/asttooling/clangCompiler.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/asttooling/clangTooling.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/clbind/adapter.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/clbind/class.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/clbind/class_registry.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/clbind/class_rep.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/clbind/clbind.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/clbind/clbindPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/clbind/derivable_class.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/clbind/inheritance.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/clbind/link_compatibility.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/clbind/open.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/clbind/scope.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/activationFrame.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/allClSymbols.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/arguments.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/array.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/arrayDisplaced.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/arrayObjects.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/backquote.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/bformat.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/bignum.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/binder.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/bits.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/bitVector.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/bootStrapCoreSymbolMap.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/builtInClass.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/bundle.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/cache.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/candoOpenMp.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/character.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/clcenv.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/cleavirEnvPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/cleavirPrimopsPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/closPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/commandLineOptions.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/commonLispPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/commonLispUserPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/compiler.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/compPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/conditions.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/cons.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/core_globals.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/corePackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/creator.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/cxxClass.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/cxxObject.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/debugger.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/designators.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/documentation.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/environment.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/evaluator.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/exceptions.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/extensionPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/externalObject.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/fileSystem.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/float_to_digits.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/float_to_string.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/forwardReferencedClass.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/foundation.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/funcallableStandardClass.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/functor.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/genericFunction.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/grayPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/hashTable.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/hashTableEq.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/hashTableEql.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/hashTableEqual.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/hashTableEqualp.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/instance.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/intArray.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/iterator.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/keywordPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/lambdaListHandler.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/lightProfiler.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/lisp.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/lispDefinitions.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/lispList.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/lispMath.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/lispReader.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/lispStream.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/lispString.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/lispVector.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/load.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/loadTimeValues.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/metaClass.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/metaobject.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/multipleValues.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/multiStringBuffer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/myReadLine.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/null.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/num_arith.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/num_co.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/numbers.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/numberToString.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/numerics.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/object.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/package.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/pathname.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/pointer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/posixTime.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/predicates.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/primitives.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/print.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/profile.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/profiler.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/random.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/reader.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/readtable.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/record.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/regex.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/sequence.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/serialize.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/sexpLoadArchive.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/sexpSaveArchive.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/sharpEqualWrapper.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/singleDispatchGenericFunction.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/singleDispatchMethod.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/smallMap.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/smallMultimap.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/sourceFileInfo.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/specialForm.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/specializer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/stacks.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/standardClass.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/standardObject.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/stdClass.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/str.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/structureClass.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/structureObject.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/strWithFillPtr.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/symbol.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/symbolToEnumConverter.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/sysprop.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/testing.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/unixfsys.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/userData.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/vectorDisplaced.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/vectorObjects.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/vectorObjectsWithFillPtr.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/weakHashTable.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/weakKeyMapping.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/weakPointer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/wrappedPointer.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/write_array.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/write_list.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/write_object.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/write_symbol.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/core/write_ugly.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/gctools/gc_boot.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/gctools/gc_interface.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/gctools/gcalloc.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/gctools/gcFunctions.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/gctools/gcStack.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/gctools/gctoolsPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/gctools/gcweak.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/gctools/globals.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/gctools/memoryManagement.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/gctools/telemetry.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/llvmo/clbindLlvmExpose.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/llvmo/debugInfoExpose.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/llvmo/debugLoc.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/llvmo/insertPoint.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/llvmo/intrinsics.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/llvmo/irtests.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/llvmo/link_intrinsics.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/llvmo/llvmoDwarf.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/llvmo/llvmoExpose.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/llvmo/llvmoPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/main/main.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/serveEvent/serveEvent.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/serveEvent/serveEventPackage.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/sockets/sockets.sif"
   "/Users/meister/Development/clasp/wbuild/boehmdc_o/src/sockets/socketsPackage.sif"
   ))

(progn
  (setq *default-pathname-defaults* #P"/Users/meister/Development/clasp/")
  (let* ((build-path (merge-pathnames #P"wbuild/boehmdc_o/"))
         (clasp-home-path *default-pathname-defaults*)
         (main-path #P"src/main/")
         (main-path (merge-pathnames main-path build-path))
         (sif-files *sif-files*))
    (process-all-sif-files clasp-home-path main-path sif-files)))

(setup-application-config *application-config*)

(generate-sif-file #P"~/Development/clasp/wbuild/src/core/cons.i" #P"~/Development/clasp/wbuild/src/core/cons.sif")
(progn
  (cscrape::do-scraping
    (list
     "/Users/meister/Development/externals-clasp/build/debug/bin/clang"
     (namestring (merge-pathnames "src/main/" cl-user::*clasp-home*))
     "bin/all-commands.txt"
     "bin/commands.txt")
  :run-preprocessor nil
  :regenerate-sifs t)
  (print "Done"))



(gethash :lisp-wrappers (cscrape::setup-application-config cscrape:*application-config*))
(cscrape::split-type-name "const string &name")


(substitute #\_ #\: "a::b")

(defparameter *a* (second cscrape::*functions*))
(print *a*)

(mapc (lambda (x)
        (format t "Function: ~a~%" x)
        (format t "types: ~a~%" (multiple-value-list (cscrape::parse-types-from-signature (cscrape::signature% x)))))
      cscrape::*functions*)

(cscrape::parse-types-from-signature (cscrape::signature% *a*))

(member #\space cscrape::+white-space+)


(position-if
 (lambda (c)
   (format t "Looking at char:~a:~%" c)
   (member c +white-space+))
 "abcd efg" :from-end t)
(split-cpps '(1 2 3 4 5 6 7) 2)
(apropos "wait")
(sb-posix:wait 
*packages-to-create*




(inherits-from* "core::Fixnum_dummy_O" "core::Integer_O" cscrape::*inheritance*)

(print "Done2")

(cscrape:extract-method-name-from-signature "inline T_sp Cons_O::setf_car(Cons_sp c)")
(untrace)
(trace cscrape::maybe-remove-one-prefix-from-start)
(gethash "llvmo::Type_O" cscrape::*classes*)
cscrape::*classes*
cscrape::*symbols*
cscrape::*functions*
(defparameter *sorted* (cscrape::sort-classes-by-inheritance *exposed-classes*))
*sorted*
*classes*
*functions*
(cscrape::inherits-from (car *classes*) (cadr *classes*) *inheritance*)q
*inheritance*
;;; Make sure you run slime from clasp/src/main


(progn
  (defparameter *clasp-home* #P"/Users/meister/Development/clasp/")
  (setf *default-pathname-defaults* (merge-pathnames "src/scraper/" *clasp-home*))
  (push :testing-scraper *features*)
  (load "scraper.lisp"))

(in-package :cscrape)

(cscrape::do-scraping
    (list
     "/Users/meister/Development/externals-clasp/build/release/bin/clang"
     (namestring (merge-pathnames "src/main/" cl-user::*clasp-home*))
     "bin/all-commands.txt"
     "bin/commands.txt")
  :run-preprocessor nil
  :regenerate-sifs t)



(trace tags::maybe-remove-one-prefix-from-start)
(untrace)
(tags::extract-method-name-from-signature "T_sp setf_car(Cons_sp x)")

(gethash "llvmo::Type_O" cscrape::*classes*)
cscrape::*tags*

(gethash "llvmo::Type_O" *exposed-classes*)
(extract-function-name-from-signature "llvm::Function *Module_O::getFunction(core::Str_sp dispatchName)" nil)

(trace tags::extract-method-name-from-signature)
(apropos "extract-method-name")
(mapc (lambda (x) (when (typep x 'tags:expose-method-tag) (print x))) *tags*)



(print "Done")

;;; ----------------------------------------------------------------------
;;;
;;; Profile the scraper
;;;
(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 10000
                                       :mode :alloc
                                       :report :flat)
  (do-scraping '("/Users/meister/Development/externals-clasp/build/release/bin/clang" "/Users/meister/Development/clasp/src/main/" "/Users/meister/Development/clasp/src/main/include/application.config" "/tmp/all-commands.txt" "/tmp/commands.txt") :run-preprocessor nil))


(print "Hello")

(tags::extract-function-name-from-signature "llvm::Function *Module_O::getFunction(core::Str_sp dispatchName)")

(trace tags::extract-method-name-from-signature)
(apropos "extract-method-name")
(mapc (lambda (x) (when (typep x 'tags:expose-method-tag) (print x))) *tags*)



(print "Done")

;;; ----------------------------------------------------------------------
;;;
;;; Profile the scraper
;;;
(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 10000
                                       :mode :alloc
                                       :report :flat)
  (do-scraping '("/Users/meister/Development/externals-clasp/build/release/bin/clang" "/Users/meister/Development/clasp/src/main/" "/Users/meister/Development/clasp/src/main/include/application.config" "/tmp/all-commands.txt" "/tmp/commands.txt") :run-preprocessor nil))

