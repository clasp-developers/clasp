cando_app_bin_dir == /Users/meister/Development/cando/build/cando.app/Contents/MacOS
...patience...
...patience...
...patience...
...found 6791 targets...
...updating 203 targets...
darwin.compile.c++ bin/darwin-4.2.1/debug/package.o
darwin.compile.c++ bin/darwin-4.2.1/debug/symbol.o
darwin.compile.c++ bin/darwin-4.2.1/debug/lisp.o
lisp.cc:630: error: prototype for ‘mbb::RPLisp mbb::O_Lisp::create(mbb::Bundle*)’ does not match any in class ‘mbb::O_Lisp’
lisp.h:99: error: candidates are: template<class oclass> boost::shared_ptr<X> mbb::O_Lisp::create()
lisp.h:65: error:                 static boost::shared_ptr<mbb::O_Lisp> mbb::O_Lisp::create(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/lisp.o" "lisp.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/lisp.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/object.o
darwin.compile.c++ bin/darwin-4.2.1/debug/conditions.o
darwin.compile.c++ bin/darwin-4.2.1/debug/lispCallback.o
darwin.compile.c++ bin/darwin-4.2.1/debug/archiveNode.o
darwin.compile.c++ bin/darwin-4.2.1/debug/multiStringBuffer.o
darwin.compile.c++ bin/darwin-4.2.1/debug/archive.o
archive.cc: In member function ‘void mbb::O_Archive::saveOnlyObjectOfClassWithinNode(mbb::Dumb_Node*, int, mbb::RPObject)’:
archive.cc:412: error: ‘class mbb::O_Lisp’ has no member named ‘classNameFromClassId’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/archive.o" "archive.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/archive.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/render.o
darwin.compile.c++ bin/darwin-4.2.1/debug/integerKeyObjectDictionary.o
darwin.compile.c++ bin/darwin-4.2.1/debug/microHeap.o
darwin.compile.c++ bin/darwin-4.2.1/debug/stream.o
darwin.compile.c++ bin/darwin-4.2.1/debug/archiveMemoryManager.o
darwin.compile.c++ bin/darwin-4.2.1/debug/keyedArguments.o
darwin.compile.c++ bin/darwin-4.2.1/debug/keyedObject.o
darwin.compile.c++ bin/darwin-4.2.1/debug/foundation.o
darwin.compile.c++ bin/darwin-4.2.1/debug/stringSet.o
darwin.compile.c++ bin/darwin-4.2.1/debug/environmentDependent.o
darwin.compile.c++ bin/darwin-4.2.1/debug/container.o
darwin.compile.c++ bin/darwin-4.2.1/debug/ovector3.o
darwin.compile.c++ bin/darwin-4.2.1/debug/oclass.o
darwin.compile.c++ bin/darwin-4.2.1/debug/executableNameSpace.o
darwin.compile.c++ bin/darwin-4.2.1/debug/objRef.o
darwin.compile.c++ bin/darwin-4.2.1/debug/bundle.o
darwin.compile.c++ bin/darwin-4.2.1/debug/intrinsics.o
intrinsics.cc:880: error: prototype for ‘mbb::RPFunctionPrimitive mbb::O_FunctionPrimitive::create(mbb::RPSymbol, mbb::uint, mbb::Functoid*, mbb::RPLisp)’ does not match any in class ‘mbb::O_FunctionPrimitive’
intrinsics.h:582: error: candidates are: static mbb::RPFunctionPrimitive mbb::O_FunctionPrimitive::create(const std::string&, mbb::Functoid*, mbb::RPLisp)
intrinsics.h:578: error:                 static boost::shared_ptr<mbb::O_FunctionPrimitive> mbb::O_FunctionPrimitive::create(mbb::RPLisp)
intrinsics.cc: In member function ‘mbb::RPFunctionPrimitive mbb::O_FunctionPrimitive::create(mbb::RPSymbol, mbb::uint, mbb::Functoid*, mbb::RPLisp)’:
intrinsics.cc:884: error: ‘class mbb::O_FunctionPrimitive’ has no member named ‘_Name’
intrinsics.cc:884: error: ‘name’ was not declared in this scope
intrinsics.cc:885: error: ‘class mbb::O_FunctionPrimitive’ has no member named ‘_MethodIdx’
intrinsics.cc: In member function ‘virtual mbb::RPObject mbb::O_FunctionPrimitive::evaluate(mbb::RPCons, mbb::RPLisp)’:
intrinsics.cc:899: error: ‘class mbb::O_FunctionPrimitive’ has no member named ‘_MethodIdx’
intrinsics.cc: At global scope:
intrinsics.cc:908: error: prototype for ‘mbb::RPMethodPrimitive mbb::O_MethodPrimitive::create(mbb::RPLisp, const std::string&, mbb::uint, int, mbb::Functoid*)’ does not match any in class ‘mbb::O_MethodPrimitive’
intrinsics.h:607: error: candidates are: static mbb::RPMethodPrimitive mbb::O_MethodPrimitive::create(mbb::RPLisp, const std::string&, mbb::uint, mbb::Functoid*)
intrinsics.h:602: error:                 static boost::shared_ptr<mbb::O_MethodPrimitive> mbb::O_MethodPrimitive::create(mbb::RPLisp)
intrinsics.cc: In member function ‘mbb::RPMethodPrimitive mbb::O_MethodPrimitive::create(mbb::RPLisp, const std::string&, mbb::uint, int, mbb::Functoid*)’:
intrinsics.cc:912: error: ‘class mbb::O_MethodPrimitive’ has no member named ‘_Name’
intrinsics.cc:913: error: ‘class mbb::O_MethodPrimitive’ has no member named ‘_MethodIdx’
intrinsics.cc:914: error: ‘class mbb::O_MethodPrimitive’ has no member named ‘_AppliesToClassIdx’
intrinsics.cc: In member function ‘virtual void mbb::O_Executable::initialize()’:
intrinsics.cc:944: error: ‘class mbb::O_Executable’ has no member named ‘_Name’
intrinsics.cc:945: error: ‘class mbb::O_Executable’ has no member named ‘_MethodIdx’
intrinsics.cc:946: error: ‘class mbb::O_Executable’ has no member named ‘_WeakSymbol’
intrinsics.cc: In member function ‘virtual void mbb::O_Executable::archiveBase(mbb::Dumb_Node*)’:
intrinsics.cc:952: error: ‘class mbb::O_Executable’ has no member named ‘_WeakSymbol’
intrinsics.cc: In member function ‘void mbb::O_Executable::setSymbol(mbb::RPSymbol)’:
intrinsics.cc:957: error: ‘class mbb::O_Executable’ has no member named ‘_WeakSymbol’
intrinsics.cc: At global scope:
intrinsics.cc:960: error: prototype for ‘mbb::RPSymbol mbb::O_Executable::getSymbol()’ does not match any in class ‘mbb::O_Executable’
intrinsics.h:530: error: candidate is: mbb::RPSymbol mbb::O_Executable::getSymbol() const
intrinsics.cc: In member function ‘mbb::RPSymbol mbb::O_Executable::getSymbol()’:
intrinsics.cc:962: error: ‘class mbb::O_Executable’ has no member named ‘_WeakSymbol’
intrinsics.cc:963: error: ‘class mbb::O_Executable’ has no member named ‘_WeakSymbol’
intrinsics.cc: In static member function ‘static mbb::RPBoundMethod mbb::O_BoundMethod::create(const std::string&, mbb::RPObject, mbb::RPLisp)’:
intrinsics.cc:1001: error: ‘class mbb::O_BoundMethod’ has no member named ‘setName’
intrinsics.cc:1003: error: ‘class mbb::O_BoundMethod’ has no member named ‘_MethodIdx’
intrinsics.cc: In member function ‘virtual mbb::RPObject mbb::O_BoundMethod::evaluate(mbb::RPCons, mbb::RPLisp)’:
intrinsics.cc:1015: error: ‘class mbb::O_BoundMethod’ has no member named ‘_MethodIdx’
intrinsics.cc:1015: error: ‘class mbb::O_Object’ has no member named ‘getInstanceClassId’
intrinsics.cc: At global scope:
intrinsics.cc:1025: error: prototype for ‘mbb::RPProcedure mbb::O_Procedure::create(const std::string&, mbb::RPCons, mbb::RPCons, mbb::RPLisp)’ does not match any in class ‘mbb::O_Procedure’
intrinsics.h:699: error: candidates are: static mbb::RPProcedure mbb::O_Procedure::create(mbb::RPSymbol, mbb::RPCons, mbb::RPCons, mbb::RPLisp)
intrinsics.h:678: error:                 static boost::shared_ptr<mbb::O_Procedure> mbb::O_Procedure::create(mbb::RPLisp)
intrinsics.cc: In member function ‘mbb::RPProcedure mbb::O_Procedure::create(const std::string&, mbb::RPCons, mbb::RPCons, mbb::RPLisp)’:
intrinsics.cc:1034: error: ‘class mbb::O_Procedure’ has no member named ‘_Name’
intrinsics.cc:1035: error: ‘class mbb::O_Procedure’ has no member named ‘_MethodIdx’
intrinsics.cc:1050: error: ‘class mbb::O_Procedure’ has no member named ‘_Name’
intrinsics.cc: In member function ‘void mbb::O_Procedure::populateLocalNameSpace(mbb::RPCons, mbb::RPLisp)’:
intrinsics.cc:1066: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
intrinsics.cc:1098: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
intrinsics.cc:1115: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
intrinsics.cc: In member function ‘virtual mbb::RPObject mbb::O_Procedure::evaluate(mbb::RPCons, mbb::RPLisp)’:
intrinsics.cc:1160: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
intrinsics.cc:1174: error: ‘class mbb::O_Procedure’ has no member named ‘_Name’
intrinsics.cc:1177: error: ‘class mbb::O_Procedure’ has no member named ‘_Name’
intrinsics.cc:1189: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
intrinsics.cc:1194: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
intrinsics.cc: At global scope:
intrinsics.cc:1318: error: prototype for ‘mbb::RPMethod mbb::O_Method::createWithoutReceiverClass(mbb::RPSymbol, mbb::RPCons, mbb::RPCons, mbb::RPLisp)’ does not match any in class ‘mbb::O_Method’
intrinsics.h:738: error: candidate is: static mbb::RPMethod mbb::O_Method::createWithoutReceiverClass(mbb::RPLisp, mbb::RPSymbol, mbb::RPCons, mbb::RPCons, mbb::RPLisp)
intrinsics.cc: In member function ‘mbb::RPMethod mbb::O_Method::createWithoutReceiverClass(mbb::RPSymbol, mbb::RPCons, mbb::RPCons, mbb::RPLisp)’:
intrinsics.cc:1327: error: ‘class mbb::O_Method’ has no member named ‘_WeakSymbol’
intrinsics.cc:1327: error: ‘keywordMessage’ was not declared in this scope
intrinsics.cc:1328: error: ‘class mbb::O_Method’ has no member named ‘_MethodIdx’
intrinsics.cc: At global scope:
intrinsics.cc:1344: error: expected ‘,’ or ‘...’ before ‘RPSymbol’
intrinsics.cc:1346: error: prototype for ‘mbb::RPMethod mbb::O_Method::create(mbb::RPMetaClass)’ does not match any in class ‘mbb::O_Method’
intrinsics.h:739: error: candidates are: static mbb::RPMethod mbb::O_Method::create(mbb::RPLisp, mbb::RPMetaClass, mbb::RPSymbol, mbb::RPCons, mbb::RPCons, mbb::RPLisp)
intrinsics.h:724: error:                 static boost::shared_ptr<mbb::O_Method> mbb::O_Method::create(mbb::RPLisp)
intrinsics.cc: In member function ‘mbb::RPMethod mbb::O_Method::create(mbb::RPMetaClass)’:
intrinsics.cc:1349: error: ‘keywordSymbol’ was not declared in this scope
intrinsics.cc:1349: error: ‘arguments’ was not declared in this scope
intrinsics.cc:1349: error: ‘code’ was not declared in this scope
intrinsics.cc:1350: error: ‘class mbb::O_Method’ has no member named ‘setAppliesToClass’
intrinsics.cc: In member function ‘std::string mbb::O_Method::getFullMethodName(mbb::RPLisp)’:
intrinsics.cc:1360: error: ‘class mbb::O_Lisp’ has no member named ‘classManager’
intrinsics.cc:1360: error: ‘class mbb::O_Method’ has no member named ‘_AppliesToClassIdx’
intrinsics.cc:1363: error: ‘class mbb::O_Method’ has no member named ‘_Name’
intrinsics.cc: In member function ‘virtual mbb::RPObject mbb::O_Method::evaluate(mbb::RPCons, mbb::RPLisp)’:
intrinsics.cc:1371: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
intrinsics.cc:1386: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
intrinsics.cc:1400: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
intrinsics.cc:1422: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
intrinsics.cc: At global scope:
intrinsics.cc:1457: error: ‘O_SetGlobal’ has not been declared
intrinsics.cc:1457: error: ‘O_SetGlobal’ was not declared in this scope
intrinsics.cc:1457: error: template argument 1 is invalid
intrinsics.cc:1457: error: ‘O_SetGlobal’ has not been declared
intrinsics.cc: In function ‘int mbb::nil(mbb::RPLisp)’:
intrinsics.cc:1457: error: ‘O_SetGlobal’ has not been declared
intrinsics.cc:1457: error: ‘O_SetGlobal’ was not declared in this scope
intrinsics.cc:1457: error: no matching function for call to ‘mbb::O_MetaClass::nilForClass()’
intrinsics.cc: At global scope:
intrinsics.cc:1457: error: ‘O_SetGlobal’ has not been declared
intrinsics.cc:1457: error: non-member function ‘bool mbb::isNil()’ cannot have cv-qualifier
intrinsics.cc: In function ‘bool mbb::isNil()’:
intrinsics.cc:1457: error: invalid use of ‘this’ in non-member function
intrinsics.cc:1457: error: ‘O_SetGlobal’ has not been declared
intrinsics.cc:1457: error: ‘O_SetGlobal’ was not declared in this scope
intrinsics.cc:1457: error: no matching function for call to ‘mbb::O_MetaClass::nilForClass()’
intrinsics.cc:1457: error: invalid use of ‘this’ in non-member function
intrinsics.cc: In function ‘void mbb::Register_O_SetGlobal()’:
intrinsics.cc:1457: error: ‘mbb::O_SetGlobal’ has not been declared
intrinsics.cc:1457: error: ‘O_SetGlobal’ is not a member of ‘mbb’
intrinsics.cc:1457: error: ‘O_SetGlobal’ is not a member of ‘mbb’
intrinsics.cc:1457: error: no matching function for call to ‘registerClass()’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/intrinsics.o" "intrinsics.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/intrinsics.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/cons.o
darwin.compile.c++ bin/darwin-4.2.1/debug/candoScript.o
In file included from candoScript.cc:15:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
In file included from candoScript.cc:31:
plug.h: In member function ‘virtual mbb::RPCons mbb::O_PlugWithMates::matesAsCons()’:
plug.h:199: error: no matching function for call to ‘mbb::List<mbb::O_Mate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Mate]
plug.h: In member function ‘mbb::RPCons mbb::O_RingClosingPlug::ringClosingMatesAsCons()’:
plug.h:305: error: no matching function for call to ‘mbb::List<mbb::O_RingClosingMate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_RingClosingMate]
candoScript.cc: In member function ‘virtual mbb::RPMultiMonomer mbb::O_OligomerPart_Monomer::createMonomer(mbb::RPBuilderDatabase)’:
candoScript.cc:54: error: no matching function for call to ‘mbb::O_MultiMonomer::create(mbb::RPBuilderDatabase&, mbb::RPLisp)’
monomer.h:317: note: candidates are: static boost::shared_ptr<mbb::O_MultiMonomer> mbb::O_MultiMonomer::create(mbb::RPLisp)
candoScript.cc: In function ‘mbb::RPObject mbb::prim_save(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
candoScript.cc:403: error: ‘class mbb::O_XmlSaveArchive’ has no member named ‘setEnvironment’
candoScript.cc: In function ‘mbb::RPObject mbb::prim_saveWithBuilderDatabase(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
candoScript.cc:426: error: ‘class mbb::O_XmlSaveArchive’ has no member named ‘setEnvironment’
candoScript.cc: In function ‘mbb::RPObject mbb::prim_saveCandoFormat(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
candoScript.cc:450: error: ‘class mbb::O_CandoFormatSaveArchive’ has no member named ‘setEnvironment’
candoScript.cc: In function ‘mbb::RPResidue mbb::findResidue(mbb::RPMatter, mbb::RPObject)’:
candoScript.cc:572: error: ‘lisp’ was not declared in this scope
candoScript.cc: In function ‘mbb::RPObject mbb::prim_calculatePoint(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
candoScript.cc:824: error: no matching function for call to ‘mbb::O_OVector3::createFromVector3(mbb::Vector3&)’
ovector3.h:38: note: candidates are: static mbb::RPOVector3 mbb::O_OVector3::createFromVector3(mbb::RPLisp, const mbb::Vector3&)
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/candoScript.o" "candoScript.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/candoScript.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/nameSpace.o
darwin.compile.c++ bin/darwin-4.2.1/debug/objectSet.o
darwin.compile.c++ bin/darwin-4.2.1/debug/model.o
darwin.compile.c++ bin/darwin-4.2.1/debug/stringList.o
darwin.compile.c++ bin/darwin-4.2.1/debug/objectDictionary.o
objectDictionary.cc:29: error: prototype for ‘mbb::RPObjectDictionary mbb::O_ObjectDictionary::createFromKeyedObjectCons(mbb::RPCons, mbb::RPLisp)’ does not match any in class ‘mbb::O_ObjectDictionary’
objectDictionary.h:31: error: candidate is: static mbb::RPObjectDictionary mbb::O_ObjectDictionary::createFromKeyedObjectCons(mbb::RPLisp, mbb::RPCons)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/objectDictionary.o" "objectDictionary.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/objectDictionary.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/objectList.o
darwin.compile.c++ bin/darwin-4.2.1/debug/xmlLoadArchive.o
darwin.compile.c++ bin/darwin-4.2.1/debug/xmlSaveArchive.o
xmlSaveArchive.cc:13: error: prototype for ‘void mbb::O_XmlSaveArchive::exposeCando()’ does not match any in class ‘mbb::O_XmlSaveArchive’
xmlSaveArchive.h:20: error: candidate is: static void mbb::O_XmlSaveArchive::exposeCando(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/xmlSaveArchive.o" "xmlSaveArchive.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/xmlSaveArchive.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/contender.o
In file included from builderState.h:16,
                 from contender.cc:9:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/contender.o" "contender.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/contender.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/trajectory.o
darwin.compile.c++ bin/darwin-4.2.1/debug/energyComponent.o
In file included from energyComponent.h:30,
                 from energyComponent.cc:4:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/energyComponent.o" "energyComponent.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/energyComponent.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/energyStretch.o
In file included from energyComponent.h:30,
                 from energyStretch.h:24,
                 from energyStretch.cc:4:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
energyStretch.cc: In member function ‘mbb::RPQDomNode mbb::EnergyStretch::asXml(mbb::RPLisp)’:
energyStretch.cc:87: error: ‘class mbb::EnergyStretch’ has no member named ‘env’
energyComponent.h: In function ‘void mbb::archiveEnergyComponentTerms(mbb::Dumb_Node*, ComponentType&) [with ComponentType = mbb::O_EnergyStretch, EntryType = mbb::EnergyStretch]’:
energyStretch.cc:506:   instantiated from here
energyComponent.h:406: error: invalid use of incomplete type ‘struct mbb::ArchiveError’
archiveNode.h:67: error: forward declaration of ‘struct mbb::ArchiveError’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/energyStretch.o" "energyStretch.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/energyStretch.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/energyAngle.o
In file included from energyComponent.h:30,
                 from energyAngle.h:24,
                 from energyAngle.cc:5:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
energyAngle.cc: In member function ‘mbb::RPQDomNode mbb::EnergyAngle::asXml(mbb::RPLisp)’:
energyAngle.cc:116: error: ‘class mbb::EnergyAngle’ has no member named ‘env’
energyAngle.cc:129: error: ‘class mbb::EnergyAngle’ has no member named ‘env’
energyComponent.h: In function ‘void mbb::archiveEnergyComponentTerms(mbb::Dumb_Node*, ComponentType&) [with ComponentType = mbb::O_EnergyAngle, EntryType = mbb::EnergyAngle]’:
energyAngle.cc:606:   instantiated from here
energyComponent.h:406: error: invalid use of incomplete type ‘struct mbb::ArchiveError’
archiveNode.h:67: error: forward declaration of ‘struct mbb::ArchiveError’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/energyAngle.o" "energyAngle.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/energyAngle.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/energyDihedral.o
In file included from energyComponent.h:30,
                 from energyDihedral.h:20,
                 from energyDihedral.cc:6:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
energyDihedral.cc: In member function ‘mbb::RPQDomNode mbb::EnergyDihedral::asXml(mbb::RPLisp)’:
energyDihedral.cc:194: error: ‘class mbb::EnergyDihedral’ has no member named ‘env’
energyComponent.h: In function ‘void mbb::archiveEnergyComponentTerms(mbb::Dumb_Node*, ComponentType&) [with ComponentType = mbb::O_EnergyDihedral, EntryType = mbb::EnergyDihedral]’:
energyDihedral.cc:791:   instantiated from here
energyComponent.h:406: error: invalid use of incomplete type ‘struct mbb::ArchiveError’
archiveNode.h:67: error: forward declaration of ‘struct mbb::ArchiveError’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/energyDihedral.o" "energyDihedral.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/energyDihedral.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/energyNonbond.o
In file included from energyComponent.h:30,
                 from energyNonbond.h:24,
                 from energyNonbond.cc:6:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
energyNonbond.cc: In member function ‘mbb::RPQDomNode mbb::EnergyNonbond::asXml(mbb::RPLisp)’:
energyNonbond.cc:152: error: ‘class mbb::EnergyNonbond’ has no member named ‘env’
energyNonbond.cc: In member function ‘int mbb::O_EnergyNonbond::countBadVdwOverlaps(double, mbb::RPNVector, mbb::RPRenderDisplayList)’:
energyNonbond.cc:320: error: no matching function for call to ‘mbb::O_GrLine::create(mbb::Vector3&, mbb::Vector3&, mbb::RPLisp)’
render.h:119: note: candidates are: static boost::shared_ptr<mbb::O_GrLine> mbb::O_GrLine::create(mbb::RPLisp)
render.h:147: note:                 static mbb::RPGrLine mbb::O_GrLine::create(mbb::RPLisp, const mbb::Vector3&, const mbb::Vector3&)
render.h:159: note:                 static mbb::RPGrLine mbb::O_GrLine::create(mbb::RPLisp, const mbb::Vector3&, const mbb::Vector3&, mbb::uint)
energyComponent.h: In function ‘void mbb::archiveEnergyComponentTerms(mbb::Dumb_Node*, ComponentType&) [with ComponentType = mbb::O_EnergyNonbond, EntryType = mbb::EnergyNonbond]’:
energyNonbond.cc:642:   instantiated from here
energyComponent.h:406: error: invalid use of incomplete type ‘struct mbb::ArchiveError’
archiveNode.h:67: error: forward declaration of ‘struct mbb::ArchiveError’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/energyNonbond.o" "energyNonbond.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/energyNonbond.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/energyChiralRestraint.o
In file included from energyComponent.h:30,
                 from energyChiralRestraint.h:25,
                 from energyChiralRestraint.cc:6:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
energyChiralRestraint.cc: In member function ‘mbb::RPQDomNode mbb::EnergyChiralRestraint::asXml(mbb::RPLisp)’:
energyChiralRestraint.cc:86: error: ‘class mbb::EnergyChiralRestraint’ has no member named ‘env’
energyChiralRestraint.cc:97: error: ‘class mbb::EnergyChiralRestraint’ has no member named ‘env’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/energyChiralRestraint.o" "energyChiralRestraint.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/energyChiralRestraint.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/energyAnchorRestraint.o
In file included from energyComponent.h:30,
                 from energyAnchorRestraint.h:21,
                 from energyAnchorRestraint.cc:5:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
energyAnchorRestraint.cc: In member function ‘mbb::RPQDomNode mbb::EnergyAnchorRestraint::asXml(mbb::RPLisp)’:
energyAnchorRestraint.cc:53: error: ‘class mbb::EnergyAnchorRestraint’ has no member named ‘env’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/energyAnchorRestraint.o" "energyAnchorRestraint.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/energyAnchorRestraint.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/energyImproperRestraint.o
In file included from energyComponent.h:30,
                 from energyImproperRestraint.h:21,
                 from energyImproperRestraint.cc:7:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
energyImproperRestraint.cc: In member function ‘mbb::RPQDomNode mbb::EnergyImproperRestraint::asXml(mbb::RPLisp)’:
energyImproperRestraint.cc:97: error: ‘class mbb::EnergyImproperRestraint’ has no member named ‘env’
energyImproperRestraint.cc:108: error: ‘class mbb::EnergyImproperRestraint’ has no member named ‘env’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/energyImproperRestraint.o" "energyImproperRestraint.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/energyImproperRestraint.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/energyFixedNonbond.o
In file included from energyComponent.h:30,
                 from energyFixedNonbond.h:21,
                 from energyFixedNonbond.cc:5:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/energyFixedNonbond.o" "energyFixedNonbond.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/energyFixedNonbond.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/energyAtomTable.o
In file included from energyComponent.h:30,
                 from energyAtomTable.cc:10:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/energyAtomTable.o" "energyAtomTable.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/energyAtomTable.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/energyFunction.o
In file included from energyComponent.h:30,
                 from energyStretch.h:24,
                 from energyFunction.cc:31:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
energyFunction.cc: In member function ‘int mbb::O_EnergyFunction::compareAnalyticalAndNumericalForceAndHessianTermByTermAtCurrentPosition()’:
energyFunction.cc:593: error: no matching function for call to ‘mbb::O_NVector::create(mbb::RPLisp, mbb::uint)’
nVector.h:25: note: candidates are: static boost::shared_ptr<mbb::O_NVector> mbb::O_NVector::create(mbb::RPLisp)
nVector.h:29: note:                 static mbb::RPNVector mbb::O_NVector::create(mbb::uint, mbb::RPLisp)
energyFunction.cc: In member function ‘mbb::uint mbb::O_EnergyFunction::checkForBeyondThresholdInteractions()’:
energyFunction.cc:615: error: no matching function for call to ‘mbb::O_NVector::create(mbb::RPLisp, mbb::uint)’
nVector.h:25: note: candidates are: static boost::shared_ptr<mbb::O_NVector> mbb::O_NVector::create(mbb::RPLisp)
nVector.h:29: note:                 static mbb::RPNVector mbb::O_NVector::create(mbb::uint, mbb::RPLisp)
energyFunction.cc: In member function ‘double mbb::O_EnergyFunction::evaluateEnergyForceFullHessianForDebugging()’:
energyFunction.cc:748: error: no matching function for call to ‘mbb::O_NVector::create(mbb::RPLisp, mbb::uint)’
nVector.h:25: note: candidates are: static boost::shared_ptr<mbb::O_NVector> mbb::O_NVector::create(mbb::RPLisp)
nVector.h:29: note:                 static mbb::RPNVector mbb::O_NVector::create(mbb::uint, mbb::RPLisp)
energyFunction.cc:749: error: no matching function for call to ‘mbb::O_NVector::create(mbb::RPLisp, mbb::uint)’
nVector.h:25: note: candidates are: static boost::shared_ptr<mbb::O_NVector> mbb::O_NVector::create(mbb::RPLisp)
nVector.h:29: note:                 static mbb::RPNVector mbb::O_NVector::create(mbb::uint, mbb::RPLisp)
energyFunction.cc: In member function ‘mbb::RPForceMatchReport mbb::O_EnergyFunction::checkIfAnalyticalForceMatchesNumericalForce(mbb::RPNVector, mbb::RPNVector)’:
energyFunction.cc:890: error: no matching function for call to ‘mbb::O_NVector::create(mbb::RPLisp, mbb::uint)’
nVector.h:25: note: candidates are: static boost::shared_ptr<mbb::O_NVector> mbb::O_NVector::create(mbb::RPLisp)
nVector.h:29: note:                 static mbb::RPNVector mbb::O_NVector::create(mbb::uint, mbb::RPLisp)
energyFunction.cc: In member function ‘void mbb::O_EnergyFunction::defineForMatter(mbb::RPMatter, mbb::RPForceField)’:
energyFunction.cc:1169: error: ‘_classId’ is not a member of ‘mbb::O_Aggregate’
energyFunction.cc:1169: error: ‘_classId’ is not a member of ‘mbb::O_Molecule’
energyFunction.cc: In member function ‘double mbb::O_EnergyFunction::calculateEnergy()’:
energyFunction.cc:1666: error: no matching function for call to ‘mbb::O_NVector::create(mbb::RPLisp, mbb::uint)’
nVector.h:25: note: candidates are: static boost::shared_ptr<mbb::O_NVector> mbb::O_NVector::create(mbb::RPLisp)
nVector.h:29: note:                 static mbb::RPNVector mbb::O_NVector::create(mbb::uint, mbb::RPLisp)
energyFunction.cc: In member function ‘double mbb::O_EnergyFunction::calculateEnergyAndForce()’:
energyFunction.cc:1677: error: no matching function for call to ‘mbb::O_NVector::create(mbb::RPLisp, mbb::uint)’
nVector.h:25: note: candidates are: static boost::shared_ptr<mbb::O_NVector> mbb::O_NVector::create(mbb::RPLisp)
nVector.h:29: note:                 static mbb::RPNVector mbb::O_NVector::create(mbb::uint, mbb::RPLisp)
energyFunction.cc:1678: error: no matching function for call to ‘mbb::O_NVector::create(mbb::RPLisp, mbb::uint)’
nVector.h:25: note: candidates are: static boost::shared_ptr<mbb::O_NVector> mbb::O_NVector::create(mbb::RPLisp)
nVector.h:29: note:                 static mbb::RPNVector mbb::O_NVector::create(mbb::uint, mbb::RPLisp)
energyFunction.cc: In member function ‘mbb::uint mbb::O_EnergyFunction::countBadVdwInteractions(double, mbb::RPRenderDisplayList)’:
energyFunction.cc:1815: error: no matching function for call to ‘mbb::O_NVector::create(mbb::RPLisp, mbb::uint)’
nVector.h:25: note: candidates are: static boost::shared_ptr<mbb::O_NVector> mbb::O_NVector::create(mbb::RPLisp)
nVector.h:29: note:                 static mbb::RPNVector mbb::O_NVector::create(mbb::uint, mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/energyFunction.o" "energyFunction.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/energyFunction.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/referencerConformation.o
darwin.compile.c++ bin/darwin-4.2.1/debug/simulatedAnnealing.o
In file included from simulatedAnnealing.cc:7:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]
simulatedAnnealing.cc: In member function ‘mbb::RPCons mbb::ScorePoint::asCons()’:
simulatedAnnealing.cc:33: error: ‘class mbb::ScorePoint’ has no member named ‘env’
simulatedAnnealing.cc:33: error: no matching function for call to ‘mbb::O_Real::create(double&)’
values.h:232: note: candidates are: static boost::shared_ptr<mbb::O_Real> mbb::O_Real::create(mbb::RPLisp)
values.h:238: note:                 static mbb::RPReal mbb::O_Real::create(mbb::RPLisp, double)
simulatedAnnealing.cc:33: error: no matching function for call to ‘mbb::O_Bool::create(bool&)’
values.h:265: note: candidates are: static boost::shared_ptr<mbb::O_Bool> mbb::O_Bool::create(mbb::RPLisp)
values.h:271: note:                 static mbb::RPBool mbb::O_Bool::create(mbb::RPLisp, bool)
simulatedAnnealing.cc:33: error: ‘class mbb::ScorePoint’ has no member named ‘env’
simulatedAnnealing.cc: At global scope:
simulatedAnnealing.cc:43: error: prototype for ‘mbb::RPCons mbb::OneTemperatureScoreSeries::asCons(mbb::RPLisp)’ does not match any in class ‘mbb::OneTemperatureScoreSeries’
simulatedAnnealing.h:51: error: candidate is: mbb::RPCons mbb::OneTemperatureScoreSeries::asCons()
simulatedAnnealing.cc: In member function ‘mbb::RPCons mbb::OneTemperatureScoreSeries::asCons(mbb::RPLisp)’:
simulatedAnnealing.cc:49: error: no matching function for call to ‘mbb::ScorePoint::asCons(mbb::RPLisp&)’
simulatedAnnealing.cc:31: note: candidates are: mbb::RPCons mbb::ScorePoint::asCons()

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/simulatedAnnealing.o" "simulatedAnnealing.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/simulatedAnnealing.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/complexRestraints.o
complexRestraints.cc: In static member function ‘static void mbb::O_ComplexRestraint::exposeCando(mbb::RPLisp)’:
complexRestraints.cc:29: error: no matching function for call to ‘mbb::class_<mbb::O_ComplexRestraint>::class_()’
wrappers.h:105: note: candidates are: mbb::class_<oclass>::class_(mbb::RPLisp) [with OT = mbb::O_ComplexRestraint]
wrappers.h:100: note:                 mbb::class_<mbb::O_ComplexRestraint>::class_(const mbb::class_<mbb::O_ComplexRestraint>&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/complexRestraints.o" "complexRestraints.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/complexRestraints.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/builderScorer.o
In file included from builderScorer.cc:11:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]
In file included from ffStretchDb.h:25,
                 from forceField.h:32,
                 from builderScorer.cc:16:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/builderScorer.o" "builderScorer.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/builderScorer.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/searchStatistics.o
darwin.compile.c++ bin/darwin-4.2.1/debug/posixTime.o
darwin.compile.c++ bin/darwin-4.2.1/debug/constitution.o
In file included from constitution.cc:15:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
In file included from constitution.cc:16:
plug.h: In member function ‘virtual mbb::RPCons mbb::O_PlugWithMates::matesAsCons()’:
plug.h:199: error: no matching function for call to ‘mbb::List<mbb::O_Mate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Mate]
plug.h: In member function ‘mbb::RPCons mbb::O_RingClosingPlug::ringClosingMatesAsCons()’:
plug.h:305: error: no matching function for call to ‘mbb::List<mbb::O_RingClosingMate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_RingClosingMate]
In file included from constitution.cc:19:
stereochemistry.h: In member function ‘mbb::RPCons mbb::O_StereoInformation::stereoisomersAsCons()’:
stereochemistry.h:159: error: no matching function for call to ‘mbb::List<mbb::O_Stereoisomer>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Stereoisomer]
constitution.cc: In member function ‘mbb::RPCons mbb::O_Constitution::topologiesAsCons()’:
constitution.cc:236: error: no matching function for call to ‘mbb::Map<mbb::O_Topology>::asCons()’
holder.h:441: note: candidates are: mbb::RPCons mbb::Map<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Topology]
constitution.cc: In member function ‘mbb::RPCons mbb::O_Constitution::plugsAsCons()’:
constitution.cc:237: error: no matching function for call to ‘mbb::Map<mbb::O_Plug>::asCons()’
holder.h:441: note: candidates are: mbb::RPCons mbb::Map<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Plug]
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/constitution.o" "constitution.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/constitution.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/matter.o
darwin.compile.c++ bin/darwin-4.2.1/debug/atom.o
atom.cc: In member function ‘mbb::RPCons mbb::O_Atom::createImplicitHydrogenNamesOnCarbon()’:
atom.cc:512: error: no matching function for call to ‘mbb::O_String::create(std::basic_string<char, std::char_traits<char>, std::allocator<char> >)’
values.h:85: note: candidates are: static boost::shared_ptr<mbb::O_String> mbb::O_String::create(mbb::RPLisp)
values.h:89: note:                 static mbb::RPString mbb::O_String::create(mbb::RPLisp, const std::string&)
atom.cc:521: error: no matching function for call to ‘mbb::O_String::create(std::basic_string<char, std::char_traits<char>, std::allocator<char> >)’
values.h:85: note: candidates are: static boost::shared_ptr<mbb::O_String> mbb::O_String::create(mbb::RPLisp)
values.h:89: note:                 static mbb::RPString mbb::O_String::create(mbb::RPLisp, const std::string&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/atom.o" "atom.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/atom.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/plug.o
In file included from plug.cc:5:
plug.h: In member function ‘virtual mbb::RPCons mbb::O_PlugWithMates::matesAsCons()’:
plug.h:199: error: no matching function for call to ‘mbb::List<mbb::O_Mate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Mate]
plug.h: In member function ‘mbb::RPCons mbb::O_RingClosingPlug::ringClosingMatesAsCons()’:
plug.h:305: error: no matching function for call to ‘mbb::List<mbb::O_RingClosingMate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_RingClosingMate]
plug.cc: In member function ‘mbb::RPCons mbb::O_OutPlug::frameFinishersAsCons()’:
plug.cc:447: error: no matching function for call to ‘mbb::List<mbb::O_FrameFinisher>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_FrameFinisher]
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_DirectionalCoupling]’:
coupling.h:118:   instantiated from here
coupling.h:123: error: ‘virtual void mbb::O_DirectionalCoupling::initialize()’ is private
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_RingCoupling]’:
coupling.h:211:   instantiated from here
coupling.h:216: error: ‘virtual void mbb::O_RingCoupling::initialize()’ is private
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/plug.o" "plug.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/plug.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/renderController.o
renderController.cc: In member function ‘mbb::RPCons mbb::O_RenderController::switchNamesAsCons()’:
renderController.cc:93: error: no matching function for call to ‘mbb::O_String::create(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
values.h:85: note: candidates are: static boost::shared_ptr<mbb::O_String> mbb::O_String::create(mbb::RPLisp)
values.h:89: note:                 static mbb::RPString mbb::O_String::create(mbb::RPLisp, const std::string&)
renderController.cc: In member function ‘mbb::RPCons mbb::O_RenderController::sliderNamesAsCons()’:
renderController.cc:107: error: no matching function for call to ‘mbb::O_String::create(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
values.h:85: note: candidates are: static boost::shared_ptr<mbb::O_String> mbb::O_String::create(mbb::RPLisp)
values.h:89: note:                 static mbb::RPString mbb::O_String::create(mbb::RPLisp, const std::string&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/renderController.o" "renderController.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/renderController.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/clusterFinder.o
clusterFinder.cc: In member function ‘mbb::RPRender mbb::O_ClusterEntry::renderWithBonds(mbb::RPResidue, mbb::VectorStrings&)’:
clusterFinder.cc:76: error: no matching function for call to ‘mbb::O_GrColor::createWithColorValue(mbb::uint)’
render.h:477: note: candidates are: static mbb::RPGrColor mbb::O_GrColor::createWithColorValue(mbb::RPLisp, mbb::uint)
clusterFinder.cc:78: error: no matching function for call to ‘mbb::O_GrColor::createWithColorValue(mbb::uint)’
render.h:477: note: candidates are: static mbb::RPGrColor mbb::O_GrColor::createWithColorValue(mbb::RPLisp, mbb::uint)
clusterFinder.cc:85: error: no matching function for call to ‘mbb::O_GrColor::createWithName(const char [8])’
render.h:466: note: candidates are: static mbb::RPGrColor mbb::O_GrColor::createWithName(mbb::RPLisp, const std::string&)
clusterFinder.cc: In member function ‘mbb::RPCons mbb::O_Cluster::entriesAsCons()’:
clusterFinder.cc:205: error: no matching function for call to ‘mbb::O_Cons::createFromRange(__gnu_cxx::__normal_iterator<boost::shared_ptr<mbb::O_ClusterEntry>*, std::vector<boost::shared_ptr<mbb::O_ClusterEntry>, std::allocator<boost::shared_ptr<mbb::O_ClusterEntry> > > >, __gnu_cxx::__normal_iterator<boost::shared_ptr<mbb::O_ClusterEntry>*, std::vector<boost::shared_ptr<mbb::O_ClusterEntry>, std::allocator<boost::shared_ptr<mbb::O_ClusterEntry> > > >)’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/clusterFinder.o" "clusterFinder.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/clusterFinder.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/arrayedMatter.o
darwin.compile.c++ bin/darwin-4.2.1/debug/specificContext.o
darwin.compile.c++ bin/darwin-4.2.1/debug/atomGridCollisionRejector.o
atomGridCollisionRejector.cc: In member function ‘virtual mbb::RPRenderDisplayList mbb::O_AtomGridCollisionRejector::getRenderForScore(mbb::RPAliasReferencer, mbb::RPScorerState)’:
atomGridCollisionRejector.cc:123: error: no matching function for call to ‘mbb::O_GrColor::createWithName(const char [4])’
render.h:466: note: candidates are: static mbb::RPGrColor mbb::O_GrColor::createWithName(mbb::RPLisp, const std::string&)
atomGridCollisionRejector.cc:127: error: no matching function for call to ‘mbb::O_GrColor::createWithName(const char [6])’
render.h:466: note: candidates are: static mbb::RPGrColor mbb::O_GrColor::createWithName(mbb::RPLisp, const std::string&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/atomGridCollisionRejector.o" "atomGridCollisionRejector.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/atomGridCollisionRejector.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/aliasReferencer.o
aliasReferencer.cc: In member function ‘virtual void mbb::AliasReferencer_Exposer::exposeCando()’:
aliasReferencer.cc:49: error: no matching function for call to ‘mbb::class_<mbb::O_AliasReferencer>::class_()’
wrappers.h:105: note: candidates are: mbb::class_<oclass>::class_(mbb::RPLisp) [with OT = mbb::O_AliasReferencer]
wrappers.h:100: note:                 mbb::class_<mbb::O_AliasReferencer>::class_(const mbb::class_<mbb::O_AliasReferencer>&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/aliasReferencer.o" "aliasReferencer.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/aliasReferencer.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/calculatePosition.o
darwin.compile.c++ bin/darwin-4.2.1/debug/moleculeReferencer.o
darwin.compile.c++ bin/darwin-4.2.1/debug/vdwCollisionRejector.o
In file included from vdwCollisionRejector.cc:27:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]
vdwCollisionRejector.cc: In constructor ‘mbb::AtomVdwEntry::AtomVdwEntry()’:
vdwCollisionRejector.cc:55: error: ‘class mbb::AtomVdwEntry’ has no member named ‘env’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/vdwCollisionRejector.o" "vdwCollisionRejector.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/vdwCollisionRejector.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/rejector.o
In file included from rejector.cc:24:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/rejector.o" "rejector.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/rejector.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/scorerStatistics.o
darwin.compile.c++ bin/darwin-4.2.1/debug/scorerStage.o
darwin.compile.c++ bin/darwin-4.2.1/debug/scorerStageList.o
darwin.compile.c++ bin/darwin-4.2.1/debug/virtualAtom.o
virtualAtom.cc: In static member function ‘static mbb::RPVirtualAtom mbb::O_VirtualAtom::create(const std::string&, mbb::RPCalculatePosition, mbb::RPLisp)’:
virtualAtom.cc:25: error: ‘e’ was not declared in this scope

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/virtualAtom.o" "virtualAtom.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/virtualAtom.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/aggregate.o
aggregate.cc: In member function ‘virtual void mbb::Aggregate_Exposer::exposeCando()’:
aggregate.cc:526: error: ‘class mbb::O_Lisp’ has no member named ‘defineClass’
aggregate.cc:526: error: expected primary-expression before ‘>’ token
aggregate.cc:526: error: expected primary-expression before ‘)’ token
archiveNode.h: In member function ‘void mbb::Dumb_Node::archiveContainerIfNotEmpty(const std::string&, boost::shared_ptr<X>&) [with OType = mbb::O_RestraintList]’:
aggregate.cc:77:   instantiated from here
archiveNode.h:1007: error: no matching function for call to ‘mbb::O_RestraintList::create()’
restraint.h:297: note: candidates are: static boost::shared_ptr<mbb::O_RestraintList> mbb::O_RestraintList::create(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/aggregate.o" "aggregate.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/aggregate.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/atomGrid.o
atomGrid.cc: In member function ‘virtual mbb::RPRender mbb::O_AtomGrid::rendered(mbb::RPKeyedArguments)’:
atomGrid.cc:498: error: no matching function for call to ‘mbb::O_GrColor::createWithName(const char [4])’
render.h:466: note: candidates are: static mbb::RPGrColor mbb::O_GrColor::createWithName(mbb::RPLisp, const std::string&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/atomGrid.o" "atomGrid.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/atomGrid.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/bond.o
darwin.compile.c++ bin/darwin-4.2.1/debug/coordSys.o
coordSys.cc: In member function ‘virtual mbb::RPRender mbb::O_CoordinateSystem::rendered(mbb::RPKeyedArguments)’:
coordSys.cc:974: error: no matching function for call to ‘mbb::O_GrColor::createWithName(const char [4])’
render.h:466: note: candidates are: static mbb::RPGrColor mbb::O_GrColor::createWithName(mbb::RPLisp, const std::string&)
coordSys.cc:976: error: no matching function for call to ‘mbb::O_GrColor::createWithName(const char [6])’
render.h:466: note: candidates are: static mbb::RPGrColor mbb::O_GrColor::createWithName(mbb::RPLisp, const std::string&)
coordSys.cc:978: error: no matching function for call to ‘mbb::O_GrColor::createWithName(const char [5])’
render.h:466: note: candidates are: static mbb::RPGrColor mbb::O_GrColor::createWithName(mbb::RPLisp, const std::string&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/coordSys.o" "coordSys.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/coordSys.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/coupling.o
In file included from coupling.cc:15:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
In file included from coupling.cc:18:
plug.h: In member function ‘virtual mbb::RPCons mbb::O_PlugWithMates::matesAsCons()’:
plug.h:199: error: no matching function for call to ‘mbb::List<mbb::O_Mate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Mate]
plug.h: In member function ‘mbb::RPCons mbb::O_RingClosingPlug::ringClosingMatesAsCons()’:
plug.h:305: error: no matching function for call to ‘mbb::List<mbb::O_RingClosingMate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_RingClosingMate]
coupling.cc: In member function ‘virtual void mbb::Coupling_Expose::exposeCando()’:
coupling.cc:826: error: no matching function for call to ‘mbb::class_<mbb::O_Coupling>::class_()’
wrappers.h:105: note: candidates are: mbb::class_<oclass>::class_(mbb::RPLisp) [with OT = mbb::O_Coupling]
wrappers.h:100: note:                 mbb::class_<mbb::O_Coupling>::class_(const mbb::class_<mbb::O_Coupling>&)
coupling.cc: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_DirectionalCoupling]’:
coupling.h:118:   instantiated from here
coupling.cc:160: error: ‘virtual void mbb::O_DirectionalCoupling::initialize()’ is private
object.h:232: error: within this context
coupling.cc: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_RingCoupling]’:
coupling.h:211:   instantiated from here
coupling.cc:557: error: ‘virtual void mbb::O_RingCoupling::initialize()’ is private
object.h:232: error: within this context
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/coupling.o" "coupling.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/coupling.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/molecule.o
archiveNode.h: In member function ‘void mbb::Dumb_Node::archiveContainerIfNotEmpty(const std::string&, boost::shared_ptr<X>&) [with OType = mbb::O_RestraintList]’:
molecule.cc:66:   instantiated from here
archiveNode.h:1007: error: no matching function for call to ‘mbb::O_RestraintList::create()’
restraint.h:297: note: candidates are: static boost::shared_ptr<mbb::O_RestraintList> mbb::O_RestraintList::create(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/molecule.o" "molecule.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/molecule.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/nVector.o
darwin.compile.c++ bin/darwin-4.2.1/debug/oligomer.o
In file included from oligomer.cc:17:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
In file included from oligomer.cc:19:
plug.h: In member function ‘virtual mbb::RPCons mbb::O_PlugWithMates::matesAsCons()’:
plug.h:199: error: no matching function for call to ‘mbb::List<mbb::O_Mate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Mate]
plug.h: In member function ‘mbb::RPCons mbb::O_RingClosingPlug::ringClosingMatesAsCons()’:
plug.h:305: error: no matching function for call to ‘mbb::List<mbb::O_RingClosingMate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_RingClosingMate]
oligomer.cc: In static member function ‘static mbb::RPObject mbb::O_Oligomer::prim_setOligomer(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
oligomer.cc:933: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
oligomer.cc: In member function ‘virtual void mbb::Oligomer_Exposer::exposeCando()’:
oligomer.cc:992: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
oligomer.cc:993: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_DirectionalCoupling]’:
coupling.h:118:   instantiated from here
coupling.h:123: error: ‘virtual void mbb::O_DirectionalCoupling::initialize()’ is private
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_RingCoupling]’:
coupling.h:211:   instantiated from here
coupling.h:216: error: ‘virtual void mbb::O_RingCoupling::initialize()’ is private
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/oligomer.o" "oligomer.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/oligomer.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/residue.o
In file included from residue.cc:14:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
residue.cc: In member function ‘void mbb::O_Residue::addVirtualAtom(const std::string&, mbb::RPCalculatePosition, mbb::RPLisp)’:
residue.cc:140: error: no matching function for call to ‘mbb::O_VirtualAtom::create(mbb::RPLisp, const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&, mbb::RPCalculatePosition&, mbb::RPLisp&)’
virtualAtom.h:19: note: candidates are: static boost::shared_ptr<mbb::O_VirtualAtom> mbb::O_VirtualAtom::create(mbb::RPLisp)
virtualAtom.h:28: note:                 static mbb::RPVirtualAtom mbb::O_VirtualAtom::create(const std::string&, mbb::RPCalculatePosition, mbb::RPLisp)
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context
archiveNode.h: In member function ‘void mbb::Dumb_Node::archiveContainerIfNotEmpty(const std::string&, boost::shared_ptr<X>&) [with OType = mbb::O_RestraintList]’:
residue.cc:191:   instantiated from here
archiveNode.h:1007: error: no matching function for call to ‘mbb::O_RestraintList::create()’
restraint.h:297: note: candidates are: static boost::shared_ptr<mbb::O_RestraintList> mbb::O_RestraintList::create(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/residue.o" "residue.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/residue.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/restraint.o
darwin.compile.c++ bin/darwin-4.2.1/debug/spanningLoop.o
darwin.compile.c++ bin/darwin-4.2.1/debug/monomer.o
In file included from monomer.cc:6:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
In file included from monomerPack.h:11,
                 from monomer.cc:21:
atomIndexer.h: In member function ‘mbb::RPStringSet mbb::O_MapOfMonomerNamesToAtomIndexers::getMonomerNamesAsStringSet()’:
atomIndexer.h:85: error: no matching function for call to ‘mbb::Map<mbb::O_AtomIndexer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_AtomIndexer]
In file included from monomer.cc:25:
plug.h: In member function ‘virtual mbb::RPCons mbb::O_PlugWithMates::matesAsCons()’:
plug.h:199: error: no matching function for call to ‘mbb::List<mbb::O_Mate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Mate]
plug.h: In member function ‘mbb::RPCons mbb::O_RingClosingPlug::ringClosingMatesAsCons()’:
plug.h:305: error: no matching function for call to ‘mbb::List<mbb::O_RingClosingMate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_RingClosingMate]
In file included from monomer.cc:28:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]
monomer.cc: In member function ‘mbb::RPCons mbb::O_Monomer::plugNamesAndCouplingsAsCons()’:
monomer.cc:147: error: no matching function for call to ‘mbb::O_String::create(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
values.h:85: note: candidates are: static boost::shared_ptr<mbb::O_String> mbb::O_String::create(mbb::RPLisp)
values.h:89: note:                 static mbb::RPString mbb::O_String::create(mbb::RPLisp, const std::string&)
monomer.cc:147: error: no matching function for call to ‘mbb::O_Cons::create(mbb::RPCoupling&, boost::shared_ptr<mbb::O_Cons>)’
cons.h:24: note: candidates are: static boost::shared_ptr<mbb::O_Cons> mbb::O_Cons::create(mbb::RPLisp)
cons.h:34: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject)
cons.h:38: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons)
monomer.cc: In member function ‘bool mbb::O_Monomer::isMonomerContextValid()’:
monomer.cc:905: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
monomer.cc: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.cc:332: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_DirectionalCoupling]’:
coupling.h:118:   instantiated from here
coupling.h:123: error: ‘virtual void mbb::O_DirectionalCoupling::initialize()’ is private
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_RingCoupling]’:
coupling.h:211:   instantiated from here
coupling.h:216: error: ‘virtual void mbb::O_RingCoupling::initialize()’ is private
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/monomer.o" "monomer.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/monomer.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/superposeEngine.o
superposeEngine.cc: In member function ‘void mbb::O_SuperposeEngine::setFixedAllPoints(mbb::RPCoordinateArray)’:
superposeEngine.cc:336: error: cannot call member function ‘mbb::RPIntArray mbb::O_IntArray::create(mbb::uint, mbb::RPLisp)’ without object
superposeEngine.cc: In member function ‘void mbb::O_SuperposeEngine::setMoveableAllPoints(mbb::RPCoordinateArray)’:
superposeEngine.cc:371: error: cannot call member function ‘mbb::RPIntArray mbb::O_IntArray::create(mbb::uint, mbb::RPLisp)’ without object

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/superposeEngine.o" "superposeEngine.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/superposeEngine.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/builderDatabase.o
In file included from monomerPack.h:11,
                 from builderDatabase.cc:30:
atomIndexer.h: In member function ‘mbb::RPStringSet mbb::O_MapOfMonomerNamesToAtomIndexers::getMonomerNamesAsStringSet()’:
atomIndexer.h:85: error: no matching function for call to ‘mbb::Map<mbb::O_AtomIndexer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_AtomIndexer]
In file included from builderDatabase.cc:35:
plug.h: In member function ‘virtual mbb::RPCons mbb::O_PlugWithMates::matesAsCons()’:
plug.h:199: error: no matching function for call to ‘mbb::List<mbb::O_Mate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Mate]
plug.h: In member function ‘mbb::RPCons mbb::O_RingClosingPlug::ringClosingMatesAsCons()’:
plug.h:305: error: no matching function for call to ‘mbb::List<mbb::O_RingClosingMate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_RingClosingMate]
In file included from builderDatabase.cc:38:
stereochemistry.h: In member function ‘mbb::RPCons mbb::O_StereoInformation::stereoisomersAsCons()’:
stereochemistry.h:159: error: no matching function for call to ‘mbb::List<mbb::O_Stereoisomer>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Stereoisomer]
builderDatabase.cc: In member function ‘mbb::RPCons mbb::O_BuilderDatabase::constitutionsAsCons()’:
builderDatabase.cc:155: error: no matching function for call to ‘mbb::Map<mbb::O_Constitution>::asCons()’
holder.h:441: note: candidates are: mbb::RPCons mbb::Map<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Constitution]
builderDatabase.cc: In member function ‘mbb::RPCons mbb::O_BuilderDatabase::monomerGroupsAsCons()’:
builderDatabase.cc:160: error: no matching function for call to ‘mbb::Map<mbb::O_MonomerGroup>::asCons()’
holder.h:441: note: candidates are: mbb::RPCons mbb::Map<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_MonomerGroup]
builderDatabase.cc: In member function ‘mbb::RPStringSet mbb::O_BuilderDatabase::getSystemMonomerGroupKeys()’:
builderDatabase.cc:223: error: no matching function for call to ‘mbb::Map<mbb::O_MonomerGroup>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_MonomerGroup]
builderDatabase.cc: In member function ‘virtual mbb::RPObject mbb::O_BuilderDatabase::oGetReference(mbb::RPObjRef)’:
builderDatabase.cc:305: error: no matching function for call to ‘mbb::Map<mbb::O_Constitution>::getDefaultNil(std::string)’
holder.h:432: note: candidates are: boost::shared_ptr<X> mbb::Map<OType>::getDefaultNil(mbb::RPLisp, const std::string&) [with OType = mbb::O_Constitution]
builderDatabase.cc: In member function ‘bool mbb::O_BuilderDatabase::recognizesMonomerSetName(const std::string&)’:
builderDatabase.cc:329: error: ‘_classId’ is not a member of ‘mbb::O_MonomerSet’
builderDatabase.cc: In member function ‘mbb::RPMonomerSet mbb::O_BuilderDatabase::getMonomerSet(const std::string&)’:
builderDatabase.cc:344: error: ‘_classId’ is not a member of ‘mbb::O_MonomerSet’
builderDatabase.cc: In member function ‘mbb::RPCons mbb::O_BuilderDatabase::monomerCoordinatesKeysAsCons()’:
builderDatabase.cc:1064: error: no matching function for call to ‘mbb::O_String::create(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
values.h:85: note: candidates are: static boost::shared_ptr<mbb::O_String> mbb::O_String::create(mbb::RPLisp)
values.h:89: note:                 static mbb::RPString mbb::O_String::create(mbb::RPLisp, const std::string&)
builderDatabase.cc: In member function ‘virtual bool mbb::O_BuilderDatabase::isNil() const’:
builderDatabase.cc:1362: error: passing ‘const mbb::O_BuilderDatabase’ as ‘this’ argument of ‘virtual mbb::RPLisp mbb::O_BuilderDatabase::getEnvironment()’ discards qualifiers
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_DirectionalCoupling]’:
coupling.h:118:   instantiated from here
coupling.h:123: error: ‘virtual void mbb::O_DirectionalCoupling::initialize()’ is private
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_RingCoupling]’:
coupling.h:211:   instantiated from here
coupling.h:216: error: ‘virtual void mbb::O_RingCoupling::initialize()’ is private
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/builderDatabase.o" "builderDatabase.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/builderDatabase.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/torsionDriver.o
torsionDriver.cc: In member function ‘void mbb::Dumb_TorsionDriver::setAggregateAndRoot(mbb::RPAggregate, std::string)’:
torsionDriver.cc:231: error: ‘class mbb::Dumb_TorsionDriver’ has no member named ‘env’
torsionDriver.cc:231: error: expected primary-expression before ‘>’ token
torsionDriver.cc:231: error: expected primary-expression before ‘)’ token
torsionDriver.cc: In member function ‘void mbb::Dumb_TorsionDriver::prepareToDriveTorsions()’:
torsionDriver.cc:346: error: ‘class mbb::Dumb_TorsionDriver’ has no member named ‘env’
torsionDriver.cc:346: error: expected primary-expression before ‘>’ token
torsionDriver.cc:346: error: expected primary-expression before ‘)’ token
torsionDriver.cc:409: error: ‘class mbb::Dumb_TorsionDriver’ has no member named ‘env’
torsionDriver.cc:428: error: ‘class mbb::Dumb_TorsionDriver’ has no member named ‘env’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/torsionDriver.o" "torsionDriver.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/torsionDriver.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/virtualSphere.o
darwin.compile.c++ bin/darwin-4.2.1/debug/moe.o
moe.cc:775: error: ‘void mbb::moeReadAggregateMoeFile(mbb::RPAggregate, mbb::MoeReadFile&, bool&, mbb::RPLisp)’ should have been declared inside ‘mbb’
moe.cc:1285: error: ‘mbb::RPAggregate mbb::moeReadAggregateWithAtomTypes(mbb::RPLisp, const std::string&)’ should have been declared inside ‘mbb’
moe.cc:1348: error: ‘mbb::RPAggregate mbb::moeReadAggregate(mbb::RPLisp, const std::string&)’ should have been declared inside ‘mbb’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/moe.o" "moe.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/moe.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/numericalFunction.o
darwin.compile.c++ bin/darwin-4.2.1/debug/builderState.o
In file included from builderState.h:16,
                 from builderState.cc:5:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]
In file included from builderState.cc:18:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/builderState.o" "builderState.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/builderState.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/confSearchDatabase.o
darwin.compile.c++ bin/darwin-4.2.1/debug/ffAngleDb.o
In file included from ffStretchDb.h:25,
                 from forceField.h:32,
                 from ffAngleDb.cc:17:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
ffAngleDb.cc: In member function ‘mbb::RPFFAngle mbb::O_FFAngleDb::estimateTerm(mbb::RPAtom, mbb::RPAtom, mbb::RPAtom)’:
ffAngleDb.cc:240: error: ‘new_RPFFAngle’ was not declared in this scope

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/ffAngleDb.o" "ffAngleDb.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/ffAngleDb.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/ffBaseDb.o
In file included from ffStretchDb.h:25,
                 from forceField.h:32,
                 from ffBaseDb.cc:16:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/ffBaseDb.o" "ffBaseDb.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/ffBaseDb.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/ffItorDb.o
darwin.compile.c++ bin/darwin-4.2.1/debug/ffNonbondDb.o
darwin.compile.c++ bin/darwin-4.2.1/debug/ffPtorDb.o
darwin.compile.c++ bin/darwin-4.2.1/debug/ffStretchDb.o
In file included from ffStretchDb.h:25,
                 from ffStretchDb.cc:18:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
ffStretchDb.cc: At global scope:
ffStretchDb.cc:34: error: ‘mbb::RPQDomNode mbb::EstimateStretch::asXml’ is not a static member of ‘class mbb::EstimateStretch’
ffStretchDb.cc:34: error: ‘RPLips’ was not declared in this scope
ffStretchDb.cc:35: error: expected ‘,’ or ‘;’ before ‘{’ token

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/ffStretchDb.o" "ffStretchDb.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/ffStretchDb.o...
...on 100th target...
darwin.compile.c++ bin/darwin-4.2.1/debug/ffTypesDb.o
darwin.compile.c++ bin/darwin-4.2.1/debug/ffVdwDb.o
darwin.compile.c++ bin/darwin-4.2.1/debug/forceField.o
In file included from ffStretchDb.h:25,
                 from forceField.h:32,
                 from forceField.cc:18:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
forceField.cc: In member function ‘virtual void mbb::O_ForceField::initialize()’:
forceField.cc:53: error: ‘new_RPInfoDb’ was not declared in this scope
forceField.cc:55: error: ‘new_RPFFTypesDb’ was not declared in this scope
forceField.cc:57: error: ‘new_RPFFStretchDb’ was not declared in this scope
forceField.cc:59: error: ‘new_RPFFAngleDb’ was not declared in this scope
forceField.cc:61: error: ‘new_RPFFItorDb’ was not declared in this scope
forceField.cc:63: error: ‘new_RPFFPtorDb’ was not declared in this scope
forceField.cc:65: error: ‘new_RPFFNonbondDb’ was not declared in this scope
forceField.cc:67: error: ‘new_RPFFVdwDb’ was not declared in this scope

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/forceField.o" "forceField.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/forceField.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/linearAlgebra.o
linearAlgebra.cc: In function ‘void mbb::backSubstituteLDLt(mbb::RPAbstractLargeSquareMatrix, mbb::RPNVector, mbb::RPNVector)’:
linearAlgebra.cc:212: error: no matching function for call to ‘mbb::O_NVector::create(mbb::RPLisp, mbb::uint)’
nVector.h:25: note: candidates are: static boost::shared_ptr<mbb::O_NVector> mbb::O_NVector::create(mbb::RPLisp)
nVector.h:29: note:                 static mbb::RPNVector mbb::O_NVector::create(mbb::uint, mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/linearAlgebra.o" "linearAlgebra.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/linearAlgebra.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/minimizer.o
In file included from ffStretchDb.h:25,
                 from minimizer.cc:22:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
minimizer.cc: In member function ‘void mbb::O_Minimizer::debugStop(const std::string&)’:
minimizer.cc:245: error: no matching function for call to ‘mbb::O_XmlSaveArchive::O_XmlSaveArchive(mbb::RPLisp)’
xmlSaveArchive.h:35: note: candidates are: mbb::O_XmlSaveArchive::O_XmlSaveArchive()
xmlSaveArchive.h:18: note:                 mbb::O_XmlSaveArchive::O_XmlSaveArchive(const mbb::O_XmlSaveArchive&)
minimizer.cc: In member function ‘void mbb::O_Minimizer::lineSearchInitialReport(mbb::RPStepReport, mbb::RPNVector, mbb::RPNVector, mbb::RPNVector, double, double, double, double, double, double)’:
minimizer.cc:626: error: no matching function for call to ‘mbb::O_NumericalFunction::create(mbb::RPLisp, const char [6], const char [6], double&, double&)’
numericalFunction.h:36: note: candidates are: static boost::shared_ptr<mbb::O_NumericalFunction> mbb::O_NumericalFunction::create(mbb::RPLisp)
minimizer.cc:627: error: no matching function for call to ‘mbb::O_NumericalFunction::create(mbb::RPLisp, const char [6], const char [8], double&, double&)’
numericalFunction.h:36: note: candidates are: static boost::shared_ptr<mbb::O_NumericalFunction> mbb::O_NumericalFunction::create(mbb::RPLisp)
minimizer.cc:628: error: no matching function for call to ‘mbb::O_NumericalFunction::create(mbb::RPLisp, const char [6], const char [6], double&, double&)’
numericalFunction.h:36: note: candidates are: static boost::shared_ptr<mbb::O_NumericalFunction> mbb::O_NumericalFunction::create(mbb::RPLisp)
minimizer.cc:629: error: no matching function for call to ‘mbb::O_NumericalFunction::create(mbb::RPLisp, const char [6], const char [9], double&, double&)’
numericalFunction.h:36: note: candidates are: static boost::shared_ptr<mbb::O_NumericalFunction> mbb::O_NumericalFunction::create(mbb::RPLisp)
minimizer.cc:630: error: no matching function for call to ‘mbb::O_NumericalFunction::create(mbb::RPLisp, const char [6], const char [8], double&, double&)’
numericalFunction.h:36: note: candidates are: static boost::shared_ptr<mbb::O_NumericalFunction> mbb::O_NumericalFunction::create(mbb::RPLisp)
minimizer.cc:631: error: no matching function for call to ‘mbb::O_NumericalFunction::create(mbb::RPLisp, const char [6], const char [9], double&, double&)’
numericalFunction.h:36: note: candidates are: static boost::shared_ptr<mbb::O_NumericalFunction> mbb::O_NumericalFunction::create(mbb::RPLisp)
minimizer.cc:632: error: no matching function for call to ‘mbb::O_NumericalFunction::create(mbb::RPLisp, const char [6], const char [16], double&, double&)’
numericalFunction.h:36: note: candidates are: static boost::shared_ptr<mbb::O_NumericalFunction> mbb::O_NumericalFunction::create(mbb::RPLisp)
minimizer.cc:633: error: no matching function for call to ‘mbb::O_NumericalFunction::create(mbb::RPLisp, const char [6], const char [16], double&, double&)’
numericalFunction.h:36: note: candidates are: static boost::shared_ptr<mbb::O_NumericalFunction> mbb::O_NumericalFunction::create(mbb::RPLisp)
minimizer.cc:634: error: no matching function for call to ‘mbb::O_NumericalFunction::create(mbb::RPLisp, const char [6], const char [22], double&, double&)’
numericalFunction.h:36: note: candidates are: static boost::shared_ptr<mbb::O_NumericalFunction> mbb::O_NumericalFunction::create(mbb::RPLisp)
minimizer.cc: In member function ‘void mbb::O_Minimizer::_conjugateGradient(int, mbb::RPNVector, double)’:
minimizer.cc:1260: error: no matching function for call to ‘mbb::O_StepReport::create()’
minimizerLog.h:73: note: candidates are: static boost::shared_ptr<mbb::O_StepReport> mbb::O_StepReport::create(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/minimizer.o" "minimizer.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/minimizer.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/minimizerLog.o
darwin.compile.c++ bin/darwin-4.2.1/debug/jobHistory.o
darwin.compile.c++ bin/darwin-4.2.1/debug/trainerArchive.o
darwin.compile.c++ bin/darwin-4.2.1/debug/omatrix.o
darwin.compile.c++ bin/darwin-4.2.1/debug/iterateObjectDictionary.o
darwin.compile.c++ bin/darwin-4.2.1/debug/iterateObjectList.o
darwin.compile.c++ bin/darwin-4.2.1/debug/iterateHits.o
iterateHits.cc: In member function ‘virtual void mbb::IterateHits_Exposer::exposeCando()’:
iterateHits.cc:60: error: call of overloaded ‘def(const char [5], <unresolved overloaded function type>)’ is ambiguous
wrappers.h:22: note: candidates are: void mbb::def(const std::string&, RT (*)(P1)) [with RT = boost::shared_ptr<mbb::O_IterateHits>, P1 = mbb::RPLisp]
wrappers.h:32: note:                 void mbb::def(const std::string&, RT (*)(P1, P2)) [with RT = mbb::RPIterateHits, P1 = mbb::RPLisp, P2 = mbb::RPHitList]

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/iterateHits.o" "iterateHits.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/iterateHits.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/iterateRange.o
darwin.compile.c++ bin/darwin-4.2.1/debug/iterateCons.o
darwin.compile.c++ bin/darwin-4.2.1/debug/iterateRestraints.o
darwin.compile.c++ bin/darwin-4.2.1/debug/iterator.o
darwin.compile.c++ bin/darwin-4.2.1/debug/pdb.o
pdb.cc:168: error: prototype for ‘mbb::RPAtom mbb::AtomPdbRec::createAtom(mbb::RPLisp)’ does not match any in class ‘mbb::AtomPdbRec’
pdb.cc:45: error: candidate is: mbb::RPAtom mbb::AtomPdbRec::createAtom()
pdb.cc:215: error: prototype for ‘mbb::RPAggregate mbb::EntirePdbRec::createAggregate(mbb::RPLisp)’ does not match any in class ‘mbb::EntirePdbRec’
pdb.cc:71: error: candidate is: mbb::RPAggregate mbb::EntirePdbRec::createAggregate()
pdb.cc:335: error: prototype for ‘mbb::RPAggregate mbb::O_PdbReader::loadPdb(const std::string&, mbb::RPLisp)’ does not match any in class ‘mbb::O_PdbReader’
pdb.h:25: error: candidate is: static mbb::RPAggregate mbb::O_PdbReader::loadPdb(const std::string&)
pdb.cc: In member function ‘mbb::RPAggregate mbb::O_PdbReader::parse(const std::string&)’:
pdb.cc:373: error: no matching function for call to ‘mbb::AtomPdbRec::AtomPdbRec()’
pdb.cc:111: note: candidates are: mbb::AtomPdbRec::AtomPdbRec(mbb::RPLisp)
pdb.cc:19: note:                 mbb::AtomPdbRec::AtomPdbRec(const mbb::AtomPdbRec&)
pdb.cc:379: error: no matching function for call to ‘mbb::AtomPdbRec::AtomPdbRec()’
pdb.cc:111: note: candidates are: mbb::AtomPdbRec::AtomPdbRec(mbb::RPLisp)
pdb.cc:19: note:                 mbb::AtomPdbRec::AtomPdbRec(const mbb::AtomPdbRec&)
pdb.cc: In function ‘void mbb::_setupAtomAndConnectRecordsForOneMolecule(mbb::RPMolecule, std::vector<mbb::AtomPdbRec, std::allocator<mbb::AtomPdbRec> >&, std::vector<mbb::ConnectPdbRec, std::allocator<mbb::ConnectPdbRec> >&, mbb::uint)’:
pdb.cc:430: error: no matching function for call to ‘mbb::AtomPdbRec::AtomPdbRec()’
pdb.cc:111: note: candidates are: mbb::AtomPdbRec::AtomPdbRec(mbb::RPLisp)
pdb.cc:19: note:                 mbb::AtomPdbRec::AtomPdbRec(const mbb::AtomPdbRec&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/pdb.o" "pdb.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/pdb.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/ringFinder.o
darwin.compile.c++ bin/darwin-4.2.1/debug/range.o
range.cc: In member function ‘virtual void mbb::Range_Exposer::exposeCando()’:
range.cc:98: error: no matching function for call to ‘def(const char [9], <unresolved overloaded function type>, mbb::RPLisp)’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/range.o" "range.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/range.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/iterateMatter.o
iterateMatter.cc: In member function ‘virtual void mbb::IterateAtoms_Exposer::exposeCando()’:
iterateMatter.cc:215: error: no matching function for call to ‘def(const char [6], <unresolved overloaded function type>, mbb::RPLisp)’
iterateMatter.cc: In member function ‘virtual void mbb::IterateResidues_Exposer::exposeCando()’:
iterateMatter.cc:240: error: call of overloaded ‘def(const char [9], <unresolved overloaded function type>)’ is ambiguous
wrappers.h:22: note: candidates are: void mbb::def(const std::string&, RT (*)(P1)) [with RT = boost::shared_ptr<mbb::O_IterateResidues>, P1 = mbb::RPLisp]
wrappers.h:32: note:                 void mbb::def(const std::string&, RT (*)(P1, P2)) [with RT = mbb::RPIterateResidues, P1 = mbb::RPLisp, P2 = mbb::RPMatter]
iterateMatter.cc: In member function ‘virtual void mbb::IterateBonds_Exposer::exposeCando()’:
iterateMatter.cc:266: error: no matching function for call to ‘def(const char [6], <unresolved overloaded function type>, mbb::RPLisp)’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/iterateMatter.o" "iterateMatter.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/iterateMatter.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/fileSystem.o
darwin.compile.c++ bin/darwin-4.2.1/debug/candoObject.o
candoObject.cc: In member function ‘virtual void mbb::O_CandoObject::archive(mbb::Dumb_Node*)’:
candoObject.cc:64: error: ‘class mbb::O_Archive’ has no member named ‘classManager’
candoObject.cc:80: error: no matching function for call to ‘mbb::Map<mbb::O_Object>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Object]
candoObject.cc:81: error: ‘class mbb::O_Lisp’ has no member named ‘classManager’
candoObject.cc:83: error: ‘class mbb::O_Archive’ has no member named ‘classManager’
candoObject.cc:84: error: ‘class mbb::ClassInfo’ has no member named ‘_MetaClass’
candoObject.cc: In member function ‘virtual void mbb::CandoObject_Exposer::exposeCando()’:
candoObject.cc:259: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
candoObject.cc:260: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/candoObject.o" "candoObject.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/candoObject.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/expose.o
darwin.compile.c++ bin/darwin-4.2.1/debug/builderDatabaseReference.o
darwin.compile.c++ bin/darwin-4.2.1/debug/macroModel.o
macroModel.cc: In member function ‘mbb::RPMolecule mbb::Dumb_MacroModelFile::readMolecule()’:
macroModel.cc:357: error: ‘class mbb::Dumb_MacroModelFile’ has no member named ‘env’
macroModel.cc:364: error: ‘class mbb::Dumb_MacroModelFile’ has no member named ‘env’
macroModel.cc:380: error: ‘class mbb::Dumb_MacroModelFile’ has no member named ‘env’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/macroModel.o" "macroModel.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/macroModel.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/typeAssignmentRules.o
darwin.compile.c++ bin/darwin-4.2.1/debug/exhaustiveSearch.o
In file included from exhaustiveSearch.cc:7:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]
exhaustiveSearch.cc: In member function ‘virtual void mbb::ExhaustiveSearch_Exposer::exposeCando()’:
exhaustiveSearch.cc:435: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
exhaustiveSearch.cc:436: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
exhaustiveSearch.cc:437: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/exhaustiveSearch.o" "exhaustiveSearch.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/exhaustiveSearch.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/exhaustiveSearchOneSequence.o
In file included from exhaustiveSearchOneSequence.cc:7:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/exhaustiveSearchOneSequence.o" "exhaustiveSearchOneSequence.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/exhaustiveSearchOneSequence.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/search.o
In file included from search.cc:8:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/search.o" "search.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/search.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/alias.o
alias.cc: In member function ‘virtual void mbb::Alias_Exposer::exposeCando()’:
alias.cc:136: error: no matching function for call to ‘mbb::class_<mbb::O_Alias>::class_()’
wrappers.h:105: note: candidates are: mbb::class_<oclass>::class_(mbb::RPLisp) [with OT = mbb::O_Alias]
wrappers.h:100: note:                 mbb::class_<mbb::O_Alias>::class_(const mbb::class_<mbb::O_Alias>&)
alias.cc:138: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/alias.o" "alias.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/alias.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/gaussianInterface.o
darwin.compile.c++ bin/darwin-4.2.1/debug/gamessInterface.o
darwin.compile.c++ bin/darwin-4.2.1/debug/externalInterface.o
darwin.compile.c++ bin/darwin-4.2.1/debug/builderDatabaseDependent.o
darwin.compile.c++ bin/darwin-4.2.1/debug/conformationExplorer.o
conformationExplorer.cc: In member function ‘virtual mbb::RPRender mbb::O_ConformationExplorerEntryStage::rendered(mbb::RPKeyedArguments)’:
conformationExplorer.cc:131: error: no matching function for call to ‘mbb::O_GrColor::createWithName(const char [7], mbb::RPLisp)’
render.h:466: note: candidates are: static mbb::RPGrColor mbb::O_GrColor::createWithName(mbb::RPLisp, const std::string&)
conformationExplorer.cc:155: error: no matching function for call to ‘mbb::O_GrInformation::create(std::basic_string<char, std::char_traits<char>, std::allocator<char> >, mbb::RPLisp)’
render.h:172: note: candidates are: static boost::shared_ptr<mbb::O_GrInformation> mbb::O_GrInformation::create(mbb::RPLisp)
render.h:180: note:                 static mbb::RPGrInformation mbb::O_GrInformation::create(mbb::RPLisp, const std::string&)
conformationExplorer.cc: In member function ‘void mbb::O_ConformationExplorerEntryStage::setConformationExplorerEntry(mbb::RPConformationExplorerEntry)’:
conformationExplorer.cc:190: error: no matching function for call to ‘mbb::O_CoordinateArray::create(mbb::uint, mbb::RPLisp)’
coordinateArray.h:29: note: candidates are: static boost::shared_ptr<mbb::O_CoordinateArray> mbb::O_CoordinateArray::create(mbb::RPLisp)
coordinateArray.h:38: note:                 static mbb::RPCoordinateArray mbb::O_CoordinateArray::create(mbb::RPLisp, mbb::uint)
conformationExplorer.cc: In member function ‘mbb::RPCoordinateArray mbb::O_ConformationExplorer::_extractCoordinateArray(mbb::RPMatter)’:
conformationExplorer.cc:520: error: no matching function for call to ‘mbb::O_CoordinateArray::create(mbb::uint, mbb::RPLisp)’
coordinateArray.h:29: note: candidates are: static boost::shared_ptr<mbb::O_CoordinateArray> mbb::O_CoordinateArray::create(mbb::RPLisp)
coordinateArray.h:38: note:                 static mbb::RPCoordinateArray mbb::O_CoordinateArray::create(mbb::RPLisp, mbb::uint)
conformationExplorer.cc: In member function ‘mbb::RPCons mbb::O_ConformationExplorer::entriesAsCons()’:
conformationExplorer.cc:558: error: no matching function for call to ‘mbb::O_ConformationExplorer::nil()’
conformationExplorer.h:201: note: candidates are: static boost::shared_ptr<mbb::O_ConformationExplorer> mbb::O_ConformationExplorer::nil(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/conformationExplorer.o" "conformationExplorer.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/conformationExplorer.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/superposableConformationCollection.o
superposableConformationCollection.cc: In member function ‘virtual mbb::RPRender mbb::O_SuperposableConformationCollection::rendered(mbb::RPKeyedArguments)’:
superposableConformationCollection.cc:228: error: no matching function for call to ‘mbb::O_GrColor::createWithName(const char [7], mbb::RPLisp)’
render.h:466: note: candidates are: static mbb::RPGrColor mbb::O_GrColor::createWithName(mbb::RPLisp, const std::string&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/superposableConformationCollection.o" "superposableConformationCollection.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/superposableConformationCollection.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/intArray.o
darwin.compile.c++ bin/darwin-4.2.1/debug/zMatrix.o
zMatrix.cc: In member function ‘void mbb::O_ZMatrix::defineForMatterWithStartingAtom(mbb::RPMatter, mbb::RPAtom)’:
zMatrix.cc:475: error: no matching function for call to ‘mbb::O_ZMatrixAngleInternal::create(mbb::RPAtom&, mbb::RPAtom&, mbb::RPAtom&, std::map<boost::shared_ptr<mbb::O_Atom>, unsigned int, std::less<boost::shared_ptr<mbb::O_Atom> >, std::allocator<std::pair<const boost::shared_ptr<mbb::O_Atom>, unsigned int> > >&, boost::shared_ptr<mbb::O_ZMatrix>)’
zMatrix.h:103: note: candidates are: static boost::shared_ptr<mbb::O_ZMatrixAngleInternal> mbb::O_ZMatrixAngleInternal::create(mbb::RPLisp)
zMatrix.h:136: note:                 static mbb::RPZMatrixAngleInternal mbb::O_ZMatrixAngleInternal::create(mbb::RPLisp, mbb::RPAtom, mbb::RPAtom, mbb::RPAtom, std::map<boost::shared_ptr<mbb::O_Atom>, unsigned int, std::less<boost::shared_ptr<mbb::O_Atom> >, std::allocator<std::pair<const boost::shared_ptr<mbb::O_Atom>, unsigned int> > >, mbb::RPZMatrix)
zMatrix.cc:496: error: no matching function for call to ‘mbb::O_ZMatrixDihedralInternal::create(mbb::RPAtom&, mbb::RPAtom&, mbb::RPAtom&, mbb::RPAtom&, std::map<boost::shared_ptr<mbb::O_Atom>, unsigned int, std::less<boost::shared_ptr<mbb::O_Atom> >, std::allocator<std::pair<const boost::shared_ptr<mbb::O_Atom>, unsigned int> > >&, boost::shared_ptr<mbb::O_ZMatrix>)’
zMatrix.h:148: note: candidates are: static boost::shared_ptr<mbb::O_ZMatrixDihedralInternal> mbb::O_ZMatrixDihedralInternal::create(mbb::RPLisp)
zMatrix.h:182: note:                 static mbb::RPZMatrixDihedralInternal mbb::O_ZMatrixDihedralInternal::create(mbb::RPLisp, mbb::RPAtom, mbb::RPAtom, mbb::RPAtom, mbb::RPAtom, std::map<boost::shared_ptr<mbb::O_Atom>, unsigned int, std::less<boost::shared_ptr<mbb::O_Atom> >, std::allocator<std::pair<const boost::shared_ptr<mbb::O_Atom>, unsigned int> > >, mbb::RPZMatrix)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/zMatrix.o" "zMatrix.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/zMatrix.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/conformationCollection.o
conformationCollection.cc: In member function ‘void mbb::O_ConformationCollectionEntry::setConformationCollection(mbb::RPConformationCollection)’:
conformationCollection.cc:73: error: no matching function for call to ‘mbb::O_CoordinateArray::create(mbb::uint, mbb::RPLisp)’
coordinateArray.h:29: note: candidates are: static boost::shared_ptr<mbb::O_CoordinateArray> mbb::O_CoordinateArray::create(mbb::RPLisp)
coordinateArray.h:38: note:                 static mbb::RPCoordinateArray mbb::O_CoordinateArray::create(mbb::RPLisp, mbb::uint)
conformationCollection.cc: In member function ‘mbb::RPCoordinateArray mbb::O_ConformationCollection::_extractCoordinateArray(mbb::RPMatter)’:
conformationCollection.cc:152: error: no matching function for call to ‘mbb::O_CoordinateArray::create(mbb::uint, mbb::RPLisp)’
coordinateArray.h:29: note: candidates are: static boost::shared_ptr<mbb::O_CoordinateArray> mbb::O_CoordinateArray::create(mbb::RPLisp)
coordinateArray.h:38: note:                 static mbb::RPCoordinateArray mbb::O_CoordinateArray::create(mbb::RPLisp, mbb::uint)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/conformationCollection.o" "conformationCollection.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/conformationCollection.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/structureComparer.o
structureComparer.cc: In member function ‘void mbb::O_StructureComparer::initializeFixedCoordinates()’:
structureComparer.cc:65: error: no matching function for call to ‘mbb::O_CoordinateArray::create(unsigned int, mbb::RPLisp)’
coordinateArray.h:29: note: candidates are: static boost::shared_ptr<mbb::O_CoordinateArray> mbb::O_CoordinateArray::create(mbb::RPLisp)
coordinateArray.h:38: note:                 static mbb::RPCoordinateArray mbb::O_CoordinateArray::create(mbb::RPLisp, mbb::uint)
structureComparer.cc: In member function ‘double mbb::O_StructureComparer::calculateRmsWithMatter(mbb::RPMatter)’:
structureComparer.cc:123: error: no matching function for call to ‘mbb::O_CoordinateArray::create(unsigned int, mbb::RPLisp)’
coordinateArray.h:29: note: candidates are: static boost::shared_ptr<mbb::O_CoordinateArray> mbb::O_CoordinateArray::create(mbb::RPLisp)
coordinateArray.h:38: note:                 static mbb::RPCoordinateArray mbb::O_CoordinateArray::create(mbb::RPLisp, mbb::uint)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/structureComparer.o" "structureComparer.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/structureComparer.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/scorer.o
In file included from scorer.cc:6:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]
scorer.cc: In member function ‘virtual mbb::RPRenderDisplayList mbb::O_Scorer::renderSuperposeDeviations(mbb::RPAliasReferencer)’:
scorer.cc:380: error: ‘O_GrColorWithName’ has not been declared
scorer.cc:389: error: no matching function for call to ‘mbb::O_GrLine::create(mbb::Vector3&, mbb::Vector3, mbb::RPLisp)’
render.h:119: note: candidates are: static boost::shared_ptr<mbb::O_GrLine> mbb::O_GrLine::create(mbb::RPLisp)
render.h:147: note:                 static mbb::RPGrLine mbb::O_GrLine::create(mbb::RPLisp, const mbb::Vector3&, const mbb::Vector3&)
render.h:159: note:                 static mbb::RPGrLine mbb::O_GrLine::create(mbb::RPLisp, const mbb::Vector3&, const mbb::Vector3&, mbb::uint)
scorer.cc:395: error: no matching function for call to ‘mbb::O_GrSphere::create(mbb::Vector3&, double, mbb::RPLisp)’
render.h:267: note: candidates are: static boost::shared_ptr<mbb::O_GrSphere> mbb::O_GrSphere::create(mbb::RPLisp)
render.h:276: note:                 static mbb::RPGrSphere mbb::O_GrSphere::create(mbb::RPLisp, const mbb::Vector3&, double)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/scorer.o" "scorer.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/scorer.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/command.o
darwin.compile.c++ bin/darwin-4.2.1/debug/commandOligomer.o
In file included from commandOligomer.cc:12:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
commandOligomer.cc: In member function ‘virtual void mbb::O_CommandOligomer_AddMultiMonomerAsLeaf::doIt()’:
commandOligomer.cc:141: error: no matching function for call to ‘mbb::O_MultiMonomer::create(mbb::RPBuilderDatabase, mbb::RPLisp)’
monomer.h:317: note: candidates are: static boost::shared_ptr<mbb::O_MultiMonomer> mbb::O_MultiMonomer::create(mbb::RPLisp)
commandOligomer.cc: In member function ‘void mbb::O_CommandOligomer_InsertBeforeMonomer::setup(mbb::RPOligomer, mbb::RPBuilderDatabase, mbb::RPMonomer, const std::string&, const std::string&)’:
commandOligomer.cc:397: error: no matching function for call to ‘mbb::O_MultiMonomer::create(mbb::RPBuilderDatabase, mbb::RPLisp)’
monomer.h:317: note: candidates are: static boost::shared_ptr<mbb::O_MultiMonomer> mbb::O_MultiMonomer::create(mbb::RPLisp)
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_DirectionalCoupling]’:
coupling.h:118:   instantiated from here
coupling.h:123: error: ‘virtual void mbb::O_DirectionalCoupling::initialize()’ is private
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_RingCoupling]’:
coupling.h:211:   instantiated from here
coupling.h:216: error: ‘virtual void mbb::O_RingCoupling::initialize()’ is private
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/commandOligomer.o" "commandOligomer.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/commandOligomer.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/monomerPack.o
In file included from monomerPack.h:11,
                 from monomerPack.cc:4:
atomIndexer.h: In member function ‘mbb::RPStringSet mbb::O_MapOfMonomerNamesToAtomIndexers::getMonomerNamesAsStringSet()’:
atomIndexer.h:85: error: no matching function for call to ‘mbb::Map<mbb::O_AtomIndexer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_AtomIndexer]
monomerPack.cc: In function ‘mbb::RPObject mbb::prim_createMonomerPack(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
monomerPack.cc:64: error: invalid use of ‘this’ in non-member function
monomerPack.cc:64: error: expected primary-expression before ‘>’ token
monomerPack.cc:64: error: expected primary-expression before ‘)’ token
monomerPack.cc: In function ‘mbb::RPObject mbb::prim_setMonomerPack(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
monomerPack.cc:142: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
monomerPack.cc: In member function ‘virtual void mbb::MonomerPack_exposer::exposeCando()’:
monomerPack.cc:471: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
monomerPack.cc:472: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
monomerPack.cc:473: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/monomerPack.o" "monomerPack.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/monomerPack.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/scorerBase.o
In file included from scorerBase.cc:7:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]
scorerBase.cc: In member function ‘virtual mbb::RPRenderDisplayList mbb::O_ScorerBase::getRenderForScore(mbb::RPAliasReferencer, mbb::RPScorerState)’:
scorerBase.cc:394: error: no matching function for call to ‘mbb::O_GrInformation::create(std::basic_string<char, std::char_traits<char>, std::allocator<char> >, mbb::RPLisp)’
render.h:172: note: candidates are: static boost::shared_ptr<mbb::O_GrInformation> mbb::O_GrInformation::create(mbb::RPLisp)
render.h:180: note:                 static mbb::RPGrInformation mbb::O_GrInformation::create(mbb::RPLisp, const std::string&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/scorerBase.o" "scorerBase.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/scorerBase.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/statusTracker.o
darwin.compile.c++ bin/darwin-4.2.1/debug/hits.o
In file included from hits.cc:12:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]
hits.cc: In function ‘mbb::RPObject mbb::prim_setHitList(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
hits.cc:813: error: invalid use of ‘this’ in non-member function
hits.cc:813: error: expected primary-expression before ‘>’ token
hits.cc:813: error: expected primary-expression before ‘)’ token
hits.cc:815: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
hits.cc: In member function ‘virtual void mbb::HitList_Exposer::exposeCando()’:
hits.cc:887: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/hits.o" "hits.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/hits.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/atomIndexer.o
In file included from atomIndexer.cc:4:
atomIndexer.h: In member function ‘mbb::RPStringSet mbb::O_MapOfMonomerNamesToAtomIndexers::getMonomerNamesAsStringSet()’:
atomIndexer.h:85: error: no matching function for call to ‘mbb::Map<mbb::O_AtomIndexer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_AtomIndexer]
atomIndexer.cc: In static member function ‘static boost::shared_ptr<mbb::O_AtomIndexer> mbb::O_AtomIndexer::nil(mbb::RPLisp)’:
atomIndexer.cc:9: error: invalid use of incomplete type ‘struct mbb::O_Lisp’
foundation.h:367: error: forward declaration of ‘struct mbb::O_Lisp’
atomIndexer.cc: In member function ‘virtual bool mbb::O_AtomIndexer::isNil() const’:
atomIndexer.cc:9: error: invalid use of incomplete type ‘struct mbb::O_Lisp’
foundation.h:367: error: forward declaration of ‘struct mbb::O_Lisp’
atomIndexer.cc: In static member function ‘static boost::shared_ptr<mbb::O_MapOfMonomerNamesToAtomIndexers> mbb::O_MapOfMonomerNamesToAtomIndexers::nil(mbb::RPLisp)’:
atomIndexer.cc:10: error: invalid use of incomplete type ‘struct mbb::O_Lisp’
foundation.h:367: error: forward declaration of ‘struct mbb::O_Lisp’
atomIndexer.cc: In member function ‘virtual bool mbb::O_MapOfMonomerNamesToAtomIndexers::isNil() const’:
atomIndexer.cc:10: error: invalid use of incomplete type ‘struct mbb::O_Lisp’
foundation.h:367: error: forward declaration of ‘struct mbb::O_Lisp’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/atomIndexer.o" "atomIndexer.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/atomIndexer.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/fragmentHolder.o
In file included from fragmentHolder.cc:8:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
In file included from fragmentHolder.cc:18:
atomIndexer.h: In member function ‘mbb::RPStringSet mbb::O_MapOfMonomerNamesToAtomIndexers::getMonomerNamesAsStringSet()’:
atomIndexer.h:85: error: no matching function for call to ‘mbb::Map<mbb::O_AtomIndexer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_AtomIndexer]
fragmentHolder.cc: In static member function ‘static mbb::RPRenderDisplayList mbb::FragmentHolder::render(mbb::RPOligomerBuilder, mbb::uint)’:
fragmentHolder.cc:48: error: no matching function for call to ‘mbb::O_GrColor::createWithName(const char [7])’
render.h:466: note: candidates are: static mbb::RPGrColor mbb::O_GrColor::createWithName(mbb::RPLisp, const std::string&)
fragmentHolder.cc: In member function ‘bool mbb::FragmentHolder::initializeFromFragmentCoordinates(mbb::RPFragmentCoordinates, mbb::ScaffoldHolder&, mbb::RPOligomerBuilder, mbb::uint, mbb::uint, mbb::RPAtomIndexer)’:
fragmentHolder.cc:112: error: ‘_classId’ is not a member of ‘mbb::O_Anchor’
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/fragmentHolder.o" "fragmentHolder.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/fragmentHolder.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/scaffoldHolder.o
In file included from scaffoldHolder.cc:11:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
scaffoldHolder.cc: In member function ‘void mbb::FrameHolder::initializeRoot(mbb::O_OligomerBuilder*)’:
scaffoldHolder.cc:47: error: ‘class mbb::FrameHolder’ has no member named ‘env’
scaffoldHolder.cc:48: error: ‘class mbb::FrameHolder’ has no member named ‘env’
scaffoldHolder.cc:49: error: ‘class mbb::FrameHolder’ has no member named ‘env’
scaffoldHolder.cc: In member function ‘void mbb::FrameHolder::initializeCompleteFrame(mbb::O_OligomerBuilder*)’:
scaffoldHolder.cc:57: error: ‘class mbb::FrameHolder’ has no member named ‘env’
scaffoldHolder.cc:58: error: ‘class mbb::FrameHolder’ has no member named ‘env’
scaffoldHolder.cc:59: error: ‘class mbb::FrameHolder’ has no member named ‘env’
scaffoldHolder.cc: In member function ‘void mbb::FrameHolder::buildUsingAtoms(mbb::O_OligomerBuilder*)’:
scaffoldHolder.cc:148: error: ‘class mbb::FrameHolder’ has no member named ‘env’
scaffoldHolder.cc: In member function ‘mbb::RPRenderDisplayList mbb::ScaffoldHolder::rendered(mbb::RPOligomerBuilder)’:
scaffoldHolder.cc:224: error: ‘class mbb::ScaffoldHolder’ has no member named ‘env’
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/scaffoldHolder.o" "scaffoldHolder.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/scaffoldHolder.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/builder.o
In file included from builder.cc:5:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]
In file included from builder.cc:20:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
builder.cc: In static member function ‘static mbb::RPObject mbb::O_Builder::prim_setBuilder(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
builder.cc:1295: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
builder.cc: In member function ‘virtual void mbb::Builder_Exposer::exposeCando()’:
builder.cc:1360: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/builder.o" "builder.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/builder.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/oligomerBuilder.o
In file included from oligomerBuilder.cc:19:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
In file included from oligomerBuilder.cc:21:
atomIndexer.h: In member function ‘mbb::RPStringSet mbb::O_MapOfMonomerNamesToAtomIndexers::getMonomerNamesAsStringSet()’:
atomIndexer.h:85: error: no matching function for call to ‘mbb::Map<mbb::O_AtomIndexer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_AtomIndexer]
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_DirectionalCoupling]’:
coupling.h:118:   instantiated from here
coupling.h:123: error: ‘virtual void mbb::O_DirectionalCoupling::initialize()’ is private
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_RingCoupling]’:
coupling.h:211:   instantiated from here
coupling.h:216: error: ‘virtual void mbb::O_RingCoupling::initialize()’ is private
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/oligomerBuilder.o" "oligomerBuilder.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/oligomerBuilder.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/frameTransform.o
darwin.compile.c++ bin/darwin-4.2.1/debug/scaffold.o
scaffold.cc: In member function ‘void mbb::O_ScaffoldList::defineFromExtractScaffoldAndConformationCollection(mbb::RPExtractScaffold, mbb::RPMonomer, mbb::RPConformationCollection, mbb::RPFragmentCoordinates, bool, mbb::uint)’:
scaffold.cc:277: error: no matching function for call to ‘mbb::O_Scaffold::create(boost::shared_ptr<mbb::O_ScaffoldList>, mbb::RPLisp)’
scaffold.h:39: note: candidates are: static boost::shared_ptr<mbb::O_Scaffold> mbb::O_Scaffold::create(mbb::RPLisp)
scaffold.cc: In member function ‘bool mbb::O_ScaffoldList::hasAnchorOrigin()’:
scaffold.cc:296: error: ‘_classId’ is not a member of ‘mbb::O_AnchorOrigin’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/scaffold.o" "scaffold.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/scaffold.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/frame.o
In file included from frame.cc:13:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/frame.o" "frame.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/frame.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/fragmentCoordinates.o
In file included from fragmentCoordinates.cc:4:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
fragmentCoordinates.cc: In member function ‘bool mbb::O_FragmentCoordinates::isCoreFragmentCoordinates()’:
fragmentCoordinates.cc:73: error: ‘_classId’ is not a member of ‘mbb::O_Anchor’
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/fragmentCoordinates.o" "fragmentCoordinates.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/fragmentCoordinates.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/monomerCoordinates.o
In file included from monomerCoordinates.cc:19:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
monomerCoordinates.cc: In member function ‘mbb::RPCons mbb::O_MonomerCoordinates::testExtraction(mbb::RPTopology, mbb::RPAggregate, mbb::RPMonomer)’:
monomerCoordinates.cc:310: error: no matching function for call to ‘mbb::O_Bool::create(bool&)’
values.h:265: note: candidates are: static boost::shared_ptr<mbb::O_Bool> mbb::O_Bool::create(mbb::RPLisp)
values.h:271: note:                 static mbb::RPBool mbb::O_Bool::create(mbb::RPLisp, bool)
monomerCoordinates.cc:312: error: no matching function for call to ‘mbb::O_Cons::create(mbb::RPKeyedObject&, boost::shared_ptr<mbb::O_Cons>)’
cons.h:24: note: candidates are: static boost::shared_ptr<mbb::O_Cons> mbb::O_Cons::create(mbb::RPLisp)
cons.h:34: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject)
cons.h:38: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons)
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context
archiveNode.h: In member function ‘void mbb::Dumb_Node::archiveOnlyObjectOfClass(boost::shared_ptr<X>&) [with O_CppClass = mbb::O_ScaffoldList]’:
monomerCoordinates.cc:58:   instantiated from here
archiveNode.h:922: error: no matching function for call to ‘mbb::O_ScaffoldList::nil(<unresolved overloaded function type>)’
scaffold.h:93: note: candidates are: static boost::shared_ptr<mbb::O_ScaffoldList> mbb::O_ScaffoldList::nil(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/monomerCoordinates.o" "monomerCoordinates.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/monomerCoordinates.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/structureList.o
structureList.cc: In member function ‘mbb::RPStructure_Old_ListEntry mbb::O_Structure_Old_List::createStructureListEntryIfConformationIsNew(mbb::RPMatter)’:
structureList.cc:215: error: no matching function for call to ‘mbb::O_CoordinateArray::create(unsigned int, mbb::RPLisp)’
coordinateArray.h:29: note: candidates are: static boost::shared_ptr<mbb::O_CoordinateArray> mbb::O_CoordinateArray::create(mbb::RPLisp)
coordinateArray.h:38: note:                 static mbb::RPCoordinateArray mbb::O_CoordinateArray::create(mbb::RPLisp, mbb::uint)
structureList.cc:271: error: incomplete type ‘mbb::O_StructureListEntry’ used in nested name specifier
structureList.cc:277: error: no matching function for call to ‘mbb::O_CoordinateArray::create(unsigned int, mbb::RPLisp)’
coordinateArray.h:29: note: candidates are: static boost::shared_ptr<mbb::O_CoordinateArray> mbb::O_CoordinateArray::create(mbb::RPLisp)
coordinateArray.h:38: note:                 static mbb::RPCoordinateArray mbb::O_CoordinateArray::create(mbb::RPLisp, mbb::uint)
structureList.cc:288: error: no matching function for call to ‘mbb::O_CoordinateArray::create(unsigned int, mbb::RPLisp)’
coordinateArray.h:29: note: candidates are: static boost::shared_ptr<mbb::O_CoordinateArray> mbb::O_CoordinateArray::create(mbb::RPLisp)
coordinateArray.h:38: note:                 static mbb::RPCoordinateArray mbb::O_CoordinateArray::create(mbb::RPLisp, mbb::uint)
structureList.cc: In member function ‘virtual mbb::RPRender mbb::O_Structure_Old_List::rendered(mbb::RPKeyedArguments)’:
structureList.cc:436: error: no matching function for call to ‘mbb::O_GrColor::createWithName(const char [7], mbb::RPLisp)’
render.h:466: note: candidates are: static mbb::RPGrColor mbb::O_GrColor::createWithName(mbb::RPLisp, const std::string&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/structureList.o" "structureList.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/structureList.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/extractFragment.o
In file included from extractFragment.cc:7:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/extractFragment.o" "extractFragment.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/extractFragment.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/anchor.o
In file included from anchor.cc:7:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
In file included from anchor.cc:11:
plug.h: In member function ‘virtual mbb::RPCons mbb::O_PlugWithMates::matesAsCons()’:
plug.h:199: error: no matching function for call to ‘mbb::List<mbb::O_Mate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Mate]
plug.h: In member function ‘mbb::RPCons mbb::O_RingClosingPlug::ringClosingMatesAsCons()’:
plug.h:305: error: no matching function for call to ‘mbb::List<mbb::O_RingClosingMate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_RingClosingMate]
anchor.cc: In member function ‘virtual void mbb::Anchor_Exposer::exposeCando()’:
anchor.cc:235: error: no matching function for call to ‘mbb::class_<mbb::O_Anchor>::class_()’
wrappers.h:105: note: candidates are: mbb::class_<oclass>::class_(mbb::RPLisp) [with OT = mbb::O_Anchor]
wrappers.h:100: note:                 mbb::class_<mbb::O_Anchor>::class_(const mbb::class_<mbb::O_Anchor>&)
anchor.cc: In member function ‘virtual void mbb::AnchorOrigin_Exposer::exposeCando()’:
anchor.cc:260: error: no matching function for call to ‘mbb::class_<mbb::O_AnchorOrigin>::class_()’
wrappers.h:105: note: candidates are: mbb::class_<oclass>::class_(mbb::RPLisp) [with OT = mbb::O_AnchorOrigin]
wrappers.h:100: note:                 mbb::class_<mbb::O_AnchorOrigin>::class_(const mbb::class_<mbb::O_AnchorOrigin>&)
anchor.cc: In member function ‘virtual void mbb::AnchorOnOtherSideOfPlug_Exposer::exposeCando()’:
anchor.cc:287: error: no matching function for call to ‘mbb::class_<mbb::O_AnchorOnOtherSideOfPlug>::class_()’
wrappers.h:105: note: candidates are: mbb::class_<oclass>::class_(mbb::RPLisp) [with OT = mbb::O_AnchorOnOtherSideOfPlug]
wrappers.h:100: note:                 mbb::class_<mbb::O_AnchorOnOtherSideOfPlug>::class_(const mbb::class_<mbb::O_AnchorOnOtherSideOfPlug>&)
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_DirectionalCoupling]’:
coupling.h:118:   instantiated from here
coupling.h:123: error: ‘virtual void mbb::O_DirectionalCoupling::initialize()’ is private
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_RingCoupling]’:
coupling.h:211:   instantiated from here
coupling.h:216: error: ‘virtual void mbb::O_RingCoupling::initialize()’ is private
object.h:232: error: within this context
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/anchor.o" "anchor.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/anchor.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/fragment.o
darwin.compile.c++ bin/darwin-4.2.1/debug/boundFrame.o
In file included from boundFrame.cc:6:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
boundFrame.cc: In member function ‘virtual mbb::RPCoordinateSystem mbb::O_OriginBoundFrame::getCoordinateSystem()’:
boundFrame.cc:33: error: no matching function for call to ‘mbb::O_CoordinateSystem::O_CoordinateSystem(mbb::RPLisp)’
coordSys.h:212: note: candidates are: mbb::O_CoordinateSystem::O_CoordinateSystem(const mbb::O_CoordinateSystem&)
coordSys.h:211: note:                 mbb::O_CoordinateSystem::O_CoordinateSystem()
boundFrame.cc: In member function ‘virtual bool mbb::O_AtomBoundFrame::definesCoordinateSystem()’:
boundFrame.cc:129: error: no matching function for call to ‘mbb::O_CoordinateSystem::O_CoordinateSystem(mbb::RPLisp)’
coordSys.h:212: note: candidates are: mbb::O_CoordinateSystem::O_CoordinateSystem(const mbb::O_CoordinateSystem&)
coordSys.h:211: note:                 mbb::O_CoordinateSystem::O_CoordinateSystem()
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/boundFrame.o" "boundFrame.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/boundFrame.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/frameRecognizer.o
frameRecognizer.cc: In member function ‘virtual void mbb::O_FrameRecognizer::archive(mbb::Dumb_Node*)’:
frameRecognizer.cc:60: error: invalid use of incomplete type ‘struct mbb::ArchiveError’
archiveNode.h:67: error: forward declaration of ‘struct mbb::ArchiveError’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/frameRecognizer.o" "frameRecognizer.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/frameRecognizer.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/values.o
values.cc: In member function ‘mbb::RPCons mbb::O_Text::splitAtWhiteSpace()’:
values.cc:165: error: no matching function for call to ‘mbb::O_String::create(std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
values.h:85: note: candidates are: static boost::shared_ptr<mbb::O_String> mbb::O_String::create(mbb::RPLisp)
values.h:89: note:                 static mbb::RPString mbb::O_String::create(mbb::RPLisp, const std::string&)
values.cc: In member function ‘mbb::RPCons mbb::O_Text::split(const std::string&)’:
values.cc:175: error: no matching function for call to ‘mbb::O_Cons::createFromRangeObjectify(__gnu_cxx::__normal_iterator<std::basic_string<char, std::char_traits<char>, std::allocator<char> >*, std::vector<std::basic_string<char, std::char_traits<char>, std::allocator<char> >, std::allocator<std::basic_string<char, std::char_traits<char>, std::allocator<char> > > > >, __gnu_cxx::__normal_iterator<std::basic_string<char, std::char_traits<char>, std::allocator<char> >*, std::vector<std::basic_string<char, std::char_traits<char>, std::allocator<char> >, std::allocator<std::basic_string<char, std::char_traits<char>, std::allocator<char> > > > >)’
values.cc: In function ‘boost::shared_ptr<oTr> mbb::t_add(mbb::RPObject, mbb::RPObject) [with oTx = mbb::O_Int, oTy = mbb::O_Int, Tx = int, Ty = int, oTr = mbb::O_Int]’:
values.cc:49:   instantiated from here
values.cc:24: error: no matching function for call to ‘mbb::O_Int::create(int)’
values.h:129: note: candidates are: static boost::shared_ptr<mbb::O_Int> mbb::O_Int::create(mbb::RPLisp)
values.h:142: note:                 static mbb::RPInt mbb::O_Int::create(mbb::RPLisp, int)
values.cc: In function ‘boost::shared_ptr<oTr> mbb::t_add(mbb::RPObject, mbb::RPObject) [with oTx = mbb::O_Int, oTy = mbb::O_LongLongInt, Tx = long long int, Ty = long long int, oTr = mbb::O_LongLongInt]’:
values.cc:52:   instantiated from here
values.cc:24: error: no matching function for call to ‘mbb::O_LongLongInt::create(long long int)’
values.h:184: note: candidates are: static boost::shared_ptr<mbb::O_LongLongInt> mbb::O_LongLongInt::create(mbb::RPLisp)
values.h:190: note:                 static mbb::RPLongLongInt mbb::O_LongLongInt::create(mbb::RPLisp, mbb::LongLongInt)
values.cc: In function ‘boost::shared_ptr<oTr> mbb::t_add(mbb::RPObject, mbb::RPObject) [with oTx = mbb::O_Int, oTy = mbb::O_Real, Tx = double, Ty = double, oTr = mbb::O_Real]’:
values.cc:55:   instantiated from here
values.cc:24: error: no matching function for call to ‘mbb::O_Real::create(double)’
values.h:232: note: candidates are: static boost::shared_ptr<mbb::O_Real> mbb::O_Real::create(mbb::RPLisp)
values.h:238: note:                 static mbb::RPReal mbb::O_Real::create(mbb::RPLisp, double)
values.cc: In function ‘boost::shared_ptr<oTr> mbb::t_add(mbb::RPObject, mbb::RPObject) [with oTx = mbb::O_LongLongInt, oTy = mbb::O_Int, Tx = long long int, Ty = long long int, oTr = mbb::O_LongLongInt]’:
values.cc:61:   instantiated from here
values.cc:24: error: no matching function for call to ‘mbb::O_LongLongInt::create(long long int)’
values.h:184: note: candidates are: static boost::shared_ptr<mbb::O_LongLongInt> mbb::O_LongLongInt::create(mbb::RPLisp)
values.h:190: note:                 static mbb::RPLongLongInt mbb::O_LongLongInt::create(mbb::RPLisp, mbb::LongLongInt)
values.cc: In function ‘boost::shared_ptr<oTr> mbb::t_add(mbb::RPObject, mbb::RPObject) [with oTx = mbb::O_LongLongInt, oTy = mbb::O_LongLongInt, Tx = long long int, Ty = long long int, oTr = mbb::O_LongLongInt]’:
values.cc:64:   instantiated from here
values.cc:24: error: no matching function for call to ‘mbb::O_LongLongInt::create(long long int)’
values.h:184: note: candidates are: static boost::shared_ptr<mbb::O_LongLongInt> mbb::O_LongLongInt::create(mbb::RPLisp)
values.h:190: note:                 static mbb::RPLongLongInt mbb::O_LongLongInt::create(mbb::RPLisp, mbb::LongLongInt)
values.cc: In function ‘boost::shared_ptr<oTr> mbb::t_add(mbb::RPObject, mbb::RPObject) [with oTx = mbb::O_LongLongInt, oTy = mbb::O_Real, Tx = double, Ty = double, oTr = mbb::O_Real]’:
values.cc:67:   instantiated from here
values.cc:24: error: no matching function for call to ‘mbb::O_Real::create(double)’
values.h:232: note: candidates are: static boost::shared_ptr<mbb::O_Real> mbb::O_Real::create(mbb::RPLisp)
values.h:238: note:                 static mbb::RPReal mbb::O_Real::create(mbb::RPLisp, double)
values.cc: In function ‘boost::shared_ptr<oTr> mbb::t_add(mbb::RPObject, mbb::RPObject) [with oTx = mbb::O_Real, oTy = mbb::O_Int, Tx = double, Ty = double, oTr = mbb::O_Real]’:
values.cc:73:   instantiated from here
values.cc:24: error: no matching function for call to ‘mbb::O_Real::create(double)’
values.h:232: note: candidates are: static boost::shared_ptr<mbb::O_Real> mbb::O_Real::create(mbb::RPLisp)
values.h:238: note:                 static mbb::RPReal mbb::O_Real::create(mbb::RPLisp, double)
values.cc: In function ‘boost::shared_ptr<oTr> mbb::t_add(mbb::RPObject, mbb::RPObject) [with oTx = mbb::O_Real, oTy = mbb::O_LongLongInt, Tx = double, Ty = double, oTr = mbb::O_Real]’:
values.cc:76:   instantiated from here
values.cc:24: error: no matching function for call to ‘mbb::O_Real::create(double)’
values.h:232: note: candidates are: static boost::shared_ptr<mbb::O_Real> mbb::O_Real::create(mbb::RPLisp)
values.h:238: note:                 static mbb::RPReal mbb::O_Real::create(mbb::RPLisp, double)
values.cc: In function ‘boost::shared_ptr<oTr> mbb::t_add(mbb::RPObject, mbb::RPObject) [with oTx = mbb::O_Real, oTy = mbb::O_Real, Tx = double, Ty = double, oTr = mbb::O_Real]’:
values.cc:79:   instantiated from here
values.cc:24: error: no matching function for call to ‘mbb::O_Real::create(double)’
values.h:232: note: candidates are: static boost::shared_ptr<mbb::O_Real> mbb::O_Real::create(mbb::RPLisp)
values.h:238: note:                 static mbb::RPReal mbb::O_Real::create(mbb::RPLisp, double)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/values.o" "values.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/values.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/quickDom.o
In file included from quickDom.cc:14:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
quickDom.cc: In member function ‘virtual void mbb::MySaxDomHandler::startElement()’:
quickDom.cc:168: error: ‘class mbb::MySaxDomHandler’ has no member named ‘env’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/quickDom.o" "quickDom.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/quickDom.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/extractScaffold.o
darwin.compile.c++ bin/darwin-4.2.1/debug/chemInfo.o
chemInfo.cc: In member function ‘mbb::RPAtom mbb::O_ChemInfoMatch::getAtomWithTag(const std::string&)’:
chemInfo.cc:104: error: no matching function for call to ‘mbb::Map<mbb::O_Atom>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Atom]
chemInfo.cc: In member function ‘bool mbb::O_ChemInfo::matches(mbb::RPAtom)’:
chemInfo.cc:361: error: no matching function for call to ‘mbb::O_Root::createNewMatch()’
chemInfo.h:746: note: candidates are: void mbb::O_Root::createNewMatch(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/chemInfo.o" "chemInfo.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/chemInfo.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/candoFormatSaveArchive.o
darwin.compile.c++ bin/darwin-4.2.1/debug/extractFrame.o
In file included from extractFrame.cc:12:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
extractFrame.cc: At global scope:
extractFrame.cc:326: error: prototype for ‘std::string mbb::O_FrameFinisher::createAlias(const std::string&, mbb::RPFrameRecognizer)’ does not match any in class ‘mbb::O_FrameFinisher’
extractFrame.h:190: error: candidate is: static std::string mbb::O_FrameFinisher::createAlias(mbb::RPLisp, const std::string&, mbb::RPFrameRecognizer)
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_DirectionalCoupling]’:
coupling.h:118:   instantiated from here
coupling.h:123: error: ‘virtual void mbb::O_DirectionalCoupling::initialize()’ is private
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_RingCoupling]’:
coupling.h:211:   instantiated from here
coupling.h:216: error: ‘virtual void mbb::O_RingCoupling::initialize()’ is private
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/extractFrame.o" "extractFrame.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/extractFrame.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/bitVector.o
darwin.compile.c++ bin/darwin-4.2.1/debug/twister.o
darwin.compile.c++ bin/darwin-4.2.1/debug/largeSquareMatrix.o
darwin.compile.c++ bin/darwin-4.2.1/debug/stereochemistry.o
In file included from stereochemistry.cc:10:
stereochemistry.h: In member function ‘mbb::RPCons mbb::O_StereoInformation::stereoisomersAsCons()’:
stereochemistry.h:159: error: no matching function for call to ‘mbb::List<mbb::O_Stereoisomer>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Stereoisomer]
stereochemistry.cc: At global scope:
stereochemistry.cc:82: error: prototype for ‘mbb::RPCons mbb::O_StereoConfiguration::stereochemicalPermutations(mbb::uint, mbb::RPLisp)’ does not match any in class ‘mbb::O_StereoConfiguration’
stereochemistry.h:50: error: candidate is: static mbb::RPCons mbb::O_StereoConfiguration::stereochemicalPermutations(mbb::uint)
stereochemistry.cc: In member function ‘mbb::RPCons mbb::O_StereoConfiguration::stereochemicalPermutations(mbb::uint, mbb::RPLisp)’:
stereochemistry.cc:94: error: no matching function for call to ‘mbb::O_String::create(const char [2])’
values.h:85: note: candidates are: static boost::shared_ptr<mbb::O_String> mbb::O_String::create(mbb::RPLisp)
values.h:89: note:                 static mbb::RPString mbb::O_String::create(mbb::RPLisp, const std::string&)
stereochemistry.cc:97: error: no matching function for call to ‘mbb::O_String::create(const char [2])’
values.h:85: note: candidates are: static boost::shared_ptr<mbb::O_String> mbb::O_String::create(mbb::RPLisp)
values.h:89: note:                 static mbb::RPString mbb::O_String::create(mbb::RPLisp, const std::string&)
stereochemistry.cc: In member function ‘virtual void mbb::StereoConfiguration_Exposer::exposeCando()’:
stereochemistry.cc:392: error: no matching function for call to ‘def(const char [27], <unresolved overloaded function type>, mbb::RPLisp)’
stereochemistry.cc:393: error: no matching function for call to ‘def(const char [35], mbb::RPCons (*)(mbb::RPCons, mbb::RPCons, mbb::RPLisp), mbb::RPLisp)’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/stereochemistry.o" "stereochemistry.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/stereochemistry.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/topology.o
In file included from topology.cc:12:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
In file included from topology.cc:14:
plug.h: In member function ‘virtual mbb::RPCons mbb::O_PlugWithMates::matesAsCons()’:
plug.h:199: error: no matching function for call to ‘mbb::List<mbb::O_Mate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_Mate]
plug.h: In member function ‘mbb::RPCons mbb::O_RingClosingPlug::ringClosingMatesAsCons()’:
plug.h:305: error: no matching function for call to ‘mbb::List<mbb::O_RingClosingMate>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_RingClosingMate]
topology.cc: In member function ‘mbb::RPCons mbb::O_Topology::extractFragmentsAsCons()’:
topology.cc:197: error: no matching function for call to ‘mbb::List<mbb::O_ExtractFragment>::asCons()’
holder.h:44: note: candidates are: mbb::RPCons mbb::List<OType>::asCons(mbb::RPLisp) [with OType = mbb::O_ExtractFragment]
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_DirectionalCoupling]’:
coupling.h:118:   instantiated from here
coupling.h:123: error: ‘virtual void mbb::O_DirectionalCoupling::initialize()’ is private
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_RingCoupling]’:
coupling.h:211:   instantiated from here
coupling.h:216: error: ‘virtual void mbb::O_RingCoupling::initialize()’ is private
object.h:232: error: within this context
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/topology.o" "topology.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/topology.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/coordinateArray.o
coordinateArray.cc: In member function ‘virtual void mbb::CoordinateArray_Exposer::exposeCando()’:
coordinateArray.cc:257: error: no matching function for call to ‘mbb::class_<mbb::O_CoordinateArray>::class_()’
wrappers.h:105: note: candidates are: mbb::class_<oclass>::class_(mbb::RPLisp) [with OT = mbb::O_CoordinateArray]
wrappers.h:100: note:                 mbb::class_<mbb::O_CoordinateArray>::class_(const mbb::class_<mbb::O_CoordinateArray>&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/coordinateArray.o" "coordinateArray.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/coordinateArray.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/monomerSet.o
monomerSet.cc:53: error: prototype for ‘mbb::RPMonomerSetBase mbb::O_MonomerSetBase::create2(mbb::RPBuilderDatabase, const std::string&)’ does not match any in class ‘mbb::O_MonomerSetBase’
monomerSet.h:40: error: candidate is: static mbb::RPMonomerSetBase mbb::O_MonomerSetBase::create2(mbb::RPLisp, mbb::RPBuilderDatabase, const std::string&)
monomerSet.cc:444: error: prototype for ‘mbb::RPMonomerSet mbb::O_MonomerSet::create2(mbb::RPBuilderDatabase, const std::string&)’ does not match any in class ‘mbb::O_MonomerSet’
monomerSet.h:165: error: candidate is: static mbb::RPMonomerSet mbb::O_MonomerSet::create2(mbb::RPLisp, mbb::RPBuilderDatabase, const std::string&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/monomerSet.o" "monomerSet.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/monomerSet.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/monomerContext.o
In file included from monomerContext.cc:13:
monomer.h: In member function ‘mbb::RPStringSet mbb::O_Monomer::plugNames()’:
monomer.h:161: error: no matching function for call to ‘mbb::WeakMultiMap<mbb::O_Coupling>::keys()’
holder.h:675: note: candidates are: mbb::RPStringSet mbb::WeakMultiMap<OType>::keys(mbb::RPLisp) [with OType = mbb::O_Coupling]
monomer.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_Monomer]’:
monomer.h:72:   instantiated from here
monomer.h:146: error: ‘virtual void mbb::O_Monomer::initialize()’ is protected
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_DirectionalCoupling]’:
coupling.h:118:   instantiated from here
coupling.h:123: error: ‘virtual void mbb::O_DirectionalCoupling::initialize()’ is private
object.h:232: error: within this context
coupling.h: In function ‘boost::shared_ptr<X> mbb::RP_Create(mbb::RPLisp) [with oClass = mbb::O_RingCoupling]’:
coupling.h:211:   instantiated from here
coupling.h:216: error: ‘virtual void mbb::O_RingCoupling::initialize()’ is private
object.h:232: error: within this context

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/monomerContext.o" "monomerContext.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/monomerContext.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/monomerGroup.o
In file included from monomerGroup.cc:3:
atomIndexer.h: In member function ‘mbb::RPStringSet mbb::O_MapOfMonomerNamesToAtomIndexers::getMonomerNamesAsStringSet()’:
atomIndexer.h:85: error: no matching function for call to ‘mbb::Map<mbb::O_AtomIndexer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_AtomIndexer]

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/monomerGroup.o" "monomerGroup.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/monomerGroup.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/trainer.o
trainer.cc: In member function ‘mbb::RPCons mbb::O_TrainerOrganizer::entriesAsCons()’:
trainer.cc:487: error: no matching function for call to ‘mbb::O_String::create(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
values.h:85: note: candidates are: static boost::shared_ptr<mbb::O_String> mbb::O_String::create(mbb::RPLisp)
values.h:89: note:                 static mbb::RPString mbb::O_String::create(mbb::RPLisp, const std::string&)
trainer.cc:487: error: no matching function for call to ‘mbb::O_Cons::create(boost::shared_ptr<mbb::O_TrainerHeader>&, boost::shared_ptr<mbb::O_Cons>)’
cons.h:24: note: candidates are: static boost::shared_ptr<mbb::O_Cons> mbb::O_Cons::create(mbb::RPLisp)
cons.h:34: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject)
cons.h:38: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons)
trainer.cc: In member function ‘virtual void mbb::Trainer_Exposer::exposeCando()’:
trainer.cc:626: error: call of overloaded ‘def(const char [19], <unresolved overloaded function type>)’ is ambiguous
wrappers.h:22: note: candidates are: void mbb::def(const std::string&, RT (*)(P1)) [with RT = mbb::RPObjectDictionary, P1 = const std::string&]
wrappers.h:32: note:                 void mbb::def(const std::string&, RT (*)(P1, P2)) [with RT = mbb::RPObjectDictionary, P1 = const std::string&, P2 = mbb::RPLisp]
trainer.cc:627: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
trainer.cc: In member function ‘virtual void mbb::TrainerOrganizer_Exposer::exposeCando()’:
trainer.cc:683: error: ‘create_TrainerOrganizer’ was not declared in this scope
trainer.cc:684: error: no matching function for call to ‘def(const char [23], void (*)(mbb::RPTrainerOrganizer), mbb::RPLisp)’
trainer.cc:685: error: no matching function for call to ‘def(const char [22], mbb::RPTrainerOrganizer (*)(mbb::RPLisp), mbb::RPLisp)’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/trainer.o" "trainer.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/trainer.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/boundingBox.o
darwin.compile.c++ bin/darwin-4.2.1/debug/scoreOperations.o
In file included from scoreOperations.cc:6:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/scoreOperations.o" "scoreOperations.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/scoreOperations.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/groupPart.o
darwin.compile.c++ bin/darwin-4.2.1/debug/readAmberParameters.o
In file included from ffStretchDb.h:25,
                 from forceField.h:32,
                 from readAmberParameters.cc:9:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/readAmberParameters.o" "readAmberParameters.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/readAmberParameters.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/cipPrioritizer.o
darwin.compile.c++ bin/darwin-4.2.1/debug/chemdraw.o
In file included from chemdraw.cc:7:
quickDom.h: In static member function ‘static mbb::RPQDomNode mbb::O_QDomNode::open(const std::string&)’:
quickDom.h:118: error: no matching function for call to ‘mbb::O_QDomNode::parseFileName(const std::basic_string<char, std::char_traits<char>, std::allocator<char> >&)’
quickDom.h:117: note: candidates are: static mbb::RPQDomNode mbb::O_QDomNode::parseFileName(const std::string&, mbb::RPLisp)
chemdraw.cc: In member function ‘mbb::RPKeyedObject mbb::O_CDFragment::_asKeyedObject(const std::string&)’:
chemdraw.cc:253: error: no matching function for call to ‘mbb::O_String::create(std::string&)’
values.h:85: note: candidates are: static boost::shared_ptr<mbb::O_String> mbb::O_String::create(mbb::RPLisp)
values.h:89: note:                 static mbb::RPString mbb::O_String::create(mbb::RPLisp, const std::string&)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/chemdraw.o" "chemdraw.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/chemdraw.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/multiScorer.o
In file included from multiScorer.cc:6:
builder.h: In member function ‘mbb::RPStringSet mbb::O_Builder::getOligomerNames()’:
builder.h:184: error: no matching function for call to ‘mbb::Map<mbb::O_Oligomer>::getKeysAsStringSet()’
holder.h:386: note: candidates are: mbb::RPStringSet mbb::Map<OType>::getKeysAsStringSet(mbb::RPLisp) [with OType = mbb::O_Oligomer]

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/multiScorer.o" "multiScorer.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/multiScorer.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/lispParse_obj.o
lispParse.yy: In function ‘int lispparse()’:
lispParse.yy:154: error: no matching function for call to ‘mbb::O_Progn::create()’
./intrinsics.h:202: note: candidates are: static boost::shared_ptr<mbb::O_Progn> mbb::O_Progn::create(mbb::RPLisp)
lispParse.yy:157: error: no matching function for call to ‘mbb::O_Cons::create(mbb::RPObject&, boost::shared_ptr<mbb::O_Cons>&)’
./cons.h:24: note: candidates are: static boost::shared_ptr<mbb::O_Cons> mbb::O_Cons::create(mbb::RPLisp)
./cons.h:34: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject)
./cons.h:38: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons)
lispParse.yy:165: error: no matching function for call to ‘mbb::O_Progn::create()’
./intrinsics.h:202: note: candidates are: static boost::shared_ptr<mbb::O_Progn> mbb::O_Progn::create(mbb::RPLisp)
lispParse.yy:167: error: no matching function for call to ‘mbb::O_Cons::create(mbb::RPObject&, boost::shared_ptr<mbb::O_Cons>)’
./cons.h:24: note: candidates are: static boost::shared_ptr<mbb::O_Cons> mbb::O_Cons::create(mbb::RPLisp)
./cons.h:34: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject)
./cons.h:38: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons)
lispParse.yy:209: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPCons&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:224: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPCons&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:241: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPCons&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:252: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPCons&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:265: error: no matching function for call to ‘mbb::O_ParsingCons::create(boost::shared_ptr<mbb::O_Cons>&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:274: error: no matching function for call to ‘mbb::O_ParsingCons::create(boost::shared_ptr<mbb::O_Object>, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:285: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPObject, boost::shared_ptr<mbb::O_Cons>&, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:290: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPObject, mbb::RPCons&, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:296: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPCons&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:307: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPObject, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:312: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPObject, mbb::RPCons&, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:318: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPCons&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:327: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPObject, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:384: error: no matching function for call to ‘mbb::O_KeyedObject::create(char [256], mbb::RPObject)’
./keyedObject.h:17: note: candidates are: static boost::shared_ptr<mbb::O_KeyedObject> mbb::O_KeyedObject::create(mbb::RPLisp)
./keyedObject.h:26: note:                 static mbb::RPKeyedObject mbb::O_KeyedObject::create(mbb::RPLisp, const std::string&, mbb::RPObject)
lispParse.yy:392: error: no matching function for call to ‘mbb::O_Quote::create()’
./intrinsics.h:425: note: candidates are: static boost::shared_ptr<mbb::O_Quote> mbb::O_Quote::create(mbb::RPLisp)
lispParse.yy:396: error: no matching function for call to ‘mbb::O_Cons::create(mbb::RPSymbol&, boost::shared_ptr<mbb::O_Cons>)’
./cons.h:24: note: candidates are: static boost::shared_ptr<mbb::O_Cons> mbb::O_Cons::create(mbb::RPLisp)
./cons.h:34: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject)
./cons.h:38: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons)
lispParse.yy:403: error: no matching function for call to ‘mbb::O_Quote::create()’
./intrinsics.h:425: note: candidates are: static boost::shared_ptr<mbb::O_Quote> mbb::O_Quote::create(mbb::RPLisp)
lispParse.yy:405: error: no matching function for call to ‘mbb::O_Cons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>&)’
./cons.h:24: note: candidates are: static boost::shared_ptr<mbb::O_Cons> mbb::O_Cons::create(mbb::RPLisp)
./cons.h:34: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject)
./cons.h:38: note:                 static mbb::RPCons mbb::O_Cons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons)
lispParse.yy:416: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPSymbol&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:421: error: no matching function for call to ‘mbb::O_Int::create(int&)’
./values.h:129: note: candidates are: static boost::shared_ptr<mbb::O_Int> mbb::O_Int::create(mbb::RPLisp)
./values.h:142: note:                 static mbb::RPInt mbb::O_Int::create(mbb::RPLisp, int)
lispParse.yy:427: error: no matching function for call to ‘mbb::O_LongLongInt::create(mbb::LongLongInt&)’
./values.h:184: note: candidates are: static boost::shared_ptr<mbb::O_LongLongInt> mbb::O_LongLongInt::create(mbb::RPLisp)
./values.h:190: note:                 static mbb::RPLongLongInt mbb::O_LongLongInt::create(mbb::RPLisp, mbb::LongLongInt)
lispParse.yy:433: error: no matching function for call to ‘mbb::O_Real::create(double&)’
./values.h:232: note: candidates are: static boost::shared_ptr<mbb::O_Real> mbb::O_Real::create(mbb::RPLisp)
./values.h:238: note:                 static mbb::RPReal mbb::O_Real::create(mbb::RPLisp, double)
lispParse.yy:439: error: no matching function for call to ‘mbb::O_String::create(char [256])’
./values.h:85: note: candidates are: static boost::shared_ptr<mbb::O_String> mbb::O_String::create(mbb::RPLisp)
./values.h:89: note:                 static mbb::RPString mbb::O_String::create(mbb::RPLisp, const std::string&)
lispParse.yy:445: error: no matching function for call to ‘mbb::O_Return::create()’
./intrinsics.h:140: note: candidates are: static boost::shared_ptr<mbb::O_Return> mbb::O_Return::create(mbb::RPLisp)
lispParse.yy:447: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:452: error: no matching function for call to ‘mbb::O_If::create()’
./intrinsics.h:63: note: candidates are: static boost::shared_ptr<mbb::O_If> mbb::O_If::create(mbb::RPLisp)
lispParse.yy:454: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:459: error: no matching function for call to ‘mbb::O_IfTrue::create()’
./intrinsics.h:77: note: candidates are: static boost::shared_ptr<mbb::O_IfTrue> mbb::O_IfTrue::create(mbb::RPLisp)
lispParse.yy:461: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:466: error: no matching function for call to ‘mbb::O_IfFalse::create()’
./intrinsics.h:92: note: candidates are: static boost::shared_ptr<mbb::O_IfFalse> mbb::O_IfFalse::create(mbb::RPLisp)
lispParse.yy:468: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:473: error: no matching function for call to ‘mbb::O_Cond::create()’
./intrinsics.h:117: note: candidates are: static boost::shared_ptr<mbb::O_Cond> mbb::O_Cond::create(mbb::RPLisp)
lispParse.yy:475: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:480: error: no matching function for call to ‘mbb::O_Lambda::create()’
./intrinsics.h:410: note: candidates are: static boost::shared_ptr<mbb::O_Lambda> mbb::O_Lambda::create(mbb::RPLisp)
lispParse.yy:482: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:487: error: no matching function for call to ‘mbb::O_Progn::create()’
./intrinsics.h:202: note: candidates are: static boost::shared_ptr<mbb::O_Progn> mbb::O_Progn::create(mbb::RPLisp)
lispParse.yy:489: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:494: error: no matching function for call to ‘mbb::O_ForEach::create()’
./intrinsics.h:391: note: candidates are: static boost::shared_ptr<mbb::O_ForEach> mbb::O_ForEach::create(mbb::RPLisp)
lispParse.yy:496: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:501: error: no matching function for call to ‘mbb::O_While::create()’
./intrinsics.h:374: note: candidates are: static boost::shared_ptr<mbb::O_While> mbb::O_While::create(mbb::RPLisp)
lispParse.yy:503: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:508: error: no matching function for call to ‘mbb::O_Invoke::create()’
./intrinsics.h:48: note: candidates are: static boost::shared_ptr<mbb::O_Invoke> mbb::O_Invoke::create(mbb::RPLisp)
lispParse.yy:510: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:515: error: no matching function for call to ‘mbb::O_Quote::create()’
./intrinsics.h:425: note: candidates are: static boost::shared_ptr<mbb::O_Quote> mbb::O_Quote::create(mbb::RPLisp)
lispParse.yy:517: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:522: error: ‘O_SetGlobal’ has not been declared
lispParse.yy:524: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:529: error: no matching function for call to ‘mbb::O_SetLocal::create()’
./intrinsics.h:457: note: candidates are: static boost::shared_ptr<mbb::O_SetLocal> mbb::O_SetLocal::create(mbb::RPLisp)
lispParse.yy:531: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:536: error: no matching function for call to ‘mbb::O_DefineClass::create()’
./intrinsics.h:473: note: candidates are: static boost::shared_ptr<mbb::O_DefineClass> mbb::O_DefineClass::create(mbb::RPLisp)
lispParse.yy:538: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:543: error: no matching function for call to ‘mbb::O_DefineFunction::create()’
./intrinsics.h:504: note: candidates are: static boost::shared_ptr<mbb::O_DefineFunction> mbb::O_DefineFunction::create(mbb::RPLisp)
lispParse.yy:545: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:550: error: no matching function for call to ‘mbb::O_MacroExecutable::create(mbb::MacroHolder*&)’
./intrinsics.h:552: note: candidates are: static boost::shared_ptr<mbb::O_MacroExecutable> mbb::O_MacroExecutable::create(mbb::RPLisp)
./intrinsics.h:556: note:                 static mbb::RPMacroExecutable mbb::O_MacroExecutable::create(mbb::RPLisp, mbb::MacroHolder*)
lispParse.yy:556: error: no matching function for call to ‘mbb::O_DefineMethod::create()’
./intrinsics.h:489: note: candidates are: static boost::shared_ptr<mbb::O_DefineMethod> mbb::O_DefineMethod::create(mbb::RPLisp)
lispParse.yy:558: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:563: error: no matching function for call to ‘mbb::O_PrognDebug::create(std::string&, int&, int&)’
./intrinsics.h:273: note: candidates are: static boost::shared_ptr<mbb::O_PrognDebug> mbb::O_PrognDebug::create(mbb::RPLisp)
./intrinsics.h:275: note:                 static mbb::RPPrognDebug mbb::O_PrognDebug::create(mbb::RPLisp, const std::string&, int, int)
lispParse.yy:565: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:571: error: no matching function for call to ‘mbb::O_PrognLog::create(std::string&, int&, int&)’
./intrinsics.h:302: note: candidates are: static boost::shared_ptr<mbb::O_PrognLog> mbb::O_PrognLog::create(mbb::RPLisp)
./intrinsics.h:304: note:                 static mbb::RPPrognLog mbb::O_PrognLog::create(mbb::RPLisp, const std::string&, int, int)
lispParse.yy:573: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:579: error: no matching function for call to ‘mbb::O_Assert::create(std::string&, int&, int&)’
./intrinsics.h:348: note: candidates are: static boost::shared_ptr<mbb::O_Assert> mbb::O_Assert::create(mbb::RPLisp)
./intrinsics.h:350: note:                 static mbb::RPAssert mbb::O_Assert::create(mbb::RPLisp, const std::string&, int, int)
lispParse.yy:581: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:587: error: no matching function for call to ‘mbb::O_Log::create(std::string&, int&, int&)’
./intrinsics.h:325: note: candidates are: static boost::shared_ptr<mbb::O_Log> mbb::O_Log::create(mbb::RPLisp)
./intrinsics.h:327: note:                 static mbb::RPLog mbb::O_Log::create(mbb::RPLisp, const std::string&, int, int)
lispParse.yy:589: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:594: error: no matching function for call to ‘mbb::O_Break::create()’
./intrinsics.h:184: note: candidates are: static boost::shared_ptr<mbb::O_Break> mbb::O_Break::create(mbb::RPLisp)
lispParse.yy:596: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:601: error: no matching function for call to ‘mbb::O_Continue::create()’
./intrinsics.h:160: note: candidates are: static boost::shared_ptr<mbb::O_Continue> mbb::O_Continue::create(mbb::RPLisp)
lispParse.yy:603: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:608: error: no matching function for call to ‘mbb::O_Raise::create(std::string&, int&, int&)’
./intrinsics.h:245: note: candidates are: static boost::shared_ptr<mbb::O_Raise> mbb::O_Raise::create(mbb::RPLisp)
./intrinsics.h:247: note:                 static mbb::RPRaise mbb::O_Raise::create(mbb::RPLisp, const std::string&, int, int)
lispParse.yy:610: error: no matching function for call to ‘mbb::O_ParsingCons::create(mbb::RPIntrinsic&, boost::shared_ptr<mbb::O_Cons>, int&, int&, std::string&)’
./cons.h:242: note: candidates are: static boost::shared_ptr<mbb::O_ParsingCons> mbb::O_ParsingCons::create(mbb::RPLisp)
./cons.h:253: note:                 static mbb::RPParsingCons mbb::O_ParsingCons::create(mbb::RPLisp, mbb::RPObject, mbb::RPCons, int, int, const std::string&)
lispParse.yy:615: error: no matching function for call to ‘mbb::O_Bool::create(bool)’
./values.h:265: note: candidates are: static boost::shared_ptr<mbb::O_Bool> mbb::O_Bool::create(mbb::RPLisp)
./values.h:271: note:                 static mbb::RPBool mbb::O_Bool::create(mbb::RPLisp, bool)
lispParse.yy:621: error: no matching function for call to ‘mbb::O_Bool::create(bool)’
./values.h:265: note: candidates are: static boost::shared_ptr<mbb::O_Bool> mbb::O_Bool::create(mbb::RPLisp)
./values.h:271: note:                 static mbb::RPBool mbb::O_Bool::create(mbb::RPLisp, bool)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -I"bin/darwin-4.2.1/debug" -c -o "bin/darwin-4.2.1/debug/lispParse_obj.o" "bin/darwin-4.2.1/debug/lispParse_obj.cpp"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/lispParse_obj.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/msmartsParse_obj.o
msmartsParse.yy: In function ‘int msmartsparse()’:
msmartsParse.yy:213: error: no matching function for call to ‘mbb::Hold<mbb::O_SmartsRoot>::Hold()’
./hold.h:17: note: candidates are: mbb::Hold<Dumb_Class>::Hold(boost::shared_ptr<X>) [with Dumb_Class = mbb::O_SmartsRoot]
./hold.h:13: note:                 mbb::Hold<mbb::O_SmartsRoot>::Hold(const mbb::Hold<mbb::O_SmartsRoot>&)
msmartsParse.yy:216: error: no matching function for call to ‘mbb::O_SmartsRoot::create(boost::shared_ptr<mbb::O_AtomOrBondMatchNode>&)’
./chemInfo.h:771: note: candidates are: static boost::shared_ptr<mbb::O_SmartsRoot> mbb::O_SmartsRoot::create(mbb::RPLisp)
./chemInfo.h:778: note:                 static mbb::RPSmartsRoot mbb::O_SmartsRoot::create(mbb::RPLisp, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:787: note:                 static mbb::RPSmartsRoot mbb::O_SmartsRoot::create(mbb::RPLisp, mbb::RPAtomOrBondMatchNode)
msmartsParse.yy:220: error: no matching function for call to ‘mbb::O_SmartsRoot::create(boost::shared_ptr<mbb::O_AtomOrBondMatchNode>&, boost::shared_ptr<mbb::O_BondListMatchNode>&)’
./chemInfo.h:771: note: candidates are: static boost::shared_ptr<mbb::O_SmartsRoot> mbb::O_SmartsRoot::create(mbb::RPLisp)
./chemInfo.h:778: note:                 static mbb::RPSmartsRoot mbb::O_SmartsRoot::create(mbb::RPLisp, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:787: note:                 static mbb::RPSmartsRoot mbb::O_SmartsRoot::create(mbb::RPLisp, mbb::RPAtomOrBondMatchNode)
msmartsParse.yy:227: error: no matching function for call to ‘mbb::O_Chain::create(boost::shared_ptr<mbb::O_BondTest>&, boost::shared_ptr<mbb::O_BondListMatchNode>&)’
./chemInfo.h:596: note: candidates are: static boost::shared_ptr<mbb::O_Chain> mbb::O_Chain::create(mbb::RPLisp)
./chemInfo.h:606: note:                 static mbb::RPChain mbb::O_Chain::create(mbb::RPLisp, mbb::RPBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:614: note:                 static mbb::RPChain mbb::O_Chain::create(mbb::RPLisp, mbb::RPBondMatchNode)
msmartsParse.yy:231: error: no matching function for call to ‘mbb::O_Branch::create(boost::shared_ptr<mbb::O_BondListMatchNode>&, boost::shared_ptr<mbb::O_BondListMatchNode>&)’
./chemInfo.h:632: note: candidates are: static boost::shared_ptr<mbb::O_Branch> mbb::O_Branch::create(mbb::RPLisp)
./chemInfo.h:641: note:                 static mbb::RPBranch mbb::O_Branch::create(mbb::RPLisp, mbb::RPBondListMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:650: note:                 static mbb::RPBranch mbb::O_Branch::create(mbb::RPLisp, mbb::RPBondListMatchNode)
msmartsParse.yy:235: error: no matching function for call to ‘mbb::O_Chain::create(boost::shared_ptr<mbb::O_BondTest>&)’
./chemInfo.h:596: note: candidates are: static boost::shared_ptr<mbb::O_Chain> mbb::O_Chain::create(mbb::RPLisp)
./chemInfo.h:606: note:                 static mbb::RPChain mbb::O_Chain::create(mbb::RPLisp, mbb::RPBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:614: note:                 static mbb::RPChain mbb::O_Chain::create(mbb::RPLisp, mbb::RPBondMatchNode)
msmartsParse.yy:239: error: no matching function for call to ‘mbb::O_Branch::create(boost::shared_ptr<mbb::O_BondListMatchNode>&)’
./chemInfo.h:632: note: candidates are: static boost::shared_ptr<mbb::O_Branch> mbb::O_Branch::create(mbb::RPLisp)
./chemInfo.h:641: note:                 static mbb::RPBranch mbb::O_Branch::create(mbb::RPLisp, mbb::RPBondListMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:650: note:                 static mbb::RPBranch mbb::O_Branch::create(mbb::RPLisp, mbb::RPBondListMatchNode)
msmartsParse.yy:245: error: no matching function for call to ‘mbb::O_BondTest::create(mbb::BondEnum&, boost::shared_ptr<mbb::O_AtomOrBondMatchNode>&)’
./chemInfo.h:354: note: candidates are: static boost::shared_ptr<mbb::O_BondTest> mbb::O_BondTest::create(mbb::RPLisp)
./chemInfo.h:363: note:                 static mbb::RPBondTest mbb::O_BondTest::create(mbb::RPLisp, mbb::BondEnum, mbb::RPAtomOrBondMatchNode)
msmartsParse.yy:249: error: no matching function for call to ‘mbb::O_BondTest::create(mbb::BondEnum, boost::shared_ptr<mbb::O_AtomOrBondMatchNode>&)’
./chemInfo.h:354: note: candidates are: static boost::shared_ptr<mbb::O_BondTest> mbb::O_BondTest::create(mbb::RPLisp)
./chemInfo.h:363: note:                 static mbb::RPBondTest mbb::O_BondTest::create(mbb::RPLisp, mbb::BondEnum, mbb::RPAtomOrBondMatchNode)
msmartsParse.yy:274: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int, int, const char*&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:287: error: no matching function for call to ‘mbb::O_TagSet::create(mbb::BondEnum, boost::shared_ptr<mbb::O_AtomOrBondMatchNode>&, char [16])’
./chemInfo.h:255: note: candidates are: static boost::shared_ptr<mbb::O_TagSet> mbb::O_TagSet::create(mbb::RPLisp)
./chemInfo.h:264: note:                 static mbb::RPTagSet mbb::O_TagSet::create(mbb::RPLisp, mbb::BondEnum, mbb::RPAtomOrBondMatchNode, const std::string&)
msmartsParse.yy:317: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:328: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:333: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:345: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:350: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:361: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:367: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:378: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, char [16])’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:389: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, char [16])’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:400: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:404: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:415: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:419: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:428: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:432: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:442: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:446: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:452: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:456: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:460: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:464: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:477: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:481: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:491: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:500: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:516: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:520: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:529: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:538: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:542: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:546: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:550: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:559: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:563: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:567: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:576: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:580: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, const char*&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:584: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, const char*&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:588: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:592: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
msmartsParse.yy:679: error: no matching function for call to ‘mbb::O_Logical::create(mbb::LogicalOperatorType, boost::shared_ptr<mbb::O_AtomOrBondMatchNode>&)’
./chemInfo.h:215: note: candidates are: static boost::shared_ptr<mbb::O_Logical> mbb::O_Logical::create(mbb::RPLisp)
./chemInfo.h:226: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:235: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode)
msmartsParse.yy:689: error: no matching function for call to ‘mbb::O_Logical::create(mbb::LogicalOperatorType, boost::shared_ptr<mbb::O_Logical>&)’
./chemInfo.h:215: note: candidates are: static boost::shared_ptr<mbb::O_Logical> mbb::O_Logical::create(mbb::RPLisp)
./chemInfo.h:226: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:235: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode)
msmartsParse.yy:699: error: no matching function for call to ‘mbb::O_Logical::create(mbb::LogicalOperatorType, boost::shared_ptr<mbb::O_Logical>&, boost::shared_ptr<mbb::O_Logical>&)’
./chemInfo.h:215: note: candidates are: static boost::shared_ptr<mbb::O_Logical> mbb::O_Logical::create(mbb::RPLisp)
./chemInfo.h:226: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:235: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode)
msmartsParse.yy:709: error: no matching function for call to ‘mbb::O_Logical::create(mbb::LogicalOperatorType, boost::shared_ptr<mbb::O_Logical>&, boost::shared_ptr<mbb::O_Logical>&)’
./chemInfo.h:215: note: candidates are: static boost::shared_ptr<mbb::O_Logical> mbb::O_Logical::create(mbb::RPLisp)
./chemInfo.h:226: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:235: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode)
msmartsParse.yy:721: error: no matching function for call to ‘mbb::O_Logical::create(mbb::LogicalOperatorType, boost::shared_ptr<mbb::O_Logical>&, boost::shared_ptr<mbb::O_Logical>&)’
./chemInfo.h:215: note: candidates are: static boost::shared_ptr<mbb::O_Logical> mbb::O_Logical::create(mbb::RPLisp)
./chemInfo.h:226: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:235: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode)
msmartsParse.yy: In function ‘mbb::RPSmartsRoot smartsCompile(const std::string&)’:
msmartsParse.yy:1195: error: no matching function for call to ‘mbb::O_SmartsRoot::nil()’
./chemInfo.h:771: note: candidates are: static boost::shared_ptr<mbb::O_SmartsRoot> mbb::O_SmartsRoot::nil(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -I"bin/darwin-4.2.1/debug" -c -o "bin/darwin-4.2.1/debug/msmartsParse_obj.o" "bin/darwin-4.2.1/debug/msmartsParse_obj.cpp"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/msmartsParse_obj.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/gaffParse_obj.o
gaffParse.yy: In function ‘int antechamber_parse()’:
gaffParse.yy:164: error: no matching function for call to ‘mbb::O_AntechamberFocusAtomMatch::create(boost::shared_ptr<mbb::O_ResidueList>&, int&, int&, int&, int&, boost::shared_ptr<mbb::O_Logical>&)’
./chemInfo.h:535: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberFocusAtomMatch> mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp)
./chemInfo.h:554: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:572: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int)
gaffParse.yy:165: error: no matching function for call to ‘mbb::O_AntechamberRoot::create(char [255], mbb::RPAntechamberFocusAtomMatch&, boost::shared_ptr<mbb::O_BondListMatchNode>&, boost::shared_ptr<mbb::O_AfterMatchBondTest>&)’
./chemInfo.h:818: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberRoot> mbb::O_AntechamberRoot::create(mbb::RPLisp)
./chemInfo.h:832: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode, mbb::RPRootMatchNode)
./chemInfo.h:849: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:855: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode)
gaffParse.yy:180: error: no matching function for call to ‘mbb::O_AntechamberFocusAtomMatch::create(boost::shared_ptr<mbb::O_ResidueList>&, int&, int&, int&, int&, boost::shared_ptr<mbb::O_Logical>&)’
./chemInfo.h:535: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberFocusAtomMatch> mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp)
./chemInfo.h:554: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:572: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int)
gaffParse.yy:181: error: no matching function for call to ‘mbb::O_AntechamberRoot::create(char [255], mbb::RPAntechamberFocusAtomMatch&, boost::shared_ptr<mbb::O_BondListMatchNode>&)’
./chemInfo.h:818: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberRoot> mbb::O_AntechamberRoot::create(mbb::RPLisp)
./chemInfo.h:832: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode, mbb::RPRootMatchNode)
./chemInfo.h:849: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:855: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode)
gaffParse.yy:195: error: no matching function for call to ‘mbb::O_AntechamberFocusAtomMatch::create(boost::shared_ptr<mbb::O_ResidueList>&, int&, int&, int&, int&, boost::shared_ptr<mbb::O_Logical>&)’
./chemInfo.h:535: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberFocusAtomMatch> mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp)
./chemInfo.h:554: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:572: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int)
gaffParse.yy:196: error: no matching function for call to ‘mbb::O_AntechamberRoot::create(char [255], mbb::RPAntechamberFocusAtomMatch&)’
./chemInfo.h:818: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberRoot> mbb::O_AntechamberRoot::create(mbb::RPLisp)
./chemInfo.h:832: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode, mbb::RPRootMatchNode)
./chemInfo.h:849: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:855: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode)
gaffParse.yy:209: error: no matching function for call to ‘mbb::O_AtomOrBondMatchNode::nil()’
./chemInfo.h:182: note: candidates are: static boost::shared_ptr<mbb::O_AtomOrBondMatchNode> mbb::O_AtomOrBondMatchNode::nil(mbb::RPLisp)
gaffParse.yy:210: error: no matching function for call to ‘mbb::O_AntechamberFocusAtomMatch::create(boost::shared_ptr<mbb::O_ResidueList>&, int&, int&, int&, int&, mbb::RPAtomOrBondMatchNode&)’
./chemInfo.h:535: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberFocusAtomMatch> mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp)
./chemInfo.h:554: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:572: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int)
gaffParse.yy:211: error: no matching function for call to ‘mbb::O_AntechamberRoot::create(char [255], mbb::RPAntechamberFocusAtomMatch&)’
./chemInfo.h:818: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberRoot> mbb::O_AntechamberRoot::create(mbb::RPLisp)
./chemInfo.h:832: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode, mbb::RPRootMatchNode)
./chemInfo.h:849: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:855: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode)
gaffParse.yy:222: error: no matching function for call to ‘mbb::O_AntechamberFocusAtomMatch::create(boost::shared_ptr<mbb::O_ResidueList>&, int&, int&, int&, int)’
./chemInfo.h:535: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberFocusAtomMatch> mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp)
./chemInfo.h:554: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:572: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int)
gaffParse.yy:223: error: no matching function for call to ‘mbb::O_AntechamberRoot::create(char [255], mbb::RPAntechamberFocusAtomMatch&)’
./chemInfo.h:818: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberRoot> mbb::O_AntechamberRoot::create(mbb::RPLisp)
./chemInfo.h:832: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode, mbb::RPRootMatchNode)
./chemInfo.h:849: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:855: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode)
gaffParse.yy:234: error: no matching function for call to ‘mbb::O_AntechamberFocusAtomMatch::create(boost::shared_ptr<mbb::O_ResidueList>&, int&, int&, int, int)’
./chemInfo.h:535: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberFocusAtomMatch> mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp)
./chemInfo.h:554: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:572: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int)
gaffParse.yy:236: error: no matching function for call to ‘mbb::O_AntechamberRoot::create(char [255], mbb::RPAntechamberFocusAtomMatch&)’
./chemInfo.h:818: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberRoot> mbb::O_AntechamberRoot::create(mbb::RPLisp)
./chemInfo.h:832: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode, mbb::RPRootMatchNode)
./chemInfo.h:849: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:855: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode)
gaffParse.yy:245: error: no matching function for call to ‘mbb::O_AntechamberFocusAtomMatch::create(boost::shared_ptr<mbb::O_ResidueList>&, int&, int, int, int)’
./chemInfo.h:535: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberFocusAtomMatch> mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp)
./chemInfo.h:554: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:572: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int)
gaffParse.yy:246: error: no matching function for call to ‘mbb::O_AntechamberRoot::create(char [255], mbb::RPAntechamberFocusAtomMatch&)’
./chemInfo.h:818: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberRoot> mbb::O_AntechamberRoot::create(mbb::RPLisp)
./chemInfo.h:832: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode, mbb::RPRootMatchNode)
./chemInfo.h:849: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:855: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode)
gaffParse.yy:254: error: no matching function for call to ‘mbb::O_AntechamberFocusAtomMatch::create(boost::shared_ptr<mbb::O_ResidueList>&, int, int, int, int)’
./chemInfo.h:535: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberFocusAtomMatch> mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp)
./chemInfo.h:554: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:572: note:                 static mbb::RPAntechamberFocusAtomMatch mbb::O_AntechamberFocusAtomMatch::create(mbb::RPLisp, mbb::RPResidueList, int, int, int, int)
gaffParse.yy:255: error: no matching function for call to ‘mbb::O_AntechamberRoot::create(char [255], mbb::RPAntechamberFocusAtomMatch&)’
./chemInfo.h:818: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberRoot> mbb::O_AntechamberRoot::create(mbb::RPLisp)
./chemInfo.h:832: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode, mbb::RPRootMatchNode)
./chemInfo.h:849: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:855: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode)
gaffParse.yy:262: error: no matching function for call to ‘mbb::O_AntechamberFocusAtomMatch::nil()’
./chemInfo.h:535: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberFocusAtomMatch> mbb::O_AntechamberFocusAtomMatch::nil(mbb::RPLisp)
gaffParse.yy:263: error: no matching function for call to ‘mbb::O_AntechamberRoot::create(char [255], mbb::RPAntechamberFocusAtomMatch&)’
./chemInfo.h:818: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberRoot> mbb::O_AntechamberRoot::create(mbb::RPLisp)
./chemInfo.h:832: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode, mbb::RPRootMatchNode)
./chemInfo.h:849: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:855: note:                 static mbb::RPAntechamberRoot mbb::O_AntechamberRoot::create(mbb::RPLisp, std::string, mbb::RPAtomOrBondMatchNode)
gaffParse.yy:283: error: no matching function for call to ‘mbb::Hold<mbb::O_ResidueList>::Hold()’
./hold.h:17: note: candidates are: mbb::Hold<Dumb_Class>::Hold(boost::shared_ptr<X>) [with Dumb_Class = mbb::O_ResidueList]
./hold.h:13: note:                 mbb::Hold<mbb::O_ResidueList>::Hold(const mbb::Hold<mbb::O_ResidueList>&)
gaffParse.yy:308: error: no matching function for call to ‘mbb::Hold<mbb::O_Logical>::Hold()’
./hold.h:17: note: candidates are: mbb::Hold<Dumb_Class>::Hold(boost::shared_ptr<X>) [with Dumb_Class = mbb::O_Logical]
./hold.h:13: note:                 mbb::Hold<mbb::O_Logical>::Hold(const mbb::Hold<mbb::O_Logical>&)
gaffParse.yy:316: error: no matching function for call to ‘mbb::Hold<mbb::O_BondListMatchNode>::Hold()’
./hold.h:17: note: candidates are: mbb::Hold<Dumb_Class>::Hold(boost::shared_ptr<X>) [with Dumb_Class = mbb::O_BondListMatchNode]
./hold.h:13: note:                 mbb::Hold<mbb::O_BondListMatchNode>::Hold(const mbb::Hold<mbb::O_BondListMatchNode>&)
gaffParse.yy:323: error: no matching function for call to ‘mbb::O_AfterMatchBondTest::create(char [255], char [255], mbb::BondEnum&)’
./chemInfo.h:688: note: candidates are: static boost::shared_ptr<mbb::O_AfterMatchBondTest> mbb::O_AfterMatchBondTest::create(mbb::RPLisp)
./chemInfo.h:696: note:                 static mbb::RPAfterMatchBondTest mbb::O_AfterMatchBondTest::create(mbb::RPLisp, const std::string&, const std::string&, mbb::BondEnum)
gaffParse.yy:327: error: no matching function for call to ‘mbb::O_Logical::create(mbb::LogicalOperatorType, boost::shared_ptr<mbb::O_AtomTest>&)’
./chemInfo.h:215: note: candidates are: static boost::shared_ptr<mbb::O_Logical> mbb::O_Logical::create(mbb::RPLisp)
./chemInfo.h:226: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:235: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode)
gaffParse.yy:328: error: no matching function for call to ‘mbb::O_Logical::create(mbb::LogicalOperatorType, boost::shared_ptr<mbb::O_Logical>&, boost::shared_ptr<mbb::O_Logical>&)’
./chemInfo.h:215: note: candidates are: static boost::shared_ptr<mbb::O_Logical> mbb::O_Logical::create(mbb::RPLisp)
./chemInfo.h:226: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:235: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode)
gaffParse.yy:329: error: no matching function for call to ‘mbb::O_Logical::create(mbb::LogicalOperatorType, boost::shared_ptr<mbb::O_Logical>&, boost::shared_ptr<mbb::O_Logical>&)’
./chemInfo.h:215: note: candidates are: static boost::shared_ptr<mbb::O_Logical> mbb::O_Logical::create(mbb::RPLisp)
./chemInfo.h:226: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode, mbb::RPAtomOrBondMatchNode)
./chemInfo.h:235: note:                 static mbb::RPLogical mbb::O_Logical::create(mbb::RPLisp, mbb::LogicalOperatorType, mbb::RPAtomOrBondMatchNode)
gaffParse.yy:332: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, mbb::BondEnum&, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
gaffParse.yy:333: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, mbb::BondEnum&, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
gaffParse.yy:334: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, mbb::BondEnum&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
gaffParse.yy:335: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, mbb::BondEnum&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
gaffParse.yy:336: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
gaffParse.yy:337: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
gaffParse.yy:338: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
gaffParse.yy:339: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum, int&, int&)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
gaffParse.yy:340: error: no matching function for call to ‘mbb::O_AtomTest::create(mbb::AtomTestEnum)’
./chemInfo.h:469: note: candidates are: static boost::shared_ptr<mbb::O_AtomTest> mbb::O_AtomTest::create(mbb::RPLisp)
./chemInfo.h:478: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int, const char*)
./chemInfo.h:491: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum)
./chemInfo.h:495: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int)
./chemInfo.h:499: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, int, int)
./chemInfo.h:503: note:                 static mbb::RPAtomTest mbb::O_AtomTest::create(mbb::RPLisp, mbb::AtomTestEnum, const char*)
gaffParse.yy:348: error: no matching function for call to ‘mbb::O_Branch::create(boost::shared_ptr<mbb::O_BondListMatchNode>&, boost::shared_ptr<mbb::O_BondListMatchNode>&)’
./chemInfo.h:632: note: candidates are: static boost::shared_ptr<mbb::O_Branch> mbb::O_Branch::create(mbb::RPLisp)
./chemInfo.h:641: note:                 static mbb::RPBranch mbb::O_Branch::create(mbb::RPLisp, mbb::RPBondListMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:650: note:                 static mbb::RPBranch mbb::O_Branch::create(mbb::RPLisp, mbb::RPBondListMatchNode)
gaffParse.yy:352: error: no matching function for call to ‘mbb::O_Chain::create(boost::shared_ptr<mbb::O_AntechamberBondTest>&)’
./chemInfo.h:596: note: candidates are: static boost::shared_ptr<mbb::O_Chain> mbb::O_Chain::create(mbb::RPLisp)
./chemInfo.h:606: note:                 static mbb::RPChain mbb::O_Chain::create(mbb::RPLisp, mbb::RPBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:614: note:                 static mbb::RPChain mbb::O_Chain::create(mbb::RPLisp, mbb::RPBondMatchNode)
gaffParse.yy:353: error: no matching function for call to ‘mbb::O_Chain::create(boost::shared_ptr<mbb::O_AntechamberBondTest>&, boost::shared_ptr<mbb::O_BondListMatchNode>&)’
./chemInfo.h:596: note: candidates are: static boost::shared_ptr<mbb::O_Chain> mbb::O_Chain::create(mbb::RPLisp)
./chemInfo.h:606: note:                 static mbb::RPChain mbb::O_Chain::create(mbb::RPLisp, mbb::RPBondMatchNode, mbb::RPBondListMatchNode)
./chemInfo.h:614: note:                 static mbb::RPChain mbb::O_Chain::create(mbb::RPLisp, mbb::RPBondMatchNode)
gaffParse.yy:362: error: no matching function for call to ‘mbb::O_AntechamberBondTest::create(char [255], int&, boost::shared_ptr<mbb::O_Logical>&, char [255])’
./chemInfo.h:387: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberBondTest> mbb::O_AntechamberBondTest::create(mbb::RPLisp)
./chemInfo.h:398: note:                 static mbb::RPAntechamberBondTest mbb::O_AntechamberBondTest::create(mbb::RPLisp, std::string, int, mbb::RPAtomOrBondMatchNode, std::string)
gaffParse.yy:367: error: no matching function for call to ‘mbb::O_AntechamberBondTest::create(char [255], int, boost::shared_ptr<mbb::O_Logical>&, char [255])’
./chemInfo.h:387: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberBondTest> mbb::O_AntechamberBondTest::create(mbb::RPLisp)
./chemInfo.h:398: note:                 static mbb::RPAntechamberBondTest mbb::O_AntechamberBondTest::create(mbb::RPLisp, std::string, int, mbb::RPAtomOrBondMatchNode, std::string)
gaffParse.yy:376: error: no matching function for call to ‘mbb::Hold<mbb::O_Logical>::Hold()’
./hold.h:17: note: candidates are: mbb::Hold<Dumb_Class>::Hold(boost::shared_ptr<X>) [with Dumb_Class = mbb::O_Logical]
./hold.h:13: note:                 mbb::Hold<mbb::O_Logical>::Hold(const mbb::Hold<mbb::O_Logical>&)
gaffParse.yy: In function ‘mbb::RPAntechamberRoot antechamberCompile(const std::string&, mbb::RPWildElementDict)’:
gaffParse.yy:781: error: no matching function for call to ‘mbb::O_AntechamberRoot::nil()’
./chemInfo.h:818: note: candidates are: static boost::shared_ptr<mbb::O_AntechamberRoot> mbb::O_AntechamberRoot::nil(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -I"bin/darwin-4.2.1/debug" -c -o "bin/darwin-4.2.1/debug/gaffParse_obj.o" "bin/darwin-4.2.1/debug/gaffParse_obj.cpp"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/gaffParse_obj.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/lex.o
In file included from lispLex.ll:18:
lispParse.yy:118: error: ‘LongLongInt’ does not name a type
lispLex.ll:138: error: ‘LongLongInt’ has not been declared
lispLex.ll: In function ‘void lexTraceArg(const char*, int)’:
lispLex.ll:138: error: redefinition of ‘void lexTraceArg(const char*, int)’
lispLex.ll:132: error: ‘void lexTraceArg(const char*, int)’ previously defined here
lispLex.ll: In function ‘int lisplex()’:
lispLex.ll:411: error: ‘union YYSTYPE’ has no member named ‘llval’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/lex.o" "bin/darwin-4.2.1/debug/lex.cpp"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/lex.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/main.o
main.cc: In function ‘int main(int, char**)’:
main.cc:179: error: ‘initializeCandoScript’ was not declared in this scope
main.cc:191: error: no matching function for call to ‘mbb::O_Lisp::create(mbb::Bundle*&)’
lisp.h:65: note: candidates are: static boost::shared_ptr<mbb::O_Lisp> mbb::O_Lisp::create(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/main.o" "main.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/main.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/candoMpiDisabled.o
In file included from candoMpiDisabled.cc:4:
candoMpi.cc:160: error: prototype for ‘int mbb::O_Mpi::mpiSize()’ does not match any in class ‘mbb::O_Mpi’
candoMpi.h:42: error: candidate is: static int mbb::O_Mpi::mpiSize(mbb::RPLisp)
candoMpi.cc:187: error: prototype for ‘void mbb::O_Mpi::Init(int, char**)’ does not match any in class ‘mbb::O_Mpi’
candoMpi.h:36: error: candidate is: static void mbb::O_Mpi::Init(int, char**, mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/candoMpiDisabled.o" "candoMpiDisabled.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/candoMpiDisabled.o...
...skipped <pbin/darwin-4.2.1/debug>cando_d for lack of <pbin/darwin-4.2.1/debug>lisp.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/mainMpi.o
In file included from mainMpi.cc:13:
main.cc: In function ‘int main(int, char**)’:
main.cc:150: error: no matching function for call to ‘mbb::O_Mpi::Init(int&, char**&)’
candoMpi.h:36: note: candidates are: static void mbb::O_Mpi::Init(int, char**, mbb::RPLisp)
main.cc:179: error: ‘initializeCandoScript’ was not declared in this scope
main.cc:191: error: no matching function for call to ‘mbb::O_Lisp::create(mbb::Bundle*&)’
lisp.h:65: note: candidates are: static boost::shared_ptr<mbb::O_Lisp> mbb::O_Lisp::create(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/mainMpi.o" "mainMpi.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/mainMpi.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/candoMpiEnabled.o
In file included from candoMpiEnabled.cc:4:
candoMpi.cc:160: error: prototype for ‘int mbb::O_Mpi::mpiSize()’ does not match any in class ‘mbb::O_Mpi’
candoMpi.h:42: error: candidate is: static int mbb::O_Mpi::mpiSize(mbb::RPLisp)
candoMpi.cc: In member function ‘int mbb::O_Mpi::mpiSize()’:
candoMpi.cc:163: error: no matching function for call to ‘mbb::O_Mpi::mpiCommWorld()’
candoMpi.h:38: note: candidates are: static mbb::RPMpi mbb::O_Mpi::mpiCommWorld(mbb::RPLisp)
candoMpi.cc: In static member function ‘static int mbb::O_Mpi::mpiRank()’:
candoMpi.cc:179: error: no matching function for call to ‘mbb::O_Mpi::mpiCommWorld()’
candoMpi.h:38: note: candidates are: static mbb::RPMpi mbb::O_Mpi::mpiCommWorld(mbb::RPLisp)
candoMpi.cc: At global scope:
candoMpi.cc:187: error: prototype for ‘void mbb::O_Mpi::Init(int, char**)’ does not match any in class ‘mbb::O_Mpi’
candoMpi.h:36: error: candidate is: static void mbb::O_Mpi::Init(int, char**, mbb::RPLisp)
candoMpi.cc: In member function ‘mbb::RPObject mbb::O_Mpi::prim_Send(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
candoMpi.cc:309: error: ‘class mbb::O_XmlSaveArchive’ has no member named ‘setEnvironment’
candoMpi.cc: In member function ‘mbb::RPObject mbb::O_Mpi::prim_Recv(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
candoMpi.cc:357: error: ‘class mbb::O_XmlLoadArchive’ has no member named ‘setEnvironment’

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -c -o "bin/darwin-4.2.1/debug/candoMpiEnabled.o" "candoMpiEnabled.cc"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/candoMpiEnabled.o...
...skipped <pbin/darwin-4.2.1/debug>candoMpi_d for lack of <pbin/darwin-4.2.1/debug>lisp.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/mwxMain_obj.o
mwxMain.cpp: In function ‘mbb::RPObject mbb::prim_redraw(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
mwxMain.cpp:106: error: no matching function for call to ‘mbb::O_Object::nil()’
object.h:315: note: candidates are: static boost::shared_ptr<mbb::O_Object> mbb::O_Object::nil(mbb::RPLisp)
mwxMain.cpp: In function ‘mbb::RPObject mbb::prim_rebuildGraphics(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
mwxMain.cpp:114: error: no matching function for call to ‘mbb::O_Object::nil()’
object.h:315: note: candidates are: static boost::shared_ptr<mbb::O_Object> mbb::O_Object::nil(mbb::RPLisp)
mwxMain.cpp: In function ‘mbb::RPObject mbb::prim_setSlider(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
mwxMain.cpp:127: error: no matching function for call to ‘mbb::O_Object::nil()’
object.h:315: note: candidates are: static boost::shared_ptr<mbb::O_Object> mbb::O_Object::nil(mbb::RPLisp)
mwxMain.cpp: In function ‘mbb::RPObject mbb::prim_getSlider(mbb::O_Executable*, mbb::RPCons, mbb::RPLisp)’:
mwxMain.cpp:134: error: no matching function for call to ‘mbb::O_Int::create(mbb::uint&)’
values.h:129: note: candidates are: static boost::shared_ptr<mbb::O_Int> mbb::O_Int::create(mbb::RPLisp)
values.h:142: note:                 static mbb::RPInt mbb::O_Int::create(mbb::RPLisp, int)
mwxMain.cpp: In member function ‘void mbb::MyApp::rebuildRenderers()’:
mwxMain.cpp:240: error: no matching function for call to ‘mbb::O_KeyedArguments::nil()’
keyedArguments.h:78: note: candidates are: static boost::shared_ptr<mbb::O_KeyedArguments> mbb::O_KeyedArguments::nil(mbb::RPLisp)
mwxMain.cpp: In member function ‘virtual bool mbb::MyApp::OnInit()’:
mwxMain.cpp:261: error: ‘initializeClasses’ was not declared in this scope
mwxMain.cpp:262: error: ‘initializeCandoScript’ was not declared in this scope
mwxMain.cpp:270: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
mwxMain.cpp:271: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
mwxMain.cpp:272: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
mwxMain.cpp:273: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
mwxMain.cpp:274: error: conversion from ‘const char*’ to non-scalar type ‘mbb::RPLisp’ requested
mwxMain.cpp:283: error: no matching function for call to ‘mbb::O_Lisp::create(mbb::Bundle*&)’
lisp.h:65: note: candidates are: static boost::shared_ptr<mbb::O_Lisp> mbb::O_Lisp::create(mbb::RPLisp)
mwxMain.cpp:322: error: ‘class mbb::O_Lisp’ has no member named ‘nameSpace’
mwxMain.cpp:348: error: no matching function for call to ‘mbb::O_BuilderDatabase::nil()’
builderDatabase.h:77: note: candidates are: static boost::shared_ptr<mbb::O_BuilderDatabase> mbb::O_BuilderDatabase::nil(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -D__WXOSX_COCOA__ -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -I"/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers" -I"/usr/local/include/wx-2.9" -I"/usr/local/lib/wx/include/osx_cocoa-unicode-release-static-2.9" -c -o "bin/darwin-4.2.1/debug/mwxMain_obj.o" "mwxMain.cpp"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/mwxMain_obj.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/mwxCandoView_obj.o
mwxCandoView.cpp: In constructor ‘mbb::CandoView::CandoView(wxWindow*, wxWindowID, const wxString&, const wxPoint&, const wxSize&, long int)’:
mwxCandoView.cpp:48: error: no matching function for call to ‘mbb::O_Render::nil()’
render.h:496: note: candidates are: static boost::shared_ptr<mbb::O_Render> mbb::O_Render::nil(mbb::RPLisp)
mwxCandoView.cpp:49: error: no matching function for call to ‘mbb::O_RenderController::nil()’
renderController.h:102: note: candidates are: static boost::shared_ptr<mbb::O_RenderController> mbb::O_RenderController::nil(mbb::RPLisp)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -D__WXOSX_COCOA__ -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -I"/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers" -I"/usr/local/include/wx-2.9" -I"/usr/local/lib/wx/include/osx_cocoa-unicode-release-static-2.9" -c -o "bin/darwin-4.2.1/debug/mwxCandoView_obj.o" "mwxCandoView.cpp"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/mwxCandoView_obj.o...
darwin.compile.c++ bin/darwin-4.2.1/debug/mwxCanvas_obj.o
mwxCanvas.cpp: In constructor ‘mbb::VirtualSphereCanvas::VirtualSphereCanvas(wxPanel*, int*)’:
mwxCanvas.cpp:34: warning: ‘__base_ctor ’ is deprecated (declared at /usr/local/include/wx-2.9/wx/osx/glcanvas.h:93)
mwxCanvas.cpp:38: error: no matching function for call to ‘mbb::O_VirtualSphere::create()’
virtualSphere.h:24: note: candidates are: static boost::shared_ptr<mbb::O_VirtualSphere> mbb::O_VirtualSphere::create(mbb::RPLisp)
mwxCanvas.cpp:39: error: no matching function for call to ‘mbb::O_Render::nil()’
render.h:496: note: candidates are: static boost::shared_ptr<mbb::O_Render> mbb::O_Render::nil(mbb::RPLisp)
mwxCanvas.cpp: In member function ‘void mbb::VirtualSphereCanvas::resized(wxSizeEvent&)’:
mwxCanvas.cpp:229: warning: ‘OnSize’ is deprecated (declared at /usr/local/include/wx-2.9/wx/glcanvas.h:141)
mwxCanvas.cpp: In member function ‘void mbb::VirtualSphereCanvas::render(wxPaintEvent&)’:
mwxCanvas.cpp:378: warning: ‘SetCurrent’ is deprecated (declared at /usr/local/include/wx-2.9/wx/glcanvas.h:139)

    "g++"  -ftemplate-depth-128 -O0 -fno-inline -Wall -g -dynamic -no-cpp-precomp -gdwarf-2 -fPIC -Wno-unused-variable -fopenmp -DEXPAT -DSOURCE_DEBUG -D__WXOSX_COCOA__ -Ddarwin  -I"." -I"../../../build/include" -I"../../../build/include/boost" -I"../../../src" -I"/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers" -I"/usr/local/include/wx-2.9" -I"/usr/local/lib/wx/include/osx_cocoa-unicode-release-static-2.9" -c -o "bin/darwin-4.2.1/debug/mwxCanvas_obj.o" "mwxCanvas.cpp"

...failed darwin.compile.c++ bin/darwin-4.2.1/debug/mwxCanvas_obj.o...
...skipped <pbin/darwin-4.2.1/debug>candoView_d for lack of <pbin/darwin-4.2.1/debug>lisp.o...
...skipped <p/Users/meister/Development/cando/build/cando.app/Contents/MacOS>cando_d for lack of <pbin/darwin-4.2.1/debug>lisp.o...
...skipped <p/Users/meister/Development/cando/build/cando.app/Contents/MacOS>candoMpi_d for lack of <pbin/darwin-4.2.1/debug>lisp.o...
...skipped <p/Users/meister/Development/cando/build/cando.app/Contents/MacOS>candoView_d for lack of <pbin/darwin-4.2.1/debug>lisp.o...
...skipped <pbin/darwin-4.2.1/debug>cal_d for lack of <pbin/darwin-4.2.1/debug>lisp.o...
...failed updating 110 targets...
...skipped 7 targets...
...updated 86 targets...
