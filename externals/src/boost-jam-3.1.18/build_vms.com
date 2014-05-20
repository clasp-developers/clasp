$ ! Copyright 2002-2003 Rene Rivera, Johan Nilsson.
$ ! Distributed under the Boost Software License, Version 1.0.
$ ! (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)
$ !
$ ! bootstrap build script for Jam
$ !
$ SAY :== WRITE SYS$OUTPUT
$ !
$ ON WARNING THEN CONTINUE
$ !
$ IF "" .NES. F$SEARCH("[.bootstrap_vms]*.*")
$ THEN
$   SAY "Cleaning previous boostrap files..."
$ !
$   SET FILE/PROTECTION=(S:RWED) [.bootstrap_vms]*.*;*
$   DELETE [.bootstrap_vms]*.*;*
$ ENDIF
$ !
$ IF "" .NES. F$SEARCH("bootstrap_vms.dir")
$ THEN
$   SAY "Removing previous boostrap directory..."
$ !
$   SET FILE/PROT=(S:RWED) bootstrap_vms.dir
$   DELETE bootstrap_vms.dir;
$ ENDIF
$ !
$ SAY "Creating boostrap directory..."
$ !
$ CREATE/DIR [.bootstrap_vms]
$ !
$ SAY "Building bootstrap jam..."
$ !
$ CC_FLAGS = "/DEFINE=VMS /STANDARD=VAXC /PREFIX_LIBRARY_ENTRIES=ALL_ENTRIES "
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]builtins.obj builtins.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]command.obj command.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]compile.obj compile.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]execvms.obj execvms.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]expand.obj expand.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]filesys.obj filesys.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]filevms.obj filevms.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]glob.obj glob.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]hash.obj hash.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]hdrmacro.obj hdrmacro.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]headers.obj headers.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]jam.obj jam.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]jambase.obj jambase.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]jamgram.obj jamgram.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]lists.obj lists.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]make.obj make.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]make1.obj make1.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]modules.obj modules.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]newstr.obj newstr.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]option.obj option.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]parse.obj parse.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]pathvms.obj pathvms.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]pwd.obj pwd.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]regexp.obj regexp.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]rules.obj rules.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]scan.obj scan.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]search.obj search.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]strings.obj strings.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]subst.obj subst.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]timestamp.obj timestamp.c
$ cc 'CC_FLAGS /OBJECT=[.bootstrap_vms]variable.obj variable.c
$ link -
 /EXECUTABLE=[.bootstrap_vms]jam0.exe -
 [.bootstrap_vms]builtins.obj, -
 [.bootstrap_vms]command.obj, -
 [.bootstrap_vms]compile.obj, -
 [.bootstrap_vms]execvms.obj, -
 [.bootstrap_vms]expand.obj, -
 [.bootstrap_vms]filesys.obj, -
 [.bootstrap_vms]filevms.obj, -
 [.bootstrap_vms]glob.obj, -
 [.bootstrap_vms]hash.obj, -
 [.bootstrap_vms]hdrmacro.obj, -
 [.bootstrap_vms]headers.obj, -
 [.bootstrap_vms]jam.obj, -
 [.bootstrap_vms]jambase.obj, -
 [.bootstrap_vms]jamgram.obj, -
 [.bootstrap_vms]lists.obj, -
 [.bootstrap_vms]make.obj, -
 [.bootstrap_vms]make1.obj, -
 [.bootstrap_vms]modules.obj, -
 [.bootstrap_vms]newstr.obj, -
 [.bootstrap_vms]option.obj, -
 [.bootstrap_vms]parse.obj, -
 [.bootstrap_vms]pathvms.obj, -
 [.bootstrap_vms]pwd.obj, -
 [.bootstrap_vms]regexp.obj, -
 [.bootstrap_vms]rules.obj, -
 [.bootstrap_vms]scan.obj, -
 [.bootstrap_vms]search.obj, -
 [.bootstrap_vms]strings.obj, -
 [.bootstrap_vms]subst.obj, -
 [.bootstrap_vms]timestamp.obj, -
 [.bootstrap_vms]variable.obj
$ !
$ SAY "Cleaning any previous build..."
$ !
$ MCR [.bootstrap_vms]jam0.exe -f build.jam --toolset=vmsdecc clean
$ !
$ SAY "Building Boost.Jam..."
$ !
$ MCR [.bootstrap_vms]jam0.exe -f build.jam --toolset=vmsdecc
