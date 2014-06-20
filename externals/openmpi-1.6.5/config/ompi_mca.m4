dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_EVAL_ARG(arg)
# ------------------
# evaluates and returns argument
AC_DEFUN([OMPI_EVAL_ARG], [$1])

######################################################################
#
# OMPI_MCA
#
# configure the MCA (modular component architecture).  Works hand in hand
# with Open MPI's autogen.sh, requiring it's specially formatted lists
# of frameworks, components, etc.
#
# USAGE:
#   OMPI_MCA()
#
######################################################################
AC_DEFUN([OMPI_MCA],[
    dnl for OMPI_CONFIGURE_USER env variable
    AC_REQUIRE([OMPI_CONFIGURE_SETUP])

    # Find which components should be built as run-time loadable components
    # Acceptable combinations:
    #
    # [default -- no option given]
    # --enable-mca-dso
    # --enable-mca-dso=[.+,]*COMPONENT_TYPE[.+,]*
    # --enable-mca-dso=[.+,]*COMPONENT_TYPE-COMPONENT_NAME[.+,]*
    # --disable-mca-dso
    #
    AC_ARG_ENABLE([mca-no-build],
        [AC_HELP_STRING([--enable-mca-no-build=LIST],
                        [Comma-separated list of <type>-<component> pairs 
                         that will not be built.  Example: "--enable-mca-no-build=maffinity,btl-portals" will disable building all maffinity components and the "portals" btl components.])])
    AC_ARG_ENABLE(mca-dso,
        AC_HELP_STRING([--enable-mca-dso=LIST],
                       [Comma-separated list of types and/or
                        type-component pairs that will be built as
                        run-time loadable components (as opposed to
                        statically linked in), if supported on this
                        platform.  The default is to build all components
                        as DSOs.]))
    AC_ARG_ENABLE(mca-static,
        AC_HELP_STRING([--enable-mca-static=LIST],
                       [Comma-separated list of types and/or
                        type-component pairs that will be built statically
                        linked into the library.  The default (if DSOs are
                        supported) is to build all components as DSOs.
                        Enabling a component as static disables it
                        building as a DSO.]))
    AC_ARG_ENABLE(mca-direct,
        AC_HELP_STRING([--enable-mca-direct=LIST],
                       [Comma-separated list of type-component pairs that
                        will be hard coded as the one component to use for
                        a given component type, saving the (small)
                        overhead of the component architecture.  LIST must
                        not be empty and implies given component pairs are
                        build as static components.]))

    AC_MSG_CHECKING([which components should be disabled])
    if test "$enable_mca_no_build" = "yes"; then
        AC_MSG_RESULT([yes])
        AC_MSG_ERROR([*** The enable-mca-no-build flag requires an explicit list
*** of type-component pairs.  For example, --enable-mca-direct=pml-ob1])
    else
        ifs_save="$IFS"
        IFS="${IFS}$PATH_SEPARATOR,"
        msg=
        for item in $enable_mca_no_build; do
            type="`echo $item | cut -s -f1 -d-`"
            comp="`echo $item | cut -s -f2- -d-`"
            if test -z $type ; then
                type=$item
            fi
            if test -z $comp ; then
                str="`echo DISABLE_${type}=1 | sed s/-/_/g`"
                eval $str
                msg="$item $msg"
            else
                str="`echo DISABLE_${type}_${comp}=1 | sed s/-/_/g`"
                eval $str
                msg="$item $msg"
            fi
        done
        IFS="$ifs_save"
    fi
    AC_MSG_RESULT([$msg])
    unset msg

    #
    # First, add all the mca-direct components / types into the mca-static
    # lists and create a list of component types that are direct compile,
    # in the form DIRECT_[type]=[component]
    #
    AC_MSG_CHECKING([which components should be direct-linked into the library])
    if test "$enable_mca_direct" = "yes" ; then
        AC_MSG_RESULT([yes])
        AC_MSG_ERROR([*** The enable-mca-direct flag requires an explicit list of
*** type-component pairs.  For example, --enable-mca-direct=pml-ob1,coll-basic])
    elif test ! -z "$enable_mca_direct" -a "$enable_mca_direct" != "" ; then
        #
        # we need to add this into the static list, unless the static list
        # is everything
        #
        if test "$enable_mca_static" = "no" ; then
            AC_MSG_WARN([*** Re-enabling static component support for direct call])
            enable_mca_static="$enable_mca_direct"
        elif test -z "$enable_mca_static" ; then
            enable_mca_static="$enable_mca_direct"
        elif test "$enable_mca_static" != "yes" ; then
            enable_mca_static="$enable_mca_direct,$enable_mca_static"
        fi

        ifs_save="$IFS"
        IFS="${IFS}$PATH_SEPARATOR,"
        msg=
        for item in $enable_mca_direct; do
            type="`echo $item | cut -f1 -d-`"
            comp="`echo $item | cut -f2- -d-`"
            if test -z $type -o -z $comp ; then
                AC_MSG_ERROR([*** The enable-mca-direct flag requires a
*** list of type-component pairs.  Invalid input detected.])
            else
                str="`echo DIRECT_$type=$comp | sed s/-/_/g`"
                eval $str
                msg="$item $msg"
            fi
        done
        IFS="$ifs_save"
    fi
    AC_MSG_RESULT([$msg])
    unset msg

    #
    # Second, set the DSO_all and STATIC_all variables.  conflict
    # resolution (prefer static) is done in the big loop below
    #
    AC_MSG_CHECKING([which components should be run-time loadable])
    if test "$enable_static" != "no"; then
        DSO_all=0
        msg=none
    elif test -z "$enable_mca_dso" -o "$enable_mca_dso" = "yes"; then
        DSO_all=1
        msg=all
    elif test "$enable_mca_dso" = "no"; then
        DSO_all=0
        msg=none
    else
        DSO_all=0
        ifs_save="$IFS"
        IFS="${IFS}$PATH_SEPARATOR,"
        msg=
        for item in $enable_mca_dso; do
            str="`echo DSO_$item=1 | sed s/-/_/g`"
            eval $str
            msg="$item $msg"
        done
        IFS="$ifs_save"
    fi
    AC_MSG_RESULT([$msg])
    unset msg
    if test "$enable_static" != "no"; then
        AC_MSG_WARN([*** Shared libraries have been disabled (--disable-shared)])
        AC_MSG_WARN([*** Building MCA components as DSOs automatically disabled])
    fi

    AC_MSG_CHECKING([which components should be static])
    if test "$enable_mca_static" = "yes"; then
        STATIC_all=1
        msg=all
    elif test -z "$enable_mca_static" -o "$enable_mca_static" = "no"; then
        STATIC_all=0
        msg=none
    else
        STATIC_all=0
        ifs_save="$IFS"
        IFS="${IFS}$PATH_SEPARATOR,"
        msg=
        for item in $enable_mca_static; do
            str="`echo STATIC_$item=1 | sed s/-/_/g`"
            eval $str
            msg="$item $msg"
        done
        IFS="$ifs_save"
    fi
    AC_MSG_RESULT([$msg])
    unset msg

    AC_MSG_CHECKING([for projects containing MCA frameworks])
    AC_MSG_RESULT([mca_project_list])

    # if there isn't a project list, abort
    m4_ifdef([mca_project_list], [],
             [m4_fatal([Could not find project list - rerun autogen.sh without -l])])

    # now configre all the projects, frameworks, and components.  Most
    # of the hard stuff is in here
    MCA_PROJECT_SUBDIRS=
    m4_foreach(mca_project, [mca_project_list], 
               [MCA_PROJECT_SUBDIRS="$MCA_PROJECT_SUBDIRS mca_project"
                MCA_CONFIGURE_PROJECT(mca_project)])

    # BWB - fix me...  need to automate this somehow
    MCA_SETUP_DIRECT_CALL(pml, ompi)
    MCA_SETUP_DIRECT_CALL(mtl, ompi)

    # make all the config output statements for the no configure
    # components
    MCA_NO_CONFIG_CONFIG_FILES()

    AC_SUBST(MCA_PROJECT_SUBDIRS)
])


######################################################################
#
# MCA_CONFIGURE_PROJECT
#
# Configure all frameworks inside the given project name.  Assumes that
# the frameworks are located in [project_name]/mca/[frameworks] and that
# there is an m4_defined list named mca_[project]_framework_list with
# the list of frameworks.
#
# USAGE:
#   MCA_CONFIGURE_PROJECT(project_name)
#
######################################################################
AC_DEFUN([MCA_CONFIGURE_PROJECT],[
    # can't use a variable rename here because these need to be evaled
    # at auto* time.

    ompi_show_subtitle "Configuring MCA for $1"

    AC_MSG_CHECKING([for frameworks for $1])
    AC_MSG_RESULT([mca_$1_framework_list])

    # iterate through the list of frameworks.  There is something
    # funky with m4 foreach if the list is defined, but empty.  It
    # will call the 3rd argument once with an empty value for the
    # first argument.  Protect against calling MCA_CONFIGURE_FRAMEWORK
    # with an empty second argument.  Grrr....
    # if there isn't a project list, abort
    #
    # Also setup two variables for Makefiles:
    #  MCA_project_FRAMEWORKS     - list of frameworks in that project
    #  MCA_project_FRAMEWORK_LIBS - list of libraries (or variables pointing
    #                               to more libraries) that must be included
    #                               in the project's main library
    m4_ifdef([mca_$1_framework_list], [], 
             [m4_fatal([Could not find mca_$1_framework_list - rerun autogen.sh without -l])])

    MCA_$1_FRAMEWORKS=
    MCA_$1_FRAMEWORKS_SUBDIRS=
    MCA_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS=
    MCA_$1_FRAMEWORK_COMPONENT_DSO_SUBDIRS=
    MCA_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS=
    MCA_$1_FRAMEWORK_LIBS=
    
    m4_foreach(mca_framework, [mca_$1_framework_list],
               [m4_ifval(mca_framework, 
                         [# common has to go up front
                          if test "mca_framework" = "common" ; then
                              MCA_$1_FRAMEWORKS="mca_framework $MCA_$1_FRAMEWORKS"
                              MCA_$1_FRAMEWORKS_SUBDIRS="[mca/]mca_framework $MCA_$1_FRAMEWORKS_SUBDIRS"
                              MCA_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS="[\$(MCA_]mca_framework[_ALL_SUBDIRS)] $MCA_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS"
                              MCA_$1_FRAMEWORK_COMPONENT_DSO_SUBDIRS="[\$(MCA_]mca_framework[_DSO_SUBDIRS)] $MCA_$1_FRAMEWORK_COMPONENT_DSO_SUBDIRS"
                              MCA_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS="[\$(MCA_]mca_framework[_STATIC_SUBDIRS)] $MCA_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS"
                          else
                              MCA_$1_FRAMEWORKS="$MCA_$1_FRAMEWORKS mca_framework"
                              MCA_$1_FRAMEWORKS_SUBDIRS="$MCA_$1_FRAMEWORKS_SUBDIRS [mca/]mca_framework"
                              MCA_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS="$MCA_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS [\$(MCA_]mca_framework[_ALL_SUBDIRS)]"
                              MCA_$1_FRAMEWORK_COMPONENT_DSO_SUBDIRS="$MCA_$1_FRAMEWORK_COMPONENT_DSO_SUBDIRS [\$(MCA_]mca_framework[_DSO_SUBDIRS)]"
                              MCA_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS="$MCA_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS [\$(MCA_]mca_framework[_STATIC_SUBDIRS)]"
                          fi
                          if test "mca_framework" != "common" ; then
                              MCA_$1_FRAMEWORK_LIBS="$MCA_$1_FRAMEWORK_LIBS [mca/]mca_framework[/libmca_]mca_framework[.la]"
                          fi
                          MCA_$1_FRAMEWORK_LIBS="$MCA_$1_FRAMEWORK_LIBS [\$(MCA_]mca_framework[_STATIC_LTLIBS)]"
                          m4_ifdef([MCA_]mca_framework[_CONFIG],
                                   [MCA_]mca_framework[_CONFIG]($1, mca_framework),
                                   [MCA_CONFIGURE_FRAMEWORK($1, mca_framework, 1)])])])

    AC_SUBST(MCA_$1_FRAMEWORKS)
    AC_SUBST(MCA_$1_FRAMEWORKS_SUBDIRS)
    AC_SUBST(MCA_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS)
    AC_SUBST(MCA_$1_FRAMEWORK_COMPONENT_DSO_SUBDIRS)
    AC_SUBST(MCA_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS)
    AC_SUBST(MCA_$1_FRAMEWORK_LIBS)
])

######################################################################
#
# MCA_CONFIGURE_FRAMEWORK
#
# Configure the given framework and all components inside the
# framework.  Assumes that the framework is located in
# [project_name]/mca/[framework], and that all components are
# available under the framework directory.  Will configure all
# no-configure and builtin components, then search for components with
# configure scripts.  Assumes that no component is marked as builtin
# AND has a configure script.
#
# USAGE:
#   MCA_CONFIGURE_PROJECT(project_name, framework_name, allow_succeed)
#
######################################################################
AC_DEFUN([MCA_CONFIGURE_FRAMEWORK],[
    ompi_show_subsubtitle "Configuring MCA framework $2"

    # setup for framework
    all_components=
    static_components=
    dso_components=
    static_ltlibs=

    # Ensure that the directory where the #include file is to live
    # exists.  Need to do this for VPATH builds, because the directory
    # may not exist yet.  For the "common" type, it's not really a
    # component, so it doesn't have a base.
    if test "$2" = "common" ; then
        outdir=$1/mca/common
    else
        outdir=$1/mca/$2/base
    fi
    AS_MKDIR_P([$outdir])

    # remove any previously generated #include files
    outfile_real=$outdir/static-components.h
    outfile=$outfile_real.new
    rm -f $outfile $outfile.struct $outfile.extern
    touch $outfile.struct $outfile.extern

    # print some nice messages about what we're about to do...
    AC_MSG_CHECKING([for no configure components in framework $2])
    AC_MSG_RESULT([mca_$2_no_config_component_list])
    AC_MSG_CHECKING([for m4 configure components in framework $2])
    AC_MSG_RESULT([mca_$2_m4_config_component_list])

    # configure components that don't have any component-specific
    # configuration.  See comment in CONFIGURE_PROJECT about the
    # m4_ifval in the m4_foreach.  If there isn't a component list,
    # abort with a reasonable message.  If there are components in the
    # list, but we're doing one of the "special" selection logics,
    # abort with a reasonable message.
    m4_ifdef([mca_$2_no_config_component_list], [], 
             [m4_fatal([Could not find mca_$2_no_config_component_list - rerun autogen.sh without -l])])
    # make sure priority stuff set right
    m4_if(OMPI_EVAL_ARG([MCA_]mca_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST],
          [m4_ifval(mca_$2_no_config_component_list,
                   [m4_fatal([Framework $2 using STOP_AT_FIRST but at least one component has no configure.m4])])])
    m4_if(OMPI_EVAL_ARG([MCA_]mca_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST_PRIORITY],
          [m4_ifval(mca_$2_no_config_component_list,
                   [m4_fatal([Framework $2 using STOP_AT_FIRST_PRIORITY but at least one component has no configure.m4])])])
    m4_foreach(mca_component, [mca_$2_no_config_component_list],
               [m4_ifval(mca_component,
                  [MCA_CONFIGURE_NO_CONFIG_COMPONENT($1, $2, mca_component, 
                                                     [all_components],
                                                     [static_components],
                                                     [dso_components],
                                                     [static_ltlibs],
                                                     [$3])])])

    # configure components that use built-in configuration scripts see
    # comment in CONFIGURE_PROJECT about the m4_ifval in the
    # m4_foreach.  if there isn't a component list, abort
    m4_ifdef([mca_$2_m4_config_component_list], [], 
             [m4_fatal([Could not find mca_$2_m4_config_component_list - rerun autogen.sh without -l])])
    best_mca_component_priority=0
    components_looking_for_succeed=$3
    components_last_result=0
    m4_foreach(mca_component, [mca_$2_m4_config_component_list],
               [m4_ifval(mca_component,
                  [m4_if(OMPI_EVAL_ARG([MCA_]mca_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST_PRIORITY],
                         [ # get the component's priority...
                          infile="$srcdir/$1/mca/$2/mca_component/configure.params"
                          mca_component_priority="`$GREP PARAM_CONFIG_PRIORITY= $infile | cut -d= -f2-`"
                          AS_IF([test -z "$mca_component_priority"], [mca_component_priority=0])
                          AS_IF([test $best_mca_component_priority -gt $mca_component_priority], [components_looking_for_succeed=0])])
                   MCA_CONFIGURE_M4_CONFIG_COMPONENT($1, $2, mca_component, 
                                                     [all_components],
                                                     [static_components],
                                                     [dso_components],
                                                     [static_ltlibs],
                                                     [$components_looking_for_succeed],
                                                     [components_last_result=1],
                                                     [components_last_result=0])
                   m4_if(OMPI_EVAL_ARG([MCA_]mca_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST],
                         [AS_IF([test $components_last_result -eq 1], [components_looking_for_succeed=0])])
                   m4_if(OMPI_EVAL_ARG([MCA_]mca_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST_PRIORITY],
                         [AS_IF([test $components_last_result -eq 1], [best_mca_component_priority=$mca_component_priority])])])])

    # configure components that provide their own configure script.
    # It would be really hard to run these for "find first that
    # works", so we don't :)
    m4_if(OMPI_EVAL_ARG([MCA_]mca_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST], [],
          [m4_if(OMPI_EVAL_ARG([MCA_]mca_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST_PRIORITY], [],
                 [AS_IF([test "$3" != "0"],
                        [MCA_CONFIGURE_ALL_CONFIG_COMPONENTS($1, $2, [all_components],
                                               [static_components], [dso_components],
                                               [static_ltlibs])])])])

    MCA_$2_ALL_COMPONENTS="$all_components"
    MCA_$2_STATIC_COMPONENTS="$static_components"
    MCA_$2_DSO_COMPONENTS="$dso_components"
    MCA_$2_STATIC_LTLIBS="$static_ltlibs"

    AC_SUBST(MCA_$2_ALL_COMPONENTS)
    AC_SUBST(MCA_$2_STATIC_COMPONENTS)
    AC_SUBST(MCA_$2_DSO_COMPONENTS)
    AC_SUBST(MCA_$2_STATIC_LTLIBS)

    OMPI_MCA_MAKE_DIR_LIST(MCA_$2_ALL_SUBDIRS, $2, [$all_components])
    OMPI_MCA_MAKE_DIR_LIST(MCA_$2_STATIC_SUBDIRS, $2, [$static_components])
    OMPI_MCA_MAKE_DIR_LIST(MCA_$2_DSO_SUBDIRS, $2, [$dso_components])

    # Create the final .h file that will be included in the type's
    # top-level glue.  This lists all the static components.  We don't
    # need to do this for "common".
    if test "$2" != "common"; then
        cat > $outfile <<EOF
/*
 * \$HEADER\$
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

`cat $outfile.extern`

const mca_base_component_t *mca_$2_base_static_components[[]] = {
`cat $outfile.struct`
  NULL
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

EOF
        # Only replace the header file if a) it doesn't previously
        # exist, or b) the contents are different.  Do this to not
        # trigger recompilation of certain .c files just because the
        # timestamp changed on $outfile_real (similar to the way AC
        # handles AC_CONFIG_HEADER files).
        diff $outfile $outfile_real > /dev/null 2>&1
        if test "$?" != "0"; then
            mv $outfile $outfile_real
        else
            rm -f $outfile
        fi
    fi
    rm -f $outfile.struct $outfile.extern 

    unset all_components static_components dso_components outfile outfile_real
])


######################################################################
#
# MCA_CONFIGURE_NO_CONFIG_COMPONENT
#
# Configure the given framework and all components inside the framework.
# Assumes that the framework is located in [project_name]/mca/[framework],
# and that all components are available under the framework directory.
# Will configure all builtin components, then search for components with
# configure scripts.  Assumes that no component is marked as builtin
# AND has a configure script.
#
# USAGE:
#   MCA_CONFIGURE_PROJECT(project_name, framework_name, component_name
#                         all_components_variable, 
#                         static_components_variable,
#                         dso_components_variable,
#                         static_ltlibs_variable,
#                         allowed_to_succeed)
#
######################################################################
AC_DEFUN([MCA_CONFIGURE_NO_CONFIG_COMPONENT],[
    ompi_show_subsubsubtitle "MCA component $2:$3 (no configuration)"

    MCA_COMPONENT_BUILD_CHECK($1, $2, $3, 
                              [should_build=$8], [should_build=0])
    MCA_COMPONENT_COMPILE_MODE($1, $2, $3, compile_mode)

    if test "$should_build" = "1" ; then
        MCA_PROCESS_COMPONENT($1, $2, $3, $4, $5, $6, $7, $compile_mode)
    else
        MCA_PROCESS_DEAD_COMPONENT($1, $2, $3)
        # add component to all component list
        $4="$$4 $3"
    fi

    # set the AM_CONDITIONAL on how we should build
    if test "$compile_mode" = "dso" ; then
        BUILD_$2_$3_DSO=1
    else
        BUILD_$2_$3_DSO=0
    fi
    AM_CONDITIONAL(OMPI_BUILD_$2_$3_DSO, test "$BUILD_$2_$3_DSO" = "1")

    unset compile_mode
])


######################################################################
#
# MCA_CONFIGURE_M4_CONFIG_COMPONENT
#
#
# USAGE:
#   MCA_CONFIGURE_PROJECT(project_name, framework_name, component_name
#                         all_components_variable, 
#                         static_components_variable,
#                         dso_components_variable,
#                         static_ltlibs_variable,
#                         allowed_to_succeed,
#                         [eval if should build], 
#                         [eval if should not build])
#
######################################################################
AC_DEFUN([MCA_CONFIGURE_M4_CONFIG_COMPONENT],[
    ompi_show_subsubsubtitle "MCA component $2:$3 (m4 configuration macro)"

    MCA_COMPONENT_BUILD_CHECK($1, $2, $3, [should_build=$8], [should_build=0])
    # Allow the component to override the build mode if it really wants to.
    # It is, of course, free to end up calling MCA_COMPONENT_COMPILE_MODE
    m4_ifdef([MCA_$2_$3_COMPILE_MODE],
             [MCA_$2_$3_COMPILE_MODE($1, $2, $3, compile_mode)],
             [MCA_COMPONENT_COMPILE_MODE($1, $2, $3, compile_mode)])

    # try to configure the component.  pay no attention to
    # --enable-dist, since we'll always have makefiles.
    m4_ifdef([MCA_]$2[_]$3[_CONFIG],
             [MCA_$2_$3_CONFIG([should_build=$should_build], 
                               [should_build=0])],
              # If they forgot to define an MCA_<fw>_<comp>_CONFIG 
              # macro, print a friendly warning and abort.
             [AC_MSG_WARN([*** The $2:$3 did not define an])
              AC_MSG_WARN([*** MCA_$2_$3_CONFIG macro in the])
              AC_MSG_WARN([*** $1/$2/$3/configure.m4 file])
              AC_MSG_ERROR([Cannot continue])])

    AS_IF([test "$should_build" = "1"],
          [MCA_PROCESS_COMPONENT($1, $2, $3, $4, $5, $6, $7, $compile_mode)],
          [MCA_PROCESS_DEAD_COMPONENT($1, $2, $3)
           # add component to all component list
           $4="$$4 $3"])

    m4_ifdef([MCA_$2_$3_POST_CONFIG],
             [MCA_$2_$3_POST_CONFIG($should_build)])

    # set the AM_CONDITIONAL on how we should build
    AS_IF([test "$compile_mode" = "dso"], 
          [BUILD_$2_$3_DSO=1],
          [BUILD_$2_$3_DSO=0])
    AM_CONDITIONAL(OMPI_BUILD_$2_$3_DSO, test "$BUILD_$2_$3_DSO" = "1")

    AS_IF([test "$should_build" = "1"], [$9], [$10])

    unset compile_mode
])


######################################################################
#
# MCA_CONFIGURE_ALL_CONFIG_COMPONENTS
#
# configure all components in the given framework that have configure
# scripts and should be configured according to the usual rules...
#
# USAGE:
#   MCA_CONFIGURE_ALL_CONFIG_COMPONENTS(project_name, 
#                         framework_name,
#                         all_components_variable, 
#                         static_components_variable,
#                         dso_components_variable,
#                         static_ltlibs_variable)
#
######################################################################
AC_DEFUN([MCA_CONFIGURE_ALL_CONFIG_COMPONENTS],[
    for component_path in $srcdir/$1/mca/$2/* ; do
        component="`basename $component_path`"
        if test -d $component_path -a -x $component_path/configure ; then
            ompi_show_subsubsubtitle "MCA component $2:$component (need to configure)"

            MCA_COMPONENT_BUILD_CHECK($1, $2, $component, 
                                      [should_build=1], [should_build=0])
            MCA_COMPONENT_COMPILE_MODE($1, $2, $component, compile_mode)

            if test "$should_build" = "1" ; then
                OMPI_CONFIG_SUBDIR([$1/mca/$2/$component],
                                   [$ompi_subdir_args],
                                   [should_build=1], [should_build=2])
            fi

            if test "$should_build" = "1" ; then
                MCA_PROCESS_COMPONENT($1, $2, $component, $3, $4, $5, $6, $compile_mode)
            else
                MCA_PROCESS_DEAD_COMPONENT($1, $2, $component)
            fi
        fi
    done
])


######################################################################
#
# MCA_COMPONENT_COMPILE_MODE
#
# set compile_mode_variable to the compile mode for the given component
#
# USAGE:
#   MCA_COMPONENT_COMPILE_MODE(project_name, 
#                              framework_name, component_name
#                              compile_mode_variable)
#
#   NOTE: component_name may not be determined until runtime....
#
######################################################################
AC_DEFUN([MCA_COMPONENT_COMPILE_MODE],[
    project=$1
    framework=$2
    component=$3

    # Is this component going to built staic or shared?  $component
    # might not be known until configure time, so have to use eval
    # tricks - can't set variable names at autogen time.
    str="SHARED_FRAMEWORK=\$DSO_$framework"
    eval $str
    str="SHARED_COMPONENT=\$DSO_${framework}_$component"
    eval $str

    str="STATIC_FRAMEWORK=\$STATIC_$framework"
    eval $str
    str="STATIC_COMPONENT=\$STATIC_${framework}_$component"
    eval $str

    shared_mode_override=static

    # Setup for either shared or static
    if test "$STATIC_FRAMEWORK" = "1" -o \
        "$STATIC_COMPONENT" = "1" -o \
        "$STATIC_all" = "1" ; then
        $4="static"
    elif test "$shared_mode_override" = "dso" -o \
        "$SHARED_FRAMEWORK" = "1" -o \
        "$SHARED_COMPONENT" = "1" -o \
        "$DSO_all" = "1"; then
        $4="dso"
    else
        $4="static"
    fi

    AC_MSG_CHECKING([for MCA component $framework:$component compile mode])
    if test "$DIRECT_$2" = "$component" ; then
        AC_MSG_RESULT([$$4 - direct])
    else
        AC_MSG_RESULT([$$4])
    fi
])


######################################################################
#
# MCA_PROCESS_COMPONENT
#
# does all setup work for given component.  It should be known before
# calling that this component can build properly (and exists)
#
# USAGE:
#   MCA_CONFIGURE_ALL_CONFIG_COMPONENTS(project_name, 
#                         framework_name, component_name
#                         all_components_variable (4), 
#                         static_components_variable (5),
#                         dso_components_variable (6),
#                         static_ltlibs_variable (7),
#                         compile_mode_variable (8))
#
#   NOTE: component_name may not be determined until runtime....
#
######################################################################
AC_DEFUN([MCA_PROCESS_COMPONENT],[
    AC_REQUIRE([AC_PROG_GREP])

    project=$1
    framework=$2
    component=$3

    # See if it dropped an output file for us to pick up some
    # shell variables in.  
    infile="$srcdir/$project/mca/$framework/$component/post_configure.sh"

    # Add this subdir to the mast list of all MCA component subdirs
    $4="$$4 $component"

    if test "$8" = "dso" ; then
        $6="$$6 $component"
    else
        $7="mca/$framework/$component/libmca_${framework}_${component}.la $$7"
        echo "extern const mca_base_component_t mca_${framework}_${component}_component;" >> $outfile.extern
        echo "  &mca_${framework}_${component}_component, " >> $outfile.struct
        $5="$$5 $component"
    fi

    # Output pretty results
    AC_MSG_CHECKING([if MCA component $framework:$component can compile])
    AC_MSG_RESULT([yes])
    
    # If there's an output file, add the values to
    # scope_EXTRA_flags.
    if test -f $infile; then

        # First check for the ABORT tag
        line="`$GREP ABORT= $infile | cut -d= -f2-`"
        if test -n "$line" -a "$line" != "no"; then
            AC_MSG_WARN([MCA component configure script told me to abort])
            AC_MSG_ERROR([cannot continue])
        fi

        # Check for flags passed up from the component.  If we're
        # compiling statically, then take all flags passed up from the
        # component.
        if test "$8" = "static"; then
            m4_foreach(flags, [LDFLAGS, LIBS],
               [[line="`$GREP WRAPPER_EXTRA_]flags[= $infile | cut -d= -f2-`"]
                eval "line=$line"
                if test -n "$line"; then
                    $1[_WRAPPER_EXTRA_]flags[="$]$1[_WRAPPER_EXTRA_]flags[ $line"]
                fi
            ])dnl
        fi

        dnl check for direct call header to include.  This will be
        dnl AC_SUBSTed later.
        if test "$DIRECT_$2" = "$component" ; then
            if test "`$GREP DIRECT_CALL_HEADER $infile`" != "" ; then
                line="`$GREP DIRECT_CALL_HEADER $infile | cut -d= -f2-`"
                str="MCA_${framework}_DIRECT_CALL_HEADER=$line"
                eval $str
            else
AC_MSG_ERROR([*** ${framework} component ${component} was supposed to be direct-called, but
*** does not appear to support direct calling.
*** Aborting])
            fi
        fi
    else
        # were we supposed to have found something in the 
        # post_configure.sh, but the file didn't exist?
        if test "$DIRECT_$2" = "$component" ; then
AC_MSG_ERROR([*** ${framework} component ${component} was supposed to be direct-called, but
*** does not appear to support direct calling.
*** Aborting])
        fi
    fi

    # now add the flags that were set in the environment variables
    # framework_component_FOO (for example, the flags set by
    # m4_configure components)
    #
    # Check for flags passed up from the component.  If we're
    # compiling statically, then take all flags passed up from the
    # component.
    if test "$8" = "static"; then
        m4_foreach(flags, [LDFLAGS, LIBS],
            [[str="line=\$${framework}_${component}_WRAPPER_EXTRA_]flags["]
             eval "$str"
             if test -n "$line" ; then
                 $1[_WRAPPER_EXTRA_]flags[="$]$1[_WRAPPER_EXTRA_]flags[ $line"]
             fi
             ])dnl
    fi
])


######################################################################
#
# MCA_PROCESS_DEAD_COMPONENT
#
# process a component that can not be built.  Do the last minute checks
# to make sure the user isn't doing something stupid.
#
# USAGE:
#   MCA_PROCESS_DEAD_COMPONENT(project_name, 
#                         framework_name, component_name)
#
#   NOTE: component_name may not be determined until runtime....
#
######################################################################
AC_DEFUN([MCA_PROCESS_DEAD_COMPONENT],[
    AC_MSG_CHECKING([if MCA component $2:$3 can compile])
    AC_MSG_RESULT([no])

    # If this component was requested as the default for this
    # type, then abort.
    if test "$with_$2" = "$3" ; then
        AC_MSG_WARN([MCA component "$3" failed to configure properly])
        AC_MSG_WARN([This component was selected as the default])
        AC_MSG_ERROR([Cannot continue])
        exit 1
    fi

    if test ! -z "$DIRECT_$2" ; then
        if test "$DIRECT_$2" = "$3" ; then
            AC_MSG_WARN([MCA component "$3" failed to configure properly])
            AC_MSG_WARN([This component was selected as the default (direct call)])
            AC_MSG_ERROR([Cannot continue])
            exit 1
        fi
    fi
])



######################################################################
#
# MCA_COMPONENT_BUILD_CHECK
#
# checks the standard rules of component building to see if the 
# given component should be built.
#
# USAGE:
#    MCA_COMPONENT_BUILD_CHECK(project, framework, component, 
#                              action-if-build, action-if-not-build)
#
######################################################################
AC_DEFUN([MCA_COMPONENT_BUILD_CHECK],[
    AC_REQUIRE([AC_PROG_GREP])

    project=$1
    framework=$2
    component=$3
    component_path="$srcdir/$project/mca/$framework/$component"
    want_component=0

    # build if:
    # - the component type is direct and we are that component
    # - there is no ompi_ignore file
    # - there is an ompi_ignore, but there is an empty ompi_unignore
    # - there is an ompi_ignore, but username is in ompi_unignore
    if test -d $component_path ; then
        # decide if we want the component to be built or not.  This
        # is spread out because some of the logic is a little complex
        # and test's syntax isn't exactly the greatest.  We want to
        # build the component by default.
        want_component=1
        if test -f $component_path/.ompi_ignore ; then
            # If there is an ompi_ignore file, don't build
            # the component.  Note that this decision can be
            # overridden by the unignore logic below.
            want_component=0
        fi
        if test -f $component_path/.ompi_unignore ; then
            # if there is an empty ompi_unignore, that is
            # equivalent to having your userid in the unignore file.
            # If userid is in the file, unignore the ignore file.
            if test ! -s $component_path/.ompi_unignore ; then
                want_component=1
            elif test ! -z "`$GREP $OMPI_CONFIGURE_USER $component_path/.ompi_unignore`" ; then
                want_component=1
            fi
        fi
        # if this component type is direct and we are not it, we don't want
        # to be built.  Otherwise, we do want to be built.
        if test ! -z "$DIRECT_$2" ; then
            if test "$DIRECT_$2" = "$component" ; then
                want_component=1
            else
                want_component=0
            fi
        fi
    fi

    # if we were explicitly disabled, don't build :)
    str="DISABLED_COMPONENT_CHECK=\$DISABLE_${framework}"
    eval $str
    if test "$DISABLED_COMPONENT_CHECK" = "1" ; then
        want_component=0
    fi
    str="DISABLED_COMPONENT_CHECK=\$DISABLE_${framework}_$component"
    eval $str
    if test "$DISABLED_COMPONENT_CHECK" = "1" ; then
        want_component=0
    fi

    AS_IF([test "$want_component" = "1"], [$4], [$5])
])


######################################################################
#
# MCA_SETUP_DIRECT_CALL
#
# Do all the things necessary to setup the given framework for direct
# call building
#
# USAGE:
#   MCA_SETUP_DIRECT_CALL(framework, project)
#
######################################################################
AC_DEFUN([MCA_SETUP_DIRECT_CALL],[
    if test ! -z "$DIRECT_$1" ; then
        MCA_$1_DIRECT_CALL_COMPONENT=$DIRECT_$1
        MCA_$1_DIRECT_CALL=1
    else
        MCA_$1_DIRECT_CALL_COMPONENT=
        MCA_$1_DIRECT_CALL=0
    fi

    AC_SUBST(MCA_$1_DIRECT_CALL_HEADER)
    AC_DEFINE_UNQUOTED([MCA_$1_DIRECT_CALL], [$MCA_$1_DIRECT_CALL],
            [Defined to 1 if $1 should use direct calls instead of components])
    AC_DEFINE_UNQUOTED([MCA_$1_DIRECT_CALL_COMPONENT], [$MCA_$1_DIRECT_CALL_COMPONENT],
            [name of component to use for direct calls, if MCA_$1_DIRECT_CALL is 1])
    AC_DEFINE_UNQUOTED([MCA_$1_DIRECT_CALL_HEADER],
                       ["[$MCA_]$1[_DIRECT_CALL_HEADER]"],
                       [Header $1 includes to be direct called])
])


# OMPI_MCA_MAKE_DIR_LIST(subst'ed variable, framework, shell list)
# -------------------------------------------------------------------------
AC_DEFUN([OMPI_MCA_MAKE_DIR_LIST],[
    $1=
    for item in $3 ; do
       $1="$$1 mca/$2/$item"
    done
    AC_SUBST($1)
])
