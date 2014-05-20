dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

######################################################################
#
# OMPI_EXT
#
# configure the Interface Extensions [similar to MCA version].  Works hand in
# hand with Open MPI's autogen.sh, requiring it's specially formatted lists
# of frameworks, components, etc.
#
# USAGE:
#   OMPI_EXT()
#
######################################################################
AC_DEFUN([OMPI_EXT],[
    dnl for OMPI_CONFIGURE_USER env variable
    AC_REQUIRE([OMPI_CONFIGURE_SETUP])

    # Note that we do not build DSO's here -- we *only* build convenience
    # libraries that get slurped into higher-level libraries
    #
    # [default -- no option given] = No extensions built
    # --enable-mpi-ext=[,]*EXTENSION[,]*
    #
    AC_ARG_ENABLE(mpi-ext,
        AC_HELP_STRING([--enable-mpi-ext[=LIST]],
                       [Comma-separated list of extensions that should be
                        built (default: none).]))


    AC_MSG_CHECKING([which extension components should be enabled])
    if test "$enable_mpi_ext" = "yes" -o "$enable_mpi_ext" = "all"; then
        msg="All Extensions"
        str="`echo ENABLE_EXT_ALL=1`"
        eval $str
    else
        ifs_save="$IFS"
        IFS="${IFS}$PATH_SEPARATOR,"
        msg=
        for item in $enable_mpi_ext; do
            type="`echo $item | cut -s -f1 -d-`"
            if test -z $type ; then
                type=$item
            fi
            str="`echo ENABLE_${type}=1 | sed s/-/_/g`"
            eval $str
            msg="$item $msg"
        done
        IFS="$ifs_save"
    fi
    AC_MSG_RESULT([$msg])
    unset msg

    AC_MSG_CHECKING([for projects containing EXT frameworks])
    AC_MSG_RESULT([ext_project_list])

    # if there isn't a project list, abort
    m4_ifdef([ext_project_list], [],
             [m4_fatal([Could not find project list - rerun autogen.sh without -l])])

    # now configre all the projects, frameworks, and components.  Most
    # of the hard stuff is in here
    EXT_PROJECT_SUBDIRS=
    m4_foreach(ext_project, [ext_project_list], 
               [EXT_PROJECT_SUBDIRS="$EXT_PROJECT_SUBDIRS ext_project"
                EXT_CONFIGURE_PROJECT(ext_project) ])

    # make all the config output statements for the no configure
    # components
    EXT_NO_CONFIG_CONFIG_FILES()

    AC_SUBST(EXT_C_HEADERS)
    AC_SUBST(EXT_CXX_HEADERS)
    AC_SUBST(EXT_F77_HEADERS)
    AC_SUBST(EXT_F90_HEADERS)
    AC_SUBST(EXT_C_LIBS)
    AC_SUBST(EXT_CXX_LIBS)
    AC_SUBST(EXT_F77_LIBS)
    AC_SUBST(EXT_F90_LIBS)

    AC_SUBST(EXT_PROJECT_SUBDIRS)
])


######################################################################
#
# EXT_CONFIGURE_PROJECT
#
# Configure all frameworks inside the given project name.  Assumes that
# the frameworks are located in [project_name]/[frameworks] and that
# there is an m4_defined list named ext_[project]_framework_list with
# the list of frameworks.
#
# USAGE:
#   EXT_CONFIGURE_PROJECT(project_name)
#
######################################################################
AC_DEFUN([EXT_CONFIGURE_PROJECT],[
    # can't use a variable rename here because these need to be evaled
    # at auto* time.

    ompi_show_subtitle "Configuring EXT for $1"

    AC_MSG_CHECKING([for frameworks for $1])
    AC_MSG_RESULT([ext_$1_framework_list])

    # iterate through the list of frameworks.  There is something
    # funky with m4 foreach if the list is defined, but empty.  It
    # will call the 3rd argument once with an empty value for the
    # first argument.  Protect against calling EXT_CONFIGURE_FRAMEWORK
    # with an empty second argument.  Grrr....
    # if there isn't a project list, abort
    #
    # Also setup two variables for Makefiles:
    #  EXT_project_FRAMEWORKS     - list of frameworks in that project
    #  EXT_project_FRAMEWORK_LIBS - list of libraries (or variables pointing
    #                               to more libraries) that must be included
    #                               in the project's main library
    m4_ifdef([ext_$1_framework_list],
             [_EXT_CONFIGURE_PROJECT($1)],
             [echo No EXT frameworks for $1 - skipped])
])dnl

######################################################################
#
# _EXT_CONFIGURE_PROJECT
#
# Back end for EXT_CONFIGURE_PROJECT
#
# USAGE:
#   _EXT_CONFIGURE_PROJECT(project_name)
#
######################################################################
AC_DEFUN([_EXT_CONFIGURE_PROJECT],[
    EXT_$1_FRAMEWORKS=
    EXT_$1_FRAMEWORKS_SUBDIRS=
    EXT_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS=
    EXT_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS=
    EXT_$1_FRAMEWORK_LIBS=
    
    m4_foreach(ext_framework, [ext_$1_framework_list],
               [m4_ifval(ext_framework, 
                       [
                           EXT_$1_FRAMEWORKS="$EXT_$1_FRAMEWORKS ext_framework"
                           EXT_$1_FRAMEWORKS_SUBDIRS="$EXT_$1_FRAMEWORKS_SUBDIRS ext_framework"
                           EXT_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS="$EXT_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS [\$(EXT_]ext_framework[_ALL_SUBDIRS)]"
                           EXT_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS="$EXT_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS [\$(EXT_]ext_framework[_STATIC_SUBDIRS)]"
                           EXT_$1_FRAMEWORK_LIBS="$EXT_$1_FRAMEWORK_LIBS [\$(EXT_]ext_framework[_STATIC_LTLIBS)]"
                           m4_ifdef([EXT_]ext_framework[_CONFIG],
                                    [EXT_]ext_framework[_CONFIG]($1, ext_framework),
                                    [EXT_CONFIGURE_FRAMEWORK($1, ext_framework, 1)])])])

    AC_SUBST(EXT_$1_FRAMEWORKS)
    AC_SUBST(EXT_$1_FRAMEWORKS_SUBDIRS)
    AC_SUBST(EXT_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS)
    AC_SUBST(EXT_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS)
    AC_SUBST(EXT_$1_FRAMEWORK_LIBS)
])

######################################################################
#
# EXT_CONFIGURE_FRAMEWORK
#
# Configure the given framework and all components inside the
# framework.  Assumes that the framework is located in
# [project_name]/[framework], and that all components are
# available under the framework directory.  Will configure all
# no-configure and builtin components, then search for components with
# configure scripts.  Assumes that no component is marked as builtin
# AND has a configure script.
#
# USAGE:
#   EXT_CONFIGURE_PROJECT(project_name, framework_name, allow_succeed)
#
######################################################################
AC_DEFUN([EXT_CONFIGURE_FRAMEWORK],[
    ompi_show_subsubtitle "Configuring EXT framework $2"

    OMPI_VAR_SCOPE_PUSH([all_components outfile outfile_real])

    # setup for framework
    all_components=
    static_components=
    static_ltlibs=

    outdir=$1/include

    # first create the output include directory
    mkdir -p $outdir

    # remove any previously generated #include files
    mpi_ext_h=$outdir/mpi-ext.h
    rm -f $mpi_ext_h

    # Create the final mpi-ext.h file.
    cat > $mpi_ext_h <<EOF
/*
 * \$HEADER\$
 */

#ifndef OMPI_MPI_EXT_H
#define OMPI_MPI_EXT_H 1

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define OMPI_HAVE_MPI_EXT 1

EOF

    #
    # XXX: Left todo: Add header files for other languages
    #

    # print some nice messages about what we're about to do...
    AC_MSG_CHECKING([for no configure components in framework $2])
    AC_MSG_RESULT([ext_$2_no_config_component_list])
    AC_MSG_CHECKING([for m4 configure components in framework $2])
    AC_MSG_RESULT([ext_$2_m4_config_component_list])

    # configure components that don't have any component-specific
    # configuration.  See comment in CONFIGURE_PROJECT about the
    # m4_ifval in the m4_foreach.  If there isn't a component list,
    # abort with a reasonable message.  If there are components in the
    # list, but we're doing one of the "special" selection logics,
    # abort with a reasonable message.
    m4_ifdef([ext_$2_no_config_component_list], [], 
             [m4_fatal([Could not find ext_$2_no_config_component_list - rerun autogen.sh without -l])])
    # make sure priority stuff set right
    m4_if(OMPI_EVAL_ARG([EXT_]ext_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST],
          [m4_ifval(ext_$2_no_config_component_list,
                   [m4_fatal([Framework $2 using STOP_AT_FIRST but at least one component has no configure.m4])])])
    m4_if(OMPI_EVAL_ARG([EXT_]ext_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST_PRIORITY],
          [m4_ifval(ext_$2_no_config_component_list,
                   [m4_fatal([Framework $2 using STOP_AT_FIRST_PRIORITY but at least one component has no configure.m4])])])
    m4_foreach(ext_component, [ext_$2_no_config_component_list],
               [m4_ifval(ext_component,
                  [EXT_CONFIGURE_NO_CONFIG_COMPONENT($1, $2, ext_component, 
                                                     [all_components],
                                                     [static_components],
                                                     [static_ltlibs],
                                                     [$3])])])

    # configure components that use built-in configuration scripts see
    # comment in CONFIGURE_PROJECT about the m4_ifval in the
    # m4_foreach.  if there isn't a component list, abort
    m4_ifdef([ext_$2_m4_config_component_list], [], 
             [m4_fatal([Could not find ext_$2_m4_config_component_list - rerun autogen.sh without -l])])
    best_ext_component_priority=0
    components_looking_for_succeed=$3
    components_last_result=0
    m4_foreach(ext_component, [ext_$2_m4_config_component_list],
               [m4_ifval(ext_component,
                  [m4_if(OMPI_EVAL_ARG([EXT_]ext_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST_PRIORITY],
                         [ # get the component's priority...
                          infile="$srcdir/$1/$2/ext_component/configure.params"
                          ext_component_priority="`$GREP PARAM_CONFIG_PRIORITY= $infile | cut -d= -f2-`"
                          AS_IF([test -z "$ext_component_priority"], [ext_component_priority=0])
                          AS_IF([test $best_ext_component_priority -gt $ext_component_priority], [components_looking_for_succeed=0])])
                   EXT_CONFIGURE_M4_CONFIG_COMPONENT($1, $2, ext_component, 
                                                     [all_components],
                                                     [static_components],
                                                     [static_ltlibs],
                                                     [$components_looking_for_succeed],
                                                     [components_last_result=1],
                                                     [components_last_result=0])
                   m4_if(OMPI_EVAL_ARG([EXT_]ext_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST],
                         [AS_IF([test $components_last_result -eq 1], [components_looking_for_succeed=0])])
                   m4_if(OMPI_EVAL_ARG([EXT_]ext_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST_PRIORITY],
                         [AS_IF([test $components_last_result -eq 1], [best_ext_component_priority=$ext_component_priority])])])])

    # configure components that provide their own configure script.
    # It would be really hard to run these for "find first that
    # works", so we don't :)
    m4_if(OMPI_EVAL_ARG([EXT_]ext_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST], [],
          [m4_if(OMPI_EVAL_ARG([EXT_]ext_framework[_CONFIGURE_MODE]), [STOP_AT_FIRST_PRIORITY], [],
                 [AS_IF([test "$3" != "0"],
                        [EXT_CONFIGURE_ALL_CONFIG_COMPONENTS($1, $2, [all_components],
                                               [static_ltlibs])])])])

    EXT_$2_ALL_COMPONENTS="$all_components"
    EXT_$2_STATIC_COMPONENTS="$static_components"
    EXT_$2_STATIC_LTLIBS="$static_ltlibs"

    AC_SUBST(EXT_$2_ALL_COMPONENTS)
    AC_SUBST(EXT_$2_STATIC_COMPONENTS)
    AC_SUBST(EXT_$2_STATIC_LTLIBS)

    # Create the final mpi-ext.h file.
    cat >> $mpi_ext_h <<EOF

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_MPI_EXT_H */

EOF

    #
    # XXX: Left todo: Close header files for other languages
    #

    OMPI_EXT_MAKE_DIR_LIST(EXT_$2_ALL_SUBDIRS, $2, [$all_components])
    OMPI_EXT_MAKE_DIR_LIST(EXT_$2_STATIC_SUBDIRS, $2, [$static_components])

    OMPI_VAR_SCOPE_POP
])


######################################################################
#
# EXT_CONFIGURE_NO_CONFIG_COMPONENT
#
# Configure the given framework and all components inside the framework.
# Assumes that the framework is located in [project_name]/[framework],
# and that all components are available under the framework directory.
# Will configure all builtin components, then search for components with
# configure scripts.  Assumes that no component is marked as builtin
# AND has a configure script.
#
# USAGE:
#   EXT_CONFIGURE_NO_CONFIG_COMPONENT(project_name, framework_name, component_name
#                         all_components_variable, 
#                         static_components_variable, 
#                         static_ltlibs_variable,
#                         allowed_to_succeed)
#
######################################################################
AC_DEFUN([EXT_CONFIGURE_NO_CONFIG_COMPONENT],[
    ompi_show_subsubsubtitle "EXT component $2:$3 (no configuration)"

    EXT_COMPONENT_BUILD_CHECK($1, $2, $3, 
                              [should_build=$7], [should_build=0])
    EXT_COMPONENT_COMPILE_MODE($1, $2, $3, compile_mode)

    if test "$should_build" = "1" ; then
        EXT_PROCESS_COMPONENT($1, $2, $3, $4, $6, $compile_mode)
        # add component to static component list
        $5="$$5 $3"
    else
        EXT_PROCESS_DEAD_COMPONENT($1, $2, $3)
        # add component to all component list
        $4="$$4 $3"
    fi

    unset compile_mode
])


######################################################################
#
# EXT_CONFIGURE_M4_CONFIG_COMPONENT
#
#
# USAGE:
#   EXT_CONFIGURE_PROJECT(project_name, framework_name, component_name
#                         all_components_variable, 
#                         static_components_variable, 
#                         static_ltlibs_variable,
#                         allowed_to_succeed,
#                         [eval if should build], 
#                         [eval if should not build])
#
######################################################################
AC_DEFUN([EXT_CONFIGURE_M4_CONFIG_COMPONENT],[
    ompi_show_subsubsubtitle "EXT component $2:$3 (m4 configuration macro)"

    EXT_COMPONENT_BUILD_CHECK($1, $2, $3, [should_build=$7], [should_build=0])
    # Allow the component to override the build mode if it really wants to.
    # It is, of course, free to end up calling EXT_COMPONENT_COMPILE_MODE
    m4_ifdef([EXT_$2_$3_COMPILE_MODE],
             [EXT_$2_$3_COMPILE_MODE($1, $2, $3, compile_mode)],
             [EXT_COMPONENT_COMPILE_MODE($1, $2, $3, compile_mode)])

    # try to configure the component.  pay no attention to
    # --enable-dist, since we'll always have makefiles.
    AS_IF([test "$should_build" = "1"],
          [m4_ifdef([EXT_]$2[_]$3[_CONFIG],
                    [EXT_$2_$3_CONFIG([should_build=1], 
                                      [should_build=0])],
                    # If they forgot to define an EXT_<fw>_<comp>_CONFIG 
                    # macro, print a friendly warning and abort.
                    [AC_MSG_WARN([*** The $2:$3 did not define an])
                     AC_MSG_WARN([*** EXT_$2_$3_CONFIG macro in the])
                     AC_MSG_WARN([*** $1/$2/$3/configure.m4 file])
                     AC_MSG_ERROR([Cannot continue])])
          ])

    AS_IF([test "$should_build" = "1"],
          [EXT_PROCESS_COMPONENT($1, $2, $3, $4, $6, $compile_mode)
           # add component to static component list
           $5="$$5 $3" ],
          [EXT_PROCESS_DEAD_COMPONENT($1, $2, $3)
           # add component to all component list
           $4="$$4 $3"])

    m4_ifdef([EXT_$2_$3_POST_CONFIG],
             [EXT_$2_$3_POST_CONFIG($should_build)])

    AS_IF([test "$should_build" = "1"],[$8], [$9])

    unset compile_mode
])


######################################################################
#
# EXT_CONFIGURE_ALL_CONFIG_COMPONENTS
#
# configure all components in the given framework that have configure
# scripts and should be configured according to the usual rules...
#
# USAGE:
#   EXT_CONFIGURE_ALL_CONFIG_COMPONENTS(project_name, 
#                         framework_name,
#                         all_components_variable, 
#                         static_ltlibs_variable)
#
######################################################################
AC_DEFUN([EXT_CONFIGURE_ALL_CONFIG_COMPONENTS],[
    for component_path in $srcdir/$1/$2/* ; do
        component="`basename $component_path`"
        if test -d $component_path -a -x $component_path/configure ; then
            ompi_show_subsubsubtitle "EXT component $2:$component (need to configure)"

            EXT_COMPONENT_BUILD_CHECK($1, $2, $component, 
                                      [should_build=1], [should_build=0])
            EXT_COMPONENT_COMPILE_MODE($1, $2, $component, compile_mode)

            if test "$should_build" = "1" ; then
                OMPI_CONFIG_SUBDIR([$1/$2/$component],
                                   [$ompi_subdir_args],
                                   [should_build=1], [should_build=2])
            fi

            if test "$should_build" = "1" ; then
                EXT_PROCESS_COMPONENT($1, $2, $component, $3, $4, $compile_mode)
            else
                EXT_PROCESS_DEAD_COMPONENT($1, $2, $component)
            fi
        fi
    done
])


######################################################################
#
# EXT_COMPONENT_COMPILE_MODE
#
# set compile_mode_variable to the compile mode for the given component
#
# USAGE:
#   EXT_COMPONENT_COMPILE_MODE(project_name, 
#                              framework_name, component_name
#                              compile_mode_variable)
#
#   NOTE: component_name may not be determined until runtime....
#
######################################################################
AC_DEFUN([EXT_COMPONENT_COMPILE_MODE],[
    project=$1
    framework=$2
    component=$3

    # Extensions are always static, no need for further checks
    $4="static"

    #AC_MSG_CHECKING([for EXT component $framework:$component compile mode])
    #AC_MSG_RESULT([$$4])
])


######################################################################
#
# EXT_PROCESS_COMPONENT
#
# does all setup work for given component.  It should be known before
# calling that this component can build properly (and exists)
#
# USAGE:
#   EXT_CONFIGURE_ALL_CONFIG_COMPONENTS(project_name, 
#                         framework_name, component_name
#                         all_components_variable (4), 
#                         static_ltlibs_variable (5),
#                         compile_mode_variable (6))
#
#   NOTE: component_name may not be determined until runtime....
#
# M4 directive to disable language support in configure.m4
#   Need to build a list of .la for each lang. to pull into final library
# List ext_c_headers, ext_c_libs {same for other lang.}
# C:   framework_component_c{.h, .la} 
# CXX: framework_component_cxx{.h, .la} 
# F77: framework_component_f77{.h, .la} 
# F90: framework_component_f90{.h, .la} ??? 
######################################################################
AC_DEFUN([EXT_PROCESS_COMPONENT],[
    AC_REQUIRE([AC_PROG_GREP])

    project=$1
    framework=$2
    component=$3

    # Output pretty results
    AC_MSG_CHECKING([if EXT component $framework:$component can compile])
    AC_MSG_RESULT([yes])

    # Save the list of headers and convenience libraries that this component will output
    # There *must* be C bindings
    EXT_C_HEADERS="$EXT_C_HEADERS $framework/$component/${framework}_${component}_c.h"
    EXT_C_LIBS="$EXT_C_LIBS $framework/$component/libext_${framework}_${component}.la"

    component_header="${framework}_${component}_c.h"
    tmp[=]m4_translit([$3],[a-z],[A-Z])
    component_define="OMPI_HAVE_MPI_EXT_${tmp}"

    cat >> $mpi_ext_h <<EOF
/* Enabled Extension: $component */
#define $component_define 1
#include "openmpi/ompi/mpiext/$component/$component_header"

EOF

    #
    # XXX: Need to add conditional logic for components that do not supply
    # XXX: some or all of the other 3 interfaces [C++, F77, F90]. If they
    # XXX: did provide those bindings, then add the header file to the relevant
    # XXX: language binding's header file.
    #
    EXT_CXX_HEADERS="$EXT_CXX_HEADERS $framework/$component/${framework}_${component}_cxx.h"
    EXT_CXX_LIBS="$EXT_CXX_LIBS $framework/$component/libext_${framework}_${component}_cxx.la"

    EXT_F77_HEADERS="$EXT_F77_HEADERS $framework/$component/${framework}_${component}_f77.h"
    EXT_F77_LIBS="$EXT_F77_LIBS $framework/$component/libext_${framework}_${component}_f77.la"

    EXT_F90_HEADERS="$EXT_F90_HEADERS $framework/$component/${framework}_${component}_f90.h"
    EXT_F90_LIBS="$EXT_F90_LIBS $framework/$component/libext_${framework}_${component}_f90.la"

    # See if it dropped an output file for us to pick up some
    # shell variables in.  
    infile="$srcdir/$project/$framework/$component/post_configure.sh"

    # Add this subdir to the mast list of all EXT component subdirs
    $4="$$4 $component"

    $5="$framework/$component/libext_${framework}_${component}.la $$5"

    # If there's an output file, add the values to
    # scope_EXTRA_flags.
    if test -f $infile; then

        # First check for the ABORT tag
        line="`$GREP ABORT= $infile | cut -d= -f2-`"
        if test -n "$line" -a "$line" != "no"; then
            AC_MSG_WARN([EXT component configure script told me to abort])
            AC_MSG_ERROR([cannot continue])
        fi

        # Check for flags passed up from the component.  If we're
        # compiling statically, then take all flags passed up from the
        # component.
        m4_foreach(flags, [LDFLAGS, LIBS],
           [[line="`$GREP WRAPPER_EXTRA_]flags[= $infile | cut -d= -f2-`"]
            eval "line=$line"
            if test -n "$line"; then
                $1[_WRAPPER_EXTRA_]flags[="$]$1[_WRAPPER_EXTRA_]flags[ $line"]
            fi
        ])dnl
    fi

    # now add the flags that were set in the environment variables
    # framework_component_FOO (for example, the flags set by
    # m4_configure components)
    #
    # Check for flags passed up from the component.  If we're
    # compiling statically, then take all flags passed up from the
    # component.
    m4_foreach(flags, [LDFLAGS, LIBS],
        [[str="line=\$${framework}_${component}_WRAPPER_EXTRA_]flags["]
          eval "$str"
          if test -n "$line" ; then
             $1[_WRAPPER_EXTRA_]flags[="$]$1[_WRAPPER_EXTRA_]flags[ $line"]
          fi
          ])dnl
])


######################################################################
#
# EXT_PROCESS_DEAD_COMPONENT
#
# process a component that can not be built.  Do the last minute checks
# to make sure the user isn't doing something stupid.
#
# USAGE:
#   EXT_PROCESS_DEAD_COMPONENT(project_name, 
#                         framework_name, component_name)
#
#   NOTE: component_name may not be determined until runtime....
#
######################################################################
AC_DEFUN([EXT_PROCESS_DEAD_COMPONENT],[
    AC_MSG_CHECKING([if EXT component $2:$3 can compile])
    AC_MSG_RESULT([no])

    # If this component was requested as the default for this
    # type, then abort.
    if test "$with_$2" = "$3" ; then
        AC_MSG_WARN([EXT component "$3" failed to configure properly])
        AC_MSG_WARN([This component was selected as the default])
        AC_MSG_ERROR([Cannot continue])
        exit 1
    fi
])



######################################################################
#
# EXT_COMPONENT_BUILD_CHECK
#
# checks the standard rules of component building to see if the 
# given component should be built.
#
# USAGE:
#    EXT_COMPONENT_BUILD_CHECK(project, framework, component, 
#                              action-if-build, action-if-not-build)
#
######################################################################
AC_DEFUN([EXT_COMPONENT_BUILD_CHECK],[
    AC_REQUIRE([AC_PROG_GREP])

    project=$1
    framework=$2
    component=$3
    component_path="$srcdir/$project/$framework/$component"
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
    fi

    # if we asked for everything, then allow it to build if able
    str="ENABLED_COMPONENT_CHECK=\$ENABLE_EXT_ALL"
    eval $str
    if test ! "$ENABLED_COMPONENT_CHECK" = "1" ; then
        # if we were explicitly disabled, don't build :)
        str="ENABLED_COMPONENT_CHECK=\$ENABLE_${component}"
        eval $str
        if test ! "$ENABLED_COMPONENT_CHECK" = "1" ; then
            want_component=0
        fi
    fi

    AS_IF([test "$want_component" = "1"], [$4], [$5])
])


# OMPI_EXT_MAKE_DIR_LIST(subst'ed variable, framework, shell list)
# -------------------------------------------------------------------------
AC_DEFUN([OMPI_EXT_MAKE_DIR_LIST],[
    $1=
    for item in $3 ; do
       $1="$$1 $2/$item"
    done
    AC_SUBST($1)
])
