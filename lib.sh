
umask 022

at_least_one ()
{
    test "$1" "$2"
}

f_info ()
{
    echo CLASP_DEPLOY_S3_BUCKET=$CLASP_DEPLOY_S3_BUCKET
    echo DISTFILES_TMP_DIR=$DISTFILES_TMP_DIR
    find "${DISTFILES_TMP_DIR-ERROR}"/. "${CLASP_DEPLOY_S3_BUCKET-ERROR}"/. \
        -type f -ls
}

ctime ()
{
    gtime -f "${CTIME_PREFIX}%E %e real %U user %S sys %P CPU %F/%R faults${CTIME_POSTFIX}" "$@"
}

check_quicklisp ()
{
    local init_quicklisp_p="$1"

    if [ "${CLASP_QUICKLISP_HOMEDIR-}" = 1 ] ; then
        # user knows what they are doing
        return 0
    fi

    if ! [ -n "${CLASP_QUICKLISP_DIRECTORY-}" ] ; then
        echo WARNING: '$CLASP_QUICKLISP_DIRECTORY' is not set 1>&2
        sleep 3
    else
        if [ "$init_quicklisp_p" = t ] ; then
            do_git_tree "$CLASP_QUICKLISP_DIRECTORY" https://github.com/quicklisp/quicklisp-client.git master
        else
            if ! [ -d "$CLASP_QUICKLISP_DIRECTORY" ] ; then
                printf "WARNING: quicklisp dir '%s' does not exist\n" \
                    "$CLASP_QUICKLISP_DIRECTORY" 1>&2
                sleep 3
            fi
        fi
    fi

    if ! [ -n "${ASDF_OUTPUT_TRANSLATIONS-}" ] ; then
        echo WARNING: '$ASDF_OUTPUT_TRANSLATIONS' is not set 1>&2
        sleep 3
    fi
}

set_python_variables ()
{
    local vers
    for vers in 3.7 3.6 3.5 3.4 ; do
        if which python$vers > /dev/null 2> /dev/null ; then
            PYTHON_VERSION=$vers
            break
        fi
    done

    if [ "$OS" = "Linux" ] ; then
        export DEPLOY_PYTHONPATH="/usr/lib/python3/dist-packages"
    elif [ "$OS" = "Darwin" ] ; then
        export DEPLOY_PYTHONPATH="/usr/local/Cellar/ipython/6.5.0/libexec/lib/python3.7/site-packages:/usr/local/Cellar/ipython/6.5.0/libexec/vendor/lib/python3.7/site-packages"
    elif [ "$OS" = "FreeBSD" ] ; then
        export DEPLOY_PYTHONPATH="/usr/local/lib/python3/dist-packages"
    else
        : # not an error.  User can set.
    fi

    if ! [ -d "$DEPLOY_PYTHONPATH" ] ; then
        # not an error, use can set pythonpath from outside
        unset DEPLOY_PYTHONPATH
    fi

    export PYTHONPATH="/opt/clasp/lib/python$PYTHON_VERSION/site-packages:/opt/clasp/lib64/python$PYTHON_VERSION/site-packages:$DEPLOY_PYTHONPATH"
}

f_setenv_clasp ()
{
    echo '# written by the clasp deploy script'
    echo '#if ! echo $LD_LIBRARY_PATH | grep -q /opt/clasp ; then'
    echo '    export LD_LIBRARY_PATH="/opt/clasp/lib:$LD_LIBRARY_PATH"'
    echo '    export PATH="/opt/clasp/bin:$PATH"'
    echo '    #export PKG_CONFIG_PATH="/opt/clasp/lib/pkgconfig,$PKG_CONFIG_PATH"'
    echo '    export ACLOCAL_FLAGS="-I /opt/clasp/share/aclocal ${ACLOCAL_FLAGS-}"'
    echo '    export JUPYTERLAB_DIR=${JUPYTERLAB_DIR-/opt/clasp/jupyter/lab}'
    echo '    export JUPYTER_PATH=${JUPYTER_PATH-/opt/clasp/run}'
    echo '    export SLIME_HOME=${SLIME_HOME-/opt/clasp/slime}'
    echo '    export XDG_CACHE_HOME="${XDG_CACHE_HOME-/opt/clasp/lib/cache}"'
    echo '    export ASDF_OUTPUT_TRANSLATIONS=${ASDF_OUTPUT_TRANSLATION-/:}'
    echo '    export CLASP_QUICKLISP_DIRECTORY=${CLASP_QUICKLISP_DIRECTORY-/opt/clasp/lib/clasp/src/lisp/modules/quicklisp}'

    if ! [ -n "$PYTHON_VERSION" ] ; then
        set_python_variables
    fi
    if [ -n "$PYTHONPATH" ] ; then
        echo "    export PYTHONPATH=\"${PYTHONPATH%:}:\$PYTHONPATH\""
    fi

    echo '#fi'
    echo 'export CRAENVPROMPT="\$clasp"'
}

f_write_setenv_clasp ()
{
    f_setenv_clasp > /opt/clasp/bin/setenv-clasp
}

prepare_quicklisp_compilation ()
{
    # we don't have to do anything anymore
    # but we check and warn
    check_quicklisp
}

get_quicklisp_cando ()
{
    check_quicklisp t
    do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/cl-netcdf https://github.com/clasp-developers/cl-netcdf.git master
    do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/static-vectors https://github.com/sionescu/static-vectors.git master
    do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/trivial-garbage https://github.com/clasp-developers/trivial-garbage.git master
    do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/bordeaux-threads https://github.com/clasp-developers/bordeaux-threads.git master
    do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/cffi https://github.com/clasp-developers/cffi.git master
    do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/usocket https://github.com/clasp-developers/usocket.git master
    do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/uuid https://github.com/clasp-developers/uuid.git master
    do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/trivial-backtrace https://github.com/clasp-developers/trivial-backtrace master
    do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/esrap https://github.com/cando-developers/esrap master
}

# this is a superset
get_quicklisp_jupyter ()
{
    check_quicklisp t

    get_quicklisp_cando
	do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/cl-jupyter https://github.com/drmeister/cl-jupyter.git origin/master
	do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/cl-ipykernel https://github.com/clasp-developers/cl-ipykernel.git origin/master
	do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/cl-ipywidgets https://github.com/clasp-developers/cl-ipywidgets.git origin/master
	do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/cl-nglview https://github.com/clasp-developers/cl-nglview.git origin/master
	do_git_tree "$CLASP_QUICKLISP_DIRECTORY"/local-projects/cl-bqplot https://github.com/clasp-developers/cl-bqplot.git origin/master
}


run_command ()
{
    if ! "$@" ; then
        echo this failed1: "$@" 1>&2
        echo in dir `pwd` 1>&2
        exit 1
    fi
    return 0
}

do_verbose_as_superuser ()
{
    echo doing "'""$@""'" as root
    if ! sudo "$@" ; then
        echo this failed1: sudo "$@" 1>&2
        echo in dir `pwd` 1>&2
        exit 1
    fi
    return 0
}

bind_mount_p ()
{
    mount | grep -q 'on /opt/clasp '
}

no_bind_mount ()
{
    while bind_mount_p ; do
        do_verbose_as_superuser umount /opt/clasp || exit 1
    done
}

# sanity checks
run_first ()
{
    if [ -n "$DO_BIND_MOUNT" ] ; then
        no_bind_mount
    fi
}

ensure_opt_clasp_is_symlink_to_dir ()
{
    local target=${1-}
    if ! [ -n "$target" ] ; then
        echo ensure_opt_clasp_is_symlink_to_dir needs targetdir arg 1>&2
        return 1
    fi
    if ! [ -d "$target" ] ; then
        echo ensure_opt_clasp_is_symlink_to_dir targetdir arg is not a dir: $target 1>&2
        return 1
    fi
    if [ -n "${DO_BIND_MOUNT-}" ] ; then
        no_bind_mount
    fi
    rm -rf /opt/clasp
    if [ -e /opt/clasp ] ; then
        echo cannot eliminate /opt/clasp 1>&2
        return 2
    fi
    ln -sf "$target" /opt/clasp || return 1
    if ! [ -L "/opt/clasp" ] ; then
        echo ensure_opt_clasp_is_symlink_to_dir inconsistent link: $target 1>&2
        return 1
    fi
    if ! [ -d "/opt/clasp/." ] ; then
        echo ensure_opt_clasp_is_symlink_to_dir inconsistent dir 1>&2
        return 1
    fi
    return 0
}

ensure_opt_clasp_is_empty_dir ()
{
    if [ -L /opt/clasp ] ; then
        rm /opt/clasp || return 1
    fi
    if [ -n "${DO_BIND_MOUNT-}" ] ; then
        no_bind_mount
    fi
    if [ -d /opt/clasp ] ; then
        if [ -n "${DO_BIND_MOUNT-}" ] ; then
            do_verbose_as_superuser mount --bind "${DO_BIND_MOUNT-}" /opt/clasp || return 1
            df /opt/clasp
        fi
        rm -rf /opt/clasp/*
    else
        mkdir /opt/clasp
    fi
    if at_least_one -e /opt/clasp/* ; then
        echo cannot eliminate /opt/clasp/ contents 1>&2
        return 2
    fi
    # sanity check
    if ! [ -d "/opt/clasp" ] ; then
        echo ensure_opt_clasp_is_empty_dir inconsistent dir: $target 1>&2
        return 1
    fi
    return 0
}

ensure_opt_clasp_is_writeable_dir ()
{
    if [ -n "${DO_BIND_MOUNT-}" ] ; then
        if ! bind_mount_p ; then
            do_verbose_as_superuser mount --bind "${DO_BIND_MOUNT-}" /opt/clasp || return 1
        else
            echo skipping bind mount, already there
        fi
    else
        mkdir -p /opt/clasp
    fi
    df /opt/clasp
    if ! [ -d "/opt/clasp" ] ; then
        echo ERROR: ensure_opt_clasp_is_writeable_dir inconsistent dir: $target 1>&2
        return 1
    fi
    if ! touch /opt/clasp/foo.txt ; then
        echo ERROR: ensure_opt_clasp_is_writeable_dir failed 1>&2
        return 1
    else
        rm /opt/clasp/foo.txt
    fi
    return 0
}

do_git_tree ()
{
    local where=$1 ; shift || return
    local url=$1 ; shift || return
    local branch_or_tag=${1-} # optional
    local reponame
    local pw

    case "${branch_or_tag-}" in
        origin/master) branch_or_tag="";; # this is not a branch
    esac

    pw=${PWD-`pwd`}

    case "$where" in
        */*) run_command mkdir -p ${where%/*};;
    esac

    if ! [ -d "$where" ] ; then
        # you cannot use --branch here because it doesn't take
        # sha256 commit ids
        echo doing git clone "$url" "$where"
        run_command git clone "$url" "$where"
        if [ -n "$branch_or_tag" ] ; then
            echo doing git checkout $branch_or_tag
            cd $where && run_command git checkout $branch_or_tag
            cd $pw
        fi
    else
        cd "$where"
        echo doing git fetch in $PWD
        run_command git fetch
        if [ -n "$branch_or_tag" ] ; then
            echo doing git checkout $branch_or_tag in $PWD
            run_command git checkout -q $branch_or_tag
            if git status | grep -q detached ; then
                echo head is detached, not doing git pull in $PWD
            else
                echo doing git pull in $PWD
                run_command git pull
            fi
        else
            echo doing git pull in $PWD
            run_command git pull
        fi
        cd "$pw"
    fi
    if false ; then # debug code
        (cd "$where"
            echo HEAD in `pwd` is:
            git rev-parse --verify HEAD
            git log --pretty=format:'%h' -n 1
            git status
            git log -n 2
        ) | cat
    fi
}

copy_tarfile ()
{
    local tarfile="${1-}"
    case "$CLASP_DEPLOY_S3_BUCKET" in
        s3:*)
            aws s3 cp "$DISTFILES_TMP_DIR/$tarfile" "$CLASP_DEPLOY_S3_BUCKET/" || return 1
            ;;
        *:*)
            # rsync target
            echo copying tarfile "$DISTFILES_TMP_DIR/$tarfile" to "$CLASP_DEPLOY_S3_BUCKET/."
            rsync -a "$DISTFILES_TMP_DIR/$tarfile" "$CLASP_DEPLOY_S3_BUCKET/." || return 1
            ;;
    esac
}

tar_to ()
{
    # arguments:
    # 1) tarfile to create
    # 2-n) files to put into tar and random other tar args

    # usage example:
    # note how you give the extension and the -z flag manually
    #tar_to foo.tar.gz -z blah.o meh.core

    local tarfile="${1-}"; shift

    mkdir -p "$DISTFILES_TMP_DIR"
    case "$tarfile" in
        "") echo tar_to tarfilename arg empty 1>&2; return 1;;
        */*) echo tar_to tarfilename arg must not have a dir 1>&2; return 1;;
    esac

    case "$CLASP_DEPLOY_S3_BUCKET" in
        # s3 or rsync target.
        # The rsync target could be piped over ssh instead of a local
        # target first, however I always want a local tarfile left here.
        s3:*|*:*)
            tar c -f "$DISTFILES_TMP_DIR/$tarfile.tmp" "$@" || return 1
            mv "$DISTFILES_TMP_DIR/$tarfile.tmp" "$DISTFILES_TMP_DIR/$tarfile" || return 1
            ;;
        *)
            mkdir -p "$CLASP_DEPLOY_S3_BUCKET"
            echo creating tarfile "$CLASP_DEPLOY_S3_BUCKET/$tarfile.tmp" from "$@" in `pwd`
            tar c -f "$CLASP_DEPLOY_S3_BUCKET/$tarfile.tmp" "$@" || return 1
            mv "$CLASP_DEPLOY_S3_BUCKET/$tarfile.tmp" "$CLASP_DEPLOY_S3_BUCKET/$tarfile" || return 1
            ;;
    esac
    # the *.tmp business is done so that we don't have incomplete
    # tarfiles floating around looking like the real thing
    copy_tarfile "$tarfile" || return 1
}

fetch_tarfiles_if_not_here ()
{
    local tarfile
    mkdir -p "$DISTFILES_TMP_DIR"
    for tarfile in "$@" ; do
        [ -f "$DISTFILES_TMP_DIR/$tarfile" ] && continue
        case "$CLASP_DEPLOY_S3_BUCKET" in
            s3:*)
                aws s3 cp "$CLASP_DEPLOY_S3_BUCKET/$tarfile" "$DISTFILES_TMP_DIR/" || return 1
                ;;
            *:*)
                # rsync target
                rsync -a "$CLASP_DEPLOY_S3_BUCKET/$tarfile" "$DISTFILES_TMP_DIR/." || return 1
                ;;
            *)
                # local
                ln -sf "$CLASP_DEPLOY_S3_BUCKET/$tarfile" "$DISTFILES_TMP_DIR/." || return 1
                ;;
        esac
    done
}

tar_from ()
{
    # arguments:
    # 1) tarfile, will only be copied from remote if missing locally
    # 2) target directory
    # 3-n) random tar flags, e.g. -z

    local tarfile="${1-}"; shift
    case "$tarfile" in
        "") echo tar_from tarfilename arg empty 1>&2; return 1;;
        */*) echo tar_from tarfilename arg must not have a dir 1>&2; return 1;;
    esac

    local target_dir="${1-}"; shift
    case "$target_dir" in
        "") echo tar_from dirname arg empty 1>&2; return 1;;
    esac

    if ! [ -d "$target_dir" ] ; then
        echo tar_from: target dir "$target_dir" must already exist 1>&2
        return 1
    fi

    fetch_tarfiles_if_not_here $tarfile || return 1
    echo extracting "$DISTFILES_TMP_DIR/$tarfile" to "$target_dir"
    tar x -f "$DISTFILES_TMP_DIR/$tarfile" -C "$target_dir" "$@" || return 1
}
