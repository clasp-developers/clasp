%define _specver        $Id$
%define _name           ecl
%define _pkg            %{_name}
%define _ver            ECL_VERSION
%define _rel            1
%define _pkgver         %{_pkg}-%{_ver}
%define _nv             %{_name}-%{_ver}

%define __installp      %{__install} -p
%define __installdir    %{__installp} -d

# define _buildtmp   # absurd that RPM connects both build and installation
                     # _tmppath values
%define _buildtmp       %{_tmppath}
%define _filelist       filelist-%{_pkg}-%{_ver}-%{_rel}

##########################################################################

Summary:        ECL - Embeddable Common-Lisp %{_ver}
Name:           %{_pkg}
Version:        %{_ver}
Release:        %{_rel}
License:        LGPL
Group:          Development/Languages/Lisp
URL:            http://ecls.sourceforge.net/
Source0:        http://easynews.dl.sourceforge.net/sourceforge/ecls/ecl-%{_ver}.tgz
# want to add these later.
#Source1:        ecls-regexp.lisp
BuildRoot:      %{_buildtmp}/%{_nv}-buildroot
#% REDHAT BuildRequires:  rpm-devel
#% REDHAT BuildRequires:  gmp-devel
#% SUSE BuildRequires:  rpm
#% SUSE BuildRequires:  gmp

%description

ECL stands for Embeddable Common-Lisp.

The ECL project is an effort to modernize Giuseppe Attardi's ECL
environment to produce an implementation of the Common-Lisp language
which complies to the ANSI X3J13 definition of the language.

build-id ---> %_specver

##########################################################################

%prep
if [ -z "%{_buildtmp}" ]; then
    cat <<EOF >&2

   !!! Required macro is not defined:  '_buildtmp'; aborting!

EOF
    return 1
fi
%setup -q -n %{_nv}

##########################################################################

%build
%configure \
        --enable-local-gmp   \
        --with-x             \
        --with-ffi           \
        --with-clos-stream   \
        --with-tcp	     \
        --with-cmuformat     \
	--with-clx
        # --enable-local-boehm \          # broken
        # --enable-threads                # non-supported still.
%{__make}

##########################################################################

%install
_r=$RPM_BUILD_ROOT
rm -rf $_r
%{__installdir} -m 755 $_r

%{__make}                               \
        PREFIX=$_r                      \
        install

[ -x /usr/lib/rpm/brp-compress ] && /usr/lib/rpm/brp-compress

# copy the files in texi/*
_d=$_r/%{_docdir}/%{_nv}
%{__installdir} -m 755 $_d
for i in \
        build/doc/*.html \
        src/doc/BUGS \
        src/doc/todo.txt
do
    %{__install} -m 644 $i $_d
done

for a in ecl ecldev
do
    _d=$_r/%{_docdir}/%{_nv}/$a
    %{__installdir} -m 755 $_d
    for i in \
            build/doc/$a/*.html
    do
        %{__install} -m 644 $i $_d
    done
done

# BUG:  the cause of the first problem is in the configure.in which set the
# values of libdir, infodir, bindir
#(
#  cd $_r/%{_prefix}
#  %{__installdir} -m 755 share
#  %{__mv} man info share
#)
# BUG: the cause of this problem is an 'install -m 644' that installs "cmp.fas"
# with some archive files and lisp files.
# find $_r -name "*.fas" | xargs chmod a+x

          ##################################################

gen_filelist()
{
  _d=$1;shift
  _l=$1;shift
  find $_d | perl -nl \
    -e "\$_d='$_d';" \
    -e 'if ( ! -d ) { $_f=1; undef $_p; }' \
    -e 'elsif ( m,$_d.*%{_pkg}, ) { $_f=1; $_p="%dir "; }' \
    -e 'if ( $_f ) {' \
    -e '  s,/*$_d/*,/,;' \
    -e '  printf "%s\n", "$_p$_";' \
    -e '  undef $_f }' \
    > $_l
}

gen_filelist $RPM_BUILD_ROOT %{_filelist}

##########################################################################

# add info files to info dir in %post
%post
for _n in ecl ecldev clx; do
    _d=%{_infodir}
    _f=$_d/$_n.info.gz
    if [ -f $_f ]; then
        /sbin/install-info $_f $_d/dir
    fi
done
/sbin/ldconfig

# remove info files from info dir in %postun
%postun
for _n in ecl ecldev clx; do
    _d=%{_infodir}
    _f=$_d/$_n.info.gz
    if [ -f $_f ]; then
        /sbin/install-info --delete $_f $_d/dir
    fi
done
/sbin/ldconfig

##########################################################################

%clean
#echo The maid is off on `date +%A`.
for i in "$RPM_BUILD_ROOT" "$RPM_BUILD_DIR/%{_nv}"; do
    [ "$i" != "/" ] && rm -rf $i || :
done

##########################################################################

%files -f %{_filelist}
%defattr(-,root,root)

##########################################################################

%changelog
* Wed Oct 23 2003 Juanjo <worm@arrakis.es>
- Ported to Suse and integrated in the ECL distribution.

* Mon Oct 20 2003 Robert Lehr <bozzio@the-lehrs.com>
- consolidated texi/info patches
- added build-dependency on gmp-devel
- changed group to Development/Languages/Lisp
- added _buildtmp definition for public use
- added rpm-build build dependency

* Fri Oct 10 2003 Robert Lehr <bozzio@the-lehrs.com>
- wrote private spec file for ECL v0.9b
