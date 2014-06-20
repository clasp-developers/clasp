10 July 2009
---------------

This version of Open MPI compiles and runs under Microsoft Windows
platforms, including Windows XP, Windows HPC Server 2003/2008 and also
Windows 7 RC. It provides two remote process launch components, one is
using WMI, and the other is using Windows CCP API for Windows HPC
Servers. But it has received limited testing compared to other POSIX-like
environments, so feedbacks are important and helpful for us. 

You can download an Open MPI tarball from the main web site
(http://www.open-mpi.org) or check out a developer copy via Subversion
(more information about SVN checkout available on the Open MPI web
site).

The rest of the document is divided on 3 sections.  The first and
second sections are for "easy" setup, based on some files that are
distributed in Open MPI (32 and 64 bits versions).  The third section
is only for advanced users or developers who want to deeply dig into
the software.

============================================================================

First approach: Using CMake 

NOTE: CMake support is available in the Open MPI development
trunk and 1.3.3 release.


I. Build Open MPI

  1. Download the latest version of CMake (at least v2.4).

  2. In the CMake GUI, add the source path and build path of Open MPI
   (out of source build is recommended).

  3. Then configure, and after the first time configuration, all
     available options will show up in the CMake GUI.  Select the
     options that you require.

  4. Run configure again to generate all Windows solution files; they
     will be generated in build path.

  5. Go to the build directory, open the generated Windows solution
     file, and compile.

  6. Build the 'INSTALL' project to install files into the path that
     has been set with CMAKE_INSTALL_PREFIX variable in CMake GUI.

  7. (optional) To generate a installer, you should install NSIS, and build the 
     'PACKAGE' project in the Open MPI sulotion. 


II. Build MPI applications with Open MPI

After successfully built and installed Open MPI, there are two ways
to compile and link your applications against the generated libraries:

  Using Visual Studio IDE

  1. In your application's Visual Studio solution, on the target project
  that needs to link with MPI, add the installed Open MPI library path 
  (for example C:\Program Files\OpenMPI_v1.4\lib) into "Project Property Pages" -> 
  "Configuration Properties" -> Linker -> General -> "Additional Library Directories"

  2. Also put the library names, for example libopen-mpi.lib, into 
  "Project Property Pages" -> "Configuration Properties" -> Linker -> 
  Input -> "Additional Dependencies". Please note that, if you built 
  debug version of Open MPI, the generated library names will have
  suffix 'd', for example, libopen-mpid.lib.

  Using Open MPI compiler wrappers in VS Command Prompt

  1. Open up a Visual Studio Command Prompt, so that the Visual Studio 
  environment is set, and the VS compiler command cl.exe is available.

  2. Set up Open MPI path environment variable, for example,
  "set PATH=c:\Program Files\OpenMPI_v1.4\bin;%PATH%", or set it up in
  the system settings. Please note, using an Open MPI installer, there
  will be an option to set the system variables automatically.

  3. Simply use one of the Open MPI compiler wrapper to compile and 
  link your application with one single command, for example:
      c:\> mpicc app.c


III. Run MPI applications

To launch the application, use the mpirun command, for example:
      c:\> mpirun -np 2 app.exe
or
      c:\> mpirun -np 2 -host host1 host2 app.exe

Please note, in order to launch remote MPI processes using WMI, a few Windows 
settings has to be configured on all nodes, please refer to the following links:
http://msdn.microsoft.com/en-us/library/aa393266(VS.85).aspx
http://community.spiceworks.com/topic/578


----------------------------------------------------------------------------

Second approach: Simple and straightforward

Step 1: Untar the contrib/platform/win32/ompi-static.tgz tarball in
        the root directory of the Open MPI distribution.

Step 2: Go in the ompi/datatype subdirectory in the Open MPI
        distribution and copy the following:

        datatype_pack.c   to datatype_pack_checksum.c
        datatype_unpack.c to datatype_unpack_checksum.c
        
Step 3: Add the following to ompi/tools/ompi_info/ompi_info.h and
        change the relevant information inside (i.e., replace
        everything between @'s).

#define OMPI_CONFIGURE_USER "@USER_NAME@"
#define OMPI_CONFIGURE_HOST "@HOST_NAME@"
#define OMPI_CONFIGURE_DATE "@TODAY_DATE@"
#define OMPI_BUILD_USER OMPI_CONFIGURE_USER
#define OMPI_BUILD_HOST OMPI_CONFIGURE_HOST
#define OMPI_BUILD_DATE OMPI_CONFIGURE_DATE
#define OMPI_BUILD_CFLAGS "/Od /Gm /EHsc /RTC1 /MDd"
#define OMPI_BUILD_CPPFLAGS "-I${HOME}/ompi-trunk -I${HOME}/opal/include -I${HOME}/ompi-trunk/orte/include -I${HOME}/ompi-trunk/ompi/include"
#define OMPI_BUILD_CXXFLAGS "/Od /Gm /EHsc /RTC1 /MDd"
#define OMPI_BUILD_CXXCPPFLAGS "-I${HOME}/ompi-trunk -I../../.. -I$(HOME}/ompi-trunk/opal/include -I${HOME}/ompi-trunk/orte/include -I${HOME}/ompi-trunk/ompi/include"
#define OMPI_BUILD_FFLAGS ""
#define OMPI_BUILD_FCFLAGS ""
#define OMPI_BUILD_LDFLAGS " "
#define OMPI_BUILD_LIBS " "
#define OPAL_CC_ABSOLUTE "cl"
#define OMPI_CXX_ABSOLUTE "cl"
#define OMPI_F77_ABSOLUTE "none"
#define OMPI_F90_ABSOLUTE "none"
#define OMPI_F90_BUILD_SIZE "small"

Step 4: Open the Open MPI project (.sln file) from the root directory
        of the distribution.

Step 5: Choose which version you want to build (from the project
        manager).

Step 6: Add the build directory to your PATH.

----------------------------------------------------------------------------

Third approach: Advanced users

The rest of this document is only for advanced users / developers;
i.e., those who has spare time or an urgent necessity to compile their
own Windows version of Open MPI.

Compiling Open MPI natively on Windows requires several tools.  Of
course, one need the Microsoft Visual Studio for their C/C++ compiler
as well as for the ml (assembler compiler) and the link utilities.
But the current version require some GNU tools as well.  Here is the
list of such tools:

1. Download any Unix for Windows environment. Tested environments include
    - cygwin http://www.cygwin.com/
    - minGW  http://www.mingw.org/
    - Windows Services for Unix (SFU)
             http://technet.microsoft.com/en-us/interopmigration/bb380242.aspx

2. Make the default shell ash.exe (install it if it's not installed by
   default) as it will highly decrease the configuration and
   compilation time.

3. Download a very recent Gnu Libtool (e.g., Libtool 2.2.6).

Now you should be set. The most difficult part is done. Just use your
favorite shell to get a window, go in the Open MPi directory and do
like in any UNIX environments: configure, make, and make install.

----------------------------------------------------------------------------

If you have any problems, find any bugs please feel free to report
them to Open MPI user's mailing list (see
http://www.open-mpi.org/community/lists/ompi.php).
