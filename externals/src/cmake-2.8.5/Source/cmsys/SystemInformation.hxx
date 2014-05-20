/*============================================================================
  KWSys - Kitware System Library
  Copyright 2000-2009 Kitware, Inc., Insight Software Consortium

  Distributed under the OSI-approved BSD License (the "License");
  see accompanying file Copyright.txt for details.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the License for more information.
============================================================================*/
#ifndef cmsys_SystemInformation_h
#define cmsys_SystemInformation_h


/* Define these macros temporarily to keep the code readable.  */
#if !defined (KWSYS_NAMESPACE) && !cmsys_NAME_IS_KWSYS
# define kwsys_stl cmsys_stl
# define kwsys_ios cmsys_ios
#endif
#include <cmsys/stl/string>
#include <stddef.h> /* size_t */

namespace cmsys
{


// forward declare the implementation class  
class SystemInformationImplementation;
  
class cmsys_EXPORT SystemInformation 
{

public:
  SystemInformation ();
  ~SystemInformation ();

  const char * GetVendorString();
  const char * GetVendorID();
  kwsys_stl::string GetTypeID();
  kwsys_stl::string GetFamilyID();
  kwsys_stl::string GetModelID();
  kwsys_stl::string GetSteppingCode();
  const char * GetExtendedProcessorName();
  const char * GetProcessorSerialNumber();
  int GetProcessorCacheSize();
  unsigned int GetLogicalProcessorsPerPhysical();
  float GetProcessorClockFrequency();
  int GetProcessorAPICID();
  int GetProcessorCacheXSize(long int);
  bool DoesCPUSupportFeature(long int);
  
  const char * GetOSName();
  const char * GetHostname();
  const char * GetOSRelease();
  const char * GetOSVersion();
  const char * GetOSPlatform();

  bool Is64Bits();

  unsigned int GetNumberOfLogicalCPU(); // per physical cpu
  unsigned int GetNumberOfPhysicalCPU();

  bool DoesCPUSupportCPUID();

  // Retrieve memory information in megabyte.
  size_t GetTotalVirtualMemory();
  size_t GetAvailableVirtualMemory();
  size_t GetTotalPhysicalMemory();
  size_t GetAvailablePhysicalMemory();  

  /** Run the different checks */
  void RunCPUCheck();
  void RunOSCheck();
  void RunMemoryCheck();
private:
  SystemInformationImplementation* Implementation;

};
} // namespace cmsys

/* Undefine temporary macros.  */
#if !defined (KWSYS_NAMESPACE) && !cmsys_NAME_IS_KWSYS
# undef kwsys_stl
# undef kwsys_ios
#endif

#endif
