/*
    File: rti_dds.h
*/

/*
Copyright (c) 2018, Christian E. Schafmeister

CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

See directory 'clasp/licenses' for full details.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */

#if !defined( _CLASP_EXT_RTI_DDS_H_ )
#define _CLASP_EXT_RTI_DDS_H_

// ----------------------------------------------------------------------------
//  EXTENSION SEPCIFIC INCLUDES
// ----------------------------------------------------------------------------

#if !defined( CLASP_USE_EXT_RTI_DDS )
#error "CLASP_USE_EXT_RTI_DDS not defined - see wscript.config.template for more info! (Needs to be set to 1 to use RTI DDS Extension."
#endif

#if ( CLASP_USE_EXT_RTI_DDS == 1 )

#if !defined( CLASP_EXT_RTI_DDS_USE_MODERN_CXX_API  )
#if !defined( SCRAPING )
#warning "CLASP_EXT_RTI_DDS_USE_MODERN_CXX_API not defined - using RTI Connext DDS Traditional C++ API !"
#endif
#define CLASP_EXT_RTI_DDS_USE_MODERN_CXX_API 0
#endif

#if ( CLASP_EXT_RTI_DDS_USE_MODERN_CXX_API == 1 )
// See: https://community.rti.com/static/documentation/connext-dds/5.3.0/doc/api/connext_dds/api_cpp2/group__DDSNamespaceModule.html
// #include <dds/dds.hpp>
#include <rti/rti.hpp>
#else // use RTI Connext DDS Traditional C++ API
#include "ndds/ndds_cpp.h"
#define NDDS_EXCLUDE_PRIMITIVE_TYPES_FROM_NAMESPACE
#include "ndds/ndds_namespace_cpp.h"
#endif //  CLASP_EXT_RTI_DDS_USE_MODERN_CXX_API

#undef Package           // hack for working around a bug in RTI Connext DDS ...
#undef Public            // hack for working around a bug in RTI Connext DDS ...
#define log_makeheader_h // hack for working around a bug in RTI Connext DDS ...

// ----------------------------------------------------------------------------
//  SYSTEM INCLUDES
// ----------------------------------------------------------------------------

#include <vector>
#include <string>
#include <memory>
#include <atomic>
#include <map>
#include <cstdint>
#include <syslog.h>
#include <libgen.h>
#include <dlfcn.h>
#ifdef _TARGET_OS_DARWIN
#import <mach-o/dyld.h>
#endif

// ----------------------------------------------------------------------------
//  OTHER INCLUDES
// ----------------------------------------------------------------------------

// #include <boost/format.hpp>

// ----------------------------------------------------------------------------
//  CLASP INCLUDES
// ----------------------------------------------------------------------------

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/lisp.h>
#include <clasp/core/fli.h>
#include <clasp/core/compiler.h>
#include <clasp/core/package.h>
#include <clasp/asttooling/expose_tools.h>
#include <clasp/gctools/gc_interface.h>

#include <clasp/clbind/class.h>
#include <clasp/clbind/clbind.h>
#include <clasp/core/wrappers.h> // last include is wrappers.h

// ----------------------------------------------------------------------------
//  GLOBAL DEFINES
// ----------------------------------------------------------------------------

#define CLASP_EXT_RTI_DDS_VERSION "A.01.00"
#define CLASP_EXT_RTI_DDS_IDENT   "CLASP.EXT.RTI-DDS"

// ----------------------------------------------------------------------------
//  C++ <-> Lisp NAMESPACE MAPPING
// ----------------------------------------------------------------------------

NAMESPACE_PACKAGE_ASSOCIATION(clasp_ext_rti_dds,ClaspExtRTIDDSPkg,"CLASP.EXT.RTI-DDS");

#if ( CLASP_EXT_RTI_DDS_USE_MODERN_CXX_API == 1 )

NAMESPACE_PACKAGE_ASSOCIATION(dds,DDSPkg,"DDS");

namespace dds {
  NAMESPACE_PACKAGE_ASSOCIATION(core,DDSCorePkg,"DDS.CORE");

  namespace core {
    NAMESPACE_PACKAGE_ASSOCIATION(cond,DDSCoreCondPkg,"DDS.CORE.COND");
    NAMESPACE_PACKAGE_ASSOCIATION(xtypes,DDSCoreXtypesPkg,"DDS.CORE.XTYPES");
    NAMESPACE_PACKAGE_ASSOCIATION(policy,DDSCorePolicyPkg,"DDS.CORE.POLICY");
    NAMESPACE_PACKAGE_ASSOCIATION(status,DDSCoreStatusPkg,"DDS.CORE.STATUS");
  }

  NAMESPACE_PACKAGE_ASSOCIATION(domain,DDSDomainPkg,"DDS.DOMAIN");

  namespace domain {
    NAMESPACE_PACKAGE_ASSOCIATION(qos,DDSDomainQosPkg,"DDS.DOMAIN.QOS");
  }

  NAMESPACE_PACKAGE_ASSOCIATION(topic,DDSTopicPkg,"DDS.TOPIC");

  namespace topic {
    NAMESPACE_PACKAGE_ASSOCIATION(qos,DDSTopicQosPkg,"DDS.TOPIC.QOS");
  }

  NAMESPACE_PACKAGE_ASSOCIATION(pub,DDSPubPkg,"DDS.PUB");

  namespace pub {
    NAMESPACE_PACKAGE_ASSOCIATION(qos,DDSPubQosPkg,"DDS.PUB.QOS");
  }

  NAMESPACE_PACKAGE_ASSOCIATION(sub,DDSSubPkg,"DDS.SUB");

  namespace sub {
    NAMESPACE_PACKAGE_ASSOCIATION(cond,DDSSubCondPkg,"DDS.SUB.COND");
    NAMESPACE_PACKAGE_ASSOCIATION(qos,DDSSubQosPkg,"DDS.SUB.QOS");
    NAMESPACE_PACKAGE_ASSOCIATION(status,DDSSubStatusPkg,"DDS.SUB.STATUS");
  }
}

NAMESPACE_PACKAGE_ASSOCIATION(rti,RTIPkg,"RTI");

namespace rti {
  NAMESPACE_PACKAGE_ASSOCIATION(core,RTICorePkg,"RTI.CORE");

  namespace core {
    NAMESPACE_PACKAGE_ASSOCIATION(builtin_profiles,RTICoreBuiltinProfilesPkg,"RTI.CORE.BUILTIN_PROFILES");

    namespace builtin_profiles {
      NAMESPACE_PACKAGE_ASSOCIATION(qos_lib,RTICoreBuiltinProfilesQosLibPkg,"RTI.CORE.BUILTIN_PROFILES.QOS_LIB");
    }

    NAMESPACE_PACKAGE_ASSOCIATION(xtypes,RTICoreXtypesPkg,"RTI.CORE.XTYPES");
  }

  NAMESPACE_PACKAGE_ASSOCIATION(sub,RTISubPkg,"RTI.SUB");
  NAMESPACE_PACKAGE_ASSOCIATION(config,RTIConfigPkg,"RTI.CONFIG");
  NAMESPACE_PACKAGE_ASSOCIATION(domain,RTIDomainPkg,"RTI.DOMAIN");
  NAMESPACE_PACKAGE_ASSOCIATION(pub,RTIPubPkg,"RTI.PUB");
  NAMESPACE_PACKAGE_ASSOCIATION(topic,RTITopicPkg,"RTI.TOPIC");
  NAMESPACE_PACKAGE_ASSOCIATION(util,RTIUtilPkg,"RTI.UTIL");
}

// ----------------------------------------------------------------------------
#else // Use RTI Connext DDS Traditinal C++ API
// ----------------------------------------------------------------------------

// --- PACKAGES ---

NAMESPACE_PACKAGE_ASSOCIATION(DDS,DDSPkg,"DDS");

// --- SYMBOLS ---

// enum DDS_ReturnCode_t
SYMBOL_EXPORT_SC_( DDSPkg, STARddsReturnCodesSTAR );


// enum DDS_StatusKind
SYMBOL_EXPORT_SC_(DDSPkg, STARddsStatusKindsSTAR);

SYMBOL_EXPORT_SC_( DDSPkg, dds_inconsistent_topic_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_offered_deadline_missed_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_requested_deadline_missed_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_offered_incompatible_qos_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_requested_incompatible_qos_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_sample_lost_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_sample_rejected_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_data_on_readers_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_data_available_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_liveliness_lost_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_liveliness_changed_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_publication_matched_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_subscriptiopn_matched_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_service_request_accepted_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_data_writer_application_acknowledgment_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_data_writer_instance_replaced_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_reliable_writer_cache_changed_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_reliable_writer_activity_changed_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_data_writer_cache_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_data_writer_protocol_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_data_reader_cache_status );
SYMBOL_EXPORT_SC_( DDSPkg, dds_data_reader_protocol_status );

SYMBOL_EXPORT_SC_( DDSPkg, _PLUS_dds_status_mask_all_PLUS_ );
SYMBOL_EXPORT_SC_( DDSPkg, _PLUS_dds_status_mask_none_PLUS_ );

// --- ENUMS ---

// * DDS_ReturnCode_t
// CL_BEGIN_ENUM(DDS::DDS_ReturnCode_t,_sym_STARddsReturnCodesSTAR, "DDS::DDS_ReturnCode_t");
//CL_VALUE_ENUM(_sym_DW_TAG_hi_user, llvm::dwarf::DW_TAG_hi_user);;
// CL_END_ENUM(_sym_STARdwarfConstantsSTAR);

// * DDS_StatusKind
CL_BEGIN_ENUM( DDS::StatusKind,DDS::_sym_STARddsStatusKindsSTAR, "DDS::StatusKind" );

CL_VALUE_ENUM( DDS::_sym_dds_inconsistent_topic_status,                      DDS::StatusKind::DDS_INCONSISTENT_TOPIC_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_offered_deadline_missed_status,                 DDS::StatusKind::DDS_OFFERED_DEADLINE_MISSED_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_requested_deadline_missed_status,               DDS::StatusKind::DDS_REQUESTED_DEADLINE_MISSED_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_offered_incompatible_qos_status,                DDS::StatusKind::DDS_OFFERED_INCOMPATIBLE_QOS_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_requested_incompatible_qos_status,              DDS::StatusKind::DDS_REQUESTED_INCOMPATIBLE_QOS_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_sample_lost_status,                             DDS::StatusKind::DDS_SAMPLE_LOST_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_sample_rejected_status,                         DDS::StatusKind::DDS_SAMPLE_REJECTED_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_data_on_readers_status,                         DDS::StatusKind::DDS_DATA_ON_READERS_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_data_available_status,                          DDS::StatusKind::DDS_DATA_AVAILABLE_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_liveliness_lost_status,                         DDS::StatusKind::DDS_LIVELINESS_LOST_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_liveliness_changed_status,                      DDS::StatusKind::DDS_LIVELINESS_CHANGED_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_publication_matched_status,                     DDS::StatusKind::DDS_PUBLICATION_MATCHED_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_subscriptiopn_matched_status,                   DDS::StatusKind::DDS_SUBSCRIPTION_MATCHED_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_service_request_accepted_status,                DDS::StatusKind::DDS_SERVICE_REQUEST_ACCEPTED_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_data_writer_application_acknowledgment_status,  DDS::StatusKind::DDS_DATA_WRITER_APPLICATION_ACKNOWLEDGMENT_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_data_writer_instance_replaced_status,           DDS::StatusKind::DDS_DATA_WRITER_INSTANCE_REPLACED_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_reliable_writer_cache_changed_status,           DDS::StatusKind::DDS_RELIABLE_WRITER_CACHE_CHANGED_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_reliable_writer_activity_changed_status,        DDS::StatusKind::DDS_RELIABLE_READER_ACTIVITY_CHANGED_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_data_writer_cache_status ,                      DDS::StatusKind::DDS_DATA_WRITER_CACHE_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_data_writer_protocol_status,                    DDS::StatusKind::DDS_DATA_WRITER_PROTOCOL_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_data_reader_cache_status,                       DDS::StatusKind::DDS_DATA_READER_CACHE_STATUS );
CL_VALUE_ENUM( DDS::_sym_dds_data_reader_protocol_status,                    DDS::StatusKind::DDS_DATA_READER_PROTOCOL_STATUS );

CL_END_ENUM( DDS::_sym_STARddsStatusKindsSTAR );


// ----------------------------------------------------------------------------
#endif // CLASP_EXT_RTI_DDS_USE_MODERN_CXX_API
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
//  TRANSLATOR SUPPORT
// ----------------------------------------------------------------------------

SYMBOL_EXPORT_SC_( KeywordPkg, dds_participant_default_qos );

// ----------------------------------------------------------------------------
//  EXPOSER for CLASP EXTENSION RTI DDS
// ----------------------------------------------------------------------------

namespace clasp_ext_rti_dds
{
  CL_DEFUN core::T_sp version();    // Returns the version string of this ext

  // NOT USED ...
  // CL_DEFUN void enable_rti_dds();             // Enables this ext
  // CL_DEFUN void disable_rti_dds();            // Disables this ext
  // CL_DEFUN core::T_sp rti_dds_enabled_p();    // Checks wheather this ext is
                                                 // esnabled - returns T ot NIL

  class RTIDDSExposer_O : public core::Exposer_O
  {
    LISP_CLASS(clasp_ext_rti_dds,ClaspExtRTIDDSPkg,RTIDDSExposer_O,"RTIDDSExposer",core::Exposer_O);

  public:

    RTIDDSExposer_O( core::Lisp_sp lisp );

    RTIDDSExposer_O() = delete;
    ~RTIDDSExposer_O();

    virtual void expose( core::Lisp_sp lisp, WhatToExpose what ) const;
  };

  void expose_ext_rti_dds() noexcept(false);

};

#endif //  CLASP_USE_EXT_RTI_DDS

// ----------------------------------------------------------------------------
//  EOF
// ----------------------------------------------------------------------------

#endif // _CLASP_EXTENSIONS_RTI_DDS_EXPOSE_H_
