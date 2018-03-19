/*
    File: rti_dds.cc
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

// ----------------------------------------------------------------------------
//  IMPLEMENTATION AND USAGE NOTES
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
//
//  Compile flag -DRTI_DDS_DEBUG_ENVVAR names an environment variable. This
//  extension checks if such named env var is defined. If it is, then
//  the entension will output debug info also on the standard error stream.
//
//  Default name of the env var: RTI_DDS_DEBUG
//
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
//  EXTENSION INCLUDES
// ----------------------------------------------------------------------------

#include <rti_dds/rti_dds.h>

// ----------------------------------------------------------------------------
//  OUTSIDE ANY NAMESPACE
// ----------------------------------------------------------------------------

// CLBIND_TRANSLATE_SYMBOL_TO_ENUM(DDS_ReturnCode_t, DDSPkg::_sym_STARddsReturnCodesSTAR);

// ----------------------------------------------------------------------------
//  NAMESPACE
// ----------------------------------------------------------------------------

namespace clasp_ext_rti_dds
{
  using namespace clbind;

  // -------------------------------------------------------------------------
  //  TYPEDEFS
  // -------------------------------------------------------------------------

  // None yet

  // -------------------------------------------------------------------------
  //  SPECIAL VARS
  // -------------------------------------------------------------------------

  static std::string      g_ext_rti_dds_version { CLASP_EXT_RTI_DDS_VERSION };
  static std::atomic_bool g_rti_dds_initialized{ false };
  static std::atomic_bool g_rti_dds_use_syslog{ true };
  static std::string      g_whoami{ CLASP_EXT_RTI_DDS_IDENT };
  static uint32_t         g_syslog_opt{ LOG_PID | LOG_NDELAY | LOG_NOWAIT };
  static std::atomic_int  g_expose_called_counter{ 0 };
  static std::atomic_bool g_rti_dds_shlibs_loaded{ false };
  static std::atomic_bool g_ext_rti_dds_enabled{ true };

  // -------------------------------------------------------------------------
  //  IMPLEMENTATION
  // -------------------------------------------------------------------------

  core::T_sp version() noexcept(false)
  {
    return core::SimpleBaseString_O::make( g_ext_rti_dds_version );
  }

  static void ext_rti_dds_log( uint32_t level,
                               const char * filename,
                               const uint32_t line_nr,
                               const uint32_t column_nr,
                               const char * const function_name,
                               const boost::format &fmt ) noexcept(false)
  {
    char * file_basename = basename( (char *) filename );

    TRY_BOOST_FORMAT_STRING(fmt, fmt_str);

    _lisp->debugLog().beginNode( DEBUG_LOG, file_basename, function_name, line_nr, column_nr, fmt_str );
    _lisp->debugLog().writeRaw( "~~~" );
    _lisp->debugLog().endNode( DEBUG_LOG );

    if( g_rti_dds_use_syslog )
    {
      if( level > LOG_DEBUG )
      {
        ext_rti_dds_log( LOG_ERR, __FILE__, __LINE__, 0, __func__, BF("Invalid log level %d received from %s (%s:%d)!") % function_name % filename % line_nr );
        level = LOG_NOTICE; // Setting to a manageable log level
      }
      syslog( level, "(fn %s)(%s:%d,%d) - %s",
              function_name, file_basename, line_nr, column_nr, fmt_str.c_str() );
    }

    if( getenv( RTI_DDS_DEBUG_ENVVAR ) != nullptr )
    {
      fprintf( stderr, "(fn %s)(%s:%d,%d) - %s\n",
               function_name, file_basename, line_nr, column_nr, fmt_str.c_str() );
    }

    return;
  }

  void ensure_rti_dds_libs_loaded() noexcept(false)
  {

    // auto result = core::do_dlsym( RTLD_DEFAULT, "NDDS_Config_Version_to_string" );
    // auto p_fn = std::get< 0 >( result );
    //
    // if( p_fn != nullptr )
    // {
    //   char *version_cstr = (char *)p_fn();
    //   ext_rti_dds_log( LOG_INFO, __FILE__, __LINE__, 0, __func__, BF("Using RTI DDS Connext version %s.") % version_cstr );
    // }
    // else
    // {
    //   SIMPLE_ERROR(BF("RTI Connext DDS Libraries don't seem to be loaded (could not get symbol for fn \"NDDS_Config_Version_to_string\"!"));
    // }
  }

} // namespace clasp_ext_rti_dds


#if ( CLASP_EXT_RTI_DDS_USE_MODERN_CXX_API == 1 )

namespace clasp_ext_rti_dds
{
  // --------------------------------------------------------------------------
  //   M O D E R N   C + +   A P I   - Currently DOES NOT WORK !
  // --------------------------------------------------------------------------
  // NOTE: SEE WAY DOWN IN THIS FILE FOR THE TRADITIONAL C++ API !!!

  // --- W O R K A R O U N D S ---

  dds::all::DomainParticipant new_DomainParticipant( int32_t domain_id ) noexcept(false)
  {
    return dds::all::DomainParticipant{ domain_id };
  }

  dds::all::Subscriber new_Subscriber( dds::all::DomainParticipant& participant ) noexcept(false)
  {
    dds::all::Subscriber subscriber{ participant };
    return subscriber;
  }

  // --- DEBUGGING AIDS ---

  // None currently

  // --- E X P O S I N G ---

  void expose_ext_rti_dds() noexcept(false)
  {
    if( g_expose_called_counter == 0 )
    {
      ext_rti_dds_log( LOG_INFO, __FILE__, __LINE__, 0, __func__, BF( "Exposing RTI DDS Connext Modern C++ to Common Lisp ..." ) );

      // This is a convenience namespace containing all standard OMG DDS
      // and also the RTI-specific symbols.
      using namespace dds::all;

      // --- DDS::CORE ---
      {
        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF( "Exposing namespace DDS::CORE ..." ) );

        package( dds::DDSCorePkg )
        [
          class_<Entity>("Entity", no_default_constructor),
          EXPOSE_BASE_CLASS(Duration),
          EXPOSE_BASE_CLASS(Exception),
          EXPOSE_CLASS(AlreadyClosedError,Exception),
          EXPOSE_CLASS(Error,Exception),
          EXPOSE_CLASS(IllegalOperationError,Exception),
          EXPOSE_CLASS(ImmutablePolicyError,Exception),
          EXPOSE_CLASS(InconsistentPolicyError,Exception),
          EXPOSE_CLASS(InvalidArgumentError,Exception),
          EXPOSE_CLASS(InvalidDowncastError,Exception),
          EXPOSE_CLASS(NotAllowedBySecError,Exception),
          EXPOSE_CLASS(NotEnabledError,Exception),
          EXPOSE_CLASS(OutOfResourcesError,Exception),
          EXPOSE_CLASS(PreconditionNotMetError,Exception),
          EXPOSE_CLASS(TimeoutError,Exception),
          EXPOSE_CLASS(UnsupportedError,Exception),
          EXPOSE_BASE_CLASS(BytesTopicType),
          EXPOSE_BASE_CLASS(StringTopicType),
          EXPOSE_BASE_CLASS(KeyedStringTopicType),
          EXPOSE_BASE_CLASS(KeyedBytesTopicType),
          EXPOSE_BASE_CLASS(Time),
          EXPOSE_BASE_CLASS(InstanceHandle),
          EXPOSE_BASE_CLASS(QosProvider)
        ];
      }

      // --- DDS::CORE::COND ---
      {
        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF( "Exposing namespace DDS::CORE::COND ..." ) );

        package( dds::core::DDSCoreCondPkg )
        [
          EXPOSE_BASE_CLASS(Condition),
          EXPOSE_CLASS(GuardCondition,Condition),
          EXPOSE_CLASS(StatusCondition,Condition),
          // .def_constructor("makeStatusCondition",constructor<dds::core::Entity&>()),
          EXPOSE_BASE_CLASS(WaitSet)
        ];
      }

      // --- DDS::CORE::XTYPES ---
      {
        using namespace rti::core::xtypes;

        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF( "Exposing namespace DDS::CORE::XTYPES ..." ) );

        package( dds::core::DDSCoreXtypesPkg )
        [
          EXPOSE_BASE_CLASS(DynamicType),
          EXPOSE_CLASS(AliasType,DynamicType),
          EXPOSE_CLASS(CollectionType,DynamicType),
          EXPOSE_CLASS(UnidimensionalCollectionTypeImpl,CollectionType),
          EXPOSE_CLASS(SequenceType,UnidimensionalCollectionTypeImpl),
          EXPOSE_CLASS(StringType,UnidimensionalCollectionTypeImpl),
          EXPOSE_CLASS(WStringType,UnidimensionalCollectionTypeImpl),
          EXPOSE_CLASS(ArrayType,CollectionType),
          EXPOSE_BASE_CLASS(DynamicData),
          EXPOSE_BASE_CLASS(LoanedDynamicData),
          EXPOSE_BASE_CLASS(DynamicDataInfo),
          EXPOSE_BASE_CLASS(DynamicDataMemberInfo),
          EXPOSE_BASE_CLASS(DynamicDataTypeSerializationProperty),
          EXPOSE_BASE_CLASS(DynamicType)
          // TODO: Implement types that require template args during expose
          // AbstractType requires template arg
          // EnumType requires template arg
          // StructType requires template arg
          // UnionType requires template arg
          // PrimitiveType requires template arg
        ];
      }

      // --- DDS::CORE::POLICY ---
      {
        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF( "Exposing namespace DDS::CORE::POLICY ..." ) );

        package( dds::core::DDSCorePolicyPkg )
        [
          class_<UserData>("UserData"),
          EXPOSE_BASE_CLASS(Durability),
          EXPOSE_BASE_CLASS(Presentation),
          EXPOSE_BASE_CLASS(Deadline),
          EXPOSE_BASE_CLASS(LatencyBudget),
          EXPOSE_BASE_CLASS(Ownership),
          EXPOSE_BASE_CLASS(OwnershipStrength),
          EXPOSE_BASE_CLASS(Liveliness),
          EXPOSE_BASE_CLASS(Partition),
          EXPOSE_BASE_CLASS(GroupData),
          EXPOSE_BASE_CLASS(TopicData),
          EXPOSE_BASE_CLASS(EntityFactory),
          EXPOSE_BASE_CLASS(TransportPriority),
          EXPOSE_BASE_CLASS(Lifespan),
          EXPOSE_BASE_CLASS(TimeBasedFilter),
          EXPOSE_BASE_CLASS(WriterDataLifecycle),
          EXPOSE_BASE_CLASS(ReaderDataLifecycle),
          EXPOSE_BASE_CLASS(Reliability),
          EXPOSE_BASE_CLASS(DestinationOrder),
          EXPOSE_BASE_CLASS(History),
          EXPOSE_BASE_CLASS(ResourceLimits),
          EXPOSE_BASE_CLASS(DurabilityService),
          EXPOSE_BASE_CLASS(TypeConsistencyEnforcement),
          EXPOSE_BASE_CLASS(QosPolicyCount)
        ];
      }

      // --- DDS::CORE::STATUS ---
      {
        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF( "Exposing namespace DDS::CORE::STATUS ..." ) );

        package( dds::core::DDSCoreStatusPkg )
        [
          EXPOSE_BASE_CLASS(SampleRejectedState),
          EXPOSE_BASE_CLASS(StatusMask),
          EXPOSE_BASE_CLASS(InconsistentTopicStatus),
          EXPOSE_BASE_CLASS(SampleLostStatus),
          EXPOSE_BASE_CLASS(SampleRejectedStatus),
          EXPOSE_BASE_CLASS(LivelinessLostStatus),
          EXPOSE_BASE_CLASS(LivelinessChangedStatus),
          EXPOSE_BASE_CLASS(OfferedDeadlineMissedStatus),
          EXPOSE_BASE_CLASS(RequestedDeadlineMissedStatus),
          EXPOSE_BASE_CLASS(OfferedIncompatibleQosStatus),
          EXPOSE_BASE_CLASS(RequestedIncompatibleQosStatus),
          EXPOSE_BASE_CLASS(PublicationMatchedStatus),
          EXPOSE_BASE_CLASS(SubscriptionMatchedStatus)
        ];
      }

      // --- DDS::DOMAIN ---
      {
        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF( "Exposing namespace DDS::DOMAIN ..." ) );

        package( dds::DDSDomainPkg )
        [
          EXPOSE_CLASS(DomainParticipant,Entity),
          // .def_constructor("new-domain-participant", constructor< int32_t >())
          def("new-domain-participant",new_DomainParticipant)
          // .def_constructor("new-domain-participant", constructor< int32_t, DomainParticipantQos&, DomainParticipantListener *, const StatusMask& >()),
          // EXPOSE_CLASS_2(DomainParticipantListener,PublisherListener,SubscriberListener)
        ];
      }

      // --- DDS::TOPIC ---
      {
        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF( "Exposing namespace DDS::TOPIC ..." ) );

        package( dds::DDSTopicPkg )
        [
          EXPOSE_BASE_CLASS(AnyTopic),
          EXPOSE_BASE_CLASS(ParticipantBuiltinTopicData),
          EXPOSE_BASE_CLASS(TopicBuiltinTopicData),
          EXPOSE_BASE_CLASS(PublicationBuiltinTopicData),
          EXPOSE_BASE_CLASS(SubscriptionBuiltinTopicData),
          EXPOSE_BASE_CLASS(BuiltinTopicKey),
          EXPOSE_BASE_CLASS(Filter)
          // EXPOSE_BASE_CLASS(TopicInstance),
          // EXPOSE_BASE_CLASS(TopicListener),
          // EXPOSE_BASE_CLASS(NoOpTopicListener),
          // EXPOSE_BASE_CLASS(TopicDescription)
        ];
      }

      // --- DDS::TOPIC::QOS ---
      {
        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF( "Exposing namespace DDS::TOPIC::QOS ..." ) );

        package( dds::topic::DDSTopicQosPkg )
        [
          EXPOSE_BASE_CLASS(TopicQos)
        ];
      }

      // --- DDS::PUB ---
      {
        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF( "Exposing namespace DDS::PUB ..." ) );

        package( dds::DDSPubPkg )
        [
          EXPOSE_BASE_CLASS(AnyDataWriter),
          EXPOSE_BASE_CLASS(AnyDataWriterListener),
          EXPOSE_CLASS(PublisherListener,AnyDataWriterListener),
          EXPOSE_BASE_CLASS(CoherentSet),
          EXPOSE_CLASS(Publisher,Entity)
          // EXPOSE_BASE_CLASS(SuspendedPublication)
        ];
      }

      // --- DDS::PUB::QOS ---
      {
        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF( "Exposing namespace DDS::PUB::QOS ..." ) );

        package( dds::pub::DDSPubQosPkg )
        [
         EXPOSE_BASE_CLASS(DataWriterQos),
         //.def_constructor("newDataWriterQos",constructor<>()),
         EXPOSE_BASE_CLASS(PublisherQos)
        ];
      }

      // --- DDS::SUB ---
      {
        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF( "Exposing namespace DDS::SUB ..." ) );

        package( dds::DDSSubPkg )
        [
          EXPOSE_BASE_CLASS(AnyDataReader),
          EXPOSE_BASE_CLASS(AnyDataReaderListener),
          EXPOSE_CLASS(SubscriberListener,AnyDataReaderListener),
          EXPOSE_CLASS(Subscriber,Entity)
        ];
      }

      // --- DDS::SUB::COND ---
      {
        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF( "Exposing namespace DDS::SUB::COND ..." ) );

        package( dds::sub::DDSSubCondPkg )
        [
          EXPOSE_CLASS(ReadCondition,Condition),
          EXPOSE_CLASS(QueryCondition,ReadCondition)
        ];
      }

      // --- DDS::SUB::QOS ---
      {
        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF("Exposing namespace DDS::SUB::QOS ..." ) );

        package( dds::sub::DDSSubQosPkg )
        [
          EXPOSE_BASE_CLASS(DataReaderQos),

          // See: https://community.rti.com/static/documentation/connext-dds/5.3.0/doc/api/connext_dds/api_cpp2/classdds_1_1sub_1_1qos_1_1SubscriberQos.html#a31d5f018023c57a4a14dae3fb54a4585  => THIS DOES NOT COMPILE
          class_<SubscriberQos>("SubscriberQos", no_default_constructor)
          .def( "presentation-policy",
                &decltype([] (dds::all::Subscriber& subscriber) { return subscriber.qos().policy< dds::core::policy::Presentation >(); })::operator() )
          //.def("liveliness-policy",(dds::core::policy::Liveliness(dds::sub::qos::SubscriberQos::*)())&dds::sub::qos::SubscriberQos::policy)
        ];

      }

      // --- FINI ---

      ext_rti_dds_log( LOG_INFO, __FILE__, __LINE__, 0, __func__, BF( "Done with exposing RTI Connext DDS Modern C++ ..." ) );
      g_expose_called_counter++;
    }
    else
    {
      g_expose_called_counter++;
      ext_rti_dds_log( LOG_NOTICE, __FILE__, __LINE__, 0, __func__, BF( "expose() called more than once ! (%d times) ... - NOT exposing again!" ) % g_expose_called_counter );
    }

  } // fn expose_ext_rti_dds

} // namespace clasp_ext_rti_dds

#else // Use Traditional C++ API of RTI Connext DDS
  // --------------------------------------------------------------------------
  //   T R A D I T I O N A L   C + +   A P I
  // --------------------------------------------------------------------------

  // --- TRANSLATORS ---

namespace translate
{
  using namespace clasp_ext_rti_dds;

  // NOTE:
  // from_object => Lisp -> C++
  // to_object   => C++  -> Lisp

  // --- DonainParticipantQos ---

  template <>
  struct from_object< const DDS_DomainParticipantQos&, std::true_type >
  {
    typedef const DDS_DomainParticipantQos& DeclareType;
    DeclareType _v;
    from_object( core::T_sp object ) : _v( object.nilp() ? DDS_PARTICIPANT_QOS_DEFAULT : *( from_object< DDS_DomainParticipantQos * >( object )._v  ) ) {};
  };

  // --- DomainParticipant ---

  // template <>
  // struct from_object< const DDS::DomainParticipant&, std::true_type >
  // {
  //   typedef const DDS::DomainParticipant& DeclareType;
  //   DeclareType _v;
  //   from_object( core::T_sp object ) : _v( object.nilp() ? nullptr : *( from_object< DDS::DomainParticipant * >( object )._v  ) ) {};
  // };

  // --- DomainParticipantListener ---

  template <>
  struct from_object< DDS::DomainParticipantListener *, std::true_type >
  {
    typedef DDS::DomainParticipantListener * DeclareType;
    DeclareType _v;
    from_object( core::T_sp object ) : _v( object.nilp() ? nullptr : ( from_object< DDS::DomainParticipantListener * >( object )._v ) ) {};
  };

} // namespace translate

namespace dds
{
  // INSTANCE CREATION HELPERS

  // Note: These functions are required as we aren't able
  // to have make-instance / clbind to this ...
  // TODO: Enable CLBIND to use default constructors.


} // namespace dds

namespace clasp_ext_rti_dds
{
  using namespace clbind;
  using namespace translate;

  // --- EXPOSER ---

  void expose_ext_rti_dds() noexcept(false)
  {
    if( g_expose_called_counter == 0 )
    {
      ext_rti_dds_log( LOG_INFO, __FILE__, __LINE__, 0, __func__, BF( "Exposing RTI DDS Connext Traditional C++ to Common Lisp ..." ) );

      // --- DEFPARAMETER & DEFVAR ---

      DDS::_sym__PLUS_dds_status_mask_all_PLUS_->defconstant( core::make_fixnum( DDS_STATUS_MASK_ALL ) );
      DDS::_sym__PLUS_dds_status_mask_none_PLUS_->defconstant( core::make_fixnum( DDS_STATUS_MASK_NONE ) );

      // --- DDS  ---
      {
        using namespace clbind;

        ext_rti_dds_log( LOG_DEBUG, __FILE__, __LINE__, 0, __func__, BF( "Exposing namespace DDS ..." ) );

        package( DDSPkg )
        [
          // TODO: Mark every RTI extension below wit h comment "RTI Connext DDS Extension"

          class_< DDS_EntityNameQosPolicy >( "DDS_EntityNameQosPolicy" )
          .def_readwrite( "name", &DDS_EntityNameQosPolicy::name )
          .def_readwrite( "role_name", &DDS_EntityNameQosPolicy::role_name )
          ,

          class_< DDS::Entity >( "Entity", no_default_constructor )
          .def( "enable", &DDS::Entity::enable )
          .def( "get_statuscondition", &DDS::Entity::get_statuscondition )
          .def( "get_status_changes", &DDS::Entity::get_status_changes )
          .def( "get_instance_handle", &DDS::Entity::get_instance_handle )
          ,

          // class_< DDS::DomainEntity, DDS::Entity >( "DomainEntity", no_default_constructor )
          // ,

          class_< DDS::Listener >( "Listener", no_default_constructor )
          ,

          class_< DDS::DomainParticipant, DDS::Entity >( "DomainParticipant", no_default_constructor )
          .def( "get_default_datawriter_qos", &DDS::DomainParticipant::get_default_datawriter_qos )
          .def( "set_default_datawriter_qos", &DDS::DomainParticipant::set_default_datawriter_qos )
          .def( "set_default_datawriter_qos_with_profile", &DDS::DomainParticipant::set_default_datawriter_qos_with_profile )
          .def( "get_default_datareader_qos", &DDS::DomainParticipant::get_default_datareader_qos )
          .def( "set_default_datareader_qos", &DDS::DomainParticipant::set_default_datareader_qos )
          .def( "set_default_datareader_qos_with_profile", &DDS::DomainParticipant::set_default_datareader_qos_with_profile )
          .def( "get_default_flowcontroller_property", &DDS::DomainParticipant::get_default_flowcontroller_property )
          .def( "set_default_flowcontroller_property", &DDS::DomainParticipant::set_default_flowcontroller_property )
          .def( "register_contentfilter", &DDS::DomainParticipant::register_contentfilter )
          .def( "lookup_contentfilter", &DDS::DomainParticipant::lookup_contentfilter )
          .def( "unregister_contentfilter", &DDS::DomainParticipant::unregister_contentfilter )
          .def( "get_default_library", &DDS::DomainParticipant::get_default_library )
          .def( "get_default_profile", &DDS::DomainParticipant::get_default_profile )
          .def( "get_default_profile_library", &DDS::DomainParticipant::get_default_profile_library )
          .def( "set_default_library", &DDS::DomainParticipant::set_default_library )
          .def( "set_default_profile", &DDS::DomainParticipant::set_default_profile )
          .def( "get_default_topic_qos", &DDS::DomainParticipant::get_default_topic_qos )
          .def( "set_default_topic_qos", &DDS::DomainParticipant::set_default_topic_qos )
          .def( "set_default_topic_qos_with_profile", &DDS::DomainParticipant::set_default_topic_qos_with_profile )
          .def( "get_default_publisher_qos", &DDS::DomainParticipant::get_default_publisher_qos )
          .def( "set_default_publisher_qos", &DDS::DomainParticipant::set_default_publisher_qos )
          .def( "set_default_publisher_qos_with_profile", &DDS::DomainParticipant::set_default_publisher_qos_with_profile )
          .def( "create_publisher", &DDS::DomainParticipant::create_publisher )
          .def( "create_publisher_with_profile", &DDS::DomainParticipant::create_publisher_with_profile )
          .def( "delete_publisher", &DDS::DomainParticipant::delete_publisher )
          .def( "create_subscriber", &DDS::DomainParticipant::create_subscriber )
          .def( "create_subscriber_with_profile", &DDS::DomainParticipant::create_subscriber_with_profile )
          .def( "delete_subscriber", &DDS::DomainParticipant::delete_subscriber )
          .def( "get_publishers", &DDS::DomainParticipant::get_publishers )
          .def( "get_subscribers", &DDS::DomainParticipant::get_subscribers )
          .def( "create_topic", &DDS::DomainParticipant::create_topic )
          .def( "create_topic_with_profile", &DDS::DomainParticipant::create_topic_with_profile )
          .def( "delete_topic", &DDS::DomainParticipant::delete_topic )
          .def( "create_contentfilteredtopic", &DDS::DomainParticipant::create_contentfilteredtopic )
          .def( "create_contentfilteredtopic_with_filter", &DDS::DomainParticipant::create_contentfilteredtopic_with_filter )
          .def( "delete_contentfilteredtopic", &DDS::DomainParticipant::delete_contentfilteredtopic )
          .def( "create_multitopic", &DDS::DomainParticipant::create_multitopic )
          .def( "delete_multitopic", &DDS::DomainParticipant::delete_multitopic )
          .def( "find_topic", &DDS::DomainParticipant::find_topic )
          .def( "lookup_topicdescription", &DDS::DomainParticipant::lookup_topicdescription )
          .def( "create_flowcontroller", &DDS::DomainParticipant::create_flowcontroller )
          .def( "delete_flowcontroller", &DDS::DomainParticipant::delete_flowcontroller )
          .def( "lookup_flowcontroller", &DDS::DomainParticipant::lookup_flowcontroller )
          .def( "get_builtin_subscriber", &DDS::DomainParticipant::get_builtin_subscriber )
          .def( "ignore_participant", &DDS::DomainParticipant::ignore_participant )
          .def( "ignore_topic", &DDS::DomainParticipant::ignore_topic )
          .def( "ignore_publication", &DDS::DomainParticipant::ignore_publication )
          .def( "ignore_subscription", &DDS::DomainParticipant::ignore_subscription )
          .def( "get_domain_id", &DDS::DomainParticipant::get_domain_id )
          .def( "get_current_time", &DDS::DomainParticipant::get_current_time )
          .def( "register_durable_subscription", &DDS::DomainParticipant::register_durable_subscription )
          .def( "delete_durable_subscription", &DDS::DomainParticipant::delete_durable_subscription )
          .def( "assert_liveliness", &DDS::DomainParticipant::assert_liveliness )
          .def( "resume_endpoint_discovery", &DDS::DomainParticipant::resume_endpoint_discovery )
          .def( "delete_contained_entities", &DDS::DomainParticipant::delete_contained_entities )
          .def( "get_discovered_participants", &DDS::DomainParticipant::get_discovered_participants )
          .def( "get_discovered_participant_data", &DDS::DomainParticipant::get_discovered_participant_data )
          .def( "get_discovered_topics", &DDS::DomainParticipant::get_discovered_topics )
          .def( "get_discovered_topic_data", &DDS::DomainParticipant::get_discovered_topic_data )
          .def( "contains_entity", &DDS::DomainParticipant::contains_entity )
          .def( "get_participant_protocol_status", &DDS::DomainParticipant::get_participant_protocol_status )
          .def( "set_qos", &DDS::DomainParticipant::set_qos )
          .def( "set_qos_with_profile", &DDS::DomainParticipant::set_qos_with_profile )
          .def( "get_qos", &DDS::DomainParticipant::get_qos )
          .def( "add_peer", &DDS::DomainParticipant::add_peer )
          .def( "remove_peer", &DDS::DomainParticipant::remove_peer )
          .def( "set_listener", &DDS::DomainParticipant::set_listener )
          .def( "get_listener", &DDS::DomainParticipant::get_listener )
          .def( "get_implicit_publisher", &DDS::DomainParticipant::get_implicit_publisher )
          .def( "get_implicit_subscriber", &DDS::DomainParticipant::get_implicit_subscriber )
          .def( "create_datawriter", &DDS::DomainParticipant::create_datawriter )
          .def( "create_datawriter_with_profile", &DDS::DomainParticipant::create_datawriter_with_profile )
          .def( "delete_datawriter", &DDS::DomainParticipant::delete_datawriter )
          .def( "create_datareader", &DDS::DomainParticipant::create_datareader )
          .def( "create_datareader_with_profile", &DDS::DomainParticipant::create_datareader_with_profile )
          .def( "delete_datareader", &DDS::DomainParticipant::delete_datareader )
          .def( "lookup_publisher_by_name", &DDS::DomainParticipant::lookup_publisher_by_name )
          .def( "lookup_subscriber_by_name", &DDS::DomainParticipant::lookup_subscriber_by_name )
          .def( "lookup_datawriter_by_name", &DDS::DomainParticipant::lookup_datawriter_by_name )
          .def( "lookup_datareader_by_name", &DDS::DomainParticipant::lookup_datareader_by_name )
          ,

          class_< DDS::DomainParticipantFactory >( "DomainParticipantFactory", no_default_constructor )
          .def( "set_default_participant_qos", &DDS::DomainParticipantFactory::set_default_participant_qos )
          .def( "set_default_participant_qos_with_profile", &DDS::DomainParticipantFactory::set_default_participant_qos_with_profile )
          .def( "get_default_participant_qos", &DDS::DomainParticipantFactory::get_default_participant_qos )
          .def( "set_default_library", &DDS::DomainParticipantFactory::set_default_library )
          .def( "get_default_library", &DDS::DomainParticipantFactory::get_default_library )
          .def( "set_default_profile", &DDS::DomainParticipantFactory::set_default_profile )
          .def( "get_default_profile", &DDS::DomainParticipantFactory::get_default_profile )
          .def( "get_default_profile_library", &DDS::DomainParticipantFactory::get_default_profile_library )
          .def( "get_participant_factory_qos_from_profile", &DDS::DomainParticipantFactory::get_participant_factory_qos_from_profile )
          .def( "get_participant_qos_from_profile", &DDS::DomainParticipantFactory::get_participant_qos_from_profile )
          .def( "get_publisher_qos_from_profile", &DDS::DomainParticipantFactory::get_publisher_qos_from_profile )
          .def( "get_subscriber_qos_from_profile", &DDS::DomainParticipantFactory::get_subscriber_qos_from_profile )
          .def( "get_datawriter_qos_from_profile", &DDS::DomainParticipantFactory::get_datawriter_qos_from_profile )
          .def( "get_datawriter_qos_from_profile_w_topic_name", &DDS::DomainParticipantFactory::get_datawriter_qos_from_profile_w_topic_name )
          .def( "get_datareader_qos_from_profile", &DDS::DomainParticipantFactory::get_datareader_qos_from_profile )
          .def( "get_datareader_qos_from_profile_w_topic_name", &DDS::DomainParticipantFactory::get_datareader_qos_from_profile_w_topic_name )
          .def( "get_topic_qos_from_profile", &DDS::DomainParticipantFactory::get_topic_qos_from_profile )
          .def( "get_topic_qos_from_profile_w_topic_name", &DDS::DomainParticipantFactory::get_topic_qos_from_profile_w_topic_name )
          .def( "get_qos_profile_libraries", &DDS::DomainParticipantFactory::get_qos_profile_libraries )
          .def( "get_qos_profiles", &DDS::DomainParticipantFactory::get_qos_profiles )
          .def( "create_participant", &DDS::DomainParticipantFactory::create_participant )
          .def( "create_participant_with_profile", &DDS::DomainParticipantFactory::create_participant_with_profile )
          .def( "delete_participant", &DDS::DomainParticipantFactory::delete_participant )
          .def( "lookup_participant", &DDS::DomainParticipantFactory::lookup_participant )
          .def( "set_qos", &DDS::DomainParticipantFactory::set_qos )
          .def( "get_qos", &DDS::DomainParticipantFactory::get_qos )
          .def( "load_profiles", &DDS::DomainParticipantFactory::load_profiles )
          .def( "reload_profiles", &DDS::DomainParticipantFactory::reload_profiles )
          .def( "unload_profiles", &DDS::DomainParticipantFactory::unload_profiles )
          .def( "unregister_thread", &DDS::DomainParticipantFactory::unregister_thread )
          .def( "create_participant_from_config", &DDS::DomainParticipantFactory::create_participant_from_config )
          .def( "create_participant_from_config_w_params", &DDS::DomainParticipantFactory::create_participant_from_config_w_params )
          .def( "lookup_participant_by_name", &DDS::DomainParticipantFactory::lookup_participant_by_name )
          // .def( "register_type_support", &DDS::DomainParticipantFactory::register_type_support ) // TODO: Make CLBIND accept register_type_support
          .def( "get_participants", &DDS::DomainParticipantFactory::get_participants )
          .def( "set_thread_factory", &DDS::DomainParticipantFactory::set_thread_factory )
          ,def("domain_participant_factory_get_instance", &DDS::DomainParticipantFactory::get_instance)
          ,def("domain_participant_factory_finalize_instance", &DDS::DomainParticipantFactory::finalize_instance) // RTI DDS Connext Extennsion
          ,

          class_< DDS::TopicDescription >( "TopicDescription", no_default_constructor )
          .def( "get_type_name", &DDS::TopicDescription::get_type_name )
          .def( "get_name", &DDS::TopicDescription::get_name )
          .def( "get_participant", &DDS::TopicDescription::get_participant )
          ,

          class_< DDS::ContentFilteredTopic, DDS::TopicDescription >( "ContentFilteredTopic", no_default_constructor )
          .def( "get_filter_expression", &DDS::ContentFilteredTopic::get_filter_expression )
          .def( "get_expression_parameters", &DDS::ContentFilteredTopic::get_expression_parameters )
          .def( "set_expression_parameters", &DDS::ContentFilteredTopic::set_expression_parameters )
          .def( "set_expression", &DDS::ContentFilteredTopic::set_expression )
          .def( "append_to_expression_parameter", &DDS::ContentFilteredTopic::append_to_expression_parameter )
          .def( "remove_from_expression_parameter", &DDS::ContentFilteredTopic::remove_from_expression_parameter )
          .def( "get_related_topic", &DDS::ContentFilteredTopic::get_related_topic )
          ,def( "contentfiltered_topic_narrow", &DDS::ContentFilteredTopic::narrow )
          ,

          class_< DDS::MultiTopic, DDS::TopicDescription >( "MultiTopic", no_default_constructor )
          .def( "get_subscription_expression", &DDS::MultiTopic::get_subscription_expression )
          .def( "get_expression_parameters", &DDS::MultiTopic::get_expression_parameters )
          .def( "set_expression_parameters", &DDS::MultiTopic::set_expression_parameters )
          ,def( "mulit_topic_narrow", &DDS::MultiTopic::narrow )
          ,

          class_< DDS::Topic, /* DDS::DomainEntity, */ DDS::TopicDescription >( "Topic", no_default_constructor )
          .def( "get_inconsistent_topic_status", &DDS::Topic::get_inconsistent_topic_status )
          .def( "set_qos", &DDS::Topic::set_qos )
          .def( "set_qos_with_profile", &DDS::Topic::set_qos_with_profile )
          .def( "get_qos", &DDS::Topic::get_qos )
          .def( "set_listener", &DDS::Topic::set_listener )
          .def( "get_listener", &DDS::Topic::get_listener )
          ,def( "topic_narrow", &DDS::Topic::narrow )
          ,

          class_< DDS::TopicListener, DDS::Listener >( "TopicListener", no_default_constructor )
          .def( "on_inconsistent_topic", &DDS::TopicListener::on_inconsistent_topic )
          ,

          class_< DDS::ContentFilter >( "ContentFilter", no_default_constructor )
          .def( "compile", &DDS::ContentFilter::compile )
          .def( "evaluate", &DDS::ContentFilter::evaluate )
          .def( "finalize", &DDS::ContentFilter::finalize )


          // class_< DDS::DDSWriterContentFilter, DDS::ContentFilter >( "DDSWriterContentFilter", no_default_constructor )
          // .def( "writer_compile", &DDS::DDSWriterContentFilter::writer_compile )
          // .def( "writer_evaluate", &DDS::DDSWriterContentFilter::writer_evaluate )
          // .def( "writer_finalize", &DDS::DDSWriterContentFilter::writer_finalize )
          // .def( "writer_attach", &DDS::DDSWriterContentFilter::writer_attach )
          // .def( "writer_detach", &DDS::DDSWriterContentFilter::writer_detach )
          // .def( "writer_return_loan", &DDS::DDSWriterContentFilter::writer_return_loan )

          // DDS STRING SUPPORT

          ,def( "DDS_String_alloc", &DDS_String_alloc )
          ,def( "DDS_String_dup", &DDS_String_dup )
          ,def( "DDS_String_free", &DDS_String_free )

         ];
      }

      // --- FINI ---

      ext_rti_dds_log( LOG_INFO, __FILE__, __LINE__, 0, __func__, BF( "Done with exposing RTI Connext DDS Traditional C++ ..." ) );
      g_expose_called_counter++;
    }
    else
    {
      g_expose_called_counter++;
      ext_rti_dds_log( LOG_NOTICE, __FILE__, __LINE__, 0, __func__, BF( "expose() called more than once ! (%d times) ... - NOT exposing again!") % g_expose_called_counter );
    }

  } // fn expose_ext_rti_dds

} // namespace clasp_ext_rti_dds

// ----------------------------------------------------------------------------
#endif // -- CLASP_EXT_RTI_DDS_USE_MODERN_CXX_API
// ----------------------------------------------------------------------------

namespace clasp_ext_rti_dds
{
  using namespace core;
  using namespace clbind;

  void shutdown_ext_rti_dds() noexcept(false)
  {
    if( g_rti_dds_initialized )
    {
      ext_rti_dds_log( LOG_INFO, __FILE__, __LINE__, 0, __func__, BF( "Clasp extension RTI DDS shutting down ..." ) );
      closelog(); // TODO: Align with drmeister to see if we need to closelog ourselves ...
      g_rti_dds_initialized = false;
    }
    g_expose_called_counter = 0;
  }

  static void initialize_ext_rti_dds() noexcept(false)
  {
    if( ! g_rti_dds_initialized )
    {
      if( g_rti_dds_use_syslog )
      {
        openlog( g_whoami.c_str(), g_syslog_opt, LOG_LOCAL0 );
        std::atexit( shutdown_ext_rti_dds );
      }

      ensure_rti_dds_libs_loaded();
      expose_ext_rti_dds();

      g_rti_dds_initialized = true;

      ext_rti_dds_log( LOG_INFO, __FILE__, __LINE__, 0, __func__, BF( "Clasp extension RTI DDS now initialized." ) );
    }
  }

  static inline void ensure_ext_rti_dds_initialized()
  {
    initialize_ext_rti_dds();
  }

  static inline void startup_ext_rti_dds()
  {
    ensure_ext_rti_dds_initialized();
  }

  RTIDDSExposer_O::RTIDDSExposer_O( core::Lisp_sp lisp ) : Exposer_O( lisp, ClaspExtRTIDDSPkg )
  {
    startup_ext_rti_dds();
  };

  RTIDDSExposer_O::~RTIDDSExposer_O()
  {
    shutdown_ext_rti_dds();
  };

  void RTIDDSExposer_O::expose( core::Lisp_sp lisp,
                                core::Exposer_O::WhatToExpose what) const
  {
    // TODO: Cleanup interdependencies between Clasp and CAMDO.
    // Note The switch statement below shows that Clasp still has
    // CANDO code in it. This needs to be cleaned (low prio, though)
    // - frgo, 2018-01-07

    switch( what )
    {
      case candoFunctions:
      {
        expose_ext_rti_dds();
      }
      break;

      case candoGlobals:
      {
      }
      break;

      case pythonClasses:
      case pythonFunctions:
      case pythonGlobals:
      {
        IMPLEMENT_ME();
      }
      break;

      default:
          ext_rti_dds_log( LOG_ERR, __FILE__, __LINE__, 0, __func__, BF( "Clasp Extension RTI DDS' expose() function called with unknown qualifier (%s) !") % what );

        break;
    }

    return;
  }

  // INITIALIZATION REGISTRATION

  CL_INITIALIZER void initializer()
  {
    RTIDDSExposer_O* pkg = new RTIDDSExposer_O( _lisp );
    _lisp->installPackage( pkg );
  }

} // namespace clasp_ext_rti_dds

// ----------------------------------------------------------------------------
//  EOF
// ----------------------------------------------------------------------------
