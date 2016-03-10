
#ifdef SCRAPING
#define BEGIN_TAG BEGIN_TAG_bfc54f90bafadf5
#define END_TAG END_TAG_bfc54f90bafadf5
#define CL_BEGIN_ENUM(type,symbol,desc) BEGIN_TAG BEGIN_ENUM_TAG ( :FILE __FILE__ :LINE __LINE__ :TYPE #type :SYMBOL #symbol :DESCRIPTION desc )
#define CL_VALUE_ENUM(symbol,value) BEGIN_TAG VALUE_ENUM_TAG ( :FILE __FILE__ :LINE __LINE__ :SYMBOL #symbol :VALUE #value )
#define CL_END_ENUM(symbol) BEGIN_TAG END_ENUM_TAG ( :FILE __FILE__ :LINE __LINE__ :SYMBOL #symbol )
#define CL_NAME(...) BEGIN_TAG NAME_TAG ( :FILE __FILE__ :LINE __LINE__ :CL-NAME #__VA_ARGS__ )
#define CL_LISPIFY_NAME(...) BEGIN_TAG LISPIFY_NAME_TAG ( :FILE __FILE__ :LINE __LINE__ :CL-NAME #__VA_ARGS__ )
#define CL_PKG_NAME(pkg,name) BEGIN_TAG PKG_NAME_TAG ( :FILE __FILE__ :LINE __LINE__ :CL-PKG #pkg :CL-NAME #name )
#define CL_LAMBDA(...) BEGIN_TAG LAMBDA_TAG ( :FILE __FILE__ :LINE __LINE__ :LAMBDA-LIST  #__VA_ARGS__ )
#define CL_DOCSTRING(...) BEGIN_TAG DOCSTRING_TAG ( :FILE __FILE__ :LINE __LINE__ )  __VA_ARGS__ END_TAG
#define CL_DECLARE(...) BEGIN_TAG DECLARE_TAG ( :FILE __FILE__ :LINE __LINE__ :DECLARE #__VA_ARGS__ )
#define CL_INITIALIZER BEGIN_TAG EXPOSE_INITIALIZE ( :FILE __FILE__ :LINE __LINE__ )
#define CL_DEFUN BEGIN_TAG EXPOSE_FUNCTION ( :FILE __FILE__ :LINE __LINE__ )
#define CL_DEFMETHOD BEGIN_TAG EXPOSE_METHOD ( :FILE __FILE__ :LINE __LINE__ )
#define CL_EXTERN_DEFMETHOD(type,pointer) BEGIN_TAG EXTERN_DEFMETHOD (:FILE __FILE__ :LINE __LINE__ :TYPE #type :POINTER #pointer ) 
#define CL_EXTERN_DEFUN(pointer) BEGIN_TAG EXTERN_DEFUN (:FILE __FILE__ :LINE __LINE__ :POINTER #pointer ) 
#define SYMBOL_SC_(pkg,name) BEGIN_TAG SYMBOL_INTERNAL ( :FILE __FILE__ :LINE __LINE__ :PACKAGE #pkg :NAME #name )
#define SYMBOL_EXPORT_SC_(pkg,name) BEGIN_TAG SYMBOL_EXTERNAL ( :FILE __FILE__ :LINE __LINE__  :PACKAGE #pkg :NAME #name )
//! Ensure that the symbol is present within the package - acts like CL:SHADOW
#define SYMBOL_SHADOW_EXPORT_SC_(pkg,name) BEGIN_TAG SYMBOL_SHADOW_EXTERNAL ( :FILE __FILE__ :LINE __LINE__  :PACKAGE #pkg :NAME #name )
#define DETAILED_SYMBOL_EXPORT_SC_(pkg,cxxname,name) BEGIN_TAG DETAILED_SYMBOL_EXTERNAL ( :FILE __FILE__ :LINE __LINE__  :PACKAGE #pkg :CXX-NAME #cxxname :NAME name )
#define INTERN_(ns,name) BEGIN_TAG SYMBOL_INTERN ( :FILE __FILE__ :LINE __LINE__ :NAMESPACE #ns :NAME #name )
#define PACKAGE_USE(name_str) BEGIN_TAG PACKAGE_USE_TAG ( :FILE __FILE__ :LINE __LINE__ :NAME name_str)
#define PACKAGE_NICKNAME(name_str) BEGIN_TAG PACKAGE_NICKNAME_TAG ( :FILE __FILE__ :LINE __LINE__ :NAME name_str)
#define NAMESPACE_PACKAGE_ASSOCIATION(ns,pkg,pkgname) BEGIN_TAG NAMESPACE_PACKAGE_ASSOCIATION_TAG ( :FILE __FILE__ :LINE __LINE__ :NAMESPACE #ns :PACKAGE #pkg :PACKAGE-NAME pkgname )
#define LISP_META_CLASS(...) BEGIN_TAG META_CLASS_TAG ( :FILE __FILE__ :LINE __LINE__ :META-CLASS  #__VA_ARGS__ )
#define LISP_CLASS(n,p,c,s,b) BEGIN_TAG LISP_CLASS_TAG ( :FILE __FILE__ :LINE __LINE__ :NAMESPACE #n :PACKAGE #p :CLASS #c :CLASS-SYMBOL s :BASE #b) \
       DETAILED_SYMBOL_EXPORT_SC_(p,c,s)
#define LISP_VIRTUAL_CLASS(n,p,c,s,b) BEGIN_TAG LISP_CLASS_TAG ( :FILE __FILE__ :LINE __LINE__ :NAMESPACE #n :PACKAGE #p :CLASS #c :CLASS-SYMBOL s :BASE #b ) \
       DETAILED_SYMBOL_EXPORT_SC_(p,c,s)
#define LISP_EXTERNAL_CLASS(n,p,l,c,s,b) BEGIN_TAG LISP_EXTERNAL_CLASS_TAG ( :FILE __FILE__ :LINE __LINE__ :NAMESPACE #n :PACKAGE #p :CXXCLASS #l :CLASS #c :CLASS-SYMBOL s :BASE #b ) \
       DETAILED_SYMBOL_EXPORT_SC_(p,c,s)
#else
#define CL_BEGIN_ENUM(type,symbol,desc)
#define CL_VALUE_ENUM(symbol,value)
#define CL_END_ENUM(symbol)
#define CL_NAME(...)
#define CL_LISPIFY_NAME(...)
#define CL_PKG_NAME(pkgid,name)
#define CL_LAMBDA(...)
#define CL_DOCSTRING(...)
#define CL_DECLARE(...)
#define CL_INITIALIZE
#define CL_DEFUN
#define CL_DEFMETHOD
#define CL_TYPE(...)
#define CL_EXTERN_DEFMETHOD(type,pointer)
#define CL_EXTERN_DEFUN(pointer)
#define CL_PKG_NAME(pkg,name)
#define SYMBOL_SC_(pkg,name)
#define SYMBOL_EXPORT_SC_(pkg,name)
#define SYMBOL_SHADOW_EXPORT_SC_(pkg,name)
#define INTERN_(ns,name) (ns::_sym_##name)
#define PACKAGE_USE(name_str)
#define PACKAGE_NICKNAME(name_str)
#define NAMESPACE_PACKAGE_ASSOCIATION(x, y, z) \
  static const std::string y = z;              \
  namespace x {                                \
  static const std::string CurrentPkg = z;     \
  }
#endif
