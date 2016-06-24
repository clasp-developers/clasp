
#ifdef SCRAPING
#define BEGIN_TAG BEGIN_TAG_bfc54f90bafadf5
#define END_TAG END_TAG_bfc54f90bafadf5
#define LAMBDA(...) BEGIN_TAG LAMBDA_TAG ( :FILE __FILE__ :LINE __LINE__ :LAMBDA-LIST  #__VA_ARGS__ )
#define DOCSTRING(...) BEGIN_TAG DOCSTRING_TAG ( :FILE __FILE__ :LINE __LINE__ )  __VA_ARGS__ END_TAG
#define DECLARE(...) BEGIN_TAG DECLARE_TAG ( :FILE __FILE__ :LINE __LINE__ :DECLARE #__VA_ARGS__ )
#define CL_DEFUN BEGIN_TAG EXPOSE_FUNCTION ( :FILE __FILE__ :LINE __LINE__ )
#define CL_DEFMETHOD BEGIN_TAG EXPOSE_METHOD ( :FILE __FILE__ :LINE __LINE__ )
#define SYMBOL_SC_(pkg,name) BEGIN_TAG SYMBOL_INTERNAL ( :PACKAGE #pkg :NAME #name )
#define SYMBOL_EXPORT_SC_(pkg,name) BEGIN_TAG SYMBOL_EXTERNAL ( :PACKAGE #pkg :NAME #name )
#define INTERN_(ns,name) BEGIN_TAG SYMBOL_INTERN ( :NAMESPACE #ns :NAME #name )
#define NAMESPACE_PACKAGE_ASSOCIATION(ns,pkg,pkgname) BEGIN_TAG NAMESPACE_PACKAGE_ASSOCIATION_TAG ( :NAMESPACE #ns :PACKAGE #pkg :PACKAGE-NAME pkgname )
#define LISP_BASE1(b) BEGIN_TAG LISP_BASE1_TAG ( :BASE #b )
#define LISP_CLASS(n,p,c,s) BEGIN_TAG LISP_CLASS_TAG ( :NAMESPACE #n :PACKAGE #p :CLASS #c :CLASS-SYMBOL s )
#define LISP_VIRTUAL_CLASS(n,p,c,s) BEGIN_TAG LISP_CLASS_TAG ( :NAMESPACE #n :PACKAGE #p :CLASS #c :CLASS-SYMBOL s )
#define LISP_EXTERNAL_CLASS(n,p,l,c,s,b) BEGIN_TAG LISP_EXTERNAL_CLASS_TAG ( :NAMESPACE #n :PACKAGE #p :CXXCLASS #l :CLASS #c :CLASS-SYMBOL s :BASE #b )
#else
#define LAMBDA(...)
#define DOCSTRING(...)
#define DECLARE(...)
#define CL_DEFUN
#define SYMBOL_SC_(pkg,name)
#define SYMBOL_EXPORT_SC_(pkg,name)
#define INTERN_(ns,name) (ns::_sym_##name)
#define NAMESPACE_PACKAGE_ASSOCIATION(x, y, z) \
  static const std::string y = z;              \
  namespace x {                                \
  static const std::string CurrentPkg = z;     \
  }
#endif
