
#ifdef SCRAPING
#define BEGIN_TAG BEGIN_TAG_bfc54f90bafadf5
#define END_TAG END_TAG_bfc54f90bafadf5
#define LAMBDA(...) BEGIN_TAG LAMBDA_BEGIN #__VA_ARGS__ END_TAG
#define DOCSTRING(...) BEGIN_TAG DOCSTRING_BEGIN __VA_ARGS__ END_TAG
#define DECLARE(...) BEGIN_TAG DECLARE_BEGIN #__VA_ARGS__ END_TAG
#define CL_DEFUN BEGIN_TAG EXPOSE_FUNCTION __FILE__ __LINE__
#define CL_NAMESPACE BEGIN_TAG NAMESPACE
#define SYMBOL_SC_(pkg,name) BEGIN_TAG SYMBOL_INTERNAL pkg name END_TAG
#define SYMBOL_EXPORT_SC_(pkg,name) BEGIN_TAG SYMBOL_EXTERNAL pkg name END_TAG
#define INTERN_(ns,name) BEGIN_TAG SYMBOL_INTERN ns name END_TAG
#define NAMESPACE_PACKAGE_ASSOCIATION(ns,pkg,pkgname) BEGIN_TAG NAMESPACE_PACKAGE_ASSOCIATION ns pkg pkgname END_TAG
#else
#define LAMBDA(...)
#define DOCSTRING(...)
#define DECLARE(...)
#define CL_DEFUN
#define CL_NAMESPACE
#define SYMBOL_SC_(pkg,name)
#define SYMBOL_EXPORT_SC_(pkg,name)
#define INTERN_(ns,name) (ns::_sym_##name)
#define NAMESPACE_PACKAGE_ASSOCIATION(x, y, z) \
  static const std::string y = z;              \
  namespace x {                                \
  static const std::string CurrentPkg = z;     \
  }
#endif
