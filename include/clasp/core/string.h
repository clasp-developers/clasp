namespace core {

  extern bool clasp_isupper(claspCharacter cc);
  extern bool clasp_islower(claspCharacter cc);
  
template <class StringType>
int template_string_case(const StringType& s) {
  int upcase = 0;
  for (typename StringType::const_iterator it = s.begin(); it!=s.end(); ++it ) {
    claspCharacter cc = static_cast<claspCharacter>(*it);
    if (clasp_isupper(cc)) {
      if (upcase < 0) return 0;
      upcase = +1;
    } else if (clasp_islower(cc)) {
      if (upcase > 0) return 0;
      upcase = -1;
    }
  }
  return upcase;
}

inline int clasp_string_case(SimpleString_sp s) {
  if (SimpleBaseCharString_sp sb = s.asOrNull<SimpleBaseCharString_O>())
    return template_string_case(*sb);
  return template_string_case(*gc::As_unsafe<SimpleCharacterString_sp>(s));
}
inline int clasp_string_case(StrNs_sp s) {
  if (Str8Ns_sp sb = s.asOrNull<Str8Ns_O>())
    return template_string_case(*sb);
  return template_string_case(*gc::As_unsafe<StrWNs_sp>(s));
}
inline int clasp_string_case(String_sp s) {
  if (SimpleString_sp sb = s.asOrNull<SimpleString_O>())
    return clasp_string_case(sb);
  return clasp_string_case(gc::As_unsafe<StrNs_sp>(s));
}

};
