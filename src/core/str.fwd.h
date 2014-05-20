#ifndef	_core_Str_fwd_H
#define _core_Str_fwd_H


namespace core
{

    FORWARD(Str);

    /*! Access functions to avoid having to include the entire class */
    string str_get(T_sp str);
    string str_get(Str_sp str);

    /*! Create a Str_O object */
    T_sp str_create(const string& val);

    /*! Create a Str_O object from a const char* */
    T_sp str_create(const char* val);

}; /* core */

#endif /* _core_Str_fwd_H */


