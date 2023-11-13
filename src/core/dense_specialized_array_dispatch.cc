#ifdef CODING
unsigned char coding[] = "@abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$";
#endif
#ifdef DISPATCHES
DISPATCH(float,float,"sf");
DISPATCH(double,double,"df");
DISPATCH(byte8_t,byte8_t,"b1");
DISPATCH(int8_t,int8_t,"i1");
DISPATCH(byte16_t,byte16_t,"b2");
DISPATCH(int16_t,int16_t,"i2");
DISPATCH(byte32_t,byte32_t,"b4");
DISPATCH(int32_t,int32_t,"i4");
DISPATCH(byte64_t,byte64_t,"b8");
DISPATCH(int64_t,int64_t,"i8");
DISPATCH(size_t,size_t,"st");
DISPATCH(fixnum,Fixnum,"fn");
#endif
