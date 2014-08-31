#ifndef values_fwd_H
#define values_fwd_H
namespace core
{
FORWARD(Number);
    FORWARD(Real);
    FORWARD(Rational);
    FORWARD(Integer);
    FORWARD(Fixnum);
    FORWARD(Float);
    FORWARD(ShortFloat);
    FORWARD(SingleFloat);
    FORWARD(DoubleFloat);
#ifdef CLASP_LONG_FLOAT
    FORWARD(LongFloat);
#endif
    FORWARD(Complex);
    FORWARD(Ratio);
    FORWARD(Bool);
}
#endif
