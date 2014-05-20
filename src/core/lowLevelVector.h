#ifndef core_lowLevelVector_H
#define core_lowLevelVector_H



template <class T>
class MutableVector {
    unsigned int        _End;
    unsigned int        _Allocated;
    T                   _Data[];
};


#endif //lowLevelVector.h
