#ifndef OTFSHRINK_H
#define OTFSHRINK_H


#include <set>
#include <unistd.h>
using namespace std;


extern set< uint32_t > cpuMap;
extern bool inverse;

extern map< uint32_t, set< uint32_t > > replacementMap;

#endif /* OTFSHRINK_H */
