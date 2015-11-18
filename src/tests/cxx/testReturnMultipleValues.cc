#include <stdio.h>

class SV {
public:
  SV(void *ptr) : p(ptr){};
  void *p;
#if 0
  SV(SV const & other) {
    p = other.p;
  }
#endif
};

class MV : public SV {
public:
  MV(void *ptr1, int nv) : SV(ptr1), n(nv){};
  int n;
};

__attribute__((noinline)) MV foo(void *ptr1, int n) {
  MV res(ptr1, n);
  return res;
}

int main(int argc, char *argv[]) {
  MV x(NULL, 1);
  x.p = NULL;
  x.n = 1;
  MV ret = foo(NULL, 1);
  printf("ret.p = %p\n", ret.p);
  printf("ret.n = %d\n", ret.n);
};
