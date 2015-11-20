#include <stdio.h>
#include <stdint.h>

void a(int num) {
  void *ptr = (void *)(((uintptr_t)__builtin_alloca(16 + 15) + 15) & (~0xf));
  printf("ptr = %p\n", ptr);
  if (num == 0)
    return;
  a(num - 1);
}

int main(int argc, char *argv[]) {
  a(20);

  for (int i = 0; i < 32; ++i) {
    void *ptr = (void *)(((uintptr_t)i + 15) & (~0xf));
    printf("If alloca starts at %x --> 16-byte aligned starts at %p\n", i, ptr);
  }
}
