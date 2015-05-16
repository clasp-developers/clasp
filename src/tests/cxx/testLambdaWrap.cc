
void wrapRun(std::function f){};

class A {
  A(int z) : _z(z){};
  int foo(int x, int y) {
    int result;
    wrapRun([&result, x, y, this]() -> int {
                result = x + y + z;
    });
    return result;
  };
};

main(int argc, const char *argv[]) {
  A a(3);
  printf("Calling foo %d\n", a.foo(1, 2));
}
