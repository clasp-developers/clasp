//
// C++ code!!  Ain't it beautiful?
//

class Person {
};

class Room {
public:
  void add_person(Person person) {
    // do stuff
  }

private:
  Person *people_in_room;
};

template <class T, int N>
class Bag {
  int size;
  double bar;
};

int main() {
  Person *p = new Person();
  Bag<Person, 42> bagofpersons;
  return 0;
}
