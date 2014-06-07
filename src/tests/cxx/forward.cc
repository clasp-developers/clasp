
template <class T>
struct GCKind {static const int Kind=0;};

template <class T>
class W;

class A {int z;};

class A;
class B;
template <> class W<A>;
template <> class W<B>;


template <> struct GCKind<A> { static const int Kind = 1; };
template <> struct GCKind<B> { static const int Kind = 2; };
template <> struct GCKind<W<A> > { static const int Kind = 3; };
template <> struct GCKind<W<B> > { static const int Kind = 4; };

