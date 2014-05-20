#include <iostream>

template <int...>
struct Indices
{};

template <int>
struct Int2Type
{};

template <typename F, typename Head, typename... Tail, int N, int... TailI>
void applyImpl(F f, Indices<N, TailI...>, Int2Type<N>, Head head, Tail... tail)
{
	std::cout << f(head) << '\n';
	applyImpl(f, Indices<TailI...>(), Int2Type<N + 1>(), tail...);
}

template <typename F, typename Head, typename... Tail, int HeadI, int... TailI, int N>
void applyImpl(F f, Indices<HeadI, TailI...>, Int2Type<N>, Head head, Tail... tail)
{
	std::cout << head << '\n';
	applyImpl(f, Indices<HeadI, TailI...>(), Int2Type<N + 1>(), tail...);
}

template <typename F, typename Head, typename... Tail, int N>
void applyImpl(F f, Indices<>, Int2Type<N>, Head head, Tail... tail)
{
	std::cout << head << '\n';
	applyImpl(f, Indices<>(), Int2Type<N>(), tail...);
}

template <typename F, int N>
void applyImpl(F f, Indices<>, Int2Type<N>)
{}

template <typename TT, typename F, typename... Args, int... I>
void apply(F f, Args... args)
{
	applyImpl(f, TT(), Int2Type<0>(), args...);
}

int times10(int x)
{
	return x * 10;
}

int main()
{
    apply<Indices<0,2> >(times10, 5, 6, 7, 8);
}
