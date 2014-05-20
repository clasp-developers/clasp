

template <int...IS>
struct Indices
{
    enum { Size = sizeof...(IS)};
};

template <int>
struct Int2Type
{};

template <typename Head, typename... Tail, int N, int... TailI>
void fillValuesImpl(core::T_sp mv[], Indices<N, TailI...>, Int2Type<N>, Head head, Tail... tail)
{
    mv[N] = translate::to_object<Head>::convert(head);
    fillValuesImpl(mv, Indices<TailI...>(), Int2Type<N + 1>(), tail...);
}

template <typename Head, typename... Tail, int HeadI, int... TailI, int N>
void fillValuesImpl(core::T_sp mv[], Indices<HeadI, TailI...>, Int2Type<N>, Head head, Tail... tail)
{
	fillValuesImpl(mv, Indices<HeadI, TailI...>(), Int2Type<N + 1>(), tail...);
}

template <typename Head, typename... Tail, int N>
void fillValuesImpl(core::T_sp mv[], Indices<>, Int2Type<N>, Head head, Tail... tail)
{
	fillValuesImpl(mv, Indices<>(), Int2Type<N>(), tail...);
}

template <int N>
void fillValuesImpl(core::T_sp mv[], Indices<>, Int2Type<N>)
{}



