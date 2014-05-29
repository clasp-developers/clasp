int compute_factorial(int n)
{
    if (n <= 1)
        return 1;

    int f = n;
    while (--n > 1)
        f *= n;
    return f;
}


int main(int argc, char** argv)
{
    if (argc < 2)
        return -1;
    char firstletter = argv[1][0];
    int result = compute_factorial(firstletter - '0');

    // Returned result is clipped at 255...
    return result;
}
