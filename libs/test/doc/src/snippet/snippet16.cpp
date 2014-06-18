class const_string {
public:
    ...
    char operator[]( size_t index ) const;
    char at( size_t index ) const;
    ...
};
