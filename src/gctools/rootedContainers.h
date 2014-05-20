


template <class _C_> rooted_vector : public vector<_C_>
{
private:
    typedef typename rooted_vector<_C_>* rooted_vector_ptr;
    rooted_vector_ptr	_next;
    rooted_vector_ptr 	_prev;
private:
    rooted_vector() : vector<_C_>()
    {
	_thread.insert_rooted_vector(this);
    };
}
