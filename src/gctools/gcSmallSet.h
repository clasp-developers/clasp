#ifndef gc_gcSmallSet_H
#define gc_gcSmallSet_H


namespace gctools {


    template <class Key, typename Allocator>
    class GCSmallSet : public GCVector<Key,Allocator> {
    public:
        typedef Key                     key_type;
        typedef Key                     value_type;
        typedef GCVector<value_type,Allocator>    Base;
        typedef value_type*             iterator;
        typedef value_type const*       const_iterator;
    public:
        const_iterator find(Key k) const {
            const_iterator it;
            for ( it=this->begin(); it!=this->end(); ++it ) {
                if ( *it == k ) {
                    return it;
                }
            }
            return it;
        }

        iterator find(Key k) {
            iterator it;
            for ( it=this->begin(); it!=this->end(); ++it ) {
                if ( *it == k ) {
                    return it;
                }
            }
            return it;
        }
        int indexOf(Key k) {
            iterator it;
            for ( it=this->begin(); it!=this->end(); ++it ) {
                if ( *it == k ) {
                    return it-this->begin();
                }
            }
            return it-this->begin();
        }

        bool contains(Key k) const {
            return this->find(k) != this->end();
        }

        int count(Key k) const {
            return this->find(k) != this->end() ? 1 : 0;
        }

        pair<iterator,bool> appendNew(const value_type& val)
        {
            iterator it = this->find(val);
            if ( it == this->end() ) {
                this->push_back(val);
                return std::make_pair(this->end()-1,true);
            }
            return std::make_pair(it,false);
        }

        pair<iterator,bool> insert( const value_type& val)
        {
            return this->appendNew(val);
        }

        size_t erase(const value_type& k)
        {
            iterator it = this->find(k);
            if ( it == this->end() ) {
                return 0;
            }
            this->Base::erase(it);
            return 1;
        }
    };

            
} // namespace gctools

#endif
