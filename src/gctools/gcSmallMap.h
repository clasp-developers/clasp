#ifndef gc_gcSmallMap_H
#define gc_gcSmallMap_H


namespace gctools {

    struct SmallMapGetError : public std::exception {};

    template <class Key, class Value, typename Allocator>
    class GCSmallMap : public GCVector<pair<Key,Value>,Allocator> {
    public:
        typedef Key                     key_type;
        typedef Value                   mapped_type;
        typedef pair<Key,Value>         value_type;
        typedef GCVector<value_type,Allocator>    Base;
        typedef value_type*             iterator;
        typedef value_type const*       const_iterator;
    public:
        const_iterator find(Key k) const {
            const_iterator it;
            for ( it=this->begin(); it!=this->end(); ++it ) {
                if ( it->first == k ) {
                    return it;
                }
            }
            return it;
        }

        iterator find(Key k) {
            iterator it;
            for ( it=this->begin(); it!=this->end(); ++it ) {
                if ( it->first == k ) {
                    return it;
                }
            }
            return it;
        }

        
        bool contains(Key k) const {
            return this->find(k) != this->end();
        }

        int count(Key k) const {
            return (this->find(k) != this->end()) ? 1 : 0;
        }

        pair<iterator,bool> insert(const value_type& val)
        {
            iterator it = this->find(val.first);
            if ( it == this->end() ) {
                this->push_back(val);
                return make_pair(this->end()-1,true);
            }
            value_type temp = *this->begin();
            *this->begin() = *it;
            *it = temp;
            return make_pair(this->begin(),false);
        }

        bool remove(const key_type& k) {
            iterator it = this->find(k);
            if ( it == this->end() ) {
                return false;
            }
            this->Base::erase(it);
            return true;
        }

        mapped_type& get(const key_type& k) {
            iterator it = this->find(k);
            if ( it==this->end() ) {
                throw SmallMapGetError();
            }
            return it->second;
        }

        const mapped_type& get(const key_type& k) const{
            const_iterator it = this->find(k);
            if ( it==this->end() ) {
                throw SmallMapGetError();
            }
            return it->second;
        }

        mapped_type& operator[](const key_type& k) {
            return (*((this->insert(std::make_pair(k,mapped_type()))).first)).second;
        }

        mapped_type& operator[](key_type&& k) {
            return (*((this->insert(std::make_pair(k,mapped_type()))).first)).second;
        }


        void set(const key_type& k, const mapped_type& v ) {
            (*((this->insert(std::make_pair(k,mapped_type()))).first)).second = v;
        }
        
    };

            
} // namespace gctools

#endif
