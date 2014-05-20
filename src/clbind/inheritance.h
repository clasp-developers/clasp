// Copyright Daniel Wallin 2009. Use, modification and distribution is
// subject to the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef CLBIND_INHERITANCE_090217_HPP
# define CLBIND_INHERITANCE_090217_HPP

# include <cassert>
# include <limits>
# include <map>
# include <memory>
# include <vector>
# include "clbind/typeid.h"
#include <clbind/class_rep.h>
# include <boost/scoped_ptr.hpp>

namespace clbind { namespace detail {

        typedef void*(*cast_function)(void*);


        class  cast_graph
        {
        public:
            cast_graph();
            ~cast_graph();

            // `src` and `p` here describe the *most derived* object. This means that
            // for a polymorphic type, the pointer must be cast with
            // dynamic_cast<void*> before being passed in here, and `src` has to
            // match typeid(*p).
            std::pair<void*, int> cast(
                void* p, class_id src, class_id target
                , class_id dynamic_id, void const* dynamic_ptr) const;
            void insert(class_id src, class_id target, cast_function cast);

        private:
            class impl;
            boost::scoped_ptr<impl> m_impl;
        };

// Maps a type_id to a class_id. Note that this actually partitions the
// id-space into two, using one half for "local" ids; ids that are used only as
// keys into the conversion cache. This is needed because we need a shared key
// even for types that hasn't been registered explicitly.
        class class_id_map
        {
        public:
            class_id_map();

            class_id get(type_id type) const;
            class_id get_local(type_id type);
            void put(class_id id, type_id type);

        private:
            typedef std::map<type_id, class_id> map_type;
            map_type m_classes;
            class_id m_local_id;

            static class_id const local_id_base;
        };

        inline class_id_map::class_id_map()
            : m_local_id(local_id_base)
        {}

        inline class_id class_id_map::get(type_id type) const
        {
            map_type::const_iterator i = m_classes.find(type);
            if (i == m_classes.end() || i->second >= local_id_base)
                return reg::unknown_class;
            return i->second;
        }

        inline class_id class_id_map::get_local(type_id type)
        {
            std::pair<map_type::iterator, bool> result = m_classes.insert(
                std::make_pair(type, 0));

            if (result.second)
                result.first->second = m_local_id++;

            assert(m_local_id >= local_id_base);

            return result.first->second;
        }

        inline void class_id_map::put(class_id id, type_id type)
        {
            assert(id < local_id_base);

            std::pair<map_type::iterator, bool> result = m_classes.insert(
                std::make_pair(type, 0));

            assert(
                result.second
                || result.first->second == id
                || result.first->second >= local_id_base
                );

            result.first->second = id;
        }

        class class_map
        {
        public:
            DECLARE_onHeapScanGCRoots();
            ClassRep_sp get(class_id id) const;
            void put(class_id id, ClassRep_sp cls);
            
        private:
            gctools::Vec0<ClassRep_sp> m_classes;
        };

        inline ClassRep_sp class_map::get(class_id id) const
        {
            if (id >= m_classes.size())
                return 0;
            return m_classes[id];
        }

        inline void class_map::put(class_id id, ClassRep_sp cls)
        {
            if (id >= m_classes.size())
                m_classes.resize(id + 1);
            m_classes[id] = cls;
        }

        template <class S, class T>
        struct static_cast_
        {
            static void* execute(void* p)
            {
                return static_cast<T*>(static_cast<S*>(p));
            }
        };

        template <class S, class T>
        struct dynamic_cast_
        {
            static void* execute(void* p)
            {
                return dynamic_cast<T*>(static_cast<S*>(p));
            }
        };

#if 0 // moved to core/foundation.h

// Thread safe class_id allocation.
        class_id allocate_class_id(type_id const& cls);

        template <class T>
        struct registered_class
        {
            static class_id const id;
        };

        template <class T>
        class_id const registered_class<T>::id = allocate_class_id(typeid(T));

        template <class T>
        struct registered_class<T const>
            : registered_class<T>
        {};
#endif
    }} // namespace clbind::detail

#endif // CLBIND_INHERITANCE_090217_HPP
