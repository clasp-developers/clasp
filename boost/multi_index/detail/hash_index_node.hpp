/* Copyright 2003-2013 Joaquin M Lopez Munoz.
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *
 * See http://www.boost.org/libs/multi_index for library home page.
 */

#ifndef BOOST_MULTI_INDEX_DETAIL_HASH_INDEX_NODE_HPP
#define BOOST_MULTI_INDEX_DETAIL_HASH_INDEX_NODE_HPP

#if defined(_MSC_VER)
#pragma once
#endif

#include <boost/config.hpp> /* keep it first to prevent nasty warns in MSVC */
#include <boost/detail/allocator_utilities.hpp>

namespace boost{

namespace multi_index{

namespace detail{

/* Certain C++ requirements on unordered associative containers (see LWG issue
 * #579) imply a data structure where nodes are linked in a single list, which
 * in its turn forces implementors to add additional overhed per node to
 * associate each with its corresponding bucket. Others resort to storing hash
 * values, we use an alternative structure providing unconditional O(1)
 * manipulation, even in situations of unfair hash distribution, plus some
 * lookup speedups. For unique indices we maintain a doubly linked list of
 * nodes except that the first node N of a bucket is back-linked to its bucket
 * node, and this bucket node forward links to the node preceding N.
 *
 *        +---+   +---+       +---+   +---+
 *        |   +-->|   +-->    |   +-->|   +-->
 *   ...  | B0|   | B1|  ...  | B1|   | B2|  ...
 *     <--+   | +-+   |    <--+   | +-+   |
 *        +---+ | +---+       +---+ | +---+
 *          ^   |               ^   |
 *          |   |               |   |
 *          +-+ |               +-+ |
 *            | |                 | |
 *            + v                 + v
 *     --+---+---+---+--   --+---+---+---+--
 *   ... |   | B1|   |  ...  |   | B2|   | ...
 *     --+---+---+---+--   --+---+---+---+--
 *
 * The fist and last nodes of buckets can be checked with
 *
 *   first node of a bucket: Npn != N
 *   last node of a bucket:  Nnp != N
 *
 * (n and p short for ->next(), ->prior()). Pure insert and erase (without
 * lookup) can be unconditionally done in O(1).
 * For non-unique indices we add the following additional complexity: when
 * there is a group of 3 or more equivalent elements, they are linked as
 * follows:
 *
 *     +-----------------------+
 *     v                       |
 *   +---+   +---+       +---+ | +---+
 *   |   +-->|   |       |   +-+ |   |
 *   | F |   | S |  ...  | P |   | L |
 *   |   | +-+   |       |   |<--+   |
 *   +---+ | +---+       +---+   +---+
 *         |                       ^
 *         +-----------------------+
 * 
 * F, S, P and L are the first, second, penultimate and last node in the
 * group, respectively (S and P can coincide if the group has size 3.) This
 * arrangement is used to skip equivalent elements in O(1) when doing lookup,
 * while preserving O(1) insert/erase. The following invariants identify
 * special positions (some of the operations have to be carefully implemented
 * as Xpp is not valid if Xp points to a bucket):
 *
 *   first node of a bucket: Npnn  == N
 *   last node of a bucket:  Nnpn  == N
 *   first node of a group:  Nnp != N && Nnppn == N
 *   second node of a group: Npn != N && Nppnn == N
 *   n-1 node of a group:    Nnp != N && Nnnpp == N
 *   last node of a group:   Npn != N && Npnnp == N
 * 
 * The memory overhead is one pointer per bucket plus two pointers per node,
 * probably unbeatable. The resulting structure is bidirectonally traversable,
 * though currently we are just providing forward iteration.
 */

template<typename Allocator>
struct hashed_index_node_impl;

/* half-header (only next() pointer) to use for the bucket array */

template<typename Allocator>
struct hashed_index_base_node_impl
{
  typedef typename
  boost::detail::allocator::rebind_to<
      Allocator,hashed_index_base_node_impl
  >::type::pointer                          base_pointer;
  typedef typename
  boost::detail::allocator::rebind_to<
    Allocator,hashed_index_base_node_impl
  >::type::const_pointer                    const_base_pointer;
  typedef typename
  boost::detail::allocator::rebind_to<
    Allocator,
    hashed_index_node_impl<Allocator>
  >::type::pointer                          pointer;
  typedef typename
  boost::detail::allocator::rebind_to<
    Allocator,
    hashed_index_node_impl<Allocator>
  >::type::const_pointer                    const_pointer;

  pointer& next(){return next_;}
  pointer  next()const{return next_;}

private:
  pointer next_;
};

/* full header (next() and prior()) for the nodes */

template<typename Allocator>
struct hashed_index_node_impl:hashed_index_base_node_impl<Allocator>
{
private:
  typedef hashed_index_base_node_impl<Allocator> super;

public:  
  typedef typename super::base_pointer           base_pointer;
  typedef typename super::const_base_pointer     const_base_pointer;
  typedef typename super::pointer                pointer;
  typedef typename super::const_pointer          const_pointer;

  base_pointer& prior(){return prior_;}
  base_pointer  prior()const{return prior_;}

  static pointer pointer_from(base_pointer x)
  {
    return static_cast<pointer>(static_cast<hashed_index_node_impl*>(&*x));
  }

  static base_pointer base_pointer_from(pointer x)
  {
    return static_cast<base_pointer>(&*x);
  }

private:
  base_pointer prior_;
};

/* Boost.MultiIndex requires machinery to reverse unlink operations. A simple
 * way to make a pointer-manipulation function undoable is to templatize
 * its internal pointer assignments with a functor that, besides doing the
 * assignment, keeps track of the original pointer values and can later undo
 * the operations in reverse order.
 */

struct default_assigner
{
  template<typename T> void operator()(T& x,const T& val){x=val;}
};

template<typename Node>
struct unlink_undo_assigner
{
  typedef typename Node::base_pointer base_pointer;
  typedef typename Node::pointer      pointer;

  unlink_undo_assigner():pointer_track_count(0),base_pointer_track_count(0){}

  void operator()(pointer& x,pointer val)
  {
    pointer_tracks[pointer_track_count].x=&x;
    pointer_tracks[pointer_track_count++].val=x;
    x=val;
  }

  void operator()(base_pointer& x,base_pointer val)
  {
    base_pointer_tracks[base_pointer_track_count].x=&x;
    base_pointer_tracks[base_pointer_track_count++].val=x;
    x=val;
  }

  void operator()() /* undo op */
  {
    /* in the absence of aliasing, restitution order is immaterial */

    while(pointer_track_count--){
      *(pointer_tracks[pointer_track_count].x)=
        pointer_tracks[pointer_track_count].val;
    }
    while(base_pointer_track_count--){
      *(base_pointer_tracks[base_pointer_track_count].x)=
        base_pointer_tracks[base_pointer_track_count].val;
    }
  }

  struct pointer_track     {pointer*      x; pointer      val;};
  struct base_pointer_track{base_pointer* x; base_pointer val;};

  /* We know the maximum number of pointer and base pointer assignments that
   * the two unlink versions do, so we can statically reserve the needed
   * storage.
   */

  pointer_track      pointer_tracks[3];
  int                pointer_track_count;
  base_pointer_track base_pointer_tracks[2];
  int                base_pointer_track_count;
};

/* algorithmic stuff for unique and non-unique variants */

struct hashed_unique_tag{};
struct hashed_non_unique_tag{};

template<typename Node,typename Category>
struct hashed_index_node_alg;

template<typename Node>
struct hashed_index_node_alg<Node,hashed_unique_tag>
{
  typedef typename Node::base_pointer       base_pointer;
  typedef typename Node::const_base_pointer const_base_pointer;
  typedef typename Node::pointer            pointer;
  typedef typename Node::const_pointer      const_pointer;

  static bool is_first_of_bucket(pointer x)
  {
    return x->prior()->next()!=x;
  }

  static pointer after(pointer x)
  {
    return x->next();
  }

  static pointer after_local(pointer x)
  {
    return is_last_of_bucket(x)?pointer(0):after(x);
  }

  static pointer next_to_inspect(pointer x)
  {
    pointer y=x->next();
    if(y->prior()==base_pointer_from(x))return y;
    else return pointer(0); /* x last of bucket, bucket finished */
  }

  static void link(pointer x,base_pointer buc,pointer end)
  {
    if(buc->next()==pointer(0)){ /* empty bucket */
      x->next()=end->next();
      x->next()->prior()->next()=x;
      x->prior()=buc;
      buc->next()=end;
      end->next()=x;
    }
    else{
      x->next()=buc->next()->next();
      x->next()->prior()=base_pointer_from(x);
      x->prior()=buc;
      buc->next()->next()=x;
    }
  };

  static void unlink(pointer x)
  {
    default_assigner assign;
    unlink(x,assign);
  }

  typedef unlink_undo_assigner<Node> unlink_undo;

  template<typename Assigner>
  static void unlink(pointer x,Assigner& assign)
  {
    if(is_first_of_bucket(x)){
      if(is_last_of_bucket(x)){
        assign(x->prior()->next()->next(),x->next());
        assign(x->next()->prior()->next(),x->prior()->next());
        assign(x->prior()->next(),pointer(0));
      }
      else{
        assign(x->prior()->next()->next(),x->next());
        assign(x->next()->prior(),x->prior());
      }
    }
    else if(is_last_of_bucket(x)){
      assign(x->prior()->next(),x->next());
      assign(x->next()->prior()->next(),pointer_from(x->prior()));
    }
    else{
      assign(x->prior()->next(),x->next());
      assign(x->next()->prior(),x->prior());
    }
  }

private:
  static pointer pointer_from(base_pointer x)
  {
    return Node::pointer_from(x);
  }

  static base_pointer base_pointer_from(pointer x)
  {
    return Node::base_pointer_from(x);
  }

  static bool is_last_of_bucket(pointer x)
  {
    return x->next()->prior()!=base_pointer_from(x);
  }
};

template<typename Node>
struct hashed_index_node_alg<Node,hashed_non_unique_tag>
{
  typedef typename Node::base_pointer       base_pointer;
  typedef typename Node::const_base_pointer const_base_pointer;
  typedef typename Node::pointer            pointer;
  typedef typename Node::const_pointer      const_pointer;

  static bool is_first_of_bucket(pointer x)
  {
    return x->prior()->next()->next()==x;
  }

  static bool is_first_of_group(pointer x)
  {
    return
      x->next()->prior()!=base_pointer_from(x)&&
      !is_first_of_bucket(x->next())&&
      pointer_from(x->next()->prior())->prior()->next()==base_pointer_from(x);
  }

  static pointer after(pointer x)
  {
    if(is_last_but_one_of_group(x)){
      return pointer_from(x->next()->next()->prior());
    }
    else{
      return x->next();
    }
  }

  static pointer after_local(pointer x)
  {
    return is_last_of_bucket(x)?pointer(0):after(x);
  }

  static pointer next_to_inspect(pointer x)
  {
    pointer y=x->next();
    if(y->prior()==base_pointer_from(x))return y;
    pointer z=y->prior()->next();
    if(z==x||                      /* x last of bucket */
       z->prior()->next()!=z)      /* group(x) last of bucket */
      return pointer(0);           /* bucket finished */
    else return z;
  }

  static void link(pointer x,base_pointer buc,pointer end)
  {
    if(buc->next()==pointer(0)){ /* empty bucket */
      x->next()=end->next();
      x->next()->prior()->next()=x;
      x->prior()=buc;
      buc->next()=end;
      end->next()=x;
    }
    else{
      x->next()=buc->next()->next();
      x->next()->prior()=base_pointer_from(x);
      x->prior()=buc;
      buc->next()->next()=x;
    }
  };

  static void link(pointer x,pointer first,pointer last)
  {
    x->prior()=first->prior();
    x->next()=first;
    if(is_first_of_bucket(first)){
      x->prior()->next()->next()=x;
    }
    else{
      x->prior()->next()=x;
    }

    if(first==last){
      last->prior()=base_pointer_from(x);
    }
    else if(first->next()==last){
      first->prior()=base_pointer_from(last);
      first->next()=x;
    }
    else{
      pointer second=first->next(),
              lastbutone=pointer_from(last->prior());
      second->prior()=base_pointer_from(first);
      first->prior()=base_pointer_from(last);
      lastbutone->next()=x;
    }
  }

  static void link_range(
    pointer first,pointer last,base_pointer buc,pointer cend)
  {
    if(buc->next()==pointer(0)){ /* empty bucket */
      last->next()=cend->next();
      last->next()->prior()->next()=last;
      first->prior()=buc;
      buc->next()=cend;
      cend->next()=first;
    }
    else{
      last->next()=buc->next()->next();
      last->next()->prior()=base_pointer_from(last);
      first->prior()=buc;
      buc->next()->next()=first;
    }
  }

  static void unlink(pointer x)
  {
    default_assigner assign;
    unlink(x,assign);
  }

  typedef unlink_undo_assigner<Node> unlink_undo;

  template<typename Assigner>
  static void unlink(pointer x,Assigner& assign)
  {
    if(x->prior()->next()==x){
      if(x->next()->prior()==base_pointer_from(x)){
        left_unlink(x,assign);
        right_unlink(x,assign);
      }
      else if(x->next()->prior()->next()==x){            /* last of bucket */
        left_unlink(x,assign);
        right_unlink_last_of_bucket(x,assign);
      }
      else if(x->next()->next()==x){             /* first of group size==3 */
        left_unlink(x,assign);
        assign(x->next()->next(),pointer_from(x->next()->prior()));
        right_unlink(x,assign);
      }
      else if(
        x->next()->next()->prior()==
          base_pointer_from(x->next())){          /* first of group size>3 */
        left_unlink(x,assign);
        right_unlink_first_of_group_gt_3(x,assign);
      }
      else{                                         /* n-1 of group size>3 */
        unlink_last_but_one_of_group_gt_3(x,assign);
      }
    }
    else if(x->prior()->next()->next()==x){             /* first of bucket */
      if(x->next()->prior()==base_pointer_from(x)){
        left_unlink_first_of_bucket(x,assign);
        right_unlink(x,assign);
      }
      else if(x->next()->prior()->next()==x){            /* last of bucket */
        left_unlink_first_of_bucket(x,assign);
        assign(x->next()->prior()->next(),x->prior()->next());
        assign(x->prior()->next(),pointer(0));
      }
      else{                                              /* first of group */
        left_unlink_first_of_bucket(x,assign);
        right_unlink_first_of_group(x,assign);
      }
    }
    else if(x->prior()->next()->next()->prior()==
            base_pointer_from(x)){                        /* last of group */
      if(x->next()->prior()==base_pointer_from(x)){ 
        left_unlink_last_of_group(x,assign);
        right_unlink(x,assign);
      }
      else{                                              /* last of bucket */
        left_unlink_last_of_group(x,assign);
        right_unlink_last_of_bucket(x,assign);
      }
    }
    else{                                               /* second of group */
      unlink_second_of_group(x,assign);
    }
  }

private:
  static pointer pointer_from(base_pointer x)
  {
    return Node::pointer_from(x);
  }

  static base_pointer base_pointer_from(pointer x)
  {
    return Node::base_pointer_from(x);
  }

  static bool is_last_of_bucket(pointer x)
  {
    return x->next()->prior()->next()==x;
  }

  static bool is_last_but_one_of_group(pointer x)
  {
    return
      x->next()->prior()!=base_pointer_from(x)&&
      !is_last_of_bucket(x->next())&&
      pointer_from(x->next()->next()->prior())->prior()==base_pointer_from(x);
  }

  template<typename Assigner>
  static void left_unlink(pointer x,Assigner& assign)
  {
    assign(x->prior()->next(),x->next());
  }
  
  template<typename Assigner>
  static void right_unlink(pointer x,Assigner& assign)
  {
    assign(x->next()->prior(),x->prior());
  }

  template<typename Assigner>
  static void left_unlink_first_of_bucket(pointer x,Assigner& assign)
  {
    assign(x->prior()->next()->next(),x->next());
  }

  template<typename Assigner>
  static void right_unlink_last_of_bucket(pointer x,Assigner& assign)
  {
    assign(x->next()->prior()->next(),pointer_from(x->prior()));
  }

  template<typename Assigner>
  static void right_unlink_first_of_group(pointer x,Assigner& assign)
  {
    pointer second=x->next(),
            last=pointer_from(second->prior()),
            lastbutone=pointer_from(last->prior());
    if(second==lastbutone){
      assign(second->next(),last);
      assign(second->prior(),x->prior());
    }
    else{
      assign(lastbutone->next(),second);
      assign(second->next()->prior(),base_pointer_from(last));
      assign(second->prior(),x->prior());
    }
  }

  template<typename Assigner>
  static void right_unlink_first_of_group_gt_3(pointer x,Assigner& assign)
  {
    pointer second=x->next(),
            last=pointer_from(second->prior()),
            lastbutone=pointer_from(last->prior());
    assign(lastbutone->next(),second);
    assign(second->next()->prior(),base_pointer_from(last));
    assign(second->prior(),x->prior());
  }

  template<typename Assigner>
  static void left_unlink_last_of_group(pointer x,Assigner& assign)
  {
    pointer lastbutone=pointer_from(x->prior()),
            first=lastbutone->next(),
            second=first->next();
    if(lastbutone==second){
      assign(lastbutone->prior(),base_pointer_from(first));
      assign(lastbutone->next(),x->next());
    }
    else{
      assign(second->prior(),base_pointer_from(lastbutone));
      assign(lastbutone->prior()->next(),first);
      assign(lastbutone->next(),x->next());
    }
  }

  template<typename Assigner>
  static void unlink_last_but_one_of_group_gt_3(pointer x,Assigner& assign)
  {
    pointer first=x->next(),
            second=first->next(),
            last=pointer_from(second->prior());
    assign(last->prior(),x->prior());
    assign(x->prior()->next(),first);
  }

  template<typename Assigner>
  static void unlink_second_of_group(pointer x,Assigner& assign)
  {
    pointer last=pointer_from(x->prior()),
            lastbutone=pointer_from(last->prior()),
            first=lastbutone->next();
    if(lastbutone==x){
      assign(first->next(),last);
      assign(last->prior(),base_pointer_from(first));
    }
    else{
      assign(first->next(),x->next());
      assign(x->next()->prior(),base_pointer_from(last));
    }
  }
};

template<typename Super>
struct hashed_index_node_trampoline:
  hashed_index_node_impl<
    typename boost::detail::allocator::rebind_to<
      typename Super::allocator_type,
      char
    >::type
  >
{
  typedef typename boost::detail::allocator::rebind_to<
    typename Super::allocator_type,
    char
  >::type                                               impl_allocator_type;
  typedef hashed_index_node_impl<impl_allocator_type>   impl_type;
};

template<typename Super,typename Category>
struct hashed_index_node:
  Super,hashed_index_node_trampoline<Super>
{
private:
  typedef hashed_index_node_trampoline<Super> trampoline;

public:
  typedef typename trampoline::impl_type          impl_type;
  typedef hashed_index_node_alg<
    impl_type,Category>                           node_alg;
  typedef typename trampoline::base_pointer       impl_base_pointer;
  typedef typename trampoline::const_base_pointer const_impl_base_pointer;
  typedef typename trampoline::pointer            impl_pointer;
  typedef typename trampoline::const_pointer      const_impl_pointer;

  impl_pointer&      next(){return trampoline::next();}
  impl_pointer       next()const{return trampoline::next();}
  impl_base_pointer& prior(){return trampoline::prior();}
  impl_base_pointer  prior()const{return trampoline::prior();}

  impl_pointer impl()
  {
    return static_cast<impl_pointer>(
      static_cast<impl_type*>(static_cast<trampoline*>(this)));
  }

  const_impl_pointer impl()const
  {
    return static_cast<const_impl_pointer>(
      static_cast<const impl_type*>(static_cast<const trampoline*>(this)));
  }

  static hashed_index_node* from_impl(impl_pointer x)
  {
    return static_cast<hashed_index_node*>(
      static_cast<trampoline*>(&*x));
  }

  static const hashed_index_node* from_impl(const_impl_pointer x)
  {
    return static_cast<const hashed_index_node*>(
      static_cast<const trampoline*>(&*x));
  }

  /* interoperability with hashed_index_iterator */

  static void increment(hashed_index_node*& x)
  {
    x=from_impl(node_alg::after(x->impl()));
  }

  static void increment_local(hashed_index_node*& x)
  {
    x=from_impl(node_alg::after_local(x->impl()));
  }
};

} /* namespace multi_index::detail */

} /* namespace multi_index */

} /* namespace boost */

#endif
