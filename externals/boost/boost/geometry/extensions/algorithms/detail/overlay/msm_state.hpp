// Boost.Geometry (aka GGL, Generic Geometry Library)

// Copyright (c) 2007-2012 Barend Gehrels, Amsterdam, the Netherlands.

// Use, modification and distribution is subject to the Boost Software License,
// Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


#ifndef BOOST_GEOMETRY_ALGORITHMS_DETAIL_OVERLAY_MSM_STATE_HPP
#define BOOST_GEOMETRY_ALGORITHMS_DETAIL_OVERLAY_MSM_STATE_HPP



#ifdef USE_MSM_MINI

#  include <boost/msm/back/mini_state_machine.hpp>
#  include <boost/fusion/include/vector.hpp>

#else

#  include <boost/msm/back/state_machine.hpp>
#  include <boost/msm/front/state_machine_def.hpp>

#endif



// Events
struct starting {};
struct visit {};
struct finish {};


// Flags
struct is_init {};
struct is_visited {};

enum StatesEnum
{
    STATE_NONE=0,
    STATE_IS_INIT=1,
    STATE_IS_VISITED=2
};



#ifndef USE_MSM_MINI

// front-end: define the FSM structure
struct traverse_state_ : public boost::msm::front::state_machine_def<traverse_state_>
{
    traverse_state_():m_state(STATE_IS_INIT){}
    // The list of FSM states
    struct Init : public boost::msm::front::state<>
    {
        typedef boost::mpl::vector1<is_init> flag_list;
        //template <class Event,class FSM>
        //void on_entry(Event const&,FSM& fsm) {fsm.m_state=STATE_IS_INIT;}
    };

    struct Started : public boost::msm::front::state<>
    {
        //template <class Event,class FSM>
        //void on_entry(Event const&,FSM& fsm) {fsm.m_state=STATE_NONE;}
    };

    struct Visited : public boost::msm::front::state<>
    {
        typedef boost::mpl::vector1<is_visited> flag_list;
        //template <class Event,class FSM>
        //void on_entry(Event const&,FSM& fsm) {fsm.m_state=STATE_IS_VISITED;}
    };

    struct Finished : public boost::msm::front::state<>
    {
        typedef boost::mpl::vector1<is_visited> flag_list;
        //template <class Event,class FSM>
        //void on_entry(Event const&,FSM& fsm) {fsm.m_state=STATE_IS_VISITED;}
    };


    // the initial state of the player SM. Must be defined
    typedef Init initial_state;

    // transition actions
    void start_traverse(starting const&)      {m_state=STATE_NONE;}
    void finish_after_visit(finish const&)    {m_state=STATE_IS_VISITED;}
    void do_finish(finish const&)             {m_state=STATE_IS_VISITED;}
    void do_visit(visit const&)               {m_state=STATE_IS_VISITED;}
    void do_visit2(visit const&)              {m_state=STATE_IS_VISITED;}
    void do_nothing(finish const&)            {m_state=STATE_IS_VISITED;}


    typedef traverse_state_ p; // makes transition table cleaner

    // Transition table for player
    struct transition_table : mpl::vector
        <
            //    Start     Event         Next      Action               Guard
            //  +---------+-------------+---------+---------------------+----------------------+
          a_row < Init   , starting  , Started    , &p::start_traverse                    >,
          a_row < Init   , visit  , Visited    , &p::do_visit                    >,
          a_row < Init   ,  finish  , Finished  , &p::do_nothing                 >,
          a_row < Started   , finish  , Finished    , &p::do_finish                    >,
          a_row < Started   , visit  , Visited    , &p::do_visit2                    >,
            //  +---------+-------------+---------+---------------------+----------------------+
          a_row < Visited , finish        , Finished , &p::finish_after_visit                       >
            //  +---------+-------------+---------+---------------------+----------------------+
        > {};

    // Replaces the default no-transition response.
    template <class Machine, class Event>
    void no_transition(Event const& e, Machine&, int state)
    {
        //std::cout << "no transition from state " << state << " on event " << typeid(e).name() << std::endl;
    }

    typedef int no_exception_thrown;
    typedef int no_message_queue;
    StatesEnum m_state;

};


typedef boost::msm::back::state_machine<traverse_state_> traverse_state;

#else

// mini-back-end


struct traverse_state : public boost::msm::back::mini::state_machine<traverse_state>
{
    traverse_state():m_state(STATE_IS_INIT){}

    // The list of FSM states
    enum states
    {
        Init, Started, Visited, Finished
        , initial_state = Init
    };

      friend class boost::msm::back::mini::state_machine<traverse_state>;
      typedef traverse_state p; // makes transition table cleaner

      // transition actions
      void start_traverse(starting const&)      {m_state=STATE_NONE;}
      void finish_after_visit(finish const&)    {m_state=STATE_IS_VISITED;}
      void do_finish(finish const&)             {m_state=STATE_IS_VISITED;}
      void do_visit(visit const&)               {m_state=STATE_IS_VISITED;}
      void do_visit2(visit const&)              {m_state=STATE_IS_VISITED;}
      void do_nothing(finish const&)            {m_state=STATE_IS_VISITED;}

        bool flag_none() const { return m_state == STATE_IS_INIT; }
        bool flag_visited() const { return m_state == STATE_IS_VISITED; }


      // Transition table
      struct transition_table : mpl::vector6<
          //    Start     Event         Next      Action
          //  +---------+-------------+---------+---------------------+
          row < Init    , starting    , Started , &p::start_traverse  >,
          row < Init    , visit       , Visited , &p::do_visit        >,
          row < Init    ,  finish     , Finished, &p::do_nothing      >,
          row < Started , finish      , Finished, &p::do_finish       >,
          row < Started , visit       , Visited , &p::do_visit2       >,
          row < Visited , finish      , Finished, &p::finish_after_visit>
          //  +---------+-------------+---------+---------------------+
      > {};

      // Replaces the default no-transition response.
      template <class Event>
      int no_transition(int state, Event const& e)
      {
          std::cout << "no transition from state " << state
                    << " on event " << typeid(e).name() << std::endl;
          return state;
      }
      StatesEnum m_state;

};

#endif


#endif // BOOST_GEOMETRY_ALGORITHMS_DETAIL_OVERLAY_MSM_STATE_HPP
