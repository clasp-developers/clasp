/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    barrier.d -- wait barriers
*/
/*
    Copyright (c) 2012, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#define AO_ASSUME_WINDOWS98 /* We need this for CAS */
#include <ecl/ecl.h>
#include <ecl/internal.h>
#include "threads/ecl_atomics.h"

static ECL_INLINE void
FEerror_not_a_barrier(cl_object barrier)
{
        FEwrong_type_argument(@'mp::barrier', barrier);
}

cl_object
ecl_make_barrier(cl_object name, cl_index count)
{
	cl_object output = ecl_alloc_object(t_barrier);
	output->barrier.name = name;
	output->barrier.arrivers_count = count;
	output->barrier.count = count;
	output->barrier.queue_list = ECL_NIL;
	output->barrier.queue_spinlock = ECL_NIL;
        return output;
}

@(defun mp::make-barrier (count &key name)
@
	if (count == ECL_T)
		count = ecl_make_fixnum(MOST_POSITIVE_FIXNUM);
	@(return ecl_make_barrier(name, fixnnint(count)))
@)

cl_object
mp_barrier_name(cl_object barrier)
{
	cl_env_ptr env = ecl_process_env();
	unlikely_if (ecl_t_of(barrier) != t_barrier) {
		FEerror_not_a_barrier(barrier);
	}
        ecl_return1(env, barrier->barrier.name);
}

cl_object
mp_barrier_count(cl_object barrier)
{
	cl_env_ptr env = ecl_process_env();
	unlikely_if (ecl_t_of(barrier) != t_barrier) {
		FEerror_not_a_barrier(barrier);
	}
	ecl_return1(env, ecl_make_fixnum(barrier->barrier.count));
}

cl_object
mp_barrier_arrivers_count(cl_object barrier)
{
	cl_fixnum arrivers, count;
	cl_env_ptr env = ecl_process_env();
	unlikely_if (ecl_t_of(barrier) != t_barrier) {
		FEerror_not_a_barrier(barrier);
	}
	arrivers = barrier->barrier.arrivers_count;
	count = barrier->barrier.count;
	if (arrivers < 0)
		arrivers = 0; /* Disabled barrier */
	else
		arrivers = count - arrivers;
	ecl_return1(env, ecl_make_fixnum(arrivers));
}

@(defun mp::barrier-unblock (barrier &key reset_count disable kill_waiting)
	int ping_flags = ECL_WAKEUP_RESET_FLAG | ECL_WAKEUP_ALL;
	int kill_flags = ECL_WAKEUP_RESET_FLAG | ECL_WAKEUP_KILL | ECL_WAKEUP_ALL;
@
	unlikely_if (ecl_t_of(barrier) != t_barrier) {
		FEerror_not_a_barrier(barrier);
	}
	if (!Null(reset_count))
		barrier->barrier.count = fixnnint(reset_count);
	if (!Null(disable))
		barrier->barrier.arrivers_count = -1;
	else
		barrier->barrier.arrivers_count = barrier->barrier.count;
	ecl_wakeup_waiters(the_env, barrier,
			   Null(kill_waiting)? ping_flags : kill_flags);
        @(return)
@)

static cl_object
barrier_wait_condition(cl_env_ptr env, cl_object barrier)
{
	/* We were signaled */
	if (env->own_process->process.woken_up != ECL_NIL)
		return ECL_T;
	/* Disabled barrier */
	else if (barrier->barrier.arrivers_count < 0)
		return ECL_T;
	else
		return ECL_NIL;
}

static cl_fixnum
decrement_counter(cl_fixnum *counter)
{
	/* The logic is as follows:
	 *  - If the counter is negative, we abort. This is a way of
	 *    disabling the counter.
	 *  - Otherwise, we decrease the counter only if it is positive
	 *  - If the counter is currently zero, then we block. This
	 *    situation implies that some other thread is unblocking.
	 */
	cl_fixnum c;
	do {
		c = *counter;
		if (c < 0) {
			return c;
		} else if (c > 0) {
			if (AO_compare_and_swap_full((AO_t*)counter,
						     (AO_t)c, (AO_t)(c-1)))
				return c;
		}
	} while (1);
}

@(defun mp::barrier-wait (barrier &key)
	cl_object output;
	cl_fixnum counter;
@
{
	cl_object own_process = the_env->own_process;

	unlikely_if (ecl_t_of(barrier) != t_barrier) {
		FEerror_not_a_barrier(barrier);
	}
	ecl_disable_interrupts_env(the_env);
	counter = decrement_counter(&barrier->barrier.arrivers_count);
	if (counter == 0) {
		print_lock("barrier %p saturated", barrier, barrier);
		/* There are (count-1) threads in the queue and we
		 * are the last one. We thus unblock all threads and
		 * proceed. */
		mp_barrier_unblock(1, barrier);
		ecl_enable_interrupts_env(the_env);
		output = @':unblocked';
	} else if (counter > 0) {
		print_lock("barrier %p waiting", barrier, barrier);
		ecl_enable_interrupts_env(the_env);
		ecl_wait_on(the_env, barrier_wait_condition, barrier);
		output = ECL_T;
	} else {
		print_lock("barrier %p pass-through", barrier, barrier);
		/* Barrier disabled */
		output = ECL_NIL;
	}
	@(return output)
}
@)
