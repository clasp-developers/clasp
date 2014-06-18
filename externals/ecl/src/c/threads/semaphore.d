/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    semaphore.d -- POSIX-like semaphores
*/
/*
    Copyright (c) 2011, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#define AO_ASSUME_WINDOWS98 /* We need this for CAS */
#include <ecl/ecl.h>
#include <ecl/internal.h>

#if !defined(AO_HAVE_fetch_and_add_full)
#error "Cannot implement semaphores without AO_fetch_and_add_full"
#endif

static ECL_INLINE void
FEerror_not_a_semaphore(cl_object semaphore)
{
        FEwrong_type_argument(@'mp::semaphore', semaphore);
}

cl_object
ecl_make_semaphore(cl_object name, cl_fixnum count)
{
	cl_object output = ecl_alloc_object(t_semaphore);
	output->semaphore.name = name;
	output->semaphore.counter = count;
	output->semaphore.queue_list = ECL_NIL;
	output->semaphore.queue_spinlock = ECL_NIL;
        return output;
}

@(defun mp::make-semaphore (&key name (count ecl_make_fixnum(0)))
@
{
	@(return ecl_make_semaphore(name, fixnnint(count)))
}
@)

cl_object
mp_semaphore_name(cl_object semaphore)
{
	cl_env_ptr env = ecl_process_env();
	unlikely_if (ecl_t_of(semaphore) != t_semaphore) {
		FEerror_not_a_semaphore(semaphore);
	}
        ecl_return1(env, semaphore->semaphore.name);
}

cl_object
mp_semaphore_count(cl_object semaphore)
{
	cl_env_ptr env = ecl_process_env();
	unlikely_if (ecl_t_of(semaphore) != t_semaphore) {
		FEerror_not_a_semaphore(semaphore);
	}
	ecl_return1(env, ecl_make_fixnum(semaphore->semaphore.counter));
}

cl_object
mp_semaphore_wait_count(cl_object semaphore)
{
	cl_env_ptr env = ecl_process_env();
	unlikely_if (ecl_t_of(semaphore) != t_semaphore) {
		FEerror_not_a_semaphore(semaphore);
	}
	ecl_return1(env, cl_length(semaphore->semaphore.queue_list));
}

@(defun mp::signal-semaphore (semaphore &optional (count ecl_make_fixnum(1)))
@
{
	cl_fixnum n = fixnnint(count);
        cl_env_ptr env = ecl_process_env();
	unlikely_if (ecl_t_of(semaphore) != t_semaphore) {
		FEerror_not_a_semaphore(semaphore);
	}
	AO_fetch_and_add((AO_t*)&semaphore->semaphore.counter, n);
	if (semaphore->semaphore.queue_list != ECL_NIL) {
		ecl_wakeup_waiters(env, semaphore, ECL_WAKEUP_ONE);
	}
        @(return)
}
@)

static cl_object
get_semaphore_inner(cl_env_ptr env, cl_object semaphore)
{
	cl_object output;
	ecl_disable_interrupts_env(env);
	do {
		cl_fixnum counter = semaphore->semaphore.counter;
		if (!counter) {
			output = ECL_NIL;
			break;
		}
		if (AO_compare_and_swap_full((AO_t*)&(semaphore->semaphore.counter),
					     (AO_t)counter, (AO_t)(counter-1))) {
			output = ecl_make_fixnum(counter);
			break;
		}
		ecl_process_yield();
	} while (1);
	ecl_enable_interrupts_env(env);
	return output;
}

cl_object
mp_wait_on_semaphore(cl_object semaphore)
{
        cl_env_ptr env = ecl_process_env();
	cl_object output;
	unlikely_if (ecl_t_of(semaphore) != t_semaphore) {
		FEerror_not_a_semaphore(semaphore);
	}
	output = get_semaphore_inner(env, semaphore);
	if (Null(output)) {
		output = ecl_wait_on(env, get_semaphore_inner, semaphore);
	}
	ecl_return1(env, output);
}
