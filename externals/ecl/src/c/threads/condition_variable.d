/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    condition_variable.d -- Native threads.
*/
/*
    Copyright (c) 2003, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <ecl/ecl.h>
#include <ecl/internal.h>

/*----------------------------------------------------------------------
 * CONDITION VARIABLES
 */

cl_object
mp_make_condition_variable(void)
{
	cl_object output = ecl_alloc_object(t_condition_variable);
	output->condition_variable.queue_list = ECL_NIL;
	output->condition_variable.queue_spinlock = ECL_NIL;
	output->condition_variable.lock = ECL_NIL;
	@(return output)
}

static cl_object
condition_variable_wait(cl_env_ptr env, cl_object cv)
{
	cl_object lock = cv->condition_variable.lock;
	cl_object own_process = env->own_process;
	/* We have entered the queue and still own the mutex? */
	print_lock("cv lock %p is %p =? %p", cv, lock, lock->lock.owner, own_process);
	if (lock->lock.owner == own_process) {
		mp_giveup_lock(lock);
	}
	/* We always return when we have been explicitly awaken */
	return own_process->process.woken_up;
}

cl_object
mp_condition_variable_wait(cl_object cv, cl_object lock)
{
        cl_env_ptr env = ecl_process_env();
	cl_object own_process = env->own_process;
	unlikely_if (ecl_t_of(cv) != t_condition_variable) {
                FEwrong_type_nth_arg(@[mp::condition-variable-wait], 1, cv,
                                     @[mp::condition-variable]);
	}
	unlikely_if (ecl_t_of(lock) != t_lock) {
                FEwrong_type_nth_arg(@[mp::condition-variable-wait], 2, lock,
                                     @[mp::lock]);
	}
        unlikely_if (cv->condition_variable.lock != ECL_NIL &&
		     cv->condition_variable.lock != lock)
	{
                FEerror("Attempt to associate lock ~A~%with condition variable ~A,"
			"~%which is already associated to lock ~A", 2, lock,
			cv, cv->condition_variable.lock);
        }
        unlikely_if (lock->lock.owner != own_process) {
                FEerror("Attempt to wait on a condition variable using lock~%~S"
                        "~%which is not owned by process~%~S", 2, lock, own_process);
        }
        unlikely_if (lock->lock.counter > 1) {
                FEerror("mp:condition-variable-wait can not be used with recursive"
                        " locks:~%~S", 1, lock);
        }
	print_lock("waiting cv %p", cv, cv);
	cv->condition_variable.lock = lock;
	ecl_wait_on(env, condition_variable_wait, cv);
	mp_get_lock_wait(lock);
	@(return ECL_T)
}

cl_object
mp_condition_variable_timedwait(cl_object cv, cl_object lock, cl_object seconds)
{
	FEerror("Timed condition variables are not supported.", 0);
}

cl_object
mp_condition_variable_signal(cl_object cv)
{
	print_lock("signal cv %p", cv, cv);
	ecl_wakeup_waiters(ecl_process_env(), cv,
			   ECL_WAKEUP_RESET_FLAG | ECL_WAKEUP_ONE | ECL_WAKEUP_DELETE);
	@(return ECL_T)
}

cl_object
mp_condition_variable_broadcast(cl_object cv)
{
	print_lock("broadcast cv %p", cv);
	ecl_wakeup_waiters(ecl_process_env(), cv,
			   ECL_WAKEUP_RESET_FLAG | ECL_WAKEUP_ALL | ECL_WAKEUP_DELETE);
	@(return ECL_T)
}
