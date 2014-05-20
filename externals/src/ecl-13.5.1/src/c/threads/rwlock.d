/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    rwlock.d -- POSIX read-write locks
*/
/*
    Copyright (c) 2003, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#ifndef __sun__ /* See unixinit.d for this */
#define _XOPEN_SOURCE 600	/* For pthread mutex attributes */
#endif
#include <errno.h>
#include <ecl/ecl.h>
#ifdef ECL_WINDOWS_THREADS
# include <windows.h>
#else
# include <pthread.h>
#endif
#include <ecl/internal.h>

/*----------------------------------------------------------------------
 * READ/WRITE LOCKS
 */

static void
FEerror_not_a_rwlock(cl_object lock)
{
        FEwrong_type_argument(@'mp::rwlock', lock);
}

static void
FEunknown_rwlock_error(cl_object lock, int rc)
{
#ifdef ECL_WINDOWS_THREADS
        FEwin32_error("When acting on rwlock ~A, got an unexpected error.", 1, lock);
#else
        const char *msg = NULL;
        switch (rc) {
        case EINVAL:
                msg = "The value specified by rwlock is invalid";
                break;
        case EPERM:
                msg = "Read/write lock not owned by us";
                break;
        case EDEADLK:
                msg = "Thread already owns this lock";
                break;
        case ENOMEM:
                msg = "Out of memory";
                break;
        default:
                FElibc_error("When acting on rwlock ~A, got an unexpected error.",
                             1, lock);
        }
        FEerror("When acting on rwlock ~A, got the following C library error:~%"
                "~A", 2, lock, make_constant_base_string(msg));
#endif
}

cl_object
ecl_make_rwlock(cl_object name)
{
        const cl_env_ptr the_env = ecl_process_env();
	cl_object output = ecl_alloc_object(t_rwlock);
#ifdef ECL_RWLOCK
        int rc;
	ecl_disable_interrupts_env(the_env);
	rc = pthread_rwlock_init(&output->rwlock.mutex, NULL);
	ecl_enable_interrupts_env(the_env);
        if (rc) {
                FEerror("Unable to create read/write lock", 0);
        }
	ecl_set_finalizer_unprotected(output, ECL_T);
#else
        output->rwlock.mutex = ecl_make_lock(name, 0);
#endif
	output->rwlock.name = name;
        return output;
}

@(defun mp::make-rwlock (&key name)
@
	@(return ecl_make_rwlock(name))
@)

cl_object
mp_rwlock_name(cl_object lock)
{
	const cl_env_ptr env = ecl_process_env();
	if (ecl_t_of(lock) != t_rwlock)
		FEerror_not_a_rwlock(lock);
        ecl_return1(env, lock->rwlock.name);
}

cl_object
mp_giveup_rwlock_read(cl_object lock)
{
        /* Must be called with interrupts disabled. */
	if (ecl_t_of(lock) != t_rwlock)
		FEerror_not_a_rwlock(lock);
#ifdef ECL_RWLOCK
        {
                int rc = pthread_rwlock_unlock(&lock->rwlock.mutex);
                if (rc)
                        FEunknown_rwlock_error(lock, rc);
                @(return ECL_T);
        }
#else
        return mp_giveup_lock(lock->rwlock.mutex);
#endif
}

cl_object
mp_giveup_rwlock_write(cl_object lock)
{
	return mp_giveup_rwlock_read(lock);
}

cl_object
mp_get_rwlock_read_nowait(cl_object lock)
{
        if (ecl_t_of(lock) != t_rwlock)
		FEerror_not_a_rwlock(lock);
#ifdef ECL_RWLOCK
        {
                const cl_env_ptr env = ecl_process_env();
                cl_object output = ECL_T;
                int rc = pthread_rwlock_tryrdlock(&lock->rwlock.mutex);
                if (rc == 0) {
                        output = ECL_T;
                } else if (rc == EBUSY) {
                        output = ECL_NIL;
                } else {
                        FEunknown_rwlock_error(lock, rc);
                }
                ecl_return1(env, output);
        }
#else
        return mp_get_lock_nowait(lock->rwlock.mutex);
#endif
}

cl_object
mp_get_rwlock_read_wait(cl_object lock)
{
        if (ecl_t_of(lock) != t_rwlock)
		FEerror_not_a_rwlock(lock);
#ifdef ECL_RWLOCK
        {
                const cl_env_ptr env = ecl_process_env();
                int rc = pthread_rwlock_rdlock(&lock->rwlock.mutex);
                if (rc != 0) {
                        FEunknown_rwlock_error(lock, rc);
                }
                ecl_return1(env, ECL_T);
        }
#else
        return mp_get_lock_wait(lock->rwlock.mutex);
#endif
}

@(defun mp::get-rwlock-read (lock &optional (wait ECL_T))
@
	if (Null(wait))
        	return mp_get_rwlock_read_nowait(lock);
        else
        	return mp_get_rwlock_read_wait(lock);
@)

cl_object
mp_get_rwlock_write_nowait(cl_object lock)
{
        if (ecl_t_of(lock) != t_rwlock)
		FEerror_not_a_rwlock(lock);
#ifdef ECL_RWLOCK
        {
                const cl_env_ptr env = ecl_process_env();
                cl_object output = ECL_T;
                int rc = pthread_rwlock_trywrlock(&lock->rwlock.mutex);
                if (rc == 0) {
                        output = ECL_T;
                } else if (rc == EBUSY) {
                        output = ECL_NIL;
                } else {
                        FEunknown_rwlock_error(lock, rc);
                }
                ecl_return1(env, output);
        }
#else
        return mp_get_lock_nowait(lock->rwlock.mutex);
#endif
}

cl_object
mp_get_rwlock_write_wait(cl_object lock)
{
        cl_env_ptr env = ecl_process_env();
        if (ecl_t_of(lock) != t_rwlock)
		FEerror_not_a_rwlock(lock);
#ifdef ECL_RWLOCK
        {
                int rc = pthread_rwlock_wrlock(&lock->rwlock.mutex);
                if (rc != 0) {
                        FEunknown_rwlock_error(lock, rc);
                }
                @(return ECL_T)
        }
#else
        return mp_get_lock_wait(lock->rwlock.mutex);
#endif
}

@(defun mp::get-rwlock-write (lock &optional (wait ECL_T))
@
	if (Null(wait))
        	return mp_get_rwlock_write_nowait(lock);
        else
        	return mp_get_rwlock_write_wait(lock);
@)
