/*
 * Copyright 1993-2002 Christopher Seiwald and Perforce Software, Inc.
 *
 * This file is part of Jam - see jam.c for Copyright information.
 */

/*  This file is ALSO:
 *  Copyright 2001-2004 David Abrahams.
 *  Distributed under the Boost Software License, Version 1.0.
 *  (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt)
 */

/*
 * make1.c - execute command to bring targets up to date
 *
 * This module contains make1(), the entry point called by make() to
 * recursively decend the dependency graph executing update actions as
 * marked by make0().
 *
 * External routines:
 *
 *  make1() - execute commands to update a TARGET and all of its dependencies.
 *
 * Internal routines, the recursive/asynchronous command executors:
 *
 *  make1a()     - recursively traverse dependency target tree, calling make1b().
 *  make1atail() - started processing all dependencies so go on to make1b().
 *  make1b()     - when dependencies are up to date, build target with make1c().
 *  make1c()     - launch target's next command, call parents' make1b() if none.
 *  make1d()     - handle command execution completion and call back make1c().
 *
 * Internal support routines:
 *
 *  make1cmds()     - turn ACTIONS into CMDs, grouping, splitting, etc.
 *  make1list()     - turn a list of targets into a LIST, for $(<) and $(>).
 *  make1settings() - for vars that get bound values, build up replacement lists.
 *  make1bind()     - bind targets that weren't bound in dependency analysis.
 *
 * 04/16/94 (seiwald) - Split from make.c.
 * 04/21/94 (seiwald) - Handle empty "updated" actions.
 * 05/04/94 (seiwald) - async multiprocess (-j) support.
 * 06/01/94 (seiwald) - new 'actions existing' does existing sources.
 * 12/20/94 (seiwald) - NOTIME renamed NOTFILE.
 * 01/19/95 (seiwald) - distinguish between CANTFIND/CANTMAKE targets.
 * 01/22/94 (seiwald) - pass per-target JAMSHELL down to exec_cmd().
 * 02/28/95 (seiwald) - Handle empty "existing" actions.
 * 03/10/95 (seiwald) - Fancy counts.
 */

#include "jam.h"

#include "lists.h"
#include "parse.h"
#include "assert.h"
#include "variable.h"
#include "rules.h"
#include "headers.h"

#include "search.h"
#include "newstr.h"
#include "make.h"
#include "command.h"
#include "execcmd.h"
#include "compile.h"
#include "output.h"

#include <stdlib.h>

#if ! defined(NT) || defined(__GNUC__)
    #include <unistd.h>  /* for unlink */
#endif

static CMD      * make1cmds    ( TARGET * );
static LIST     * make1list    ( LIST *, TARGETS *, int flags );
static SETTINGS * make1settings( LIST * vars );
static void       make1bind    ( TARGET * );

/* Ugly static - it is too hard to carry it through the callbacks. */

static struct
{
    int failed;
    int skipped;
    int total;
    int made;
} counts[ 1 ] ;

/* Target state - remove recursive calls by just keeping track of state target
 * is in.
 */
typedef struct _state
{
    struct _state * prev;      /* previous state on stack */
    TARGET        * t;         /* current target */
    TARGET        * parent;    /* parent argument necessary for make1a() */
#define T_STATE_MAKE1A     0   /* make1a() should be called */
#define T_STATE_MAKE1ATAIL 1   /* make1atail() should be called */
#define T_STATE_MAKE1B     2   /* make1b() should be called */
#define T_STATE_MAKE1C     3   /* make1c() should be called */
#define T_STATE_MAKE1D     4   /* make1d() should be called */
    int             curstate;  /* current state */
    int             status;
} state;

static void make1a      ( state * );
static void make1atail  ( state * );
static void make1b      ( state * );
static void make1c      ( state * );
static void make1d      ( state * );
static void make_closure( void * closure, int status, timing_info *, char *, char * );

typedef struct _stack
{
    state * stack;
} stack;

static stack state_stack = { NULL };

static state * state_freelist = NULL;


static state * alloc_state()
{
    if ( state_freelist != NULL )
    {
        state * pState = state_freelist;
        state_freelist = pState->prev;
        memset( pState, 0, sizeof( state ) );
        return pState;
    }

    return (state *)BJAM_MALLOC( sizeof( state ) );
}


static void free_state( state * pState )
{
    pState->prev = state_freelist;
    state_freelist = pState;
}


static void clear_state_freelist()
{
    while ( state_freelist != NULL )
    {
        state * pState = state_freelist;
        state_freelist = state_freelist->prev;
        BJAM_FREE( pState );
    }
}


static state * current_state( stack * pStack )
{
    return pStack->stack;
}


static void pop_state( stack * pStack )
{
    if ( pStack->stack != NULL )
    {
        state * pState = pStack->stack->prev;
        free_state( pStack->stack );
        pStack->stack = pState;
    }
}


static state * push_state( stack * pStack, TARGET * t, TARGET * parent, int curstate )
{
    state * pState = alloc_state();

    pState->t = t;
    pState->parent = parent;
    pState->prev = pStack->stack;
    pState->curstate = curstate;

    pStack->stack = pState;

    return pStack->stack;
}


/*
 * Pushes a stack onto another stack, effectively reversing the order.
 */

static void push_stack_on_stack( stack * pDest, stack * pSrc )
{
    while ( pSrc->stack != NULL )
    {
        state * pState = pSrc->stack;
        pSrc->stack = pSrc->stack->prev;
        pState->prev = pDest->stack;
        pDest->stack = pState;
    }
}


/*
 * make1() - execute commands to update a TARGET and all of its dependencies.
 */

static int intr = 0;

int make1( TARGET * t )
{
    state * pState;

    memset( (char *)counts, 0, sizeof( *counts ) );

    /* Recursively make the target and its dependencies. */
    push_state( &state_stack, t, NULL, T_STATE_MAKE1A );

    do
    {
        while ( ( pState = current_state( &state_stack ) ) != NULL )
        {
            if ( intr )
                pop_state( &state_stack );

            switch ( pState->curstate )
            {
                case T_STATE_MAKE1A    : make1a    ( pState ); break;
                case T_STATE_MAKE1ATAIL: make1atail( pState ); break;
                case T_STATE_MAKE1B    : make1b    ( pState ); break;
                case T_STATE_MAKE1C    : make1c    ( pState ); break;
                case T_STATE_MAKE1D    : make1d    ( pState ); break;
            }
        }
    }
    /* Wait for any outstanding commands to finish running. */
    while ( exec_wait() );

    clear_state_freelist();

    /* Talk about it. */
    if ( counts->failed )
        printf( "...failed updating %d target%s...\n", counts->failed,
                counts->failed > 1 ? "s" : "" );
    if ( DEBUG_MAKE && counts->skipped )
        printf( "...skipped %d target%s...\n", counts->skipped,
                counts->skipped > 1 ? "s" : "" );
    if ( DEBUG_MAKE && counts->made )
        printf( "...updated %d target%s...\n", counts->made,
                counts->made > 1 ? "s" : "" );

    return counts->total != counts->made;
}


/*
 * make1a() - recursively traverse target tree, calling make1b().
 *
 * Called to start processing a specified target. Does nothing if the target is
 * already being processed or otherwise starts processing all of its
 * dependencies. Once all of its dependencies have started being processed goes
 * on and calls make1b() (actually does that indirectly via a helper
 * make1atail() state).
 */

static void make1a( state * pState )
{
    TARGET * t = pState->t;
    TARGETS * c;

    /* If the parent is the first to try to build this target or this target is
     * in the make1c() quagmire, arrange for the parent to be notified when this
     * target is built.
     */
    if ( pState->parent )
        switch ( pState->t->progress )
        {
            case T_MAKE_INIT:
            case T_MAKE_ACTIVE:
            case T_MAKE_RUNNING:
                pState->t->parents = targetentry( pState->t->parents,
                    pState->parent );
                ++pState->parent->asynccnt;
        }

    /* If this target is already being processed then do nothing. There is no
     * need to start processing the same target all over again.
     */
    if ( pState->t->progress != T_MAKE_INIT )
    {
        pop_state( &state_stack );
        return;
    }

    /* Asynccnt counts the dependencies preventing this target from proceeding
     * to make1b() for actual building. We start off with a count of 1 to
     * prevent anything from happening until we can notify all dependencies that
     * they are needed. This 1 is accounted for when we call make1b() ourselves,
     * below. Without this if a a dependency gets built before we finish
     * processing all of our other dependencies our build might be triggerred
     * prematurely.
     */
    pState->t->asynccnt = 1;

    /* Add header nodes created during the building process. */
    {
        TARGETS * inc = 0;
        for ( c = t->depends; c; c = c->next )
            if ( c->target->rescanned && c->target->includes )
                inc = targetentry( inc, c->target->includes );
        t->depends = targetchain( t->depends, inc );
    }

    /* Guard against circular dependencies. */
    pState->t->progress = T_MAKE_ONSTACK;

    {
        stack temp_stack = { NULL };
        for ( c = t->depends; c && !intr; c = c->next )
            push_state( &temp_stack, c->target, pState->t, T_STATE_MAKE1A );

        /* Using stacks reverses the order of execution. Reverse it back. */
        push_stack_on_stack( &state_stack, &temp_stack );
    }

    pState->curstate = T_STATE_MAKE1ATAIL;
}


/*
 * make1atail() - started processing all dependencies so go on to make1b().
 */

static void make1atail( state * pState )
{
    pState->t->progress = T_MAKE_ACTIVE;
    /* Now that all of our dependencies have bumped up our asynccnt we can
     * remove our own internal bump added to prevent this target from being
     * built before all of its dependencies start getting processed.
     */
    pState->curstate = T_STATE_MAKE1B;
}


/*
 * make1b() - when dependencies are up to date, build target with make1c().
 *
 * Called after all dependencies have started being processed and after each of
 * them finishes its processing. The target actually goes on to getting built in
 * make1c() only after all of its dependencies have finished their processing.
 */

static void make1b( state * pState )
{
    TARGET  * t = pState->t;
    TARGETS * c;
    TARGET  * failed = 0;
    char    * failed_name = "dependencies";

    /* If any dependencies are still outstanding, wait until they call make1b()
     * to signal their completion.
     */
    if ( --pState->t->asynccnt )
    {
        pop_state( &state_stack );
        return;
    }

    /* Try to aquire a semaphore. If it is locked, wait until the target that
     * locked it is built and signal completition.
     */
#ifdef OPT_SEMAPHORE
    if ( t->semaphore && t->semaphore->asynccnt )
    {
        /* Append 't' to the list of targets waiting on semaphore. */
        t->semaphore->parents = targetentry( t->semaphore->parents, t );
        t->asynccnt++;

        if ( DEBUG_EXECCMD )
            printf( "SEM: %s is busy, delaying launch of %s\n",
                t->semaphore->name, t->name );
        pop_state( &state_stack );
        return;
    }
#endif

    /* Now ready to build target 't', if dependencies built OK. */

    /* Collect status from dependencies. */
    for ( c = t->depends; c; c = c->next )
        if ( c->target->status > t->status && !( c->target->flags & T_FLAG_NOCARE ) )
        {
            failed = c->target;
            pState->t->status = c->target->status;
        }
    /* If an internal header node failed to build, we want to output the target
     * that it failed on.
     */
    if ( failed )
    {
        failed_name = failed->flags & T_FLAG_INTERNAL
            ? failed->failed
            : failed->name;
    }
    t->failed = failed_name;

    /* If actions for building any of the dependencies have failed, bail.
     * Otherwise, execute all actions to make the current target.
     */
    if ( ( pState->t->status == EXEC_CMD_FAIL ) && pState->t->actions )
    {
        ++counts->skipped;
        if ( ( pState->t->flags & ( T_FLAG_RMOLD | T_FLAG_NOTFILE ) ) == T_FLAG_RMOLD )
        {
            if ( !unlink( pState->t->boundname ) )
                printf( "...removing outdated %s\n", pState->t->boundname );
        }
        else
            printf( "...skipped %s for lack of %s...\n", pState->t->name, failed_name );
    }

    if ( pState->t->status == EXEC_CMD_OK )
        switch ( pState->t->fate )
        {
            /* These are handled by the default case below now
        case T_FATE_INIT:
        case T_FATE_MAKING:
            */

        case T_FATE_STABLE:
        case T_FATE_NEWER:
            break;

        case T_FATE_CANTFIND:
        case T_FATE_CANTMAKE:
            pState->t->status = EXEC_CMD_FAIL;
            break;

        case T_FATE_ISTMP:
            if ( DEBUG_MAKE )
                printf( "...using %s...\n", pState->t->name );
            break;

        case T_FATE_TOUCHED:
        case T_FATE_MISSING:
        case T_FATE_NEEDTMP:
        case T_FATE_OUTDATED:
        case T_FATE_UPDATE:
        case T_FATE_REBUILD:
            /* Prepare commands for executing actions scheduled for this target
             * and then schedule transfer to make1c() state to proceed with
             * executing the prepared commands. Commands have their embedded
             * variables automatically expanded, including making use of any "on
             * target" variables.
             */
            if ( pState->t->actions )
            {
                ++counts->total;
                if ( DEBUG_MAKE && !( counts->total % 100 ) )
                    printf( "...on %dth target...\n", counts->total );

                pState->t->cmds = (char *)make1cmds( pState->t );
                /* Set the target's "progress" so that make1c() counts it among
                 * its successes/failures.
                 */
                pState->t->progress = T_MAKE_RUNNING;
            }
            break;

            /* All possible fates should have been accounted for by now. */
        default:
            printf( "ERROR: %s has bad fate %d", pState->t->name,
                pState->t->fate );
            abort();
        }

    /* Call make1c() to begin the execution of the chain of commands needed to
     * build the target. If we are not going to build the target (due of
     * dependency failures or no commands needing to be run) the chain will be
     * empty and make1c() will directly signal the target's completion.
     */

#ifdef OPT_SEMAPHORE
    /* If there is a semaphore, indicate that it is in use. */
    if ( pState->t->semaphore )
    {
        ++pState->t->semaphore->asynccnt;
        if ( DEBUG_EXECCMD )
            printf( "SEM: %s now used by %s\n", pState->t->semaphore->name,
                pState->t->name );
    }
#endif

    pState->curstate = T_STATE_MAKE1C;
}


/*
 * make1c() - launch target's next command, call parents' make1b() if none.
 *
 * If there are (more) commands to run to build this target (and we have not hit
 * an error running earlier comands) we launch the command using exec_cmd(). If
 * there are no more commands to run, we collect the status from all the actions
 * and report our completion to all the parents.
 */

static void make1c( state * pState )
{
    CMD * cmd = (CMD *)pState->t->cmds;

    if ( cmd && ( pState->t->status == EXEC_CMD_OK ) )
    {
        char * rule_name = 0;
        char * target = 0;

        if ( DEBUG_MAKEQ ||
            ( !( cmd->rule->actions->flags & RULE_QUIETLY ) && DEBUG_MAKE ) )
        {
            rule_name = cmd->rule->name;
            target = lol_get( &cmd->args, 0 )->string;
            if ( globs.noexec )
                out_action( rule_name, target, cmd->buf, "", "", EXIT_OK );
        }

        if ( globs.noexec )
        {
            pState->curstate = T_STATE_MAKE1D;
            pState->status = EXEC_CMD_OK;
        }
        else
        {
            /* Pop state first because exec_cmd() could push state. */
            pop_state( &state_stack );
            exec_cmd( cmd->buf, make_closure, pState->t, cmd->shell, rule_name,
                target );
        }
    }
    else
    {
        TARGETS * c;
        ACTIONS * actions;

        /* Collect status from actions, and distribute it as well. */
        for ( actions = pState->t->actions; actions; actions = actions->next )
            if ( actions->action->status > pState->t->status )
                pState->t->status = actions->action->status;
        for ( actions = pState->t->actions; actions; actions = actions->next )
            if ( pState->t->status > actions->action->status )
                actions->action->status = pState->t->status;

        /* Tally success/failure for those we tried to update. */
        if ( pState->t->progress == T_MAKE_RUNNING )
            switch ( pState->t->status )
            {
                case EXEC_CMD_OK  : ++counts->made  ; break;
                case EXEC_CMD_FAIL: ++counts->failed; break;
            }

        /* Tell parents their dependency has been built. */
        {
            stack temp_stack = { NULL };
            TARGET * t = pState->t;
            TARGET * additional_includes = NULL;

            t->progress = T_MAKE_DONE;

            /* Target has been updated so rescan it for dependencies. */
            if ( ( t->fate >= T_FATE_MISSING ) &&
                ( t->status == EXEC_CMD_OK ) &&
                !t->rescanned )
            {
                TARGET * target_to_rescan = t;
                SETTINGS * s;

                target_to_rescan->rescanned = 1;

                if ( target_to_rescan->flags & T_FLAG_INTERNAL )
                    target_to_rescan = t->original_target;

                /* Clean current includes. */
                target_to_rescan->includes = 0;

                s = copysettings( target_to_rescan->settings );
                pushsettings( s );
                headers( target_to_rescan );
                popsettings( s );
                freesettings( s );

                if ( target_to_rescan->includes )
                {
                    target_to_rescan->includes->rescanned = 1;
                    /* Tricky. The parents have already been processed, but they
                     * have not seen the internal node, because it was just
                     * created. We need to make the calls to make1a() that would
                     * have been made by the parents here, and also make sure
                     * all unprocessed parents will pick up the includes. We
                     * must make sure processing of the additional make1a()
                     * invocations is done before make1b() which means this
                     * target is built, otherwise the parent would be considered
                     * built before this make1a() processing has even started.
                     */
                    make0( target_to_rescan->includes, target_to_rescan->parents->target, 0, 0, 0 );
                    for ( c = target_to_rescan->parents; c; c = c->next )
                        c->target->depends = targetentry( c->target->depends,
                                                          target_to_rescan->includes );
                    /* Will be processed below. */
                    additional_includes = target_to_rescan->includes;
                }
            }

            if ( additional_includes )
                for ( c = t->parents; c; c = c->next )
                    push_state( &temp_stack, additional_includes, c->target, T_STATE_MAKE1A );

            for ( c = t->parents; c; c = c->next )
                push_state( &temp_stack, c->target, NULL, T_STATE_MAKE1B );

#ifdef OPT_SEMAPHORE
            /* If there is a semaphore, it is now free. */
            if ( t->semaphore )
            {
                assert( t->semaphore->asynccnt == 1 );
                --t->semaphore->asynccnt;

                if ( DEBUG_EXECCMD )
                    printf( "SEM: %s is now free\n", t->semaphore->name );

                /* If anything is waiting, notify the next target. There is no
                 * point in notifying waiting targets, since they will be
                 * notified again.
                 */
                if ( t->semaphore->parents )
                {
                    TARGETS * first = t->semaphore->parents;
                    if ( first->next )
                        first->next->tail = first->tail;
                    t->semaphore->parents = first->next;

                    if ( DEBUG_EXECCMD )
                        printf( "SEM: placing %s on stack\n", first->target->name );
                    push_state( &temp_stack, first->target, NULL, T_STATE_MAKE1B );
                    BJAM_FREE( first );
                }
            }
#endif

            /* Must pop state before pushing any more. */
            pop_state( &state_stack );

            /* Using stacks reverses the order of execution. Reverse it back. */
            push_stack_on_stack( &state_stack, &temp_stack );
        }
    }
}


/*
 * call_timing_rule() - Look up the __TIMING_RULE__ variable on the given
 * target, and if non-empty, invoke the rule it names, passing the given
 * timing_info.
 */

static void call_timing_rule( TARGET * target, timing_info * time )
{
    LIST * timing_rule;

    pushsettings( target->settings );
    timing_rule = var_get( "__TIMING_RULE__" );
    popsettings( target->settings );

    if ( timing_rule )
    {
        /* rule timing-rule ( args * : target : start end user system ) */

        /* Prepare the argument list. */
        FRAME frame[ 1 ];
        frame_init( frame );

        /* args * :: $(__TIMING_RULE__[2-]) */
        lol_add( frame->args, list_copy( L0, timing_rule->next ) );

        /* target :: the name of the target */
        lol_add( frame->args, list_new( L0, target->name ) );

        /* start end user system :: info about the action command */
        lol_add( frame->args, list_new( list_new( list_new( list_new( L0,
            outf_time  ( time->start  ) ),
            outf_time  ( time->end    ) ),
            outf_double( time->user   ) ),
            outf_double( time->system ) ) );

        /* Call the rule. */
        evaluate_rule( timing_rule->string, frame );

        /* Clean up. */
        frame_free( frame );
    }
}


/*
 * call_action_rule() - Look up the __ACTION_RULE__ variable on the given
 * target, and if non-empty, invoke the rule it names, passing the given info,
 * timing_info, executed command and command output.
 */

static void call_action_rule
(
    TARGET      * target,
    int           status,
    timing_info * time,
    char        * executed_command,
    char        * command_output
)
{
    LIST * action_rule;

    pushsettings( target->settings );
    action_rule = var_get( "__ACTION_RULE__" );
    popsettings( target->settings );

    if ( action_rule )
    {
        /* rule action-rule (
            args * :
            target :
            command status start end user system :
            output ? ) */

        /* Prepare the argument list. */
        FRAME frame[ 1 ];
        frame_init( frame );

        /* args * :: $(__ACTION_RULE__[2-]) */
        lol_add( frame->args, list_copy( L0, action_rule->next ) );

        /* target :: the name of the target */
        lol_add( frame->args, list_new( L0, target->name ) );

        /* command status start end user system :: info about the action command */
        lol_add( frame->args,
            list_new( list_new( list_new( list_new( list_new( list_new( L0,
                newstr( executed_command ) ),
                outf_int( status ) ),
                outf_time( time->start ) ),
                outf_time( time->end ) ),
                outf_double( time->user ) ),
                outf_double( time->system ) ) );

        /* output ? :: the output of the action command */
        if ( command_output )
            lol_add( frame->args, list_new( L0, newstr( command_output ) ) );
        else
            lol_add( frame->args, L0 );

        /* Call the rule. */
        evaluate_rule( action_rule->string, frame );

        /* Clean up. */
        frame_free( frame );
    }
}


/*
 * make_closure() - internal function passed as a notification callback for when
 * commands finish getting executed by the OS.
 */

static void make_closure
(
    void        * closure,
    int           status,
    timing_info * time,
    char        * executed_command,
    char        * command_output
)
{
    TARGET * built = (TARGET *)closure;

    call_timing_rule( built, time );
    if ( DEBUG_EXECCMD )
        printf( "%f sec system; %f sec user\n", time->system, time->user );

    call_action_rule( built, status, time, executed_command, command_output );

    push_state( &state_stack, built, NULL, T_STATE_MAKE1D )->status = status;
}


/*
 * make1d() - handle command execution completion and call back make1c().
 *
 * exec_cmd() has completed and now all we need to do is fiddle with the status
 * and call back to make1c() so it can run the next command scheduled for
 * building this target or close up the target's build process in case there are
 * no more commands scheduled for it. On interrupts, we bail heavily.
 */

static void make1d( state * pState )
{
    TARGET * t      = pState->t;
    CMD    * cmd    = (CMD *)t->cmds;
    int      status = pState->status;

    if ( t->flags & T_FLAG_FAIL_EXPECTED )
    {
        /* Invert execution result when FAIL_EXPECTED has been applied. */
        switch ( status )
        {
            case EXEC_CMD_FAIL: status = EXEC_CMD_OK  ; break;
            case EXEC_CMD_OK:   status = EXEC_CMD_FAIL; break;
        }
    }

    if ( ( status == EXEC_CMD_FAIL ) &&
         ( cmd->rule->actions->flags & RULE_IGNORE ) )
        status = EXEC_CMD_OK;

    /* On interrupt, set intr so _everything_ fails. */
    if ( status == EXEC_CMD_INTR )
        ++intr;

    /* Print command text on failure. */
    if ( ( status == EXEC_CMD_FAIL ) && DEBUG_MAKE )
    {
        if ( !DEBUG_EXEC )
            printf( "%s\n", cmd->buf );

        printf( "...failed %s ", cmd->rule->name );
        list_print( lol_get( &cmd->args, 0 ) );
        printf( "...\n" );
    }

    /* Treat failed commands as interrupts in case we were asked to stop the
     * build in case of any errors.
     */
    if ( ( status == EXEC_CMD_FAIL ) && globs.quitquick )
        ++intr;

    /* If the command was interrupted or failed and the target is not
     * "precious", remove the targets.
     */
    if (status != EXEC_CMD_OK) 
    {
        LIST * targets = lol_get( &cmd->args, 0 );
        for ( ; targets; targets = list_next( targets ) )
        {
            int need_unlink = 1;
            TARGET* t = bindtarget ( targets->string );
            if (t->flags & T_FLAG_PRECIOUS)
            {                
                need_unlink = 0;
            }
            if (need_unlink && !unlink( targets->string ) )
                printf( "...removing %s\n", targets->string );
        }
    }

    /* Free this command and call make1c() to move onto the next one scheduled
     * for building this same target.
     */
    t->status = status;
    t->cmds = (char *)cmd_next( cmd );
    cmd_free( cmd );
    pState->curstate = T_STATE_MAKE1C;
}


/*
 * swap_settings() - replace the settings from the current module and target
 *                   with those from the new module and target
 */

static void swap_settings
(
    module_t * * current_module,
    TARGET   * * current_target,
    module_t   * new_module,
    TARGET     * new_target
)
{
    if ( new_module == root_module() )
        new_module = 0;

    if ( ( new_target == *current_target ) && ( new_module == *current_module ) )
        return;

    if ( *current_target )
        popsettings( (*current_target)->settings );

    if ( new_module != *current_module )
    {
        if ( *current_module )
            exit_module( *current_module );

        *current_module = new_module;

        if ( new_module )
            enter_module( new_module );
    }

    *current_target = new_target;
    if ( new_target )
        pushsettings( new_target->settings );
}


/*
 * make1cmds() - turn ACTIONS into CMDs, grouping, splitting, etc.
 *
 * Essentially copies a chain of ACTIONs to a chain of CMDs, grouping
 * RULE_TOGETHER actions, splitting RULE_PIECEMEAL actions, and handling
 * RULE_NEWSRCS actions. The result is a chain of CMDs which can be expanded by
 * var_string() and executed using exec_cmd().
 */

static CMD * make1cmds( TARGET * t )
{
    CMD      * cmds = 0;
    LIST     * shell = 0;
    module_t * settings_module = 0;
    TARGET   * settings_target = 0;
    ACTIONS  * a0;

    /* Step through actions. Actions may be shared with other targets or grouped
     * using RULE_TOGETHER, so actions already seen are skipped.
     */
    for ( a0 = t->actions ; a0; a0 = a0->next )
    {
        RULE         * rule = a0->action->rule;
        rule_actions * actions = rule->actions;
        SETTINGS     * boundvars;
        LIST         * nt;
        LIST         * ns;
        ACTIONS      * a1;
        int            start;
        int            chunk;
        int            length;

        /* Only do rules with commands to execute. If this action has already
         * been executed, use saved status.
         */
        if ( !actions || a0->action->running )
            continue;

        a0->action->running = 1;

        /* Make LISTS of targets and sources. If `execute together` has been
         * specified for this rule, tack on sources from each instance of this
         * rule for this target.
         */
        nt = make1list( L0, a0->action->targets, 0 );
        ns = make1list( L0, a0->action->sources, actions->flags );
        if ( actions->flags & RULE_TOGETHER )
            for ( a1 = a0->next; a1; a1 = a1->next )
                if ( a1->action->rule == rule && !a1->action->running )
                {
                    ns = make1list( ns, a1->action->sources, actions->flags );
                    a1->action->running = 1;
                }

        /* If doing only updated (or existing) sources, but none have been
         * updated (or exist), skip this action.
         */
        if ( !ns && ( actions->flags & ( RULE_NEWSRCS | RULE_EXISTING ) ) )
        {
            list_free( nt );
            continue;
        }

        swap_settings( &settings_module, &settings_target, rule->module, t );
        if ( !shell )
            shell = var_get( "JAMSHELL" );  /* shell is per-target */

        /* If we had 'actions xxx bind vars' we bind the vars now. */
        boundvars = make1settings( actions->bindlist );
        pushsettings( boundvars );

        /*
         * Build command, starting with all source args.
         *
         * If cmd_new returns 0, it is because the resulting command length is
         * > MAXLINE. In this case, we will slowly reduce the number of source
         * arguments presented until it does fit. This only applies to actions
         * that allow PIECEMEAL commands.
         *
         * While reducing slowly takes a bit of compute time to get things just
         * right, it is worth it to get as close to MAXLINE as possible, because
         * launching the commands we are executing is likely to be much more
         * compute intensive.
         *
         * Note we loop through at least once, for sourceless actions.
         */

        start = 0;
        chunk = length = list_length( ns );

        do
        {
            /* Build cmd: cmd_new consumes its lists. */
            CMD * cmd = cmd_new( rule,
                list_copy( L0, nt ),
                list_sublist( ns, start, chunk ),
                list_copy( L0, shell ) );

            if ( cmd )
            {
                /* It fit: chain it up. */
                if ( !cmds ) cmds = cmd;
                else cmds->tail->next = cmd;
                cmds->tail = cmd;
                start += chunk;
            }
            else if ( ( actions->flags & RULE_PIECEMEAL ) && ( chunk > 1 ) )
            {
                /* Reduce chunk size slowly. */
                chunk = chunk * 9 / 10;
            }
            else
            {
                /* Too long and not splittable. */
                printf( "%s actions too long (max %d):\n", rule->name, MAXLINE
                    );

                /* Tell the user what didn't fit. */
                cmd = cmd_new( rule, list_copy( L0, nt ),
                    list_sublist( ns, start, chunk ),
                    list_new( L0, newstr( "%" ) ) );
                fputs( cmd->buf, stdout );
                exit( EXITBAD );
            }
        }
        while ( start < length );

        /* These were always copied when used. */
        list_free( nt );
        list_free( ns );

        /* Free the variables whose values were bound by 'actions xxx bind
         * vars'.
         */
        popsettings( boundvars );
        freesettings( boundvars );
    }

    swap_settings( &settings_module, &settings_target, 0, 0 );
    return cmds;
}


/*
 * make1list() - turn a list of targets into a LIST, for $(<) and $(>).
 */

static LIST * make1list( LIST * l, TARGETS * targets, int flags )
{
    for ( ; targets; targets = targets->next )
    {
        TARGET * t = targets->target;

        if ( t->binding == T_BIND_UNBOUND )
            make1bind( t );

        if ( ( flags & RULE_EXISTING ) && ( flags & RULE_NEWSRCS ) )
        {
            if ( ( t->binding != T_BIND_EXISTS ) && ( t->fate <= T_FATE_STABLE ) )
                continue;
        }
        else
        {
            if ( ( flags & RULE_EXISTING ) && ( t->binding != T_BIND_EXISTS ) )
                continue;

            if ( ( flags & RULE_NEWSRCS ) && ( t->fate <= T_FATE_STABLE ) )
                continue;
        }

        /* Prohibit duplicates for RULE_TOGETHER. */
        if ( flags & RULE_TOGETHER )
        {
            LIST * m;
            for ( m = l; m; m = m->next )
                if ( !strcmp( m->string, t->boundname ) )
                    break;
            if ( m )
                continue;
        }

        /* Build new list. */
        l = list_new( l, copystr( t->boundname ) );
    }

    return l;
}


/*
 * make1settings() - for vars that get bound values, build up replacement lists.
 */

static SETTINGS * make1settings( LIST * vars )
{
    SETTINGS * settings = 0;

    for ( ; vars; vars = list_next( vars ) )
    {
        LIST * l = var_get( vars->string );
        LIST * nl = 0;

        for ( ; l; l = list_next( l ) )
        {
            TARGET * t = bindtarget( l->string );

            /* Make sure the target is bound. */
            if ( t->binding == T_BIND_UNBOUND )
                make1bind( t );

            /* Build a new list. */
            nl = list_new( nl, copystr( t->boundname ) );
        }

        /* Add to settings chain. */
        settings = addsettings( settings, VAR_SET, vars->string, nl );
    }

    return settings;
}


/*
 * make1bind() - bind targets that were not bound during dependency analysis
 *
 * Spot the kludge! If a target is not in the dependency tree, it did not get
 * bound by make0(), so we have to do it here. Ugly.
 */

static void make1bind( TARGET * t )
{
    if ( t->flags & T_FLAG_NOTFILE )
        return;

    pushsettings( t->settings );
    t->boundname = search( t->name, &t->time, 0, ( t->flags & T_FLAG_ISFILE ) );
    t->binding = t->time ? T_BIND_EXISTS : T_BIND_MISSING;
    popsettings( t->settings );
}
