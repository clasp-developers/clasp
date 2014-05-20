/* -*- C -*-
 *
 * $HEADER$
 *
 * A test to trap user signals
 */
#include "orte_config.h"

#include <stdio.h>
#include <signal.h>

#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"

void sigusr_handler(int signum)
{
    switch (signum) {
        case SIGUSR1:
            fprintf(stderr, "%s Trapped SIGUSR1\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            break;

        case SIGUSR2:
            fprintf(stderr, "%s Trapped SIGUSR2\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            return;

        default:
            fprintf(stderr, "%s Undefined signal %d trapped\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), signum);
            return;
    }
}

void exit_handler(int signum)
{
    int rc;

    exit(0);
}


int main(int argc, char* argv[])
{

    int rc;
    int i;
    double pi;

    orte_init(&argc, &argv, ORTE_PROC_TOOL);
    
    if (signal(SIGUSR1, sigusr_handler) == SIG_IGN) {
        fprintf(stderr, "Could not setup signal trap for SIGUSR1\n");
        exit(1);
    }

    if (signal(SIGUSR2, sigusr_handler) == SIG_IGN) {
        fprintf(stderr, "Could not setup signal trap for SIGUSR2\n");
        exit(1);
    }

    if (signal(SIGINT, exit_handler) == SIG_IGN) {
        fprintf(stderr, "Could not setup signal trap for SIGINT\n");
        exit(1);
    }

    if (signal(SIGHUP, exit_handler) == SIG_IGN) {
        fprintf(stderr, "Could not setup signal trap for SIGHUP\n");
        exit(1);
    }

    if (signal(SIGTERM, exit_handler) == SIG_IGN) {
        fprintf(stderr, "Could not setup signal trap for SIGTERM\n");
        exit(1);
    }

    i = 0;
    while (1) {
        i++;
        pi = i / 3.14159256;
        if (i > 100) i = 0;
    }

    return 0;
}
