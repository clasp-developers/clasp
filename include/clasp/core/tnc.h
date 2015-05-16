/*
    File: tnc.h
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */

//
// (C) 2004 Christian E. Schafmeister
//

/* tnc : truncated newton bound contrained minimization
         using gradient information, in C */

/*
 * Copyright (c) 2002, Jean-Sebastien Roy (js@jeannot.org)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * This software is a C implementation of TNBC, a truncated newton minimization
 * package originally developed by Stephen G. Nash in Fortran.
 *
 * The original source code can be found at :
 * http://iris.gmu.edu/~snash/nash/software/software.html
 *
 * Copyright for the original TNBC fortran routines:
 *
 *   TRUNCATED-NEWTON METHOD:  SUBROUTINES
 *     WRITTEN BY:  STEPHEN G. NASH
 *           SCHOOL OF INFORMATION TECHNOLOGY & ENGINEERING
 *           GEORGE MASON UNIVERSITY
 *           FAIRFAX, VA 22030
 */

/* $Id: tnc.h,v 1.1.1.1 2003/04/21 18:25:24 meister Exp $ */

#ifndef _TNC_
#define _TNC_

#define TNC_VERSION "1.0.2"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Verbosity level
 */
typedef enum {
  TNC_MSG_NONE = 0, /* No messages */
  TNC_MSG_ITER = 1, /* One line per iteration */
  TNC_MSG_INFO = 2, /* Informational messages */
  TNC_MSG_VERS = 4, /* Version info */
  TNC_MSG_EXIT = 8, /* Exit reasons */

  TNC_MSG_ALL = TNC_MSG_ITER | TNC_MSG_INFO | TNC_MSG_VERS | TNC_MSG_EXIT /* All messages */
} tnc_message;

/*
 * Possible return values for tnc
 */
typedef enum {
  TNC_EINVAL = -2,      /* Invalid parameters (n<1)*/
  TNC_INFEASIBLE = -1,  /* Infeasible (low > up) */
  TNC_LOCALMINIMUM = 0, /* Local minima reach (|pg| ~= 0) */
  TNC_CONVERGED = 1,    /* Converged (]f_n-f_(n-1)] ~= 0) */
  TNC_MAXFUN = 2,       /* Max. number of function evaluations reach */
  TNC_LSFAIL = 3,       /* Linear search failed */
  TNC_CONSTANT = 4,     /* All lower bounds are equal to the upper bounds */
  TNC_NOPROGRESS = 5    /* Unable to progress */
} tnc_rc;

/*
 * A function as required by tnc
 * state is a void pointer provided to the function at each call
 *
 * x     : on input, then vector of variables (should not be modified)
 * f     : on output, the value of the function
 * g     : on output, the value of the gradient
 * state : on input, the value of the state variable as provided to tnc
 *
 */
typedef void tnc_function(double x[], double *f, double g[], void *state);

/*
 * tnc : minimize a function with variables subject to bounds, using
 *       gradient information.
 *
 * n         : number of variables (must be > 0)
 * x         : on input, initial estimate ; on output, the solution
 * f         : on output, the function value at the solution
 * g         : on output, the gradient value at the solution
 *             g should be an allocated vector of size n or NULL,
 *             in which case the gradient value is not returned.
 * function  : the function to minimize (see tnc_function)
 * state     : used by function (see tnc_function)
 * low, up   : the bounds
 *             set low[i] to -HUGE_VAL to remove the lower bound
 *             set up[i] to HUGE_VAL to remove the upper bound
 * scale     : scaling factors to apply to each variable
 *             if NULL, the factors are up-low for interval bounded variables
 *             and 1+|x] fo the others.
 * messages  : see the tnc_message enum
 * maxCGit   : max. number of hessian*vector evaluation per main iteration
 *             if maxCGit == 0, the direction chosen is -gradient
 *             if maxCGit < 0, maxCGit is set to max(1,min(50,n/2))
 * maxnfeval : max. number of function evaluation (must be > 1)
 * eta       : severity of the line search. if < 0 or > 1, set to 0.25
 * stepmx    : maximum step for the line search. may be increased during call
 *             if too small, will be set to 10.0
 * accuracy  : relative precision for finite difference calculations
 *             if <= machine_precision, set to sqrt(machine_precision)
 * fmin      : minimum function value estimate
 * ftol      : precision goal for the value of f in the stoping criterion
 *             relative to the machine precision and the value of f.
 *             if ftol < 0.0, set ftol to 0.0
 * rescale   : Scaling factor (in log10) used to trigger rescaling
 *             0 -> rescale at each iteration
 *             big value -> never rescale
 *             < 0 -> set to 1.3 (default value)
 *
 * The tnc function returns a code defined in the tnc_rc enum.
 * On output, x, f and g may be very slightly out of sync because of scaling.
 *
 */
extern int tnc(int n, double x[], double *f, double g[],
               tnc_function *function, void *state,
               double low[], double up[], double scale[],
               int messages, int maxCGit, int maxnfeval, double eta, double stepmx,
               double accuracy, double fmin, double ftol, double rescale);

#ifdef __cplusplus
}
#endif

#endif /* _TNC_ */
