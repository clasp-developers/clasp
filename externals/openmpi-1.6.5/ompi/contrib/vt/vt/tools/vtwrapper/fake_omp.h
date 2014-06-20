/**
 * This header file temporary replaces the original omp.h when the VT compiler
 * wrapper is preprocessing a source file before it will be parsed by OPARI.
 * This is necessary to prevent multiple declarations of OpenMP functions,
 * types, etc. which results in various compiler errors.
 **/
