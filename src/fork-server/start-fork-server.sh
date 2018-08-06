#! /bin/bash
exec ../../build/boehm/iclasp-boehm -f cando-jupyter -l "source-dir:extensions;cando;src;lisp;start-cando.lisp" -e "(in-package :cando-user)" -e "(cando-user:jupyterlab-fork-server 9998)" &
