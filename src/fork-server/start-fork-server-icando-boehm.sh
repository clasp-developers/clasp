#! /bin/bash
exec ../../build/boehm/icando-boehm -f cando-jupyter -e "(ql:quickload :cando-jupyter)" -e "(cando-user:jupyterlab-fork-server \"/tmp/clasp-fork-server/\")"
