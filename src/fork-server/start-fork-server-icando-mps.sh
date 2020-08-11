#! /bin/bash
exec ../../build/mps/icando-mps -f cando-jupyter -e "(ql:quickload :cando-jupyter)" -e "(cando-user:jupyterlab-fork-server \"/tmp/clasp-fork-server/\")"
