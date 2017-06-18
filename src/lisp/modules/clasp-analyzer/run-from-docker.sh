#! /bin/bash

mkdir -p /tmp/clasp-out
sudo chown 9999:9999 /tmp/clasp-out
docker run -it -v $HOME/Dev/cando:/home/ubuntu/Dev/cando -v /tmp/clasp-out:/home/app/mount-out clasp-cxxanalysis -l "/home/ubuntu/Dev/cando/src/lisp/modules/clasp-analyzer/run-from-docker.lisp"
