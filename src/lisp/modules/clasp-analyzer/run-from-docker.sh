#! /bin/bash

mkdir -p /tmp/clasp-out
sudo chown 9999:9999 /tmp/clasp-out
docker run -it -v $HOME/Dev/cando:/home/app/mount -v /tmp/clasp-out:/home/app/mount-out clasp -l "/home/app/mount/src/lisp/modules/clasp-analyzer/run-from-docker.lisp"
