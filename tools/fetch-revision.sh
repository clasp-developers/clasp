#!/bin/sh

if [ ! -e "$2" ]; then
   git clone "$1" "$2" || exit $?
   cd "$2" || exit $?
else
    cd "$2" || exit $?
    if ! git cat-file -e "$3"; then
        git pull || exit $?
    fi
fi
git reset --hard "$3" || exit $?
