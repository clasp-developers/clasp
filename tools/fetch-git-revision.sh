#!/bin/sh

path=$1
url=$2
revision=$3

#set -x

gitCloneIt () {
   git clone --depth 1 "$url" "$path" || exit $?
}

waitForYes () {
    while true; do
        read -p "Type 'yes' to continue: " response
        case $response in
            yes* ) break;;
            * ) echo "Please answer 'yes' or press C-c to cancel the entire process.";;
        esac
    done
}

# if .git is a file, then we assume it's a git submodule and clean it up
if [ -f "$path/.git" ]; then
    echo "WARNING: '$path' seems to hold the remains of a git submodule (the old scheme). Do you want me to *RECURSIVELY DELETE* this directory, and then check out the git repo using the new scheme?"
    waitForYes
    rm -rf "$path"
    rm -rf ".git/modules/$path"
    git config -f .git/config --remove-section "submodule.$path" 2> /dev/null
fi

if [ ! -e "$path" ]; then
    gitCloneIt
fi

cd "$path" || exit $?

if ! git cat-file -e "$revision"; then
    git fetch origin $revision: || exit $?
fi

git reset --hard "$revision" || exit $?
