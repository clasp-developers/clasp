#!/bin/sh

path=$1
url=$2
revision=$3
label=$4

#set -x

echo Updating git repo at \'$path\'

gitCloneIt () {
   git clone "$url" "$path" || exit $?
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
    echo "WARNING: \"$path\" seems to hold the remains of a git submodule (the old scheme). Do you want me to *RECURSIVELY DELETE* this directory, and then check out the git repo using the new scheme?"
    waitForYes
    rm -rf "$path"
    rm -rf ".git/modules/$path"
    git config -f .git/config --remove-section "submodule.$path" 2> /dev/null
elif [ -d "$path/.git" ]; then
    currentRemoteUrl=`git --git-dir "$path/.git" remote get-url origin`
    if [ "$currentRemoteUrl" != "$url" ]; then
        echo "WARNING: \"$path\" seems to be holding a checkout from a different URL (current: \"$currentRemoteUrl\", requested: \"$url\"). Do you want me to *RECURSIVELY DELETE* this directory, and then check out the git repo from the new location?"
        waitForYes
        rm -rf "$path"
        rm -rf ".git/modules/$path"
        git config -f .git/config --remove-section "submodule.$path" 2> /dev/null
    fi
fi

if [ ! -e "$path" ]; then
    gitCloneIt
fi

cd "$path" || exit $?

if ([ -n "$revision" ] && ! (git cat-file -e "$revision")) ||
   ([ -n "$label" ]    && ! (git cat-file -e "$label")); then
    git fetch origin || exit $?
fi

if [ -n "$label" ]; then
    git checkout --quiet "$label" || exit $?
fi

if [ -n "$revision" ]; then
    git reset --quiet --merge "$revision" || exit $?
fi

# Print the locally modified files as a reminder
git status --short
