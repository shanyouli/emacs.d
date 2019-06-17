#!/usr/bin/env bash

# update git-submodule package
dir=$(pwd) #current directory
workdir=$(cd "$(dirname "$0")" && pwd) # work directory

for i in "$workdir"/* ; do
    [[ -d $i ]] && {
        cd "$i" && {
            git checkout master
            git pull
        }
    }
done

# Return to the directory where the work was performed
cd "$dir" || return 0
