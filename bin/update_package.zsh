#!/usr/bin/env zsh

# update git-submodule package

Dir=$(pwd) #current directory
Scriptdir=$(cd "$(dirname "$0")" && pwd) # The directory where the script is located
if [[ -d $Scriptdir/../site-lisp ]]; then
    Workdir=$(cd $Scriptdir/../site-lisp && pwd)
else
    print -P "%F{red}%B[ ER ] The git-Submodule-Dir don't exist."
    exit 1
fi


for i in "$Workdir"/* ; do
    [[ -d $i ]] && {
        cd "$i" && {
            print -P "%F{green} [ .. ] The workdir is $i%f"
            git checkout master
            git pull
        }
    }
done

# Return to the directory where the work was performed
cd "$Dir" || return 0
