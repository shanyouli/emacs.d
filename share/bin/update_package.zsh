#!/usr/bin/env zsh

# update git-submodule package
Dir=$PWD

if [[ -n $1 ]] && [[ -d $1 ]]; then
    Emacs_Home=$1
else
    Emacs_Home=$HOME/.emacs.d
fi

Workdir=$Emacs_Home/site-lisp

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
cd "$Dir" || exit 0
