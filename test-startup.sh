#!/usr/bin/env bash

# see @https://emacs-china.org/t/topic/5421
# copy @https://github.com/purcell/emacs.d/blob/master/test-startup.sh
# log : ./test-startup.sh 2&>1 | tee start.log
if [[ -n "$TRAVIS" ]] ; then
    # Make it look like this is ~/.emacs.d (needed for Emacs 24.3, at least)
    export HOME=$PWD/..
    [[ -d emacs.d ]] || ln -s emacs.d ../.emacs.d
fi

echo "Attempting startup..."

${EMACS:=emacs} -nw --batch \
                --eval '(let ((debug-on-error t)
                              (url-show-status nil)
                              (user-emacs-directory default-directory)
                              (user-init-file (expand-file-name "init.el"))
                              (load-path (delq default-directory load-path)))
                           (load-file user-init-file)
                           (run-hooks (quote after-init-hook)))'

echo "Startup successful!"
