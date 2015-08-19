#!/bin/bash

set -efu

TEST=$(ps ax | grep "org-capture" | grep -v grep | wc -l)
if [ $TEST = 1 ] ; then
    exec emacsclient --socket-name=sawfish-emacs-daemon $*;
    exit 0;
else
    exec emacsclient --socket-name=sawfish-emacs-daemon -c -a ''  -F '((name . "org-capture"))' $*;
    exit 0;
fi
