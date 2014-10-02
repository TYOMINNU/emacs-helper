#!/bin/bash

set -efu

EMACSCLIENT=/usr/local/tumashu-emacs/bin/emacsclient

TEST=$(ps ax | grep "org-capture" | grep -v grep | wc -l)
if [ $TEST = 1 ] ; then
        exec $EMACSCLIENT --socket-name=default $*;
	exit 0;
else
        exec $EMACSCLIENT --socket-name=default -c -a ''  -F '((name . "org-capture"))' $*;
        exit 0;
fi
