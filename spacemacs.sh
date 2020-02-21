#!/bin/sh

# unsetting XMODIFIERS gets rid of "<dead_tilde> is undefined" bug
exec env TMPDIR="/tmp/`whoami`" XMODIFIERS= emacsclient -ca emacs "$@"
