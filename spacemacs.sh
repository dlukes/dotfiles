#!/bin/sh

exec env TMPDIR="/tmp/`whoami`" emacsclient -ca emacs $@
