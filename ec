#!/bin/bash

# Copy this file to /bin or /usr/local/bin (for OS X)

# emacsclient sends requested file to emacs-server for edit

#### SSH experimental part
# params=()
# for p in "$@"; do
#     if [ "$p" == "-n" ]; then
#         params+=( "$p" )
#     elif [ "${p:0:1}" == "+" ]; then
#         params+=( "$p" )
#     else
#         params+=( "/ssh:dev33:"$(readlink -f $p) )
#     fi
# done
# emacsclient "${params[@]}"
####

emacsclient -n -s ~/.emacs.d/server/server $@
