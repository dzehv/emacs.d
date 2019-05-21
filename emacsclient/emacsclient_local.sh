#!/bin/sh

# Use this script as your EDITOR to allow editing local files with emacsclient.
# Works by connecting to the local Emacs-server and opens file in new buffer.
# Allows to jump to line number

# A POSIX variable
OPTIND=1 # Reset in case getopts has been used previously in the shell.

LINE=''
COLUMN=''

while getopts "l:c:" opt; do
    case $opt in
        l) LINE=$OPTARG
           ;;
        c) COLUMN=$OPTARG
           ;;
        *) echo "No reasonable options found!"
           exit 1
           ;;
    esac
done

shift $((OPTIND-1))

[ "${1:-}" = "--" ] && shift

LINE_ARG=''

if [ $LINE ] && [ $COLUMN ]; then
    LINE_ARG="+$LINE:$COLUMN"
elif [ $LINE ]; then
    LINE_ARG="+$LINE"
fi

if [ $LINE_ARG ]; then
    emacsclient -n -s ~/.emacs.d/server/server $LINE_ARG "$1"
else
    emacsclient -n -s ~/.emacs.d/server/server "$@"
fi
