#!/bin/sh

# Use this script as your EDITOR to allow editing local files with emacsclient.
# Works by connecting to the local Emacs-server and opens file in new buffer.
# Allows to jump to line number

# transform long options to short ones
for arg in "$@"; do
    shift
    case "$arg" in
        "--line")             set -- "$@" "-l" ;;
        "--column")           set -- "$@" "-c" ;;
        "--fundamental-mode") set -- "$@" "-f" ;;
        *)                    set -- "$@" "$arg" ;;
    esac
done

# A POSIX variable
OPTIND=1 # Reset in case getopts has been used previously in the shell.

LINE=''
COLUMN=''
# to find file and set fundamental mode (like find-file-literally)
FUNDAMENTAL_MODE=0

while getopts "fl:c:" opt; do
    case $opt in
        l) LINE=$OPTARG
           ;;
        c) COLUMN=$OPTARG
           ;;
        f) FUNDAMENTAL_MODE=1
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

if [ "$FUNDAMENTAL_MODE" -eq 1 ]; then
    # only 1 file
    emacsclient -n -s ~/.emacs.d/server/server -e "(progn (find-file \"$1\") (fundamental-mode))"
elif [ $LINE_ARG ]; then
    # also only 1 file
    emacsclient -n -s ~/.emacs.d/server/server $LINE_ARG "$1"
else
    # can be multiple files
    emacsclient -n -s ~/.emacs.d/server/server "$@"
fi
