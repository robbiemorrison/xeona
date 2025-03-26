#! /bin/bash

SCRIPT=$(basename "$0")

# help message

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        echo "$SCRIPT: <patter>    search selected R files for 'pattern' using 'grep'"
        exit 0
        ;;
esac

# active code

pattern="$*"
GREP="grep --binary-files=without-match --ignore-case --color"

echo

$GREP "$pattern" {b,d,e,g,r,s,t,x}*.R utils.R  # CAUTION: soft-quotes essential
ret=$?

echo
echo "  ---"
echo "  pattern   : '$pattern'"
echo "  grep exit : $ret"
echo

exit $ret

# emacs text editor settings
# local variables:
#   mode: sh
#   make-backup-files: nil
# end:

