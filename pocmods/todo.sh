#! /bin/bash

#  file-purpose     : scan poc files for todo comments / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 12-Sep-2011 11:58 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 7998 $
#  $Date: 2011-11-16 15:29:08 +0100 (Wed, 16 Nov 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/pocmods/todo.sh $

# ---------------------------------
#  settings
# ---------------------------------

pattern="^    \*\*"                     # less permissive
pattern="^[[:blank:]]*\*\*"             # more permissive

xgrab="$HOME/$SYNK/xeona/svn2/futz/trunk/bin/xgrab/xgrab"

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2

# ---------------------------------
#  process comand line
# ---------------------------------

context_before=0
context_after=0

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        echo
        cat <<EOM
      usage: $SCRIPT           run script without context
           : $SCRIPT 0-9       run script with given context
             $SCRIPT --help    display this message and exit
    purpose: scan poc files for todo comments using 'grep'
       note: skips when 'program.run-script-settings.script-model-status' is "stale"
  hardcodes: pattern  = '$pattern'
             contexts = $context_before $context_after
EOM
        echo
        exit $E_SUCCESS
        ;;
    "")
        context_before=0
        context_after=0
        ;;
    [0-4])
        context_before=1
        context_after="$1"
        ;;
    [0-9])
        context_before="$1"
        context_after="$1"
        ;;
    *)
        echo "$SCRIPT: incorrect usage (try --help)"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  active code
# ---------------------------------

test $(which "xmole") && xmole

pocs=$(find -P . -maxdepth 1 -name "poc.[0-9][0-9][0-9].guard.xem" -type f -printf "%P\n" | sort)

for poc in $pocs
do
      if [ $(which "$xgrab") ]
      then
          status=$(eval "$xgrab $poc 'program.run-script-settings.script-model-status'")
          status=$(sed 's/"\(.*\)"/\1/' <<< "$status") # strip double-quotes
          test "$status" == "stale" && continue
      fi
    echo
    echo "$poc"
    echo
    grep --color --before-context=$context_before --after-context=$context_after "$pattern" "$poc"
done
echo

echo "----"
echo "$SCRIPT: contexts: $context_before $context_after"

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $E_SUCCESS

#  $Id: todo.sh 7998 2011-11-16 14:29:08Z robbie $
#  end of file

