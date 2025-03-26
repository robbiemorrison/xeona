#! /bin/bash

#  file-purpose     : run 'scriptreplay' using a constructed command-line / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Fri 14-Dec-2007 09:57 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 1649 $
#  $Date: 2008-06-19 16:14:58 +0200 (Thu, 19 Jun 2008) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/replay.sh $

#  Useful 'script' commands
#
#  $ man script
#  $ man scriptreplay
#
#  $ rm --force ../typescript ../timingfile
#  $ script -t ../typescript 2> ../timingfile
#  ...
#  $ exit
#  $ scriptreplay ../timingfile ../typescript 5
#
#  $ cat ../timingfile | sort -n
#  $ gawk 'BEGIN { FS = " " } { if ( $1 > 3 ) $1 = 3; print $0 }' ../timingfile  > ../timingfile2
#  $ time scriptreplay ../timingfile2 ../typescript 3
#
#  C-s stop output
#  C-q restart output
#
#  $ sleep 500; beep -r 10 -f 300

# ---------------------------------
#  preamble
# ---------------------------------

SPEEDUP=10                              # default speed-up factor

SCRIPT=$(basename "$0")

TYPESCRIPT="../typescript"
TIMINGFILE="../timingfile"

E_SUCCESS=0
E_USAGE=2

# ---------------------------------
#  process command-line
# ---------------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|-\?)
        echo
        echo "     usage: $SCRIPT  [speedup]       run 'scriptreplay' utility"
        echo "            $SCRIPT  -h --help       show this message and exit"
        echo "      note: speedup values from 1 thru 9999, default $SPEEDUP"
        echo
        exit $E_SUCCESS
        ;;
    "")                                 # fall thru
        :
        ;;
    [1-9]|[1-9][0-9]|[1-9][0-9][0-9]|[1-9][0-9][0-9][0-9])  # 1 thru 9999
        SPEEDUP=$1
        shift
        ;;
    *)
        echo "$SCRIPT: incorrect usage (try --help)"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  check for files
# ---------------------------------

test -f "../timingfile2" && TIMINGFILE="../timingfile2"

if [[ ! -f "$TYPESCRIPT" || ! -f "$TIMINGFILE" ]]
    then
    echo "$SCRIPT: at least one file missing: $TYPESCRIPT $TIMINGFILE"
    exit 1
fi

# ---------------------------------
#  reset speed-up as required
# ---------------------------------

test -n "$1" && SPEEDUP="$1"

# ---------------------------------
#  main call
# ---------------------------------

call="time scriptreplay "$TIMINGFILE" "$TYPESCRIPT" $SPEEDUP"
eval "$call"

# 'time' utility displays here
echo "call: $call"

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $E_SUCCESS

#  $Id: replay.sh 1649 2008-06-19 14:14:58Z robbie $
#  end of file

