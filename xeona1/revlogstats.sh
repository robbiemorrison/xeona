#! /bin/bash

#  file-purpose     : count commit lines (strictly curiosity) / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Wed 10-Mar-2010 15:34 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 5250 $
#  $Date: 2010-10-06 13:35:46 +0200 (Wed, 06 Oct 2010) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/revlogstats.sh $

# ---------------------------------
#  settings
# ---------------------------------

RANGE="PREV:HEAD .. "                   # compare: svlo  ..
RANGE="1:HEAD .."                       # compare: svlog ..

A4LINES=82                              # emacs 8pt portrait (82 lines x 100 cols)

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")                 # script leafname

# ---------------------------------
#  help message
# ---------------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        echo
        echo "       usage: $SCRIPT                count commit log lines (strictly for curiosity)"
        echo "              $SCRIPT -h --help      display this message and exit"
        echo "     purpose: summarize the commit log"
        echo
        exit 0
        ;;
    "")
        :                               # fall thru is correct
        ;;
    *)
        echo "$SCRIPT: incorrect usage"
        exit 2
        ;;
esac

# ---------------------------------
#  active code
# ---------------------------------

echo
echo "         range: $RANGE"

lines=$(svn log --verbose --revision $RANGE \
    | grep --invert-match "^$"              \
    | grep --invert-match "^----------"     \
    | grep --invert-match "^r.*line"        \
    | grep --invert-match "^Changed paths"  \
    | grep --invert-match "^   [AM] "       \
    | wc --lines)

echo "     log lines: $lines"

pages=0
let "pages = lines / A4LINES"

echo "     pages 8pt: $pages"
echo "  elapsed time: $SECONDS seconds"
echo

# ---------------------------------
#  housekeeping
# ---------------------------------

exit 0

# note "grep --count"

#  $Id: revlogstats.sh 5250 2010-10-06 11:35:46Z robbie $
#  end of file

