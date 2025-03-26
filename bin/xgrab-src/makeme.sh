#! /bin/bash

#  file-purpose     : optional build script for 'xgrab'
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 07-Nov-2011 10:23 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 8380 $
#  $Date: 2011-12-09 12:51:41 +0100 (Fri, 09 Dec 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xgrab-src/makeme.sh $

#  the purpose of this script is to turn most g++
#  warnings and then filter out any junk arising from
#  the Boost C++ libraries
#
#  this script also supplies a sensible runtime
#  argument for the 'valgrind' test of 'xgrab'
#
#  CAUTION: change 'scn' screen call around if 'grep'
#  reports: "Binary file (standard input) matches" (it
#  seems the last screening command has to find
#  something, as a work-around perhaps add "| true" to
#  the end)

# preamble
SCRIPT=$(basename "$0")                 # name of this script
E_SUCCESS=0
E_FAILURE=1
E_USAGE=2

# help message
case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        target="xgrab"                  # just for the help message
        echo
        cat << EOM
      usage: $SCRIPT             build '$target'
             $SCRIPT  --help     display this message and exit
    purpose: interface to 'makefile' for '$target', adds in more g++ warnings
               and filters out any junk from the Boost C++ libraries,
               also passes a sensible runtime argument for 'valgrind'
  note also: make all, make check, make clean, make cleanall
EOM
        echo
        exit $E_SUCCESS
        ;;
    "")
        :                               # fall thru is correct
        ;;
    *)
        echo "$SCRIPT: incorrect usage (try --help)"
        exit $E_USAGE
        ;;
esac

# strings
jobs="4"                                # CAUTION: do no use, out-of-order errors arise
jobs="1"
cmd="make --jobs=$jobs WARNINGS=\"-Wall -Weffc++ -pedantic\" RUNARGS=\"--debug --help\""
scn="grep -v '/usr/local/include/boost/' | grep -v 'instantiated from'"
call="$cmd"                             # call with screen
call="$cmd 2>&1 | $scn"                 # call without screen

# report
echo
echo "$call"
echo
echo "---"
echo

# real call
eval "$call"

#report
echo "---"
echo
echo "build call             : $cmd"
echo "screen call            : $scn"
echo "elapsed time (seconds) : $SECONDS"
echo

#  $Id: makeme.sh 8380 2011-12-09 11:51:41Z robbie $
#  end of file

