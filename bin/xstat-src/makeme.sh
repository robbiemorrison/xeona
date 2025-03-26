#! /bin/bash

#  file-purpose     : optional build script for 'xstat'
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Tue 15-Nov-2011 12:18 UTC
#  file-status      : compelte
#  file-keywords    : xeona

#  $Revision: 7972 $
#  $Date: 2011-11-15 17:06:23 +0100 (Tue, 15 Nov 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xstat-src/makeme.sh $

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
cmd="make --jobs=1 WARNINGS=\"-Wall -Weffc++ -pedantic\" RUNARGS=\" --max +5.00e+00 +15.00e+00\""
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

#  $Id: makeme.sh 7972 2011-11-15 16:06:23Z robbie $
#  end of file

