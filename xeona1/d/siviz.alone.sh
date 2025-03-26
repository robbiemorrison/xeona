#! /bin/bash

#  file-purpose     : stand-alone build and run of 'SolverIf' and 'GlpkViz' / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Wed 18-Jun-2008 21:17 UTC
#  file-status      : work-in-progress
#  file-keywords    : xeona

#  $Revision: 9163 $
#  $Date: 2012-03-06 10:35:12 +0100 (Tue, 06 Mar 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/d/siviz.alone.sh $

# ---------------------------------
#  preamble
# ---------------------------------

ASSOC="siviz.alone.cc"                  # associated file
BINARY="siviz.alone"                    # default binary name, can be overridden

ASSOC="lmp.alone.cc"                    # associated file
BINARY="lmp.alone"                      # default binary name, can be overridden

VALGRIND="valgrind --leak-check=full --show-reachable=yes"
VALGRIND=""

PRECALL=""
PRECALL="/usr/bin/time --portability nice"

# ---------------------------------

ALERT=180                               # audible beep if more than ALERT seconds

SDIR="d"                                # hard-coded subdirectory

SCRIPT=$(basename "$0")                 # script leafname

E_SUCCESS=0
E_WRONG_PWD=64

# ---------------------------------
#  create call string
# ---------------------------------

call=$(echo $PRECALL $VALGRIND ./$BINARY)

# -------------------------
#  process command-line
# -------------------------

text1="SolverIf and GlpkViz units"
case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|-\?)
        echo
        echo "      usage: $SCRIPT  [binary-name]   build and run stand-alone $text1"
        echo "             $SCRIPT  -h --help       show this message and exit"
        echo "    default: binary name     : $BINARY"
        echo "  hardcodes: subdirectory    : $SDIR"
        echo "             associated file : $ASSOC"
        echo "             call string     : $call"
        echo "             alert trigger   : $ALERT seconds"
        echo
        exit $E_SUCCESS
        ;;
esac

test -n "$1" && BINARY="$1"             # take from command-line if given

# ---------------------------------
#  confirm call directory
# ---------------------------------

if [ "$(basename $(pwd -P))" != "$SDIR" ]
    then
    echo "$SCRIPT: called from wrong directory, should be in subdirectory: $SDIR"
    exit $E_WRONG_PWD
fi

echo
echo "$SCRIPT:        run call : $call"

# ---------------------------------
#  build binary
# ---------------------------------

echo "$SCRIPT: associated file : $ASSOC"
cd ..                                   # drop down to current base directory
echo "$SCRIPT:    in directory : $(pwd -P)"
echo
echo "$SCRIPT: about to call make"
echo

# note 'CPPFLAG_SVNREV' futz which allows the makefile
# to run when the svn version is stale

# the internal macro 'XE_GLPKVIZ_ALONE' is used to
# select for the stand-alone code

make --file "makefile" \
CPPFLAG_SVNREV="-D_XSVNREV=0 -DXE_GLPKVIZ_ALONE=0" \
binary="$SDIR/$BINARY" \
sources="common.cc $SDIR/glpkviz.cc $SDIR/siglp.cc \
$SDIR/$ASSOC a/logger.cc a/exbase.cc a/exapp.cc \
c/files.cc"

# check make return

ret=$?
if [ $ret -ne 0 ]
    then
    echo "$SCRIPT: abandoning: make returned fail: $ret"
    exit $ret
fi

# ---------------------------------
#  open firefox as needed
# ---------------------------------

# delay: if firefox is running, use a 1s delay, else longer

secs=15                                  # delay after invoking firefox

echo
echo "$SCRIPT: checking for 'firefox-bin' processes"
ps --no-headers -C "firefox-bin"
ret=$?
case $ret in
    0)                                  # present
        echo "$SCRIPT: 'firefox' is running"
        sleep 1
        ;;
    1)                                  # not present
        echo "$SCRIPT: about to invoke 'firefox' in background"
        firefox &
        echo "$SCRIPT: about to sleep $secs"
        sleep "$secs"
        ;;
    *)
        echo "$SCRIPT: unknown return: $ret"
        ;;
esac

# ---------------------------------
#  run binary
# ---------------------------------

cd $SDIR
echo "$SCRIPT:    in directory : $(pwd -P)"
echo "$SCRIPT: about to invoke : $call"
tic=$SECONDS                            # start timer, SECONDS is a bash built-in

$call                                   # call binary

toc=$SECONDS                            # stop timer
delta=0
let "delta = toc - tic"                 # calculate time interval, integer arithmetic
echo "$SCRIPT: execute time (seconds): $delta"

# ---------------------------------
#  alert
# ---------------------------------

# additional reporting for long searches

test $SECONDS -gt $ALERT && test $(which beep) && beep -f 1800

# ---------------------------------
#  housekeeping
# ---------------------------------

echo "$SCRIPT: elapsed time (seconds): $SECONDS"
echo
exit $E_SUCCESS

#  $Id: siviz.alone.sh 9163 2012-03-06 09:35:12Z robbie $
#  end of file

