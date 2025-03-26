#! /bin/bash

#  file-purpose     : stand-alone build and run of the 'GlpkViz' class / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Thu 05-Jun-2008 22:25 UTC
#  file-status      : working (more or less)
#  file-keywords    : xeona

#  $Revision: 3600 $
#  $Date: 2009-10-22 10:48:30 +0200 (Thu, 22 Oct 2009) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/d/glpkviz.alone.sh $

# ---------------------------------
#  preamble
# ---------------------------------

BINARY="glpkviz.alone"                  # default binary name, can be overridden
ASSOC="glpkviz.alone.cc"                # associated file
SDIR="d"                                # hardcoded subdirectory

SCRIPT=$(basename "$0")                 # script leafname

E_SUCCESS=0
E_WRONG_PWD=64

# -------------------------
#  process command-line
# -------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|-\?)
        echo
        echo "     usage: $SCRIPT  [binary-name]   build and run stand-alone GlpkViz unit"
        echo "            $SCRIPT  -h --help       show this message and exit"
        echo "   default: binary name  : $BINARY"
        echo "  hardcode: subdirectory : $SDIR"
        echo
        exit $E_SUCCESS
        ;;
esac

test -n "$1" && BINARY="$1"             # take from command-line if given

# ---------------------------------
#  confirm call directory
# ---------------------------------

if [ "$(basename $(pwd))" != "$SDIR" ]
    then
    echo "$SCRIPT: called from wrong directory, should be in subdirectory: $SDIR"
    exit $E_WRONG_PWD
fi

# ---------------------------------
#  build binary
# ---------------------------------

echo
cd ..                                   # drop down to current base directory
echo "$SCRIPT: in directory: $(pwd)"
echo "$SCRIPT: about to call make"
echo

# note 'CPPFLAG_SVNREV' futz which allows the makefile
# to run when the svn version is stale

# the internal macro 'XE_GLPKVIZ_ALONE' is used to
# select for the stand-alone code

make --file "makefile" \
CPPFLAG_SVNREV="-D_XSVNREV=0 -DXE_GLPKVIZ_ALONE=1" \
binary="$SDIR/$BINARY" \
sources="common.cc $SDIR/glpkviz.cc $SDIR/$ASSOC a/logger.cc a/exbase.cc a/exapp.cc"

# ---------------------------------
#  open firefox as needed
# ---------------------------------

# delay: if firefox is running, use a 1s delay, else longer

secs=10                                  # delay after invoking firefox

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
        echo "$SCRIPT: unknow return: $ret"
        ;;
esac

# ---------------------------------
#  run binary
# ---------------------------------

cd $SDIR
echo "$SCRIPT: in directory: $(pwd)"
echo "$SCRIPT: about to run binary: $BINARY"

./$BINARY                               # call binary

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $E_SUCCESS

#  $Id: glpkviz.alone.sh 3600 2009-10-22 08:48:30Z robbie $
#  end of file

