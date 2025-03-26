#! /bin/bash

#  file-purpose     : check text issues using 'textcheck' utility / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Wed 28-Nov-2007 13:34 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 8557 $
#  $Date: 2011-12-22 09:00:25 +0100 (Thu, 22 Dec 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/textcheck.sh $

# ---------------------------------
#  preamble
# ---------------------------------

TEXTCHECK="textcheck"                   # name of my text check utility

SCRIPT=$(basename "$0")                 # for identifying this script

LONGLINE=90
IGNORELINE=999                          # line length must lie between 0 and 999 inclusive

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2
E_NO_UTILITY=64

# ---------------------------------
#  utility_check()
# ---------------------------------

function utility_check
{
    if [ $(which "$TEXTCHECK") ]
    then
        return 0
    else
        echo "$SCRIPT: required utility not found: $TEXTCHECK"
        echo
        return 1
    fi
}

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
    echo
    cat << EOM
       usage: $SCRIPT             run 'textcheck' on 'xeona' source code
              $SCRIPT  <subdir>   run 'textcheck' on given subdirectory (faster)
              $SCRIPT  [opt]      run 'textcheck' on subset (see below)
              $SCRIPT --help      display this message and exit
     options:  --code              source code
               --elisp             elisp
               --R                 R scripts
               --scripts           bash scripts
     purpose: wrapper to 'textcheck' utility (see below)
EOM
    echo
    utility_check && textcheck --help
}

# -------------------------
#  patterns
# -------------------------

pats1=". a b c d e f h i ../bin/xgrab ../bin/xstat"                             # "g" is omitted, *.{h,cc}
pats2="xeona-xmoks ../pocmods"                                                  # *.guard.xem
pats3=". ./xeona-xmoks ../models ../models/scripts ../pocmods"                  # *.sh
pats4="../scripts"                                                              # *
pats5="../elisp"                                                                # *.el
pats6="../xeonar ../models/scripts ../rpacks/robbie/R ../rpacks/xem/R"          # *.R

# -------------------------
#  process command-line
# -------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        display_help
        exit $E_SUCCESS
        ;;
    [a-z])
        pats1="$1"                                     # *.{h,cc}
        shift
        pats2=""
        pats3=""
        pats4=""
        pats5=""
        pats6=""
        echo
        echo "$SCRIPT: operating on subdirectory $pats1"
        ;;
    --code|-c)
        pats2=""
        pats3=""
        pats4=""
        pats5=""
        pats6=""
        echo
        echo "$SCRIPT: operating on code"
        ;;
    --elisp|-e|--lisp|-l)
        pats1=""
        pats2=""
        pats3=""
        pats4=""
        pats6=""
        echo
        echo "$SCRIPT: operating on elisp"
        ;;
    --R|--r|-r)
        pats1=""
        pats2=""
        pats3=""
        pats4=""
        pats5=""
        echo
        echo "$SCRIPT: operating on R code"
        ;;
    --scripts|-s)
        pats1=""
        pats2=""
        pats5=""
        pats6=""
        echo
        echo "$SCRIPT: operating on bash scripts"
        ;;
    ""|--)                              # the '--' is simply for convenience
        echo
        ;;
    *)
        echo "$SCRIPT: invalid input (try --help): $@"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  active code
# ---------------------------------

utility_check || exit $E_NO_UTILITY

status="success"

echo "$SCRIPT: standard line $LONGLINE, extended line $IGNORELINE"

# source code files
for pat in $pats1
do
    textcheck -$LONGLINE ${pat}/*.{h,cc} || status="failure"
done

# test model files
for pat in $pats2
do
    textcheck -$IGNORELINE ${pat}/*.guard.xem || status="failure"
done

# test scripts 1
for pat in $pats3
do
    textcheck -$IGNORELINE ${pat}/*.sh || status="failure"
done

# test scripts 2
for pat in $pats4
do
    textcheck -$IGNORELINE ${pat}/* || status="failure"
done

# test elisp
for pat in $pats5
do
    textcheck -$IGNORELINE  ${pat}/*.el || status="failure"
done

# test R scripts
for pat in $pats6
do
    textcheck -$IGNORELINE  ${pat}/*.R || status="failure"
done

# ---------------------------------
#  completion reporting and exit
# ---------------------------------

case $status in
    success)
        echo "$SCRIPT: no text issues detected"
        echo
        exit $E_SUCCESS
        ;;
    failure)
        echo "$SCRIPT: text problems encountered **"
        echo
        beep
        exit $E_FAILURE
        ;;
esac

#  $Id: textcheck.sh 8557 2011-12-22 08:00:25Z robbie $
#  end of file

