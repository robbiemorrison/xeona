#! /bin/bash

#  file-purpose     : get, run, say a xeona suite / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Thu 17-Nov-2011 13:15 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 8728 $
#  $Date: 2011-12-30 18:18:39 +0100 (Fri, 30 Dec 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/allstudy.sh $

# note that --jobs can be set
#
#     0   : run as many jobs as possible (it seems, the documentation is not clear)
#    +0   : run no-of-cores jobs
#    -1   : run no-of-cores minus one jobs

# ---------------------------------
#  preamble
# ---------------------------------

XOPTS=""
XOPTS=" -R"                             # extra 'xsenario.sh' options
XOPTS=" --jobs=-1 -R -t dumb"           # extra 'xsenario.sh' options
XOPTS=" --jobs=-1 -R"                   # extra 'xsenario.sh' options

run="fake"                              # development run
run="real"                              # genuine run

sep="---"                               # between calls separator

defnum="008"

getstudy="getstudy.sh"
runstudy="runstudy.sh"

SCRIPT=$(basename "$0")                 # name of this script

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2
E_BAD_POCNUM=10

# ---------------------------------
#  display_usage()
# ---------------------------------

function display_usage
{
    local ooo="$1"
    echo
    cat << EOM
      usage: $SCRIPT  $ooo         process study '$ooo'
             $SCRIPT  $ooo 0       also pass -m '0'
             $SCRIPT  $ooo 0000    also pass -c '0000'
             $SCRIPT  -p [$ooo]    call: $runstudy --patch-log $ooo
             $SCRIPT  -x [$ooo]    call: $runstudy --xeona-log $ooo
             $SCRIPT  --help      display this message and exit (takes priority)
             $SCRIPT  --Help      display all relevant help messages and exit (takes priority)
    purpose: get and run a xeona study (intentionally simple)
       note: two or three continuation prompts at the beginning
  hardcodes: default study number = $defnum
    example: one step test = $SCRIPT $ooo 6
             full year run = $SCRIPT $ooo 8
EOM
    echo
}

# ---------------------------------
#  process command-line
# ---------------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        shift
        case "$1" in
            [0-9][0-9][0-9]) ooo="$1"       ;;
            *)               ooo="$defnum"  ;;
        esac
        display_usage "$ooo"
        exit $E_SUCCESS
        ;;
    --Help|--Hel|--He|--H|-Help|-Hel|-He|-H|--HELP|--HEL|--HE|--H|-HELP|-HEL|-HE|-H)
        shift
        $SCRIPT --help
        echo "$SCRIPT: calling runstudy.sh --help"
        runstudy.sh --help
        echo "$SCRIPT: calling xsenario.sh --help"
        xsenario.sh --help
        exit
        ;;
    [0-9][0-9][0-9])
        pocnum="$1"
        shift
        ;;
    --patch-log|--patchlog|-p)
        shift
        num="$1"
        test -n "$num" || num="$defnum"
        plcall="$runstudy --patch-log $num"
        echo
        echo "$SCRIPT: call to: $plcall"
        eval "$plcall" || echo          # add trailing blank line on failure
        exit
        ;;
    --xeona-log|--xeonalog|-x)
        shift
        num="$1"
        test -n "$num" || num="$defnum"
        xcall="$runstudy --xeona-log $num"
        echo
        echo "$SCRIPT: call to: $xcall"
        eval "$xcall" || echo           # add trailing blank line on failure
        exit
        ;;
    *)
        echo "$SCRIPT: no go with pocmodel '$1'"
        exit $E_USAGE
        ;;
esac
case "$1" in
    [6-9])
        xopts=" -m $1"
        shift
        ;;
    [0-7][6-9][0-9]|[0-7][6-9][0-9][0-9])
        xopts=" -c $1"
        shift
        ;;
    "")
        :                               # fall thru is correct
        ;;
    *)
        echo "$SCRIPT: no go with additional argument '$1'"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  active code
# ---------------------------------

# create call
qsep="\"$sep\""
runstudyall="$runstudy$XOPTS$xopts"
call="$getstudy $pocnum && echo $qsep && $runstudyall $pocnum"

echo
echo "  call : $call"

# user-friendly reporting
echo
len=${#runstudyall}
printf "  %-*s  %s\n" $len "$getstudy"    "$pocnum"
printf "  %-*s  %s\n" $len "$runstudyall" "$pocnum"

# seek confirmation
echo
prompt="  hit 'y' to run: "
read -n 1 -p "$prompt" response          # "-n 1" is read one character and return
echo
case "$response" in
    y|Y)
        echo
        echo "$sep"
        case "$run" in
            fake) echo; echo "$call"; echo  ;;
            real) eval "$call"              ;;
        esac
        ret=$?
        elapsed=$SECONDS
        test $(which hms) && elapsed=$(hms $elapsed)  # 'hms' is a my (user-local) utility
        echo "  $SCRIPT: elapsed (hms or seconds) : $elapsed"
        case "$ret" in
            0) echo "  $SCRIPT: call chain return        : $ret (success)"  ;;
            *) echo "  $SCRIPT: call chain return        : $ret (failure)"  ;;
        esac
        echo
        exit $ret
        ;;
    *)
        exit $E_FAILURE
        ;;
esac

#  $Id: allstudy.sh 8728 2011-12-30 17:18:39Z robbie $
#  end of file

