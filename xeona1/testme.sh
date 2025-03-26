#! /bin/bash

#  file-purpose     : tarball test script / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 18-Jan-2010 16:46 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 5532 $
#  $Date: 2010-11-22 19:45:13 +0100 (Mon, 22 Nov 2010) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/testme.sh $

#  note: this script can identify the binary in two ways, based on:
#
#    * the current directory string                              << currently active
#    * the highest revision number in the current directory

# ---------------------------------
#  preamble
# ---------------------------------

REPORTLEVEL=6                           # 'xeona' reporting level
LOGXTN="log"                            # logfile extension
MODELSDIR="xeona-xmoks"                 # subdirectory containing the guard models

SCRIPT=$(basename "$0")                 # strip path

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2

ERRORS=0                                # global error counter

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
    echo
    echo "        usage: $SCRIPT           test this xeona tarfile"
    echo "               $SCRIPT --help    display this message and exit"
    echo "      purpose: run xeona application and model tarballs"
    echo "         note: current directory leaf : $(basename $PWD)"
    echo "               $ rm --force --verbose ${SCRIPT%.sh}.[0-9][0-9][0-9][0-9][0-9][0-9].log"
    echo "    hardcodes: model subdirectory : $MODELSDIR"
    echo "               xeona report level : $REPORTLEVEL"
    echo "               logfile extension  : .$LOGXTN"
    echo
    return 0
}

# ---------------------------------
#  report()
# ---------------------------------

reports=""                              # collected reports for display on completion

function report
{
    local msg="$1"
    case $# in
        0) echo                 | tee --append $LOGFILE  ;;
        1) echo "$SCRIPT: $msg" | tee --append $LOGFILE  ;;
    esac
    printf -v reports "$reports  $msg\n"
}

# ---------------------------------
#  process command-line
# ---------------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        shift
        display_help
        exit $E_SUCCESS
        ;;
    "")
        :                               # fall thru is correct
        ;;
    *)
        echo "$SCRIPT: incorrect argument (try --help for usage): $*"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  make_logname()
# ---------------------------------

# create logfile name
# note 999999/3600/24 = 11.6 days

# CAUTION: remember to update the help message if the
# logic changes here

LOGFILE=""

function make_logname
{
    local svn="$1"                      # CAUTION: 'svn' not currently used

    sleep 1                             # to enable name uniqueness

    local base="${SCRIPT%.*}"           # strip, for instance, ".sh"
    local secs=$(date --utc "+%s")      # seconds since 1970
    secs=${secs:4}                      # trim first 4 chars -- thus no repeats within 11 days

    LOGFILE="$base.$secs.$LOGXTN"
}

# ---------------------------------
#  clear_logfile()
# ---------------------------------

# remove logfile of same name if present

function clear_logfile
{
    test -f "$LOGFILE" &&
    {
        rm --force $LOGFILE             # CAUTION: order of statements critical
        report "info: clear_logfile(): cleared logfile: $LOGFILE"     # for testing
    }
}

# ---------------------------------
#  highest_release_binary()
# ---------------------------------

BINARY=

# assumes release binary has 4 digits

# for 'find', both work, the second removes the "./"
# %p  file's name
# %P  file's name with the name of the command line argument under which it was found removed

function highest_release_binary
{
    # CAUTION: note the outer "()" on the 'glob' line
    local glob
    local bin
    glob=($(find -P . -maxdepth 1 -name "xeona.r[0-9][0-9][0-9][0-9]" -type f -printf "%p\n" | sort --reverse))
    bin=${glob[0]}                      # grab highest release number
    bin=${bin#./}                       # strip any leading "./"
    test -n "$bin" && BINARY="$bin"
    report "info: highest_release_binary(): binary name: $bin"
    return 0
}

# ---------------------------------
#  subleaf_release_binary()
# ---------------------------------

BINARY=

# extracts current directory leaf

function subleaf_release_binary
{
    local bin
    bin=$(basename "$PWD")
    test -n "$bin" && BINARY="$bin"
    report "info: subleaf_release_binary(): binary name: $bin"
    return 0
}

# ---------------------------------
#  xemtests()
# ---------------------------------

function xemtests
{
    local noise="$1" # :-6}                 # ":" includes empty, "-" is not reassign host

    local modeldir="$MODELSDIR"
    local binary="./$BINARY"

    test -f "$binary" ||
    {
        report "WARN: xemtests(): binary not found: $binary"
        let "ERRORS++"
        return 1
    }

    test -x "$binary" ||
    {
        report "WARN: xemtests(): binary found but lacks an execute permission: $binary"
        let "ERRORS++"
        return 2
    }

    test -d "$modeldir" ||
    {
        report "WARN: xemtests(): subdirectory not found: $modeldir"
        let "ERRORS++"
        return 3
    }

    local trail=".guard.xem"
    local xems=$(find "$modeldir" -name "*$trail")
    local rets=""                       # string of concatenated returns
    local fails=0                       # fails count

    for xem in $xems
      do
      xem=${xem%$trail}
      call="$binary --guard --report $noise --file $xem"
      report "info: xemtests(): call = $call"
      eval "$call" 2>&1 | tee --append $LOGFILE
      ret=${PIPESTATUS[0]}              # must follow call chain, 'PIPESTATUS' is built-in
      test $ret -eq 0 || let "ERRORS++"
      test $ret -eq 0 || let "fails++"
      rets="$rets $ret"
    done

    report "info: xemtests(): returns =$rets"
    test $fails -eq 0 || report "WARN: xemtests(): fails recorded, count: $fails"
    test $fails -eq 0 && report "info: xemtests(): test success"

    return 0
}

# ---------------------------------
#  active
# ---------------------------------

make_logname "$svn"
clear_logfile
printf "\n---\n\n" >> $LOGFILE

date=$(date "+%Z %z %A %d-%b-%Y %H:%M")
host=$(hostname --long)
user=$(id --user --name)
report "info: main code begins"
report "meta: timestamp = $date"
report "meta: host = $host  user = $user"
report "info: xeona reporting level (0-6): $REPORTLEVEL"

highest_release_binary                  # highest in current directory
subleaf_release_binary                  # same as subdirectory leaf

report "info: model tests commence"
xemtests "$REPORTLEVEL"
if [ $? = 0 ]
    then
    report "info: model tests complete"
else
    report "info: model tests abandoned"
fi

lines=$(wc --lines $LOGFILE | awk '{ print $1 }')
report "info: logfile lines (about 27 short): $lines"
report "call: view logfile: less +G $LOGFILE"
report "info: elapsed time: $SECONDS seconds"

if [ $ERRORS -eq 0 ]
    then
    report "info: overall assessment: PASS"
else
    report "info: overall assessment: ** FAIL **"
fi

# ---------------------------------
#  completion reporting
# ---------------------------------

test -n "$reports" &&
{
    echo
    echo "$SCRIPT: reports repeated for convenience"
    echo
    echo -n "$reports"                  # CAUTION: soft-quotes necessary, "-n" for preexisting newline
    echo
} | tee --append $LOGFILE

printf "===\n\n" >> $LOGFILE

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $E_SUCCESS

#  $Id: testme.sh 5532 2010-11-22 18:45:13Z robbie $
#  end of file

