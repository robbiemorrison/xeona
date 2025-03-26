
#  file-purpose     : test 'xgrab' utility
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 07-Nov-2011 11:32 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 7962 $
#  $Date: 2011-11-15 12:46:02 +0100 (Tue, 15 Nov 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xgrab-src/testme.sh $

# ---------------------------------
#  user-modifiable settings
# ---------------------------------

xgrab="./xgrab"

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")                 # name of this script
E_SUCCESS=0
E_FAILURE=1
E_USAGE=2

# ---------------------------------
#  help message
# ---------------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        target="xgrab"                  # just for the help message
        echo
        cat << EOM
      usage: $SCRIPT             test '$target'
             $SCRIPT  --help     display this message and exit
    purpose: test '$target' utility using know XEM files
    expects: raw.xem run.xem
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

# ---------------------------------
#  confirm_file()
# ---------------------------------

errors=0
function confirm_file
{
    local filename="$1"

    if [ -f "$filename" ]
    then
        echo "$SCRIPT: file confirmed: '$filename'"
        return 0
    else
        echo "$SCRIPT: file not found: '$filename'"
        let "errors++"
        return 1
    fi
}

# ---------------------------------
#  callme()
# ---------------------------------

fails=0
count=0
function callme
{
    local cline="$1"
    local exret="$2"

    let "count++"
    local call="$xgrab $cline"
    echo
    echo "********************************************************************************"
    printf "call %02d: %s\n" "$count" "$call"
    echo
    eval "$call"
    local ret=$?
    echo
    case "$ret" in
        "$exret") echo "return: $ret"                                     ;;
        *)        echo "return: $ret / expected: $exret"; let "fails++"  ;;
    esac
}

# ---------------------------------
#  active code
# ---------------------------------

confirm_file "raw.xem"
confirm_file "run.xem"

test $errors -eq 0 || exit $E_FAILURE

callme "--help"                                                                  "0"
callme "--version"                                                               "0"
callme "--legal"                                                                 "0"
callme "                 no.xem"                                                "11"
callme "# no arguments"                                                          "2"
callme "--failmsg \"fail\" run.xem  \"entity.time-horizon.steps\""               "0"
callme "--failmsg \"fail\" run.xem  \"entity.time-horizon.bad-id\""             "24"
callme "--quiet          run.xem  \"duh.duh.duh\" || $xgrab --exitcode \$?"      "0"
callme "--quiet          raw.xem  \"entity.overseer.total-financial\""          "25"
callme "--wasrun         raw.xem"                                                "1"
callme "--exitcode 130"                                                          "0"
callme "--summary        run.xem"                                                "0"
callme "--debug --summary  run.xem  \"entity.time-horizon.steps\""               "0"
callme "--debug --wasrun  run.xem"                                               "0"

# ---------------------------------
#  housekeeping
# ---------------------------------

echo
echo "----"
case "$fails" in
    0) echo "$SCRIPT: all tests passed";          exit $E_SUCCESS  ;;
    *) echo "$SCRIPT: failures occurred: $fails"; exit $E_FAILURE  ;;
esac

#  $Id: testme.sh 7962 2011-11-15 11:46:02Z robbie $
#  end of file

