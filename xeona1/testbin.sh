#! /bin/bash

#  file-purpose     : run application using various command-line options and filters / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Wed 25-Jul-2007 12:22 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 2774 $
#  $Date: 2009-05-22 06:45:00 +0200 (Fri, 22 May 2009) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/testbin.sh $

# ---------------------------------
#  preamble
# ---------------------------------

# default list of binaries to try in order given

DEFAULT="
xeona.mach
xeona.make
"

# the 'time' utility needs a full path to distinguish
# it from the shell builtin
#
#   --quiet       : do not report the status of the program
#   --portability : POSIX output
#   --verbose     : one line per resource use information
#
# using the shell keyword 'time'
#
#   -p            : POSIX output

TIMEV="/usr/bin/time --quiet --verbose"
TIMEP="/usr/bin/time --quiet --portability"
TIMEB="time -p"

TIME=$TIMEV                             # nominate one to use

TAB=40                                  # end-of-run reporting alignment

SCRIPT=$(basename "$0")

E_SUCCESS=0
E_FAIL=1
E_USAGE=2

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
    echo
    echo "      usage : $SCRIPT  --cycle   [binary-name]             cycle calls on given or latest 'xeona'"
    echo "              $SCRIPT  --filter  <binary-name> [options]   simply filter for stderr"
    echo "              $SCRIPT  --help                              display this message and exit"
    echo "   purposes : cycle  : run given or latest 'xeona' under various command-line options and summarize outcomes"
    echo "              filter : redirect stderr to oblivion"
    echo "    default : 'xeona' list : $DEFAULT xeona.r* (reverse sort)"
    echo
    return 0
}

# -------------------------
#  process command-line
# -------------------------

DEFAULT=$(echo $DEFAULT)

mode=""

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        display_help
        exit $E_SUCCESS
        ;;
    --cycle|-c)
        mode="cycle"
        shift
        bins=$(find -name 'xeona.r*' -type f -print | sort --reverse --numeric)
        BINLIST="$1 $DEFAULT $bins"
        shift
        BINLIST=$(echo $BINLIST)
        ;;
    --filter|-f)
        mode="filter"
        shift
        COMMAND="$*"
        ;;
    "")
        echo "$SCRIPT: require either --cycle or --filter (or try --help)"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  define separator line
# ---------------------------------

# create line
char=":"                                # line character
len=$(stty size | gawk '{ print $2 }')  # dynamic setting
let "len = len - 2"
for i in $(seq $len); do line="$line$char"; done

# colorize line
case "$TERM" in
    dumb) line="$line"                                ;;
    *)    line=$(echo -e "\033[01;35m$line\033[00m")  ;;    # 31=red 35=magenta
esac

# ---------------------------------
#  is_cwd_on_path()
# ---------------------------------

function is_cwd_on_path                 # current working directory
{
    echo $PATH | gawk 'BEGIN { RS = ":" } { if ($1 == ENVIRON["PWD"]) exit 1 }'
    test $? -eq 1 && return 0           # cwd on path (1 is coded exit from gawk, else 0)
    return 1                            # cwd not on path
}

# ---------------------------------
#  establish_cwd_path()
# ---------------------------------

path=""
function establish_cwd_path
{
    is_cwd_on_path || path="./"
}

# ---------------------------------
#  establish_bin()
# ---------------------------------

bin=""                                  # binary to call

function establish_bin
{
    for file in $BINLIST
      do
      if [ -x "$file" ]                 # file exists and is user executable
          then
          bin="$path$file"
          return 0
      fi
    done

    echo "$SCRIPT: cannot locate a usable executable: $BINLIST"
    exit $E_FAIL
}

# ---------------------------------
#  call()
# ---------------------------------

endrep=""
function call
{
    local args="$1"
    local callstr="$bin  $args"
    local interpretation=""

    echo
    echo "call    : $callstr"
    eval "$TIME  $callstr"
    ret=$?
    interpretation=$( $bin --output $ret )
    test $ret -eq 0 && interpretation="success"   # simplify meeting
    echo
    echo "call    : $callstr"
    echo "return  : $ret ($interpretation)"
    echo
    let "rets += $ret"
    let "++loops"
    printf -v endrep "$endrep  %-*s %2d   %s\n" $TAB "$callstr" $ret "$interpretation"
    echo "$line"
}

# ---------------------------------
#  cycle_calls()
# ---------------------------------

OKAY_TOTAL=49                           # reset as necessary, based on the cycle of calls

function cycle_calls
{
    establish_bin

    # call ""
    call "--help"
    call "--nonexistent_option"
    call "nonexistent_model.xem"
    call "--inbuilt 24"
    call "inbuilt"

    printf -v endrep "$endrep  %-*s --\n"  $TAB ""
    printf -v endrep "$endrep  %-*s %2d\n" $TAB "total" $rets

    echo
    echo "completion reporting"
    echo "script: $SCRIPT"
    echo "elapsed time (seconds) : $SECONDS"
    echo "call results:"
    echo
    echo "$endrep"

    if [ $rets -eq $OKAY_TOTAL ]
        then
        printf "  outcome: good total: %d as expected\n" $OKAY_TOTAL
        echo
        return $E_SUCCESS
    else
        printf "  outcome: PROBLEM total: got %d, expecting %d **\n" $rets $OKAY_TOTAL
        echo
        return $E_FAILURE
    fi
}

# ---------------------------------
#  filter()
# ---------------------------------

function filter
{
    $COMMAND 2>/dev/null
    ret=$?
    echo "$SCRIPT: elapsed time (seconds) : $SECONDS"
    return $ret                         # command return
}

# ---------------------------------
#  main code
# ---------------------------------

establish_cwd_path

case "$mode" in
    cycle)  cycle_calls  ;;
    filter) filter       ;;
esac

# ---------------------------------
#  housekeeping
# ---------------------------------

exit                                    # exit with most recent function return value

#  $Id: testbin.sh 2774 2009-05-22 04:45:00Z robbie $
#  end of file

