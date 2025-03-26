#! /bin/bash

#  file-purpose     : run 'pruf' and then optionally 'poweroff' / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Tue 10-Feb-2009 14:57 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 4601 $
#  $Date: 2010-04-14 09:02:37 +0200 (Wed, 14 Apr 2010) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/signoff.sh $

# ---------------------------------
#  settings
# ---------------------------------

PRUF="nice pruf"                        # principal call with any options
TAILNO=96                               # number of lines to 'tail'
DELAY=30                                # final delay in seconds

#  some test settings
#    PRUF="nice mach -a f/trav"
#    DELAY=5

# ---------------------------------
#  preliminaries
# ---------------------------------

SCRIPT=$(basename "$0" ".sh")           # this script

PATTERN="$SCRIPT-*.txt"                 # CAUTION: must match file name generation code

E_SUCCESS=0
E_ABANDON=1
E_USAGE=2
E_TEEFILE_EXISTS=64
E_NO_FILE=65

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
local text1="optional 'machcount' or 'text'"

echo
echo "       usage: $SCRIPT  --|--beepoff     run 'pruf' call and capture output"
echo "              $SCRIPT  --shutdown       as above but then \"sudo poweroff\""
echo "              $SCRIPT  --tail           show tail of latest $SCRIPT file"
echo "              $SCRIPT  --look [term]    use 'less' to search for $text1 in latest $SCRIPT file"
echo "              $SCRIPT  --rm             interactively remove $SCRIPT files"
echo "              $SCRIPT  --help           display this message and exit"
echo "       notes: user is prompted for active confirmation at the very outset"
echo "              can exit safely at any point with a 'Ctrl-C' interrupt"
echo "              hit 'q' to quit under '--look'"
echo "              suitable 'sudo' authority required for poweroff"
echo "   hardcodes: pruf call with options  : $PRUF"
echo "              teefile pattern         : $PATTERN"
echo "              poweroff delay          : $DELAY seconds"
echo "              tail size               : $TAILNO lines"
echo
}

# ---------------------------------
#  tail_logfile()
# ---------------------------------

function tail_logfile
{
    local lscap=($( ls --reverse $PATTERN )) # CAUTION: no soft-quotes for 'pattern'
    test $lscap || return $E_NO_FILE
    local latest=${lscap[0]}                 # dereference the first entry
    tail -$TAILNO $latest                    # open with 'tail'
    local lines=$( wc --lines $latest | awk '{ print $1}' )
    echo "---"
    echo "$SCRIPT: tail = $TAILNO   total = $lines   name = $latest"  # report
    return 0
}

# ---------------------------------
#  pattern_logfile()
# ---------------------------------

# 'less' options
#
#     '--RAW-CONTROL-CHARS' means output ANSI "color" escape sequences in "raw" form
#     '--chop-long-lines' means no line wrap

function pattern_logfile
{
    local pattern="$*"
    test -n "$pattern" &&
    {
        printf -v machcount "%d" "$pattern" 2>/dev/null && pattern="-- $pattern --"
        pattern="--pattern=\"$pattern\""
    }

    local lscap=($( ls --reverse $PATTERN )) # CAUTION: no soft-quotes for 'pattern'
    test $lscap || return $E_NO_FILE
    local latest=${lscap[0]}                 # dereference the first entry
    call="less --RAW-CONTROL-CHARS --chop-long-lines $pattern $latest"
    eval $call
    echo "$SCRIPT: call = $call"             # report
    return 0
}

# ---------------------------------
#  help message
# ---------------------------------

mode=""

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        display_help
        exit $E_SUCCESS
        ;;
    --beepoff|-b)
        shift
        mode="beepoff"
        ;;
    --shutdown|--shut)
        shift
        mode="shut"
        ;;
    --tail|-t)
        shift
        tail_logfile
        exit                            # with return value from 'tail_logfile'
        ;;
    --look|-l)
        shift
        pattern_logfile "$*"
        exit                            # with return value from 'pattern_logfile'
        ;;
    --remove|--rm|-r)
        shift
        rm --interactive $PATTERN
        exit $E_SUCCESS
        ;;
    --)
        mode="normal"
        ;;
    *)
        echo "$SCRIPT: invalid argument (try --help or perhaps --): $*"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  tee file name creation
# ---------------------------------

sleep 1                                 # ensure unique name
STAMP=$( date --utc '+%Y-%m-%dt%H-%M-%Sz' )
TEEFILE="$SCRIPT-$STAMP.txt"

test -f "$TEEFILE" &&                   # regular file test
{
    echo "$SCRIPT: fatal: created tee file exists (check name creation code): $TEEFILE"
    exit $E_TEEFILE_EXISTS
}

# ---------------------------------
#  user confirmation
# ---------------------------------

echo "$SCRIPT: pruf call = $PRUF"       # report the call
echo "$SCRIPT: mode      = $mode"       # current mode
echo "$SCRIPT: tee file  = $TEEFILE"    # report tee file name

case $mode in
    shut)           MSG="(turn off mains power too)"  ;;
    normal|beepoff) MSG="(retain mains power)"        ;;
    *)              MSG="(CODING ERROR: mode)"        ;;
esac

echo -n "  $SCRIPT: enter 'y' to continue $MSG: "
read response
if [ "$response" == "y" ]
    then
    echo "$SCRIPT: underway"
    SECONDS=0                           # reset elapsed time
else
    echo "$SCRIPT: abandoning script, no action taken"
    echo "$SCRIPT: elapsed time (seconds) : $SECONDS"
    exit $E_ABANDON
fi

# ---------------------------------
#  update_battery_status()
# ---------------------------------

# sample output
#
#      explanation
#        local Friday 03-Apr-2009 09:40
#        charging, 72%, 00:47:38 until charged
#        ok, 20.0 degrees C
#        on-line

battery_status=""

function update_battery_status2
{
    local tag="$1"                      # optional explanation tag

    test $( which acpi ) || {
        battery_status="    warning: 'acpi' utilty not found"
        return 1
    }

    test -z "$tag" && tag="status"      # default value
    local acpi=$( acpi --everything | gawk 'BEGIN { FS = ": "} { print "      " $2 }' )
    local date=$( date "+%d-%b-%Y %H:%M" )
    printf -v battery_status "    %s : %s\n%s" "$tag" "$date" "$acpi"
    return 0
}

# sample output
#
#     start : 06-Apr-2009 15:48 / charging, 62%, 01:04:16 until charged / on-line
#    finish : 06-Apr-2009 15:48 / charging, 62%, 01:04:01 until charged / on-line

function update_battery_status
{
    local tag="$1"                      # optional explanation tag

    test $( which acpi ) || {
        battery_status="    warning: 'acpi' utilty not found"
        return 1
    }

    test -z "$tag" && tag="status"      # default value

    local acpi=$( acpi -ab | gawk 'BEGIN { FS = ": "} { printf " / " $2 }' )
    local date=$( date "+ / %d-%b-%Y %H:%M" )
    printf -v battery_status "%6s%s%s" "$tag" "$date" "$acpi"
    return 0
}

# ---------------------------------
#  principal code
# ---------------------------------

# the following code is now Ctrl-C friendly, that is,
# the 'poweroff' call is within the subshell code block

(                                       # execute in subshell environment
    update_battery_status "start"       # function defined above

    SECONDS=0                           # reset

    $PRUF 2>&1                          # standard error needs redirection
    ret=$?                              # 'pruf' return as aggregate of 'mach' returns

    case $ret in
        0)
            # success
            printf "$SCRIPT: call '$PRUF' returned success: $ret\n"
            test "$mode" == "beepoff" || beep -f 1000 -r  2      # two beeps
            ;;
        *)
            # failure
            printf "$SCRIPT: call '$PRUF' returned FAILURE: $ret\n"
            test "$mode" == "beepoff" || beep -f 1000 -r 10      # ten beeps
            ;;
    esac

    sync || printf "$SCRIPT: 'sync' (write to disk) call return: $?\n"

    case "$mode" in
        shut)
            # sleep
            printf "$SCRIPT: will power down in $DELAY seconds\n"
            sleep $DELAY

            # report battery status
            printf "$SCRIPT: battery status: %s\n" "$battery_status"  # start information recovered at outset
            update_battery_status "finish"
            printf "$SCRIPT: battery status: %s\n" "$battery_status"  # finish information from previous line

            # report elapsed time
            elapsed=$SECONDS
            test $( which hms ) && elapsed=$( hms $elapsed )          # 'hms' is a my (user-local) utility
            printf "$SCRIPT: elapsed time: $elapsed\n"

            # poweroff
            printf "$SCRIPT: about to call 'sudo poweroff'\n"
            sudo poweroff ||            # CAUTION: call requires appropriate 'sude' authority
            {
                printf "$SCRIPT: power down call FAILURE, 'sudo poweroff' returned: $?\n"
            }

            # exit
            exit $ret                   # will probably get here because the script keeps running momentarily
            ;;
        normal|beepoff)
            # report battery status
            printf "$SCRIPT: battery status: %s\n" "$battery_status"  # start information recovered at outset
            update_battery_status "finish"
            printf "$SCRIPT: battery status: %s\n" "$battery_status"  # finish information from previous line

            # exit
            printf "$SCRIPT: completing in 'noshut' mode\n"
            exit $ret
            ;;
    esac

) | tee $TEEFILE                        # CAUTION: 'ret' assignment evaporates here

# ---------------------------------
#  housekeeping
# ---------------------------------

# none required!

#  signoff: call 'nice pruf' returned success: 0
#  signoff: will power down in 30 seconds
#  signoff: battery status:  start / 24-Feb-2010 14:31 / charged, 100% / on-line
#  signoff: battery status: finish / 24-Feb-2010 15:43 / charged, 100% / on-line
#  signoff: elapsed time: 01:11:52
#  signoff: about to call 'sudo poweroff'

#  $Id: signoff.sh 4601 2010-04-14 07:02:37Z robbie $
#  end of file
