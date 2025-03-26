#! /bin/bash

#  file-purpose     : report on progress during scenario set runs
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 20-Feb-2012 12:05 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9122 $
#  $Date: 2012-02-22 09:15:52 +0100 (Wed, 22 Feb 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/xtail.sh $

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")                 # name of this script

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2

# ---------------------------------
#  help
# ---------------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        echo
        cat << EOM
       usage: $SCRIPT            run script
              $SCRIPT  --con     less the console file
              $SCRIPT  --time    show start timestamp, current timestamp, and elapsed time
              $SCRIPT  --watch   run script under 'watch' (^C to quit)
              $SCRIPT  --help    display this message and exit
     purpose: review running or complete scenario set by examining XOG files
     assumes: a sole 'con' file or a (possibly empty) set of 'xog' files
    examples: add options to 'watch' : $ watch -n 2 -d $SCRIPT
              'cat' the console file : $ $SCRIPT --con | cat
EOM
        echo
        exit $E_SUCCESS
        ;;
    "")
        :                               # do nothing is correct
        ;;
    --con|-c)
        shift
        con=$(find . -maxdepth 1 -type f -name "*.con" -print)   # 'con' console file
        less +G "$con"
        exit
        ;;
    --watch|-w)
        shift
        watch -n 1 "$SCRIPT"            # recursive call
        $SCRIPT --time                  # reports time on Ctrl-C
        exit
        ;;
    --time|-t)
        shift
        con=$(find . -maxdepth 1 -type f -name "*.con" -print)   # 'con' console file
        was=$(grep "  timestamp" "$con" | sed 's/^.*: //')
        now=$(date --utc +'%a %d-%b-%Y %H:%M:%S %Z')
        echo "$was"
        echo "$now"
        swas=$(date --utc --date="$was" +'%s')                   # parse string and output seconds since 1970
        snow=$(date --utc --date="$now" +'%s')
        diff=$(($snow - $swas))
        test $( which hms ) && diff=$(hms $diff)                 # 'hms' is a my (user-local) utility
        echo "$diff"
        exit
        ;;
    *)
        echo "$SCRIPT: invalid usage (try --help)"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  report_xog()
# ---------------------------------

function report_xog
{
    local xog="$1"

    local last=$(tail -1 "$xog")        # grab current last line
    local call=""
    case "$last" in
        *"CTA"*)
            call="tail -1  $xog"
            echo -n "$xog    "
            eval "$call"
            ;;
        *"problematic exit"*)
            call="tail -35 $xog | sed -n '/^[0-9]/'p | sed 's/^/  /'"
            echo "$xog"
            echo
            eval "$call"
            echo
            ;;
        *"full success"*)
            call="tail -1  $xog"        # 11
            echo -n "$xog    "
            eval "$call"
            ;;
        *)
            call="tail -40 $xog"
            echo "$xog"
            echo
            echo "  unexpected last line = $last "
            echo
            eval "$call"
            echo
            ;;
    esac

    return
}

# ---------------------------------
#  active code
# ---------------------------------

# locate xogs

fcall="find . -maxdepth 1 -type f -name \"*.?.xog\" -printf %f\\\n"
xogs=$(eval "$fcall | sort")

# loop xogs

echo
for xog in $xogs
do
    report_xog "$xog"
done
echo

# ---------------------------------
#  housekeeping
# ---------------------------------

#  $Id: xtail.sh 9122 2012-02-22 08:15:52Z robbie $
#  end of file

