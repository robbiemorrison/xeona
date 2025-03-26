#! /bin/bash

#  file-purpose     : list run comment files in directory order
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 26-Dec-2011 22:12 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9111 $
#  $Date: 2012-02-20 16:43:10 +0100 (Mon, 20 Feb 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/xattic.sh $

#  NOTES
#
#     comment file prepared by 'runstudy.sh' but the
#     displayed output determined by function
#     'report_comfile' in this code

# ---------------------------------
#  settings
# ---------------------------------

study="trial-008"
sext=".txt"
xext=".xem"
lext=".log"

catalogs="catalogs"
attic="attic"
sdirpat="[0-9][0-9]"

xtail="xtail.sh"

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2

# ---------------------------------
#  help
# ---------------------------------

mode="(not set)"

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        echo
        cat << EOM
        usage: $SCRIPT  --list       show run logs
               $SCRIPT  --tail       show tails
               $SCRIPT  --shift 00   shift suite to attic directory 00
               $SCRIPT  --rm    00   remove suite in attic directory 00 after confirmation
               $SCRIPT  --update     update the default reference log
               $SCRIPT  --help       display this message and exit
      purpose: manage the attic branch where select 'xeona' suites can be stored
EOM
        echo
        exit $E_SUCCESS
        ;;
    --shift|-s)
        shift
        mode="shift00"
        target="$1"
        ;;
    --tail|-t)
        if [ ! $(which "$xtail") ]       # CAUTION: no (test) -n and no soft-quotes
        then
            echo "$SCRIPT: required utility missing: $xtail"
            exit $E_FAILURE
        fi
        shift
        mode="tail"
        target="$1"
        ;;
    --rm|-r)
        shift
        mode="rm00"
        target="$1"
        ;;
    --update|-u)
        shift
        mode="update"
        ;;
    --list|-l)
        shift
        mode="showall"
        ;;
    *)
        echo "$SCRIPT: incorrect usage (try --help): $@"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  functions
# ---------------------------------

# ---------------------------------
#  report_steps()
# ---------------------------------

function report_steps
{
    local dir="$1"
    local resleta=".+"                                    # CAUTION: cannot be "" because then hits unrun XEM file

    local fqf="entity.time-horizon.steps"
    local xem="$dir/$study$resleta$xext"                  # no scenario leta
    local steps=$(xgrab --quiet "$xem" "$fqf")
    test -n "$steps" && echo "steps      : $steps"
}

# ---------------------------------
#  report_comfile()
# ---------------------------------

# 'sed' calls 'np' print the nth line
# note the ability to reorder the output
# the 'sed' calls fail gracefully if the defined line does not exist

function report_comfile
{
    local dir="$1"
    local comfile="$dir/$study$sext"

    echo
    echo "dir        : ${comfile%/$study$sext}"
    sed --silent '3p' "$comfile" 2>/dev/null
    sed --silent '4p' "$comfile" 2>/dev/null
    sed --silent '6p' "$comfile" 2>/dev/null
    sed --silent '5p' "$comfile" 2>/dev/null
}

# ---------------------------------
#  report_all()
# ---------------------------------

function report_all
{
    local base="$1"
    local dirs=$(find -P "$base" -maxdepth 1 -name "$sdirpat" -type d -printf "%p\n" | sort)
    for dir in $dirs
    do
        report_comfile "$dir"
        report_steps   "$dir"
    done
    echo
}

# ---------------------------------
#  tail_all()
# ---------------------------------

function tail_all
{
    local base="$1"
    local dirs=$(find -P "$base" -maxdepth 1 -name "$sdirpat" -type d -printf "%p\n" | sort)
    echo
    for dir in $dirs
    do
        local num=$(basename "$dir")
        echo "=== $num ==="
        report_all "$dir"               # report all call
        (cd "$dir" && $xtail)           # run commands in subshell
    done
}

# ---------------------------------
#  confirm_pat()
# ---------------------------------

function confirm_pat
{
    local test="$1"

    case "$test" in
        $sdirpat)                       # CAUTION: omit soft-quotes
            return 0
            ;;
        *)
            echo "  pattern match $sdirpat failed: $test"
            exit $E_FAILURE
            ;;
    esac
}

# ---------------------------------
#  rm00()
# ---------------------------------

function rm00
{
    local dir="$1"

    # preamble
    confirm_pat "$dir"

    local target="$attic/$dir"
    if [ ! -d "$target" ]
    then
        echo "  target directory not found (abandoning task): $target"
        return 1
    fi

    # seek confirmation, use simple key strike
    local prompt="  hit 'y' to delete '$target': "
    read -n 1 -p "$prompt" response     # "-n 1" is read one character and return
    echo
    case "$response" in
        y|Y)
            rm --force --recursive "$target"
            echo "  target directory deleted: $target"
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

# ---------------------------------
#  shift00()
# ---------------------------------

function shift00
{
    local dir="$1"

    confirm_pat "$dir"

    local target="$attic/$dir"
    if [ -d "$target" ]
    then
        echo "  target directory exists (abandoning task): $target"
        return 1
    else
        mkdir "$target"
        local call="mv $study* $target"
        eval "$call"
        echo "  shifted study files to target directory: $target"
        return 0
    fi
}

# ---------------------------------
#  update_reflog()
# ---------------------------------

function update_reflog
{
    local log="$study$lext"

    local reflog="$catalogs/$log"
    local trunk=${XEONA%/xeona1}
    local branch="$trunk/models"
    local target="$branch/$reflog"
    if [ -f "$target" ]
    then
        cp --force --backup="simple" "$log" "$target"
        test "$PWD" == "$branch" && echo "  updated reference log file (simple backup made): $reflog"
        test "$PWD" != "$branch" && echo "  updated reference log file (simple backup made): $target"
    else
        echo "  target log file not found (abandoning task): $target"
    fi
}

# ---------------------------------
#  main code
# ---------------------------------

trunk=${XEONA%/xeona1}
absattic="$trunk/models/$attic"         # absolute path for 'attic' just for "showall" mode

case "$mode" in
    showall) report_all    "$absattic"  ;;
    tail)    tail_all      "$absattic"  ;;
    shift00) shift00       "$target"    ;;
    rm00)    rm00          "$target"    ;;
    update)  update_reflog              ;;
esac

# ---------------------------------
#  housekeeping
# ---------------------------------

exit

#  $Id: xattic.sh 9111 2012-02-20 15:43:10Z robbie $
#  end of file

