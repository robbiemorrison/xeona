#! /bin/bash

#  file-purpose     : compare a model file and its backup / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Thu 07-Feb-2008 08:26 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 4252 $
#  $Date: 2010-01-14 00:15:13 +0100 (Thu, 14 Jan 2010) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/xeona-xmoks/xemdiff.sh $

# ---------------------------------
#  settings
# ---------------------------------

# the 'DEFAULT_STUB' can be overridden on the command-line

DEFAULT_STUB="test"                     # default filename in 'xeona'
DEFAULT_STUB="inbuilt"                  # used by the 'mach' script

# ---------------------------------
#  preamble
# ---------------------------------

EXT="xem"
GUARD="guard"
BACKUP="~"
LINES=85                                # cutoff between full and compact reporting

WIDTH=$(stty size | gawk '{ print $2 }')     # dynamic width setting
let "WIDTH--"                                # subtract one

SCRIPT=$(basename "$0")

E_SUCCESS=0
E_FAIL=1
E_USAGE=2
E_MISSING_FILES=64

# ---------------------------------
#  command-line processing
# ---------------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        echo
        echo "      usage: $SCRIPT  [model[.xem]]    compare model files"
        echo "             $SCRIPT  --help           display this message and exit"
        echo "    purpose: compare a nominated model file and its backup"
        echo "       note: terminal width set dynamically, increase using Ctrl-Shift-="
        echo "  important: run an original xem thru 'xeona --tidy' first to change the line ordering"
        echo "   defaults: stub name (overridable)  : $DEFAULT_STUB"
        echo "             extension                : .$EXT"
        echo "             backup indicator         : $BACKUP"
        echo "             guard tag                : $GUARD"
        echo "             compact reporting after  : $LINES lines average"
        echo "             terminal width (dynamic) : $WIDTH"
        echo
        exit $E_SUCCESS
        ;;
esac

case $# in
    0)  STUB="$DEFAULT_STUB"   ;;       # use default value
    1)  STUB="$1"              ;;       # take from command line
    *)  echo "$SCRIPT: usage problem (try --help)"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  deport()
# ---------------------------------

# debug reporting, controlled here

testmode="true"
testmode=""

function deport
{
    test -n "$testmode" || return 1
    local format="-25"                   # - is left-align, 00 is minimum width
    case "$#" in
        1) printf "debug: %*s\n"         $format "$1"            ;;
        2) printf "debug: %*s : %s\n"    $format "$1" "$2"       ;;
        *) printf "debug: %*s : %s %s\n" $format "$1" "$2" "$3"  ;;
    esac
    return 0
}

# ---------------------------------
#  process filenames
# ---------------------------------

STUB=${STUB%.$EXT}

deport "stub"   "$STUB"
deport "ext"    "$EXT"
deport "backup" "$BACKUP"
deport "guard"  "$GUARD"

FILE2="$STUB.$EXT"
FILE1a="$STUB.$EXT$BACKUP"
FILE1b="$STUB.$GUARD.$EXT"

fail=0                                  # failure counter

if [ ! -f "$FILE2"  ]
    then
    let "fail++"
    echo "$SCRIPT:   final file not found: $FILE2"
fi

if   [ -f "$FILE1a" ]; then FILE1="$FILE1a"  # try backup variant first
elif [ -f "$FILE1b" ]; then FILE1="$FILE1b"  # try guard variant second
else
    echo "$SCRIPT: initial file not found: $FILE1a or $FILE1b"
    let "fail++"
fi

test $fail -eq 0 || exit $E_MISSING_FILES

# ---------------------------------
#  main code
# ---------------------------------

output="(not reset)"                    # for end-of-script reporting

# cheap way of checking for presence of files -- note
# that cmp [--silent] performs differently when one or
# both files are missing

diff "$FILE1" "$FILE2" 1>/dev/null      # errors to file descriptor 2
test $? -gt 1 && exit $E_FAIL           # file or files not found
{
    output="strictly identical"
    diff "$FILE1" "$FILE2" 1>/dev/null; # strict comparison
} ||
{
    output="whitespace different"
    diff --ignore-space-change "$FILE1" "$FILE2" 1>/dev/null
} ||
{
    output="significantly different"
    line1=$(wc --lines "$FILE1" | gawk '{ print $1 }')
    line2=$(wc --lines "$FILE2" | gawk '{ print $1 }')
    let "lines = $line1 + $line2"
    let "threshold = $LINES * 2"
    if [ $lines -lt $threshold ]
        then                            # long format
        # CAUTION: do not soft quote '$opts'
        opts="--side-by-side --left-column --ignore-space-change"
        diff --width $WIDTH $opts "$FILE1" "$FILE2"
    else                                # compressed format
        echo
        opts="--side-by-side --suppress-common-lines --ignore-space-change"
        diff --width $WIDTH $opts "$FILE1" "$FILE2"
        echo
    fi
}

# ---------------------------------
#  completion reporting
# ---------------------------------

echo "$FILE1 | $FILE2 : $output"

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $E_SUCCESS

#  $Id: xemdiff.sh 4252 2010-01-13 23:15:13Z robbie $
#  end of file

