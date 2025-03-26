#! /bin/bash

#  file-purpose     : compare two attic suites using 'sdiff'
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Fri 23-Mar-2012 09:36 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9198 $
#  $Date: 2012-03-23 10:46:26 +0100 (Fri, 23 Mar 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/cmpstudy.sh $

# ---------------------------------
#  volatile settings
# ---------------------------------

ATTIC="$HOME/$SYNK/xeona/svn2/futz/trunk/models/attic"
STUB="trial-008"
LETAS="+ a b c d e f g h i j"

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")                 # name of this script

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2

E_BAD_ATTICS=64
E_BAD_DIRS=65

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
    echo
    cat << EOM
       usage: $SCRIPT <one>          compare attic 'one' with current attic
              $SCRIPT <one> <two>    compare attics 'one' and 'two'
              $SCRIPT  --help        display this message and exit (takes priority)
     purpose: compare two model suits
   hardcodes: attic = $ATTIC
              stub  = $STUB
              letas = $LETAS
    see also: cmpstudy.R
EOM
    echo
}

# ---------------------------------
#  process command-line
# ---------------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        display_help
        exit $E_SUCCESS
        ;;
    "")
        echo "$SCRIPT: incorrect usage (try --help)"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  is_integer()
# ---------------------------------

function is_integer
{
    local int="$1"
    local val=0                         # not used

    printf -v val "%d" "$int" 2>/dev/null  # effectively confirms 'int' is an integer [1]
    # [1] note formatted printing to a variable and see $ help printf
    case "$?" in
        0)
            return 0
            ;;
        *)
            echo "$SCRIPT: $FUNCNAME(): bad integer encountered: $int"
            return 1
            ;;
    esac
}

# ---------------------------------
#  is_dir()
# ---------------------------------

function is_dir
{
    local dir="$1"

    if [ -d "$dir" ]
    then
        return 0
    else
        echo "$SCRIPT: $FUNCNAME(): directory not found: $dir"
        return 1
    fi
}

# ---------------------------------
#  cmpleta()
# ---------------------------------

swidth=166
swidth=$(stty size | awk '{ print $2 }')
let "swidth--"

function cmpleta
{
    local stub="$1"
    local leta="$2"

    local target1="$one/$stub.$leta.xem"
    local target2="$two/$stub.$leta.xem"

    local call="sdiff --width=$swidth --suppress-common-lines $target1 $target2"

    (                                   # run in subshell
        cd "$ATTIC"
        echo "call = $call"
        echo
        eval "$call"
    )
}

# ---------------------------------
#  active code
# ---------------------------------

errors=0

echo

# read arguments

one="$1"
two="$2"

# substitute second argument as required

numba=$(basename $PWD)
test -z "$two" && two="$numba"

# report

echo "$SCRIPT: one = '$one'"
echo "$SCRIPT: two = '$two'"

# confirm integers

is_integer "$one" || let "errors++"
is_integer "$two" || let "errors++"

test $errors -ne 0 && echo
test $errors -ne 0 && exit $E_BAD_ATTICS

# confirm directories

is_dir "$ATTIC/$one"  || let "errors++"
is_dir "$ATTIC/$two"  || let "errors++"

test $errors -ne 0 && echo
test $errors -ne 0 && exit $E_BAD_DIRS

# main loop

for leta in $LETAS                      # CAUTION: no soft quotes
do
    echo
    cmpleta "$STUB" "$leta"
done

# ---------------------------------
#  housekeeping
# ---------------------------------

echo

exit $E_SUCCESS

#  $Id: cmpstudy.sh 9198 2012-03-23 09:46:26Z robbie $
#  end of file

