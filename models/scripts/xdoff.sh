#! /bin/bash

#  file-purpose     : diff xog files
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Fri 30-Dec-2011 18:15 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 8744 $
#  $Date: 2011-12-31 00:48:40 +0100 (Sat, 31 Dec 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/xdoff.sh $

# ---------------------------------
#  user modifiable
# ---------------------------------

study="trial-008"
defscens="+b"

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")                 # name of this script

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
    echo
    cat << EOM
             usage: $SCRIPT  [opts] <pat>    hunt for 'pat' in XOG files
                    $SCRIPT --help           display this message and exit
      grep options: -a <num>                 apply 'num' lines after context
                    -b <num>                 apply 'num' lines before context
                    -c <num>                 apply 'num' lines before and after context
                    -m <count>               stop after 'count' hits
                    -n                       add line numbers
    script options: -s <sens>                use 'sens' scenarios (default '$defscens')
           purpose: display portions of XOG files in various ways
    case-sensitive: search  is case-sensitive iff uppercase letters in 'pat'
             notes: works best with: $ allstudy.sh 008 770 # 12 steps with no yeek output
                    also             $ allstudy.sh 008 772 # 12 steps with full yeek output
          examples: $ $SCRIPT -a 12                HVAC operations
                    $ $SCRIPT -a 10 -m 2 -n -s +ab calcHvac
EOM
    echo
}

# ---------------------------------
#  add_opts()
# ---------------------------------

function add_opts
{
    local opt="$1"
    local arg="$2"

    case "$opt" in
        A)   printf -v gopts "%s %s" "$gopts" "--after-context=$arg"          ;;
        B)   printf -v gopts "%s %s" "$gopts" "--before-context=$arg"         ;;
        C)   printf -v gopts "%s %s" "$gopts" "--context=$arg"                ;;
        H)   printf -v gopts "%s %s" "$gopts" "--with-filename"               ;;
        T)   printf -v gopts "%s %s" "$gopts" "--initial-tab"                 ;;
        c)   printf -v gopts "%s %s" "$gopts" "--color"                       ;;
        i)   printf -v gopts "%s %s" "$gopts" "--ignore-case"                 ;;
        m)   printf -v gopts "%s %s" "$gopts" "--max-count=$arg"              ;;
        n)   printf -v gopts "%s %s" "$gopts" "--line-number"                 ;;
        --*) printf -v gopts "%s %s" "$gopts" "$opt"                          ;;     # use 'opt' directly
        *)   echo "$SCRIPT: coding error: 'add_opts' argument ignored: $opt"  ;;
    esac
    return 0
}

# ---------------------------------
#  argument processing
# ---------------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        shift
        display_help
        exit $E_SUCCESS
        ;;
esac

cline="$SCRIPT $@"

scens=""
gopts=""

while getopts ":-a:b:c:hm:ns:" option             # CAUTION: the leading : should be correct
do
    case "$option" in
        h)  display_help && exit $E_SUCCESS  ;;
        -)  :                                ;;   # this codes for option "--" and do nothing is correct
        a) add_opts "A" "$OPTARG"            ;;
        b) add_opts "B" "$OPTARG"            ;;
        c) add_opts "C" "$OPTARG"            ;;
        m) add_opts "m" "$OPTARG"            ;;
        n) add_opts "n" && add_opts "T"      ;;
        s) scens="$OPTARG"                   ;;
    esac
done
shift $(($OPTIND - 1))

phrase="$@"

# ---------------------------------
#  integrity checks
# ---------------------------------

errors=0

if [ "${scens:0:1}" == "-" ]            # no problem if 'scens' is null or empty
then
    echo "$SCRIPT: invalid -s argument (try --help): $scens"
    let "errors++"
fi

if [ -z "$phrase" ]
then
    echo "$SCRIPT: no pattern argument given (try --help)"
    let "errors++"
fi

test $errors -eq 0 || exit $E_USAGE

# ---------------------------------
#  more processing
# ---------------------------------

add_opts "c"                            # color
add_opts "H"                            # show filename

case "$phrase" in
    *[[:upper:]]*) :             ;;     # uppercase letters, search is case-sensitive
    *)             add_opts "i"  ;;     # no uppercase letters, search is case-insensitive
esac

test -z "$scens" && scens="$defscens"

spliz="$scens"
spliz=$(sed 's/.\{1\}/& /g' <<< "$spliz")
spliz=$(sed 's/ $//'        <<< "$spliz")

# ---------------------------------
#  active code
# ---------------------------------

first="$study.${spliz:0:1}.xog"

base="grep$gopts"

echo
echo "first     = $first"
echo "scenarios = $spliz"
echo "base call = $base"
echo "pattern   = \"$phrase\""
echo

for split in $spliz
do
    echo "--- $split ---"
    echo
    file="$study.$split.xog"
    call="$base \"$phrase\" $file"
    eval "$call" && echo
done
echo "--- - ---"

echo
echo "cline = $cline"

# ---------------------------------
#  housekeeping
# ---------------------------------

echo
exit $E_SUCCESS

# ---------------------------------
#  junked code
# ---------------------------------

#  letas="$scens"
#  letas=$(sed 's/.\{1\}/&,/g' <<< "$letas")
#  letas=$(sed 's/,$//'        <<< "$letas")
#  braces="{${letas}}"
#
#  files="$study.$braces.xog"
#
#  echo
#  call="grep$gopts \"$phrase\" $files"
#  echo "call : $call"
#  echo
#  eval "$call"

#  $Id: xdoff.sh 8744 2011-12-30 23:48:40Z robbie $
#  end of file

