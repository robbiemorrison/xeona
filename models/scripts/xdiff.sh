#! /bin/bash

#  file-purpose     : diff two scenario dumps
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Fri 30-Dec-2011 00:04 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 8729 $
#  $Date: 2011-12-30 19:06:14 +0100 (Fri, 30 Dec 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/xdiff.sh $

#  NOTES
#
#  works fine but full-on bash is never elegant
#
#  for an alternative to 'csplit', see: http://h3manth.com/content/sort-text-blocks

# ---------------------------------
#  user modifiable
# ---------------------------------

study="trial-008"
delext=".del"

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")                 # name of this script

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2
E_NO_XGRAB=11
E_GRAB_DATA_FAIL=13
E_MKTEMP_FAIL=14

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
    echo
    cat << EOM
                 usage: $SCRIPT -u [-f|-F] ab    unified diff scenarios 'a' and 'b'
                        $SCRIPT -y [-l|-s] ab    side-by-side diff scenarios 'a' and 'b'
                        $SCRIPT --help           display this message and exit
       unified options:  -f                       basic filter
                         -F                       full pair-wise filter (may pay to check with -f also)
                         -k                       keep output as '$study$delext'
  side-by-side options:  -s                       omit unchanged lines
                         -l                       output only the left column of common lines
               purpose: diff two scenarios in various ways
                 notes: the option letters are similar to those used by 'diff'
                        best with 12 step 'xeona' model files
              requires: 'xgrab' utility
              examples: unified with basic filter     $ $SCRIPT -uf +b
                        unified with full filter      $ $SCRIPT -uF +b
                        two-col without repetition    $ $SCRIPT -yl +b
                        two-col with changes only     $ $SCRIPT -ys +b
EOM
    echo
}

# ---------------------------------
#  add_opts()
# ---------------------------------

function add_opts
{
    local opt="$1"

    case "$opt" in
        l)   printf -v dopts "%s %s" "$dopts" "--left-column"                 ;;
        s)   printf -v dopts "%s %s" "$dopts" "--suppress-common-lines"       ;;
        u)   printf -v dopts "%s %s" "$dopts" "--unified=0"                   ;;     # note the zero
        y)   printf -v dopts "%s %s" "$dopts" "--side-by-side"                ;;
        --*) printf -v dopts "%s %s" "$dopts" "$opt"                          ;;     # use 'opt' directly
        *)   echo "$SCRIPT: coding error: 'add_opts' argument ignored: $opt"  ;;
    esac
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

filter="0"
keep="no"
dopts=""

while getopts ":-fFhklsuy" option          # CAUTION: the leading : should be correct
do
    case "$option" in
        h)  display_help && exit $E_SUCCESS  ;;
        -)  :             ;;            # this codes for option "--" and do nothing is correct
        f)  filter="1"    ;;
        F)  filter="2"    ;;
        k)  keep="yes"    ;;
        l)  add_opts "l"  ;;
        s)  add_opts "s"  ;;
        u)  add_opts "u"  ;;
        y)  add_opts "y"  ;;
    esac
done
shift $(($OPTIND - 1))

two="$1"
shift

# ---------------------------------
#  standard options
# ---------------------------------

termwidth=$(stty size | cut -d ' ' -f 2)
add_opts "--width=$termwidth"

# ---------------------------------
#  integrity checks
# ---------------------------------

len=${#two}

# reporting

echo
echo "arg  : $two"
echo "len  : $len"

# test length

if [ $len -ne 2 ]
then
    echo
    echo "$SCRIPT: arguments not length two (try --help) (abandoning task)"
    echo
    exit $E_USAGE
fi

# 'xgrab' utility

if [ ! $(which "xgrab") ]               # CAUTION: no (test) -n and no soft-quotes
    then
    echo
    echo "$SCRIPT: FATAL: required 'xgrab' utility not found"
    echo
    exit $E_NO_XGRAB
fi

# ---------------------------------
#  getdump()
# ---------------------------------

function getdump
{
    local leta="$1"

    # form names
    local target="$study.$leta.xem"
    local jettif="$study.$leta.jet"

    # integrity checks
    if [ ! -f "$target" ]
    then
        echo "target not found           : $target"
        return 1
    fi
    if [ -f "$jettif" ]
    then
        echo "will clobber jettison file : $jettif"
    fi

    # record for 'glob'
    printf -v glob "%s %s" "$glob" "$jettif"

    # harvest
    local call="xgrab --quiet --jettison $target | sed 's/^entity\.//' >| $jettif"
    echo "grab : $call"
    eval "$call"

    # return
    return $?                           # the 'call' return
}

# ---------------------------------
#  active code
# ---------------------------------

echo

errors=0
glob=""                                 # list of filenames created by 'getdump' calls

getdump "${two:0:1}" || let "errors++"  # first leta
getdump "${two:1:1}" || let "errors++"  # second leta

test $errors -eq 0 || echo
test $errors -eq 0 || exit $E_GRAB_DATA_FAIL

case "$filter" in
    0)
        call="diff$dopts$glob | less --chop-long-lines"
        eval "$call"
        echo
        echo "call : $call"
        echo
        ;;
    1)
        call="diff$dopts$glob | sed 's/^@.*@$//' | less --chop-long-lines"
        eval "$call"
        echo
        echo "call : $call"
        echo
        ;;
    2)
        # create temporary files

        script=$(basename "$SCRIPT" ".sh")        # strip extension
        tmpfile1=$(mktemp -t "$script~XXXXXX") || {
            echo "$SCRIPT: FATAL: 'mktemp' returned $?"
            exit $E_MKTEMP_FAIL
        }
        tmpfile2=$(mktemp -t "$script~XXXXXX")    # this call unlikely to fail
        tmpfile3=$(mktemp -t "$script~XXXXXX")    # this call unlikely to fail
        echo
        echo "temp file 1 : $tmpfile1"
        echo "temp file 2 : $tmpfile2"
        echo "temp file 3 : $tmpfile3"

        # principal code
        #
        # 'csplit' will eventually exits with "`/^$/':
        # match not found on repetition 56" when it
        # reaches the end of the file
        #
        # 'diff' introduces tabs in the header, hence 'expand'

        echo
        call="diff$dopts$glob | sed 's/^@.*@$//' | sed 's/^[-+]//' >| $tmpfile1"
        eval "$call"
        csplit --silent --keep-files --prefix="xdiffxx" --digits=3 "$tmpfile1" '/^$/' "{999}"

        echo
        for chunk in xdiffxx???
        do
            sort "$chunk"
        done >| "$tmpfile2"
        rm --force xdiffxx???           # clean up immediately

        cat "$tmpfile2" \
            | grep --invert-match '^$' \
            | awk '{ if ( NR % 2 == 1 ) {print} else { print; print ""} }' \
            | expand \
            >| "$tmpfile3"

        less --chop-long-lines "$tmpfile3"

        # save file as required

        case "$keep" in
            yes)
                keepfile="$study$delext"
                cp --force "$tmpfile3" "$keepfile"
                chmod 0640 "$keepfile"
                echo "modified unified diff file saved: $keepfile"
                echo
                ;;
        esac
        ;;
esac

rm --force $glob                        # clean up 'getdump' files

#  $Id: xdiff.sh 8729 2011-12-30 18:06:14Z robbie $
#  end of file

