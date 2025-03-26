#! /bin/bash

#  file-purpose     : report source lines of code, nonblank lines, total lines / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 03-Dec-2007 09:53 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9240 $
#  $Date: 2012-04-17 15:51:21 +0200 (Tue, 17 Apr 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/sloc.sh $

# ---------------------------------
#  preamble
# ---------------------------------

A4LINES=0                               # lines per A4 page
A4LINES=82                              # emacs 8pt portrait (82 lines x 100 cols)

SLOCCOUNT="sloccount"                   # SLOC (source lines of code) utility

SCRIPT=$(basename "$0")                 # script leafname

E_SUCCESS=0
E_NO_SLOCCOUNT=64

# ---------------------------------
#  comment_blocks()
# ---------------------------------

function comment_blocks
{
    # 30 is the standard non-trivial meta information in each file

    local regx="^[[:blank:]]*//.\+$"    # non-trivial comment line
    local glob=$(find -P . -maxdepth 2 -name "*.h" -o -name "*.cc" -not -name "*.ut*.cc" -type f -printf "%P\n")
    local grab=$(xargs grep --count $regx <<< "$glob")
    local coms=$(awk 'BEGIN { FS = ":"; lines = 0 } { lines += $2 } END { printf "%d", lines - ( NR * 30 ) }' <<< "$grab")
    local filz=$(wc --lines <<< "$glob")
    test -z "$glob" && filz=0           # deal with sole newline
    echo
    echo $grab                          # emits a lone "0" if no strikes
    echo
    printf "files : %d\n" $filz
    printf "regex : %s\n" "$regx"
    printf "lines : %d\n" $coms
    printf "\n"
    printf "note  : this routine uses different file globbing from sloc\n"
    echo
}

# -------------------------
#  process command-line
# -------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        echo
        cat <<EOM
        usage: $SCRIPT  [-a|-f] [-l]    count lines of code (based on lists)
               $SCRIPT  -c --coms       count comment block lines (based on find pattern)
               $SCRIPT  -h --help       show this message and exit
       option:  --app   -a    application code only (based on makefile sources plus extras)
                --frag  -f    report fragments directory
                --long  -l    include SLOCCount output
    hardcodes: lines per A4 page: $A4LINES
  definitions: SLOC is source lines of code
               squeezed means multiple blank lines count as one
    note also: $ sloccount {.,[a-z]}/*.{h,cc}
               $ xgrep --title | grep CLASS | sort | wc --line
               $ xgrep --title | grep FREE  | sort | wc --line
EOM
        echo
        exit $E_SUCCESS
        ;;
    --comments|--coms|--comm|-c)
        comment_blocks
        exit $E_SUCCESS
        ;;
    --app|-a)
        mode="short"
        target="app"
        ;;
    --frag|-f)
        mode="short"
        target="frag"
        ;;
    --long|-l)
        mode="long"
        target="xeona"
        ;;
    -al|-la)
        mode="long"
        target="app"
        ;;
    -fl|-lf)
        mode="long"
        target="frag"
        ;;
    "")
        mode="short"
        target="xeona"
        ;;
    *)
        echo "$SCRIPT: invalid option (try --help for usage): $*"
        exit 2
        ;;
esac

case "$2" in
    --long|-l)
        mode="long"
        ;;
    --app|-a)
        target="app"
        ;;
    "")
        :                               # do nothing is correct
        ;;
    *)
        echo "$SCRIPT: invalid option (try --help for usage): $*"
        exit 2
        ;;
esac

# ---------------------------------
#  report()
# ---------------------------------

function report
{
    local tab1=22
    local tab2=6

    local number=0                      # because formatting takes two hits
    printf -v number "%g" "$2"
    printf "%-*s : %*s\n" "$tab1" "$1" "$tab2" "$number"
}

# ---------------------------------
#  test for sloccount
# ---------------------------------

if [ ! $(which "$SLOCCOUNT") ]          # CAUTION: for test, no -n (but soft-quotes okay)
    then
    echo "$SCRIPT: SLOC utility not found: $SLOCCOUNT"
    exit $E_NO_SLOCCOUNT
fi

# ---------------------------------
#  create file lists (xeona, fragments, application)
# ---------------------------------

function xeonas
{
    # CAUTION: omit -print (or use correct syntax)
    local cpp=$(find ..   -name '*.h' -o -name '*.cc'                               -type f | sort)
    msg=""

    # second variant which excludes certain subdirectories, comment out to revert
    local cpp=$(find . \( -name '*.h' -o -name '*.cc' \) -a -not -wholename './g/*' -type f | sort)
    local fil=$(find . \( -name '*.h' -o -name '*.cc' \) -a      -wholename './g/*' -type f | sort)
    local bin=$(find ../bin \( -name '*.h' -o -name '*.cc' \) -type f | sort)
    fil=$(echo $fil)
    msg="omitting 'g' subdirectory, contains massive datasets: $fil"

    local mak="
./makefile"

    local sh1="
./machunits"

    local sh2="
../scripts/pruf
../scripts/mach
../scripts/xedoc
../scripts/xmok"

#  ../scripts/ganz
#  ../scripts/jede
#  ../scripts/mach.units.sh

    local elisp1="
../elisp/xem.el
../elisp/xeona.el
../elisp/xog.el
../elisp/xrstat.el
../elisp/xumber.el"

    # CAUTION: check 'files' carefully to make sure the required strings are present
    files=$(echo $cpp $bin $sh1 $sh2 $mak $elisp1)  # cheap whitespace strip, care not -neE
}

function fragments
{
    local dir="/home/robbie/synk/xeona/fragments"

    local cpp=$(find "$dir" -name '*.cc' ! -name "_template.cc" -type f | sort)

    files=$(echo $cpp)                  # cheap whitespace strip, care -neE
}

function application
{
    local mak="./makefile"              # exclude by simply commenting out
    local sources=$(make ccs)
    local extras=$(make hs)
    local headers=""

    sources=$(echo $sources)
    for source in $sources
      do
      stub=${source%".cc"}
      test "$stub" == "./main" && continue
      headers="$headers $stub.h"
    done

    # the data files in 'g' are expressly excluded, viz:
    #   datas=$(find 'g' -name '*.h' -type f | sort)
    #   headers="$headers $datas"
    # for instance, the Colorado data-set adds 6466 source lines

    files=$(echo $extras $mak $headers $sources)
}

# ---------------------------------
#  establish list
# ---------------------------------

files=""

case "$target" in
    app)   application   ;;
    frag)  fragments     ;;
    xeona) xeonas        ;;
esac

# ---------------------------------
#  initial report
# ---------------------------------

echo
echo "$SCRIPT: xeona-specific interface to '$SLOCCOUNT' utility"
echo "$SCRIPT: mode = $mode  target = $target"
test -n "$msg" && echo "$SCRIPT: $msg"

# ---------------------------------
#  calculate metrics
# ---------------------------------

# file loop calls (also protects awk from file not found problems)

errors=0                                # error counter
ficount=0                               # file count
utcount=0                               # unit count
nbcount=0                               # nonblank line count
sqcount=0                               # squeezed line count
tocount=0                               # total line count
wdcount=0                               # word count

for file in $files
  do
  if [ -f "$file" ]
      then
      nb=$(awk '{ if ( length($0) ) print }' $file | wc --lines)
      sq=$(cat --squeeze-blank "$file" | wc --lines)
      to=$(wc --lines "$file" | awk '{ print $1 }')    # drop filename
      wd=$(wc --words "$file" | awk '{ print $1 }')    # drop filename

      let "ficount++"
      let "nbcount += nb"
      let "sqcount += sq"
      let "tocount += to"
      let "wdcount += wd"

      # count units, with 'main.cc' also included
      test "${file%.h}" != "$file" \
          && grep --no-filename "^//" "$file" | grep "file-role" | grep --quiet " / header" \
          && let "utcount++"
      test "$file" ==   "main.cc" && let "utcount++"
      test "$file" == "./main.cc" && let "utcount++"
  else
      let "errors++"
      echo "file not found: $file"
  fi
done

# straight calls

ncpp=$($SLOCCOUNT $files | awk '/^cpp:/ { print $2}')
nbsh=$($SLOCCOUNT $files | awk '/^sh:/  { print $2}')
let "total = ncpp + nbsh"

# ---------------------------------
#  R code
# ---------------------------------

rlines=0
rfiles=0
rtotal=0

function rloc
{
    local rpattern="../xeonar/*.R ../models/scripts/*.R ../rpacks/xem/R/*.R ../rpacks/robbie/R/*.R"

    rlines=$(cat --squeeze-blank $rpattern \
        | grep --invert-match '^[[:blank:]]*$' \
        | grep --invert-match '^[[:blank:]]*\#' \
        | grep --invert-match '^[[:blank:]]*}$' \
        | wc --lines)

    rfiles=$(ls -1 $rpattern \
        | wc --line)

    rtotal=$(wc --line $rpattern \
        | tail -1 \
        | awk '{ print $1 }')

    rsqcount=$(cat --squeeze-blank $rpattern \
        | wc --lines)

}

rloc

# ---------------------------------
#  final report
# ---------------------------------

echo

test "$mode" == "long" && { $SLOCCOUNT $files; echo; }

ls $files

echo

report "units"          "$utcount"
report "files"          "$ficount"
report "SLOC lines"     "$total"
report "nonblank lines" "$nbcount"
report "squeezed lines" "$sqcount"
test $sqcount -ne $tocount && report "total lines" "$tocount"
#report "Words"          "$wdcount"

echo

let "average = $total/$ficount"                   # CAUTION: integer arithmetic
report "average SLOC/file"   "$average"

# NOTE: for doing floating point arithmetic in bash,
# see Cooper (2007 etc) Advanced bash-scripting guide >
# External filters, programs and commands > Math
# commands > bc and then the 'bc' manpage (note too
# that 'awk' provides an alternative to 'bc')

ratio=$(echo "scale=2; $total/$nbcount" | bc)     # floating point arithmetic
report "ratio SLOC/nonblank" "$ratio"

ratio=$(echo "scale=2; $total/$sqcount" | bc)
report "ratio SLOC/squeezed" "$ratio"

pages=0
let "pages = sqcount / A4LINES"
report "squeezed A4 8pt" "$pages"

test $errors -ne 0 && printf "\nERRORS = %d\n" "$errors"

# temporary report
case "$target" in
    xeona)
        sloc_plus_r=0
        files_plus_r=0
        sqcount_plus_r=0
        pages_plus_r=0
        let "sloc_plus_r = total + rlines"
        let "files_plus_r = ficount + rfiles"
        let "sqcount_plus_r = sqcount + rsqcount"
        let "pages_plus_r = sqcount_plus_r / A4LINES"

        echo
        # echo "R code = SLOC lines: $rlines   squeezed lines: $rtotal   files: $rfiles"
        report "plus R files"           "$files_plus_r"
        report "plus R SLOC"            "$sloc_plus_r"
        report "plus R squeezed A4 8pt" "$pages_plus_r"
        ;;
esac

echo
report "elapsed time (seconds)" "$SECONDS"

echo

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $E_SUCCESS

#  $Id: sloc.sh 9240 2012-04-17 13:51:21Z robbie $
#  end of file

