#! /bin/bash

#  file-purpose     : show value of nominated file key for selected files / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Thu 13-Dec-2007 20:23 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 7997 $
#  $Date: 2011-11-16 15:27:47 +0100 (Wed, 16 Nov 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/role.sh $

#  comment: this script is not particularly well written

# ---------------------------------
#  preamble
# ---------------------------------

INDENT=20                               # controls printf indenting

SCRIPT=$(basename "$0")                 # this script

E_SUCCESS=0
E_USAGE=2

# ---------------------------------
#  select awk or gawk
# ---------------------------------

AWK=""
test $(which  awk) && AWK="$(which awk)"
test $(which gawk) && AWK="$(which gawk)"    # prefer gawk over awk

test -z "$AWK" && echo "$SCRIPT: error: (g)awk utility not present"

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
    local msg1="show value of nominated file key for unit tests, headers, and scripts"

    echo
    echo "       usage: $SCRIPT  [-d|-n|-s]      show meta information recorded in files"
    echo "              $SCRIPT  <key>           show 'key' information recorded in files"
    echo "              $SCRIPT  --help          display this message and exit"
    echo "     options:  <default>      \"file-role\""
    echo "               -d --date      \"file-create-date\""
    echo "               -n --name      \"file-create-name\""
    echo "               -s --status    \"file-status\""
    echo "     purpose: $msg1"
    echo "    examples: $SCRIPT Copyright # mostly readable"
    echo "     utility: $AWK"
    echo
}

# -------------------------
#  display help
# -------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        display_help
        exit $E_SUCCESS
        ;;
    ""|--|--role|-r)
        KEY="file-role"
        ;;
    --date|-d)
        KEY="file-create-date"
        ;;
    --name|-n)
        KEY="file-create-name"
        ;;
    --status|-s)
        KEY="file-status"
        ;;
    *)
        KEY="$*"
        ;;
esac

# ---------------------------------
#  header()
# ---------------------------------

function header
{
    printf "%*s   %s\n" $INDENT "$1" "$2"  # note '*' syntax for setting format flags
    printf "%*s%s\n"    $INDENT "---------" "---------------------"
    return 0
}

# ---------------------------------
#  show_value()
# ---------------------------------

function show_value
{
    local files="$1"

    local trunc=""
    for file in $files
      do
      trunc=$(basename "$file")
      trunc=${trunc%%.*.cc}
      printf "%*s     " $INDENT "$trunc"
      # the 'sed' call removes any trailing " / " and subsequent text
      awkcall="$AWK 'BEGIN { FS = \" : \" } / ${KEY} *:/ { print \$2 }' $file | sed 's/ * \/ .*$//'"
      eval "$awkcall"
    done
    return 0
}

# ---------------------------------
#  active code
# ---------------------------------

# CAUTION: unit-test sorting includes subdir stub
# "[.abc]/" but the reporting does not

echo

# UNIT-TESTS

header "units" "(by ut level)"
for i in $(seq 0 9)                                                        # level 0 thru 9
  do
  f1=$(find . -name "*.ut${i}.cc" ! -name "*_.*" -type f -printf "%p\n" | sort)  # SOURCE
  f2=$(find . -name "*_.ut${i}.cc"               -type f -printf "%p\n" | sort)  # non-SOURCE
  files="$f1"
  test -n "$f2" && files="$files $f2"
  show_value "$files"
  test -n "$files" && echo
done

# HEADERS

header "headers" "(by directory)"
for x in $(find . -maxdepth 1 -type d -name '[.a-z]' | sort)               # single letter dirs
  do
  f1=$(find "$x" -maxdepth 1 -name '*.h' ! -name "_*" -type f -printf "%p\n" | sort) # SOURCE
  f2=$(find "$x" -maxdepth 1 -name '_*.h'             -type f -printf "%p\n" | sort) # non-SOURCE
  files="$f1 $f2"
  show_value "$files"
  test -n "$files" && echo
done

test "$KEY" == "file-create-name" && exit $E_SUCCESS   # not for scripts
test -f "main.cc"                 || exit $E_SUCCESS   # exit if not main directory

# SCRIPTS

header "scripts" "(by directory)"
test "$KEY" == "file-role" && KEY="file-purpose"  # script files use a different template

files=$(find . -maxdepth 1 -name '*.sh' ! -name "_*" -type f -printf "%p\n" | sort)
show_value "$files"
test -n "$files" && echo

cd xeona-xmoks
files=$(find . -maxdepth 1 -name '*.sh' ! -name "_*" -type f -printf "%p\n" | sort)
show_value "$files"
test -n "$files" && echo
cd - >/dev/null

cd ../scripts                           # main scripts directory
files="mach pruf jede ganz xmok"        # sort order given
files=$(find . -maxdepth 1 -name '*' -type f -printf "%p\n" | sort)
show_value "$files"
test -n "$files" && echo
cd - >/dev/null

cd ../models
files=$(find . -maxdepth 1 -name '*.sh' -type f -printf "%p\n" | sort)
show_value "$files"
test -n "$files" && echo
cd - >/dev/null

cd ../pocmods
files=$(find . -maxdepth 1 -name '*.sh' -type f -printf "%p\n" | sort)
show_value "$files"
test -n "$files" && echo
cd - >/dev/null

# MAKEFILES

header "makefile" "(standard naming)"
test "$KEY" == "file-role" && KEY="file-purpose"  # script files use a different template
files=$(find . -maxdepth 1 -iname 'makefile' -type f -printf "%p " | sort)
show_value "$files"
test -n "$files" && echo

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $E_SUCCESS

#  $Id: role.sh 7997 2011-11-16 14:27:47Z robbie $
#  end of file

