#! /bin/bash

#  file-purpose     : import a fresh model from the required directory / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Wed 28-Jul-2010 11:41 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9267 $
#  $Date: 2012-07-05 12:04:30 +0200 (Thu, 05 Jul 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/getstudy.sh $

# ---------------------------------
#  settings
# ---------------------------------

SOURCE_DIR="notset"                     # can be relative or absolute
SOURCE_TAG="notset"                     # source model prefix
TARGET_TAG="notset"                     # target model prefix
EDITORS="memacs emacs gedit"            # list of editors to try

TRUNK="$HOME/$SYNK/xeona/svn2/futz/trunk"

xgrab="xgrab"                           # binary for parsing XEM files

# ---------------------------------
#  preamble
# ---------------------------------

EXT="xem"
GUARD="guard"

XEMRUNSTUB="xemrrun-region"

SCRIPT=$(basename "$0")

E_SUCCESS=0
E_FAIL=1
E_USAGE=2
E_USER_QUIT=9
E_BANNED_DIRECTORY=63
E_MISSING_SOURCE_DIR=64
E_MISSING_MODEL=65
E_CODING_ERROR=66

# ---------------------------------
#  report()
# ---------------------------------

# standardized output

function report
{
    local format="-25"                  # - is left-align, 00 is minimum width
    local prefix="  "
    case "$#" in
        0) printf "\n"                                           ;;
        1) printf "$prefix%*s\n"         $format "$1"            ;;
        2) printf "$prefix%*s : %s\n"    $format "$1" "$2"       ;;
        *) printf "$prefix%*s : %s %s\n" $format "$1" "$2" "$3"  ;;
    esac
    return 0
}

# ---------------------------------
#  deport() - additional reporting
# ---------------------------------

function deport
{
    : # report "$@"                         # simple wrapper
}

# ---------------------------------
#  fatal() - standardized exit
# ---------------------------------

function fatal
{
    local ret="$1"
    shift
    report "$@"                         # CAUTION: must be soft-quoted '$@'
    report "exit code" "$ret"
    report "PROBLEMATIC EXIT"
    report
    exit $ret
}

# ---------------------------------
#  clean()
# ---------------------------------

# prompted local clean-up
# note the better syntax "-not -name \"pattern\" used in 'sdir'

function clean
{
    local pattern="$1"

    case "$pattern" in
        glpk-export) local seek="-maxdepth 1 \( -name \"*.glpk\" -o -name \"*.prob\" -o -name \"*.sens\" -o -name \"*.soln\" \) -type f" ;;
        html)        local seek="-maxdepth 1 \( -name \"*.html\" \) -type f"                                                             ;;
        svg)         local seek="-maxdepth 1 \( -name \"*.svg\" \) -type f"                                                              ;;
        junk)        local seek="-maxdepth 1 \( -name \"\#*\" \) -type f"                                                                ;;
        poc)         local seek="-maxdepth 1 \( -name \"poc.[0-9][0-9][0-9].xem*\" \) -type f"                                           ;;
        r)           local seek="-maxdepth 1 \( -name \"*.r\" \) -type f"                                                                ;;
        rplots.pdf)  local seek="-maxdepth 1 \( -name \"Rplots.pdf\" \) -type f"                                                         ;;
        sdir)        local seek="-maxdepth 1 \
-name \"*\" -not -name \".*\" -not -name \"attic\" -not -name \"scripts\" -not -name \"analysis\" -not -name \"catalogs\" -type d"       ;;
        tgz)         local seek="-maxdepth 1 \( -name \"*.tgz\" -o -name \"*.tar.gz\" -o -name \"*.tar\" \) -type f"                     ;;
        xemrun)      local seek="-maxdepth 1 \( -name \"$XEMRUNSTUB*.*\" \) -type f"                                                     ;;
        xglpsol)     local seek="-maxdepth 1 \( -name \"*.term\" -o -name \"*.term~\" \) -type f"                                        ;;
        xsen-tmp)    local seek="-maxdepth 1 \( -name \"xsenario~*\" \) -type f"                                                         ;;
        *)           local seek="-maxdepth 1 \( -name \"*$pattern*\" -a \! -name \"*.html\" -a \! -name \"*.tgz\" \) -type f"            ;;
    esac
    local call="find -P . $seek -print"
    local glob=$(eval "$call")

    test -z "$glob" && return 0

    echo
    echo "      files associated with '$pattern':"
    echo
    echo "$glob"
    echo

    local prompt="      hit 'y' to clean en-masse: "
    read -n 1 -p "$prompt" response     # "-n 1" is read one character and return
    echo
    case "$response" in
        y|Y)
            echo
            case "$pattern" in
                sdir) rm --force --recursive $glob  ;;
                *)    rm --force $glob              ;;      # CAUTION: omit soft-quotes
            esac
            return 0
            ;;
        "")                             # a newline which counts, hence no 'echo'
            return 1
            ;;
        *)
            echo
            return 1
            ;;
    esac
}

# ---------------------------------
#  list_remaining()
# ---------------------------------

# CAUTION: 'tail' with "+2" is correct

function list_remaining
{
    local capture=$(ls --format=long --classify | tail --lines=+2 |  grep --invert-match "$SCRIPT")
    if [ -n "$capture" ]
    then
        echo
        echo "      current files (filtered)"
        echo
        echo "$capture"
        echo
    fi
}

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
    echo
    cat <<EOM
       usage: $SCRIPT [opt]  <00>       clean up local "00"  and import relevant 'submodel' sub-model
              $SCRIPT [opt]  <000>      clean up local "000" and import relevant 'poc' proof-of-concept model
              $SCRIPT [opt]  [--clean]  clean up local "00" and "000" and "html"
              $SCRIPT  --help           display this message and exit
      option:  --relax                      remove requirement to work in models directory
     purpose: import a fresh model from the 'xeona-xmoks' directory
       notes: - 00 means 00 thru 99, 000 means 000 thru 999
              - the sought guard file must exist for any action to occur
              - the path and stub names for 'submodel' and 'poc' are hard-coded
              - all file deletions must be confirmed
   hardcodes: model extension             : .$EXT
              guard file tag              : .$GUARD
EOM
    echo
}

# ---------------------------------
#  command-line processing #1
# ---------------------------------

cline="$0 $@"                           # grab command-line with given path
cline="$SCRIPT $@"                      # grab command-line without given path

mode="strict"

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        display_help
        exit $E_SUCCESS
        ;;
    --relax|-r)
        shift
        mode="relax"
        ;;
esac

# some protection against working in wrong directories

if [ $(pwd -P) == "$TRUNK/xeona1" ]
then
    report
    report "$SCRIPT"  "refusing to work in main directory"
    fatal $E_BANNED_DIRECTORY
fi

# ---------------------------------
#  command-line processing #2
# ---------------------------------

case "$1" in

    ""|--clean|-cl|-c|clean|cl|c)       # CAUTION: note the "" variation!
        clean "core"                    # core dumps
        clean "poc"                     # poc.000.xem files
        clean "xglpsol"                 # *.term files
        clean "glpk-export"             # exported GLPK files
        clean "-[0-9][0-9]."            # 00 thru 99
        clean "-[0-9][0-9][0-9]."       # 000 thru 999
        clean "xsen-tmp"                # 'xsenario.sh' temporary files
        clean "r"                       # r-script files
        clean "rplots.pdf"              # "Rplots.pdf" files
        clean "xemrun"                  # part-plot files
        clean "html"                    # html files
        clean "svg"                     # svg files
        clean "sdir"                    # non-hidden subdirectories
        clean "junk"                    # \#file
        clean "tgz"                     # compressed and uncompressed tar files
        list_remaining
        exit $E_SUCCESS
        ;;
    [0-9][0-9])                         # 00 thru 99
        MODEL_NUMBER="$1"
        SOURCE_DIR="$TRUNK/xeona1/xeona-xmoks"  # can be relative or absolute
        SOURCE_TAG="submodel"                   # source model prefix
        TARGET_TAG="test"                       # target model prefix
        ;;
    [0-9][0-9][0-9])                    # 000 thru 099
        MODEL_NUMBER="$1"
        SOURCE_DIR="$TRUNK/pocmods"             # can be relative or absolute
        SOURCE_TAG="poc"                        # source model prefix
        TARGET_TAG="trial"                      # target model prefix
        ;;
    *)
        echo "$SCRIPT: incorrect usage (try --help): $*"
        exit $E_USAGE
        ;;
esac

SOURCE_DIR_ABS=$(readlink --canonicalize "$SOURCE_DIR")

# ---------------------------------
#  integrity()
# ---------------------------------

function integrity
{
    if [ -d "$SOURCE_DIR_ABS" ]
    then
        report "located source directory" "$SOURCE_DIR_ABS"
    else
        fatal $E_MISSING_SOURCE_DIR "cannot locate directory" "$SOURCE_DIR_ABS"
    fi

    if [ -f "$MODEL_NAME_ABS" ]
    then
        report "located source model" "$MODEL_NAME_ABS"
    else
        fatal $E_MISSING_MODEL "cannot locate model" "$MODEL_NAME_ABS"
    fi
}

# ---------------------------------
#  mystat()
# ---------------------------------

# works well, typical output: '  143150   2011-11-07 13:46   trial-008.xem'

function mystat
{
    local filename="$1"
    if [ -f "$filename" ]
    then
        local listing=$(stat --format "%8s   %.16y   %n" "$filename")
        report "local listing" "$listing"
        test -s "$filename" || let "zerobytes++"
        return 0
    else
        report "not found" "$filename"
        return 1
    fi
}

# ---------------------------------
#  import1() - single poc
# ---------------------------------

# copy the base file

function import1
{
    local source="$1"
    local target="$2"
    if [ -f "$target" ]
    then
        fatal "$E_CODING_ERROR" "coding error"
    else
        cp --archive "$source" "$target"
        mystat "$target"
    fi
}

# ---------------------------------
#  import2() - entire study
# ---------------------------------

# copy the relevant patch files

function import2
{
    local source="$1"
    local target="$2"

    # import the diff files
    deport "entering function $FUNCNAME"
    deport "source" "$source"       # /home/robbie/synk/xeona/svn2/futz/trunk/pocmods/poc.008.guard.xem
    deport "target" "$target"       # trial-008.xem
    local srcdir="$SOURCE_DIR"
    local srcpath=$(dirname "$source")
    local srcstub=$(basename "$source" ".$GUARD.$EXT")
    local tag="$TARGET_TAG"
    deport "source sdir" "$srcdir"
    deport "source stub" "$srcstub"
    local difs=$(find -P "$srcdir" -name "$srcstub.[a-z].dif" -type f -printf "%P\n")
    for dif in $difs
    do
        local difsrc="$srcdir/$dif"
        local difget="$tag-${dif#poc.}"
        cp --archive "$difsrc" "$difget"
        mystat "$difget"
    done
    deport "leaving function $FUNCNAME"
}

# ---------------------------------
#  age()
# ---------------------------------

# the "%Y" is correct : the last time the contents were modified
# but "%Z" is wrong   : the last time the inode information (including permissions and name) was changed
# for details, refer to $ man 2 stat

function age
{
    local filename="$1"
    local then=$(stat --format='%Y' "$filename")  # last modified in seconds since UNIX epoch
    local now=$(date '+%s')                       # current time in seconds since UNIX epoch
    local delta
    let "delta = now - then"
    let "delta = delta / ( 60 * 60 * 24 )"   # integer arithmetic
    case "$delta" in
        0) report "file age" "under one day" ;;
        1) report "file age" "$delta day"    ;;
        *) report "file age" "$delta days"   ;;
    esac
}

# ---------------------------------
#  title()
# ---------------------------------

function title
{
    local target="$1"

    grab1="$xgrab --quiet --failmsg=\"($SCRIPT fail)\" $target \"program.study-description.scenario-name\""
    grab2="$xgrab --quiet --failmsg=\"($SCRIPT fail)\" $target \"program.r-processing.r-title\""
    scenario_name=$(eval "$grab1") && report "scenario name" "$scenario_name"
    r_title=$(eval "$grab2")       && report "r title"       "$r_title"
}

# ---------------------------------
#  editor()
# ---------------------------------

EDITOR=""

function editor
{
    local editors="$1"
    for editor in $editors
    do
        local call=$(which "$editor")
        test -n "$call" && EDITOR="$editor"
        test -n "$call" && return
    done
    report "working editor not found" "$editors"
}

# ---------------------------------
#  add_hx()
# ---------------------------------

function add_hx
{
    local command="$*"
    local hxfile=$(printenv "ROBBIE_HXFILE")

    test -z "$command" && return
    test -z "$hxfile"  && return
    test -f "$hxfile"  || return

    perms=$(stat --printf %a "$hxfile") # probably 640 anyway
    chmod u+w "$hxfile"                 # make user-writable
    echo "$command" >> "$hxfile"        # append line
    chmod "$perms" "$hxfile"            # reinstate previous permissions

    report "command history addition" "$command"
}
# ---------------------------------
#  active code
# ---------------------------------

report
report "command" "$cline"
report "mode"    "$mode"

case "$mode" in
    strict)
        if [ $(pwd -P) != "$TRUNK/models" ]
        then
            report "$SCRIPT"  "refusing to work outside models directory"
            fatal $E_BANNED_DIRECTORY
        fi
        ;;
esac

zerobytes=0

list_remaining                          # remove if distracting

report

MODEL_NAME="$SOURCE_TAG.$MODEL_NUMBER.$GUARD.$EXT"
MODEL_NAME_ABS="$SOURCE_DIR_ABS/$MODEL_NAME"
TARGET_NAME="$TARGET_TAG-$MODEL_NUMBER.$EXT"

report "model number"    "$MODEL_NUMBER"
report "model name"      "$MODEL_NAME"
report "target name"     "$TARGET_NAME"

integrity

clean   "core"                           # useful
clean   "-${MODEL_NUMBER}."            || fatal $E_USER_QUIT "abandoning model import without action" # note leading hyphen and trailing dot
clean   "-${MODEL_NUMBER}_[0-9][0-9]." || fatal $E_USER_QUIT "abandoning model import without action" # note leading hyphen and trailing dot

import1 "$MODEL_NAME_ABS" "$TARGET_NAME" # copy over base scenario file
import2 "$MODEL_NAME_ABS" "$TARGET_NAME" # copy over any diff files
title   "$MODEL_NAME_ABS"                # report the title
age     "$TARGET_NAME"
editor  "$EDITORS"                       # look for working editor
add_hx  "$EDITOR $TARGET_NAME"           # add to command history, strictly optional / may need trailing "&"

report
test $zerobytes -gt 0 && report "WARN : zero byte files = $zerobytes"
test $zerobytes -gt 0 && report

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $E_SUCCESS

#  $Id: getstudy.sh 9267 2012-07-05 10:04:30Z robbie $
#  end of file

