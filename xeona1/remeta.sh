#! /bin/bash

#  file-purpose     : update codebase copyright notices and such / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 02-Feb-2009 14:18 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 2063 $
#  $Date: 2009-02-02 15:21:17 +0100 (Mon, 02 Feb 2009) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/copyright.sh $

#  See also for copyright updates
#
#    r1262 | robbie | 2008-01-03 17:18:29 +0100 (Thu, 03 Jan 2008) | 4 lines
#    r1676 | robbie | 2008-06-27 22:57:07 +0200 (Fri, 27 Jun 2008) | 3 lines
#    r2064 | robbie | 2009-02-02 15:26:12 +0100 (Mon, 02 Feb 2009) | 4 lines
#    r2065 | robbie | 2009-02-02 15:37:13 +0100 (Mon, 02 Feb 2009) | 5 lines
#    r4214 | robbie | 2010-01-06 14:20:40 +0100 (Wed, 06 Jan 2010) | 7 lines
#    r8750 | robbie | 2012-01-10 13:56:40 +0100 (Tue, 10 Jan 2012) | 5 lines

# ---------------------------------
#  hardcodes
# ---------------------------------

# CAUTION: regex escaping NOT required for '/' nor for '().' but '|' cannot be used.

OLDSTR="//  Copyright : This software is copyright (c) 2007 - 2012 Robbie Morrison."
NEWSTR="//  Copyright : This software is copyright (c) 2007 - 2013 Robbie Morrison."

OLDSTR=""
NEWSTR=""

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2
E_NO_OLDSTR=64

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
    echo
    echo "            usage: $SCRIPT                dry run"
    echo "                   $SCRIPT --run          run in earnest"
    echo "                   $SCRIPT --help         display this message and exit"
    echo "          purpose: update codebase copyright notices and such"
    echo "        operation: searches subdirs [a-z] for matches and acts accordingly"
    echo "                   uses 'find' 'file' 'grep' and 'sed'"
    echo "             note: copyright fix also requires changes to:"
    echo "                       common.cc: 'xeona::buildCopyright'"
    echo "                       ccplate.*"
    echo "        test mode: --test as first argument"
    echo
    echo "           OLDSTR: ${OLDSTR:-(empty)}"
    echo "           NEWSTR: ${NEWSTR:-(empty)}"
    echo
    return 0
}

# ---------------------------------
#  process command line
# ---------------------------------

testmode=""
case "$1" in
    --test|-t)
        echo "$SCRIPT: test mode set"
        testmode="yes"
        shift
        ;;
esac

mode="dry"                              # default mode
case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        shift
        display_help
        exit $E_SUCCESS
        ;;
    --run)
        shift
        mode="run"
        ;;
    "")
        :                               # fall thru
        ;;
    *)
        echo "$SCRIPT: must give suitable argument (try --help for usage)"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  deport()
# ---------------------------------

function deport                         # debug reporting
{
    local line="$*"
    test "$testmode" && echo "$line"
    return
}

# ---------------------------------
#  modify()
# ---------------------------------

sedprog=""                              # used in completion reporting
extrarep=""                             # used by 'locate'

function modify
{
    local target="$1"
    local ret=-1

    extrarep=""                         # reset each time

    # option '-t' is "generate a path rooted in a temporary directory"
    local tempfile=$(mktemp -t "$SCRIPT~XXXXXXXXXXXX") || {
        ret=$?                          # CAUTION: exit status should be captured here
        extrarep="    bad 'mktemp' return: $ret  "
        return 1
    }

    # note: add 'm' to 'sedprog' for multilines (not
    # tested) and 'g' for multiple replaces (not tested)

    printf -v sedprog "'s|%s|%s|'" "$OLDSTR" "$NEWSTR"
    eval "sed $sedprog $target > $tempfile" || {
        ret=$?
        extrarep="    bad 'sed' return: $ret  "
        return 2
    }

    case $mode in
        run)
            cp --force "$tempfile" "$target" || {
                ret=$?
                extrarep="    bad 'cp' return: $ret"
                return 3
            }
            rm --force "$tempfile" || {
                ret=$?
                extrarep="    bad 'rm' return: $ret"
                return 4
            }
            ;;
        dry)
            extrarep="    diff --report-identical-files $target $tempfile"
            ;;
    esac

    return 0
}

# ---------------------------------
#  locate()
# ---------------------------------

grepstr=""                              # used in completion reporting

function locate
{
    local xs
    local x
    local fs
    local f
    local file
    local ret

    # identify subdirectories
    xs=$(find -P . -maxdepth 1 -name '[.a-z]' -type d | sort)    # single letter dirs

    # loop thru subdirectories
    for x in $xs
      do
      fs=$(find -P "$x" -maxdepth 1 -type f -print | sed 's/\.\///' | sort)
      test -z "$fs" && deport "$x empty"
      test -z "$fs" && continue         # skip empty subdirectories
      deport "$fs"

      # loop thru files
      for file in $fs
        do
        test "$file" == "$SCRIPT" && continue     # skip this script

        printf "  %-*s" 30 "$file"
        let "tested++"

        # examine file and skip nontext, also report
        output=$(file --brief "$file")  # --brief is do not append filename
        case "$output" in               # seek text files
            "Binary II (apple ][) data") : ;;     # resolve conflict (file bug) with "\nGL" lead-in
            *text*)                      : ;;     # perhaps "* text*" would be better
            *)
                printf "   non-text\n"
                continue                # loop again
                ;;
        esac

        # grep file and report
        # CAUTION: option '--extended-regexp' not required
        printf -v grepstr "'%s'" "$OLDSTR"
        eval "grep --binary-files=without-match --quiet $grepstr $file"
        ret=$?
        case $ret in
            0)
                # modify file and report
                modify "$file"
                ret=$?
                case $ret in
                    0) printf "<  replaced$extrarep\n"
                        let "changed++"
                        ;;
                    1) printf "** 'mktemp' problem (file not modified)\n"                  ;;
                    2) printf "** 'sed' problem (file not modified)\n"                     ;;
                    3) printf "** 'cp' over file problem (file probably not modified)\n"   ;;
                    4) printf "** 'rm' tempfile problem (file probably modified)\n"        ;;
                    *) printf "** undocumented error, 'modify' return: $ret\n"             ;;
                esac
                ;;
            1)
                printf "   no-match\n"
                ;;
            *)
                printf "** grep return: $ret\n"
                ;;
        esac
      done
    done
    return 0
}

# ---------------------------------
#  main code
# ---------------------------------

tested=0
changed=0

case "$OLDSTR" in
    "")
        echo "$SCRIPT: FATAL: old string not set, no action taken"
        deport "old (quotes added) : \"$OLDSTR\""
        exit $E_NO_OLDSTR
        ;;
    *)
        echo
        locate
        ret=$?
        ;;
esac

# ---------------------------------
#  completion reporting
# ---------------------------------

echo
echo "old (quotes added) : \"$OLDSTR\""
echo "new (quotes added) : \"$NEWSTR\""
echo "grepstr            : $grepstr"
echo "sedprog            : $sedprog"

echo
echo "tested files       : $tested"
echo "changed files      : $changed"
echo

case $mode in
    dry) echo "mode : DRY RUN"    ;;
    run) echo "mode : ACTIVE RUN" ;;
esac
echo

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $ret

#  $Id: copyright.sh 2063 2009-02-02 14:21:17Z robbie $
#  end of file

