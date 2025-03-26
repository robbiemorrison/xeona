#! /bin/bash

#  file-purpose     : generate a 'patch' to store a new 'xeona' scenario / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 07-Nov-2011 09:29 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 8654 $
#  $Date: 2011-12-27 08:55:07 +0100 (Tue, 27 Dec 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/pocmods/xpatch.sh $

#  NOTES
#
#  useful coding examples include: 'wgetme' 'xglpk.sh' 'xglpsol'

# ---------------------------------
#  user modifiable section
# ---------------------------------

RES="008"                               # reference energy system

xgrab="xgrab"                           # binary for parsing XEM files
emacs="cemacs"                          # 'emacs' call

xleaf=$(basename "$xgrab")

poc="poc."
sen="sen."

guard=".guard"
xem=".xem"
dif=".dif"
rej=".rej"

exregex="\$Revision:\|\$Date:\|\$Author:\|\$URL\|\$Id:"
conlines=5                              # amount of context to print

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")                 # name of this script

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2
E_NO_UTILITY=11
E_FILE_NOT_FOUND=12
E_TEXTCHECK_FAIL=13
E_FILENAME_CLEAR_FAIL=14
E_XGRAB_NOT_FOUND=15
E_XGRAB_FAIL=16
E_DIFF_SAME=17
E_DIFF_FAIL=18
E_REVPATCH_FAIL=19
E_UPDIFF_FAIL=20

# ---------------------------------
#  report() - formatted output
# ---------------------------------

function report
{
    case "$#" in
        0) printf "\n"                        ;;
        1) printf "  %s\n"         "$1"       ;;
        2) printf "  %-25s : %s\n" "$1" "$2"  ;;
    esac
}

# ---------------------------------
#  deport() - additional reporting
# ---------------------------------

function deport
{
    : # report "$@"                     # simple wrapper
}

# ---------------------------------
#  cexit() - controlled exit
# ---------------------------------

function cexit
{
    local msg="$1"                      # exit message
    local xval=$2                       # exit value
    local errs=$3                       # optional 'errors' value

    case "$errs" in
        0)  return 0                                ;;
        "") report                                  ;;
        *)  report && report "error count" "$errs"  ;;
    esac

    test -n "$msg"   && report "$FUNCNAME message" "$msg"
    test -n "$msg"   && report
    report "command"           "$cline"
    report "elapsed (seconds)" "$SECONDS"
    report "script exit"       "$xval"
    report

    exit $xval
}

# ---------------------------------
#  display_help() - display help
# ---------------------------------

function display_help
{
    # preamble
    local  oo="00"
    local ooo="$RES"
    local   x="a"
    case "$1" in
        [0-9][0-9])
            oo="$1"
            case "$2" in
                [+a-z])
                    x="$2"              # iff 00 is set
                    ;;
            esac
            ;;
        [0-9][0-9][0-9])
            ooo="$1"
            ;;
    esac
    local tstamp=$(date --rfc-3339=seconds | sed 's/ /_/g') # 2011-12-16_11:04:45+01:00
    local tstamp=$(date '+%Y-%j-%H%M')                      # 2011-350-1104

    # pad string
    local len=${#0}                     # length of "$0"
    let "len += 2"                      # trial and error adjustment
    printf -v pad '%*s' "$len" " "      # pad string

    # message
    echo
    cat << EOM
               usage: $SCRIPT [opts]       $oo     diff scenario file '$oo' to create a patch file against '$ooo'
                      $SCRIPT  --rm       [$ooo]   interactively remove diffs associated with $ooo (long option required)
                      $SCRIPT  --clean-rejs   -c  interactively remove patch reject files
                      $SCRIPT  --rev-patch    -r  reverse patch the '00' scenario files, run after altering the RES
                      $SCRIPT  --summary      -s  summarize scenario files, add # if diff exists
                      $SCRIPT  --tell-diffs   -t  show abbreviated view of the diff files piped thru 'less' (-S to unchop, q to quit)
                      $SCRIPT  --up-diff      -u  run 'diff' over all 00 files
                      $SCRIPT  --edit         -e  open all scenario files using '$emacs'
                      $SCRIPT  --help         -h  display this message and exit (takes priority)
                      $SCRIPT  --HELP         -H  display '$xleaf' help message and exit (takes priority)
    options (getopt):  -c                     run diff thru 'cat'
                       -i                     scan RCS idents (default is to skip)
                       -l                     run diff thru 'less' (q to quit)
                       -y                     dry-run (no files will change)
                       -z                     zero lines of context (default is $conlines lines)
             purpose: generate a 'patch' to store a new 'xeona' scenario
        dynamic args: $ooo $oo $x
        diff against: $RES (edit script to change)
  naming conventions: reference energy system = $poc$ooo$guard$xem
                      scenario base           = $sen$oo$xem
                      scenario patch          = $poc$ooo.$x$dif
            examples: $ $0 -lz $oo
                      $ $0 -c $oo
                      $ $0 -t | cat > told.${tstamp}.txt
     update workflow: when the RES file has been modified (the base file and all diff files should be under 'subversion')
                      first align the scenario files based on information held in the diff files, then update the diff files
                      $ $0 --rev-patch                $pad # automatic
                      $ $0 --up-diff                  $pad # prompted for overwrite
                      $ $0 --rev-patch && $0 --up-diff     # combined
                      # check in above, then rerun to confirm just timestamps, and then revert: $ svn revert *.dif
    reverse patching: $ patch $poc$ooo$guard$xem --output="$sen$oo$xem" --input="$poc$ooo.$x$dif"
               notes: a study is a collection of scenarios relative to the given reference energy system or RES
                      adopting zero lines of context will make it more difficult to patch altered originals
        useful links: http://www.linuxchix.org/content/courses/kernel_hacking/lesson9
                      http://ramblings.narrabilis.com/wp/generating-and-reading-patch-files/
            see also: xgrab = utility to parse XEM files
                      xsenario.sh  getstudy.sh  runstudy.sh  saystudy.sh  allstudy.sh  xplot.R  seestudy.R
EOM
    echo
}

# ---------------------------------
#  rm_difs()
# ---------------------------------

function rm_difs
{
    local pocnum="$1"
    case "$pocnum" in
        "")
            local pattern="${poc}$RES.[+a-z]${dif}"
            echo "$SCRIPT: pattern: $pattern"
            rm --interactive $pattern
            ;;
        [0-9][0-9][0-9])                # CAUTION: the three 000 is correct
            local pattern="${poc}$pocnum.[+a-z]${dif}"
            echo "$SCRIPT: pattern: $pattern"
            rm --interactive $pattern
            ;;
        *)
            echo "$SCRIPT: unacceptable poc number: '$pocnum'"
            return 1
            ;;
    esac
}

# ---------------------------------
#  sum_sens()
# ---------------------------------

#  program.study-description
#
#      study-name s            > "phd"
#      scenario-name s         > "indoor temp at 17C (normally 21C)"
#      scenario-author s       > "robbie"
#      scenario-leta s (+,a-z) > "a"

function sum_sens
{
    local   res=$(find -P . -maxdepth 1 -name "$poc$RES$guard$xem" -printf "%P\n" | sort)
    local files=$(find -P . -maxdepth 1 -name "$sen[0-9][0-9]$xem" -printf "%P\n" | sort)
    files="$res $files"
    test -n "$files" && echo
    for file in $files
    do
        # interrogate sen file
        local leta=$($xgrab --quiet --unquote "$file" "program.study-description.scenario-leta")
        local name=$($xgrab --quiet --unquote "$file" "program.study-description.scenario-name")
        # hunt for diff file
        local dis=" "
        local difile="$poc$RES.$leta$dif"
        test -f "$difile" && dis="#"
        # assemble and report
        printf "  %-18s  %-2s  %-2s  %s\n" "$file" "$leta" "$dis" "$name"
    done
    test -n "$files" && echo
}

# ---------------------------------
#  rev_patch_helper()
# ---------------------------------

function rev_patch_helper
{
    local diff="$1"

    local line=$(sed --silent '1,1p' "$diff")     # line one
    local base=$(awk '{ print $2 }' <<< "$line")  # field two

    local line=$(sed --silent '2,2p' "$diff")     # line two
    local sen=$(awk '{ print $2 }' <<< "$line")   # field two

    if [ -f "$sen" ]
    then
        local call="patch \"$base\" --output=\"$sen\" --input=\"$diff\""
        echo "  $diff : $call"
        eval "$call"
    else
        echo "  $diff : associated scenario file not found: $sen"
        return 1
    fi
}

# ---------------------------------
#  rev_patch()
# ---------------------------------

function rev_patch
{
    local rets=0                        # summed returns
    local files=$(find -P . -maxdepth 1 -name "$poc$RES.[a-z]$dif" -printf "%P\n" | sort)

    test -n "$files" && echo
    for file in $files
    do
        rev_patch_helper "$file"
        local ret=$?
        let "rets += ret"
        echo
    done

    case "$rets" in
        0) echo "  no problems identified"                   ;;
        *) echo "  problems occured, summed returns: $rets"  ;;
    esac
    echo

    case "$rets" in
        0) return 0                 ;;
        *) return $E_REVPATCH_FAIL  ;;
    esac
}

# ---------------------------------
#  tell_diffs_helper()
# ---------------------------------

function tell_diffs_helper
{
    local file="$1"
    echo "$file"
    echo
    cat "$file" \
        | tail --lines=+3 \
        | grep "^[+-]" | grep "[<>]" \
        | grep --invert-match "summarize-list\|report-list"
    echo
}

# ---------------------------------
#  tell_diffs()
# ---------------------------------

function tell_diffs
{
    local files=$(find -P . -maxdepth 1 -name "$poc$RES.[a-z]$dif" -printf "%P\n" | sort)
    for file in $files
    do
        tell_diffs_helper "$file" | less --chop-long-lines  # this 'less' pipe works really well
    done
}

# ---------------------------------
#  clean_rejs()
# ---------------------------------

function clean_rejs
{
    echo "$SCRIPT: interactively cleaning patch reject files"
    local files=$(find -P . -maxdepth 1 -name "$sen[0-9][0-9]$xem$rej" -printf "%P\n" | sort)
    test -z "$files" && return 0
    echo "$files"
    rm --interactive *$rej              # CAUTION: omit soft-quotes
}

# ---------------------------------
#  up_diff()
# ---------------------------------

function up_diff
{
    echo "$SCRIPT: interactively updating all diff files"
    local files=$(find -P . -maxdepth 1 -name "$sen[0-9][0-9]$xem" -printf "%P\n" | sort)
    test -z "$files" && return 0
    echo
    echo "scenario files : "$files
    echo
    local fails=0
    local nums=$(sed 's/.*\([0-9][0-9]\).*/\1/' <<< "$files")
    for num in $nums
    do
        echo "scenario $num"
        $0 "$num"                       # principal call
        ret=$?
        test $ret -ne 0 && let "fails++"
    done

    echo "$SCRIPT: showing svn dif (may well be blank)"
    echo
    svn diff --diff-cmd sdiffwrap *.dif # CAUTION: omit soft-quotes

    case "$fails" in
        0)
            return $E_SUCCESS
            ;;
        1)
            echo "$SCRIPT: $fails diff attempt failed"
            echo
            return $E_UPDIFF_FAIL
            ;;
        *)
            echo "$SCRIPT: $fails diff attempts failed"
            echo
            return $E_UPDIFF_FAIL
            ;;
    esac
}

# ---------------------------------
#  emacs_sens()
# ---------------------------------

function emacs_sens
{
    local glob=$(find -P . -maxdepth 1 -name "$sen[0-9][0-9]$xem" -printf " %P\n" | sort)
    local glob=$(echo $glob)
    local call="$emacs $glob"

    echo
    echo "$SCRIPT: $call"
    echo
    eval "$call"
    ret=$?
    echo

    return $ret
}

# ---------------------------------
#  process command-line 1
# ---------------------------------

cline="$SCRIPT $@"                      # grab command-line without given path
cline="$0 $@"                           # grab command-line with given path

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        shift
        display_help "$1" "$2"
        sleep 1                         # so the 'tstamp' is always unique
        exit $E_SUCCESS
        ;;

    --HELP|--HEL|--HE|--H|-HELP|-HEL|-HE|-H|--Help|--Hel|--He|--H|-Help|-Hel|-He|-H)
        shift
        echo
        echo "$SCRIPT: invoking help for '$xgrab'"
        $xgrab --help
        exit $E_SUCCESS
        ;;
    --rm)
        shift
        rm_difs "$1"
        exit
        ;;
    --clean-rejs|--cleanrejs|-c)
        shift
        clean_rejs
        exit
        ;;
    --rev-patch|--revpatch|-r)
        shift
        rev_patch
        exit
        ;;
    --up-diff|-updiff|-u)
        shift
        up_diff
        exit
        ;;
    --edit|-e)
        shift
        emacs_sens
        exit
        ;;
    --summary|-s)
        shift
        sum_sens
        exit
        ;;
    --tell-diffs|--telldiffs|-t)
        shift
        tell_diffs
        exit
        ;;
esac

# ---------------------------------
#  process command-line 2
# ---------------------------------

# CAUTION: neither '-r' nor '-m' are options, hence '-rm' errors out

dryrun=""
idents="skip"

while getopts ":chilyz" option           # CAUTION: the leading : should be correct
do
    case "$option" in
        -)  :                      ;;   # this codes for option "--" and do nothing is correct
        c)  mode="cat"             ;;
        i)  idents="scan"          ;;
        l)  mode="less"            ;;
        y)  dryrun="yes"           ;;
        z)  conlines=0             ;;

        h)
            display_help
            exit $E_SUCCESS
            ;;
        *)
            echo "$SCRIPT: incorrect usage (try --help)"
            exit $E_USAGE
            ;;
    esac
done
shift $(($OPTIND - 1))

# obtain the remaining arguments

now="$1"                                # something like "01"
shift

# ---------------------------------
#  integrity checks
# ---------------------------------

# [1] if the RES case is allowed, it produces an empty
# (zero byte) diff file with the scenario leta '+'

case "$*" in
    "")
        :
        ;;
    *)
        echo "$SCRIPT: surplus command-line arguments given (try --help): '$*'"
        exit $E_USAGE
        ;;
esac

case "$now" in
    "$RES")
        echo "$SCRIPT: command-line argument matches RES '$RES' (try --help)"   # [1]
        exit $E_USAGE
        ;;
    [0-9][0-9])
        :
        ;;
    "")
        echo "$SCRIPT: command-line argument not given (try --help)"
        exit $E_USAGE
        ;;
    *)
        echo "$SCRIPT: invalid command-line argument (try --help)"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  confirm_utility()
# ---------------------------------

function confirm_utility
{
    local utility="$1"

    test $(which "$utility")
    local ret=$?
    case "$ret" in
        0)
            return 0
            ;;
        1)
            report "utility not found" "'$utility'"
            let "errors++"
            return 1
            ;;
        *)
            report "unexpected 'which' return" "$ret"
            let "errors++"
            return 2
            ;;
    esac
}

# ---------------------------------
#  confirm_file()
# ---------------------------------

function confirm_file
{
    local filename="$1"

    if [ -f "$filename" ]
    then
        report "file confirmed" "$filename"
        return 0
    else
        report "file not found" "$filename"
        let "errors++"
        return 1
    fi
}

# ---------------------------------
#  textcheck_file()
# ---------------------------------

function textcheck_file
{
    local filename="$1"

    local result=""                     # store results

    # look for trailing spaces and trailing tabs
    gawk "/[ \t]$/ { exit 9 }" "$filename"
    test $? -eq 9 && result="$result trails"

    # look for double blank lines
    local doubleblank=$(gawk 'BEGIN { RS = "\n\n\n" } END { print NR - 1 }' "$filename")
    test $doubleblank -eq 0 || result="$result, blanks"

    result=${result# }
    result=${result#,}
    result=${result# }
    if [ -n "$result" ]
    then
        report "text $result" "$filename"
        let "errors++"
        return 1
    else
        return 0
    fi
}

# ---------------------------------
#  clear_filename()
# ---------------------------------

# will prompt if files need deleting

function clear_filename
{
    local filename="$1"

    # return as required
    echo
    test ! -f "$filename" && return 0

    # use simple key strike
    local prompt="  hit 'y' to delete '$filename': "
    read -n 1 -p "$prompt" response     # "-n 1" is read one character and return
    echo

    test "$dryrun" == "yes" && echo
    test "$dryrun" == "yes" && return 0

    case "$response" in
        y|Y)
            echo
            rm "$filename"
#           rm --verbose "$filename"
            return 0
            ;;
        *)
            let "errors++"
            return 1
            ;;
    esac
}

# ---------------------------------
#  get_leta()
# ---------------------------------

leta="(not set)"

function get_leta
{
    local xemfile="$1"
    local target="program.study-description.scenario-leta"

    test "dryrun" == "yes" && return 0

    local call="$xgrab --quiet --unquote $xemfile \"$target\""
    report "$xleaf call" "$call"
    leta=$(eval "$call")
    local ret=$?
    test $ret -eq 0 || cexit "$xleaf returned fail: $ret" $E_XGRAB_FAIL
    report "leta" "$leta"
}

# ---------------------------------
#  run_diff()
# ---------------------------------

# CAUTION: regarding ignored idents: the use of
# slashes, single-quotes, and double-quotes has been
# carefully refined -- so modify at your peril

function run_diff
{
    case "$idents" in
        skip) local call="diff --unified=\"$conlines\" --ignore-matching-lines='$exregex' $base $scen > $pach"  ;;
        scan) local call="diff --unified=\"$conlines\" $base $scen > $pach"                                     ;;
    esac

    report "diff call:"                 # intentionally on two lines
    report "$call"

    test "dryrun" == "yes" && return 0

    eval "$call"
    local ret=$?

    case "$ret" in
        0) cexit "diff returned identical: $ret" $E_DIFF_SAME  ;;
        1) report "files differ" "yes"                         ;;
        *) cexit "diff returned fail: $ret"      $E_DIFF_FAIL  ;;
    esac

    local wcpach=$(wc --lines "$pach" 2>/dev/null | cut --delimiter=' ' --fields="1")
    local lspach=$(ls -l      "$pach" 2>/dev/null)
    report "diff file listing"    "$lspach"
    report "diff file line count" "$wcpach lines"
    report "diff file"            "$pach"
}

# ---------------------------------
#  active code
# ---------------------------------

errors=0                                # zero error count

report
deport "active code commencing"

test "$dryrun" == "yes" && report "dryrun" "$dryrun"
report "lines of context" "$conlines"
report "RCS idents"       "$idents"
report

confirm_utility "$xgrab"

cexit "utility checks failed" $E_NO_UTILITY $errors

base="${poc}${RES}${guard}${xem}"       # file containing RES
scen="${sen}${now}${xem}"               # scenario file

confirm_file "$base"
confirm_file "$scen"

cexit "file find checks failed" $E_FILE_NOT_FOUND $errors

textcheck_file "$base"
textcheck_file "$scen"

cexit "textchecks failed" $E_TEXTCHECK_FAIL $errors

get_leta "$scen"                        # makes use of 'xgrab'

pach="${poc}${RES}.${leta}${dif}"

clear_filename "$pach"

cexit "filename clearances failed" $E_FILENAME_CLEAR_FAIL $errors

run_diff

case "$mode" in
    less) less "$pach"        ;;
    cat)  echo; cat  "$pach"  ;;
esac

cexit "script complete" $E_SUCCESS

# ---------------------------------
#  housekeeping
# ---------------------------------

#  $Id: xpatch.sh 8654 2011-12-27 07:55:07Z robbie $
#  end of file

