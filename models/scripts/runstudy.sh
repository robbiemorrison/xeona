#! /bin/bash

#  file-purpose     : run a study -- a suite of 'xeona' scenarios / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Tue 08-Nov-2011 17:13 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 8745 $
#  $Date: 2011-12-31 10:24:37 +0100 (Sat, 31 Dec 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/runstudy.sh $

#  TODO: scan slog and sum the 'xeona' returns
#
#  file was early 'xstudy.sh'
#
#  the last sequential version was r7957
#
#  developed with 'parallel' version 20111022

#  NOTES
#
#  Parallel processing of scripts
#
#    There are a number of strategies for invoking
#    scripts in parallel on multi-core hardware (or on
#    other machines).  This file uses 'GNU parallel'.
#    GNU parallel is written in perl and needs to
#    downloaded as a tarball, then: unpacked,
#    configured, made, installed (under root or using
#    sudo).  See:
#
#      http://www.gnu.org/software/parallel/
#      http://en.wikipedia.org/wiki/GNU_parallel
#
#    Other parallelization utilities include special
#    makefiles and make --jobs=4, xargs -P 4, and
#    'parallel' from package 'moreutils'.  If you do
#    not wish to use GNU parallel, then these other
#    methods may work.  Only one line of code in this
#    file should need replacement.  There is a lot of
#    information on this topic on the web.
#
#  Number of cores
#
#    $ grep processor /proc/cpuinfo | wc --lines

# ---------------------------------
#  user modifiable section
# ---------------------------------

say="all"                               # do both (not recommended)
say="log"                               # print scenario log using 'xsenario.sh'
say="rep"                               # print scenario log and report using 'saystudy.sh'

# support scripts

xsenario="xsenario.sh"
saystudy="saystudy.sh"

# settings (note leading space)

XOPTS_DEF=" -C"                         # xsenario options: '-C' is display study log file (note leading space)
XOPTS_DEF=""                            # xsenario options: null

SOPTS=" --extended"                     # saystudy options: "min mean max" reporting (note leading space)
SOPTS=""                                # saystudy options: null

defjobs="3"                             # default number of cores to use
defjobs="0"                             # default number of cores to use
defjobs="-1"                            # default number of cores to use

nicedelta="+0"                          # default priority, see 'nice' manpage
nicedelta="+10"                         # deprioritize significantly, see 'nice' manpage

# file extensions

xem=".xem"
xog=".xog"
dif=".dif"
log=".log"
rej=".rej"                              # 'patch' reject file extension
con=".con"
com=".txt"                              # run comment

defcode="2714"                          # as per 'xem-mode' input code, used only in examples

# old pattern = "poc.000.guard.xem"

pix="poc."
guard=".guard"

# new pattern = "trial-000.xem"

pix="trial-"
guard=""

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")                 # name of this script

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2
E_BAD_POCNUM=10

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
#  display_help() - display help
# ---------------------------------

function display_help
{
    local example="$1"
    test -z "$example" && example="000"

    echo
    cat << EOM
               usage: $SCRIPT [--jobs=n] [opts] 000    run study '000' using all available files
                      $SCRIPT  --dryrun  [opts] 000    dryrun for above (also -d, must be first)
                      $SCRIPT  --patch-log      000    display patch log
                      $SCRIPT  --xeona-log      000    display xeona exceptions log
                      $SCRIPT  --help                  display this message and exit (takes priority)
                      $SCRIPT  --Help                  display help for $xsenario (takes priority)
               where: 'opts' are passed directly to '$xsenario'
             purpose: run a suite of 'xeona' scenarios using '$xsenario'
                note: '+' is the special reference energy system (RES) scenario
           hardcodes: $xsenario option:$XOPTS_DEF
  required utilities: 'xgrab' '$xsenario'
  naming conventions: reference energy system = ${pix}000${guard}${xem}
                      study-wide log file     = ${pix}000${guard}${log}
                      console capture file    = ${pix}000${con}
                      scenario patch          = ${pix}000.a${dif}
                      scenario model          = ${pix}000.a${guard}${xem}
             example: single interval (mode 6) confirmation test : $ $SCRIPT  -m 6 $example
                      8760 hour run with release built           : $ $SCRIPT -Rm 8 $example
               notes: a study is a collection of scenarios relative to the given reference energy system or RES
              useful: 'htop' for noting processes and TIME+ elapsed time
            see also: xuse = display 'xeona' usage message
EOM
    echo
}

# ---------------------------------
#  patch_log()
# ---------------------------------

function patch_log
{
    local pocnum="$1"

    test -z "$pocnum" && pocnum="008"   # assumption confined to this file (maybe it should be general)
    local confile="$pix$pocnum$con"
    if [ -f "$confile" ]
    then
        echo
        echo "patch log recovered from console capture file: $confile"
        echo
        cat "$confile" \
            | grep --invert-match "^$" \
            | grep --invert-match "^ " \
            | grep --invert-match "^console capture" \
            | grep --invert-match "^\*\* xeona::" \
            | cat --squeeze
        echo
        return 0
    else
        echo "$SCRIPT: console capture file not found: $confile"
        return 1
    fi
}

# ---------------------------------
#  xeona_log()
# ---------------------------------

function xeona_log
{
    local pocnum="$1"

    test -z "$pocnum" && pocnum="008"   # assumption confined to this file (maybe it should be general)
    local confile="$pix$pocnum$con"
    if [ -f "$confile" ]
    then
        echo
        echo "xeona exception messages recovered from console capture file: $confile"
        echo
        cat "$confile" \
            | grep --invert-match "^$" \
            | grep --invert-match "^ " \
            | grep --invert-match "^console capture" \
            | grep --invert-match "^patching file" \
            | cat --squeeze
        echo
        return 0
    else
        echo "$SCRIPT: console capture file not found: $confile"
        return 1
    fi
}

# ---------------------------------
#  process command-line 1
# ---------------------------------

cline="$0 $@"                           # grab command-line with given path
cline="$SCRIPT $@"                      # grab command-line without given path

mode="normal"
jobs=$defjobs                           # set in preamble

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        shift
        display_help "$1"
        exit $E_SUCCESS
        ;;
    --Help|--Hel|--He|--H|-Help|-Hel|-He|-H|--HELP|--HEL|--HE|--H|-HELP|-HEL|-HE|-H)
        shift
        display_help "$1"
        echo "$SCRIPT: calling $xsenario --help"
        $xsenario --help
        exit
        ;;
    --dryrun|--dry-run|-d|-y)
        shift
        mode="dryrun"
        ;;
    --patch-log|--patchlog|-p)
        shift
        patch_log "$1"
        exit
        ;;
    --xeona-log|--xeonalog|-x)
        shift
        xeona_log "$1"
        exit
        ;;
    --jobs\=[0-9]|--jobs\=[+-][0-9])    # jobs long-form
        buf="$1"
        shift
        arg=$(cut --only-delimited --delimiter="=" --fields=2 <<< "$buf")
        jobs="$arg"
        ;;
    -j[0-9]|-j[+-][0-9])                # jobs short-form
        buf="$1"
        shift
        arg=${buf#-j}
        jobs="$arg"
        ;;
    "")
        echo "$SCRIPT: at least one command-line argument required (try --help)"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  process arguments (subtle)
# ---------------------------------

xopts="$XOPTS_DEF"
pocnum=""

argc=$#
while [ true ]
do
    pocnum="$1"
    test $argc -eq 1 && break
    xopts="$xopts $1"                   # CAUTION: do not later trim leading space
    shift
    let "--argc"
done

# ---------------------------------
#  integrity checks
# ---------------------------------

case "$pocnum" in
    [0-9][0-9][0-9])
        :                               # argument is valid
        ;;
    *)
        echo "$SCRIPT: final command-line argument invalid (try --help): '$pocnum'"
        exit $E_BAD_POCNUM
        ;;
esac

cores=$(grep "processor" /proc/cpuinfo | wc --lines)

report
report "command"                 "$cline"
report "pocnum"                  "$pocnum"
report "mode"                    "$mode"
report "cores"                   "$cores"
report "parallel jobs (see dox)" "$jobs"
report "nice delta"              "$nicedelta"
test -n "$xopts" && report "$xsenario passed" "${xopts# }"
test -n "$xopts" || report "$xsenario passed" "(nothing)"
report

# ---------------------------------
#  confirm_utility
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
            test "$utility" == "parallel" && report "note" "GNU parallel requires tarball plus configure make install"
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
#  create_alphabet()
# ---------------------------------

alphabet=""                                       # will end up { a .. z }

function create_alphabet
{
    for ascii in $(seq 97 122)
    do
        printf -v leta "\x$(printf %x $ascii)\n"  # would be nicer in 'perl'
        alphabet="$alphabet $leta"
    done
    alphabet=${alphabet# }                        # strip leading space
    report "scan list" "'$alphabet'"
}

# ---------------------------------
#  user_confirm()
# ---------------------------------

function user_confirm
{
    local extra="$1"                         # dedicated message

    test "$mode" == "dryrun" && return 0     # CAUTION: must be zero

    echo
    local prompt="  hit 'y' to $extra: "
    read -n 1 -p "$prompt" response          # "-n 1" is read one character and return
    echo
    case "$response" in
        y|Y)
            return 0                         # confirmed
            ;;
        *)
            echo
            return 1
            ;;
    esac
}

# ---------------------------------
#  delete_file()
# ---------------------------------

function delete_file
{
    local filename="$1"

    test "$mode" == "dryrun" && return 1     # CAUTION: should be non-zero

    if [ -f "$filename" ]
    then
        rm "$filename"
        return 0
    else
        return 1
    fi
}

# ---------------------------------
#  check_rej()
# ---------------------------------

# form: trial-008.e.xem.rej

function check_rej
{
    local stub="$1"

    local glob=$(find -P . -maxdepth 1 -name "$stub.[+a-z]$xem$rej" -type f -printf "%P\n" | sort)

    if [ -z "$glob" ]
    then
        report "no patch failures detected"
        return 0
    else
        local rejs=$(echo -n $glob)     # stringify
        report "patch failures occured"
        report "problematic files" "$rejs"
        return 1
    fi
}

# ---------------------------------
#  check_slog()
# ---------------------------------

slog_total=0

function check_slog
{
    local slog="$1"

    if [ ! -f "$slog" ]
    then
        report "scenario log file missing" "$slog"
        return 1
    fi

    local total=0
    while read line
    do
        local num=$(cut --delimiter=' ' --field=1 <<< "$line")
        let "total += num"
    done < "$slog"

    slog_total="$total"
    case "$total" in
        0)
            report "no problematic xeona invocations"
            return 0
            ;;
        *)
            report "combined xeona return" "$total (issues occured)"
            return 1
            ;;
    esac
}

# ---------------------------------
#  record_opening_comment()
# ---------------------------------

# interactively take a comment and write it, together
# with other start-time information, to the study
# comment file

function record_opening_comment
{
    local cfile="$1"
    local comment

    # opening
    local info="study run information"
    info="role       : $info"

    # current subversion
    local xeona=${XEONA%/trunk/xeona1}
    local csvn=$(svnversion "$xeona")
    csvn="subversion : $csvn"

    # opening timestamp
    local date=$(date '+%a %d-%b-%Y')
    local time=$(date '+%H:%M %Z %z')
    start="start      : $date $time"

    # user information
    local user="$USER"
    local full=$(getent passwd $USER)
    full=$(cut --delimiter=":" --fields=5 <<< "$full")      # extract
    full=$(sed 's/,//g'                   <<< "$full")      # remove any ","
    user="user       : $user ($full)"

    # comment
    echo
    echo -n "  enter a comment describing this run: "
    read comment
    comment=$(echo $comment)            # cheap way of removing spaces
    test -z "$comment" && comment="(none)"
    comm="comment    : $comment"

    # record
    echo "$info"  >> "$cfile"
    echo "$user"  >> "$cfile"
    echo "$start" >> "$cfile"
    echo "$csvn"  >> "$cfile"
    echo "$comm"  >> "$cfile"

    # return
    return 0
}

# ---------------------------------
#  record_closing_comment()
# ---------------------------------

function record_closing_comment
{
    local cfile="$1"

    # used svn
    local leta="+"
    local xemfile="$pix$pocnum.$leta$xem"
    local fqf="program.last-run.used-svn"
    local fmsg="(perhaps not run)"
    local usvn=$(xgrab --quiet --failmsg "$fmsg" "$xemfile" "$fqf")
    usvn="used svn   : $usvn"

    # closing timestamp
    local date=$(date '+%a %d-%b-%Y')
    local time=$(date '+%H:%M %Z %z')
    close="finish     : $date $time"

    # record
    echo "$usvn"  >> "$cfile"
    echo "$close" >> "$cfile"

    # return
    case '$usvn' in
        "$fmsg") return 1  ;;
        *)     return 0  ;;
    esac
}

# ---------------------------------
#  active code 1 - preparation
# ---------------------------------

confirm_utility "parallel"              # GNU 'parallel' requires tarball plus configure make install

havebase="1"                            # presume 'base' is present

base="$pix$pocnum$guard$xem"            # reference energy system
slog="$pix$pocnum$log"                  # study log
scom="$pix$pocnum$com"                  # study comment

confirm_file "$base" || havebase="0"

# identify relevant files (script 'saystudy.sh' uses 'find' and is probably cleaner code)

letas=""
create_alphabet 1>/dev/null
for leta in $alphabet
do
    pach="$pix$pocnum.$leta$dif"
    confirm_file "$pach" 1>/dev/null && letas="$letas $leta"
done
letas=${letas# }
if [ -n "$letas" ]
then
    report "letas" "$letas"
else
    report "letas" "(none)"
fi

for leta in $letas
do
    pach="$pix$pocnum.$leta$dif"
    confirm_file "$pach"                # here for reporting side-effect
done

case "$havebase" in
    0) mode="dryrun"     ;;             # morph into dryrun mode
    1) letas="+ $letas"  ;;             # add scenario '+'
esac

report
for leta in $letas
do
    xcall="$xsenario$xopts $pocnum $leta"
    report "xcall" "$xcall"
done

# console capture

confile="$pix$pocnum$con"
timestamp=$(date --utc '+%a %d-%b-%Y %T UTC')
rm --force "$confile"

{
    echo
    echo "console capture starting"
    echo
    echo "  capture file : $confile"
    echo "  host script  : $SCRIPT"
    echo "  timestamp    : $timestamp"
} >> "$confile"

# ---------------------------------
#  active code 2 - run
# ---------------------------------

# function 'user_confirm' tests for dryrun mode

exitsum=0                                  # sum of some (not all) exit codes

user_confirm "continue with run (relevant study log and scenario xog files will be automatically deleted)" &&
{
    delete_file            "$scom"
    record_opening_comment "$scom"

    delete_file "$slog"                 # clear study log

    feed=""                             # newline-separated version of 'letas' for 'parallel'
    for leta in $letas
    do
        xogfile="$pix$pocnum.$leta$xog"
        delete_file "$xogfile"                    # delete XOG file
        printf -v feed "%s%s\n" "$feed" "$leta"   # newline-separated is necessary
    done
    feed=$(sed 's/\n$//' <<< "$feed")   # remove last newline

    callstub="$xsenario$xopts $pocnum"  # base call

    report
    report "letas"     "$letas"
    report "call stub" "$callstub"
    report "scenarios" "about to invoke GNU parallel with a maximum of $jobs jobs"
    report "BE AWARE"  "output buffered until that job and all earlier jobs are complete"
    report

    # some subtleties to the GNU 'parallel' call:
    # 'feed' is newline-separated but should not have a
    # trailing newline and the "{}" should not contain
    # a space char

    case "$mode" in
        dryrun) echo "$feed" | parallel --dry-run --nice="$nicedelta" --jobs="$jobs" --keep-order "$callstub" {}; echo ;;    # WORKHORSE
        *)      echo "$feed" | parallel           --nice="$nicedelta" --jobs="$jobs" --keep-order "$callstub" {}       ;;    # WORKHORSE
    esac

    # sort the logfile
    #
    # although the job output is shown in order of
    # invocation, this guarantee does not extend to
    # writing to the scenario log file -- but given
    # that + sorts above [a-z], a simple call to 'sort'
    # will fix that:

    if [ -f "$slog" ]
    then
        sortcall="sort --key=3 --output=\"$slog\" $slog"
        eval "$sortcall"                # comment out if raw file is required
    fi

    # print sorted log

    case "$say" in
        log|all)
            slogcall="$xsenario --show-log $pocnum"
            report "show log call" "$slogcall"
            eval "$slogcall"
            ;;
    esac

    # print summary

    case "$say" in
        rep|all)
            saycall="$saystudy$SOPTS $pocnum"
            report "say study call" "$saycall"
            report "comment"        "saystudy run directly offers extended reporting"
            eval "$saycall"
            ;;
    esac

    # report any patch failures

    check_rej "$pix$pocnum" || let "++exitsum"

    # check the 'xeona' exits recorded in the study log

    check_slog "$slog" || let "++exitsum"

    # extend and display comment file

    report
    record_closing_comment "$scom"
    cat                    "$scom"

    # final newline

    report

} |& tee --append "$confile"            # user-confirm block

{
    info="$xsenario$xopts $pocnum"
    elapsed=$SECONDS
    test $(which hms) && elapsed=$(hms $elapsed)  # 'hms' is a my (user-local) utility

    report "command"                  "$cline"
    report "$SCRIPT"                  "complete using '$info' and '$letas'"
    report "elapsed (hms or seconds)" "$elapsed"

} |& tee --append "$confile"            # reporting block

{
    echo
    echo "console capture complete"
    echo
} >> "$confile"

# ---------------------------------
#  housekeeping
# ---------------------------------

case "$exitsum" in
    0)
        report "script exit" "$E_SUCCESS (success)"
        report
        exit $E_SUCCESS
        ;;
    *)
        report "script exit" "$E_FAILURE (issues)"
        report
        exit $E_FAILURE
esac

#  $Id: runstudy.sh 8745 2011-12-31 09:24:37Z robbie $
#  end of file

