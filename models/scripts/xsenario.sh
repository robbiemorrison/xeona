#! /bin/bash

#  file-purpose     : run a 'xeona' scenario after applying 'patch' / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Thu 03-Nov-2011 10:23 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9114 $
#  $Date: 2012-02-21 15:33:27 +0100 (Tue, 21 Feb 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/xsenario.sh $

#  NOTES
#
#  useful coding examples include: 'wgetme' 'xglpk.sh' 'xglpsol'
#  function 'get_field_value' present in r7874 and then removed
#
#  SAMPLE HARVEST
#
#  xeona  time leta  scenario name               steps    fin       ghg       nox       dep       luc         xem file
#
#      0     5   +   "reference energy system"    12     +5.49e+17 +9.94e+06 +0.00e+00 +7.38e+13 +0.00e+00    trial-008.+.xem

# ---------------------------------
#  user modifiable section
# ---------------------------------

xgrab="xgrab"                           # binary for parsing XEM files

# file extensions

xem=".xem"
xog=".xog"
dif=".dif"
log=".log"

defcode="2714"                          # as per 'xem-mode' input code, used only in examples

# pattern = "poc.000.guard.xem"

pix="poc."
guard=".guard"

# pattern = "trial-000.xem"

pix="trial-"
guard=""

# utilities

xeonabin="xeonabin"

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")                 # name of this script

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2
E_FILE_EXISTS=11
E_NO_UTILITY=12
E_PATTERN_FAIL=13
E_CP_FAIL=14
E_PATCH_FAIL=15
E_FILE_FIND_FAIL=16
E_FILENAME_CLEAR_FAIL=17
E_XEONA_FAIL=18
E_XEONA_USER_INTERRUPT=19
E_XGRAB_FAIL=20
E_MKTEMP_FAIL=21

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
        0)  return 0                               ;;
        "") report                                 ;;
        *)  report && report "error count" "$errs" ;;
    esac

    local elapsed=$SECONDS
    test $(which hms) && elapsed=$(hms $elapsed)  # 'hms' is a my (user-local) utility

    test -n "$msg"   && report "$FUNCNAME message" "$msg"
    test -n "$msg"   && report
    report "command"                  "$cline"
    report "elapsed (hms or seconds)" "$elapsed"
    report "script exit"              "$xval"
    report

    test -f "$RETLOG" && rm --force "$RETLOG"     # clean up the temporary file

    exit $xval
}

# ---------------------------------
#  display_help() - display help
# ---------------------------------

function display_help
{
    local xgrableaf=$(basename "$xgrab")
    local xeonabinleaf=$(basename "$xeonabin")
    echo
    cat << EOM
               usage: $SCRIPT [opts] 000 a        run pocmodel '000' after applying scenario 'a' patch file
                      $SCRIPT [opts] 000 +        run pocmodel '000' straight
                      $SCRIPT  --show-log 000     sort and show scenario log and quit
                      $SCRIPT  --help             display this message and exit (takes priority)
      script options:  -A s.log   append key scenario results to 's.log' and not the default '${pix}000${log}'
                       -C         display log file a the end of the run
                       -M         use make version 'xeona' instead of mach version
                       -R         use latest release version of 'xeona'
                       -X         prepare scenarios but do not run 'xeona'
       xeona options:  -c 0000    coded value: report[mode[yeek]] (as per 'xem-mode')
                       -a         apply --again
                       -e 0       apply --exittrip 0
                       -j         apply --jumpy
                       -k         apply --krazy
                       -m 0       apply --mode 0
                       -n         apply --nodata
                       -p         apply --pepper
                       -r 0       apply --report 0
                       -t term    apply --tout term
                       -y 00      apply --yeek 00
                       -z         apply --zero
             purpose: prepare and run 'xeona' scenarios and harvest info
  required utilities: '$xeonabinleaf' '$xgrableaf'
  naming conventions: reference energy system = ${pix}000${guard}${xem}
                      study-wide log file     = ${pix}000${guard}${log}
                      scenario patch          = ${pix}000.a${dif}
                      scenario model          = ${pix}000.a${guard}${xem}
               notes: '+' is the special reference energy system (RES) scenario (consider it a null patch)
                      single interval (--mode 6) and full year (--mode 8) options automatically reset the 'steps' field
             example: $ $SCRIPT -m 6                000 +
                      $ $SCRIPT -c $defcode -A "my${log}" 000 a
               notes: a study is a collection of scenarios relative to the given reference energy system or RES
            see also: xgrab     = utility to parse XEM files
                      runstudy = run a suite of 'xeona' scenarios using this script
                      xuse     = display 'xeona' usage message
EOM
    echo
}

# ---------------------------------
#  reset()
# ---------------------------------

function reset
{
    local pocnum="$1"
    local scenleta="$2"

    local errors=0

    local now=$(date '+%Z %z %A %d-%b-%Y %H:%M')
    local now=$(date '+%b %_d %H:%M')

    echo
    echo "$SCRIPT: $FUNCNAME(): hard reset for given files"

    echo
    report "RES"      "$pocnum"
    report "scenario" "$scenleta"
    report "now"      "$now"
    echo

    local trunk="$HOME/$SYNK/xeona/svn2/futz/trunk"

    local srcbase="$trunk/pocmods/poc.$pocnum.guard.xem"
    local srcdif="$trunk/pocmods/poc.$pocnum.$scenleta.dif"

    local tarbase="$trunk/models/$pix$pocnum$guard$xem"
    local tardif="$trunk/models/$pix$pocnum.$scenleta$guard$dif"

    cp --force "$srcbase" "$tarbase" || let "errors++"
    cp --force "$srcdif"  "$tardif"  || let "errors++"

    ls -ltrF $srcbase $tarbase
    echo
    ls -ltrF $srcdif $tardif
    echo

    case "$errors" in
        0) return 0  ;;
        *) return 1  ;;
    esac
}

# ---------------------------------
#  show_slog() - cat scenario log
# ---------------------------------

# although under "parallel --keep-order", the job
# output is shown in order of invocation, this
# guarantee does not extend to writing to the scenario
# log file -- but given that + sorts above [a-z], a
# simple pipe thru 'sort' will fix that
#
# postscript: 'runstudy.sh' now sorts the log file in a
# similar manner so the 'sort' call is now redundant

function show_slog
{
    local slog="$1"

    test -f "$slog" || return 1

    local sled=""                       # scenario log header

    sled="$sled xeona  time leta  scenario name                                    "
    sled="$sled steps    fin        ghg        nox        dep        luc          xem file"

    echo
    echo "$sled"
    echo
    cat "$slog" | sort --key=3          # 3 for third column

    return 0
}

# ---------------------------------
#  process command-line 1
# ---------------------------------

cline="$0 $@"                           # grab command-line with given path
cline="$SCRIPT $@"                      # grab command-line without given path

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        shift
        display_help
        exit $E_SUCCESS
        ;;
    --fix|-f)                           # undocumented option
        reset "008" "a"
        exit
        ;;
    --show-log|--showlog)
        shift
        num="$1"
        show_slog "$pix$num$log"
        ret=$?
        echo
        exit $ret
        ;;
    --fix|-f)                           # undocumented option
        reset "008" "a"
        exit
        ;;
esac

# ---------------------------------
#  xopts() - assemble 'xeona' options
# ---------------------------------

mode6="no"                              # CAUTION: "xeona --mode 6" requires special treatment

function xopts
{
    case "$#" in
        1) printf -v XOPTS "%s %s"    "$XOPTS" "$1"       ;;
        2) printf -v XOPTS "%s %s %s" "$XOPTS" "$1" "$2"  ;;
        *) echo "$SCRIPT: $FUNCNAME(): coding error"      ;;
    esac
    XOPTS=${XOPTS# }                    # strip any leading space
    test "$1" == "--mode" -a "$2" == "6" && mode6="yes"
}

# ---------------------------------
#  ecode() - decode 'xeona' options
# ---------------------------------

# expecting number like 0 thru 0000

function ecode
{
    local nums="$1"
    test -n "$nums" || return
    arg=${nums:0:1}
    nums=${nums#$arg}
    xopts "--report" "$arg"             # call to assemble 'xeona' options
    test -n "$nums" || return
    arg=${nums:0:1}
    nums=${nums#$arg}
    xopts "--mode" "$arg"
    test -n "$nums" || return
    arg="${nums}"                       # use any remainder without further processing
    xopts "--yeek" "$arg"
}

# ---------------------------------
#  process command-line 2
# ---------------------------------

XOPTS=""                                # 'xeona' options
slog=""                                 # study-wide (collection of scenarios) log
mode=""
show=""
binary="mach"

while getopts ":-aA:c:Ce:hjkm:Mnpr:Rt:y:z"  option     # CAUTION: the leading : should be correct
do
    case "$option" in
        -)  :                                ;;        # this codes for option "--" and do nothing is correct
        A)  slog="$OPTARG"                   ;;
        C)  show="logfile"                   ;;
        R)  binary="release"                 ;;
        M)  binary="make"                    ;;
        X)  mode="norun"                     ;;
        h)  display_help; exit $E_SUCCESS    ;;

        a)  xopts "--again"                  ;;
        c)  ecode "$OPTARG"                  ;;
        e)  xopts "--exittrip" "$OPTARG"     ;;
        j)  xopts "--jumpy"                  ;;
        k)  xopts "--krazy"                  ;;
        m)  xopts "--mode"     "$OPTARG"     ;;
        n)  xopts "--nodata"                 ;;
        p)  xopts "--pepper"                 ;;
        r)  xopts "--report"   "$OPTARG"     ;;
        t)  xopts "--tout"     "$OPTARG"     ;;
        y)  xopts "--yeek"     "$OPTARG"     ;;
        z)  xopts "--zero"                   ;;

        *)
            echo "$SCRIPT: incorrect usage (try --help)"
            exit $E_USAGE
            ;;
    esac
done
shift $(($OPTIND - 1))

# obtain the remaining arguments

pocnum="$1"                             # should form '000' pattern (tested later)
shift
scenleta="$1"                           # should form 'a' pattern (tested later)
shift

# ---------------------------------
#  integrity checks
# ---------------------------------

case "$*" in
    "") :                                                                                                 ;;
    *)  echo "$SCRIPT: additional command-line arguments invalid (try --help): '$*'" && exit $E_USAGE     ;;
esac

case "$pocnum" in
    [0-9][0-9][0-9]) :                                                                                    ;;
    "")              echo "$SCRIPT: no command-line arguments given (try --help)" && exit $E_USAGE        ;;
    *)               echo "$SCRIPT: invalid command-line argument (try --help)"   && exit $E_USAGE        ;;
esac

case "$scenleta" in
    "+")   :                                                                                              ;;
    [a-z]) :                                                                                              ;;
    "")    echo "$SCRIPT: scenario leta required as command-line argument (try --help)" && echo $E_USAGE  ;;
    *)     echo "$SCRIPT: additional command-line arguments invalid (try --help): '$*'" && echo $E_USAGE  ;;
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
#  confirm_pattern()
# ---------------------------------

function confirm_pattern
{
    local pattern="$1"
    local target="$2"

    case "$target" in
        $pattern)                       # CAUTION: omit soft-quotes
            return 0
            ;;
        *)
            report "invalid '$pattern'" "'$target'"
            let "errors++"
            return 1
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
#  clear_filename()
# ---------------------------------

# will prompt if files need deleting

function clear_filename
{
    local filename="$1"

    # return as required
    test ! -f "$filename" && return

    # use simple key strike
    echo
    local prompt="  hit 'y' to delete '$filename': "
    read -n 1 -p "$prompt" response     # "-n 1" is read one character and return
    echo
    case "$response" in
        y|Y)
            rm "$filename"
            echo
            return
            ;;
        *)
            let "errors++"
            return 1
            ;;
    esac
}

# ---------------------------------
#  report_xeona()
# ---------------------------------

function report_xeona
{
    local ret="$1"

    # write exit status to file
    echo "$ret" >| "$RETLOG"

    # report and respond
    report "xeona exit status" "$ret"
    case "$ret" in
        0)     report "xeona outcome" "full success";     return 0                                                   ;;
        [1-6]) report "xeona outcome" "okay to continue"; return 0                                                   ;;
        130)   cexit "'xeona' tripped by POSIX SIGINT $ret :  user-initiated ^C interrupt"  $E_XEONA_USER_INTERRUPT  ;;
        134)   cexit "'xeona' tripped by POSIX SIGABRT $ret : runtime issue"                $E_XEONA_FAIL            ;;
        137)   cexit "'xeona' tripped by POSIX SIGKILL $ret : user-initiated hard kill"     $E_XEONA_FAIL            ;;
        139)   cexit "'xeona' tripped by POSIX SIGSEGV $ret : invalid memory usage"         $E_XEONA_FAIL            ;;
        143)   cexit "'xeona' tripped by POSIX SIGTERM $ret : user-initiated mild kill"     $E_XEONA_FAIL            ;;
        *)     report "xeona outcome" "problematic exit"; return 1                                                   ;;
    esac
}

# POSIX signals exit codes:
#
#     130 = user-initiated ^C (^ = ctrl) interrupt (not trapped)  SIGINT  =  2
#     134 = glibc detects memory corruption during malloc call    SIGABRT =  6
#     134 = glibc detects invalid pointer during free call        SIGABRT =  6
#     134 = terminate on uncaught throw                           SIGABRT =  6
#     134 = GLPK abort call                                       SIGABRT =  6
#     137 = user-initiated hard kill (not trapped)                SIGKILL =  9
#     139 = invalid memory use causing a segmentation fault       SIGSEGV = 11
#     143 = user-initiated mild kill (not trapped)                SIGTERM = 15

# ---------------------------------
#  run_xeona()
# ---------------------------------

# Data capture
#
#   Capturing data from the 'xeona' run call is
#   complicated.  The aim was to print stdout but log
#   stdout and stderr correctly.
#
#   For general background on redirection see:
#
#     http://www.linuxtutorialblog.com/post/tutorial-the-best-tips-tricks-for-bash
#
#   For an explanation of the issues see:
#
#     http://mywiki.wooledge.org/BashFAQ/106
#
#   For the code used here see (the 19-Mar-2011 post by 'artistoex'):
#
#      http://unix.stackexchange.com/questions/9646
#      http://unix.stackexchange.com/questions/9646/show-only-stderr-on-screen-but-write-both-stdout-and-stderr-to-file
#
#   The '{}' form a group command.  The '>()' is for
#   process substitution.  The various '>' commands
#   create redirections.  In particular:
#
#     i>&j  means all 'i' gets sent to 'j'
#
#     >(cat)  makes 'cat' command behave like a file
#
#   The variables inside a '{}' block are automatically
#   local.  Only global variables can break this scope.
#
#   For reference, the simpler alternative is:
#
#     { .. } &> "$xogfile"
#
#   where '&>' is semantically equivalent to '> "$file" 2>&1'

xeona_ret=0

function run_xeona
{
    local xemfile="$1"                  # 'xeona' model
    local xogfile="$2"                  # 'xeona' runtime log
    local call=""

    report "about to run scenario" "$xemfile"

    # assemble the call chain
#   xopts "--guard"
    xopts "--file $xemfile"
#   local xeona=${xeona/#$HOME/"~"}     # CAUTION: the ~ must be in quotes to avoid re-expansion
    call="$xeona $XOPTS"
    report "xeona call" "'$call'"
    report "about to invoke xeona"

    # clear 'xogfile' ('runstudy' usually does this first)
    clear_filename "$xogfile"

    # run xeona
    exec 3>"$xogfile"                   # aim file-descriptor 3 at the xeona log file
    {                                   # CAUTION: runs in a subshell due to the pipe
        eval "$call"
        ret=$?                          # CAUTION: 'ret' cannot be made global
        sleep 5                         # because output can become interleaved
        report_xeona "$ret"
    } 2>&3 | tee >(cat 1>&3)            # CAUTION: complicated (see notes for this function)
    exec 3>&-                           # close output file-descriptor 3 (the trailing dash)
}

# ---------------------------------
#  grab_data()
# ---------------------------------

data=""                                 # harvested data buffer

function grab_data
{
    local tab="$1"                      # 'fprint' alignment specifier, use leading minus to left justify
    local xem="$2"                      # 'xeona' model (XEM) file to interrogate
    local fqf="$3"                      # fully-qualified field name (two dot separators)
    local enc="$4"                      # enclosing string, use "" to disable, "\"" for double-quotes, "'" for single-quotes

    local xgrabopts=" --quiet --unquote --failmsg=\"($FUNCNAME)\""
    local xgrabcall="$xgrab$xgrabopts $xem \"$fqf\""
    deport "$xgrab call" "$xgrabcall"
    local value=$(eval "$xgrabcall")
    case "$enc" in
        "") printf -v data "%s %*s" "$data" "$tab" "$value"          ;;
        *)  printf -v data "%s %*s" "$data" "$tab" "$enc$value$enc"  ;;
    esac
    local ret=$?
    test $ret -ne 0 && cexit "xgrab returned fail: $ret" $E_XGRAB_FAIL
}

# ---------------------------------
#  harvest_data()
# ---------------------------------

# subsequently process within 'emacs' or using 'R'
#
#   0  999   a  "good building"   8760 +0.0e+00 +0.0e+00 +0.0e+00 0.+0e+00   trial-000.a.xem
#
# CAUTION: "xeona --mode 6" offers particular
# difficulties as the timeseries are not filled
# properly -- hence the 'mode6' case block

function harvest_data
{
    local xsen="$1"                     # already-run patched 'xeona' scenario
    local slog="$2"                     # study-wide log file

    # premable
    deport "about to harvest data"
    data=""                             # reset global variable

    # get xeona exit status
    local xret=$(cat "$RETLOG")

    # harvest data
    printf -v data "%4d"        $xret
    printf -v data "%s %5d"    "$data" "$SECONDS"
    printf -v data "%s   %-3s" "$data" "$scenleta"

    grab_data "-50" "$xsen" "program.study-description.scenario-name" "\""
    case "$mode6" in
        no)  grab_data  "-6" "$xsen" "entity.time-horizon.steps" ""  ;;
        yes) printf -v data "%s%-7s" "$data" "mode6"                 ;;
    esac
    grab_data  "-9" "$xsen" "entity.overseer.total-financial"  ""
    grab_data  "-9" "$xsen" "entity.overseer.total-greenhouse" ""
    grab_data  "-9" "$xsen" "entity.overseer.total-nox"        ""
    grab_data  "-9" "$xsen" "entity.overseer.total-depletion"  ""
    grab_data  "-9" "$xsen" "entity.overseer.total-landuse"    ""

    printf -v data "  %s    %s" "$data" "$xsen"

    # report data
    report "harvested data (despaced)" "$(echo $data)"

    # write out
    report "about to write to" "$slog"
    test -f "$slog" && touch "$slog"    # create file
    echo "$data" >> "$slog"             # append, 'echo' also adds a newline
}

# ---------------------------------
#  active code  1 : preparations
# ---------------------------------

errors=0                                # zero the error count

deport "active code commencing"

# confirm command-line

echo "     ----------------------------------------------------------------------"
report
report "commencing" "$cline"

report

confirm_pattern "[0-9][0-9][0-9]" "$pocnum"
confirm_pattern "[a-z+]"          "$scenleta"

cexit "pocmodel number and/or scenario letter pattern checks failed" $E_PATCH_FAIL $errors

# confirm utilities

confirm_utility "$xeonabin"

xeona=$($xeonabin --$binary)            # 'binary' in "mach" "make" "release"
xgrab="$xgrab"

confirm_utility "$xgrab"
confirm_utility "$xeona"

cexit "utility checks failed" $E_NO_UTILITY $errors

deport "xeonabin" "$xeonabin"
deport "xgrab"    "$xgrab"
report "xeona"    "$xeona"

# define files

base="$pix$pocnum$guard$xem"            # reference energy system
pach="$pix$pocnum.$scenleta$dif"        # scenario patch
xsen="$pix$pocnum.$scenleta$guard$xem"  # scenario guard model
xlog="$pix$pocnum.$scenleta$xog"        # scenario xog file
test -z "$slog" && slog="$pix$pocnum$log"

# confirm files

confirm_file "$base"
case "$scenleta" in
    [a-z]) confirm_file "$pach"          ;;
    "+")   :                             ;;
    *)     echo "$SCRIPT: coding error"  ;;
esac

cexit "file find checks failed" $E_FILE_FIND_FAIL $errors

# clear any preexisting files

clear_filename "$xmod"
clear_filename "$xlog"

cexit "filename clearances failed" $E_FILENAME_CLEAR_FAIL $errors

# create a safe temporary file

stub=$(basename "$0" ".sh")
RETLOG=$(mktemp --tmpdir="." "$stub~XXXXXXXXXXXX") || cexit "'mktemp' returned fail: $?" $E_MKTEMP_FAIL

# ---------------------------------
#  active code 2 : cp and patch
# ---------------------------------

case "$scenleta" in
    [a-z])
        # copy
        cpcall="cp $base $xsen"
        report "cp call"    "$cpcall"
        eval "$cpcall" || cexit "copy failed ($?)" $E_CP_FAIL
        # patch
        patchcall="patch --no-backup-if-mismatch $xsen $pach" # no backup with ".orig" if mismatch
        report "patch call" "$patchcall"
        echo
        eval "$patchcall" || cexit "patch failed ($?)" $E_PATCH_FAIL
        echo
        # xgrab
        xgrabcall="$xgrab --quiet --unquote --failmsg=\"($SCRIPT)\" $xsen \"program.study-description.scenario-leta\""
        report "call" "$xgrabcall"
        leta=$(eval "$xgrabcall")
        test "$leta" == "$scenleta" || report "scenario-leta / scenleta mismatch" "'$leta' : '$scenleta'"
        # report
        report "copy and patch complete"
        report "scenario xem file"   "$xsen"
        report "scenario xog file"   "$xlog"
        report "study-wide log file" "$slog"
        ;;
    "+")
        # copy (same as case a-z)
        cpcall="cp $base $xsen"
        report "cp call"    "$cpcall"
        eval "$cpcall" || cexit "copy failed ($?)" $E_CP_FAIL
        # report
        report "copy complete (no patch required)"
        report "RES xem file"        "$xsen"
        report "RES xog file"        "$xlog"
        report "study-wide log file" "$slog"
        ;;
esac

xgrabcall1="$xgrab --quiet --failmsg=\"($SCRIPT fail)\" $xsen \"program.study-description.study-name\""
xgrabcall2="$xgrab --quiet --failmsg=\"($SCRIPT fail)\" $xsen \"program.study-description.scenario-name\""
study_name=$(eval "$xgrabcall1")
scenario_name=$(eval "$xgrabcall2")

# ---------------------------------
#  active code 3 : run and harvest
# ---------------------------------

case "$mode" in
    norun)
        report "not running or harvesting"
        cexit "script complete" $E_SUCCESS
        ;;
    *)
        script_exit=255                 # dummy value
        run_xeona "$xsen" "$xlog"
        case "$?" in
            0)
                harvest_data "$xsen" "$slog"
                script_exit=$E_SUCCESS
                ;;
            1)
                report "leta"          "$scenleta"
                report "scenario name" "$scenario_name"
                script_exit=$E_XEONA_FAIL
                ;;
        esac
        test "$show" == "logfile" && report "study name" "$study_name"     # information captured earlier
        test "$show" == "logfile" && show_slog "$slog"
        cexit "script complete" $script_exit
        ;;
esac

# ---------------------------------
#  housekeeping
# ---------------------------------

echo "$SCRIPT: coding error: should not be here"
echo "$SCRIPT: temporary exit status file will probably persist: $RETLOG"

#  $Id: xsenario.sh 9114 2012-02-21 14:33:27Z robbie $
#  end of file

