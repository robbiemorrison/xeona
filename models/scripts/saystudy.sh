
#  file-purpose     : report a study -- a suite of run 'xeona' scenarios / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Wed 16-Nov-2011 10:06 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9165 $
#  $Date: 2012-03-06 16:11:02 +0100 (Tue, 06 Mar 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/saystudy.sh $

#  TODO: the repeat reporting code could be cleaner

# ---------------------------------
#  user modifiable
# ---------------------------------

# formatting

TAB=50                                  # field identifier space allocation

digits=3                                # number of digits to report
digits=4                                # number of digits to report

trunc=2                                 # increasingly shortens entity identifier, see function 'record_target' for explanation
trunc=1                                 # increasingly shortens entity identifier, see function 'record_target' for explanation

test "$trunc" -eq 2 && let "TAB -= 5"

# auxiliary script

xsenario="xsenario.sh"

# target

fqf="program.post-processing.report-list"

# file extensions

xem=".xem"
log=".log"

# file pattern

pix="trial-"
guard=""

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")                 # name of this script

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2
E_GLOB_FAIL=11
E_XGRAB_FAIL=12
E_NO_TARGETS=13

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
#  cexit() - controlled exit
# ---------------------------------

function cexit
{
    local msg="$1"                      # exit message
    local xval=$2                       # exit value

    local elapsed=$SECONDS
    test $(which hms) && elapsed=$(hms $elapsed)  # 'hms' is a my (user-local) utility

    test -n "$msg" && report "$FUNCNAME message" "$msg"
    test -n "$msg" && report
    report "command"                  "$cline"
    report "elapsed (hms or seconds)" "$elapsed"
    report "script exit"              "$xval"
    report

    exit $xval
}

# ---------------------------------
#  display_help() - display help
# ---------------------------------

function display_help
{
    echo
    cat << EOM
               usage: $SCRIPT [--csv|--org][--extended] [--file <file>]  000            report study '000'
                      $SCRIPT [--csv|--org]             [--file <file>]  000 a [b ..]   report scenarios '000.a' and so on (implies extended)
                      $SCRIPT --help                                       display this message and exit (takes priority)
             options:  --csv                      display as comma-separated variables (cut-and-paste into spreadsheet)
                       --org                      display as pipe-separated variables (cut-and-paste into emacs org-mode)
                       --extended                 print "min mean max" values, else print "mean" only
                       --file                     source report list from 'file'
             purpose: report on a suite of run 'xeona' scenarios
                note: '+' is the special reference energy system (RES) scenario
                      the extended reporting wraps after 4 scenarios
                      file entries beginning '#' or '>' are ignored
           hardcodes: target = $fqf
                      digits = $digits
                      trunc  = $trunc
  naming conventions: scenario model      = ${pix}000.a${guard}${xem}
                      study-wide log file = ${pix}000${guard}${log}
               notes: a study is a collection of scenarios relative to the given RES reference energy system
                      this script can be slow
EOM
    echo
}

# ---------------------------------
#  slurp()
# ---------------------------------

function slurp
{
    local srcfile="$1"

    if [ ! -f "$srcfile" ]
    then
        echo "$SCRIPT: report list file not found: $srcfile"
        return 1
    fi
    targets=$(grep --invert-match "^[[:blank:]]*[#>]" "$srcfile")  # selective slurp
    targets=$(echo -n $targets)                                    # stringify
    if [ -z "$targets" ]
    then
        echo "$SCRIPT: report list file is empty: $srcfile"
        return 2
    fi
    echo "$targets"
    return 0
}

# ---------------------------------
#  process command-line
# ---------------------------------

cline="$0 $@"                           # grab command-line with given path
cline="$SCRIPT $@"                      # grab command-line without given path

source="res"                            # default is read from RES
xrep="normal"                           # extended reporting mode
format="normal"
num="(not set)"

while true
do
    case "$1" in
        --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
            display_help
            exit $E_SUCCESS
            ;;
        --csv|-c)
            shift
            format="csv"
            ;;
        --org|-o)
            shift
            format="org"
            ;;
        --extended|-x)
            shift
            xrep="extended"
            ;;
        --file|-f)
            source="file"
            shift
            srcname="$1"
            shift
            slurp "$srcname" || exit
            ;;
        --)                             # for convenience
            shift
            ;;
        *)
            break                       # CAUTION: no 'shift'
            ;;
    esac
done

case "$1" in
    [0-9][0-9][0-9])
        num="$1"
        shift
        ;;
    *)
        echo "$SCRIPT: invalid usage (check the 000) (try --help)"
        exit $E_USAGE
        ;;
esac

senletas=""

while [ -n "$1" ]
do
    case "$1" in
        [+a-z])
            printf -v senletas "%s%s\n" "$senletas" "$1"
            shift
            xrep="extended"
            ;;
        "")
            :                               # optional argument, fall thru is correct
            ;;
        *)
            echo "$SCRIPT: invalid usage (check the scenario letas) (try --help)"
            exit $E_USAGE
            ;;
    esac
done

senletas=$(sort --unique <<< "$senletas")    # leaves leading newline
senletas=$(echo -n $senletas)                # convert newlines to spaces
senletas=${senletas// /}                     # squish spaces

# ---------------------------------
#  glob_files()
# ---------------------------------

function glob_files
{
    local pocnum="$1"                   # should be "000", tested under command-line processing
    local senletas="$2"                 # should be default "+a-z" or custom list

    # find and sort files
    # CAUTION: '+' sorts before 'a' which is what we want
    local glob=$(find -P . -maxdepth 1 -name "$pix$pocnum.[$senletas]$guard$xem" -type f -printf "%P\n" | sort)
    local gres=$(find -P . -maxdepth 1 -name "$pix$pocnum.[+]$guard$xem"         -type f -printf "%P\n" | sort)

    test -z "$glob" && cexit "no xem files globbed (check argument $pocnum)" $E_GLOB_FAIL

    # update global variables
    res="$gres"
    files=$(echo -n $glob)              # stringify

    # recover 'letas'
    local letaz=""
    for file in $files
    do
        local leta=$(sed 's/.*\.\([+a-z]\)\..*/\1/' <<< "$file")
        printf -v letaz "%s %s" "$letaz" "$leta"
    done
    letas=${letaz# }                    # strip leading space
}

# ---------------------------------
#  print_header()
# ---------------------------------

function print_header
{
    local header=""
    local values=""

    case "$xrep" in
        normal)   values="mean"          ;;
        extended) values="min mean max"  ;;
    esac

    # prepare header
    case "$format" in
        csv) printf -v header " %-*s" $TAB "\"key : $values\""  ;;
        org) printf -v header " %-*s" $TAB "Fields : $values"   ;;
        *)   printf -v header " %-*s" $TAB "key : $values"      ;;
    esac

    local tab=0
    let "tab = digits + 5"
    for leta in $letas
    do
        case "$format" in
            org) printf -v leta "\\code{%s}" "$leta"  ;;    # wrap in \code{}
            csv) leta="\"$leta\""                     ;;
        esac

        case "$xrep" in
            normal)
                printf -v header "%s  %-*s" "$header" $tab "$leta"
                ;;
            extended)
                printf -v header "%s  %-*s" "$header" $tab "$leta"
                printf -v header "%s  %-*s" "$header" $tab ""
                printf -v header "%s  %-*s" "$header" $tab ""
                ;;
        esac
    done

    # comma-ize
    case "$format" in
        csv)
            header=$(sed 's/^[[:blank:]]*//'   <<< "$header")
            header=$(sed 's/[[:blank:]]\+/ /g' <<< "$header")
            header=$(sed 's/" "/","/g'         <<< "$header")
#           header=$(sed 's/"//g'              <<< "$header")
            ;;
        *)
            printf -v header "%s\n" "$header"
            ;;
    esac

    # pipe-ize
    case "$format" in
        org)
            header=$(sed 's/:[[:blank:]]*mean//'  <<< "$header")
            header=$(sed 's/[[:blank:]]\+/|/g'    <<< "$header")
            printf -v header "%s\n%s" "$header" "|-|"
            ;;
    esac

    # print line
    echo -n "$header"
    echo
}

# ---------------------------------
#  record_target()
# ---------------------------------

function record_target
{
    local target="$1"

    # short-circuit on '+'
    case "$format" in
        csv) test "$target" == "+" &&         return 0  ;;
        org) test "$target" == "+" &&         return 0  ;;
        *)   test "$target" == "+" && echo && return 0  ;;
    esac

    # identifier shortening or not
    case "$trunc" in                            # set in preamble
        0)
            local tag=$target                   # leave intact
            ;;
        1)
            local tag=${target#entity.}         # strip leading "entity."
            ;;
        2)
            local tag=${target#entity.}         # strip leading "entity."
            tag=${tag#asop-}                    # strip leading "asop-" etc
            tag=${tag#cm-}
            tag=${tag#cx-}
            tag=${tag#gate-}
            tag=${tag#junc-}
            tag=${tag#node-}
            tag=${tag#teas-}
            ;;
        *)
            echo "$SCRIPT: settings error: 'trunc' not in {0,1,2}: $trunc"
            ;;
    esac

    # code-ize for org-mode
    case "$format" in
        org) printf -v tag "\\code{%s}" "$tag"  ;;     # wrap in \code{}
    esac

    # harvest data
    local data=""
    case "$format" in
        csv) printf -v data "%s %-*s" "$data" "$TAB" "\"$tag\""  ;;
        org) printf -v data "%s %-*s" "$data" "$TAB" "$tag"      ;;
        *)   printf -v data "%s %-*s" "$data" "$TAB" "$tag"      ;;
    esac
    local tab=0
    let "tab = digits + 6"
    for file in $files
    do
        local val=$(xgrab  --quiet "$file" "$target")
        local min=$(xstats --quiet --digits "$digits" --min  $val)     # CAUTION: must omit soft-quotes on 'val'
        local ave=$(xstats --quiet --digits "$digits" --mean $val)
        local max=$(xstats --quiet --digits "$digits" --max  $val)
        case "$xrep" in
            normal)
                printf -v data "%s %-*s" "$data" $tab $ave         # 'ave' is a string, can be empty, hence the "-9"
                ;;
            extended)
                printf -v data "%s %-*s" "$data" $tab $min
                printf -v data "%s %-*s" "$data" $tab $ave
                printf -v data "%s %-*s" "$data" $tab $max
                ;;
        esac
    done

    # comma-ize
    case "$format" in
        csv|org)
            data=$(sed 's/^[[:blank:]]*//' <<< "$data")
            data=$(sed 's/[[:blank:]]/,/g' <<< "$data")
            data=$(sed 's/,,\+/,,/g'       <<< "$data")
            data=$(sed 's/,,/,/'           <<< "$data")
            ;;
    esac

    # pipe-ize
    case "$format" in
        org)
            data=$(sed 's/,/|/g'           <<< "$data")
            data="|${data}|"
            ;;
    esac

    # print line
    printf -v data "%s\n" "$data"
    echo -n "$data"
}

# ---------------------------------
#  print_data()
# ---------------------------------

function print_data
{
    # create and print data line-by-line
    for target in $targets
    do
        record_target "$target"
    done

    case "$format" in
        org) printf "|-|\n"  ;;
    esac
}

# ---------------------------------
#  active code 1 : preparation
# ---------------------------------

report
report "command" "$cline"
case "$xrep" in
    normal)   report "reporting" "mean"          ;;
    extended) report "reporting" "min mean max"  ;;
esac

res=""                                  # reset, contains base xemfile
files=""                                # reset, contains all xemfiles

case "$senletas" in
    "") glob_files "$num" "+a-z"       ;;    # default, noting "[]" not required
    *)  glob_files "$num" "$senletas"  ;;
esac

study_name=$(xgrab --quiet --unquote "$res" "program.study-description.study-name") || cexit "'xgrab' call failed with exit code $?" $E_XGRAB_FAIL

report "study name" "$study_name"
report "letas"      "$letas"
report "files"      "$files"
report "RES"        "$res"

case "$source" in
    res)  targets=$(xgrab --quiet --unquote "$res" "$fqf") || cexit "'xgrab' call failed with exit code $?" $E_XGRAB_FAIL  ;;
    file) :  ;;                         # slurped earlier
esac

test -n "$targets" || cexit "targets list empty" $E_NO_TARGETS

# ---------------------------------
#  active code 2 : checking
# ---------------------------------

status=0                                # +1 to log, +2 to report

# checks -- while noting 'res' must exist

run=$(xgrab --wasrun "$res")            # 0 if run, else 1
case "$run" in
    "not run")                          # RES was not run
        let "status = -1"
        ;;
    "was run")                          # RES was run
        # check 'xsenario.sh'
        script=$(which "$xsenario")
        case "$script" in
            "") :                  ;;
            *)  let "status += 1"  ;;
        esac
        # "xeona --mode 6" special case
        mode=$(xgrab --quiet "$res" "program.last-run.run-kind")
        case "$mode" in
            "\"first simulation step - deep overwrite (6)\"") :                  ;;
            *)                                                let "status += 2"  ;;
        esac
        ;;
    *)
        echo "$SCRIPT: coding error"
        echo "$run"
        ;;
esac

# ---------------------------------
#  active code 3 : reporting
# ---------------------------------

exitcode=$E_FAILURE

case "$status" in
    -1)
        report "log and report omitted" "suite evidently not run"
        report
        exitcode=$E_FAILURE
        ;;
    0)
        report "log and report omitted" "check $xsenario"
        report
        exitcode=$E_FAILURE
        ;;
    1)
        report "report omitted" "cannot generate a sensible report when \"xeona --mode 6\" is used"
        $xsenario --show-log "$num"
        exitcode=$E_SUCCESS
        ;;
    2)
        report "log omitted" "'$xsenario' not found"
        report
        print_header
        print_data
        report
        exitcode=$E_SUCCESS
        ;;
    3)                                  # normal action
        $xsenario --show-log "$num"
        print_header
        print_data
        report
        # repeat iff subset first reported
        if [ -n "$senletas" ]
        then
            xrep="normal"
            glob_files "$num" "+a-z"
            print_header
            print_data
            report
        fi
        exitcode=$E_SUCCESS
        ;;
    *)
        echo "$SCRIPT: coding error, status: $status"
        exit 255
        ;;
esac

elapsed=$SECONDS
test $(which hms) && elapsed=$(hms $elapsed)   # 'hms' is a my (user-local) utility
report "elapsed (hms or seconds)" "$elapsed"
case "$exitcode" in
    $E_SUCCESS) report "exit code" "$exitcode (success)"         ;;
    *)          report "exit code" "$exitcode (problems arose)"  ;;
esac
report

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $exitcode

#  $Id: saystudy.sh 9165 2012-03-06 15:11:02Z robbie $
#  end of file

