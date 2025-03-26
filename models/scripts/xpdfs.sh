#! /bin/bash

#  file-purpose     : regenerate all general and specific timeseries PDFs for PhD write-up
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Fri 16-Mar-2012 19:48 UTC
#  file-status      : work-in-progress
#  file-keywords    : xeona

#  $Revision: 9200 $
#  $Date: 2012-03-23 11:30:52 +0100 (Fri, 23 Mar 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/xpdfs.sh $

#  'runstudy.sh' is a good source of code

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")                 # name of this script

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2
E_USER_QUIT=10

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
    echo
    cat << EOM
               usage: $SCRIPT  --all                          run all options
                      $SCRIPT  --normal                       run selected option, deletes existing SVG files
                      $SCRIPT  ----special|--bidtaf           run selected option, retains existing SVG files
                      $SCRIPT  --two                          run 'special' then 'bidtaf'
                      $SCRIPT  --help                         display this message and exit (takes priority)
             options:  --normal    -n      run normal plots
                       --special   -s      run specialist plots
                       --bidtaf    -b      run bidset plots
             purpose: regenerate all report building PDFs
               notes: all PDFs are regenerated in all cases (could be considered a design flaw)
                      add 'special' and 'bidtaf' calls directly to this script
       file deletion: select "yes" or fall thru to the utility scripts
EOM
    echo
}

# ---------------------------------
#  process command-line 1
# ---------------------------------

cline="$0 $@"                           # grab command-line with given path
cline="$SCRIPT $@"                      # grab command-line without given path

mode=""

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        shift
        display_help "$1"
        exit $E_SUCCESS
        ;;
    --normal|-n)
        shift
        mode="normal"
        ;;
    --special|-s)
        shift
        mode="special"
        ;;
    --bidtaf|-b)
        shift
        mode="bidtaf"
        ;;
    --two|-t|-2)                        # 'special' + 'bidtaf'
        shift
        mode="two"
        ;;
    --all|-a)
        shift
        mode="all"
        ;;
    *)
        echo "$SCRIPT: incorrect usage (try --help)"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  user_confirm()
# ---------------------------------

function user_confirm
{
    local extra="$1"                         # dedicated message

#   test "$mode" == "dryrun" && return 0     # CAUTION: must be zero

    echo
    local prompt="    hit 'y' to $extra: "
    read -n 1 -p "$prompt" response          # "-n 1" is read one character and return
    echo
    case "$response" in
        y|Y)
            return 0                         # confirmed
            ;;
        *)
            return 1
            ;;
    esac
}

# ---------------------------------
#  delete_ext()
# ---------------------------------

function delete_ext
{
    local ext="$1"

    local label=${ext^^}
    user_confirm "delete all current $label files" || return 1

    local call="rm --force *.$ext"
    report
    report "rm call" "$call"
    eval "$call" && return 0

    return 1
}

# ---------------------------------
#  chmod_ext()
# ---------------------------------

function chmod_ext
{
    local ext="$1"

    local call="chmod u+w *.$ext"
    report
    report "chmod call" "$call"
    eval "$call" && return 0

    return 1
}

# ---------------------------------
#  port()
# ---------------------------------

function port
{
    report
    report "$FUNCNAME()" "entering function"
    report                              # to separate the Tcl/Tk reporting

    local call="xport.R --active --eXport"
    report "export call" "$call"
    report
    eval "$call" || return 1

    report "$FUNCNAME()" "success"
    return 0
}

# ---------------------------------
#  analyze()
# ---------------------------------

function analyze
{
    local opt="$1"

    report
    report "$FUNCNAME()" "entering function"
    report                              # to separate the Tcl/Tk reporting

    local call="xanalyze.R $opt"
    report "xanalyse call" "$call"
    report                              # to separate the Tcl/Tk reporting
    eval "$call" || return 1

    report "$FUNCNAME()" "success"
    return 0
}

# ---------------------------------
#  bidtaf()
# ---------------------------------

function bidtaf
{

#  plot bidset and tariffset curves using default scenario '+'      $ xbidtaf.R -1kr
#  plot bidset and tariffset curves using nominated scenario 'e'    $ xbidtaf.R -1krs e
#  plot timeseries using default scenario '+'                       $ xbidtaf.R -2kr
#  ditto but examine just the opening phase                         $ xbidtaf.R -2krc :1000
#  plot timeseries using nominated scenario 'e'                     $ xbidtaf.R -2krs e
#  plot everything                                                  $ xbidtaf.R -3kr

    local opt="$1"

    report
    report "$FUNCNAME()" "entering function"
    report                              # to separate the Tcl/Tk reporting

    local call="xbidtaf.R $opt"
    report "xbidtaf call" "$call"
    report                              # to separate the Tcl/Tk reporting
    eval "$call" || return 1

    report "$FUNCNAME()" "success"
    return 0
}

# ---------------------------------
#  pdfall()
# ---------------------------------

function pdfall
{
    report
    report "$FUNCNAME()" "entering function"
    report                              # to separate the Tcl/Tk reporting

    local call="xport.R --pdf"
    report "xport call" "$call"
    report                              # to separate the Tcl/Tk reporting
    eval "$call" || return 1

    report "$FUNCNAME()" "success"
    return 0
}

# ---------------------------------
#  active code
# ---------------------------------

# preamble

report
report "$SCRIPT" "commercing script"

# delete or make writable

case "$mode" in
    normal|all)
        delete_ext "svg"
        delete_ext "pdf"
        ;;
    special|bidtaf|two)
        chmod_ext "svg"
        chmod_ext "pdf"
        ;;
esac

# calls

case "$mode" in
    normal|all)
        port
        ;;
esac

case "$mode" in
    special|two|all)
        analyze "--doubleCcgt --eXport --cut :"
        ;;
esac

case "$mode" in
    bidtaf|two|all)
        bidtaf "--role2 --report --eXport --cut :"
        ;;
esac

case "$mode" in
    normal|special|bidtaf|two|all)
        pdfall                          # CAUTION: must be last
        ;;
esac

# completion reporting

elapsed=$SECONDS
test $(which hms) && elapsed=$(hms $elapsed)  # 'hms' is a my (user-local) utility

report "$SCRIPT"                  "complete"
report "elapsed (hms or seconds)" "$elapsed"
report

exit $E_SUCCESS

#  $Id: xpdfs.sh 9200 2012-03-23 10:30:52Z robbie $
#  end of file

