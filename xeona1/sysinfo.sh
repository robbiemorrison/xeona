#! /bin/bash

#  file-purpose     : informational script, fully passive / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Sat 20-Feb-2010 19:17 UTC
#  file-status      : ongoing
#  file-keywords    : xeona

#  $Revision: 5532 $
#  $Date: 2010-11-22 19:45:13 +0100 (Mon, 22 Nov 2010) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/sysinfo.sh $

# ---------------------------------
#  preamble
# ---------------------------------

LOGXTN="log"                            # logfile extension

SCRIPT=$(basename "$0")

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
    echo
    echo "     usage: $SCRIPT           report on build-time environment for 'xeona'"
    echo "            $SCRIPT --help    display this message and exit"
    echo "   purpose: assess the suitability of the current environment for building 'xeona'"
    echo "     notes: - the name '$SCRIPT' is a misnomer, this script is entirely passive"
    echo "            - the interpretation of issues is left to the user"
    echo "     exits: $E_SUCCESS  no issues flagged"
    echo "            $E_FAILURE  issues flagged"
    echo "            $E_USAGE  command-line usage problem"
    echo
}

# ---------------------------------
#  parse command-line
# ---------------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        display_help
        exit $E_SUCCESS
        ;;
    "")
        :                               # fall-thru is correct
        ;;
    *)
        echo "$SCRIPT: usage: unsupported arguments (try --help): $*"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  global variables
# ---------------------------------

ISSUES=0
EXITVAL=255
LOGFILE="/dev/null"                     # CAUTION: to be overwritten

# ---------------------------------
#  blank()    blank line
#  report()   echo with newline
#  reportn()  echo without newline
#  reporti()  initial part
#  reporte()  end part - okay
#  reportz()  end part - error
# ---------------------------------

function blank
{
    printf "\n"
} >> "$LOGFILE"

function report
{
    printf "%s\n"  "$1"
} >> "$LOGFILE"

function reportn
{
    printf "%s"    "$1"
} >> "$LOGFILE"

function reporti
{
    case $# in
        1) printf "  %-10s "        "$1"       ;;
        2) printf "  %-10s %-30s  " "$1" "$2"  ;;
    esac
} >> "$LOGFILE"

function reporte
{
    printf "      %s\n" "$1"
} >> "$LOGFILE"

COUNT=0                                 # reportz calls

function reportz
{
    let "COUNT++"
    printf "%2d    %s\n" "$COUNT" "$1"
} >> "$LOGFILE"

# ---------------------------------
#  make_logname()
# ---------------------------------

# create logfile name
# note 999999/3600/24 = 11.6 days

# CAUTION: remember to update the help message if the
# logic changes here

function make_logname
{
    sleep 1                             # to enable name uniqueness
    local base="${SCRIPT%.*}"           # strip, for instance, ".sh"
    local secs=$(date --utc "+%s")      # seconds since 1970
    secs=${secs:4}                      # trim first 4 chars -- thus no repeats within 11 days
    LOGFILE="$base.$secs.$LOGXTN"
}

# ---------------------------------
#  clear_logfile()
# ---------------------------------

# remove logfile of same name if present

function clear_logfile
{
    test -f "$LOGFILE" &&
    {
        rm --force $LOGFILE             # CAUTION: order of statements critical
        report "info: clear_logfile(): cleared logfile: $LOGFILE"     # for testing
    }
    touch "$LOGFILE" ||
    {
        echo "$SCRIPT: cannot write (check permissions) to logfile: $LOGFILE"
        LOGFILE="/dev/stdout"           # CAUTION: write to standard out instead
    }
}

# ---------------------------------
#  look_file()
# ---------------------------------

# try to locate a given file

function look_file
{
    local filename="$1"
    local comment="$2"

    local path=$(dirname  "$filename")
    local leaf=$(basename "$filename")

    local buffer="$leaf"
    test -n "$comment" && buffer="$leaf ($comment)"

    reporti "source" "$buffer"

    local call="find -P $path -maxdepth 1 -name \"$leaf\" -type f -print 2>/dev/null"
    capture=$(eval "$call")
    ret=$?
    case $ret in
        0)
            reporte "$capture"
            return 0
            ;;
        1)
            reportz "not found: $filename"
            let "ISSUES++"
            return 1
            ;;
        *)
            reportz "'find' returned: $ret"
            let "ISSUES++"
            return 1
            ;;
    esac
}

# ---------------------------------
#  look_util()
# ---------------------------------

# try to locate given utility

function look_util
{
    local utility="$1"

    reporti "utility" "$utility"

    local intro="locating $utility .."
    which "$utility" 1>/dev/null 2>/dev/null # 'which' call
    local ret=$?                             # capture return
    case $ret in
        0)
            reporte "$(which $utility)"
            return 0
            ;;
        1)
            reportz "utility not found: $utility"
            let "ISSUES++"
            return 1
            ;;
        2)
            reportz "problem: 'which' call problem"
            let "ISSUES++"
            return 1
            ;;
        *)
            report "PROBLEM: 'which' unknown return: $ret"
            let "ISSUES++"
            return 1
            ;;
    esac
}

# ---------------------------------
#  look_library()
# ---------------------------------

# try to locate library

function look_library
{
    local libname="$1"

    reporti "library" "$libname"

    local output
    local libpath=$(sed 's/:/ /g' <<< "$LIBRARY_PATH")      # envar
    local dirs="/usr/lib /usr/local/lib $libpath"
    dirs=${dirs% }

    for dir in $dirs
      do
      call="find -P $dir -maxdepth 1 -name \"$libname\" -type f -print"
      capture=$(eval "$call")
      ret=$?
      test $ret -eq 0 || reportn " warn"
      test -n "$capture" && printf -v output " %s" "$capture"
    done

    case $output in
        "")
            reportz "library not found: searched $dirs"
            let "ISSUES++"
            return 1
            ;;
        *)
            output=${output# }          # strip any leading space
            reporte "$output"
            return 0
            ;;
    esac
}

# ---------------------------------
#  test_boost()
# ---------------------------------

# currently hollow

function test_boost()
{
    return 0
}

# ---------------------------------
#  bash_version()
# ---------------------------------

# get and test bash version using 'BASH_VERSION'

function bash_version
{
    reporti "version" "bash"

    local bashmajor=$(cut -d . -f 1 <<< "$BASH_VERSION")
    local bashversion=$(cut -d . -f 1,2 <<< "$BASH_VERSION")

    case "$bashmajor" in
        0|1|2)
            reportz "incompatible version: got $bashversion, need 3.0 or better"
            let "ISSUES++"
            return 1
            ;;
        3|4|5)
            reporte "$bashversion"
            return 0
            ;;
        *)
            reportz "not a bash shell: $SHELL"
            let "ISSUES++"
            return 1
            ;;
    esac
}

# ---------------------------------
#  gcc_version()
# ---------------------------------

function gcc_version
{
    local gcc="g++"                                    # compiler call

    which "$gcc" 1>/dev/null 2>/dev/null || return 1   # 'which' call

    local gccversion=$($gcc --version | head --lines 1 | sed 's/^g++ (GCC) //')
    reporti "version" "$gcc"
    reporte "$gccversion"
    return 0
}

# ---------------------------------
#  initial_report()
# ---------------------------------

function initial_report
{
    local tag="system"

    report "  passive 'xeona' script $SCRIPT commencing"
    report "  the interpretation of issues is left to the user"
    report "  a Linux or UNIX system is presumed"
    report

    reporti "$tag" "timestamp"
    local timestamp=$(date -u '+%d-%b-%Y %Z')
    local timestamp=$(date -u '+%a %d-%b-%Y %H:%M %Z %z')
    reporte "$timestamp"

    reporti "$tag" "operating system / kernel"
    local opsys=$(uname --operating-system 2>/dev/null)
    case "$opsys" in
        GNU/Linux)                      # extended reporting
            local disnam=$(lsb_release --id      | gawk 'BEGIN { FS = ":\\W" } { print $2 }')  # \W is GNU awk
            local disrel=$(lsb_release --release | gawk 'BEGIN { FS = ":\\W" } { print $2 }')  # double \\ needed
            local kernam=$(uname --kernel-name)
            local kerrel=$(uname --kernel-release)
            local osinfo="$disnam $disrel / $kernam $kerrel"
            reporte "$osinfo"
            ;;
        *)
            reporte "$opsys"
            ;;
    esac

    reporti "$tag" "hardware type"
    local hardware=$(uname --machine 2>/dev/null)
    reporte "$hardware"

    reporti "$tag" "user"
    local passwdfile="/etc/passwd"
    local user=$(id --user --name)
    local fulluser=$(grep "^${user}:" "$passwdfile" 2>/dev/null | cut -d : -f 5  | cut -d , -f 1)
    test -n "$fulluser" && user="$user ($fulluser)"
    reporte "$user"

    reporti "$tag" "node"
    local hostname=$(uname --nodename)  # this call does not requires network connection
    local hostname=$(hostname --long)
    reporte "$hostname"

    reporti "$tag" "TERM environment variable"
    local term="$TERM"
    reporte "${term:-(not set)}"

    reporti "$tag" "working directory"
    local workdir=$(pwd -P)             # -P is without symlinks, pwd is a bash built-in
    reporte "$workdir"

    return 0
}

# ---------------------------------
#  final_report()
# ---------------------------------

function final_report
{
    local issue="issue"                 # express in singular
    local tag="summary"

    reporti "elapsed"
    local secs="$SECONDS"
    case $secs in
        0|1) report "1 second"      ;;
        *)   report "$secs seconds" ;;
    esac

    if [ -f "$LOGFILE" ]
    then
        reporti "log file" "$LOGFILE"
        report
    else
        reporti "log file" "not logged"
        report
    fi

    case $ISSUES in
        0)
            reporti "$tag" "no ${issue}s encountered"
            report
            EXITVAL=$E_SUCCESS
            return $E_SUCCESS
            ;;
        1)
            reporti "$tag" "one ${issue} encountered"
            report
            echo -en "\a"               # sound
            EXITVAL=$E_FAILURE
            return $E_FAILURE
            ;;
        *)
            reporti "$tag" "$ISSUES ${issue}s encountered"
            report
            echo -en "\a"               # sound
            EXITVAL=$E_FAILURE
            return $E_FAILURE
            ;;
    esac
}

# ---------------------------------
#  active code
# ---------------------------------

make_logname
clear_logfile

blank

initial_report

blank

bash_version
gcc_version

blank

look_file "$SCRIPT"           "me"
look_file "main.cc"           "main function file"
look_file "makefile"
look_file "../scripts/mach"   "build script"
look_file "LICENSE_GPLv3"     "GPL license"

blank

# could also add : R, display, graphviz

look_util "gawk" || look_util "awk"
look_util "beep"
look_util "g++"
look_util "make"
look_util "fastdep"
look_util "gdb"
look_util "valgrind"
look_util "svn"

blank

look_library "libglpk.a" || look_util "glpsol"    # fall-back call on fail

look_library "libboost_date_time.so*"
look_library "libboost_filesystem.so*"
look_library "libboost_program_options.so*"
look_library "libboost_regex.so*"
look_library "libboost_system.so*"
look_library "libgmp.so*"

test_boost

blank

final_report

blank

# report to console too

test -f "$LOGFILE" && cat "$LOGFILE"

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $EXITVAL

#  $Id: sysinfo.sh 5532 2010-11-22 18:45:13Z robbie $
#  end of file

