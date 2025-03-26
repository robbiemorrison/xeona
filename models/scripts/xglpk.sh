
#  file-purpose     : mine data from individual GLPK problem and solution files / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Fri 06-May-2011 09:45 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 8049 $
#  $Date: 2011-11-18 00:46:29 +0100 (Fri, 18 Nov 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/xglpk.sh $

#  NOTES
#
#    script requires bash 4
#
#    the term "pin" is often used instead of "pid" to avoid name collisions with the system

# ---------------------------------
#  settings
# ---------------------------------

EXTS="glpk prob sens soln"

PINPAD=6                                # PID padding level
CALPAD=4                                # call padding level

XEONA="../xeona1/xeona.mach"

XGLPSOL="xglpsol"                       # support script providing an interface to 'glpsol' proper

# note also 'xgopts' is set later

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")

E_SUCCESS=0
E_FAIL=1
E_USAGE=2
E_BAD_PIN=64
E_BAD_CAL=65

# ---------------------------------
#  report()
# ---------------------------------

# standardized output

function report
{
    local format="-18"                  # - is left-align, 00 is minimum width
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
#  display_help()
# ---------------------------------

function display_help
{
    echo
    cat <<EOM
       usage: $SCRIPT  000       show files with PID 000
              $SCRIPT  000 00    show files with PID 000 and call count 00
              $SCRIPT  000-00    as above but with the cut-and-paste hyphen still present
              $SCRIPT  000 xx    show files with PID 000 and objective name matching xx
              $SCRIPT  --yeek    attempt to confirm the 'xeona' yeek value (possibly 1 and 4)
              $SCRIPT  --help    display this message and exit
              $SCRIPT  --Help    display '$XGLPSOL' help and exit
     purpose: mine data from individual GLPK problem and solution files
       notes: leading zeros are optional on the PID and call count
              the GLPK files were probably created earlier with a 'xeona' yeek option
    examples: $ $SCRIPT 1234 nodal  # LMP pricing objectives
              $ $SCRIPT 1234 \-003  # escape hyphens
   hardcodes: file extensions : $EXTS
       emacs: S-<left-mouse> to reduce font size
EOM
echo
}

# ---------------------------------
#  display_help_xglpsol()
# ---------------------------------

function display_help_xglpsol
{
    if [ $(which "$XGLPSOL") ]
    then
        echo "$SCRIPT: showing $XGLPSOL help (else try --help)"
        $XGLPSOL --help
    else
        echo "$SCRIPT: support script not found: $XGLPSOL"
        echo
    fi
}

# ---------------------------------
#  show_yeek()
# ---------------------------------

function show_yeek
{
    local call="$XEONA --usage 2>/dev/null | grep \"GLPK\" | grep --invert-match \"abort\""
    eval "$call" || echo "$SCRIPT: no match from call: $call"
}

# ---------------------------------
#  command-line processing
# ---------------------------------

pin=""
two=""

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        display_help
        exit $E_SUCCESS
        ;;
    --HELP|--HEL|--HE|--H|-HELP|-HEL|-HE|--Help|--Hel|--He|--H|-Help|-Hel|-He|-H)
        display_help_xglpsol
        exit $E_SUCCESS
        ;;
    --yeek|-y)
        show_yeek
        exit
        ;;
    "")
        echo "$SCRIPT: incorrect usage (try --help): $*"
        exit $E_USAGE
        ;;
    *-*)                                # cute code!
        hold="$1"
        shift
        pin=${hold%-*}
        two=${hold#*-}
        ;;
    *)
        pin="$1"                        # input PID not yet confirmed
        shift
        two="$1"                        # the first positional argument is fine
        shift
        ;;
esac

# ---------------------------------
#  process_pid()
# ---------------------------------

pid=""                                  # zero-padded PID

function process_pid
{
    local pin="$1"
    while [ "${pin:0:1}" == "0" ]       # while at least one leading zero exists
    do
        pin=${pin#0}                    # strip the leading zero
    done
    printf -v pid "%0*d" $PINPAD "$pin" 2>/dev/null    # effectively confirms 'pin' is an integer
    test $? -ne 0 && fatal $E_BAD_PIN "ill-formed PID given" "$pin"
    report "padded PID" "$pid"
    return 0
}

# ---------------------------------
#  process_two()
# ---------------------------------

cal=""                                  # padded call count
obj=""                                  # objective function name pattern

function process_two
{
    local two="$1"
    test -z "$two" && return 1
    while [ "${two:0:1}" == "0" ]       # while at least one leading zero exists
    do
        two=${two#0}                    # strip the leading zero
    done
    printf -v cal "%0*d" $CALPAD "$two" 2>/dev/null    # effectively confirms 'two' is an integer
    if [ $? -eq 0 ]
    then
        obj=""                          # just to be sure
        report "padded call" "$cal"
    else
        cal=""                          # just to be sure
        obj="$two"
        report "objective match" "$obj"
    fi
    return 0
}

# ---------------------------------
#  glob_files()
# ---------------------------------

# pattern : 002619-0088-pro00030.dc-a.soln

files=""
count=0

function glob_files
{
    case "$cal" in
        "") local name="$pid-[0-9][0-9][0-9][0-9]-pro[0-9][0-9][0-9][0-9][0-9].dc-*.soln" ;;
        *)  local name="$pid-$cal-pro[0-9][0-9][0-9][0-9][0-9].dc-*.soln"                 ;;
    esac
    files=$(find -P . -maxdepth 1 -name "$name" -printf "%P\n" | sed 's/\.soln$//' | sort)
    count=$(wc --lines <<< "$files")
    test -z "$files" && count=0
    report "found files" "$count"
    test $count -eq 0 && return 1
    test $count -ne 0 && return 0
}

# ---------------------------------
#  mine_tag()
# ---------------------------------

#  Problem:    pro-00029.dc-s
#  Rows:       8
#  Columns:    4
#  Non-zeros:  11
#  Status:     OPTIMAL
#  Objective:  obj.tsolve-srfin = 13427.74444 (MINimum)

val=""
mat="0"

function mine_tag
{
    local tag="$1"
    local file="$2"
    val=$(grep "^$tag" "$file" | head --lines=1 | cut --delimiter=":" --fields=2 )
    val=$(echo $val)                    # cheap way to strip surplus space chars
    val=${val,,}                        # downcase (bash 4)
    case "$tag" in
        Problem)
            printf -v val "%-25s" "$val"
            ;;
        Objective)
            local objnam=$(cut --delimiter="=" --fields=1 <<< "$val")
            local objrhs=$(cut --delimiter="=" --fields=2 <<< "$val")
            local objnam=$(echo $objnam)
            local objrhs=$(echo $objrhs)
            local objval=$(cut --delimiter=" " --fields=1 <<< "$objrhs")
            local objdir=$(cut --delimiter=" " --fields=2 <<< "$objrhs")
            local objdir=$(sed -e 's/(//' -e 's/)//'      <<< "$objdir")
            local objdir=${objdir:0:3}  # truncate to 3 chars
            printf -v val "%-25s  %-22s %-3s" "$objnam" "$objval" "$objdir"
            # match switch
            if grep -q "$obj" <<< "$objnam"
            then
                mat="1"
            else
                mat="0"
            fi
            ;;
        Status)
            case "$val" in
                "integer optimal") val="MIP"     ;;
                "optimal")         val="LP"      ;;
                *)                 val="$val **" ;;
            esac
            ;;
        *)
            val="** $tag not found **"
            ;;
      esac
}

# ---------------------------------
#  mine_file()
# ---------------------------------

function mine_file
{
    local file="$1"
    local tags="Problem Objective Status"
    local buf=""
    for tag in $tags
    do
        mine_tag "$tag" "$file.soln"
        printf -v buf "$buf  %s" "$val"
    done
    test "$mat" == "1" && printf "  $file  :   $buf\n"
}

# ---------------------------------
#  single()
# ---------------------------------

xgopts="--"
xgopts="-w"
xgopts="-wr"
xgopts="-wrf"

function single
{
    test $count -ne 1 && return 1

    local stub="$1"                     # just one file guaranteed
    local file=""
    for ext in $EXTS
    do
        file="$stub.$ext"
        test -f "$file" || continue
        echo "  less $file"
    done

    test $(which "$XGLPSOL") || return 0

    file="$stub.glpk"
    test -f "$file" && echo
    test -f "$file" && echo "  $XGLPSOL $xgopts $file"

    return 0
}

# ---------------------------------
#  active code
# ---------------------------------

report
process_pid "$pin"
process_two "$two"
glob_files && report

for file in $files
do
    mine_file "$file"
done

report
single "$files" && report
report "elapsed (seconds)" "$SECONDS"

report

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $E_SUCCESS

#  $Id: xglpk.sh 8049 2011-11-17 23:46:29Z robbie $
#  end of file

