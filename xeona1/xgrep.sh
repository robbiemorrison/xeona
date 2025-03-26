#! /bin/bash

#  file-purpose     : custom 'grep' searches of 'xeona' codebase / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Fri 25-Nov-2011 12:28 UTC
#  file-status      : ongoing
#  file-keywords    : xeona

#  $Revision: 8152 $
#  $Date: 2011-11-25 15:23:09 +0100 (Fri, 25 Nov 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/xgrep.sh $

# ---------------------------------
#  user modifiable
# ---------------------------------

genopts=""
genopts=" --color"                      # general 'grep' options, note leading space
genopts=" --color --line-number"        # general 'grep' options, note leading space

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")                 # for identifying this script

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2
E_NO_FILES=10

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
    echo
    cat << EOM
           usage: $SCRIPT [opts] <stub> <pattern>   run 'grep' on 'pattern' using 'stub' files
                  $SCRIPT [opts]   ''   <pattern>   run 'grep' on 'pattern' using entire codebase
                  $SCRIPT --help                    display this message and exit (takes precedence)
 special options:  -r               respect case, nonstandard for 'grep' (also --respect-case)
                   -A=n             shortcut for --after-context=n
                   -B=n             shortcut for --before-context=n
                   -C=n             shortcut for --context=n
    grep options: do not aggregate (for instance -xy)
                   -w               word search
                   -x               whole line search
                   -o               print only the match
     purpose: custom 'grep' searches of 'xeona' codebase
        note: uses 'find' to locate files
              context n in [0,999]
     example: $ $SCRIPT -w -A=2 cost convention
EOM
    echo
}

# ---------------------------------
#  grepopts()
# ---------------------------------

# peel off 'grep' options, note that '-r' in nonstandard

gopts=""
patcase=" --ignore-case"

function grepopts
{
    local gopt="$1"

#      echo "$FUNCNAME: testing '$gopt'"

    case "$gopt" in
        -r|--respect-case)
            patcase=""                  # remove the '--ignore-case'
            return 0
            ;;
        -[A-C]=[0-9]|-[A-C]=[1-9][0-9]|-[A-C]=[1-9][0-9][0-9])   # 0 thru 999
            printf -v gopts "%s %s" "$gopts" "${gopt/=/ }"
            return 0
            ;;
        -*|--*)
            printf -v gopts "%s %s" "$gopts" "$gopt"
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

# ---------------------------------
#  command-line processing
# ---------------------------------

cline="$0 $@"
cline="$SCRIPT $@"

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        display_help
        exit $E_SUCCESS
        ;;
esac

while grepopts "$1"
do
    shift
done

stub="$1"
shift
patt="$@"

test -z "$patt" && exit $E_USAGE

# ---------------------------------
#  code
# ---------------------------------

glob=$(find -P . -maxdepth 2 \( -iname "*$stub*.h" -or -iname "*$stub*.cc" -and -not -iname "*$stub*.ut[0-9].cc" \) -type f -printf "%P\n")
glob=$(sort <<< "$glob")
filz=$(wc --lines <<< "$glob")
test -z "$glob" && filz=0
files=$(echo -n $glob)
call="grep$genopts$patcase$gopts \"$patt\""

case "$filz" in
    0)
        echo "  cline   : $cline"
        echo "  stub    : '$stub'"
        echo "  files   : $filz"
        exit $E_NO_FILES
        ;;
    [1-9])
        echo
        ;;
    *)
        echo
        echo "  stub    : '$stub'"
        echo "  files   : $filz"
        echo "  glob    : $files"
        echo "  ---"
        echo
        ;;
esac

eval "$call $files"
echo
echo "  ---"
echo "  stub    : '$stub'"
echo "  files   : $filz"
test $filz -lt 9 && echo "  glob    : $files"
echo "  pattern : \"$patt\""
echo "  grep    : $call"
echo "  cline   : $cline"
echo

# ---------------------------------
#  housekeeping
# ---------------------------------

exit

#  $Id: xgrep.sh 8152 2011-11-25 14:23:09Z robbie $
#  end of file

