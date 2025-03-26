#! /bin/bash

#  file-purpose     : open graphviz diagrams in selected image viewer / script (not under svn)
#  file-status      : working

# ---------------------------------
#  preamble
# ---------------------------------

LIST="03 06 10 11 15 16"
LIST="16"

EXTS="viz.png"

GTOPTS="--fullscreen"

SCRIPT=$(basename "$0")

# ---------------------------------
#  call_gthumb()
# ---------------------------------

function call_gthumb
{
    echo "$SCRIPT: requested files: $LIST"
    # bunch files
    local nums=$(sed 's/ /,/g' <<< "$LIST")
    local pngs="submodel.{$nums}.$EXTS"
    call="gthumb $GTOPTS $pngs &"

    echo "  $call"
    eval "$call"
}

# ---------------------------------
#  call_display()
# ---------------------------------

function call_display
{
    echo "$SCRIPT: requested files: $LIST"

    # loop files
    local skips=""
    for num in $LIST
    do
        local file="submodel.$num.$EXTS"
        test -f "$file" ||
        {
            skips="$skips $num"
            continue
        }
        call="display $file &"
        echo "  $call"
        eval "$call"
    done
    test -z "$skips" || echo "$SCRIPT: WARNING: skipped files:$skips"

    if which addhist 1>/dev/null
    then
        addhist "killall display"
    else
        echo "  \$ killall display"
    fi
}

# ---------------------------------
#  active code
# ---------------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        echo
        echo "      usage: $SCRIPT [-d]           use display"
        echo "             $SCRIPT  -d  <list>    use display on <list>"
        echo "             $SCRIPT  -g            use gthumb"
        echo "             $SCRIPT  -g  <list>    use gthumb on <list>"
        echo "             $SCRIPT  -h            show this message and exit"
        echo "    purpose: open graphviz diagrams in selected image viewer"
        echo "  submodels: $LIST"
        echo "  extension: $EXTS"
        echo
        ;;
    "")                                 # the default
        call_display
        ;;
    --display|-d)
        shift
        test "$*" && LIST="$*"
        call_display
        ;;
    --gthumb|-g)
        shift
        call_gthumb
        ;;
    *)
        echo "$SCRIPT: unsupported option (try --help for usage): $1"
        exit 2
        ;;
esac

# end of file

