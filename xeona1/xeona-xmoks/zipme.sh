
#  NOTES
#
#  --archive-comment : entire line dot or ^D to end

#  important        : not under version control
#  file-purpose     : pack visual file for circulation using 'zip' utility
#  file-status      : working

SCRIPT=$(basename "$0")

# ---------------------------------
#  abort()
# ---------------------------------

function abort
{
    local msg="$1"
    echo "  FATAL: $msg"
    exit 1
}

# ---------------------------------
#  make_readme()
# ---------------------------------

function make_readme
{
    local readme="readme.txt"
    local sdir="$1"
    local rfile="$sdir/$readme"

    local contact="Robbie Morrison <robbie@actrix.co.nz>"
    local username=$(id --user --name)
    local timestamp=$(date "+%Z %z %A %d-%b-%Y %H:%M")
    local host=$(hostname --fqdn)
    local year=$(date "+%Y")

    local msg="
  Archive details

    created : $timestamp
       host : $host
       user : $username
    contact : $contact
    license : unless otherwise stated, these files are

  Copyright: (c) $year Robbie Morrison. Some rights reserved.
  License: this material is licensed under a Creative Commons BY-NC-SA.
  http://www.creativecommons.org/licenses/by-nc-sa/3.0/
" # final double-quote

    echo "$msg" > $rfile
    echo "  contents of '$rfile'"
    cat $rfile

    return 0
}

# ---------------------------------
#  zippngs
# ---------------------------------

function zippngs
{
    # hard codes
    local tag="submodel"
    local ext="png"

    # help message
    case "$1" in
        --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
            echo
            echo "   usage: $SCRIPT <00>      archive files matching: $tag.00.*.$ext"
            echo " purpose: pack visual file for circulation using 'zip' utility"
            echo "    note: zipfile comment sought, simply enter a \".\" to sidestep"
            echo
            return 0
            ;;
    esac

    # check
    local count=$#
    test $count -gt 1 && abort "too many arguments: $count"

    # recover argument
    local number="$1"

    # check 'number'
    case "$number" in
        "")         abort "number argument required"                      ;;
        00)         abort "ignoring number argument $number"              ;;
        [0-9][0-9]) :                                                     ;;  # do nothing is correct
        *)          abort "zero padded number required but got: $number"  ;;
    esac

    # preamble
    local stub="$tag.$number"
    local sdir="$stub"
    local zfile="$stub.zip"

    # create and copy
    test -f "$zfile" && abort "zip file exists: $zfile"
    rmdir "$sdir" 2>/dev/null
    test -d "$sdir" && abort "subdirectory exits (please delete): $sdir"
    mkdir "$sdir" || abort "mkdir call failure: $?"
    cp --preserve=mode,ownership,timestamps,link --no-dereference $stub.*.$ext "$sdir" || abort "cp call failure: $?"

    # add readme file
    make_readme "$sdir"

    # readonly the subdirectory
    chmod 0444 $sdir/* || abort "chmod call failure: $?"

    # report
    echo "  listing for '$sdir/*'"
    ls -lF $sdir/*

    # archive
    local callz="zip $zfile $sdir/*"
    local callz="zip --archive-comment $zfile $sdir/*" # seeks a comment, enter "." to sidestep
    echo "  zip call = $callz"
    eval "$callz" || abort "$callz call failure: $?"

    # mopup
    chmod 0440 "$zfile" || abort "chmod call failure: $?"
    rm --force --recursive "$sdir" || abort "subdirectory '$sdir' removal failure: $?"

    # report
    echo "  integrity check for '$zfile'"
    zip -Tv "$zfile" || abort "zip -Tv failure: $?"

    # housekeeping
    echo "  script complete"
    return 0
}

# ---------------------------------
#  primary function call
# ---------------------------------

zippngs "$@"

# end of file

