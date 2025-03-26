#! /bin/bash

#  file-purpose     : build Texinfo variants
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 18-Jan-2010 18:52 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 4770 $
#  $Date: 2010-07-16 11:34:53 +0200 (Fri, 16 Jul 2010) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/DOCS/manuals/texime.sh $

#  The 'TEXIMERC' config file is read early on and can
#  also overwrite all the reasonable preamble values.
#  This can include 'GRAPHICS.'  The config file is
#  automatically sourced if present, unless empty

# ---------------------------------
#  preamble
# ---------------------------------

STUB="userman"                          # hardcoded target stubname

TEXIMERC="./texime.rc"                  # optional configuration file: STUB="alternative"

GRAPHICS="
by-nc-sa-robbie_003
submodel.15.bread
submodel.15.viz
xeona-manex"                            # hardcoded list of HTML symlinked images

MARK="disable"                          # trailing filename tag for disable/enable PNG code

TEXI="texi"                             # texinfo extension, possibilities include "txi" "texi" "texinfo"
PDFVIEWER="evince"                      # can be "evince" or "acroread"
EDITOR="memacs"

SCRIPT=$(basename "$0")

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2
E_NO_FILE=64

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
    # pdftex information
    pdftexver=$(which pdftex 1>/dev/null && pdftex --version | head --lines=1)
    case "$pdftexver" in
        "") local pdftexinfo="utility not found"    ;;
        *)  local pdftexinfo="current = $pdftexver" ;;
    esac

    echo
    echo "      usage: $SCRIPT              run using config file stub if present, else hardcode stub"
    echo "             $SCRIPT  <stub>      run using cmdline stub"
    echo "             $SCRIPT  --clean     clean up tex-related ancillary files"
    echo "             $SCRIPT  --symhtml   add graphics symlinks for split html (once is sufficient)"
    echo "             $SCRIPT  --dvifix    prompted removal of xdvi configuration file"
    echo "             $SCRIPT  --emacs     display jumpy emacs text editor settings and exit"
    echo "             $SCRIPT  --help    display this message and exit"
    echo "    purpose: build texi file in all modes: info, dvi, pdf, html, txt"
    echo "       hint: if TeX chokes, enter \"x\", wait, and repeat for as long as is required"
    echo "   packages: non-default dependencies = texinfo texi2html texlive (or texlive-full)"
    echo "     pdftex: $pdftexinfo"
    echo "  hardcodes: default input file     : $STUB.$TEXI"
    echo "             config file (optional) : $TEXIMERC"
    echo "             pdf viewer             : $PDFVIEWER"
    echo
}

# ---------------------------------
#  display_emacs_settings()
# ---------------------------------

function display_emacs_settings
{
    cat <<EOF

  emacs text editor settings
  note the jumpy auto-save settings in order to better interact with '$SCRIPT'
  local variables:
    mode: texinfo
    make-backup-files: t
    fill-column: 55
    truncate-lines: nil
    tab-stop-list: (04 08)
    auto-save-default: t
    auto-save-interval: 1
    auto-save-timeout: 1
  end:

EOF

    return 0
}

# ---------------------------------
#  report()
# ---------------------------------

# structured reporting

function report
{
    case $# in
        0) echo         ;;
        1) echo "  $1"  ;;
    esac
}

# ---------------------------------
#  kill_xdvirc
# ---------------------------------

# used to kill '.xdvirc' settings when 'xdvi' plays up,
# logic needs testing

function kill_xdvirc()
{
    local rcfile="$HOME/.xdvirc"

    if [ -f "$rcfile" ]
    then
        cp --backup=simple $rcfile .
        echo -n "$SCRIPT: "
        rm --interactive $rcfile || echo "$SCRIPT: rm return fail: $?"
        touch $rcfile
        return
    else
        echo "$SCRIPT: file not found: $rcfile"
        return 1
    fi
}

# ---------------------------------
#  symlink_html()
# ---------------------------------

function symlink_html
{
    test -d "$STUB" ||
    {
        echo "$SCRIPT: WARN: HTML subdirectory not found: $STUB"
        return
    }
    cd "$STUB"
    echo "$SCRIPT: symlinking in '$STUB' subdirectory"
    for graphic in $GRAPHICS
    do
        test -f "../$graphic.png" ||    # file exists and is a regular file
        {
            echo "$SCRIPT: WARN: source image file not found: $graphic.png"
            continue
        }
        ln --symbolic --verbose "../$graphic.png" "$graphic.png"
    done
    cd "$OLDPWD"
}

# ---------------------------------
#  clean()
# ---------------------------------

extns="aux cp cps fls fn fns ky kys log pg pgs tmp toc tp tps vr vrs"     # from 'tex' 'texi2pdf' 'pdftex'

function clean
{
    local buffer
    buffer="rm = $STUB + "

    for ext in $extns
    do
        test -f "$STUB.$ext" || continue     # file exists and is a regular file
        printf -v buffer "$buffer %s" ".$ext"
        rm --force $STUB.$ext
    done

    report "$buffer"
    report
}

# ---------------------------------
#  load_stub
# ---------------------------------

function load_stub
{
    # load stub
    local arg="$1"
    stubfrom="hardcode"
    test -s "$TEXIMERC" &&              # file exists and has a size greater than zero
    {
        source "$TEXIMERC"
        stubfrom=$(basename "$TEXIMERC")
    }
    test -n "$arg" &&
    {
        STUB="$arg"
        stubfrom="cmdline"
    }

    # confirm stub
    local stub="$STUB.$TEXI"
    test -f "$stub" ||                  # file exists and is a regular file
    {
        echo "$SCRIPT: FATAL: invalid stub: $STUB"
        exit $E_INVALID_STUB
    }
}

# ---------------------------------
#  process command-line
# ---------------------------------

ERRORS=0                                # error count

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        display_help
        exit $E_SUCCESS
        ;;
    --emacs|--emac|--e|-e)
        shift
        display_emacs_settings
        exit $E_SUCCESS
        ;;
    --dvifix|--dvi|--d|-d)
        shift
        kill_xdvirc
        exit
        ;;
    --symhtml|--symhtm|--s|-s)
        shift
        load_stub "$1"
        symlink_html
        exit
        ;;
    --clean|--c|-c)
        shift
        load_stub "$1"
        echo
        clean
        exit
        ;;
    -*!--*)
        echo "$SCRIPT: unsupported option (try --help): $*"
        exit $E_USAGE
        ;;
    *)                                  # includes ""
        load_stub "$1"
        ;;
esac

# ---------------------------------
#  call()
# ---------------------------------

# invoke build call, but also report other information

function call
{
    local tell="$1"
    shift
    report "role = $tell"

    local view="$1"
    shift

    while [ $# -gt 0 ]
    do
        local call="$1"
        shift
        report "call = $call"
        eval "$call" 1>/dev/null
        ret=$?
        test $ret -eq 0 ||
        {
            report "WARN: call returns fail: $ret"
            let "ERRORS++"
        }
    done

    report "view = $view"
    report
}

# ---------------------------------
#  clean_html()
# ---------------------------------

function clean_html
{
    rm --force $STUB/*.html
    report "note = removed $STUB/*.html"
}

# ---------------------------------
#  check_saved()
# ---------------------------------

function check_saved
{
    local name="#$STUB.$TEXI#"
    test -f "$name" &&                  # file exists and is a regular file
    {
        report "WARN: texinfo file unequivocally needs saving: $STUB.$TEXI"
        let "ERRORS++"
        return 1
    }
    return 0
}

# ---------------------------------
#  make_beep()
# ---------------------------------

function make_beep
{
    if $(which beep)                    # 'beep' package not necessarily present
    then
        beep -l 600
    else
        echo -en "\a"
    fi
    return 0
}

# ---------------------------------
#  check_errors()
# ---------------------------------

function check_errors
{
    test $ERRORS -gt 0 &&
    {
        make_beep
        report "WARN: errors occurred: $ERRORS"
        report
        return 1
    }
    return 0
}

# ---------------------------------
#  confirm_file()
# ---------------------------------

function confirm_file
{
    local filename="$1"
    test -f "$filename" ||              # file exists and is a regular file
    {
        report "WARN: file not found: $filename"
        let "ERRORS++"
        check_errors
        exit $E_NO_FILE
    }
    return 0
}

# ---------------------------------
#  display_sets()
# ---------------------------------

# used to extract and display "@set FLAG" values

function display_sets
{
    local pat="^@set "                  # trigger pattern

    local result=$(grep "$pat" "$STUB.$TEXI" \
        | sed "s/$pat//" \
        | sed 's/\ \ */ /' \
        | sed 's/ /\t/' \
        | expand --tabs=12 \
        | sed 's/^/    /')

    if [ -n "$result" ]
    then
        report "texinfo sets (pattern = '$pat')"
        report
        echo "$result"
        report
    else
        report "texinfo sets (none found using pattern '$pat')"
        report
    fi
    return 0
}

# ---------------------------------
#  report_stub
# ---------------------------------

function report_stub
{
    local buffer
    printf -v buffer "%-18s : %s" "stub ($stubfrom)" "$STUB"
    report "$buffer"
}

# ---------------------------------
#  report_elapsed()
# ---------------------------------

# report elapsed (wall clock) time since script was invoked

function report_elapsed
{
    local seconds="$1"

    case "$seconds" in
        0|1) report "elapsed time       : 1 second"          ;;
        *)   report "elapsed time       : $seconds seconds"  ;;
    esac
    return 0
}

# ---------------------------------
#  toggle_pngs
# ---------------------------------

# the ordering of graphics format seems to have
# changed for 'pdfTeX' -- on 'sojus' PDF took
# precedence over PNG, on 'hinau' that is reversed
#
#  $ pdftex --version
#  'sojus' : pdfeTeX 3.141592-1.21.2 (Web2C 7.5.4)
#  'hinau' : pdfTeX 3.1415926-1.40.10-2.2 (TeX Live 2009/Debian)
#
# the following code disables and reinstates the
# relevant graphics files

function toggle_pngs
{
    local host=$(hostname --short)
    test "$host" == "hinau" || return 0

    local mode="$1"

    echo "  $FUNCNAME $mode"
    echo

    case "$mode" in
        disable)
            for graphic in $GRAPHICS
            do
                mv "$graphic.png" "$graphic.png~$MARK"
            done
            ;;
        enable)
            for graphic in $GRAPHICS
            do
                mv "$graphic.png~$MARK" "$graphic.png"
            done
            ;;
    esac
    return 0
}

# ---------------------------------
#  active code
# ---------------------------------

report

# initial information
report "native texinfo file = $STUB.$TEXI"
report "edit = $EDITOR $STUB.$TEXI"
report

# display @set values
display_sets

# confirm texi file
confirm_file "$STUB.$TEXI"

# INFO
call "build info" "info -f $STUB.info" "makeinfo --no-split $STUB.$TEXI" # ancillary files: none

toggle_pngs "disable"

# PDF (various routes)
#  call "build dvi" "xdvi $STUB.dvi &" "tex $STUB.$TEXI" "texindex $STUB.??" "tex $STUB.$TEXI"
#  call "build dvi" "xdvi $STUB.dvi &" "texi2dvi $STUB.$TEXI"
#  call "build pdf" "$PDFVIEWER $STUB.pdf &" "pdftex --recorder $STUB.$TEXI" "pdftex --recorder $STUB.$TEXI" "pdftex --recorder $STUB.$TEXI"
call "build pdf" "$PDFVIEWER $STUB.pdf &" "texi2pdf --quiet $STUB.$TEXI"

toggle_pngs "enable"

# HTML
clean_html
#  call "build html" "firefox $STUB.html &" "makeinfo --html --no-headers $STUB.$TEXI >| $STUB.html"
#  call "build html" "firefox $STUB/index.html &" "makeinfo --html $STUB.$TEXI"
call "build html" "firefox $STUB.html &" "makeinfo --html --no-split $STUB.$TEXI"

# TXT (understandable warnings may occur)
call "build txt" "less $STUB.txt" "makeinfo --no-warn --plaintext $STUB.$TEXI | sed 's/[[:blank:]]*$//' >| $STUB.txt"

# clean up (after tex and friends)
clean

# attempt to discover if the native texinfo file needs saving
check_saved

# confirm if errors
check_errors

# completion reporting
report_stub
report_elapsed $SECONDS

report

# ---------------------------------
#  housekeeping
# ---------------------------------

if [ $ERRORS -gt 0 ]
then
    exit $E_FAILURE
else
    exit $E_SUCCESS
fi

#  $Id: texime.sh 4770 2010-07-16 09:34:53Z robbie $
#  end of file

