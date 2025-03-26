#! /bin/bash

#  file-purpose     : open and close R graphics en-masse / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Wed 14-Jul-2010 19:57 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 7509 $
#  $Date: 2011-10-12 15:10:50 +0200 (Wed, 12 Oct 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/xeona-xmoks/x11show.sh $

# ---------------------------------
#  settings
# ---------------------------------

STUB="countplot"                        # default stub

EXTS="ps eps pdf svg png"
DEFAULT_DELAY_1=2                       # opening delay for viewability
DEFAULT_DELAY_2=4                       # opening delay for viewability
TAB=10                                  # output alignment

# ---------------------------------
#  viewer selections
# ---------------------------------

EPS_VIEWER="evince"

PDF_VIEWER="acroread"
PDF_VIEWER="evince"

SVG_VIEWER="firefox"
SVG_VIEWER="inkscape"
SVG_VIEWER="gthumb"
SVG_VIEWER="eog"

PNG_VIEWER="display"
PNG_VIEWER="gthumb"
PNG_VIEWER="eog"

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")

E_SUCCESS=0
E_FAIL=1
E_USAGE=2
E_FAULTY_EXTN=64

# ---------------------------------
#  help
# ---------------------------------

mode="open"
DELAY=0

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        echo
        echo "      usage: $SCRIPT  [--]                    view all '$STUB' image files"
        echo "             $SCRIPT  [--]   <stub>           view all <stub> image files"
        echo "             $SCRIPT  [--]    <00>            view all '$STUB.00' image files"
        echo "             $SCRIPT  [--]   <stub> <ext>     view all <stub> image files filtered by <ext>"
        echo "             $SCRIPT --slow  [stub] [ext]     add delay (as above)"
        echo "             $SCRIPT --SLOW  [stub] [ext]     add delay (as above) (also -ss)"
        echo "             $SCRIPT --kill                   kill all viewers"
        echo "             $SCRIPT --clean [stub] [ext]     clean (as above)"
        echo "             $SCRIPT --help                   display this message and exit"
        echo "    purpose: open and close R graphics files en-masse"
        echo "   defaults: file extensions : $EXTS"
        echo "             filename stub   : $STUB"
        echo "             slow delay      : $DEFAULT_DELAY_1"
        echo "             SLOW delay      : $DEFAULT_DELAY_2"
        echo "    example: \$ $SCRIPT -- $STUB-00 svg"
        echo "   see also: R scripts: robbie.saveX11 robbie.saveX11s"
        echo
        exit $E_SUCCESS
        ;;
    --clean|--c|-c)
        shift
        mode="clean"
        ;;
    --slow|--s|-s)
        shift
        DELAY=$DEFAULT_DELAY_1
        ;;
    --SLOW|--Slow|--ss|-ss)
        shift
        DELAY=$DEFAULT_DELAY_2
        ;;
    --killall|--kill|-k)
        echo "  closing displays"
        killall --quiet "$EPS_VIEWER"
        killall --quiet "$PDF_VIEWER"
        killall --quiet "$SVG_VIEWER"
        killall --quiet "$PNG_VIEWER"
        exit
        ;;
    --)                                 # for convenience
        shift
        ;;
    -|-*|--*)
        shift
        echo "$SCRIPT: ERROR: invalid command-line (try --help for usage)"
        echo $E_USAGE
        ;;
    "")
        :
        ;;
esac

# ---------------------------------
#  process arguments
# ---------------------------------

# grab stub if given

arg1="$1"
arg2="$2"

test -n "$arg1" &&
{
    printf -v tmp "%02d" "$arg1" 2>/dev/null # effectively confirms 'arg' is an integer
    case "$?" in
        0)
            STUB="$STUB-$tmp"
            ;;
        *)
            STUB=${arg1%.}                   # trim any trailing dot
            ;;
    esac
}

# grab extension if given

test -n "$arg2" && {
    ext="$arg2"
    pos=$(expr index "$EXTS" "$ext")         # one-based indexing
    case "$pos" in
        0)
            echo "$SCRIPT: FATAL: faulty extension given: $ext"
            exit $E_FAULTY_EXTN
            ;;
        *)
            EXTS="$ext"
            ;;
    esac
}

case "$mode" in

# ---------------------------------
#  clean
# ---------------------------------

    clean)
        loop=0
        for ext in $EXTS                        # can thu extensions
        do
            for file in $STUB*.$ext             # globbing occurs
            do
                test -f "$file" || continue     # file not found
                let "loop++"
                rm --force "$file"
            done
        done
        echo "  deleted $loop files"
        ;;

# ---------------------------------
#  active code
# ---------------------------------

    open)
        loop=0
        for ext in $EXTS                        # can thu extensions
        do
            for file in $STUB*.$ext             # globbing occurs
            do
                test -f "$file" || continue     # file not found
                test -s "$file" || continue     # file is empty
                case "$ext" in
                    ps|eps) call="$EPS_VIEWER"  ;;
                    pdf)    call="$PDF_VIEWER"  ;;
                    svg)    call="$SVG_VIEWER"  ;;
                    png)    call="$PNG_VIEWER"  ;;
                esac
                # report with bytes
                let "loop++"
                size=$(stat --print="%s" "$file")
                printf "    %02d    %-*s %-28s %10s\n"  "$loop" $TAB "$call" "$file" "$size"
                # action
                line="$call $file &"
                eval "$line"
                sleep $DELAY
            done
        done

        case "$loop" in
            0)
                echo "  opened $loop files"
                ;;
            *)
                echo -n "  enter 'y' to keep displays open "
                read response
                case "$response" in
                    y|Y)
                        echo "  retaining displays"
                        ;;
                    *)
                        echo "  closing displays"
                        killall --quiet "$EPS_VIEWER"
                        killall --quiet "$PDF_VIEWER"
                        killall --quiet "$SVG_VIEWER"
                        killall --quiet "$PNG_VIEWER"
                        ;;
                esac
                case "$#" in
                    0) echo "  arguments     : (none)"           ;;
                    1) echo "  arguments     : $arg1"            ;;
                    2) echo "  arguments     : $arg1  +  $arg2"  ;;
                esac
                echo "  resolved      : $STUB  +  $EXTS"
                case "$DELAY" in
                    0) echo "  opening delay : instant"         ;;
                    1) echo "  opening delay : $DELAY second"   ;;
                    *) echo "  opening delay : $DELAY seconds"  ;;
                esac
                ;;
        esac
        ;;

# ---------------------------------
#  housekeeping
# ---------------------------------

esac

exit $E_SUCCESS

#  $Id: x11show.sh 7509 2011-10-12 13:10:50Z robbie $
#  end of file

