#! /bin/bash

#  file-purpose     : refine the output from 'xeona --class' calls / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Thu 03-Dec-2009 21:30 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9243 $
#  $Date: 2012-04-20 15:38:03 +0200 (Fri, 20 Apr 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/xclass.sh $

# ---------------------------------
#  preamble
# ---------------------------------

BINARY="./xeona.mach"

SCRIPT=$(basename "$0")                 # script leafname

E_SUCCESS=0
E_FAIL=1
E_USAGE=2
E_NO_AWK=65
E_BINARY_NOT_FOUND=66
E_BINARY_NOT_EXE=67
E_MKTEMP_FAIL=68
E_TEX_FAIL=69                           # here
E_PDFLATEX_FAIL=70
E_PDFVIEW_FAIL=71

# ---------------------------------
#  display_help()
# ---------------------------------

function display_help
{
    local orgfile="entity.org"
    local texfile="entities.tex"
    echo
cat << EOM
        usage: $SCRIPT [opt]                    display preliminary info about all classes
               $SCRIPT [opt] <class-regex>      display and list matching classes
               $SCRIPT [opt] .                  display and list all classes (special case of above)
               $SCRIPT [opt] +                  list all classes
               $SCRIPT --org                    display all classes in org-mode format
               $SCRIPT --tex                    display all classes in org-mode LaTeX format
               $SCRIPT --pdf                    create PDF file using '--tex' and 'pdflatex'
               $SCRIPT --help                   show this message and exit
      options:  --less                   pipe output thu 'less'
      purpose: refine the output from 'xeona --class' calls
     matching: case-sensitive search, white-space invalid
        hints: pipe this call thru 'less'
               \$ ./$SCRIPT + | grep --invert-match "xeona classes" | sort --ignore-leading-blanks --key=2
               \$ ./$SCRIPT --org >| $orgfile && less $orgfile
               \$ ./$SCRIPT --tex >| $texfile && less $texfile   # see --pdf
    hardcodes: binary = $BINARY
     see also: xedoc
EOM
    echo
}

# ---------------------------------
#  make_pdf()
# ---------------------------------

#      # temporary file
#      local tempfile=$(mktemp -t "$SCRIPT~XXXXXX") || {
#          echo "$SCRIPT: FATAL: 'mktemp' returned: $?"
#          return $E_MKTEMP_FAIL
#      }
#      echo "$SCRIPT: temporary file: tempfile"

function make_pdf
{
    local tempfile="entities.tex"       # becomes 'entities.pdf'
    local stub=$(basename "$tempfile" ".tex")
    local pdffile="$stub.pdf"

    # make TeX
    ./$SCRIPT --tex >| "$tempfile" || {
        echo "$SCRIPT: FATAL: '$SCRIPT --tex' returned: $?"
        return $E_TEX_FAIL
    }

    # create PDF
    pdflatex "$tempfile" 2>/dev/null || {
        echo "$SCRIPT: FATAL: 'pdflatex' #1 returned: $?"
        return $E_PDFLATEX_FAIL
    }
    pdflatex "$tempfile" 2>/dev/null || {
        echo "$SCRIPT: FATAL: 'pdflatex' #2 returned: $?"
        return $E_PDFLATEX_FAIL
    }

    # open PDF
    evince "$pdffile" || {
        echo "$SCRIPT: FATAL: 'evince' returned: $?"
        return $E_PDFVIEW_FAIL
    }

    # clean up
    rm --force $stub.{tex,toc,out,aux,log}

    # housekeeping
    echo "---"
    echo "$SCRIPT: make PDF complete, see $pdffile"
    return $E_SUCCESS
}

# ---------------------------------
#  process command-line
# ---------------------------------

finalpipe="cat"                         # 'cat' offers no special behavior, whereas 'less' does

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        display_help
        exit $E_SUCCESS
        ;;
    --less|-l)
        shift
        finalpipe="less"
        ;;
    --org-mode|--orgmode|--org|-o)
        shift
        mode="org"
        ;;
    --tex|--latex|-t)
        shift
        mode="tex"
        ;;
    --pdf|-p)
        shift
        make_pdf
        exit
        ;;
esac

# ---------------------------------
#  utility and binary checks
# ---------------------------------

test $( which gawk ) ||                 # CAUTION: no double-quotes
{
    echo "$SCRIPT: 'gawk' utility not found (check your system)"
}

test -f "$BINARY" ||
{
    echo "$SCRIPT: FATAL: xeona binary not found: $BINARY"
    exit $E_BINARY_NOT_FOUND
}

test -x "$BINARY" ||
{
    echo "$SCRIPT: FATAL: xeona binary found but not executable by you (perhaps under compilation): $BINARY"
    exit $E_BINARY_NOT_EXE
}

# ---------------------------------
#  main case statement
# ---------------------------------

case "$mode" in
    tex)                                # this code still shows its former org-mode to latex heritage
        svn=$(./xeona.mach --svn 2>/dev/null)
        date=$(date "+%d %B %Y")
        script=$(pwd -P)"/"$(basename "$0")
        {
            printf "\n"
            $BINARY --class "." 2>/dev/null \
                | sed \
                --expression='s/^entity\.\(.*\)-0$/* \1/' \
                --expression='s/^entity.time-horizon/* time-horizon/' \
                --expression='s/^entity.overseer/* overseer/' \
                --expression='/^* /s/-/ /g' \
                --expression='s/^*/\n\n*/g'
            printf "\n\n"
        } | awk --assign svn="$svn" --assign date="$date" \
            'BEGIN {
                RS = "\n\n\n"                            # two blank lines
                FS = "\n"
                print
                print "\\documentclass[10pt,a4paper,oneside]{article}"
                print
                print "\\usepackage{hyperref}"
                print "\\hypersetup{colorlinks,citecolor=black,filecolor=black,linkcolor=blue,urlcolor=black}"
                print "\\usepackage[left=30mm,top=30mm,right=25mm,bottom=20mm,head=5mm,foot=5mm]{geometry}"
                print
                print "\\begin{document}"
                print
                printf "\\title{\\textit{xeona} entity listing}\n"
                printf "\\author{aligns with binary svn %s}\n", svn
                printf "\\date{%s}\n", date
                print
                print "\\maketitle"
                print
                printf "Note: the * indicates an entity in which the associated commodity needs to be specified.\n"
                print
                print "\\setcounter{tocdepth}{3}"
                print "\\tableofcontents"
                print "\\vspace*{1cm}"
                print
            }
            NF > 2 {                                     # skip any (hollow) record artifacts
                print $2
                print "\n\\small\n\\begin{verbatim}"     # "#+LaTeX:" caused problems
                for (i = 3; i <= NF; i++) print $i
                print "\n\\end{verbatim}\n"
            }
            END {
                print "\\end{document}"
                print ""
                print "% end of file"
                print
            }' | sed \
                --expression='s/^* \(.*\)$/\\section{\1}/'
        ;;
    org)
        svn=$(./xeona.mach --svn 2>/dev/null)
        date=$(date "+%Z %z %A %d-%b-%Y %H:%M")
        script=$(pwd -P)"/"$(basename "$0")
        printf "\nXEONA ENTITIES\n\n"
        printf "binary svn : %s\n" "$svn"
        printf "date       : %s\n" "$date"
        printf "script     : %s\n" "$script"
        printf "\n"
        printf "* summary\n\n"
        $BINARY --class "+" 2>/dev/null \
            | sed --expression='/^$/d'
        $BINARY --class "." 2>/dev/null \
            | sed \
            --expression='s/^entity\.\(.*\)-0$/* \1/' \
            --expression='s/^entity.time-horizon/* time-horizon/' \
            --expression='s/^entity.overseer/* overseer/' \
            --expression='/^* /s/-/ /g'
        printf "###\n\n"
        ;;
    *)
        case "$#" in
            0)
                $BINARY --class . 2>/dev/null \
                    | gawk 'BEGIN { RS = "\n\n\\<"; ORS = "\n"; FS = "[<>]"; print "" } /class/ { print $2 }' \
                    | grep --invert-match "builtin-remark" \
                    | grep --invert-match "my .*socket" \
                    | cat  --squeeze-blank
                echo -n " "
                $BINARY --class . 2>/dev/null \
                    | gawk 'BEGIN { RS = "\n\n\\<" } END { print "xeona classes = " NR }'
                ;;
            1)
                $BINARY --class "$1" 2>/dev/null
                ret=$?
                test $ret -eq 8 ||              # exit code 8 means no match
                {
                    if test "$1" == "+"         # argument "+" causes special processing
                    then
                        echo -n " "
                        $BINARY --class . 2>/dev/null \
                            | gawk 'BEGIN { RS = "\n\n\\<" } END { print "xeona classes = " NR }'
                    else
                        echo "---"
                        echo
                        $BINARY --class + 2>/dev/null \
                            | grep "$1"
                        echo
                    fi
                }
                ;;
            *)
                echo "$SCRIPT: unsupported argument (try --help for usage): $*"
                exit $E_USAGE
                ;;
        esac | $finalpipe
        ;;
esac

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $E_SUCCESS

#  $Id: xclass.sh 9243 2012-04-20 13:38:03Z robbie $
#  end of file

