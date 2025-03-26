#! /bin/bash

#  file-purpose     : create PDF of xeona codebase / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 09-Aug-2010 18:46 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 5532 $
#  $Date: 2010-11-22 19:45:13 +0100 (Mon, 22 Nov 2010) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/pdfall.sh $

#  NOTES
#
#    * takes about 4 minutes for 192 files on Intel Core i5 (one core utilized)
#    * PostScript point = 1/72" ~ 0.35mm
#    * outputs PDF/A archive subset based on PDF version 1.4 (should be highly portable)
#    * see known issues below
#
#  Behavior settings
#
#    * underlay (watermark) in { yes no }
#    * header (each page) in { plain fancy xeona }
#    * highlight (code syntax) in { yes no }
#
#  Caveats
#
#    * scripting assumes no whitespace in filenames
#
#  Page count limitations
#
#    80gsm paper
#    10mm   92 sheets
#    12mm  110 sheets
#    15mm  138 sheets
#    18mm  165 sheets
#    20mm  184 sheets
#    24mm  220 sheets
#    30mm  276 sheets
#    up to 50mm binding offered = 460
#
#    http://www.sprintout.de/download/Flyer_Preisblatt_sprintout.pdf
#    Fastback-Heißleimbindung bis 350 Blatt
#
#  Known issues
#
#    'xedoc' script gives memory core dump with
#    non-trivial margin setting, however the output is
#    fine
#
#  Improvements
#
#    refactor function 'clean_dir'

# ---------------------------------
#  settings
# ---------------------------------

PREFIX="xeona"                                    # filename prefix, for example "xeona.r5043.code0.pdf"
PDF_DIR="$HOME/synk/xeona/xeona-codebase-pdfs"    # directory for created files (will be created if required)
PDFVIEWER="evince"                                # PDF viewer to use for viewing

# ---------------------------------
#  svn version
# ---------------------------------

# svn version checked later for cleanliness (pure integer) if required

host=$(hostname --short)
case "$host" in
    hinau) SVN_BASE="$HOME/synk/xeona/svn2/futz/trunk"  ;;  # svn version base for the purposes of this script
    sojus) SVN_BASE="$HOME/synk/xeona/svn/futz"         ;;  # svn version base for the purposes of this script
    *)     SVN_BASE="$HOME/synk/xeona/svn/futz"         ;;  # svn version base for the purposes of this script
esac

SVN=$(svnversion $SVN_BASE 2>/dev/null)

# ---------------------------------
#  preamble
# ---------------------------------

DATE=$(date '+%d-%b-%Y')                # used on cover PDF: 14-Oct-2010
DATE=$(date '+%d %b %Y')                # used on cover PDF: 14 Oct 2010

STUB="$PREFIX-$SVN"
OUTFILE="$PREFIX.r$SVN.code"
TEXIFILE="$PDF_DIR/$STUB-000.texi"
PSFILE="$PDF_DIR/$STUB-999.ps"

badens=""                               # list of files encountering 139 enscript errors
badwraps=""                             # list of files requiring line wrapping (strict exit 2))

SCRIPT=$(basename "$0")                 # script leafname

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2
E_SVN_STALE=3
E_MKTEMP_FAIL=64
E_MISSING_UTILITIES=65

# ---------------------------------
#  show_help()
# ---------------------------------

function show_help
{
    cat << EOM

       usage: $SCRIPT  --run         run script (takes about 4 minutes)
              $SCRIPT  --clean       prompted clean of target directory (recommended before a run)
              $SCRIPT  --review      review files using 'textcheck'
              $SCRIPT  --list        list files, organized by volume, and exit
              $SCRIPT  --help        display this message and exit
     purpose: create PDF of xeona codebase
     testing: -0  run without requiring a clean svn
              -1  single cpp file
              -2  cover sheet
              -3  report final PDFs
              -4  confirm svn version
   hardcodes: current stub  : $STUB
              PDF directory : $PDF_DIR
     columns: recommended  90
              absolute    103

EOM
}

# ---------------------------------
#  process command-line
# ---------------------------------

mode="none"
clean="yes"

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        show_help
        exit $E_SUCCESS
        ;;
    --review|-r)
        shift
        mode="review"
        ;;
    --clean|-c)
        shift
        mode="clean"
        ;;
    --run|-r)
        shift
        mode="run"
        ;;
    --list|-l)
        shift
        mode="list"
        ;;
    --0|-0)
        shift
        mode="run"
        clean="no"
        ;;
    --1|-1)
        shift
        mode="trial-1"
        ;;
    --2|-2)
        shift
        mode="trial-2"
        ;;
    --3|-3)
        shift
        mode="trial-3"
        ;;
    --4|-4)
        shift
        mode="trial-4"
        ;;
esac

# ---------------------------------
#  report()
# ---------------------------------

function report
{
    local tab=16
    case "$#" in
        0) printf "\n"                            ;;
        1) printf "  %s\n"             "$1"       ;;
        2) printf "  %-*s : %s\n" $tab "$1" "$2"  ;;
    esac
}

# ---------------------------------
#  svn_confirm
# ---------------------------------

function svn_confirm
{
    printf -v dummy "%d" "$SVN" 2>/dev/null  # effectively confirms 'SVN' is an integer
    case "$?" in
        0)
            report "svn check" "$SVN"
            ;;
        *)
            report "BAD svn"    "$SVN"
            report "FATAL exit" "$E_SVN_STALE"
            case "$clean" in
                yes)
                    report
                    exit $E_SVN_STALE
                    ;;
                *)
                    report "skipping exit $E_SVN_STALE under clean = $clean"
                    ;;
            esac
            ;;
    esac
}

# ---------------------------------
#  check_for_utility()
# ---------------------------------

function check_for_utility
{
    local utility="$1"                  # utility name
    local info="$2"                     # optional addition information

    local msg="WARNING: support utility not present:"
    if [ $(which "$utility") ]
    then
        return 0
    else
        case "$#" in
            1) report "$utility" "utility not found"          ;;
            2) report "$utility" "utility not found ($info)"  ;;
        esac
        return 1
    fi
}

# ---------------------------------
#  check_for_utilities()
# ---------------------------------

function check_for_utilities
{
    local errors=0

    check_for_utility "enscript"    "try package 'enscript'"          || let "errors++"
    check_for_utility "ps2pdf"      "try package 'ghostscript'"       || let "errors++"
    check_for_utility "pdftk"       "try package 'pdftk'"             || let "errors++"
    check_for_utility "textcheck"   "custom bash script"              || let "errors++"
    check_for_utility "$PDFVIEWER"  "can be altered in $SCRIPT"       || let "errors++"

    case "$errors" in
        0)
            report "utilities check" "all sought utilities located"
            return 0
            ;;
        *)
            report "utilities check" "some utilities not located"
            return 1
            ;;
    esac
}

# ---------------------------------
#  application_list()
# ---------------------------------

files1=""
files2=""
files3=""

function application_list
{
    # obtain data
    local sources=$(make ccs)
    local extras=$(make hs)             # unaccompanied headers
    local headers=""                    # normal headers

    # split sources (and make any adjustments here)
    local split1=$(awk 'BEGIN { RS = "\n" } { if( NR <= 44 ) print $0 }' <<< "$sources")
    local split2=$(awk 'BEGIN { RS = "\n" } { if( NR >  44 ) print $0 }' <<< "$sources")

    # locate headers
    sources=$(echo $sources)
    for source in $sources
      do
      stub=${source%".cc"}
      test "$stub" == "./main" && continue
      headers="$headers $stub.h"
    done

    # export
    files1=$(echo $headers $extras)
    files2=$(echo $split1)
    files3=$(echo $split2)
}

# ---------------------------------
#  ancillary_list()
# ---------------------------------

files4=""

function ancillary_list
{
    # obtain data
    local mak="
./makefile"

    local sh="
../scripts/xedoc
../scripts/mach
../scripts/xmok"

    # CAUTION: omit -print (or use correct syntax)

    local rstat=$(find "../xeonar" \( -name '*.R'  -a ! -name 'userFuncs.R' \) -type f | sort)

    local elisp1=$(find "../elisp"  \( -name '*.el' -a ! -name 'dev*.el'     \) -type f | sort)

    local elisp2="
../elisp/xeona.el
../elisp/xem.el
../elisp/xem-1.el
../elisp/xem-2.el
../elisp/xem-3.el
../elisp/xrstat.el
../elisp/xog.el
../elisp/xumber.el
"

    # export
    files4=$(echo $mak $sh $rstat $elisp2)  # cheap whitespace strip, care not -neE
}

# ---------------------------------
#  size_list()
# ---------------------------------

size0=-1
size1=-1
size2=-1
size3=-1
size4=-1

function size_list
{
    size1=$(wc --word <<< "$files1")
    size2=$(wc --word <<< "$files2")
    size3=$(wc --word <<< "$files3")
    size4=$(wc --word <<< "$files4")
    size0=$(wc --word <<< "$files1 $files2 $files3 $files4")
}

# ---------------------------------
#  display_list()
# ---------------------------------

function display_list
{
    # check for dirty text

    textcheck $files
}

# ---------------------------------
#  user_confirm()
# ---------------------------------

function user_confirm
{
    local msg="$1"                      # like "delete saved workspace"

    local response="n"
    local prompt="  $SCRIPT: hit 'y' to $msg: "
    echo
    read -n 1 -p "$prompt" response     # "-n 1" is read one character and return
    echo
    case "$response" in
        y|Y) return 0 ;;
        *)   return 1 ;;
    esac
}

# ---------------------------------
#  clean_dir()
# ---------------------------------

# this function could be refactored, but it does work all the same

function clean_dir
{
    local dir="$1"
    stub="$STUB"
    outfile="$OUTFILE"
    (                                   # local subshell
        cd $dir

        report
        local pdfs=$(ls $stub-*.{ps,pdf}* 2>/dev/null)
        local number=$(ls -1 $stub-*.{ps,pdf}* 2>/dev/null | wc --lines)
        pdfs=$(echo $pdfs)              # CAUTION: strip line returns
        report "pattern" "$stub-*.{ps,pdf}*"
        case "$number" in
            0)
                report "no matched ps/pdfs found"
                ;;
            *)
                report "matched ps/pdfs" "$number"
                user_confirm "delete files"
                case $? in
                    0)
                        rm --force $stub-*.{ps,pdf}*
                        report "stub files deleted"
                        ;;
                    1)
                        report "NO stub files deleted"
                        ;;
                esac
                ;;
        esac

        report
        local texs=$(ls $stub-*.{texi,aux,cp,cps,fls,fn,fns,ky,kys,log,pg,pgs,tmp,toc,tp,tps,vr,vrs} 2>/dev/null)
        local number=$(ls -1 $stub-*.{texi,aux,cp,cps,fls,fn,fns,ky,kys,log,pg,pgs,tmp,toc,tp,tps,vr,vrs} 2>/dev/null | wc --lines)
        texs=$(echo $texs)              # CAUTION: strip line returns
        report "TeX files" "$stub-*.{texi,aux,cp,cps,fls,fn,fns,ky,kys,log,pg,pgs,tmp,toc,tp,tps,vr,vrs}"
        case "$number" in
            0)
                report "no TeX files found"
                ;;
            *)
                report "matched TeX files" "$number"
                user_confirm "delete files"
                case $? in
                    0)
                        rm --force $stub-*.{texi,aux,cp,cps,fls,fn,fns,ky,kys,log,pg,pgs,tmp,toc,tp,tps,vr,vrs}
                        report "TeX files deleted"
                        ;;
                    1)
                        report "NO TeX files deleted"
                        ;;
                esac
                ;;
        esac

        report
        local cmps=$(ls $outfile[0-9].pdf 2>/dev/null)
        local number=$(ls -1 $outfile[0-9].pdf 2>/dev/null | wc --lines)
        cmps=$(echo $cmps)              # CAUTION: strip line returns
        report "composite pdf" "$outfile[0-9].pdf"
        case "$number" in
            0)
                report "no composites found"
                ;;
            *)
                report "matched composites" "$number"
                user_confirm "delete files (no)"
                case $? in
                    0)
                        rm --force $outfile[0-9].pdf
                        report "composite files deleted"
                        ;;
                    1)
                        report "NO composite files deleted"
                        ;;
                esac
                ;;
        esac

        report
        local number=$(ls -1 * 2>/dev/null | wc --lines)
        case "$number" in
            0) report "non-dot clean" "$dir"       ;;
            *) report "REMAINING files" "$number"  ;;
        esac

        return 0
    )
}

# ---------------------------------
#  clean_cores()
# ---------------------------------

function clean_cores
{
    report
    local core=$(ls "./core" 2>/dev/null)
    local number=$(ls -1 "./core" 2>/dev/null | wc --lines)
    core=$(echo $core)              # CAUTION: strip line returns
    report "local core dumps" "./core"
    case "$number" in
        0)
            report "no local core dumps found"
            ;;
        *)
            report "matched dumps" "$number"
            user_confirm "delete files"
            case $? in
                0)
                    rm --force "./core"
                    report "core dumps deleted"
                    ;;
                1)
                    report "NO core dumps deleted"
                    ;;
            esac
            ;;
    esac
}

# ---------------------------------
#  check_dir()
# ---------------------------------

function check_dir
{
    local dir="$1"
    mkdir --parents "$dir"
    ret=$?
    case "$ret" in
        0)
            return 0
            ;;
        *)
            report "mkdir failed" "$dir"
            report "FATAL"
            report
            exit $E_FAILURE
            ;;
    esac
}

# ---------------------------------
#  pdf_meta()
# ---------------------------------

#  based on 'pdfrobbie'

function pdf_meta
{
    local pdfile="$1"
    local title="$2"

    local AUTHOR="Robbie Morrison <robbie@actrix.co.nz>"
    local TITLE="$title"
    local SUBJECT="principal files from the xeona energy systems modeling environment"

    local infofile="$pdfile.pdftk"                # temporary info file

    # active code

    # establish a safe temporary and move original file
    # 'mktemp' also creates an empty file with umask perms
    local access=$(stat --format %a "$pdfile")    # access permissions in octal form

    local pdftemp=$(mktemp "$pdfile~XXXXXX") || {
        echo "$SCRIPT: FATAL: 'mktemp' returned: $?"
        exit $E_MKTEMP_FAIL
    }
    mv "$pdfile" "$pdftemp"               # move PDF file over

    # defensive programming
    rm --force "$infofile"

    # produce modified metadata
    cat > "$infofile" << EOF

InfoKey: Author
InfoValue: $AUTHOR

InfoKey: Title
InfoValue: $TITLE

InfoKey: Subject
InfoValue: $SUBJECT

EOF

    # update metadata and re-establish original file
    pdftk "$pdftemp" update_info "$infofile" output "$pdfile"
    chmod "0$access" "$pdfile"            # reinstate file permissions
    report "pdf_meta" "metadata updated: $title"

    # remove temporaries as required
    rm --force "$infofile"                # info file
    rm --force "$pdftemp"                 # original pdf copied to temp file
}

# ---------------------------------
#  type_file()
# ---------------------------------

# better to rely on your knowledge of your file
# extensions and not on the sometimes faulty assessment
# by the 'file' utility

filetype="(not set)"

function type_file
{
    local file="$1"

    filetype="(reset)"
    local ftype=$(file --brief $file)
    report "file"  "$file"
    report "ftype" "$ftype"

    local leaf=$(basename "$file")      # CAUTION: else the ".." in paths can distort the matching
    case "$leaf" in
        [mM]akefile) filetype="make"      ;;
        *.R)         filetype="rstat"     ;;
        *.sh)        filetype="bash"      ;;
        *.el)        filetype="elisp"     ;;
        *.h)         filetype="cpp-head"  ;;
        *.cc)        filetype="cpp-impl"  ;;
        *.*)         filetype=""          ;;
        *)                                       # no extension
            case "$ftype" in
                "Bourne-Again shell script text*") filetype="bash"  ;;
                *)                                 filetype=""      ;;
            esac
            ;;
    esac

    report "filetype" "$filetype"

    #  'file' reporting
    #
    #    elisp    : "Lisp/Scheme program text"
    #    bash     : "Bourne-Again shell script text executable"
    #    .{h,cc}  : "ASCII C++ program text"
    #    .R       : not apparently supported
    #    makefile : not apparently supported
}

# ---------------------------------
#  pdf_file()
# ---------------------------------

ENSCRIPT_HEADER_DEFN="$HOME/.enscript/xeona.hdr"
count=0                                 # file counter

function pdf_file_1
{
    local infile="$1"

    # 0 - OUTSET

    let "count++"
    local cntstr
    printf -v cntstr "%03d" $count

    # 1 - ENSCRIPT HEADER DEFINITION

    report "header defn" "$ENSCRIPT_HEADER_DEFN"

    local enscript_header_dir=$(dirname $ENSCRIPT_HEADER_DEFN)
    mkdir --parents "$enscript_header_dir"
    ret=$?
    report "mkdir return" "$ret"
    test $ret -eq 0 || return 1

    rm --force "$ENSCRIPT_HEADER_DEFN"
    ret=$?
    report "force return" "$ret"
    test $ret -eq 0 || return 1

    # see: $ ll /usr/share/enscript/*.hdr

    local headerfont=11                 # originally 13

    cat > "$ENSCRIPT_HEADER_DEFN" << EOF

% GNU enscript header definition file
% hacked from 'edd.hdr', version 1.6.5
% generated automatically by $SCRIPT
% under bash, requires escapes on non-bash "\$"
% headerfont set to $headerfont

% -- code follows this line --
%%DocumentNeededResources: font Helvetica
%Format: xeonastr       xeona $SVN
%Format: datestr        %D{%d-%b-%Y}
%Format: pagenumstr     $count.\$V$%

% Fonts.
/Helvetica /HeaderFont-Bold MF
/HeaderF /HeaderFont-Bold findfont $headerfont scalefont def

/Helvetica /HeaderFont-Large MF
/HeaderLF /HeaderFont-Large findfont $headerfont 1.3 mul scalefont def

/do_header {    % print emacs header
  gsave
    d_header_x d_header_y translate
    0 setgray

    % gray box
    0 1 d_header_w 1 sub d_header_h 1 sub Box
    gsave
      .9 setgray fill
    grestore

    % texts

    /xmarg 5 def

    % Top line.
    HeaderLF setfont
    /y d_header_h 12 div d_header_h 2 div add def
    xmarg y moveto fname show

    pagenumstr dup stringwidth pop
    d_header_w exch sub 1 sub xmarg sub y moveto show

    % Bottom line
    HeaderF setfont
    /y d_header_h 6 div def
    xmarg y moveto xeonastr show

    datestr dup stringwidth pop
    d_header_w exch sub 1 sub xmarg sub y moveto show

  grestore
} def

EOF

    ret=$?
    report "cat return" "$ret"
    test $ret -eq 0 || return 1

    # 2 - PREAMBLE

    local stub="$PDF_DIR/$STUB"

    local psfile=""
    local pdfile=""
    printf -v psfile "%s-%03d.ps"  $stub $count
    printf -v pdfile "%s-%03d.pdf" $stub $count

    # report filenames

    report "infile" "$infile"
    report "psfile" "$psfile"
    report "pdfile" "$pdfile"

    # remove files

    rm --force "$psfile"
    rm --force "$pdfile"

    # 3 - PS OUTPUT : enscript

    # set behaviors

    local underlay="yes"
    local underlay="no"

    local header="fancy"
    local header="plain"
    local header="xeona"

    local highlight="yes"
    local highlight="no"

    # set options

    local opts=""
    local opts="$opts --font='Courier8'"
    local opts="$opts --media='A4'"                         # see $ enscript  --list-media
    local opts="$opts --lines-per-page='82'"
#   local opts="$opts --margins='60:200::'"                 # for testing
    local opts="$opts --margins='60:30::'"                  # "left:right:top:bottom" in postscript points (1/72")
    local opts="$opts --mark-wrapped-lines='arrow'"         # use "box" if hunting for offenders
    local opts="$opts --word-wrap"                          # nicer if just the occasional over-length line

    case $header in
        plain)
            local enheader="|%W|Page \$% of $="             # example 1
            local enheader="|%W|page $count.\$%"            # good for pagination
            local enheader="file \$n|date %D{%d-%b-%Y}    xeona r$SVN|page $count.\$%"              # development
            local enheader="date %D{%d-%b-%Y}|xeona r$SVN file \$n|page $count.\$%"                 # development
            local enheader="date %D{%d-%b-%Y}|xeona r$SVN file $infile|page $count.\$%"             # development

            local opts="$opts --header='"$enheader"'"       # concat header string and add to options
            ;;
        fancy)
            local opts="$opts --fancy-header"
            ;;
        edd)
            local fancy="edd"
            local opts="$opts --fancy-header='"$fancy"'"    # concat header string and add to options
            ;;
        xeona)
            local fancy="xeona"
            local opts="$opts --fancy-header='"$fancy"'"    # concat header string and add to options
            ;;
    esac

#   local opts="$opts --file-align='2'"
#   local opts="$opts --highlight-bars='2'"
#   local opts="$opts --highlight-bar-gray='0.98'"

    # see $ enscript --help-highlight
    type_file "$infile"
    case "$highlight" in
        yes)
            case "$filetype" in
                make)     local opts="$opts --highlight='makefile'"  ;;
                cpp-head) local opts="$opts --highlight='cpp'"       ;;
                cpp-impl) local opts="$opts --highlight='cpp'"       ;;
                bash)     local opts="$opts --highlight='bash'"      ;;
                elisp)    local opts="$opts --highlight='elisp'"     ;;
                rstat)    :                                          ;;
                "")       :                                          ;;
                *)
                    report "BAD filetype" "$filetype"
                    return 1
                    ;;
            esac
            ;;
    esac

    case "$underlay" in
        yes)
            local opts="$opts --underlay='"$SVN"'"   # concat
            local opts="$opts --underlay='xeona'"
            local opts="$opts --ul-style='filled'"
            local opts="$opts --ul-font='Helvetica100'"
            local opts="$opts --ul-gray='0.95'"
            ;;
    esac

    local opts="$opts --title='xeona $SVN file "$infile"'"

    report "enscript options"
    echo
    sed 's/ --/\n/g' <<< "$opts" \
        | grep --invert-match "^$" \
        | awk 'BEGIN { FS = "=" } { printf("      %-20s :  %s\n", $1, $2) }'
    echo

    # create enscript call

    local opts="$opts --extended-return-values"
    local opts="$opts --output=\"$psfile\""
    local opts=${opts#" "}                   # strip the leading space
    call="enscript $opts $infile"

    report "filetype" "$filetype"
    report "call" "$call"
    eval "$call"
    ret=$?
    report "enscript return" "$ret"
    case "$ret" in
        0)
            :
            ;;
        2)
            badwraps="$badwraps $infile"
            ;;
        139)                            # [1]
            report "SKIPPING BAD RETURN"
            badens="$badens $infile"
            ;;
        *)
            return 1
            ;;
    esac

    # [1] file 176 which is '../scripts/xedoc' cores on
    # non-trivial '--margin' settings but seems to
    # produce okay PostScript

    # 4 - PDF CONVERSION : ps2pdf

    local opts=""
    local opts="$opts -dEmbedAllFonts='true'"          # confirm biting (possibly only PDF/A)
    local opts="$opts -dCompatibilityLevel='1.4'"
    local opts="$opts -dPDFA"                          # PDF/A archive subset
    local opts="$opts -sPAPERSIZE='a4'"

    report "ps2pdf options"
    echo
    sed 's/ -/\n/g' <<< "$opts" \
        | grep --invert-match "^$" \
        | awk 'BEGIN { FS = "=" } { printf("      %-20s :  %s\n", $1, $2) }'
    echo

    local opts=${opts#" "}                             # strip the leading space
    call="ps2pdf $opts $psfile $pdfile"

    report "call" "$call"
    eval "$call"
    ret=$?
    report "ps2pdf return" "$?"
    test $ret -eq 0 || return 1

    # 5 - UPDATE METADATA

    pdf_meta "$pdfile" "xeona $SVN file $infile"

    # 6 - CHANGE PERMS

    chmod 0440 "$psfile"
    chmod 0440 "$pdfile"

    # 7 - PDF VIEW : evince

    case "$view" in
        yes)
            call="evince $pdfile &"
            eval "$call"
            ;;
    esac

    return 0
}

# ---------------------------------
#  pdf_list()
# ---------------------------------

function pdf_list
{
    for file in $files
    do
        pdf_file_1 "$file"
        report
    done
}

# ---------------------------------
#  create_texi()
# ---------------------------------

# 000 is always the covering PDF

function create_texi
{
    local arg="$1"
    local filelist1=$(sed 's/ /\n/g' <<< "$files1")
    local filelist2=$(sed 's/ /\n/g' <<< "$files2")
    local filelist3=$(sed 's/ /\n/g' <<< "$files3")
    local filelist4=$(sed 's/ /\n/g' <<< "$files4")

    case "$arg" in
        0) local title="Core codebase"                                      ;;
        1) local title="Core codebase : C++ header files"                   ;;
        2) local title="Core codebase : C++ implementation files / 1 of 2"  ;;
        3) local title="Core codebase : C++ implementation files / 2 of 2"  ;;
        4) local title="Core codebase : ancillary files"                    ;;
    esac

    case "$arg" in
        0) local vstate=""                      ;;
        *) local vstate="@ (complete version)"  ;;
    esac

    cat > "$TEXIFILE" << EOF

\input texinfo

@c -------------------------------------------------------- document header

@c %**start of header
@settitle xeona : an energy systems modeling environment
@afourpaper                                  @c A4 paper
@c %**end of header

@c ----------------------------------------------------------------- macros

@set DATE     $DATE
@set YEAR     2010
@set SVN      $SVN
@set STUB     code$arg
@set TITLE    $title
@set OUTNAME  ${OUTFILE}0.pdf
@set VERSION  $vstate
@set COUNT    $count files

@c ------------------------------------------------------------- title page

@finalout                                    @c suppress overfull hbox marks
@titlepage

@c @title @sl{xeona}
@title xeona
@subtitle An Energy Systems Modeling Environment
@sp 2.0
@subtitle @b{@value{TITLE}}
@subtitle @value{DATE}
@subtitle @emph{xeona} revision @value{SVN}
@subtitle @value{COUNT}
@author @c Robbie Morrison

@page
@vskip 0pt plus 1filll

Suggested citation@value{VERSION}:
@sp 0.2
Morrison, Robbie.  @value{YEAR}.  Core codebase
of the @i{xeona} energy @* systems modeling
environment --- revision @value{SVN}. PDF version. @*
Available as @code{@value{OUTNAME}}.

@sp 10.0

Refer to the source code listings for license details.
@sp 1.0
Robbie Morrison@*
Institute for Energy Engineering@*
Technical University of Berlin@*
Marchstrasse 18, D--10587 Berlin, Germany@*
Email: @email{robbie@@actrix.co.nz}

@sp 3.0

@end titlepage

@c ------------------------------------------------------------------- code

@majorheading C++ header files

@itemize @bullet
@item C++ language
@end itemize

@sp 0.5

@example
$filelist1
@end example

@majorheading C++ implementation files

@noindent This list comprises only the core
implementation files and omits the layered @* unit test
files.

@sp 0.5

@itemize @bullet
@item C++ language
@end itemize

@sp 0.5

@example
$filelist2
@end example

@example
$filelist3
@end example

@majorheading Ancillary files

@noindent Ancillary files provide support for building
@i{xeona} and for variously editing, visualizing, @*
and running @i{xeona} models.

@sp 0.5

@itemize @bullet
@item GNU make language
@item GNU bash language
@item GNU R language
@item GNU emacs lisp language
@end itemize

@sp 0.5

@example
$filelist4
@end example

@sp 2.0

@tex
$\diamond$
@end tex

@c -------------------------------------------------------------------- bye

@bye

EOF

    report "created" "$TEXIFILE"
    ls -l $TEXIFILE
}

# ---------------------------------
#  process_texi()
# ---------------------------------

function process_texi
{
    (
        cd "$PDF_DIR"
        local texifile="$STUB-000.texi"
        call="texi2pdf --quiet \"$texifile\""
        report "call" "$call"
        eval "$call"
        ret=$?
        report "texi2pdf return" "$ret"
        test $ret -eq 0 || return 1
        return 0
    )
}

# ---------------------------------
#  make_endpage()
# ---------------------------------

# 999 is always the endpage PDF

function make_endpage
{
    local arg="$1"

    local coverstub="$PDF_DIR/$STUB-999"
    local psfile="$coverstub.ps"
    local pdfile="$coverstub.pdf"

    rm --force $psfile
    rm --force $pdfile

    cat > "$psfile" << EOF

%!
% xeona codebase coversheet

/Times-Roman findfont                                  % get the basic font
11 scalefont                                           % scale the font
setfont                                                % make it the current font

newpath                                                % start a new path
100 600 moveto                                         % lower left corner of text at (x, y)
(final page: xeona.r$SVN.code$arg.pdf) show            % typeset

showpage

EOF

    # 4 - PDF CONVERSION : ps2pdf

    local opts=""
    local opts="$opts -dEmbedAllFonts='true'"          # confirm biting (possibly only PDF/A)
    local opts="$opts -dCompatibilityLevel='1.4'"
    local opts="$opts -dPDFA"                          # PDF/A archive subset
    local opts="$opts -sPAPERSIZE='a4'"

    local opts=${opts#" "}                             # strip the leading space
    call="ps2pdf $opts $psfile $pdfile"

    report "call" "$call"
    eval "$call"
    ret=$?
    report "ps2pdf return" "$?"
    test $ret -eq 0 || return 1

    # 5 - UPDATE METADATA

    pdf_meta "$pdfile" "end page"

    # 6 - CHANGE PERMS

    chmod 0440 "$psfile"
    chmod 0440 "$pdfile"

    # 7 - PDF VIEW : evince

    case "$view" in
        yes)
            call="evince $pdfile &"
            eval "$call"
            ;;
    esac

    return 0
}

# ---------------------------------
#  combine()
# ---------------------------------

# /home/robbie/synk/temp-printall/xeona-9876-001.pdf

function combine
{
    local arg="$1"

    local stub="$STUB"
    local outfile="$OUTFILE$arg.pdf"

    local beg=0
    local end=0
    case "$arg" in
        0)
            let "beg = 1"
            let "end = beg + size0 - 1"
            ;;
        1)
            let "beg = 1"
            let "end = beg + size1 - 1"
            ;;
        2)
            let "beg = size1 + 1"
            let "end = beg + size2 - 1"
            ;;
        3)
            let "beg = size1 + size2 + 1"
            let "end = beg + size3 - 1"
            ;;
        4)
            let "beg = size1 + size2 + size3 + 1"
            let "end = beg + size4 - 1"
            ;;
    esac

    case "$arg" in
        0) local title="xeona $SVN core codebase"                                      ;;
        1) local title="xeona $SVN core codebase : C++ header files"                   ;;
        2) local title="xeona $SVN core codebase : C++ implementation files / 1 of 2"  ;;
        3) local title="xeona $SVN core codebase : C++ implementation files / 2 of 2"  ;;
        4) local title="xeona $SVN core codebase : ancillary files"                    ;;
    esac

    local pdfname=""
    local pdfs=""
    local seq=$(seq $beg $end)
    local seq=$(echo $seq)
    local seq="0 $seq 999"
    for num in $seq
    do
        printf -v pdfname "%s-%03d.%s" $stub $num "pdf"
        pdfs="$pdfs $pdfname"
    done
    local pdfs=${pdfs#" "}              # strip the leading space

    report                              # blank line

    (                                   # local subshell
        cd "$PDF_DIR"

        local errors=0
        for pdf in $pdfs
        do
            if [ ! -f "$pdf" ]
            then
                report "CANNOT find PDF" "$pdf"
                let "errors++"
            fi
        done

        test $errors -eq 0 || return 1

        call="pdftk $pdfs cat output $outfile verbose"
        report "call" "$call"
        eval "$call"
        ret=$?
        report "pdftk return" "$?"
        test $ret -eq 0 || return 1

        # UPDATE METADATA

        pdf_meta "$outfile" "$title"

        # CHANGE PERMS

        chmod 0440 "$outfile"
    )

    report "combine complete" "$outfile"
}

# ---------------------------------
#  pdf_pages()
# ---------------------------------

pdfpages="-2"

function pdf_pages
{
    local file="$1"
    test $(which pdfinfo) || return     # CAUTION: no soft-quotes
    pdfpages=$(pdfinfo "$file" | gawk 'BEGIN { FS = ": *" }  /Pages/ { print $2 }')  # NOTE: FS regex
}

# ---------------------------------
#  reader_report()
# ---------------------------------

function reader_report
{
    report
    report "general reporting"

    (                                   # local subshell
        cd "$PDF_DIR"

        report
        for arg in $(seq 0 4)
        do
            case "$arg" in
                0) local tag="complete"         ;;
                1) local tag="C++ headers"      ;;
                2) local tag="C++ source 1"     ;;
                3) local tag="C++ source 2"     ;;
                4) local tag="ancillary files"  ;;
            esac
            local pdf="$PDF_DIR/$OUTFILE$arg.pdf"
            if [ -f "$pdf" ]
            then
                report "$tag" "$PDFVIEWER $pdf &"
            else
                report "$tag NOT found" "$pdf"
            fi
        done

        report
        for arg in $(seq 0 4)
        do
            case "$arg" in
                0) local tag="complete"         ;;
                1) local tag="C++ headers"      ;;
                2) local tag="C++ source 1"     ;;
                3) local tag="C++ source 2"     ;;
                4) local tag="ancillary files"  ;;
            esac
            case "$arg" in
                0) local cnt=$size0  ;;
                1) local cnt=$size1  ;;
                2) local cnt=$size2  ;;
                3) local cnt=$size3  ;;
                4) local cnt=$size4  ;;
            esac
            local pdf="$PDF_DIR/$OUTFILE$arg.pdf"
            test -f "$pdf" && pdf_pages "$pdf"
            local buffer
            printf -v buffer "%4d files  %5d pages" $cnt $pdfpages
            report "$tag"  "$buffer"
        done

        report

        report "pdf info"        "(cd $PDF_DIR; echo; for pinf in $OUTFILE[0-9].pdf; do echo \"\$pinf\"; echo; pdfinfo \"\$pinf\"; echo; done)"
        report "pdf report"      "filereport $PDF_DIR/$OUTFILE[0-9].pdf"
        report "clean directory" "rm --force $PDF_DIR/*"

    )
}

# ---------------------------------
#  list_files
# ---------------------------------

function list_files
{
    echo
    report "files1" "$size1 header files"
    echo
    echo "$files1"
    echo
    report "files2" "$size2 implementation files"
    echo
    echo "$files2"
    echo
    report "files3" "$size3 implementation files"
    echo
    echo "$files3"
    echo
    report "files4" "$size4 ancillary files"
    echo
    echo "$files4"
    echo
    report "total"  "$size0 files"
}

# ---------------------------------
#  final_report()
# ---------------------------------

function final_report
{
    report
    report "final reporting"
    report
    test -n "$badens"   && report "139 enscripts" "${badens# }"
    test -n "$badwraps" && report "wrapped lines" "${badwraps# }"
    test -n "$badwraps" && report
    report "svn version"  "$SVN"
    report "mode"         "$mode"
    report "file loops"   "$count"
    elapsed=$SECONDS
    test $(which hms) && elapsed=$(hms $elapsed)      # 'hms' is a my (user-local) utility
    report "elapsed time" "$elapsed"
    report
}

# ---------------------------------
#  active code
# ---------------------------------

report
check_for_utilities || exit $E_MISSING_UTILITIES

case "$mode" in
    clean)
        clean_cores
        clean_dir "$PDF_DIR"
        ;;
    list)
        application_list
        ancillary_list
        size_list
        list_files
        ;;
    review)
        application_list
        ancillary_list
        size_list
        files=$(echo $files1 $files2 $files3 $files4)
        display_list
        check_dir "$PDF_DIR"
        ;;
    trial-1)
        view="yes"
        pdf_file_1 "./common.h"
        ;;
    trial-2)
        application_list
        ancillary_list
        size_list
        files=$(echo $files1 $files2 $files3 $files4)
        view="yes"
        count=999
        create_texi "0"
        process_texi
        ;;
    trial-3)
        reader_report
        ;;
    trial-4)
        svn_confirm
        ;;
    run)
        svn_confirm                     # a clean svn is essential
        application_list
        ancillary_list
        size_list
        files=$(echo $files1 $files2 $files3 $files4)
        display_list
        check_dir "$PDF_DIR"
        view="no"
        pdf_list
        for arg in $(seq 0 4)           # loops 0 then 1 thru 4
        do
            create_texi  "$arg"
            process_texi
            make_endpage "$arg"
            combine      "$arg"
        done
        reader_report
        ;;
esac

final_report

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $E_SUCCESS

#  $Id: pdfall.sh 5532 2010-11-22 18:45:13Z robbie $
#  end of file

