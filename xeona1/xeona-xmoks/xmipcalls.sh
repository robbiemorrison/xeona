#! /bin/bash

#  file-purpose     : extract MIP build calls from logging output / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Wed 21-Oct-2009 18:27 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 4715 $
#  $Date: 2010-07-13 13:48:44 +0200 (Tue, 13 Jul 2010) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/xeona-xmoks/xmipcalls.sh $

#  Improvement
#
#    Should capture output to temporary file and 'grep'
#    that, rather than running the model seven times!

# -------------------------
#  preamble
# -------------------------

XEONA="../xeona.mach"                   # 'xeona' binary
REPORT=4                                # report level (needs to be >= 4)
MODE=6                                  # mode (must be 6)

SCRIPT=$(basename $0)

E_SUCCESS=0
E_USAGE=2
E_NO_XEM=60
E_NO_BINARY=61

# -------------------------
#  display help
# -------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        echo
        echo "        usage: $SCRIPT  <xem>     process given model (stub name is acceptable)"
        echo "               $SCRIPT --help     show this message and exit"
        echo "      purpose: extract colorized MIP build calls from logging output"
        echo "   basic call: \$ $XEONA  --guard  --mode $MODE  --report $REPORT  --file <xem>"
        echo "      comment: currently wastefully employs multiple calls to the same xeona model"
        echo
        exit $E_SUCCESS
        ;;
    --|-|"")
        echo "$SCRIPT: try --help for usage"
        exit $E_USAGE
        ;;
esac

# ---------------------------------
#  grab and test xem name and binary
# ---------------------------------

xem="$1"
xem=${xem%.xem}                         # strip first extension
xem=${xem%.guard}                       # strip second extension

files=$( ls $xem* ) || {
    echo "$SCRIPT: xem file not found: $xem"
    exit $E_NO_XEM
}

test $( which $XEONA ) || {
    echo "$SCRIPT: binary not found or not executable: $XEONA"
    exit $E_NO_BINARY
}

# ---------------------------------
#  active code
# ---------------------------------

echo

# filter for load calls

echo "  MIP calls"
echo

declare -a patterns                     # list of 'grep' patterns

patterns=(
"load col"
"mark col"
"load row"
"load cof"
"incr col"
"load bnd")

xcal="$XEONA --guard --mode $MODE --report $REPORT --file $xem"
fcal="$xcal 2>&1 | grep --color"

let "last = ${#patterns[*]} - 1"        # CAUTION: array indexing is zero-based

for index in $( seq 0 $last )           # loop
do
  pattern="${patterns[$index]}"
  call="$fcal \"$pattern\""
  eval "$call" && echo                  # skip 'echo' if no match
done

# filter for interface balance specifically

echo "  connection calls repeated"
echo

pattern="iface-bal"
call="$fcal \"$pattern\""
eval "$call" && echo                  # skip 'echo' if no match

# obtain exit status

eval "$xcal 1>/dev/null 2>/dev/null"
ret=$?
interp=$( $XEONA --output $ret 2>/dev/null )

# ---------------------------------
#  completion reporting
# ---------------------------------

echo "---"
echo
echo "$SCRIPT: stub = $xem"
echo -n "$SCRIPT: list = "
echo $files
echo "$SCRIPT: call = $fcal \"(varies)\""
echo "$SCRIPT: info = ./xmole.sh $xem"
echo "$SCRIPT: exit = $ret = $interp"
echo

# ---------------------------------
#  housekeeping
# ---------------------------------

exit $E_SUCCESS

#  $Id: xmipcalls.sh 4715 2010-07-13 11:48:44Z robbie $
#  end of file

