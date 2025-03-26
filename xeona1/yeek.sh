#! /bin/bash

#  file-purpose     : search for yeek conditionals in codebase
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 17-Oct-2011 19:23 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 7602 $
#  $Date: 2011-10-17 21:35:22 +0200 (Mon, 17 Oct 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/yeek.sh $

#  usage : $ ./yeek.sh
#
#  todo: add --help

# ---------------------------------
#  preamble
# ---------------------------------

UPPER=60                                # upper bound for yeek count

SCRIPT=$(basename "$0")                 # this script

stub=${SCRIPT%.sh}                      # strip any ".sh" extension
file="$stub.txt"                        # add ".txt"

# ---------------------------------
#  active code
# ---------------------------------

# clear file

if [ -f "$file" ]
then
    response="n"
    prompt="  $SCRIPT: hit 'y' to remove $file: "
    read -n 1 -p "$prompt" response     # "-n 1" is read one character and return
    case "$response" in
        y|Y)
            echo
            rm --force "$file"
            echo
            ;;
        *)
            echo "  $SCRIPT: quiting without action"
            exit 0
            ;;
    esac
fi

# loop yeeks

for ind in $(seq $UPPER)
do
    printf "  yeek %2d  : " $ind
    pat="xeona::yeek *== *$ind "
    xgrep "$pat" | grep "file hits"
done | tee "$file"

# report zero matches again

echo
echo "zeros"
echo
grep " 0$" "$file"
echo

# ---------------------------------
#  housekeeping
# ---------------------------------

exit 0

#  $Id: yeek.sh 7602 2011-10-17 19:35:22Z robbie $
#  end of file

