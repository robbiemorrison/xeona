#! /bin/bash

#  file-purpose     : help tune the new AC transmission
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Thu 16-Feb-2012 19:39 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9141 $
#  $Date: 2012-02-29 01:31:30 +0100 (Wed, 29 Feb 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/pocmods/actune.sh $

# ---------------------------------
#  user modifiable
# ---------------------------------

POC="008"
target="poc.$POC.guard.xem"
old=".sed"

# ---------------------------------
#  trips
# ---------------------------------

#  was="    length \[m\] f                                  > 50e+03"
#  now="    length [m] f                                  > 200e+03"

#  was="    capacity \[W\] f                                > 400e+06"
#  now="    capacity [W] f                                > 800e+06"

#  was="    resistance-per-metre \[ohm/m\] f                > 67e-06"
#  now="    resistance-per-metre [ohm/m] f                > 38e-06"

#  was="    reactance-per-metre \[ohm/m\] f                 > 364e-06"
#  now="    reactance-per-metre [ohm/m] f                 > 184e-06"

#  was="    reactance-per-metre \[ohm/m\] f                 > 184e-06"
#  now="    reactance-per-metre [ohm/m] f                 > 250e-06"

#  was="    voltage-angle-delta-upper \[degrees\] f         > 90.0"
#  now="    voltage-angle-delta-upper [degrees] f         > 45.0"

#  was="    discretization-steps \[-\] i                    > 5"
#  now="    discretization-steps [-] i                    > 10"

#  was="    discretization-steps \[-\] i                    > 10"
#  now="    discretization-steps [-] i                    > 5"

# ---------------------------------
#  preamble
# ---------------------------------

SCRIPT=$(basename "$0")                 # name of this script

E_SUCCESS=0
E_FAILURE=1
E_USAGE=2

# ---------------------------------
#  help
# ---------------------------------

case "$1" in
    --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
        echo
    cat << EOM
       usage: $SCRIPT            run script
              $SCRIPT  --help    display this message and exit
     purpose: en-masse modify current proof-of-concept model
EOM
    echo
    exit $E_SUCCESS
    ;;
esac

# ---------------------------------
#  active code
# ---------------------------------

# protection

test -z "$was" && exit 1
test -z "$now" && exit 1

# sed -- note the ":" separator because 'was' and 'now' may contain "/"

call="sed --in-place='"$old"' '"s:^$was\$:$now:"' $target"

echo
echo "target : $target"
echo "call   : $call"
eval "$call"

# diff

echo
sdiff --suppress-common-lines $target$old $target

# house keeping

rm --force $target$old

# finish up

echo
echo "---"
echo "trip   : $was"
echo "change : $now"
echo

#  $Id: actune.sh 9141 2012-02-29 00:31:30Z robbie $
#  end of file

