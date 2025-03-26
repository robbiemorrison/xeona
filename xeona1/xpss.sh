#! /bin/bash

#  file-purpose     : keep tabs on a 'xeona' build using 'ps' calls / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Tue 17-Nov-2009 14:25 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 4719 $
#  $Date: 2010-07-13 15:45:22 +0200 (Tue, 13 Jul 2010) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/xpss.sh $

function xpsx
{
    # ---------------------------------
    #  preamble
    # ---------------------------------

    # defaults
    local tries1=4                      # default
    local tries2=8                      # default
    local hms="hms"                     # my binary '/home/robbie/synk/bin/hms'
    local TAB=22                        # print alignment tab
    local delay=1

    # help message
    case "$1" in
        --help|--hel|--he|--h|-help|-hel|-he|-h|"-?")
            echo
            echo "      usage: xpsx  [tries]    run using tries attempts"
            echo "    purpose: keep tabs on a 'xeona' build using 'ps' calls"
            echo "  hardcodes: try attempts default = $tries1 $tries2"
            echo "    caution: not useful when the make utility runs with option --jobs=4"
            echo
            return 0
            ;;
    esac

    # ---------------------------------
    #  log()
    # ---------------------------------

    local logCount=0
    local lastTimestamp=0

    function log
    {
        # grab message
        local msg="$1"

        # create timestamp
        local timestamp=$SECONDS
        local elapsed=0
        printf -v elapsed "%03d" $timestamp
        test $( which $hms ) && elapsed=$( $hms $timestamp ) # CAUTION: for test, no -n and no soft-quotes

        # calculate time delta
        local delta=0
        let "delta = timestamp - lastTimestamp"
        lastTimestamp=$timestamp

        # update count
        let "logCount++"

        # print call
        printf "%02d  $elapsed  %3d  %*s\n" "$logCount" "$delta" "$TAB" "$msg"
    }

    # ---------------------------------
    #  active code
    # ---------------------------------

    # check for 'beep' utility
    test $(which beep) || echo "beep utility not found"

    # set delay by argument or by default
    local tries
    test -n "$1" && printf -v temp "%d" "$1" 2>/dev/null && tries=$temp
    test -n "$1" && tries1=$tries
    test -n "$1" && tries2=$tries
    echo "trip attempts : $tries1 $tries2"

    # pattern definitions
    local pat1="[a-z]/[a-z]*[0-9]*\.cc"
    local pat2="xeona\.[a-z]*[0-9]*"

    # normal variables
    local elapsed
    local ret1
    local ret2
    local match1="(not overwritten)"
    local match2="(not overwritten)"
    local prior1="(not overwritten)"
    local binary="(not overwritten)"

    # ---------------------------------
    #  main loop
    # ---------------------------------

    # control flow variables
    local phase="comp"
    local first2="true"
    local trip1=0
    local trip2=0

    while $(true)                       # always true
      do

      # ---------------------------------
      #  data harvest
      # ---------------------------------

      # ps calls - mutually exclusive
      match1=$( ps --format command -C "g++" | grep --only-matching "$pat1" )
      ret1=$?
      match2=$( ps --format command -C "g++" | grep --only-matching "$pat2" )
      ret2=$?

      # toggle phase if linking
      test $ret2 -eq 0 && {
          phase="link"
          binary="$match2"              # persistent value needed
      }

      # phase-dependent processing
      case "$phase" in

          # ---------------------------------
          #  compilation
          # ---------------------------------

          comp)
              # trip or loop code
              case "$ret1" in
                  0)                    # match found
                      trip1=0
                      case "$match1" in
                          $prior1)      # loop if no change
                              sleep 1
                              ;;
                          *)
                              log "$match1"
                              prior1="$match1"
                              sleep 1
                              ;;
                      esac
                      ;;
                  *)                    # match not found
                      if [[ $trip1 -ge $tries1 ]]  # CAUTION: double brackets necessary
                          then
                          phase="link"  # 'continue' not needed
                      else
                          let "trip1++"
#                         log "compile time trip set" $trip1
                          sleep 1
                      fi
                      ;;
              esac
              ;;

          # ---------------------------------
          #  linking
          # ---------------------------------

          link)
              # trip or loop code
              case "$ret2" in
                  0)                    # match found
                      trip2=0
                      # report first time
                      case "$first2" in
                          true)
                              test "$binary" == "(not overwritten)" || log "$binary"
                              first2="false"
                              ;;
                      esac
                      sleep 1
                      ;;
                  *)                    # match not found
                      if [[ $trip2 -ge $tries2 ]]  # CAUTION: double brackets necessary
                          then
                          break
                      else
                          let "trip2++"
                          first2="true" # TOFIX: confirm
#                         log "link time trip set" $trip2
                          sleep 1
                      fi
                      ;;
              esac
              ;;
      esac
    done

    # ---------------------------------
    #  final report
    # ---------------------------------

    log "complete"                      # , grep returns $ret1 $ret2\n"
    test $(which beep) && beep -f 2600

    # final return
    return 0
}

xpsx "$1"

#  $Id: xpss.sh 4719 2010-07-13 13:45:22Z robbie $
#  end of file

