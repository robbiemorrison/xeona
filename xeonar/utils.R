
#  file-purpose     : utility functions
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Thu 04-Jun-2009 19:55 UTC
#  file-status      : working
#  file-keywords    : xeona R

#  $Revision: 6005 $
#  $Date: 2011-02-25 19:12:42 +0100 (Fri, 25 Feb 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeonar/utils.R $

#  LEGAL NOTICE
#
#  Software  : This file is part of the source code for the xeona energy
#              systems modeling environment.
#  License   : This software is distributed under the GNU General Public
#              License version 3, a copy of which is provided in the text
#              file LICENSE_GPLv3.
#  Warranty  : There is no warranty for this software, to the extent permitted
#              by applicable law.  Refer to the license for further details.
#  Copyright : This software is copyright (c) 2007 - 2011 Robbie Morrison.
#  Request   : The software is distributed with the request that you forward
#              any modifications you make to the xeona project for possible
#              inclusion in the main codebase.
#
#  PROJECT CONTACT
#
#  Robbie Morrison
#  Institute for Energy Engineering
#  Technical University of Berlin
#  Marchstrasse 18, D-10587 Berlin, Germany
#  Email: robbie@actrix.co.nz

# ---------------------------------
#  function : xem.trim
# ---------------------------------
#  description  : trim space and/or tab characters from both ends of a given string
#  role         : utility
#  takes        : string (or vector of strings)
#  returns      : string (or vector of strings)
#  techniques   : 'sub'
#  status       : complete
#
#  terminology-wise, a normal string (in other
#  languages) is officially known as a "single
#  character string" and represented by a "length one
#  character vector"
#
# ---------------------------------

xem.trim <- function (s)
{
  if ( ! is.character(s) ) {
    warning("string vector expected")
  }
  s <- sub("^[[:blank:]]+", "", s)
  s <- sub("[[:blank:]]+$", "", s)
  return(s)
}

# ---------------------------------
#  function : xem.beep
# ---------------------------------
#  description  : predefined beep function
#  role         : utility
#  takes        : type in { alert error weird }
#  returns      : system exit
#  techniques   : 'beep'
#  status       : complete
#  CAUTION      : platform-specific (requires a 'beep')
#
#  this function requires either:
#
#    * the Linux package 'beep' - advanced pc-speaker beeper
#    * a substitute bash script, bash function, or bash alias
#
#  some troubleshooting calls:
#
#    $ which beep
#    $ type beep
#
# ---------------------------------

xem.beep <- function (type = "alert")
{
  switch(type,
         "alert" = beepcall <- "beep -f 2000 -r 1",
         "error" = beepcall <- "beep -f 300 -n -f 350 -n -f 450",
         "weird" = beepcall <- "beep -f 800 -n -f 1800 -n -f 1300 -n -f 900 -n -f 1100")
  retBeep <- system(beepcall)                # system call
}

# ---------------------------------
#  function : xem.linesOfCode
# ---------------------------------
#  description  : estimate source lines of code
#  role         : background
#  takes        : n/a
#  returns      : source lines of code
#  techniques   : 'cat' 'grep' 'wc'
#  status       : complete
# ---------------------------------

# $ cat --squeeze-blank *.R | grep --invert-match "etc" | wc --lines

xem.linesOfCode <- function ()
{
  # the escape on the '#' is to preserve emacs alignment rules
  cal <- character()
  cal <- append(cal, "cat --squeeze-blank *.R")
  cal <- append(cal, "grep --invert-match \"^[[:blank:]]*$\"")   # empty line
  cal <- append(cal, "grep --invert-match \"^[[:blank:]]*#\"")   # comment line
  cal <- append(cal, "grep --invert-match \"^[[:blank:]]*}$\"")  # single '}' line
  cal <- append(cal, "wc --lines")
  cat <- paste(cal, collapse = " | ")
  loc <- system(cat, intern = TRUE)
  loc                                        # additional statement required for visibility
}

# ---------------------------------
#  function : xem.svnver
# ---------------------------------
#  description  : recover current subversion version
#  role         : utility
#  takes        : n/a
#  returns      : svn version as integer or zero if unclean
#  techniques   : 'system' 'svnversion'
#  status       : complete
#  CAUTION      : Linux-specific (possibly)
#
#  the 'system' argument "intern = TRUE" setting is not
#  supported on all platforms -- but Linux should be
#  safe
#
#  "unclean" means uncommitted changes or out-of-sync working copy or both
#
#  CAUTION: the path '/home/robbie/xeona/svn2/futz' may
#  need adjusting.
#
# ---------------------------------

xem.svnver <- function ()
{
  message("  info : xem.svnver : calling 'svnversion' (can be slow)")
  on.exit(message("  info : xem.svnver : complete (on.exit), elapsed time ", elapsed, "s"))

  time    <- system.time(verstr <- system("svnversion /home/robbie/synk/xeona/svn2/futz", intern = TRUE))
  elapsed <- round(time[3], 0)

  prior   <- options(warn = -1)              # negative is suppress warnings, zero is store warnings
  ver     <- as.numeric(verstr)              # string to number coercion
  options(warn = prior$warn)                 # restore warnings
  if ( is.na(ver) ) ver <- 0                 # reset to zero if svn version is "unclean"
  message("  info : xem.svnver : xeona version : ", ver)

  invisible(ver)                             # return invisibly
}

# ---------------------------------
#  function : xem.preamble
# ---------------------------------
#  description  : parse subversion 'Id' ident and return file name (see example)
#  role         : not used
#  takes        : 'Id' ident line
#  returns      : file name
#  techniques   : 'strsplit'
#  status       : complete
#
#  example (replace DOLLAR with $)
#
#    DOLLARId: file.R 5113 2010-08-18 08:26:13Z robbie DOLLAR
#
# ---------------------------------

xem.preamble <- function (svnid)
{
  # CAUTION: a space char is needed between the
  # double-quotes and the svn:keyword 'Id' dollar chars
  # at both ends

  svnids <- strsplit(svnid, " ", fixed = TRUE)    # the first element is duly ""
  rfile  <- unlist(svnids)[3]
  rev    <- unlist(svnids)[4]
  invisible(rfile)                                # return invisibly
}

# ---------------------------------
#  function : xem.pyramidSort
# ---------------------------------
#  description  : undertakes what I called a "pyramid sort"
#  role         : used by 'xem.breadTraverse' to keep the arrow layering correct
#  takes        : 'numbers' number sequence, 'peak' which need not be in 'numbers'
#  returns      : pyramid sort of same length
#  note         : 'peak' is included once in the middle if it matches
#  techniques   : 'is.numeric' 'sort' 'c'
#  status       : complete
#
#  example
#
#     numbers :  1 3 7 8 9 12 13
#     peak    :  8
#     output  :  1 3 7 8 13 12 9
#
#  see Crawley (2007 pp21-22) for a discussion on logical
#  subscripts within vectors
#
# ---------------------------------

xem.pyramidSort <- function(numbers,
                            peak)
{
  if ( ! ( is.numeric(numbers) && is.numeric(peak) ) ) {
    warning("numeric arguments required")
    return(numbers)
  }

  if ( length(peak) != 1 ) {
    warning("peak must be a scalar ", length(peak))
    return(numbers)
  }

  buf <- sort(numbers)
  lo  <- buf[buf <= peak]
  hi  <- buf[buf >  peak]
  buf <- c(lo, rev(hi))
}

# ---------------------------------
#  function : xem.isTwoContained
# ---------------------------------
#  description  : test a given aggregate against a pure mask
#  role         : decoding aggregates
#  takes        : a 'pure' mask > 0 and an 'aggregate' test value >= 0, both integer-valued
#  returns      : logical if 2-contained
#  techniques   : integer-style (and not bitwise) arithmetic
#  status       : complete
#
#  examples
#
#      xem.isTwoContains(13, 4) returns  TRUE, reduced vector 0 8 4 0 1
#      xem.isTwoContains(11, 4) returns FALSE, reduced vector 0 8 0 2 1
#      xem.isTwoContains(4,  8) returns FALSE, reduced vector   0 4 0 0
#      xem.isTwoContains(0,  1) returns FALSE, reduced vector         0
#      xem.isTwoContains(13)    returns vector 0 8 4 0 1
#      xem.isTwoContains( 0)    returns vector         0
#      xem.isTwoContains()      returns NA
#
#  this function uses integer arithmetic, rather than
#  more elegant bitwise arithmetic
#
#  in comparison to C/C++, integer arithmetic in R a
#  somewhat opaque -- still the following code tested
#  out okay
#
#  note the following
#
#      is.integer(4)   returns FALSE
#      is.integer(4:4) returns TRUE
#
#  note in particular the '%%' 'modulus' operator
#  (known elsewhere as the 'modulo' and/or 'remainder'
#  operator), and the inconsistent convention for
#  negative inputs (although not used here) (see
#  wikipedia for details)
#
#  note also the 'bitopts' add-on package from CRAN --
#  probably useful if a bitwise rewrite is indicated
#
#    bitops - functions for bitwise operations on
#             integer vectors
#
#  modeled on 'xeona' C++ code for 'xeona::isTwoContained'
#  -- note the argument order is reversed
#
# ---------------------------------

xem.isTwoContained <- function (aggregate = 0,    # integer under investigation (0 or greater)
                               pure      = 1)     # mask as power of two (1 or greater)
{
  # return if 'aggregate' (and probably no arguments) given
  if ( missing(aggregate) ) return(NA)

  # choke if arguments out of range
  if ( pure < 1 || aggregate < 0 ) {
    message("  warn : xem.isTwoContained : either pure < 1 or aggregate < 0, values ", pure, " ", aggregate)
    warning("problematic arguments ", pure, " ", aggregate)
    return(FALSE)
  }

  # establish an upper limit power of two using the larger of 'pure' or 'aggregates'
  base <- 1
  max  <- max(aggregate, pure)
  while ( base < max ) base <- base * 2
  if ( DBUG > 2 ) message("base                : ", base)

  # fill vector using 'aggregate'
  reds <- integer()                          # vector
  lid  <- base                               # "lid"
  red  <- aggregate                          # scalar
  while ( lid >= 1 ) {
    if ( red >= lid ) {
      red <- red %% lid                      # modulus operator, remainder after division
      reds <- c(reds, lid)
    }
    else {
      reds <- c(reds, as.integer(0))
    }
    lid <- lid / 2                           # halving operation
    if ( DBUG > 2 ) message("lid just halved ", lid)
  }
  if ( DBUG > 1 ) { cat("reds (structure)    :"); str(reds) }

  # bail out when only 'aggregate' given
  if ( missing(pure) ) {
    return(reds)                             # CAUTION: 'pure' must remain unchanged to this point
  }

  # report
  if ( DBUG > 1 ) message("pure                : ", pure)
  if ( DBUG > 1 ) message("aggregate           : ", aggregate)

  # confirm pure
  ispure <- FALSE
  x      <- 1
  while ( x <= pure ) {
    if ( x == pure ) ispure <- TRUE
    x <- x * 2
  }

  # exit if not okay
  if ( ispure == FALSE ) {
    message("  warn : xem.isTwoContained : pure not pure ", pure)
    warning("pure not pure ", pure)
    return(FALSE)
  }
  else {
    if ( DBUG > 2 ) message("pure power of two ")
  }

  # hunt in vector
  hit <- match(pure, reds, nomatch = 0)
  ret <- as.logical(hit)

  if ( DBUG > 1 ) message("hit                 : ", hit)
  if ( DBUG > 1 ) message("ret                 : ", ret)

  # return
  ret
}

# ---------------------------------
#  function : tst.pyramidSort
# ---------------------------------

tst.pyramidSort <- function()
{
  numbers <- c(1, 3, 7, 8, 9, 12, 13)
  peak    <- 8
  output  <- xem.pyramidSort(numbers, peak)

  cat("numbers : ", numbers, "\n")
  cat("peak    : ", peak,    "\n")
  cat("output  : ", output,  "\n")
}

# ---------------------------------
#  function : tst.isTwoContained
# ---------------------------------

tst.isTwoContained <- function ()
{
  if ( DBUG > 1 ) cat("\n")
  xem.isTwoContained(13, 4)
  if ( DBUG > 1 ) cat("\n")
  xem.isTwoContained(11, 4)
  if ( DBUG > 1 ) cat("\n")
  xem.isTwoContained(0, 1)
  if ( DBUG > 1 ) cat("\n")
  xem.isTwoContained(123455, 2^16)           # 65536, TRUE
  if ( DBUG > 1 ) cat("\n")
  xem.isTwoContained(4.4, 4.4)
  if ( DBUG > 1 ) cat("\n")
  xem.isTwoContained(13, 5)
  if ( DBUG > 1 ) cat("\n")

  thirteen <- xem.isTwoContained(13)
  if ( DBUG > 1 ) cat("reduced vector      :", thirteen, "\n\n")

  zero <- xem.isTwoContained(0)
  if ( DBUG > 1 ) cat("reduced vector      :", zero, "\n\n")

  null <- xem.isTwoContained()
  if ( DBUG > 1 ) cat("reduced vector      :", null, "\n")
  if ( DBUG > 1 ) cat("\n")
}

# ---------------------------------
#  function : tst.utils
# ---------------------------------

tst.utils <- function (dbug)
{
  DBUG <<- dbug
  message("  * * * * * * * * * *\n  test : tst.utils : test commencing / DBUG = ", DBUG)

  str <- xem.trim("  abc def  ")
  xem.beep("alert")
  Sys.sleep(2)
  xem.beep("error")
  Sys.sleep(2)
  xem.beep("weird")
  xem.linesOfCode()
  xem.svnver()
  tst.pyramidSort()
  tst.isTwoContained()

  message("  test : tst.utils : test complete")
}

#  $Id: utils.R 6005 2011-02-25 18:12:42Z robbie $
#  end of file

