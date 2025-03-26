
#  file-purpose     : run 'xeona' to generate or refresh data
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Tue 09-Jun-2009 16:23 UTC
#  file-status      : working
#  file-keywords    : xeona R

#  $Revision: 6005 $
#  $Date: 2011-02-25 19:12:42 +0100 (Fri, 25 Feb 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeonar/run.R $

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
#  function : xem.xemconf.model
# ---------------------------------
#  description  : simply checks for existence
#  role         : preparation
#  takes        : 'xemname' string
#  returns      : 'xemname' string
#  techniques   : 'file.exists'
#  status       : complete
# ---------------------------------

xem.xemconf.model <- function (xemname)
{
  # reporting
  message("  info : xem.xemconf.model : commencing")
  on.exit(message("  info : xem.xemconf.model : complete (on.exit)"))

  # report
  if ( ! file.exists(xemname) ) {
    message("  warn : xem.xemconf.model : xem file not present: ", xemname)
    warning("xem file not found ", xemname)
  }

  # return
  return(xemname)
}

# ---------------------------------
#  function : xem.xemconf.guard
# ---------------------------------
#  description  : cleans model name and checks for existence
#  role         : preparation
#  takes        : 'xemname' string
#  returns      : 'xemname' string duly cleaned
#  techniques   : 'sub', 'file.exists'
#  status       : complete
#
#  note that this program is currently limited
#  models with guard files
#
# ---------------------------------

xem.xemconf.guard <- function (xemname)
{
  # reporting
  message("  info : xem.xemconf.guard : commencing")
  on.exit(message("  info : xem.xemconf.guard : complete (on.exit)"))

  # process names
  xemname  <- sub("(|\\.xem)$"        ,       ".xem", xemname)   # add ".xem" extension if absent
  xemguard <- sub("(|\\.guard)\\.xem$", ".guard.xem", xemname)   # guard file name if not already
  xemmodel <- sub("\\.guard\\.xem$"   ,       ".xem", xemname)   # normal model file

  # report
  if ( ! file.exists(xemmodel)  ) {
    message("  info : xem.xemconf.guard : xem model file not present: ", xemmodel)
  }
  if ( ! file.exists(xemguard) ) {
    message("  warn : xem.xemconf.guard : xem guard file not present: ", xemguard)
    warning("xem guard file not found ", xemguard)
  }

  # return
  return(xemmodel)
}

# ---------------------------------
#  function : xem.xeona
# ---------------------------------
#  description  : invokes xeona
#  role         : a step in the call chain
#  takes        : 'xeona', 'xemname', optionally 'mode', optionally 'report' reporting level
#  returns      : 'xret' (this is buggy, see caution below)
#  side-effects : sets 'xcall'
#  techniques   : 'system'
#  status       : complete
#
#  CAUTION: some internal problem with non-zero system
#  return values, for instance ( system("false") )
#  yields 256 and not 1 as expected
#
#  CAUTION: cannot add 'grep' filters to the call
#  easily, without working tricks with the bash
#  'PIPESTATUS' builtin -- either that or run a
#  successful call twice
#
# ---------------------------------

xem.xeona <- function (xeona,
                       xemname,
                       mode     = 7,
                       report   = 0,
                       exittrip = 1,
                       nodata   = FALSE)
{
  # reporting
  message("  info : xem.xeona : commencing")
  on.exit(message("  info : xem.xeona : complete (on.exit), xret ", xret))

  # preparation
# xemname <- sub("\\.xem$", "", xemname)     # trim trailing ".xem", strictly optiona1

  # preamble
  xopt  <- character()
  xopt  <- append(xopt, paste("--guard"))
# xopt  <- append(xopt, paste("--pepper"))   # leads to testing instabilities
  xopt  <- append(xopt, paste("--mode"    , mode))
  xopt  <- append(xopt, paste("--report"  , report))
  xopt  <- append(xopt, paste("--exittrip", exittrip))
  if ( nodata ) xopt <- append(xopt, paste("--nodata"))
  xopt  <- append(xopt, paste("--file", xemname))
  xopts <- paste(xopt, collapse = "  ")      # double space is correct
  xcal  <- paste(xeona, xopts, sep = "  ")   # double space is correct
  message("xeona call     : ", xcal)

  # rule
  rule <- paste("  ", paste(rep("=", 90), collapse = ""), "\n",sep = "")

  # return value
  xret <- NA                              # should be overwritten

  # system call
  if ( report == 0 ) {
    redir <- "1>/dev/null 2>/dev/null"
    call  <- paste(xcal, redir, sep = " ")
    xret  <- system(call, intern = FALSE, ignore.stderr = FALSE)
  }
  else if ( report < 3 ) {
    cat(rule)
    redir <- "1>/dev/null"
    call  <- paste(xcal, redir, sep = " ")
    xret  <- system(call, intern = FALSE, ignore.stderr = FALSE)
    cat(rule)
  }
  else {
    cat(rule)
    call  <- paste(xcal, sep = " ")
    xret  <- system(call, intern = FALSE, ignore.stderr = FALSE)
    cat(rule)
  }

  # set the exported call string
  xcall  <<- xcal

  # return
  return(xret)
}

# ---------------------------------
#  function : xem.freshen
# ---------------------------------
#  description  : run xeona binary on XEM file in guard mode
#  role         : ensure the nominated XEM file is fresh
#  takes        : 'xeona' binary, 'xemname' filename, 'report' reporting level
#  returns      : best achieved mode (7 thru 0, 7 is ideal, 0 means even mode 1 failed)
#  techniques   : 'super-assignment operator'
#  status       : complete
#
#  the various modes are described in 'xeona' option '--usage'
#
#  the returned 'mode' variable may be used to control
#  future actions, for instance, timeseries from mode 3
#  and less are unlikely to work -- this variable is
#  also used for labeling plots
#
# ---------------------------------

xem.freshen <- function (xeona,              # binary name
                         xemname,            # XEM model name
                         report = 0)
{
  # reporting
  message("  info : xem.freshen : commencing")
  on.exit(message("  info : xem.freshen : complete with mode (on.exit) ", mode))

  # get some meta info
  xemguard <- sub("(|\\.guard)\\.xem$", ".guard.xem", xemname)
  data     <- xem.slurp(xemguard)            # temporary slurp
  xtrip   <<- as.numeric(xem.scanForInValue(data, "program.run-script-settings.script-option-exittrip"))
  xnodata <<- as.logical(as.numeric(xem.scanForInValue(data, "program.run-script-settings.script-option-nodata")))

  # step backward thru modes
  modes <- c(7, 6, 5, 4, 3, 2, 1)            # deep to shallow
  for ( mode in modes ) {
    ret <- xem.xeona(xeona, xemname, mode, report, xtrip, xnodata)
    if ( ret == 0 ) break                    # success
    mode <- 0                                # mode 0 indicates that even mode 1 failed
  }

  # abandon on mode zero
  if ( mode == 0 ) {
    stop("modes ", modes ," failed (the xem file may be missing)")
  }

  # return
  return(mode)
}

# ---------------------------------
#  function : xem.slurp
# ---------------------------------
#  description  : open file and read in XEM file
#  role         : obtain data
#  takes        : 'xemname' filename
#  returns      : read-in line-oriented data
#  techniques   : 'readLines'
#  status       : complete
# ---------------------------------

xem.slurp <- function (xemname)
{
  # reporting
  message("  info : xem.slurp : commencing with model file = ", xemname)
  on.exit(message("  info : xem.slurp : complete (on.exit)"))

  # active code
  data  <- readLines(xemname)                # default is to read all
  lines <- length(data)
  message("lines read     : ", lines)
  if ( lines == 0 ) warning("no lines read")

  # return
  invisible(data)
}

# ---------------------------------
#  function : xem.getRunPolicy
# ---------------------------------
#  description  : obtain run policy by regex scanning the data
#  role         : not used
#  takes        : read-n data
#  returns      : run policy
#  techniques   : 'grep', 'as.integer'
#  status       : mostly complete but not tested
#
#  caution: better to use 'xem.scanForValue'
#
# ---------------------------------

xem.getRunPolicy <- function (data)          # unprocessed xem data
{
  trigger <- "r-policy"
  regex   <- paste("^[[:blank:]]*", trigger, "[[:blank:]]*", ">", sep = "")
  hit     <- grep(regex, data)
  len     <- length(hit)
  if ( len == 0 ) {
    message("  ")
    ret = NA
  }
  else if ( len == 1 ) {                     # looking good ..
    policy <- strsplit(">", data[hit])[2]    # grab value
    prior  <- options(warn = WARNSET)        # negative is suppress warnings, zero is store warnings
    ret    <- as.integer(policy)             # NA on failure
    options(warn = prior$warn)               # restore warnings
    if ( is.na(ret) ) {
      message("  ")
    }
  }
  else {
    message("  ")
    ret = NA
  }
  return(ret)
}

# ---------------------------------
#  function : xem.scanForInValue
# ---------------------------------
#  description  : finds field value in unprocessed data
#  role         : any value recovery prior to loading the nested data-structure
#  takes        : slurped 'data' and fully-resolved 'fieldname'
#  returns      : sought value, else 'NULL' if not found or 'NA' if arguments fail integrity checks
#  techniques   : nested 'if' statements (ugly)
#  status       : close-to-complete
#
#  example
#
#    r.policy <- xem.scanForInValue(data,
#                                 "program.r-processing.r-policy")
#
# ---------------------------------

xem.scanForInValue <- function (data,        # unprocessed xem data
                                fieldname)   # fully-resolved field name
{
  # split 'fieldname'
  dotseps <- unlist(strsplit(fieldname, ".", fixed = TRUE))

  # integrity checks
  if ( length(dotseps) != 3 ) {              # three part split expected
    return(NA)
  }
  if ( sum(nchar(data)) == 0 ) {             # xem data character count is zero
    return(NA)
  }

  # further processing
  reckind <- dotseps[1]                      # in { program, entity, ... }
  id      <- dotseps[2]                      # trailing separator, which may or may not be hardcoded in 'xeona'
  fname   <- dotseps[3]                      # field name

  dotsep  <- paste(dotseps, collapse = ".")  # for reporting

  # main loop
  procFlag <- FALSE                          # flag to enable further processing of the current record
  for ( i in 1:length(data) )
    {
      line <- data[i]                        # grab next line

      pat <- paste("^", reckind, ".", id, sep = "")
      if ( regexpr(pat, line) < 1                      # a hit for "^reckind.id"
           ||                                          # .. or ..
           procFlag == TRUE  )                         # the current record has been selected
        {
          if ( regexpr("^[[:alpha:]#]", line) > 0 )    # meaning the next (and possibly disabled) record encountered
            {
              procFlag <- FALSE                        # unselect this record
            }
          else
            {
              procFlag <- TRUE                         # select this record

              inseps <- unlist(strsplit(line, ">", fixed = TRUE))

              if ( length(inseps) == 2 )               # meaning in-data encountered
                {
                  left  <- xem.trim(inseps[1])
                  right <- xem.trim(inseps[2])
                  if ( regexpr(fname, left, fixed = TRUE) > 0 )  # a hit for the field name
                    {
                      message("  info : xem.scanForValue : value encountered for in-field '", dotsep, "' of ", right )
                      return(right)
                    }
                }
            }
        }
    }
  message("  warn : xem.scanForValue : NO value encountered for ", dotsep)
  return(NULL)
}

# ---------------------------------
#  junk
# ---------------------------------
#
#  filt <- "" #"| grep \"^[[:blank:]]*[[:digit:]]+[[:print:]]+WARN\""

#  $Id: run.R 6005 2011-02-25 18:12:42Z robbie $
#  end of file

