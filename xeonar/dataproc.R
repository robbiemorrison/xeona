
#  file-purpose     : build nested dataset from xem file data
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Thu 04-Jun-2009 19:55 UTC
#  file-status      : working
#  file-keywords    : xeona R

#  $Revision: 6005 $
#  $Date: 2011-02-25 19:12:42 +0100 (Fri, 25 Feb 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeonar/dataproc.R $

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

#  OVERVIEW
#
#  This code uses a nested list structure with named
#  components.  In pseudo-UML:
#
#                *             *            1
#    recset <>---- record <>---- field <>---- value
#    |             |             |
#    + modelend    + enabled     + enabled
#                  + kind        + kind
#
#      where * indicates a multiplicity of 0..*
#
#  Non-mandatory components, which arise from the
#  optional xem field remark duly split (see code for
#  details), are omitted from the above diagram.
#
#  Metadata, which is added here, is also omitted from
#  the above diagram.
#
#  Individual sub-objects can be dereferenced as
#  follows, with '$' being the R "list subset"
#  operator:
#
#    x <- varname  $ identifier  $ fieldname  $ ..
#
#  Specifically, the 'recset' is a workspace variable
#  referred to by a symbol in the normal way.
#
#  The 'record' component is named using its
#  'identifier' with the dot-separated prefix
#  ("program."  or "entity.") duly removed.  The record
#  'kind' can be {"program", "entity"} and thus retains
#  this information.
#
#  The field 'kind' can be {"in", "out"}.
#
#  The record and field 'enabled' are {TRUE, FALSE}, as
#  are the recset 'modelend'
#
#  The 'value' may be -- in terms of R -- of type
#  logical, string, or double (with classic ints
#  treated as double) and may be a single element
#  vector (a scalar) or a multi-element vector.
#
#  ACCESS
#
#  Access a recset/record/field/value as follows (and
#  note single or double quotes or back-ticks required
#  for names containing '-' chars):
#
#    > rrfv <- rs$"entity-1"$"field-ts"$value
#    > rrfv <- getValue(rs, "entity-1", "field-ts")
#
#  CODING ISSUES
#
#  Regarding the recursive indexing of nested lists,
#  see:
#
#    > ?Extract
#
#  In particular, '[' omits the component name (if
#  present), whereas '[[' does not.  Moreover, '[[' and
#  '$' can only select a single element, whereas '['
#  can select several (by submitting an integer
#  vector).  In addition, both '[' and '[[' accept
#  computed indexes (via symbols), whereas '$' does
#  not.  In most environments, '[[' and '$' allow
#  partial (first encountered) matching.
#
#  Component names are forced onto list components
#  using the 'names' call, see:
#
#    > ?names

# ---------------------------------
#  debug settings
# ---------------------------------

# 'WARNSET' is used in 'as.numeric' lexical cast calls
# negative is suppress warnings, zero is store warnings

WARNSET <- -1

# ---------------------------------
#  function : xem.delist
# ---------------------------------
#  description  : unlist a list or timeseries raw string
#  role         : data processing
#  takes        : quote-separated list or timeseries as raw string
#  returns      : string vector
#  techniques   : 'strsplit'
#  status       : complete
#
#  the 'xeona' "pairwise delimiter for string values"
#  is assumed to be defined as a double quote
#
#  yields NA if 'rawString' is empty, that is, no
#  pairwise delimiters are present -- hence the minimum
#  sensible input is the string "\"\""
#
#  examples
#
#    c("")                            gives   NA
#    c("\"\"")                        gives   c("")
#    c("\"one two\"")                 gives   c("one two")
#    c("\"one two\" \"eins zwei\"")   gives   c("one two", "eins zwei")
#
# ---------------------------------

xem.delist <- function (rawString)
{
  # define the split regex
  sep <- "\"[[:blank:]]*\""                  # CAUTION: note the * (and not +) repetition operator

  # integrity check
  if ( ! is.character(rawString) ) {
    message("  warn : xem.delist : 'rawString' argument is not of type character (structure follows)")
    warning("'rawString' argument is not of type character")
    print(str(rawString))
  }

  # main code
  buff      <- unlist(strsplit(rawString, split = sep))     # CAUTION: 'unlist' is essential
  len       <- length(buff)
  buff[1]   <- substr(buff[1]  , 2, nchar(buff[1])      )   # trim leading quote
  buff[len] <- substr(buff[len], 1, nchar(buff[len]) - 1)   # trim trailing quote

  # return
  buff
}

# ---------------------------------
#  function : xem.unitsThintRemark
# ---------------------------------
#  description  : splits a " [-] x remaining remark" string
#  role         : recset building
#  takes        : single string
#  returns      : string vector (unnamed)
#  techniques   : 'grep'
#  status       : complete
#
#  expected format (optional) : [-] x remaining remark
#  interpretation             : units / type-hint / remaining-remark
#
#  multiple spaces in the remaining remark are
#  collapsed into one (think of this as a feature)
#
# ---------------------------------

xem.unitsThintRemark <- function (remain)
{
  output <- c(NA,                            # optional units
              NA,                            # optional type hint
              NA)                            # optional remaining remark

  sep    <- "[[:blank:]]+"                   # one or more space and/or tab characters
  buffer <- unlist(strsplit(remain, sep))

  # process optional units field
  if ( length(grep("^\\[.+\\]$", buffer[1])) )
    {
      # pop buffer
      output[1] <- buffer[1]
      buffer    <- buffer[-1]
    }

  # process optional type hint field
  if ( length(grep("^[biflsxBIFLSX]$", buffer[1])) )
    {
      # pop buffer
      output[2] <- buffer[1]
      buffer    <- buffer[-1]
    }

  # process optional remaining remark
  if ( length(buffer) )
    {
      output[3] <- paste(buffer, sep = " ", collapse = " ") # CAUTION: string not vector required
    }

  # return
  output
}

# ---------------------------------
#  function : xem.loop
# ---------------------------------
#  description  : contains main processing loop
#  role         : build call
#  takes        : line-oriented xem model 'data'
#  returns      : equivalent 'recset' data structure
#  techniques   : nested named lists (multi-type vectors)
#  status       : complete
#
#  acceptable construct: "if ( length(grep(..)) )"
#  alternative: "if ( any(i <- grep(..)) )" # the 'i' vector can then be used
#
# ---------------------------------

xem.loop <- function (data,
                      xemscope = 1:length(data))
{
  # reporting
  message("  info : xem.loop : commencing")
  on.exit(  message("  info : xem.loop : complete (on.exit)"))
  if ( DBUG > 0 ) message()

  # nested datatype
  recset <- list(modelend = FALSE)

  # separators
  sepA <- "\\."                              # dot char in regex format
  sepB <- "[<>]"                             # either a '<' or a '>' character
  sepC <- "[[:blank:]]+"                     # one or more space and/or tab characters

  # associated count variables
  rcnt  <- length(recset)                    # record counter
  fcnt  <- 0                                 # field counter (should be reset)

  # variables for completion reporting
  records     <- 0
  entitys     <- 0
  disEntitys  <- 0
  fields      <- 0
  disFields   <- 0
  blanks      <- 0
  comments    <- 0
  loops       <- 0

  # meta data
  ids  <- NULL

  # loop
  for ( i in xemscope )
    {
      loops  <- loops + 1
      line   <- data[i]
      line   <- xem.trim(line)
      if ( DBUG >  2 ) if ( nchar(line) > 0 ) message("LINE ", line)

      # OPTION: a blank line
      if ( 0 == nchar(line) )
        {
        blanks  <- blanks + 1
      }

      # OPTION: the end of the model
      else if ( length(grep("^model-end", line)) )
        {
          recset[["modelend"]] <- TRUE
          break
        }

      # OPTION: a record
      else if ( length(grep("^(#[[:blank:]]*|)(program|entity)\\.", line)) )
        {
          # identify and remove any column zero disable char
          if ( charmatch("#", line, nomatch = FALSE) ) {
            line <- substr(line, 2 , nchar(line))      # strip disable char
            line <- xem.trim(line)                     # trim any leading spaces
            enabled <- FALSE
            records <- records + 1
          }
          else {
            enabled <- TRUE
            records <- records + 1
          }

          # process core information
          two      <- unlist(strsplit(line, sepA))     # 'unlist' transforms "list" to "character"
          entid    <- two[2]

          if      ( length(grep("program\\.", line)) ) kind <- "program"
          else if ( length(grep("entity\\." , line)) ) kind <- "entity"
          else warning("coding error 01")

          if ( kind == "entity" ) {
            if ( enabled ) entitys    <- entitys    + 1
            else           disEntitys <- disEntitys + 1
            ids[length(ids) + 1]      <- entid         # meta information
          }

          # load
          record   <- list(enabled = enabled,
                           kind    = kind)

          rcnt     <- rcnt + 1
          fcnt     <- length(record)                   # reset the field counter appropriately

          recset[[rcnt]]      <- record                # insert record stub
          names(recset)[rcnt] <- entid                 # associate name

          if ( DBUG > 0 ) message("+ record push: ", names(recset)[rcnt])
        }

      # OPTION: a field
      else if ( length(grep(sepB, line)) )
        {
          # identify and remove any leading disable char
          line <- xem.trim(line)
          if ( charmatch("#", line, nomatch = FALSE) ) {
            line <- substr(line, 2 , nchar(line))       # strip disable char
            enabled   <- FALSE
            disFields <- disFields + 1
          }
          else {
            enabled   <- TRUE
            fields    <- fields + 1
          }

          # process core information
          two      <- unlist(strsplit(line, sepB))     # 'unlist' transforms "list" to "character"
          left     <- xem.trim(two[1])                 # recover left hand part and trim
          lefts    <- unlist(strsplit(left, sepC))     # split
          fname    <- lefts[1]                         # grab first element
          fremain  <- lefts[-1]                        # grab the rest

          if      ( length(grep(">", line)) ) kind <- "in"
          else if ( length(grep("<", line)) ) kind <- "out"
          else warning("coding error 02")

          output   <- xem.unitsThintRemark(fremain)    # local function
          funits   <- output[1]
          fthint   <- output[2]
          fremark  <- output[3]

          right    <- xem.trim(two[2])                 # recover right hand part and trim
          buffer1  <- unlist(strsplit(right, sepC))    # split

          prior    <- options(warn = WARNSET)          # negative is suppress warnings, zero is store warnings
          buffer2  <- as.numeric(buffer1)              # string to number coercion
          options(warn = prior$warn)                   # restore warnings

          # process right data
          if ( fname == "class" ) {                    # class value is never given in quotes
            fvalues <- right                           # revert
          }
          else if ( any(is.na(buffer2)) ) {            # the lexical cast failed
            fvalues <- xem.delist(right)               # split string timeseries but NOT entity lists
          }
          else {
            fvalues <- buffer2                         # accept the lexical cast
          }

          if ( DBUG >  1 ) {
            message("fremain (one)  : ", toString(fremain))
            message("fvalues        : ", fvalues)
          }

          # load
          fcnt      <- fcnt + 1
          record    <- recset[[rcnt]]
          field     <- list(enabled = enabled,
                            kind    = kind,
                            units   = funits,
                            thint   = fthint,
                            remark  = fremark,
                            value   = fvalues)

          record[[fcnt]]      <- field
          names(record)[fcnt] <- fname
          recset[[rcnt]]      <- record

          if ( DBUG > 1 ) message("  - info       (", rcnt, "/", fcnt,"): ", paste(fname, fremain, right))
          if ( DBUG > 0 ) message("  + field push (", rcnt, "/", fcnt,"): ", names(record)[fcnt])
        }

      # OPTION: a comment
      else                                             #  by process of elimination
        {
          comments <- comments + 1
        }
    }

  # meta data lead-up
  timestamp <- format(Sys.time(), "%d-%b-%Y %H:%M:%S")

  # add meta data
  rcnt                <- rcnt + 1
  recset[[rcnt]]      <- list(timestamp = timestamp,
                              entitys   = entitys,
                              ids       = ids)
  names(recset)[rcnt] <- "metadata"                    # associate name

  if ( DBUG > 0 ) message("+ metadata push: ", names(recset)[rcnt])
  if ( DBUG > 0 ) message()

  # completion reporting

  message("entities       : ", sprintf("%4d / %d", entitys, disEntitys))
  message("fields         : ", sprintf("%4d / %d", fields , disFields))
  message("comments       : ", sprintf("%4d", comments))
  message("blanks         : ", sprintf("%4d", blanks))
  message("loops          : ", sprintf("%4d", loops))

  if ( recset$"modelend" == TRUE  ) message("model end      : found")
  else                              message("model end      : not encountered")
  if ( recset$"modelend" == FALSE ) warning("no model end marker found")

  # final return
  invisible(recset)
}

# ---------------------------------
#  function : tst.delist
# ---------------------------------

tst.delist <- function ()
{
  inputs <- c("",
              "\"\"",
              "\"one two\"",
              "\"one two\" \"eins zwei\" \"alpha beta\"")
  for ( i in 1:length(inputs) )
    {
      input  <- inputs[i]
      output <- xem.delist(input)
      cat(sprintf("  test : %-40s  %-40s  %3d\n", input, output[1], length(output)))
    }
}

# ---------------------------------
#  function : tst.dataproc
# ---------------------------------

tst.dataproc <- function (xemfile,
                          dbug)
{
  DBUG <<- dbug
  message("  * * * * * * * * * *\n  test : data processing test (largely hollow) commencing / DBUG = ", DBUG)

  tst.delist()

  message("  test : data processing test complete")
}

# ---------------------------------
#  junk
# ---------------------------------

#   # CAUTION: line ranges refer to the processed XEM file
#   xemscope <- 50:60                          # just entity of class 'Overseer'
#   xemscope <- 76:99                          # just entity of class 'CxAmbientAirSim'

#  $Id: dataproc.R 6005 2011-02-25 18:12:42Z robbie $
#  end of file

