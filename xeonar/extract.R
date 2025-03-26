
#  file-purpose     : extract data and plot timeseries
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Fri 05-Jun-2009 08:01 UTC
#  file-status      : working
#  file-keywords    : xeona R

#  $Revision: 6005 $
#  $Date: 2011-02-25 19:12:42 +0100 (Fri, 25 Feb 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeonar/extract.R $

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

#  SUPPLIES
#
#      xem.summarize
#
#      xem.getPolicy
#      xem.getPlotList
#
#      xem.getSteps
#      xem.getInterval
#      xem.getClass
#      xem.getKind
#      xem.getUnits
#      xem.getValue
#
#      xem.getCounts
#      xem.examineEntity     (incomplete)
#      xem.plotValue
#      xem.plotAllTs
#
#      tst.extract

# ---------------------------------
#  function : xem.summarize
# ---------------------------------
#  description  : summarizes dataset using a plot frame
#  role         : job 1 point of contact
#  takes        : 'recset'
#  returns      : data (with any formatting blanks removed)
#  techniques   : 'text'
#  status       : complete
# ---------------------------------

xem.summarize <- function (recset)
{
  # reporting
  message("  info : xem.summarize : commencing")
  on.exit(message("  info : xem.summarize : complete (on.exit)"))

  # overall plot
  plot.new()
  title(xtitle)
  mtext("model summary")

  # dedicated 'load' function
  i <- 0                                     # declared to prevent 'i' from leaking further
  load <- function(tag = "", value = "")
    {
      i         <<- length(tags) + 1
      tags[i]   <<- tag
      values[i] <<- as.character(value)
    }

  # specify the summary data
  tags   <- character()
  values <- character()

  if ( is.null(xeona) ) xeona <- "na"        # correction for when 'xeona' is NULL

  load("xeona"                , xeona)
  load("xemfile"              , xemfile)
  load("xeona call"           , xcall)
  load()
  load("jobs"                 , paste(jobs, " = ", jobMsgs))
  load("xeona best mode"      , xmode)
  load("model end"            , recset$"modelend")     # logical
  load("svn"                  , xver)
  load()
  load("steps"                , xem.getSteps(recset))
  load("interval"             , xem.getInterval(recset))
  load()
  load("enabled entities"     , xem.getCounts(recset)["enabled-entitys"])
  load("disabled entities"    , xem.getCounts(recset)["disabled-entitys"])
# load("overall data"         , xem.getCounts(recset)["value-fields"])
  load("timeseries data"      , xem.getCounts(recset)["timeseries-fields"])
  load("singles data"         , xem.getCounts(recset)["single-fields"])

  # adjusment and plotting
  hinch <- par("csi")                        # character height in inches
  winch <- max(strwidth(tags))
  v0 <- (0.22 * hinch * 1:i) + 0.3           # final is bottom up adjust
  h1 <-  0.0                                 # value is rightwards adjust
  h2 <-  h1 + 0.06 + winch
  text(h1, v0, rev(tags)  , adj = c(0, 0), xpd = TRUE, font = 2)
  text(h2, v0, rev(values), adj = c(0, 0), xpd = TRUE)

  # export data
  namedvec        <- values
  names(namedvec) <- tags
  namedvec        <- namedvec[names(namedvec) != ""]   # skip the blank line calls on export

  # return
  invisible(namedvec)
}

# ---------------------------------
#  function : xem.splitFieldName
# ---------------------------------

xem.splitFieldname <- function (fieldname,
                                prefix = "entity")
{
  parts <- unlist(strsplit(fieldname, split = ".", fixed = TRUE))
  if ( length(parts) == 0 ) {
    message("  warn : xem.splitFieldname : empty field name")
    warning("empty field name")
    parts <- NA
  }
  else if ( parts[1] != prefix ) {
    message("  warn : xem.splitFieldname : field name without required qualifier")
    warning("field name without required qualifier '", fieldname, "'")
    parts <- NA
  }
  else if ( length(parts) != 3 ) {
    message("  warn : xem.splitFieldname : malformed field name")
    warning("malformed field name '", fieldname, "'")
    parts <- NA
  }
  return(parts)
}

# ---------------------------------
#  function : xem.getPolicy
# ---------------------------------

xem.getPolicy <- function (recset)
{
  policy <- recset$"r-processing"$"r-policy"$value
  as.integer(policy)                         # already cast to numeric and known to be an integer
}

# ---------------------------------
#  function : xem.getPlotList
# ---------------------------------

xem.getPlotList <- function (recset)
{
  recset$"r-processing"$"r-plot-list"$value
}

# ---------------------------------
#  function : xem.getSteps
# ---------------------------------

xem.getSteps <- function (recset)
{
  xem.getValue(recset, "time-horizon", "steps")
}

# ---------------------------------
#  function : xem.getInterval
# ---------------------------------

xem.getInterval <- function (recset)
{
  xem.getValue(recset, "time-horizon", "interval")
}

# ---------------------------------
#  function : xem.getClass
# ---------------------------------

xem.getClass <- function (recset,
                          identifier)
{
  xem.getValue(recset, identifier, "class")
}

# ---------------------------------
#  function : xem.getKind
# ---------------------------------

xem.getKind <- function (recset,
                         identifier,
                         fieldname)
{
  recset[[identifier]][[fieldname]]$kind
}

# ---------------------------------
#  function : xem.getUnits
# ---------------------------------

xem.getUnits <- function (recset,
                          identifier,
                          fieldname)
{
  recset[[identifier]][[fieldname]]$units
}

# ---------------------------------
#  function : xem.getValue
# ---------------------------------

xem.getValue <- function (recset,
                          identifier,
                          fieldname)
{
  recset[[identifier]][[fieldname]]$value
}

# ---------------------------------
#  function : xem.getCounts
# ---------------------------------

xem.getCounts <- function (recset)
{
  # grab names directly
  entitys <- recset$"metadata"$"ids"

  # define counters
  yesEntitys    <- 0                         # not counting 'entity.time-horizon'
  noEntitys     <- 0
  cntValues     <- 0
  cntTimeseries <- 0
  cntSingles    <- 0
  cntNA         <- 0                         # should normally remain zero

  # traverse dataset
  for ( i in 1:length(entitys) )
    {
      id     <- entitys[i]
      entity <- recset[[id]]                 # grab entity
      if ( id == "time-horizon" ) next
      if ( entity[["enabled"]] == TRUE ) yesEntitys = yesEntitys + 1
      else                                noEntitys =  noEntitys + 1

      for ( j in 1:length(entity) )
        {
          field <- entity[[j]]               # grab field
          if ( length(field) != 6 ) next
          value <- field[["value"]]
          cntValues                                    <- cntValues     + 1
          if      ( any(is.na(value))  ) cntNA         <- cntNA         + 1
          else if ( length(value)  > 1 ) cntTimeseries <- cntTimeseries + 1
          else if ( length(value) == 1 ) cntSingles    <- cntSingles    + 1
          else {
            warning("strange value ", value)  # defensive coding
          }
        }
    }

  # load data
  data <- numeric()
  data <- c(data, "enabled-entitys"   = yesEntitys)
  data <- c(data, "disabled-entitys"  =  noEntitys)
  data <- c(data, "value-fields"      = cntValues)
  data <- c(data, "timeseries-fields" = cntTimeseries)
  data <- c(data, "single-fields"     = cntSingles)

  # return
  invisible(data)
}

# ---------------------------------
#  function : xem.examineEntity
# ---------------------------------

xem.examineEntity <- function (recset,
                               identifier)
{
  x <- recset[[identifier]]
  cat("str(ucture):\n")
  str(x)                                     # compactly display structure of an object
  cat("summary:\n")
  print(summary(x))                          # summarize an object

  message()
  class <- xem.getClass(recset, identifier)
  message("class           : ", class)
  builtinRemark <- xem.getValue(recset, identifier, "builtin-remark")
  message("builtin remark  : ", builtinRemark)
  fieldCount <- length(recset[[identifier]])
  message("distinct fields : ", fieldCount - 1)
  fieldNames <- names(recset[[identifier]])
  if ( fieldCount > 2 )
    message("distinct names  : ", fieldNames[2])
  for ( i in 3:fieldCount )
    message("                  ", fieldNames[i])
  message()

  # TOFIX: examineEntity: extend field reporting (not important at present)
}

# ---------------------------------
#  function : xem.plotValue
# ---------------------------------

xem.plotValue <- function (recset,
                           identifier,
                           fieldname)
{
  # obtain timeseries
  y <- xem.getValue(recset, identifier, fieldname)

  # integrity check
  if ( is.null(y) ) {
    msg <- paste("cannot plot timeseries (check name)        ", identifier, ".", fieldname)
    plot.new()
    text(0.5, 0.5, labels = msg, cex = 1.3, col = "orange")
    return(numeric())                        # CAUTION: do not use 'invisible' for early returns
  }

  # zero-base the step count (because that is how 'xeona' works)
  steps <- xem.getSteps(recset)
  x     <- 0:(steps - 1)                     # CAUTION: incorrect code: xlim = c(0, steps-1)

  # some defensive coding
  if ( steps < 2 ) {
    message("  warn : xem.plotValue : timeseries of length ", steps)
    warning("timeseries of length ", steps)
  }

  # label processing
  class     <- xem.getClass(recset, identifier)
  inv       <- xem.getValue(recset, "time-horizon", "interval")
  kind      <- xem.getKind(recset, identifier, fieldname)

  mean      <- signif(mean(y), 4)
  units     <- xem.getUnits(recset, identifier, fieldname)
  fname     <- sub("ies$", "y", fieldname)        # remove trailing pluralization if present
  fname     <- sub("s$"  , "" , fname)            # remove trailing pluralization if present

  classtag  <- paste("class ="       , class)
  invaltag  <- paste("interval ="    , inv, "s")   # simply assume units of 's'
  kindtag   <- paste("kind ="        , kind)
  meantag   <- paste("mean ="        , mean)
  modetag   <- paste("xeona best mode =", xmode)
  steptag   <- paste("steps ="       , steps)
  vertag    <- paste("svn version =" , xver)
  xtag      <- paste("step count")

  if ( is.na(units) ) ytag <- fname
  else                ytag <- paste(fname, units)

  mlabel    <- substitute(paste( identifier, " ", phi, " ", fieldname))    # recovers parts!
  mlabel    <- expression(paste( identifier, " ", phi, " ", fieldname))
  mlabel    <- paste(identifier, fieldname, sep = " \xb7 ")                # ASCII 183 middle dot

  if ( xver == 0 ) tlabel <- paste(classtag, kindtag, modetag, sep = " / ")
  else             tlabel <- paste(classtag, kindtag, modetag, vertag ,sep = " / ")
  xlabel    <- paste(xtag, invaltag, steptag, sep = " / ")
  ylabel    <- paste(ytag, sep = " / ")
  slabel    <- paste(vertag, sep =" / ")

  # plot call
  # 'bty' box type usefully in { "o", "l", "n" }
  # 'ylim' controls y-axis limits
  plot.new()
  # plot(x, y, xlab = xlabel, ylab = ylabel, las = 1, bty = "l")                     # y-axis starts min(y)
  plot(x, y, ylim = c(0, max(y)), xlab = xlabel, ylab = ylabel, las = 1, bty = "l")  # y-axis starts zero
  title(main = mlabel)
  mtext(text = tlabel)
  abline(h = mean(y), lty = "dotted", col = "lightgray")
  # alternative label: text(x = 0, y = mean(y), adj = c(0, -1), labels = "mean")
  mtext(side = 4, at = mean(y), las = 0, text = meantag)

  # return invisibly
  invisible(y)
}

# ---------------------------------
#  function : xem.multiplot
# ---------------------------------
#
#  currently uses 'par(mfrow)', but could equally use
#  'layout' or 'trellis' if more sophistication is
#  required
#
# ---------------------------------

xem.multiplot <- function (recset,
                           flist)            # field list to plot
{
  # reporting
  message("  info : xem.multiplot : commencing, plots, ", length(flist))
  on.exit(message("  info : xem.multiplot : complete (on.exit), plots ", subs))

  # number of subplots
  subs <- length(flist)

   # rc "algorithm"
  if      ( subs <=  1 ) plots <- c(1,2)
  else if ( subs <=  2 ) plots <- c(1,2)
  else if ( subs <=  4 ) plots <- c(2,2)
  else if ( subs <=  6 ) plots <- c(2,3)
  else if ( subs <=  9 ) plots <- c(3,3)
  else if ( subs <= 12 ) plots <- c(3,4)
  else if ( subs <= 16 ) plots <- c(4,4)
  else if ( subs <= 20 ) plots <- c(5,4)
  else {
    message("  warn : xem.plotAllSomeTs : too many plots, plot flist truncated, was ", subs)
    warning("plot flist truncated")
    flist <- flist[1:20]
    plots <- c(5,4)
  }

  cntPlots <- plots[1] * plots[2]
  if     ( cntPlots <= 4 ) mycex <- 1.0
  else                     mycex <- 0.7

  # prepare plot
  opar <- par(no.readonly = TRUE)
  plot.new()
  par(mfrow = plots)

  # recover data and plot
  if ( subs > 0 ) {                          # protect against empty feild lists
    for ( k in 1:length(flist) )
      {
        out    <- flist[k]                   # use as label also
        part   <- unlist(strsplit(out, split = ".", fixed = TRUE))
        ts     <- xem.getValue(recset, part[2], part[3])
        mlabel <- paste(part[2], part[3], sep = " \xb7 ")   # ASCII 183 middle dot

        plot(ts,                             # timeseries as unnamed numeric vector
             main = mlabel,                  # see above
             xlab = "", ylab = "",           # suppress axis labels
             pch  = 3,                       # 3 = a cross
             cex  = mycex)                   # reduce plot character as determined above
        mtext(text = paste("plot", k),
              padj = -0.5,                   # 'padj' is perpendicular, negative is raise
              cex  = mycex)
      }
  }

  # housekeeping
  par(opar)

  # return
  invisible(subs)
}

# ---------------------------------
#  function : xem.plotAllTs
# ---------------------------------

xem.plotAllTs <- function (recset,
                           kind)             # 'kind' in { "in", "out" }
{
  # reporting
  message("  info : xem.plotAllTs : commencing")
  on.exit(message("  info : xem.plotAllTs : complete (on.exit), kind ", kind))

  # grab names directly and traverse dataset
  entitys   <- recset$"metadata"$"ids"
  xins     <<- character()
  xouts    <<- character()
  for ( i in 1:length(entitys) )
    {
      id     <- entitys[i]
      entity <- recset[[id]]                 # grab entity
      for ( j in 1:length(entity) )
        {
          fname <- names(entity[j])
          field <- entity[[fname]]           # grab field
          if ( length(field) != 6 )   next   # not a data field
          value <- field[["value"]]
          kind2 <- field[["kind"]]
          if ( ! is.numeric(value) )  next   # not numeric data
          if ( length(value)  < 2 )   next   # not a timeseries
          if ( kind2 == "in"  ) xins  <<- c(xins,  paste("entity", id, fname, sep = "."))
          if ( kind2 == "out" ) xouts <<- c(xouts, paste("entity", id, fname, sep = "."))
        }
    }

  # remove (as it happens adjacent) duplicate entries
  # from 'xins' and 'xouts' -- these arise from
  # additional disabled fields on the XEM file
  xins  <<- unique(xins)
  xouts <<- unique(xouts)

  # switch on kind and then plot
  switch (kind,
          "in"  = xem.multiplot(recset, xins),
          "out" = xem.multiplot(recset, xouts))

  # return
  invisible()
}

# ---------------------------------
#  function : tst.extract
# ---------------------------------

tst.extract <- function (xemfile,
                         dbug)
{
  DBUG <<- dbug
  message("  * * * * * * * * * *\n  test : extract test (currently hollow) commencing / DBUG = ", DBUG)
  message("  test : extract complete")
}

# ---------------------------------
#  junk
# ---------------------------------

# for specifier 'g' the '.precision' indicates
# significant digits, with rounding performed if
# necessary
#
# mean      <- sprintf("%.4g", mean(y))

#  $Id: extract.R 6005 2011-02-25 18:12:42Z robbie $
#  end of file

