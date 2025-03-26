
#  file-purpose     : point of contact, user functions, call 'xeona' as required
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Thu 04-Jun-2009 19:56 UTC
#  file-status      : working
#  file-keywords    : xeona R

#  $Revision: 6005 $
#  $Date: 2011-02-25 19:12:42 +0100 (Fri, 25 Feb 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeonar/xem.R $

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

#  PURPOSE
#
#  point of entry for client scripts
#
#  REQUIRES
#
#  * environment variable 'XEONAR' be appropriately set
#  * a standard R installation
#  * 'dot' (from 'Graphviz') if graph visualization is to be used
#  * 'display' (from 'ImageMagick') (until the code migrates to R)
#
#  TYPICAL USAGE
#
#  robbie.clean()  # CAUTION: must come first
#  source((file.path(Sys.getenv("XEONAR"), "xem.R"))
#  xem.job("model.xem", dbug = 2)
#
#  see also R script 'gaia.R' and similar
#
#  CLEAN-UP
#
#  no 'robbie.clean' calls are made in this code
#
#  UML DATA DIAGRAM
#
#  search on 'uml-dataset.png' and 'xeona-recset_00.xmi'
#
#  JOB CODES
#
#       0 = silent
#       1 = summarize
#       2 = plot all
#       4 = plot list
#       8 = breadplot
#      16 = graphviz diagram
#
#  TESTED USING
#
#  Initial development ('sojus')
#
#      R version 2.3.1 (2006-06-01)
#      Ubuntu 6.10 / Linux 2.6.17-12-generic
#      Graphviz version 2.22.2 (20090313.1817)
#      ImageMagick 6.2.4 10/02/07
#
#  Later development ('hinau')
#
#      note older graphviz package / see 'change-log.sojus' entry 2009-06-15
#
#      R version 2.10.1 (2009-12-14)
#      Ubuntu 10.04  Linux 2.6.32-24-generic (etc)
#      dot - Graphviz version 2.20.2 (Tue Mar  2 19:03:41 UTC 2010)
#      ImageMagick 6.5.7-8 2009-11-26 Q16

message("  file : xem.R : starting")

# ---------------------------------
#  variable : xem.colors
# ---------------------------------

# based on X11 and must be acceptable to both R and
# 'graphviz' (best to work from the 'graphviz' list)

xem.colors <- c(# default (1) - gray
                "dimgray",

                # authority (4) - reds
                "coral3",
                "tomato",
                "orange",
                "gold",

                # interface (3) - blues
                "mediumslateblue",
                "steelblue3",
                "cadetblue3",

                # context (2) - green
                "olivedrab3",
                "olivedrab4")

# ---------------------------------
#  function : xem.colors.print
# ---------------------------------

xem.colors.print <- function()
{
  x11()
  main <- paste("xem arrow color test")
  sub  <- "using the subset of x11 color names supported by graphviz"
  colorkey <<- sprintf("%2d \xb7 %s", seq(length(xem.colors)), xem.colors)
  pie(rep(1, length(xem.colors)), col = xem.colors, main = main, labels = colorkey)
  mtext(sub)
}

# ---------------------------------
#  function : xem.x11
# ---------------------------------
#  description  : control x11 plot frames (as a type of device)
#  role         : usually an early call for a particular job
#  status       : complete
# ---------------------------------

xem.x11 <- function (snooze)
{
  # slide show
  Sys.sleep(snooze)

  # host specific settings
  host <- Sys.info()["nodename"]
  if      ( host == "sojus" ) x11(width = 12, height = 8)
  else if ( host == "hinau" ) x11(width = 14, height = 7)
  else                        x11()

  # can add 'par' statements here
  par(col.main = "darkslategray")
}

# ---------------------------------
#  function : xem.saveplot
# ---------------------------------
#  description  : control x11 plot frames (as a type of device)
#  role         : usually an early call for a particular job
#  status       : complete
# ---------------------------------

xem.saveplot <- function (type = "notset",
                          dbug = 0)
{
  # source support file
  source(file.path(Sys.getenv("XEONAR"), "saveplots.R"), local = TRUE)     # save plot windows

  # report
  saveplot.report <- character()             # used for completion reporting

  # preparation
  filestub <- sub("\\.xem$", "", xemfile)    # trim trailing ".xem"

  # loop file formats
  if ( dev.cur() > 1 ) {                     # null device is number 1
    xem.saveplot.png(device = dev.cur(), filestub = filestub, type = type, dbug = dbug)
    xem.saveplot.pdf(device = dev.cur(), filestub = filestub, type = type, dbug = dbug)
    xem.saveplot.svg(device = dev.cur(), filestub = filestub, type = type, dbug = dbug)
  }

  # report
  message("  info : xem.saveplot : summary:")
  message(saveplot.report)
}

# ---------------------------------
#  function : xem.job
# ---------------------------------
#  description  : main function
#  role         : point of contact call
#  takes        : see signature
#  returns      : 'xmode'
#  techniques   : see code
#  status       : complete
# ---------------------------------

xem.job <- function (xemfile,
                     xeona  = "xeona.mach",  # for default use, must be on the system 'PATH'
                     mlabel = "xem.R trial", # main label in plots
                     dbug   = 0,             # override default value set above
                     jobs   = NULL,          # override XEM file 'program.r-processing.r-policy'
                     snooze = 0,
                     report = 0)
{
  # ---------------------------------
  #  preamble
  # ---------------------------------

  # set debug level here
  DBUG <- dbug

  # report
  if ( is.null(jobs) ) jobrep <- "(XEM file)"
  else                 jobrep <- jobs
  message("  info : xem.job : commencing with",
          " xemfile = '" , xemfile, "'",
          " mlabel = "   , mlabel,
          " / DBUG = "   , DBUG,
          " / jobs = "   , jobrep,
          " / snooze = " , snooze,
          " / report = " , report)
  on.exit(message("  info : xem.job : complete (on.exit)"))
  message("  info : xem.job : nodename = " , Sys.info()["nodename"])

  # ---------------------------------
  #  source
  # ---------------------------------

  source(file.path(Sys.getenv("XEONAR"), "utils.R"),     local = TRUE)    # utility functions
  source(file.path(Sys.getenv("XEONAR"), "run.R"),       local = TRUE)    # run calls
  source(file.path(Sys.getenv("XEONAR"), "dataproc.R"),  local = TRUE)    # nested dataset
  source(file.path(Sys.getenv("XEONAR"), "extract.R"),   local = TRUE)    # extract data, normal plots
  source(file.path(Sys.getenv("XEONAR"), "breadplot.R"), local = TRUE)    # breadboard plot
  source(file.path(Sys.getenv("XEONAR"), "graphviz.R"),  local = TRUE)    # graphviz visualization

  # ---------------------------------
  #  core establishment calls
  # ---------------------------------

  xver   <<- xem.svnver()

  if ( is.null(xeona) ) {
    xemfile <<- xem.xemconf.model(xemfile)             # reports on but does no change 'xemfile' string
    xcall   <<- "na"                                   # normally set as side-effect of 'xem.xeona'
    xmode   <<- "na"                                   # normally set by 'xem.freshen'
  }
  else {
    xemfile <<- xem.xemconf.guard(xemfile)             # CAUTION: can change 'xemfile' string
    xmode   <<- xem.freshen(xeona, xemfile, report)    # generate a fresh deep or perhaps shallow XEM file
  }

  xdata  <<- xem.slurp(xemfile)                        # slurp XEM file
  xset   <<- xem.loop(xdata)

  xsteps <<- xem.getValue(xset, "time-horizon", "steps")

  # ---------------------------------
  #  recset reporting
  # ---------------------------------

  message("recset length  : ", sprintf("%4d", length(xset)))
  message("data lines     : ", sprintf("%4d", length(xdata)))

  if ( DBUG > 1 ) {
    message()
    message("structure")
    message()
    str(xset)
    message()
  }
  if ( DBUG > 0 ) {
    message("summary")
    print(summary(xset))                     # CAUTION: 'print' wrapper needed
    message()
  }

  # ---------------------------------
  #  job specification
  # ---------------------------------

  jobTerms <- c("summarize", "plot all", "plot list", "breadplot", "graphviz")

  xpolicy <<- xem.getPolicy(xset)
  xtitle  <<- xem.getValue(xset, "r-processing", "r-title")

  if ( length(xpolicy) == 0 ) stop("no run policy found in xem file (malformed data)")
  if ( is.null(jobs) ) jobs <- xpolicy       # function argument takes priority over XEM file value

  jobsDecomp <- xem.isTwoContained(jobs)
  jobsDecomp <- append(jobsDecomp, rep(0, 6 - length(jobsDecomp)), after = 0)   # pad to length 6
  jobMsgs    <- paste(jobTerms[rev(jobsDecomp) > 0], collapse = " * ")
  message("jobs           : ", jobs, " = ", jobsDecomp, " = ", jobMsgs)         # values like 008401

  # ---------------------------------
  #  job 1 : summarize
  # ---------------------------------

  if ( xem.isTwoContained(jobs, 1) ) {
    message("  info : xem.job : summarize job starts")
    xem.x11(snooze)
    xsummary <<- xem.summarize(xset)
    xem.saveplot("summary", DBUG)
  }

  # ---------------------------------
  #  job 2 : plot all (but will truncate if necessary)
  # ---------------------------------

  if ( xem.isTwoContained(jobs, 2) ) {
    message("  info : xem.job : plot all job starts")
    xem.x11(snooze)
    xem.plotAllTs(xset, kind = "in")
    xem.saveplot("in", DBUG)
    xem.x11(snooze)
    xem.plotAllTs(xset, kind = "out")
    xem.saveplot("out", DBUG)
  }

  # ---------------------------------
  #  job 4 : plot list
  # ---------------------------------

  # loop to plot list
  if ( xem.isTwoContained(jobs, 4) ) {
    message("  info : xem.job : plot list job starts")

    # grab data
    plotStr <- xem.getPlotList(xset)
    if ( nchar(plotStr) > 0 ) {

      # split single strings
      sep     <- "[[:blank:]]+"                # define the split regex
      plots   <- unlist(strsplit(plotStr, split = sep))

      # loop
      for ( i in 1:length(plots) )
        {
          if ( nchar(plots[i] ) == 0 ) next    # protect against empty strings, namely the XEM value ""
          parts <- xem.splitFieldname(plots[i])
          xem.x11(snooze)
          xem.plotValue(xset, parts[2], parts[3])
          xem.saveplot("plotlist", DBUG)
        }
    }
    else {
      message("  warn : xem.job : plot list is empty, abandoning job")
    }
  }

  # ---------------------------------
  #  job 8 : breadplot
  # ---------------------------------

  if ( xem.isTwoContained(jobs, 8) ) {
    message("  info : xem.job : breadplot job starts")
    xem.x11(snooze)
    xcapture <<- xem.breadplot(xset, xtitle)
    xem.saveplot("breadplot", DBUG)
  }

  # ---------------------------------
  #  job 16 : graphviz
  # ---------------------------------

  if ( xem.isTwoContained(jobs, 16) ) {
    message("  info : xem.job : graphviz job starts")
    Sys.sleep(snooze)
    # xem.x11(snooze)
    # plot.new()
    xret <<- xem.graphviz(xset, xtitle)
  }

  # ---------------------------------
  #  completion reporting
  # ---------------------------------

  if ( TRUE ) {
    message("xeona call     : ", xcall)
    message("xem steps      : ", xsteps)

    message("debugging      : DBUG set to ", DBUG)

    message("call           : str(xset)")
    message("call           : summary(xset)")
  }

  # ---------------------------------
  #  housekeeping
  # ---------------------------------

  xem.beep("alert")

  # TOFIX: 17-Aug-2010: temporary settings

  Sys.sleep(1)                               # TOFIX: 15-Aug-2010: temporary
  system("killall display")                  # kill graphviz plot window

  invisible(xmode)
}

message("  file : xem.R : finishing")

#  $Id: xem.R 6005 2011-02-25 18:12:42Z robbie $
#  end of file

