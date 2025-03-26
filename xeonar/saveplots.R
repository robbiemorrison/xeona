
#  file-purpose     : save open plot windows in various formats
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 16-Aug-2010 18:16 UTC
#  file-status      : work-in-progress
#  file-keywords    : xeona R

#  $Revision: 6005 $
#  $Date: 2011-02-25 19:12:42 +0100 (Fri, 25 Feb 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeonar/saveplots.R $

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
#  function : xem.rofile
# ---------------------------------
#  description  : make file read-only
#  role         : normally called after file creation
#  takes        : 'filename' as string
#  returns      : TRUE on success
#  techniques   : 'system' 'chmod'
#  status       : complete
#  CAUTION      : Linux-specific ('chmod')
# ---------------------------------

xem.rofile <- function (filename,
                        kill.empty = TRUE)
{
  if ( ! is.logical(kill.empty) ) {
    stop("expecting a logical argument type for 'kill.empty'")
  }

  if ( length(filename) !=  1 ) {
    if ( nchar(filename) == 0 ) {
      message("  warn : xem.rofile : problem filename ", filename)
      warning("problem filename")
      return(FALSE)
    }
  }

  if ( ! file.exists(filename) ) {
    message("  warn : xem.rofile : cannot locate file ", filename)
    warning("cannot locate file ", filename)
    return(FALSE)
  }

  # the following CANNOT be implemented using 'Sys.chmod'
  modestr <- paste("a-w", "o-r", "ug+r", sep = ",")
  ret <- system(paste("chmod", "--silent", modestr, filename, sep = " "))

  if ( kill.empty ) {                        # logical
    finfo <- file.info(filename)             # data frame
    size  <- finfo[ , "size"]                # CAUTION: the comma is correct, yields a numeric
    if ( size == 0 ) file.remove(filename)
  }

  if ( ret == 0 ) TRUE else FALSE
}

# ---------------------------------
#  function : xem.saveplot.png
# ---------------------------------
#  description  : save given device as PNG
#  status       : working
#
#  R version    : 2.10.1 (2009-12-14)
#  development  : Ubuntu 10.04 Linux 2.6.32-23-generic / Intel Core i5 laptop /'hinau'
#  see also     : bash script 'x11show.sh'
# ---------------------------------

# [1] 700 is too tight, 1100 and 1200 are okay, 1700 is rather loose

xem.saveplot.png <- function (device,
                              filestub,             # save name
                              type,                 # plot type
                              dbug,
                              width.px  = 1200)     # bitmap width [1]
{
  # set debug level here
  DBUG <- dbug

  # report
  message("  info : xem.saveplot.png : commencing with",
          " device = "      , device,
          " / filestub = '" , filestub, "'",
          " / type = '"     , type, "'",
          " / dbug = "      , dbug,
          " / width.px = "  , width.px)
  on.exit(message("  info : xem.saveplot.png : complete (on.exit)"))

  # print calls - no dimensions specified

  xem.saveplot.printme(png, filestub, type, "a", width.px)

  # clean run indication
  message("  info : xem.saveplot.png : complete run")

}

# ---------------------------------
#  function : xem.saveplot.pdf
# ---------------------------------
#  description  : save given device as PDF
#  status       : work-in-progress
#
#  R version    : 2.10.1 (2009-12-14)
#  development  : Ubuntu 10.04 Linux 2.6.32-23-generic / Intel Core i5 laptop /'hinau'
#  see also     : bash script 'x11show.sh'
#
#  CAUTION: coded on the assumption 'cairo' is supported
#
#  CAUTION: 'xeona' breadboard plots are sometimes scrambled
#           with some settings -- this can happen across all
#           vector graphics plots at least
# ---------------------------------

# [1] 150mm is scrambled, 300mm is okay

xem.saveplot.pdf <- function (device,
                              filestub,             # save name
                              type,                 # plot type
                              dbug,
                              width.mm   = 300,     # vector width for format b [1]
                              doctitle   = "",      # CAUTION: NULL produces NA
                              format.alt = TRUE)    # use alternative formats
{
  # set debug level here
  DBUG <- dbug

  # report
  message("  info : xem.saveplot.pdf : commencing with",
          " device = "      , device,
          " / filestub = '" , filestub, "'",
          " / type = '"     , type, "'",
          " / dbug = "      , dbug,
          " / width.mm = "  , width.mm,
          " / doctitle = '" , doctitle, "'",
          " / format.alt =" , format.alt)

  ##  TOFIX: 17-Aug-2010: solve or delete
  ##  CALL <- match.call()
  ##  message("  info : xem.saveplot.pdf : call = ", CALL) # TOFIX: 17-Aug-2010: experimental

  on.exit(message("  info : xem.saveplot.pdf : complete (on.exit)"))

  # report on cairo
  if ( DBUG > 0 ) {
    x <- capabilities("cairo")
    if ( length(x) > 0 )
      if ( x )
        message("  info : xem.saveplot.pdf : this R installation supports cairo")
      else
        message("  info : xem.saveplot.pdf : this R installation does NOT support cairo")
  }

  # R device    : cairo_pdf
  # creator     : cairo 1.8.10
  # fonts       : NimbusSansL (Type 1) / embedded
  # PDF version : 1.4

  # print calls - no dimensions specified

  xem.saveplot.printme(cairo_pdf, filestub, type, "a")
  if ( format.alt ) {
    xem.saveplot.printme(cairo_pdf, filestub, type, "d", width.mm, 1.00)
  }

  # clean run indication
  message("  info : xem.saveplot.pdf : complete run")

}

# ---------------------------------
#  function : xem.saveplot.svg
# ---------------------------------
#  description  : save given device as SVG
#  status       : work-in-progress
#
#  R version    : 2.10.1 (2009-12-14)
#  development  : Ubuntu 10.04 Linux 2.6.32-23-generic / Intel Core i5 laptop /'hinau'
#
#  CAUTION: 'xeona' breadboard plots are sometimes scrambled
#           with some settings -- this can happen across all
#           vector graphics plots at least
# ---------------------------------

# [1] 150mm is scrambled, 200mm is over-tight, 300mm is okay

xem.saveplot.svg <- function (device,
                              filestub,             # save name
                              type,                 # plot type
                              dbug,
                              width.mm   = 300,     # vector width for alternative format [1]
                              doctitle   = "",      # CAUTION: NULL produces NA
                              format.alt = TRUE)    # use alternative formats
{
  # set debug level here
  DBUG <- dbug

  # report
  message("  info : xem.saveplot.svg : commencing with",
          " device = "      , device,
          " / filestub = '" , filestub, "'",
          " / type = '"     , type, "'",
          " / dbug = "      , dbug,
          " / width.mm = "  , width.mm,
          " / doctitle = '" , doctitle, "'",
          " / format.alt =" , format.alt)

  on.exit(message("  info : xem.saveplot.svg : complete (on.exit)"))

  # XML version  : 1.0
  # encoding     : UTF-8
  # fonts        : no fonts, rather characters are cloned to vectored glyphs defined in <defs>
  # default size : width="850pt" height="424pt

  # 'title' argument not supported
  # 'width' alone is okay

  # print calls - no dimensions specified

  xem.saveplot.printme(svg, filestub, type, "a")

  if ( format.alt ) {
    scale <- 0.67

    xem.saveplot.printme(svg, filestub, type, "b1", scale * width.mm, 0.50)
    xem.saveplot.printme(svg, filestub, type, "c1", scale * width.mm, 0.67)
    xem.saveplot.printme(svg, filestub, type, "d1", scale * width.mm, 1.00)
    xem.saveplot.printme(svg, filestub, type, "e1", scale * width.mm, 1.41)
    xem.saveplot.printme(svg, filestub, type, "f1", scale * width.mm, 2.00)

    scale <- 1.00

    xem.saveplot.printme(svg, filestub, type, "b2", scale * width.mm, 0.50)
    xem.saveplot.printme(svg, filestub, type, "c2", scale * width.mm, 0.67)
    xem.saveplot.printme(svg, filestub, type, "d2", scale * width.mm, 1.00)
    xem.saveplot.printme(svg, filestub, type, "e2", scale * width.mm, 1.41)
    xem.saveplot.printme(svg, filestub, type, "f2", scale * width.mm, 2.00)

    scale <- 1.41

    xem.saveplot.printme(svg, filestub, type, "b3", scale * width.mm, 0.50)
    xem.saveplot.printme(svg, filestub, type, "c3", scale * width.mm, 0.67)
    xem.saveplot.printme(svg, filestub, type, "d3", scale * width.mm, 1.00)
    xem.saveplot.printme(svg, filestub, type, "e3", scale * width.mm, 1.41)
    xem.saveplot.printme(svg, filestub, type, "f3", scale * width.mm, 2.00)
  }

  # clean run indication
  message("  info : xem.saveplot.svg : complete run")

}

# ---------------------------------
#  function : xem.saveplot.printme
# ---------------------------------
#  description  :
#  role         :
#  takes        :
#  returns      :
#  techniques   :
#  status       : under testing
#
#  typical call
#
#    xem.saveplot.printme(svg, filestub, type, "d", width.mm, 2.000)
#
#  infeasible layout (solution change
#
# Error in dev.copy(device = function (filename = if (onefile) "Rplots.svg" else "Rplot%03d.svg",  :
#   invalid graphics state
#
# the plot shows: "figure margins too large"
#
# ---------------------------------

xem.saveplot.printme <- function (device,         # { png cairo_pdf svg}
                                  filestub,       # filestub
                                  type,           # { summary .. }
                                  tag,            # { a b c .. }
                                  width  = NA,    # px or mm
                                  ratio  = NA)    # 1.000 gives a square
{
  # report
  message("  info : xem.saveplot.printme : commencing with",
          " device = "      , "(not recordable)",
          " / filestub = '" , filestub, "'",
          " / tag = '"      , tag, "'",
          " / width = "     , width,
          " / ratio = "     , ratio)
  on.exit(message("  info : xem.saveplot.printme : complete (on.exit)"))

  # obtain format information
  devname <- deparse(substitute(device))     # based on code from 'dev.print'
  if      ( devname == "png"      ) extn <- "png"
  else if ( devname == "cairo_pdf") extn <- "pdf"
  else if ( devname == "svg"      ) extn <- "svg"
  else stop("unsupported device", devname)

  # process variables
  filename  <- paste(filestub, type, tag, extn, sep = ".")  # say "filestub.summary.a.svg"
  reportstr <- paste(toupper(extn), tag, sep = " ")         # say "SVG b"

  # print calls
  printme.0 <- function()
    {
      # background color not shown by 'display', but described in "Image Info"
      message("  info : xem.saveplot.printme : making 0: ", filename)
      if ( file.exists(filename) ) file.remove(filename)
      from <- dev.print(device = device,
                        file   = filename,
                        width  = width,
                        bg     = "white")    # CAUTION: "transparent" would not take
      xem.rofile(filename)
      xem.report(filename, reportstr, from, width)
    }

  printme.1 <- function()
    {
      # 'title' argument not supported for pdf
      message("  info : xem.saveplot.printme : making 1: ", filename)
      if ( file.exists(filename) ) file.remove(filename)
      from <- dev.print(device = device,
                        file   = filename)
      xem.rofile(filename)
      xem.report(filename, reportstr, from)
    }

  printme.2 <- function()
    {
      message("  info : xem.saveplot.printme : making 2: ", filename)
      if ( file.exists(filename) ) file.remove(filename)
      from <- dev.print(device = device,
                        file   = filename,
                        width  = width / 25.400,
                        height = width / 25.400 * ratio)
      xem.rofile(filename)
      xem.report(filename, reportstr, from, width, ratio)
    }

  # make appropriate call (the logic here a little subtle)
  if      ( is.na(ratio) && ! is.na(width) ) try( printme.0() )
  else if ( is.na(width)                   ) try( printme.1() )
  else                                       try( printme.2() )

  # clean run indication
  message("  info : xem.saveplot.printme : complete run")

}

# ---------------------------------
#  function : xem.report
# ---------------------------------
#  description  : assemble and submit a standard report line
#  role         : the various 'xem.saveplot.*' calls
#  takes        : see below
#  returns      : nothing of interest
#  techniques   : 'sprintf' 'append'
#  status       : under testing
#
#    the typical call:
#
# xem.report(svgfilec, "SVG c", svgc, width.mm, ratio)
#
#    gives approximately
#
# '    ../test-12.summary.c.svg               tag = SVG c   from = 2   width = 0300  ratio = 1.500'
#
# ---------------------------------

xem.report <- function (filename,            # filename as string
                        tag,                 # 5-char tag as string
                        fromdev,             # number of from device
                        width    = NA,       # width in px or mm as number
                        ratio    = NA)       # ratio as number
{
  # check for bad coding
  if ( ! is.na(ratio) && is.na(width) ) stop("if 'ratio' is used, 'width' is also required")

  # concat
  repline <- sprintf("    %-70s  tag = %-6s   from = %d", filename, tag, fromdev)
  if ( ! is.na(width) ) {
    repline <- sprintf("%s   width = %04d", repline, width)
  }
  if ( ! is.na(ratio) ) {
    repline <- sprintf("%s   ratio = %.3f", repline, ratio)
  }
  repline <- sprintf("%s\n", repline)

  # export details
  saveplot.report <<- append(saveplot.report, repline)
}

#  $Id: saveplots.R 6005 2011-02-25 18:12:42Z robbie $
#  end of file

