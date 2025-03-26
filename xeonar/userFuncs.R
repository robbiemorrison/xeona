
#  file-purpose     : default set of user functions automatically loaded at startup
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Tue 16-Jun-2009 13:41 UTC
#  file-status      : ongoing
#  file-keywords    : xeona R

#  $Revision: 6005 $
#  $Date: 2011-02-25 19:12:42 +0100 (Fri, 25 Feb 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeonar/userFuncs.R $

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
#  function : robbie.rofile
# ---------------------------------
#  description  : makes file readonly
#  role         : normally called after file creation
#  takes        : 'filename' as string
#  returns      : TRUE on success
#  techniques   : 'system' 'chmod'
#  status       : complete
# ---------------------------------

robbie.rofile <- function (filename,
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

  # the following cannot be implemented using 'Sys.chmod'
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
#  function : robbie.bye
# ---------------------------------
#  description  : wrapper for function 'quit'
#  status       : complete
# ---------------------------------

robbie.bye <- function ()
{
  cat("\n")
  cat("robbie.bye: quiting R without saving workspace\n")
  quit("no")
}

# ---------------------------------
#  function : robbie.examine
# ---------------------------------
#  description  : runs several exploratory calls on the given object
#  status       : complete
# ---------------------------------

robbie.examine <- function (x)
{
  cat("structure:\n")
  str(x)                                     # compactly display structure of an object
  cat("mode (programmatic) * typeof (internal) * nchar (as human-readable):\n")
  print(mode(x))                             # get the programmatic type or storage mode of an object
  print(typeof(x))                           # get internal type
  print(nchar(x))                            # bytes or alternatively human-readable characters
  cat("summary:\n")
  print(summary(x))                          # summarize an object
}

# ---------------------------------
#  function : robbie.clean
# ---------------------------------
#  description  : remove variables in a sensitive manner, defaults to non-dots except "robbie.xxx"
#  status       : complete
# ---------------------------------

robbie.clean <- function (retains = NULL,    # optional string vector of variables to exclude
                          say     = FALSE)   # report retains
{
  # note R inbuilts
  # > search()
  # > environment() <environment: R_GlobalEnv>
  # > pos.to.env(1) <environment: R_GlobalEnv>

  # remove(pos = 1, list = setdiff(ls(name = 1), ls(name = 1, pattern = "^robbie\\..*")))

  vars  <- ls(name = 1)                                # current delete list, will be slowly reduced
  vars  <- vars[regexpr("^robbie\\..*", vars) == -1]   # note that zero values are not possible
  was   <- length(vars)
  vars  <- setdiff(vars, retains)                      # remove any 'retains' present
  delta <- was - length(vars)
  if ( delta == 0 ) cat("robbie.clean:")
  if ( delta == 1 ) cat("robbie.clean: will retain", delta, "nominated variable,")
  if ( delta  > 1 ) cat("robbie.clean: will retain", delta, "nominated variables,")
  remove(pos = 1, list = vars)             # core call
  cat(" complete\n")

# if ( say ) invisible(lapply(retains, function(var) { print(class(var)) } ))

  if ( length(retains) > 0 && say == TRUE ) {
    loop <- 0
    for ( i in 1:length(retains) ) {
      varname <- retains[i]
      if ( exists(varname) ) {
        var = get(varname)
        if ( mode(var) == "function" ) next
        varinfo <- class(var)
        varlen  <- length(var)
        cat(sprintf("    %-25s  %4d    %s\n", varname, varlen, varinfo))
        loop <- loop + 1
      }
    }
    if ( loop != delta ) warning("coding problem with retains reporting ", immediate. = TRUE)
  }
}

# ---------------------------------
#  function : robbie.ls
# ---------------------------------
#  description  : filtered listing of all no-dots except those like "robbie.xxx"
#  status       : complete
# ---------------------------------

robbie.ls <- function ()
{
  vars <- ls(name = 1)
  vars <- vars[regexpr("^robbie\\..*", vars) == -1]    # note that zero values are not possible
  if ( length(vars) == 0 ) message("robbie.ls: no general non-dot variables")
  else                     print(vars)
}

# ---------------------------------
#  function : robbie.source
# ---------------------------------
#  description  : wrapper for function 'source'
#  status       : complete
# ---------------------------------

robbie.source.file <- "funcdev.R"            # hardcoded but rewritable name

robbie.source <- function ()
{
  if ( is.character(robbie.source.file) & nchar(robbie.source.file) > 0 )  # CAUTION: [1]
    {
      # [1] nchar(NA) returns 2, hence the need for the is.character() test

      leafname <- basename(robbie.source.file)    # strip any leading directory
      file     <- file.path(getwd(), leafname)    # creates a simple string, nothing more

      if ( file.exists(file) ) {
        source(file, echo = TRUE)
        cat("\n")
        cat("* just sourced/refreshed file: ",        file,                 "\n", sep = "")
        cat("* using dataset robbie.source.file: \"", robbie.source.file, "\"\n", sep = "")
        cat("\n")
      } else {
        message("robbie.source: R source file not found: ", file)
      }
    } else {
      message("robbie.source: variable not found or improperly set: 'robbie.source.file'")
    }
}

# ---------------------------------
#  function : robbie.mm2inch
#  function : robbie.inch2mm
# ---------------------------------
#  description  : see code
#  status       : complete
# ---------------------------------

robbie.mm2inch <- function () { return(25.400) }
robbie.inch2mm <- function () { return(1/robbie.mm2inch()) }

# ---------------------------------
#  function : robbie.plotSize
# ---------------------------------
#  description  : report plot size
#  status       : complete
# ---------------------------------

robbie.plotSize <- function ()
{
  if ( dev.cur() == 1 ) {                    # to avoid opening a new plot frame
    cat("the null device is current\n")
    invisible("")
  }

  hw  <- par("pin") * robbie.mm2inch()       # 'pin' is current plot height/width dimensions in inches
  hw  <- sprintf("%.0f", hw)                 # coerce into a character vector with 0 decimal places
  msg <- paste("current plot size (mm):",
               hw[1],
               "x",
               hw[2],
               "wide")                       # default sep = " "
  cat(msg, "\n", sep = "")                   # default sep = " "
  invisible(msg)
}

# ---------------------------------
#  function : robbie.filestub
# ---------------------------------
#  description  :
#  role         :
#  takes        :
#  returns      :
#  techniques   :
#  status       : incomplete
# ---------------------------------

robbie.filestub <- function (filestub)
{
  return(filestub)
}

# ---------------------------------
#  function : robbie.saveX11
# ---------------------------------
#  description  : save current focus plot windows
#  status       : working (but will be tweaks)
#
#  svn rollback : commit r4740 (2010-07-14)
#  R version    : 2.10.1 (2009-12-14)
#  development  :  Ubuntu 10.04 Linux 2.6.32-23-generic / Intel Core i5 laptop /'hinau'
#  see also     : bash script 'x11show.sh'
#
#  CAUTION: coded on the assumption 'cairo' is supported
#
#  CAUTION: 'xeona' breadboard plots are sometimes scrambled
#           with some settings -- this can happen across all
#           vector graphics plots at least
# ---------------------------------

robbie.saveX11 <- function (filestub = NULL,      # save name
                            width.mm = 150,       # vector formats
                            size.px  = 700,       # bitmap formats
                            device   = dev.cur(), # current device
                            doctitle = "",        # CAUTION: NULL produces NA
                            dbug     = 1,         # should be 1 unless testing
                            format.b = FALSE)
{
  # ---------------------------------
  #  preamble
  # ---------------------------------

  DBUG <- dbug
  if ( DBUG > 1) message("")

  # FUTZING ONLY
  # graphics.off()                           # kill all graphics devices, not just screen devices

  # make 'device' current, note protection against no plot windows
  if ( device != 1 ) {                       # 1 is reserved for the null device
    dev.set(device)
  }

  # ---------------------------------
  #  initial report
  # ---------------------------------

  # command-line arguments, including defaults
  message("robbie.saveX11: commencing,",
          " filestub = \""   , filestub, "\"",
          " / width.mm = "   , width.mm,
          " / size.px = "    , size.px,
          " / device = "     , device,
          " / doctitle = \"" , doctitle, "\"",
          " / dbug = "       , dbug,
          " / format.b = "   , format.b)
  on.exit(message("robbie.saveX11: complete (on.exit)"))

  # report on cairo
  if ( DBUG > 0 ) {
    x <- capabilities("cairo")
    if ( length(x) > 0 )
      if ( x )
        message("robbie.saveX11: this R installation supports: cairo")
      else
        message("robbie.saveX11: this R installation does not support: cairo")
  }

  # ---------------------------------
  #  general test plot as required
  # ---------------------------------

  if ( device == 1 ) {                       # 1 is reserved for the null device
    if ( DBUG > 1 ) {
      x11()
      plot.new()
      plot(sin, -pi, 2*pi)
      title("some long and wonderful title")
      browser()
      if ( is.null(filestub) ) filestub <- "piplot"   # NULL, meaning not passed in
    } else {
      message("robbie.saveX11: ABANDONING: null device current (check there is an active plot frame)")
      stop("no device to print from")
    }
  }

  # ---------------------------------
  #  filestub
  # ---------------------------------

  if ( is.null(filestub) ) {                 # NULL, meaning not passed in
    defaultstub <- "saveplot"
    prompt <- sprintf("  enter filestub [%s]? ", defaultstub)
    filestub <- readline(prompt)
    if ( nchar(filestub) == 0 ) {            # empty string, meaning the user hit enter
      filestub <- defaultstub
      message("robbie.saveX11: filename defaulting to: ", filestub, ".*")
    }
  }
  filestub <- robbie.filestub(filestub)      # currently hollow

  # ---------------------------------
  #  dev.print
  # ---------------------------------

  # 'dev.print' copies the graphics contents of the current device to
  # a new device which has been created by the function specified by
  # 'device' and then shuts the new device
  #
  # possible arguments: device file horizontal paper pointsize width height

  report <- character()                      # used for completion reporting

  # ---------------------------------
  #  EPS
  # ---------------------------------

  # appliations: 'inkscape' 'ggv'
  # fonts (inkscape): Bitstream VeraSans
  # issues: only this call respects "stretch"

  # under 'dev.copy2eps' the 'title' will appear in the PS header

  #   epsfile <- paste(filestub, "eps", sep = ".")
  #   message("robbie.saveX11: making EPS: ", epsfile)
  #   dev.print(  device     = postscript
  #             , file       = epsfile
  # #           , width      = width.mm / robbie.mm2inch()
  # #           , height     = width.mm / robbie.mm2inch()
  #             )
  #   robbie.rofile(epsfile)
  #   report <- append(report, paste("EPS (width = ", width.mm,"mm)", sep = ""))

  # ---------------------------------
  #  PDF
  # ---------------------------------

  # R device: cairo_pdf
  # creator: cairo 1.8.10
  # fonts: NimbusSansL (Type1)
  # version: 1.4
  # viewers: acroread evince

  # 'title' argument not supported
  # tested with just 'width'

  pdffilea <- paste(filestub, "a", "pdf", sep = ".")
  message("robbie.saveX11: making PDF: ", pdffilea)
  if ( file.exists(pdffilea) ) file.remove(pdffilea)
  funcpdfa <- function()
    {
      pdfa <- dev.print(  device     = cairo_pdf
                        , file       = pdffilea
                        )
      robbie.rofile(pdffilea)
      report <<- append(report, paste("PDF a [", pdfa, "] (no width)", sep = ""))
    }
  try( funcpdfa() )

  pdffileb <- paste(filestub, "b", "pdf", sep = ".")
  message("robbie.saveX11: making PDF: ", pdffileb)
  if ( file.exists(pdffileb) ) file.remove(pdffileb)
  funcpdfb <- function()
    {
      pdfb <- dev.print(  device     = cairo_pdf
                        , file       = pdffileb
                        , width      = width.mm / robbie.mm2inch()
                        , height     = width.mm / robbie.mm2inch()
                        )
      robbie.rofile(pdffileb)
      report <<- append(report, paste("PDF b [", pdfb, "] (width = ", width.mm, "mm)", sep = ""))
    }
  if ( format.b ) try( funcpdfb() )

  # ---------------------------------
  #  PNG
  # ---------------------------------

  # viewers: display eog gthumb
  #
  # must set both width and height as required
  # background color not shown by 'display', but described in "Image Info"

  pngfile <- paste(filestub, "png", sep = ".")
  message("robbie.saveX11: making PNG: ", pngfile)
  if ( file.exists(pngfile) ) file.remove(pngfile)
  funcpng <- function()
    {
      png <- dev.print(  device     = png
                       , file       = pngfile
                       , width      = size.px
                       , height     = size.px
                       , bg         = "white"    # CAUTION: "transparent" would not take
                       )
      robbie.rofile(pngfile)
      report <<- append(report, paste("PNG [", png, "] (size = ", size.px, "px)", sep = ""))
    }
  try( funcpng() )

  # ---------------------------------
  #  SVG
  # ---------------------------------

  # viewers: gthumb firefox inkscape'

  svgfilea <- paste(filestub, "a", "svg", sep = ".")
  message("robbie.saveX11: making SVG: ", svgfilea)
  if ( file.exists(svgfilea) ) file.remove(svgfilea)
  funcsvga <- function()
    {
      svga <- dev.print(  device     = svg
                        , file       = svgfilea
                        )
      robbie.rofile(svgfilea)
      report <<- append(report, paste("SVG a [", svga, "] (no width)", sep = ""))
    }
  try( funcsvga() )

  svgfileb <- paste(filestub, "b", "svg", sep = ".")
  message("robbie.saveX11: making SVG: ", svgfileb)
  if ( file.exists(svgfileb) ) file.remove(svgfileb)
  funcsvgb <- function()
    {
      svgb <- dev.print(  device     = svg
                        , file       = svgfileb
                        , width      = width.mm / robbie.mm2inch()
                        , height     = width.mm / robbie.mm2inch()
                        )
      robbie.rofile(svgfileb)
      report <<- append(report, paste("SVG b [", svgb, "] (width = ", width.mm,"mm)", sep = ""))
    }
  if ( format.b ) try( funcsvgb() )

  # ---------------------------------
  #  mop up
  # ---------------------------------

  # report saved formats
  message("robbie.saveX11: saved formats: ", paste(report, collapse = ", "))

  # add PDF author and say PDF rotation (extend, turn into calls)
  if ( file.exists(pdffilea) ) {
    robbie.robbiePDF(pdffilea)
    robbie.rotatePDF(pdffilea)
  }
  if ( file.exists(pdffileb) ) {
    robbie.robbiePDF(pdffileb)
    robbie.rotatePDF(pdffileb)
  }

  # clean run indication
  message("robbie.saveX11: complete run")
}

# ---------------------------------
#  function : robbie.robbiePDF
# ---------------------------------
#  description  : add author to a PDF
#  status       : working
#  requires     : 'pdftk' and custom script 'pdfrobbie'
#
#  note: 'pdfrobbie' could be written out
#
# ---------------------------------

robbie.robbiePDF <- function (pdffile)
{
  pdftk <- "pdftk"
  call <- paste("pdfrobbie", pdffile, "1>/dev/null")
  ret <- system(call,
                intern = FALSE,              # do not capture terminal output
                ignore.stderr = FALSE)       # allow 'stderr' to print
  message("robbie.robbiePDF: updated PDF author field (pdfrobbie) : ", pdffile)
  ret
}

# ---------------------------------
#  function : robbie.rotatePDF
# ---------------------------------
#  description  : rotates a PDF
#  status       : working
#  requires     : 'pdftk'
# ---------------------------------

robbie.rotatePDF <- function (pdffile)
{
  pdftk <- "pdftk"
  pdffilerot <- sub("\\.pdf$", ".rot.pdf", pdffile)    # add ".rot" qualifier
  call <- paste(pdftk, pdffile, "cat 1E", "output", pdffilerot)
  ret <- system(call,
                intern = FALSE,              # do not capture terminal output
                ignore.stderr = FALSE)       # allow 'stderr' to print
  message("robbie.rotatePDF: rotated PDF: ", pdffile)
  ret
}

# ---------------------------------
#  function : robbie.saveX11s
# ---------------------------------
#  description  : save all open plot windows
#  status       : work-in-progress
# ---------------------------------

robbie.saveX11s <- function (filestub = NULL,      # save name
                             width.mm = 150,       # vector formats
                             size.px  = 700,       # bitmap formats
                             dbug     = 1,         # should be 1 unless testing
                             format.b = FALSE)
{
  # opening
  DBUG <- dbug
  if ( DBUG > 1) message("................................................................................")
  if ( DBUG > 1) message("")

  # command-line arguments, including defaults
  message("robbie.saveX11s: commencing,",
          " filestub = \""   , filestub, "\"",
          " / width.mm = "   , width.mm,
          " / size.px = "    , size.px,
          " / dbug = "       , dbug)
  on.exit(message("robbie.saveX11s: complete (on.exit)"))

  # report device list
  message("robbie.saveX11s: device list")
  print(dev.list())

  # device list
  xs   <- dev.list()
  if ( is.null(xs) ) {
    message("robbie.saveX11s: ABANDONING: no devices open (check there is at least one plot frame open)")
    stop("no devices to print from")
  }

  # weed out non-screen devices, first identify
  # prevailing X11 tag and then filter the 'dev.list()'
  # recovered earlier
  tag <- "Xll"                                    # presume no cairo support
  x <- capabilities("cairo")                      # named integer vector
  if ( length(x) > 0 ) if ( x ) tag <- "X11cairo" # cairo support
  xs <- xs[names(xs) == tag]
  screens <- length(xs)
  message("robbie.saveX11s: screen devices identified: ", screens)

  # process filestub if global on an R workspace basis
  # (rather than a current directory files list basis)
  #
  # note > rm(robbie.countstub)
  starttext <- "countplot"
  if ( ! exists("robbie.countstub") ) {
    text  <- starttext
    count <- 1
    robbie.countstub <<- sprintf("%s-%02d", text, count)    # namely "countplot-01"
    message("robbie.saveX11s: original: text = ", text, ", count = ", count, ", filestub = ", robbie.countstub)
  } else {
    text   <- unlist(strsplit(robbie.countstub, "-"))[1]    # grab text part, like "countplot"
    digits <- unlist(strsplit(robbie.countstub, "-"))[2]    # grab counter part, like "00"
    count  <- as.numeric(digits)                            # string to number coercion
    count  <- count + 1
    robbie.countstub <<- sprintf("%s-%02d", text, count)
    message("robbie.saveX11s: increment: text = ", text, ", count = ", count, ", filestub = ", robbie.countstub)
  }
  filestub <- robbie.countstub               # CAUTION: important (disable to stop countstub process)

  # seek filestub if not supplied
  if ( is.null(filestub) ) {                 # NULL, meaning not passed in
    defaultstub <- "saveplot"
    prompt <- sprintf("  enter filestub [%s]? ", defaultstub)
    filestub <- readline(prompt)
    if ( nchar(filestub) == 0 ) {            # empty string, meaning the user hit enter
      filestub <- defaultstub
      message("robbie.saveX11s: filename defaulting to: ", filestub, ".*")
    }
  }
  filestub <- robbie.filestub(filestub)      # currently hollow

  # main loop
  loop <- 0
  for ( i in 1:length(xs) ) {
    stub <- paste(filestub, i, sep=".")
    message("robbie.saveX11s: stub: ", stub)
    print(xs[i])
    robbie.saveX11(stub,
                   device   = xs[i],
                   dbug     = dbug,
                   format.b = format.b)
    loop <- loop + 1
  }

  # completion reporting
  message("robbie.saveX11s: reset counter with: > rm(robbie.countstub)")
  message("robbie.saveX11s: loops: ", loop)
  message("robbie.saveX11s: complete run")
}

# ---------------------------------
#  function : robbie.killX11s
# ---------------------------------
#  description  : kill X11 devices
#  comment      : badly written loop code - see 'robbie.killDisplays' instead!
#  status       : complete and working
# ---------------------------------

robbie.killX11s <- function ()
{
  # host specific settings
  host <- Sys.info()["nodename"]
  if      ( host == "sojus" ) tag <- "X11"        # version 2.3.1
  else if ( host == "hinau" ) tag <- "X11cairo"   # version 2.10.1
  else                        tag <- "X11"

  xs   <- dev.list()
  loop <- 0
  if ( ! is.null(xs) ) {
    for ( i in 1:length(xs) ) {
      if ( names(xs[i]) == tag ) {
        dev.off(xs[i])
        loop <- loop + 1
      }
    }
  }
  message("robbie.killX11s: complete, count ", loop)
}

# ---------------------------------
#  function : robbie.killDisplays
# ---------------------------------
#  description  : kill X11 and ImageMagick 'display' devices
#  comment      : well-coded
#  status       : working
# ---------------------------------

robbie.killDisplays <- function ()
{
  # ---------------------------------
  # kill R plot windows
  # ---------------------------------

  # the following is based on 'graphics.off()' code

  loop <- 0
  while ( (which <- dev.cur()) != 1 )
    {
      dev.off(which)
      loop <- loop + 1
    }
  message("robbie.killDisplays: R device kill complete, count ", loop)

  # ---------------------------------
  # kill 'GraphViz' plots
  # ---------------------------------

  # assumes such plots displayed via ImageMagick 'display' utility

  call <- "killall --quiet display"
  sret <- system(call,
                 intern = FALSE,             # do not capture terminal output
                 ignore.stderr = FALSE)      # allow 'stderr' to print
  sret <- sret / 256                         # quirk of R to multiply return by 256, see "?system"
  if      ( sret ==   0 ) message("robbie.killDisplays: ImageMagick display kill complete, count 1 or more")
  else if ( sret ==   1 ) message("robbie.killDisplays: ImageMagick display kill complete, count 0")
  else if ( sret == 127 ) message("robbie.killDisplays: system command \"", call, "\" could not be run")
  else                    message("robbie.killDisplays: system command \"", call, "\" returned fail: ", sret)

  # return
  invisible()
}

#   if ( ! is.null(dev.list()) ) {
#     for ( i in dev.list() ) {
#       dev.off(i)
#     }

#  $Id: userFuncs.R 6005 2011-02-25 18:12:42Z robbie $
#  end of file

