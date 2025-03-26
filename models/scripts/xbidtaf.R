#! /usr/bin/Rscript --vanilla

#  file-purpose     : plot bidsets and tariffsets in various ways
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Tue 20-Dec-2011 23:49 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9268 $
#  $Date: 2012-07-23 15:53:21 +0200 (Mon, 23 Jul 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/xbidtaf.R $

# TODO
#
#  - resolve any TOFIX and ##
#  - integrated color spec
#  - save functionality
#
# NOTES
#
#   Separators (as set in 'xeona' 'common.cc')
#
#     * bid separator    : *
#     * bidset separator : /
#
#   As currently coded (minimization in LMP routines),
#   'xeona' produces bidsets and tariffsets in lower
#   semi-continuous form (refer wikipedia page
#   "semi-continuity")
#

# ---------------------------------
#  user modifiable
# ---------------------------------

study         <- "trial-008"                 # not flexible

default.scen  <- "e"
default.scen  <- "+"

# ==== command processing section ==============================================================

# ---------------------------------
#  libraries
# ---------------------------------

# functions may also 'require' libraries

ret <- suppressPackageStartupMessages(expr = T)

library(getopt)                              # straightforward custom installation
library(tools)                               # standard library package

library(robbie)                              # utilities (my private library)
library(xem)                                 # 'xeona' XEM files (private library)

# overwrite 'xem.plot.win' 'xem.plot.svg' package defaults here if you wish

# ---------------------------------
#  function : displayUsage
# ---------------------------------
#  description  : display usage message and quit
#  role         : used by option '--help'
#  status       : complete
# ---------------------------------

displayUsage <- function(script,             # script name
                         exit)               # required exit status
{
  message()
  message("               usage: ", script, "                 show internal database and exit")
  message("                      ", script, "   [opts]        run script (--keep is useful)")
  message("                      ", script, "  --help     -h  display help message and exit")
  message("        role options:  --role0              -0  plot all roles")
  message("                       --role1              -1  plot supply or demand curves")
  message("                       --role2              -2  plot bid metrics timeseries")
  message("   selection options:  --scen <leta>        -s  run 'leta' scenario (instead of default '", default.scen, "')")
  message("        plot options:  --cut <00:00>        -c  subset plots to [00,00) zero-based steps (omitted values seen as start and finish)")
  message("                       --grid               -g  add grid (ignore any --gui warning message from Rscript)")
  message("                       --identify           -i  invoke 'identify' to record points (role 2 only)")
  message("    behavior options:  --export             -x  interactive export (silently overwrites existing files unless readonly)")
  message("                       --eXport             -X  non-interactive export (silently overwrites existing files unless readonly)")
  message("                       --keep               -k  use Tk dialog to retain plot windows")
  message("                       --report             -r  report useful information")
  message(" development options:  --debug              -d  add debug messages but exclude timeseries processing")
  message("                       --Debug              -D  add debug messages and include timeseries processing")
  message("                       --sleep <seconds>    -S  sleep for 'seconds' after each plot (some cases only)")
  message("                       --test               -t  run special code (may well be inoperative)")
  message("             purpose: plot bidsets and tariffsets in various ways")
  message("               notes: the lime-yellow line is the weighted-average price, relative to quantity")
  message("                      the bidset and tariffset plots are \"lower semi-continuous\"")
  message("                      attempting to overwrite a readonly file is fatal (delete or chmod files before invoking this functionality)")
  message("            examples: plot bidset and tariffset curves using default scenario '", default.scen,"'      $ ", script, " -1kr")
  message("                      plot bidset and tariffset curves using nominated scenario 'e'    $ ", script, " -1krs e")
  message("                      plot timeseries using default scenario '", default.scen,"'                       $ ", script, " -2kr")
  message("                      ditto but examine just the opening phase                         $ ", script, " -2krc :1000")
  message("                      plot timeseries using nominated scenario 'e'                     $ ", script, " -2krs e")
  message("                      plot everything                                                  $ ", script, " -0kr")
  message()
  quit(save = "no", status = exit, runLast = F)
}

# ---------------------------------
#  command-line processing
# ---------------------------------

# first define some exit codes
exit <- list(success            = 0,
             failure            = 1,
             usage              = 2,
             development        = 255)

# recover script name
script <- robbie.getScriptName()

# define no catalog message
nocatmsg <- paste(script, ":", "a catalog file must be specified using --cat00 or --file <file> or --all must be used (try --help)")

# recover command line args
clargs <- commandArgs(trailingOnly = T)
if ( length(clargs) == 0 )
  {
#     message(nocatmsg)
#     quit(save = "no", status = exit$usage, runLast = F)
    clargs <- "(none)"                       # the above rule may change
  } else {
    clargs <- paste(clargs, sep = "", collapse = " ")
  }

# usage specification
spec = c(                                    # 'getopt' specification
  'cut'          , 'c', 1, "character",
  'debug'        , 'd', 0, "logical",
  'Debug'        , 'D', 0, "logical",
  'eXport'       , 'X', 0, "logical",
  'export'       , 'x', 0, "logical",
  'grid'         , 'g', 0, "logical",
  'help'         , 'h', 0, "logical",
  'identify'     , 'i', 0, "logical",
  'keep'         , 'k', 0, "logical",
  'report'       , 'r', 0, "logical",
  'role0'        , '0', 0, "logical",
  'role1'        , '1', 0, "logical",
  'role2'        , '2', 0, "logical",
  'scens'        , 's', 1, "character",
  'sleep'        , 'S', 1, "integer",
  'test'         , 't', 0, "logical")

# 'getopt' call
opt <- getopt(spec = matrix(ncol = 4, data = spec, byrow = T))   # principal call

# set 'debug' and 'report' defaults early
if ( is.null(opt$debug)    ) opt$debug    <- F
if ( is.null(opt$Debug)    ) opt$Debug    <- F
if ( is.null(opt$report)   ) opt$report   <- F

# ensure the debug trips align, mostly 'opt$debug' is used in the code
if ( opt$Debug ) opt$debug = T

# 'help' and friends -- order is significant
if ( ! is.null(opt$help)    ) displayUsage (script, exit = exit$success)

# set some more defaults (CAUTION: NA for those that can be redefined in a catalog file)
if ( is.null(opt$cut)       ) opt$cut       <- ":"
if ( is.null(opt$eXport)    ) opt$eXport    <- F
if ( is.null(opt$export)    ) opt$export    <- F
if ( is.null(opt$help)      ) opt$help      <- F
if ( is.null(opt$grid)      ) opt$grid      <- F
if ( is.null(opt$identify)  ) opt$identify  <- F
if ( is.null(opt$keep)      ) opt$keep      <- F
if ( is.null(opt$report)    ) opt$report    <- F
if ( is.null(opt$role0)     ) opt$role0     <- F
if ( is.null(opt$role1)     ) opt$role1     <- F
if ( is.null(opt$role2)     ) opt$role2     <- F
if ( is.null(opt$sleep)     ) opt$sleep     <- 0
if ( is.null(opt$scens)     ) opt$scens     <- default.scen
if ( is.null(opt$test)      ) opt$test      <- F

# dynamic terminal width for R reporting
width <- robbie.updateTerminalWidth()

# ensure the export trips align, mostly 'opt$export' is used in the code
if ( opt$eXport ) opt$export = T

# sort out roles
if ( opt$role0 )
  {
    opt$role1 <- T
    opt$role2 <- T
  }

# end of command-line processing reporting
if ( opt$report ) message()
if ( opt$report ) message("cut                 : ", opt$cut)
if ( opt$report ) message("scenario            : ", opt$scen)
if ( opt$report ) message("terminal width      : ", width)

# ==== active section ====================================================================

# integrity check and choke
if ( nchar(opt$scen) != 1 )
  {
    message()
    message("caution             : problem with 'scen' argument")
    message("fatal               : ", exit$usage)
    message()
    quit(save = "no", status = exit$usage, runLast = F)
  }

# model meta-data
if ( opt$debug ) message()
steps    <- xem.grab(paste(study, opt$scen, "entity.time-horizon.steps",    sep = "."), report = opt$debug)
interval <- xem.grab(paste(study, opt$scen, "entity.time-horizon.interval", sep = "."), report = opt$debug)
usedsvn  <- xem.grab(paste(study, opt$scen, "program.last-run.used-svn",    sep = "."), report = opt$debug)

# model name
model    <- paste(study, opt$scen, "xem", sep = ".")

# cut processing call, expands 'cut' while honoring 'steps'
if ( opt$debug ) message()
cutdata <- xem.processCutArg(cut    = opt$cut,
                             steps  = steps,
                             report = opt$debug)
opt$cut <- cutdata$rrange
if ( opt$report ) message("cut (rranged)       : ", opt$cut)

# integrity report and choke
if ( cutdata$good == FALSE )
  {
    message()
    message("caution             : 'xem.processCutArg' just returned fail, check 'cut' argument")
    message("fatal               : ", exit$usage)
    message()
    quit(save = "no", status = exit$usage, runLast = F)
  }

# ---------------------------------
#  function : trim
# ---------------------------------
#  description  : trim space and/or tab characters from both ends of a given string
#  role         : utility
#  takes        : string (or vector of strings)
#  returns      : string (or vector of strings)
#  techniques   : 'sub'
#  status       : complete
#
#  terminology-wise, a normal string (in other
#  languages) is known in R as a "single character
#  string" and represented by a "length one character
#  vector"
#
# ---------------------------------

trim <- function (s)
{
  if ( ! is.character(s) ) warning("string vector expected but instead got class '", class(s), "'", immediate. = T)

  s <- sub("^[[:blank:]]+", "", s)
  s <- sub("[[:blank:]]+$", "", s)
  return(s)
}

# ---------------------------------
#  function : dequote
# ---------------------------------
#  description  : remove double quotes from 's'
#  role         : utility
#  takes        : string (or vector of strings)
#  returns      : string (or vector of strings)
#  techniques   : 'sub'
#  status       : complete
#
#  terminology-wise, a normal string (in other
#  languages) is known in R as a "single character
#  string" and represented by a "length one character
#  vector"
#
# ---------------------------------

dequote <- function (s)
{
  if ( ! is.character(s) ) warning("string vector expected but instead got class '", class(s), "'", immediate. = T)

  s <- sub("^\"[[:space:]]*", "", s)
  s <- sub("[[:space:]]*\"$", "", s)
  return(s)
}

# ---------------------------------
#  function : getxset
# ---------------------------------
#  description  : obtain an 'xset', meaning either a "bidset" or a "tariffset"
#  role         : call
#  status       : complete
#
#  Processing options
#
#    "xsets"   : can be simple or "/"-separated - returns character vector of length one or several
#    "tseries" : timeseries of simple xsets     - returns character vector of length steps
#
#  Value
#
#    Function returns a vector of bidset strings in the
#    canonical form:
#
#      "2.0e+00 * 210.0e+06 40.0e-09 * 210.0e+06 60.0e-09"
#
#  CAUTION: not supported
#
#    In the interests of simplicity, only simple
#    timeseries are considered here and "/"-separated
#    timeseries are not supported.
#
# ---------------------------------

getxsets <- function(xfqf,                   # extended fully-qualified field name
                     type = "xsets")         # also "tseries"
{
  # nested function
  procts <- function(value)
    {
      value <- paste("\"", value, "\"", sep = " ")
      xsets <- unlist(strsplit(value, split = "\" \"", fixed = T))
      if ( xsets[1] == "" ) xsets <- xsets[-1]    # the trailing case is automatically omitted
      invisible(xsets)
    }

  # main code
  if ( opt$debug ) message()                 # compensate for 'xem.grab' call messaging
  value <- xem.grab(xfqf = xfqf, want = "string", report = opt$debug)      # trimmed string

  # basic protection (left-to-right short-circuit evaluation)
  if ( length(value) == 1 && is.na(value) ) warning("returned 'value' is NA (trouble ahead)", immediate. = T)

  # reporting
  trip <- 5                                  # limit console reporting to something useful
  len  <- length(value)
  if ( opt$report )
    {
      message()
      message("value               : ")
      if ( len < 2 * trip + 2 )
        {
          message(paste(value, collapse = "\n"))
        } else {
          message(paste(value[1:trip], collapse = "\n"))
          message("...")
          message(paste(value[(len - trip + 1):len], collapse = "\n"))     # CAUTION: bracket essential
        }
    }

  if ( any(is.na(value))      ) return(character(0))
  if ( any(nchar(value) == 0) ) return(character(0))
  xsets <- switch(type,
                  xsets   = unlist(strsplit(value, split = "/", fixed = T)),
                  tseries = procts(value))

  xsets <- trim(xsets)
  xsets <- dequote(xsets) # remove enclosing double quotes

  # reporting
  if ( opt$report ) message()
  if ( opt$report ) message("value length        : ", length(value))
  if ( opt$report ) message("value nchars first  : ", nchar(value[1]))
  if ( opt$report ) message("getxsets type       : ", paste("\"", type, "\"", sep = ""))
  if ( opt$report ) message("xsets length        : ", length(xsets))

  # return
  return(xsets)
}

# ---------------------------------
#  function : sortx
# ---------------------------------
#  description  : sort an 'xset' on unit price
#  role         : call
#  note         : not currently invoked, issues a warning in any case
#  status       : incomplete
# ---------------------------------

sortx <- function(xset)
{
  if ( ncol(xset) != 2 ) warning("xset columns not two: ", ncol(xset), immediate. = T)
  warning("function not implemented (returning 'xset' without mods)", immediate. = T)
  buffer <- xset
  return(buffer)
}

# ---------------------------------
#  function : makeQP
#  function : addQP
# ---------------------------------
#  description  : create new matrix 'QP' to hold raw points
#  description  : add a new pair of plot points to matrix 'QP'
#  role         : support for 'unpackx'
#  status       : complete
#
#  Design note
#
#    Function 'makeQP' adds the fixed price as the
#    first col two entry.
#
#    Note that an empty matrix can be formed, but not
#    by default: 'nrow' and 'ncol' must both be set to
#    zero.
#
# ---------------------------------

makeQP <- function(fixed)
{
  QP <- matrix(data     = c(0.0, fixed),     # holder for the 'fixed' price
               nrow     = 1,
               ncol     = 2,
               dimnames = list(c("fixed"), c("quantity", "unitprice")),
               byrow    = T)
  if ( opt$Debug ) message()
  if ( opt$Debug ) message("matrix QP initial   : ", "(below)")
  if ( opt$Debug ) print(QP)
  return(QP)
}

addQP <- function(QP,                        # raw point matrix
                  q,                         # quantity in absolute terms
                  p)                         # unit price in absolute terms
{
  if ( opt$Debug ) message()
  if ( opt$Debug ) message("QP dimensions in    : ", paste(dim(QP), collapse = " "))
  if ( ncol(QP) != 2 ) warning("QP columns not two: ", ncol(QP), immediate. = T)
  if ( opt$Debug ) message("arguments q p       : ", q, " ", p)

  r     <- nrow(QP)                          # current number of rows
  Q     <- QP[r,1]                           # previous quantity (set to zero for first visit, see 'makeQP')
  left  <- c(Q, p)                           # left point
  right <- c(q, p)                           # right point
  QP    <- rbind(QP, left)
  QP    <- rbind(QP, right)

  if ( opt$Debug ) message("left                : ", paste(left,  collapse = " "))
  if ( opt$Debug ) message("right               : ", paste(right, collapse = " "))
  if ( opt$Debug ) message("QP dimensions out   : ", paste(dim(QP), collapse = " "))

  return(QP)
}

# ---------------------------------
#  function : qpplot
# ---------------------------------
#  description  : plot matrix 'QP'
#  role         : call
#  status       : complete
#
#  Circle plotting
#
#    The line is lower semi-continuous because "lmp"
#    invokes minimization.  Hence the 'left' and
#    'right' plotting symbols.
#
#  Plot calls
#
#    null symbol : plot(pch = NA)
#    open dots   : plot(pch =  1)
#    black dots  : plot(pch = 19)
#    white dots  : plot(pch = 21, bg = "white")
#    filled dots : plot(pch = 21, bg = "yellow")
#
# ---------------------------------

qpplot <- function(qp,
                   units    = "*",           # extensive physical quantity, for instance: "kg"
                   title,
                   xmax     = NA,
                   ymax     = NA,
                   fixed    = NA,
                   wave     = NA,
                   grid     = F,
                   overplot = F)
{
  main <- title
  xlab <- paste("quantity ", "[", units, "]", sep = "")
  ylab <- paste("unit price ", "[$/", units, "]", sep = "")

  if ( opt$report ) message()
  if ( opt$report ) message("qp dimensions       : ", paste(dim(qp), collapse = " "))

  if ( opt$debug  ) message("qp matrix           : ", "(below)")
  if ( opt$debug  ) message()
  if ( opt$debug  ) scipen <- options(scipen = -9)               # fixed/scientific penalty (default 0)
  if ( opt$debug  ) print(qp)
  if ( opt$debug  ) scipen <- options(scipen = scipen)           # fixed/scientific penalty (default 0)
  if ( opt$debug  ) message()
  if ( opt$debug  ) message("overplot            : ", overplot)

  # TOFIX: 23-Dec-2011: could be improved

  # subtitle1
  bustup    <- unlist(strsplit(xfqf, split = ".", fixed = T))    ## could be improved
  bustup    <- bustup[-1]
  bustup    <- bustup[-1]
  fqf       <- paste(bustup, collapse = ".")
  subtitle1 <- fqf

  # subtitle2
  xfqf2     <- paste(study, opt$scen, "program.study-description.scenario-name", sep = ".")
  desc      <- xem.grab(xfqf = xfqf2, want = "string", report = opt$debug)
  subtitle2 <- paste(opt$scen, desc, sep = " = ")
  subtitle2 <- paste("scenario", subtitle2, sep = " ")

  # svg export (external data)
  mm2inch    <- function(dims){ return(dims / 25.4) }
  svg.size   <- c(mm2inch(xem.plot.svg$width.mm), mm2inch(xem.plot.svg$height.mm))
  svg.points <- xem.plot.svg$points.pt

  # export filename generation
  export.name <- NA
  if ( opt$export )                          # implied by 'opt$eXport'
    {
      export.name <- xem.generateExportName(stub  = study,                 # typically the study
                                            leta  = opt$scen,
                                            svn   = usedsvn,               # typically the run svn
                                            title = title,                 # typically the given plot title
                                            ext   = "svg",                 # reflects the export format
                                            ask   = ( ! opt$eXport ))      # 'F' for automatic filename generation
      message()
      message("export name         : ", export.name)
    }

  # plot
  if ( overplot == F )
    {
      x <- qp[,1]
      y <- qp[,2]

      fac     <- 1.2                                             # CAUTION: not less than 1.2
      xlim    <- if ( is.na(xmax) ) range(x) else c(0, xmax)
      ylim    <- if ( is.na(ymax) ) range(y) else c(0, ymax)
      xlim[2] <- xlim[2] * fac
      ylim[2] <- ylim[2] * fac

      if ( opt$debug ) message("xlim                : ", paste(xlim, collapse = " "))
      if ( opt$debug ) message("ylim                : ", paste(ylim, collapse = " "))

      # open device
      if ( opt$export )                      # implied by 'opt$eXport'
        {
          svg(filename  = export.name,
              width     = svg.size[1],
              height    = svg.size[2],
              pointsize = svg.points,
              bg        = "white")
        }
      else
        {
          x11()
        }

      # plot
      par(mar = c(5, 4, 4 + 1,  2) + 0.1)    # extra line at top (pos 3)
      plot(x    = qp[,1],                    # R built-in
           y    = qp[,2],
           xlim = xlim,
           ylim = ylim,
           ann  = F,
           pch  = NA)

      # top titles
      title(main = main, line = 3.5)
      title(ylab = ylab, xlab = xlab)
      mtext(text = subtitle1, side = 3, line = 2)
      mtext(text = subtitle2, side = 3, line = 1)

      # right-side annotation
      utcstamp <- format(as.POSIXlt(Sys.time(), tz = "GMT"), "%d-%b-%Y %H:%M UTC")
      msgs <- character()
      msgs <- append(msgs, paste("script",  script,   sep = " : "))
      msgs <- append(msgs, paste("model",   model,    sep = " : "))
      msgs <- append(msgs, paste("svn",     usedsvn,  sep = " : "))
      msgs <- append(msgs, paste("plotted", utcstamp, sep = " : "))
      msg  <- paste(msgs, collapse = " | ")
      mtext(msg, side = 4, adj = 0.05, cex = 0.55)     # was: cex = 0.75

      # fixed component text
      if ( ! is.na(fixed) )
        {
          if ( T                 ) fixstr <- sprintf("%.2f", fixed)
          if ( abs(fixed) > 10.0 ) fixstr <- sprintf("%.2e", fixed)
          if ( abs(fixed) <  0.1 ) fixstr <- sprintf("%.2e", fixed)
          if ( fixed     ==  0.0 ) fixstr <- "0.0"
          label <- paste("fixed charge =", fixstr, "$/s", sep = " ")
          text(x = xlim[2], y = ylim[1], labels = label, adj = c(+1.0, -1.5), cex = 0.75)
        }

      # line and test for weighted average 'wave'
      if ( ! is.na(wave) )
        {
          col  <- rev(rainbow(11))[8+1]
          line <- matrix(data = c(xlim[1], wave, xlim[2], wave), ncol = 2, byrow = T)
          lines(line, col = col)

          label <- paste("weighted", "average", sep = "\n")
          text(x = xlim[2], y = wave, labels = label, adj = c(+1.0, -0.5), cex = 0.75)
        }
    }

  # grid
  if ( grid ) grid()

  # add axes
  abline(h = 0, col = "gray")
  abline(v = 0, col = "gray")

  # define the loop range 'even'
  even <- seq(from = 2, to = nrow(qp) - 1, by = 2)
  if ( opt$debug ) message("even                : ", paste(even, collapse = " "))

  # overprint vertical (riser) line segments
  for (i in even)
    {
      if ( i == 2 ) next
      down <- qp[i-1,]                       # R vector, down dot (supply curve)
      up   <- qp[i+0,]                       # R vector, up dot
      segment <- matrix(c(down, up), ncol = 2, byrow = T)
      lines(segment, lty = 2, col = "gray")
    }

  # TOFIX: 19-Dec-2011: better to use 'subset' than 'matrix' calls

  # overprint horizontal (tread) line segments and dots
  beg <- 0                                   # beginning phantom point (determine how first point is plotted)
  end <- Inf                                 # ending phantom point (determine how last point is plotted)
  for (i in even)
    {
      left    <- qp[i+0,]                                   # R vector, left dot
      right   <- qp[i+1,]                                   # R vector, right dot
      lud     <- ifelse( i > 2, qp[i-1,2], beg)             # scalar, left updown value
      rud     <- ifelse( i < nrow(qp) - 1, qp[i+2,2], end)  # scalar, right updown value
      segment <- matrix(c(left, right), ncol = 2, byrow = T)
      lines(segment, lty = 1)                               # plot line
      left    <- matrix(c(left),  ncol = 2, byrow = T)
      right   <- matrix(c(right), ncol = 2, byrow = T)
      lcol    <- ifelse( left[2] > lud, "white", "black")   # [1]
      rcol    <- ifelse(right[2] > rud, "white", "black")   # [1]
      points(left,  pch = 21, bg = lcol)                    # plot left dot
      points(right, pch = 21, bg = rcol)                    # plot right dot [1]
      # white/black is lower semi-continuous (I guess black/white would be upper
      # semi-continuous, would probably need to adjust 'beg' and 'end' too)
    }

  # close device if 'svg'
  if ( opt$export ) dev.off()
}

# ---------------------------------
#  function : unpackx
# ---------------------------------
#  description  :
#  role         : call
#  status       : complete
#
#  Usage
#
#    QP <- unpackx(xset)
#
# ---------------------------------

unpackx <- function(xset)                         # "*"-separated string
{
  if ( opt$Debug ) message()
  if ( opt$Debug ) message("entering function 'unpackx'")
  if ( opt$Debug ) message()
  if ( opt$Debug ) message("xset (argument)     : ", xset)

  # integrity checks
  if ( ! is.character(xset) || ! is.vector(xset) ) warning("xset is not a character vector", immediate. = T)
  if ( length(xset) != 1                         ) warning("multiple xsets supplied: ", length(xset),  immediate. = T)
  if ( is.na(xset)                               ) warning("xsets is NA: ", xset,  immediate. = T)

  # 'xset' to 'x' split
  x <- unlist(strsplit(xset, split = "*", fixed = T))  # gives vector of space-separated (band, unitprice) pair strings
  x <- trim(x)

  # protection
  if ( opt$Debug ) message()
  if ( opt$Debug ) message("x length            : ", length(x))
  if ( length(x) == 0 ) warning("x is zero length", immediate. = T)
  if ( length(x) == 0 ) return(NULL)              # ## ponder would an empty matrix be better

  # rework the first term if solitary .. meaning this is the fixed price term
  fixed <- 0
  if ( grepl(" ", x[1], fixed = T) == FALSE )     # no space present
    {
      fixed <- as.numeric(x[1])                   # recover the 'fixed' term, overwrite 'fixed'
      x     <- x[-1]                              # remove 'fixed' term entry from 'x' altogether
    }

  # make QP matrix
  qp <- makeQP(fixed)

  # unpack main part
  if ( length(x) > 0 )                            # protect against no substantive bids
    {
      quantity <- 0
      for (i in 1:length(x))
        {
          buff     <- unlist(strsplit(x[i], split = " ", fixed = T))
          if ( length(buff) != 2 ) warning("split not length two: ", length(buff), immediate. = T)
          if ( opt$Debug ) message("buff                : '", paste(buff, collapse = "' '"), "'")
          band     <- as.numeric(buff[1])
          unit     <- as.numeric(buff[2])        # unit price without 'fixed' component
          quantity <- quantity + band
          price    <- unit
          qp       <- addQP(qp, quantity, price) # add two further raw points to the QP matrix
        }
    }

  # return
  return(qp)
}

# ---------------------------------
#  function : metrix
# ---------------------------------
#  description  : obtain xset metrics from raw points matrix 'qp'
#  role         : call
#  status       : complete
#
#  Output
#
#    type    : named numeric vector
#    names   : "fixed" "first" "last" "low" "wave" "high" "cap" "count"
#    note    : "first" "last" "low" "wave" "high" are unit prices
#
#  Usage
#
#    metrics    <- metrix(QP)
#    fixedprice <- metrics["fixed"]
#
# ---------------------------------

metrix <- function(qp)
{
  # integrity checks
  if ( ! is.matrix(qp) || ! is.numeric(qp) ) warning("qp is not a numeric matrix", immediate. = T)
  if ( nrow(qp) == 0                       ) warning("qp matrix lacks rows: ", nrow(qp), immediate. = T)

  # remove names and dimnames, otherwise they stick
  qp <- unname(qp)

  # preliminaries
  fixed <- qp[1,2]
  beg   <- NA
  end   <- NA
  min   <- +Inf
  max   <- -Inf
  area  <- 0                                 # used to calculate 'wave'
  cap   <- 0
  count <- 0

  # extract and process data
  even <- seq(from = 2, to = nrow(qp) - 1, by = 2)
  for (i in even)
    {
      count <- count + 1
      left  <- qp[i,]                        # R vector, left dot
      right <- qp[i + 1,]                    # R vector, right dot
      bar   <- (right[1] - left[1]) * right[2]
      area  <- area + bar
      if ( i == 2 ) beg <- right[2]
      cap   <- right[1]                      # continually updated
      end   <- right[2]                      # continually updated
      min   <- min(min, end)
      max   <- max(max, end)
    }
  wave <- ifelse(cap == 0, NA, area / cap)   # weighted average if cumulative 'area' normalized by 'cap'

  # load and return named vector
  metrics <- unlist(list(fixed = fixed,      # fixed price is NOT a unit price -- it has a different dimensionality
                         first = beg,
                         last  = end,
                         low   = min,
                         wave  = wave,       # weighted average
                         high  = max,
                         cap   = cap,
                         count = count))
  return(metrics)
}

# ---------------------------------
#  function : tseriesplot
# ---------------------------------
#  description  : plot a timeseries of simple xsets metrics
#  role         : high-level plot function
#  status       : complete
# ---------------------------------

tseriesplot <- function(xfqf,
                        units,
                        title)
{

  if ( opt$debug ) message()
  if ( opt$debug ) message("entering function 'tseriesplot'")

  tseries <- getxsets(xfqf = xfqf, type = "tseries")
  tlen    <- length(tseries)

  if ( opt$debug ) message()
  if ( opt$debug ) message("tlen (for looping)  : ", tlen)

  data <- matrix(data = NA, nrow = tlen, ncol = 3)
  if ( opt$Debug ) message()
  if ( opt$Debug ) message("data (initial)      : ", "(below)")
  if ( opt$Debug ) print(data)
  for (i in 1:tlen)
    {
      # core calls
      xset    <- tseries[i]
      QP      <- unpackx(xset)

      if ( opt$Debug ) message()
      if ( opt$Debug ) message("QP dims             : ", paste(dim(QP), collapse = " "))
      if ( opt$Debug ) message("QP [3,2]            : ", QP[3,2])

      metrics <- metrix(QP)

      if ( opt$Debug ) message()
      if ( opt$Debug ) message("metrics             : ", "(below)")
      if ( opt$Debug ) message()
      if ( opt$Debug ) print(metrics)

      # obtain and store data
      fixed   <- metrics["fixed"]
      low     <- metrics["low"]
      wave    <- metrics["wave"]
      high    <- metrics["high"]
      count   <- metrics["count"]

      if ( opt$Debug ) message()
      if ( opt$Debug ) message("wave                : ", wave)
      if ( opt$Debug ) message("count               : ", count)

      data[i,1] <- low
      data[i,2] <- wave
      data[i,3] <- high
    }

  if ( opt$Debug ) message()
  if ( opt$Debug ) message("data (filled)       : ", "(below)")
  if ( opt$Debug ) message()
  if ( opt$Debug ) print(data)

  # timebase
  tbase <- seq(tlen) - 1

  # truncate matrix and timebase
  rowz  <- eval(parse(text = opt$cut))       # parse and evaluate the string form "01:01"
  data  <- data[rowz,]                       # truncate
  tbase <- tbase[rowz]

  # y-axis limits
  ylim <- range(data)
  if ( ylim[1] > 0 ) ylim[1] <- 0

  # report
  if ( opt$debug ) message()
  if ( opt$debug ) message("data dims           : ", paste(dim(data), collapse = " "))
  if ( opt$debug ) message("base length         : ", length(tbase))

  # set up colors
  color.cnt <- 14
  color.beg <- 11

  color.cnt <- 11
  color.beg <- 08

  colors <- rev(rainbow(color.cnt))

  # TOFIX: 23-Dec-2011: the following is a bit rough

  # subtitle1
  bustup    <- unlist(strsplit(xfqf, split = ".", fixed = T))    ## could be improved
  bustup    <- bustup[-1]
  bustup    <- bustup[-1]
  fqf       <- paste(bustup, collapse = ".")
  subtitle1 <- fqf

  # subtitle2
  xfqf2     <- paste(study, opt$scen, "program.study-description.scenario-name", sep = ".")
  desc      <- xem.grab(xfqf = xfqf2, want = "string", report = opt$debug)
  subtitle2 <- paste(opt$scen, desc, sep = " = ")
  subtitle2 <- paste("scenario", subtitle2, sep = " ")

  # export filename generation
  export.name <- NA
  if ( opt$export )                          # also implied by 'opt$eXport'
    {
      export.name <- xem.generateExportName(stub  = study,               # typically the study
                                            leta  = opt$scen,
                                            svn   = usedsvn,             # typically the run svn
                                            title = subtitle1,           # typically the given plot title
                                            ext   = "svg",               # reflects the export format
                                            ask   = ( ! opt$eXport) )    # 'F' for automatic filename generation
      message("export name         : ", export.name)
    }

  # plot
  xem.plot(timebase    = tbase,              # CAUTION: not 'xem.xplot' because we are overplotting
           timeseries  = data[,1],
           ylim        = ylim,
           grid        = opt$grid,
           title       = title,
           subtitle1   = subtitle1,
           subtitle2   = subtitle2,
           yano        = "unit price",
           units       = paste("[$/", units, "]", sep = ""),
           interval    = interval,
           script      = script,
           model       = model,
           svn         = usedsvn,
           type        = "n",
           export      = export.name,
           exportClose = F)                  # overplotting is required, hence the client must later: dev.off()

  points(tbase, data[,1], type = "l", col = colors[color.beg + 0])
  points(tbase, data[,2], type = "l", col = colors[color.beg + 1])
  points(tbase, data[,3], type = "l", col = colors[color.beg + 2])
  abline(h = 0, col = "gray")

  # close device if exporting

  if (   opt$eXport ) dev.off()              ## this may need to change to 'opt$export'
  if ( ! opt$eXport ) Sys.sleep(opt$sleep)

  # turn off any '--keep' option
  opt$keep <<- F

  # return
  return(NULL)
}

# ---------------------------------
#  function : xsetsplot
# ---------------------------------
#  description  : obtain xset metrics from raw points matrix 'qp'
#  role         : high-level plot function
#  status       : complete
# ---------------------------------

xsetsplot <- function(xfqf,
                      units,
                      title,
                      restrict = NA)         # normally single value or range
{
  xsets    <- getxsets(xfqf = xfqf, type = "xsets")
  xlenorig <- length(xsets)

  # restrict as required
  if ( ! is.na(restrict) ) xsets <- xsets[restrict]

  # make unique
  xsets <- unique(xsets)

  # determine revised length
  xlen <- length(xsets)

  # determine plot bounds
  xmax <- 0
  ymax <- 0
  for (i in 1:xlen)
    {
      xset  <- xsets[i]
      QP    <- unpackx(xset = xset)
      high  <- metrix(QP)["high"]
      cap   <- metrix(QP)["cap"]
      xmax  <- max(xmax, cap)
      ymax  <- max(ymax, high)
    }

  if ( opt$debug ) message()
  if ( opt$debug ) message("xmax ymax           : ", xmax, "  ", ymax)

  # submit plot information
  for (i in 1:xlen)
    {
      xset  <- xsets[i]
      QP    <- unpackx(xset = xset)
      fixed <- metrix(QP)["fixed"]
      wave  <- metrix(QP)["wave"]
      cap   <- metrix(QP)["cap"]

      if ( opt$debug ) message()
      if ( opt$debug ) message("xsets lengths       : ", xlenorig, " ", xlen)
      if ( opt$debug ) message("xmax ymax           : ", xmax, " ", ymax)

      qpplot(qp         = QP,                # plot matrix 'QP'
             units      = units,
             title      = title,
             xmax       = xmax,
             ymax       = ymax,
             fixed      = fixed,
             wave       = wave,
             grid       = opt$grid,
             overplot   = (i > 1))           # overplot after the first call
    }

  # return the counter
  return(i)
}

# ---------------------------------
#  function : insert
# ---------------------------------
#  description  : insert data
#  role         : used to load hard-coded data
#  status       : complete
# ---------------------------------

insert <- function(action,                   # T = enable, F = disable
                   plot,                     # 1 = plot 8760 supply/demand curve,  2 = plot small supply/demand curve, 3 = plot timeseries metrics (low, wave, high)
                   ref,
                   fqf,
                   units,
                   title   = "**",
                   comment = "")             # max 29 chars
{
  reference <- sprintf("%d.%02d", plot, ref)
  if ( nchar(comment) > 29 ) comment = paste(substring(comment, 1, 26), "..", sep = " ")
  entry     <- data.frame(fqf = fqf, units = units, title = title, reference = reference, plot = plot, action = action, comment = comment)
  df       <<- rbind(df, entry)
  if ( opt$debug ) message("insert row ", sprintf("%02d", nrow(df)), " complete")
}

# =================================
#  active code
# =================================

# ---------------------------------
#  preparation
# ---------------------------------

if ( opt$debug ) message()

df <- data.frame(fqf       = character(0),
                 units     = character(0),
                 title     = character(0),
                 reference = character(0),
                 plot      = integer(0),
                 action    = logical(0),
                 comment   = character(0))

# ---------------------------------
#  data
# ---------------------------------

# first three fields: action plot ref

# OVERPLOTTING : xsets overplotting - full takes about 15 minutes full or 10 minutes if uniqued

insert(F, 1, 1, "entity.teas-hydro-scheme-a10.submitted-bidsets"      ,    "W", "hydro scheme : submitted bidsets"                         , "15 min not uniqued")
insert(T, 1, 2, "entity.gate-stated-tariff-elec-a01.tariffsets"       ,    "W", "gateway : urban area supply tariffset (invariant)"        , "invariant")
insert(T, 1, 3, "entity.gate-stated-tariff-elec-a02.tariffsets"       ,    "W", "gateway : aluminium smelter supply tariffset (invariant)" , "invariant")
insert(T, 1, 4, "entity.gate-stated-tariff-ef-natgas-d01.tariffsets"  , "kg/s", "gateway : CCGT gas supply tariffset (invariant)"          , "invariant")

# FIRST ENTRY PLOTTING : xsets overplotting

insert(T, 3, 1, "entity.teas-hydro-scheme-a10.submitted-bidsets"      ,    "W", "hydro scheme : horizon start bidset"                      , "experimental")
insert(T, 3, 2, "entity.asop-lmp-bid-adaptive1-a02.opening-bidset"    ,    "W", "CCGT operator : horizon start bidset")
insert(T, 3, 3, "entity.asop-lmp-bid-adaptive1-a02.final-bidsets"     ,    "W", "CCGT operator : horizon end bidset"                       , "unit 1")
insert(T, 3, 4, "entity.asop-lmp-bid-param-a05.bidset-list"           ,    "W", "demand bidder : bidset (invariant)")

# TIMESERIES : xset timeseries normal plotting

insert(T, 4, 1, "entity.teas-hydro-scheme-a10.submitted-bidsets"      ,    "W", "hydro scheme operator : submitted bidsets")
insert(T, 4, 2, "entity.asop-lmp-bid-param-a05.submitted-bidsets"     ,    "W", "demand bidder : submitted bidsets")
insert(T, 4, 3, "entity.asop-lmp-bid-adaptive1-a02.submitted-bidsets" ,    "W", "CCGT operator : submitted bidsets")

# ---------------------------------
#  plotting
# ---------------------------------

message()
print(df)

for (i in 1:nrow(df))
  {
    # set up
    row  <- df[i,]                           # extract a 'row' for notational convenience
    xfqf <- paste(study, opt$scen, row$fqf, sep = ".")
    num  <- row$plot
    act  <- row$action
    if ( act == F ) next                     # skip the task

    # filter on role
    if ( ! opt$role1 && num <= 3 ) next
    if ( ! opt$role2 && num == 4 ) next

    # normal code again
    if ( num == 1)    xsetsplot(xfqf, row$units, row$title)
    if ( num == 2 )   xsetsplot(xfqf, row$units, row$title)
    if ( num == 3 )   xsetsplot(xfqf, row$units, row$title, restrict = 1)
    if ( num == 4 ) tseriesplot(xfqf, row$units, row$title)

    # sleep if set
    if ( num != 4 ) Sys.sleep(opt$sleep)
  }

# ---------------------------------
#  housekeeping
# ---------------------------------

# call up Tk dialog as required
xem.keepOpen(extra = paste("script :", script), action = opt$keep)   # note the 'opt$keep' test too

message()

#  $Id: xbidtaf.R 9268 2012-07-23 13:53:21Z robbie $
#  end of file

