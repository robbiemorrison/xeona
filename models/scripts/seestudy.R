#! /usr/bin/Rscript --vanilla

#  file-purpose     : process 'trial-000.log' high level performance log / script
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Tue 29-Nov-2011 19:35 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9195 $
#  $Date: 2012-03-21 18:24:34 +0100 (Wed, 21 Mar 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/seestudy.R $

#  TODO
#
#    - variable and function name rationalization (minor)
#
#  GRAPHICS EXPORT
#
#    - "tex" export is just for any text fragments
#    - "svg" export produces horribly large files (five times as great as the PDF route)
#    - "pdf" export is better, then load into 'inkscape' with low sampling (2.0 = rough)
#    - the SVG is very broken up (it would be, in theory, possible to rework)
#
#  GENERAL NOTES
#
#    * script developed on R version 2.10.1 (2009-12-14)
#    * alternative file: attic/01/trial-008.log
#    * function 'scatterplot3d' from package 'scatterplot3d' was tried but is not as good
#    * documentation: http://cran.r-project.org/web/packages/rgl/rgl.pdf
#    * for color names, see ? col2rgb and > colors()
#    * option '--png' currently requires the ImageMagick 'convert' utility for upscaling and sharpening
#
#  CODING STYLE
#
#    * Google R style guide : http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html
#    * the inbuilt 'attach' not used here in the interests of good style
#    * 0 is FALSE, 1 is TRUE
#
#  WORK-AROUNDS
#
#    * encountered problems with 'decorate3d' and the axes labels getting scrambled
#    * function 'file.choose' choked with "Error in file.choose() : file choice cancelled"
#
#  KNOWN ISSUES
#
#    * multiple plot windows
#
#        when more than one plot windows is open and a
#        plot is being manipulated, the following
#        warning note may arise:
#
#          do_wait: drmWaitVBlank returned -1, IRQs don't seem to be working correctly.
#          Try adjusting the vblank_mode configuration parameter.
#
#        there does not seemed to be a loss in
#        functionality -- and certainly nothing freezes
#        up.
#
#    * split short options (package 'getopt' shortcoming)
#
#        may result in: "WARNING: unknown gui '..', using X11"
#
#    * function 'file.remove' seems to return 1
#      irrespective of success, so just ignore
#
#  DOCUMENTATION
#
#    * $ find -P ~ -iname "*rgl*.pdf" -type f
#    * Kabacoff (2011) "R in action"
#    * Crawley (2007) "The R book"
#
#  PLOT WINDOWS SIZING
#
#    viewport  0   0 256 256
#    first     2 110 258 366
#    second    4 164 260 420
#    third     6 218 262 474
#
#  TRASHED CODE
#
#    For future reference:
#
#    window sizing    : $ svn cat --revision r8270 seestudy.R | less +/"open3dWindow"
#    simple plot test : $ svn cat --revision r8273 seestudy.R | less +/"simplePlotTest"
#    rgl demos        : $ svn cat --revision r8273 seestudy.R | less +/"demos"

# ---------------------------------
#  user modifiable parameters
# ---------------------------------

# logfile

logfile <- "trial-008.log"                   # default '--file' argument

# dimension selection -- the three vectors to plot in required (x,y,z) ordering

vecs <- c("fin", "nox", "luc")
vecs <- c("ghg", "dep", "fin")
vecs <- c("fin", "ghg", "dep")

default <- list(move = 0.1)

# default units for plotting -- note also option '--jump'

mass <- "kilogram  "                         # for example: Gkg (not recommended)
mass <- "tonne"                              # for example: Mt
mass <- "gram"                               # for example: Tg (retains "kg" if all values are zero)

dollars <- "giga"                            # for example: $G
dollars <- "billion"                         # for example: $B

# reporting tweaks

options(scipen = 0)                          # fixed/scientific penalty (default 0)

# =================================
#  part one : command-line
# =================================

# just enough code to process the command-line and no more

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
#  function : displayUsage
# ---------------------------------
#  description  : support for option '--help'
#  role         : basic call
#  status       : complete
# ---------------------------------

displayUsage <- function(script,             # script name
                         exit.status)        # required exit status
{
  # spare: j q y

  message()
  message("           usage: ", script, "  [opts]         run script")
  message("                  ", script, "  --clean    -c  prompted removal of exported graphics and/or data files")
  message("                  ", script, "  --info     -i  display information on Pareto frontiers and exit")
  message("                  ", script, "  --version  -V  display R-related version information and exit")
  message("                  ", script, "  --help     -h  display this message and exit")
  message(" general options:  --debug               -d  increase reporting and remove some checks (not normally used)")
  message("                   --file <slog>         -f  read data from study results file 'slog' (else use '", logfile, "')")
  message("    plot options:  --axes <int>          -a  use alternative axes 'int' in { 0 1 2 3 } (see notes)")
  message("                   --baseline [leta]     -b  set baseline case scenario for comparison (defaults to \"+\")")
  message("                   --early               -e  process data but do not plot")
  message("                   --exclude [letas]     -x  exclude 'letas' scenarios, where 'letas' might be \"abc\" (tolerant)")
  message("                   --hack                -k  use hacked data for a more expressive plot (modifies the data frame)")
  message("                   --jump                -j  toggle default mass unit '", mass, "'")
  message("                   --move <factor>       -m  adjust Pareto frontier by 'factor' (default ", default$move, ") for aesthetic reasons")
  message("                   --nolabels            -n  omit all plot labels (useful with --png for subsequent reworking)")
  message("                   --origin              -o  add origin (0,0,0) marker")
  message("                   --quip                -q  add transposed origin marker (used to confirm Pareto frontier)")
  message("                   --spin                -s  spin the plot")
  message("                   --unpareto            -u  omit the Pareto frontier")
  message("                   --wire                -w  add a wire-frame (aka bounding box) to plot")
  message("                   --zero                -z  draw Pareto frontier relative to origin (overrides --move)")
  message("  export options:  --gif                 -g  spin, film, and capture as animated GIF (can view in web-browser)")
  message("                   --png                 -p  capture as PNG raster graphic")
  message("                   --record              -r  write data frame to CSV file (useful adjunct to graphics files)")
  message("                   --tag <tag>           -t  append 'tag' to the export file names (with spaces changed to hyphens)")
  message("                   --vex                 -v  export as PDF and SVG vector graphics (very slow and produces unwieldy files)")
  message("         purpose: process and plot 'xeona' study results, add Pareto frontier to identify dominating points")
  message("       hardcodes: default study log file = ", logfile)
  message("                  x,y,z dimension names = ", paste(vecs, collapse = " "))
  message("           notes: often best to fully group short options (to a avoid a line one warning from package 'getopt')")
  message("                  axes: 0 for default, 1 for origin in front, 2 for origin in right distance, 3 is omit")
  message("                  exclude: argument is optional, 'leta' need not exist, \"+\" is normally best avoided")
  message("                  the plot window size make little difference when using '--vex'")
  message("                  the resolution is limited under '--png', in any case, maximize the plot window and further magnify")
  message("        examples: plot using defaults                           $ ", script)
  message("                  plot using data from 'study.log'              $ ", script, " -f study.log")
  message("                  process all rows except 'b' but do not plot   $ ", script, " -ex b")
  message("                  plot without labels and with special axes 2   $ ", script, " -na 2")
  message("                  complicated example                           $ ", script, " -dknosugprv -a 2 -x ab -t tag -f study.log")
  message("        requires: R packages: getopt (command processing), rgl (graphics), tcltk (hold open dialogs)")
  message()
  quit(save    = "no",
       status  = exit.status,
       runLast = F)
}

# ---------------------------------
#  function : displayInfo
# ---------------------------------
#  description  : support for option '--info'
#  role         : basic call
#  status       : complete
# ---------------------------------

displayInfo <- function(exit.status)         # required exit status
{
  msg <- "

MULTI-DIMENSIONAL TRADE-OFFS

The 'xeona' energy system program calculates the
high-level multi-dimensional performance for any given
scenario.  This performance is essentially
three-dimensional: financial cost in $, greenhouse cost
in kg CO2e, and depletion cost in J HHV.  NOx in kg and
land-use change in m2 are also supported but are not
typically invoked.  A \"study\" is a set of scenarios,
with one scenario, normally (but not necessarily) the
status quo, selected as the \"baseline case\" for the
purposes of comparison.

The Pareto frontier captures the subset of scenarios,
dependent on their high-level performance, that are
said to \"dominate\" and which, in the absence of other
criteria, should alone be examined and traded-off.  The
remaining scenarios need not then be considered.  This
dominating subset is also known as the \"Pareto set\".
It is possible that one scenario dominates all others
and hence the Pareto set contains only one element.

The problem of identifying the Pareto set is called the
\"maximal or maximum vector problem\" and is well
studied in computer science.

The Pareto set can also be readily visualized in
3-space by constructing the Pareto frontier.  Moreover
it is straightforward to generate this frontier using
the 'rgl' graphics package.

The dominating subset is sometimes also termed the
\"skyline\" and several algorithms for searching
large-scale databases have been reported recently.
These methods are of no direct relevance here.

abbreviations: CO2e = carbon dioxide equivalent, HHV =
higher heating value, NOx = mono-nitrogen oxides.

"

  msg <- sub("^\n", "", msg)
  msg <- sub("\n$", "", msg)

  message(msg)
  quit(save    = "no",
       status  = exit.status,
       runLast = F)
}

# ---------------------------------
#  function : displayVersion
# ---------------------------------
#  description  : support for option '--version'
#  role         : basic call
#  status       : complete
# ---------------------------------

displayVersion <- function(exit.status)         # required exit status
{
  message()

  require(rgl)
  require(getopt)
  require(tcltk)
  require(tools)

  pkgs <- sessionInfo()

  message()
  message("R version       (developed with 2.10.1)    : ", getRversion())
  message("rgl version     (developed with 0.92.798)  : ", pkgs$otherPkgs$rgl$Version)
  message("getopt version  (developed with 1.17)      : ", pkgs$otherPkgs$getopt$Version)
  message()

  if ( opt$debug )
    {
      cat("debug information follows ('sessionInfo' 'installed.packages'):\n")
      cat("\n")
      print(pkgs)
      cat("\n")
      print(installed.packages(priority = "high"))
      cat("\n")
      print(installed.packages(priority = "NA"))
      cat("\n")
    }

  quit(save    = "no",
       status  = exit.status,
       runLast = F)
}

# ---------------------------------
#  function : cleanGraphics
# ---------------------------------
#  description  : support for option '--clean'
#  role         : basic call
#  status       : complete
#  notes        : tried 'readline' but would not stop, hence 'readLines'
# ---------------------------------

cleanGraphics <- function(stub,              # pattern
                          exit.status)       # required exit status
{
  if ( opt$debug )
    {
      message("stub                : ", stub)
    }

  killist <- c()
  exts    <- c("gif", "png", "pdf", "svg", "csv")
  path    <- dirname(stub)
  core    <- basename(stub)
  if ( opt$debug )
    {
      message("path                : ", path)
      message("core                : ", core)
    }
  for ( ext in exts )
    {
      regex <- glob2rx(paste(core, "*", ".", ext, sep = ""))
      if ( opt$debug ) message("regex (not glob)    : ", regex)
      files <- list.files(path    = path,
                          pattern = regex)
      killist <- c(killist, files)
    }
  if ( opt$debug ) message()

  if ( length(killist) > 0 )
    {
      message()
      cat(killist, sep = "\n")

      if ( opt$debug ) message()
      if ( opt$debug ) print(killist)

      message()
      cat("  enter 'y' to delete: ")
      f <- file("stdin")
      y <- readLines(f, n = 1, warn = T)
      message()
      if ( y == "y" )
        {
          status <- file.remove(killist)     # was 'exit.status' but always returned 1
          message("files deleted")
        } else {
          message("no files deleted")
        }
      message()
    }

  quit(save    = "no",
       status  = exit.status,
       runLast = F)
}

# ---------------------------------
#  function : reprocessFilename
# ---------------------------------
#  description  : reprocess 'filename' and add 'tag' if given
#  role         : basic call
#  note         : filename extension must be present and can only comprise [:alnum:] and '~'
#  caution      : code contains linux-only file slashes
#  status       : complete
# ---------------------------------

reprocessFilename <- function(filename,      # filename, need not exist
                              tag)           # extra tag, can be empty string
{
  if ( opt$debug )
    {
      message("filename            : ", filename)
      message("tag                 : ", tag)
    }

  file <- filename                           # keep 'filename' intact
  adir <- normalizePath(filename)            # CAUTION: probably unsafe on Windows, see: help(normalizePath)
  rdir <- dirname(filename)                  # CAUTION: behaves differently from shell and C namesakes
  rdir <- path.expand(rdir)                  # expand "~" if present
  leaf <- basename(file)                     # CAUTION: behaves differently from shell and C namesakes
  base <- sub("\\.[[:alnum:]~]+$", "", leaf) # somewhat restrictive idea of what is an extension
  extn <- substring(leaf, nchar(base) + 2)
  base <- file.path(rdir, base)
  core <- basename(base)
  orig <- file.path(rdir, leaf)

  if ( nchar(tag) > 0 )                      # process the 'tag' argument
    {
      xtra <- tag
      xtra <- sub("[[:space:]]+", "-", xtra) # hyphenize 'tag'
      stub <- paste(base, xtra, sep = ".")   # add leading dot
    } else {
      stub <- base
    }

  stub <- sub("./", "", stub, fixed = T)     # clean up, CAUTION: certainly problematic on Windows

  # load buffer
  buff <- list("adir" = adir,                # absolute directory
               "rdir" = rdir,                # relative directory
               "stub" = stub,                # stub including 'tag'
               "extn" = extn,                # original extension
               "orig" = orig,
               "leaf" = leaf)                # 'core' plus 'extn'

  if ( opt$debug )
    {
      message()
      message("filename processing:")
      message()
      message("  adir          = ", buff$adir)
      message("  rdir          = ", buff$rdir)
      message("  leaf          = ", buff$leaf)
      message("  extn          = ", buff$extn)
      message("  orig (read)   = ", buff$orig)
      message("  stub (write)  = ", buff$stub)
      message()
    }

  # assertion
  if ( leaf !=  paste(core, extn, sep = ".") )
    {
      msg <- "filename extension must be present and can only comprise [:alnum:] and '~'"
      message(script, ": ", msg)
      stop("integrity check failed, confirm extension for: ", filename)
    }

  return (buff)
}

# ---------------------------------
#  command-line processing
# ---------------------------------

library(tools)                               # standard library package
library(getopt)                              # straightforward custom installation

library(robbie)                              # private library

exit <- list(success = 0,                    # exit codes
             failure = 1,
             usage   = 2,
             test    = 255)

spec = c(                                    # 'getopt' specification
  'clean'     , 'c', 0, "logical",
  'info'      , 'i', 0, "logical",
  'help'      , 'h', 0, "logical",
  'legal'     , 'l', 0, "logical",
  'version'   , 'V', 0, "logical",

  'axes'      , 'a', 1, "integer",           # 1 means mandatory argument
  'baseline'  , 'b', 1, "character",
  'debug'     , 'd', 0, "logical",
  'early'     , 'e', 0, "logical",
  'exclude'   , 'x', 2, "character",         # 2 means optional argument
  'file'      , 'f', 1, "character",
  'gif'       , 'g', 0, "logical",
  'hack'      , 'k', 0, "logical",
  'jump'      , 'j', 0, "logical",
  'move'      , 'm', 1, "numeric",
  'nolabels'  , 'n', 0, "logical",
  'origin'    , 'o', 0, "logical",
  'png'       , 'p', 0, "logical",
  'quip'      , 'q', 0, "logical",
  'record'    , 'r', 0, "logical",
  'spin'      , 's', 0, "logical",
  'tag'       , 't', 1, "character",
  'unpareto'  , 'u', 0, "character",
  'vex'       , 'v', 0, "logical",
  'wire'      , 'w', 0, "logical",
  'yeek  '    , 'y', 1, "integer",           # CAUTION: undocumented, run nominated test code
  'zero'      , 'z', 0, "logical")

opt    <- getopt(spec = matrix(ncol = 4, data = spec, byrow = T))     # principal call
script <- robbie.getScriptName()

# set defaults for options that were not specified
if ( is.null(opt$axes)     ) opt$axes     <- 0
if ( is.null(opt$baseline) ) opt$baseline <- "+"
if ( is.null(opt$debug)    ) opt$debug    <- F
if ( is.null(opt$early)    ) opt$early    <- F
if ( is.null(opt$exclude)  ) opt$exclude  <- ""
if ( is.null(opt$file)     ) opt$file     <- ""
if ( is.null(opt$gif)      ) opt$gif      <- F
if ( is.null(opt$hack)     ) opt$hack     <- F
if ( is.null(opt$help)     ) opt$help     <- F
if ( is.null(opt$info)     ) opt$info     <- F
if ( is.null(opt$jump)     ) opt$jump     <- F
if ( is.null(opt$move)     ) opt$move     <- default$move
if ( is.null(opt$nolabels) ) opt$nolabels <- F
if ( is.null(opt$origin)   ) opt$origin   <- F
if ( is.null(opt$png)      ) opt$png      <- F
if ( is.null(opt$quip)     ) opt$quip     <- F
if ( is.null(opt$record)   ) opt$record   <- F
if ( is.null(opt$spin)     ) opt$spin     <- F
if ( is.null(opt$tag)      ) opt$tag      <- ""
if ( is.null(opt$unpareto) ) opt$unpareto <- F
if ( is.null(opt$version)  ) opt$version  <- F
if ( is.null(opt$vex)      ) opt$vex      <- F
if ( is.null(opt$wire)     ) opt$wire     <- F
if ( is.null(opt$yeek)     ) opt$yeek     <- 0
if ( is.null(opt$zero)     ) opt$zero     <- F

# process get-and-exit options in desired order
if ( opt$help    ) displayUsage(script, exit$success)
if ( opt$info    ) displayInfo(exit$success)
if ( opt$version ) displayVersion(exit$success)

# process the log file filename
if ( opt$debug ) message()
if ( nchar(opt$file) == 0 )
  {
    log <- reprocessFilename(logfile, opt$tag)
  } else {
    log <- reprocessFilename(opt$file, opt$tag)
  }

# process get-and-exit options in desired order
if ( ! is.null(opt$clean) ) cleanGraphics(log$stub, exit$success)

# option usage integrity checks
if ( ! opt$debug )
  {
    if ( opt$axes            >= 4 )    stop("'--axes option '", opt$axes, "' not supported")
    if ( nchar(opt$baseline) >  1 )    stop("'--baseline option '", opt$baseline, "' not supported")
    if ( opt$exclude == opt$baseline ) stop("'--exclude option '", opt$exclude, "' conflicts with --baseline option '", opt$baseline, "'")
  }

# report
if ( opt$debug )
  {
    message("command-line options:")
    message()
    message("  axes          = ", opt$axes)
    message("  baseline      = ", opt$baseline)
    message("  debug         = ", opt$debug)
    message("  early         = ", opt$early)
    message("  exclude       = ", opt$exclude)
    message("  file          = ", opt$file)
    message("  gif           = ", opt$gif)
    message("  hack          = ", opt$hack)
    message("  jump          = ", opt$jump)
    message("  nolabels      = ", opt$nolabels)
    message("  origin        = ", opt$origin)
    message("  pdf           = ", opt$pdf)
    message("  spin          = ", opt$spin)
    message("  tag           = ", opt$tag)
    message("  vex           = ", opt$vex)
    message("  wire          = ", opt$wire)
    message("  record        = ", opt$record)
    message("  zero          = ", opt$zero)
  }

# toggle (jump) the unit of mass
if ( opt$jump ) mass <- if (mass == "gram") "tonne" else if (mass == "tonne") "gram" else mass

# =================================
#  part two : principal code
# =================================

message()

# ---------------------------------
#  dependencies
# ---------------------------------

suppressMessages(library(tcltk))             # for message box and thus hold-open functionality
library(rgl)                                 # custom installation required plus perhaps some OpenGL build-time dependencies

lib.paths    <- .libPaths()
r3d.defaults <- r3dDefaults                  # not currently used in this code

if ( opt$debug )
  {
    message("---")
    message()
    cat("library paths (.libPaths)  : ")
    cat(lib.paths)
    cat("\n")
    message("rgl defaults (r3dDefaults) :")
    message()
    print(r3d.defaults)                      # adds extra newline
    message("---")
    message()
  }

# ---------------------------------
#  function : confirmFile
# ---------------------------------
#  description  : confirm file or choke
#  role         : utility call
#  status       : complete
# ---------------------------------

confirmFile <- function(filename)
{
  if ( file.exists(filename) )
    {
      message("file found          : ", filename)
      message()
    } else {
      cat("file not found      :", filename, "\n")
      files <- list.files(pattern     = filename,
                          full.names  = F,
                          recursive   = T,
                          ignore.case = F)
      if ( length(files) > 0 ) cat("possible options    :", files, "\n")
      cat("\n")
      stop("cannot proceed without data")
    }
}

# ---------------------------------
#  function : slurpData
# ---------------------------------
#  description  : slurp logfile into data frame, then sort and return
#  role         : basic call
#  status       : complete
# ---------------------------------

slurpData <- function(logfile)
{
  colnames <- c("xeona", "time", "leta", "scenario name", "steps", "fin", "ghg", "nox", "dep", "luc", "xem file")
  if ( ! file.exists(logfile) ) stop(paste("requested logfile not found :", logfile))
  data <- read.table(file = logfile, header = F, sep = "", col.names = colnames)
  data <- data[with(data, order(leta)), ]                   # sort on 'leta' col, noting "+" sorts first
}

# ---------------------------------
#  function : unFactor
# ---------------------------------
#  description  : return underlying data from factor 'factor'
#  role         : basic call
#  status       : complete
#
#  Rationale
#
#    A factor is a mapping between an integer index and
#    a categorical variable (like kind or status).  The
#    notion of an ordering can apply (status) but is
#    not required (kind).
#
#    This function recovers the underlying data as a
#    vector.  It is for provided for convenience (but
#    should be part of R).
#
# ---------------------------------

unFactor <- function(factor)
{
  levels(factor)[as.integer(factor)]
}

# ---------------------------------
#  step 01 : preliminaries
# ---------------------------------

message("script              : ", script)

clargs <- commandArgs(trailingOnly = T)
if ( length(clargs) == 0 )
  {
    clargs <- "(none)"
  } else {
    clargs <- paste(clargs, sep = "", collapse = " ")
  }
message("command-line args   : ", clargs)
message()

width <- robbie.updateTerminalWidth()
message("terminal width      : ", width)

# ---------------------------------
#  step 02 : obtain the data
# ---------------------------------

# obtain the logfile name
if ( nchar(opt$file) > 0 ) logfile <- opt$file
confirmFile(logfile)

# create and display data frame
study <- slurpData(logfile)
scipen <- options(scipen = -9)               # fixed/scientific penalty (default 0)
study
scipen <- options(scipen = scipen)           # fixed/scientific penalty (default 0)
message()

# some reporting to be sure
scens  <- nrow(study)
times  <- study[["time"]]
aggsec <- sum(times)
message("scenarios           : ", scens)
message("aggregate time [s]  : ", aggsec)

# use 'hms' is present
hms <- "hms"                                 # utility of mine for converting 's' to 'hh:mm:ss'
if ( nchar(Sys.which(hms)) > 0 )
  {
    cat("aggregate time      : ")
    system(paste(hms, aggsec))
  }

message()

# ---------------------------------
#  step 03 : rework the data
# ---------------------------------

data <- study

# filter rows
frows <- c("i")                              # 'i' is an experimental scenario
frows <- c()                                 # no default row filtering

drows <- c()                                 # '--exclude' exclusions
if ( nchar(opt$exclude) > 0 ) drows <- unlist(strsplit(opt$exclude, split = ""))

xrows <- c(frows, drows)                     # combine
data  <- subset(data, ! (match(leta, xrows, nomatch = 0)))

# filter cols
data <- subset(data, select = -c(xeona))     # CAUTION: note leading dash
data <- subset(data, select = -c(time))      # CAUTION: note leading dash
data <- subset(data, select = -c(steps))     # CAUTION: note leading dash
data <- subset(data, select = -c(xem.file))  # CAUTION: note leading dash

# identify row 'i' -- and choke if required
s <- rownames(data)[data["leta"] == opt$baseline]  # extracts row number
t <- rownames(data)
u <- match(s, t)
i <- u

if ( opt$debug )
  {
    cat("  s =", s)
    cat("\n")
    cat("  t =", t)
    cat("\n")
    cat("  u =", u)
    cat("\n")
    message()
  }

message("baseline leta       : ", opt$baseline)
message("baseline row        : ", i)
message("baseline deltas     : ", "reductions negative, increases positive")
message()

if ( ! length(i) ) stop("option --baseline leta not supported: ", opt$baseline)

# delta-ize -- relative to row 'i'
data["fin"] <- data$fin - data$fin[i]
data["ghg"] <- data$ghg - data$ghg[i]
data["nox"] <- data$nox - data$nox[i]
data["dep"] <- data$dep - data$dep[i]
data["luc"] <- data$luc - data$luc[i]

# define labels (set signs above to record an "increase" or "decrease")
attr(data, "labels") <- c("", "", "financial decrease", "greenhouse decrease", "NOx decrease", "depletion decrease", "landuse change")
attr(data, "labels") <- c("", "", "financial", "greenhouse", "NOx", "depletion", "landuse")

# hack in a value
if ( opt$hack )
  {
    data[5,4] <- 2.2
    warning("hacked data used", immediate. = T)
    message()
  }

# ---------------------------------
#  function : rescale
# ---------------------------------
#  description  : rescale 'dataset' and modify 'baseunit' -- with no prefix initially assumed
#  role         : utility call
#  status       : complete
#
#  Units
#
#    mass        : can opt for mass in "g" "kg" "t" (tonnes)
#    currency    : can opt for 1e9 dollars as "$G" "$B" (short-scale billion, the UK changed in 1974)
#    limitations : just positive exponents currently
#
#  SI prefixes
#
#    Y yotta (24) Z zetta (21) E exa (18) P peta (15) T tera (12) G giga (9) M mega (6) k kilo (3)
#
# ---------------------------------

rescale <- function(dataset,                 # scalar or timeseries -- here one of the assessment dimensions
                    baseunit)                # base SI unit, with mass in kilograms
{
  # parameters
  SI   <- c( "", "k", "M", "G", "T", "P", "E", "Z", "Y") # SI prefixes
  exp  <- 8:0                                            # matching 1000s sequence: 8 7 6 5 4 3 2 1 0
  trip <- 1.0                                            # can be adjusted up from unity to give fatter numbers

  if ( opt$debug ) message("baseunit            : ", baseunit)
  if ( opt$debug ) message("dataset             :", format(dataset, width = 11, scientific = T))

  # active code
  max <- max(abs(dataset))                   # scalar value
  for (n in exp)                             # count down
    {
      fac <- 10 ** (3 * n)                   # 'fac' is returned
      if ( (max / fac) > trip ) break
    }

  # reprocess variously for "g" "kg" "t" (tonne)
  if ( mass == "gram"     ) if ( baseunit == "kg" ) { baseunit <- "g"; n <- n + 1 }   # grams
  if ( mass == "tonne"    ) if ( baseunit == "kg" ) { baseunit <- "t"; n <- n - 1 }   # tonnes
  if ( mass == "kilogram" ) invisible()

  pre <- SI[n + 1]                           # recover prefix

  # reprocess variously for "$B" (billion dollars) "$G" (giga dollars)
  if ( dollars == "billion" ) pre <- if ( baseunit == "$" && pre == "G" ) "B" else pre  # billion dollars
  if ( dollars == "giga"    ) invisible()

  # assemble and create named list
  new <- ifelse(baseunit == "$",
                paste(baseunit, pre, sep = ""),   # postfix for "$" convention
                paste(pre, baseunit, sep = ""))   # prefix for other measures
  rescale <- list(scale = fac, unit = new)
  if ( opt$debug ) message("maximum             : ", sprintf("%+.2e", max))
  if ( opt$debug ) message("rescale factor, SI  : ", rescale$scale, " ", rescale$unit )
  if ( opt$debug ) message()
  rescale
}

#---------------------------------
#  function : reunitMass
# ---------------------------------
#  description  : indicate prefixed grams in tonnes
#  role         : reporting purposes
#  note         : - k M G T P E Z Y
#  status       : complete
# ---------------------------------

reunitMass <- function(in.unit)
{
  out.unit <- switch(in.unit,
                     # grams to tonnes
                     Yg = "Pt (zettatonne or billion gigatonne)",
                     Zg = "Et (exatonne or million gigatonne)",
                     Eg = "Tt (teratonne or thousand gigatonne)",
                     Pg = "Gt (gigatonne)",
                     Tg = "Mt (megatonne)",
                     Gg = "kt (kilotonne)",
                     Mg = "t (tonne)",
                     kg = "0.001 t (not normally used)",
                     g  = "1e-6 t (not normally used)",
                     # tonnes to grams
                     Pt = "Yg (yottagram)",
                     Et = "Zg (zettagram)",
                     Tt = "Eg (exagram)",
                     Gt = "Pg (petagram)",
                     Mt = "Tg (teragram)",
                     kt = "Gg (gigagram)",
                     t  = "Mg (megagram)",
                     # not available
                     NA)
  conversion <- paste(in.unit, out.unit, sep = " = ")
  note       <- paste("where", conversion, sep = " ")
  return(note)                               # with annotation
  return(conversion)                         # without annotation
}

# process
fin <- rescale(data$fin, "$" )
ghg <- rescale(data$ghg, "kg")
nox <- rescale(data$nox, "kg")
dep <- rescale(data$dep, "J" )
luc <- rescale(data$luc, "m2")

# selectively rescale and sign-change
data["fin"] <- data$fin / fin$scale
data["ghg"] <- data$ghg / ghg$scale
data["nox"] <- data$nox / nox$scale
data["dep"] <- data$dep / dep$scale
data["luc"] <- data$luc / luc$scale

# add measurement units as an attribute
attr(data, "units") <- c("", "", fin$unit, ghg$unit, nox$unit, dep$unit, luc$unit)

# report data
data
message()

# report units
a <- attr(data, "units")
cat("units = ")
cat(a)
cat("\n")
message(reunitMass(a[4]))               # where Eg = Gt (gigatonne)
message()

# ---------------------------------
#  function : getAttrib
# ---------------------------------
#  description  : get associated attribute
#  role         : direct use or called by function 'listAttribs'
#  status       : complete
# ---------------------------------

getAttrib <- function(frame, attrib, colname)
{
  index <- match(colname, colnames(frame))
  unit  <- attr(frame, attrib)[index]
}

# ---------------------------------
#  function : listAttribs
# ---------------------------------
#  description  : summarize all measurement units
#  role         : direct use
#  status       : complete
# ---------------------------------

listAttribs <- function(frame)
{
  cat("  vec  unit    label\n")
  for ( colname in colnames(frame) )
  {
    unit  <- getAttrib(data, "units",  colname)
    label <- getAttrib(data, "labels", colname)
    if ( nchar(unit) == 0 ) next             # skip empty units
    cat(sprintf("  %-6s %-5s %s\n", colname, unit, label))
  }
}

# make call
listAttribs(data)
message()

# report attributes
if ( opt$debug )
  {
    message("---")
    message()
    cat("data names = ")
    cat(names(data))
    cat("\n")
    message()
    robbie.examine(data)
    message()
    message("---")
    message()
  }

# save data
if ( opt$record )
  {
    output <- paste(log$stub, "csv", sep = ".")
    message("filename            : ", output)
    message("format              : ", "CSV")
    message("precision loss      : ", "at around 15 digits")
    write.csv(data, output, append = F)      # CAUTION: 'csv2' if for European formatting
    message()
  }

# exit early as required
if ( opt$early )
  {
    message("omit plots          : ", opt$early)
    message("command-line        : ", script, " ", clargs)
    message("script              : complete")
    message()
    quit(save    = "no",
         status  = exit$success,
         runLast = F)
  }

# ---------------------------------
#  step 04 : appearance
# ---------------------------------

# plot labels
main <- "proof-of-concept model"
main <- ""

if ( opt$nolabels )
  {
    xlab <- ""
    ylab <- ""
    zlab <- ""
  } else {
    xlab <- paste(getAttrib(data, "labels", vecs[1]), " [" , getAttrib(data, "units", vecs[1]), "]", sep = "")
    ylab <- paste(getAttrib(data, "labels", vecs[2]), " [" , getAttrib(data, "units", vecs[2]), "]", sep = "")
    zlab <- paste(getAttrib(data, "labels", vecs[3]), " [" , getAttrib(data, "units", vecs[3]), "]", sep = "")
  }

# color parameters
# quad = Pareto surfaces, point = data points, line = origin marker, label = all labels
background.color <- "white"
point.color      <- "orange"
quad.color       <- "gray"                   # shroud color
quad.alpha       <-  0.95                    # shroud transparency
line.color       <- "gray20"
line.weight      <- 2.0
label.color      <- "black"
label.adjust     <- c(-2, -1)                # +down, +left

# color futzing (these are nice)
quad.color       <- "khaki"                  # set later (set using 'rainbow')

# ---------------------------------
#  step 05 : plot preparation
# ---------------------------------

# at the outset define some functions: three for
# displaying shrouds and two for plotting axes

# ---------------------------------
#  function : quad
# ---------------------------------
#  description  : generate quadrilateral from four "flat" vertices
#  role         : called by function 'rect'
#  caution      : the 'OpenGL' graphics library uses homogeneous, not Euclidean, coordinates
#  status       : complete
#
#  Typical input uses Euclidean coordinates
#
#    vertices (3 x 4) : 21.4 0.77 15.3 21.4 0 15.3 21.4 0.77 0 21.4 0 0
#    indices  (4)     : 1 2 4 3
#
#  Coordinate systems
#
#    This material is not as relevant after adopting "homogeneous = F"
#
#    http://www.mail-archive.com/r-help@r-project.org/msg58482.html
#
#      One would think that each vertex would have 3
#      coordinates (x,y,z) what does the fourth one in
#      the definition of the variable vertices stand
#      for.
#
#    By default qmesh3d uses 4-coordinate homogeneous
#    coordinates, because that's the coordinate system
#    used by OpenGL. See the ?rgl::matrices help topic
#    for a description of how they work.
#
#  Report formatting
#
#     Note that -1.0 * 0.0 gives -0.0 which 'formatC'
#     then outputs as "-0".  This is quite normal under
#     floating point arithmetic.
#
# ---------------------------------

quad <- function(vertices,                   # four 3-space vertices
                 indices)                    # stated ordering for above
{
  # report
  if ( opt$debug )
    {
      cat("    vertices", formatC(unlist(vertices), width = 9))
      cat("\n")
    }

  # active calls
  object <- qmesh3d(unlist(vertices),
                    indices,
                    homogeneous = F)     # 'rgl' quad-mesh function
  shade3d(object,
          col   = quad.color,
          alpha = quad.alpha)
}

# ---------------------------------
#  function : rect
# ---------------------------------
#  description  : generate four 3-space coordinates to pass to function 'quad'
#  role         : utility call
#  comment      : hardcoded (not the most elegant method)
#  status       : complete
# ---------------------------------

rect <- function(point,                      # data point
                 ofset,                      # transposed origin
                 focus)                      # "x" "y" "z"
{
  switch(focus,
         x = {
           p1 <- c(point[1], point[2], point[3])
           p2 <- c(point[1], ofset[2], point[3])
           p3 <- c(point[1], ofset[2], ofset[3])
           p4 <- c(point[1], point[2], ofset[3])
         },
         y = {
           p1 <- c(point[1], point[2], point[3])
           p2 <- c(ofset[1], point[2], point[3])
           p3 <- c(ofset[1], point[2], ofset[3])
           p4 <- c(point[1], point[2], ofset[3])
         },
         z = {
           p1 <- c(point[1], point[2], point[3])
           p2 <- c(ofset[1], point[2], point[3])
           p3 <- c(ofset[1], ofset[2], point[3])
           p4 <- c(point[1], ofset[2], point[3])
         },
         stop("coding error: focus not supported", focus))
  list(p1, p2, p3, p4)                       # positive spin (right-hand rule) relative to 'focus' axis
}

# ---------------------------------
#  function : shroud
# ---------------------------------
#  description  : create a "shroud" over 'point' relative to 'ofset'
#  role         : utility call
#  status       : complete
# ---------------------------------

shroud <- function(point,                    # 3-space point of interest
                   ofset)                    # 3-space transposed origin
{
  if ( opt$debug ) message("    point :", format(point, digits = 2, width = 6))
  if ( opt$debug ) message("    ofset :", format(ofset, digits = 2, width = 6))

  indices <- c(1, 2, 3, 4)                   # CAUTION: ordering is significant (else get "twisted bow-tie" shapes)
  quad(rect(point, ofset, "x"), indices)
  quad(rect(point, ofset, "y"), indices)
  quad(rect(point, ofset, "z"), indices)
}

# ---------------------------------
#  function : line
# ---------------------------------
#  description  : generate line from two vertices
#  role         : called by function 'rule'
#  status       : complete
# ---------------------------------

line <- function(vertices)                   # two 3-space vertices
{
  # report
  if ( opt$debug )
    {
      cat("    vertices", formatC(unlist(vertices), width = 9))
      cat("\n")
    }

  # active calls
  lines3d(matrix(unlist(vertices), ncol = 3, byrow = T),
          col         = line.color,
          lwd         = line.weight,
          homogeneous = F)                   # 'rgl' line function
}

# ---------------------------------
#  function : rule
# ---------------------------------
#  description  : generate two 3-space coordinates to pass to function 'line'
#  role         : utility call
#  status       : complete
# ---------------------------------

rule <- function(range,                      # data range
                 const,                      # constant dimensions
                 focus,                      # "x" "y" "z"
                 add = 0.0)
{
  if ( add != 0.0 )
    {
      len      <- abs(range[1] - range[2])
      adj      <- add * len
      range[1] <- range[1] - adj
      range[2] <- range[2] + adj
    }

  switch(focus,
         x = {
           p1 <- c(range[1], const[1], const[2])
           p2 <- c(range[2], const[1], const[2])
         },
         y = {
           p1 <- c(const[2], range[1], const[1])
           p2 <- c(const[2], range[2], const[1])
         },
         z = {
           p1 <- c(const[1], const[2], range[1])
           p2 <- c(const[1], const[2], range[2])
         },
         stop("coding error: focus not supported", focus))

  list(p1, p2)
}

# ---------------------------------
#  step 06 : now plot the data
# ---------------------------------

device.no <- open3d()

bg3d(background.color)

# regarding points: use "size = 10" if 'type' not set
plot3d(data[vecs],
       main = main,
       xlab = xlab,
       ylab = ylab,
       zlab = zlab,
       col  = point.color,
       type = "s",                           # CAUTION: 'type' not 'pch', "s" for spheres
       size = 0.7,
       box  = F,                             # added in below if required
       axes = F)                             # added in below if required

if ( opt$axes == 0 ) axes3d()                          # add back standard axes
if ( opt$axes == 1 ) axes3d(c('x--', 'y--', 'z-+'))    # used fixed specified axes
if ( opt$axes == 2 ) axes3d(c('x+-', 'y--', 'z++'))    # used fixed specified axes
if ( opt$axes == 3 ) invisible()                       # do nothing

# extract the offset (looping unavoidable)
offset <- c()
if ( opt$zero )
  {
    offset <- rep(0.0, 3)                    # the origin
  } else {
    for ( vec in vecs )                      # usually 'fin' 'ghg' 'dep'
      {
        raw    <- data[, vec]
        off    <- max(raw)
        skirt  <- opt$move * (max(raw) - min(raw))
        off    <- off + skirt
        offset <- c(offset, off)             # collect
      }
  }
message("offset (transposed origin) =", format(offset, digits = 2, width = 8))
message()

# add the pareto frontier
if ( ! opt$unpareto )
  {
    for ( i in 1:nrow(data) )
      {
        sen <- unFactor(data$leta)[i]        # 'sen' is a string, 'unFactor' is a local unfactor function
        if ( opt$debug ) message(sprintf("  i = %2d   scenario = %s", i, sen))
        point <- unlist(data[i, vecs])       # each row with selected cols
        # active calls
        quad.color <- rainbow(20)[i]
        shroud(point, offset)                # shroud creation call
        if ( ! opt$nolabels )
          {
            text3d(point,                    # point labeling call
                   text = sen,
                   adj  = label.adjust,
                   col  = label.color)
          }
      }
    if ( opt$debug ) message()
}

# add an origin marker
if ( opt$origin )
  {
    message("adding origin marker")
    if ( opt$debug ) message()

    axes <- c("x", "y", "z")
    for ( i in 1:3 )
      {
        raw   <- data[, vecs[i]]
        ran   <- range(raw)
        const <- c(0.0, 0.0)
        line(rule(ran, const, axes[i], opt$move))
      }
  }

# add a transposed origin marker
if ( opt$quip )
  {
    message("adding transposed origin marker")
    if ( opt$debug ) message()

    offsets <- c(offset, offset)             # useful to be able to roll around
    axes    <- c("x", "y", "z")
    for ( i in 1:3 )
      {
        raw   <- data[, vecs[i]]
        ran   <- range(raw)
        const <- c(offsets[i + 1], offsets[i + 2])
        line(rule(ran, const, axes[i], opt$move))
      }
  }

if ( opt$origin || opt$quip ) message()

# finally add the bounding box
if ( opt$wire )
  {
    box3d()
  }

# ---------------------------------
#  step 07 : capture the plot
# ---------------------------------

if ( opt$png )
  {
#     viewport <- par3d("viewport")
#     pixels   <- paste(viewport[3], viewport[4], sep="x")
#     stub     <- paste(log$stub, pixels, sep = "_")     # was "." but LaTeX chokes on multiple dots

    stub   <- log$stub

    fmt    <- "png"
    raster <- paste(stub, fmt, sep = ".")
    message("capture             : ", raster)

    prompt  <- "hit spacebar to capture the plot"
    extra   <- paste("filename: ", raster, "\n", "format: PNG", sep = "")
    capture <- tk_messageBox(message = prompt, detail = extra)

    if ( file.exists(raster) ) file.remove(raster)

    rgl.snapshot(filename = raster,
                 fmt      = fmt)

    if ( file.exists(raster) ) Sys.chmod(raster, mode = "0440")            # "-r--r-----"

    # ImageMagick processing (seems quite useful)

    im.raster  <- paste(paste(stub, "im", sep = "_"), fmt, sep = ".")      # finishes "_im.png"
    im.util    <- "convert"
    im.args    <- "-resize '200%' -sharpen '0x1'"
    im.call    <- paste(im.util, im.args, raster, im.raster, sep = " ")

    if ( file.exists(im.raster) ) file.remove(im.raster)

    im.capture <- system(im.call, intern = TRUE)

    if ( file.exists(im.raster) ) Sys.chmod(im.raster, mode = "0440")      # "-r--r-----"

  }

# ---------------------------------
#  step 08 : export the plot
# ---------------------------------

if ( opt$vex )
  {
    stub    <- log$stub

    prompt  <- "hit spacebar to export the plot"
    extra   <- paste("filenames: ", stub, ".pdf|svg", "\n", "formats: PDF, SVG", sep = "")
    capture <- tk_messageBox(message = prompt, detail = extra)

    fmts <- c("pdf", "svg")
    for ( fmt in fmts )
      {
        still <- paste(stub, fmt, sep = ".")
        message("export              : ", still)
        if ( file.exists(still) ) file.remove(still)

        rgl.postscript(filename = still,
                       fmt      = fmt,
                       drawText = T)         # although '--nolabels' prevents creation

        if ( file.exists(still) ) Sys.chmod(still, mode = "0440")     # "-r--r-----"
      }
    message()
  }

# ---------------------------------
#  step 09  : spin the plot
# ---------------------------------

axis <- c(1, 1, 0)                           # original axis
axis <- c(0, 1, 1)

if ( opt$spin )
  {
    prompt  <- "hit spacebar to spin the plot"
    extra   <- "no associated capture or export"
    capture <- tk_messageBox(message = prompt, detail = extra)

    sleep <- 1
    Sys.sleep(sleep)

    play3d(spin3d(axis = axis, rpm = 5),
           duration = 15)
  }

# ---------------------------------
#  step 10 : film the plot
# ---------------------------------

if ( opt$gif )                               # film the object
  {
    fps   <- 10                              # default
    fps   <- 20                              # "slower"
    if( opt$debug ) dur <- 2 else dur <- 12
    tmp   <- tempdir()                       # under my control
    stub  <- log$stub
    movi  <- paste(stub, "gif", sep = ".")
    message("film the plot       : yes")
    message("movie fps           : ", fps)
    message("duration [s]        : ", dur)
    message("filename            : ", movi)
    message("temp dir            : ", tmp)
    message()

    prompt  <- "hit spacebar to film the plot"
    extra   <- paste("filename: ", movi, "\n", "format: animated GIF", sep = "")
    capture <- tk_messageBox(message = prompt, detail = extra)

    if ( file.exists(movi) ) file.remove(movi)

    movie3d(spin3d(axis = axis, rpm = 5),
            duration = dur,
            fps      = fps,
            dir      = tmp,
            clean    = T)
    message()

    src <- file.path(tmp, "movie.gif")       # 'movie3d' default
    tar <- file.path(getwd(), movi)
    message("rescue movie file   : ", src, " > ", tar)
    message()
    ret <- file.copy(from      = src,
                     to        = tar,
                     overwrite = T)

    if ( ret ) Sys.chmod(tar, mode = "0440")  # "-r--r-----"

    # 'duration' = 12 seconds and 'rpm' = 5 means one revolution

    # $ watch --interval=0.5 --differences ls -l *.gif
    # $ watch -n 0.5 -d ls -l *.gif

    # Writing movie240.png
    # Will create:  /tmp/RtmpRX89sY/movie.gif
    # Executing:  convert -delay 1x10 movie*.png movie.gif
    # Deleting frames.
  }

# ---------------------------------
#  hold-open functionality
# ---------------------------------

prompt  <- "hit spacebar to close plot"
extra   <- paste("script :", script)
capture <- tk_messageBox(message = prompt, detail = extra)

# ---------------------------------
#  housekeeping
# ---------------------------------

viewport <- paste(par3d("viewport"), collapse = " ")

if ( opt$debug ) message("debug mode          : ", "yes")
message("viewport px         : ", viewport)
message("command-line        : ", script, " ", clargs)
message("script              : complete")
message()

# ---------------------------------
#  junked code
# ---------------------------------

# names(data)[names(data) == "fin"] = "fin*" # rename
# bits <- unlist(strsplit(still, ".", fixed = T))
# fmt  <- tail(bits, n = 1)
# i <- as.integer(rownames(data)[data["leta"] == opt$base])

# ---------------------------------
#  urls
# ---------------------------------

# par3d  : http://hosho.ees.hokudai.ac.jp/~kubo/Rdoc/library/rgl/html/par3d.html
# search : http://www.dangoldstein.com/search_r.html

#  $Id: seestudy.R 9195 2012-03-21 17:24:34Z robbie $
#  end of file

