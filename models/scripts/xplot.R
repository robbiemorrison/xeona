#! /usr/bin/Rscript --vanilla

#  file-purpose     : plot 'xeona' data
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Fri 09-Dec-2011 16:46 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9204 $
#  $Date: 2012-03-30 13:36:35 +0200 (Fri, 30 Mar 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/xplot.R $

#  note 'cairo_pdf'

#  todo:
#
#    - work on the TOFIXs
#    - 'study' and 'meta$study' both float around, not good practice

# ---------------------------------
#  user modifiable
# ---------------------------------

study           <- "trial-008"               # defines the required study, hard-coded because not likely to change
xemext          <- "xem"

yano            <- "quantity"                #  y-axis annotation
yano            <- ""                        #  y-axis annotation, was "quantity"

catalog.default <- "xplot-01"                # catalog file contains (key = value) meta information and (fqf title) plot pairs
catext          <- "cat"                     # catalog file extension

all.scens       <- "+abcdefghij"

plot.alarm      <-  3                        # useful during testing, disable in due course
plot.alarm      <- 50                        # prompted closure, less than 64 - max(scenarios)

clean.exts <- c("gif", "png", "pdf", "svg", "csv")
clean.exts <- c("gif", "png", "svg")

# ==== command processing section ==============================================================

# ---------------------------------
#  general libraries
# ---------------------------------

# functions may also 'require' libraries

ret <- suppressPackageStartupMessages(expr = T)

library(tools)                               # standard library package
library(xem)                                 # 'xeona' XEM files (private library)
library(robbie)                              # utilities (my private library)
library(getopt)                              # straightforward custom installation

# overwrite 'xem.plot.win' 'xem.plot.svg' package defaults here if you wish

# ---------------------------------
#  function : confirmFiles
# ---------------------------------
#  description  : confirm files
#  role         : utility
#  status       : complete
# ---------------------------------

confirmFiles <- function(filenames,
                         fault)
{
  exists <- file.exists(filenames)
  msgs   <- sprintf("%-19s : %s", filenames, ifelse(exists, "found", "NOT FOUND"))
  if ( opt$report || any(exists == FALSE) )
    {
      message()
      cat(msgs, sep = "\n", file = "/dev/stderr")
    }
  if ( any(! exists) )
    {
      message("fatal status        : ", fault)
      message()
      quit(save = "no", status = fault, runLast = F)
    }
  count <- length(exists[exists == TRUE])
}

# ---------------------------------
#  function : checkForCatalog
# ---------------------------------
#  description  : check for alternatives if 'opt$file' not specified
#  role         : utility call
#  status       : complete
# ---------------------------------

checkForCatalog <- function(tests)
{
  if ( any(tests) )               return(TRUE)  # alternatives exist
  if ( is.na(opt$file) == FALSE ) return(TRUE)  # catalog file is specified
  if ( opt$debug )
    {
      message()
      cat("test                : ", test, file = "/dev/stderr")
      cat("\n", file = "/dev/stderr")
    }
  message(nocatmsg)
  quit(save = "no", status = exit$usage, runLast = F)
}

# ---------------------------------
#  function : moleCatalogs
# ---------------------------------
#  description  : mole catalogs
#  role         : used by '--mole'
#  status       : complete
# ---------------------------------

moleCatalogs <- function(script,             # script name (not currently used)
                         path,
                         catext,             # catalog file extension
                         exit)
{
  fmt    <- "  %-30s %-8s %-12s%5s    %-13s %3s    %s\n"
  rule   <- "  ----------------------------------------------------------------------------------------------------------\n"
  header <- sprintf(fmt, "catalog", "fqfs", "letas", "plots", "study", "num", "role")

  line1 <- paste("note that 'plots' includes both timeseries plots and scalar reports", sep = " ")
  line2 <- paste("explore catalogs using:", script, "-er0", sep = " ")
  final <- paste("", line1, line2, "", sep = "\n")
  final <- ifelse(opt$report, final, "")

  if ( ! opt$debug ) opt$report <<- F        # turn off reporting unless in debug mode

  regex  <- glob2rx(paste("*", catext, sep = "."))
  files  <- list.files(path = path, pattern = regex, recursive = T)

  if ( length(files) > 0 )
    {
      message()
      cat(header, file = "/dev/stderr")
      cat(rule,   file = "/dev/stderr")
      for (file in files)
        {
          fullfile <- file.path(path, file)
          # filename pattern processing (with clear limitations) -- source: Gabor Grothendieck on [R-help] mailing list
          nuz   <- as.integer(gsub("\\D", "", file))   # "xplot-09_1.cat" gives 91, can give NA
          num   <- ifelse( is.na(nuz), "-", sprintf("%3d", nuz))
          # meta
          meta  <- slurpMeta(fullfile)
          if ( length(meta) == 0 ) next
          senz  <- length(meta$scens)
          letas <- paste(meta$scens, collapse = "")
          role  <- ifelse(is.na(meta$role), "--", meta$role)     # replace any NA with "--"
          # data
          data <- slurpFqfs(fullfile, fault = 0)  # 'fault' value zero prevents 'slurpFqfs' from quiting on empty data
          rowz <- nrow(data)
          rows <- sprintf("%4d", rowz)
          # consolidation and printing
          plotz <- senz * rowz
          plots <- sprintf("%5d", plotz)
          line  <- sprintf(fmt, file, rows, letas, plotz, meta$study, num, role)
          cat(line)
        }
      cat(final, file = "/dev/stderr")
      message()
    }
  quit(save = "no", status = exit, runLast = F)
}

# ---------------------------------
#  function : printTemplate
# ---------------------------------
#  description  : print sample catalog template to stdout
#  role         : used by '--plate'
#  status       : complete
# ---------------------------------

printTemplate <- function(script,            # script name (not currently used)
                          exit)
{
  template <- "
# role   = TEMPLATE
# study  = trial-008
# scens  = *
# cut    = :
# lines  = F
# tall   = T
# ymax   = 0
# zero   = T

  entity.fully-qualified-field-name                         \"fully-qualified field name\"

##  emacs text editor settings
##  local variables:
##    mode: text
##    make-backup-files: nil
##    truncate-lines: t
##    comment-start: \"#\"
##    tab-stop-list: (02 12 60)
##  end:

"
  cat(template)
  quit(save = "no", status = exit, runLast = F)
}

# ---------------------------------
#  function : mypush
# ---------------------------------

mypush.counter <- 0

mypush <- function(vector,                   # container
                   annot,                    # annotation (as name attribute)
                   optns)                    # test options (as string value)
{
  suppressMessages(require(methods))

  mypush.counter <<- mypush.counter + 1
  annot  <- sprintf("%02d  %s", mypush.counter, annot)
  vector <- substitute(vector)
  str1   <- paste(vector, ' <- c(', vector, ', "', optns, '")', sep = "")
  str2   <- paste('names(', vector, ')[length(', vector,')] <- "', annot, '"', sep = "")
  eval.parent(parse(text = str1), n = 1)
  eval.parent(parse(text = str2), n = 1)
}

# ---------------------------------
#  function : runTestSuite
# ---------------------------------
#  description  : run a test suite
#  role         : used by option '--test'
#  status       : complete
#
#  CAUTION: '--all' seems buggy, use '-a' instead
#  CAUTION: '-g' may result in a R warning, just ignore
#
# ---------------------------------

runTestSuite <- function(script,             # script name
                         fault,              # exit status on fault
                         depth)
{
  if ( depth == 0 ) depth = 999
  tests <- character(0)
  if ( depth >  0 ) mypush(tests, "help message",                              "--help"                        )
  if ( depth >  1 ) mypush(tests, "mole test",                                 "--mole --report"               )
  if ( depth >  1 ) mypush(tests, "template test",                             "--plate"                       )
  if ( depth >  2 ) mypush(tests, "default catalog basic test",                "--cat01 --number 1"            )
  if ( depth >  2 ) mypush(tests, "default catalog stress test",               "--cat01 --cut 200 --grid --lines --report --sleep 2 --zero --word teas")
  if ( depth >  3 ) mypush(tests, "all test with heavy filtering",             "-a --scens ab --keep --lines --report --zero --word carbon-emissions"  )

  rule <- "---------------------------------------------------------"

  message()
  message(script, ": test suite commences")

  tic  <- Sys.time()
  rets <- 0
  msgs <- character(0)
  for (i in 1:length(tests))
    {
      call <- paste(script, tests[i], sep = " ")
      message(rule)
      loop <- sprintf("%02d", i)
      message(script, ": ", loop, " = ", call)
      ret  <- system(call, intern = F)
      rets <- rets + ret
      anot <- names(tests)[i]
      msgs <- append(msgs, sprintf("%5d   %-45s   %s", ret, anot, call))
    }
  toc <- Sys.time() - tic

  # final report
  message(script, ": tests complete, elapsed time (seconds) : ", sprintf("%.0f", toc))
  message()
  message("  ret   no  annotation                                  call")
  message("  ----------------------------------------------------------------")
  message(paste(msgs, collapse = "\n"))
  message("  ----")
  message(sprintf("%5d", rets))
  message()

  exit <- if ( rets == 0 ) exit$success else fault
  quit(save = "no", status = exit, runLast = F)
}

# ---------------------------------
#  function : runCatalogs
# ---------------------------------
#  description  : run all catalogs
#  role         : used by option '--mmmm'
#  status       : complete
# ---------------------------------

runCatalogs <- function(script,              # script name
                        fault)               # exit status on fault
{
  path   <- xem.consts()$catalogs.dir
  regex  <- glob2rx(paste("*", catext, sep = "."))
  files  <- list.files(path = path, pattern = regex, recursive = T)

  message()
  message(script, ": catalog test commences")
  message()

  rets <- 0
  for (file in files)
    {
      call <- paste(script, "--file", file, sep = " ")
      message("---------------------------------------------------------")
      message(call)
      ret  <- system(call, intern = F)
      rets <- rets + ret
    }

  if ( length(files) > 0) message("---")
  if ( length(files) > 0) message()
  message(script, ": aggregate return = ", rets/256)
  message()

  exit <- if ( rets == 0 ) exit$success else fault
  quit(save = "no", status = exit, runLast = F)
}

# ---------------------------------
#  function : displayUsage
# ---------------------------------
#  description  : display usage message and quit
#  role         : used by option '--help'
#  status       : complete
# ---------------------------------

displayUsage <- function(script,             # script name
                         long = TRUE,
                         exit)               # required exit status
{
  message()
  message("               usage: ", script, "   [opts]          run script (must provide a source option)")
  message("                      ", script, "  --mole       -m  summarize available catalog files")
  message("                      ", script, "  --plate      -p  print catalog template to stdout")
  message("                      ", script, "  --help       -h  display this message and exit")
  message("                      ", script, "  --usage      -u  display help message followed by examples and exit")
  message("      source options:  --all                -a  plot all fields and scenarios (best with filter options and reporting)")
  message("                       --cat00              -0  use --file catalog files if specified, else use default")
  message("                       --cat01              -1  shortcut to default catalog file (explore with -1er)")
  message("                       --cat02 ..           -2  shortcut to hardcoded catalog file 02 and so on")
  message("                       --file <catalog>     -f  read catalog information from catalog file 'catalog' (low precedence)")
  message("      filter options:  --number <00>        -n  plot just data frame number '00' (check with --early first)")
  message("                       --omitcosts          -d  omit \"-costs-\" fqfs")
  message("                       --scens <letas>      -s  restrict specified scenarios to 'letas' (may also over-specify, \"'*'\" acceptable)")
  message("                       --word <pattern>     -w  filter fully-qualified field names on 'pattern'")
  message("        plot options:  --cut <00:00>        -c  subset plots to [00,00) zero-based steps (omitted values seen as start and finish)")
  message("                       --grid               -g  add grid (ignore any --gui warning message from Rscript)")
  message("                       --identify           -i  invoke 'identify' to record points")
  message("                       --lines              -l  plot with lines (not points)")
  message("                       --tall               -t  use common ymin:ymax for each fully-qualified field")
  message("                       --ymax               -y  set ymax (only useful with individual plots)")
  message("                       --zero               -z  zero the y-axis if ymin is positive")
  message("    behavior options:  --export             -x  interactive export (silently overwrites existing files)")
  message("                       --eXport             -X  non-interactive export (silently overwrites existing files)")
  message("                       --keep               -k  use Tk dialog to retain plot windows")
  message("                       --report             -r  report useful information")
  message(" development options:  --clean              -C  clean up exported files")
  message("                       --debug              -D  add debug messages")
  message("                       --mmmm               -M  run all catalogs")
  message("                       --sleep <seconds>    -S  sleep for 'seconds' after each plot (some cases only)")
  message("                       --test <int>         -T  run test suite to depth 'int' (zero for all) (other options ignored)")
  message("             purpose: plot or display a subset of data from a 'xeona' study using cataloged information")
  message("         definitions: catalog = a text file containing (key = value) meta information and (fqf title) plot pairs")
  message("                      study = a set of 'xeona' scenarios, fqf = fully-qualified field name, xfqf = extended fully-qualified field name")
  message("               notes: identify : left-mouse to record point, right-mouse to exit plot")
  message("                      all      : plot titles are generated not specified")
  message("            see also: pareto frontier plots   : seestudy.R")
  message("                      specialized development : xanalyze.R")
  if ( long )
    {
      message()
      message("            examples: testing                           $ ", script, " -1     -c 200 -gklr -S 2 -zw teas")
      message("                      default with lines                $ ", script, " -1     -lk")
      message("                      plot 'burials' from 'a b'         $ ", script, " -as ab -klrz -w emissions")
      message("                      show and grep data frame          $ ", script, " -as +  -er 2>/dev/null | grep --color \"emissions\"")
      message("                also: show and grep directly            $ xgrab --names ", paste(study, xemext, sep ="."), "    | grep --color \"emissions\"")
    }
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
             testsFailed        = 3,
             catalogsFailed     = 4,
             missingCatalogFile = 10,
             missingXemFiles    = 11,
             emptyDataFrame     = 12,
             tmpCatalogFault    = 13,
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
  'all'          , 'a', 0, "logical",         # CAUTION: '--all' is buggy, but '-a' is fine
  'clean'        , 'C', 0, "logical",
  'cut'          , 'c', 1, "character",
  'debug'        , 'D', 0, "logical",
  'early'        , 'e', 0, "logical",
  'export'       , 'x', 0, "logical",
  'eXport'       , 'X', 0, "logical",
  'file'         , 'f', 1, "character",
  'grid'         , 'g', 0, "logical",
  'help'         , 'h', 0, "logical",
  'identify'     , 'i', 0, "logical",
  'keep'         , 'k', 0, "logical",
  'lines'        , 'l', 0, "logical",
  'mmmm'         , 'M', 0, "logical",
  'mole'         , 'm', 0, "logical",
  'number'       , 'n', 1, "integer",
  'omitcosts'    , 'o', 0, "logical",
  'plate'        , 'p', 0, "logical",
  'report'       , 'r', 0, "logical",
  'scens'        , 's', 1, "character",
  'sleep'        , 'S', 1, "numeric",
  'tall'         , 't', 0, "logical",
  'test'         , 'T', 1, "integer",
  'usage'        , 'u', 0, "logical",
  'word'         , 'w', 1, "character",
  'ymax'         , 'y', 1, "numeric",
  'zero'         , 'z', 0, "logical",
  # cats
  'cat00'        , '0', 0, "character",
  'cat01'        , '1', 0, "character",
  'cat02'        , '2', 0, "character",
  'cat03'        , '3', 0, "character",
  'cat04'        , '4', 0, "character",
  'cat05'        , '5', 0, "character",
  'cat06'        , '6', 0, "character",
  'cat07'        , '7', 0, "character",
  'cat08'        , '8', 0, "character",
  'cat09'        , '9', 0, "character")

# 'getopt' call
opt <- getopt(spec = matrix(ncol = 4, data = spec, byrow = T))   # principal call

# set 'debug' and 'report' defaults early
if ( is.null(opt$debug)    ) opt$debug    <- F
if ( is.null(opt$report)   ) opt$report   <- F

# 'help' and friends -- order is significant
if ( ! is.null(opt$usage)   ) displayUsage (script, long = T, exit = exit$success)
if ( ! is.null(opt$help)    ) displayUsage (script, long = F, exit = exit$success)
if ( ! is.null(opt$plate)   ) printTemplate(script, exit = exit$success)
if ( ! is.null(opt$clean)   ) xem.cleanExportFiles(stub = study, exts = clean.exts, debug = opt$debug, exit = exit$success)
if ( ! is.null(opt$test)    ) runTestSuite (script, fault = exit$testsFailed, depth = opt$test)
if ( ! is.null(opt$mmmm)    ) runCatalogs  (script, fault = exit$catalogsFailed)

# set some more defaults (CAUTION: NA for those that can be redefined in a catalog file)
if ( is.null(opt$all)       ) opt$all       <- F
if ( is.null(opt$cut)       ) opt$cut       <- NA
if ( is.null(opt$omitcosts) ) opt$omitcosts <- F
if ( is.null(opt$early)     ) opt$early     <- F
if ( is.null(opt$export)    ) opt$export    <- F
if ( is.null(opt$eXport)    ) opt$eXport    <- F
if ( is.null(opt$file)      ) opt$file      <- NA
if ( is.null(opt$grid)      ) opt$grid      <- F
if ( is.null(opt$identify)  ) opt$identify  <- F
if ( is.null(opt$keep)      ) opt$keep      <- F
if ( is.null(opt$lines)     ) opt$lines     <- NA
if ( is.null(opt$mole)      ) opt$mole      <- F
if ( is.null(opt$number)    ) opt$number    <- 0
if ( is.null(opt$scens)     ) opt$scens     <- NA
if ( is.null(opt$sleep)     ) opt$sleep     <- 0
if ( is.null(opt$tall)      ) opt$tall      <- NA
if ( is.null(opt$word)      ) opt$word      <- ""
if ( is.null(opt$ymax)      ) opt$ymax      <- NA
if ( is.null(opt$zero)      ) opt$zero      <- NA
# the cats
if ( is.null(opt$cat00)     ) opt$cat00     <- NA
if ( is.null(opt$cat01)     ) opt$cat01     <- NA
if ( is.null(opt$cat02)     ) opt$cat02     <- NA
if ( is.null(opt$cat03)     ) opt$cat03     <- NA
if ( is.null(opt$cat04)     ) opt$cat04     <- NA
if ( is.null(opt$cat05)     ) opt$cat05     <- NA
if ( is.null(opt$cat06)     ) opt$cat06     <- NA
if ( is.null(opt$cat07)     ) opt$cat07     <- NA
if ( is.null(opt$cat08)     ) opt$cat08     <- NA
if ( is.null(opt$cat09)     ) opt$cat09     <- NA

# process catalog files
defcatalog <- paste(catalog.default, catext, sep = ".")
if ( ! is.na(opt$cat00) && is.na(opt$file) ) opt$file <- defcatalog
if ( ! is.na(opt$cat01) )                    opt$file <- defcatalog
if ( ! is.na(opt$cat02) )                    opt$file <- "xplot-02.cat"
if ( ! is.na(opt$cat03) )                    opt$file <- "xplot-03.cat"
if ( ! is.na(opt$cat04) )                    opt$file <- "xplot-04.cat"
if ( ! is.na(opt$cat05) )                    opt$file <- "xplot-05.cat"
if ( ! is.na(opt$cat06) )                    opt$file <- "xplot-06.cat"
if ( ! is.na(opt$cat07) )                    opt$file <- "xplot-07.cat"
if ( ! is.na(opt$cat08) )                    opt$file <- "xplot-08.cat"
if ( ! is.na(opt$cat09) )                    opt$file <- "xplot-09.cat"

# make the catalog file absolute if not NA and not already absolute
catalogs.dir <- xem.consts()$catalogs.dir
if ( ! is.na(opt$file) && substring(opt$file, 1, 1) != "/" ) opt$file <- file.path(catalogs.dir, opt$file)

# quit on absent catalog file, but only if all the 'tests' prove false
ret <- checkForCatalog(tests = c(opt$mole, opt$all))

# dynamic terminal width for R reporting
width <- robbie.updateTerminalWidth()

# ensure the export trips align
if ( opt$eXport ) opt$export = T

# end of command-line processing reporting
if ( opt$report ) message()
if ( opt$report ) message("catalog file        : ", opt$file)
if ( opt$report ) message("plot alarm          : ", plot.alarm)
if ( opt$report ) message("terminal width      : ", width)

# ==== active section ====================================================================

# ---------------------------------
#  function : tstrunc
# ---------------------------------
#  description  : extract a (null, proper, improper) subset from 'timeseries'
#  role         : utility
#  status       : complete
# ---------------------------------

tstrunc <- function(timeseries,              # R vector, probably but not necessarily 'numeric'
                    rrange)                  # one-based "00:00"
{
  rowz <- eval(parse(text = rrange))         # parse and evaluate the string in the form "01:01"
  timeseries[rowz]                           # truncate, see ? ':' and ? seq
}

# ---------------------------------
#  function : processHorizon
# ---------------------------------
#  description  : obtain horizon data, including the 'cut', one-based reprocessed for R
#  role         : utility
#  status       : complete
#
#  Range safe
#
#    This function will not extend the 'timeseries'.
#    Nor should it fail on length zero timeseries.
#
#  Typical 'strsplit' results
#
#    ""       character(0)
#    ":"      [1] ""
#    ":90"    [1] ""   "90"
#    "10:"    [1] "10"
#    "10:90"  [1] "10" "90"
#
# ---------------------------------

processHorizon <- function(cut,              # in ":" ":00" "00:" "00:00"
                           letas)
{
  # leta
  leta <- letas[1]                           # take the first leta

  # obtain information
  xfqf1    <- paste(study, leta, "entity.time-horizon.steps",    sep = ".")
  xfqf2    <- paste(study, leta, "entity.time-horizon.interval", sep = ".")
  xfqf3    <- paste(study, leta, "program.last-run.used-svn",    sep = ".")
  steps    <- xem.grab(xfqf = xfqf1, want = "data", report = opt$debug)
  interval <- xem.grab(xfqf = xfqf2, want = "data", report = opt$debug)
  svn      <- xem.grab(xfqf = xfqf3, want = "data", report = opt$debug)
  usedsvn  <- as.integer(svn)

  # reporting
  if ( opt$report ) message()
  if ( opt$report ) message("horizon steps       : ", steps)
  if ( opt$report ) message("horizon interval    : ", interval)
  if ( opt$report ) message("used svn            : ", usedsvn)
  if ( opt$report ) message("cut                 : ", cut)

  # cut processing call, expands 'cut' while honoring 'steps'
  if ( opt$debug ) message()
  cutdata <- xem.processCutArg(cut    = cut,
                               steps  = steps,
                               report = opt$debug)

  # integrity report
  if ( cutdata$good == FALSE ) message()
  if ( cutdata$good == FALSE ) message("caution             : 'xem.processCutArg' just returned fail, check 'cut' argument")

  # choke
  if ( cutdata$good == FALSE ) message("fatal               : ", exit$usage)
  if ( cutdata$good == FALSE ) message()
  if ( cutdata$good == FALSE ) quit(save = "no", status = exit$usage, runLast = F)

  # recover info
  lo     <- cutdata$lo
  hi     <- cutdata$hi
  rrange <- cutdata$rrange
  xrange <- cutdata$xrange

  # update cut
  opt$cut <<- xrange                         # CAUTION: deep assign

  # reporting
  if ( opt$report ) message()
  if ( opt$report ) message("lo                  : ", lo)
  if ( opt$report ) message("hi                  : ", hi)
  if ( opt$report ) message("R range             : ", rrange)

  # return value
  list(steps    = steps,                     # original, typically 8760
       interval = interval,                  # original, typically 3600
       usedsvn  = usedsvn,                   # recast into integer
       cut      = cut,                       # 'opt$cut'
       lo       = lo,                        # zero-based (add one for R)
       hi       = hi,                        # zero-based (add one for R)
       rrange   = rrange)                    # one-based "00:00"
}

# ---------------------------------
#  function : getScenDescs
# ---------------------------------
#  description  :
#  role         : utility
#  status       : complete
# ---------------------------------

getScenDescs <- function(letas)
  {
    if ( opt$debug ) message()

#     final <- "h"
#     end   <- which(letters == final)
#     letas <- c("+", letters[1:end])

    buffer <- list()
    for (leta in letas)
      {
        xfqf <- paste(study, leta, "program.study-description.scenario-name", sep = ".")
        desc <- xem.grab(xfqf = xfqf, want = "string", report = opt$debug)
        buffer[[leta]] <- desc
      }
    list(letas        = letas,
         descriptions = buffer)
  }

# ---------------------------------
#  function : summarizeTimeseries
# ---------------------------------
#  description  : summarize 'timeseries' and return a suitably formatted string
#  role         : utility
#  status       : complete
# ---------------------------------

summarizeTimeseries <- function(timeseries,
                                interval = 3600)
{
  stats    <- character(0)
  stats[1] <- sprintf(  "len = %d",    length(timeseries))
  stats[2] <- sprintf(  "min = %+.3e", min(timeseries))
  stats[3] <- sprintf( "mean = %+.3e", mean(timeseries))
  stats[4] <- sprintf(  "max = %+.3e", max(timeseries))
  stats[5] <- sprintf(  "sum = %+.3e", sum(timeseries))
  stats[6] <- sprintf(  "int = %+.3e", sum(timeseries) * interval)
  stats[7] <- sprintf("first = %+.3e", head(timeseries, 1))
  stats[8] <- sprintf( "last = %+.3e", tail(timeseries, 1))
  paste(stats, collapse = " | ")
}

# ---------------------------------
#  function : myplot
# ---------------------------------
#  description  : interface to 'xem.xplot'
#  role         : utility
#  status       : complete
# ---------------------------------

myplot <- function(timeseries,
                   title,
                   subtitle1,                # top subtitle
                   kind,
                   units,
                   stub,
                   leta,
                   tall,                     # length two numeric vector
                   identify = FALSE)
{
  ## experimental -- test: $ xplot.R -9k -n 5 -s +
# if ( units == "[C]" ) units <- deparse(substitute(degree, "C"))  # chokes
# if ( units == "[C]" ) units <- degree ~ C                        # buggy
# if ( units == "[C]" ) units <- "[" ~ degree ~ C ~ "]"
# if ( units == "[C]" ) units <- expression(paste("[", degree, "C", "]", sep=""))

  # horizon data
  steps     <- horizon$steps
  interval  <- horizon$interval
  usedsvn   <- horizon$usedsvn
  rrange    <- horizon$rrange

  # meta-data
  desc      <- scenario$descriptions[leta]
  subtitle2 <- paste(leta, desc, sep = " = ")
  subtitle2 <- paste("scenario", subtitle2, sep = " ")
  subtitle2 <- paste(subtitle2, kind, sep = " / ")
  model     <- paste(stub, leta, xemext, sep = ".")

  # x-axis creation and truncation
  timebase <- 1:length(timeseries) - 1            # zero-base via the minus one
  tbase    <- tstrunc(timebase, rrange)

  # x-axis limits
  # not required, implicit in 'tbase'

  # y-axis truncation along x-dimension
  tseries <- tstrunc(timeseries, rrange)

  # y-axis limits
  yme  <- range(tseries)                          # 'yme' is for this scenario
  ylo  <- if ( opt$tall ) tall[1] else yme[1]     # 'tall' is for all scenarios
  yhi  <- if ( opt$tall ) tall[2] else yme[2]
  # specific adjustments
  ylo  <- if ( opt$zero && ylo > 0.0 ) 0.0 else ylo
  yhi  <- if ( opt$ymax == 0.0 ) yhi else opt$ymax
  # package
  ylim <- c(ylo, yhi)                             # suitable for a 'plot' command

  # plot type
  type <- ifelse(opt$lines, 'l', 'p')             # lines, points

  # extended plot call
  pts <- xem.xplot(timebase     = tbase,
                   timeseries   = tseries,
                   ylim         = ylim,
                   grid         = opt$grid,
                   title        = title,
                   subtitle1    = subtitle1,
                   subtitle2    = subtitle2,
                   yano         = yano,           # set in preamble
                   units        = units,
                   interval     = interval,
                   stub         = stub,
                   leta         = leta,
                   script       = script,
                   model        = model,
                   svn          = usedsvn,
                   type         = type,
                   identify     = identify,
                   export       = opt$export,
                   exportClose  = T,              # no post-call plot additions required
                   exportAuto   = opt$eXport)     # CAUTION: upcase X is correct

  # report as required
  if ( opt$report )
    {
      plotno <- sprintf("%02d", xem.plot.count)   # incremented by now!
      message()
      message("plot      : ", plotno)
      if ( opt$eXport ) message("export    : automatic (batch mode) export invoked")
      message("model     : ", model)
      message("subtitle  : ", subtitle1)
      message("report    : ", summarizeTimeseries(tseries, interval))
    }
  if ( length(pts) )
    {
      message("values    : ", pts)
    }
}

# ---------------------------------
#  function : convertLetas
# ---------------------------------
#  description  : convert 'letas' string to character vector
#  role         : general use
#  status       : complete
# ---------------------------------

convertLetas <- function(letas)
{
  if ( letas == "*" ) letas <- all.scens                    # "*" means use all scenarios
  sens <- unlist(strsplit(letas, split = character(0)))     # split on null
  sens <- sens[sens != " "]                                 # remove single blanks
}

# ---------------------------------
#  function : parseMeta
# ---------------------------------
#  description  : extract meta-information using 'key' and 'text'
#  role         : general use
#  status       : complete
# ---------------------------------

parseMeta <- function(key,                   # key
                      text)                  # catalog file as string
{
  regex <- "[[:blank:]]*="                   # permissive form
  trip  <- paste(key, regex, sep = "")
  hit   <- text[grepl(trip, text)]           # 'grepl' is logical grep
  got   <- unlist(strsplit(hit, split = "=", fixed = T))
  if ( length(got) != 2 )
    {
      if ( opt$debug ) warning("split on \"=\" failed, assigning NA instead", immediate. = T)
      value <- NA
    } else {
      value <- got[2]                            # take the second
      value <- gsub('^[[:space:]]+', '', value)  # trim leading whitespace
      value <- gsub('[[:space:]]+$', '', value)  # trim trailing whitespace
      value <- gsub('[[:space:]]+', ' ', value)  # trim trailing whitespace
      if ( key == "scens" ) value <- convertLetas(value)
    }
  if ( opt$report )
    {
      keystr <- sprintf("%-20s:", key)
      cat(keystr, value, file = "/dev/stderr")
      cat("\n", file = "/dev/stderr")
    }
  value
}

# ---------------------------------
#  function : slurpMeta
# ---------------------------------
#  description  : obtain scenario meta-information
#  role         : general use
#  status       : complete
#
#  Format
#
#    This function expects something similar in the
#    first few lines:
#
#      # role      = general review
#      # study     = trial-008
#      # scens     = + e
#      # cut       = 1000:2000
#
# ---------------------------------

slurpMeta <- function(catalogFile)
{
  if ( opt$debug ) message("function            : ", "slurpMeta")

  # slurp
  text <- readLines(con  = catalogFile,
                    n    = 15,               # number of lines to read
                    ok   = TRUE,             # can be short
                    warn = FALSE)            # final EOL not required

  if ( opt$report ) message()
  if ( opt$report ) message("meta information    : ", catalogFile)

  # parse
  keys <- c("role", "study", "scens", "cut", "lines", "tall", "ymax", "zero")
  bufs <- list()
  for (key in keys)
    {
      value      <- parseMeta(key, text)
      buf        <- list(value)              # add value
      names(buf) <- key                      # add name
      bufs       <- append(bufs, buf)
    }
  if ( opt$debug )
    {
      message()
      print(text)
      print(bufs)
    }

  # experimental
  val <- bufs[["study"]]
  if ( ! is.na(val) ) study <<- val

  # overwrite 'opt' values as required, noting that
  # command-line usage takes precedence
  reps <- c("cut", "lines", "tall", "ymax", "zero")
  for (rep in reps)
    {
      val <- bufs[[rep]]
      if ( opt$debug ) message("key = value         : ", rep, " = ", val)
      # defensive
      if ( is.null(val) ) stop("you forgot to add the new key '", rep, "' to 'keys' vector")
      # nothing read
      if ( is.na(val) ) next
      # option explicitly set on command-line (maintain order of tests)
##    if (    length(opt[[rep]]) == 0 ) next
      if ( !   is.na(opt[[rep]])      ) next

      # coerce (recast) as required
      was <- opt[[rep]]
      opt[[rep]] <<- switch(rep,                       # CAUTION: must write deep
                            cut   = val,               # 'cut'   is string
                            lines = as.logical(val),   # 'lines' is boolean string like "T" or "TRUE"
                            tall  = as.logical(val),   # 'tall'  is boolean string like "T" or "TRUE"
                            ymax  = as.numeric(val),   # 'ymax'  is float
                            zero  = as.logical(val))   # 'zero'  is boolean string like "T" or "TRUE"

      # report as required (no further colons in the output)
      if ( opt$report )
        {
          msg1 <- sprintf("%-5s", rep)
          msg2 <- sprintf("%4s",  val)
          msg3 <- format(was)
          msg4 <- format(opt[[rep]])
          message("default overwrite   : ", paste(msg1, msg3, msg4, sep = "  "))
        }
    }

  # check data integrity / TOFIX: 13-Dec-2011: consider setting defaults here and using values in code
  statii <- logical(0)
  for (rep in reps) statii <- append(statii, is.na(opt[[rep]]))
  nas <- reps[statii == TRUE]                    # subset
  if ( length(nas) > 0 )
    {
      message()
      cat("PROBLEM opts        :", nas, file = "/dev/stderr")
      cat("\n", file = "/dev/stderr")
    }

  # restrict scenarios based optionally on '--scens' -- could also simply overwrite
  if ( ! is.na(opt$scens) )
    {
      if ( opt$report ) message()
      optscens <- convertLetas(opt$scens)
      if ( opt$report ) message("user input          : ", optscens)
      if ( opt$report ) message("read in             : ", bufs$scens)
      bufs$scens <- intersect(bufs$scens, optscens)
      if ( opt$report ) message("scenario intersect  : ", bufs$scens)
    }

  # return
  bufs
}

# ---------------------------------
#  function : slurpFqfs
# ---------------------------------
#  description  : obtain list of fully-qualified field names and titles
#  role         : general use
#  status       : complete
# ---------------------------------

slurpFqfs <- function(catalogFile,
                      fault = exit$emptyDataFrame)     # complains and exits 'fault' if greater than zero
{
  colnames <- c("fqf", "title")
  recs     <- read.table(file         = catalogFile,
                         col.names    = colnames,
                         comment.char = "#")

  if ( nrow(recs) == 0 && fault != 0 )
    {
      message()
      message("empty data frame")
      message("fatal status       : ", fault)
      message()
      quit(save = "no", status = fault, runLast = F)
    }

  if ( opt$report )
    {
      message()
      message("fields + titles     : ", catalogFile)
      message("data                : (below)")
      print(recs)
    }

  recs
}

# ---------------------------------
#  function : dataWrapper2
# ---------------------------------
#  description  : create plot call or print to console
#  role         : general use
#  status       : complete
#
#  Error handling logic
#
#    when 'xgrab' returns exit code 23 for "sought
#    record not found", 'xem.grab' returns an NA but
#    does not warn -- in all other fault cases,
#    'xem.grab' issues a warning too
#
#    the reason for this "suppress warnings" code is
#    that 'scan' chokes on strings under "data"
#
#  Note also:
#
#    prior.sem <- options(show.error.messages = opt$debug)
#    options(show.error.messages = prior.sem)
#
# ---------------------------------

line.trip <- T
plot.trip <- F

dataWrapper2 <- function(stub,
                         sen,
                         fqf,
                         title,
                         value,
                         kindtag,
                         units,
                         tall)
{
  if ( length(value) == 0 ) {                # problem (probably a bug)
    stop("empty value : ", value)
  } else if ( length(value) > 1 ) {          # timeseries
    subtitle <- fqf
    myplot(value, title, subtitle, kindtag, units, stub, sen, tall = tall, identify = opt$identify)
    Sys.sleep(opt$sleep)
    line.trip <<- opt$report
    ifelse( length(dev.list()) > plot.alarm, plot.trip <<- T, plot.trip <<- F)
  } else if ( is.na(value)) {                # absent
    if ( line.trip ) message()
    line.trip <<- F
    xfqf <- paste(stub, sen, fqf, sep = ".")
    buf  <- sprintf("absent    : %-80s = %s", xfqf, value)
    if ( opt$report ) message(buf)
  } else {                                   # scalar - note the "%+.3e"
    if ( line.trip ) message()
    line.trip <<- F
    xfqf       <- paste(stub, sen, fqf, sep = ".")
    xfqfunits  <- paste(xfqf, units, sep = " ")
    buf <- sprintf("scalar    : %-80s %s %+.3e", xfqfunits, kindtag, value)
    if ( opt$report ) message(buf)
  }
  return(NULL)
}

# ---------------------------------
#  function : dataWrapper1
# ---------------------------------
#  description  : create call to 'dataWrapper2'
#  role         : general use
#  status       : complete
# ---------------------------------

dataWrapper1 <- function(stub,               # study stub
                         sens,               # scenario letas
                         row)                # fqf and title
{
  ylo   <- +Inf
  yhi   <- -Inf
  i     <- 0                                 # a counter required for loading 'infos' (no 'push'!)
  infos <- list()
  for (sen in sens)
    {
      i <- i + 1

      fqf   <- row["fqf"]
      title <- row["title"]
      xfqf  <- paste(stub, sen, fqf, sep = ".")

      prior.swm <- options(show.warning.messages = opt$debug)
      prior.sem <- options(show.error.messages   = opt$debug)

      value <- try(xem.grab(xfqf = xfqf, want = "data",    report = opt$debug))
      kind  <- try(xem.grab(xfqf = xfqf, want = "kind",    report = opt$debug))
      units <- try(xem.grab(xfqf = xfqf, want = "measure", report = opt$debug))

      options(show.errors.messages  = prior.sem)
      options(show.warning.messages = prior.swm)

      if (   class(value) == "try-error"
          || class(kind)  == "try-error"
          || class(units) == "try-error" )
        {
          file <- paste(stub, sen, xemext, sep = ".")
          if ( opt$report) message("skip      : ", file , " | ", fqf)
          next                               # CAUTION: skip around
        }

      # ratchet the 'y' bounds
      ymin <- min(value, na.rm = T)          # CAUTION: NA protection essential
      ymax <- max(value, na.rm = T)
      ylo  <- min(ylo, ymin)
      yhi  <- max(yhi, ymax)

      # process 'kind'
      kindtag <- character(0)
      if ( ! is.na(kind) )                   # CAUTION: NA protection required
        {
          kindtag <- if ( kind == ">" ) "input" else if ( kind == "<" ) "output" else kind
          kindtag <- paste("data source", kindtag, sep = " = ")
        }

      # protect when units are completely omitted in XEM file
      if ( length(units) == 0 )              # CAUTION: NA protect caused faults, length must not be zero
        {
          units <-"(none)"
        }

      # pack and store this information
      info       <- list(sen = sen, fqf = fqf, title = title, value = value, kindtag = kindtag, units = units)
      infos[[i]] <- info

    } # scenarios loop

  # cycle thru
  for (info in infos)
    {
      dataWrapper2(stub,
                   info$sen,
                   info$fqf,
                   info$title,
                   info$value,
                   info$kindtag,
                   info$units,
                   c(ylo, yhi))              # known as 'tall'
    }
}

# ---------------------------------
#  function : regexize
# ---------------------------------
#  description  : improve 'word' under simple conditions
#  role         : general use
#  status       : complete
# ---------------------------------

regexize <- function(word)
{
  term <- word
  term <- sub("^ +", "", term)
  term <- sub(" +$", "", term)
  if ( ! grepl("*", term, fixed = T) )
    {
      term <- sub("\\.", "\\\\.", term)      # regexize '.' if no '*' present
    }
  if ( term != word || opt$report )
    {
      message()
      message("word (before/after) : '", word, "' '", term, "'")
    }
  return(term)
}

# ---------------------------------
#  function : allFqfs
# ---------------------------------
#  description  : get all fully-qualified fields
#  role         : general use
#  status       : complete
# ---------------------------------

allFqfs <- function(leta = "+")
{
  # grab data
  xemstub <- paste(study, leta, xemext, sep = ".")
  allfqfs <- xem.grab(xfqf = xemstub, want = "names", report = opt$debug)

  # rework titles
  titles  <- allfqfs
  titles  <- gsub("^entity\\.", "", titles)
  titles  <- gsub("\\.", " . ", titles)
  if ( F )
    {
      titles  <- gsub("-", " ", titles)
    }
  # spelling tweaks
  titles  <- gsub("losss$", "losses", titles)
  titles  <- gsub("dutys$", "duties", titles)

  # prefixes - optional
  if ( T )
    {
      titles  <- sub("^asop[ -]", "asset operator . ",  titles)
      titles  <- sub(  "^cm[ -]", "commodity . ",       titles)
      titles  <- sub(  "^cx[ -]", "context . ",         titles)
      titles  <- sub("^junc[ -]", "junction . ",        titles)
      titles  <- sub("^node[ -]", "node . ",            titles)
      titles  <- sub("^teas[ -]", "technical asset . ", titles)
    }

  # create data frame
  recs    <- data.frame(fqf   = allfqfs,
                        title = titles)
  if ( opt$report )
    {
      message()
      message("xemstub             : ", xemstub)
      message("fqf count           : ", length(allfqfs))
      message("first element       : ", allfqfs[1])
      message("data                : (below)")
      print(recs)
    }
  recs
}

# ---------------------------------
#  function : tmpCatalog
# ---------------------------------
#  description  : create a temporary catalog for use under '--all'
#  role         : general use
#  status       : complete
# ---------------------------------

tmpCatalog <- function(file,
                       role = "temporary catalog file",
                       study,                # hardcoded at start of file
                       scens,                # perhaps "+abcdefgh"
                       defs,
                       fault)
{
  if ( nchar(scens) == 0 )
    {
      message()
      message("no scenarios")
      message("fatal status       : ", fault)
      message()
      quit(save = "no", status = fault, runLast = F)
    }

  text <- paste(role, " (", file, ")", sep = "")
  str <- character(0)
  str <- append(str, paste("# role  ", text,    sep = " = "))
  str <- append(str, paste("# study ", study,   sep = " = "))
  str <- append(str, paste("# scens ", scens,   sep = " = "))
  str <- append(str, paste("# cut   ", defs[1], sep = " = "))
  str <- append(str, paste("# lines ", defs[2], sep = " = "))
  str <- append(str, paste("# tall  ", defs[3], sep = " = "))
  str <- append(str, paste("# ymax  ", defs[4], sep = " = "))
  str <- append(str, paste("# zero  ", defs[5], sep = " = "))

  if ( opt$report ) message()
  if ( opt$report ) cat(str, sep = "\n", file = "/dev/stderr")

  cat(str, sep = "\n", file = file)
  file
}

# ---------------------------------
#  function : fakeAsRequired
# ---------------------------------
#  description  : create a symlink with a 'leta' to fit the general pattern
#  role         : only for 'xeona' submodels (as opposed to proof-of-concept models)
#  returns      : symlink name or NA
#  status       : complete
#
#  CAUTION: 'file.exists'
#
#    The 'file.exists' call checks the target and not
#    the symlink and cannot be persuaded to do
#    otherwise.
#
# ---------------------------------

fakeAsRequired <- function(meta)
{
  if ( ! grepl("^test-", meta$study) ) return(NA)

  original  <- paste(meta$study, xemext, sep = ".")
  reference <- paste(meta$study, meta$scens[1], xemext, sep = ".")

  if ( ! file.exists(original) ) return(NA)

  call1     <- paste("rm --force", reference)
  call2     <- paste("ln --symbolic", original, reference)

  if ( opt$report )
    {
      message()
      message("temporary symlink   : ", reference)
      message("system call         : ", call2)
    }

  ret1 <- system(call1, intern = T)
  ret2 <- system(call2, intern = T)
  reference
}

# ---------------------------------
#  active code
# ---------------------------------
#  description  : main code
#  status       : complete
# ---------------------------------

# initial matters
if ( opt$mole ) moleCatalogs(script = script,
                             path   = xem.consts()$catalogs.dir,
                             catext = catext,
                             exit   = exit$success)

# aesthetics
if ( ! opt$report && opt$identify ) message()     # to separate "values: "
if ( ! opt$report && opt$export   ) message()     # to separate "filename"

# create temporary catalog file in under '--all'
if ( opt$all ) opt$file <- tmpCatalog(file  = tempfile(),
                                      study = study,                       # hardcoded at start of file
                                      scens = all.scens,                   # can be restricted via '--scens'
                                      defs  = c(":", "F", "F", "0", "F"),  # default values set here
                                      fault = exit$tmpCatalogFault)

# meta-data processing
catcnt   <- confirmFiles(opt$file, fault = exit$missingCatalogFile)
meta     <- slurpMeta(opt$file)
xems     <- paste(study, meta$scens, xemext, sep = ".")
fake     <- fakeAsRequired(meta)
xemcnt   <- confirmFiles(xems, fault = exit$missingXemFiles)
horizon  <- processHorizon(opt$cut, meta$scens)
scenario <- getScenDescs(meta$scens)

# catalog-data processing with additionally '--number' then '--word' then '--omitcosts'
fqfs     <- if ( opt$all ) allFqfs() else slurpFqfs(opt$file)
origs    <- nrow(fqfs)
fqfs     <- if ( opt$number > 0 ) fqfs[opt$number,] else fqfs    # extract single row
word     <- regexize(opt$word)                                   # word regex used to filter the catalog
sel1     <- grepl(word, as.matrix(fqfs["fqf"]))                  # identify selections
fqfs     <- fqfs[sel1,]                                          # apply selections

triga    <- "\\.[[:alnum:]]+-costs-[[:alnum:]]+$"                # TOFIX: 16-Dec-2011: tidy this up perhaps
sel2     <- (grepl(triga, as.matrix(fqfs["fqf"])) == F)          # identify selections
fqfs     <- if ( opt$omitcosts ) fqfs[sel2,] else fqfs

# report some
if ( opt$debug )
  {
    message()
    message("selections length   : ", length(sel1))
    cat("selections          :", sel1, "\n", file = "/dev/stderr")
    message("fqfs class          : ", class(fqfs))
    if ( nchar(word) != 0 ) message()        # when 'regexize' does not add blank line
  }
if ( opt$report )
  {
    if ( nchar(word) == 0 ) message()        # else 'regexize' adds blank line
    if ( opt$number >  0 || nchar(word) >  0 )
      {
        message("row number          : ", opt$number)
        if ( nrow(fqfs) >  0 )
          {
            message("data subset         : ", "(below)")
            print(fqfs)
          }
        message()
      }
    final <- nrow(fqfs)
    delta <- origs - final
    message("fqf final count     : ", sprintf("%4d", final))
    if ( delta != 0   ) message("fqf screened out    : ", sprintf("%4d", delta))
    if ( opt$identify ) message("identify            : ", "active")
    if ( opt$debug    ) message()

    snooze <- ifelse(opt$sleep > 0, opt$sleep, 1)      # 'opt$sleep' defaults to zero
    if ( opt$identify ) snooze <- snooze + 2
    Sys.sleep(snooze)                                  # quick read of console
  }

# plot loop
for (i in 1:nrow(fqfs))
  {
    if ( opt$early ) break
    if ( nrow(fqfs) == 0 ) break             # TOFIX: 16-Dec-2011: consider another way
    row <- as.matrix(fqfs)[i,]
    warnmsg <- suppressWarnings(dataWrapper1(study, meta$scens, row))
    if ( plot.trip )                         # maintained by 'dataWrapper'
      {
        xem.keepOpen(prompt = "hit space bar to close plots and continue",
                     extra  = paste("plot windows count:", length(dev.list())),
                     action = opt$keep)
        graphics.off()                       # kill all plot windows
      }
  }

# ---------------------------------
#  hold-open functionality
# ---------------------------------

# call up Tk dialog as required
if ( xem.plot.count > 0 ) xem.keepOpen(extra = paste("script :", script), action = opt$keep)   # note the 'opt$keep' test too

# ---------------------------------
# housekeeping
# ---------------------------------

# remove symlink if present, only created for 'xeona' submodels tho
ret <- if ( ! is.na(fake) ) system(paste("rm --force", fake), intern = F)

# completion reporting
message()
if ( opt$report               ) message("xeona range         : ", opt$cut)
if ( opt$report && opt$zero   ) message("zeroed              : ", "yes")
if ( nchar(word) > 0          ) message("word                : '", word, "'")
if ( opt$early                ) message("early               : ", "yes")
if ( opt$sleep > 0            ) message("sleep               : ", opt$sleep)
if ( xem.plot.count > 0       ) message("plot count          : ", xem.plot.count)   # as per the 'xemplot' window titles
message("catalog role        : ", meta$role)
message("catalog file        : ", opt$file)
message("command-line        : ", script, " ", clargs)
message()

# repeat buffered warnings
wz <- warnings()
if ( ! is.null(wz) ) print(wz)
if ( ! is.null(wz) ) message()

# exit
quit(save = "no", status = exit$success, runLast = F)

#  $Id: xplot.R 9204 2012-03-30 11:36:35Z robbie $
#  end of file

