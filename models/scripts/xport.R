#! /usr/bin/Rscript --vanilla

#  file-purpose     : save a near complete set of graphics for report writing purposes
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Wed 21-Dec-2011 11:11 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9160 $
#  $Date: 2012-03-03 02:06:12 +0100 (Sat, 03 Mar 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/xport.R $

# TODO
#
#    - improve 'cleanAll' and seek confirmation for non-stub PDFs

# NOTES

# emacs stanza

#  emacs text editor settings
#  local variables:
#    mode: text
#    make-backup-files: nil
#    truncate-lines: t
#    comment-start: "#"
#    eval: (highlight-lines-matching-regexp (concat "#" "#") "hi-yellow")
#    eval: (highlight-lines-matching-regexp (concat "#" ">") "highlight-current-line-face"))
#    tab-stop-list: (02 15 26 32 38 44 50 108)
#  end:

# ---------------------------------
#  preamble
# ---------------------------------

study      <- "trial-008"                    # hardcoded study
xplot      <- "xplot.R"                      # workhorse utility
hms        <- "hms"                          # optional utility that converts seconds to hh:mm:ss

job.file   <- "xport-01.txt"                 # hardcoded jobs file

clean.exts <- c("gif", "png", "pdf", "svg", "csv")
clean.exts <- c("gif", "png", "svg", "pdf")

# ==== command processing section ==============================================================

# ---------------------------------
#  general libraries
# ---------------------------------

# functions may also 'require' libraries

ret <- suppressPackageStartupMessages(expr = T)

library(getopt)                              # straightforward custom installation

library(robbie)
library(xem)

# ---------------------------------
#  function : editFiles
# ---------------------------------
#  description  : open 'files' in given editor
#  role         : utility
#  status       : complete
# ---------------------------------

editFiles <- function(script,
                      files)                 # files a character vector
{
  filez <- paste(files, collapse = " ")
  call  <- paste("memacs", files, sep = " ")
  ret   <- system(call, intern = F)
  quit(save = "no", status = ret, runLast = F)
}

# ---------------------------------
#  function : argument
# ---------------------------------
#  description  : create and return string: --option="file"
#  role         : utility
#  status       : complete
# ---------------------------------

argument <- function(option,
                   file)
{
  qfile    <- paste("\"", file, "\"", sep = "")
  argument <- paste(option, qfile, sep = "=")
}

# ---------------------------------
#  function : pdfAll
# ---------------------------------
#  description  : use 'inkscape' program to convert SVG to PDF
#  role         : utility
#  status       : complete
# ---------------------------------

pdfAll <- function(script,
                   debug)
{
  # set exit code
  ret <- exit$success

  # grab time
  tic <- as.numeric(format(Sys.time(), "%s"))

  # identify files
  svgs <- list.files(pattern=".*\\.svg")

  # read-only -- the following cannot be implemented using 'Sys.chmod'
  modes  <- paste("a-w", "o-r", "ug+r", sep = ",")

  # convert files
  message()
  count <- 0
  rets  <- 0
  for (svg in svgs)
    {
      count  <- count + 1
      pdf    <- sub("\\.svg$", ".pdf", svg)  # swap extension
      # LaTeX fix for multi-dotted graphics file names
      # see http://www.tex.ac.uk/cgi-bin/texfaq2html?label=grffilenames
      pdf    <- gsub("\\.", "_", pdf)
      pdf    <- gsub("\\_pdf", ".pdf", pdf)
      # 'inkscape' call
      input  <- argument("--file", svg)
      output <- argument("--export-pdf", pdf)
      call1  <- paste("inkscape", input, output, sep = " ")
      if ( debug ) message("call : ", call1)
      ret1   <- system(call1, intern = F)
      # 'pdfrobbie' call
      if ( count == 1 ) Sys.sleep(1)         # CAUTION: this delay necessary
      call2  <- paste("pdfrobbie", paste("\"", pdf, "\"", sep = ""), "1>/dev/null", sep = " ")
      if ( debug ) message("call : ", call2)
      ret2   <- system(call2, intern = F)
      # read-only calls
      ret3   <- system(paste("chmod", "--silent", modes, svg, sep = " "))
      ret4   <- system(paste("chmod", "--silent", modes, pdf, sep = " "))
      # housekeeping
      rets <- rets + ret1 + ret2 + ret3 + ret4
      if ( ! debug ) cat(".")                # somewhat crude progress bar
    }
  if ( ! debug ) cat("\n")

  # process time
  toc <- as.numeric(format(Sys.time(), "%s"))
  tif <- toc - tic
  tos <- sprintf("%d", tif)
  if ( nchar(Sys.which(hms)) > 0 )
    {
      call <- paste(hms, tos, sep = " ")
      tos  <- system(call, intern = TRUE)
    }

  # final reporting
  message()
  message("total files converted : ", count)
  if ( rets > 0 ) message("combined              : ", rets)
  message("elapsed time          : ", tos)
  message()

  # exit
  quit(save = "no", status = ret, runLast = F)
}

# ---------------------------------
#  function : pdfView
# ---------------------------------
#  description  : view pdfs in order
#  role         : used by option '--view'
#  status       : complete
# ---------------------------------

pdfView <- function(debug)
{
  # set exit code
  ret <- exit$success

  # identify files
  pdfs <- list.files(pattern=".*\\.pdf")

  # view files
  message()
  count <- 0
  for (pdf in pdfs)
    {
      count <- count + 1
      call  <- paste("evince", "--fullscreen", pdf, sep = " ")
      if ( debug ) message("call = ", call)
      else cat(".")                          # somewhat crude progress bar
      Sys.sleep(1)                           # a hack -- gives an opportunity to ^C
      system(call, wait = TRUE)
    }
  if ( ! debug ) cat("\n")

  # reporting
  message()
  message("total files viewed : ", count)
  message()

  # exit
  quit(save = "no", status = ret, runLast = F)
}

# ---------------------------------
#  function : displayFormat
# ---------------------------------
#  description  : display format message and quit
#  role         : used by option '--Help'
#  status       : complete
# ---------------------------------

displayFormat <- function(script,             # script name
                          exit)               # required exit status
{
  message()
  message("# sens         cut        lines tall  ymax  zero  fqf                                                       title")
  message("# +abcdefghij  0000:0000  F     T     0     F     entity.fully-qualified-field-name                         \"title title\"")
  message()
  message("## role")
  message()
  message("  sens     : scenarios to include      * means all, - means none")
  message("  cut      : timebase                  : means no trucation")
  message("  line     : plot type                 F for points, T for lines")
  message("  tall     : maintain common y-axis")
  message("  ymax     : set y-axis max (only useful for single plots) else use 0 to disable")
  message("  zero     : zero the y-axis if min(y) is positive")
  message("  fqf      : fully-qualified fieldname")
  message("  title    : plot title")
  message()
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
                         exit)               # required exit status
{

  # values set in 'xem.plot.R'
  svg.width  <- xem.plot.svg$width.mm
  svg.height <- xem.plot.svg$height.mm
  svg.size   <- sprintf("%d wide x %dmm", svg.width, svg.height)

  # message
  message()
  message("               usage: ", script, "   [opts]             dry-run the script")
  message("                      ", script, "   [opts] --active    run script in earnest")
  message("                      ", script, "   --clean            clean up exported SVG and PDF files")
  message("                      ", script, "   --pdf              convert exported SVG files to PDF using 'inkscape'")
  message("                      ", script, "   --view             view PDF files using 'evince'")
  message("                      ", script, "   --edit             edit the job file")
  message("                      ", script, "   --help             display this message and exit")
  message("                      ", script, "   --Help             display job file format and exit")
  message("             options:  --active        -a  run in earnest")
  message("                       --debug         -d  add debug messages")
# message("                       --file <file>   -f  run using 'file' instead of '", job.file, "'")
  message("                       --keep          -k  use Tk dialog to retain plot windows")
  message("                       --report        -r  report useful information")
  message("                       --export        -x  interactive export")
  message("                       --eXport        -X  non-interactive export")
  message("             purpose: produce a set of suitably-titled graphics files from a hardcopy list")
  message("              method: creates an individual temporary catalog file for each task and invokes '", xplot, "'")
  message("           hardcodes: job file = ", job.file)
  message("               notes: output size (including a small margin) = ", svg.size)
  message("                      option --pdf converts basename dots to underscore to satisfy a LaTeX requirement")
  message("            examples: $ ", script, " --active --eXport         # seeks confirmation if previous SVG output needs deleting")
  message("                      $ ", script, " --pdf    --debug          # automatic overwrite of existing PDF files")
  message("                      $ ", script, " -aX ", "&& ",  script, " -p", "         # combined")
  message("                      $ time (", script, " -aX ", "&& ",  script, " -p", ")  # combined with external timing")
  message()

  # exit
  quit(save = "no", status = exit, runLast = F)
}

# ---------------------------------
#  function : cleanAll
# ---------------------------------
#  description  : separately clean each file extension
#  role         : utility
#  status       : complete
# ---------------------------------

cleanAll <- function(stub,
                     exts,
                     debug,
                     exit)
{
  # code
  loops <- 0
  kills <- 0
  for (ext in exts)
    {
      loops <- loops + 1
      files <- xem.cleanExportFiles(stub = stub, exts = ext, debug = debug, exit = NA)
      kills <- kills + files
    }

  message()
  message("total loops         : ", loops)
  message("total files deleted : ", kills)
  message()

  # continue unannounced with "non-stub" PDFs (should ask tho)
  pdfs <- list.files(pattern=".*\\.pdf")
  if ( length(pdfs) > 0 )
    {
      unlink(pdfs)
      no <- length(pdfs)
      message("total pdfs deleted  : ", no)
    }
  else
    {
      message("no general PDFs to delete")
    }
  message()

  # exit
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
  'active'       , 'a', 0, "logical",
  'clean'        , 'c', 0, "logical",
  'debug'        , 'd', 0, "logical",
  'eXport'       , 'X', 0, "logical",
  'edit'         , 'e', 0, "logical",
  'export'       , 'x', 0, "logical",
  'help'         , 'h', 0, "logical",
  'Help'         , 'H', 0, "logical",
  'keep'         , 'k', 0, "logical",
  'pdf'          , 'p', 0, "logical",
  'view'         , 'v', 0, "logical",
  'report'       , 'r', 0, "logical")

# 'getopt' call
opt <- getopt(spec = matrix(ncol = 4, data = spec, byrow = T))   # principal call

# set 'debug' and 'report' defaults early
if ( is.null(opt$debug)     ) opt$debug     <- F
if ( is.null(opt$report)    ) opt$report    <- F

# set up the 'job.file' properly
catalogs.dir <- xem.consts()$catalogs.dir
job.file     <- file.path(catalogs.dir, job.file)

# 'help' and friends -- order is significant
if ( ! is.null(opt$help)    ) displayUsage  (script, exit  = exit$success)
if ( ! is.null(opt$Help)    ) displayFormat (script, exit  = exit$success)
if ( ! is.null(opt$edit)    ) editFiles(script, job.file)
if ( ! is.null(opt$clean)   ) cleanAll(stub = study, exts = clean.exts, debug = opt$debug, exit = exit$success)
if ( ! is.null(opt$pdf)     ) pdfAll(debug = opt$debug)
if ( ! is.null(opt$view)    ) pdfView(debug = opt$debug)

# set some more defaults
if ( is.null(opt$active)    ) opt$active    <- F
if ( is.null(opt$export)    ) opt$export    <- F
if ( is.null(opt$eXport)    ) opt$eXport    <- F
if ( is.null(opt$keep)      ) opt$keep      <- F

# dynamic terminal width for R reporting
width <- robbie.updateTerminalWidth()

# ==== active section ====================================================================

# ---------------------------------
#  function : deleteAsk
# ---------------------------------
#  description  : delete files matching "body.ext" on user confirmation
#  role         : utility call
#  notes        : tried 'readline' but would not stop, hence 'readLines'
#  status       : complete
# ---------------------------------

# TOFIX: 28-Dec-2011: place this in the 'xem' library ??

deleteAsk <- function(body,                  # file body pattern
                      ext)                   # file extension
{
  # hunt for files
  ext   <- sub("^\\.", "", ext)               # strip any leading dot
  regex <- glob2rx(paste(body, ext, sep = "."))
  files <- list.files(pattern = regex)

  # request deletion
  count <- 0
  if ( length(files) > 0 )
    {
      message()
      message("  files to delete:")
      message()
      cat(files, sep = "\n")

      message()
      cat("  enter 'y' to delete: ")
      f <- file("stdin")
      y <- readLines(f, n = 1, warn = T)
      message()
      if ( y == "y" )
        {
          status <- file.remove(files)     # was 'exit.status' but always returned 1
          count  <- length(files)
          plural <- if ( count == 1 ) "" else "s"
          message(count, " file", plural, " deleted")
        }
      else
        {
          message("no files deleted")
        }
    }

  # return number of files deleted
  return(count)
}

# ---------------------------------
#  function : slurpJobs
# ---------------------------------
#  description  :
#  role         : utility call
#  status       : complete
# ---------------------------------

slurpJobs <- function(jobfile)
{
  ret <- file.access(jobfile, mode = 4)
  if ( ret == 0 ) message("file readable       : ", jobfile) else message("file not readable   : ", jobfile)

  if ( opt$debug )
    {
      text <- scan(file             = jobfile,
                   what             = character(0),
                   sep              = "\n",
                   quiet            = T,
                   blank.lines.skip = F)
      if ( text[1] == "" ) text <- text[-1]  # check for and trim leading blank line
      x <- length(text)
      if ( text[x] == "" ) text <- text[-x]  # check for and trim trailing blank line
      text <- paste(text, collapse = "\n")
      message()
      message("---")
      message(text)
      message("---")
    }

  colnames <- c("scens", "cut", "lines", "tall", "ymax", "zero", "fqf", "title")
  recs     <- read.table(file         = jobfile,
                         col.names    = colnames,
                         comment.char = "#")
  nrows <- nrow(recs)
  message()
  message("jobs                : ", nrows)

  if ( opt$report )
    {
      message("data                : ", "(below)")
      message()
      print(recs)
    }

  return(recs)
}

# ---------------------------------
#  function : createCatalog
# ---------------------------------
#  description  : create a catalog
#  role         : utility call
#  status       : complete
# ---------------------------------

createCatalog <- function(study,
                          job)
{
  if ( length(job) == 0 ) stop("job empty")
  if ( nrow(job)   != 1 ) stop("job not single row")

  role <- paste("temporary ", xplot, " catalog file generated by script '", script, "'", sep = "")
  quotetitle <- paste("\"", job$title, "\"", sep = "")

  text <- character(0)
  text <- append(text, paste("# role  ", role       , sep = " = "))
  text <- append(text, paste("# study ", study      , sep = " = "))
  text <- append(text, paste("# scens ", job$scens  , sep = " = "))
  text <- append(text, paste("# cut   ", job$cut    , sep = " = "))
  text <- append(text, paste("# lines ", job$lines  , sep = " = "))
  text <- append(text, paste("# tall  ", job$tall   , sep = " = "))
  text <- append(text, paste("# ymax  ", job$ymax   , sep = " = "))
  text <- append(text, paste("# zero  ", job$zero   , sep = " = "))
  text <- append(text, "")
  text <- append(text, paste("", job$fqf, quotetitle, sep = "    "))
  text <- append(text, "")
  text <- append(text, "# end of file")
  text <- append(text, "")

  catalog <- paste(text, collapse = "\n")

  if ( opt$debug ) message()
  if ( opt$debug ) message("---")
  if ( opt$debug ) cat(catalog, file = "/dev/stderr")
  if ( opt$debug ) message("---")

  return(catalog)
}

# ---------------------------------
#  function : doJobs
# ---------------------------------
#  description  :
#  role         : utility call
#  status       : complete
# ---------------------------------

doJobs <- function(study,
                   jobs,                     # data frame
                   active = FALSE)
{
  rets <- 0
  for (i in 1:nrow(jobs))
    {
      job     <- jobs[i,]
      if ( job$scens == "-" ) next           # a disabled job
      catalog <- createCatalog(study, job)
      tmpcat  <- tempfile()
      cat(catalog, file = tmpcat)
      export  <- ifelse(opt$export, " --export", "")
      export  <- ifelse(opt$eXport, " --eXport", "")
      report  <- ifelse(opt$report, " --report", "")
      keep    <- ifelse(opt$keep,   " --keep"  , "")
      file    <- paste(" --file", tmpcat, sep = " ")
      call    <- paste(xplot, export, report, keep, file, sep = "")
      if ( opt$report ) message()
      if ( opt$report ) message("system call         : ", call)
      if ( active ) ret  <- system(call, intern = F)
      if ( active ) rets <- rets + ret
    }
  return(rets)
}

# ---------------------------------
#  active code
# ---------------------------------

message()
message("commencing          : ", ifelse(opt$active, "active", "inactive"))

tic   <- as.numeric(format(Sys.time(), "%s"))

jobs  <- slurpJobs(job.file)                  # read file into data frame
count <- deleteAsk("*", "svg")
rets  <- doJobs(study, jobs, opt$active)      # process jobs

toc   <- as.numeric(format(Sys.time(), "%s"))
tif   <- toc - tic
tos   <- sprintf("%d", tif)
if ( nchar(Sys.which(hms)) > 0 )
  {
    call <- paste(hms, tos, sep = " ")
    tos  <- system(call, intern = TRUE)
  }

# ---------------------------------
#  housekeeping
# ---------------------------------

status <- ifelse(rets == 0, 0, 1)

message()
message("combined return    : ", rets)
message("command-line       : ", script, " ", clargs)
message("elapsed time       : ", tos)
message("script exit status : ", status)
message()

quit(save = "no", status = status, runLast = F)

#  $Id: xport.R 9160 2012-03-03 01:06:12Z robbie $
#  end of file

