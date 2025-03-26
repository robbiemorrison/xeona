#! /usr/bin/Rscript --vanilla

#  file-purpose     : compare two 'xeona' log files
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Tue 20-Dec-2011 23:51 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9125 $
#  $Date: 2012-02-28 15:52:19 +0100 (Tue, 28 Feb 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/cmpstudy.R $

#  TODO
#
#  NOTES
#
#    * the data frames are sorted on 'leta' which means
#      that the original log files can be written out
#      in any row order
#
#    * numeric-looking data is cast to 'numeric' and
#      compared using floating point arithmetic --
#      given that no calculations occur, this should
#      not be a problem (but note also 'all.equal'
#      which tests floats to 'tolerance')

# ---------------------------------
#  preamble
# ---------------------------------

study  <- "trial-008"
logext <- "log"
catdir <- "catalogs"
attdir <- 'attic'

# ---------------------------------
#  libraries
# ---------------------------------

ret <- suppressPackageStartupMessages(expr = T)

library(getopt)                              # straightforward custom installation
library(robbie)                              # private library

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
  logfile <- paste(study, logext, sep = ".")
  message()
  message("              usage: ", script, "                     compare against default")
  message("                     ", script, "  --attic <num>  -a  compare against attic 'num'")
  message("                     ", script, "  --help         -h  display this message and exit")
  message("            purpose: compare two 'xeona' log files")
  message("          hardcodes: logfile = ", logfile)
  message()
 quit(save = "no", status = exit.status, runLast = F)
}

# ---------------------------------
#  command-line processing
# ---------------------------------

# first define some exit codes
exit <- list(success      = 0,
             failure      = 1,
             usage        = 2,
             missingFiles = 10,
             test         = 255)

# obtain script name
script <- robbie.getScriptName()

# usage specification
spec = c(                                    # 'getopt' specification
  'help'        , 'h', 0, "logical",
  'attic'       , 'a', 1, "integer")

# process command-line
opt <- getopt(spec = matrix(ncol = 4, data = spec, byrow = T))   # principal call

# help
if ( ! is.null(opt$help) ) displayUsage(script, exit$success)

# ==== active section ====================================================================

# ---------------------------------
#  function : getRefdir
# ---------------------------------
#  description  : determin the path to the reference directory
#  role         : basic call
#  status       : complete
# ---------------------------------

getRefdir <- function(opt.attic)
{
  # modify path if necessary
  wkdir <- getwd()
  parts <- unlist(strsplit(wkdir ,split = .Platform$file.sep))
  tail2 <- parts[length(parts) - 1]
  if ( tail2 == attdir )
    {
      attdir <- file.path("..", "..", attdir)     # attic directory
      catdir <- file.path("..", "..", catdir)     # catalog directory
    }

  # determine reference dir
  if ( is.null(opt.attic) )
    {
      refdir <- catdir
    }
  else
    {
      numdir <- sprintf("%02d", opt$attic)
      refdir <- file.path(attdir, numdir)
    }
  return(refdir)
}

# ---------------------------------
#  function : testFiles
# ---------------------------------
#  description  : compare the given 'files'
#  role         : basic call
#  status       : complete
# ---------------------------------

testFiles <- function(files)                 # expected youngest first
{
  exists  <- file.exists(files)
  absents <- files[exists == FALSE]

  if ( length(absents) > 0 )
    {
      for (file in absents)
        {
          message("required file missing: ", file)
        }
      status <- exit$missingFiles
      quit(save = "no", status = status, runLast = F)
    }

  infos  <- file.info(files)

  sizes  <- infos$size                       # sizes must be equal
  mtimes <- infos$mtime                      # file last write times must get older (decrease)

  sizerange <- range(sizes)
  mtimesort <- sort(mtimes, decreasing = T)

  sizeret   <- ( sizerange[1] == sizerange[2] )           # minimum bytes versus maximum bytes
  mtimeret  <- all(( mtimes == mtimesort ) == TRUE)       # element-wise, then 'all'
  ret       <- all(c(sizeret, mtimeret) == TRUE)

  if ( ret == FALSE )
    {
      message()
      message("size range      : ", paste(sizerange, collapse = " "))
      message("mtime raw       : ", paste(mtimes,    collapse = " "))
      message("mtime sorted    : ", paste(mtimesort, collapse = " "))
      message("size return     : ", sizeret)
      message("mtime return    : ", paste(mtimeret, collapse = " "))
      message("combined        : ", ret)
    }

  return (ret)
}

# ---------------------------------
#  function : slurpLogs
# ---------------------------------
#  description  : slurp logfile into data frame, then sort and return
#  role         : basic call
#  status       : complete
# ---------------------------------

slurpLogs <- function(logfile)
{
  if ( ! file.exists(logfile) ) stop(paste("requested logfile not found :", logfile))
  finfo <- file.info(logfile)

  colnames <- c("xeona", "time", "leta", "scenario.name", "steps", "fin", "ghg", "nox", "dep", "luc", "xem.file")
  data <- read.table(file = logfile, header = F, sep = "", col.names = colnames, stringsAsFactors = F)
  data <- data[with(data, order(leta)), ]                   # sort on 'leta' col, noting "+" sorts first

  message()
  message("logfile             : ", logfile)
  message("bytes               : ", finfo$size)
  message("last modified       : ", finfo$mtime)
  message("data rows           : ", nrow(data))

  return(data)
}

# ---------------------------------
#  function : compareRows
# ---------------------------------
#  description  : compare two rows by column
#  role         : called by 'compareLogs'
#  status       : complete
# ---------------------------------

compareRows <- function(rowlog, rowref)
{
  buff <- character(0)
  scen <- rowref[["leta"]]
  cols <- ncol(rowref)
  for (j in 1:cols)
    {
      colname <- names(rowref)[j]
      vallog  <- rowlog[j]                   # class data. frame
      valref  <- rowref[j]                   # class data. frame
      if ( colname == "time" ) next          # skip the timing column
      if ( vallog == valref  ) next
      colname <- paste("'", colname, "'", sep = "")
      buff <- append(buff, sprintf("  study '%s' scenario '%s' colname %-8s mismatch: %10s  %10s", study, scen, colname, vallog, valref))
    }
  return(buff)                               # non-zero length indicates failure
}

# ---------------------------------
#  function : compareLogs
# ---------------------------------
#  description  : compare two log files
#  role         : basic call
#  status       : complete
# ---------------------------------

compareLogs <- function(datalog, dataref)
{
  buff <- character(0)
  rows = nrow(dataref)
  for (i in 1:rows)
    {
      rowlog <- datalog[i,]
      rowref <- dataref[i,]
      okay   <- compareRows(rowlog, rowref)
      buff   <- append(buff, okay)
    }

  if ( length(buff) == 0 ) return(TRUE)

  msgs <- paste(buff, collapse = "\n")
  message()
  message(msgs)
  return(FALSE)
}

# ---------------------------------
#  active code
# ---------------------------------

logfile <- paste(study, logext, sep = ".")
refdir  <- getRefdir(opt$attic)

filelog <- file.path(        logfile)
fileref <- file.path(refdir, logfile)

ret1 <- testFiles(c(filelog, fileref))

datalog <- slurpLogs(filelog)
dataref <- slurpLogs(fileref)

ret2 <- compareLogs(datalog = datalog, dataref = dataref)

ret  <- all(c(ret1, ret2) == TRUE)           # combined returns

message()
if ( ret  == TRUE  ) message("okay")
if ( ret1 == FALSE ) message("file test failures (ref listed last) **")
if ( ret2 == FALSE ) message("log match failures (ref listed last) **")

# ---------------------------------
#  housekeeping
# ---------------------------------

message()

status <- ifelse(ret, 0, 1)
quit(save = "no", status = status, runLast = F)

# end of file

#  $Id: cmpstudy.R 9125 2012-02-28 14:52:19Z robbie $
#  end of file

