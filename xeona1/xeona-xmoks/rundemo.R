
#  file-purpose     : manage 'xem.R' calls for a suite of XEM "submodel.00" files
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Fri 05-Jun-2009 09:17 UTC
#  file-status      : ongoing
#  file-keywords    : xeona R

#  $Revision: 5986 $
#  $Date: 2011-02-23 16:30:44 +0100 (Wed, 23 Feb 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/xeona-xmoks/rundemo.R $

#  USAGE: source("rundemo.R")
#
#  CAUTION: read-only raster files will cause problems
#
#  note that function 'robbie.killX11s' does not (currently) close "svg" devices

message("  file : rundemo.R : starting")

# ---------------------------------
#  function : gaia
# ---------------------------------
#  description  : interface to 'xem.job'
#  role         : direct to user (although 'gaias' may be more convenient)
#  takes        : 'xemfile'
#  returns      : 'mode'
#  techniques   : in particular, see CAUTIONS
#  status       : complete
# ---------------------------------

gaia <- function (xemfile,
                  dbug   = 0,                # 0 thru 3
                  jobs   = NULL,
                  snooze = 0,
                  report = 2)                # report level 2 is warning
{
  # reporting
  message("  info : gaia : commencing")
  on.exit(message("  info : gaia : complete, user time ", time, " s"))

  # preamble
  time <- numeric()                          # CAUTION: should be early, enables a clean 'stop'

  # tidying
  keep <- c("gaia", "gaias", "rundemo", "cleanup", "getStatics", "getXems", "getXmoks", "getSubs", "listXems")
  robbie.clean(keep)                         # CAUTION: this call can give lots of problems

  # source R files
  source(file.path(Sys.getenv("XEONAR"), "xem.R"))

  # specify external files
  xeona <- "../xeona.mach"

  # call job
  times <- system.time(mode <- xem.job(xemfile, xeona, dbug = dbug, jobs = jobs, snooze = snooze, report = report))

  # completion
  time  <- signif(times[1], 3)

  # close windows
  xs   <- dev.list()                         # on host 'hinau', a mix of "X11cairo" and "svg"
  loop <- 0
  if ( ! is.null(xs) ) {
    for ( i in 1:length(xs) ) {
      dev.off(which = xs[i])                 # there is no 'which' for all and dev.off(dev.list()) fails
      loop <- loop + 1
    }
  }
  message("  gaia : device kill = ", loop)
  Sys.sleep(2)                               # CAUTION: essential otherwise the device count is not reset

  # return
  invisible(mode)
}

# ---------------------------------
#  function : gaias
# ---------------------------------
#  description  : loop 'gaia' calls
#  role         : direct to user
#  techniques   : 'try', 'Sys.sleep'
#  status       : complete
#
#  jobs = NULL means use the 'r-policy' setting from the xem file
#
# ---------------------------------

gaias <- function (xemfiles,
                   range  = NULL,
                   dbug   = 0,               # 0 thru 3
                   jobs   = NULL,            # { NULL, 0 .. 31 }
                   snooze = 0,
                   report = 0,
                   clean  = 1)
{
  # check for files
  if (length(xemfiles) == 0) {
    stop("kill : gaias : no xem files supplied or captured")
  }

  # clean up as required
  if ( clean > 0 ) {
    cleanup()
  }

  # information stores
  gs       <- numeric()
  gmodes   <- numeric()
  gtrips   <- numeric()
  gtitles  <- character()
  gfiles   <- character()
  gsteps   <- numeric()
  gpolicys <- numeric()

  # loop calls
  if ( is.null(range) ) range <- 1:length(xemfiles)
  for ( g in range ) {
    xemfile <- xemfiles[g]
    if ( is.na(xemfile) )         next     # only with length(xemfiles) > 0, otherwise R complains
    mode    <- try(gaia(xemfile, dbug = dbug, jobs = jobs, snooze = snooze, report = report))
    if ( ! is.numeric(mode) ) mode <- 0
    gs       <- append(gs      , g)
    gmodes   <- append(gmodes  , mode)
    gtrips   <- append(gtrips  , xtrip)
    gtitles  <- append(gtitles , xtitle)
    gfiles   <- append(gfiles  , xemfile)
    gsteps   <- append(gsteps  , xsteps)
    gpolicys <- append(gpolicys, xpolicy)
    Sys.sleep(snooze)
  }

  # issue a mode report
  if ( is.null(jobs) ) jobstr <- "(use policy value from xem file)" else jobstr <- jobs
  rangestr <- paste(sprintf("%02d", range), collapse = " ")
  line <- "-------------------------------------------------------------"
  line <- paste("  ", line, line, "\n", sep = "")
  msg1 <- "  gaias : mode report follows (0 = all attempted modes failed)\n"
  msg2 <- paste("  gaias : range = ", rangestr, "\n", sep = "")
  msg3 <- paste("  gaias :",
                " dbug = "     , dbug,
                " / jobs = "   , jobstr,
                " / snooze = " , snooze,
                " / report = " , report,
                " / clean = "  , clean,
                "\n", sep = "")
  msg4 <-         "  idx mode trip   xem                            steps    title"
  msg4 <- c(msg4, "                                            recorded policy\n")
  cat(msg1)
  cat(msg2)
  cat(msg3)
  cat(msg4)
  cat(line)

  gmsg <- sprintf("  %02d    %d    %d    %-30s %5d    %-60s  %2d", gs, gmodes, gtrips, gfiles, gsteps, gtitles, gpolicys)
  cat(gmsg, sep = "\n")                      # the newline sep avoids a 'for' loop

  cat(line)
  if ( any(gmodes < 6 ) ) {                  # CAUTION: presumes that mode 6 was attempted
    cat("  MODE ISSUES ENCOUNTERED\n")
  }
  if ( ! setequal(range, gs) ) {
    cat("  RANGE ISSUES ENCOUNTERED\n")
  }

  # clean up as required
  if ( clean > 1 ) {
    sleep <- max(3, 2 * snooze)
    message("  sleep: ", sleep)
    Sys.sleep(sleep)
    cleanup()
  }

}

# ---------------------------------
#  function : cleanup
# ---------------------------------
#  description  : cleans up all frames opened by the R code
#  techniques   : 'killall' (system), 'dev.off' (indirectly)
#  status       : complete
# ---------------------------------

cleanup <- function()
{
  robbie.killX11s()                          # CAUTION: destroys open plot frames
  killcall <- "killall --quiet display"
  system(killcall)
  message("  system call: ", killcall)
}

# ---------------------------------
#  function : rundemo
# ---------------------------------
#  description  : wrapper to 'gaias'
#  techniques   : 'switch'
#  status       : ongoing
# ---------------------------------

rundemo <- function (snooze = 0)
{
  # reporting
  message("  info : rundemo : commencing")
  on.exit(message("  info : rundemo : complete (on.exit) using snooze = \"", snooze, "\""))

  # grab files
  xems <- character()

# xems <- getStatics()   # 1 : hardcoded (well out of date)
# xems <- getXems()      # 2 : generated
  xems <- getXmoks()     # 3 : generated : just the runable submodels
# xems <- getSubs()      # 4 : generated : from hard coded integers

  # check for files
  if (length(xems) == 0) {
    stop("kill : rundemo : no xem files supplied or captured")
  }

  # run
  time <- system.time(gaias(xems, jobs = 31, report = 3, clean = 2, snooze = snooze))

  # elapsed time (index 3)
  elapsed <- round(time[3], 0)
  message("  info : rundemo : elapsed time: ", elapsed, "s")
}

# ---------------------------------
#  function : getStatics
# ---------------------------------
#  description  : returns list of hardcoded files
#  techniques   : subset thru logical index
#  status       : complete
#  CAUTION      : just submodel files as currently coded
# ---------------------------------

getStatics <- function()
{
  message("  info : getStatics : commencing")
  on.exit(message("  info : getStatics : complete (on.exit), file hits ", length(xems)))

  xems <- character()
  xems <- c(xems,  "experiment-01.xem")
  xems <- c(xems, "experiment-02_.xem")
  xems <- c(xems,      "test-asop.xem")
  xems <- c(xems,   "test-context.xem")
  xems <- c(xems,    "test-commod.xem")
  xems <- c(xems,    "test-domain.xem")
  xems <- c(xems,   "test-gateway.xem")
  xems <- c(xems,      "test-null.xem")
  xems <- c(xems,      "test-teas.xem")

  # report
  for (file in xems) message(file)

  # silent return
  invisible(xems)
}

# ---------------------------------
#  function : getXmoks
# ---------------------------------
#  description  : returns list of runnable xmok files
#  techniques   : subset thru logical index
#  status       : complete
#  CAUTION      : just submodel files as currently coded
# ---------------------------------

getXmoks <- function()
{
  message("  info : getXmoks : commencing")
  on.exit(message("  info : getXmoks : complete (on.exit), file hits ", length(filz)))

  # get list of files
  call.ls  <- "ls submodel.*.guard.xem"
  files    <- character()
  files    <- system(call.ls, intern = TRUE, ignore.stderr = TRUE)
  files    <- sub(".guard", "", files, fixed = TRUE)

  # get run status using 'awk'
  call.awk <- "awk '/script-run-me/ { print $4 }' submodel.*.guard.xem"
  runme    <- character()
  runme    <- system(call.awk, intern = TRUE, ignore.stderr = TRUE)
  runme    <- as.integer(runme)
  runme    <- as.logical(runme)

  # subset base in run status index
  filz   <<- character()
  filz    <- files[runme]

  # report
  for (file in filz) message(file)

  # silent return
  invisible(filz)
}

# ---------------------------------
#  function : getSubs
# ---------------------------------
#  description  : returns list of arbitrary submodels
#  techniques   : 'sprintf', hard-coded vector of ints
#  status       : complete
#  CAUTION      : file existence not tested
# ---------------------------------

getSubs <- function()
{
  message("  info : getSubs : commencing")
  on.exit(message("  info : getSubs : complete (on.exit), file hits ", length(files)))

  # hard-coded list of integers -- change as you like
  ints <- 22:25

  # "loop" the 'ints'
  files <- sprintf("submodel.%2d.xem", ints) # without ".guard" is correct

  # report
  for (file in files) message(file)

  # silent return
  invisible(files)
}

# ---------------------------------
#  function : getXems
# ---------------------------------
#  description  : grab xems in reverse chronological order
#  role         : used in 'gaias'
#  techniques   : 'system' 'grep'
#  status       : complete
# ---------------------------------

getXems <- function()
{
  message("  info : getXems : commencing")
  on.exit(message("  info : getXems : complete (on.exit), file hits ", length(files)))

  # equivalent bash code from 'xmok' script
  #   files=$(ls -rt $SDIR/*.$GUARD.$XEM) # C
  #   files=$(echo "$files" | grep --invert-match "_")

  call.ls <- "ls --reverse --sort=time *.guard.xem"
  files   <- system(call.ls, intern = TRUE, ignore.stderr = TRUE)
  files   <- sub(".guard", "", files, fixed = TRUE)
  ## BUGGY: files <- files[- grep("_", files)]
}

# ---------------------------------
#  function : listXems
# ---------------------------------
#  description  : list xems
#  role         : not used
#  techniques   : (nothing special)
#  status       : complete
# ---------------------------------

listXems <- function()
{
  line <- "  -------------------------------------------------\n"
  cat(line)
  files <- getXems()
  gmsg <- sprintf("  %02d    %30s", seq(length(files)), files)
  cat(gmsg, sep = "\n")                      # the newline sep avoids a 'for' loop
  cat(line)
}

# ---------------------------------
#  call
# ---------------------------------

# uncomment to automate
source("../../xeonar/userFuncs.R")
rundemo()

message("  file : rundemo.R : finishing")

#  $Id: rundemo.R 5986 2011-02-23 15:30:44Z robbie $
#  end of file

