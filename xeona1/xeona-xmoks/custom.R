
#  file-purpose     : manage 'xem.R' calls for single harded-coded XEM file
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Wed 14-Jul-2010 20:01 UTC
#  file-status      : ongoing
#  file-keywords    : xeona R

#  $Revision: 5692 $
#  $Date: 2011-01-21 18:41:56 +0100 (Fri, 21 Jan 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/xeona-xmoks/custom.R $

#  USAGE: source("custom.R")

message("  file : custom.R : starting")

# ---------------------------------
#  specify parameters
# ---------------------------------

xeona   <- "../xeona.mach"              # binary call, sufficiently specified or on system 'PATH'
jobs    <- 31                           # NULL means use XEM file 'program.r-processing.r-policy'
jobs    <- NULL                         # NULL means use XEM file 'program.r-processing.r-policy'
report  <- 6                            # report level 2 is warning

# CAUTION: 'xemfile.guard.xem' must be present

xemfile <- "submodel.16"
xemfile <- "inbuilt"

# ---------------------------------
#  function : custom
# ---------------------------------
#  description  : hardcoded custom call
#  techniques   : 'switch'
#  status       : ongoing
# ---------------------------------

custom <- function ()
{
  # reporting
  message("  info : custom : commencing")
  on.exit(message("  info : custom : complete (on.exit) using xemfile = \"", xemfile, "\""))

  # source R files
  source(file.path(Sys.getenv("XEONAR"), "xem.R"))          # essential
  source(file.path(Sys.getenv("XEONAR"), "userFuncs.R"))    # essential

  # clean up
  cleanup()

  # process file name
  xemfile  <- sub("(|\\.xem)$"        ,       ".xem", xemfile)   # add ".xem" extension if absent
  xemguard <- sub("(|\\.guard)\\.xem$", ".guard.xem", xemfile)   # guard file name if not already

  # confirm guard file
  if ( ! file.exists(xemguard)  ) {
    # warn
    message("  warn : xem.xemconf : xem guard file not present: ", xemguard)
    warning("xem guard file not found ", xemguard)
  }
  else {
    # call job, function 'xem.job' defined in 'xem.R'
    time <- system.time(mode <- xem.job(xemfile = xemfile,
                                        xeona   = xeona,
                                        dbug    = 0,          # default is 0
                                        jobs    = jobs,
                                        snooze  = 1,          # default is 0
                                        report  = report))

    # elapsed time (index 3)
    elapsed <- round(time[3], 0)
    message("  info : custom : elapsed time: ", elapsed, "s")

    # report mode
    message("  info : custom : best mode: ", mode)
  }

  # return
  invisible(mode)
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
  robbie.killX11s()                          # destroys all open R plot frames
  killcall <- "killall --quiet display"
  system(killcall)                           # destroys all open ImageMagick 'display' windows
  message("  system call: ", killcall)
}

# ---------------------------------
#  call
# ---------------------------------

custom()                                     # run automatically

# ---------------------------------
#  housekeeping
# ---------------------------------

message("  file : custom.R : finishing")

#  $Id: custom.R 5692 2011-01-21 17:41:56Z robbie $
#  end of file

#  $Id: custom.R 5692 2011-01-21 17:41:56Z robbie $
#  end of file

