#! /usr/bin/Rscript --vanilla

#  file-purpose     : ad-hoc post-processing
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Thu 08-Dec-2011 11:08 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Revision: 9237 $
#  $Date: 2012-04-07 15:05:07 +0200 (Sat, 07 Apr 2012) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/models/scripts/xanalyze.R $

# ----------------------------------------
# preamble
# ----------------------------------------

ret <- suppressPackageStartupMessages(expr = T)

library(tools)                               # standard library package
library(getopt)                              # straightforward custom installation

library(xem)                                 # 'xeona' XEM files (private library)
library(robbie)                              # private library

study   <- "trial-008"
xemext  <- "xem"

scenario.bound <- "j"                        # current last scenario leta

default <- list(color = 655)

# ---------------------------------
#  function : robbie.examine
# ---------------------------------
#  description  : runs several exploratory calls on the given object
#  status       : complete
# ---------------------------------

robbie.examine <- function (x)
{
  cat("structure:")
  str(x)                                     # compactly display structure of an object
  cat("class * mode (programmatic) * typeof (internal) * nchar (wide):\n")
  print(class(x))
  print(mode(x))                             # get the programmatic type or storage mode of an object
  print(typeof(x))                           # get internal type
  nchar(x)                                   # bytes or alternatively human-readable characters
  cat("summary:")
  print(summary(x))                          # summarize an object
}

# ---------------------------------
#  function : testMe
# ---------------------------------
#  description  : support for option '--test'
#  role         : basic call
#  status       : complete
# ---------------------------------

testMe <- function(path)                     # path usually "" or "./"
{
  rule <- "    ----------------------------------------------------------------------"

  message()
  message("RUNNING TESTS")

  cs <- seq(1, 3)                            # cases 1 thru 3

  rets  <- 0
  for (c in cs)
    {
      case <- sprintf("-%d", c)
      if ( opt$debug  ) case <- paste(case, "d", sep = "")
      if ( opt$keep   ) case <- paste(case, "k", sep = "")
      if ( opt$report ) case <- paste(case, "r", sep = "")
      call <- paste(path, script, " ", case, sep = "")
      ret  <- system(call)
      rets <- rets + ret
      message(call, " exit status : ", ret)
      message()
      message(rule)
      message()                              # needed only because some library load calls are not (yet) suppressed
    }
  message("COMBINED EXITS : ", rets)
  message()
  ifelse(rets == 0,
         quit(status = exit$success),
         quit(status = exit$failure))
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
  message()
  message("              usage: ", script, "   [opts]       run script (at least one option)")
  message("                     ", script, "  --test    -T  run tests (also supports --debug --keep --report: -Tdkr)")
  message("                     ", script, "  --help    -h  display this message and exit")
  message("    general options:  --cut <00:00>        -c  subset plots to [00,00) zero-based steps (omitted values seen as start and finish)")
  message("                      --debug              -d  add debug messages")
  message("                      --identify           -i  invoke 'identify' to record points (some cases only)")
  message("                      --keep               -k  use Tk dialog to prevent closure")
  message("                      --lines              -l  plot with lines (not points)")
  message("                      --report             -r  report useful information")
  message("                      --sleep <seconds>    -s  sleep for 'seconds' after each plot (some cases only)")
  message("                      --yeek <num>         -y  run arbitrary code during development")
  message("                      --zero               -z  zero the y-axis if necessary")
  message("   behavior options:  --export             -x  interactive export (silently overwrites existing files)")
  message("                      --eXport             -X  non-interactive export (silently overwrites existing files)")
  message("       case options:  --buildAnom          -1  explore scenario 'b' anomalous building demand issue")
  message("                      --subtotals <int>    -2  explore subtotals under code 'int' 0-4 (0 for all information)")
  message("                      --doubleCcgt         -3  explore CCGT units 1 and 2")
  message("                      --cashFlows          -4  analyze cash flows and unit costs for various facilities as text")
  message("                      --capacities         -5  analyze capacities for various facilities")
  message("                      --unitCosts          -6  analyze cash flows and unit costs for various facilities as org-mode table")
  message("                      --gridLosses         -7  analyze supply grid losses, also the LMP auction gain")
  message("            purpose: explore various issues using R")
  message("              notes: the export options under case 3 are a bit non-standard")
  message()
  quit(save = "no", status = exit.status, runLast = F)
}

# ---------------------------------
#  command-line processing
# ---------------------------------

# first define some exit codes
exit <- list(success = 0,
             failure = 1,
             usage   = 2,
             test    = 255)

# obtain script name
script <- robbie.getScriptName()

# recover command line args
clargs <- commandArgs(trailingOnly = T)
if ( length(clargs) == 0 )
  {
    message(script, ": arguments mandatory (try --help)")
    quit(status = exit$usage)
    clargs <- "(none)"                       # the above rule may change
  } else {
    clargs <- paste(clargs, sep = "", collapse = " ")
  }

# usage specification
spec = c(                                    # 'getopt' specification
  # special options
  'help'         , 'h', 0, "logical",
  'yeek'         , 'y', 1, "integer",
  # general options
  'color'        , 'C', 1, "integer",         # 1 is mandatory entry
  'cut'          , 'c', 1, "character",
  'debug'        , 'd', 0, "logical",
  'eXport'       , 'X', 0, "logical",
  'export'       , 'x', 0, "logical",
  'identify'     , 'i', 0, "logical",
  'keep'         , 'k', 0, "logical",
  'lines'        , 'l', 0, "logical",
  'report'       , 'r', 0, "logical",
  'sleep'        , 's', 1, "numeric",
  'zero'         , 'z', 0, "logical",
  # individual cases
  'buildAnom'    , '1', 0, "logical",
  'subtotals'    , '2', 1, "integer",
  'doubleCcgt'   , '3', 0, "logical",
  'cashFlows'    , '4', 0, "logical",
  'capacities'   , '5', 0, "logical",
  'unitCosts'    , '6', 0, "logical",
  'gridLosses'   , '7', 0, "logical",
  'fake'         , '0', 0, "logical")

# process command-line
opt <- getopt(spec = matrix(ncol = 4, data = spec, byrow = T))   # principal call

# help
if ( ! is.null(opt$help) ) displayUsage(script, exit$success)

# set some defaults
if ( is.null(opt$color)    ) opt$color    <- default$color  # defined in preamble
if ( is.null(opt$cut)      ) opt$cut      <- ":"            # was NA
if ( is.null(opt$debug)    ) opt$debug    <- F
if ( is.null(opt$eXport)   ) opt$eXport   <- F
if ( is.null(opt$export)   ) opt$export   <- F
if ( is.null(opt$identify) ) opt$identify <- F
if ( is.null(opt$keep)     ) opt$keep     <- F
if ( is.null(opt$lines)    ) opt$lines    <- F
if ( is.null(opt$report)   ) opt$report   <- F
if ( is.null(opt$sleep)    ) opt$sleep    <- 0
if ( is.null(opt$test)     ) opt$test     <- F
if ( is.null(opt$yeek)     ) opt$yeek     <- 0
if ( is.null(opt$zero)     ) opt$zero     <- F

# test
if ( opt$test ) testMe(path = "")

# dynamic terminal width for R reporting
width <- robbie.updateTerminalWidth()

# ensure the export trips align
if ( opt$eXport ) opt$export = T

# end of command-line processing reporting
if ( opt$report ) message()
if ( opt$report ) message("terminal width      : ", width)

# start timestamp
tic <- Sys.time()

# ==== active section ====================================================================

if ( opt$debug ) message()
basescen <- "+"
steps    <- xem.grab(paste(study, basescen, "entity.time-horizon.steps",    sep = "."), report = opt$debug)
interval <- xem.grab(paste(study, basescen, "entity.time-horizon.interval", sep = "."), report = opt$debug)
usedsvn  <- xem.grab(paste(study, basescen, "program.last-run.used-svn",    sep = "."), report = opt$debug)

# cut processing call, expands 'cut' while honoring 'steps'
cutdata <- xem.processCutArg(cut    = opt$cut,
                             steps  = steps,
                             report = opt$debug)
opt$cut <- cutdata$rrange
if ( opt$report ) message("cut (rranged)       : ", opt$cut)

# integrity report and choke
if ( cutdata$good == FALSE ) message()
if ( cutdata$good == FALSE ) message("caution             : 'xem.processCutArg' just returned fail, check 'cut' argument")
if ( cutdata$good == FALSE ) message("fatal               : ", exit$usage)
if ( cutdata$good == FALSE ) message()
if ( cutdata$good == FALSE ) quit(save = "no", status = exit$usage, runLast = F)

# ---------------------------------
#  function : dif
# ---------------------------------

dif <- function(fqf,
                sen1,
                sen2,
                trunc = NA)
{
  f1 <- xem.grab(paste(study, sen1, fqf, sep = "."), report = opt$debug)
  f2 <- xem.grab(paste(study, sen2, fqf, sep = "."), report = opt$debug)
  diff <- f1 - f2
  if ( ! is.na(trunc) ) diff <- diff[1:trunc]
  title    <- paste(sen1, "minus", sen2)
  subtitle <- paste(fqf )
  list(data = diff, title = title, subtitle = subtitle)
}

# ---------------------------------
#  function : getScenDescs
# ---------------------------------

getScenDescs <- function(report = FALSE)
  {
    end   <- which(letters == scenario.bound)
    letas <- c("+", letters[1:end])

    buffer <- list()
    for (leta in letas)
      {
        desc <- xem.grab(paste(study, leta, "program.study-description.scenario-name", sep = "."), want = "string", report = report)
        buffer[[leta]] <- desc
      }
    list(letas = letas,
         descs = buffer)
  }

#  collect the scenario descriptions

if ( opt$debug ) message()
temp <- getScenDescs(report = opt$debug)
scenario.letas        <- temp$letas
scenario.descriptions <- temp$descs

# ---------------------------------
#  function : summarizeTimeseries
# ---------------------------------

summarizeTimeseries <- function(timeseries,
                                interval = interval)
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

myplot <- function(timeseries,
                   title,
                   subtitle1,
                   study,
                   leta,
                   report   = FALSE,
                   identify = FALSE)
{
  # metadata
  desc1     <- scenario.descriptions[leta]
  desc2     <- paste(leta, desc1, sep = " = ")
  subtitle2 <- paste("scenario", desc2, sep = " ")
  model     <- paste(study, leta, xemext, sep = ".")
  interval  <- interval

  # plot type
  type <- ifelse(opt$lines, "l", "p")        # line either "h" or "l"

  # prepare timebase
  timebase <- 1:length(timeseries) - 1

  # truncate timeseries and timebase
  rowz       <- eval(parse(text = opt$cut))  # parse and evaluate the string form "01:01"
  timeseries <- timeseries[rowz]             # truncate
  timebase   <- timebase[rowz]

  # plot call
  pts <- xem.xplot(timebase    = timebase,
                   timeseries  = timeseries,
                   ylim        = NA,         # use fall-back version
                   grid        = FALSE,
                   title       = title,
                   subtitle1   = subtitle1,
                   subtitle2   = subtitle2,
                   yano        = "delta",
                   units       = "[W]",      # CAUTION: hardcoded
                   interval    = interval,
                   stub        = study,
                   leta        = leta,
                   script      = script,
                   model       = model,
                   svn         = usedsvn,
                   type        = type,
                   identify    = FALSE,
                   export      = opt$export,
                   exportClose = TRUE,
                   exportAuto  = opt$eXport)

  # report as required
  if ( report )
    {
      plotno <- sprintf("%02d", xem.plot.count)   # incremented by now!
      message()
      message("plot      : ", plotno)
      message("model     : ", model)
      message("subtitle  : ", subtitle1)
      message("report    : ", summarizeTimeseries(timeseries, interval))
    }
  if ( length(pts) )
    {
      message("values    : ", pts)           # if 'identify' used
    }
}

# ---------------------------------
#  function : summarizeElement
# ---------------------------------

summarizeElement <- function(timeseries,
                             step = 0,
                             msg  = "value")
{
  buffer <- character(0)
  buffer[1] <- paste("value     :", msg, "=", sep = " ")
  buffer[2] <- sprintf("%+.3e", timeseries[step + 1])
  buffer[3] <- paste("(", step, ")", sep = "")
  paste(buffer, collapse = " ")
}

# ---------------------------------
#  function : sayElement
# ---------------------------------

sayElement <- function(timeseries,
                       step   = 0,
                       msg    = "value",
                       report = FALSE)
{
  if ( ! is.character(msg) ) warning("expected string: ", msg)
  if ( report )
    {
      output <- summarizeElement(timeseries, step, msg[1])
      message(output)
    }
}

# ==== cases =============================================================================

# ---------------------------------
#  case 1 : building demand anomaly
# ---------------------------------

if ( ! is.null(opt$buildAnom) )
{
  message()
  message("BUILDING DEMAND ANOMALY")

  end   <- which(letters == "h")
  letas <- letters[1:end]

  if ( ! opt$report ) message()
  for (leta in letas)
    {
      diff <- dif(fqf   = "entity.teas-building-elec-f02.demands",
                  sen1  = leta,
                  sen2  = "+",
                  trunc = NA)

      myplot(timeseries = diff$data,
             title      = diff$title,
             subtitle1  = diff$subtitle,
             study      = study,
             leta       = leta,
             report     = opt$report,
             identify   = opt$identify)
    }

  # keep open if requested
  xem.keepOpen(extra = paste("script :", script), action = opt$keep)
}

# ---------------------------------
#  case 2 : omit high-level CCS domain
# ---------------------------------

if ( ! is.null(opt$subtotals) )
{
  message()
  message("CCS DOMAIN HIGH-LEVEL OMITTED")

  # background
  letas <- scenario.letas
  doms  <- letters                           # 'letters' is a built-in constant
  kinds <- c("subtotal-financial", "subtotal-greenhouse", "subtotal-nox", "subtotal-depletion", "subtotal-landuse")

  # truncate above as required
  option <- opt$subtotals                    # CAUTION: option choice set here
  if ( option == 0 )
    {
      focus <- "everything"
      # do nothing
    }
  else if ( option == 1 )
    {
      focus <- "CCS exploration"
      letas <- c("+", "e")                   # scenarios
      doms  <- c("a", "c")                   # domains
      kinds <- kinds[c(1, 2, 4)]             # cost kinds
    }
  else if ( option == 2 )
    {
      focus <- "far-from-load and building stock exploration"
      letas <- c("+", "b")                   # scenarios
      doms  <- c("a", "f")                   # domains
      kinds <- kinds[c(1, 2, 4)]             # cost kinds
    }
  else if ( option == 3 )
    {
      focus <- "reference energy system only"
      letas <- c("+")                        # scenarios
      kinds <- kinds[c(1, 2, 4)]             # cost kinds
    }
  else if ( option == 4 )
    {
      focus <- "none"
      letas <- c()                           # scenarios
      doms  <- c()                           # domains
      kinds <- kinds[c(1, 2, 4)]             # cost kinds
    }
  else
    {
      status <- exit$usage
      message()
      message("option not valid    : ", option)
      message("fatal               : ", status)
      message()
      quit(save = "no", status = status, runLast = F)
    }

  message()
  message("option    : ", option)
  message("focus     : ", focus)

  # reveal the domains
  message()
  domcodes <- character(0)
  domroles <- character(0)
  for (dom in letters)
    {
      leta <- "e"                            # CAUTION: scenario 'e' has only active domains (clearly fragile and better to loop all scenarios)
      fqf  <- paste("entity.domain-controller-", dom, ".", "domain-role", sep = "")
      xfqf <- paste(study, leta, fqf, sep = ".")
      domrole <- xem.grab(xfqf, want = "string", report = opt$debug)
      if ( is.na(domrole) ) break
      domcodes <- append(domcodes, dom)
      domroles <- append(domroles, domrole)
      if ( opt$report ) message("domains   : ", dom, " = ", domrole)
    }

  # report
  if ( opt$report )
    {
      message()
      cat("letas     :", letas)
      cat("\n")
      cat("doms      :", doms)
      cat("\n")
      cat("kinds     :", kinds)
      cat("\n")
      message()
      for (dom in doms)
        {
          role <- domroles[which(letters == dom)]
          message("domain    : ", dom, " = ", role)
        }
      message()
      for (leta in letas)
        {
          desc <- scenario.descriptions[leta]
          message("scenario  : ", leta, " = ", desc)
        }
      message()
    }

  # active code
  message("ordering : cost kind < domains < scenarios")
  message()
  for (kind in kinds)
    {
      for (domcode in domcodes)
        {
          values <- numeric(0)
          for (leta in letas)
            {
              domcon <- paste("entity.domain-controller", domcode, sep="-")
              name   <- paste(study, leta, domcon, kind, sep = ".")
              value  <- xem.grab(name, report = opt$debug)
              values <- append(values, value)
              message(sprintf("  %-70s %+.3e", name, value))
            }
          if ( option == 3 ) next            # only one value present
          delta  <- values[2] - values[1]
          message(sprintf("  %-70s %+.3e", "delta", delta))
          if ( opt$report || opt$debug )
            {
              change <- (values[2] - values[1]) / ((values[2] + values[1]) / 2.0)    # can produce div-by-zero errors
              if ( ! is.na(change) && ! is.nan(change) )
                {
                  if ( change == 0 ) message(sprintf("  %-70s %s %%", "change", " 0"))
                  else               message(sprintf("  %-70s %+.3g %%", "change", 100 * change))
                }
              else if ( is.nan(change) )
                {
                  message(sprintf("  %-70s %s", "change", " 0 %"))                   # could also be " --"
                }
              message()
            }
        }
    }
}

# ---------------------------------
#  case 3 : double CCGT
# ---------------------------------

# CAUTION: based on given relationship between 'leta' and '[no]capture'

if ( ! is.null(opt$doubleCcgt) )
{
  message()
  message("CCGT UNITS 1 AND 2")

  ymax.start <- 21
  letas      <- scenario.letas
  field      <- "fuel-demands"
  symbol     <- "p"                          # "l" for line not recommended

  if ( opt$yeek == 1 ) letas  <- c("+", "e") # YEEK 1: for development

  ylim.collect <- range(ymax.start)          # CAUTION: hack: should be set dynamically

  message()
  for (leta in letas)
    {
      firstplot <- TRUE
      for (unit in c("a07", "a08"))
        {
          ent  <- paste("entity.teas-ccgt-nocapture", unit, sep = "-")
          fqf  <- paste(ent, field, sep = ".")
          if ( leta == "e" ) fqf <- sub("nocapture", "capture", fqf)     # 'e' uses "capture"
          xfqf <- paste(study, leta, fqf, sep = ".")
          if ( opt$report ) message("xfqf                : ", xfqf)
          ts    <- xem.grab(xfqf, want = "data",    report = opt$debug)
          units <- xem.grab(xfqf, want = "measure", report = opt$debug)

          # report if problematic 'ts'
          if ( length(ts) == 1 && is.na(ts) )
            {
              message("timeseries is NA    : ", ts)
              message("check the field     : ", paste("xgrab", "--summary", xfqf, sep = " "))
              message()
            }

          # timebase
          tbase <- seq(length(ts)) - 1

          # y-axis limits
          ylim <- range(ts)
          if ( ylim[1] > 0 ) ylim[1] <- 0
          ylim.collect <- range(c(ylim.collect, ylim)) # CAUTION: enable at least some adaptive behavior
          ylim         <- ylim.collect                 # use this value

          ## ylim <- NA                           # use fall-back value, which is: range(ts) when plotted

          # truncate timeseries and timebase
          rowz  <- eval(parse(text = opt$cut))    # parse and evaluate the string form "01:01"
          ts    <- ts[rowz]                       # truncate
          tbase <- tbase[rowz]

          # plot
          if ( firstplot )
            {
              title     <- "CCGT units 1 and 2"
              subtitle1 <- paste(field, "overplotted", sep = " / ")
              subtitle2 <- paste("scenario", leta, " = ", scenario.descriptions[[leta]])

              model     <- paste(study, leta, xemext, sep = ".")
              usedsvn   <- xem.grab(paste(study, leta, "program.last-run.used-svn",    sep = "."), report = opt$debug)
              interval  <- interval

              # export filename generation
              export.name <- NA
              if ( opt$export )                          # also implied by 'opt$eXport'
                {
                  export.name <- xem.generateExportName(stub  = study,               # typically the study
                                                        leta  = leta,
                                                        svn   = usedsvn,             # typically the run svn
                                                        title = subtitle1,           # typically the given plot title
                                                        ext   = "svg",               # reflects the export format
                                                        ask   = ( ! opt$eXport) )    # 'F' for automatic filename generation
                  message("export name         : ", export.name)
                }

              # plot
              xem.plot(timebase    = tbase,  # CAUTION: not 'xem.xplot' because we are overplotting
                       timeseries  = ts,
                       ylim        = ylim,
                       grid        = F,
                       title       = title,
                       subtitle1   = subtitle1,
                       subtitle2   = subtitle2,
                       yano        = "",     # was "quantity"
                       units       = units,
                       interval    = interval,
                       script      = script,
                       model       = model,
                       svn         = usedsvn,
                       type        = symbol,
                       exportName  = export.name,
                       exportClose = F)      # overplotting is required, hence the client must later: dev.off()

              mtext("unit 1 is black, unit 2 is colored", side = 1, adj = 0.98, padj = 5, cex = 0.75)

            }
          else
            {
              # overplot color
              col = colors()[opt$color]
              # set some attributes adaptively
              len <- length(ts)
              pch <- 03                      # crosses
              pch <- 01                      # open circles
              cex <- if ( len < 1000 ) 1.0 else 0.7
              # plot
              if ( opt$sleep > 0 ) Sys.sleep(1)
              points(tbase, ts, type = symbol, pch = pch, col = col, cex = cex)
              cap <- if ( opt$identify ) identify(ts)
            }
          firstplot <- FALSE
        }
      if (   opt$eXport ) dev.off()
      if ( ! opt$eXport ) Sys.sleep(opt$sleep)
    }

  # final 'ymax' reporting
  ymax.final <- ylim.collect[2]
  ymax.delta <- ymax.final - ymax.start
  message()
  message("ymax final          : ", ymax.final)
  if ( ymax.delta == 0 ) message("ymax delta          : ", "no change")
  if ( ymax.delta != 0 ) message("ymax delta          : ", ymax.delta)

  # keep open if requested
  xem.keepOpen(extra = paste("script :", script), action = opt$keep)
}

# ---------------------------------
#  case 4 : cashflow calcs
# ---------------------------------

if ( ! is.null(opt$cashFlows) )
{
  message()
  message("CASHFLOWS AND PRICES")

  letas <- scenario.letas

  if ( opt$yeek == 1 ) letas  <- c("+", "e") # YEEK 1: for development

  message()
  message("study    : ", study)
  message("last     : ", scenario.bound)
  message("xemmext  : ", xemext)
  message("interval : ", interval)

  # CCGT FACILITY

  # scenario : +
  # mean per-period sale                          [GJ] :     2119
  # mean duty                                     [MW] :  588.729
  # mean (non-weighted) nodal price             [$/GJ] :   20.141
  # mean per-period revenue                        [$] :    46928
  # mean per-period gas purchases                  [$] :    38468
  # mean cost of generation -- + fuel           [$/GJ] :   18.150
  # mean cost of generation -- + variable       [$/GJ] :   20.842
  # mean cost of generation -- + fixed          [$/GJ] :   22.334
  # ratio of revenue to purchases                  [-] : 1.220

  message()
  message("item = CCGT facility")

  for (leta in letas)
    {
      message()
      message("scenario : ", leta)

      p   <- "entity.node-2-inj-elec-a02.nodal-prices"                     # [$/J]

      Q1  <- "entity.teas-ccgt-nocapture-a07.productions"                  # [W]
      Q2  <- "entity.teas-ccgt-nocapture-a08.productions"                  # [W]
      V1  <- "entity.teas-ccgt-nocapture-a07.variable-costs-financial"     # [$] (for entire interval)
      V2  <- "entity.teas-ccgt-nocapture-a08.variable-costs-financial"     # [$] (for entire interval)
      F1  <- "entity.teas-ccgt-nocapture-a07.fixed-costs-financial"        # [$] (for entire interval)
      F2  <- "entity.teas-ccgt-nocapture-a08.fixed-costs-financial"        # [$] (for entire interval)

      QG  <- "entity.gate-stated-tariff-ef-natgas-d01.quantitys"           # [W]
      G   <- "entity.gate-stated-tariff-ef-natgas-d01.total-costs"         # [$] (for entire interval)

      QS  <- "entity.gate-stated-tariff-cseq-c01.quantitys"                # [kg/s]
      S   <- "entity.gate-stated-tariff-cseq-c01.total-costs"              # [$] (for entire interval)

      if ( leta == "e" )                     # revise CCGT entity names for scenario 'e'
        {
          Q1 <- sub("nocapture", "capture", Q1, fixed = TRUE)
          Q2 <- sub("nocapture", "capture", Q2, fixed = TRUE)
          V1 <- sub("nocapture", "capture", V1, fixed = TRUE)
          V2 <- sub("nocapture", "capture", V2, fixed = TRUE)
          F1 <- sub("nocapture", "capture", F1, fixed = TRUE)
          F2 <- sub("nocapture", "capture", F2, fixed = TRUE)
        }

      ps  <- xem.grab(xfqf = paste(study, leta, p,  sep = "."), want = "data", report = opt$debug)
      Q1s <- xem.grab(xfqf = paste(study, leta, Q1, sep = "."), want = "data", report = opt$debug)
      Q2s <- xem.grab(xfqf = paste(study, leta, Q2, sep = "."), want = "data", report = opt$debug)
      V1s <- xem.grab(xfqf = paste(study, leta, V1, sep = "."), want = "data", report = opt$debug)
      V2s <- xem.grab(xfqf = paste(study, leta, V2, sep = "."), want = "data", report = opt$debug)
      F1s <- xem.grab(xfqf = paste(study, leta, F1, sep = "."), want = "data", report = opt$debug)
      F2s <- xem.grab(xfqf = paste(study, leta, F2, sep = "."), want = "data", report = opt$debug)
      Gs  <- xem.grab(xfqf = paste(study, leta, G,  sep = "."), want = "data", report = opt$debug)
      Ss  <- xem.grab(xfqf = paste(study, leta, S,  sep = "."), want = "data", report = opt$debug)

      Qs    <- Q1s + Q2s                               # combined duty
      Vs    <- V1s + V2s
      Fs    <- F1s + F2s
      Qmean <- mean(Qs)                                # mean combined duty
      Pmean <- interval * mean(ps * Qs)                # mean per-period revenue
      Gmean <- mean(Gs)                                # mean per-period gas costs
      Smean <- mean(Ss)                                # mean per-period carbon storage costs
      if ( is.na(Smean) ) Smean <- 0.0
      Vmean <- mean(Vs)
      Fmean <- mean(Fs)

      pmean <- mean(ps)                                # mean LMP price

      cog1  <- (Gmean + Smean)                 / (interval * Qmean)   # cost of generation -- + fuel
      cog2  <- (Gmean + Smean + Vmean)         / (interval * Qmean)   # cost of generation -- + variable
      cog3  <- (Gmean + Smean + Vmean + Fmean) / (interval * Qmean)   # cost of generation -- + fixed

      ratio <- Pmean / (Gmean + Smean)                 # revenue-to-fuel ratio

      message("mean per-period sale                          [GJ] : ", sprintf("%8.0f", (interval * Qmean) / 1.0e+09))
      message("mean duty                                     [MW] : ", sprintf("%8.3f",  Qmean / 1.0e+06))
      message("mean (non-weighted) nodal price             [$/GJ] : ", sprintf("%8.3f",  pmean * 1.0e+09))
      message("mean per-period revenue                        [$] : ", sprintf("%8.0f",  Pmean))
      message("mean per-period gas purchases                  [$] : ", sprintf("%8.0f",  Gmean))
      if ( leta == "e" ) message("mean per-period carbon storage purchases       [$] : ", sprintf("%8.0f",  Smean))
      message("mean cost of generation -- + fuel           [$/GJ] : ", sprintf("%8.3f",  cog1 * 1.0e+09))
      message("mean cost of generation -- + variable       [$/GJ] : ", sprintf("%8.3f",  cog2 * 1.0e+09))
      message("mean cost of generation -- + fixed          [$/GJ] : ", sprintf("%8.3f",  cog3 * 1.0e+09))
      message("ratio of revenue to purchases                  [-] : ", sprintf("%.3f",   ratio))
    }

  # MV GATEWAY

  message()
  message("item = medium-voltage gateway")

  # scenario : +
  # mean per-period sale                          [GJ] :      312
  # mean duty                                     [MW] :   86.599
  # mean (non-weighted) nodal price             [$/GJ] :   25.547
  # mean per-period nodal purchases                [$] :     9090
  # mean weighted-average contract price        [$/GJ] :   96.292
  # mean per-period contract sales                 [$] :    19781
  # ratio of sales to purchases                    [-] : 2.176

  for (leta in letas)
    {
      message()
      message("scenario : ", leta)

      p   <- "entity.node-2-xit-elec-a03.nodal-prices"                # [$/J]

      Q   <- "entity.gate-stated-tariff-elec-a01.quantitys"           # [W]
      C   <- "entity.gate-stated-tariff-elec-a01.total-costs"         # [$] (for entire interval)
      m   <- "entity.gate-stated-tariff-elec-a01.marginal-prices"     # [$/J]

      ps  <- xem.grab(xfqf = paste(study, leta, p, sep = "."), want = "data", report = opt$debug)
      Qs  <- xem.grab(xfqf = paste(study, leta, Q, sep = "."), want = "data", report = opt$debug)
      Cs  <- xem.grab(xfqf = paste(study, leta, C, sep = "."), want = "data", report = opt$debug)
      ms  <- xem.grab(xfqf = paste(study, leta, m, sep = "."), want = "data", report = opt$debug)  # not used further

      Pmean <- interval * mean(ps * Qs)      # mean per-period LMP purchases
      Cmean <- 1.0      * mean(Cs)           # mean per-period contract sales

      Qmean <- mean(Qs)

      pmean <- mean(ps)                      # mean LMP price
      cmean <- mean(Cs / Qs) / interval      # mean average contract price

      ratio <- Cmean / Pmean                 # sales-to-purchases ratio

      message("mean per-period sale                          [GJ] : ", sprintf("%8.0f", (interval * Qmean) / 1.0e+09))
      message("mean duty                                     [MW] : ", sprintf("%8.3f",  Qmean / 1.0e+06))
      message("mean (non-weighted) nodal price             [$/GJ] : ", sprintf("%8.3f",  pmean * 1.0e+09))
      message("mean per-period nodal purchases                [$] : ", sprintf("%8.0f",  Pmean))
      message("mean weighted-average contract price        [$/GJ] : ", sprintf("%8.3f",  cmean * 1.0e+09))
      message("mean per-period contract sales                 [$] : ", sprintf("%8.0f",  Cmean))
      message("ratio of sales to purchases                    [-] : ", sprintf("%.3f",   ratio))
    }
}

# ---------------------------------
#  case 5 : utility functions
# ---------------------------------

percentme <- function(value)
{
  if ( ! is.numeric(value) ) warning("given value not numeric: ", value)
  percent <- value * 100.0                                       # convert to percentage
  str <- sprintf("%4.1f", percent)
  buf <- paste("\\SI{", str, "}{\\percent}", sep = "")           # CAUTION: escape the back-slashes
# buf <- paste(str, "%", sep = "")
  if ( identical(all.equal(value, 0.0), TRUE) ) buf <- "0.0"     # CAUTION: "x == y" not recommended, see: help("==")
  if ( is.null(value) || is.na(value) )         buf <- ""        # CAUTION: short-circuit evaluated left to right
  return(buf)
}

insert.rule <- function()                    # no argument taken
{
  cat("  |-|\n")
}

insert.me <- function(buffer)                # inserts as list
{
  if ( ! is.character(buffer) ) warning("given buffer is not a string: ", tag)

  cat("  | ")
  cat(buffer, sep = " | ")
  cat(" |\n")
}

insert.1 <- function(tag,                    # facility tag
                     cap,                    # facility capacity
                     pro,                    # facility contribution timeseries
                     tot,                    # total capacity for class: source, gate, sink
                     con)                    # total contribution timeseries for ditto
{
  if ( ! is.character(tag) ) warning("given tag not a string: ", tag)

  buf <- character()
  buf <- append(buf, sprintf("%-30s", tag))
  buf <- append(buf, sprintf("%8.1f", cap / 1.0e+06))   # MW
  buf <- append(buf, percentme(cap / tot))
  buf <- append(buf, percentme(mean(pro) / cap))
  buf <- append(buf, percentme(mean(pro) / mean(con)))

  insert.me(buf)
}

# ---------------------------------
#  case 5 : capacity calcs
# ---------------------------------

if ( ! is.null(opt$capacities) )
{
  message()
  message("CAPACITIES AND UTILIZATIONS")

  letas <- scenario.letas
  letas <- c("+")                            # swap lines to control which scenarios are processed

  if ( opt$yeek == 1 ) letas  <- c("+", "e") # YEEK 1: for development

  message()
  message("study    : ", study)
  message("last     : ", scenario.bound)
  message("xemmext  : ", xemext)
  message("interval : ", interval)

  for (leta in letas)
    {
      message()
      message("scenario : ", leta)
      message()

      # TABLE START

      header  <- c("Facility", "Capacity [\\si{\\mega\\watt}]", "Capacity share", "Capacity factor", "Contribution")
      total   <- "\\textit{total}"

      insert.me(header)
      insert.rule()

      # SOURCES

      source.tot   <- 0.0                    # capacity total
      source.con   <- 0.0                    # contribution total

      ccgt         <- list()
      ccgt$tag     <- "CCGT facility"
      ccgt1.cap.id <- "entity.teas-ccgt-nocapture-a07.on-design-capacity"       # [W]
      ccgt1.pro.id <- "entity.teas-ccgt-nocapture-a07.productions"              # [W]
      ccgt2.cap.id <- "entity.teas-ccgt-nocapture-a08.on-design-capacity"       # [W]
      ccgt2.pro.id <- "entity.teas-ccgt-nocapture-a08.productions"              # [W]
      ccgt1.cap    <- xem.grab(xfqf = paste(study, leta, ccgt1.cap.id,  sep = "."), want = "data", report = opt$debug)
      ccgt1.pro    <- xem.grab(xfqf = paste(study, leta, ccgt1.pro.id,  sep = "."), want = "data", report = opt$debug)
      ccgt2.cap    <- xem.grab(xfqf = paste(study, leta, ccgt2.cap.id,  sep = "."), want = "data", report = opt$debug)
      ccgt2.pro    <- xem.grab(xfqf = paste(study, leta, ccgt2.pro.id,  sep = "."), want = "data", report = opt$debug)
      ccgt$cap     <- ccgt1.cap + ccgt2.cap
      ccgt$pro     <- ccgt1.pro + ccgt2.pro
      source.tot   <- source.tot + ccgt$cap
      source.con   <- source.con + ccgt$pro

      if ( opt$debug ) message("source.tot = ", source.tot)
      if ( opt$debug ) message("source.con = ", mean(source.con))

      hydro        <- list()
      hydro$tag    <- "Hydro scheme"
      hydro.cap.id <- "entity.teas-hydro-scheme-a10.generator-capacity"         # [W]
      hydro.pro.id <- "entity.teas-hydro-scheme-a10.actual-productions"         # [W]
      hydro$cap    <- xem.grab(xfqf = paste(study, leta, hydro.cap.id,  sep = "."), want = "data", report = opt$debug)
      hydro$pro    <- xem.grab(xfqf = paste(study, leta, hydro.pro.id,  sep = "."), want = "data", report = opt$debug)
      source.tot   <- source.tot + hydro$cap
      source.con   <- source.con + hydro$pro

      if ( opt$debug ) message("source.tot = ", source.tot)
      if ( opt$debug ) message("source.con = ", mean(source.con))

      wind1        <- list()
      wind1$tag    <- "Windfarm 1"
      wind1.cnt.id <- "entity.teas-windfarm-a09.count"                          # [-]
      wind1.rat.id <- "entity.teas-windfarm-a09.turbine-rating"                 # [W]
      wind1.pro.id <- "entity.teas-windfarm-a09.actual-productions"             # [W]
      wind1.cnt    <- xem.grab(xfqf = paste(study, leta, wind1.cnt.id,  sep = "."), want = "data", report = opt$debug)
      wind1.rat    <- xem.grab(xfqf = paste(study, leta, wind1.rat.id,  sep = "."), want = "data", report = opt$debug)
      wind1$pro    <- xem.grab(xfqf = paste(study, leta, wind1.pro.id,  sep = "."), want = "data", report = opt$debug)
      wind1$cap    <- wind1.cnt * wind1.rat
      source.tot   <- source.tot + wind1$cap
      source.con   <- source.con + wind1$pro

      if ( opt$debug ) message("source.tot = ", source.tot)
      if ( opt$debug ) message("source.con = ", mean(source.con))

      wind2        <- list()
      wind2$tag    <- "Windfarm 2"
      wind2.cnt.id <- "entity.teas-windfarm-e02.count"                          # [-]
      wind2.rat.id <- "entity.teas-windfarm-e02.turbine-rating"                 # [W]
      wind2.pro.id <- "entity.teas-windfarm-e02.actual-productions"             # [W]
      wind2.cnt    <- xem.grab(xfqf = paste(study, leta, wind2.cnt.id,  sep = "."), want = "data", report = opt$debug)
      wind2.rat    <- xem.grab(xfqf = paste(study, leta, wind2.rat.id,  sep = "."), want = "data", report = opt$debug)
      wind2$pro    <- xem.grab(xfqf = paste(study, leta, wind2.pro.id,  sep = "."), want = "data", report = opt$debug)
      wind2$cap    <- wind2.cnt * wind2.rat
      source.tot   <- source.tot + wind2$cap
      source.con   <- source.con + wind2$pro

      if ( opt$debug ) message("source.tot = ", source.tot)
      if ( opt$debug ) message("source.con = ", mean(source.con))

      photo        <- list()
      photo$tag    <- "Photovoltaic installation"
      photo.cap.id <- "entity.teas-pv-installation-f01.nameplate-capacity"      # [W]
      photo.pro.id <- "entity.teas-pv-installation-f01.actual-productions"      # [W]
      photo$cap    <- xem.grab(xfqf = paste(study, leta, photo.cap.id,  sep = "."), want = "data", report = opt$debug)
      photo$pro    <- xem.grab(xfqf = paste(study, leta, photo.pro.id,  sep = "."), want = "data", report = opt$debug)
      source.tot   <- source.tot + photo$cap
      source.con   <- source.con + photo$pro

      if ( opt$debug ) message("source.tot = ", source.tot)
      if ( opt$debug ) message("source.con = ", mean(source.con))

      # insertions

      sources <- "\\textbf{Sources}"

      insert.me(sources)
      insert.1(ccgt$tag,  ccgt$cap,  ccgt$pro,  source.tot, source.con)
      insert.1(hydro$tag, hydro$cap, hydro$pro, source.tot, source.con)
      insert.1(wind1$tag, wind1$cap, wind1$pro, source.tot, source.con)
      insert.1(wind2$tag, wind2$cap, wind2$pro, source.tot, source.con)
      insert.1(photo$tag, photo$cap, photo$pro, source.tot, source.con)
      insert.1(total, source.tot, NA, NA, NA)
      insert.rule()

      # GATEWAYS

      gateway.tot  <- 0.0                    # capacity total
      gateway.con  <- 0.0                    # contribution total

      gate1        <- list()
      gate1$tag    <- "\\Domain{a} \\rightarrow \\code{e}"
      gate1.cap.id <- "entity.gate-stated-tariff-elec-a01.defined-capacitys"    # [W]
      gate1.via.id <- "entity.gate-stated-tariff-elec-a01.quantitys"            # [W]
      gate1.cap    <- xem.grab(xfqf = paste(study, leta, gate1.cap.id,  sep = "."), want = "data", report = opt$debug)
      gate1$via    <- xem.grab(xfqf = paste(study, leta, gate1.via.id,  sep = "."), want = "data", report = opt$debug)
      gate1$cap    <- mean(gate1.cap)        # scalarize
      gateway.tot  <- gateway.tot + gate1$cap
      gateway.con  <- gateway.con + gate1$via

      if ( opt$debug ) message("gateway.tot = ", gateway.tot)
      if ( opt$debug ) message("gateway.con = ", mean(gateway.con))

      gate2        <- list()
      gate2$tag    <- "\\Domain{e} \\rightarrow \\code{f}"
      gate2.cap.id <- "entity.gate-stated-tariff-efac-elec-e01.defined-capacitys"    # [W]
      gate2.via.id <- "entity.gate-stated-tariff-efac-elec-e01.quantitys"            # [W]
      gate2.cap    <- xem.grab(xfqf = paste(study, leta, gate2.cap.id,  sep = "."), want = "data", report = opt$debug)
      gate2$via    <- xem.grab(xfqf = paste(study, leta, gate2.via.id,  sep = "."), want = "data", report = opt$debug)
      gate2$cap    <- mean(gate2.cap)        # scalarize
      gateway.tot  <- gateway.tot + gate2$cap
      gateway.con  <- gateway.con + gate2$via

      if ( opt$debug ) message("gateway.tot = ", gateway.tot)
      if ( opt$debug ) message("gateway.con = ", mean(gateway.con))

      # insertions -- these may be hand-edited later to remove inappropriate data

      gateways <- "\\textbf{Gateways}"

      insert.me(gateways)
      insert.1(gate1$tag, gate1$cap, gate1$via, gateway.tot, gateway.con)
      insert.1(gate2$tag, gate2$cap, gate2$via, gateway.tot, gateway.con)
      insert.1(total, gateway.tot, NA, NA, NA)
      insert.rule()

      # SINKS

      sink.tot        <- 0.0                 # capacity total
      sink.con        <- 0.0                 # contribution total

      smelter         <- list()
      smelter$tag     <- "Aluminium smelter"
      smelter.cap.id  <- "entity.teas-load-all-elec-b01.demand-hi-bound"    # [W]
      smelter.dem.id  <- "entity.teas-load-all-elec-b01.loads"              # [W]
      smelter$cap     <- xem.grab(xfqf = paste(study, leta, smelter.cap.id,  sep = "."), want = "data", report = opt$debug)
      smelter$dem     <- xem.grab(xfqf = paste(study, leta, smelter.dem.id,  sep = "."), want = "data", report = opt$debug)
      sink.tot        <- sink.tot + smelter$cap
      sink.con        <- sink.con + smelter$dem

      if ( opt$debug ) message("sink.tot = ", sink.tot)
      if ( opt$debug ) message("sink.con = ", mean(sink.con))

      flexload        <- list()
      flexload$tag    <- "Flexible load"
      flexload.cap.id <- "entity.teas-load-all-elec-a11.demand-hi-bound"    # [W]
      flexload.dem.id <- "entity.teas-load-all-elec-a11.loads"              # [W]
      flexload$cap    <- xem.grab(xfqf = paste(study, leta, flexload.cap.id,  sep = "."), want = "data", report = opt$debug)
      flexload$dem    <- xem.grab(xfqf = paste(study, leta, flexload.dem.id,  sep = "."), want = "data", report = opt$debug)
      sink.tot        <- sink.tot + flexload$cap
      sink.con        <- sink.con + flexload$dem

      if ( opt$debug ) message("sink.tot = ", sink.tot)
      if ( opt$debug ) message("sink.con = ", mean(sink.con))

      bilstock        <- list()
      bilstock$tag    <- "Building stock"
      bilstock.cnt.id <- "entity.teas-building-elec-f02.count"              # [-]
      bilstock.dhb.id <- "entity.teas-building-elec-f02.demand-hi-bound"    # [W]
      bilstock.dem.id <- "entity.teas-building-elec-f02.demands"            # [W]
      bilstock.cnt    <- xem.grab(xfqf = paste(study, leta, bilstock.cnt.id,  sep = "."), want = "data", report = opt$debug)
      bilstock.dhb    <- xem.grab(xfqf = paste(study, leta, bilstock.dhb.id,  sep = "."), want = "data", report = opt$debug)
      bilstock$dem    <- xem.grab(xfqf = paste(study, leta, bilstock.dem.id,  sep = "."), want = "data", report = opt$debug)
      bilstock$cap    <- bilstock.cnt * bilstock.dhb
      sink.tot        <- sink.tot + bilstock$cap
      sink.con        <- sink.con + bilstock$dem

      if ( opt$debug ) message("sink.tot = ", sink.tot)
      if ( opt$debug ) message("sink.con = ", mean(sink.con))

      # insertions

      sinks <- "\\textbf{Sinks}"

      insert.me(sinks)
      insert.1(smelter$tag,  smelter$cap,  smelter$dem,  sink.tot, sink.con)
      insert.1(flexload$tag, flexload$cap, flexload$dem, sink.tot, sink.con)
      insert.1(bilstock$tag, bilstock$cap, bilstock$dem, sink.tot, sink.con)
      insert.1(total, sink.tot, NA, NA, NA)
      insert.rule()
    }
}

# ---------------------------------
#  case 6 : utility functions
# ---------------------------------

sepfour <- function(val)
{
  if ( ! is.character(val) ) warning("given val not a string: ", val)

  num <- paste("\\num[sepfour=true]{", val, "}", sep = "")
  buf <- sprintf("%-28s", num)
}

insert.2 <- function(tag,
                     interval,               # interval length [s]
                     dutyMean,
                     cash.in,
                     cash.out,
                     var,
                     fix,
                     emb)
{
  if ( ! is.character(tag) ) warning("given tag not a string: ", tag)

  div      <- interval * dutyMean            # divisor for unit costs
  ratio    <- cash.in / cash.out
  factor   <- cash.out                       # cash out only goes to fuel
  direct   <- factor                         # definitional
  shortrun <- factor + var + fix             # 'fix' could be moved to 'longrun'
  longrun  <- shortrun + emb

  buf <- character()
  buf <- append(buf, sprintf("%-30s", tag))                           # CCGT facility
  if ( cash.in == 0.0 )
    {
      buf <- append(buf, "0.0")
    }
  else
    {
      buf <- append(buf, sepfour(sprintf("%7.3f", cash.in  / interval)))  # \num[sepfour=true]{46928}
    }
  if ( cash.out == 0.0 )
    {
      buf <- append(buf, "0.0")
    }
  else
    {
      buf <- append(buf, sepfour(sprintf("%7.3f", cash.out / interval)))  # \num[sepfour=true]{46928}
    }
  if ( cash.out == 0.0 )                                              # div-by-zero protection
    {
      buf <- append(buf, "--")
    }
  else
    {
      buf <- append(buf, sepfour(sprintf("%4.2f", ratio)))            # \num{1.22}
    }
  if ( dutyMean == 0.0 )                                              # div-by-zero protection
    {
      buf <- append(buf, "--")
      buf <- append(buf, "--")
      buf <- append(buf, "--")
    }
  else
    {
      buf <- append(buf, sepfour(sprintf("%4.1f", direct   / div)))   # \num{18.2}
      buf <- append(buf, sepfour(sprintf("%4.1f", shortrun / div)))   # \num{20.8}
      buf <- append(buf, sepfour(sprintf("%4.1f", longrun  / div)))   # \num{22.3}
    }

  insert.me(buf)
}

insert.3 <- function(tag,
                     interval,               # interval length [s]
                     cash.in,
                     cash.out)
{
  if ( ! is.character(tag) ) warning("given tag not a string: ", tag)

  ratio    <- cash.in / cash.out

  buf <- character()
  buf <- append(buf, sprintf("%-30s", tag))                           # CCGT facility
  if ( cash.in == 0.0 )
    {
      buf <- append(buf, "0.0")
    }
  else
    {
      buf <- append(buf, sepfour(sprintf("%7.3f", cash.in  / interval)))  # \num[sepfour=true]{46928}
    }
  if ( cash.out == 0.0 )
    {
      buf <- append(buf, "0.0")
    }
  else
    {
      buf <- append(buf, sepfour(sprintf("%7.3f", cash.out / interval)))  # \num[sepfour=true]{46928}
    }
  buf <- append(buf, sepfour(sprintf("%4.2f", ratio)))                # \num{1.22}
  buf <- append(buf, "--")
  buf <- append(buf, "--")
  buf <- append(buf, "--")

  insert.me(buf)
}

# ---------------------------------
#  case 6 : unit cost calcs
# ---------------------------------

if ( ! is.null(opt$unitCosts) )
{
  message()
  message("CASH FLOWS AND UNIT COSTS")

  letas <- scenario.letas
  letas <- c("+")                            # swap lines to control which scenarios are processed

  if ( opt$yeek == 1 ) letas  <- c("+", "e") # YEEK 1: for development

  message()
  message("study    : ", study)
  message("last     : ", scenario.bound)
  message("xemmext  : ", xemext)
  message("interval : ", interval)

  for (leta in letas)
    {
      message()
      message("scenario : ", leta)
      message()

      # TABLE START

      header   <- c("Facility",
                    "\\begin{sideways}Average cash in [\\si{\\NZD\\per\\second}]\\end{sideways}",
                    "\\begin{sideways}Average cash out [\\si{\\NZD\\per\\second}]\\end{sideways}",
                    "\\begin{sideways}Ratio [--]\\end{sideways}",
                    "\\begin{sideways}Direct (fuel-only) unit cost [\\si{\\NZD\\per\\giga\\joule}]\\end{sideways}",
                    "\\begin{sideways}Short-run unit cost [\\si{\\NZD\\per\\giga\\joule}]\\end{sideways}",
                    "\\begin{sideways}Long-run unit cost [\\si{\\NZD\\per\\giga\\joule}]\\end{sideways}")

      insert.me(header)

      # SOURCES

      padding  <- 0.0
      padding  <- 5.0                        # column one horizontal padding in "ems"

      ems      <- sprintf("%.1fem", padding)
      sources  <- "\\textbf{Sources}"
      sources  <- paste("\\textbf{Sources} \\hspace{", ems, "}", sep = "")

      insert.rule()
      insert.me(sources)

      # CCGT FACILITY

      ccgt.tag <-  "CCGT facility"

      p   <- "entity.node-2-inj-elec-a02.nodal-prices"                     # [$/J]

      Q1  <- "entity.teas-ccgt-nocapture-a07.productions"                  # [W]
      Q2  <- "entity.teas-ccgt-nocapture-a08.productions"                  # [W]
      V1  <- "entity.teas-ccgt-nocapture-a07.variable-costs-financial"     # [$] (for entire interval)
      V2  <- "entity.teas-ccgt-nocapture-a08.variable-costs-financial"     # [$] (for entire interval)
      F1  <- "entity.teas-ccgt-nocapture-a07.fixed-costs-financial"        # [$] (for entire interval)
      F2  <- "entity.teas-ccgt-nocapture-a08.fixed-costs-financial"        # [$] (for entire interval)
      E1  <- "entity.teas-ccgt-nocapture-a07.embedded-costs-financial"     # [$] (for entire interval)
      E2  <- "entity.teas-ccgt-nocapture-a08.embedded-costs-financial"     # [$] (for entire interval)

      QG  <- "entity.gate-stated-tariff-ef-natgas-d01.quantitys"           # [W]
      G   <- "entity.gate-stated-tariff-ef-natgas-d01.total-costs"         # [$] (for entire interval)

      QS  <- "entity.gate-stated-tariff-cseq-c01.quantitys"                # [kg/s]
      S   <- "entity.gate-stated-tariff-cseq-c01.total-costs"              # [$] (for entire interval)

      if ( leta == "e" )                     # revise CCGT entity names for scenario 'e'
        {
          Q1 <- sub("nocapture", "capture", Q1, fixed = TRUE)
          Q2 <- sub("nocapture", "capture", Q2, fixed = TRUE)
          V1 <- sub("nocapture", "capture", V1, fixed = TRUE)
          V2 <- sub("nocapture", "capture", V2, fixed = TRUE)
          F1 <- sub("nocapture", "capture", F1, fixed = TRUE)
          F2 <- sub("nocapture", "capture", F2, fixed = TRUE)
          E1 <- sub("nocapture", "capture", E1, fixed = TRUE)
          E2 <- sub("nocapture", "capture", E2, fixed = TRUE)
        }

      ps  <- xem.grab(xfqf = paste(study, leta, p,  sep = "."), want = "data", report = opt$debug)
      Q1s <- xem.grab(xfqf = paste(study, leta, Q1, sep = "."), want = "data", report = opt$debug)
      Q2s <- xem.grab(xfqf = paste(study, leta, Q2, sep = "."), want = "data", report = opt$debug)
      V1s <- xem.grab(xfqf = paste(study, leta, V1, sep = "."), want = "data", report = opt$debug)
      V2s <- xem.grab(xfqf = paste(study, leta, V2, sep = "."), want = "data", report = opt$debug)
      F1s <- xem.grab(xfqf = paste(study, leta, F1, sep = "."), want = "data", report = opt$debug)
      F2s <- xem.grab(xfqf = paste(study, leta, F2, sep = "."), want = "data", report = opt$debug)
      E1s <- xem.grab(xfqf = paste(study, leta, E1, sep = "."), want = "data", report = opt$debug)
      E2s <- xem.grab(xfqf = paste(study, leta, E2, sep = "."), want = "data", report = opt$debug)
      Gs  <- xem.grab(xfqf = paste(study, leta, G,  sep = "."), want = "data", report = opt$debug)
      Ss  <- xem.grab(xfqf = paste(study, leta, S,  sep = "."), want = "data", report = opt$debug)

      Qs    <- Q1s + Q2s                               # combined duty
      Vs    <- V1s + V2s
      Fs    <- F1s + F2s
      Es    <- E1s + E2s
      Qmean <- mean(Qs)                                # mean combined duty
      Pmean <- interval * mean(ps * Qs)                # mean per-period revenue
      Gmean <- mean(Gs)                                # mean per-period gas costs
      Smean <- mean(Ss)                                # mean per-period carbon storage costs
      if ( is.na(Smean) ) Smean <- 0.0
      Vmean <- mean(Vs)
      Fmean <- mean(Fs)
      Emean <- mean(Es)

      ratio <- Pmean / (Gmean + Smean)                 # revenue-to-fuel ratio

      insert.2(ccgt.tag,                     # tag
               interval,                     # interval
               Qmean / 1.0e+09,              # mean duty (joint in this case) in [GJ]
               Pmean,                        # cash in
               Gmean + Smean,                # cash out
               Vmean,                        # variable  (joint in this case)
               Fmean,                        # fixed     (joint in this case)
               Emean)                        # embedded  (joint in this case)

      if ( opt$debug )
        {
          pmean <- mean(ps)                                                          # mean LMP price

          cog1  <- (Gmean + Smean)                         / (interval * Qmean)      # cost of generation -- + fuel
          cog2  <- (Gmean + Smean + Vmean)                 / (interval * Qmean)      # cost of generation -- + variable
          cog3  <- (Gmean + Smean + Vmean + Fmean)         / (interval * Qmean)      # cost of generation -- + fixed
          cog4  <- (Gmean + Smean + Vmean + Fmean + Emean) / (interval * Qmean)      # cost of generation -- + embedded

          message("mean per-period sale                          [GJ] : ", sprintf("%8.0f", (interval * Qmean) / 1.0e+09))
          message("mean duty                                     [MW] : ", sprintf("%8.3f",  Qmean / 1.0e+06))
          message("mean (non-weighted) nodal price             [$/GJ] : ", sprintf("%8.3f",  pmean * 1.0e+09))
          message("mean per-period revenue                        [$] : ", sprintf("%8.0f",  Pmean))
          message("mean per-period gas purchases                  [$] : ", sprintf("%8.0f",  Gmean))
          if ( leta == "e" ) message("mean per-period carbon storage purchases       [$] : ", sprintf("%8.0f",  Smean))
          message("mean cost of generation -- + fuel           [$/GJ] : ", sprintf("%8.3f",  cog1 * 1.0e+09))
          message("mean cost of generation -- + variable       [$/GJ] : ", sprintf("%8.3f",  cog2 * 1.0e+09))
          message("mean cost of generation -- + fixed          [$/GJ] : ", sprintf("%8.3f",  cog3 * 1.0e+09))
          message("mean cost of generation -- + embedded       [$/GJ] : ", sprintf("%8.3f",  cog4 * 1.0e+09))
          message("ratio of revenue to purchases                  [-] : ", sprintf("%.3f",   ratio))
        }

      # HYDRO SCHEME

      hydro.tag <-  "Hydro scheme"

      p   <- "entity.node-2-inj-elec-a06.nodal-prices"                # [$/J]

      Q   <- "entity.teas-hydro-scheme-a10.actual-productions"        # [W]
      V   <- "entity.teas-hydro-scheme-a10.variable-costs-financial"  # [$] (for entire interval)
      F   <- "entity.teas-hydro-scheme-a10.fixed-costs-financial"     # [$] (for entire interval)
      E   <- "entity.teas-hydro-scheme-a10.embedded-costs-financial"  # [$] (for entire interval)

      ps  <- xem.grab(xfqf = paste(study, leta, p, sep = "."), want = "data", report = opt$debug)
      Qs  <- xem.grab(xfqf = paste(study, leta, Q, sep = "."), want = "data", report = opt$debug)
      Vs  <- xem.grab(xfqf = paste(study, leta, V, sep = "."), want = "data", report = opt$debug)
      Fs  <- xem.grab(xfqf = paste(study, leta, F, sep = "."), want = "data", report = opt$debug)
      Es  <- xem.grab(xfqf = paste(study, leta, E, sep = "."), want = "data", report = opt$debug)

      Qmean <- mean(Qs)                      # mean combined duty
      Pmean <- interval * mean(ps * Qs)      # mean per-period revenue
      Vmean <- mean(Vs)
      Fmean <- mean(Fs)
      Emean <- mean(Es)

      Cmean <- 0.0                           # no outgoings

      insert.2(hydro.tag,                    # tag
               interval,                     # interval
               Qmean / 1.0e+09,              # mean duty (joint in this case) in [GJ]
               Pmean,                        # cash in
               Cmean,                        # cash out
               Vmean,                        # variable  (joint in this case)
               Fmean,                        # fixed     (joint in this case)
               Emean)                        # embedded  (joint in this case)

      if ( opt$debug )
        {
          pmean <- mean(ps)                  # mean LMP price

          cog1  <- (Cmean)                         / (interval * Qmean)    # cost of generation -- + fuel
          cog2  <- (Cmean + Vmean)                 / (interval * Qmean)    # cost of generation -- + variable
          cog3  <- (Cmean + Vmean + Fmean)         / (interval * Qmean)    # cost of generation -- + fixed
          cog4  <- (Cmean + Vmean + Fmean + Emean) / (interval * Qmean)    # cost of generation -- + embedded

          message("mean per-period sale                          [GJ] : ", sprintf("%8.0f", (interval * Qmean) / 1.0e+09))
          message("mean duty                                     [MW] : ", sprintf("%8.3f",  Qmean / 1.0e+06))
          message("mean (non-weighted) nodal price             [$/GJ] : ", sprintf("%8.3f",  pmean * 1.0e+09))
          message("mean per-period revenue                        [$] : ", sprintf("%8.0f",  Pmean))
          message("mean cost of generation -- + fuel           [$/GJ] : ", sprintf("%8.3f",  cog1 * 1.0e+09))
          message("mean cost of generation -- + variable       [$/GJ] : ", sprintf("%8.3f",  cog2 * 1.0e+09))
          message("mean cost of generation -- + fixed          [$/GJ] : ", sprintf("%8.3f",  cog3 * 1.0e+09))
          message("mean cost of generation -- + embedded       [$/GJ] : ", sprintf("%8.3f",  cog4 * 1.0e+09))
          message("ratio of revenue to purchases                  [-] : ", sprintf("%.3f",   ratio))
        }

      # WINDFARM 1

      wind.tag <-  "Windfarm 1"

      p   <- "entity.node-2-inj-elec-a04.nodal-prices"                # [$/J]

      Q   <- "entity.teas-windfarm-a09.actual-productions"            # [W]
      V   <- "entity.teas-windfarm-a09.variable-costs-financial"      # [$] (for entire interval)
      F   <- "entity.teas-windfarm-a09.fixed-costs-financial"         # [$] (for entire interval)
      E   <- "entity.teas-windfarm-a09.embedded-costs-financial"      # [$] (for entire interval)

      ps  <- xem.grab(xfqf = paste(study, leta, p, sep = "."), want = "data", report = opt$debug)
      Qs  <- xem.grab(xfqf = paste(study, leta, Q, sep = "."), want = "data", report = opt$debug)
      Vs  <- xem.grab(xfqf = paste(study, leta, V, sep = "."), want = "data", report = opt$debug)
      Fs  <- xem.grab(xfqf = paste(study, leta, F, sep = "."), want = "data", report = opt$debug)
      Es  <- xem.grab(xfqf = paste(study, leta, E, sep = "."), want = "data", report = opt$debug)

      Qmean <- mean(Qs)                      # mean combined duty
      Pmean <- interval * mean(ps * Qs)      # mean per-period revenue
      Vmean <- mean(Vs)
      Fmean <- mean(Fs)
      Emean <- mean(Es)

      Cmean <- 0.0                           # no outgoings

      insert.2(wind.tag,                     # tag
               interval,                     # interval
               Qmean / 1.0e+09,              # mean duty (joint in this case) in [GJ]
               Pmean,                        # cash in
               Cmean,                        # cash out
               Vmean,                        # variable  (joint in this case)
               Fmean,                        # fixed     (joint in this case)
               Emean)                        # embedded  (joint in this case)

      if ( opt$debug )
        {
          pmean <- mean(ps)                  # mean LMP price

          cog1  <- (Cmean)                         / (interval * Qmean)    # cost of generation -- + fuel
          cog2  <- (Cmean + Vmean)                 / (interval * Qmean)    # cost of generation -- + variable
          cog3  <- (Cmean + Vmean + Fmean)         / (interval * Qmean)    # cost of generation -- + fixed
          cog4  <- (Cmean + Vmean + Fmean + Emean) / (interval * Qmean)    # cost of generation -- + embedded

          message("mean per-period sale                          [GJ] : ", sprintf("%8.0f", (interval * Qmean) / 1.0e+09))
          message("mean duty                                     [MW] : ", sprintf("%8.3f",  Qmean / 1.0e+06))
          message("mean (non-weighted) nodal price             [$/GJ] : ", sprintf("%8.3f",  pmean * 1.0e+09))
          message("mean per-period revenue                        [$] : ", sprintf("%8.0f",  Pmean))
          message("mean cost of generation -- + fuel           [$/GJ] : ", sprintf("%8.3f",  cog1 * 1.0e+09))
          message("mean cost of generation -- + variable       [$/GJ] : ", sprintf("%8.3f",  cog2 * 1.0e+09))
          message("mean cost of generation -- + fixed          [$/GJ] : ", sprintf("%8.3f",  cog3 * 1.0e+09))
          message("mean cost of generation -- + embedded       [$/GJ] : ", sprintf("%8.3f",  cog4 * 1.0e+09))
          message("ratio of revenue to purchases                  [-] : ", sprintf("%.3f",   ratio))
        }

      # GATEWAYS

      gateways <- "\\textbf{Gateways}"

      insert.rule()
      insert.me(gateways)

      # GATE 1

      gate1.tag <- "\\Domain{a} \\rightarrow \\code{b}"

      p   <- "entity.node-1-xib-elec-a07.nodal-prices"                # [$/J]

      Q   <- "entity.gate-stated-tariff-elec-a02.quantitys"           # [W]
      C   <- "entity.gate-stated-tariff-elec-a02.total-costs"         # [$] (for entire interval)
      m   <- "entity.gate-stated-tariff-elec-a02.marginal-prices"     # [$/J]

      ps  <- xem.grab(xfqf = paste(study, leta, p, sep = "."), want = "data", report = opt$debug)
      Qs  <- xem.grab(xfqf = paste(study, leta, Q, sep = "."), want = "data", report = opt$debug)
      Cs  <- xem.grab(xfqf = paste(study, leta, C, sep = "."), want = "data", report = opt$debug)
      ms  <- xem.grab(xfqf = paste(study, leta, m, sep = "."), want = "data", report = opt$debug)  # not used further

      Pmean <- interval * mean(ps * Qs)      # mean per-period LMP purchases
      Cmean <- 1.0      * mean(Cs)           # mean per-period contract sales

      Qmean <- mean(Qs)

      pmean <- mean(ps)                      # mean LMP price
      cmean <- mean(Cs / Qs) / interval      # mean average contract price

      insert.3(gate1.tag,                    # tag
               interval,
               Cmean,                        # cash in
               Pmean)                        # cash out

      if ( opt$debug )
        {
          ratio <- Cmean / Pmean             # sales-to-purchases ratio

          message("mean per-period sale                          [GJ] : ", sprintf("%8.0f", (interval * Qmean) / 1.0e+09))
          message("mean duty                                     [MW] : ", sprintf("%8.3f",  Qmean / 1.0e+06))
          message("mean (non-weighted) nodal price             [$/GJ] : ", sprintf("%8.3f",  pmean * 1.0e+09))
          message("mean per-period nodal purchases                [$] : ", sprintf("%8.0f",  Pmean))
          message("mean weighted-average contract price        [$/GJ] : ", sprintf("%8.3f",  cmean * 1.0e+09))
          message("mean per-period contract sales                 [$] : ", sprintf("%8.0f",  Cmean))
          message("ratio of sales to purchases                    [-] : ", sprintf("%.3f",   ratio))
        }

      # GATE 2

      gate2.tag <- "\\Domain{a} \\rightarrow \\code{e}"

      p   <- "entity.node-2-xit-elec-a03.nodal-prices"                # [$/J]

      Q   <- "entity.gate-stated-tariff-elec-a01.quantitys"           # [W]
      C   <- "entity.gate-stated-tariff-elec-a01.total-costs"         # [$] (for entire interval)
      m   <- "entity.gate-stated-tariff-elec-a01.marginal-prices"     # [$/J]

      ps  <- xem.grab(xfqf = paste(study, leta, p, sep = "."), want = "data", report = opt$debug)
      Qs  <- xem.grab(xfqf = paste(study, leta, Q, sep = "."), want = "data", report = opt$debug)
      Cs  <- xem.grab(xfqf = paste(study, leta, C, sep = "."), want = "data", report = opt$debug)
      ms  <- xem.grab(xfqf = paste(study, leta, m, sep = "."), want = "data", report = opt$debug)  # not used further

      Pmean <- interval * mean(ps * Qs)      # mean per-period LMP purchases
      Cmean <- 1.0      * mean(Cs)           # mean per-period contract sales

      Qmean <- mean(Qs)

      pmean <- mean(ps)                      # mean LMP price
      cmean <- mean(Cs / Qs) / interval      # mean average contract price

      insert.3(gate2.tag,                    # tag
               interval,
               Cmean,                        # cash in
               Pmean)                        # cash out

      if ( opt$debug )
        {
          ratio <- Cmean / Pmean             # sales-to-purchases ratio

          message("mean per-period sale                          [GJ] : ", sprintf("%8.0f", (interval * Qmean) / 1.0e+09))
          message("mean duty                                     [MW] : ", sprintf("%8.3f",  Qmean / 1.0e+06))
          message("mean (non-weighted) nodal price             [$/GJ] : ", sprintf("%8.3f",  pmean * 1.0e+09))
          message("mean per-period nodal purchases                [$] : ", sprintf("%8.0f",  Pmean))
          message("mean weighted-average contract price        [$/GJ] : ", sprintf("%8.3f",  cmean * 1.0e+09))
          message("mean per-period contract sales                 [$] : ", sprintf("%8.0f",  Cmean))
          message("ratio of sales to purchases                    [-] : ", sprintf("%.3f",   ratio))
        }

      # END

      insert.rule()

    }
}

# ---------------------------------
#  case 7 : utility functions
# ---------------------------------

# | Grid | Domain | Units | Injection | Exit | Loss | Comment |

insert.4 <- function(grid)
{
  if ( ! is.character(grid$tag) ) warning("given tag not a string: ", grid$tag)

  los <- (grid$inj - grid$xit ) / grid$inj

  buf <- character()
  buf <- append(buf, sprintf("%-40s", grid$tag))                      # identifier
  buf <- append(buf, paste("\\code{", grid$domain, "}", sep = ""))    # domain
  buf <- append(buf, paste("\\si{", grid$units, "}", sep = ""))       # units
  buf <- append(buf, sepfour(sprintf("%.1f", grid$inj)))             # \num[sepfour=true]{46928}
  buf <- append(buf, sepfour(sprintf("%.1f", grid$xit)))             # \num[sepfour=true]{46928}
  if ( grid$inj == 0.0 )                                              # div-by-zero protection
    {
      buf <- append(buf, "--")
    }
  else
    {
      loss <- sprintf("%4.1f", los * 100.0)
      buf  <- append(buf, paste("\\SI{", loss, "}{\\percent}", sep = ""))  # \SI{1.22}{\percent}
    }
  buf <- append(buf, sprintf("%-30s", grid$comment))                  # comment

  insert.me(buf)
}

# ---------------------------------
#  case 7 : grid loss calcs
# ---------------------------------

if ( ! is.null(opt$gridLosses) )
{
  message()
  message("GRID LOSSES AND LMP AUCTION GAINS")

  letas <- scenario.letas
  letas <- c("+")                            # swap lines to control which scenarios are processed

  if ( opt$yeek == 1 ) letas  <- c("+", "e") # YEEK 1: for development

  message()
  message("study    : ", study)
  message("last     : ", scenario.bound)
  message("xemmext  : ", xemext)
  message("interval : ", interval)

  for (leta in letas)
    {
      message()
      message("scenario : ", leta)
      message()

      # TABLE START

      header.grids    <- c("Grid",    "Domain", "Units", "Injections", "Exits", "Losses", "Comment")
      header.auctions <- c("Auction", "Domain", "Units", "Purchases",  "Sales", "Losses", "Comment")
      spacer          <- c("\\bigskip")      # between the two tables

      insert.me(header.grids)
      insert.rule()

      # GRIDS

      # 220KV

      hvgrid         <- list()
      hvgrid$tag     <- "Electricity --- \\SI{220}{\\kilo\\volt}"
      hvgrid$domain  <- "a"
      hvgrid$units   <- "\\mega\\watt"
      hvgrid$comment <- "enhanced DC load flow"

      p1   <- "entity.node-2-xit-elec-a01.nodal-prices"          # [$/J]
      Q1   <- "entity.teas-load-all-elec-a11.loads"              # [W]

      p2   <- "entity.node-2-inj-elec-a02.nodal-prices"          # [$/J]
      Q21  <- "entity.teas-ccgt-nocapture-a07.productions"       # [W]
      Q22  <- "entity.teas-ccgt-nocapture-a08.productions"       # [W]

      p3   <- "entity.node-2-xit-elec-a03.nodal-prices"          # [$/J]
      Q3   <- "entity.gate-stated-tariff-elec-a01.quantitys"     # [W]

      p4   <- "entity.node-2-inj-elec-a04.nodal-prices"          # [$/J]
      Q4   <- "entity.teas-windfarm-a09.actual-productions"      # [W]

      p6   <- "entity.node-2-inj-elec-a06.nodal-prices"          # [$/J]
      Q6   <- "entity.teas-hydro-scheme-a10.actual-productions"  # [W]

      p7   <- "entity.node-1-xib-elec-a07.nodal-prices"          # [$/J]
      Q7   <- "entity.gate-stated-tariff-elec-a02.quantitys"     # [W]

      if ( leta == "e" )                     # revise CCGT entity names for scenario 'e'
        {
          Q21 <- sub("nocapture", "capture", Q21, fixed = TRUE)
          Q22 <- sub("nocapture", "capture", Q22, fixed = TRUE)
        }

      p1s  <- xem.grab(xfqf = paste(study, leta, p1,  sep = "."), want = "data", report = opt$debug)
      p2s  <- xem.grab(xfqf = paste(study, leta, p2,  sep = "."), want = "data", report = opt$debug)
      p3s  <- xem.grab(xfqf = paste(study, leta, p3,  sep = "."), want = "data", report = opt$debug)
      p4s  <- xem.grab(xfqf = paste(study, leta, p4,  sep = "."), want = "data", report = opt$debug)
      p6s  <- xem.grab(xfqf = paste(study, leta, p6,  sep = "."), want = "data", report = opt$debug)
      p7s  <- xem.grab(xfqf = paste(study, leta, p7,  sep = "."), want = "data", report = opt$debug)

      Q1s  <- xem.grab(xfqf = paste(study, leta, Q1,  sep = "."), want = "data", report = opt$debug)
      Q21s <- xem.grab(xfqf = paste(study, leta, Q21, sep = "."), want = "data", report = opt$debug)
      Q22s <- xem.grab(xfqf = paste(study, leta, Q22, sep = "."), want = "data", report = opt$debug)
      Q3s  <- xem.grab(xfqf = paste(study, leta, Q3,  sep = "."), want = "data", report = opt$debug)
      Q4s  <- xem.grab(xfqf = paste(study, leta, Q4,  sep = "."), want = "data", report = opt$debug)
      Q6s  <- xem.grab(xfqf = paste(study, leta, Q6,  sep = "."), want = "data", report = opt$debug)
      Q7s  <- xem.grab(xfqf = paste(study, leta, Q7,  sep = "."), want = "data", report = opt$debug)

      Q2s  <- Q21s + Q22s

      P1   <- mean(p1s * Q1s)                # used by the auction section
      P2   <- mean(p2s * Q2s)                # used by the auction section
      P3   <- mean(p3s * Q3s)                # used by the auction section
      P4   <- mean(p4s * Q4s)                # used by the auction section
      P6   <- mean(p6s * Q6s)                # used by the auction section
      P7   <- mean(p7s * Q7s)                # used by the auction section

      hvgrid$inj     <- mean(Q2s) + mean(Q4s) + mean(Q6s)
      hvgrid$xit     <- mean(Q1s) + mean(Q3s) + mean(Q7s)

      hvgrid$inj     <- hvgrid$inj / 1.0e+06
      hvgrid$xit     <- hvgrid$xit / 1.0e+06

      insert.4(hvgrid)

      # 11KV

      mvgrid         <- list()
      mvgrid$tag     <- "Electricity --- \\SI{11}{\\kilo\\volt}"
      mvgrid$domain  <- "e"
      mvgrid$units   <- "\\mega\\watt"
      mvgrid$comment <- "includes both substations"

      Qa   <- "entity.gate-stated-tariff-elec-a01.quantitys"
      Qf   <- "entity.gate-stated-tariff-efac-elec-e01.quantitys"
      Qw   <- "entity.teas-windfarm-e02.actual-productions"

      Qas  <- xem.grab(xfqf = paste(study, leta, Qa,  sep = "."), want = "data", report = opt$debug)
      Qfs  <- xem.grab(xfqf = paste(study, leta, Qf,  sep = "."), want = "data", report = opt$debug)
      Qws  <- xem.grab(xfqf = paste(study, leta, Qw,  sep = "."), want = "data", report = opt$debug)

      mvgrid$inj <- mean(Qas) + mean(Qws)
      mvgrid$xit <- mean(Qfs)

      mvgrid$inj <- mvgrid$inj / 1.0e+06
      mvgrid$xit <- mvgrid$xit / 1.0e+06

      insert.4(mvgrid)

      # NATURAL GAS

      natgas         <- list()
      natgas$tag     <- "Natural gas"
      natgas$domain  <- "d"
      natgas$units   <- "\\kilo\\gram\\per\\second"
      natgas$comment <- "compressors and leakage"

      Qi  <- "entity.teas-pipeline-gas-d02.inputs"
      Qo  <- "entity.teas-pipeline-gas-d02.outputs"

      Qis <- xem.grab(xfqf = paste(study, leta, Qi,  sep = "."), want = "data", report = opt$debug)
      Qos <- xem.grab(xfqf = paste(study, leta, Qo,  sep = "."), want = "data", report = opt$debug)

      natgas$inj <- mean(Qis)
      natgas$xit <- mean(Qos)

      insert.4(natgas)

      # AUCTIONS

      insert.rule()
      insert.me(spacer)
      insert.me(header.auctions)
      insert.rule()

      # LMP AUCTION

      lmp         <- list()
      lmp$tag     <- "LMP auction"
      lmp$domain  <- "a"
      lmp$units   <- "\\NZD\\per\\second"
      lmp$comment <- "negative value means gain"

      lmp$inj <- P2 + P4 + P6
      lmp$xit <- P1 + P3 + P7

      insert.4(lmp)

      # END

      insert.rule()
    }
}

# ==== ending ============================================================================

# ---------------------------------
#  finishing up
# ---------------------------------

if ( is.null(opt$subtotals) && opt$report )
  {
    settings <- xem.plot.settings()
    message()
    message(settings)
    message("color               : ", opt$color, " = ", colors()[opt$color])
    message("cut                 : ", opt$cut)
    if ( opt$zero   ) message("zero                : ", "yes")
  }

# ----------------------------------------
# housekeeping
# ----------------------------------------

toc <- Sys.time() - tic

message()
message("command-line        : ", script, " ", clargs)
message("elapsed time (secs) : ", sprintf("%.0f", toc))
message()

quit(status = exit$success)

#  $Id: xanalyze.R 9237 2012-04-07 13:05:07Z robbie $
#  end of file

