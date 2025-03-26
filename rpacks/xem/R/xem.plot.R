
#---------------------------------
#  preamble : settings
# ---------------------------------

xem.plot.x11 <- list(width.in  = 13.5,       # default 7 inch
                     height.in =  7.5)       # default 7 inch

xem.plot.svg <- list(width.mm  =  160,
                     height.mm =  110,       # was 140 then 130 then 120
                     points.pt =    9)       # default 12

xem.plot.settings <- function()
{
  buf <- character(0)
  buf <- c(buf, sprintf("%-19s : %4g\n", "x11 width (in)"  , xem.plot.x11$width.in ))
  buf <- c(buf, sprintf("%-19s : %4g\n", "x11 height (in)" , xem.plot.x11$height.in))
  buf <- c(buf, sprintf("%-19s : %4g\n", "svg width (mm)"  , xem.plot.svg$width.mm))
  buf <- c(buf, sprintf("%-19s : %4g\n", "svg height (mm)" , xem.plot.svg$height.mm))
  buf <- c(buf, sprintf("%-19s : %4g"  , "svg points (pt)" , xem.plot.svg$points.pt))
  invisible(buf)
}

#---------------------------------
#  function : xem.plot
# ---------------------------------
#  description  : main plot call
#  role         : utility
#  status       : complete
# ---------------------------------

xem.plot.count <- 0                          # for labeling plot windows

xem.plot <- function(timebase,
                     timeseries,             # class 'numeric' expected
                     ylim        = NA,       # 'xlim' implicit in 'timebase'
                     grid        = FALSE,    # add grid
                     title,                  # main title
                     subtitle1,              # first subtitle
                     subtitle2,              # second subtitle
                     yano,                   # y-axis annotation
                     units,                  # units of measurement
                     interval,               # horizon interval
                     script      = NA,       # calling script name
                     model       = NA,       # xem model filename for side note
                     svn         = NA,       # xem model svn for side note
                     type        = "p",      # plot type
                     exportName  = NA,       # filename, will be overwritten if present
                     exportClose = TRUE)     # the 'exportName' must also be given
{
  if ( ! is.numeric(timeseries) ) warning("numeric vector expected")
  if ( length(timeseries) == 0  ) warning("non-empty vector expected")

  # set defaults
  if ( length(ylim) == 1 && is.na(ylim) ) ylim <- range(timeseries)

  # plot count (external data)
  xem.plot.count <<- xem.plot.count + 1

  # plot window (external data)
  x11.title <- sprintf("%s : %03d", script, xem.plot.count)
  x11.size  <- c(xem.plot.x11$width.in, xem.plot.x11$height.in)   # set outside

  # svg export (external data)
  mm2inch    <- function(dims){ return(dims / 25.4) }
  svg.size   <- c(mm2inch(xem.plot.svg$width.mm), mm2inch(xem.plot.svg$height.mm))
  svg.points <- xem.plot.svg$points.pt

  # y-axis labeling
  # CAUTION: need to fix Windows "symbols" font bug whereby octal 260 is the Greek Upsilon1
  ylab <- yano
  if ( ylab == "quantity" && units == "[C]" )
    {
      ylab <- expression(paste("quantity [", degree, "C", "]", sep=""))   # needs fix
      ylab <- expression(paste("quantity [", "C", "]", sep=""))           # work-around
    }
  else if ( nchar(units) > 0 )
    {
      ylab <- paste(ylab, units, sep = " ")
    }

  # x-axis labeling
  xlabs    <- character(0)
  xlabs[1] <- paste("interval", interval, "s")
  xlabs[2] <- paste("length", length(timebase))
  if ( timebase[1] > 0 ) xlabs[3] <- paste("start", timebase[1])
  xlab <- paste(xlabs, collapse = " / ")

  # call either the display or the export device
  if ( is.na(exportName) )
    {
      x11(width  = x11.size[1],
          height = x11.size[2],
          title  = x11.title)
    }
  else
    {
      svg(filename  = exportName,
          width     = svg.size[1],
          height    = svg.size[2],
          pointsize = svg.points,
          bg        = "white")
    }

  # set some attributes adaptively
  len <- length(timeseries)
  pch <- 01
  cex <- if ( len < 1000 ) 1.0 else 0.7
  col <- "gray11"                            # colors()[164]

  # plot calls
  par(mar = c(5, 4, 4 + 1,  2) + 0.1)        # extra line at top (pos 3)
  plot(timebase, timeseries,  ylim = ylim, ann = FALSE, type = type, pch = pch, col = col, cex = cex)
  if ( grid ) grid()
  abline(h = 0.0, col = "gray")
  title(main = title, line = 3.5)
  title(ylab = ylab, xlab = xlab)
  mtext(text = subtitle1, side = 3, line = 2)
  mtext(text = subtitle2, side = 3, line = 1)

  # right-side annotation
  utcstamp <- format(as.POSIXlt(Sys.time(), tz = "GMT"), "%d-%b-%Y %H:%M UTC")
  msgs <- character()
  msgs <- append(msgs, paste("script",  script,   sep = " : "))
  msgs <- append(msgs, paste("model",   model,    sep = " : "))
  msgs <- append(msgs, paste("svn",     svn,      sep = " : "))
  msgs <- append(msgs, paste("plotted", utcstamp, sep = " : "))
  msg  <- paste(msgs, collapse = " | ")
  mtext(msg, side = 4, adj = 0.05, cex = 0.55)    # was: cex = 0.75

  # x-axis annotation
  msg <- "zero-based"
  if ( length(timeseries) == 8760 ) mtext(msg, side = 1, adj = 1.00, cex = 0.75)

  # close if export and permitted
  if ( ! is.na(exportName) && exportClose ) dev.off()

  # return
  return(invisible(NULL))
}

# end of file

