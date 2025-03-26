
#---------------------------------
#  function : xem.xplot
# ---------------------------------
#  description  : interface to 'xem.plot', supports export and identify functionality
#  role         : utility
#  status       : complete
# ---------------------------------

# see 'xem.plot' for an explanation of the undocumented arguments

xem.xplot <- function(timebase,
                      timeseries,
                      ylim        = NA,
                      grid        = FALSE,
                      title,
                      subtitle1,
                      subtitle2,
                      yano,
                      units,
                      interval,
                      stub,
                      leta,
                      script      = NA,
                      model       = NA,
                      svn         = NA,
                      type        = "p",
                      identify    = FALSE,   # run 'identify' feature
                      export      = FALSE,   # export to SVG
                      exportClose = TRUE,
                      exportAuto  = FALSE)
{
  # plot call
  xem.plot(timebase   = timebase,
           timeseries = timeseries,
           ylim       = ylim,
           grid       = grid,
           title      = title,
           subtitle1  = subtitle1,
           subtitle2  = subtitle2,
           yano       = yano,
           units      = units,
           interval   = interval,
           script     = script,
           model      = model,
           svn        = svn,
           type       = type,
           exportName = NA)

  # export graphics format as required
  ext <- "svg"
  if ( export )
    {
      exportName <- xem.generateExportName(stub  = stub,
                                           leta  = leta,
                                           svn   = svn,
                                           title = subtitle1,
                                           ext   = ext,
                                           ask   = ( ! exportAuto ))
      yesno <- if ( exportAuto ) "yes" else xem.exportYesNo(filename = exportName)
      if ( yesno == "yes" )
        {
          xem.plot.count <<- xem.plot.count - 1   # else gets incremented twice
          xem.plot(timebase    = timebase,
                   timeseries  = timeseries,
                   ylim        = ylim,
                   grid        = grid,
                   title       = title,
                   subtitle1   = subtitle1,
                   subtitle2   = subtitle2,
                   yano        = yano,
                   units       = units,
                   interval    = interval,
                   script      = script,
                   model       = model,
                   svn         = svn,
                   type        = type,
                   exportName  = exportName,
                   exportClose = TRUE)
        }
    }

  # collect identified points as required
  pts <- character(0)
  if ( identify )
    {
      points <- identify(timeseries)         # note also 'locator'
      pts    <- paste(sprintf("%+.2e (%d)", timeseries[points], points), collapse = "  ")
    }

  # return
  return(invisible(pts))
}

# end of file
