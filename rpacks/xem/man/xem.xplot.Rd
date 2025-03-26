
\name{xem.xplot}
\alias{xem.xplot}
\title{Create and export standard xeona plot}

\description{

  Create a plot of consistent style for use by \emph{xeona} with export
  and identify functionality.

}
\usage{

  xem.xplot(timebase,
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
            identify    = FALSE,
            export      = FALSE,
            exportClose = TRUE,
            exportAuto  = FALSE)

}
\arguments{

  \item{timebase}{timebase, notionally zero-based, as numeric vector.}
  \item{timeseries}{timeseries as numeric vector.}
  \item{ylim}{y-axis limits (noting \code{xlim} in implicit in \code{timebase}).}
  \item{grid}{boolean: for add grid to plot.}
  \item{title}{main title.}
  \item{subtitle1}{first subtitle.}
  \item{subtitle2}{second subtitle.}
  \item{yano}{y-axis annotation.}
  \item{units}{units of measurement (with square brackets).}
  \item{interval}{horizon interval.}
  \item{stub}{study prefix.}
  \item{leta}{scenario letter.}
  \item{script}{caller script name for side note.}
  \item{model}{XEM model filename for side note.}
  \item{svn}{XEM model svn for side note.}
  \item{type}{plot type.}
  \item{identify}{boolean: true to invoke \code{identify} feature.}
  \item{export}{boolean: true to export plot (dialog boxes are used to
    finalize the filename).}
  \item{exportClose}{boolean: whether to close on export or not (the
    'exportName' must also be given).}
  \item{exportAuto}{boolean: true to sidestep user confirmation for
    export name and file save (batch mode support).}

}
\details{

  \emph{xeona} is an energy systems modeling environment.

}
\value{

  Invisible string of points if \code{identify} is selected.  Else \code{character(0).}

}
\author{

  Robbie Morrison <robbie@actrix.co.nz>.

}
\seealso{

  \code{xem.plot}

}
\examples{

  # optional preparation (note the 'x' is not used)
  xem.plot.win <- list(width.in  = 13.5,
                       height.in =  7.5)

  xem.plot.svg <- list(width.mm  =  160,
                       height.mm =  140,
                       points.pt =   11)

  \dontrun{xem.xplot(tbase, tseries, ylim, grid, title, subtitle1,
subtitle2, units, interval, study, scen, script, model, usedsvn, type,
TRUE, TRUE FALSE)}

}

% end of file

