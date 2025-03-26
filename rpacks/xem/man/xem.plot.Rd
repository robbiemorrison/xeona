
\name{xem.plot}
\alias{xem.plot}
\alias{xem.plot.x11}
\alias{xem.plot.svg}
\alias{xem.plot.settings}
\alias{xem.plot.count}
\title{Create standard xeona plot}

\description{

  Create a plot of consistent style for use by  \emph{xeona}.
}
\usage{

  xem.plot.x11 <- list(width.in  = 13.5,
                       height.in =  7.5)
  xem.plot.svg <- list(width.mm  =  160,
                       height.mm =  140,
                       points.pt =    9)

  xem.plot.settings()

  xem.plot.count <- 0    # for labeling plot windows

  xem.plot(timebase,
           timeseries,
           ylim        = NA,
           grid        = FALSE,
           title,
           subtitle1,
           subtitle2,
           yano,
           units,
           interval,
           script      = NA,
           model       = NA,
           svn         = NA,
           type        = "p",
           exportName  = NA,
           exportClose = TRUE)

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
  \item{script}{caller script name for side note.}
  \item{model}{XEM model filename for side note.}
  \item{svn}{XEM model svn for side note.}
  \item{type}{plot type.}
  \item{exportName}{export filename (will be silently overwritten if present).}
  \item{exportClose}{boolean: whether to close on export or not (the 'exportName' must also be given).}

}
\details{

  \emph{xeona} is an energy systems modeling environment.

}
\value{

  Invisible null.

}
\author{

  Robbie Morrison <robbie@actrix.co.nz>.

}
\seealso{

  \code{xem.xplot}

}
\examples{

  # settings
  msg <- xem.plot.settings()    # final newline not added
  message(msg)                  # 'message' adds final newline

  # optional preparation
  xem.plot.svg$width.mm  <- 180 # default 160
  xem.plot.svg$height.mm <- 200 # default 140
  xem.plot.svg$points.pt <-  10 # default   9

  # call
  \dontrun{xem.plot(tbase, tseries, ylim, grid, title, subtitle1, subtitle2, units, interval, model, usedsvn, type, exportName = NA)}

}

% end of file

