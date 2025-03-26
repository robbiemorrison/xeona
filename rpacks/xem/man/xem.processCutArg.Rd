
\name{xem.processCutArg}
\alias{xem.processCutArg}
\title{Process a cut argument}

\description{

  Process a script \emph{cut} argument (usually entered via the
  command-line option \code{--cut}).

}
\usage{

  xem.processCutArg(cut,
                    steps,
                    report = FALSE)

}
\arguments{

  \item{cut}{string: a partial or complete colon-separated zero-based
  timeseries range (see details).}

  \item{steps}{(integer-valued) numeric: horizon steps (used to confirm
  cut range is valid).}

  \item{report}{boolean: true means issue warnings.}

}
\details{

  Cut may be one of \code{":"} \code{":00"} \code{"00:"} or
  \code{"00:00"} thereby specifying the zero-based range [00,00).
  Unspecified entries indicate the current start and finish values.
  Hence \code{":"} offers no narrowing.

}
\value{

  List containing zero-based and one-based ranges, namely:

  \item{cut}{original \code{cut} argument.}
  \item{steps}{original \code{steps} argument, typically 8760.}
  \item{good}{quality of result (false if \code{cut} invalidated \code{steps}).}
  \item{lo}{zero-based inclusive low value (add one for R).}
  \item{hi}{zero-based inclusive high value (add one for R).}
  \item{xrange}{zero-based \emph{xeona} range "00:00".}
  \item{rrange}{one-based R range"01:01".}

}
\author{

  Robbie Morrison <robbie@actrix.co.nz>.

}
\note{

  \emph{xeona} is an energy systems modeling environment.

}
\examples{

 # reprocess 'cut' for use by R
 cut      <- "100:"
 steps    <- 500
 cutdata  <- xem.processCutArg(cut = cut, steps = steps, report = TRUE)
 cutdata$good    # TRUE
 cutdata$rrange  # 101:500

}

% end of file

