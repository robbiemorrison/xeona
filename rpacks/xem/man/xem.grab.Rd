
\name{xem.grab}
\alias{xem.grab}
\title{Obtain xeona model field data}

\description{

  Obtain field data from \emph{xeona} model (XEM) files.

}
\usage{

  xem.grab(xfqf, want = "data", report = FALSE)

}
\arguments{

  \item{xfqf}{extended fully-qualified field name.}

  \item{want}{"string" (string data) "data" (numeric data) "kind"
  (angles) "measure" (units) "names" (complete list of names) "jettison"
  (dump the entire XEM file).}

  \item{report}{issue \code{message} calls if TRUE.}

}
\details{

  \emph{xeona} is an energy systems modeling environment.

}
\value{

  The grabbed data or nothing (warnings from \code{xgrab} are suppressed
  with \code{--quiet}).

}
\author{

  Robbie Morrison <robbie@actrix.co.nz>.

}
\note{

  Requires \code{xgrab} be present.  See the documentation for
  \code{xgrab} for more details about the use of \code{want}.

}
\examples{

 # obtain a string value
 stub <- "trial-008"
 leta <- "d"
 fqf  <- "program.study-description.scenario-name"
 xfqf <- paste(stub, leta, fqf, sep = ".")
 desc <- xem.grab(xfqf, want = "string", report = TRUE)
 desc

}

% end of file

