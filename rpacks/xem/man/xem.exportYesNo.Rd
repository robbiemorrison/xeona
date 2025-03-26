
\name{xem.exportYesNo}
\alias{xem.exportYesNo}
\title{Confirm export.}
\description{

  Confirm the export of a plot to the given filename.

}
\usage{

  xem.exportYesNo(prompt = "enter yes to export",
                  filename)

}
\arguments{

  \item{prompt}{main message.}
  \item{filename}{selected filename to confirm.}

}
\details{

  \emph{xeona} is an energy systems modeling environment.

}
\value{

  Returns "yes" or "no" as string.

}
\author{

  Robbie Morrison <robbie@actrix.co.nz>.

}
\note{

  Requires package \code{tcltk} be present.

}
\examples{

 # confirm the export of 'filename'
 filename <- "xxxx.yyy.zzz"
 \dontrun{yesno    <- xem.exportYesNo(filename = filename)}

}

% end of file

