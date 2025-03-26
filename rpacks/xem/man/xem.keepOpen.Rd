
\name{xem.keepOpen}
\alias{xem.keepOpen}
\title{Prevent Rscript from completing}
\description{

  Prevent \code{Rscript} from completing (or otherwise pause en-route),
  thereby holding any active or inactive plot windows open.

}
\usage{

  xem.keepOpen(prompt = "hit spacebar to close plots",
               extra  = "",
               action = FALSE)

}
\arguments{

  \item{prompt}{main prompt message.}
  \item{extra}{subprompt message.}
  \item{action}{logical indicating whether to act or not.}

}
\details{

  This function is used to prevent \code{Rscript} from completing.

  \emph{xeona} is an energy systems modeling environment.

}
\value{

  Returns the string "ok" (not much use).

}
\author{

  Robbie Morrison <robbie@actrix.co.nz>.

}
\note{

  Requires package \code{tcltk} be present.

}
\examples{

 # obtain a string value
 stub <- "trial-008"
 leta <- "d"
 fqf  <- "program.study-description.scenario-name"
 xfqf <- paste(stub, leta, fqf, sep = ".")
 desc <- xem.keepOpen()
 desc

}

% end of file

