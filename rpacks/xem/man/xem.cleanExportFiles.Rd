
\name{xem.cleanExportFiles}
\alias{xem.cleanExportFiles}
\title{Clean up generated files}

\description{

  Clean up generated files after en-masse confirmation by the user.

}
\usage{

  xem.cleanExportFiles(stub,
                       exts,            # usually subset of c("gif", "png", "pdf", "svg", "csv")
                       debug = FALSE,
                       exit  = NA)
}
\arguments{

  \item{stub}{usually the study}

  \item{exts}{character vector: containing extension strings.}

  \item{debug}{issue \code{message} calls if TRUE.}

  \item{exit}{if set, \code{quit} under \code{exit}, must be either NA
  or integer-valued.}

}
\details{

  As currently coded, the glob pattern is:

  \code{for (ext in exts) stub.?.r????.*.ext}

}
\value{

  If \code{exit} if NA, the number of files removed.

}
\author{

  Robbie Morrison <robbie@actrix.co.nz>.

}
\note{

  \emph{xeona} is an energy systems modeling environment.

}
\examples{

 # delete typical files after user confirmation
 stub  <- "trial-000"
 exts  <- c("gif", "png", "pdf", "svg", "csv")
 \dontrun{count <- xem.cleanExportFiles(stub = stub, exts = exts, debug = TRUE)}

}

% end of file

