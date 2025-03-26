
\name{xem.generateExportName}
\alias{xem.generateExportName}
\title{Generate an export name}

\description{

  Generate a suitable file name for exporting a plot.

}
\usage{

  xem.generateExportName(stub,
                         leta,
                         svn   = "0",
                         title = "",
                         ext,
                         ask   = TRUE)

}
\arguments{

  \item{stub}{typically the study name.}
  \item{leta}{typically the scenario letter.}
  \item{svn}{typically the run svn.}
  \item{title}{typically the given plot title.}
  \item{ext}{a file extension consistent with the export format.}
  \item{ask}{boolean: true means issue user dialogs, else work
    automatically (batch mode support).}

}
\details{

  \emph{xeona} is an energy systems modeling environment.

}
\value{

  The generated filename as a string.

}
\author{

  Robbie Morrison <robbie@actrix.co.nz>.

}
\note{

  Requires package \code{tcltk} be present.

}
\examples{

 # obtain a generated export name
 stub  <- "trial-008"
 leta  <- "d"
 svn   <- "0000"
 title <- "plot title"
 \dontrun{fname <- xem.generateExportName(stub, leta, svn, title, ext)}

}

% end of file

