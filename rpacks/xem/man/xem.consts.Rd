
\name{xem.consts}
\alias{xem.consts}
\title{Obtain list holding useful consts}
\description{

  Obtain list holding useful constants, including common directory paths
  and file names.}

\usage{

  xem.consts(confirm = FALSE)

}
\arguments{

  \item{confirm}{boolean: true means print the names of confirmed paths
    and skip those not found.}

}
\value{

  List of constants.

}
\author{

  Robbie Morrison <robbie@actrix.co.nz>.

}
\note{

  The R language does not support the notion of a constant so the use of
  the term \emph{constant} is advisory only.

  \emph{xeona} is an energy systems modeling environment.
}
\examples{

 # obtain the name of your home directory (trivial example)
 xem.consts <- xem.consts()
 trunk.dir  <- xem.consts$trunk.dir
 trunk.dir

}

% end of file

