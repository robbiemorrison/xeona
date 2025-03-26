
\name{robbie.updateTerminalWidth}
\alias{robbie.updateTerminalWidth}
\title{Dynamically update terminal width}
\description{Dynamically update the width of the current terminal.}
\usage{
  robbie.updateTerminalWidth()
}
\details{
  Uses the \code{stty size} system call to obtain the current size of
  the terminal.
}
\value{
  The width of the current terminal in characters.
}
\author{
  Robbie Morrison <robbie@actrix.co.nz>.
}
\note{
  The \code{stty} function is a standard Linux function.

  The examples below cannot be run because \code{stty} cannot be called
  on standard input --- as occurs during testing.  That said, this
  function can equally be used in interactive sessions or in scripts.
}
\examples{
 # update terminal width
\dontrun{width <- robbie.updateTerminalWidth()}
\dontrun{width}
}

% emacs text editor settings
% local variables:
%   mode: Rd
%   make-backup-files: nil
% end:

