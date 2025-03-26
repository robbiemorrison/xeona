
#---------------------------------
#  function : xem.consts
# ---------------------------------
#  description  : some useful constants
#  role         : utility
#  status       : complete
# ---------------------------------

xem.consts <- function(confirm = FALSE)      # report when a const passes the 'file.exists' predicate
{
  # build subversion trunk -- hardcoded 'path' method
  home     <- Sys.getenv("HOME")
  synk     <- Sys.getenv("SYNK")
  path     <- "xeona/svn2/futz/trunk"
  trunk1   <- file.path(home, synk, path)

  # build subversion trunk -- 'XEONA' envar method
  fsep     <- .Platform$file.sep                       # R built-in
  xeona    <- Sys.getenv("XEONA")                      # 'xeona'-specific envar
  xeonas   <- unlist(strsplit(xeona, split = fsep))    # split on file separator
  xeonas   <- head(xeonas, -1)                         # remove last entry: 'xeona1'
  trunk2   <- paste(xeonas, collapse = fsep)           # reassemble

  # select
  trunk    <- trunk2

  # load list
  consts   <- list(trunk.dir    = file.path(trunk),
                   catalogs.dir = file.path(trunk, "models", "catalogs"))

  # expected number of directories and regular files that would be found
  expected <- length(consts) - 0

  # confirm the entries, conditional on 'confirm'
  if ( confirm )
    {
      count <- 0
      for (const in consts)
        {
          ret <- file.exists(const)
          if ( ret ) message("file present: ", const)
          if ( ret ) count <- count + 1
        }
      hitz   <- ifelse(count == 1, "hit", "hits")
      interp <- ifelse(count == expected, "(as expected)",paste("(expected", expected, ")", sep = ""))
      msg    <- paste(count, hitz, interp, sep = " ")
      message(msg)
    }

  # return the list invisibly
  invisible(consts)
}

# end of file

