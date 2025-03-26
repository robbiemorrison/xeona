
#---------------------------------
#  function : xem.grab
# ---------------------------------
#  description  : interface to 'xgrab' for 'xfqf' extended fully-qualified field names
#  role         : basic call
#  status       : complete
# ---------------------------------

xem.grab <- function(xfqf,                   # extended fully-qualified field name
                     want   = "data",        # "string" "data" "kind" "measure" "names" "jettison"
                     report = FALSE)         # non-silent behavior
{
  func    <- match.call()[1]                 # function name
  xgrab   <- switch(want,
                    string   = "xgrab --quiet",
                    data     = "xgrab --quiet",
                    kind     = "xgrab --quiet --kind",
                    measure  = "xgrab --quiet --measure",
                    names    = "xgrab --quiet --names",
                    jettison = "xgrab --quiet --jettison",
                    warning("unsupported argument want: ", want))
  what    <- switch(want,
                    string   = character(0),
                    data     = numeric(0),
                    kind     = character(0),
                    measure  = character(0),
                    names    = character(0),
                    jettison = character(0))
  tmpfile <- tempfile()                      # make a tempfile, probably cleaned up on script exit
  call    <-  sprintf(fmt = "%-25s %-70s > %s", xgrab, xfqf, tmpfile)
  if ( report ) message(func, ": call = ", call)
  ret     <- system(call, intern = FALSE)
  if ( ret == 0 ) {
    data <- scan(file = tmpfile, sep = " ", what, quiet = (! report))
  } else if ( ret == 23*256 ) {              # "sought record not found"
    data <- NA
  } else {
    warning("system call choked  : ", ret, " (", ret/256, ")")
    data <- NA
  }
  invisible(data)
}

# end of file

