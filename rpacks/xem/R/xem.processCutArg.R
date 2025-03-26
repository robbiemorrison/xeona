
# ---------------------------------
#  function : xem.processCutArg
# ---------------------------------
#  description  : turn zero-based 'cut' argument in one-based R sequence specifier
#  role         : utility
#  status       : complete
#
#  Typical io
#
#     'cut' in ":" ":00" "00:" "00:00"
#     reprocess to "00:00" while acknowledging 'steps'
#     output to "01:01" for use by R
#
#  Typical 'strsplit' results
#
#    ""       character(0)
#    ":"      [1] ""
#    ":90"    [1] ""   "90"
#    "10:"    [1] "10"
#    "10:90"  [1] "10" "90"
#
# ---------------------------------

xem.processCutArg <- function(cut,           # in ":" ":00" "00:" "00:00"
                              steps,         # horizon steps, often 8760
                              report = FALSE)
{
  # define the splitter char
  cutsplit <- ":"

  # integrity checks ('is.numeric' includes 'is.integer')
  if ( report &&   is.na(cut)        ) warning("'cut' argument is NA: ",                  cut,   immediate. = TRUE)
  if ( report &&   nchar(cut) == 0   ) warning("'cut' argument is empty string: ",        cut,   immediate. = TRUE)
  if ( report &&   is.na(steps)      ) warning("horizon 'steps' argument is NA: ",        steps, immediate. = TRUE)
  if ( report && ! is.numeric(steps) ) warning("horizon 'steps' argument not numeric: ",  steps, immediate. = TRUE)
  if ( report &&   steps == 0        ) warning("horizon 'steps' argument is zero: ",      steps, immediate. = TRUE)

  # for robustness
  if ( is.na(cut)      ) cut = ":"
  if ( nchar(cut) == 0 ) cut = ":"

  # split the 'cut', filling the gaps with 'steps' information
  parts <- unlist(strsplit(cut, split = cutsplit, fixed = TRUE))
  lo <- 0
  lo <- if ( parts[1] != "" ) as.integer(parts[1]) else lo
  hi <- steps - 1
  hi <- if ( length(parts) == 2 ) as.integer(parts[2]) - 1 else hi    # exclude last element: [lo,hi)
  hi <- min(steps - 1, hi)

  # recover a little if bad cut
  good <- TRUE                               # presume okay
  if ( lo > hi )
    {
      lo  <- hi
      msg <- paste("bad cut: steps = ", steps, " | cut = '", cut, "'", " | zero-based work-around = ", lo, ":", hi, sep = "")
      if ( report ) warning(msg, immediate. = TRUE)
      good <- FALSE
    }

  # 'lo' and 'hi' are now known
  xrange <- paste(lo, hi, sep = ":")              # 'xeona' range
  rrange <- paste((lo + 1), (hi + 1), sep = ":")  # R range

  # return value
  list(cut      = cut,                       # original cut argument
       steps    = steps,                     # original steps, typically 8760
       good     = good,                      # quality of result
       lo       = lo,                        # zero-based (add one for R)
       hi       = hi,                        # zero-based (add one for R)
       xrange   = xrange,                    # zero-based "00:00"
       rrange   = rrange)                    # one-based "01:01"

}

# end of file

