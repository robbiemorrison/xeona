# ---------------------------------
#  function : robbie.updateTerminalWidth
# ---------------------------------
#  description  : adjust for the terminal width dynamically
#  role         : utility call
#  status       : complete
# ---------------------------------

robbie.updateTerminalWidth <-
function()
{
  widthcall <- "stty size | cut --delimiter=' ' --field=2"    # second argument from "stty size"
  width     <- system(widthcall, intern = TRUE)               # capture
  width     <- as.numeric(width)                              # convert string to integer
  width     <- width - 2                                      # create some rightwards space
  options(width = width)
  getOption("width")                                          # return the new setting
}

