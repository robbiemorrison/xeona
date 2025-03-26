# ---------------------------------
#  function : robbie.getScriptName
# ---------------------------------
#  description  : get leaf name of this script
#  role         : utility call
#  status       : complete
# ---------------------------------

robbie.getScriptName <-
function()
{
  argv <- commandArgs(trailingOnly = FALSE)
  file <- substring(argv[grep("--file=", argv)], 8)
  leaf <- basename(file)
}

