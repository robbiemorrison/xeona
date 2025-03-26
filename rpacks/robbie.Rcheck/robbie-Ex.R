pkgname <- "robbie"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('robbie')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("robbie.getScriptName")
### * robbie.getScriptName

flush(stderr()); flush(stdout())

### Name: robbie.getScriptName
### Title: Obtain script name
### Aliases: robbie.getScriptName

### ** Examples

 # obtain leaf name of current script
 script <- robbie.getScriptName()
 script



cleanEx()
nameEx("robbie.updateTerminalWidth")
### * robbie.updateTerminalWidth

flush(stderr()); flush(stdout())

### Name: robbie.updateTerminalWidth
### Title: Dynamically update terminal width
### Aliases: robbie.updateTerminalWidth

### ** Examples

 # update terminal width
## Not run: width <- robbie.updateTerminalWidth()
## Not run: width



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
