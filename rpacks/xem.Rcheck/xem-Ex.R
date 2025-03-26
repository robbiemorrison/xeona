pkgname <- "xem"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('xem')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("xem.cleanExportFiles")
### * xem.cleanExportFiles

flush(stderr()); flush(stdout())

### Name: xem.cleanExportFiles
### Title: Clean up generated files
### Aliases: xem.cleanExportFiles

### ** Examples


 # delete typical files after user confirmation
 stub  <- "trial-000"
 exts  <- c("gif", "png", "pdf", "svg", "csv")
 ## Not run: count <- xem.cleanExportFiles(stub = stub, exts = exts, debug = TRUE)




cleanEx()
nameEx("xem.consts")
### * xem.consts

flush(stderr()); flush(stdout())

### Name: xem.consts
### Title: Obtain list holding useful consts
### Aliases: xem.consts

### ** Examples


 # obtain the name of your home directory (trivial example)
 xem.consts <- xem.consts()
 trunk.dir  <- xem.consts$trunk.dir
 trunk.dir




cleanEx()
nameEx("xem.exportYesNo")
### * xem.exportYesNo

flush(stderr()); flush(stdout())

### Name: xem.exportYesNo
### Title: Confirm export.
### Aliases: xem.exportYesNo

### ** Examples


 # confirm the export of 'filename'
 filename <- "xxxx.yyy.zzz"
 ## Not run: yesno    <- xem.exportYesNo(filename = filename)




cleanEx()
nameEx("xem.generateExportName")
### * xem.generateExportName

flush(stderr()); flush(stdout())

### Name: xem.generateExportName
### Title: Generate an export name
### Aliases: xem.generateExportName

### ** Examples


 # obtain a generated export name
 stub  <- "trial-008"
 leta  <- "d"
 svn   <- "0000"
 title <- "plot title"
 ## Not run: fname <- xem.generateExportName(stub, leta, svn, title, ext)




cleanEx()
nameEx("xem.grab")
### * xem.grab

flush(stderr()); flush(stdout())

### Name: xem.grab
### Title: Obtain xeona model field data
### Aliases: xem.grab

### ** Examples


 # obtain a string value
 stub <- "trial-008"
 leta <- "d"
 fqf  <- "program.study-description.scenario-name"
 xfqf <- paste(stub, leta, fqf, sep = ".")
 desc <- xem.grab(xfqf, want = "string", report = TRUE)
 desc




cleanEx()
nameEx("xem.keepOpen")
### * xem.keepOpen

flush(stderr()); flush(stdout())

### Name: xem.keepOpen
### Title: Prevent Rscript from completing
### Aliases: xem.keepOpen

### ** Examples


 # obtain a string value
 stub <- "trial-008"
 leta <- "d"
 fqf  <- "program.study-description.scenario-name"
 xfqf <- paste(stub, leta, fqf, sep = ".")
 desc <- xem.keepOpen()
 desc




cleanEx()
nameEx("xem.plot")
### * xem.plot

flush(stderr()); flush(stdout())

### Name: xem.plot
### Title: Create standard xeona plot
### Aliases: xem.plot xem.plot.x11 xem.plot.svg xem.plot.settings
###   xem.plot.count

### ** Examples


  # settings
  msg <- xem.plot.settings()    # final newline not added
  message(msg)                  # 'message' adds final newline

  # optional preparation
  xem.plot.svg$width.mm  <- 180 # default 160
  xem.plot.svg$height.mm <- 200 # default 140
  xem.plot.svg$points.pt <-  10 # default   9

  # call
  ## Not run: xem.plot(tbase, tseries, ylim, grid, title, subtitle1, subtitle2, units, interval, model, usedsvn, type, exportName = NA)




cleanEx()
nameEx("xem.processCutArg")
### * xem.processCutArg

flush(stderr()); flush(stdout())

### Name: xem.processCutArg
### Title: Process a cut argument
### Aliases: xem.processCutArg

### ** Examples


 # reprocess 'cut' for use by R
 cut      <- "100:"
 steps    <- 500
 cutdata  <- xem.processCutArg(cut = cut, steps = steps, report = TRUE)
 cutdata$good    # TRUE
 cutdata$rrange  # 101:500




cleanEx()
nameEx("xem.xplot")
### * xem.xplot

flush(stderr()); flush(stdout())

### Name: xem.xplot
### Title: Create and export standard xeona plot
### Aliases: xem.xplot

### ** Examples


  # optional preparation (note the 'x' is not used)
  xem.plot.win <- list(width.in  = 13.5,
                       height.in =  7.5)

  xem.plot.svg <- list(width.mm  =  160,
                       height.mm =  140,
                       points.pt =   11)

  ## Not run: 
##D xem.xplot(tbase, tseries, ylim, grid, title, subtitle1,
##D subtitle2, units, interval, study, scen, script, model, usedsvn, type,
##D TRUE, TRUE FALSE)
## End(Not run)




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
