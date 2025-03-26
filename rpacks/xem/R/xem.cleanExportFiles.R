
# ---------------------------------
#  function : xem.cleanExportFiles
# ---------------------------------
#  description  : clean up generated files after user confirmation
#  role         : typically run under '--clean'
#  notes        : tried 'readline' but would not stop, hence 'readLines'
#  status       : complete
#
#  Pattern
#
#     glob pattern = for (ext in exts) stub.?.r????.*.ext
#
# ---------------------------------

xem.cleanExportFiles <- function(stub,            # usually the study
                                 exts,            # subset of: c("gif", "png", "pdf", "svg", "csv")
                                 debug = FALSE,   # 'TRUE' means issue debugging messages
                                 exit  = NA)      # if set, quit under 'exit', must be integer-valued
{
  if ( debug ) message()
  if ( debug ) message("stub                : ", stub)

  count   <- 0
  path    <- dirname(stub)
  core    <- basename(stub)

  if ( debug ) message("path                : ", path)
  if ( debug ) message("core                : ", core)

  killist <- c()                             # list of files to kill
  for ( ext in exts )
    {
      regex <- glob2rx(paste(core, ".?.r????.", "*", ".", ext, sep = ""))
#     regex <- glob2rx(paste(core, "*", ".", ext, sep = ""))
      if ( debug ) message("regex (not glob)    : ", regex)
      files <- list.files(path    = path,
                          pattern = regex)
      killist <- c(killist, files)
    }
  if ( debug ) message()

  if ( length(killist) > 0 )
    {
      message()
      cat(killist, sep = "\n")

      if ( debug ) message()
      if ( debug ) print(killist)

      message()
      cat("  enter 'y' to delete: ")
      f <- file("stdin")
      y <- readLines(f, n = 1, warn = TRUE)
      message()
      if ( y == "y" )
        {
          status <- file.remove(killist)     # was 'exit.status' but always returned 1
          count  <- length(killist)
          plural <- if ( count == 1 ) "" else "s"
          message(count, " file", plural, " deleted")
        } else {
          message("no files deleted")
        }
      if ( ! is.na(exit) ) message()
    }

  # either 'quit' or return
  if ( ! is.na(exit) )
    {
      quit(save = "no", status = exit, runLast = FALSE)
    }
  else
    {
      return(count)
    }
}

# end of file
