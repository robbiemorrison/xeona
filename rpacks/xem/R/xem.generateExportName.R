
#---------------------------------
#  function : xem.generateExportName
# ---------------------------------
#  description  : supports name generation
#  role         : called from 'myplot'
#  status       : complete (the keybinds work too!)
#
#  TclTk library
#
#    The bulk of this code came from James Wettenhall:
#
#      http://bioinf.wehi.edu.au/~wettenhall/RTclTkExamples/
#      http://bioinf.wehi.edu.au/~wettenhall/RTclTkExamples/modalDialog.html
#
#    More recent documentation base on same:
#
#      http://www.sciviews.org/_rgui/tcltk/index.html
#
#    Note also the prepended "tlctk::" namespace
#    resolution.
#
# ---------------------------------

xem.generateExportName <- function(stub,          # typically the study
                                   leta,
                                   svn   = "0",   # typically the run svn
                                   title = "",    # typically the given plot title
                                   ext,           # reflects the export format
                                   ask   = TRUE)  # 'F' for automatic filename generation
{
  suppressMessages(require(tcltk))

  rsvn     <- sprintf("r%04d", as.integer(svn))
  westside <- paste(stub, leta, rsvn, sep = ".")
  info     <- paste(westside, "  ", ext, sep = ".")

  title    <- tolower(title)
  title    <- gsub(":", " ", title, fixed = TRUE)      # remove ":" chars
  title    <- gsub("/", " ", title, fixed = TRUE)      # remove "/" chars
  title    <- gsub("(", " ", title, fixed = TRUE)      # remove "(" chars
  title    <- gsub(")", " ", title, fixed = TRUE)      # remove ")" chars
  title    <- gsub("[[:space:]]+", " ", title)         # multi-despace
  title    <- gsub("[[:space:]]+$", "", title)         # despace trailing

  if ( ask == FALSE )                                  # automatic filename generation
    {
      title   <- gsub("[[:blank:]]+", "-", title)      # hyphenize
      fname <- paste(westside, title, ext, sep = ".")  # create automatic filename
      fname <- gsub("..", ".", fname, fixed = TRUE)    # swap double dots for single dots
      return(fname)                                    # CAUTION: early return
    }

  modalDialog <- function(title,
                          prompt,
                          info,
                          initial,
                          entryWidth = 20,
                          returnValOnCancel = "CANCEL")
    {
      dlg <- tcltk::tktoplevel()
      tcltk::tkwm.deiconify(dlg)             # not sure what this does
      tcltk::tkgrab.set(dlg)
      tcltk::tkfocus(dlg)
      tcltk::tkwm.title(dlg, title)
      textEntryVarTcl <- tcltk::tclVar(paste(initial))
      textEntryWidget <- tcltk::tkentry(dlg, width = paste(entryWidth), textvariable = textEntryVarTcl)
      tcltk::tkgrid(tcltk::tklabel(dlg, text = info))                     # info
      tcltk::tkgrid(tcltk::tklabel(dlg, text = prompt), textEntryWidget)  # prompt
      tcltk::tkgrid(tcltk::tklabel(dlg, text = ""))                       # room
      returnVal <- returnValOnCancel

      onOk <- function()
        {
          returnVal <<- tcltk::tclvalue(textEntryVarTcl)
          tcltk::tkgrab.release(dlg)
          tcltk::tkdestroy(dlg)
        }

      onCancel <- function()
        {
          returnVal <<- returnValOnCancel
          tcltk::tkgrab.release(dlg)
          tcltk::tkdestroy(dlg)
        }

      ok.button     <- tcltk::tkbutton(dlg, text = "Accept name",      command = onOk)
      cancel.button <- tcltk::tkbutton(dlg, text = "Ignore qualifier", command = onCancel)
      tcltk::tkgrid(ok.button, cancel.button)
      tcltk::tkgrid(tcltk::tklabel(dlg, text = ""))    # room

      tcltk::tkfocus(dlg)
      tcltk::tkbind(dlg, "<Return>", onOk)             # was incorrectly 'textEntryWidget'
      tcltk::tkbind(dlg, "<Escape>", onCancel)         # my addition
      tcltk::tkwait.window(dlg)

      return(returnVal)
    }

  returnVal <- modalDialog(title      = "SVG export",
                           prompt     = "enter a filename qualifier: ",
                           info       = info,
                           initial    = title,
                           entryWidth = 40)

  msg   <- if ( returnVal == "CANCEL" ) "" else returnVal
  msg   <- gsub("[[:blank:]]+", "-", msg)              # hyphenize
  fname <- paste(westside, msg, ext, sep = ".")        # create filename
  fname <- gsub("..", ".", fname, fixed = TRUE)        # swap double dots for single dots
  message("filename            : ", fname)

  # expose filename
  return(fname)
}

# end of file

