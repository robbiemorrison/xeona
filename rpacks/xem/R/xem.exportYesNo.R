
# ---------------------------------
#  function : xem.exportYesNo
# ---------------------------------
#  description  : issue a TclTk dialog to ask about export
#  role         : utility call
#  returns      : "yes" or "no"
#  status       : complete
# ---------------------------------

xem.exportYesNo <- function(prompt = "enter yes to export",      # main message
                            filename)                            # mandatory filename
{
  suppressMessages(require(tcltk))                     # for message box and thus hold-open functionality

  tcltk::.Tcl("option add *Dialog.msg.wrapLength 9i")  # default is 3i (inches)
  tcltk::.Tcl("option add *Dialog.dtl.wrapLength 9i")  # default is 3i (inches)
  extra   <- paste("file:", filename)
  capture <- tcltk::tk_messageBox(message = prompt,
                            detail  = extra,
                            icon    = "question",
                            type    = "yesno",
                            default = "no")
}

# end of file

