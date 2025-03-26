
# ---------------------------------
#  function : xem.keepOpen
# ---------------------------------
#  description  : issue a TclTk dialog to prevent script from continuing or completing
#  role         : utility call
#  returns      : "ok" (not much use)
#  status       : complete
#
#  Information
#
#    messageBox settings : http://objectmix.com/tcl/390387-tk_messagebox-query.html
#    R + TclTk           : http://www.sciviews.org/_rgui/tcltk/
#
# ---------------------------------

xem.keepOpen <- function(prompt = "hit spacebar to close plots",      # main message
                         extra  = "",                                 # extra message
                         action = FALSE)                              # only action if TRUE
{
  suppressMessages(require(tcltk))                         # for message box and thus hold-open functionality

  if ( ! is.logical(action) ) warning("'action' not TRUE or FALSE")
  if ( action )
    {
      tcltk::.Tcl("option add *Dialog.msg.wrapLength 9i")   # default is 3i (inches)
      tcltk::.Tcl("option add *Dialog.dtl.wrapLength 9i")   # default is 3i (inches)
      capture <- tcltk::tk_messageBox(message = prompt,
                                      detail  = extra)
    }
}

# end of file

