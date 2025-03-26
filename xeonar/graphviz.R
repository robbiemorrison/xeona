
#  file-purpose     : process and display graphviz data
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Mon 15-Jun-2009 06:11 UTC
#  file-status      : working
#  file-keywords    : xeona R

#  $Revision: 6756 $
#  $Date: 2011-05-10 12:11:51 +0200 (Tue, 10 May 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeonar/graphviz.R $

#  LEGAL NOTICE
#
#  Software  : This file is part of the source code for the xeona energy
#              systems modeling environment.
#  License   : This software is distributed under the GNU General Public
#              License version 3, a copy of which is provided in the text
#              file LICENSE_GPLv3.
#  Warranty  : There is no warranty for this software, to the extent permitted
#              by applicable law.  Refer to the license for further details.
#  Copyright : This software is copyright (c) 2007 - 2011 Robbie Morrison.
#  Request   : The software is distributed with the request that you forward
#              any modifications you make to the xeona project for possible
#              inclusion in the main codebase.
#
#  PROJECT CONTACT
#
#  Robbie Morrison
#  Institute for Energy Engineering
#  Technical University of Berlin
#  Marchstrasse 18, D-10587 Berlin, Germany
#  Email: robbie@actrix.co.nz

#  GRAPHVIZ
#
#  This code produces a .dot file which is subsequently
#  processed using the 'dot' utility -- using, for
#  instance:
#
#      $ dot -Tpng -o digraph.png digraph.dot
#
#  Tested with graphviz version 2.22.2 (20090313.1817).
#
#  REFERENCE
#
#  Gansner, Emden R, Eleftherios Koutsofios, and
#  Stephen C North.  2006.  Drawing graphs with 'dot'.
#  Report.  (probably published by AT&T).
#
#      download name: 'dotguide.pdf'
#      rename: 'graphviz-dot-guide.pdf'

# ---------------------------------
#  function : xem.rofile
# ---------------------------------
#  description  : make file read-only
#  role         : normally called after file creation
#  takes        : 'filename' as string
#  returns      : TRUE on success
#  techniques   : 'system' 'chmod'
#  status       : complete
#  CAUTION      : Linux-specific ('chmod')
# ---------------------------------

xem.rofile <- function (filename,
                        kill.empty = TRUE)
{
  if ( ! is.logical(kill.empty) ) {
    stop("expecting a logical argument type for 'kill.empty'")
  }

  if ( length(filename) !=  1 ) {
    if ( nchar(filename) == 0 ) {
      message("  warn : xem.rofile : problem filename ", filename)
      warning("problem filename")
      return(FALSE)
    }
  }

  if ( ! file.exists(filename) ) {
    message("  warn : xem.rofile : cannot locate file ", filename)
    warning("cannot locate file ", filename)
    return(FALSE)
  }

  # the following CANNOT be implemented using 'Sys.chmod'
  modestr <- paste("a-w", "o-r", "ug+r", sep = ",")
  ret <- system(paste("chmod", "--silent", modestr, filename, sep = " "))

  if ( kill.empty ) {                        # logical
    finfo <- file.info(filename)             # data frame
    size  <- finfo[ , "size"]                # CAUTION: the comma is correct, yields a numeric
    if ( size == 0 ) file.remove(filename)
  }

  if ( ret == 0 ) TRUE else FALSE
}

# ---------------------------------
#  function : xem.makeGvEdge
# ---------------------------------
#  description  : makes graphviz edges
#  role         : part of the build chain
#  techniques   : text processing
#  status       : mostly complete
# ---------------------------------
#
#  the leading space on the label string is for aesthetic reasons
#
#  null heads, those with 'head' == 0, are ignored
#
# ---------------------------------

edges <- character()

xem.makeGvEdge <- function (conex,           # data structure
                            assoc,           # association index
                            tail,
                            head)
{
  # for my system (sojus), the following colorway worked well
  spectrum <- xem.colors

  # grab class details
  outclass <- "(not specified)"              # tail entity class (later grouping)
  inclass  <- "(not specified)"              # head entity class (later grouping)
  if ( tail != 0 ) outclass <- conex[[tail]][["class"]]
  if ( head != 0 ) inclass  <- conex[[head]][["class"]]

  # process color
  tecs <- "^Teas|^Junc|^Node"
  if ( TRUE                                             ) acolor <- spectrum[ 1]
  if ( charmatch("Cx"      , outclass, nomatch = FALSE) ) acolor <- spectrum[10]
  if ( charmatch("Cx"      ,  inclass, nomatch = FALSE) ) acolor <- spectrum[ 9]
  if ( charmatch("Cm"      ,  inclass, nomatch = FALSE) ) acolor <- spectrum[ 8]
# if ( charmatch("Teas"    , outclass, nomatch = FALSE)
#      &&
#      charmatch("Teas"    ,  inclass, nomatch = FALSE) ) acolor <- spectrum[ 7]
  if ( length(grep(tecs, outclass))
       &&
      length(grep(tecs, inclass)) )                       acolor <- spectrum[ 7]
  if ((charmatch("Gate"    ,  inclass, nomatch = FALSE))
       ||
      (charmatch("Gate"    , outclass, nomatch = FALSE)
       &&
       length(grep(tecs, inclass)) ))                     acolor <- spectrum[ 6]
#      charmatch("Teas"    ,  inclass, nomatch = FALSE))) acolor <- spectrum[ 6]
  if ( charmatch("Asop"    , outclass, nomatch = FALSE) ) acolor <- spectrum[ 5]
  if ( charmatch("Asop"    ,  inclass, nomatch = FALSE) ) acolor <- spectrum[ 4]
  if ( charmatch("Domain"  , outclass, nomatch = FALSE)
       &&
       charmatch("Gate"    ,  inclass, nomatch = FALSE) ) acolor <- spectrum[ 3]
  if ( charmatch("Overseer", outclass, nomatch = FALSE) ) acolor <- spectrum[ 2]

  if ( head == 0 ) acolor <- spectrum[1]
  if ( tail == 0 ) acolor <- "black"         # quite noticeable

  # create line
  line <- ""
  if ( head > 0 )                            # ignore null heads
    {
      # 02 -> 04 [label = ' 05', color = orange]
      line    <- sprintf("%02d -> %02d [label = ' %02d', color = %s]", tail, head, assoc, acolor)
      edges  <<- append(edges, line)
    }
  invisible(line)
}

# ---------------------------------
#  function : xem.makeGvNode
# ---------------------------------
#  description  : makes graphviz nodes
#  role         : part of the build chain
#  techniques   : text processing
#  status       : mostly complete
# ---------------------------------

nodes <- character()
ranks <- character(7)

xem.makeGvNode <- function (vertex,
                            i,
                            name)
{
  # make node (can remove second 'i' to omit count on label)
  # center dot: high ASCII         : \xb7     (not acceptable to UTF-8 under inkscape)
  #             XML decimal coding : &#8226;
  line   <- sprintf("%02d [label = '%02d &#8226; %s']", i, i, name)
  nodes <<- append(nodes, line)

  # determine ranking
  class <- vertex[["class"]]
  if      ( charmatch("Overseer", class, nomatch = FALSE) ) rank <- 1
  else if ( charmatch("Domain"  , class, nomatch = FALSE) ) rank <- 2
  else if ( charmatch("Asop"    , class, nomatch = FALSE) ) rank <- 3
  else if ( charmatch("Gate"    , class, nomatch = FALSE) ) rank <- 4
  else if ( charmatch("Teas"    , class, nomatch = FALSE) ) rank <- 5
  else if ( charmatch("Junc"    , class, nomatch = FALSE) ) rank <- 5
  else if ( charmatch("Node"    , class, nomatch = FALSE) ) rank <- 5
  else if ( charmatch("Cm"      , class, nomatch = FALSE) ) rank <- 6
  else if ( charmatch("Cx"      , class, nomatch = FALSE) ) rank <- 7
  else                                                      rank <- NA

  # add rank data
  if ( ! is.na(rank) ) ranks[rank] <<- sprintf("%s %02d", ranks[rank], i)
}

# ---------------------------------
#  function : xem.makeGvGraph
# ---------------------------------
#  description  : makes a graphviz graph
#  role         : part of the build chain
#  techniques   : text processing
#  status       : mostly complete
# ---------------------------------
#
#  see r2885 for code without the left-side layline
#
#  fonts: it may be necessary to change from the default
#  font "Times Roman,serif" which was not known to
#  'ImageMagick' or 'inkscape' on my system
#
# ---------------------------------

xem.makeGvGraph <- function (name = "")      # the name of the dot-file graph object (but otherwise not used)
{
  # ---------------------------------
  #  appearance settings
  # ---------------------------------

# set <- list(over = TRUE, pts = 9, high = 0.5, font = "DejaVu Sans")  # loose (sojus)
  set <- list(over = TRUE, pts = 8, high = 0.3, font = "DejaVu Sans")  # normal(sojus)
# set <- list(over = TRUE, pts = 7, high = 0.0, font = "DejaVu Sans")  # tight (sojus)

  override   <- set$over                     # if FALSE, the remaining settings are ignored
  points     <- set$pts                      # default = 14
  nodeheight <- set$high                     # default = 0.5
  myfont     <- set$font                     # "" results in the default font "Times Roman,serif"

  # ---------------------------------
  #  attributes
  # ---------------------------------

  # create an introductory block, ## = technical issues

  intro <- character()
  if ( override ) {
  # intro <- append(intro,       "size = \"6.30, 10.63\"  // maximum size before rescaling: 160 wide x 270mm")
  # intro <- append(intro,       "margin = \"1.00, 1.00\"  // transparent")
 ## intro <- append(intro, paste("font = \"", myfont, "\""))

    intro <- append(intro,       "node [shape = box]")
    intro <- append(intro, paste("node [height = ", nodeheight,"]", sep = ""))
  # intro <- append(intro,       "node [style = filled  fillcolor = lightgray  color = lightgray]")
    intro <- append(intro,       "node [color = slategray]")
    intro <- append(intro, paste("node [fontname = \"", myfont, "\"]", sep = ""))
    intro <- append(intro,       "node [fontcolor = darkslategray]")
    intro <- append(intro, paste("node [fontsize = ", points, "]", sep = ""))

    intro <- append(intro,       "edge [color = orange]")
  # intro <- append(intro,       "edge [arrowsize = 1.0]")
  # intro <- append(intro,       "edge [style = bold]")
  # intro <- append(intro,       "edge [labelfloat = 1]")
  # intro <- append(intro,       "edge [decorate = 1]")
    intro <- append(intro, paste("edge [fontname = \"", myfont, "\"]", sep = ""))
    intro <- append(intro,       "edge [fontcolor = darkslategray]")
    intro <- append(intro, paste("edge [fontsize = ", points, "]", sep = ""))
  }
  else {
    intro <- "// using Graphviz default attributes"
  }

  # ---------------------------------
  #  layline
  # ---------------------------------

  # these labels can be easily modified
  labels    <- c("system coordination",
                 "domains",
                 "operators",
                 "gateways",
                 "assets",
                 "commodities",
                 "contexts")

  layers    <- c("overseer",
                 "domains",
                 "operators",
                 "gates",
                 "assets",
                 "commodities",
                 "contexts")

  laylines  <- character()
  laylabels <- character()
  layline   <- paste(layers, collapse = " -> ")
  for ( i in 1:length(layers) ) {
    line      <- paste(sprintf("%-11s", layers[i]), " ", "[label = '", labels[i], "']", sep = "")
    laylabels <- append(laylabels, line)
  }

  laylines <- append(laylines, "{")
  laylines <- append(laylines, "// preamble")
  laylines <- append(laylines, "node [shape = plaintext]")
  laylines <- append(laylines, "edge [style = invis]")
  laylines <- append(laylines, "// labels")
  laylines <- append(laylines, laylabels)
  laylines <- append(laylines, "// chain")
  laylines <- append(laylines, layline)
  laylines <- append(laylines, "}")

  # locally indent code
  for ( i in 2:(length(laylines) - 1) ) {
    laylines[i] <- paste("    ", laylines[i], sep = "")
  }

  # ---------------------------------
  #  rank (or layer) info
  # ---------------------------------

  rankset <- character()
  for ( i in 1:length(ranks) ) {
    buff    <- ranks[i]
    buff    <- xem.trim(buff)
    layer   <- layers[i]
    layer   <- sprintf("%-11s", layer)
    line    <- paste("rank = same;", layer, buff)
    line    <- xem.trim(line)
    line    <- paste("{", line, "}", sep = "")
    rankset <- append(rankset, line)
  }

  # ---------------------------------
  #  create output
  # ---------------------------------

  lines <- character()
  lines <- append(lines, paste("digraph '", name, "' {", sep = ""))
  lines <- append(lines, paste("// DOT file generated from:", xemfile))
  lines <- append(lines,       "// preamble")
  lines <- append(lines,       intro)
  lines <- append(lines,       "// layline (the left-side)")
  lines <- append(lines,       laylines)
  lines <- append(lines,       "// rank sets")
  lines <- append(lines,       rankset)
  lines <- append(lines,       "// node details")
  lines <- append(lines,       nodes)
  lines <- append(lines,       "// edge details (any leading space is for aesthetics)")
  lines <- append(lines,       edges)
  lines <- append(lines, "}")

  # indent code
  for ( i in 2:(length(lines) - 1) ) {
    lines[i] <- paste("    ", lines[i], sep = "")
  }

  # ---------------------------------
  #  finalize output
  # ---------------------------------

  # stringify
  linestr <- paste(lines, sep = "", collapse = "\n")   # insert newlines
  linestr <- paste(linestr, "\n", sep = "")            # add trailing newline

  # substitute ''' for '"' (to fix an earlier problem from 'sprintf')
  linestr <- gsub("'", "\"", linestr, fixed = TRUE)

  # return
  return(linestr)
}

# ---------------------------------
#  function : xem.gvTraverse
# ---------------------------------
#  description  : main conex traversal
#  role         : called by 'xem.graphviz'
#  takes        : 'conex'
#  returns      : edge call count
#  techniques   : nested 'for', 'sort', 'rev'
#  status       : complete
# ---------------------------------

xem.gvTraverse <- function (conex)
{
  calls  <- 0                                # make call count (excludes empty association lists)
  acount <- 0                                # association count (includes empty association lists)
  for ( i in 1:length(conex) )
    {
      vertex <- conex[[i]]                   # grab out vertex
      xem.makeGvNode(vertex,
                     i,
                     names(conex)[i])

      if ( length(vertex) < 2 ) {            # data preparation problem: vertex lacks adjacency list
        next                                 # applies to innermost loop
      }

      for ( j in 2:length(vertex) )
        {
          adjlist <- vertex[[j]]             # graph theory list, numeric R vector
          acount  <- acount + 1
          if ( is.null(adjlist) ) {          # necessary protection against data 'c()'
            next                             # applies to innermost loop
          }

          adjlist <- rev(sort(adjlist))      # to keep plot layering correct

          for ( k in 1:length(adjlist)  )
            {
              xem.makeGvEdge(conex,
                             acount,         # a number (or alternatively use 'names(vertex)[j]')
                             i,
                             adjlist[k])

              calls <- calls + 1
            }
        }
    }

  # return
  invisible(calls)
}

# ---------------------------------
#  function : xem.displayPNG.1
# ---------------------------------
#  description  : display PNG image
#  role         : display Graphviz representation
#  takes        : 'pngname' filename (assumed to exist)
#  returns      : TRUE on success
#  techniques   : 'display'
#  status       : complete
# ---------------------------------

xem.displayPNG.1 <- function (pngname,       # PNG file to display
                              frametitle)    # frame title string
{
  # reporting
  message("  info : xem.displayPNG.1 : commencing")
  on.exit(message("  info : displayPNG.1 : complete (on.exit)"))

  # check for presence of 'display' (call also echos the full path)
  if ( system("which display") != 0 ) {
    warning("required ImageMagick utility 'display' not found (omit job 16)")
  }

  # call creation
  call <- paste("display", "-title", frametitle, pngname, "&")

  # system call
  if ( system(call) != 0 ) {
    message("  warn : xem.displayPNG : display PNG using ImageMagick 'display' failed")
    warning("display PNG failed")
    invisible(FALSE)
  }

  # return
  invisible(TRUE)
}

# ---------------------------------
#  function : xem.displayPNG.2
# ---------------------------------
#  description  : plot PNG image
#  role         : display Graphviz representation
#  takes        : 'pngname' filename (assumed to exist)
#  returns      : TRUE on success
#  techniques   : 'EBImage-package'
#  status       : pseudo-code, requires library
# ---------------------------------

xem.displayPNG.2 <- function (pngname,            # PNG file to display
                              frametitle = NULL)  # frame title string (not used)
{
  # reporting
  message("  info : xem.displayPNG.2 : commencing")
  on.exit(message("  info : displayPNG.2 : complete (on.exit)"))

  # code
  if ( require(EBImage) ) {
    p   <- system.file("images", pngname, package = "EBImage")
    png <- readImage(p)
    display(png)
    invisible(TRUE)
  }
  else {
    message("  warn : xem.displayPNG.2 : required package 'EBImage' not present (change code to use xem.displayPNG.1)")
    warning("required package 'EBImage' not present")
    invisible(FALSE)
  }
}

# ---------------------------------
#  function : xem.gvOverall
# ---------------------------------
#  description  : processes and displays the 'dot' dataset
#  role         : external calls
#  takes        : 'dot' DOT language string vector
#  returns      : TRUE on success
#  techniques   : 'system'
#  status       : mostly complete
#
#  typical calls
#
#      $ dot -Tpng -o my.png my.gv
#      $ display -title "new title" my.png
#
#  another possibility
#
#      $ inkview my.svg
#
# ---------------------------------

xem.gvOverall <- function (dot)
{
  # reporting
  message("  info : xem.graphviz : commencing")
  on.exit(message("  info : xem.graphviz : complete (on.exit)"))

  # check for presence of 'dot' (call also echos the full path)
  if ( system("which dot") != 0 ) {
    warning("required Graphviz utility 'dot' not found (omit job 16)")
  }

  # filenames (any ".guard" has been stripped by now)
  tag <- ".gv"                               # can be ""
  tag <- ".viz"                              # added later!
  xdotname <<- sub("\\.xem$", paste(tag, ".dot", sep = ""), xemfile)
  xpngname <<- sub("\\.xem$", paste(tag, ".png", sep = ""), xemfile)
  xsvgname <<- sub("\\.xem$", paste(tag, ".svg", sep = ""), xemfile)

  # display title
  modelno <-  sub("\\.xem$", "", xemfile)
  pos     <- regexpr("[[:digit:]][[:digit:]]$", modelno)
  modelno <- substring(modelno, pos[1])      # assumes $ is used
# buggy sample: modelno <- substr(modelno, pos[1], pos[1] + attr(pos, "match.length")[1] - 1)
  if ( pos[1] == -1 ) modelno <- ""
  if ( length(modelno) != 0 ) modelno <- paste(modelno, ":", sep = "")
  frametitle  <- paste("\"", "graphviz: ", modelno, " ", xtitle, "\"", sep = "")
# frametitle  <- paste("\"", "graphviz: ", xtitle, "\"", sep = "")

  # call creation
  call1 <- paste("dot", "-Tpng", "-o", xpngname, xdotname)
  call2 <- paste("dot", "-Tsvg", "-o", xsvgname, xdotname)

  # backup existing files (Linux-specific code, sorry)
  # -- these filenames need to be cleared because they
  # were write-protected on creation
  #
  # later consider deletion, see ? unlink which is also vectorized
  xnames <- c(xdotname, xpngname, xsvgname)
  for ( xname in xnames )
    {
      xname.backup <- paste(xname, "~", sep = "")
      mv.call <- paste("mv", "--force", xname, xname.backup, sep = " ")    # 'mv' is Linux utility
      if ( ret <- system(mv.call) != 0 ) {
        warning("move call failed : ", mv.call)
      }
    }

  # create new 'dot' file
  cat(dot, file = xdotname)                            # write the 'dot' file
  xem.rofile(xdotname, kill.empty = FALSE)             # write-protect the DOT and also retain empty files

  # file processing (CAUTION: note the early returns)
  if ( system(call1) != 0 ) {                          # create PNG
    message("  warn : xem.graphviz : create PNG from dot file failed, filename ", xpngname)
    warning("create PNG failed ", xpngname)
    return(FALSE)
  }
  else {
    xem.rofile(xpngname, kill.empty = FALSE)           # write-protect the PNG and also retain empty files
  }

  # display PNG
  if ( ! xem.displayPNG.1(xpngname, frametitle) ) {    # display PNG
    message("  warn : xem.graphviz : display PNG failed, filename ", xpngname)
  }

  # create SVG
  if ( system(call2) != 0 ) {                          # create SVG
    message("  warn : xem.graphviz : create SVG from dot file failed, filename ", xsvgname)
    warning("create SVG failed ", xsvgname)
    return(FALSE)
  }
  else {
    xem.rofile(xsvgname, kill.empty = FALSE)           # write-protect the SVG and also retain empty files
  }

  # housekeeping
  message ("  info : xem.graphviz : dot, PNG, SVG files created")

  # return success
  invisible(TRUE)
}

# ---------------------------------
#  function : xem.graphviz
# ---------------------------------
#  description  : wrapper to the core calls
#  role         : job 16 point of contact
#  takes        :
#  returns      : TRUE on success (including 'system' calls)
#  techniques   : see core calls
#  status       : incomplete
# ---------------------------------

xem.graphviz <- function (recset,
                          title)
{
  # reporting
  message("  info : xem.graphviz : commencing")
  on.exit(message("  info : xem.graphviz : complete (on.exit)"))

  # code
  conex1    <- xem.createBreadset.1(recset)
  conex2    <- xem.createBreadset.2(conex1)
  edgecalls <- xem.gvTraverse(conex2)
  dot       <- xem.makeGvGraph(xtitle)
  ret       <- xem.gvOverall(dot)

  # return
  invisible(ret)
}

# ---------------------------------
#  junk
# ---------------------------------
#
#  'sojus' command-line limited to 98304 chars,
#  which limits this call to about 1000 nodes -- the
#  solution is to write to a temporary file
#
#  dotstr <- paste("'", dot, "'", sep = "")
#  call1 <<- paste("echo", dotstr, "|", "dot", "-Tpng", "-o", xpngname)
#
#  better to use "cat("...", file = "| cmd")

#  $Id: graphviz.R 6756 2011-05-10 10:11:51Z robbie $
#  end of file

