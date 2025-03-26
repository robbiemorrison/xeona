
#  file-purpose     : create breadboard plot
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Thu 04-Jun-2009 19:56 UTC
#  file-status      : working
#  file-keywords    : xeona R

#  $Revision: 6005 $
#  $Date: 2011-02-25 19:12:42 +0100 (Fri, 25 Feb 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeonar/breadplot.R $

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

#  OVERVIEW
#
#  associations  : tie / link / connect / couple / bind / span
#  relevant here : link to commodities and contexts / connect cables to sockets

# ---------------------------------
#  function : xem.createBreadset.1
# ---------------------------------
#  description  : takes recset and builds label-based conex
#  role         : breadplot data preparation
#  takes        : recset
#  returns      : conex1
#  techniques   : 'is.element', (asymmetric) 'setdiff', 'next'
#  status       : complete
# ---------------------------------

xem.createBreadset.1 <- function (recset)
{
  message("  info : xem.createBreadset.1 : commencing")
  if ( DBUG > 0 ) message()

  recordIds <- names(recset)                 # not used
  entityIds <- recset$metadata$ids           # collected as part of the build process

  if ( is.null(entityIds) ) stop("no entities specified in in model (malformed data)")

  breadset  <- list()                        # declare an empty list object
  p         <- 0                             # breadset counter

  for ( i in 1:length(entityIds) )           # loop the entities
    {
      if ( DBUG > 0 ) message("entity loop ", i, " with id ", entityIds[i])

      bread    <- list()                     # may or may not be inserted
      q        <- 0                          # bread counter

      entityId <- entityIds[i]
      entity   <- recset[[entityId]]         # grab the entity

      if ( entity$enabled == FALSE ) next    # skip disabled entities

      for ( j in 3:length(entity) )          # loop the fields, but skip the first two
        {
          if ( DBUG > 0 ) message("+ field loop ", j, " with name ", names(entity)[j])

          field   <- entity[[j]]
          value   <- field$value
          name    <- names(entity)[j]
          enabled <- field$enabled

          if ( enabled == FALSE )                             next    # skip disabled fields

          if ( ! is.character(value) )                        next    # only interested in sets of strings
          if ( name == "builtin-remark" )                     next    # a mandatory field in most cases
          if ( name != "class" )                                      # class is always wanted
            {
              # split single strings
              split1 <- "[[:blank:]]+"                                # define the split regex
              value  <- unlist(strsplit(value, split = split1))       # CAUTION: 'value' is overwritten

              # split if contains sub-entity
              #
              # assumes that "sub-id" associations are individual
              # or else the code (such as [1]) should be vectorized
              # (or confirmed to be thus)
              #
              split2 <- "\\."
              if ( length(value) == 1 ) {
                temp <- unlist(strsplit(value, split = split2))
                value        <- temp[1]
                names(value) <- temp[2]      # [1]
              }

              # set-theory test
              if ( length(intersect(value, entityIds)) == 0 ) next    # disjoint

              # integrity checks
              msg <- character()
              if ( ! is.element(entityId, entityIds)   ) msg[1] <- "coding error"
              if (   is.element(entityId, value)       ) msg[2] <- "self-referenced value"
              if (   length(setdiff(value, entityIds)) ) msg[3] <- "unresolved values"

              if ( length(msg) > 0 ) {                 # at least one of the above tripped
                msg <- msg[!is.na(msg)]                # remove NA element
                msg <- paste(msg, collapse = " / ")    # squish the rest into one string
                message("issues : ", msg)
                message("entity id '", entityId, "'")
                message("values:")
                noquote(value)
                message("entity ids:")
                noquote(entityIds)
                message("  warn : createBreadset : integrity check failure, see above")
                warning("integrity check failure (longer explanations earlier) ", msg)
              }
            }

          # load connection information
          bread[q <- q + 1] <- list(value)   # the value
          names(bread)[q]   <- name          # the field name
        }

      # skip
      if ( length(bread) < 1 ) {
        # indicates missing class data
        message("  warn : xem.createBreadset.1 : a length zero bread encountered", entityId)
        warning("integrity check failure (longer explanations earlier) ", "short bread")
        next
      }

      # skip the time horizon, it doesn't associate with anybody
      if ( entityId == "time-horizon" ) next

      # load entity
      if ( DBUG > 0 ) {
        message("about to insert a bread")
        str(bread)
      }

      breadset[[p <- p + 1]] <- bread        # insert
      names(breadset)[p]     <- entityId
    }

  # report
  if ( DBUG > 0 ) {
    message()
    message("i   : ", i)
    message("j   : ", j)
    message("p   : ", p)
    message("q   : ", q)
    message()
    message("breadset")
    str(breadset)
  }

  message("  info : xem.createBreadset.1 : complete, breadset length ", length(breadset))

  # return
  invisible(breadset)
}

# ---------------------------------
#  function : xem.createBreadset.2
# ---------------------------------
#  description  : takes conex1, converts to numerical indexes, and removes non-tail-and/or-head entitys
#  role         : breadplot data preparation
#  takes        : conex1
#  returns      : conex2
#  techniques   : 'match'
#  status       : complete
#
#  processed in xem file order - nested list sorting may require
#  dedicated code
#
# ---------------------------------

xem.createBreadset.2 <- function (vertexs)
{
  # report
  message("  info : xem.createBreadset.2 : commencing")

  # return early
  if ( length(vertexs) == 0 ) {              # got empty list
    message("  info : xem.createBreadset.1 : early return, vertexs length ", 0)
    return ( list() )                        # give empty list
  }
  if ( DBUG > 0 ) message()

  # obtain names
  vertexIds <- names(vertexs)

  # traverse and match
  for ( i in 1:length(vertexIds) )
    {
      vertexId <- vertexIds[i]

      if ( length(vertexs[[i]]) < 2 ) next   # only the class name present so skip

      for ( j in 2:length(vertexs[[i]]) )
        {
          value              <- vertexs[[i]][[j]]
          nums               <- match(value, vertexIds, nomatch = 0)  # non-default zero for no match
          vertexs[[i]][[j]]  <- nums
          names(vertexs[[i]][[j]]) <- names(value)     # CAUTION: required to carry over the "sub-id"
        }
    }

  # report
  if ( DBUG > 0 ) {
    message("vertexs")
    str(vertexs)
    message()
  }

  message("  info : xem.createBreadset.2 : complete, vertexs length ", length(vertexs))

  # return
  invisible(vertexs)
}

# ---------------------------------
#  function : xem.makeArrow
# ---------------------------------
#  description  : plots an "arrow", currently an "open head" segment
#  role         : called by 'xem.traverse'
#  takes        : see code
#  returns      : 'acolor' invisibly, which is a string like "#000000"
#  techniques   : 'rainbow', 'charmatch' and currently 'segments', 'points'
#  status       : complete - but coloration scheme probably needs work
#
#  last-match coloration scheme, based on class information
#
#  presumption                should usually be overwritten   --            gray
#  from "Cx"                  not currently implemented       link          blueish
#  to   "Cx"                  link                            link
#  to   "Cm"                  commodity                       link          .
#  from "Teas|Junc|Node"      to teas or gate                 connection    .
#  from "Asop"                                                link          .
#  to   "Asop"                                                link
#  from "Domain" to "Gate"    ie "ranked-sel-gates"           link
#  from "Overseer"                                            link          reddish
#
# ---------------------------------

xem.makeArrow <- function (conex,            # data structure
                           assoc,            # association index
                           tail,
                           head)
{
  # for my system (sojus), the following colorway worked well
  spectrum <- xem.colors

  # warn
  if ( head == 0 ) {
    message("  data : xem.makeArrow : null head found, tail-head-assoc ",
            tail, "-", head, "-", assoc)
  }
  if ( tail == 0 ) {
    message("  warn : xem.makeArrow : null tail found (very odd)")
    warning("null tail found (very odd)")
  }

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
       length(grep(tecs, inclass)) )                      acolor <- spectrum[ 7]
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

  # explain
  if ( DBUG > 0 ) message("  dbug : spanning ", outclass, " -> ", inclass, " with color ", acolor)

  # plot
  linewidth <- 1                             # line width, default = par("lwd"), probably 1
  linewidth <- 2                             # line width, default = par("lwd"), probably 1

  if ( head != 0 && tail != 0 )
    {
      segments(assoc, tail,                  # tail
               assoc, head,                  # head
               lwd = linewidth,
               col = acolor)
    }
  if ( tail != 0 )
    {
      points(assoc, tail,                    # tail
             pch = 21,                       # 21 = circle, 22 = square, 24 = up-triangle
             lwd = linewidth,
             bg  = acolor,
             col = acolor)

      # add hint about disabled head
      disabledTag <- "disabled"
      disabledTag <- "??"
      if ( head == 0 ) text(assoc, tail, labels = disabledTag, adj = c(0.5, -1.5))    # 'pos = 3' overrides 'adj'
    }
  if ( head != 0 )
    {
      points(assoc, head,                    # head
             pch = 21,                       # 21 = circle, 22 = square, 24 = up-triangle
             lwd = linewidth,
             bg  = "white",
             col = acolor)
    }

  # process "sub-id" if present via the name attribute
  subid <- names(head)
  if ( ! is.null(subid) ) {                  # protect against no names
    if ( ! is.na(subid) ) {                  # protect against unassigned names
      text(assoc, head,
           subid,
           srt = 90,
           adj = c(0.5, -1.0),
           col = "lightslategray")
    }
  }

  # invisible return
  invisible(acolor)
}

# ---------------------------------
#  function : xem.breadTraverse
# ---------------------------------
#  description  : main conex traversal
#  role         : called by 'xem.breadboard'
#  takes        : 'conex'
#  returns      : arrow call count
#  techniques   : nested 'for', 'sort', 'rev'
#  status       : complete
# ---------------------------------

xem.breadTraverse <- function (conex)
{
  calls  <- 0                                # make call count (excludes empty association lists)
  acount <- 0                                # association count (includes empty association lists)
  for ( i in 1:length(conex) )
    {
      vertex <- conex[[i]]                   # grab out vertex

      if ( length(vertex) < 2 ) {            # data preparation problem
        message("  data : vertex lacks adjacency list (probably okay): ", names(conex)[i])
        next                                 # applies to innermost loop
      }

      for ( j in 2:length(vertex) )
        {
          adjlist <- vertex[[j]]             # graph theory list, numeric R vector
          acount  <- acount + 1
          if ( is.null(adjlist) ) {          # necessary protection against 'c()'
            message("  data : empty adjacency list (modeling issue): ", names(conex)[i], ".", names(vertex)[j])
            next                             # applies to innermost loop
          }

          adjlist <- xem.pyramidSort(adjlist, i)  # to keep the plot layering correct

          for ( k in 1:length(adjlist)  )
            {
              xem.makeArrow(conex,           # pass across data structure
                            acount,          # current association index
                            i,               # tail entity index
                            adjlist[k])      # head entity index

              calls <- calls + 1
            }
        }
    }

  # return
  invisible(calls)
}

# ---------------------------------
#  function : xem.breadboard
# ---------------------------------
#  description  : create a breadboard plot
#  role         : model visualization
#  takes        : data structure 'conex' and optional annotation and colorway arguments
#  returns      : void
#  techniques   : nested named list structure
#  status       : incomplete
#
#  in graph theoretic terms, 'conex' represents a
#  discriminated adjacency list with additional data --
#  comprising vertex labels, vertex class information,
#  and collective edge labels
#
#  in computer science terms, 'conex' is a two-deep
#  nested named list structure
#
#  the plot window layout is based on code from
#  plot function 'dotchart'
#
# ---------------------------------

xem.breadboard <- function (conex,                # discriminated adjacency list with additional data
                            main = "anonymous",   # optional main label (arrow count later added underneath)
                            cex  = par("cex"))    # character magnification (NOT passed to 'xem.makeArrow')
{
  # reporting
  message("  info : xem.breadboard : commencing")

  # some integrity checks
  if ( !is.list(conex) ) stop("conex is not a list")
  if ( length(conex) == 0 ) {
    message("  warn : xem.breadboard : conex is empty")
    warning("conex is empty")
  }

  # modify graphical parameters
  opar <- par("mai",                         # margin sizes in inches for c(bottom, left, top, right)
              "xaxs",                        # x-axis intervals
              "yaxs")                        # y-axis intervals
  on.exit(par(opar))
  par(xaxs     = "r")                        # "i" is without the 4% extension utilized by "r"
  par(yaxs     = "r")                        # "i" is without the 4% extension utilized by "r"

  # open a plot
  plot.new()                                 # create or start a new plot frame

  # define some fallbacks
  arrowcalls <- 0
  ylabels <- character()
  xlabels <- character()

  if ( length(conex) > 0 ) {                 # protect against non-data

    # extract label data
    entitys <- names(conex)
    assocs  <- character()                   # length zero string vector
    for ( i in 1:length(conex) )
      {
        assocs <- c(assocs, names(conex[[i]])[-1])     # skip class (the "[-1]")
      }

    # add leading counts
    if ( length(entitys) > 0 ) entitys <- sprintf("%02d \xb7 %s", seq(length(entitys)), entitys)
    if ( length(assocs)  > 0 ) assocs  <- sprintf("%02d \xb7 %s", seq(length(assocs)) , assocs)

    # more processing with empty list protection
    ylabels <- entitys
    xlabels <- assocs
    if ( length(ylabels) == 0 ) ylabels <- ""
    if ( length(xlabels) == 0 ) xlabels <- ""
    y       <- length(ylabels)
    x       <- length(xlabels)

    # reset margins
    linch   <- max(strwidth(ylabels,         # left margin in inches
                            unit = "inch"))
    nmai    <- par("mai")
    nmai[2] <- nmai[4] + linch + 0.0         # was + 0.1
    par(mai = nmai)

    binch   <- max(strwidth(xlabels,         # bottom margin in inches
                            unit = "inch"))
    nmai    <- par("mai")
    nmai[1] <- nmai[4] + binch + 0.0
    par(mai = nmai)

    # set and add axes
    ylim <- c(1, y)
    xlim <- c(1, x)
    plot.window(xlim = xlim, ylim = rev(ylim), log = "")

    height  <- par("csi")                    # character height in inches
    color   <- par("fg")

    loffset <- (linch + 0.1)/height
    mtext(ylabels,
          side = 2,
          line = loffset,
          at   = 1:y,
          adj  = 0,
          col  = color,
          las  = 2,
          cex  = cex)

    boffset <- (binch + 0.1)/height
    mtext(xlabels,
          side = 1,
          line = boffset,
          at   = 1:x,
          adj  = 0,
          col  = color,
          las  = 2,
          cex  = cex)

    abline(h   = 1:y,                        # horizontal gridlines
           lty = "dotted",
           col = "lightgray")

    # traverse data structure and add arrows (the real work)
    arrowcalls <- xem.breadTraverse(conex)   # traversal call

    # debugging support
    if ( DBUG > 1 ) {
      message("  info : adding axes and box to plot")
      axis(3)                                # add an axis to a plot, 3 = top
      axis(4)                                # add an axis to a plot, 4 = right
      box()                                  # draw a box around a plot
    }

  } # empty data protection

  # finalize and print main title
  main <- paste(main)
  title(main = main, xlab = "", ylab = "")

  # finalize and print ancillary message
  msg    <- character()
  msg[1] <- paste("xeona best mode =", xmode)
  msg[2] <- paste("associations =", as.character(arrowcalls))
  if ( xmode == "na" ) msgs <- msg[2]
  else                 msgs <- paste(msg, collapse = " / ")
  mtext(msgs, col = "darkslategray")

  message("  info : xem.breadboard : display ", msgs)
  message("  info : xem.breadboard : complete, arrow count ", arrowcalls)

  # housekeeping
  invisible(list(entitys      = ylabels,
                 associations = xlabels,
                 arrowcount   = arrowcalls))

}

# ---------------------------------
#  function : xem.breadplot
# ---------------------------------
#  description  : wrapper to the core calls
#  role         : job 8 point of contact
#  techniques   : see core calls
#  status       : complete
#
#  'cex' defaults to 1, useful values can be 0.9 and 0.8
#
# ---------------------------------

xem.breadplot <- function (recset,                # recset
                           main = "anonymous",    # optional main label (arrow count later added underneath)
                           cex  = par("cex"))     # character magnification (NOT passed to 'xem.makeArrow')
{
  # reporting
  message("  info : xem.breadplot : commencing")
  on.exit(message("  info : xem.breadplot : complete (on.exit)"))

  # code
  conex1  <- xem.createBreadset.1(recset)
  conex2  <- xem.createBreadset.2(conex1)
  capture <- xem.breadboard(conex2, main = main, cex = cex)

  # return
  invisible(capture)
}

# ---------------------------------
#  function : xem.breadplot.info
# ---------------------------------
#  description  : explain the prevailing color scheme
#  role         : strictly informational
#  status       : ongoing
# ---------------------------------

xem.breadplot.info <<- function ()           # CAUTION: exported
{
  msg <- "
current status (currently outdated)

 1   gray              starting point

 2   purple            Cx       out
 3   dark-blue         Cx        in
 4   mid-blue          Cm        in
 5   blue-green        Teas     out                 ambient-air-context, socket-electricity-commodity
 6   dark-blue         Gate     connection
 7   leaf-green        Asop     out                 technical-assets
 8   yellow-green      Asop      in                 asset-operators
 9   orange            Domain   out && Gate in      ranked-sel-gates
10   red               Overseer out                 ranked-orig-domains

 1   gray              head absent
 0   black             tail absent (error)

"
  cat(msg)
  invisible(msg)
}

# ---------------------------------
#  function : tst.breadboard
# ---------------------------------

tst.breadboard <- function (dbug)
{
  DBUG <<- dbug
  message("  * * * * * * * * * *\n  test : breadboard test commencing / DBUG = ", DBUG)

  conex <<-
    list("overseer"             = list(class                 = "Overseer"             ,
                                      "ranked-orig-domains"  = c(2   )                ) ,
         "domain-controller-1"  = list(class                 = "DomainController"     ,
                                      "ranked-sel-gates"     = c(    )                ,   # note 'c()'
                                      "asset-operators"      = c(3, 4)                ) , # 4 is for test purposes
         "asop-null-1"          = list(class                 = "AsopNull"             ,
                                      "technical-assets"     = c(    )                ) ,
         "cx-ambient-air-sim-1" = list(class                 = "CxAmbientAirSim"      ) )

  ids <- names(conex)
  cxs <- character()
  for ( i in 1:length(conex) )
    {
      cxs <- c(cxs, names(conex[[i]])[-1])   # skip class (the "[-1]")
    }

  print(ids)
  print(cxs)

  capture <<- xem.breadboard(conex,
                            main = paste("associations test plot / DBUG = ", DBUG),
                            cex  = 1.0)      # optional, scales 'mtext' labels only (not passed to 'xem.makeArrow')

  print(capture$entitys)
  print(capture$associations)
  print(capture$arrowcount)

  str(conex)                                 # CAUTION: neither 'cat' nor 'print' (which adds final NULL) needed

  message("  test : breadboard test complete")
}

# ---------------------------------
#  junk
# ---------------------------------
#
#  print(lapply(conex, names))
#
#     > Lst[5] <- list(Mat)
#     > names(Lst)[5] <- "matrix"
#
#  this adds the entire field: bread[[n <- n+1]] <- entity[["class"]]

#  $Id: breadplot.R 6005 2011-02-25 18:12:42Z robbie $
#  end of file

