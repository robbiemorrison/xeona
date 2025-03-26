
#  file-purpose     : test suite - source directly to run
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Thu 04-Jun-2009 20:46 UTC
#  file-status      : ongoing
#  file-keywords    : xeona R

#  $Revision: 6005 $
#  $Date: 2011-02-25 19:12:42 +0100 (Fri, 25 Feb 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeonar/test.R $

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

#  ADDITIONAL REQUIREMENTS
#
#    userFuncs.R    # normally loaded automatically
#
#  USAGE
#
#    > source("test.R")

# ---------------------------------
#  preamble
# ---------------------------------

message("  file : test.R : commencing")
keep <- c("keep")                            # keep me too!

keep     <- c(keep, "BROWSER", "BESTMODE")
BROWSER  <- TRUE                             # better than ask = T
BESTMODE <- 0                                # 'xeona' best mode state, should be overwritten

opar <- par(no.readonly = TRUE)              # all par settings which could be changed
keep <- c(keep, "opar")
# par(ask = F)                               # T = prompt, F = rush past
# par(new = T)

# ---------------------------------
#  utilities test
# ---------------------------------

robbie.clean(keep)                           # CAUTION: note the clean calls in this script
source("utils.R")
tst.utils(dbug = 1)
if ( BROWSER ) browser()
plot.new()
keep <- c(keep)

# ---------------------------------
#  breadboard test
# ---------------------------------

# dbug 4 = print color test and (currently) stop

robbie.clean(keep)
source("utils.R")
source("breadplot.R")
xmode <- 0                                   # for test purposes
tst.breadboard(dbug = 1)
if ( BROWSER ) browser()
plot.new()
keep <- c(keep, "capture", "conex")          # 'conex' is hardcoded rather than generated

# ---------------------------------
#  data processing test
# ---------------------------------

# relatively hollow, test with point of entry test instead

robbie.clean(keep)
source("dataproc.R")
tst.dataproc(xemfile = "test.xem",           # hence 'test.guard.xem'
             dbug    = 1)
if ( BROWSER ) browser()
keep <- c(keep)

# ---------------------------------
#  extract test
# ---------------------------------

robbie.clean(keep)
source("extract.R")
tst.extract(dbug = 1)
if ( BROWSER ) browser()
plot.new()
keep <- c(keep)

# ---------------------------------
#  point of entry test
# ---------------------------------

robbie.clean(keep)
source(file.path(Sys.getenv("XEONAR"), "xem.R"))
xem.job(xemfile = "../xeona1/xeona-xmoks/test-context.xem",
        xeona   = "../xeona1/xeona.mach",
        mlabel  = "intentionally empty",
        dbug    = 1)
# if ( BROWSER ) browser()
# plot.new()
keep <- c(keep, "data", "policy", "recset")

# ---------------------------------
# report on lines of code
# ---------------------------------

source("utils.R")
message("  info : cat/grep/wc lines of code: ", xem.linesOfCode())

# ---------------------------------
#  housekeeping
# ---------------------------------

par(opar)
rm(list = "opar")

robbie.clean(keep, say = TRUE )              # 'keep' is the retains list

message("  file : test.R : finishing")

#  $Id: test.R 6005 2011-02-25 18:12:42Z robbie $
#  end of file

