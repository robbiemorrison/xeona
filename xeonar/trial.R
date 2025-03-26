
#  file-purpose     : simple trial script for 'xem.job'
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Fri 05-Jun-2009 09:17 UTC
#  file-status      : ongoing
#  file-keywords    : xeona R

#  $Revision: 6005 $
#  $Date: 2011-02-25 19:12:42 +0100 (Fri, 25 Feb 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeonar/trial.R $

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
#    > source("trial.R")

# ---------------------------------
#  commencement
# ---------------------------------

message("  file : trial.R : starting")

robbie.clean()
robbie.killX11s()

source(file.path(Sys.getenv("XEONAR"), "xem.R"))

# ---------------------------------
#  specify external files
# ---------------------------------

xeona   <- "../xeona1/xeona.mach"

xemfile <- "../xeona1/xeona-xmoks/test-context.xem"
xemfile <- "../xeona1/xeona-xmoks/experiment-01.xem"

# ---------------------------------
#  call job
# ---------------------------------

xem.job(xemfile,
        xeona,
        dbug = 0)                            # debug level in { 0, .. 4 }

# ---------------------------------
#  completion
# ---------------------------------

rm(list = c("xem.job"))
rm(list = c("xeona", "xemfile"))             # from here
# rm(list = c(""))                             # from 'xem.R'

message("  file : trial.R : finishing")

#  $Id: trial.R 6005 2011-02-25 18:12:42Z robbie $
#  end of file

