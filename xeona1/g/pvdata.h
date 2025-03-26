//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : pvdata.h
//  file-create-date : Mon 10-Jan-2011 13:52 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : Sandia model data file
//  file-status      : working
//  file-release-tag :
//
//  LEGAL NOTICE
//
//  Software  : This file is part of the source code for the xeona energy
//              systems modeling environment.
//  License   : This software is distributed under the GNU General Public
//              License version 3, a copy of which is provided in the text
//              file LICENSE_GPLv3.
//  Warranty  : There is no warranty for this software, to the extent permitted
//              by applicable law.  Refer to the license for further details.
//  Copyright : This software is copyright (c) 2007 - 2012 Robbie Morrison.
//  Request   : The software is distributed with the request that you forward
//              any modifications you make to the xeona project for possible
//              inclusion in the main codebase.
//
//  PROJECT CONTACT
//
//  Robbie Morrison
//  Institute for Energy Engineering
//  Technical University of Berlin
//  Marchstrasse 18, D-10587 Berlin, Germany
//  Email: robbie@actrix.co.nz
//
//  SVN VERSION CONTROL
//
//  $Author: robbie $
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/g/pvdata.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Stand-alone file
//
//      This file contains hard-coded data.  It has no
//      accompanying implementation file.
//
//  Background
//
//      This data supports the Sandia Photovoltaic Array
//      Performance Model current 2010.
//
//      This file is the data file for unit 'h/sandia' and class
//      'PvModule'.  Please read the documentation there for
//      details.  In particular, refer to member function
//      'PvModule::slurp' for formatting requirements.
//
//  Data source (data format current 2010)
//
//      The primary data was shipped with the Solar Advisor Model
//      (SAM, see below) version 2010.11.9 and was also available
//      as a spreadsheet:
//
//        * 'C:/SAM/2010.11.9/exelib/libraries/SandiaModules.samlib'
//        * 'SandiaModuleDatabase_20100730.xlsx'
//
//      The 'xlsx' file can be successfully opened by OpenOffice
//      3.2.0 and, by extension, LibreOffice.  The 'samlib' file
//      is human-readable text.  If need be, the SAM Windows
//      win32 executable can be installed under Linux/Wine in
//      order to extract the required 'samlib' file.
//
//      The spreadsheet option was used here.
//
//  Legal status
//
//      The first paragraph of the disclaimer that ships with SAM
//      version 2010.11.9 states:
//
//         "DISCLAIMER
//
//          The System Advisor Model ("Model") is provided
//          by the National Renewable Energy Laboratory
//          ("NREL"), which is operated by the Alliance for
//          Sustainable Energy, LLC ("Alliance") for the
//          U.S. Department Of Energy ("DOE") and may be
//          used for any purpose whatsoever."
//
//      The above text is taken from the about screen.  The same
//      text is given in the user manual 'exelib/help/user_guide.pdf'.
//      I could not find any other legal information in the
//      install branch, including claims over copyright.
//
//      The above text suggests that the data has been placed in
//      the public domain -- a position which also aligns with
//      normal US Government policy on publicly-funded research
//      outputs.
//
//      Consequently, the 'xeona' GNU GPLv3 license does not
//      apply to the data contained in this file.
//
//  File history
//
//      As of commit r5634 of 18-Jan-2011, the pre-2004 format
//      applies.  After commit r5638 of 19-Jan-2011, the 2004
//      format applies!  See the commit r5679 version of this
//      file for more details about formats.

//  HEADER GUARD

#ifndef _PVDATA_H_
#define _PVDATA_H_

// ---------------------------------------------------------
//  documentation   : Data source
// ---------------------------------------------------------
//
//  The primary source used here has the following attributes:
//
//      filename  : SandiaModuleDatabase_20100730.xlsx
//      size      : 149 860 bytes
//      format    : XML document text (Zip archive data, at least v2.0 to extract)
//      mimetype  : application/zip
//      file-ext  : Microsoft Excel 2007/8 XML spreadsheet (often Mac OS X)
//      details   : okay, 14 files, 83% ratio
//      dated     : 2011-01-18 (Linux file last modified day)
//      md5sum    : 5da1c5a59824f2cfe44e89ebed83fd2c
//
//  The spreadsheet file was emailed to me (Robbie Morrison) by
//  Chris Cameron <cpcamer@sandia.gov>, Sandia National
//  Laboratories, on 17-Jan-2011.
//
//  The "E" in the vintage column indicates that primary module
//  performance parameters (Isco, Impo, Voco, Vmpo) are from the
//  manufacturer's datasheet.
//
//  The manufacturer's datasheet is available from:
//
//      http://www.siemen.co.uk/sm55.pdf
//
// ---------------------------------------------------------

static const std::string csvSiemensSM55 = "\"Siemens Solar SM55\",\"2002 (E)\",0.425,\"c-Si\",36,1,3.45,21.7,3.15,17.4,0.00045,-0.00014,1.02,-0.02,-0.076,0,-0.076,0,1.289,0.004416,-7.5891,0.938,0.054228,-0.0099032,0.00072969,-0.000019074,1,-0.002438,0.0003103,-0.00001246,2.11E-07,-1.36E-09,3,1,-3.56,-0.075,0.998,0.002,3.4,2.25,1.122,-0.122";

#endif // _PVDATA_H_

//  end of file

