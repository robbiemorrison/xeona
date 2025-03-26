//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : common.h
//  file-create-date : Mon 07-Nov-2011 10:21 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : header-only common code / header
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
//  Copyright : This software is copyright (c) 2007 - 2011 Robbie Morrison.
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xgrab-src/common.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Header-only file.

//  HEADER GUARD

#ifndef _COMMON_H_
#define _COMMON_H_

//  PREPROCESSOR MACROS FOR CODE-SWITCH PURPOSES

// compiler-specific code to obtain the name of this function
#ifdef __GNUG__                              // a GNU g++ compiler
# define FUNC __PRETTY_FUNCTION__
#else
# define FUNC  __func__
#endif

#endif // _COMMON_H_

//  end of file

