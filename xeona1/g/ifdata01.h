//  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT
//
//  file-create-name : ifdata01.h
//  file-create-date : Thu 03-Feb-2011 11:53 UTC
//  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
//
//  file-role        : lake inflow data file
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
//  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/g/ifdata01.h $
//
//  GENERAL NOTES FOR THIS FILE
//
//  Stand-alone file
//
//      This file contains hard-coded data.  It has no
//      accompanying implementation file.
//
//  Dependencies
//
//      All header dependencies are taken care of by the client
//      unit 'c/flowset'.

//  HEADER GUARD

#ifndef _IFDATA01_H_
#define _IFDATA01_H_

//  DATA

//  note that this data also contains a few lines of C++ code to
//  pack it into a 'InflowSet' object and then into a map
//  container, along with its key string

// ---------------------------------------------------------
//  DATASET         : Benmore, New Zealand / 1983
// ---------------------------------------------------------
//
//  Data source
//
//      The Benmore 1983 flow data was extracted from:
//
//          worksheets : 'Daily Data' sheet, 'Monthly' sheet
//          file       : 'Benmore 98614(4).xls'
//          last saved : 2010-01-21T00:07:34Z
//          md5sum     : c79edec313a89cce958bb164759ffa4b
//          archive    : 'Data-Excel-format-Jan10.zip'
//          web        : http://www.ea.govt.nz/
//                       industry/modelling/cds/hydrological-inflows-spectra-update-7/
//
//  Monitoring station details
//
//      This is one of two datasets for Lake Benmore, the other
//      being BEN_TP.  More information can be found in Tribe and
//      Morrow (2009).  The details for here are:
//
//          flowset    : BENMORE
//          site       : 98614
//          item       : 4
//          range      : 1931 - 2008
//
//  Flowset span, interval definition, and later resampling
//
//      The first entry covers 01-Jan-1983 and the last entry
//      covers 31-Dec-1983.  The entries are deemed to represent
//      midday readings -- which aligns with the mid-interval
//      assumption that pervades 'xeona'.
//
//      When later resampled by 'xeona::vectorFromDailyLin' the
//      first and last half days are extrapolated.
//
//  Public domain data
//
//      The New Zealand Electricity Association advise the hydro
//      data contained here is public domain and can be freely
//      published/circulated to other parties without referring
//      back to the Electricity Association.  (See emails from
//      Bruce Smith, General Manager Market Performance and Brian
//      Kirtlan, Senior Analyst, both dated 02 March 2011.)
//
//  Additional information
//
//      Herath etal (2011) provide the following separate data on
//      the Benmore reservoir:
//
//          annual energy output [GWh]    : 2200
//          surface area [ha]             : 7523.9
//          rainfall [mm/y]               :  460
//          evapotranspiration [mm/y]     :  454
//          open-water evaporation [mm/y] : 1153
//
//      where evaporation relates to the shallow-rooted pasture
//      that predated the dam.
//
//  References
//
//      Herath, Indika, Markus Deurer, David Horne, Ranvir Singh,
//        and Brent Clothier.  2011.  The water footprint of
//        hydroelectricity : a methodological comparison from a
//        case study in New Zealand.  Journal of Cleaner
//        Production.  (article in press).
//
//      Tribe, Helli and Francie Morrow.  2009.  Generator
//        SPECTRA update : issue 7 : data to 30 June 2008 --
//        Reference 350478.00.  Opus International Consultants
//        Ltd, Wellington, New Zealand.
//
// ---------------------------------------------------------

const std::string flowsetId  = "benmore1983";
const int         year       = 1983;

const std::string description
  = "Benmore 1983 inflow data from New Zealand Electricity Authority 2010 spreadsheet"
  " 'Benmore 98614(4).xls' and described in Tribe and Morrow (2009)";

// CAUTION: declaration then assignment for 'Boost.Assign'
std::vector<double> days;
days +=
   144.9,  129.7,  126.8,  129.7,  127.6,  157.5,  140.3,  184.4,  133.6,  124.8,
   387.5,  584.8,  846.5, 1070.0,  550.4,  449.5,  405.8,  307.4,  281.7,  241.3,
   194.1,  169.2,  244.6,  186.3,  175.2,  164.3,  168.7,  156.3,  142.6,  140.9,
   129.2,  130.4,  132.8,  114.8,  125.5,  172.4,  167.5,   82.8,  118.3,   96.9,
   104.6,   80.5,   91.1,   89.3,   88.4,   93.7,   65.1,   93.8,   84.2,   82.5,
    99.0,   95.2,   84.2,   81.2,   82.0,   66.0,   70.6,   71.5,   71.7,   67.5,
    78.7,   73.7,   57.6,   70.7,   74.2,   51.5,   64.3,   61.8,  102.9,  459.4,
   268.7,  148.9,  125.5,  123.4,  117.7,  161.2,   87.1,   77.7,   68.9,   55.9,
    61.2,   59.2,   66.7,  137.9,  348.4,  160.4,  218.5,  186.7,  158.3,  127.7,
   114.1,   97.9,  121.9,  113.9,   98.5,   88.3,   92.2,  300.0,  208.6,  142.8,
   117.0,   96.5,   86.8,   76.0,  113.0,  289.0,  223.0,  155.3,  129.2,  110.3,
   103.5,  116.4,  109.4,   97.9,   89.5,   80.7,   94.3,  233.6,  166.1,  129.0,
   102.1,  102.7,   86.4,   78.3,   78.6,  274.4,  450.9,  266.1,  185.8,  361.7,
   617.1,  322.2,  237.5,  188.1,  159.7,  147.4,  132.8,  281.9,  198.5,  190.7,
   139.6,  126.2,  112.5,   99.7,   94.7,   87.0,   82.3,   75.7,   74.6,   72.1,
    89.7,  140.2,  114.3,   94.3,   75.6,   75.9,   69.3,   67.6,   67.1,   62.9,
    62.4,   59.0,   60.7,   58.3,   64.6,   94.8,  113.1,   98.2,   77.2,   79.3,
    67.2,   60.4,   61.2,   56.8,   56.9,   55.8,   56.7,   72.6,  239.2,  164.0,
   148.9,  101.4,   88.3,   81.3,   71.3,   64.3,   67.5,   61.2,   56.3,   75.0,
   190.7,  248.9,  168.8,  129.6,  106.5,   94.0,   86.7,   77.6,   75.3,   68.2,
    67.2,   64.3,   62.1,   62.7,   63.5,   47.3,   57.0,   56.0,   53.5,   54.9,
    53.5,   55.9,  157.4,  332.0,  201.8,  165.3,  153.5,  130.7,  112.6,   99.1,
   100.5,  125.1,  223.3,  152.4,  143.1,   96.5,  100.5,   90.6,   88.0,   76.6,
    92.0,   72.5,   84.5,  123.0,   74.3,   82.6,   72.7,   67.7,   67.6,   63.3,
    64.2,   61.9,   59.3,   57.3,   74.8,   50.8,   62.6,   61.2,   53.7,   50.4,
    76.6,  124.4,  121.3,  115.9,  111.8,  139.4,  179.6,  142.3,  108.7,   98.5,
    90.6,   86.3,   78.6,   92.1,  281.7,  261.1,  234.8,  441.5,  281.4,  210.8,
   182.0,  167.3,  153.3,  188.3,  212.8,  182.2,  291.0,  249.0,  184.6,  160.5,
   290.7,  449.8,  558.7,  358.2,  260.3,  207.1,  172.8,  150.5,  142.3,  128.8,
   122.9,  124.1,  130.5,  195.8,  812.2,  607.6,  363.1,  291.0,  294.0,  300.3,
   275.0,  258.2,  237.1,  230.9,  232.7,  218.4,  188.7,  181.8,  172.8,  334.5,
   289.6,  231.7,  216.6,  216.0,  321.6,  423.9,  273.6,  225.0,  188.9,  184.0,
   168.7,  370.2,  652.8,  598.0,  488.7,  363.3,  291.1,  246.1,  216.8,  211.8,
   191.2,  181.1,  175.9,  177.5,  192.4,  182.3,  195.5,  237.9,  247.0,  216.8,
   209.8,  207.6,  409.3, 1116.0,  727.5,  466.3,  379.0,  356.0,  309.2,  274.8,
   270.1,  243.4,  235.1,  207.4,  198.1,  141.3,  127.0,  175.5,  170.5,  138.3,
   121.5,  155.5,  138.8,  129.0,  120.6;
// days += 88.3; // entry number 366

std::vector<double> months;                  // long-run (1931-2008) monthly averages
months +=
  166.5,   140.9,  124.2,  117.2,  106.8,   88.6,   70.9,   76.8,   99.9,  147.4,
  172.9,   191.8;

// create an 'InflowSet' and then add this to the 'infosets' map
shared_ptr<InflowSet> inflowset(new InflowSet(description, year, days, months));
inflowsets->insert(std::make_pair(flowsetId, inflowset));

//  DATA ENDS

#endif // _IFDATA01_H_

//  end of file

