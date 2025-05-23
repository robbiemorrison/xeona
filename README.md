<!--
  author    : Robbie Morrison
  commenced : December 2017
  license   : this work is licensed under a Creative Commons Attribution 4.0 International License
-->

<img src="img/xeona-logo.png" width="15%" style="float: right">

# *xeona*

*xeona* is an **environment** for modeling **energy systems** that provides endogenous support for **multi&#8209;party operations**, least&#8209;cost **market clearing**, simplified **AC&nbsp;power&#8209;flow**, and capacitated **network effects**&nbsp;&mdash; plus exogenous **structural change** through user&#8209;defined **scenarios**.

*xeona* was completed in 2014, at which point active development ceased.

The source code, documentation, and development data are deposited here as an historical record.

## Overview

*xeona* represents one of the earlier attempts to combine **agent&#8209;based modeling** with classical optimization&#8209;informed **energy system modeling**.

*xeona* relies on object&#8209;oriented design and analysis (OODA) and was necessarily written from the ground&nbsp;up in a high&#8209;performance general&#8209;purpose programming language.

<figure style="margin-top: 4.0ex; margin-bottom: 2.0ex; text-align: center">
  <img src="img/ecos-2005-xeona-three-layer-21-for-egy-paper.internal-crop.png" width="100%" style="float: left; padding=10%"">
  <!-- <p style="margin-top: 0.0ex">&nbsp;</p> -->
  <!-- <figcaption><b>Figure&nbsp;1</b>: Key components divided into three realms</figcaption> -->
</figure>

**Figure&nbsp;1**: Key components divided into three realms

*xeona* offers a&nbsp;clear separation of responsibilities between technical characterizations, operator decisioning, and market&#8209;based dispatch.  Figure&nbsp;1 shows the general arrangements.  The implementation of *xeona* includes the following features:

- high&#8209;resolution technical modeling
- linear AC&nbsp;power&#8209;flow (with phase angles)
- ability to capture network saturation effects
- agent&#8209;based decisioning
- contractual relationships (considered structural)
- simplified [locational marginal pricing](https://en.wikipedia.org/wiki/Electricity_market#Bid-based,_security-constrained,_economic_dispatch_with_nodal_prices) (*aka* nodal pricing)
- structural change by means of exogenous scenarios
- solver&#8209;agnostic internal interface (shares some functionality with Linopy)

The system itself did not look ahead but the agents can&nbsp;&mdash; just as in real life.

<figure style="margin-top: 4.0ex; margin-bottom: 2.0ex; text-align: center">
  <img src="img/xem-key.crop.png" width="80%">
  <!-- <p style="margin-top: 0.0ex">&nbsp;</p> -->
  <!-- <figcaption><b>Figure&nbsp;2</b>: Some design features shown schematically</figcaption> -->
</figure>

**Figure&nbsp;2**: Some design features shown schematically

*xeona* supports the concept of distributed autonomy and the system is thus divided into commitment domains separated by gateway entities&nbsp;&mdash; as indicated in figure&nbsp;2.  The so&#8209;called CTA algorithm embedded in *xeona* then provides a novel way to traverse the system at each time&nbsp;step, incrementally capacitating and solving the domains and gateways whenever feasible until a complete solution is realized. Figure&nbsp;2 also shows the&nbsp;plug metaphor that is used to wire the system and define notional demand flow in the process.

*xeona* was designed to study [sector&#8209;coupling](https://en.wikipedia.org/wiki/sector_coupling) specifically and system integration under distributed decision-making more generally&nbsp;&mdash; both emerging topics when the software was conceived.

## Implementation

*xeona* was written as production software.

*xeona* was implemented using 58&thinsp;000 source lines of C++ with ancillary processing and visualization coded in R and bash.  The main code is located in the `xeona1` directory tree.

The source code is licensed under the GNU&nbsp;GPLv3+ license and the documentation and data are under the Creative Commons CC&#8209;BY&#8209;4.0 licenses.

## Publications

Related publications:

- Morrison, Robbie, Tobias Wittmann, Jan Heise, and Thomas Bruckner (2005).  In Kjelstrup, Signe, Johan&nbsp;E Hustad *et&nbsp;al* (editors).  [*Chapter: Policy-oriented energy system modeling with 'xeona'*](https://www2.wifa.uni-leipzig.de/fileadmin/user_upload/iirm-tm/energiemanagement/publikationen/Conference_Proceedings/2005_PolicyOrientedEXeona.pdf).  Trondheim, Norway: Tapir Academic Press.  pp 659–668.  ISBN 82-519-2041-8.  Conference held Norwegian University of Science and Technology (NTNU) on <nowrap>20–22 June</nowrap> 2005.

- Bruckner, Thomas, Robbie Morrison, and Tobias Wittmann (2005).  "Public policy modeling of distributed energy technologies: strategies, attributes, and challenges".  *Ecological Economics*.  **54**&nbsp;(2–3):&nbsp;328–245.  ISSN [0921-8009](https://www.worldcat.org/issn/0921-8009).  doi:[10.1016/j.ecolecon.2004.12.032](https://doi.org/10.1016/j.ecolecon.2004.12.032).

- Morrison, Robbie, Tobias Wittmann, and Thomas Bruckner (2004).  [*Energy sustainability through representative large-scale simulation: the logical and physical design of 'xeona'*](https://www.thesustainabilitysociety.org.nz/conference/2004/Session5/37%20Morrison1.pdf).  Auckland, New Zealand: International Conference on Sustainability Engineering and Science (ICSES).  Conference held Sheraton Auckland on <nowrap>6–9 July</nowrap> 2004.

- Morrison, Robbie (14&nbsp;October 2010).  [*Core codebase of the xeona energy systems modeling environment — revision 5314*](https://zenodo.org/record/4817704/files/xeona.r5314.code0.pdf).  doi:[10.5281/zenodo.4817704](https://doi.org/10.5281/zenodo.4817704).  PDF version.  Zenodo upload 27 May 2021.  Contains 197&nbsp;source files and circa 58&#8239;000 source lines of code of C++.  1164&nbsp;pages.

<br>

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width: 0" src="https://i.creativecommons.org/l/by/4.0/88x31.png"/></a><br/>Copyright (c) Robbie Morrison<br/>This file is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

Text last modified <nowrap>12 May 2025</nowrap>.

&#9634;

<!-- eof -->
