#  XEONA ENERGY SYSTEMS MODELING ENVIRONMENT

#  file-purpose     : project makefile (requires clean svn, unlike 'mach')
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Wed 11-Apr-2007 12:10 UTC
#  file-status      : working
#  file-keywords    : xeona

#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/xeona1/makefile $

#  --------------------------------------------------------
#  fastdep
#  --------------------------------------------------------
#
#  This makefile utilizes the 'fastdep' utility (v0.16)
#  by Bart Vanhauwaert to generate file dependencies.
#  The equivalent scripted code using g++ -M and sed
#  calls is rather convoluted.  (If you wish to avoid
#  third party utilities, refer particularly to Stephens
#  etal (2006 p75-76) and to the GNU make info
#  documentation "4.14 Generating Prerequisites
#  Automatically" for sample code).
#
#  From 04-Feb-2010, an earlier fastdep file (as named
#  by 'fastdep_file') will be recycled if the fastdep
#  utility cannot be located.  This strategy assumes
#  that all '#include' statements remain unchanged and
#  should be considered a temporary measure.  Indeed, it
#  is highly recommended that fastdep be installed and
#  used.
#
#  If fastdep proves an impediment, it may be better to
#  replicate fastdep as a local script using g++ -M*
#  calls.)
#
#  --------------------------------------------------------
#  Overview
#  --------------------------------------------------------
#
#  In terms of references, Welsh etal (2003, pp437--447)
#  give a good review of make.  In addition, Stephens etal
#  (2006, pp 64-82) provide an excellent coverage (with
#  examples) of advanced topics like non-static rules and
#  command-line customization.  Alternatively, basic
#  multi-file makefiles are described by Oualline (1995,
#  pp 420-424).
#
#  The GNU info documentation is very comprehensive.  See,
#  in particular, "10 Using Implicit Rules"
#
#  --------------------------------------------------------
#  References for make
#  --------------------------------------------------------
#
#  Standard documentation for GNU make
#
#  Oualline, Steve.  1995.  Practical C++ programming
#    -- First edition.  O'Reilly and Associates,
#    Sebastopol, USA.  ISBN 1-56592-139-9.
#
#  Stephens, D Ryan, Christopher Diggins, Jonathan
#    Turkanis, and Jeff Cogswell.  2006.  C++ cookbook.
#    O'Reilly and Associates, Sebastopol, USA.  ISBN
#    0-596-00761-2.
#
#  Welsh, Matt, Matthias Kalle Dalheimer, Terry Dawson,
#    Lar Laufman.  2003.  Running Linux -- Fourth edition.
#    O'Reilly and Associates, Sebastopol, USA.  ISBN
#    0-596-00272-6.  [see above]
#
#  --------------------------------------------------------
#  GNU make options
#  --------------------------------------------------------
#
#    --jobs=4
#        better utilize 4 core processors, results in a
#        more than 200% speed up on the Intel Core i5
#
#        certain caveats apply so omit on release builds
#
#  --------------------------------------------------------
#  GNU g++ 4 compiler options
#  --------------------------------------------------------
#
#    -###
#        like -v except the commands are not executed and
#        all command arguments are quoted, will probably
#        return a fail exit status (perhaps 2) if the linker
#        is invoked
#
#    -v
#        print (on standard error output) the commands
#        executed to run the stages of compilation -- also
#        print the version number of the compiler driver
#        program and of the preprocessor and the compiler
#        proper
#
#    -g
#        produce debugging information in the operating
#        system's native format (stabs, COFF, XCOFF, or
#        DWARF 2), also okay for GDB
#
#    -ggdb
#        produce debugging information for use by GDB -- this
#        means to use the most expressive format available
#
#    -fno-inline
#        ignore the "inline" keyword (effective only under
#        optimization)
#
#    -O3
#        maximum level of optimization for speed
#
#    -Wall
#        enable all warnings about questionable
#        constructs, see also -Wextra for constructs not
#        often regarded as questionable
#
#    -pedantic [-Wno-long-long]
#        issue all the warnings demanded by strict ISO C
#        and ISO C++ [Boost 1.35 and better requires long long
#        errors to be suppressed]
#
#    -Weffc++
#        warn about bad style as defined in Scott Meyers'
#        Effective C++ book (may also need to
#        | grep --invert-match "boost::" and so forth)
#
#    -Wextra
#        warn if pointer compared to zero, enums and
# 	 non-enums compared, base class not initialized
# 	 by derived class, and other matters
#
#
#  --------------------------------------------------------
#  Third-party libraries
#  --------------------------------------------------------
#
#  All have installed as pre-built ubuntu packages, but some
#  may later have been locally built to yield a newer release.
#
#    libglpk                   : Andrew Makhorin's MILP solver
#    lboost_filesystem         : Boost filesystem operations
#    lboost_regex              : Boost regular expression support
#    libboost_date_time        : Boost date and time support
#    libboost_program_options  : Boost command-line and config file parser
#
#  --------------------------------------------------------

#---------------------------------
# internal variables
#---------------------------------

# NOTE: naming convention: as recommended in the GNU
# documentation for make:
#
#   internal variables : lowercase
#   external variables : uppercase

# CAUTION: best not to add trailing comments to
# definition statements

# binary         : name of binary
# binary_opts    : options to be passed to binary at run-time (no double quotes)
# common         : stem for common header
# install_dir    : subdirectory to install program
# fastdep_file   : file generated by fastdep
# clean_exts     : list of file extensions to remove
# max_depth      : 'find -maxdepth' level, 1 = base director only
# script         : used only for console messages

  binary       = xeona.make
  binary_opts  = --beep 1 --report 6 --inbuilt 6

  common       = common
  install_dir  = ./bin
  fastdep_file = dependsfile

  clean_exts   = .o
  max_depth    = 2

  script       = $(firstword $(MAKEFILE_LIST))

#---------------------------------
# external variables
#---------------------------------

# Usage notes:
#
#  * simply comment out unwanted definitions (best to
#    use a double ##)
#
#  * or redefine on the command-line, for instance to
#    cancel -D_XDEBUG or -D_XSVNREV, use respectively
#
#      $ make CPPFLAG_DEBUG=  [all]
#      $ make CPPFLAG_SVNREV= [all]
#
#  * to combine command-line and makefile settings, use
#    the 'override' directive and the += operator (see
#    GNU manual, section 6.7)
#
# Note: implicit variables from make are ALWAYS
# uppercase, so too should any user-declared variables
# that might need to be overwritten on the command-line
# (although this second guideline was not adhered to
# here in respect of 'sources'!)

# The application will record the prevailing subversion
# revision if the macro '_XSVNREV' is set to a valid
# integer.  The following code does this iff the
# revision control is up-to-date.  Note that only
# 'common.cc' need be rebuilt -- and that always
# happens due to the 'touch' command later.  Note also
# the word "shell" in the following statement

# The code stanza below sets RES to zero if the
# 'svnversion' call returns the phrase "exported".
# This phrase means that the makefile no longer resides
# within a directory under subversion version control.
# For more information on syntax, see info make.

  # RES = $(shell svnversion .. 2>/dev/null)
  # ifndef $(RES)          # svnversion not present
  #   RES = 0
  # endif
  # ifeq ($(RES),exported) # present but outside repository
  #   RES = 0
  # endif
  CPPFLAG_SVNREV = -D_XSVNREV=0

# CAUTION: do not use trailing comments in the above
# assignments!

# To bypass the above protective code for testing
# purposes, use the following macro:
#
#   $ make CPPFLAG_SVNREV="-D_XSVNREV=0"

# The debug flag is also independently defined

  CPPFLAG_DEBUG  += -D_XDEBUG

# CXX        : compiler call          defaults to g++
# CXXFLAGS   : compiler flags         defaults to nil
# CPPFLAGS   : preprocessor flags     defaults to nil
# LDFLAGS    : linker flags           defaults to nil
# LDLIBS     : link-time libraries    defaults to nil

# Note GCC linker option -static for static linking,
# which can be enacted via LDFLAGS.

# The following debug build options are hard-coded
# '_XTCOLS' on 'hinau' now 166, was 142 for 'sojus'

  CXX         = g++

  CXXFLAGS   += -Wall
  CXXFLAGS   += -ggdb
  CXXFLAGS   += -O0
  CXXFLAGS   += -fno-inline

  CPPFLAGS   += $(CPPFLAG_DEBUG)
  CPPFLAGS   += $(CPPFLAG_SVNREV)
  CPPFLAGS   += -D_XTCOLS=166

  LDFLAGS    +=

# A cross-compiling trial using mingwin on Linux (by
# Peter Salhmann) normally deploys the following make
# two options:
#
#     CROSS="i686-pc-mingw32-"
#     -I "/opt/mingw/usr/i686-pc-mingw32/include"
#
# Given the above, the following code ensures that the
# cross-compiled binary will NOT be linked with shared
# libraries.  And also that a different binary naming
# convention will be used.

  ifdef CROSS                 # cross-compile
    LDFLAGS += -static
  endif

  ifdef CROSS		      # cross-compile
    ifeq ($(RES),0)
      binary = xeona.exe
    else
      binary = xeona$(RES).exe
    endif
  endif

# The GNU MP bignum library (libgmp) is required
# because the GLPK library (libglpk) was configured
# using --with-gmp.  If needed, bignum support can be
# omitted.

  LDLIBS     += -lboost_iostreams
  LDLIBS     += -lboost_date_time
  LDLIBS     += -lboost_filesystem
  LDLIBS     += -lboost_program_options
  LDLIBS     += -lboost_regex
  LDLIBS     += -lboost_system
  LDLIBS     += -lglpk
# LDLIBS     += -lgmp

# The POSIX threads library (lpthread) needs to be
# added to LDLIBS if LDFLAG contains '-static'.  The
# '-static' option prevents the compiler linking with
# shared libraries, in which case 'ldd' then reports
# "not a dynamic executable".  The following stanza
# simply relieves the caller of the need to modify
# LDLIBS under these circumstances.  Note that 'xeona'
# itself is NOT multi-threaded.

  ifneq (,$(findstring -static,$(LDFLAGS)))
    LDLIBS   += -lpthread
  endif

# the Boost libraries link to release 1.35 and better --
# to backtrack to release 1.34.1, remove the trailing
# "-mt" and set CPATH="/usr/local/include/boost-1_34_1"

# explicit linking with 'libgmp' was required from GLPK
# 4.33, but only when GLPK is configured with
# '--with-gmp'

#---------------------------------
# source list
#---------------------------------

# CAUTION: 'make' will stop at, rather than jump, the
# first commented out line, also leave space between
# filename and slash or 'make' will crunch the entries
# together

sources = \
	./$(common).cc \
	a/appinfo.cc \
	a/exapp.cc \
	a/exbase.cc \
	a/exent.cc \
	a/exitstat.cc \
	a/floatstat.cc \
	a/helpers.cc \
	a/logger.cc \
	a/recorder.cc \
	a/xedocs.cc \
	a/xemopt.cc \
	a/yeek.cc \
	b/actor.cc \
	b/asop.cc \
	b/asop01.cc \
	b/asop02.cc \
	b/asop03.cc \
	b/asop04.cc \
	b/asop05.cc \
	b/auxs01.cc \
	b/bandtaf.cc \
	b/block.cc \
	b/builtins.cc \
	b/commods.cc \
	b/commods01.cc \
	b/costreg.cc \
	b/domcon.cc \
	b/entity.cc \
	b/gate.cc \
	b/gate01.cc \
	b/junc.cc \
	b/junc01.cc \
	b/junc02.cc \
	b/junc03.cc \
	b/junc04.cc \
	b/lmpbid.cc \
	b/node.cc \
	b/node01.cc \
	b/node02.cc \
	b/node03.cc \
	b/node04.cc \
	b/node05.cc \
	b/node06.cc \
	b/optctl.cc \
	b/optgate.cc \
	b/optjunc.cc \
	b/optnode1.cc \
	b/optnode2.cc \
	b/optops1.cc \
	b/optops2.cc \
	b/optprob.cc \
	b/overseer.cc \
	b/propdata.cc \
	b/register.cc \
	b/teas.cc \
	b/teas01.cc \
	b/teas02.cc \
	b/teas03.cc \
	b/teas04.cc \
	b/teas05.cc \
	b/teas06.cc \
	b/teas07.cc \
	b/teas08.cc \
	b/teas09.cc \
	b/teas10.cc \
	b/teas11.cc \
	b/teas12.cc \
	b/teas13.cc \
	b/teasdev.cc \
	b/tests.cc \
	b/tictoc.cc \
	c/buildinfo.cc \
	c/conex.cc \
	c/costs.cc \
	c/datio.cc \
	c/extunits.cc \
	c/factory.cc \
	c/files.cc \
	c/fincalc.cc \
	c/flowset.cc \
	c/ghouse.cc \
	c/hydro.cc \
	c/inbuilt.cc \
	c/label.cc \
	c/linklog.cc \
	c/opssched.cc \
	c/recset.cc \
	c/si3units.cc \
	c/simcall.cc \
	c/stats.cc \
	c/suncalc.cc \
	c/tsalgos.cc \
	c/tsops.cc \
	c/tsset.cc \
	c/util1.cc \
	c/util2.cc \
	c/util3.cc \
	c/util4.cc \
	c/wxinfo.cc \
	c/xemgen.cc \
	c/xeona_ptr.cc \
	c/yearcalc.cc \
	d/glpkviz.cc \
	d/siglp.cc \
	e/context.cc \
	e/cxamb01.cc \
	e/cxamb02.cc \
	e/cxamb03.cc \
	e/cxecon01.cc \
	e/cxpol01.cc \
	f/cta.cc \
	f/gatesreg.cc \
	f/ospinfo.cc \
	f/trav.cc \
	h/plank.cc \
	h/r134a.cc \
	h/sandia.cc \
	h/thrmperf.cc \
	i/gnuplot.cc \
	./main.cc \

#---------------------------------
# unaccompanied headers list
#---------------------------------

# list used for information requests only, does not
# impact on the build at all -- note that the 'g'
# subdirectory datafiles (normally huge) are not given
# here

unheaders = \
	./sasserts.h \
	a/logger_fwd.h \
	a/license.h \
	c/smart_ptr.h \
	f/ospmodes.h \

#---------------------------------
# phony and suffix lists
#---------------------------------

# the phony target prevents similarly named files from
# blocking the execution of makefile targets

.PHONY: .compile_start .confirm_fastdep .depends .cross_echo .sources_echo
.PHONY: all ccs check clean deps h help inst ls rmbin tidy uninst

# reset suffix list

.SUFFIXES:
.SUFFIXES: .cc .o

#---------------------------------
# check for fastdep (used later)
#---------------------------------

.confirm_fastdep:
	@which fastdep >/dev/null \
          || echo "$(script): WARN: fastdep utility not present"

#---------------------------------
# build commands
#---------------------------------

# the touch common.cc call ensures that the xeona::DBUG
# variable, in turn set by the _XDEBUG preprocessor
# macro, will propogate through the app at link-time

objects = $(patsubst %.cc, %.o, $(sources))  # simple file extension substitution

# overarching call -- note use of reporting targets for timely information

all: .confirm_fastdep .cross_echo .sources_echo .depends .compile_start $(binary)
	@echo "$(script): completion of makefile target: all"

.cross_echo:
  ifdef CROSS
	@echo "$(script): CROSS: $(CROSS)"
  endif

.sources_echo:
	@echo "$(script): sources: $(sources)"

.compile_start:
	@echo "$(script): touching $(common).cc"; \
	  touch $(common).cc
	@echo "$(script): building objects" # "(perhaps)"

# linking call

$(binary): $(objects)
	@echo "$(script): linking binary: $(binary)"
	@sleep 1 # to ensure binary is listed oldest on ls -t (sort by modification time)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LDLIBS)

# compilation calls -- often implicit but hardcoded here for safety's sake

%.o: %.cc
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c -o $@  $<

#---------------------------------
# dependency list generation
#---------------------------------

# CAUTION: "Put the code to generate dependencies at
# end of your makefile" (Stephens etal, 2006, p75)

# the following code will recycle an earlier fastdep
# header dependencies file if fastdep itself cannot be
# located -- see the fastdep section (at the top of
# this file) for more information

# CAUTION: note the $$ when using bash command substitution

msg1 = fastdep header dependencies

.depends:
	@if test $$( which fastdep ); then \
          echo "$(script): executing fastdep"; fastdep $(sources) > $(fastdep_file); \
          else echo "$(script): WARN: unable to freshen $(msg1)"; fi

	@test -s $(fastdep_file) \
          || echo "$(script): ERROR: substantive $(msg1) file not found: $(fastdep_file)"

-include $(fastdep_file)  # leading - suppresses 'no such file' warnings under multi-pass

#---------------------------------
# utility calls
#---------------------------------

inst:
	@mkdir -p $(install_dir)
	@cp -p $(binary) $(install_dir)
	@echo "$(script): install complete"
	@ls -lF $(install_dir)/$(binary)

uninst:
	@rm --force --recursive $(install_dir)
	@echo "$(script): uninstall complete"

check:
	@echo "$(script): attempting to execute binary: $(binary) $(binary_opts)"
	@if test -x $(binary); then \
          echo; ./$(binary) $(binary_opts); echo "\n$(script): binary exit: $$?"; \
          else echo "$(script): $(binary) missing or not executable (test -x failed)"; fi

deps: .confirm_fastdep .depends
	@echo "$(script): using fastdep file: $(fastdep_file)"
	@echo "$(script): using sources: $(sources)"
	@echo
	@cat $(fastdep_file)
	@echo

ls:
	@echo -n "$(script): name: "
	@ls [Mm]akefile*

ccs:
	@echo $(sources) | awk 'BEGIN { RS = " "; print "" } { print $0 }'

hs:
	@echo $(unheaders) | awk 'BEGIN { RS = " "; print "" } { print $0 }'

#---------------------------------
# cleanup calls
#---------------------------------

tidy:
	@echo "$(script): removing empty files to depth $(max_depth)"; \
	  find . -maxdepth $(max_depth) -type f -empty -delete
	@for ext in $(clean_exts); \
	  do echo "$(script): removing *$$ext to depth $(max_depth)" ; \
	    find . -maxdepth $(max_depth) -type f -name "*$$ext" -delete ; \
	  done

rmbin:
	@echo "$(script): removing $(binary)"; \
	  rm --force $(binary)

clean: tidy rmbin

#---------------------------------
# help message
#---------------------------------

h: help   # short-cut to help

help:
	@ echo
	@ echo "  action:"
	@ echo "     [all]       rebuild as required"
	@ echo "     deps        update and cat fastdep file"
	@ echo "     tidy        remove created files but leave binary"
	@ echo "     rmbin       remove binary"
	@ echo "     clean       tidy + rmbin"
	@ echo "     inst        install program in $(install_dir)/"
	@ echo "     uninst      prune branch $(install_dir)/"
	@ echo "     check       attempt to run program"
	@ echo "  info:"
	@ echo "     help|h      this message"
	@ echo "     ls          list makefiles"
	@ echo "     ccs         list source files"
	@ echo "     hs          list unaccompanied header files"
	@ echo "  internal variables:"
	@ echo "     fastdep file (fastdep_file)   : $(fastdep_file)"
	@ echo "     binary name (binary)          : $(binary)"
	@ echo "     binary options (binary_opts)  : $(binary_opts)"
	@ echo "  external variables (as currently overwritten):"
	@ echo "     compiler call (CXX)           : $(CXX)"
	@ echo "     compiler flags (CXXFLAGS)     : $(CXXFLAGS)"
	@ echo "     preprocessor flags (CPPFLAGS) : $(CPPFLAGS)"
	@ echo "     linker flags (LDFLAGS)        : $(LDFLAGS)"
	@ echo "     link-time libraries (LDLIBS)  : $(LDLIBS)"
	@ echo "  problems with $(common).cc?"
	@ echo "     first, ensure that the revision control is up-to-date"
	@ echo "     then, try setting CPPFLAG_SVNREV to nil from the command-line"
	@ echo

#  --------------------------------------------------------
#  GNU make database (extract)
#  --------------------------------------------------------
#
#  special meaning    : LDFLAGS : extra flags to give linker
#  no special meaning : TARGET_ARCH LOADLIBES LDLIBS
#  not set by default : CXXFLAGS CPPFLAGS LDFLAGS TARGET_ARCH LOADLIBES LDLIBS
#
#  # Variables
#
#  # default
#  CXX = g++
#
#  # default
#  LINK.cc    = $(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS) $(TARGET_ARCH)
#
#  # default
#  COMPILE.cc = $(CXX) $(CXXFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c
#
#  # default
#  LINK.o = $(CC) $(LDFLAGS) $(TARGET_ARCH)
#
#  # default
#  OUTPUT_OPTION = -o $@
#
#  # Implicit Rules
#
#  %: %.cc
#  #  commands to execute (built-in):
#          $(LINK.cc) $^ $(LOADLIBES) $(LDLIBS) -o $@
#
#  %.o: %.cc
#  #  commands to execute (built-in):
#          $(COMPILE.cc) $(OUTPUT_OPTION) $<
#
#  --------------------------------------------------------

#---------------------------------
# junk code
#---------------------------------

# @fastdep --extraremakedep=$(script) --remakedeptarget=.depends $(sources) \
#   > $(fastdep_file)

# define a special CPPFLAG when building on 'sojus',
# the principal development host

#   MYSELF = $(shell hostname --long)
#   ifeq ($(MYSELF),sojus.fb10.tu-berlin.de)
#     CPPFLAGS += -D_XWEFF
#   endif

#  end of file

