
#  file-purpose     : makefile for 'xstat'
#  file-initiator   : Robbie Morrison <robbie@actrix.co.nz>
#  file-create-date : Tue 15-Nov-2011 12:18 UTC
#  file-status      : work-in-progress
#  file-keywords    : xeona

#  $Revision: 7966 $
#  $Date: 2011-11-15 13:28:17 +0100 (Tue, 15 Nov 2011) $
#  $Author: robbie $
#  $URL: file:///home/robbie/svn-root/xeona/futz/trunk/bin/xstat-src/makefile $

#  Usage
#
#     This file can be deployed in two ways:
#
#         $ make       # as is normal
#         $ makeme.sh  # accessed thru a bash script
#
#     In the latter case, -Weffc++ and -pedantic are
#     also applied, together with some grep screens to
#     combat the resulting avalanche of warnings from
#     various 'faulty' Boost and system headers.
#
#  CAUTION: current directory need NOT be on search PATH
#
#     Option 'memchk' no longer assumes the current
#     directory is on the search PATH.
#
#  CAUTION: valgrind utility required for memory checking
#
#      Otherwise, delete the 'valgrind' command and call
#      the target directly as a simple execute check.
#
#  CAUTION: Boost headers and libs
#
#      Ensure that the Boost header and library searches
#      refer to the same Boost release!  This can be a
#      problem if you have more than one release installed.
#      Use the following to find out:
#
#         $ ldd xgrab

MAKEFILE =  makefile
OBJECTS  =  stats.o main.o
TARGET   =  xstats
DEBUG    =
WARNINGS =
OPTIMS   = -O2
LIBS     =
SEARCH   =
RUNARGS  =

all: start cleanall echo $(TARGET) memchk         # CAUTION: note 'cleanall' as first call!
# all: start               $(TARGET) memchk         # CAUTION: note 'cleanall' as first call!

echo:
	@echo

start:
	@echo "commencing build of '$(TARGET)'"
	@echo

$(TARGET): $(OBJECTS)
	g++ $(DEBUG) $(WARNINGS) $(OPTIMS) $(SEARCH) -o $(TARGET) $(OBJECTS) $(LIBS)
	@echo

stats.o: smart_ptr.h common.h stats.h stats.cc
	g++ $(DEBUG) $(WARNINGS) $(OPTIMS) $(SEARCH) -c -o   stats.o   stats.cc

main.o: smart_ptr.h common.h stats.h main.cc
	g++ $(DEBUG) $(WARNINGS) $(OPTIMS) $(SEARCH) -c -o    main.o    main.cc

memchk:
	@valgrind ./$(TARGET) $(RUNARGS); echo "valgrind return (probably from $(TARGET)) : $$?" || true
	@echo

clean:
	rm --force $(OBJECTS)

cleanall: clean
	rm --force $(TARGET)

check:
	@echo
	textcheck *.h *.cc || true
# 	echo && git status --short
# 	@echo
	echo && svn status
	@echo
	echo && ls --format=long *.o $(TARGET) 2>/dev/null || true
	@echo

#  $Id: makefile 7966 2011-11-15 12:28:17Z robbie $
#  end of file





#  $Id: makefile 7966 2011-11-15 12:28:17Z robbie $
#  end of file


