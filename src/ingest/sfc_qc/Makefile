# User options are in src/include/makefile.inc
# This Makefile is designed for gnu make version 3.75 or newer
# Please report problems to lapsbugs@fsl.noaa.gov
#

SRCROOT=../../..


include $(SRCROOT)/src/include/makefile.inc


LAPSLIBS = $(SRCROOT)/src/lib/bgdata/libbgdata.a \
           $(SRCROOT)/src/lib/util/libutil.a \
           $(SRCROOT)/src/lib/liblaps.a  \
           $(SRCROOT)/src/lib/modules/libmodules.a \
           $(SRCROOT)/src/lib/mthermo/libmthermo.a

DBLAPSLIBS = $(SRCROOT)/src/lib/bgdata/libdebugbgdata.a \
             $(SRCROOT)/src/lib/util/libdebugutil.a \
             $(SRCROOT)/src/lib/libdebuglaps.a   \
             $(SRCROOT)/src/lib/modules/libdebugmodules.a \
             $(SRCROOT)/src/lib/mthermo/libdebugmthermo.a

EXE = sfc_qc.exe

SRC   = kalman.f    matrix_tools.f   \
        QC_tools.f       model_tools.f    write_qc.c    \
        write_tools.f 

#FFLAGS+=-pg

#DBFFLAGS+=-pg


MAIN=QC_main.f

LIB=libqc.a

ARCHIVE = $(LIB)($(addsuffix .o, $(basename $(SRC))))

all: exe

exe: $(ARCHIVE) $(EXE)

$(EXE): $(ARCHIVE) $(MAIN)
	$(FC) $(MAIN) $(FFLAGS) $(LIB) $(LAPSLIBS) $(OTHERLIBS) -o $@
	rm -f *.o


debug: $(SRC)
	$(RM) $(EXE) ;
	$(MAKE) LAPSLIBS='$(DBLAPSLIBS)' \
	CFLAGS='$(DBCFLAGS)' FFLAGS='$(DBFLAGS)'

install: exe
	$(INSTALL) $(EXE) $(INSTALLROOT)/bin/$(EXE)

clean:
	rm -f $(EXE) $(LIB) *.o *~ *# *.i


distclean: clean

%.f : %.F
	rm -f $*.f
	$(CPP) $(CPPFLAGS) $< 
	mv $*.i $*.f

%.o : %.F













