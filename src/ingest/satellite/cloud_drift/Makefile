# User options are in src/include/makefile.inc
# This Makefile is designed for gnu make version 3.75 or newer
# Please report problems to lapsbugs@fsl.noaa.gov
#
#

SRCROOT=../../../..

include $(SRCROOT)/src/include/makefile.inc

LAPSLIBS = $(SRCROOT)/src/lib/liblaps.a  \
           $(SRCROOT)/src/lib/modules/libmodules.a  

DBLAPSLIBS = $(SRCROOT)/src/lib/libdebuglaps.a  \
             $(SRCROOT)/src/lib/modules/libdebugmodules.a

.SUFFIXES:
.SUFFIXES: .o .c .f .a

EXE = ingest_cloud_drift.exe

SRC=ingest_cloud_drift.f  \
    cloud_drift_sub.f  \
    cloud_drift_sub_af.f \
    cloud_drift_sub_cwb_hdsw.f \
    cloud_drift_sub_madis.f \
    cloud_drift_sub_cwb_satob.f 

all: exe

exe: $(EXE)

$(EXE): $(SRC) $(LAPSLIBS) 
	$(FC) $(FFLAGS) $(SRC) $(LAPSLIBS) $(OTHERLIBS) -o $@
	rm -f *.o

debug: $(SRC)
	$(RM) $(EXE) ;
	$(MAKE) LAPSLIBS='$(DBLAPSLIBS)' \
	CFLAGS='$(DBCFLAGS)' FFLAGS='$(DBFLAGS)'

install: exe
	$(INSTALL) $(EXE) $(INSTALLROOT)/bin/$(EXE)

clean:
	rm -f $(EXE)  *.o *~ *#


distclean: clean
