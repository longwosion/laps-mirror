SUBROUTINE LAPS_INGEST

!==============================================================================
!doc  THIS ROUTINE INGESTS OBSERVATION DATA BY LAPS LIBRARIES.
!doc
!doc  HISTORY:
!doc	CREATION:	YUANFU XIE	MAY 2007
!==============================================================================

  USE LAPS_PARAMS

  IMPLICIT NONE

  INTEGER :: ISTATUS

  ! OPEN CHANNELS FOR LAPSPLOT OUTPUT: TMG, PIG, PRG, SAG ETC:
  CALL OPEN_LAPSPRD_FILE(PIGOUT_CHANNEL,SYSTEM_IN4TIME,'pig',ISTATUS)
  CALL OPEN_LAPSPRD_FILE(PRGOUT_CHANNEL,SYSTEM_IN4TIME,'prg',ISTATUS)
  CALL OPEN_LAPSPRD_FILE(SAGOUT_CHANNEL,SYSTEM_IN4TIME,'sag',ISTATUS)
  CALL OPEN_LAPSPRD_FILE(TMGOUT_CHANNEL,SYSTEM_IN4TIME,'tmg',ISTATUS)

  ! RADAR:
  ! CALL READ_RADAR		! PREFER TO USE LAPS GRIDDED RADAR DATA FOR NOW YUANFU

  ! PROFILER:
  CALL CONV_PROFLR

  ! RASS:
  CALL CONV_RASS

  ! SONDE:
  CALL CONV_SONDES

  ! SURFACE OBS:
  CALL CONV_SFCOBS

  ! CDW AND ACARS:
  CALL CONV_CDWACA

  CLOSE(PIGOUT_CHANNEL)
  CLOSE(PRGOUT_CHANNEL)
  CLOSE(TMGOUT_CHANNEL)
  CLOSE(SAGOUT_CHANNEL)

END SUBROUTINE LAPS_INGEST

SUBROUTINE CONV_PROFLR

!==============================================================================
!doc  THIS ROUTINE READS AND CONVERTS PROFILER OBSERVATION DATA INTO DATA FORMAT
!doc  REQUESTED.
!doc
!doc  HISTORY:
!doc	CREATION:	YUANFU XIE	JUN 2007
!==============================================================================

  USE LAPS_PARAMS

  IMPLICIT NONE

  CHARACTER :: FSPECS*225, EXTNSN*31,C5NAME(MAXNUM_PROFLRS)*5, &
               OBTYPE(MAXNUM_PROFLRS)*8
  INTEGER   :: IOFILE,STATUS,NPRFLR,NLEVEL(MAXNUM_PROFLRS)
  INTEGER   :: I4TIME,OBTIME(MAXNUM_PROFLRS)
  REAL	    :: PRFLAT(MAXNUM_PROFLRS),PRFLON(MAXNUM_PROFLRS),&
               PRFELV(MAXNUM_PROFLRS)
  REAL      :: HGHTOB(MAXNUM_PROFLRS,MAXLVL_PROFLRS)	! HEIGHT OBS
  REAL      :: UWNDOB(MAXNUM_PROFLRS,MAXLVL_PROFLRS)	! U WIND OBS
  REAL      :: VWNDOB(MAXNUM_PROFLRS,MAXLVL_PROFLRS)	! V WIND OBS
  REAL      :: RMSOBS(MAXNUM_PROFLRS,MAXLVL_PROFLRS)	! RMS
  REAL      :: TSFCOB(MAXNUM_PROFLRS)			! SFC T OBS
  REAL      :: PSFCOB(MAXNUM_PROFLRS)			! SFC P OBS
  REAL      :: RHSFCO(MAXNUM_PROFLRS)			! SFC RH OBS
  REAL      :: USFCOB(MAXNUM_PROFLRS)			! SFC U OBS
  REAL      :: VSFCOB(MAXNUM_PROFLRS)			! SFC V OBS

  IOFILE = 12
  NLEVEL = 0

  ! OPEN NEAREST PROFILER FILE TO THE LAPS ANALYSIS TIME:
  EXTNSN = 'pro'
  CALL GET_FILESPEC(EXTNSN,2,FSPECS,STATUS)

  CALL GET_FILE_TIME(FSPECS,SYSTEM_IN4TIME,I4TIME)

  ! CHECK IF DATA FILE IS CLOSE TO SYSTEM TIME:
  IF (ABS(SYSTEM_IN4TIME-I4TIME) .GT. LENGTH_ANATIME) THEN
    PRINT*,'CONV_PROFLR: No recent profiler data files'
    RETURN
  ENDIF

  ! READ PROFILER DATA: NOTE READ_PRO_DATA RETURNS M/S WIND:
  CALL READ_PRO_DATA(IOFILE,I4TIME,EXTNSN,MAXNUM_PROFLRS,MAXLVL_PROFLRS, & ! I
                     NPRFLR,NLEVEL,PRFLAT,PRFLON,PRFELV,C5NAME,OBTIME,   & ! O
                     OBTYPE,HGHTOB,UWNDOB,VWNDOB,RMSOBS,TSFCOB,PSFCOB,   & ! O
                     RHSFCO,USFCOB,VSFCOB,STATUS)                          ! O
  CALL LAPS_DIVIDER
  WRITE(6,*) 'CONV_PROFLR: Number of profilers data read: ',NPRFLR
  CALL LAPS_DIVIDER

  ! CONVERT TO THE REQUESTED FORMAT:
  IF (NPRFLR .GT. 0) THEN
    WRITE(6,*) 'CONV_PROFLR: Levels at each profiler: ',NLEVEL(1:NPRFLR)
    IF      (FORMAT_REQUEST .EQ. 'BUFR') THEN
      CALL BUFR_PROFLR(NPRFLR,NLEVEL,C5NAME,OBTIME,PRFLAT,PRFLON,PRFELV, &
                       OBTYPE,MAXNUM_PROFLRS,HGHTOB,UWNDOB,VWNDOB,RMSOBS, &
                       PSFCOB,TSFCOB,RHSFCO,USFCOB,VSFCOB)
    ELSE IF (FORMAT_REQUEST .NE. 'WRF') THEN
      CALL WRFD_PROFLR(NPRFLR,NLEVEL,C5NAME,OBTIME,PRFLAT,PRFLON,PRFELV, &
                       OBTYPE,MAXNUM_PROFLRS,HGHTOB,UWNDOB,VWNDOB,PSFCOB, &
                       TSFCOB,RHSFCO,USFCOB,VSFCOB)
    ENDIF
  ENDIF

END SUBROUTINE CONV_PROFLR

SUBROUTINE CONV_RASS

!==============================================================================
!doc  THIS ROUTINE READS AND CONVERTS RASS OBSERVATION DATA INTO DATA FORMAT
!doc  REQUESTED.
!doc
!doc  HISTORY:
!doc	CREATION:	YUANFU XIE	JUN 2007
!==============================================================================

  USE LAPS_PARAMS

  IMPLICIT NONE

  CHARACTER :: C5NAME(MAXNUM_SONDES)*5,OBTYPE(MAXNUM_SONDES)*8
  INTEGER   :: STATUS,NRASS,NLEVEL(MAXNUM_SONDES)
  INTEGER   :: I4TIME,OBTIME(MAXNUM_SONDES)
  REAL	    :: PRFLAT(MAXNUM_SONDES),PRFLON(MAXNUM_SONDES),&
               PRFELV(MAXNUM_SONDES)
  REAL      :: HGHTOB(MAXNUM_SONDES,MAXLVL_SONDES)	! HEIGHT OBS
  REAL      :: TEMPOB(MAXNUM_SONDES,MAXLVL_SONDES)	! TEMPERATURE OBS
  REAL      :: ERROBS(MAXNUM_SONDES)			! TEMPERATURE ERROR

  ! READ PROFILER DATA:
  CALL READ_RASS_DATA(SYSTEM_IN4TIME,LENGTH_ANATIME,MAXNUM_SONDES, &
                     MAXLVL_SONDES,RVALUE_MISSING,OBTIME, & ! I
                     NRASS,NLEVEL,C5NAME,OBTYPE,PRFLAT,PRFLON,PRFELV,ERROBS, &
                     HGHTOB,TEMPOB,STATUS)                          ! O
  CALL LAPS_DIVIDER
  WRITE(6,*) 'CONV_RASS: Number of RASS data read: ',NRASS
  CALL LAPS_DIVIDER

  ! CONVERT TO THE REQUESTED FORMAT:
  IF (NRASS .GT. 0) THEN
    WRITE(6,*) 'CONV_RASS: Levels at each profiler: ',NLEVEL(1:NRASS)
    IF      (FORMAT_REQUEST .EQ. 'BUFR') THEN
      CALL BUFR_RASS(NRASS,NLEVEL,C5NAME,OBTIME,PRFLAT,PRFLON,PRFELV, &
                       OBTYPE,MAXNUM_SONDES,HGHTOB,TEMPOB,ERROBS)
    ELSE IF (FORMAT_REQUEST .NE. 'WRF') THEN
      CALL WRFD_RASS(NRASS,NLEVEL,C5NAME,OBTIME,PRFLAT,PRFLON,PRFELV, &
                       OBTYPE,MAXNUM_SONDES,HGHTOB,TEMPOB)
    ENDIF
  ENDIF

END SUBROUTINE CONV_RASS


SUBROUTINE CONV_SONDES

!==============================================================================
!  THIS ROUTINE READS AND CONVERTS SONDING OBSERVATION DATA INTO DATA FORMAT
!  REQUESTED.
!
!  HISTORY:
!	CREATION:	YUANFU XIE	JUN 2007
!==============================================================================

  USE LAPS_PARAMS

  IMPLICIT NONE

  CHARACTER :: FSPECS*225, EXTNSN*3,C5NAME(MAXNUM_SONDES)*5, &
               OBTYPE(MAXNUM_SONDES)*8
  INTEGER   :: IOFILE,STATUS,NPRFLR,NLEVEL(MAXNUM_SONDES), &
               WINDOW,MODEOB
  INTEGER   :: I4TIME,PRFTIM(MAXNUM_SONDES,MAXLVL_SONDES),I
  REAL	    :: PRFLAT(MAXNUM_SONDES,MAXLVL_SONDES), &
               PRFLON(MAXNUM_SONDES,MAXLVL_SONDES), &
               PRFELV(MAXNUM_SONDES)
  REAL      :: HGHTOB(MAXNUM_SONDES,MAXLVL_SONDES)	! HEIGHT OBS
  REAL      :: PRSOBS(MAXNUM_SONDES,MAXLVL_SONDES)	! PRESSURE OBS
  REAL      :: UWNDOB(MAXNUM_SONDES,MAXLVL_SONDES)	! U WIND OBS
  REAL      :: VWNDOB(MAXNUM_SONDES,MAXLVL_SONDES)	! V WIND OBS
  REAL      :: TEMPOB(MAXNUM_SONDES,MAXLVL_SONDES)	! T OBS
  REAL      :: DEWOBS(MAXNUM_SONDES,MAXLVL_SONDES)	! DEW POINT OBS

  IOFILE = 12
  NLEVEL = 0
  WINDOW = 0			! I4TIME WINDOW MIMIC READ_PROFILES.F
  MODEOB = 3			! KEY LEVELS OFF OF WIND DATA

  ! OPEN NEAREST PROFILER FILE TO THE LAPS ANALYSIS TIME:
  EXTNSN = 'snd'
  CALL GET_FILESPEC(EXTNSN,2,FSPECS,STATUS)

  CALL GET_FILE_TIME(FSPECS,SYSTEM_IN4TIME,I4TIME)
  IF (ABS(SYSTEM_IN4TIME-I4TIME) .GT. LENGTH_ANATIME) THEN
    WRITE(6,*) 'CONV_SONDES: Warning: nearest sonde file is outside window'
  ELSE

    ! READ SONDE DATA:
    CALL READ_SND_DATA2(IOFILE,I4TIME,EXTNSN,MAXNUM_SONDES,MAXLVL_SONDES, & ! I
                       DOMAIN_LATITDE,DOMAIN_LONGITD,DOMAIN_TOPOGRP,NUMBER_GRIDPTS(1),     & ! I
                       NUMBER_GRIDPTS(2),NUMBER_GRIDPTS(3),HEIGHT_GRID3DM,  & ! I
                       .TRUE.,MODEOB,                                       & ! I
                       NPRFLR,PRFELV,NLEVEL,C5NAME,OBTYPE,HGHTOB,           & ! O
                       PRSOBS,UWNDOB,VWNDOB,TEMPOB,DEWOBS,PRFLAT,PRFLON,    & ! O
                       PRFTIM,STATUS)                                         ! O
    CALL LAPS_DIVIDER
    WRITE(6,*) 'CONV_SONDES: Number of sonde data read: ',NPRFLR
    CALL LAPS_DIVIDER

    ! CONVERT TO THE REQUESTED FORMAT:
    IF (STATUS .EQ. 1 .AND. NPRFLR .GT. 0) THEN
      WRITE(6,*) 'CONV_SONDES: Levels at each sonde: '
      DO I=1,NPRFLR
        IF (NLEVEL(I) .GT. 0) WRITE(6,*) I,': ',NLEVEL(I)
      ENDDO
      IF      (FORMAT_REQUEST .EQ. 'BUFR') THEN
        CALL BUFR_SONDES(NPRFLR,NLEVEL,C5NAME,PRFTIM,PRFLAT,PRFLON,PRFELV, &
                         OBTYPE,HGHTOB,PRSOBS,TEMPOB,DEWOBS,UWNDOB,VWNDOB)
      ELSE IF (FORMAT_REQUEST .NE. 'WRF') THEN
        CALL WRFD_SONDES(NPRFLR,NLEVEL,C5NAME,PRFTIM,PRFLAT,PRFLON,PRFELV, &
                         OBTYPE,HGHTOB,PRSOBS,TEMPOB,DEWOBS,UWNDOB,VWNDOB)
      ENDIF
    ENDIF

  ENDIF

END SUBROUTINE CONV_SONDES

SUBROUTINE CONV_SFCOBS

!==============================================================================
!  THIS ROUTINE READS AND CONVERTS SURFACE OBSERVATIONS INTO REQUESTED DATA
!  FORMAT.
!
!  HISTORY:
!	CREATION:	YUANFU XIE	JUN 2007
!==============================================================================

  USE LAPS_PARAMS

  IMPLICIT NONE

  CHARACTER*24 :: FILETM		! OBS FILE TIME
  INTEGER :: MAXSTN,STATUS,I		! MAXIMUM STATIONS
  INTEGER :: NOBGRD,NOBBOX		! NUMBER OF OBS OVER GRID AND BOX
  INTEGER :: NSS,NSNGRD,NSNBOX		! NUMBER OF SND OBS OVER GRID AND BOX

  CHARACTER*20,ALLOCATABLE,DIMENSION(:) :: STNAME	! STATION NAMES
  CHARACTER*11,ALLOCATABLE,DIMENSION(:) :: PVNAME	! PROVIDER NAMES
  CHARACTER*25,ALLOCATABLE,DIMENSION(:) :: PRSTWX	! PRESENT WEATHER
  CHARACTER*6, ALLOCATABLE,DIMENSION(:) :: RPTYPE	! REPORT TYPE
  CHARACTER*6, ALLOCATABLE,DIMENSION(:) :: STNTYP	! STATION TYPE
  INTEGER,     ALLOCATABLE,DIMENSION(:) :: OBTIME,WMOIDS! OBS TIME/WMO ID
  INTEGER,     ALLOCATABLE,DIMENSION(:) :: CLDLYR,PRSCHC! CLOUD LAYER/PRS CHG
  REAL,        ALLOCATABLE,DIMENSION(:) :: &
    OBSLAT,OBSLON,OBSELV,OBSTMP,ERRTMP,OBSDEW,ERRDEW,OBSRHS,ERRRHS, &
    OBSDIR,ERRDIR,OBSSPD,ERRSPD,GUSDIR,GUSSPD,OBSALT,ERRALT,STNPRS, &
    MSLPRS,PRSCH3,ERRPRS,OBSVIS,ERRVIS,OBSSOL,ERRSOL,SFCTMP,ERRSFT, &
    SFCMOI,ERRSFM,PRECP1,PRECP3,PRECP6,PREC24,ERRPCP,SNOWCV,ERRSNW, &
    MAXTMP,MINTMP !,igrid,jgrid

  CHARACTER*4, ALLOCATABLE,DIMENSION(:,:) :: CLDAMT     ! CLOUD AMOUNT

  REAL,        ALLOCATABLE,DIMENSION(:,:) :: CLDHGT	! CLOUD HEIGHTS
  REAL,        ALLOCATABLE,DIMENSION(:,:) :: OBSWND, &  ! OBS WIND
                                             ERRWND	! OBS WIND ERROR

  ! GET MAXIMUM NUMBER OF SURFACE STATIONS:
  CALL GET_MAXSTNS(MAXSTN,STATUS)
  IF (STATUS .NE. 1) THEN
    WRITE(6,*) 'CONV_SONDES: ERROR IN READING MAXIMUM SFC STATIONS'
    STOP
  ENDIF

  ! ADD NUMBER OF SONDE SURFACE:
  MAXSTN = MAXSTN+MAXNUM_SONDES

  ! ALLOCATABLE MEMORY FOR SURFACE VARIABLES:
  ALLOCATE(OBTIME(MAXSTN),WMOIDS(MAXSTN),STNAME(MAXSTN), &
           PVNAME(MAXSTN),PRSTWX(MAXSTN),RPTYPE(MAXSTN), &
           STNTYP(MAXSTN),OBSLAT(MAXSTN),OBSLON(MAXSTN), &
           OBSELV(MAXSTN),OBSTMP(MAXSTN),OBSDEW(MAXSTN), &
           OBSRHS(MAXSTN),OBSDIR(MAXSTN),OBSSPD(MAXSTN), &
           GUSDIR(MAXSTN),GUSSPD(MAXSTN),OBSALT(MAXSTN), &
           STNPRS(MAXSTN),MSLPRS(MAXSTN),PRSCHC(MAXSTN), &
           PRSCH3(MAXSTN),OBSVIS(MAXSTN),OBSSOL(MAXSTN), &
           SFCTMP(MAXSTN),SFCMOI(MAXSTN),PRECP1(MAXSTN), &
           PRECP3(MAXSTN),PRECP6(MAXSTN),PREC24(MAXSTN), &
           SNOWCV(MAXSTN),CLDLYR(MAXSTN),MAXTMP(MAXSTN), &
           MINTMP(MAXSTN),ERRTMP(MAXSTN),ERRDEW(MAXSTN), &
           ERRRHS(MAXSTN),ERRDIR(MAXSTN),ERRSPD(MAXSTN), &
           ERRALT(MAXSTN),ERRPRS(MAXSTN),ERRVIS(MAXSTN), &
           ERRSOL(MAXSTN),ERRSFT(MAXSTN),ERRSFM(MAXSTN), &
           ERRPCP(MAXSTN),ERRSNW(MAXSTN), & ! igrid(maxstn),jgrid(maxstn), &
           CLDAMT(MAXSTN,5),CLDHGT(MAXSTN,5), &
           OBSWND(2,MAXSTN),ERRWND(2,MAXSTN), STAT=STATUS)
  IF (STATUS .NE. 0) THEN
    WRITE(6,*) 'CONV_SFCOBS: ERROR IN ALLOCATING MEMORY FOR SURFACE DATA'
    STOP
  ENDIF

  ! READ SFC OBS: READ_SURFACE_DATA RETURNS KNOTS FOR WIND:
  CALL READ_SURFACE_DATA(SYSTEM_IN4TIME,FILETM,NOBGRD,NOBBOX,OBTIME,WMOIDS,&
                         STNAME,PVNAME,PRSTWX,RPTYPE,STNTYP,OBSLAT,OBSLON, &
                         OBSELV,OBSTMP,OBSDEW,OBSRHS,OBSDIR,OBSSPD,GUSDIR, &
                         GUSSPD,OBSALT,STNPRS,MSLPRS,PRSCHC,PRSCH3,OBSVIS, &
                         OBSSOL,SFCTMP,SFCMOI,PRECP1,PRECP3,PRECP6,PREC24, &
                         SNOWCV,CLDLYR,MAXTMP,MINTMP,                      &
                         ERRTMP,ERRDEW,ERRRHS,ERRDIR,ERRSPD,ERRALT,ERRPRS, &
                         ERRVIS,ERRSOL,ERRSFT,ERRSFM,ERRPCP,ERRSNW,CLDAMT, &
                         CLDHGT,MAXSTN,STATUS)

  IF (STATUS .NE. 1) THEN
    WRITE(6,*) 'CONV_SFCOBS: ERROR IN READING SURFACE OBS'
    STOP
  ENDIF

  ! READ SONDE SFC OBS: READ_SURFACE_DATA RETURNS KNOTS FOR WIND:
  NSS = NOBBOX+1
  NSNGRD = 0		! NOTE: READ_SFC_SND DOES NOT INITIALIZE NSNGRD
  NSNBOX = 0		! NOTE: READ_SFC_SND DOES NOT INITIALIZE NSNBOX
  ! THE FOLLOWING CALL COMMAND ALLOWS SND SURFACE DATA INGESTED BUT THESE DATA
  ! ARE INGESTED FROM SONDE READER ALREADY. 
  ! CALL READ_SFC_SND(SYSTEM_IN4TIME,FILETM,NSNGRD,NSNBOX,OBTIME(NSS),WMOIDS(NSS),&
  !                      STNAME(NSS),PVNAME(NSS),PRSTWX(NSS),RPTYPE(NSS), &
  !                      STNTYP(NSS),OBSLAT(NSS),OBSLON(NSS),OBSELV(NSS), & 
  !                      OBSTMP(NSS),OBSDEW(NSS),OBSRHS(NSS),OBSDIR(NSS), &
  !                      OBSSPD(NSS),GUSDIR(NSS),GUSSPD(NSS),OBSALT(NSS), & 
  !                      STNPRS(NSS),MSLPRS(NSS),PRSCHC(NSS),PRSCH3(NSS), &
  !                      OBSVIS(NSS),OBSSOL(NSS),SFCTMP(NSS),SFCMOI(NSS), &
  !                      PRECP1(NSS),PRECP3(NSS),PRECP6(NSS),PREC24(NSS), &
  !                      SNOWCV(NSS),CLDLYR(NSS),MAXTMP(NSS),MINTMP(NSS), &
  !                      ERRTMP(NSS),ERRDEW(NSS),ERRRHS(NSS),ERRDIR(NSS), &
  !                      ERRSPD(NSS),ERRALT(NSS),ERRPRS(NSS),ERRVIS(NSS), &
  !                      ERRSOL(NSS),ERRSFT(NSS),ERRSFM(NSS),ERRPCP(NSS), &
  !                      ERRSNW(NSS),CLDAMT(NSS,1),CLDHGT(NSS,1),MAXSTN, &
  !                      DOMAIN_LATITDE,DOMAIN_LONGITD,NUMBER_GRIDPTS(1), &
  !                      NUMBER_GRIDPTS(2),NUMBER_GRIDPTS(3),MAXNUM_SONDES, &
  !                      MAXLVL_SONDES,DOMAIN_TOPOGRP,STATUS)
  !
  ! IF (NSNBOX .LT. 1) THEN
  !   WRITE(6,*) 'CONV_SFCOBS: NO SONDE SFC DATA AVAILABLE'
  ! ENDIF
  ! ACCOUNT TOTAL SFC DATA:
  ! THE FOLLOWING COMMAND ALLOWS SND SURFACE DATA INGESTED BUT THESE DATA
  ! ARE INGESTED FROM SONDE READER ALREADY. 
  ! NOBBOX = NOBBOX+NSNBOX

  ! CONVERT WIND DIRECTION AND SPEED INTO U AND V:
  DO I=1,NOBBOX
    IF ((OBSDIR(I) .NE. RVALUE_MISSING) .AND. &
        (OBSDIR(I) .NE. SFCOBS_INVALID) .AND. &
        (OBSSPD(I) .NE. RVALUE_MISSING) .AND. &
        (OBSSPD(I) .NE. SFCOBS_INVALID)) THEN
      CALL DISP_TO_UV(OBSDIR(I),OBSSPD(I),OBSWND(1,I),OBSWND(2,I))
      CALL DISP_TO_UV(ERRDIR(I),ERRSPD(I),ERRWND(1,I),ERRWND(2,I))

      ! CONVERT WIND INTO M/S:
      OBSWND(1:2,I) = OBSWND(1:2,I)*0.514791
    ELSE
      OBSWND(1:2,I) = RVALUE_MISSING
      ERRWND(1:2,I) = RVALUE_MISSING
    ENDIF

    ! TEMPERATURE AND DEW:
    IF ((OBSTMP(I) .NE. RVALUE_MISSING) .AND. &
        (OBSTMP(I) .NE. SFCOBS_INVALID) ) OBSTMP(I) = (OBSTMP(I)-32.0)*5.0/9.0
    IF ((OBSDEW(I) .NE. RVALUE_MISSING) .AND. &
        (OBSDEW(I) .NE. SFCOBS_INVALID) ) OBSDEW(I) = (OBSDEW(I)-32.0)*5.0/9.0
  ENDDO

  CALL LAPS_DIVIDER
  WRITE(6,*) 'CONV_SFCOBS: Number of surface obs data read: ',NOBBOX,NOBGRD
  CALL LAPS_DIVIDER

  ! CONVERT TO THE REQUESTED DATA FORMAT:
  IF      (FORMAT_REQUEST .EQ. 'BUFR') THEN
    CALL BUFR_SFCOBS(NOBBOX,OBTIME, &
                     OBSLAT,OBSLON,STNAME,RPTYPE,PVNAME,OBSELV, &
                     MSLPRS,ERRPRS,STNPRS,ERRPRS,OBSTMP,ERRTMP, &
                     OBSWND,ERRWND,OBSRHS,ERRRHS,STNPRS,PRECP1,ERRPCP)
  ELSE IF (FORMAT_REQUEST .EQ. 'WRF' ) THEN
    CALL WRFD_SFCOBS(NOBBOX,OBTIME, &
                     OBSLAT,OBSLON,STNAME,RPTYPE,PVNAME,OBSELV, &
                     MSLPRS,ERRPRS,STNPRS,ERRPRS,OBSTMP,ERRTMP, &
                     OBSWND,ERRWND,OBSRHS,ERRRHS,STNPRS,PRECP1,ERRPCP)
  ENDIF

  ! DEALLOCATABLE MEMORY FOR SURFACE VARIABLES:
  DEALLOCATE(OBTIME,WMOIDS,STNAME, &
             PVNAME,PRSTWX,RPTYPE, &
             STNTYP,OBSLAT,OBSLON, &
             OBSELV,OBSTMP,OBSDEW, &
             OBSRHS,OBSDIR,OBSSPD, &
             GUSDIR,GUSSPD,OBSALT, &
             STNPRS,MSLPRS,PRSCHC, &
             PRSCH3,OBSVIS,OBSSOL, &
             SFCTMP,SFCMOI,PRECP1, &
             PRECP3,PRECP6,PREC24, &
             SNOWCV,CLDLYR,MAXTMP, &
             MINTMP,ERRTMP,ERRDEW, &
             ERRRHS,ERRDIR,ERRSPD, &
             ERRALT,ERRPRS,ERRVIS, &
             ERRSOL,ERRSFT,ERRSFM, &
             ERRPCP,ERRSNW, &
             CLDAMT,CLDHGT, &
             OBSWND,ERRWND, STAT=STATUS)
  IF (STATUS .NE. 0) THEN
    WRITE(6,*) 'CONV_SFCOBS: ERROR IN DEALLOCATING MEMORY FOR SURFACE OBS'
    STOP
  ENDIF

END SUBROUTINE CONV_SFCOBS

SUBROUTINE CONV_CDWACA

!==============================================================================
!  THIS ROUTINE READS AND CONVERTS CLOUD DRIFT WIND AND ACARS (PIREP) DATA INTO
!  REQUESTED DATA FORMAT
!
!  HISTORY:
!	CREATION:	YUANFU XIE	JUN 2007
!==============================================================================

  USE LAPS_PARAMS

  IMPLICIT NONE

  ! LOCAL VARIABLES:
  INTEGER, PARAMETER :: MAXOBS=300000	! LOCALLY DEFINED,CHANGE IT IF NEEDED
  CHARACTER :: EXTEND*3,OBSTYP*4,ASCTIM*9
  INTEGER   :: NUMOBS,STATUS
  INTEGER   :: OBSINT(3,MAXOBS)
  LOGICAL   :: GEOALT
  REAL      :: OBSLAT,OBSLON,OBSELV,OBSPRS,OBSDIR,OBSSPD,OBSTMP,OBSNON,ZTOPSA
  REAL      :: UCMPNT,VCMPNT,RI,RJ,HEIGHT_TO_PRESSURE
  REAL*8    :: OBARRY(7,MAXOBS)

  ! CLOUD DRIFT WIND:
  NUMOBS = 0
  ! ACAR (PIREP):
  EXTEND = 'pin'

  ! 1. TEMP:
  OBSTYP = 'temp'
  CALL OPEN_LAPSPRD_FILE_READ(POINTS_CHANNEL,SYSTEM_IN4TIME,EXTEND,STATUS)
  IF (STATUS .NE. 1) THEN
    PRINT*,'CONV_CDWACA: No ACAR (PIREP) temp data'
  ELSE
    STATUS = 0
    DO
      CALL READ_ACARS_OB(POINTS_CHANNEL,OBSTYP,OBSLAT,OBSLON,OBSELV,OBSTMP,OBSNON, &
                               ASCTIM,0,GEOALT,STATUS)
      IF (STATUS .NE. 0) EXIT
      NUMOBS = NUMOBS+1
      IF (NUMOBS .GT. MAXOBS) THEN
        PRINT*,'CONV_CDW_ACA: Data array is too small, MAXOBS needs enlarge ',NUMOBS,MAXOBS
        STOP
      ENDIF
      OBARRY(1:7,NUMOBS) = RVALUE_MISSING
      OBARRY(1,NUMOBS) = OBSLAT
      OBARRY(2,NUMOBS) = OBSLON
      IF (GEOALT) THEN
        ! GEOMETRIC HEIGHT:
        OBARRY(3,NUMOBS) = OBSELV
      ELSE
        ! PRESSURE HEIGHT IN STANDARD ATMOSPHERE:
        OBARRY(4,NUMOBS) = ZTOPSA(OBSELV)	! ZTOPSA RETURNS PRESSURE IN MB
      ENDIF
      OBARRY(7,NUMOBS) = OBSTMP-273.15	! READ_ACARS_OB RETURNS KELVIN
      OBSINT(1,NUMOBS) = 41		! USE BUFR REPORT TYPE CODE: 
                                        ! 41 ACARS TEMPERATURE
      IF (GEOALT) THEN 
        OBSINT(2,NUMOBS) = 666		! TEMPORARILY USE 666 FOR WIDSOM DATA
      ELSE
        OBSINT(2,NUMOBS) = 130		! PILOR REPORT: TEMPERATURE
      ENDIF
      CALL CV_ASC_I4TIME(ASCTIM,OBSINT(3,NUMOBS))
      !PRINT*,'ACAR TEMP: ',OBSLAT,OBSLON,OBSELV,OBSTMP,OBSINT(3,NUMOBS)
    ENDDO
  ENDIF
  CLOSE(POINTS_CHANNEL)
  PRINT*,'CONV_CDWACA: Total of CDW + ACAR TEMP OBS: ',NUMOBS

  ! 1. WIND:
  OBSTYP = 'wind'
  CALL OPEN_LAPSPRD_FILE_READ(POINTS_CHANNEL,SYSTEM_IN4TIME,EXTEND,STATUS)
  IF (STATUS .NE. 1) THEN
    PRINT*,'CONV_CDWACA: No ACAR (PIREP) wind data'
  ELSE
    STATUS = 0
    DO
      CALL READ_ACARS_OB(POINTS_CHANNEL,OBSTYP,OBSLAT,OBSLON,OBSELV,OBSDIR,OBSSPD, &
                               ASCTIM,0,GEOALT,STATUS)
      IF (STATUS .NE. 0) EXIT
      NUMOBS = NUMOBS+1
      IF (NUMOBS .GT. MAXOBS) THEN
        PRINT*,'CONV_CDWACA: Data array is too small, MAXOBS needs enlarge ',NUMOBS,MAXOBS
        STOP
      ENDIF
      OBARRY(1:7,NUMOBS) = RVALUE_MISSING
      OBARRY(1,NUMOBS) = OBSLAT
      OBARRY(2,NUMOBS) = OBSLON
      IF (GEOALT) THEN 
        ! GEOMETRIC HEIGHT:
        OBARRY(3,NUMOBS) = OBSELV
      ELSE
        ! PRESSURE HEIGHT IN STANDARD ATMOSPHERE:
        OBARRY(4,NUMOBS) = ZTOPSA(OBSELV)	! ZTOPSA RETURNS PRESSURE IN MB
      ENDIF
      CALL DISP_TO_UV(OBSDIR,OBSSPD,OBSTMP,OBSNON)
      OBARRY(5,NUMOBS) = OBSTMP !OBSDIR	! WHEN CALLING UVTRUE_TO_UVGRID
      OBARRY(6,NUMOBS) = OBSNON !OBSSPD
      OBSINT(1,NUMOBS) = 41		! USE BUFR REPORT TYPE CODE: 
                                        ! ACARS WIND
      IF (GEOALT) THEN
        OBSINT(2,NUMOBS) = 666		! TEMPORARILY USE 666 FOR WISDOM DATA
      ELSE
        OBSINT(2,NUMOBS) = 230		! PILOR REPORT: WIND
      ENDIF
      CALL CV_ASC_I4TIME(ASCTIM,OBSINT(3,NUMOBS))
      ! PRINT*,'ACAR WIND: ',OBSLAT,OBSLON,OBSELV,OBSDIR,OBSSPD,OBSINT(3,NUMOBS)
    ENDDO
  ENDIF
  CLOSE(POINTS_CHANNEL)

  EXTEND = 'cdw'
  CALL OPEN_LAPSPRD_FILE_READ(POINTS_CHANNEL,SYSTEM_IN4TIME,EXTEND,STATUS)
  IF (STATUS .NE. 1) THEN
    PRINT*,'CONV_CDWACA: No cloud drift wind data'
  ELSE
    STATUS = 0
    DO
      CALL READ_LAPS_CDW_WIND(POINTS_CHANNEL,OBSLAT,OBSLON,OBSPRS,OBSDIR,OBSSPD, &
                               ASCTIM,STATUS)
      IF (STATUS .NE. 0) EXIT
      NUMOBS = NUMOBS+1
      IF (NUMOBS .GT. MAXOBS) THEN
        PRINT*,'CONV_CDW_ACA: Data array is too small, MAXOBS needs enlarge ',NUMOBS,MAXOBS
        STOP
      ENDIF
      ! PRINT*,'CONV_CDWACA: FOUND CDW DATA, COMPLETE THIS CODE'

      OBARRY(1:7,NUMOBS) = RVALUE_MISSING
      OBARRY(1,NUMOBS) = OBSLAT
      OBARRY(2,NUMOBS) = OBSLON
      OBARRY(4,NUMOBS) = OBSPRS/100.0	! BUFR POB IN MB

      ! CONVERT DIR/SPD INTO U/V FOR BUFR
      CALL DISP_TO_UV(OBSDIR,OBSSPD,UCMPNT,VCMPNT)
      OBARRY(5,NUMOBS) = UCMPNT
      OBARRY(6,NUMOBS) = VCMPNT

      OBSINT(1,NUMOBS) = 63		! USE BUFR REPORT TYPE CODE: 
                                        ! 63 SATELLITE DERIVED WIND
      OBSINT(2,NUMOBS) = 241		! RERORT TYPE: SATWIND
      CALL CV_ASC_I4TIME(ASCTIM,OBSINT(3,NUMOBS))
      ! PRINT*,'CDW: ',OBSLAT,OBSLON,OBSPRS,OBSDIR,OBSSPD,OBSINT(3,NUMOBS)
    ENDDO
  ENDIF
  CLOSE(POINTS_CHANNEL)
  PRINT*,'CONV_CDWACA: Total of CDW OBS: ',NUMOBS

  PRINT*,'CONV_CDWACA: Total of CDW + ACAR TEMP + ACAR WIND OBS: ',NUMOBS

  IF (NUMOBS .EQ. 0) RETURN

  IF (FORMAT_REQUEST .EQ. 'BUFR') THEN
    CALL BUFR_CDWACA(NUMOBS,OBARRY,OBSINT)
  ELSE IF (FORMAT_REQUEST .EQ. 'WRF' ) THEN
    CALL WRFD_CDWACA(NUMOBS,OBARRY,OBSINT)
  ENDIF

END SUBROUTINE CONV_CDWACA


SUBROUTINE READ_RADAR

!==============================================================================
!doc  THIS ROUTINE READS MULTIPLE RADAR RADIAL WIND VELOCITY USING LAPS'
!doc  GET_MULTIRADAR_VEL.
!doc
!doc  HISTORY:
!doc	CREATION:	YUANFU XIE	MAR 2008
!==============================================================================

  USE LAPS_PARAMS
  USE MEM_NAMELIST		! LAPS WIND PARAMETER MODULE

  IMPLICIT NONE

  ! LOCAL VARIABLES:
  CHARACTER*31 :: RADEXT(MAX_RADARS)	! POSSIBLE RADAR NAME EXTENSIONS
  CHARACTER*4  :: RADNAM(MAX_RADARS)	! RADAR STATION NAMES
  INTEGER      :: NRADAR		! NUMBER OF RADAR AVAILABLE
  INTEGER      :: LCYCLE		! LAPS CYCLE TIME
  INTEGER      :: STTRAD,STTNQY 	! RADAR AND ITS NYQUIST STATUS
  INTEGER      :: NGRDRD(MAX_RADARS) 	! NUMBER OF GRIDPOINTS WITH MEASURABLE VEL
  INTEGER      :: NGRDRD_old(MAX_RADARS) 	! NUMBER OF GRIDPOINTS WITH MEASURABLE VEL
  INTEGER      :: RADTIM(MAX_RADARS)	! RADAR OBSERVATION TIME
  INTEGER      :: RADIDS(MAX_RADARS) 	! RADAR IDS
  INTEGER      :: I,J,K,L
  LOGICAL      :: CLUTTR		! .TRUE. -- REMOVE 3D RADAR CLUTTER
  REAL         :: RADVEL_old(NUMBER_GRIDPTS(1),NUMBER_GRIDPTS(2),NUMBER_GRIDPTS(3),MAX_RADARS)
  REAL         :: RADVEL(NUMBER_GRIDPTS(1),NUMBER_GRIDPTS(2),NUMBER_GRIDPTS(3),MAX_RADARS)
                  ! RADAR 4D VELOCITY GRID
  REAL         :: RADNQY(NUMBER_GRIDPTS(1),NUMBER_GRIDPTS(2),NUMBER_GRIDPTS(3),MAX_RADARS)
                  ! RADAR 4D NYQUIST VELOCITY
  REAL         :: UVZERO(NUMBER_GRIDPTS(1),NUMBER_GRIDPTS(2),NUMBER_GRIDPTS(3),2)
                  ! ZERO UV GRIDS USED FOR CALLING LAPS QC_RADAR_OBS
  REAL         :: UVBKGD(NUMBER_GRIDPTS(1),NUMBER_GRIDPTS(2),NUMBER_GRIDPTS(3),2)
                  ! UV BACKGROUND GRIDS USED FOR CALLING LAPS QC_RADAR_OBS
  REAL         :: UV4DML(NUMBER_GRIDPTS(1),NUMBER_GRIDPTS(2),NUMBER_GRIDPTS(3),-1:1,2)
                  ! UV BACKGROUND GRIDS USED FOR CALLING LAPS QC_RADAR_OBS
  REAL         :: VOLNQY(MAX_RADARS)		! VOLUME NYQUIST VELOCITY
  REAL         :: RADLAT(MAX_RADARS),RADLON(MAX_RADARS),RADHGT(MAX_RADARS)
  REAL         :: UVGRID(2)

  INCLUDE 'main_sub.inc'

  CLUTTR = .TRUE.		! TRUE. -- REMOVE 3D RADAR CLUTTER
  RADARS_TIMETOL = 900
  CALL GET_MULTIRADAR_VEL(SYSTEM_IN4TIME,RADARS_TIMETOL,RADTIM,MAX_RADARS, &
                           NRADAR,RADEXT,RVALUE_MISSING,CLUTTR, &
                           NUMBER_GRIDPTS(1),NUMBER_GRIDPTS(2),NUMBER_GRIDPTS(3), &
                           RADVEL,RADNQY,RADIDS,VOLNQY,NGRDRD,RADLAT,RADLON,RADHGT, &
                           RADNAM,STTRAD,STTNQY)

  ! SET UV ZERO GRIDS:
  UVZERO = 0.0

  ! GET LAPS BACKGROUND:
  CALL GET_LAPS_CYCLE_TIME(LCYCLE,STTRAD)
  CALL GET_FG_WIND_NEW(SYSTEM_IN4TIME,LCYCLE,NUMBER_GRIDPTS(1),NUMBER_GRIDPTS(2), &
                        NUMBER_GRIDPTS(3),-1,1,UV4DML(1,1,1,-1,1),UV4DML(1,1,1,-1,2), &
                        UVBKGD(1,1,1,1),UVBKGD(1,1,1,2),STTRAD)

  ! CONVERT TO GRID NORTH FROM TRUE NORTH:
  IF (( .NOT. L_GRID_NORTH_BKG) .AND. L_GRID_NORTH_ANAL) THEN
    WRITE(6,*) ' Rotating first guess (background) to grid north'

    DO K=1,NUMBER_GRIDPTS(3)
      DO J=1,NUMBER_GRIDPTS(2)
        DO I=1,NUMBER_GRIDPTS(1)
          CALL UVTRUE_TO_UVGRID(UVBKGD(1,1,1,1),UVBKGD(1,1,1,2), &
                                 UVGRID(1),UVGRID(2),DOMAIN_LONGITD(I,J))
          UVBKGD(i,j,k,1:2) = UVGRID(1:2)
        ENDDO
      ENDDO
    ENDDO
  ENDIF ! END OF CONVERSION TO GRID NORTH


  RADVEL_old = RADVEL
  NGRDRD_old = NGRDRD

  ! NYQUIST UNFOLDING:
!!! UNFINISHED: NEED MORE WORK APR. 2008
  DO L=1,NRADAR
    ! QC and unfolding radar nyquist:
!    CALL QC_RADAR_OBS(NUMBER_GRIDPTS(1),NUMBER_GRIDPTS(2),NUMBER_GRIDPTS(3), &
!                       RVALUE_MISSING,RADVEL(1,1,1,L),RADNQY(1,1,1,L),NGRDRD(L), &
!                       DOMAIN_LATITDE,DOMAIN_LONGITD,RADLAT(L),RADLON(L),RADHGT(L), &
!                       UVZERO(1,1,1,1),UVZERO(1,1,1,2),UVBKGD(1,1,1,1),UVBKGD(1,1,1,2), &
!                       VOLNQY(L),L_CORRECT_UNFOLDING,L_GRID_NORTH,STTRAD)
    PRINT*,'STATUS OF QC_RADAR_OBS: ',STTRAD

  IF (NGRDRD_old(L) .NE. NGRDRD(L)) PRINT*,'Number grid RADAR CHANGE: ',ngrdrd_old(L),ngrdrd(L)
    DO K=1,NUMBER_GRIDPTS(3)
      DO J=1,NUMBER_GRIDPTS(2)
        DO I=1,NUMBER_GRIDPTS(1)
          IF (RADVEL(I,J,K,L) .NE. RVALUE_MISSING) PRINT*,'RADIAL: ', &
	      RADVEL(I,J,K,L),RADNQY(I,J,K,L),I,J,K,L,NGRDRD(L),VOLNQY(L)
          IF (RADVEL_old(i,j,k,L) .NE. RADVEL(i,j,k,L)) print*,'Unfolded: ', &
              radvel_old(i,j,k,l),radvel(i,j,k,l)
        ENDDO
      ENDDO
    ENDDO

  ENDDO

  STOP

END SUBROUTINE READ_RADAR