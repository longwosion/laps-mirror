      SUBROUTINE SBYTES(IOUT,IN,ISKIP,NBYTE,NSKIP,N)
C THIS PROGRAM WRITTEN BY.....
C             DR. ROBERT C. GAMMILL, CONSULTANT
C             NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
C             JULY 1972
C THIS IS THE FORTRAN VERSIONS OF SBYTES.
C     
C             FORTRAN 90
C             AUGUST 1990  RUSSELL E. JONES
C             NATIONAL WEATHER SERVICE
C     
C USAGE:    CALL SBYTES (PCKD,UNPK,INOFST,NBIT, NSKIP,ITER)
C     
C   INPUT ARGUMENT LIST:
C     UNPK     -  NBITS OF THE RIGHT SIDE OF EACH WORD OF ARRAY
C                 UNPK IS MOVED TO ARRAY PCKD. INOFST BITS ARE
C                 SKIPPED OVER BEFORE THE 1ST DATA IS MOVED, NBITS
C                 ARE STORED, NSKIP BITS ARE SKIPPED OVER, THE NEXT
C                 NBITS ARE MOVED,  BIT ARE SKIPPED OVER, ETC. UNTIL
C                 ITER GROUPS OF BITS ARE PACKED.
C    INOFST    -  A FULLWORD INTEGER SPECIFYING THE INITAL OFFSET
C                 IN BITS OF THE FIRST BYTE, COUNTED FROM THE
C                 LEFTMOST BIT IN PCKD.
C    NBITS     -  A FULLWORD INTEGER SPECIFYING THE NUMBER OF BITS
C                 IN EACH BYTE TO BE PACKED.  LEGAL BYTE WIDTHS
C                 ARE IN THE RANGE 1 - 32.
C    NSKIP     -  A FULLWORD INTEGER SPECIFYING THE NUMBER OF BITS
C                 TO SKIP BETWEEN SUCCESSIVE BYTES.  ALL NON-NEGATIVE
C                 SKIP COUNTS ARE LEGAL.
C    ITER      -  A FULLWORD INTEGER SPECIFYING THE TOTAL NUMBER OF
C                 BYTES TO BE PACKED, AS CONTROLLED BY INOFST,
C                 NBIT AND NSKIP ABOVE.   ALL NON-NEGATIVE ITERATION
C                 COUNTS ARE LEGAL.
C     
C   OUTPUT ARGUMENT LIST:
C    PCKD      -  THE FULLWORD IN MEMORY TO WHICH PACKING IS TO
C                 BEGIN STARTING AT BIT INOFST. THE INOSTAT BITS
C                 ARE NOT ALTERED. NSKIP BITS ARE NOT ALTERED.
C     
      INTEGER    IN(*)
      INTEGER    IOUT(*)
      INTEGER    MASKS(32)
C     
      DATA  NBITSW/32/
C     
C      DATA  MASKS /Z'00000001',Z'00000003',Z'00000007',Z'0000000F',
C     &             Z'0000001F',Z'0000003F',Z'0000007F',Z'000000FF',
C     &             Z'000001FF',Z'000003FF',Z'000007FF',Z'00000FFF',
C     &             Z'00001FFF',Z'00003FFF',Z'00007FFF',Z'0000FFFF',
C     &             Z'0001FFFF',Z'0003FFFF',Z'0007FFFF',Z'000FFFFF',
C     &             Z'001FFFFF',Z'003FFFFF',Z'007FFFFF',Z'00FFFFFF',
C     &             Z'01FFFFFF',Z'03FFFFFF',Z'07FFFFFF',Z'0FFFFFFF',
C     &             Z'1FFFFFFF',Z'3FFFFFFF',Z'7FFFFFFF',Z'FFFFFFFF'/
C     
C     MASKS TABLE PUT IN DECIMAL SO IT WILL COMPILE ON ANY 32 BIT
C     COMPUTER
C     
      DATA  MASKS / 1, 3, 7, 15, 31, 63, 127, 255, 511, 1023, 2047,
     & 4095, 8191, 16383, 32767, 65535, 131071, 262143, 524287,
     & 1048575, 2097151, 4194303, 8388607, 16777215, 33554431,
     & 67108863, 134217727, 268435455, 536870911, 1073741823,
     & 2147483647, -1/
C     
C NBYTE MUST BE LESS THAN OR EQUAL TO NBITSW
C     
      ICON = NBITSW - NBYTE
      IF (ICON.LT.0) RETURN
      MASK   = MASKS(NBYTE)
C     
C INDEX TELLS HOW MANY WORDS INTO IOUT THE NEXT BYTE IS TO BE STORED.
C     
      INDEX  = ISKIP / NBITSW
C     
C II TELLS HOW MANY BITS IN FROM THE LEFT SIDE OF THE WORD TO STORE IT.
C     
      II     = MOD(ISKIP,NBITSW)
C     
C ISTEP IS THE DISTANCE IN BITS FROM ONE BYTE POSITION TO THE NEXT.
C     
      ISTEP  = NBYTE + NSKIP
C     
C IWORDS TELLS HOW MANY WORDS TO SKIP FROM ONE BYTE TO THE NEXT.
C     
      IWORDS = ISTEP / NBITSW
C     
C IBITS TELLS HOW MANY BITS TO SKIP AFTER SKIPPING IWORDS.
C     
      IBITS  = MOD(ISTEP,NBITSW)
C     
      DO 10 I = 1,N
        J     = IAND(MASK,IN(I))
        MOVEL = ICON - II
C     
C BYTE IS TO BE STORED IN MIDDLE OF WORD.  SHIFT LEFT.
C     
        IF (MOVEL.GT.0) THEN
          MSK           = ISHFT(MASK,MOVEL)
          IOUT(INDEX+1) = IOR(IAND(NOT(MSK),IOUT(INDEX+1)),
     &    ISHFT(J,MOVEL))
C     
C THE BYTE IS TO BE SPLIT ACROSS A WORD BREAK.
C     
        ELSE IF (MOVEL.LT.0) THEN
          MSK           = MASKS(NBYTE+MOVEL)
          IOUT(INDEX+1) = IOR(IAND(NOT(MSK),IOUT(INDEX+1)),
     &    ISHFT(J,MOVEL))
          ITEMP         = IAND(MASKS(NBITSW+MOVEL),IOUT(INDEX+2))
          IOUT(INDEX+2) = IOR(ITEMP,ISHFT(J,NBITSW+MOVEL))
C     
C BYTE IS TO BE STORED RIGHT-ADJUSTED.
C     
        ELSE
          IOUT(INDEX+1) = IOR(IAND(NOT(MASK),IOUT(INDEX+1)),J)
        ENDIF
C     
        II    = II + IBITS
        INDEX = INDEX + IWORDS
        IF (II.GE.NBITSW) THEN
          II    = II - NBITSW
          INDEX = INDEX + 1
        ENDIF
C     
10    CONTINUE
C     
      RETURN
      END

