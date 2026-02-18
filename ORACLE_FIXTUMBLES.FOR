CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CONFIDENTIAL
C       Property of United States Steel Corporation
C       Copyright 2009 United States Steel Corporation
C       All rights reserved.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       ORACLE_FIXTUMBLES
C
C       Scratch program to resend tumbles from MET_SCREEN:SCREEN.DAT to
C       Oracle prod2.tolive.aggl_tumbles table.
C
C       Revisions:
C               20090508 JOH8811 (No ticket number): Initial version.
C         20100809 IM322852  SPA6635 changed COMP2(2,3,2,5) to COMP2(3,3,2,5)

      CHARACTER*8 KD

      EXTERNAL oraclD
      INTEGER*4 SQLITR
      INTEGER*4 SQHSTV(28)
      INTEGER*4 SQHSTL(28)
      INTEGER*4 SQINDV(28)
      INTEGER*4 SQHARM(28)
      INTEGER*4 SQHARC(28)
        CHARACTER*444 SQ0001
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(226)
      CHARACTER*21 SQLFNM
      COMMON / oraclI / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / oraclC / SQLFNM, SQ0001
        INTEGER
     1  DAT(2),
     1  IS,
     1  IH,
     1  S4,
     1  H4
        DOUBLE PRECISION SCREEN(14,3,2,5),
     1  COMP(2,3,2,5),
     1  CAB(2,3,5),
     1  SIL(3,5),
     1  FILT2(5,3),
     1  FILT3(5,3),
     1  BL2,
     1  BL3,
     1  FLUXG,
     1  FCA(2,3),
     1  FMG(2,3),
     1  FAL(2,3),
     1  FSI(2,3),
     1  PA(3,5),
     1  PC(3,5),
     1  PM(3,5),
     1  SCWT(3,2,5),
     1  TONS4(3,2,5),
     1  GHRS(3,2,5),
C     1  COMP2(2,3,2,5)
     1  COMP2(3,3,2,5)

        REAL*4 PCT(14,3,2,5),
     1  TOTW,
     1  TOTW3
C
C SQL variables
C
C
C       EXEC SQL INCLUDE SQLCA
C
C     
C      sqlca.for 
C     
C     *****************************************************************
C     *                                                               *
C     *               S  Q  L  C  A  .  F  O  R                       *
C     *                                                               *
C     *****************************************************************
 
C     *****************************************************************
C     *  Note: some compilers do  not  allow CHAR and INT data in the *
C     *  same common block;  therefore the CHAR vars are declared as  *
C     *  LOGICAL*1.  Sigh.                                            *
C     *****************************************************************
 
       LOGICAL*1   SQLAID  (8)
       INTEGER*4   SQLABC
       INTEGER*4   SQLCDE
C    SQLERRM
       INTEGER*2   SQLEML
       LOGICAL*1   SQLEMC (70)
       LOGICAL*1   SQLERP  (8)
       INTEGER*4   SQLERD  (6)
C    SQLWRN(8)
       LOGICAL*1   SQLWN0, SQLWN1, SQLWN2, SQLWN3,
     1             SQLWN4, SQLWN5, SQLWN6, SQLWN7
       LOGICAL*1   SQLEXT  (8)
C
       COMMON /SQLCA/
     1   SQLAID,
     2   SQLABC,
     3   SQLCDE,
     4   SQLEML,
     5   SQLEMC,
     6   SQLERP,
     7   SQLERD,
     8   SQLWN0, SQLWN1, SQLWN2, SQLWN3,
     9   SQLWN4, SQLWN5, SQLWN6, SQLWN7,
     1   SQLEXT
C
C     *****************************************************************
C     *  The following common block is provided for use by Dynamic    *
C     *  Method 4.  Along with the SQLCA, it must be present in any   *
C     *  subroutine which executes a dynamic SQL statement using      *
C     *  descriptors.                                                 *
C     *****************************************************************
C
       INTEGER *4   DSC2N
       INTEGER *4   DSC2V
       INTEGER *4   DSC2L
       INTEGER *4   DSC2T
       INTEGER *4   DSC2I
       INTEGER *4   DSC2F
       INTEGER *4   DSC2S
       INTEGER *4   DSC2M
       INTEGER *4   DSC2C
       INTEGER *4   DSC2X
       INTEGER *4   DSC2Y
       INTEGER *4   DSC2Z
C
       COMMON /DSC2/   DSC2N, DSC2V, DSC2L, DSC2T, DSC2I, DSC2F,
     1                 DSC2S, DSC2M, DSC2C, DSC2X, DSC2Y, DSC2Z
C
C       EXEC SQL BEGIN DECLARE SECTION
                CHARACTER*10    USERNAME
                CHARACTER*20    PASSWORD
                CHARACTER*12    DBSTR1
                CHARACTER*400   DYNSTM
                CHARACTER*11    DATEX
                INTEGER         SHIFT
                INTEGER         HALF
                INTEGER         LINE
                REAL*4          B58
                REAL*4          B12
                REAL*4          B38
                REAL*4          B14
                REAL*4          B4M
                REAL*4          B28
                REAL*4          BM28
                REAL*4          A58
                REAL*4          A12
                REAL*4          A38
                REAL*4          A14
                REAL*4          A4M
                REAL*4          A28
                REAL*4          AM28
                REAL*4          ACOMP
                REAL*4          ACOMP200
                REAL*4          B916
                REAL*4          A916
                REAL*4          COMP600
                REAL*4          C600M200
                REAL*4          B716
                REAL*4          A716
                REAL*4          TONS
                REAL*4          HOURS
                CHARACTER*1     MTYPE
C       EXEC SQL END DECLARE SECTION
C
C       EXEC SQL WHENEVER SQLERROR GOTO 900
C
C Connect to database
C
        USERNAME='TOLIVE'
        PASSWORD='MANAGER2'
        DBSTR1='PROD2'
C
C       EXEC SQL DECLARE DBNAM1 DATABASE
C
C       EXEC SQL CONNECT :USERNAME IDENTIFIED BY :PASSWORD
C    1          AT DBNAM1 USING :DBSTR1
C
C Retrieve tumble/compression record
C
        CALL SQLADR(%REF(USERNAME), SQHSTV(1))
        SQHSTL(1) = 10
        SQINDV(1) = 0
        SQHARM(1) = 0
        CALL SQLADR(%REF(PASSWORD), SQHSTV(2))
        SQHSTL(2) = 20
        SQINDV(2) = 0
        SQHARM(2) = 0
        CALL SQLADR(%REF(DBSTR1), SQHSTV(3))
        SQHSTL(3) = 12
        SQINDV(3) = 0
        SQHARM(3) = 0
        CALL SQLFIN(21, %REF(SQLFNM))
        SQLITR = 10
        CALL SQLFX7(6, %REF(SQLCTX), 3, SQLITR, 2, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
        IF (SQLCDE .LT. 0) GO TO 900
        OPEN (11,FILE='MET_SCREEN:SCREEN.DAT',
     1  ACCESS='KEYED',STATUS='OLD',SHARED)
C
15      READ (11,KEY='20090503',IOSTAT=MM) KD,DAT,IS,IH,SCREEN,COMP,
     1          CAB,SIL,FILT2,
     1          FILT3,BL2,BL3,FLUXG,FCA,FMG,FAL,FSI,   
     1          PA,PC,PM,S4,H4,SCWT,TONS4,GHRS,COMP2
C
        IF (MM .NE. 0) GO TO 999
C
C Calculate data for database record
C (Calculations copied from ORACLE_TEST9I program.)
C
20      CALL SYS$ASCTIM (,ASD,DAT,)
C
        DO 100 NL=1,5 ! Loop through lines
        DO 90  NS=1,3 ! Loop through shifts
        DO 80  NH=1,2 ! Loop through shifthalves
                TOTW=0
                TOTW3=0
                DO 70 I=1,7
                        TOTW=TOTW+SCREEN(I,NS,NH,NL)
                        TOTW3=TOTW3+SCREEN(I+7,NS,NH,NL)
                        PCT(I,NS,NH,NL)=0
                        PCT(I+7,NS,NH,NL)=0
70              CONTINUE
                IF (TOTW .GT. 0 .AND. TOTW3 .GT. 0) THEN
                        DO 71 I=1,7
                                PCT(I,NS,NH,NL)=
     1                          SCREEN(I,NS,NH,NL)/TOTW*100.
                                PCT(I+7,NS,NH,NL)=
     1                          SCREEN(I+7,NS,NH,NL)/TOTW3*100.
71                      CONTINUE
                END IF
C
C Set SQL variables
C
                CALL SYS$ASCTIM (,DATEX,DAT,)
                SHIFT=NS
                HALF=NH
                LINE=NL+2
                B58=0
                B916=PCT(1,NS,NH,NL)
                B12=PCT(2,NS,NH,NL)
                B716=PCT(3,NS,NH,NL)
                B38=PCT(4,NS,NH,NL)
                B14=PCT(5,NS,NH,NL)
                B4M=0
                B28=PCT(6,NS,NH,NL)
                BM28=PCT(7,NS,NH,NL)
                A58=0
                A916=PCT(8,NS,NH,NL)
                A12=PCT(9,NS,NH,NL)
                A716=PCT(10,NS,NH,NL)
                A38=PCT(11,NS,NH,NL)
                A14=PCT(12,NS,NH,NL)
                A4M=0
                A28=PCT(13,NS,NH,NL)
                AM28=PCT(14,NS,NH,NL)
                ACOMP=COMP2(1,NS,NH,NL)
                ACOMP200=COMP2(2,NS,NH,NL)
                COMP600=COMP(1,NS,NH,NL)
                C600M200=COMP(2,NS,NH,NL)
                TONS=TONS4(NS,NH,NL)
                HOURS=GHRS(NS,NH,NL)
C
C Execute update statement
C
                WRITE (6,*) DATEX, SHIFT, HALF, LINE
C       EXEC SQL AT DBNAM1 UPDATE AGGL_TUMBLES 
C    1  SET B58=:B58,B12=:B12,B38=:B38,B14=:B14,
C    1          B4M=:B4M,B28=:B28,BM28=:BM28,A58=:A58,
C    1          A12=:A12,A38=:A38,
C    1          A14=:A14,A4M=:A4M,A28=:A28,
C    1          AM28=:AM28,ACOMP=:ACOMP,ACOMP200=:ACOMP200,
C    1          B916=:B916,A916=:A916,COMP600=:COMP600,
C    1          C600M200=:C600M200,B716=:B716,A716=:A716,
C    1          TONS=:TONS,HOURS=:HOURS
C    1  WHERE 
C    1          TRUNC(DATEX)=TRUNC(TO_DATE(:DATEX,'DD-MON-YYYY')) AND
C    1          SHIFT=:SHIFT AND HALF=:HALF AND LINE=:LINE
C
        CALL SQLADR(%REF(B58), SQHSTV(1))
        SQHSTL(1) = 4
        SQINDV(1) = 0
        SQHARM(1) = 0
        CALL SQLADR(%REF(B12), SQHSTV(2))
        SQHSTL(2) = 4
        SQINDV(2) = 0
        SQHARM(2) = 0
        CALL SQLADR(%REF(B38), SQHSTV(3))
        SQHSTL(3) = 4
        SQINDV(3) = 0
        SQHARM(3) = 0
        CALL SQLADR(%REF(B14), SQHSTV(4))
        SQHSTL(4) = 4
        SQINDV(4) = 0
        SQHARM(4) = 0
        CALL SQLADR(%REF(B4M), SQHSTV(5))
        SQHSTL(5) = 4
        SQINDV(5) = 0
        SQHARM(5) = 0
        CALL SQLADR(%REF(B28), SQHSTV(6))
        SQHSTL(6) = 4
        SQINDV(6) = 0
        SQHARM(6) = 0
        CALL SQLADR(%REF(BM28), SQHSTV(7))
        SQHSTL(7) = 4
        SQINDV(7) = 0
        SQHARM(7) = 0
        CALL SQLADR(%REF(A58), SQHSTV(8))
        SQHSTL(8) = 4
        SQINDV(8) = 0
        SQHARM(8) = 0
        CALL SQLADR(%REF(A12), SQHSTV(9))
        SQHSTL(9) = 4
        SQINDV(9) = 0
        SQHARM(9) = 0
        CALL SQLADR(%REF(A38), SQHSTV(10))
        SQHSTL(10) = 4
        SQINDV(10) = 0
        SQHARM(10) = 0
        CALL SQLADR(%REF(A14), SQHSTV(11))
        SQHSTL(11) = 4
        SQINDV(11) = 0
        SQHARM(11) = 0
        CALL SQLADR(%REF(A4M), SQHSTV(12))
        SQHSTL(12) = 4
        SQINDV(12) = 0
        SQHARM(12) = 0
        CALL SQLADR(%REF(A28), SQHSTV(13))
        SQHSTL(13) = 4
        SQINDV(13) = 0
        SQHARM(13) = 0
        CALL SQLADR(%REF(AM28), SQHSTV(14))
        SQHSTL(14) = 4
        SQINDV(14) = 0
        SQHARM(14) = 0
        CALL SQLADR(%REF(ACOMP), SQHSTV(15))
        SQHSTL(15) = 4
        SQINDV(15) = 0
        SQHARM(15) = 0
        CALL SQLADR(%REF(ACOMP200), SQHSTV(16))
        SQHSTL(16) = 4
        SQINDV(16) = 0
        SQHARM(16) = 0
        CALL SQLADR(%REF(B916), SQHSTV(17))
        SQHSTL(17) = 4
        SQINDV(17) = 0
        SQHARM(17) = 0
        CALL SQLADR(%REF(A916), SQHSTV(18))
        SQHSTL(18) = 4
        SQINDV(18) = 0
        SQHARM(18) = 0
        CALL SQLADR(%REF(COMP600), SQHSTV(19))
        SQHSTL(19) = 4
        SQINDV(19) = 0
        SQHARM(19) = 0
        CALL SQLADR(%REF(C600M200), SQHSTV(20))
        SQHSTL(20) = 4
        SQINDV(20) = 0
        SQHARM(20) = 0
        CALL SQLADR(%REF(B716), SQHSTV(21))
        SQHSTL(21) = 4
        SQINDV(21) = 0
        SQHARM(21) = 0
        CALL SQLADR(%REF(A716), SQHSTV(22))
        SQHSTL(22) = 4
        SQINDV(22) = 0
        SQHARM(22) = 0
        CALL SQLADR(%REF(TONS), SQHSTV(23))
        SQHSTL(23) = 4
        SQINDV(23) = 0
        SQHARM(23) = 0
        CALL SQLADR(%REF(HOURS), SQHSTV(24))
        SQHSTL(24) = 4
        SQINDV(24) = 0
        SQHARM(24) = 0
        CALL SQLADR(%REF(DATEX), SQHSTV(25))
        SQHSTL(25) = 11
        SQINDV(25) = 0
        SQHARM(25) = 0
        CALL SQLADR(%REF(SHIFT), SQHSTV(26))
        SQHSTL(26) = 4
        SQINDV(26) = 0
        SQHARM(26) = 0
        CALL SQLADR(%REF(HALF), SQHSTV(27))
        SQHSTL(27) = 4
        SQINDV(27) = 0
        SQHARM(27) = 0
        CALL SQLADR(%REF(LINE), SQHSTV(28))
        SQHSTL(28) = 4
        SQINDV(28) = 0
        SQHARM(28) = 0
        CALL SQLFIN(21, %REF(SQLFNM))
        SQLITR = 1
        CALL SQLFX7(6, %REF(SQLCTX), 28, SQLITR, 39, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), %REF(SQ0001),
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
        IF (SQLCDE .LT. 0) GO TO 900
        GOTO 80 ! Next loop
C
C Loop continue statements
C
80      CONTINUE ! NH=1,2 ! Loop through shifthalves
90      CONTINUE ! NS=1,3 ! Loop through shifts
100     CONTINUE ! NL=1,5 ! Loop through lines
C
C Go to end
C
        GOTO 990
C
C Show error
C
900             CONTINUE
                WRITE (6,*) SQLCDE
                WRITE (6,'(70A)') SQLEMC
C               EXEC SQL WHENEVER SQLERROR CONTINUE
C               EXEC SQL AT DBNAM1 ROLLBACK RELEASE
      CALL SQLFIN(21, %REF(SQLFNM))
      SQLITR = 1
      CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 176, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
                GOTO 999
C
C End of program
C
  990 CONTINUE
C90     EXEC SQL AT DBNAM1 COMMIT RELEASE
C
        CALL SQLFIN(21, %REF(SQLFNM))
        SQLITR = 1
        CALL SQLFX7(6, %REF(SQLCTX), 0, SQLITR, 201, 1, 256,
     +%REF(SQC000), %REF(SQLAID(1)), 0,
     +SQHSTV, SQHSTL, SQINDV, SQHARM, SQHARC)
999     STOP
        END

      BLOCK DATA oraclD
      INTEGER*2 SQCNT
        CHARACTER*444 SQ0001
      INTEGER*4 SQLCTX
      INTEGER*4 IAPSUC,IAPFAI,IAPFTL
      INTEGER*2 SQC000(226)
      CHARACTER*21 SQLFNM
      COMMON / oraclI / IAPSUC,IAPFAI,IAPFTL,SQLCTX,SQC000
      COMMON / oraclC / SQLFNM, SQ0001
      DATA SQLCTX / 4848 /
      DATA IAPSUC / 0 /, IAPFAI / 1403 /, IAPFTL / 535 /
      DATA SQ0001 /'UPDATE AGGL_TUMBLES   SET B58=:b1,B12=:b2,B38=:b3,B1
     +4=:b4,          B4M=:b5,B28=:b6,BM28=:b7,A58=:b8,          A12=:b9
     +,A38=:b10,          A14=:b11,A4M=:b12,A28=:b13,          AM28=:b14
     +,ACOMP=:b15,ACOMP200=:b16,          B916=:b17,A916=:b18,COMP600=:b
     +19,          C600M200=:b20,B716=:b21,A716=:b22,          TONS=:b23
     +,HOURS=:b24 WHERE           TRUNC(DATEX)=TRUNC(TO_DATE(:b25,''DD-M
     +ON-YYYY'')) AND          SHIFT=:b26 AND HALF=:b27 AND LINE=:b28 '/
      DATA SQLFNM /'oracle_fixtumbles.PFO'/
      DATA (SQC000(SQCNT), SQCNT=1, 226) /6,4130,2,8,0,0,0,0,0,0,0,0,0,
     +0,27,99,3,3,0,0,6,68,66,78,65,77,49,1,1,0,0,1,1,0,0,1,1,0,0,39,8,
     +0,0,0,0,0,0,0,0,1,444,5,173,28,28,0,3,6,68,66,78,65,77,49,1,4,0,
     +0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,
     +0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,
     +0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,4,0,0,1,1,0,
     +0,1,3,0,0,1,3,0,0,1,3,0,0,176,8,0,0,0,0,0,0,0,0,1,0,32,204,0,0,0,
     +3,6,68,66,78,65,77,49,201,8,0,0,0,0,0,0,0,0,1,0,30,209,0,0,0,3,6,
     +68,66,78,65,77,49/
      END
