       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. SCHEDULE-MASTER.                                     
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE1     ASSIGN        TO
                                       '../FILES/201501.TXT'            
                               ORGANIZATION  IS LINE SEQUENTIAL.
           SELECT IN-FILE2     ASSIGN        TO
                                       '../FILES/201502.TXT'            
                               ORGANIZATION  IS LINE SEQUENTIAL.
           SELECT IN-FILE3     ASSIGN        TO
                                       '../FILES/201504.TXT'           
                               ORGANIZATION  IS LINE SEQUENTIAL.
           SELECT IN-FILE4     ASSIGN        TO
                                       '../FILES/201505.TXT'            
                               ORGANIZATION  IS LINE SEQUENTIAL.
           SELECT INST-MST     ASSIGN TO
                                       '../FILES/INSTR-MASTER.DAT'
                               ORGANIZATION IS INDEXED
                               ACCESS        IS SEQUENTIAL
                               RECORD KEY    IS INST-ID
                               ALTERNATE KEY IS INST-NAME
                               FILE STATUS   IS WS-STAT.
                               
           SELECT SORT-WORK    ASSIGN        TO 'SORTWORK.TXT'.
           
           SELECT OUT-FILE     ASSIGN        TO
                                       '../FILES/SCHEDULE-MASTER.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS DYNAMIC
                               RECORD KEY    IS SCHEDULE-ID-O
                               FILE STATUS   IS WS-STAT.
                               
                               
           SELECT MST-CTRL-LIST    ASSIGN TO
                                       "../Files/MST-CTRL-LST.DAT"
                               ORGANIZATION IS RELATIVE
                               ACCESS IS RANDOM
                               RELATIVE KEY IS WS-MST-REC-KEY
                               FILE STATUS IS WS-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       COPY MST-CTRL-LIST-RECS.
       
       FD  IN-FILE1.
       01  IN-REC1.
           03  FILLER              PIC X(6).
           03  COURSE-ID-1         PIC X(9).
           03  FILLER              PIC X(6).
           03  COURSE-NAME-1       PIC X(30).
           03  FILLER              PIC X(6).
           03  COURSE-CREDIT-1.
               05 CRS-CRED-CHECK-1 PIC 9.
               05 COURSE-REST      PIC XX.
           03  FILLER              PIC X.
           03  I-TIMEDAY-1         PIC X(20).
           03  BUILDING-ID-1       PIC X(11).
           03  FILLER              PIC XX.
           03  INSTRUCTOR-NAME-1   PIC X(22).
           03  OPEN-SEATS-1        PIC X(2).
           03  FILLER              PIC XX.
           03  ENR-1.
               05 ENR-CHECK-1      PIC 9.
               05 ENR-REST-1       PIC 9.
       FD  IN-FILE2.
       01  IN-REC2.
           03  FILLER              PIC X(6).
           03  COURSE-ID-2         PIC X(9).
           03  FILLER              PIC X(6).
           03  COURSE-NAME-2       PIC X(30).
           03  FILLER              PIC X(6).
           03  COURSE-CREDIT-2.
               05 CRS-CRED-CHECK-2 PIC 9.
               05 COURSE-REST      PIC XX.
           03  FILLER              PIC X.
           03  I-TIMEDAY-2         PIC X(20).
           03  BUILDING-ID-2       PIC X(11).
           03  FILLER              PIC XX.
           03  INSTRUCTOR-NAME-2   PIC X(22).
           03  OPEN-SEATS-2        PIC X(2).
           03  FILLER              PIC XX.
           03  ENR-2.
               05 ENR-CHECK-2      PIC 9.
               05 ENR-REST-2       PIC 9.
       FD  IN-FILE3.
       01  IN-REC3.
           03  FILLER              PIC X(6).
           03  COURSE-ID-3         PIC X(9).
           03  FILLER              PIC X(6).
           03  COURSE-NAME-3       PIC X(30).
           03  FILLER              PIC X(6).
           03  COURSE-CREDIT-3.
               05 CRS-CRED-CHECK-3 PIC 9.
               05 COURSE-REST      PIC XX.
           03  FILLER              PIC X.
           03  I-TIMEDAY-3         PIC X(20).
           03  BUILDING-ID-3       PIC X(11).
           03  FILLER              PIC XX.
           03  INSTRUCTOR-NAME-3   PIC X(22).
           03  OPEN-SEATS-3        PIC X(2).
           03  FILLER              PIC XX.
           03  ENR-3.
               05 ENR-CHECK-3      PIC 9.
               05 ENR-REST-3       PIC 9.
       FD  IN-FILE4.
       01  IN-REC4.
           03  FILLER              PIC X(6).
           03  COURSE-ID-4         PIC X(9).
           03  FILLER              PIC X(6).
           03  COURSE-NAME-4       PIC X(30).
           03  FILLER              PIC X(6).
           03  COURSE-CREDIT-4.
               05 CRS-CRED-CHECK-4 PIC 9.
               05 COURSE-REST      PIC XX.
           03  FILLER              PIC X.
           03  I-TIMEDAY-4         PIC X(20).
           03  BUILDING-ID-4       PIC X(11).
           03  FILLER              PIC XX.
           03  INSTRUCTOR-NAME-4   PIC X(22).
           03  OPEN-SEATS-4        PIC X(2).
           03  FILLER              PIC XX.
           03  ENR-4.
               05 ENR-CHECK-4      PIC 9.
               05 ENR-REST-4       PIC 9.
       FD  INST-MST.
       01  INST-REC.
           03  INST-ID    PIC 9999.
           03  INST-NAME  PIC X(22).
           
       SD  SORT-WORK.
       01  SRT-REC.
           03  SCHEDULE-ID         PIC X(12).
           03  FILLER              PIC X.
           03  COURSE-ID-S         PIC X(9).
           03  FILLER              PIC X.
           03  COURSE-NAME-S       PIC X(30).
           03  FILLER              PIC X.
           03  COURSE-CREDIT-S     PIC X(3).
           03  FILLER              PIC X.
           03  TIMEDAY-S         PIC X(20).
           03  FILLER              PIC X.
           03  BUILDING-ID-S       PIC X(11).
           03  FILLER              PIC X.
           03  INSTRUCTOR-NAME-S   PIC X(22).
           03  FILLER              PIC X.
           03  OPEN-SEATS-S        PIC X(2).

       
     
       FD  OUT-FILE.
       01  OUT-REC.
           03  SCHEDULE-ID-O       PIC X(12).
           03  FILLER              PIC X.
           03  COURSE-ID-O         PIC X(9).
           03  FILLER              PIC X.
           03  TIMEDAY-O           PIC X(20).
           03  FILLER              PIC X.
           03  BUILDING-ID-O       PIC X(11).
           03  FILLER              PIC X.
           03  INSTRUCTOR-ID-O     PIC X(4).
           03  FILLER              PIC X(3).
           03  OPEN-SEATS-O        PIC X(2).

       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-SCHED-ID.
               05 SCHED-YR         PIC 9(4).
               05 FILLER           PIC X VALUE SPACES.
               05 SCHED-TM         PIC 99.
               05 FILLER           PIC X VALUE SPACES.
               05 SCHED-CRN        PIC 9(4).
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-INST-NAME        PIC X(22).
           03  WS-INST-ID          PIC X(4).
           03  WS-MST-REC-KEY          PIC 99.

       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.           
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           
           OPEN OUTPUT OUT-FILE.
           OPEN INPUT INST-MST.
           OPEN I-O MST-CTRL-LIST.
           PERFORM 100-SORT-FILES
           DISPLAY BLNK-SCRN.
           DISPLAY 'MASTER BUILT SUCCESSFULLY'.
           DISPLAY 'PRESS ENTER TO RETURN TO MENU'.
           ACCEPT WS-RESP.
           CLOSE MST-CTRL-LIST.
           CLOSE OUT-FILE.
           
           EXIT PROGRAM.
      *-----------------------------------------------------------------
       100-SORT-FILES.
       SORT SORT-WORK
                ON ASCENDING KEY SCHEDULE-ID
                INPUT  PROCEDURE 110-INPUT-1
                OUTPUT PROCEDURE 300-FILE-OUT.
       SORT SORT-WORK
                ON ASCENDING KEY SCHEDULE-ID
                INPUT  PROCEDURE 120-INPUT-2
                OUTPUT PROCEDURE 300-FILE-OUT.
       SORT SORT-WORK
                ON ASCENDING KEY SCHEDULE-ID
                INPUT  PROCEDURE 130-INPUT-3
                OUTPUT PROCEDURE 300-FILE-OUT.
       SORT SORT-WORK
                ON ASCENDING KEY SCHEDULE-ID
                INPUT  PROCEDURE 140-INPUT-4
                OUTPUT PROCEDURE 300-FILE-OUT.
      *-----------------------------------------------------------------
         110-INPUT-1.
           OPEN INPUT IN-FILE1.
           MOVE 2015 TO SCHED-YR.
           MOVE 01 TO SCHED-TM.
           MOVE 0001 TO SCHED-CRN.
               PERFORM UNTIL EOF
                   READ IN-FILE1 
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           IF ENR-CHECK-1 IS NUMERIC AND
                           CRS-CRED-CHECK-1 IS NUMERIC
                               MOVE WS-SCHED-ID     TO SCHEDULE-ID
                               MOVE COURSE-ID-1     TO COURSE-ID-S
                               MOVE COURSE-NAME-1   TO COURSE-NAME-S
                               MOVE COURSE-CREDIT-1 TO COURSE-CREDIT-S
                               MOVE BUILDING-ID-1   TO BUILDING-ID-S
                               MOVE I-TIMEDAY-1     TO TIMEDAY-S
                               MOVE INSTRUCTOR-NAME-1 TO  
                                   INSTRUCTOR-NAME-S
                               MOVE OPEN-SEATS-1    TO OPEN-SEATS-S
                               ADD 1 TO SCHED-CRN GIVING SCHED-CRN
                               RELEASE SRT-REC
                           END-IF
                   END-READ
               END-PERFORM.
               MOVE 3 TO WS-MST-REC-KEY.
               ADD 1 TO SCHED-CRN GIVING SCHED-CRN.
               MOVE SCHED-CRN TO MST-NEXT-CRN-CRN.
               MOVE SCHED-YR TO MST-NEXT-CRN-YR.
               MOVE SCHED-TM TO MST-NEXT-CRN-SEM.
               REWRITE  MST-NEXT-CRNS.
               CLOSE IN-FILE1.
               
      *-----------------------------------------------------------------
         120-INPUT-2.
           MOVE 'N' TO WS-EOF.
           OPEN INPUT IN-FILE2.
           MOVE 2015 TO SCHED-YR.
           MOVE 02 TO SCHED-TM.
           MOVE 0001 TO SCHED-CRN.
               PERFORM UNTIL EOF
                   READ IN-FILE2 
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           IF ENR-CHECK-2 IS NUMERIC AND
                           CRS-CRED-CHECK-2 IS NUMERIC
                               MOVE WS-SCHED-ID     TO SCHEDULE-ID
                               MOVE COURSE-ID-2     TO COURSE-ID-S
                               MOVE COURSE-NAME-2   TO COURSE-NAME-S
                               MOVE COURSE-CREDIT-2 TO COURSE-CREDIT-S
                               MOVE BUILDING-ID-2   TO BUILDING-ID-S
                               MOVE I-TIMEDAY-2     TO TIMEDAY-S
                               MOVE INSTRUCTOR-NAME-2 TO 
                                   INSTRUCTOR-NAME-S
                               MOVE OPEN-SEATS-2    TO OPEN-SEATS-S
                               ADD 1 TO SCHED-CRN GIVING SCHED-CRN
                               RELEASE SRT-REC
                           END-IF
                   END-READ
               END-PERFORM.
               MOVE 7 TO WS-MST-REC-KEY.
               ADD 1 TO SCHED-CRN GIVING SCHED-CRN.
               MOVE SCHED-CRN TO MST-NEXT-CRN-CRN.
               MOVE SCHED-YR TO MST-NEXT-CRN-YR.
               MOVE SCHED-TM TO MST-NEXT-CRN-SEM.
               REWRITE  MST-NEXT-CRNS.
               CLOSE IN-FILE2.
               
               
      *-----------------------------------------------------------------
         130-INPUT-3.
           MOVE 'N' TO WS-EOF.
           OPEN INPUT IN-FILE3.
           MOVE 2015 TO SCHED-YR.
           MOVE 04 TO SCHED-TM.
           MOVE 0001 TO SCHED-CRN.
               PERFORM UNTIL EOF
                   READ IN-FILE3 
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           IF ENR-CHECK-3 IS NUMERIC AND
                           CRS-CRED-CHECK-3 IS NUMERIC
                               MOVE WS-SCHED-ID     TO SCHEDULE-ID
                               MOVE COURSE-ID-3     TO COURSE-ID-S
                               MOVE COURSE-NAME-3   TO COURSE-NAME-S
                               MOVE COURSE-CREDIT-3 TO COURSE-CREDIT-S
                               MOVE BUILDING-ID-3   TO BUILDING-ID-S
                               MOVE I-TIMEDAY-3     TO TIMEDAY-S
                               MOVE INSTRUCTOR-NAME-3 TO 
                                   INSTRUCTOR-NAME-S
                               MOVE OPEN-SEATS-3    TO OPEN-SEATS-S
                               ADD 1 TO SCHED-CRN GIVING SCHED-CRN
                               RELEASE SRT-REC
                           END-IF
                   END-READ
               END-PERFORM.
               MOVE 8 TO WS-MST-REC-KEY.
               ADD 1 TO SCHED-CRN GIVING SCHED-CRN.
               MOVE SCHED-CRN TO MST-NEXT-CRN-CRN.
               MOVE SCHED-YR TO MST-NEXT-CRN-YR.
               MOVE SCHED-TM TO MST-NEXT-CRN-SEM.
               REWRITE  MST-NEXT-CRNS.
               CLOSE IN-FILE3.
               
               
      *-----------------------------------------------------------------
         140-INPUT-4.
           MOVE 'N' TO WS-EOF.
           OPEN INPUT IN-FILE4.
           MOVE 2015 TO SCHED-YR.
           MOVE 05 TO SCHED-TM.
           MOVE 0001 TO SCHED-CRN.
               PERFORM UNTIL EOF
                   READ IN-FILE4 
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           IF ENR-CHECK-4 IS NUMERIC AND
                           CRS-CRED-CHECK-4 IS NUMERIC
                               MOVE WS-SCHED-ID     TO SCHEDULE-ID
                               MOVE COURSE-ID-4     TO COURSE-ID-S
                               MOVE COURSE-NAME-4   TO COURSE-NAME-S
                               MOVE COURSE-CREDIT-4 TO COURSE-CREDIT-S
                               MOVE BUILDING-ID-4   TO BUILDING-ID-S
                               MOVE I-TIMEDAY-4     TO TIMEDAY-S
                               MOVE INSTRUCTOR-NAME-4 TO 
                                   INSTRUCTOR-NAME-S
                               MOVE OPEN-SEATS-4    TO OPEN-SEATS-S
                               ADD 1 TO SCHED-CRN GIVING SCHED-CRN
                               RELEASE SRT-REC
                           END-IF
                   END-READ
               END-PERFORM.
               MOVE 4 TO WS-MST-REC-KEY.
               ADD 1 TO SCHED-CRN GIVING SCHED-CRN.
               MOVE SCHED-CRN TO MST-NEXT-CRN-CRN.
               MOVE SCHED-YR TO MST-NEXT-CRN-YR.
               MOVE SCHED-TM TO MST-NEXT-CRN-SEM.
               REWRITE  MST-NEXT-CRNS.
               CLOSE IN-FILE4.                                          
      *-----------------------------------------------------------------
       
       
       
       
       300-FILE-OUT.
           MOVE 'N' TO WS-EOF.
           PERFORM UNTIL EOF
               RETURN SORT-WORK 
                   AT END 
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE INSTRUCTOR-NAME-S TO INST-NAME
                       START INST-MST KEY EQUALS INST-NAME
                           INVALID KEY
                               MOVE 9999 TO INSTRUCTOR-ID-O
                           NOT INVALID KEY
                               READ INST-MST
                               MOVE INST-ID TO INSTRUCTOR-ID-O
                       MOVE SCHEDULE-ID TO SCHEDULE-ID-O
                       MOVE COURSE-ID-S TO COURSE-ID-O
                       MOVE TIMEDAY-S TO TIMEDAY-O
                       MOVE BUILDING-ID-S TO BUILDING-ID-O
                       MOVE OPEN-SEATS-S TO OPEN-SEATS-O
                       WRITE OUT-REC
               END-RETURN
           END-PERFORM.
           
           
           
      
               
               