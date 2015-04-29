       IDENTIFICATION DIVISION.
       PROGRAM-ID. REGISTER-ADD.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL.
           SELECT REG-MASTER   ASSIGN        TO 
                               '../FILES/REGISTER-MASTER.DAT'   
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS REG-KEY
                               FILE STATUS   IS WS-STAT.
           SELECT STU-MST      ASSIGN TO 
                               '../FILES/STUDENT-MASTER.DAT'
                               ORGANIZATION    IS INDEXED
                               ACCESS          IS RANDOM
                               RECORD KEY      IS STU-ID
                               ALTERNATE   KEY IS STU-NAME
                               FILE STATUS     IS WS-STAT.
           SELECT SCHE-MST     ASSIGN TO
                               '../FILES/SCHEDULE-MST.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS SCHEDULE-ID-O
                               FILE STATUS   IS WS-STAT.
           SELECT CRSE-MASTER  ASSIGN        TO 
                               '../FILES/COURSE-MASTER-SORT.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS CRSE-ID
                               FILE STATUS   IS WS-STAT.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       COPY STU-MST-DEF.
       FD  REG-MASTER.
       01  REG-REC.
           03  REG-KEY.
               05  REG-STU-ID          PIC 9(4).
               05  REG-YEAR            PIC 9999.
               05  REG-SEM             PIC 99.
           03  FIRST-CRN           PIC 9(4).
           03  SECOND-CRN          PIC 9(4).
           03  THIRD-CRN           PIC 9(4).
           03  FOURTH-CRN          PIC 9(4).
           03  FIFTH-CRN           PIC 9(4).
       FD  SCHE-MST.
       01  SCHE-REC.
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
       FD  CRSE-MASTER.
       01  CRSE-REC.
           03  CRSE-ID        PIC X(9).
           03  CRSE-NAME      PIC X(35).
           03  CRSE-CREDIT    PIC X(4).
           03  CRSE-STAT      PIC X.
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-SAVE             PIC X   VALUE SPACE.
               88  SAVE                    VALUE 'Y' 'y'.
           03  WS-ANOTHER          PIC X   VALUE 'Y'.
               88  ANOTHER                 VALUE 'N' 'n'.
           03  WS-STU-NAME         PIC X(20).
           03  WS-SPACE            PIC X VALUE SPACE.
           03  WS-YEAR             PIC 9999.
           03  WS-SEM              PIC 99.
           03  WS-CRN              PIC 9999.
           03  WS-CRSE-NAME        PIC X(35).
       01  WS-DTL-LN.
           03  WS-STU-ID              PIC 9(4).
           03  WS-FIRST-CRN           PIC 9(4).
           03  WS-SECOND-CRN          PIC 9(4).
           03  WS-THIRD-CRN           PIC 9(4).
           03  WS-FOURTH-CRN          PIC 9(4).
           03  WS-FIFTH-CRN           PIC 9(4).
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.
       01  SCRN-TITLE.
           03  LINE 3  COL 30  VALUE 'REGISTER ADD CLASSES'.
       01  SCRN-DATA.
           03  SCRN-STU-ID.
               05  LINE 5  COL 25  VALUE   'STUDENT ID   : '.
               05          COL 40  PIC 9(4) TO WS-STU-ID          
                                            AUTO REQUIRED.
           03  SCRN-STU-NAME.
               05  LINE 7  COL 25  VALUE   'STUDENT NAME : '.
               05          COL 40  PIC X(20) FROM WS-STU-NAME.
           03  SCRN-YEAR-SEM.
               05  LINE 9  COL 15  VALUE   'YEAR: '.
               05          COL 22  PIC ZZZ9 TO WS-YEAR
                                            AUTO REQUIRED FULL.
               05          COL 50  VALUE   'SEMESTER: '.
               05          COL 61  PIC Z9   TO WS-SEM
                                            AUTO REQUIRED.               
       01  SCRN-CRN1.
           03  SCRN-FIRST-CRN1.
               05  LINE 11  COL 25  VALUE   'FIRST CRN    : '.
               05          COL 40  PIC ZZZ9 USING WS-FIRST-CRN
                                            AUTO.
           03  SCRN-SECOND-CRN1.
               05  LINE 12 COL 25  VALUE   'SECOND CRN   : '.
               05          COL 40  PIC ZZZ9 USING WS-SECOND-CRN
                                            AUTO.
           03  SCRN-THIRD-CRN1.
               05  LINE 13  COL 25  VALUE   'THIRD CRN    : '.
               05           COL 40  PIC ZZZ9 USING WS-THIRD-CRN
                                             AUTO.
           03  SCRN-FOURTH-CRN1.
               05  LINE 14  COL 25  VALUE   'FOURTH CRN   : '.
               05           COL 40  PIC ZZZ9 USING WS-FOURTH-CRN
                                             AUTO.
           03  SCRN-FIFTH-CRN1.
               05  LINE 15  COL 25  VALUE   'FIFTH CRN    : '.
               05           COL 40  PIC ZZZ9 USING WS-FIFTH-CRN
                                             AUTO.
       01  SCRN-CRN2.
           03  SCRN-FIRST-CRN2.
               05  LINE 11  COL 25  VALUE   'FIRST CRN    : '.
               05          COL 40  PIC ZZZ9 USING FIRST-CRN
                                            AUTO.
           03  SCRN-SECOND-CRN2.
               05  LINE 12 COL 25  VALUE   'SECOND CRN   : '.
               05          COL 40  PIC ZZZ9 USING SECOND-CRN
                                            AUTO.
           03  SCRN-THIRD-CRN2.
               05  LINE 13 COL 25  VALUE   'THIRD CRN    : '.
               05          COL 40  PIC ZZZ9 USING THIRD-CRN
                                            AUTO.
           03  SCRN-FOURTH-CRN2.
               05  LINE 14 COL 25  VALUE   'FOURTH CRN   : '.
               05          COL 40  PIC ZZZ9 USING FOURTH-CRN
                                            AUTO.
           03  SCRN-FIFTH-CRN2.
               05  LINE 15 COL 25  VALUE   'FIFTH CRN    : '.
               05          COL 40  PIC ZZZ9 USING FIFTH-CRN
                                            AUTO.
       01  SCRN-CLASS-NAME1.
           03  LINE 17  COL 15  PIC ZZZ9 FROM WS-CRN.
           03           COL 20  PIC X(35) FROM WS-CRSE-NAME.
       01  SCRN-CLASS-NAME2.
           03  LINE 18  COL 15  PIC ZZZ9 FROM WS-CRN.
           03           COL 20  PIC X(35) FROM WS-CRSE-NAME.
       01  SCRN-CLASS-NAME3.
           03  LINE 19  COL 15  PIC ZZZ9 FROM WS-CRN.
           03           COL 20  PIC X(35) FROM WS-CRSE-NAME.
       01  SCRN-CLASS-NAME4.
           03  LINE 20  COL 15  PIC ZZZ9 FROM WS-CRN.
           03           COL 20  PIC X(35) FROM WS-CRSE-NAME.
       01  SCRN-CLASS-NAME5.
           03  LINE 21  COL 15  PIC ZZZ9 FROM WS-CRN.
           03           COL 20  PIC X(35) FROM WS-CRSE-NAME.
       01  SCRN-SAVE.
           03  LINE 23  COL 32  VALUE     'SAVE (Y/N)'.
           03           COL 30  PIC X     TO WS-SAVE.
       01  SCRN-WRITE-ERR.
           03  LINE 5  COL 30  VALUE 'STUDENT CAN NOT BE FOUND'.
       01  SCRN-WRITE-SAVE.
           03  LINE 5  COL 30  VALUE 'REGISTERED SUCCESSFULLY'.
       01  SCRN-WRITE-NOT-SAVE.
           03  LINE 5  COL 30  VALUE 'REGISTERED UNSUCCESSFULLY'.      
       01  SCRN-ANOTHER.
           03  LINE 7 COL 32  VALUE 'CONTINUE? (Y/N)'.
           03          COL 30  PIC X TO WS-ANOTHER.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN I-O REG-MASTER.
           OPEN INPUT STU-MST.

           MOVE 'Y' TO WS-ANOTHER.
           PERFORM UNTIL ANOTHER
                   MOVE ZEROS TO WS-FIRST-CRN
                   MOVE ZEROS TO WS-SECOND-CRN
                   MOVE ZEROS TO WS-THIRD-CRN
                   MOVE ZEROS TO WS-FOURTH-CRN
                   MOVE ZEROS TO WS-FIFTH-CRN
                   DISPLAY BLNK-SCRN
                   DISPLAY SCRN-TITLE
                   DISPLAY SCRN-STU-ID
                   ACCEPT  SCRN-STU-ID
                   
                   MOVE WS-STU-ID TO STU-ID
                   READ STU-MST
                       INVALID KEY
                           DISPLAY BLNK-SCRN
                           DISPLAY SCRN-WRITE-ERR
                           DISPLAY SCRN-ANOTHER
                           ACCEPT  SCRN-ANOTHER
                       NOT INVALID KEY
                           STRING
                               STU-F-NAME DELIMITED BY SPACE
                               WS-SPACE   DELIMITED BY SIZE
                               STU-L-NAME DELIMITED BY SPACE
                               INTO WS-STU-NAME
                               DISPLAY SCRN-STU-NAME
                               DISPLAY SCRN-YEAR-SEM
                               ACCEPT  SCRN-YEAR-SEM
                               MOVE WS-STU-ID TO REG-STU-ID
                               MOVE WS-YEAR   TO REG-YEAR
                               MOVE WS-SEM    TO REG-SEM
                               READ REG-MASTER
                                   INVALID KEY
                                       MOVE SPACE TO WS-SAVE
                                       PERFORM UNTIL WS-SAVE = 'Y'
                                                  OR WS-SAVE = 'N'
                                           DISPLAY SCRN-CRN1
                                           ACCEPT  SCRN-FIRST-CRN1
                                           MOVE WS-FIRST-CRN TO WS-CRN
                                           PERFORM 300-GET-CLASS-NAME
                                           MOVE WS-CRN TO WS-FIRST-CRN
                                           DISPLAY SCRN-CLASS-NAME1
                                           ACCEPT  SCRN-SECOND-CRN1
                                           MOVE WS-SECOND-CRN TO WS-CRN
                                           PERFORM 300-GET-CLASS-NAME
                                           PERFORM 400-SECOND-CRN-VAL1
                                           MOVE WS-CRN TO WS-SECOND-CRN
                                           DISPLAY SCRN-CLASS-NAME2
                                           ACCEPT  SCRN-THIRD-CRN1
                                           MOVE WS-THIRD-CRN TO WS-CRN
                                           PERFORM 300-GET-CLASS-NAME
                                           PERFORM 410-THIRD-CRN-VAL1
                                           MOVE WS-CRN TO WS-THIRD-CRN
                                           DISPLAY SCRN-CLASS-NAME3
                                           ACCEPT  SCRN-FOURTH-CRN1
                                           MOVE WS-FOURTH-CRN TO WS-CRN
                                           PERFORM 300-GET-CLASS-NAME
                                           PERFORM 420-FOURTH-CRN-VAL1
                                           MOVE WS-CRN TO WS-FOURTH-CRN
                                           DISPLAY SCRN-CLASS-NAME4
                                           ACCEPT  SCRN-FIFTH-CRN1
                                           MOVE WS-FIFTH-CRN TO WS-CRN
                                           PERFORM 300-GET-CLASS-NAME
                                           PERFORM 430-FIFTH-CRN-VAL1
                                           MOVE WS-CRN TO WS-FIFTH-CRN
                                           DISPLAY SCRN-CLASS-NAME5
                                           DISPLAY SCRN-SAVE
                                           ACCEPT  SCRN-SAVE
                                       END-PERFORM
                                       PERFORM 100-SAVE1
                                   NOT INVALID KEY
                                       MOVE SPACE TO WS-SAVE
                                       PERFORM UNTIL WS-SAVE = 'Y'
                                                  OR WS-SAVE = 'N'                                       
                                           DISPLAY SCRN-CRN2
                                           ACCEPT  SCRN-FIRST-CRN2
                                           MOVE FIRST-CRN TO WS-CRN
                                           PERFORM 300-GET-CLASS-NAME
                                           PERFORM 480-FIRST-CRN-VAL2
                                           MOVE WS-CRN TO FIRST-CRN
                                           DISPLAY SCRN-CLASS-NAME1
                                           ACCEPT  SCRN-SECOND-CRN2
                                           MOVE SECOND-CRN TO WS-CRN
                                           PERFORM 300-GET-CLASS-NAME
                                           PERFORM 440-SECOND-CRN-VAL2
                                           MOVE WS-CRN TO SECOND-CRN
                                           DISPLAY SCRN-CLASS-NAME2
                                           ACCEPT  SCRN-THIRD-CRN2
                                           MOVE THIRD-CRN TO WS-CRN
                                           PERFORM 300-GET-CLASS-NAME
                                           PERFORM 450-THIRD-CRN-VAL2
                                           MOVE WS-CRN TO THIRD-CRN
                                           DISPLAY SCRN-CLASS-NAME3
                                           ACCEPT  SCRN-FOURTH-CRN2
                                           MOVE FOURTH-CRN TO WS-CRN
                                           PERFORM 300-GET-CLASS-NAME
                                           PERFORM 460-FOURTH-CRN-VAL2
                                           MOVE WS-CRN TO FOURTH-CRN
                                           DISPLAY SCRN-CLASS-NAME4
                                           ACCEPT  SCRN-FIFTH-CRN2
                                           MOVE FIFTH-CRN TO WS-CRN
                                           PERFORM 300-GET-CLASS-NAME
                                           PERFORM 470-FIFTH-CRN-VAL2
                                           MOVE WS-CRN TO FIFTH-CRN
                                           DISPLAY SCRN-CLASS-NAME5
                                           DISPLAY SCRN-SAVE
                                           ACCEPT  SCRN-SAVE
                                       END-PERFORM
                                       PERFORM 200-SAVE2
                   END-READ
           END-PERFORM.
           
           CLOSE REG-MASTER.
           CLOSE STU-MST.
           
           EXIT PROGRAM.
      *-----------------------------------------------------------------
       100-SAVE1.
           IF SAVE
               THEN
                   MOVE WS-YEAR TO REG-YEAR
                   MOVE WS-SEM TO REG-SEM
                   MOVE WS-FIRST-CRN TO FIRST-CRN
                   MOVE WS-SECOND-CRN TO SECOND-CRN
                   MOVE WS-THIRD-CRN TO THIRD-CRN
                   MOVE WS-FOURTH-CRN TO FOURTH-CRN
                   MOVE WS-FIFTH-CRN TO FIFTH-CRN
                   WRITE REG-REC
                   DISPLAY BLNK-SCRN
                   DISPLAY SCRN-WRITE-SAVE
                   DISPLAY SCRN-ANOTHER
                   ACCEPT SCRN-ANOTHER
           ELSE 
               DISPLAY BLNK-SCRN
               DISPLAY SCRN-WRITE-NOT-SAVE
               DISPLAY SCRN-ANOTHER
               ACCEPT SCRN-ANOTHER
           END-IF.
      *-----------------------------------------------------------------
       200-SAVE2.
           IF SAVE
               THEN
                   REWRITE REG-REC
                   DISPLAY BLNK-SCRN
                   DISPLAY SCRN-WRITE-SAVE
                   DISPLAY SCRN-ANOTHER
                   ACCEPT SCRN-ANOTHER
           ELSE 
               DISPLAY BLNK-SCRN
               DISPLAY SCRN-WRITE-NOT-SAVE
               DISPLAY SCRN-ANOTHER
               ACCEPT SCRN-ANOTHER
           END-IF.
      *-----------------------------------------------------------------
       300-GET-CLASS-NAME.
           STRING WS-YEAR DELIMITED BY SIZE
                  WS-SPACE DELIMITED BY SIZE
                  WS-SEM DELIMITED BY SIZE
                  WS-SPACE DELIMITED BY SIZE
                  WS-CRN DELIMITED BY SIZE
                  INTO SCHEDULE-ID-O.

           OPEN INPUT SCHE-MST.
           OPEN INPUT CRSE-MASTER.
           READ SCHE-MST
               INVALID KEY
                   MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                   MOVE ZERO TO WS-CRN
               NOT INVALID KEY
                   MOVE COURSE-ID-O TO CRSE-ID
                   READ CRSE-MASTER
                       INVALID KEY
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                          MOVE ZERO TO WS-CRN
                       NOT INVALID KEY
                          MOVE CRSE-NAME TO WS-CRSE-NAME
                   END-READ
           END-READ
           
           CLOSE SCHE-MST.
           CLOSE CRSE-MASTER.
      *-----------------------------------------------------------------
       400-SECOND-CRN-VAL1.
           
           IF WS-CRN = WS-FIRST-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
      *-----------------------------------------------------------------
       410-THIRD-CRN-VAL1.
       
           IF WS-CRN = WS-FIRST-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = WS-SECOND-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
      *-----------------------------------------------------------------
       420-FOURTH-CRN-VAL1.
       
           IF WS-CRN = WS-FIRST-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = WS-SECOND-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = WS-THIRD-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
      *-----------------------------------------------------------------
       430-FIFTH-CRN-VAL1.
       
           IF WS-CRN = WS-FIRST-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = WS-SECOND-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = WS-THIRD-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = WS-FOURTH-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
      *-----------------------------------------------------------------
       440-SECOND-CRN-VAL2.
       
           IF WS-CRN = FIRST-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = FIFTH-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = THIRD-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = FOURTH-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
      *-----------------------------------------------------------------
       450-THIRD-CRN-VAL2.
       
           IF WS-CRN = FIRST-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = SECOND-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = FIFTH-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = FOURTH-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
      *-----------------------------------------------------------------
       460-FOURTH-CRN-VAL2.
       
           IF WS-CRN = FIRST-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = SECOND-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = THIRD-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = FIFTH-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
      *-----------------------------------------------------------------
       470-FIFTH-CRN-VAL2.
       
           IF WS-CRN = FIRST-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = SECOND-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = THIRD-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = FOURTH-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
      *-----------------------------------------------------------------
       480-FIRST-CRN-VAL2.
       
           IF WS-CRN = FIFTH-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = SECOND-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = THIRD-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.
           IF WS-CRN = FOURTH-CRN
               THEN
                   IF WS-CRN = 0
                       THEN
                          MOVE 'COURSE CAN NOT BE FOUND' TO WS-CRSE-NAME
                       ELSE
                          MOVE 'DUPLICATE COURSE' TO WS-CRSE-NAME
                          MOVE ZEROS TO WS-CRN
                   END-IF
           END-IF.