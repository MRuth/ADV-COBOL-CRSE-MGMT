       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLASS-ROLL.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REG-MASTER   ASSIGN        TO 
                               '../FILES/REGISTER-MASTER.DAT'   
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS SEQUENTIAL
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
                               '../FILES/SCHEDULE-MASTER.DAT'
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
           SELECT INSTR-MASTER ASSIGN        TO 
                               '../FILES/INSTR-MASTER.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS INSTR-ID
                               ALTERNATE KEY IS INSTR-NAME
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
       FD  INSTR-MASTER.
       01  INSTR-REC.
           03  INSTR-ID    PIC 9999.
           03  INSTR-NAME  PIC X(22).
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-ANOTHER          PIC X   VALUE 'Y'.
               88  ANOTHER                 VALUE 'N'.
           03  WS-STU-ID           PIC 9999.
           03  WS-STU-NAME         PIC X(20).
           03  WS-SPACE            PIC X VALUE SPACE.
           03  WS-YEAR             PIC 9999.
           03  WS-SEM              PIC 99.
           03  WS-CRN              PIC 9999.
           03  WS-CRSE-NAME        PIC X(35).
           03  WS-COUNTER          PIC 99 VALUE 0.
           03  WS-BLNK-LN          PIC X(80) VALUE SPACES.
           03  WS-INSTR-ID         PIC 9999.
           03  WS-INSTR-NAME       PIC X(20).
       01  WS-HEADER.
           03  FILLER              PIC X(25) VALUE 'NAME'.
           03  FILLER              PIC X(55) VALUE 'CLASS ROLE'.
       01  WS-DTL-LN.
           03  WS-NAME             PIC X(20).
           03  FILLER              PIC X(5) VALUE SPACES.
           03  WS-ROLE             PIC X(10).
       01  WS-PG-BREAK.
           03  FILLER              PIC X(15) VALUE 'PRESS ENTER TO '.
           03  FILLER              PIC X(16) VALUE 'DISPLAY 07 MORE '.
           03  FILLER              PIC X(49) VALUE 'STUDENTS'.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.
       01  SCRN-TITLE.
           03  LINE 3  COL 30  VALUE 'CLASS ROLL'.
       01  SCRN-DATA.
           03  SCRN-CRSE.
               05  LINE 5  COL 1   VALUE  'COURSE CRN: '.
               05          COL 14  PIC 9999 USING WS-CRN         
                                            AUTO REQUIRED FULL.
               05          COL 20  VALUE   'YEAR: '.
               05          COL 26  PIC ZZZ9 USING WS-YEAR
                                            AUTO REQUIRED FULL.
               05          COL 35  VALUE   'SEMESTER: '.
               05          COL 45  PIC Z9   USING WS-SEM
                                            AUTO REQUIRED FULL.   
           03  SCRN-CRSE-NAME.
               05  LINE 7  COL 1   VALUE   'COURSE NAME: '.
               05          COL 14  PIC X(35) FROM WS-CRSE-NAME. 
       01  SCRN-ERR.
           03  LINE 5  COL 30  VALUE 'COURSE CAN NOT BE FOUND'.     
       01  SCRN-ANOTHER.
           03  LINE 7  COL 32  VALUE 'LOOK UP ANOTHER COURSE? (Y/N)'.
           03          COL 30  PIC X TO WS-ANOTHER. 
       01  BLNK-SCREEN.
           03  BLANK SCREEN.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT REG-MASTER.
           OPEN INPUT STU-MST.

           DISPLAY BLNK-SCRN.
           DISPLAY SCRN-TITLE
           DISPLAY SCRN-CRSE
           ACCEPT  SCRN-CRSE

           PERFORM 100-GET-COURSE-NAME
           DISPLAY WS-BLNK-LN
           DISPLAY WS-HEADER
           DISPLAY WS-BLNK-LN
           MOVE 'N' TO WS-EOF
           MOVE ZERO TO WS-COUNTER
           PERFORM UNTIL EOF
           READ REG-MASTER
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF  WS-YEAR = REG-YEAR
                       AND WS-SEM = REG-SEM
                       THEN
                           IF  WS-CRN = FIRST-CRN
                               OR WS-CRN = SECOND-CRN
                               OR WS-CRN = THIRD-CRN
                               OR WS-CRN = FOURTH-CRN
                               OR WS-CRN = FIFTH-CRN
                               THEN
                                   MOVE REG-STU-ID TO WS-STU-ID
                                   PERFORM 200-GET-STU-NAME
                                   PERFORM 300-DISPLAY
                           END-IF
                   END-IF
           END-READ 
           END-PERFORM.

           PERFORM 400-GET-INSTRUCTOR-NAME.
           DISPLAY WS-DTL-LN.
           DISPLAY WS-BLNK-LN.
           DISPLAY 'PRESS ENTER TO RETURN TO MENU'.
           ACCEPT WS-RESP.
           CLOSE REG-MASTER.
           CLOSE STU-MST.

           EXIT PROGRAM.
      *-----------------------------------------------------------------
       100-GET-COURSE-NAME.
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
                   DISPLAY BLNK-SCRN
                   DISPLAY SCRN-ERR
                   ACCEPT  WS-RESP
                   EXIT PROGRAM
               NOT INVALID KEY
                   MOVE COURSE-ID-O TO CRSE-ID
                   MOVE INSTRUCTOR-ID-O TO WS-INSTR-ID
                   READ CRSE-MASTER
                       INVALID KEY
                           DISPLAY BLNK-SCRN
                           DISPLAY SCRN-ERR
                           ACCEPT  WS-RESP
                           EXIT PROGRAM
                       NOT INVALID KEY
                          MOVE CRSE-NAME TO WS-CRSE-NAME
                          DISPLAY SCRN-CRSE-NAME
                   END-READ
           END-READ
           
           CLOSE SCHE-MST.
           CLOSE CRSE-MASTER.
      *-----------------------------------------------------------------
       200-GET-STU-NAME.
           MOVE WS-STU-ID TO STU-ID.
           
                   READ STU-MST
                       INVALID KEY
                       NOT INVALID KEY
                           STRING
                               STU-F-NAME DELIMITED BY SPACE
                               WS-SPACE   DELIMITED BY SIZE
                               STU-L-NAME DELIMITED BY SPACE
                               INTO WS-STU-NAME
                   END-READ.
           MOVE WS-STU-NAME TO WS-NAME.
           MOVE 'STUDENT' TO WS-ROLE.
      *-----------------------------------------------------------------
       300-DISPLAY.

           DISPLAY WS-DTL-LN
           DISPLAY WS-BLNK-LN
           ADD 1 TO WS-COUNTER
           IF WS-COUNTER = 7
               THEN
                   DISPLAY WS-PG-BREAK
                   ACCEPT WS-RESP
                   DISPLAY BLNK-SCREEN
                   DISPLAY SCRN-TITLE
                   DISPLAY SCRN-DATA
                   DISPLAY WS-BLNK-LN
                   DISPLAY WS-HEADER
                   DISPLAY WS-BLNK-LN
                   MOVE 0 TO WS-COUNTER
           END-IF. 
      *-----------------------------------------------------------------
       400-GET-INSTRUCTOR-NAME.
           
           OPEN INPUT INSTR-MASTER.
           MOVE WS-INSTR-ID TO INSTR-ID.
           READ INSTR-MASTER
               INVALID KEY
                   MOVE SPACES TO WS-NAME
                   MOVE SPACES TO WS-ROLE
               NOT INVALID KEY
                   MOVE INSTR-NAME TO WS-NAME
                   MOVE 'INSTRUCTOR' TO WS-ROLE 
           END-READ.
           CLOSE INSTR-MASTER.