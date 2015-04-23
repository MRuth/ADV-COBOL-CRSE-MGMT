       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-SCHEDULE.
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
           SELECT STU-MST      ASSIGN        TO 
                               '../FILES/STUDENT-MASTER.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS STU-ID
                               FILE STATUS   IS WS-STAT.
           SELECT CRSE-MASTER  ASSIGN        TO 
                               '../FILES/COURSE-MASTER-SORT.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS CRSE-ID
                               FILE STATUS   IS WS-STAT. 
           SELECT SCHE-MST     ASSIGN        TO
                               '../FILES/SCHEDULE-MASTER.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS SCHEDULE-ID-O
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
       FD  CRSE-MASTER.
       01  CRSE-REC.
           03  CRSE-ID        PIC X(9).
           03  CRSE-NAME      PIC X(35).
           03  CRSE-CREDIT    PIC X(4).
           03  CRSE-STAT      PIC X.
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
       FD  INSTR-MASTER.
       01  INSTR-REC.
           03  INSTR-ID    PIC 9999.
           03  INSTR-NAME  PIC X(22).
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-STU-NAME         PIC X(20).
           03  WS-SPACE            PIC X VALUE SPACE.
           03  WS-CRN              PIC 9999.
           03  WS-STU-ID           PIC 9999.
       01  WS-DTL-LN.
           03  WS-CRSE-NAME        PIC X(35).
           03  FILLER              PIC X VALUE SPACE.
           03  WS-CRSE-D-T         PIC X(20).
           03  FILLER              PIC X VALUE SPACE.
           03  WS-INST-NAME        PIC X(22).
       01  WS-BLNK-LN              PIC X(80) VALUE SPACES.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.
       01  SCRN-TITLE.
           03  LINE 1  COL 30  VALUE 'STUDENT SCHEDULE'.
       01  SCRN-DATA.
           03  SCRN-STU-ID.
               05  LINE 3  COL 1   VALUE   'STUDENT ID  : '.
               05          COL 17  PIC 9(4) TO WS-STU-ID          
                                            AUTO REQUIRED.
               05          COL 25  VALUE   'YEAR: '.
               05          COL 31  PIC ZZZ9 TO REG-YEAR
                                            AUTO REQUIRED FULL.
               05          COL 37  VALUE   'SEMESTER: '.
               05          COL 47  PIC Z9   TO REG-SEM
                                            AUTO REQUIRED. 
           03  SCRN-STU-NAME.
               05  LINE 5  COL 1  VALUE   'STUDENT NAME: '.
               05          COL 17  PIC X(20) FROM WS-STU-NAME.
       01  SCRN-CRSE.    
           03  LINE 7  COL 1   VALUE 'COURSE NAME'.
           03          COL 37  VALUE 'DATE AND TIME'.
           03          COL 58  VALUE 'INSTRUCTOR'.
       01  SCRN-ERR1.
           03  LINE 8  COL 30  VALUE 'STUDENT CANNOT BE FOUND'.
       01  SCRN-ERR2.
           03  LINE 8  COL 30  VALUE 'STUDENT HAS NOT REGISTERED CLASS'.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN I-O REG-MASTER.
           OPEN INPUT STU-MST.
           OPEN INPUT SCHE-MST.
           OPEN INPUT CRSE-MASTER.
           OPEN INPUT INSTR-MASTER.
           
           DISPLAY BLNK-SCRN.
           DISPLAY SCRN-TITLE.
           DISPLAY SCRN-STU-ID.
           ACCEPT  SCRN-STU-ID.
           
           MOVE WS-STU-ID TO STU-ID.
           READ STU-MST
               INVALID KEY
                   DISPLAY BLNK-SCRN
                   DISPLAY SCRN-ERR1
                   ACCEPT WS-RESP
                   EXIT PROGRAM
               NOT INVALID KEY
                   MOVE WS-STU-ID TO REG-STU-ID
                   READ REG-MASTER
                       INVALID KEY
                           DISPLAY BLNK-SCRN
                           DISPLAY SCRN-ERR2
                           ACCEPT WS-RESP
                           EXIT PROGRAM
                       NOT INVALID KEY
                           STRING
                           STU-F-NAME DELIMITED BY SPACE
                           WS-SPACE   DELIMITED BY SIZE
                           STU-L-NAME DELIMITED BY SPACE
                           INTO WS-STU-NAME
                           DISPLAY SCRN-STU-NAME
                           DISPLAY SCRN-CRSE
                           DISPLAY WS-BLNK-LN
                           IF FIRST-CRN <> 0
                               THEN
                                   MOVE FIRST-CRN TO WS-CRN
                                   PERFORM 100-GET-COURSE-INFO
                                   DISPLAY WS-DTL-LN
                                   DISPLAY WS-BLNK-LN
                           END-IF
                           IF SECOND-CRN <> 0
                               THEN
                                   MOVE SECOND-CRN TO WS-CRN
                                   PERFORM 100-GET-COURSE-INFO
                                   DISPLAY WS-DTL-LN
                                   DISPLAY WS-BLNK-LN
                           END-IF
                           IF THIRD-CRN <> 0
                               THEN
                                   MOVE THIRD-CRN TO WS-CRN
                                   PERFORM 100-GET-COURSE-INFO
                                   DISPLAY WS-DTL-LN
                                   DISPLAY WS-BLNK-LN
                           END-IF
                           IF FOURTH-CRN <> 0
                               THEN
                                   MOVE FOURTH-CRN TO WS-CRN
                                   PERFORM 100-GET-COURSE-INFO
                                   DISPLAY WS-DTL-LN
                                   DISPLAY WS-BLNK-LN
                           END-IF
                           IF FIFTH-CRN <> 0
                               THEN
                                   MOVE FIFTH-CRN TO WS-CRN
                                   PERFORM 100-GET-COURSE-INFO
                                   DISPLAY WS-DTL-LN
                                   DISPLAY WS-BLNK-LN
                           END-IF
                   END-READ
           END-READ. 

           DISPLAY 'PRESS ENTER TO RETURN TO MENU'.
           ACCEPT WS-RESP.

           CLOSE REG-MASTER.
           CLOSE STU-MST.
           CLOSE SCHE-MST.
           CLOSE CRSE-MASTER.
           CLOSE INSTR-MASTER.
           
           EXIT PROGRAM.
      *-----------------------------------------------------------------
       100-GET-COURSE-INFO.
           
           MOVE SPACES TO WS-CRSE-NAME
           MOVE SPACES TO WS-CRSE-D-T
           MOVE SPACES TO WS-INST-NAME
    
           STRING REG-YEAR DELIMITED BY SIZE
                  WS-SPACE DELIMITED BY SIZE
                  REG-SEM DELIMITED BY SIZE
                  WS-SPACE DELIMITED BY SIZE
                  WS-CRN DELIMITED BY SIZE
                  INTO SCHEDULE-ID-O
 
           READ SCHE-MST
               INVALID KEY
               NOT INVALID KEY
                   MOVE COURSE-ID-O TO CRSE-ID
                   MOVE TIMEDAY-O   TO WS-CRSE-D-T
                   MOVE INSTRUCTOR-ID-O TO INSTR-ID
                   READ CRSE-MASTER
                       INVALID KEY
                       NOT INVALID KEY
                           MOVE CRSE-NAME TO WS-CRSE-NAME
                   END-READ
                   READ INSTR-MASTER
                       INVALID KEY
                       NOT INVALID KEY
                           MOVE INSTR-NAME TO WS-INST-NAME
                   END-READ
           END-READ
      *-----------------------------------------------------------------           