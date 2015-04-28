       IDENTIFICATION DIVISION.
       PROGRAM-ID. REGISTER-DROP.
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
           SELECT CRSE-MASTER  ASSIGN        TO 
                               '../FILES/COURSE-MASTER-SORT.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS CRSE-ID
                               FILE STATUS   IS WS-STAT. 
           SELECT SCHE-MST     ASSIGN TO
                               '../FILES/SCHEDULE-MASTER.DAT'
                               ORGANIZATION  IS INDEXED
                               ACCESS        IS RANDOM
                               RECORD KEY    IS SCHEDULE-ID-O
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
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-SEL              PIC X.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-SAVE             PIC X   VALUE 'N'.
               88  SAVE                    VALUE 'Y'.
           03  WS-ANOTHER          PIC X   VALUE 'Y'.
               88  ANOTHER                 VALUE 'N'.
           03  WS-STU-NAME         PIC X(20).
           03  WS-SPACE            PIC X VALUE SPACE.
           03  WS-CRN              PIC 9999.
           03  WS-CRSE-NAME        PIC X(39).
       01  WS-DTL-LN.
           03  WS-STU-ID               PIC 9(4).
           03  WS-FIRST-CRSE           PIC X(35).
           03  WS-SECOND-CRSE          PIC X(35).
           03  WS-THIRD-CRSE           PIC X(35).
           03  WS-FOURTH-CRSE          PIC X(35).
           03  WS-FIFTH-CRSE           PIC X(35).
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.
       01  SCRN-TITLE.
           03  LINE 3  COL 30  VALUE 'REGISTER DROP CLASSES'.
       01  SCRN-DATA.
           03  SCRN-STU-ID.
               05  LINE 5  COL 25  VALUE   'STUDENT ID   : '.
               05          COL 42  PIC 9(4) TO WS-STU-ID          
                                            AUTO REQUIRED.
           03  SCRN-STU-NAME.
               05  LINE 7  COL 25  VALUE   'STUDENT NAME : '.
               05          COL 42  PIC X(20) FROM WS-STU-NAME.
           03  SCRN-YEAR-SEM.
               05  LINE 9  COL 15  VALUE   'YEAR: '.
               05          COL 22  PIC ZZZ9 TO REG-YEAR
                                            AUTO REQUIRED FULL.
               05          COL 50  VALUE   'SEMESTER: '.
               05          COL 61  PIC Z9   TO REG-SEM
                                            AUTO REQUIRED. 
       01  SCRN-CRSE.    
           03  SCRN-FIRST-CRSE.
               05  LINE 11  COL 25  VALUE   '1) FIRST COURSE : '.
               05          COL 43  PIC X(39) FROM WS-CRSE-NAME.
           03  SCRN-SECOND-CRSE.
               05  LINE 12 COL 25  VALUE   '2) SECOND COURSE: '.
               05          COL 43  PIC X(39) FROM WS-CRSE-NAME.
           03  SCRN-THIRD-CRSE.
               05  LINE 13 COL 25  VALUE   '3) THIRD COURSE : '.
               05          COL 43  PIC X(39) FROM WS-CRSE-NAME.
           03  SCRN-FOURTH-CRSE.
               05  LINE 14 COL 25  VALUE   '4) FOURTH COURSE: '.
               05          COL 43  PIC X(39) FROM WS-CRSE-NAME.
           03  SCRN-FIFTH-CRSE.
               05  LINE 15 COL 25  VALUE   '5) FIFTH COURSE : '.
               05          COL 43  PIC X(39) FROM WS-CRSE-NAME.
           03  SCRN-RETURN.
               05  LINE 16 COL 25  VALUE   'R) FINISH'.
           03  SCRN-SEL.
               05  LINE 18  COL 32  VALUE     'SELECTION'.
               05           COL 30  PIC X     TO WS-SEL.
       01  SCRN-ERR1.
           03  LINE 5  COL 30  VALUE 'STUDENT CANNOT BE FOUND'.
       01  SCRN-ERR2.
           03  LINE 5  COL 30  VALUE 'STUDENT HAS NOT REGISTERED'.
       01  SCRN-CONTINUE.
           03  LINE 7 COL 32  VALUE 'CONTINUE? (Y/N)'.
           03          COL 30  PIC X TO WS-ANOTHER
                                        REQUIRED.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN I-O REG-MASTER.
           OPEN INPUT STU-MST.
           
           PERFORM UNTIL ANOTHER
           DISPLAY BLNK-SCRN
           DISPLAY SCRN-TITLE
           DISPLAY SCRN-STU-ID
           ACCEPT  SCRN-STU-ID
           
           MOVE WS-STU-ID TO STU-ID
               READ STU-MST
                   INVALID KEY
                       DISPLAY BLNK-SCRN
                       DISPLAY SCRN-ERR1
                       DISPLAY SCRN-CONTINUE
                       ACCEPT  SCRN-CONTINUE
                   NOT INVALID KEY
                       STRING
                       STU-F-NAME DELIMITED BY SPACE
                       WS-SPACE   DELIMITED BY SIZE
                       STU-L-NAME DELIMITED BY SPACE
                       INTO WS-STU-NAME
                       DISPLAY SCRN-STU-NAME
                       DISPLAY SCRN-YEAR-SEM
                       ACCEPT  SCRN-YEAR-SEM
                       MOVE WS-STU-ID  TO REG-STU-ID                               
                           READ REG-MASTER
                               INVALID KEY
                                   DISPLAY BLNK-SCRN
                                   DISPLAY SCRN-ERR2
                                   DISPLAY SCRN-CONTINUE
                                   ACCEPT  SCRN-CONTINUE
                               NOT INVALID KEY
                                   MOVE 'X' TO WS-SEL
                                   PERFORM UNTIL WS-SEL = 'R'
                                       MOVE FIRST-CRN TO WS-CRN
                                       PERFORM 200-GET-CLASS-NAME
                                       DISPLAY SCRN-FIRST-CRSE
                                       MOVE SECOND-CRN TO WS-CRN
                                       PERFORM 200-GET-CLASS-NAME
                                       DISPLAY SCRN-SECOND-CRSE
                                       MOVE THIRD-CRN TO WS-CRN
                                       PERFORM 200-GET-CLASS-NAME
                                       DISPLAY SCRN-THIRD-CRSE
                                       MOVE FOURTH-CRN TO WS-CRN
                                       PERFORM 200-GET-CLASS-NAME
                                       DISPLAY SCRN-FOURTH-CRSE
                                       MOVE FIFTH-CRN TO WS-CRN
                                       PERFORM 200-GET-CLASS-NAME
                                       DISPLAY SCRN-FIFTH-CRSE
                                       DISPLAY SCRN-RETURN
                                       DISPLAY SCRN-SEL
                                       ACCEPT SCRN-SEL
                                       PERFORM 100-EVALUATE-SEL
                                   END-PERFORM
                                   DISPLAY BLNK-SCRN
                                   DISPLAY SCRN-CONTINUE
                                   ACCEPT  SCRN-CONTINUE
                           END-READ
               END-READ
           END-PERFORM.
           
           CLOSE REG-MASTER.
           CLOSE STU-MST.
           
           EXIT PROGRAM.
      *-----------------------------------------------------------------
       100-EVALUATE-SEL.
           
           EVALUATE WS-SEL
               WHEN '1'
                   MOVE ZEROS TO FIRST-CRN
                   REWRITE REG-REC
               WHEN '2'
                   MOVE ZEROS TO SECOND-CRN
                   REWRITE REG-REC
               WHEN '3'
                   MOVE ZEROS TO THIRD-CRN
                   REWRITE REG-REC      
               WHEN '4'
                   MOVE ZEROS TO FOURTH-CRN
                   REWRITE REG-REC
               WHEN '5'
                   MOVE ZEROS TO FIFTH-CRN
                   REWRITE REG-REC
           END-EVALUATE. 
      *-----------------------------------------------------------------
       200-GET-CLASS-NAME.
       
           MOVE SPACES TO WS-CRSE-NAME

           STRING REG-YEAR DELIMITED BY SIZE
                  WS-SPACE DELIMITED BY SIZE
                  REG-SEM DELIMITED BY SIZE
                  WS-SPACE DELIMITED BY SIZE
                  WS-CRN DELIMITED BY SIZE
                  INTO SCHEDULE-ID-O.

           OPEN INPUT SCHE-MST.
           OPEN INPUT CRSE-MASTER.
           READ SCHE-MST
               INVALID KEY
                   MOVE 'NOT YET REGISTERED' TO WS-CRSE-NAME
               NOT INVALID KEY
                   MOVE COURSE-ID-O TO CRSE-ID
                   READ CRSE-MASTER
                       INVALID KEY
                       NOT INVALID KEY
                          STRING WS-CRN DELIMITED BY SIZE
                                 WS-SPACE DELIMITED BY SIZE
                                 CRSE-NAME DELIMITED BY SIZE
                                 INTO WS-CRSE-NAME
                   END-READ
           END-READ
           
           CLOSE SCHE-MST.
           CLOSE CRSE-MASTER.
      *----------------------------------------------------------------- 