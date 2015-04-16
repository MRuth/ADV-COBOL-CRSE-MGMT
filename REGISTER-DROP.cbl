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
                               RECORD KEY    IS REG-STU-ID
                               FILE STATUS   IS WS-STAT.
           SELECT STU-MST      ASSIGN TO 
                               '../FILES/STUDENT-MASTER.DAT'
                               ORGANIZATION    IS INDEXED
                               ACCESS          IS RANDOM
                               RECORD KEY      IS STU-ID
                               FILE STATUS     IS WS-STAT.                        
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       COPY STU-MST-DEF.
       FD  REG-MASTER.
       01  REG-REC.
           03  REG-STU-ID          PIC 9(4).
           03  REG-YEAR            PIC 9999.
           03  REG-SEM             PIC 99.           
           03  FIRST-CRN           PIC 9(4).
           03  SECOND-CRN          PIC 9(4).
           03  THIRD-CRN           PIC 9(4).
           03  FOURTH-CRN          PIC 9(4).
           03  FIFTH-CRN           PIC 9(4).
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
           03  LINE 1  COL 30  VALUE 'REGISTER DROP CLASSES'.
       01  SCRN-DATA.
           03  SCRN-STU-ID.
               05  LINE 3  COL 25  VALUE   'STUDENT ID   : '.
               05          COL 42  PIC 9(4) TO WS-STU-ID          
                                            AUTO REQUIRED.
           03  SCRN-STU-NAME.
               05  LINE 5  COL 25  VALUE   'STUDENT NAME : '.
               05          COL 42  PIC X(20) FROM WS-STU-NAME.
       01  SCRN-CRN.
           03  SCRN-YEAR-SEM.
               05  LINE 7  COL 15  VALUE   'YEAR: '.
               05          COL 22  PIC ZZZ9 FROM REG-YEAR.
               05          COL 50  VALUE   'SEMESTER: '.
               05          COL 61  PIC Z9   FROM REG-SEM.       
           03  SCRN-FIRST-CRN.
               05  LINE 9  COL 25  VALUE   '1) FIRST CRN    : '.
               05          COL 42  PIC ZZZ9 FROM FIRST-CRN.
           03  SCRN-SECOND-CRN.
               05  LINE 10 COL 25  VALUE   '2) SECOND CRN   : '.
               05          COL 42  PIC ZZZ9 FROM SECOND-CRN.
           03  SCRN-THIRD-CRN.
               05  LINE 11 COL 25  VALUE   '3) THIRD CRN    : '.
               05          COL 42  PIC ZZZ9 FROM THIRD-CRN.
           03  SCRN-FOURTH-CRN.
               05  LINE 12 COL 25  VALUE   '4) FOURTH CRN   : '.
               05          COL 42  PIC ZZZ9 FROM FOURTH-CRN.
           03  SCRN-FIFTH-CRN.
               05  LINE 13 COL 25  VALUE   '5) FIFTH CRN    : '.
               05          COL 42  PIC ZZZ9 FROM FIFTH-CRN.
           03  SCRN-RETURN.
               05  LINE 14 COL 25  VALUE   'R) FINISH'.
           03  SCRN-SEL.
               05  LINE 16  COL 32  VALUE     'SELECTION'.
               05           COL 30  PIC X     TO WS-SEL.
       01  SCRN-ERR1.
           03  LINE 8  COL 30  VALUE 'STUDENT CANNOT BE FOUND'.
       01  SCRN-ERR2.
           03  LINE 8  COL 30  VALUE 'STUDENT HAS NOT REGISTERED'.
       01  SCRN-CONTINUE.
           03  LINE 10 COL 32  VALUE 'CONTINUE? (Y/N)'.
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
                       MOVE WS-STU-ID  TO REG-STU-ID
                           READ REG-MASTER
                               INVALID KEY
                                   DISPLAY BLNK-SCRN
                                   DISPLAY SCRN-ERR2
                                   DISPLAY SCRN-CONTINUE
                                   ACCEPT  SCRN-CONTINUE
                               NOT INVALID KEY
                                   STRING
                                   STU-F-NAME DELIMITED BY SPACE
                                   WS-SPACE   DELIMITED BY SIZE
                                   STU-L-NAME DELIMITED BY SPACE
                                   INTO WS-STU-NAME
                                   DISPLAY SCRN-STU-NAME
                                   PERFORM UNTIL WS-SEL = 'R'
                                       DISPLAY SCRN-CRN
                                       ACCEPT SCRN-SEL
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
                                       END-EVALUATE
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
               