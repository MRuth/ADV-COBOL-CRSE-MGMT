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
           03  FIRST-CRN           PIC 9(4).
           03  SECOND-CRN          PIC 9(4).
           03  THIRD-CRN           PIC 9(4).
           03  FOURTH-CRN          PIC 9(4).
           03  FIFTH-CRN           PIC 9(4).
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
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
           03  LINE 1  COL 30  VALUE 'REGISTER ADD CLASSES'.
       01  SCRN-DATA.
           03  SCRN-STU-ID.
               05  LINE 3  COL 25  VALUE   'STUDENT ID   : '.
               05          COL 40  PIC 9(4) TO WS-STU-ID          
                                            AUTO REQUIRED.
           03  SCRN-STU-NAME.
               05  LINE 5  COL 25  VALUE   'STUDENT NAME : '.
               05          COL 40  PIC X(20) FROM WS-STU-NAME.
       01  SCRN-CRN1.        
           03  SCRN-FIRST-CRN1.
               05  LINE 7  COL 25  VALUE   'FIRST CRN    : '.
               05          COL 40  PIC X(4) TO WS-FIRST-CRN
                                            AUTO.
           03  SCRN-SECOND-CRN1.
               05  LINE 8  COL 25  VALUE   'SECOND CRN   : '.
               05          COL 40  PIC X(4) TO WS-SECOND-CRN
                                            AUTO.
           03  SCRN-THIRD-CRN1.
               05  LINE 9  COL 25  VALUE   'THIRD CRN    : '.
               05          COL 40  PIC X(4) TO WS-THIRD-CRN
                                            AUTO.
           03  SCRN-FOURTH-CRN1.
               05  LINE 10  COL 25  VALUE   'FOURTH CRN   : '.
               05           COL 40  PIC X(4) TO WS-FOURTH-CRN
                                            AUTO.
           03  SCRN-FIFTH-CRN1.
               05  LINE 11  COL 25  VALUE   'FIFTH CRN    : '.
               05           COL 40  PIC X(4) TO WS-FIFTH-CRN
                                            AUTO.
       01  SCRN-CRN2.        
           03  SCRN-FIRST-CRN2.
               05  LINE 7  COL 25  VALUE   'FIRST CRN    : '.
               05          COL 40  PIC X(4) USING FIRST-CRN
                                            AUTO.
           03  SCRN-SECOND-CRN2.
               05  LINE 8  COL 25  VALUE   'SECOND CRN   : '.
               05          COL 40  PIC X(4) USING SECOND-CRN
                                            AUTO.
           03  SCRN-THIRD-CRN2.
               05  LINE 9  COL 25  VALUE   'THIRD CRN    : '.
               05          COL 40  PIC X(4) USING THIRD-CRN
                                            AUTO.
           03  SCRN-FOURTH-CRN2.
               05  LINE 10 COL 25  VALUE   'FOURTH CRN   : '.
               05          COL 40  PIC X(4) USING FOURTH-CRN
                                            AUTO.
           03  SCRN-FIFTH-CRN2.
               05  LINE 11 COL 25  VALUE   'FIFTH CRN    : '.
               05          COL 40  PIC X(4) USING FIFTH-CRN
                                            AUTO.                                                                                                                                                                      
           03  SCRN-SAVE.
               05  LINE 13  COL 32  VALUE     'SAVE (Y/N)'.
               05           COL 30  PIC X     TO WS-SAVE.
       01  SCRN-WRITE-ERR.
           03  LINE 8  COL 30  VALUE 'STUDENT CAN NOT BE FOUND'.
       01  SCRN-WRITE-SAVE.
           03  LINE 8  COL 30  VALUE 'REGISTERED SUCCESSFULLY'.
       01  SCRN-WRITE-NOT-SAVE.
           03  LINE 8  COL 30  VALUE 'REGISTERED UNSUCCESSFULLY'.      
       01  SCRN-ANOTHER.
           03  LINE 10 COL 32  VALUE 'CONTINUE? (Y/N)'.
           03          COL 30  PIC X TO WS-ANOTHER.
      *----------------------------------------------------------------- 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN I-O REG-MASTER.
           OPEN INPUT STU-MST.

           MOVE 'Y' TO WS-ANOTHER.
           PERFORM UNTIL ANOTHER
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
                               MOVE WS-STU-ID TO REG-STU-ID
                               READ REG-MASTER
                                   INVALID KEY
                                       DISPLAY SCRN-CRN1
                                       ACCEPT  SCRN-FIRST-CRN1
                                       ACCEPT  SCRN-SECOND-CRN1
                                       ACCEPT  SCRN-THIRD-CRN1
                                       ACCEPT  SCRN-FOURTH-CRN1
                                       ACCEPT  SCRN-FIFTH-CRN1
                                       DISPLAY SCRN-SAVE
                                       ACCEPT  SCRN-SAVE
                                       PERFORM 100-SAVE1
                                   NOT INVALID KEY
                                       DISPLAY SCRN-CRN2
                                       ACCEPT  SCRN-FIRST-CRN2
                                       ACCEPT  SCRN-SECOND-CRN2
                                       ACCEPT  SCRN-THIRD-CRN2
                                       ACCEPT  SCRN-FOURTH-CRN2
                                       ACCEPT  SCRN-FIFTH-CRN2
                                       DISPLAY SCRN-SAVE
                                       ACCEPT  SCRN-SAVE
                                       PERFORM 200-SAVE2
                   END-READ
           END-PERFORM.
           
           CLOSE REG-MASTER.
           CLOSE STU-MST.
           
           EXIT PROGRAM.
                                       
       
       100-SAVE1.
           IF SAVE
               THEN
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