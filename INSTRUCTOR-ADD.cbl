      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID INSTRUCTOR-ADD.
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INSTR-MASTER         ASSIGN        TO 
                                       '../FILES/INSTR-MASTER.DAT'
                                       ORGANIZATION  IS INDEXED
                                       ACCESS        IS RANDOM
                                       RECORD KEY    IS INSTR-ID
                                       ALTERNATE KEY IS INSTR-NAME
                                       FILE STATUS   IS WS-STAT.
           SELECT MST-CTRL-LIST        ASSIGN TO 
                                       "../Files/MST-CTRL-LST.DAT"
                                       ORGANIZATION  IS RELATIVE
                                       ACCESS IS RANDOM
                                       RELATIVE KEY  IS WS-MST-REC-KEY
                                       FILE STATUS   IS WS-STAT.                        
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       COPY MST-CTRL-LIST-RECS.       
       FD  INSTR-MASTER.
       01  INSTR-REC.
           03  INSTR-ID    PIC 9999.
           03  INSTR-NAME  PIC X(22).
       WORKING-STORAGE SECTION.
       COPY WS-COMMON.
       
           03  WS-CURR-INSTR-ID    PIC 9999.
       01  WS-DTL-LN.
           03  WS-INSTR-ID         PIC 9999.
           03  WS-INSTR-NAME       PIC X(22).

      *-----------------------------------------------------------------
       SCREEN SECTION.
       COPY SCR-COMMON.
       01  SCRN-TITLE.
           03  LINE 3  COL 38  VALUE 'ADD INSTRUCTOR'.
       01  SCRN-INSTR-ID.
           03  LINE 7  COL 25  VALUE   'INSTRUCTOR ID: '.
           03          COL 43  PIC 9999 FROM WS-INSTR-ID.
       01  SCRN-INSTR-NAME.
           03  LINE 9  COL 25  VALUE     'INSTRUCTOR NAME: '.
           03          COL 43  PIC X(35) USING WS-INSTR-NAME 
                                         AUTO REQUIRED.
       
       01  SCRN-WRITE-ERR.
           03  LINE 22  COL 30  VALUE '   INSTRUCTOR ALREADY EXISTS'.
       01  SCRN-WRITE-SUC.
           03  LINE 22  COL 30  VALUE '      INSTRUCTOR ADDED'.
       01  SCRN-WRITE-NOT-SAVE.
           03  LINE 22  COL 30  VALUE '     INSTRUCTOR NOT ADDED'.      
       
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       000-MAIN.
       
           OPEN I-O INSTR-MASTER.
           OPEN I-O MST-CTRL-LIST.
           ACCEPT WS-DATE FROM DATE.
           ACCEPT WS-TIME FROM TIME.
           
           MOVE 5 TO WS-MST-REC-KEY.
           READ MST-CTRL-LIST
               NOT INVALID KEY
                   MOVE MST-INST-ID TO WS-INSTR-ID
           END-READ.

           MOVE 'Y' TO WS-ANOTHER.
           PERFORM UNTIL ANOTHER
               
               DISPLAY HEADER
               DISPLAY SCRN-TITLE
               DISPLAY SCRN-INSTR-ID
               MOVE SPACE TO WS-SAVE
               PERFORM UNTIL WS-SAVE = 'Y' OR WS-SAVE = 'N'
                   DISPLAY SCRN-INSTR-NAME
                   ACCEPT  SCRN-INSTR-NAME
                   
                   DISPLAY SCRN-SAVE
                   ACCEPT  SCRN-SAVE
               END-PERFORM
                   
               IF SAVE
                   THEN
                       MOVE WS-INSTR-ID   TO INSTR-ID
                       MOVE WS-INSTR-NAME TO INSTR-NAME
                       WRITE INSTR-REC
                           INVALID KEY
                               DISPLAY SCRN-WRITE-ERR
                               DISPLAY SCRN-ANOTHER
                               ACCEPT  SCRN-ANOTHER
                           NOT INVALID KEY
                               ADD 1 TO WS-INSTR-ID
                               MOVE WS-INSTR-ID TO MST-INST-ID
                               REWRITE MST-NEXT-INST
                               DISPLAY SCRN-WRITE-SUC
                               DISPLAY SCRN-ANOTHER
                               ACCEPT  SCRN-ANOTHER
               ELSE
                   DISPLAY SCRN-WRITE-NOT-SAVE
                   DISPLAY SCRN-ANOTHER
                   ACCEPT SCRN-ANOTHER
               END-IF
           END-PERFORM.
           
           CLOSE INSTR-MASTER.
           CLOSE MST-CTRL-LIST.
           EXIT PROGRAM.
           
           
           
           