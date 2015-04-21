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
                                       FILE STATUS   IS WS-MST-STAT.                        
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
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-SAVE             PIC X   VALUE 'N'.
               88  SAVE                    VALUE 'Y'.
           03  WS-ANOTHER          PIC X   VALUE 'Y'.
               88  ANOTHER                 VALUE 'N'.
           03  WS-MST-REC-KEY      PIC 9999.
           03  WS-MST-STAT         PIC XX.
           03  WS-CURR-INSTR-ID    PIC 9999.
       01  WS-DTL-LN.
           03  WS-INSTR-ID         PIC 9999.
           03  WS-INSTR-NAME       PIC X(22).
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.
       01  SCRN-TITLE.
           03  LINE 1  COL 30  VALUE 'ADD INSTRUCTOR'.
       01  SCRN-DATA.
           03  SCRN-INSTR-ID.
               05  LINE 3  COL 25  VALUE   'INSTRUCTOR ID: '.
               05          COL 43  PIC 9999 FROM WS-INSTR-ID.
           03  SCRN-INSTR-NAME.
               05  LINE 4  COL 25  VALUE     'INSTRUCTOR NAME: '.
               05          COL 43  PIC X(35) TO WS-INSTR-NAME 
                                             AUTO REQUIRED.
           03  SCRN-SAVE.
               05  LINE 6  COL 32  VALUE   'SAVE (Y/N)'.
               05          COL 30  PIC X    TO WS-SAVE.
       01  SCRN-WRITE-ERR.
           03  LINE 1  COL 30  VALUE 'INSTRUCTOR IS ALREADY EXIST'.
       01  SCRN-WRITE-SUC.
           03  LINE 1  COL 30  VALUE 'INSTRUCTOR IS ADDED'.
       01  SCRN-WRITE-NOT-SAVE.
           03  LINE 1  COL 30  VALUE 'INSTRUCTOR IS NOT ADDED'.         
       01  SCRN-ANOTHER.
           03  LINE 3  COL 32  VALUE 'ADD ANOTHER? (Y/N)'.
           03          COL 30  PIC X TO WS-ANOTHER.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       000-MAIN.
       
           OPEN I-O INSTR-MASTER.
           OPEN I-O MST-CTRL-LIST.
           
           MOVE 5 TO WS-MST-REC-KEY.
           READ MST-CTRL-LIST
               NOT INVALID KEY
                   MOVE MST-INST-ID TO WS-INSTR-ID
           END-READ.

           MOVE 'Y' TO WS-ANOTHER.
           PERFORM UNTIL ANOTHER
                   DISPLAY BLNK-SCRN
                   DISPLAY SCRN-TITLE
                   DISPLAY SCRN-DATA
                   
                   ACCEPT  SCRN-INSTR-NAME
                   
                   DISPLAY SCRN-SAVE
                   ACCEPT  SCRN-SAVE
                   
                   IF SAVE
                       THEN
                           MOVE WS-INSTR-ID   TO INSTR-ID
                           MOVE WS-INSTR-NAME TO INSTR-NAME
                           WRITE INSTR-REC
                               INVALID KEY
                                   DISPLAY BLNK-SCRN
                                   DISPLAY SCRN-WRITE-ERR
                                   DISPLAY SCRN-ANOTHER
                                   ACCEPT  SCRN-ANOTHER
                               NOT INVALID KEY
                                   ADD 1 TO WS-INSTR-ID
                                   DISPLAY BLNK-SCRN
                                   DISPLAY SCRN-WRITE-SUC
                                   DISPLAY SCRN-ANOTHER
                                   ACCEPT  SCRN-ANOTHER
                   ELSE 
                       DISPLAY BLNK-SCRN
                       DISPLAY SCRN-WRITE-NOT-SAVE
                       DISPLAY SCRN-ANOTHER
                       ACCEPT SCRN-ANOTHER
                   END-IF
           END-PERFORM.
           
           MOVE WS-INSTR-ID TO MST-INST-ID
           REWRITE MST-NEXT-INST.
           
           CLOSE INSTR-MASTER.
           CLOSE MST-CTRL-LIST.
           EXIT PROGRAM.
           
           
           
           