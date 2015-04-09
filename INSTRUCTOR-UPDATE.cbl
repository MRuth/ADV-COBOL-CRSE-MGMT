      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID INSTRUCTOR-UPDATE.
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
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       FD  INSTR-MASTER.
       01  INSTR-REC.
           03  INSTR-ID    PIC 9999.
           03  INSTR-NAME  PIC X(22).
       WORKING-STORAGE SECTION.
       01  MISC-VARS.
           03  WS-RESP             PIC X   VALUE SPACE.
           03  WS-STAT             PIC 99.
           03  WS-ANOTHER          PIC X   VALUE 'Y'.
               88  ANOTHER                 VALUE 'N'.
           03  WS-EOF              PIC X   VALUE 'N'.
               88  EOF                     VALUE 'Y'.
           03  WS-SAVE             PIC X   VALUE 'N'.
               88  SAVE                    VALUE 'Y'.   
           03  WS-OLD-NAME         PIC X(22).     
       01  WS-DTL-LN.
           03  WS-INSTR-ID         PIC 9999.
           03  WS-INSTR-NAME       PIC X(22).
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  BLNK-SCRN.
           03  BLANK SCREEN.
       01  SCRN-TITLE.
           03  LINE 1  COL 30  VALUE 'INSTRUCTOR UPDATE'.
       01  SCRN-ID.
            05  LINE 3  COL 25  VALUE    'INSTRUCTOR ID  : '.
            05          COL 42  PIC ZZZZ TO WS-INSTR-ID          
                                         AUTO REQUIRED.       
       01  SCRN-DATA.
           03  SCRN-INSTR-NEW-NAME.
               05  LINE 5  COL 25  VALUE 'INSTRUCTOR NAME: '.
               05          COL 42  PIC X(22)    USING WS-INSTR-NAME
                                                AUTO REQUIRED.
           03  SCRN-SAVE.
               05  LINE 7  COL 32  VALUE   'SAVE (Y/N)'.
               05          COL 30  PIC X    TO WS-SAVE
                                               REQUIRED.
       01  SCRN-CONFIRM1.
           03  LINE 8  COL 30  VALUE 'INSTRUCTOR IS UPDATED'.
       01  SCRN-CONFIRM2.
           03  LINE 8  COL 30  VALUE 'INSTRUCTOR IS NOT UPDATED'.                                                                          
       01  SCRN-ANOTHER.
           03  LINE 9  COL 32  VALUE 'UPDATE ANOTHER? (Y/N)'.
           03          COL 30  PIC X TO WS-ANOTHER.
       01  SCRN-ERR.
           03  LINE 8  COL 30  VALUE 'INSTRUCTOR NOT FOUND'.    
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       000-MAIN. 
           OPEN I-O INSTR-MASTER.
           
           MOVE 'Y' TO WS-ANOTHER.
           PERFORM UNTIL ANOTHER
           
               DISPLAY BLNK-SCRN
               DISPLAY SCRN-TITLE
               DISPLAY SCRN-ID
               ACCEPT  SCRN-ID
               
               MOVE WS-INSTR-ID TO INSTR-ID
               
               READ INSTR-MASTER
                   INVALID KEY
                       DISPLAY BLNK-SCRN
                       DISPLAY SCRN-ERR
                       DISPLAY SCRN-ANOTHER
                       ACCEPT SCRN-ANOTHER
                   NOT INVALID KEY
                       MOVE INSTR-NAME TO WS-INSTR-NAME
                       DISPLAY SCRN-DATA
                       ACCEPT SCRN-INSTR-NEW-NAME
                       ACCEPT SCRN-SAVE
                       IF SAVE
                           THEN                                         
                               MOVE WS-INSTR-NAME TO INSTR-NAME
                               REWRITE INSTR-REC
                               DISPLAY BLNK-SCRN
                               DISPLAY SCRN-CONFIRM1
                               DISPLAY SCRN-ANOTHER
                               ACCEPT SCRN-ANOTHER
                       ELSE
                           DISPLAY BLNK-SCRN
                           DISPLAY SCRN-CONFIRM2
                           DISPLAY SCRN-ANOTHER
                           ACCEPT SCRN-ANOTHER
                       END-IF    
               END-READ
           END-PERFORM.

           CLOSE INSTR-MASTER.
           EXIT PROGRAM.